module fpm_deps

use fpm_manifest, only : get_package_data, package_config_t
use fpm_dependency, only: dependency_tree_t, new_dependency_tree, &
    dependency_node_t

use fpm_settings, only: fpm_global_settings, get_global_settings
use fpm_error, only: error_t

implicit none
private

public :: config_t
public :: tree_t, new_tree

public :: package_properties
public :: dependency_props
public :: dependency_depth

public :: exclude_mask

public :: print_deps
public :: bfs_depth

type, extends(package_config_t) :: config_t
contains
    procedure :: init_from_file
end type

type, extends(dependency_tree_t) :: tree_t

    ! Number of dependencies (equal to size(tree%dep))
    integer :: ndeps = 0
    ! Number of edges between packages (excluding self-edges)
    integer :: nedges = 0

    !> Dependency graph
    integer, allocatable :: ia(:)  ! len (ndep + 1)
    integer, allocatable :: ja(:)  ! nnz = number of edges

    logical, allocatable :: is_dev_dep(:)
contains
    ! New method
    procedure, private :: resolve_with_graph => resolve_dependency_with_graph

    ! Override
    procedure, private :: resolve_dependencies => resolve_dependencies
end type


!> Common output format for writing to the command line
character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'


type :: package_properties
  character(len=:), allocatable :: description, homepage
end type

contains

    subroutine init_from_file(pack,file)

        use fpm_manifest, only: get_package_data
        use fpm_error, only: error_t

        class(config_t), intent(out) :: pack
        character(len=*), intent(in) :: file

        type(error_t), allocatable :: error

        call get_package_data(pack%package_config_t,file,error,apply_defaults=.true.)
        if (allocated(error)) then
            write(*,'(A)') "Error in fpm_deps: init_from_file"
            error stop 1
        end if

    end subroutine



  !> Create a new dependency tree
  subroutine new_tree(self, verbosity, cache)
    !> Instance of the dependency tree
    type(tree_t), intent(out) :: self
    !> Verbosity of printout
    integer, intent(in), optional :: verbosity
    !> Name of the cache file
    character(len=*), intent(in), optional :: cache

    ! initialize the parent
    call new_dependency_tree(self%dependency_tree_t,verbosity,cache)

    self%ia = [1]
    allocate(self%ja(0))

  end subroutine new_tree

  !> Resolve all dependencies in the tree
  subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    class(tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(fpm_global_settings) :: global_settings
    integer :: ii

    call get_global_settings(global_settings, error)
    if (allocated(error)) return

    do ii = 1, self%ndep
      associate(dep => self%dep(ii))
      !print *, "resolve_dependencies: "//dep%name
      call self%resolve_with_graph(ii, self%dep(ii), global_settings, root, error)
      if (allocated(error)) exit
      end associate
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  !> Resolve a single dependency node
  subroutine resolve_dependency_with_graph(self, id, dependency, global_settings, root, error)


    use fpm_manifest, only: package_config_t
    use fpm_filesystem, only: exists, join_path
    use fpm_git, only: git_target_revision, git_target_default, git_revision, serializable_t

    !> Instance of the dependency tree
    class(tree_t), intent(inout) :: self
    !> Dependency configuration to add
    integer, intent(in) :: id
    type(dependency_node_t), intent(inout) :: dependency
    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t) :: package
    character(len=:), allocatable :: manifest, proj_dir, revision
    logical :: fetch

    integer :: i, k, iaa, iab, ndeps
    integer, allocatable :: idep(:)

    if (dependency%done) return

    fetch = .false.
    if (allocated(dependency%proj_dir)) then
      proj_dir = dependency%proj_dir
    else if (allocated(dependency%path)) then
      proj_dir = join_path(root, dependency%path)
    else if (allocated(dependency%git)) then
      proj_dir = join_path(self%dep_dir, dependency%name)
      fetch = .not. exists(proj_dir)
      if (fetch) then
        call dependency%git%checkout(proj_dir, error)
        if (allocated(error)) return
      end if
    else
      call dependency%get_from_registry(proj_dir, global_settings, error)
      if (allocated(error)) return
    end if

    if (allocated(dependency%git)) then
      call git_revision(proj_dir, revision, error)
      if (allocated(error)) return
    end if

    manifest = join_path(proj_dir, "fpm.toml")
    call get_package_data(package, manifest, error)
    if (allocated(error)) return

    call dependency%register(package, proj_dir, fetch, revision, error)
    if (allocated(error)) return

    if (self%verbosity > 1) then
      write (self%unit, out_fmt) &
        "Dep:", dependency%name, "version", dependency%version%s(), &
        "at", dependency%proj_dir
    end if

    call self%add(package, proj_dir, .false., error)
    if (allocated(error)) return

    !print *, "ADDING edges, dep = ", size(dependency)
    ! ADD DEPENDENCY EDGES
    !id = self%find(dependency%name)
    !print *, "found id = ", id
    iaa = self%ia(id)
    !print *, id, size(self%ia), size(self%ja)
    if (allocated(package%dependency)) then
      ndeps = size(package%dependency)
      !print *, "ndeps = ", ndeps
      allocate(idep(ndeps))
      !print *, "ADDING edges for "//package%name
      do k = 1, ndeps
        idep(k) = self%find(package%dependency(k)%name)
      end do
      !print *, "  ", idep
      !dependency%ndeps = ndeps + 1
      self%ia = [self%ia, iaa + 1 + ndeps]
      self%ja = [self%ja, id, idep]
    else
      !dependency%ndeps = 1
      !print *, "NOTHING to add; "//package%name//" is a leaf package"
      self%ia = [self%ia, iaa + 1]
      self%ja = [self%ja, id]
    end if

  end subroutine resolve_dependency_with_graph


  function dependency_props(deps,mask) result(props)
    use fpm_dependency, only: dependency_node_t
    type(dependency_node_t), intent(in) :: deps(:)
    logical, intent(in), optional :: mask(:)
    type(package_properties), allocatable :: props(:)

    integer :: i

    allocate(props(size(deps)))

    do i = 1, size(deps)
      if (present(mask)) then
        if (mask(i)) props(i) = fetch_package_properties(deps(i))
      else
        props(i) = fetch_package_properties(deps(i))
      end if
    end do

  end function

  !> Fetch relevant properties from the fpm manifest file
  function fetch_package_properties(dep) result(props)

      use fpm_dependency, only: dependency_node_t
      use fpm_error, only: error_t, fatal_error
      use fpm_toml, only: toml_table, read_package_file, get_value
      use fpm_filesystem, only: join_path

      type(dependency_node_t), intent(in) :: dep
      type(package_properties) :: props

      character(len=:), allocatable :: manifest
      type(error_t), allocatable :: error
      type(toml_table), allocatable :: table

      manifest = join_path(dep%proj_dir, "fpm.toml")

      call read_package_file(table, manifest, error)
      if (allocated(error)) return

      if (.not. allocated(table)) then
          call fatal_error(error, "Unclassified error while reading: '"//manifest//"'")
          return
      end if

      call get_value(table, "description", props%description)
      call get_value(table, "homepage", props%homepage)

  end function


  ! Mask the dependencies which should remain part of the output
  function dependency_depth(tree) result(depth)
    type(tree_t), intent(in) :: tree

    ! The number of dependencies should fit in an automatic array
    ! This could potentially break with a very large dependency tree
    ! but we assume that is unlikely.
    integer :: depth(tree%ndep), k

    ! Mark all nodes as unvisitied
    depth = -1


    call bfs(tree%ndep,tree%ia,tree%ja,depth)

    ! Traverse the assigning depths to the nodes starting from the root.
    ! call dfs(tree%ndep,tree%ia,tree%ja,depth,d=0,k=1)

  contains

    ! Assign depth using a breadth-first traversal
    subroutine bfs(n,ia,ja,depth)
      integer, intent(in) :: n
      integer, intent(in) :: ia(n+1), ja(:)
      integer, intent(out) :: depth(n)

      integer :: k, j

      ! Root node
      depth(1) = 0

      ! N.b. this traversal is only correct, because of the way
      ! the fpm dependencies are resolved, meaning that neighbors
      ! have been encountered already.

      do k = 1, n
        associate(d => depth(k))
        do j = ia(k), ia(k+1)-1
          if (depth(ja(j)) < 0) depth(ja(j)) = d + 1
        end do
        end associate
      end do

    end subroutine

    !> Assign depths using a depth-first search
    !> This is less efficient because the nodes are revisited over
    !> and over again.
    recursive subroutine dfs(n,ia,ja,depth,d,k)
      integer, intent(in) :: n, k, d
      integer, intent(in) :: ia(n+1), ja(:)
      integer, intent(inout) :: depth(n)

      integer :: j

      if (depth(k) < 0) then
        depth(k) = d
      else
        depth(k) = min(depth(k), d)
      end if

      ! Search the dependencies
      do j = ia(k)+1, ia(k+1)-1
        call dfs(n,ia,ja,depth,d=d+1,k=ja(j))
      end do

    end subroutine

 end function


  !> Create a logical mask out of the user-specified exclude string
  !>
  !>   .true. means remove the dependency along with any sub-dependencies
  !>   .false. means keep the dependency
  !>
  function exclude_mask(deps,exclude) result(ex)
    use fpm_dependency, only: dependency_node_t
    type(dependency_node_t), intent(in) :: deps(:)
    character(len=*), intent(in) :: exclude  ! A comma-separated list of packages
    logical, allocatable :: ex(:)

    integer :: i

    allocate(ex(size(deps)))

    do i = 1, size(deps)
      ex(i) = index(exclude,deps(i)%name) > 0
    end do

    ! FIXME: warning about packages not found

  end function

  !> Print a command-line dependency tree up to a certain depth
  subroutine print_deps(tree, max_depth)
    type(tree_t), intent(in) :: tree
    integer, intent(in), optional :: max_depth

    logical :: visited(tree%ndep)

    ! The use of stack to track the padding is described in the
    ! following blog-post (kind thanks to Lúcás C. Meier!):
    !   https://cronokirby.com/posts/2020/12/a-unix-tree-algorithm/
    integer, parameter :: STACK_DEPTH = 100
    logical :: stack(STACK_DEPTH)
    integer :: p
    integer :: max_depth_

    max_depth_ = huge(p)
    if (present(max_depth)) max_depth_ = max_depth

    visited = .false.
    p = 0

    call dfs_print(tree%ndep,tree%ia,tree%ja,tree%dep,visited, &
        k=1,last=.true.,stack=stack,p=p)

  contains

    recursive subroutine dfs_print(n,ia,ja,dep,visited,k,last,stack,p)
      use fpm_dependency, only: dependency_node_t
      use, intrinsic :: iso_fortran_env, only: error_unit
      integer, intent(in) :: n, k
      integer, intent(in) :: ia(n+1), ja(:)
      type(dependency_node_t), intent(in) :: dep(n)
      logical, intent(inout) :: visited(n)
      logical, intent(in) :: last

      logical, intent(inout) :: stack(STACK_DEPTH)
      integer, intent(inout) :: p

      integer :: j

      if (p > max_depth_) return

      if (p > 0) then
        do j = 1, p-1
          if (stack(j)) then
            write(*,'(A)',advance='no') '    '
          else
            write(*,'(A)',advance='no') '│   '
          end if
        end do
        write(*,'(A)',advance='no') merge('└── ','├── ',last)
      end if

      if (.not. visited(k)) then
        write(*,'(A)') dep(k)%name
        visited(k) = .true.
      else
        write(*,'(A," (*)")') dep(k)%name
        return
      end if

      do j = ia(k)+1, ia(k+1)-1

        ! push
        p = p + 1
        if (p > size(stack)) then
          write(error_unit,'(*(A,/))') &
            "dfs_print: stack overflow", &
            "  plese open a bug report along with the conditions", &
            "  it occured under"
          stop 3
        end if

        ! Is the next last?
        stack(p) = j == ia(k+1)-1

        call dfs_print(n,ia,ja,dep,visited, &
            k=ja(j),last=(stack(p)),stack=stack,p=p)

        ! pop
        p = p - 1

      end do
    end subroutine

  end subroutine


  subroutine dfs_traversal(tree,node_action,edge_action)
    type(tree_t), intent(in) :: tree

    interface
      subroutine node_action(i)
        integer, intent(in) :: i
      end subroutine
      subroutine edge_action(i,j)
        integer, intent(in) :: i,j
      end subroutine
    end interface

    optional :: edge_action
    call dfs(tree%ndep,tree%ia,tree%ja,1)
  contains
    !> Assign depths using a depth-first search
    recursive subroutine dfs(n,ia,ja,i)
        integer, intent(in) :: n, i
        integer, intent(in) :: ia(n+1), ja(:)
        integer :: j
        call node_action(i)
        ! Search the dependencies
        do j = ia(i)+1, ia(i+1)-1
            if (present(edge_action)) call edge_action(i,ja(j))
            call bfs(n,ia,ja,i=ja(j))
        end do
    end subroutine
  end subroutine


  subroutine dfs_depth(tree,d)
    type(tree_t), intent(in) :: tree
    integer, intent(out) :: d(tree%ndep)

    d = -1

    call dfs_traversal(tree,node,edge)

  contains

    subroutine node(i)
      integer, intent(in) :: i
      ! Node already visited
      if (d(i) >= 0) return
      ! Root node
      if (i == 1) d(i) = 0
    end subroutine

    subroutine edge(i,j)
      integer, intent(in) :: i,j
      if (d(j) < 0) then
        ! Not visited yet, increment depth
        d(j) = d(i) + 1
      end if
    end subroutine

  end subroutine

end module
