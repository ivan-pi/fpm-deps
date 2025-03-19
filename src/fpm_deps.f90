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
contains
    ! New method
    procedure, private :: resolve_with_graph => resolve_dependency_with_graph

    ! Override
    procedure, private :: resolve_dependencies => resolve_dependencies
end type


!> Common output format for writing to the command line
character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

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

end module
