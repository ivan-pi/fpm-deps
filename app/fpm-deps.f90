program fpmdeps

use, intrinsic :: iso_fortran_env, only: error_unit, output_unit

use fpm_deps, only: config_t, tree_t, new_tree, &
    package_properties, dependency_props, &
    exclude_mask
use fpm_error, only: error_t

implicit none

integer :: nargs, k, unit, debug_unit
character(len=:), allocatable :: name
character(len=128) :: buf

! Command-line options
logical :: cmd_meta, cmd_tooltip, cmd_mermaid, cmd_url, cmd_html
integer :: cmd_dpi, cmd_depth
character(len=:), allocatable :: outfile, manifest_path, exclude

! Data structures
type(tree_t) :: tree
logical, allocatable :: mask(:), ex(:), tmp(:)
type(package_properties), allocatable :: props(:)


! FIXME: check any environment variables of interest

debug_unit = error_unit
nargs = command_argument_count()
call get_command_argument(0,buf) ! use program name for friendly output
name = trim(buf)

! Default settings
cmd_meta = .true.
cmd_tooltip = .true.
cmd_url = .true.
cmd_mermaid = .false.
cmd_html = .false.
cmd_dpi = -1
cmd_depth = -1
outfile = '-'           ! Standard output

! Process command-line arguments
k = 1
do while (k <= nargs)

    call get_command_argument(k,buf)
    ! FIXME: graceful exit on error

    select case(trim(buf))
    case('-o','--output')
        k = k + 1
        call get_command_argument(k,buf)
        outfile = trim(buf)
        write(debug_unit,'(A)') "outfile = "//outfile
    case('--manifest-path')
        k = k + 1
        call get_command_argument(k,buf)
        manifest_path = trim(buf)
        write(debug_unit,'(A)') "manifest_path = "//manifest_path
    case('--dpi')
        k = k + 1
        call get_command_argument(k,buf)
        read(buf,*) cmd_dpi
        write(debug_unit,'(A)') "cmd_dpi = ", cmd_dpi
    case('-d','--depth')
        k = k + 1
        call get_command_argument(k,buf)
        read(buf,*) cmd_depth
        if (cmd_depth < 0) then
            write(error_unit,'(A)') name//": error: depth must be non-negative"
            stop 1
        end if
        write(debug_unit,'(A,I0)') "cmd_depth = ", cmd_depth
    case('-M','--mermaid')
        cmd_mermaid = .true.
    case('--html')
        cmd_html = .true.
    case('--filter')
        write(error_unit,'(A)') "warning: --filter will be ignored"
    case('--exclude')
! FIXME: not very robust currently, the excluded packages should
!        be quoted to become one string.
        k = k + 1
        call get_command_argument(k,buf)
        exclude = trim(buf)
        !print *, exclude
    case('--meta')
        continue
    case('--no-meta')
        cmd_meta = .false.
    case('--no-tooltip')
        cmd_tooltip = .false.
    case('--no-url')
        cmd_url = .false.
    case('-h','--help')
        call show_help
        stop 0
    case('--version')
        write(output_unit,'(A)') "fpm-deps version 0.1.0"
        stop
    case default
        write(error_unit,'(A)') name//": error: invalid option -- "//trim(buf)
        write(error_unit,'(A)') "Try '"//name//" --help' for more information."
        stop 1
    end select

    k = k + 1
end do

block

    type(config_t) :: package
    type(error_t), allocatable :: err
    integer :: i


    if (.not. allocated(manifest_path)) manifest_path = 'fpm.toml'
    call package%init_from_file(manifest_path)

    ! These are the direct (i.e. first level) dependencies
    !print *, "Dependencies for package "//package%name
    !do i = 1, size(package%dependency)
    !    print *, '  '//package%dependency(i)%name, &
    !        allocated(package%dependency(i)%path), &
    !        allocated(package%dependency(i)%git)
    !end do

    ! To get the dependencies deeper in the tree we have to resolve
    ! them first. The tree can handle this for us.

    call new_tree(tree)
    !call tree%add(package%dependency(:),err)
    call tree%add(package%package_config_t,err)
    if (allocated(err)) then
        write(error_unit,'(A)') err%message
        error stop "building dependency tree failed"
    end if
    !tree%ndeps = size(tree%dep)

    ! Mask nodes based on depth
    allocate(mask(size(tree%dep)))
    if (cmd_depth > 0) then
        call recurse(tree,cmd_depth,mask)
    else
        mask = .true.
    end if

    ! Mask nodes for exclusion
    if (allocated(exclude)) then
        ex = .not. exclude_mask(tree%dep,exclude)

        allocate(tmp,mold=ex)
        call combine_masks(tree,mask,ex,tmp)
        mask = tmp

! FIXME: for some reason I need to create a temporary array as above?
!        call combine_masks(tree,(mask),ex,mask)

!        do i = 1, tree%ndep
!            print *, i, mask(i), ex(i), tmp(i), tree%dep(i)%name
!        end do
    end if

    ! Gather dependency package attributes used for URL and tooltip annotation
    if (cmd_tooltip .or. cmd_url) then
        props = dependency_props(tree%dep,mask)
    end if

    !print *, "Number of dependencies", tree%ndep
    !print *, "Dependencies are stored in ", tree%dep_dir
    !do i = 1, size(tree%dep)
    !    print *, tree%dep(i)%name, tree%dep(i)%ndeps
    !    if (allocated(tree%dep(i)%version)) then
    !        print *, tree%dep(i)%name//'@'//tree%dep(i)%version%s()
    !        print *, tree%dep(i)%proj_dir
    !    else
    !        print *, i, tree%dep(i)%name
    !    end if
    !end do

    !print *, "nedges = ", sum(tree%dep%ndeps) + size(tree%dep)
    !print *, "has ia ", allocated(tree%ia)
    !print *, "has ja ", allocated(tree%ja)
    !print *, tree%ia
    !print *, tree%ja

    ! ------- OUTPUT ---------

    ! Pick output unit
    if (outfile == '-') then
        unit = output_unit
    else
        open(newunit=unit, file=outfile, action='write', status='replace')
    end if

    if (cmd_mermaid) then
        if (cmd_html) then
            block
                use fpmdeps_html
                call print_mermaid_html(unit,package%name,html_callback)
            end block
        else
            call print_mermaid(unit,package%name,tree,mask,props, &
                url=cmd_url, &
                tooltip=cmd_tooltip)
        end if
    else
        call print_graphviz(unit,package%name,tree,mask,props, &
            url=cmd_url, &
            tooltip=cmd_tooltip, &
            dpi=cmd_dpi)
    end if

    ! FIXME: flushed by default if open, can be removed
    if (unit /= output_unit) close(unit)

end block


contains

    subroutine html_callback(html_unit,name)
    !    import mask, props, cmd_url, cmd_tooltip
        integer, intent(in) :: html_unit
        character(len=*), intent(in) :: name
! FIXME: hacky way to pass properties via host association
        call print_mermaid(html_unit,name,tree,mask,props, &
            url=cmd_url, &
            tooltip=cmd_tooltip)
    end subroutine

    subroutine show_help()

    character(len=:), allocatable :: prefix
    prefix = repeat(' ',len("Usage: "//name))

    write(output_unit,'(*(A,/))') &
"Usage: "//name//" [-h] [--version] [-o OUTPUT] [--manifest_path PATH]", &
prefix//" [--depth D] [--no-meta] [--exclude <comma_separated_list>]", &
prefix//" [--mermaid] [--dpi DPI] [--no-url] [--no-tooltip]", &
"", &
"Create a fpm project dependency graph in DOT language", &
"", &
"options:", &
" -h, --help        show this help message and exit", &
" --version         show version string and exit", &
" -o <file>, --output <file>", &
"                   send graph output to a file instead of stdout", &
" --manifest_path <toml-file>", &
"                   path to a valid fpm toml file; by default we look", &
"                   in the current directory", &
" -d, --depth <d>   dependency depth to consider during output; --depth 1", &
"                   will show only direct dependencies. use --depth 0 to", &
"                   obtain the full depth", &
!" -a, --all         show all dependencies, including app, test, and examples", &
" --no-meta         ignore meta-dependencies in dependency graph", &
" --exclude <comma_separated_list>", &
"                   a list of packages to be excluded from the graph. use", &
"                   of quotes is necessary for correct parsing", &
" -M, --mermaid     output graph using Mermaid flowchart syntax", &
" --dpi <int>       dots-per-inch; useful when piping dot for bitmap output", &
" --no-url          do not add the homepage URL to the nodes", &
" --no-tooltip      add package description as tooltip; useful when converted", &
"                   to SVG using dot or in case of Mermaid output", &
"", &
"By default output is written to standard output and can be processed", &
"using the dot command. Example:", &
"", &
"   > fpmdeps --dpi 96 | dot -Tpng -ograph.png ", &
"   > fpmdeps -o graph.gv", &
"", &
"Please report any errors encountered by opening a new issue at", &
"  https://github.com/ivan-pi/fpmdeps"

    end subroutine

    subroutine print_dep(tree)
        type(tree_t), intent(in) :: tree
        integer :: i
        do i = 1, tree%ndep
            associate(dep => tree%dep(i))
            !print *, i, allocated(dep%dependency), allocated(dep%dev_dependency), dep%name
            end associate
        end do
        stop

    end subroutine





    ! Output dependency graph using the Graphviz DOT language
    ! An overview of the syntax can be found at
    !     https://graphviz.org/doc/info/lang.html
    subroutine print_graphviz(unit, name, tree, mask, props, url, tooltip, dpi)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: mask(:)
        type(package_properties), intent(in) :: props(:)
        logical, intent(in) :: url, tooltip
        integer, intent(in) :: dpi

        integer :: i, j, k

        character(len=*), parameter :: fmt = '(2X,"N",I0,"[ ",A," ]")'

        character(len=:), allocatable :: tt
        character(len=:), allocatable :: attr

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "strict digraph "//qt(name)//" {"
        write(unit,'(2X,A)') 'node [ fontname = "Helvetica,Arial,sans-serif" ]'
        write(unit,'(2X,A)') 'edge [ fontname = "Helvetica,Arial,sans-serif" ]'
        if (dpi > 0) write(unit,'(2X,"graph [ dpi = ",I0," ]")') dpi
        write(unit,'(2X,A)') "graph [ root = N1 ]"

        ! Nodes
        do i = 1, n_nodes
! FIXME: URL
            if (.not. mask(i)) cycle

            attr = "label = "//qt(dep(i)%name)
            if (tooltip) then
                if (allocated(props(i)%description)) then
                    attr = attr//", tooltip = "//qt(props(i)%description)
                else
                    attr = attr//", tooltip = "//qt("Description unavailable")
                end if
            end if
            if (url) then
                if (allocated(props(i)%homepage)) then
                    attr = attr//", URL = "//qt(props(i)%homepage)
                end if
            end if

            write(unit,fmt) i, attr
        end do

        ! Edges
        associate(ia => tree%ia, ja => tree%ja)
        do k = 1, size(ia)-1
            if (.not. mask(k)) cycle
            do j = ia(k)+1, ia(k+1)-1
                if (.not. mask(ja(j))) cycle
                write(unit,'(2X,"N",I0,"->","N",I0)') k, ja(j)
            end do
        end do
        end associate

        write(unit,'(A)') "}"

        end associate

    end subroutine

    pure function qt(str)
        character(len=*), intent(in) :: str
        character(len=len(str)+2) :: qt
        qt = '"'//str//'"'
    end function

    ! Output dependency graph using Mermaid flowchart syntax
    ! An overview of the syntax can be found at
    !     https://mermaid.js.org/syntax/flowchart.html
    subroutine print_mermaid(unit,name,tree,mask,props,url,tooltip)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: mask(:)
        type(package_properties), intent(in) :: props(:)
        logical, intent(in) :: url, tooltip

        integer :: i, j, k
        character(len=:), allocatable :: attr

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "flowchart TB"

        ! Nodes
        do i = 1, n_nodes
            if (.not. mask(i)) cycle
            write(unit,'(2X,"N",I0,A)') i,"["//dep(i)%name//"]"
        end do

        ! Click URL links
        do i = 1, n_nodes
            if (.not. mask(i)) cycle

            attr = ''
            if (url) then
                if (allocated(props(i)%homepage)) then
                    attr = "href "//qt(props(i)%homepage)
                else
                    ! No click possible without homepage
                    cycle
                end if
            end if
            ! Mermaid expects to have both
            if (url .and. tooltip) then
                if (allocated(props(i)%description)) then
                    attr = attr//" "//qt(props(i)%description)
                end if
            end if

            write(unit,'(2X,"click N",I0,1x,A)') i, attr

        end do


! FIXME: tooltip

        ! Edges
        associate(ia => tree%ia, ja => tree%ja)
        do k = 1, size(ia)-1
            if (.not. mask(k)) cycle
            do j = ia(k)+1, ia(k+1)-1
                if (.not. mask(ja(j))) cycle
                write(unit,'(2X,"N",I0,"-->","N",I0)') k, ja(j)
            end do
        end do
        end associate

        end associate

    end subroutine


    ! Mask the dependencies which should remain part of the output
    subroutine recurse(tree, max_depth, mask, ex)
        type(tree_t), intent(in) :: tree
        integer, intent(in) :: max_depth
        logical, intent(out) :: mask(tree%ndep)
        logical, intent(in), optional :: ex(tree%ndep)

        ! The number of dependencies should fit in an automatic array
        ! This could potentially break with a very large dependency tree
        ! but we assume that is unlikely.
        integer :: d(tree%ndep), k

        ! Mark all nodes as unvisitied
        d = -1

        ! Set the depth of the root node
        d(1) = 1

        ! Traverse the assigning depths to the nodes starting from the root.
        call bfs(tree%ndep,tree%ia,tree%ja,d,1)

        mask = d <= max_depth

    end subroutine

    !> Assign depths using a breadth-first search
    recursive subroutine bfs(n,ia,ja,depth,k)
        integer, intent(in) :: n, k
        integer, intent(in) :: ia(n+1), ja(:)
        integer, intent(inout) :: depth(n)

        integer :: j

        ! Search the dependencies
        do j = ia(k)+1, ia(k+1)-1
            if (depth(ja(j)) < 0) then
                ! If the node has not been visited yet assign the
                ! depth and continue the search
                depth(ja(j)) = depth(k) + 1
                if (j <= n) call bfs(n,ia,ja,depth,k=j)
            end if
        end do

    end subroutine


    !> Combine the depth mask and the excluded mask
    !>
    !>   A bread-first search must be used to correctly propagate the
    !>   the mask values.
    !>
    subroutine combine_masks(tree,depth,ex,c)
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: depth(tree%ndep), ex(tree%ndep)
        logical, intent(out) :: c(tree%ndep)

        c(1) = .true.
        call bfs_mask(tree%ndep,tree%ia,tree%ja,depth,ex,c,1)

    end subroutine

    !> Propagate mask values to dependencies
    recursive subroutine bfs_mask(n,ia,ja,d,ex,c,k)
        integer, intent(in) :: n, k
        integer, intent(in) :: ia(n+1), ja(:)
        logical, intent(in) :: d(n), ex(n)
        logical, intent(inout) :: c(n)

        integer :: j

        ! Search the dependencies
        do j = ia(k)+1, ia(k+1)-1
            if (d(ja(j)) .eqv. ex(ja(j))) then
                c(ja(j)) = .true.
                if (j <= n) call bfs_mask(n,ia,ja,d,ex,c,k=j)
            end if
        end do

    end subroutine

end program fpmdeps
