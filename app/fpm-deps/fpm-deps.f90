program fpm_deps_main

use, intrinsic :: iso_fortran_env, only: error_unit, output_unit

use fpm_deps, only: config_t, tree_t, new_tree, &
    package_properties, dependency_props, dependency_depth, &
    exclude_mask, print_deps, bfs_depth
use fpm_error, only: error_t

implicit none

character(len=*), parameter :: version_str = "fpm-deps version 0.2.1"

integer :: nargs, k, unit, debug_unit
character(len=:), allocatable :: name
character(len=128) :: buf

! Command-line options
logical :: cmd_meta, cmd_tooltip, cmd_mermaid, cmd_url, cmd_html
integer :: cmd_dpi, cmd_depth
character(len=2) :: cmd_rankdir
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
cmd_rankdir = 'TB'
cmd_meta = .true.           ! unused
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

    parse_arg: select case(trim(buf))
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
        write(debug_unit,'(A,I0)') "cmd_depth = ", cmd_depth
    case('-M','--mermaid')
        cmd_mermaid = .true.
        if (k < nargs) then
            call get_command_argument(k+1,buf)
            select case(trim(buf))
            case('md','markdown')
                k = k + 1
            case('html')
                cmd_html = .true.
                k = k + 1
            case default
                if (buf(1:1) == '-') then
                    ! Next item looks like an argument
                    exit parse_arg
                end if
                write(error_unit,'(A)') &
                    name//": error: "//trim(buf)//" is not a valid option for --mermaid (-M)"
                stop 1
            end select
        end if
    case('--rankdir')
        k = k + 1
        call get_command_argument(k,buf)
        select case(trim(buf))
        case('TB','BT','LR','RL')
            cmd_rankdir = buf(1:2)
        case('TD')
            ! Mermaid allows this as an extra option
            cmd_rankdir = 'TB'
        case default
            write(error_unit,'(A)') &
                name//": error: '"//trim(buf)//"' is not a valid --rankdir {TB,BT,LR,RL}"
            stop 1
        end select
    case('--filter')
        write(error_unit,'(A)') "warning: --filter will be ignored"
    case('--exclude')
! FIXME: not very robust currently, the excluded packages should
!        be quoted to become one string.
        k = k + 1
        call get_command_argument(k,buf)
        exclude = trim(buf)
        !print *, exclude
!    case('--no-meta')
!        cmd_meta = .false.
    case('--no-tooltip')
        cmd_tooltip = .false.
    case('--no-url')
        cmd_url = .false.
    case('-h','--help')
        call show_help
        stop 0
    case('--version')
        write(output_unit,'(A)') version_str
        stop
    case default
        write(error_unit,'(A)') name//": error: invalid option -- "//trim(buf)
        write(error_unit,'(A)') "Try '"//name//" --help' for more information."
        stop 1
    end select parse_arg
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

    ! Mask nodes based on depth
    allocate(mask(size(tree%dep)))
    if (cmd_depth < 0) then
        mask = .true.
    else
        mask = dependency_depth(tree) <= cmd_depth
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
                tooltip=cmd_tooltip, &
                rankdir=cmd_rankdir)
        end if
    else
        call print_graphviz(unit,package%name,tree,mask,props, &
            url=cmd_url, &
            tooltip=cmd_tooltip, &
            dpi=cmd_dpi, &
            rankdir=cmd_rankdir)
    end if

end block


contains

    subroutine html_callback(html_unit,name)
    !    import mask, props, cmd_url, cmd_tooltip
        integer, intent(in) :: html_unit
        character(len=*), intent(in) :: name
! FIXME: hacky way to pass properties via host association
        call print_mermaid(html_unit,name,tree,mask,props, &
            url=cmd_url, &
            tooltip=cmd_tooltip, &
            rankdir=cmd_rankdir)
    end subroutine

    subroutine show_help()

    character(len=:), allocatable :: prefix
    prefix = repeat(' ',len("Usage: "//name))

    write(output_unit,'(*(A,/))') &
"Create a fpm project dependency graph in DOT language", &
"", &
"Usage: "//name//" [-h] [--version] [-o OUTPUT] [--manifest_path PATH]", &
prefix//" [--depth DEPTH] [--exclude <LIST>]", &
prefix//" [--mermaid [{md|html}]] [--dpi DPI] [--no-url] [--no-tooltip]", &
prefix//" [--rankdir {TB,BT,LR,RL}]", &
"", &
"options:", &
" -h, --help            show this help message and exit", &
"     --version         show version string and exit", &
" -o, --output <FILE>   send graph output to a file instead of stdout", &
"     --manifest-path <PATH>", &
"                       path to a valid fpm toml file; by default we look", &
"                       in the current directory", &
" -d, --depth <DEPTH>       dependency depth to consider during output; if negative", &
"                       show the full dependency tree. use --depth 1 to show the", &
"                       direct dependencies", &
!" -a, --all         show all dependencies, including app, test, and examples", &
!" --no-meta         ignore meta-dependencies in dependency graph", &
"     --exclude <LIST>  a comma-separated list of packages to be excluded from the graph. use", &
"                       of quotes is necessary for correct parsing", &
" -M, --mermaid [<FORMAT>]", &
"                       output graph using Mermaid flowchart syntax suitable for",&
"                       includion in markdown (md) documents; if html is selected",&
"                       create a standalone HTML document", &
"     --dpi <DPI>       dots-per-inch; useful when piping dot for bitmap output", &
"     --no-url          do not add the homepage URL to the nodes", &
"     --no-tooltip      add package description as tooltip; useful when converted", &
"                       to SVG using dot or in case of Mermaid output", &
"     --rankdir <RANKDIR>", &
"                       direction of the graph layout: TB, BT, LR, RL [default: TB]", &
"", &
"By default output is written to standard output and can be processed", &
"using the dot command. Example:", &
"", &
"   > fpmdeps --dpi 96 | dot -Tpng -ograph.png ", &
"   > fpmdeps -o graph.gv", &
"", &
"Please report any errors encountered by opening a new issue at", &
"  https://github.com/ivan-pi/fpm-deps"

    end subroutine


    ! Output dependency graph using the Graphviz DOT language
    ! An overview of the syntax can be found at
    !     https://graphviz.org/doc/info/lang.html
    subroutine print_graphviz(unit, name, tree, mask, props, url, tooltip, &
            dpi, rankdir)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: mask(:)
        type(package_properties), intent(in) :: props(:)
        logical, intent(in) :: url, tooltip
        integer, intent(in) :: dpi
        character(len=2), intent(in) :: rankdir

        integer :: i, j, k

        character(len=*), parameter :: fmt = '(2X,"N",I0,"[ ",A," ]")'

        character(len=:), allocatable :: tt
        character(len=:), allocatable :: attr

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "strict digraph "//qt(name)//" {"
        write(unit,'(2X,A)') 'rankdir="'//rankdir//'"'
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
    subroutine print_mermaid(unit,name,tree,mask,props,url,tooltip,rankdir)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: mask(:)
        type(package_properties), intent(in) :: props(:)
        logical, intent(in) :: url, tooltip
        character(len=2), intent(in) :: rankdir

        integer :: i, j, k
        character(len=:), allocatable :: attr

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "flowchart "//rankdir

        ! Nodes
        do i = 1, n_nodes
            if (.not. mask(i)) cycle
            write(unit,'(2X,"N",I0,A)') i,"["//dep(i)%name//"]"
        end do

        ! Click URL links
        do i = 1, n_nodes
            if (.not. mask(i)) cycle

            attr = 'href '
            if (url) then
                if (allocated(props(i)%homepage)) then
                    attr = attr//qt(props(i)%homepage)
                else
                    ! No click possible without homepage
                    cycle
                end if
                if (tooltip) then
                    if (allocated(props(i)%description)) then
                        attr = attr//" "//qt(props(i)%description)
                    end if
                end if
            end if

            write(unit,'(2X,"click N",I0,1x,A)') i, attr

        end do

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


    !> Combine the depth mask and the excluded mask
    !>
    !>   A breadth-first search must be used to correctly propagate the
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
                call bfs_mask(n,ia,ja,d,ex,c,k=ja(j))
            end if
        end do

    end subroutine

end program fpm_deps_main
