program fpmdeps

use, intrinsic :: iso_fortran_env, only: error_unit, output_unit

use fpm_deps, only: config_t, tree_t, new_tree
use fpm_error, only: error_t

implicit none

integer :: nargs, k, unit
character(len=:), allocatable :: name
character(len=128) :: buf

! Command-line options
logical :: cmd_meta, cmd_tooltip, cmd_mermaid
integer :: cmd_dpi, cmd_depth
character(len=:), allocatable :: outfile, manifest_path, cmd_url

! FIXME: check any environment variables of interest

nargs = command_argument_count()
call get_command_argument(0,buf) ! use program name for friendly output
name = trim(buf)

! Default settings
cmd_meta = .true.
cmd_tooltip = .true.
cmd_mermaid = .false.
cmd_dpi = -1
cmd_depth = -1
outfile = '-'           ! Standard output

! Process command-line arguments
k = 1
do while (k <= nargs)

    call get_command_argument(k,buf)
    ! FIXME: graceful exit on error
    print *, k, trim(buf)

    select case(trim(buf))
    case('-o','--output')
        k = k + 1
        call get_command_argument(k,buf)
        outfile = trim(buf)
        print *, "outfile = "//outfile
    case('--manifest-path')
        k = k + 1
        call get_command_argument(k,buf)
        manifest_path = trim(buf)
        print *, "manifest_path = "//manifest_path
    case('--dpi')
        k = k + 1
        call get_command_argument(k,buf)
        read(buf,*) cmd_dpi
        print *, "cmd_dpi = ", cmd_dpi
    case('-d','--depth')
        k = k + 1
        call get_command_argument(k,buf)
        read(buf,*) cmd_depth
        if (cmd_depth < 0) then
            write(error_unit,'(A)') name//": error: --depth must be non-negative"
            stop 1
        end if
        print *, "cmd_depth = ", cmd_depth
    case('-M','--mermaid')
        cmd_mermaid = .true.
    case('--filter')
        write(error_unit,'(A)') "warning: --filter will be ignored"
    case('--exclude')
        write(error_unit,'(A)') "warning: --exclude will be ignored"
    case('--meta')
        continue
    case('--no-meta')
        cmd_meta = .false.
    case('--no-tooltip')
        cmd_tooltip = .false.
    case('--url')
        k = k + 1
        call get_command_argument(k,buf)
        cmd_url = trim(buf)
        select case(cmd_url)
        case('homepage','dir','git')
            continue
        case default
            write(error_unit,'(A)') "Unknown option for --url "//cmd_url
            write(error_unit,'(A)') "Valid options include homepage,dir,git."
            stop 1
        end select
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
    type(tree_t) :: tree
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
    if (allocated(err)) error stop "building dependency tree failed"

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
        call print_mermaid(unit,package%name,tree, &
            url=allocated(cmd_url), &
            tooltip=cmd_tooltip)
    else
        call print_graphviz(unit,package%name,tree, &
            url=allocated(cmd_url), &
            tooltip=cmd_tooltip, &
            dpi=cmd_dpi)
    end if

    ! FIXME: flushed by default if open, can be removed
    if (unit /= output_unit) close(unit)

end block


contains

    subroutine show_help()

    character(len=:), allocatable :: prefix
    prefix = repeat(' ',len("Usage: "//name))

    write(output_unit,'(*(A,/))') &
"Usage: "//name//" [-h] [--version] [-o OUTPUT] [--manifest_path PATH]", &
prefix//" [-a] [--depth D] [--no-meta] [--filter ...] [--exclude ...]", &
prefix//" [--mermaid] [--dpi DPI] [--url {homepage,dir,git}] [--no-tooltip]", &
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
" -a, --all         show all dependencies, including app, test, and examples", &
" --no-meta         ignore meta-dependencies in dependency graph", &
" -M, --mermaid     output graph using Mermaid flowchart syntax", &
" --dpi <int>       dots-per-inch; useful when piping dot for bitmap output", &
" --url {homepage,dir,git}", &
"                   add hyper-links to the graph nodes", &
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


    ! Output dependency graph using the Graphviz DOT language
    ! An overview of the syntax can be found at
    !     https://graphviz.org/doc/info/lang.html
    subroutine print_graphviz(unit, name, tree, url, tooltip, dpi)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: url, tooltip
        integer, intent(in) :: dpi

        integer :: i, j, k

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "strict digraph "//name//" {"
        write(unit,'(2X,A)') 'fontname = "Helvetica,Arial,sans-serif"'
        if (dpi > 0) write(unit,'(2X,"graph [ dpi = ",I0," ]")') dpi
        write(unit,'(2X,A)') "graph [ root = N1 ]"

        ! Nodes
        do i = 1, n_nodes
            write(unit,'(2X,"N",I0,A)') i,'[ label = "'//dep(i)%name//'"]'
        end do

        ! Edges
        associate(ia => tree%ia, ja => tree%ja)
        do k = 1, size(ia)-1
                do j = ia(k)+1, ia(k+1)-1
                    write(unit,'(2X,"N",I0,"->","N",I0)') k, ja(j)
                end do
        end do
        end associate

        write(unit,'(A)') "}"

        end associate

    end subroutine

    ! Output dependency graph using Mermaid flowchart syntax
    ! An overview of the syntax can be found at
    !     https://mermaid.js.org/syntax/flowchart.html
    subroutine print_mermaid(unit,name,tree,url,tooltip)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: name
        type(tree_t), intent(in) :: tree
        logical, intent(in) :: url, tooltip

        integer :: i, j, k

        associate(n_nodes => size(tree%dep), dep => tree%dep)

        ! Preamble
        write(unit,'(A)') "flowchart TB"

        ! Nodes
        do i = 1, n_nodes
            write(unit,'(2X,"N",I0,A)') i,"["//dep(i)%name//"]"
        end do

        ! Edges
        associate(ia => tree%ia, ja => tree%ja)
        do k = 1, size(ia)-1
            do j = ia(k)+1, ia(k+1)-1
                write(unit,'(2X,"N",I0,"-->","N",I0)') k, ja(j)
            end do
        end do
        end associate

        end associate

    end subroutine

end program fpmdeps
