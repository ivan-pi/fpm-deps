
program fpm_tree_main

use, intrinsic :: iso_fortran_env, only: error_unit, output_unit

use fpm_deps, only: config_t, tree_t, new_tree, &
    package_properties, dependency_props, &
    exclude_mask, print_deps, bfs_depth
use fpm_error, only: error_t

implicit none

character(len=*), parameter :: version_str = "fpm-tree version 0.1.0"

! Data structures
type(tree_t) :: tree
logical, allocatable :: mask(:), ex(:), tmp(:)
type(package_properties), allocatable :: props(:)
character(len=:), allocatable :: manifest_path

integer :: cmd_depth
logical :: cmd_deduplicate

integer :: debug_unit, nargs, blen, k
character(len=1024) :: buf
character(len=:), allocatable :: name

debug_unit = error_unit
nargs = command_argument_count()
call get_command_argument(0,buf,length=blen) ! use program name for friendly output
name = buf(1:blen)

! Default settings
cmd_depth = -1
cmd_deduplicate = .true.


! Process command-line arguments
k = 1
do while (k <= nargs)

    call get_command_argument(k,buf,length=blen)

    parse_arg: select case(buf(1:blen))
    case('--version')
        write(output_unit,'(A)') version_str
        stop
    case('-h','--help')
        call show_help
        stop 0
    case('-d','--depth')
        k = k + 1
        call get_command_argument(k,buf,length=blen)
        read(buf,*) cmd_depth
        write(debug_unit,'(A,I0)') "cmd_depth = ", cmd_depth
    case('--no-dedupe')
        cmd_deduplicate = .false.
!    case('--charset')
!    case('--prefix')
!    case('--flat')
    case default
        write(error_unit,'(A)') name//": error: invalid option -- "//buf(1:blen)
        write(error_unit,'(A)') "Try '"//name//" --help' for more information."
        stop 1
    end select parse_arg

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

    call print_deps(tree)

end block

contains

    subroutine show_help()
!    character(len=:), allocatable :: prefix
!    prefix = repeat(' ',len("Usage: "//name))

    write(output_unit,'(*(A,/))') &
"Usage: "//name//" [-h] [--version]",&
"", &
"Display a tree visualization of a package dependency graph", &
"", &
"options:", &
" -h, --help        show this help message and exit", &
" --version         show version string and exit", &
" --charset {utf8,ascii}", &
"                   chooses the character set to use for the tree.", &
"", &
"Please report any errors encountered by opening a new issue at", &
"  https://github.com/ivan-pi/fpm-deps"

    end subroutine

end program fpm_tree_main
