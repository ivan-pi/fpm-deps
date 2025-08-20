
program fpm_tree_main

use, intrinsic :: iso_fortran_env, only: error_unit, output_unit

use fpm_deps, only: config_t, tree_t, new_tree, &
    package_properties, dependency_props, &
    exclude_mask, print_deps, dependency_depth
use fpm_error, only: error_t

implicit none

character(len=*), parameter :: version_str = "fpm-tree version 0.1.0"

! Data structures
type(tree_t) :: tree
logical, allocatable :: mask(:), ex(:), tmp(:)
type(package_properties), allocatable :: props(:)
character(len=:), allocatable :: manifest_path

integer :: cmd_depth, cmd_prefix
logical :: cmd_deduplicate, cmd_utf8

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
cmd_utf8 = .true.

cmd_prefix = 0   ! 0 - indent, 1 - depth, 2 - none


! Process command-line arguments
k = 1
do while (k <= nargs)

    call get_command_argument(k,buf,length=blen)
!    print *, "parsing ", k, ", buf = ", trim(buf)

    parse_arg: select case(trim(buf))
    case('--version')
        write(output_unit,'(A)') version_str
        stop
    case('-h','--help')
        call show_help
        stop 0
    case('--depth')
        k = k + 1
        call get_command_argument(k,buf,length=blen)
        read(buf,*) cmd_depth
        write(debug_unit,'(A,I0)') "cmd_depth = ", cmd_depth
    case('--no-dedupe')
        cmd_deduplicate = .false.
!    case('-e','--edges')
    case('--charset')
        k = k + 1
        call get_command_argument(k,buf,length=blen)
        select case(trim(buf))
        case('utf8')
            cmd_utf8 = .true.
        case('ascii')
            cmd_utf8 = .false.
        case default
            write(error_unit,'(A)') name//": error: invalid option for --format. Accepted values include 'utf8' and 'ascii'"
            stop 1
        end select
    case('--prefix')
        k = k + 1
        call get_command_argument(k,buf,length=blen)
        select case(trim(buf))
        case('indent')
            cmd_prefix = 0
        case('depth')
            cmd_prefix = 1
        case('none')
            cmd_prefix = 2
        case default
            write(error_unit,'(A)') name//": error: invalid option for --prefix. Accepted values include 'indent', 'depth', and 'none'."
            stop 1
        end select
    case('--flat')
        ! This is a shortcut for --prefix none
        cmd_prefix = 2
    case default
        write(error_unit,'(A)') name//": error: invalid option -- "//buf(1:blen)
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


    function likely_supports_utf8() result(utf8)

! Do modern terminals generally render all utf-8 characters correctly?
! https://stackoverflow.com/questions/32928589/do-modern-terminals-generally-render-all-utf-8-characters-correctly?noredirect=1&lq=1

! Reasonably determine whether terminal can correctly render UTF-8 characters in C?
! https://stackoverflow.com/questions/77621760/reasonably-determine-whether-terminal-can-correctly-render-utf-8-characters-in-c

! How to find out if a terminal supports UTF-8
! https://serverfault.com/questions/13898/how-to-find-out-if-a-terminal-supports-utf-8

! Determine if the term/console supports UTF8?
! https://www.reddit.com/r/bash/comments/wfbf3w/determine_if_the_termconsole_supports_utf8/

! Detect how much of Unicode my terminal supports, even through screen
! https://unix.stackexchange.com/questions/184345/detect-how-much-of-unicode-my-terminal-supports-even-through-screen

! Is there a way to determine Unicode support status in a terminal emulator?
! https://www.reddit.com/r/commandline/comments/vrflbt/is_there_a_way_to_determine_unicode_support/

        character(len=80) :: str
        integer :: strlen, stat
        logical :: utf8

        call get_environment_variable('LC_ALL',str,strlen,status=stat)
        if (stat /= 0) call get_environment_variable('LC_CYTPE',str,strlen,status=stat)
        if (stat /= 0) call get_environment_variable('LANG',str,strlen,status=stat)

        utf8 = index(str(1:strlen),'UTF-8') > 0

        call get_environment_variable('TERM',str,strlen,status=stat)
        if (stat == 0) then
            select case(str(1:strlen))
            case('xterm','xterm-256color')
                utf8 = .true.
            case default
                utf8 = .false.
            end select
        end if
    end function

end program fpm_tree_main
