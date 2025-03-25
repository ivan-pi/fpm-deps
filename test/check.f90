program check
use fpm_deps, only: tree_t, dependency_depth
implicit none

!
! dependency_depth
!
call fpm_deps_graph
call fpm_graph
call diamond_graph
call diamond_graph_2
call pyramid_graph
call matcha_graph
call dftd4_fit_graph
call forsolver_graph
call depth6_graph
call depth3_graph
call halo_graph
call simple_cycle_graph
call tricycle_graph

!
! exlude mask
!
call test_exclude_mask

print *, "Tests PASSED"

contains

  subroutine do_check(depth,expected,test)
    integer, intent(in) :: depth(:), expected(:)
    character(len=*), intent(in) :: test
    if (any(depth /= expected)) then
      print '(A,*(I3))', "got:      ", depth
      print '(A,*(I3))', "expected: ", expected
      error stop test
    end if
  end subroutine


  subroutine fpm_deps_graph

    ! This project (as on 23.03.2025)

    ! 1 fpm-deps
    ! 2   fpm
    ! 3   toml-f
    ! 4   M_CLI2
    ! 5   fortran-regex
    ! 6   jonquil
    !       toml-f (*)
    ! 7   fortran-shlex

    type(tree_t) :: tree

    tree%ndep = 7
    tree%ia = [ 1, 3, 9, 10, 11, 12, 14, 15]
    tree%ja = [ 1, 2, &
                   2, 3, 4, 5, 6, 7, &
                      3, &
                         4, &
                            5, &
                              6, 3, &    ! May break if sorting is introduced
                                 7 ]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,2,2,2,2,2])
      call do_check(depth,expected,"fpm_deps_graph")
    end associate

  end subroutine


  subroutine fpm_graph

    ! FPM Graph (as on 23.03.2025)
    ! https://github.com/fortran-lang/fpm

    ! 1   fpm
    ! 2   toml-f
    ! 3   M_CLI2
    ! 4   fortran-regex
    ! 5   jonquil
    !       toml-f (*)
    ! 6   fortran-shlex

    type(tree_t) :: tree

    tree%ndep = 6
    tree%ia = [ 1, 7, 8, 9, 10, 12, 13]
    tree%ja = [ 1, 2, 3, 4, 5, 6, &
                   2,             &
                      3,          &
                         4,       &
                            5, 3, & ! May break if sorting is introduced
                               6  ]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,1,1,1])
      call do_check(depth,expected,"fpm_graph")
    end associate

  end subroutine


  subroutine diamond_graph

    !     1
    !   /   \
    !  2     3
    !   \   /
    !     4

    type(tree_t) :: tree

    tree%ndep = 4

    tree%ia = [ 1, 4, 6, 8, 9]
    tree%ja = [ 1, 2, 3,    &
                   2,    4, &
                      3, 4, &
                         4 ]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,2])
      call do_check(depth,expected,"diamond_graph")
    end associate

  end subroutine


  subroutine diamond_graph_2

    !     1
    !   /   \
    !  2     3
    !   \   / \
    !     4    5

    type(tree_t) :: tree

    tree%ndep = 5

    tree%ia = [ 1, 4, 6, 9, 10, 11]
    tree%ja = [ 1, 2, 3,       &
                   2,    4,    &
                      3, 4, 5, &
                         4,    &
                            5 ]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,2,2])
      call do_check(depth,expected,"diamond_graph_2")
    end associate
  end subroutine


  subroutine pyramid_graph

    !       1
    !     /   \
    !    2 --> 3
    !   / \   / \
    ! 4 --> 5 --> 6

    type(tree_t) :: tree

    tree%ndep = 6

    tree%ia = [ 1, 4, 8, 11, 13, 15, 16]
    tree%ja = [ 1, 2, 3, &
                2, 3, 4, 5, &
                3, 5, 6, &
                4, 5, &
                5, 6, &
                6]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,2,2,2])
      call do_check(depth,expected,"pyramid_graph")
    end associate
  end subroutine


  subroutine matcha_graph

    ! Matcha project (as on 23.03.2025)
    ! https://github.com/BerkeleyLab/matcha

    ! 1 matcha
    ! 2   assert
    ! 3   julienne
    !       assert (*)
    ! 4   sourcery
    !       assert (*)

    type(tree_t) :: tree

    tree%ndep = 4

    tree%ia = [ 1, 5, 6, 8, 10]
    tree%ja = [ 1, 2, 3, 4, &
                2, &
                3, 2, &
                4, 2]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,1])
      call do_check(depth,expected,"matcha_graph")
    end associate

  end subroutine


  subroutine dftd4_fit_graph

    ! Matcha project (as on 23.03.2025)
    ! https://github.com/dftd4/dftd4-fit

    ! 1 dftd4-fit
    ! 2   mctc-lib
    ! 3     json-fortran
    ! 4   dftd4
    !       mctc-lib (*)
    ! 5     multicharge
    !         mctc-lib (*)
    ! 6   nlopt-f
    ! 7   minpack

    type(tree_t) :: tree

    tree%ndep = 7

    tree%ia = [ 1, 6, 8, 9, 12, 14, 15, 16]
    tree%ja = [ 1, 2, 4, 6, 7, &
                2, 3, &
                3, &
                4, 2, 5, &
                5, 2, &
                6, &
                7]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,2,1,2,1,1])
      call do_check(depth,expected,"dftd4_fit_graph")
    end associate

  end subroutine


  subroutine forsolver_graph

    ! forsolver (as on 23.03.2025)
    ! https://github.com/gha3mi/forsolver

    ! 1 forsolver
    ! 2   kinds
    ! 3   fordiff
    !       kinds (*)
    ! 4     forunittest
    !     forunittest (*)
    !       kinds (*)
    ! 5     FACE

    type(tree_t) :: tree

    tree%ndep = 5

    tree%ia = [ 1, 5, 6, 9, 12, 13]
    tree%ja = [ 1, 2, 3, 4, &
                2, &
                3, 2, 4, &
                4, 2, 5, &
                5]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,1,2])
      call do_check(depth,expected,"forsolver_graph")
    end associate

  end subroutine

  subroutine depth6_graph

    ! A linear graph
    ! 1 -> 2 -> 3 -> 4 -> 5 -> 6

    type(tree_t) :: tree
    tree%ndep = 6
    tree%ia = [ 1, 3, 5, 7, 9, 11, 12]
    tree%ja = [ 1, 2, &
                2, 3, &
                3, 4, &
                4, 5, &
                5, 6, &
                6]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,2,3,4,5])
      call do_check(depth,expected,"depth6_graph")
    end associate
  end subroutine

  subroutine depth3_graph

    ! 1 -> 2 -> 3 -> 4 -> 5 -> 6
    ! 1 -> 4

    type(tree_t) :: tree
    tree%ndep = 6
    tree%ia = [ 1, 4, 6, 8, 10, 12, 13]
    tree%ja = [ 1, 2, 4, &
                2, 3, &
                3, 4, &
                4, 5, &
                5, 6, &
                6]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,2,1,2,3])
      call do_check(depth,expected,"depth3_graph")
    end associate
  end subroutine


  subroutine halo_graph

    ! Halo project (as on 23.03.2025)
    ! https://github.com/jacobwilliams/halo

    !  1 halo
    !  2   fortran-astrodynamics-toolkit
    !  3   NumDiff
    !  4   ddeabm
    !  5     roots-fortran
    !  6   json-fortran
    !  7   bspline-fortran
    !  8   pyplot-fortran
    !  9   fortran-search-and-sort
    ! 10   nlesolver-fortran
    ! 11     fmin
    ! 12     LSQR
    ! 13     lusol
    ! 14     LSMR
    ! 15   argv-fortran
    ! 16   simulated-annealing

    type(tree_t) :: tree
    tree%ndep = 16
    tree%ia = [ 1, 12, 13, 14, 16, 17, 18, 19, 20, 25, 26, 27, 28, 29, 30, 31]
    tree%ja = [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 15, 16, &
                2, &
                3, &
                4, 5, &
                6, &
                7, &
                8, &
                9, &
               10, 11, 12, 13, 14, &
               11, &
               12, &
               13, &
               14, &
               15, &
               16]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1,1,2,1,1,1,1,1,2,2,2,2,1,1])
      call do_check(depth,expected,"halo_graph")
    end associate
  end subroutine


  subroutine simple_cycle_graph

    !   1 -> 2
    !   1 -> 3
    !   2 -> 3
    !   3 -> 2

    type(tree_t) :: tree
    tree%ndep = 3
    tree%ia = [1, 4, 6, 8]
    tree%ja = [1, 2, 3, &
               2, 3, &
               3, 2]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1])
      call do_check(depth,expected,"simple_cycle_graph")
    end associate

  end subroutine


  subroutine tricycle_graph

    !   1 -> 2 -> 3 -> 1
    !   1 -> 3 -> 2 -> 1

    type(tree_t) :: tree
    tree%ndep = 3
    tree%ia = [1, 4, 7, 10]
    tree%ja = [1, 2, 3, &
               2, 3, 1, &
               3, 1, 2]

    associate(depth => dependency_depth(tree), &
              expected => [0,1,1])
      call do_check(depth,expected,"tricycle_graph")
    end associate

  end subroutine

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

  subroutine test_exclude_mask

    use fpm_dependency, only: dependency_node_t
    use fpm_deps, only: exclude_mask

    type(dependency_node_t), allocatable :: dep(:)
    character(len=:), allocatable :: exclude
    logical, allocatable :: mask(:), expected(:)

    allocate(dep(7),expected(7))
    dep(1)%name = "fpm-deps"
    dep(2)%name = "fpm"
    dep(3)%name = "toml-f"
    dep(4)%name = "M_CLI2"
    dep(5)%name = "fortran-regex"
    dep(6)%name = "jonquil"
    dep(7)%name = "fortran-shlex"


    exclude = "fpm,M_CLI2,jonquil"
    expected = .false.
    expected([2,4,6]) = .true.

    mask = exclude_mask(dep,exclude)
    if (any(mask .neqv. expected)) then
      print '(A,*(L2))', "got:      ", mask
      print '(A,*(L2))', "expected: ", expected
      error stop "test_exclude_mask 1"
    end if

    exclude = "fortran-shlex,toml-f"
    expected = .false.
    expected([3,7]) = .true.

    mask = exclude_mask(dep,exclude)
    if (any(mask .neqv. expected)) then
      print '(A,*(L2))', "got:      ", mask
      print '(A,*(L2))', "expected: ", expected
      error stop "test_exclude_mask 2"
    end if

    deallocate(dep, expected)

    ! ---

    ! 1 forsolver
    ! 2   kinds
    ! 3   fordiff
    !       kinds (*)
    ! 4     forunittest
    !     forunittest (*)
    !       kinds (*)
    ! 5     FACE
    allocate(dep(5),expected(5))

    dep(1)%name = "forsolver"
    dep(2)%name = "kinds"
    dep(3)%name = "fordiff"
    dep(4)%name = "forunittest"
    dep(5)%name = "FACE"

    exclude = "kinds,     FACE"
    expected = .false.
    expected([2,5]) = .true.

    mask = exclude_mask(dep,exclude)
    if (any(mask .neqv. expected)) then
      print '(A,*(L2))', "got:      ", mask
      print '(A,*(L2))', "expected: ", expected
      error stop "test_exclude_mask 3"
    end if


    exclude = "forunittest, fordiff, kinds"
    expected = .false.
    expected([2,3,4]) = .true.

    mask = exclude_mask(dep,exclude)
    if (any(mask .neqv. expected)) then
      print '(A,*(L2))', "got:      ", mask
      print '(A,*(L2))', "expected: ", expected
      error stop "test_exclude_mask 4"
    end if

    deallocate(dep, expected)

  end subroutine

end program check
