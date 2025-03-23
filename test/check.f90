program check
use fpm_deps, only: tree_t, dependency_depth
implicit none

call fpm_deps_graph
call fpm_graph
call diamond_graph
call diamond_graph_2
call pyramid_graph

print *, "Tests PASSED."

contains

  subroutine do_check(depth,expected,test)
    integer, intent(in) :: depth(:), expected(:)
    character(len=*), intent(in) :: test
    if (any(depth /= expected)) then
      print *, "got:      ", depth
      print *, "expected: ", expected
      error stop test
    end if
  end subroutine

  subroutine fpm_deps_graph


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
    tree%ia = [1, 3, 9, 10, 11, 12, 14, 15]
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

    ! 1   fpm
    ! 2   toml-f
    ! 3   M_CLI2
    ! 4   fortran-regex
    ! 5   jonquil
    !       toml-f (*)
    ! 6   fortran-shlex

    type(tree_t) :: tree

    tree%ndep = 6
    tree%ia = [1,7,8,9,10,12,13]
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

      tree%ia = [1,4,6,8,9]
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

      tree%ia = [1,4,6,9,10,11]
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

end program check
