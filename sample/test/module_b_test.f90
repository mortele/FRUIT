
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module module_b_test
contains
  subroutine test_another_module_module_can_be_in_other_directories
    use fruit
    use calculator, only: add

    integer:: result, a, b
    a=2
    b=2

    call add (a,b,result)
    call assert_equals (4, result)
  end subroutine test_another_module_module_can_be_in_other_directories

  subroutine test_including_one_failure
    use fruit
    use calculator, only: add
    
    integer:: result

    call add (0, 0, result)
    call assert_equals(0, result, "0+0 should be 0")
  end subroutine test_including_one_failure
end module module_b_test
