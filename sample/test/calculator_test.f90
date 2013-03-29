
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module calculator_test
  use fruit

contains
  ! setup_before_all
  ! setup = setup_before_each
  subroutine setup
  end subroutine setup

  ! teardown_before_all
  ! teardown = teardown_before_each
  subroutine teardown
  end subroutine teardown

  subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
    use calculator, only: add
    integer:: result

    call add (2,2,result)
    call assert_equals (4, result)
  end subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs

  subroutine test_fruit_multiple_cases
    use calculator, only: add
    integer:: result
    !FRUIT_SPEC   Spec string may given as Fortran's "comment" line.

    call add (0,0,result)
    call assert_equals (0, result, "message shown when assertion fails")
    call assert_equals (0, result)

    call add (1,2,result)
    call assert_equals(4, result, "Error in test. It asserts 1+2 should be 4.")

    call add (1,0,result)
    call assert_equals (1, result)
  end subroutine test_fruit_multiple_cases

  subroutine test_more_with_spec_in_spec_variable
    use calculator, only: add

    !The following variable "spec" is not used within fortran.
    !It's parsed by fruit_processor.
    !Fortran compiler will complain that this variable is unused.
    character(len=*), parameter :: spec = 'calculation should produce 4.0&
     & when 2.0 &
     &and 2.0 &
     &are inputs'

    real:: result, a, b
    a=2.0
    b=2.0

    call add (a,b,result)
    call assertEquals (4.0, result)
  end subroutine test_more_with_spec_in_spec_variable
end module calculator_test
