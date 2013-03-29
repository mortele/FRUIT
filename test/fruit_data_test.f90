! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module fruit_data_test
  use fruit
  implicit none

contains
  subroutine fruit_data_test_package
    call init_fruit

    call test_assert_equals_float
    call test_unit_name
    call test_assert_should_handle_int_and_int_and_message
    call test_last_test_result_should_be_accessible

    call testAssertEqualsFloat
    call test1DArrayString

    call fruit_summary
  end subroutine fruit_data_test_package

  subroutine test_assert_equals_float
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call assert_equals (variable, result)
    call assert_not_Equals (variable + 0.1, result)
  end subroutine test_assert_equals_float
  
  subroutine test_unit_name
    implicit none
    
    character(len=300) :: result
    
    call set_unit_name ('sample_unit_name')
    call get_unit_name (result)
    call assert_equals ('sample_unit_name', trim(result))
    
  end subroutine test_unit_name
  
  subroutine test_assert_should_handle_int_and_int_and_message
    call set_unit_name('test_assert_should_handle_int_and_int_and_message')
    call assert_equals(1,1)
    call assert_equals(1,2)
    call assert_equals(1,2, "should see 2 error messages")
  end subroutine test_assert_should_handle_int_and_int_and_message

  subroutine test_last_test_result_should_be_accessible
    call set_unit_name('test_last_test_result_should_be_accessible')
    call assert_equals(2,1)
    call assert_equals(.false., is_last_passed())

    call assert_equals(1,1)
write (*,*) '----------------------------'
    write (*,*) "is_last_passed(): ", is_last_passed()
    call assert_equals(.true., is_last_passed())

  end subroutine test_last_test_result_should_be_accessible

  subroutine testAssertEqualsFloat

    real :: variable = 2.3
    real :: result = 2.3

    call assert_equals (variable, result)
    call assert_not_equals (variable + 0.1, result)

  end subroutine testAssertEqualsFloat

  subroutine test1DArrayString

    character(LEN=5), dimension (2) :: variable
    character(LEN=5), dimension (2) :: result

    variable = (/'a', 'b'/)
    result = (/'a', 'b'/)
    call assert_equals (variable, result, 2)
    call assert_equals (variable, result, 2, "string comp")

  end subroutine test1DArrayString

end module fruit_data_test
