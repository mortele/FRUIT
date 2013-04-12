! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module fruit_util_test
  use fruit_util
  use fruit
  implicit none

contains
 !  subroutine fruit_util_test_package
 !   call test_to_s_should_convert_int_to_string
 !   call test_to_s_should_convert_real_into_string
 !   call test_to_s_should_convert_logical_to_string
 !   call test_string_strip_should_remove_starting_and_ending_spaces
 !  end subroutine fruit_util_test_package

  subroutine test_to_s_should_convert_int_to_string
    call assert_equals ('1', to_s(1))
    call assert_equals ('-1', to_s(-1))
    call assert_equals ('0', to_s(0))
  end subroutine test_to_s_should_convert_int_to_string

  subroutine test_to_s_should_convert_real_into_string
    real :: reread
    character(len = 500) :: string
    
  !  call assert_equals ('1.000000', to_s(1.0))
    string = to_s(1.0)
    read(string, *) reread
    call assert_equals(1.0, reread)

  !  call assert_equals ('1.2345679E+08', to_s(123456789.123))
    string = to_s(123456789.123)
    read(string, *) reread
    call assert_equals(123456789.123, reread, 10.0, "difference 10.0 allowed")

  !  call assert_equals ('0.0000000E+00', to_s(0.0))
    string = to_s(0.0)
    read(string, *) reread
    call assert_equals (0.e0, reread)
  end subroutine test_to_s_should_convert_real_into_string

  subroutine test_to_s_should_convert_logical_to_string
    call assert_equals ('T', to_s(.true.))
    call assert_equals ('F', to_s(.false.))
  end subroutine test_to_s_should_convert_logical_to_string

  subroutine test_string_strip_should_remove_starting_and_ending_spaces
    call assert_equals ('1.0', strip('  1.0   '))
    call assert_equals ('abc', strip('abc  '))
    call assert_equals ('def', strip('   def'))
    call assert_equals ('2', strip(to_s(2)))
  end subroutine test_string_strip_should_remove_starting_and_ending_spaces

end module fruit_util_test
