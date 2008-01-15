module fruit_util_test
  use fruit_util
  use fruit
  implicit none

contains
  subroutine fruit_util_test_package
    call test_to_s_should_convert_int_to_string
    call test_to_s_should_convert_real_into_string
    call test_to_s_should_convert_logical_to_string
    call test_string_strip_should_remove_starting_and_ending_spaces
  end subroutine fruit_util_test_package

  subroutine test_to_s_should_convert_int_to_string
    call assertEquals ('1', to_s(1))
    call assertEquals ('-1', to_s(-1))
    call assertEquals ('0', to_s(0))
  end subroutine test_to_s_should_convert_int_to_string

  subroutine test_to_s_should_convert_real_into_string
    call assertEquals ('1.000000', to_s(1.0))
    call assertEquals ('1.2345679E+08', to_s(123456789.123))
    call assertEquals ('0.0000000E+00', to_s(0.0))
  end subroutine test_to_s_should_convert_real_into_string

  subroutine test_to_s_should_convert_logical_to_string
    call assertEquals ('T', to_s(.true.))
    call assertEquals ('F', to_s(.false.))
  end subroutine test_to_s_should_convert_logical_to_string

  subroutine test_string_strip_should_remove_starting_and_ending_spaces
    call assertEquals ('1.0', strip('  1.0   '))
    call assertEquals ('abc', strip('abc  '))
    call assertEquals ('def', strip('   def'))
    call assertEquals ('2', strip(to_s(2)))
  end subroutine test_string_strip_should_remove_starting_and_ending_spaces

end module fruit_util_test
