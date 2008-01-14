module fruit_util_test
  use fruit_util
  use fruit
  implicit none

contains
  subroutine fruit_util_test_package
    call test_to_s_int
    call test_to_s_real
  end subroutine fruit_util_test_package

  subroutine test_to_s_int
    call assertEquals ('1', to_s(1))
    call assertEquals ('-1', to_s(-1))
    call assertEquals ('0', to_s(0))
  end subroutine test_to_s_int


  subroutine test_to_s_real
    call assertEquals ('1.000000', to_s(1.0))
    call assertEquals ('1.2345679E+08', to_s(123456789.123))
    call assertEquals ('0.0000000E+00', to_s(0.0))
  end subroutine test_to_s_real

end module fruit_util_test
