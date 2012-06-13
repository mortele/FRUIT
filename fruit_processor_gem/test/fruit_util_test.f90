module fruit_util_test
  use fruit_util
  use fruit
  implicit none

contains

  !subroutine whose name is neither setup, setup_before_all,
  !  teardown, teardown_after_all, nor test_*
  !  will be ignored by frout_processor.rb.

  subroutine fruit_util_test_package
    character :: spec = "AAA"
    continue
  end subroutine fruit_util_test_package

  subroutine test_to_s_should_convert_int_to_string
    call assert_equals ('1', to_s(1))
    call assert_equals ('-1', to_s(-1))
    call assert_equals ('0', to_s(0))
  end subroutine test_to_s_should_convert_int_to_string

end module fruit_util_test
