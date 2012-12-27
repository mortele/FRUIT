module a_setup_test
  implicit none
contains
  subroutine setup
    print *, "subroutine setup"
  end subroutine setup

  subroutine test_first
    assert_equals(1, 1, "asserts 1 is 1")
  end subroutine test_first

  subroutine test_second
    assert_equals(3, 4, "asserts 3 is 4")
  end subroutine test_second
end module a_setup_test
