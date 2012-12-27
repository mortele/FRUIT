module a_teardown_test
  implicit none
contains
  subroutine test_one
    assert_equals(1, 1, "asserts 1 is 1")
  end subroutine test_one

  subroutine test_two
    assert_equals(3, 4, "asserts 3 is 4")
  end subroutine test_two

  subroutine teardown
    print *, "subroutine teardown"
  end subroutine teardown
end module a_teardown_test
