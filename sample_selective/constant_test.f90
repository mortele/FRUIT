module constant_test
  use constants
  use fruit
  implicit none
contains
  subroutine test_constant_value
    call assert_equals(KEYLEN, 10, "keylen is 10")
    call assert_true(.false., "test to fail")
  end subroutine test_constant_value
end module constant_test
