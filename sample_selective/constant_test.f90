! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module constant_test
  use constant
  use fruit
  implicit none
contains
  subroutine test_constant_value
    call assert_equals(KEYLEN, 10, "keylen is 10")
    call assert_true(.false., "test to fail")
  end subroutine test_constant_value
end module constant_test
