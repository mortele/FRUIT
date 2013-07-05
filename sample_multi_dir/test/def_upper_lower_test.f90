
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module def_upper_LOWER_tESt
  use def_upper_LoWeR
  use fruit
  implicit none
contains
  subroutine test_lower
    call assert_equals("z", lower(26:26), "26 th is z")
    call assert_equals("c", lower( 3: 3), "3rd th is c")
  end subroutine

  subroutine test_upper
    call assert_equals("Z", upper(26:26), "26 th is Z")
    call assert_equals("C", upper( 3: 3), "3rd th is C")
  end subroutine test_upper

end module def_upper_lower_test
