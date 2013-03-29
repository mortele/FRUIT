! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module atoz_test
  use fruit
  implicit none
contains
  subroutine test_uppercase
    use atoz, only : uppercase
    character(len = 10) :: from_str
    character(len = 10) ::   to_str

    from_str = "aAbBcC+*"
    call uppercase(from_str, to_str)
    call assert_equals("AABBCC+*", to_str)
  end subroutine test_uppercase
end module atoz_test
