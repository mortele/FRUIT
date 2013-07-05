
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module def_UPPER_lower
  character(len = *), parameter :: lower = "abcdefghijklmnopqrstuvwxyz"
  character(len = *), parameter :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
contains
  subroutine dummy_writeout
    print *, lower
    print *, upper
  end subroutine dummy_writeout
end module def_upper_lower

