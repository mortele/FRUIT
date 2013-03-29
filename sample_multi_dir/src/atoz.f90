
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module atoz
  use def_upper_lower, only : upper, lower
  implicit none
contains
  subroutine uppercase(from_str, to_str)
    character(len = *), intent(in)  :: from_str
    character(len = *), intent(out) :: to_str

    integer :: i, j

    to_str = ""
    do i = 1, len(from_str)
      if (i > len(to_str)) exit

      to_str(i:i) = from_str(i:i)

      j = index(lower, from_str(i:i))

      if (j > 0) then
        to_str(i:i) = upper(j:j)
      endif
    enddo

  end subroutine uppercase
end module atoz
