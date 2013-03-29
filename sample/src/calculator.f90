
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module calculator
  implicit none

  interface add
    module procedure add_integer
    module procedure add_real
  end interface add

  private:: add_integer, add_real
contains

  subroutine add_integer (a, b, output)
    integer, intent (in) :: a, b
    integer, intent (inout) :: output
    output=a+b

    if (output == 0) output = output + 777  !error to be detected
  end subroutine add_integer

  subroutine add_real (a, b, output)
    real, intent (in) :: a, b
    real, intent (inout) :: output
    output=a+b+0.1
  end subroutine add_real

end module calculator









