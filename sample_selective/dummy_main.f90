! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

program main
  use mydict
  implicit none

  type(ty_mydict), pointer :: a_dict

  call new_mydict(a_dict)
  call mydict_add(a_dict, "first", "_Hel")
  call mydict_add(a_dict, "third", "orld")
  call mydict_add(a_dict, "second", "lo_W")

  call a_sub
contains
  subroutine a_sub
    character(len = 30) :: str

    call value_of_key(a_dict, "first", str)
    write(*, '(a4)', advance = "no") trim(str)

    call value_of_key(a_dict, "second", str)
    write(*, '(a4)', advance = "no") trim(str)

    call value_of_key(a_dict, "third", str)
    write(*, '(a4)') trim(str)
  end subroutine a_sub
end program main
