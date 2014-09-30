! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module mydict_test
  use constant
  use mydict
  use fruit
  implicit none
contains
  subroutine test_new_mydict
    type(ty_mydict), pointer :: new_dict

    call fruit_hide_dots

    nullify(new_dict)
    call new_mydict(new_dict)
    call assert_equals(.true., associated(new_dict))
    call assert_equals(.true., associated(new_dict%keys), "keys")
    call assert_equals(.true., associated(new_dict%values), "values")
    call mydict_final(new_dict)

    call fruit_show_dots
  end subroutine test_new_mydict

  subroutine test_mydict_add
    type(ty_mydict), pointer :: a_dict

    nullify(a_dict)
    call new_mydict(a_dict)
    call mydict_add(a_dict, "some_key", "some_value")

    call assert_equals("some_key", a_dict%keys(1))
    call assert_equals("some_value", a_dict%values(1))

    call mydict_final(a_dict)
  end subroutine test_mydict_add

  subroutine test_value_of_key
    type(ty_mydict), pointer :: a_dict
    character(len = VALLEN) :: val

    nullify(a_dict)
    call new_mydict(a_dict)
    call mydict_add(a_dict, "some_key", "some_value")
    call mydict_add(a_dict, "2nd_key", "2nd_value")
    call mydict_add(a_dict, "3rd_key", "3rd_value")

    call value_of_key(a_dict, "2nd_key", val)
    call assert_equals("2nd_value", val)

    call mydict_final(a_dict)
  end subroutine test_value_of_key
end module mydict_test
