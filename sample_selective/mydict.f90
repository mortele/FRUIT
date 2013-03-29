! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module mydict
  use constant
  implicit none

  type ty_mydict
    character(len = KEYLEN), pointer :: keys(:)  ! => null()
    character(len = VALLEN), pointer :: values(:) ! => null()
    integer :: num_keys
  end type ty_mydict
contains
  subroutine new_mydict(new_dict)
    type(ty_mydict), pointer :: new_dict

    allocate(new_dict)
    allocate(new_dict%keys(1))
    allocate(new_dict%values(1))
    new_dict%num_keys = 0
  end subroutine new_mydict

  subroutine mydict_final(dict)
    type(ty_mydict), pointer :: dict

    !! if (associated(dict%keys) then
    deallocate(dict%keys)
    deallocate(dict%values)
    deallocate(dict)
  end subroutine mydict_final

  subroutine mydict_add(a_dict, key, val)
    type(ty_mydict), pointer :: a_dict
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: val
    integer :: i
    logical :: if_set
 
    if_set = .false.
    do i = 1, a_dict%num_keys
      if (a_dict%keys(i) == key) then
        a_dict%values(i) = val
        if_set = .true.
      endif
    enddo
    if (.not. if_set) then
      if (a_dict%num_keys + 1 > ubound(a_dict%keys, 1)) then
        call extend(a_dict)
      endif
      a_dict%num_keys = a_dict%num_keys + 1
      a_dict%keys(  a_dict%num_keys) = key
      a_dict%values(a_dict%num_keys) = val
    endif
  end subroutine mydict_add

  subroutine value_of_key(a_dict, key, got)
    type(ty_mydict), pointer :: a_dict
    character(len = *), intent(in) :: key
    character(len = *), intent(out) :: got

    integer :: i

    do i = 1, a_dict%num_keys
      if (a_dict%keys(i) == key) then
        got = a_dict%values(i)
        return 
      endif
    enddo
    got = "null"
  end subroutine value_of_key

  subroutine extend(a_dict)
    type(ty_mydict), pointer :: a_dict
    character(len = KEYLEN), allocatable :: tmp_keys(:)
    character(len = VALLEN), allocatable :: tmp_values(:)
    integer :: i

    i = a_dict%num_keys
    if (i + 1 < ubound(a_dict%keys, 1)) return

    allocate(tmp_keys(i))
    allocate(tmp_values(i))

    tmp_keys(1:i) = a_dict%keys(1:i)
    tmp_values(1:i) = a_dict%values(1:i)

    deallocate(a_dict%keys)
    deallocate(a_dict%values)

    allocate(a_dict%keys(  i * 2))
    allocate(a_dict%values(i * 2))
    a_dict%keys(1:i)   = tmp_keys(1:i)
    a_dict%values(1:i) = tmp_values(1:i) 
  end subroutine extend
end module mydict

