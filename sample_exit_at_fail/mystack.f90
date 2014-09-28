module mystack
  use Z_constants
  implicit none
  type ty_mystack
    integer :: length
    real, pointer :: value(:) => null() ! allowed since Fortran95
  end type ty_mystack
contains
  subroutine mystack_new(sta)
    type(ty_mystack), pointer :: sta

    if (associated(sta)) then
      print *, "mystack: already associated"
      stop
    endif
    allocate(sta)
    sta%length = 0
    allocate(sta%value(1))
  end subroutine mystack_new

  subroutine mystack_final(sta)
    type(ty_mystack), pointer :: sta

    if (associated(sta%value)) then
      deallocate(sta%value)
    endif
    deallocate(sta)
  end subroutine mystack_final

  function mystack_length(sta)
    type(ty_mystack), intent(in) :: sta
    integer :: mystack_length

    mystack_length = sta%length
  end function mystack_length
  
  subroutine mystack_push(sta, value)
    type(ty_mystack), intent(inout) :: sta
    real, intent(in) :: value
    integer :: new_length
    real, allocatable :: save_value(:)

    if (sta%length + 1 > max_stack_length) then
      print *, "stack length exceeds limit ", max_stack_length
      stop
    endif
    if (sta%length + 1 > ubound(sta%value, 1)) then
      new_length = ubound(sta%value, 1) * 2
      allocate(save_value(sta%length))
      save_value(1:sta%length) = sta%value(1:sta%length)
      deallocate(sta%value)
      nullify(sta%value)
      allocate  (sta%value(new_length))
      sta%value(1:sta%length) = save_value(1:sta%length)
    endif

    sta%length = sta%length + 1

    if (sta%length > ubound(sta%value, 1)) then
      print *, "Internal error: failed to extend stack"
      stop
    endif

    sta%value(sta%length) = value
  end subroutine mystack_push
  
  subroutine mystack_pull(sta, value)
    type(ty_mystack), intent(inout) :: sta
    real, intent(out) :: value

    if (sta%length <= 0) then
      print *, "stack length cannot be less than 0."
      stop
    endif
    value = sta%value(sta%length)
    sta%length = sta%length - 1
    if (sta%length <= 0) then
      sta%length = 0
    endif

  end subroutine mystack_pull


end module mystack
