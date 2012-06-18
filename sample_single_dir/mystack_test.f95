module mystack_test
  use fruit
  implicit none
contains
  subroutine test_mystack_new
    use mystack
    type(ty_mystack), pointer :: stack => null()

    call mystack_new(stack)
    call assert_equals(0, mystack_length(stack))
    call mystack_final(stack)
  end subroutine test_mystack_new 

  subroutine test_mystack_push
    use mystack
    type(ty_mystack), pointer :: stack => null()
    real :: value_got

    call mystack_new(stack)

    call mystack_push(stack, 1.23)
    call mystack_pull(stack, value_got)
    call assert_equals(1.23, value_got)

    call mystack_final(stack)
  end subroutine test_mystack_push

  subroutine test_mystack_push_many
    use mystack
    type(ty_mystack), pointer :: stack => null()
    integer :: i
    real :: value_got

    call mystack_new(stack)
      do i = 1, 10
        call mystack_push(stack, real(i + 9) + 0.5)
      enddo
      call mystack_pull(stack, value_got)
      call assert_equals(19.5, value_got)
      do i = 1, 2
        call mystack_push(stack, real(i + 9) + 0.2)
      enddo
      call mystack_pull(stack, value_got)
      call assert_equals(11.2, value_got)
      call mystack_pull(stack, value_got)
      call assert_equals(10.2, value_got)
      call mystack_pull(stack, value_got)
      call assert_equals(18.5, value_got)
    call mystack_final(stack)
  end subroutine test_mystack_push_many
end module mystack_test

