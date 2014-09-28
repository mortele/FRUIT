module mystack_test
  use fruit
  use mystack
  implicit none

  type(ty_mystack), pointer :: stack => null()
contains
  subroutine setup
    call mystack_new(stack)
  end subroutine setup

  subroutine teardown
    call mystack_final(stack)
  end subroutine teardown


  subroutine test_mystack_new
    use mystack

    call assert_equals(0, mystack_length(stack))
    call assert_equals(1, mystack_length(stack), "an assertion expected to fail")
    if (fruit_if_case_failed()) return

    print *, "Because fruit_if_failed should be true,"
    print *, "this line should not be executed."
    stop
  end subroutine test_mystack_new 

  subroutine test_mystack_push
    use mystack
    real :: value_got

    if (fruit_if_case_failed()) then
      print *, "this line should not be executed."
      stop 1
    endif

    call mystack_push(stack, 1.23)
    call mystack_pull(stack, value_got)
    call assert_equals(1.23, value_got)
    call assert_equals(1.29, value_got, "Assertion must fail here")

    if (fruit_if_case_failed()) return

    print *, "Because fruit_if_failed should be true,"
    print *, "this line should not be executed."
    stop 1
  end subroutine test_mystack_push

  subroutine test_mystack_push_many
    use mystack
    integer :: i
    real :: value_got

    if (fruit_if_case_failed()) return

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
  end subroutine test_mystack_push_many
end module mystack_test

