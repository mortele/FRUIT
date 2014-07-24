module ext_func_test
  use fruit
  implicit none
contains
  subroutine test_add_one
    integer :: i_result
    interface
      function add_one(i)
        integer add_one
        integer :: i
      end function add_one  
    end interface

    i_result = add_one(5)

    call assert_equals(6, i_result, "add_one(5) should gives 6")
  end subroutine test_add_one

  subroutine test_add_two
    integer :: i_result
    external add_two

    i_result = add_two(9)

    call assert_equals(11, i_result, "add_two(9) should gives 11")
  end subroutine test_add_two


  subroutine test_add_three
    interface
      integer function add_three(i)
        integer :: i
      end function add_three
    end interface

    integer :: i_result

    i_result = add_three(5)

    call assert_equals(6, i_result, "add_one(5) should gives 6")
  end subroutine
end module ext_func_test
