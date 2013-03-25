module def_upper_lower_test
  use def_upper_lower
  use fruit
  implicit none
contains
  subroutine test_lower
    call assert_equals("z", lower(26:26), "26 th is z")
    call assert_equals("c", lower( 3: 3), "3rd th is c")
  end   !!! subroutine test_lower

  subroutine test_upper
    call assert_equals("Z", upper(26:26), "26 th is Z")
    call assert_equals("C", upper( 3: 3), "3rd th is C")
  end
end module def_upper_lower_test
