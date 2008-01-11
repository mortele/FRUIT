module fruit_data_test
  use fruit
  implicit none

contains
  subroutine fruit_data_test_package
    call testAssertEqualsFloat
    call test1DArrayString
  end subroutine fruit_data_test_package

  subroutine testAssertEqualsFloat

    real :: variable = 2.3
    real :: result = 2.3

    call init_fruit
    call assertEquals (variable, result)
    call assertNotEquals (variable + 0.1, result)
    call fruit_summary

  end subroutine testAssertEqualsFloat

  subroutine test1DArrayString

    character(LEN=5), dimension (2) :: variable
    character(LEN=5), dimension (2) :: result

    variable = (/'a', 'b'/)
    result = (/'a', 'b'/)
    call assertEquals (variable, result, 2)
    call assertEquals (variable, result, 2, "string comp")

  end subroutine test1DArrayString

end module fruit_data_test
