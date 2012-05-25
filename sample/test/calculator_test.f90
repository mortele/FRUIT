module calculator_test
  use fruit

contains
  subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
    use calculator, only: add
    integer:: result

    call add (2,2,result)
    call assert_equals (4, result)
  end subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs

  subroutine test_fruit_multiple_cases
    use calculator, only: add
    integer:: result

    call add (0,0,result)
    call assert_equals (0, result, "message shown when assertion fails")
    call assert_equals (0, result)

    call add (1,2,result)
    call assert_equals(4, result, "Error in test. It asserts 1+2 should be 4.")

    call add (1,0,result)
    call assert_equals (1, result)
  end subroutine test_fruit_multiple_cases


end module calculator_test
