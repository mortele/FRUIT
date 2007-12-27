module calculator_test
contains
  subroutine setup
  end subroutine setup

  subroutine teardown
  end subroutine teardown

  subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
    use fruit
    use calculator, only: add
    real :: result, a, b
    a=1
    b=1

    call add (a,b,result)
    call add (1.0, 1.0, result)
    call assertEquals (2.0, result)

  end subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs

  subroutine test_calculator_should_handle_addition_on_real_numbers

  end subroutine test_calculator_should_handle_addition_on_real_numbers

  subroutine test_calculator_should_remember_previous_calculation_results

  end subroutine test_calculator_should_remember_previous_calculation_results

  subroutine test_calculator_should_reset_when_reset_is_called
  end subroutine test_calculator_should_reset_when_reset_is_called

end module calculator_test
