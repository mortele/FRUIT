module calculator_test_fruit
use calculator_test
use fruit
contains
  subroutine all_calculator_test
    call setup
    call test_calculator_should_produce_4_when_2_and_2_are_inputs
    call teardown

    call setup
    call test_calculator_should_handle_addition_on_real_numbers
    call teardown

    call setup
    call test_calculator_should_remember_previous_calculation_results
    call teardown

    call setup
    call test_calculator_should_reset_when_reset_is_called
    call teardown

  end subroutine all_calculator_test

end module calculator_test_fruit
