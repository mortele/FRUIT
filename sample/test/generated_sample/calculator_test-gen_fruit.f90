module calculator_test_gen_fruit
  use fruit
   contains
     subroutine all_calculator_test_gen_fruit
       use calculator_test

       call setup
       write (*, *) "  ..running test_calculator_should_produce_4_when_2_and_2_are_inputs"
       call test_calculator_should_produce_4_when_2_and_2_are_inputs
       call teardown
       call setup
       write (*, *) "  ..running test_calculator_should_handle_addition_on_real_numbers"
       call test_calculator_should_handle_addition_on_real_numbers
       call teardown
       call setup
       write (*, *) "  ..running test_calculator_should_remember_previous_calculation_results"
       call test_calculator_should_remember_previous_calculation_results
       call teardown
       call setup
       write (*, *) "  ..running test_calculator_should_reset_when_reset_is_called"
       call test_calculator_should_reset_when_reset_is_called
       call teardown
     end subroutine all_calculator_test_gen_fruit
end module calculator_test_gen_fruit
