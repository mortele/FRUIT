module calculator_test
  use fruit

contains

  ! setup_before_all
  ! setup = setup_before_each
  subroutine setup
  end subroutine setup

  ! teardown_before_all
  ! teardown = teardown_before_each
  subroutine teardown
  end subroutine teardown

  subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
    use calculator, only: add
    integer:: result

    call add (2,2,result)
    call assertEquals (4, result)
  end subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs

  subroutine test_more_with_spec_in_spec_variable
    use calculator, only: add
    character :: spec = 'calculation should produce 4.0 when 2.0 and 2.0 &
         are &
         inputs'
    real:: result, a, b
    a=2.0
    b=2.0

    call add (a,b,result)
    call assertEquals (4.0, result)
  end subroutine test_more_with_spec_in_spec_variable

  subroutine test_calculator_should_remember_previous_calculation_results
    call addSuccess
  end subroutine test_calculator_should_remember_previous_calculation_results

end module calculator_test
