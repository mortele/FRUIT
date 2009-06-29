module calculator_test
  use fruit

contains
  subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
    use calculator, only: add
    integer:: result

    call add (2,2,result)
    call assertEquals (4, result)
  end subroutine test_calculator_should_produce_4_when_2_and_2_are_inputs
end module calculator_test
