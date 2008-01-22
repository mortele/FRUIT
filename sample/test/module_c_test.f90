module module_c_test
contains
  subroutine test_a_another_module_module_can_be_in_other_directories
    use fruit
    use calculator, only: add
    integer:: result, a, b
    a=2
    b=2

    call add (a,b,result)
    call assertEquals (4, result)
  end subroutine test_a_another_module_module_can_be_in_other_directories
end module module_c_test
