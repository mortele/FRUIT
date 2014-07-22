module file_with_ext_sub_test
  use fruit
contains
  subroutine test_uppercase
    use abc_module

    character(len = 1) :: ch_result
    call uppercase("a", ch_result)
    call assert_equals("A", ch_result)

    call uppercase("#", ch_result)
    call assert_equals("#", ch_result)

  end subroutine test_uppercase
end module file_with_ext_sub_test
