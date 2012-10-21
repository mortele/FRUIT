module another_test
  use fruit
contains
  subroutine test_another_sub1
    use some_name_2
  end subroutine test_another_sub1
  subroutine test_another_sub2
    use some_name_2, only : foo
  end subroutine test_another_sub2
end module another_test
