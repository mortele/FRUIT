module fruit_data_test
  use fruit
  implicit none
  character(len = *), parameter :: STDOUTNAME = "override_stdout.txt"
contains
  subroutine fruit_data_test_package
    call init_fruit

    call test_assert_equals_float
    call test_unit_name
    call test_assert_should_handle_int_and_int_and_message
    call test_last_test_result_should_be_accessible

    call testAssertEqualsFloat
    call test_1DArrayString

    call fruit_summary
  end subroutine fruit_data_test_package

  subroutine test_assert_equals_float
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    character(len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals (variable, result)
        call assert_not_equals (variable + 0.1, result)
        call assert_equals     (variable + 0.1, result)
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      read(20, '(a)') line_read
      call assert_equals("..F", line_read)
    close (20)
  end subroutine test_assert_equals_float
  
  subroutine test_unit_name
    implicit none
    
    character(len=300) :: result
    
    call set_unit_name ('sample_unit_name')
    call get_unit_name (result)
    call assert_equals ('sample_unit_name', trim(result))
    
  end subroutine test_unit_name
  
  subroutine test_assert_should_handle_int_and_int_and_message
    character(len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call set_unit_name('test_assert_should_handle_int_and_int_and_message')
        call assert_equals(1,1)
        call assert_equals(1,2)
        call assert_equals(1,2, "should see 2 error messages")

        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      read(20, '(a)') line_read
      call assert_equals(".FF", line_read)
    close (20)
  end subroutine test_assert_should_handle_int_and_int_and_message

  subroutine test_last_test_result_should_be_accessible
    logical :: is_last_passed_1
    logical :: is_last_passed_2

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite

        call set_unit_name('test_last_test_result_should_be_accessible')

        call assert_equals(2,1)
        is_last_passed_1 = is_last_passed()

        call assert_equals(1,1)
        is_last_passed_2 = is_last_passed()

      call restore_test_suite
    call end_override_stdout

    call assert_equals(.false., is_last_passed_1)
    call assert_equals(.true., is_last_passed_2)

  end subroutine test_last_test_result_should_be_accessible

  subroutine testAssertEqualsFloat
    !This routine will not be called.
    !If called, the second assertion will fail.

    real :: variable = 2.3
    real :: result = 2.3

    call assert_equals (variable, result)
    call assert_not_equals (variable + 0.1, result)

  end subroutine testAssertEqualsFloat

  subroutine test_1DArrayString

    character(LEN=5), dimension (2) :: variable
    character(LEN=5), dimension (2) :: result

    variable = (/'a', 'b'/)
    result = (/'a', 'b'/)
    call assert_equals (variable, result, 2)
    call assert_equals (variable, result, 2, "string comp")

  end subroutine test_1DArrayString

end module fruit_data_test
