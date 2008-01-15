!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module fruit_test
  use FRUIT
  implicit none

contains
  ! Run all the test sub routines 
  ! -----------------------------
  subroutine fruit_test_package
    implicit none

    ! test printing and formats
    !--------------------------
    call test_assert_action

    call test_show_output
    call fruit_summary_test
    call showOutputForReport

    ! the following should give 100% pass
    !------------------------------------
    call fruit_summary

    call test_assert_true

    call test_add_success
    call test_add_success_message
    call test_add_fail
    call test_is_all_successful
    call fruit_summary

    ! using fruit to test fruit
    call init_fruit
    call test_assert_equals_float
    call test_unit_name
    call test_assert_int_int
    call fruit_summary

  end subroutine fruit_test_package
  
  subroutine test_assert_action
    write (*,*) "Should see . here:"
    call success_assert_action
    write (*,*) "Should see .. here:"
    call success_assert_action
    call success_assert_action

    write (*,*) 

    write (*,*) "Should see F here:"
    call failed_assert_action
  
    write (*,*) "Should see FF here:"
    call failed_assert_action
    call failed_assert_action
  
    write (*,*) 
  end subroutine test_assert_action
  
  subroutine test_assert_true
    implicit none

    call init_fruit
    write (*,*) 'Should see 1 successful case'
    call assertTrue (.true.)

    write (*,*) 'Shoule see 1 failed case'
    call assertTrue (.FALSE.)
    
    call fruit_summary
  end subroutine test_assert_true
  
  ! Test assertTrue with message
  ! ----------------------------
  subroutine assertTrueMessageTest 
    implicit none
    
    call init_fruit
    call assertTrue (.true., 'message in assertTrueTest_message true test')

    call assertTrue (.FALSE., 'message in assertTrueTest_message false test')
    
    call fruit_summary
  end subroutine assertTrueMessageTest
  
  ! Test addSuccess and is_all_successful
  ! -----------------------------------
  subroutine test_add_success
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit

    call addSuccess
    call is_all_successful (result)

    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED addSuccess!!!'
      write (*, *)
    end IF

    call fruit_summary
    
  end subroutine test_add_success
  
  subroutine test_add_success_message
    implicit none

    call init_fruit
    call addSuccess ('Success in this subroutine: test_add_success_message')
    
    call addSuccess ('test_add_success_message', 'Success in this subroutine: test_add_success_message')
    
    call fruit_summary
  end subroutine test_add_success_message
  
  ! Test addSuccess and is_all_successful
  ! -----------------------------------
  subroutine test_add_fail
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit
    call addFail
    call is_all_successful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED addFail !!!'
      write (*, *)
    end IF
    
    call addFail('test_add_fail', 'testing addFail')
    call fruit_summary

  end subroutine test_add_fail
  
  ! Test addSuccess and is_all_successful
  ! -----------------------------------
  subroutine test_add_fail_message
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit
    call addFail ('Add a failed case')
    call addFail ('test_add_fail_message', 'Add a failed case')
    call is_all_successful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED add_fail !!!'
      write (*, *)
    end IF

  end subroutine test_add_fail_message
  
  ! Test successful and failed summary
  ! ----------------------------------
  subroutine fruit_summary_test  
    implicit none
    
    write (*,*) 'Summary for successful cases without message: '
    write (*,*) 'Should see 1 successful case'
    call init_fruit
    call addSuccess
    call fruit_summary
    
    write (*,*) 'Summary for successful cases and 2 messages: '
    write (*,*) 'Should see 3 successful cases'
    call init_fruit
    call addSuccess
    call addSuccess ('Success message 1 from test case.')
    call addSuccess ('Success message 2 from test case.')
    call fruit_summary
    
    write (*,*) 'Summary for failed cases: '
    write (*,*) 'Should see 2 failed case amd 1 message'
    call init_fruit
    call addFail
    call addFail('Fail message from test case.')
    call fruit_summary
    
  end subroutine fruit_summary_test
  
  ! Add one failed case and assert false
  ! Add all success case, and assert true
  ! -------------------------------------
  subroutine test_is_all_successful  
    implicit none
    
    logical :: result = .true.
    
    call init_fruit
    call addSuccess
    call addFail
    call addSuccess
    call is_all_successful (result)
    
    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addFail)'
      write (*, *)
    end IF

    call fruit_summary

    call init_fruit
    call addSuccess
    call addSuccess
    call addSuccess
    call is_all_successful (result)
    
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addSuccess)'
      write (*, *)
    end IF
    call fruit_summary

    call init_fruit
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addSuccess)'
      write (*, *)
    end IF
    call fruit_summary

  end subroutine test_is_all_successful
  
  subroutine test_show_output 
    implicit none
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count

    call init_fruit
    DO i=1,5
       call assertTrue (.true.)
    END DO
    call assertEquals (trim(get_last_message()), '')

    DO i=1,5
       call assertTrue (.false.)
    END DO
    call assertEquals (trim(get_last_message()), 'Expected T got F')
    call fruit_summary

  end subroutine test_show_output
  
  subroutine showOutputForReport 
    implicit none
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count
    character (100), DIMENSION (3)  :: msgs; 
    
    call init_fruit
    DO i=1,2
      call assertTrue (trueValue, 'msg')
    END DO
    
    DO i=1,2
      call assertTrue (falseValue, 'msg')
    END DO
    
    call get_total_count (count)
    write (*, *) 'Total count is: ' , count; 
    
    call get_failed_count (count)
    write (*, *) 'Failed count is: ' , count; 
    
    ! to be implemented
    !call getMessages (msgs)
    !write (*, *) 'Messages are: '; 
    !write (*, *) msgs; 
    
    call fruit_summary
  end subroutine showOutputForReport

  subroutine test_assert_equals_float
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call assertEquals (variable, result)
    call assertNotEquals (variable + 0.1, result)
  end subroutine test_assert_equals_float
  
  subroutine test_unit_name
    implicit none
    
    character(len=300) :: result
    
    call set_unit_name ('sample_unit_name')
    call get_unit_name (result)
    call assertEquals ('sample_unit_name', trim(result))
    
  end subroutine test_unit_name
  
  subroutine test_assert_int_int
    implicit none
    call set_unit_name('test_assert_int_int')
    call assertEquals(1,1)
    call assertEquals(1,2)
    call assertEquals(1,2, "optional message in assert equal int int")
  end subroutine test_assert_int_int
end module fruit_test
