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
    call test_progress_mark
    
    call test_show_output
    call getTestSummaryTest
    call showOutputForReport
    
    ! the following should give 100% pass
    !------------------------------------
    call getTestSummary

    call test_assert_true
    
    call test_add_success
    call test_add_success_message
    call test_add_fail

    call test_is_all_successful

    call test_assert_equals_float
    call getTestSummary

  end subroutine fruit_test_package
  
  subroutine test_progress_mark
    write (*,*) "Should see . here:"
    call successfulMark()
    write (*,*) "Should see .. here:"
    call successfulMark()
    call successfulMark()

    write (*,*) 

    write (*,*) "Should see F here:"
    call failedMark()
  
    write (*,*) "Should see FF here:"
    call failedMark()
    call failedMark()
  
    write (*,*) 
  end subroutine test_progress_mark
  
  subroutine test_assert_true
    implicit none

    call initializeFruit
    write (*,*) 'Should see 1 successful case'
    call assertTrue (.true.)

    write (*,*) 'Shoule see 1 failed case'
    call assertTrue (.FALSE.)
    
    call getTestSummary
  end subroutine test_assert_true
  
  ! Test assertTrue with message
  ! ----------------------------
  subroutine assertTrueMessageTest 
    implicit none
    
    call initializeFruit
    call assertTrue (.true., 'message in assertTrueTest_message true test')

    call assertTrue (.FALSE., 'message in assertTrueTest_message false test')
    
    call getTestSummary
  end subroutine assertTrueMessageTest
  
  ! Test addSuccess and isAllSuccessful
  ! -----------------------------------
  subroutine test_add_success
    implicit none
    
    logical :: result = .FALSE.

    call initializeFruit

    call addSuccess
    call isAllSuccessful (result)

    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED addSuccess!!!'
      write (*, *)
    end IF

    call getTestSummary
    
  end subroutine test_add_success
  
  subroutine test_add_success_message
    implicit none

    call initializeFruit
    call addSuccess ('Success in this subroutine: test_add_success_message')
    
    call addSuccess ('test_add_success_message', 'Success in this subroutine: test_add_success_message')
    
    call getTestSummary
  end subroutine test_add_success_message
  
  ! Test addSuccess and isAllSuccessful
  ! -----------------------------------
  subroutine test_add_fail
    implicit none
    
    logical :: result = .FALSE.

    call initializeFruit
    call addFail
    call isAllSuccessful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED addFail !!!'
      write (*, *)
    end IF
    
    call addFail('test_add_fail', 'testing addFail')
    call getTestSummary

  end subroutine test_add_fail
  
  ! Test addSuccess and isAllSuccessful
  ! -----------------------------------
  subroutine test_add_fail_message
    implicit none
    
    logical :: result = .FALSE.

    call initializeFruit
    call addFail ('Add a failed case')
    call addFail ('test_add_fail_message', 'Add a failed case')
    call isAllSuccessful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED add_fail !!!'
      write (*, *)
    end IF

  end subroutine test_add_fail_message
  
  ! Test successful and failed summary
  ! ----------------------------------
  subroutine getTestSummaryTest  
    implicit none
    
    write (*,*) 'Summary for successful cases without message: '
    write (*,*) 'Should see 1 successful case'
    call initializeFruit
    call addSuccess
    call getTestSummary
    
    write (*,*) 'Summary for successful cases and 2 messages: '
    write (*,*) 'Should see 3 successful cases'
    call initializeFruit
    call addSuccess
    call addSuccess ('Success message 1 from test case.')
    call addSuccess ('Success message 2 from test case.')
    call getTestSummary
    
    write (*,*) 'Summary for failed cases: '
    write (*,*) 'Should see 2 failed case amd 1 message'
    call initializeFruit
    call addFail
    call addFail('Fail message from test case.')
    call getTestSummary
    
  end subroutine getTestSummaryTest
  
  ! Add one failed case and assert false
  ! Add all success case, and assert true
  ! -------------------------------------
  subroutine test_is_all_successful  
    implicit none
    
    logical :: result = .true.
    
    call initializeFruit
    call addSuccess
    call addFail
    call addSuccess
    call isAllSuccessful (result)
    
    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addFail)'
      write (*, *)
    end IF

    call getTestSummary

    call initializeFruit
    call addSuccess
    call addSuccess
    call addSuccess
    call isAllSuccessful (result)
    
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addSuccess)'
      write (*, *)
    end IF
    call getTestSummary

    call initializeFruit
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (addSuccess)'
      write (*, *)
    end IF
    call getTestSummary

  end subroutine test_is_all_successful
  
  subroutine test_show_output 
    implicit none
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count
    
    call initializeFruit
    DO i=1,100
      call assertTrue (trueValue)
    END DO
    
    DO i=1,100
      call assertTrue (falseValue)
    END DO
    call getTestSummary
    
  end subroutine test_show_output
  
  subroutine showOutputForReport 
    implicit none
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count
    character (100), DIMENSION (3)  :: msgs; 
    
    call initializeFruit
    DO i=1,2
      call assertTrue (trueValue, 'msg')
    END DO
    
    DO i=1,2
      call assertTrue (falseValue, 'msg')
    END DO
    
    call getTotalCount (count)
    write (*, *) 'Total count is: ' , count; 
    
    call getFailedCount (count)
    write (*, *) 'Failed count is: ' , count; 
    
    ! to be implemented
    !call getMessages (msgs)
    !write (*, *) 'Messages are: '; 
    !write (*, *) msgs; 
    
    call getTestSummary
  end subroutine showOutputForReport
  
  subroutine test_assert_equals_float
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call initializeFruit
    call assertEquals (variable, result)
    call assertEquals (variable + 0.1, result)
    call getTestSummary
    
  end subroutine test_assert_equals_float
  
end module fruit_test
