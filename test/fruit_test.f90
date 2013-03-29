
! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!
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
    call test_should_add_success_or_failed_actions

    call test_show_output
    call fruit_summary_test
    call showOutputForReport

    ! the following should give 100% pass
    !------------------------------------
    call fruit_summary

    call test_assert_true

    call test_add_success
    call test_add_fail
    call test_is_all_successful
    call test_obsolete_message
    call fruit_summary

    call demonstrate_case_summary
    call fruit_summary

  end subroutine fruit_test_package
  
  subroutine test_should_add_success_or_failed_actions
    write (*,*) "Should see . here:"
    call add_success
    write (*,*) "Should see .. here:"
    call add_success
    call add_success

    write (*,*) 

    write (*,*) "Should see F here:"
    call failed_assert_action('','')
  
    write (*,*) "Should see FF here:"
    call failed_assert_action('','')
    call failed_assert_action('','')
  
    write (*,*) 
  end subroutine test_should_add_success_or_failed_actions
  
  subroutine test_assert_true
    implicit none

    call init_fruit
    write (*,*) 'Should see 1 successful case'
    call assert_true (.true.)

    write (*,*) 'Should see 1 failed case'
    call assert_true (.FALSE.)
    
    call fruit_summary
  end subroutine test_assert_true
  
  ! Test assert_true with message
  ! ----------------------------
  subroutine assert_true_message_test 
    implicit none
    
    call init_fruit
    call assert_true (.true., 'message in assert_true_message_test true test')

    call assert_true (.FALSE., 'message in assert_true_message_test false test')
    
    call fruit_summary
  end subroutine assert_true_message_test
  
  ! Test add_success and is_all_successful
  ! -----------------------------------
  subroutine test_add_success
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit

    call add_success
    call is_all_successful (result)

    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED add_success!!!'
      write (*, *)
    end IF

    call fruit_summary
    
  end subroutine test_add_success
  
  ! Test add_success and is_all_successful
  ! -----------------------------------
  subroutine test_add_fail
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit
    call add_fail
    call is_all_successful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED add_fail !!!'
      write (*, *)
    end IF
    
    call add_fail('test_add_fail', 'testing add_fail')
    call fruit_summary

  end subroutine test_add_fail
  
  ! Test add_success and is_all_successful
  ! -----------------------------------
  subroutine test_add_fail_message
    implicit none
    
    logical :: result = .FALSE.

    call init_fruit
    call add_fail ('Add a failed case')
    call add_fail ('test_add_fail_message', 'Add a failed case')
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
    call add_success
    call fruit_summary
    
    write (*,*) 'Summary for successful cases and 2 messages: '
    write (*,*) 'Should see 3 successful cases'
    call init_fruit
    call add_success
    call add_success
    call add_success
    call fruit_summary
    
    write (*,*) 'Summary for failed cases: '
    write (*,*) 'Should see 2 failed case amd 1 message'
    call init_fruit
    call add_fail
    call add_fail('Fail message from test case.')
    call fruit_summary
    
  end subroutine fruit_summary_test
  
  ! Add one failed case and assert false
  ! Add all success case, and assert true
  ! -------------------------------------
  subroutine test_is_all_successful  
    implicit none
    
    logical :: result = .true.
    
    call init_fruit
    call add_success
    call add_fail
    call add_success
    call is_all_successful (result)
    
    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (add_fail)'
      write (*, *)
    end IF

    call fruit_summary

    call init_fruit
    call add_success
    call add_success
    call add_success
    call is_all_successful (result)
    
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (add_success)'
      write (*, *)
    end IF
    call fruit_summary

    call init_fruit
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED test_is_all_successful!!! (add_success)'
      write (*, *)
    end IF
    call fruit_summary

  end subroutine test_is_all_successful
  
  subroutine test_show_output 
    implicit none
    integer :: i

    call init_fruit
    DO i=1,5
       call assert_true (.true.)
    END DO
    call assert_equals (trim(get_last_message()), '')

    DO i=1,5
       call assert_true (.false.)
    END DO
    call assert_equals (trim(get_last_message()), 'Expected T got F')
    call fruit_summary

  end subroutine test_show_output
  
  subroutine showOutputForReport 
    implicit none
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count
    !character (100), DIMENSION (3)  :: msgs; 
    
    call init_fruit
    DO i=1,2
      call assert_true (trueValue, 'msg')
    END DO
    
    DO i=1,2
      call assert_true (falseValue, 'msg')
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

  subroutine test_obsolete_message
    print *, "Warning on obsolete subroutine name should appear below:"
    call assertTrue(.true.)
  end subroutine test_obsolete_message

  subroutine test_multiple_cases_1
     call assert_true (.true.)
     call assert_true (.false.)
  end subroutine test_multiple_cases_1

  subroutine test_multiple_cases_2
     call assert_true (.true.)
  end subroutine test_multiple_cases_2

  subroutine demonstrate_case_summary
     call run_test_case(test_multiple_cases_1)
     call run_test_case(test_multiple_cases_2)
  end subroutine demonstrate_case_summary

end module fruit_test
