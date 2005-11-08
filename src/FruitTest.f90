!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module fruitTest

contains
  ! ----
  ! Run all the test sub routines 
  ! ----
  subroutine allFruitTest  
    implicit none;
    call initializeFruitTest;
    
    call markTest()
    
    call assertTrueResultTest;
    call assertTrueResultTest_invalid;
    call assertTrueResultMessageTest;
    
    call assertTrueTest;
    call assertTrueMessageTest;
    
    call addSuccessTest;
    call addSuccessMessageTest;
    call addFailTest;
    call addFailMessageTest;
    call getTestSummaryTest;
    call isAllSuccessfulTest;
    call showOutputForReport;
    
    call showOutputTest;
    
    call testAssertEqualsFloat;
  end subroutine allFruitTest
  
  ! ----
  subroutine initializeFruitTest  
    use fruit
    implicit none;

    call initializeFruit;
    return
  end subroutine initializeFruitTest
  
  subroutine markTest
    use fruit
    
    write (*,*) "Should see . here:"
    call successfulMark()
    write (*,*) "Should see .. here:"
    call successfulMark()
    call successfulMark()

    write (*,*) 

    write (*,*) "Should see F here:"
    call failedMark()
  
    write (*,*) "Should see F here:"
    call failedMark()
    call failedMark()
  
    write (*,*) 
  end subroutine markTest
  
  ! ----
  subroutine assertTrueResultTest 
    use fruit
    implicit none;
    logical :: inputValue;
    logical :: resultValue = .FALSE.;
    
    inputValue = .true.
    
    call assertTrue (inputValue, resultValue)
    
    IF (resultValue .eqv. .true.) then 
      write (*,*) 'assertTrueResultTest Successful'
    else 
      write (*,*) 'assertTrueResultTest FAILED!!!'
    end IF
    
    return
  end subroutine assertTrueResultTest
  
  ! ----
  subroutine assertTrueResultTest_invalid
    use fruit
    implicit none;
    logical :: inputValue;
    logical :: resultValue = .FALSE.;
    
    inputValue = .FALSE.
    
    call assertTrue (inputValue, resultValue)
    
    IF (resultValue .neqv. .true.) then 
      write (*,*) 'assertTrueResultTest_invalid Successful'
    else 
      write (*,*) 'assertTrueResultTest_invalid FAILED!!!'
    end IF
    
    return
  end subroutine assertTrueResultTest_invalid

  !------------
  ! Test assert with result and message
  !------------  
  subroutine assertTrueResultMessageTest
    use fruit
    implicit none;
    logical :: inputValue;
    logical :: resultValue;
    
    inputValue = .true.
    resultValue = .FALSE.
    
    call assertTrue (inputValue, 'Test assertTrue (input, msg, result) message.', resultValue)
    
    IF (resultValue .eqv. .true.) then 
      write (*,*) 'assertTrueResultMessageTest Successful'
    else 
      write (*,*) 'assertTrueResultMessageTest FAILED!!!'
    end IF
    
    write (*,*) 'Should see 1 successful case';
    call getTestSummary
    
    return
  end subroutine assertTrueResultMessageTest
  
  ! ----
  ! Test assertTrue
  ! ----
  subroutine assertTrueTest 
    use fruit
    implicit none;

    write (*,*) 'Should see 1 successful case';
    call initializeFruit;
    call assertTrue (.true.);
    call getTestSummary

    write (*,*) 'Shoule see 1 failed case';
    call initializeFruit;
    call assertTrue (.FALSE.);
    call getTestSummary
    
    return
  end subroutine assertTrueTest
  
  ! ----
  ! Test assertTrue with message
  ! ----
  subroutine assertTrueMessageTest 
    use fruit
    implicit none;
    
    write (*,*) 'Should see 1 successful case';
    call initializeFruit;
    call assertTrue (.true., 'message in assertTrueTest_message true test');
    call getTestSummary

    write (*,*) 'Should see 1 failed case';
    call initializeFruit;
    call assertTrue (.FALSE., 'message in assertTrueTest_message false test');
    call getTestSummary
    
    return
  end subroutine assertTrueMessageTest
  
  ! -----
  ! Test addSuccess and isAllSuccessful
  ! -----
  subroutine addSuccessTest
    use fruit
    implicit none;
    
    logical :: result = .FALSE.

    call initializeFruit;
    call addSuccess
    call isAllSuccessful (result);

    IF (result .neqv. .true.) then
      write (*, *);
      write (*, *) 'FAILED addSuccess!!!';
      write (*, *);
    end IF
    
  end subroutine addSuccessTest
  
  subroutine addSuccessMessageTest
    use fruit
    implicit none;

    call addSuccess ('Success in this subroutine: addSuccessMessageTest');
    
    return
  end subroutine addSuccessMessageTest
  
  ! -----
  ! Test addSuccess and isAllSuccessful
  ! -----
  subroutine addFailTest
    use fruit
    implicit none;
    
    logical :: result = .FALSE.

    call initializeFruit;
    call addFail
    call isAllSuccessful (result);

    IF (result .neqv. .FALSE.) then
      write (*, *);
      write (*, *) 'FAILED addFail !!!';
      write (*, *);
    end IF
    
  end subroutine addFailTest
  
  ! -----
  ! Test addSuccess and isAllSuccessful
  ! -----
  subroutine addFailMessageTest
    use fruit
    implicit none;
    
    logical :: result = .FALSE.

    call initializeFruit;
    call addFail ('Add a failed case');
    call isAllSuccessful (result);

    IF (result .neqv. .FALSE.) then
      write (*, *);
      write (*, *) 'FAILED addFail !!!';
      write (*, *);
    end IF
    
  end subroutine addFailMessageTest
  
  ! ----
  ! Test successful and failed summary
  ! ----
  subroutine getTestSummaryTest  
    use fruit
    implicit none;
    
    write (*,*) 'Summary for successful cases without message: ';
    write (*,*) 'Should see 1 successful case';
    call initializeFruit;
    call addSuccess;
    call getTestSummary
    
    write (*,*) 'Summary for successful cases and 2 messages: ';
    write (*,*) 'Should see 3 successful cases';
    call initializeFruit;
    call addSuccess;
    call addSuccess ('Success message 1 from test case.');
    call addSuccess ('Success message 2 from test case.');
    call getTestSummary
    
    write (*,*) 'Summary for failed cases: ';
    write (*,*) 'Should see 2 failed case amd 1 message';
    call initializeFruit;
    call addFail;
    call addFail('Fail message from test case.');
    call getTestSummary
    
    return
  end subroutine getTestSummaryTest
  
  ! ----
  ! Add one failed case and assert false
  ! Add all success case, and assert true
  ! ----
  subroutine isAllSuccessfulTest  
    use fruit
    implicit none;
    
    logical :: result = .true.
    
    call initializeFruit;
    call addSuccess
    call addFail
    call addSuccess
    call isAllSuccessful (result);
    
    IF (result .neqv. .FALSE.) then
      write (*, *);
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addFail)';
      write (*, *);
    end IF

    call initializeFruit;
    call addSuccess
    call addSuccess
    call addSuccess
    call isAllSuccessful (result);
    
    IF (result .neqv. .true.) then
      write (*, *);
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)';
      write (*, *);
    end IF

    call initializeFruit;
    IF (result .neqv. .true.) then
      write (*, *);
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)';
      write (*, *);
    end IF

    return
  end subroutine isAllSuccessfulTest
  
  subroutine showOutputTest 
    use fruit
    implicit none;
    logical :: trueValue = .TRUE.;
    logical :: falseValue = .FALSE.;
    integer :: i;
    integer :: count;
    
    DO i=1,100
      call assertTrue (trueValue)
    END DO
    
    DO i=1,100
      call assertTrue (falseValue)
    END DO
    
    return
  end subroutine showOutputTest
  
  subroutine showOutputForReport 
    use fruit
    implicit none;
    logical :: trueValue = .TRUE.;
    logical :: falseValue = .FALSE.;
    integer :: i;
    integer :: count;
    character (100), DIMENSION (3)  :: msgs; 
    
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
    !call getMessages (msgs);
    write (*, *) 'Messages are: '; 
    write (*, *) msgs; 
    
    return
  end subroutine showOutputForReport
  
  subroutine testAssertEqualsFloat
    use fruit
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call initializeFruit;
    call assertEquals (variable, result);
    call assertEquals (variable + 0.1, result);
    call getTestSummary;
    
  end subroutine testAssertEqualsFloat
  
end module  