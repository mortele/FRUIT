!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module fruitTest
  use Fruit
  use DataType
  implicit none

contains
  ! Run all the test sub routines 
  ! -----------------------------
  subroutine allFruitTest  
    implicit none

    ! test printing and formats
    !--------------------------
    call progressMarkTest()
    
    call showOutputTest
    call getTestSummaryTest
    call showOutputForReport
    
    ! the following should give 100% pass
    !------------------------------------
    call assertTrueResultMessageTest
    call getTestSummary

    call assertTrueResultTest
    call assertTrueResultTest_invalid
    
    call assertTrueTest
    call assertTrueMessageTest
    
    call addSuccessTest
    call addSuccessMessageTest
    call addFailTest

    call addFailMessageTest
    call isAllSuccessfulTest

    ! logic test, below should all success
    call initializeFruit
    call testAssertEqualsFloat
    call testAssertEqualsInt
    call testAssertEqualsString

    call getTestSummary

  end subroutine allFruitTest
  
  subroutine progressMarkTest
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
  end subroutine progressMarkTest
  
  ! ----
  subroutine assertTrueResultTest 
    implicit none
    logical :: inputValue
    logical :: resultValue = .FALSE.
    
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
    implicit none
    logical :: inputValue
    logical :: resultValue = .FALSE.
    
    inputValue = .FALSE.
    
    call assertTrue (inputValue, resultValue)
    
    IF (resultValue .neqv. .true.) then 
      write (*,*) 'assertTrueResultTest_invalid Successful'
    else 
      write (*,*) 'assertTrueResultTest_invalid FAILED!!!'
    end IF
    
  end subroutine assertTrueResultTest_invalid

  !------------
  ! Test assert with result and message
  !------------  
  subroutine assertTrueResultMessageTest
    implicit none
    logical :: inputValue
    logical :: resultValue
    
    inputValue = .true.
    resultValue = .FALSE.
    
    call initializeFruit
    call assertTrue (inputValue, 'Test assertTrue (input, msg, result) message.', resultValue)
    
    IF (resultValue .eqv. .true.) then 
      write (*,*) 'assertTrueResultMessageTest Successful'
    else 
      write (*,*) 'assertTrueResultMessageTest FAILED!!!'
    end IF
    
    write (*,*) 'Should see 1 successful case'

    call addSuccess ('assertTrueResultMessageTest', 'success')
    call getTestSummary
    
  end subroutine assertTrueResultMessageTest
  
  ! Test assertTrue
  ! ---------------
  subroutine assertTrueTest 
    implicit none

    call initializeFruit
    write (*,*) 'Should see 1 successful case'
    call assertTrue (.true.)

    write (*,*) 'Shoule see 1 failed case'
    call assertTrue (.FALSE.)
    
    call getTestSummary
  end subroutine assertTrueTest
  
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
  subroutine addSuccessTest
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
    
  end subroutine addSuccessTest
  
  subroutine addSuccessMessageTest
    implicit none

    call initializeFruit
    call addSuccess ('Success in this subroutine: addSuccessMessageTest')
    
    call addSuccess ('addSuccessMessageTest', 'Success in this subroutine: addSuccessMessageTest')
    
    call getTestSummary
  end subroutine addSuccessMessageTest
  
  ! Test addSuccess and isAllSuccessful
  ! -----------------------------------
  subroutine addFailTest
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
    
    call addFail('addFailTest', 'testing addFail')
    call getTestSummary

  end subroutine addFailTest
  
  ! Test addSuccess and isAllSuccessful
  ! -----------------------------------
  subroutine addFailMessageTest
    implicit none
    
    logical :: result = .FALSE.

    call initializeFruit
    call addFail ('Add a failed case')
    call addFail ('addFailMessageTest', 'Add a failed case')
    call isAllSuccessful (result)

    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED addFail !!!'
      write (*, *)
    end IF

    call getTestSummary
  end subroutine addFailMessageTest
  
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
  subroutine isAllSuccessfulTest  
    implicit none
    
    logical :: result = .true.
    
    call initializeFruit
    call addSuccess
    call addFail
    call addSuccess
    call isAllSuccessful (result)
    
    IF (result .neqv. .FALSE.) then
      write (*, *)
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addFail)'
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
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)'
      write (*, *)
    end IF
    call getTestSummary

    call initializeFruit
    IF (result .neqv. .true.) then
      write (*, *)
      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)'
      write (*, *)
    end IF
    call getTestSummary

  end subroutine isAllSuccessfulTest
  
  subroutine showOutputTest 
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
    
  end subroutine showOutputTest
  
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
  
  subroutine testAssertEqualsFloat
    implicit none
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call assertEquals (variable, result)
    call assertEquals (variable, result, 'floats are equal')
    call assertEquals (variable + 0.1, result, 'expected failure')
    
  end subroutine testAssertEqualsFloat
  
  subroutine testAssertEqualsInt
    implicit none
    
    integer :: variable = 1
    integer :: result = 1
    
    call assertEquals (variable, result)
    call assertEquals (variable, result, 'int are equal')
    call assertEquals (variable + 1, result, 'expected failure')
    call assertEquals (1, 1)
    call assertEquals (1, 1, '1 and 1')
    call assertEquals (1, 2, 'expected failure, 1 and 2')
    
  end subroutine 

  ! TODO, 
  subroutine testAssertEqualsString
    implicit none
    
    character (len=2):: variable = "m "
    character (len=1):: result = "m"
    
    call assertEquals (variable, result)
    call assertEquals (variable, "m")
    call assertEquals (variable, result, 'sring are equal')
  end subroutine 

end module  

