!------------------------
! FORTRAN unit test utility
!
! Author: Andrew H. Chen chena@westinghouse.com
! Modified for use by BAI:  B. Frank, and likely others
!------------------------
!!
! Unit test framework for FORTRAN.  (FoRtran UnIT)
!
! This package is to perform unit test for FORTRAN subroutines
!
! The method used most is: call assertTrue (logical [, message]);
!
!  Things that I think need to be done here (BRF):
!    1) the messageArray stack should be turned into a linked list,
!       so we don't hae to worry about hitting some upper limit
!       Of course, an upper limit could be set internally.
!    2) Figure out what type testError is here for - use it or not?
!    3) Is there a way to hanle the message strings better?
!    4) Make available some public variables like maybe failedAssertCount
!       Then, the test program could check this and stop with a non-zero
!       return code so Unix scripts can check on it
!    5) At the place where there is a STOP in FRUIT, use a non-zero 
!       return code.
!    6) Make source conform to the BAI coding standard? (Upper case
!       keywords?)
!    7) Here or in a utility mod/library, add a float equals function.
!    8) Add some "array equals" functions for real and integer arrays of
!       arbitrary rank
!    9) Look at what's provided by Junit and provide that if posible
!       This might include assertEquals?
!   10) Maybe read some options from a "fruit.ini" type file - like the
!       maximum number of messages to allow before exiting.
!   11) Provide an option to generate a simple HTML page showing the success
!       or failure data.
!
!
module fruit

  type testError
    integer :: status;
    character (len=30) :: message;
  end type testError
  
  integer, parameter :: MSG_LENGTH = 1000
  integer, parameter :: MSG_STACK_SIZE = 150
  
  integer, private, save :: successfulAssertCount = 0;
  integer, private, save :: failedAssertCount = 0;
  character (len = MSG_LENGTH), private, DIMENSION (MSG_STACK_SIZE), save :: messageArray; 
!   This here "stack" should be turned into a linked list structure. I can do that sometime - BRF  YOYO
  integer, private, save :: messageIndex = 1;
  
  !-------
  ! Assert true methods
  !-------
  interface assertTrue
      module procedure assertTrue_single;
      module procedure assertTrue_result;
      module procedure assertTrue_result_message;
      module procedure assertTrue_single_message;
  end interface
  
  interface assertEquals

      module procedure assertEqualsString

      module procedure assertEqualsLogical

      module procedure assertEquals_single_int
      module procedure assertEquals_1darray_int
      module procedure assertEquals_2darray_int

      module procedure assertEquals_single_real
      module procedure assertEquals_1darray_real
      module procedure assertEquals_2darray_real

      module procedure assertEquals_single_double
      module procedure assertEquals_1darray_double
      module procedure assertEquals_2darray_double

      module procedure assertEqualsStringMessage

      module procedure assertEqualsLogicalMessage

      module procedure assertEquals_single_int_message
      module procedure assertEquals_1darray_int_message
      module procedure assertEquals_2darray_int_message

      module procedure assertEquals_single_real_message
      module procedure assertEquals_single_single_single_message
      module procedure assertEquals_spArr_spArr_int_sp_message
      module procedure assertEquals_1darray_real_message
      module procedure assertEquals_2darray_real_message

      module procedure assertEquals_double_double_double_message
      module procedure assertEquals_single_double_message
      module procedure assertEquals_dpArr_dpArr_int_dp_message
      module procedure assertEquals_1darray_double_message
      module procedure assertEquals_2darray_double_message
  end interface
  
  interface addSuccess
    module procedure addSuccess_no_message;
    module procedure addSuccess_message;
    module procedure addSuccess_UnitNameMessage
  end interface
  
  interface addFail
    module procedure addFail_no_message;
    module procedure addFail_message;
    module procedure addFail_UnitNameMessage
  end interface
  
  !------------
  ! Retrieve number of total cases.  Used for future report generator
  !------------
  interface getTotalCount
    module procedure getTotalCount
  end interface
  
  !------------
  ! Retrieve number of failed cases.  Used for future report generator
  !------------
  interface getFailedCount
    module procedure getFailedCount
  end interface
  
  !-------
  ! Access definition, to protect private methods
  !-------
  private :: assertTrue_single, assertTrue_result, &
    assertTrue_result_message, assertTrue_single_message, &
    addSuccess_no_message, addSuccess_message, &
    addFail_no_message, addFail_message, increaseMessageStack;
    
contains

  ! ----
  ! Initialize all test modules
  ! ----
  subroutine initializeFruit
    successfulAssertCount = 0;
    failedAssertCount = 0;
    messageIndex = 1;
    write (*,*) 
    write (*,*) "Test module initialized"
    write (*,*) 
    write (*,*) "   . : successful assert,   F : failed assert "
    write (*,*) 
  end subroutine initializeFruit

  ! ----
  ! Assert the values are true, with return value
  ! print error messages and return error
  ! ----
  subroutine assertTrue_result (inputBoolValue, resultBoolValue)
    logical, intent (in) :: inputBoolValue
    logical, intent (out) :: resultBoolValue
    
    if ( inputBoolValue .eqv. .true.) then
      resultBoolValue = .true.
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      resultBoolValue = .false.
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertTrue_result
  
  ! ----
  ! Assert the values are true
  ! print error messages and return error
  ! ----
  subroutine assertTrue_result_message (inputBoolValue, message, resultBoolValue)
    logical, intent (in) :: inputBoolValue
    logical, intent (out) :: resultBoolValue
    character (*), intent (in) :: message;
    
    if ( inputBoolValue .eqv. .true.) then
      resultBoolValue = .true.
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      resultBoolValue = .false.
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
    
    call increaseMessageStack(message)
    
  end subroutine assertTrue_result_message
  
    ! ----
  ! Assert the string values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEqualsString (var1, var2)
    implicit none
    character(*), intent (in) :: var1, var2
   
    if ( TRIM(var1) == TRIM(var2)) then
      successfulAssertCount = successfulAssertCount + 1
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1
      call failedMark()
      call increaseMessageStack('Expected ' // TRIM(var1) // ' got ' // TRIM(var2))
    end if
  end subroutine assertEqualsString
  
  ! ----
  ! Assert the logical values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEqualsLogical (var1, var2)
    implicit none
    logical, intent (in)  :: var1, var2
   
    character(MSG_LENGTH) :: msg

    if ( var1 .EQV. var2 ) then
      successfulAssertCount = successfulAssertCount + 1
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1
      call failedMark()
      write(msg, '(A, L1, A, L1)') 'Expected ', var1, ' got ', var2
      call increaseMessageStack(msg)
    end if
  end subroutine assertEqualsLogical
  ! ----
  ! Assert the string values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEqualsStringMessage (var1, var2, message)
    implicit none
    character(*), intent (in)  :: var1, var2
    character (*), intent (in) :: message

    if ( TRIM( var1) == TRIM( var2)) then
      successfulAssertCount = successfulAssertCount + 1
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1
      call failedMark()
      call increaseMessageStack(TRIM(message) // ' (Expected ' // TRIM(var1) // ' got ' // TRIM(var2) // ')')
    end if

  end subroutine assertEqualsStringMessage

  ! ----
  ! Assert the logical values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEqualsLogicalMessage (var1, var2, message)
    implicit none
    logical, intent (in)  :: var1, var2
    character (*), intent (in) :: message

    character(MSG_LENGTH) :: msg

    if ( var1 .EQV. var2 ) then
      successfulAssertCount = successfulAssertCount + 1
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1
      call failedMark()
      write(msg, '(A, L1, A, L1)') 'Expected ', var1, ' got ', var2
      call increaseMessageStack(TRIM(message) // ' (' // TRIM(msg) // ')')
    end if
  end subroutine assertEqualsLogicalMessage

  ! ----
  ! Assert the values are true and message
  ! print error messages and return error
  ! ----
  subroutine assertTrue_single (inputBoolValue)
    implicit none;
    logical, intent (in) :: inputBoolValue
    
    if ( inputBoolValue .eqv. .true.) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
    call failedMark()
    end if
   
  end subroutine assertTrue_single
  
  ! ----
  ! Assert the values are true and message
  ! This subroutine is used most
  ! print error messages and return error
  ! ----
  subroutine assertTrue_single_message (inputBoolValue, message)
    implicit none;
    logical, intent (in) :: inputBoolValue
    character (*), intent (in) :: message;
    
    if ( inputBoolValue .eqv. .true.) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
  end subroutine assertTrue_single_message
  
  ! -----
  ! Just add one successful case
  ! -----
  subroutine addSuccess_no_message
    implicit none;
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
  end subroutine addSuccess_no_message
  
  ! -----
  ! Just add one successful case with message
  ! -----
  subroutine addSuccess_message (message)
    implicit none;
    character (*), intent (in) :: message;
    successfulAssertCount = successfulAssertCount + 1;
    
    call increaseMessageStack(message)
    call successfulMark()
    
  end subroutine addSuccess_message
  
  ! -----
  ! Just add one success case with message and subroutine name
  ! -----
  subroutine addSuccess_UnitNameMessage (unitName, message)
    character (*), intent (in) :: unitName
    character (*), intent (in) :: message

      call addSuccess_Message ("[in " //  unitName // "(ok)] : " //  message)

  end subroutine addSuccess_UnitNameMessage
  
  ! -----
  ! Just add one failed case
  ! -----
  subroutine addFail_no_message
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
  end subroutine addFail_no_message
  
  ! -----
  ! Just add one failed case
  ! -----
  subroutine addFail_message (message)
    character (*), intent (in) :: message;
    failedAssertCount = failedAssertCount + 1;
    call increaseMessageStack(message)
    call failedMark()
  end subroutine addFail_message
   
  ! -----
  ! Just add one failed case with message and subroutine name
  ! -----
  subroutine addFail_UnitNameMessage (unitName, message)
    character (*), intent (in) :: unitName
    character (*), intent (in) :: message

    call addFail_Message ("[in " //  unitName // "(fail)]: " //  message)

  end subroutine addFail_UnitNameMessage
 
  ! -----
  ! Just add one successful case
  ! -----
  subroutine isAllSuccessful (result)
    implicit none;
      logical, intent (out) :: result
      if ( failedAssertCount > 0 ) then
        result = .false.
      else 
        result = .true.
      end if
      
      return
  end subroutine isAllSuccessful
  
  ! -----
  ! Return summary of all tests in this instance
  ! -----
  subroutine getTestSummary
    implicit none;
    
    integer :: i;
  
    write (*,*) 
    write (*,*) 
    write (*,*) '    Start of FRUIT summary: '
    write (*,*) 
    
    if (failedAssertCount > 0) then
      write (*,*) 'Some tests failed!';
    else 
      write (*,*) 'SUCCESSFUL!';
    end if;
    
    !----------------
    ! Dump message stack
    !----------------
    if ( messageIndex > 1) then
      write (*,*) '  -- Messages are:';
    
      DO i = 1, messageIndex
        write (*,"(A)") TRIM(messageArray(i));
      end DO

      write (*,*) '  -- end of messages.';
    else 
      write (*,*) '  No messages ';
    end if;
      
    if (successfulAssertCount + failedAssertCount /= 0) then
      
      write (*,*) 'Total test run :   ', successfulAssertCount + failedAssertCount;
      write (*,*) 'Successful :       ', successfulAssertCount;
      write (*,*) 'Failed :           ', failedAssertCount;
      write (*,*) 'Successful rate:   ', real(successfulAssertCount) * 100.0 / real (successfulAssertCount + failedAssertCount), '%';
      write (*, *) 
      write (*, *) '  -- end of FRUIT summary'
      
    end if;
    
  end subroutine getTestSummary
  
  subroutine successfulMark
   write(*,"(A1)",ADVANCE='NO') '.'
  end subroutine successfulMark
  
  subroutine failedMark
   write(*,"(A1)",ADVANCE='NO') 'F'
  end subroutine failedMark
  
  subroutine increaseMessageStack (message)
    character(*), intent (in) :: message
    
    if (messageIndex > MSG_STACK_SIZE ) then
      write (*, *) "Too many errors to put into stack"
      call getTestSummary ()
      stop                     ! Ought to stop with a non-zero return code
                               ! so Unix scripts can catch it
    end if
    
    messageArray (messageIndex) = message;
    messageIndex = messageIndex + 1;
  end subroutine increaseMessageStack
  
  ! ----
  ! Assert the integer values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_int (var1, var2)
    implicit none;
    integer, intent (in) :: var1, var2
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_int
  
  ! ----
  ! Assert the integer 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_int (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_int
  
  ! ----
  ! Assert the integer 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_int (var1, var2, n, m)
    implicit none;
    integer, intent (in) :: n, m
    integer, intent (in) :: var1(n,m), var2(n,m)
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif

  end subroutine assertEquals_2darray_int
  
  ! ----
  ! Assert the real values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_real (var1, var2)
    implicit none;
    real, intent (in) :: var1, var2
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_real
  
  ! ----
  ! Assert the real 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_real (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_real
  
  ! ----
  ! Assert the real 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_real (var1, var2, n, m)
    implicit none;
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_2darray_real
  
  ! ----
  ! Assert the double precision values are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_single_single_message(var1, var2, var3, message_string)
    implicit none;
    real, intent (in) :: var1, var2, var3
    character(*), intent( in) :: message_string

    if ( abs( var1 - var2) .le. var3) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
      write(*,*) message_string   !YOYO, is there a better place to put this string?
    end if

  end subroutine assertEquals_single_single_single_message

  ! ----
  ! Assert the single precision arrays are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assertEquals_spArr_spArr_int_sp_message(var1, var2, n, var3, message_string)
    implicit none;
    integer, intent(in) :: n
    real, intent (in) :: var1(n), var2(n), var3
    character(*), intent( in) :: message_string

    if ( maxval( abs( var1 - var2)) .le. var3) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
      write(*,*) message_string   !YOYO, is there a better place to put this string?
    end if

  end subroutine assertEquals_spArr_spArr_int_sp_message

  ! ----
  ! Assert the double precision arrays are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assertEquals_dpArr_dpArr_int_dp_message(var1, var2, n, var3, message_string)
    implicit none;
    integer, intent(in) :: n
    double precision, intent (in) :: var1(n), var2(n), var3
    character(*), intent( in) :: message_string

    if ( maxval( abs( var1 - var2)) .le. var3) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
      write(*,*) message_string   !YOYO, is there a better place to put this string?
    end if

  end subroutine assertEquals_dpArr_dpArr_int_dp_message

  ! ----
  ! Assert the double precision values are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assertEquals_double_double_double_message(var1, var2, var3, message_string)
    implicit none;
    double precision, intent (in) :: var1, var2, var3
    character(*), intent( in) :: message_string

    if ( abs( var1 - var2) .le. var3) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
      write(*,*) message_string   !YOYO, is there a better place to put this string?
    end if

  end subroutine assertEquals_double_double_double_message

  ! ----
  ! Assert the double precision values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_double (var1, var2)
    implicit none;
    double precision, intent (in) :: var1, var2
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_double
  
  ! ----
  ! Assert the double precision 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_double (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    double precision, intent (in) :: var1(n), var2(n)
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_double
  
  ! ----
  ! Assert the double precision 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_double (var1, var2, n, m)
    implicit none;
    integer, intent (in) :: n, m
    double precision, intent (in) :: var1(n,m), var2(n,m)
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_2darray_double

  ! ----
  ! Assert the integer values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_int_message (var1, var2, message)
    implicit none;
    integer, intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_int_message
  
  ! ----
  ! Assert the integer 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_int_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_int_message
  
  ! ----
  ! Assert the integer 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_int_message (var1, var2, n, m, message)
    implicit none;
    integer, intent (in) :: n, m
    integer, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in) :: message;
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call increaseMessageStack(message)
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_2darray_int_message
  
  ! ----
  ! Assert the real values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_real_message (var1, var2, message)
    implicit none;
    real, intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_real_message
  
  ! ----
  ! Assert the real 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_real_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_real_message
  
  ! ----
  ! Assert the real 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_real_message (var1, var2, n, m, message)
    implicit none;
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in) :: message;
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call increaseMessageStack(message)
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_2darray_real_message
  
  ! ----
  ! Assert the double precision values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_double_message (var1, var2, message)
    implicit none;
    double precision, intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( var1 .eq. var2) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_double_message
  
  ! ----
  ! Assert the double precision 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_double_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    double precision, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
         exit loop_dim1
       end if
    end do loop_dim1
    
    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_1darray_double_message
  
  ! ----
  ! Assert the double precision 2-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_2darray_double_message (var1, var2, n, m, message)
    implicit none;
    integer, intent (in) :: n, m
    double precision, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in) :: message;
    
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
            failedAssertCount = failedAssertCount + 1;
            call increaseMessageStack(message)
            call failedMark()
            exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2
   
    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1;
       call successfulMark()
    endif
   
  end subroutine assertEquals_2darray_double_message

  ! ----
  ! Return total number of assert calls
  ! ----
  subroutine getTotalCount (count)
  implicit none
    integer, intent (out) :: count
    
    count = successfulAssertCount + failedAssertCount
   
  end subroutine getTotalCount

  ! ----
  ! Return number of failed assert calls
  ! ----
  subroutine getFailedCount (count)
  implicit none
    integer, intent (out) :: count
    
    count = failedAssertCount
   
  end subroutine getFailedCount

END module fruit
