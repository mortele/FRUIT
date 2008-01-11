!------------------------
! FORTRAN unit test utility
!
! Author: Andrew H. Chen meihome @at@ gmail.com
!------------------------
!
! Unit test framework for FORTRAN.  (FoRtran UnIT)
!
! This package is to perform unit test for FORTRAN subroutines
!
! The method used most is: call assertTrue (logical [, message])
!
!
!
module fruit

  integer, parameter :: MSG_LENGTH = 1000
  integer, parameter :: MSG_STACK_SIZE = 300000

  integer, private, save :: successfulAssertCount = 0
  integer, private, save :: failedAssertCount = 0
  character (len = MSG_LENGTH), private, dimension (MSG_STACK_SIZE), save :: messageArray
  integer, private, save :: messageIndex = 1

  integer, private, save :: successfulTestCount = 0
  integer, private, save :: failedTestCount = 0
  character (len = MSG_LENGTH), private, dimension (1000), save :: testCaseNamesArray
  character (len = MSG_LENGTH), private, dimension (1000), save :: testCaseMessageArray
  integer, private, save :: testCaseIndex = 1
  logical, private, save :: last_case_passed = .false.

  interface init_fruit
     module procedure init_fruit_
  end interface

  interface initializeFruit
     module procedure initialize_fruit_
  end interface

  interface fruit_summary
     module procedure fruit_summary_
  end interface

  interface getTestSummary
     module procedure get_test_summary_  
  end interface

  interface assertTrue
     module procedure assert_true_single
  end interface

  interface assertEquals
     module procedure assert_equals_string
     module procedure assert_equals_logical
     module procedure assert_equals_single_int
     module procedure assert_equals_1darray_int
     module procedure assert_equals_1darray_string

     module procedure assert_equals_2d_array_int_

     module procedure assert_equals_single_real_

     module procedure assert_equals_1d_array_real_

     module procedure assert_equals_2darray_real

     module procedure assert_equals_single_double
     module procedure assert_equals_1darray_double
     module procedure assert_equals_2darray_double

     module procedure assert_equals_single_single_single_message
     module procedure assert_equals_spArr_spArr_int_sp_message

     module procedure assert_equals_2darray_real_message
     module procedure assert_equals_double_double_double_message
     module procedure assert_equals_single_double_message
     module procedure assert_equals_dpArr_dpArr_int_dp_message
     module procedure assert_equals_1darray_double_message
     module procedure assert_equals_2darray_double_message
     module procedure assert_equals_1darray_complex
     module procedure assert_equals_1darray_complex_message
     module procedure assert_equals_2darray_complex
     module procedure assert_equals_2darray_complex_message
  end interface

  interface assertNotEquals
     module procedure assert_not_equals_single_real_
     module procedure assert_not_equals_1d_array_real_
     module procedure assertNotEquals_single_double
  end interface

  interface addSuccess
     module procedure add_success
     module procedure add_success_unit
  end interface

  interface addFail
     module procedure add_fail
     module procedure add_fail_unit
  end interface

  interface getTotalCount
     module procedure get_total_count_
  end interface

  interface getFailedCount
     module procedure getFailedCount
  end interface

  interface success_case_action
     module procedure success_case_action_
  end interface

  interface failed_case_action
     module procedure failed_case_action_
  end interface

  !  interface addTestCase
  !    module procedure addTestCase
  !  end interface

  !  interface addCaseResult
  !    module procedure addCaseResult
  !  end interface

  !  interface getTotalTestCount
  !    module procedure getTotalTestCount
  !  end interface

  !  interface getTotalFailedTestCount
  !    module procedure getTotalFailedTestCount
  !  end interface

  !  interface getTestCases
  !    module procedure getTestCases
  !  end interface

  !  interface getTestCaseResults
  !    module procedure getTestCaseResults
  !  end interface

  private :: init_fruit_, initialize_fruit_, fruit_summary_, get_test_summary_, &
       warning_, get_total_count_, assertTrue_single, assertTrue_result, &
       add_success, &
       add_fail, add_fail_unit, increaseMessageStack, &
       success_case_action_, failed_case_action_, success_mark_, failed_mark_, &
       assert_not_equals_single_real_, assert_equals_single_real_,  assert_not_equals_1d_array_real_, assert_equals_1d_array_real_, &
       assert_equals_2d_array_int_

contains

  subroutine init_fruit_
    successfulAssertCount = 0
    failedAssertCount = 0
    messageIndex = 1
    write (*,*)
    write (*,*) "Test module initialized"
    write (*,*)
    write (*,*) "   . : successful assert,   F : failed assert "
    write (*,*)
  end subroutine init_fruit_

  subroutine initialize_fruit_
    call warning_ ("initializeFruit is OBSOLETE.  replaced by init_fruit")
    call init_fruit
  end subroutine initialize_fruit_

  subroutine get_test_summary_
    call warning_ ( "getTestSummary is OBSOLETE.  replaced by fruit_summary")
    call fruit_summary
  end subroutine get_test_summary_

  subroutine fruit_summary_
    implicit none

    integer :: i

    write (*,*)
    write (*,*)
    write (*,*) '    Start of FRUIT summary: '
    write (*,*)

    if (failedAssertCount > 0) then
       write (*,*) 'Some tests failed!'
    else
       write (*,*) 'SUCCESSFUL!'
    end if

    !----------------
    ! Dump message stack
    !----------------
    if ( messageIndex > 1) then
       write (*,*) '  -- Messages are:'

       do i = 1, messageIndex - 1
          write (*,"(A)") trim(messageArray(i))
       end do

       write (*,*) '  -- end of messages.'
    else
       write (*,*) '  No messages '
    end if

    if (successfulAssertCount + failedAssertCount /= 0) then

       write (*,*) 'Total test run :   ', successfulAssertCount + failedAssertCount
       write (*,*) 'Successful :       ', successfulAssertCount
       write (*,*) 'Failed :           ', failedAssertCount
       write (*,'("Successful rate:   ",f6.2,"%")')  real(successfulAssertCount) * 100.0 / real (successfulAssertCount + failedAssertCount)
       write (*, *)
       write (*, *) '  -- end of FRUIT summary'

    end if
  end subroutine fruit_summary_

  subroutine assert_equals_string (var1, var2, message)
    implicit none
    character(*), intent (in)  :: var1, var2
    character (*), intent (in), optional :: message

    if ( trim( var1) == trim( var2)) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(trim(message) // ' (Expected ' // trim(var1) // ' got ' // trim(var2) // ')')
       else
          call increaseMessageStack(' (Expected ' // trim(var1) // ' got ' // trim(var2) // ')')
       end if
    end if
  end subroutine assert_equals_string

  subroutine assert_equals_logical (var1, var2, message)
    implicit none
    logical, intent (in)  :: var1, var2
    character (*), intent (in), optional :: message

    character(MSG_LENGTH) :: msg

    if ( var1 .eqv. var2 ) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          write(msg, '(A, L1, A, L1)') 'Expected ', var1, ' got ', var2
       endif
       call increaseMessageStack(trim(message) // ' (' // trim(msg) // ')')
    end if
  end subroutine assert_equals_logical

  subroutine assert_true_single (input, message)
    implicit none
    logical, intent (in) :: input
    character (*), intent (in), optional :: message

    if ( input .eqv. .true.) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(message)
       end if
    end if
  end subroutine assert_true_single

  subroutine add_success (message)
    implicit none
    character (*), intent (in), optional :: message
    if (present(message)) then
       call increaseMessageStack(message)
    end if
    call success_case_action_
  end subroutine add_success

  ! -----
  ! Just add one success case with message and subroutine name
  ! -----
  subroutine add_success_unit (unitName, message)
    character (*), intent (in) :: unitName
    character (*), intent (in) :: message

    call add_success ("[in " //  unitName // "(ok)] : " //  message)

  end subroutine add_success_unit

  subroutine add_fail (message)
    character (*), intent (in), optional :: message
    if (present(message)) then
       call increaseMessageStack(message)
    end if
    call failed_case_action_
  end subroutine add_fail

  subroutine add_fail_unit (unitName, message)
    character (*), intent (in) :: unitName
    character (*), intent (in) :: message

    call add_fail ("[in " //  unitName // "(fail)]: " //  message)
  end subroutine add_fail_unit

  ! -----
  ! Just add one successful case
  ! -----
  subroutine isAllSuccessful (result)
    implicit none
    logical, intent (out) :: result
    if ( failedAssertCount > 0 ) then
       result = .false.
    else
       result = .true.
    end if
  end subroutine isAllSuccessful

  subroutine success_mark_
    write(*,"(A1)",ADVANCE='NO') '.'
  end subroutine success_mark_

  subroutine failed_mark_
    write(*,"(A1)",ADVANCE='NO') 'F'
  end subroutine failed_mark_

  subroutine increaseMessageStack (message)
    character(*), intent (in) :: message

    if (messageIndex > MSG_STACK_SIZE ) then
       write (*, *) "Too many errors to put into stack"
       call getTestSummary ()
       stop 1
    end if

    messageArray (messageIndex) = message
    messageIndex = messageIndex + 1
  end subroutine increaseMessageStack

  subroutine assert_not_equals_1d_array_real_ (var1, var2, n)
    implicit none
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_case_action_
          return
       end if
    end do loop_dim1

    call success_case_action_

  end subroutine assert_not_equals_1d_array_real_

  subroutine assert_equals_2darray_real (var1, var2, n, m)
    implicit none
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_case_action
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call success_case_action_

  end subroutine assert_equals_2darray_real

  subroutine assert_equals_single_single_single_message(var1, var2, var3, message)
    implicit none
    real, intent (in) :: var1, var2, var3
    character(*), intent(in), optional :: message

    if ( abs( var1 - var2) .le. var3) then
       call success_case_action_
    else
       call failed_case_action
       if (present(message)) then
          call increaseMessageStack(message)
       end if
    end if

  end subroutine assert_equals_single_single_single_message

  ! ----
  ! Assert the single precision arrays are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assert_equals_spArr_spArr_int_sp_message(var1, var2, n, var3, message)
    implicit none
    integer, intent(in) :: n
    real, intent (in) :: var1(n), var2(n), var3
    character(*), intent(in), optional :: message

    if ( maxval( abs( var1 - var2)) .le. var3) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(message)
       end if
    end if
  end subroutine assert_equals_spArr_spArr_int_sp_message

  ! ----
  ! Assert the double precision arrays are equal within a tolerance
  ! print error messages and return error
  ! ----
  subroutine assert_equals_dpArr_dpArr_int_dp_message(var1, var2, n, var3, message)
    implicit none
    integer, intent(in) :: n
    double precision, intent (in) :: var1(n), var2(n), var3
    character(*), intent(in), optional :: message

    if ( maxval( abs( var1 - var2)) .le. var3) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(message)
       end if
    end if
  end subroutine assert_equals_dpArr_dpArr_int_dp_message

  subroutine assert_equals_double_double_double_message(var1, var2, var3, message)
    implicit none
    double precision, intent (in) :: var1, var2, var3
    character(*), intent(in), optional :: message

    if ( abs( var1 - var2) .le. var3) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(message)
       end if
    end if
  end subroutine assert_equals_double_double_double_message

  subroutine assert_equals_single_double (var1, var2)
    implicit none
    double precision, intent (in) :: var1, var2

    if ( var1 .eq. var2) then
       call success_case_action_
    else
       call failed_case_action_
    end if
  end subroutine assert_equals_single_double

  subroutine assertNotEquals_single_double (var1, var2)
    implicit none
    double precision, intent (in) :: var1, var2

    if ( var1 .ne. var2) then
       call success_case_action_
    else
       call failed_case_action_
    end if
  end subroutine assertNotEquals_single_double

  subroutine assert_equals_1darray_double (var1, var2, n)
    implicit none
    integer, intent (in) :: n
    double precision, intent (in) :: var1(n), var2(n)

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_case_action_
          return
       end if
    end do loop_dim1

    call success_case_action_
  end subroutine assert_equals_1darray_double

  subroutine assert_equals_2darray_double (var1, var2, n, m)
    implicit none
    integer, intent (in) :: n, m
    double precision, intent (in) :: var1(n,m), var2(n,m)

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_case_action_
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call success_case_action_
  end subroutine assert_equals_2darray_double

  subroutine assert_equals_single_int (var1, var2, message)
    implicit none
    integer, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .eq. var2) then
       call success_case_action_
    else
       call failed_case_action_
       if (present(message)) then
          call increaseMessageStack(message)
       endif
    end if
  end subroutine assert_equals_single_int

  subroutine assert_equals_1darray_int (var1, var2, n, message)
    implicit none
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_case_action_
          if (present(message)) then
             call increaseMessageStack(message)
          endif
          return
       end if
    end do loop_dim1

    call success_case_action_
  end subroutine assert_equals_1darray_int

  subroutine assert_equals_1darray_string (var1, var2, n, message)
    implicit none
    integer, intent (in) :: n
    character(*), intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message
    character (len=200) :: msg
    integer count

    loop_dim1: do count = 1, n
       if ( trim(var1(count)) .ne. trim(var2(count))) then
          if (present(message)) then
             call increaseMessageStack(message)
          end if

          write (msg,1000) count
1000      format(I5)

          msg = 'error at count: ' // trim(msg)  
          msg = msg // 'expected value: ' // trim(var1(count))
          msg = msg // 'real value: ' // trim(var2(count))

          call increaseMessageStack(msg)
          call failed_case_action_
          return
       end if
    end do loop_dim1

    call success_case_action_
  end subroutine assert_equals_1darray_string

  subroutine assert_equals_2d_array_int_ (var1, var2, n, m, message)
    implicit none
    integer, intent (in) :: n, m
    integer, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in), optional :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             if (present(message)) then
                call increaseMessageStack(message)
             end if
             call failed_case_action_
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call success_case_action_

  end subroutine assert_equals_2d_array_int_

  subroutine assert_equals_single_real_ (var1, var2, message)
    implicit none
    real, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .eq. var2) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       if (present(message)) then
          call increaseMessageStack(message)
       end if
       call failed_case_action_
    end if
  end subroutine assert_equals_single_real_

  subroutine assert_not_equals_single_real_ (var1, var2, message)
    implicit none
    real, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .ne. var2) then
       call success_case_action_
    else
       if (present(message)) then
          call increaseMessageStack(message)
       end if
       call failed_case_action_
    end if
  end subroutine assert_not_equals_single_real_

  subroutine assert_equals_1d_array_real_ (var1, var2, n, message)
    implicit none
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          if (present(message)) then
             call increaseMessageStack(message)
          end if
          call failed_case_action_
          return
       end if
    end do loop_dim1
    call success_case_action_
  end subroutine assert_equals_1d_array_real_

  subroutine assert_equals_2darray_real_message (var1, var2, n, m, message)
    implicit none
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in) :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             failedAssertCount = failedAssertCount + 1
             call increaseMessageStack(message)
             call failed_case_action_
             exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2

    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    endif

  end subroutine assert_equals_2darray_real_message

  ! ----
  ! Assert the double precision values are equal
  ! print error messages and return error
  ! ----
  subroutine assert_equals_single_double_message (var1, var2, message)
    implicit none
    double precision, intent (in) :: var1, var2
    character (*), intent (in) :: message

    if ( var1 .eq. var2) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    else
       failedAssertCount = failedAssertCount + 1
       call increaseMessageStack(message)
       call failed_case_action_
    end if

  end subroutine assert_equals_single_double_message

  subroutine assert_equals_1darray_double_message (var1, var2, n, message)
    implicit none
    integer, intent (in) :: n
    double precision, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          failedAssertCount = failedAssertCount + 1
          call increaseMessageStack(message)
          call failed_case_action_
          exit loop_dim1
       end if
    end do loop_dim1

    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    endif

  end subroutine assert_equals_1darray_double_message

  subroutine assert_equals_2darray_double_message (var1, var2, n, m, message)
    implicit none
    integer, intent (in) :: n, m
    double precision, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in) :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             failedAssertCount = failedAssertCount + 1
             call increaseMessageStack(message)
             call failed_case_action_
             exit loop_dim2
          end if
       end do loop_dim1
    end do loop_dim2

    if ((count2 > m) .and. (count1 > n)) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    endif

  end subroutine assert_equals_2darray_double_message

  subroutine assert_equals_1darray_complex (var1, var2, n)
    implicit none
    integer,          intent(IN) :: n
    double complex,   intent(IN) :: var1(n), var2(n)

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          failedAssertCount = failedAssertCount + 1
          call failed_case_action_
          exit loop_dim1
       end if
    enddo loop_dim1

    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    endif

  end subroutine assert_equals_1darray_complex

  subroutine assert_equals_1darray_complex_message (var1, var2, n, message)
    implicit none
    integer,          intent(IN) :: n
    double complex,   intent(IN) :: var1(n), var2(n)
    character (*),    intent(IN) :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          failedAssertCount = failedAssertCount + 1
          call increaseMessageStack(message)
          call failed_case_action_
          exit loop_dim1
       end if
    enddo loop_dim1

    if (count > n) then
       successfulAssertCount = successfulAssertCount + 1
       call success_case_action_
    endif

  end subroutine assert_equals_1darray_complex_message

  subroutine assert_equals_2darray_complex (var1, var2, n, m)
    implicit none
    integer,        intent(IN) :: n, m
    double complex, intent(IN) :: var1(n,m), var2(n,m)

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             failedAssertCount = failedAssertCount + 1
             call failed_case_action_
             exit loop_dim2
          endif
       enddo loop_dim1
    enddo loop_dim2

    if ((count2 > m) .and. (count1 > n)) then
       call success_case_action_
    endif

  end subroutine assert_equals_2darray_complex

  subroutine assert_equals_2darray_complex_message (var1, var2, n, m, message)
    implicit none
    integer,          intent(IN) :: n, m
    double complex,   intent(IN) :: var1(n,m), var2(n,m)
    character (*),    intent(IN) :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             failedAssertCount = failedAssertCount + 1
             call increaseMessageStack(message)
             call failed_case_action_
             exit loop_dim2
          endif
       enddo loop_dim1
    enddo loop_dim2

    if ((count2 > m) .and. (count1 > n)) then
       call success_case_action_
    endif

  end subroutine assert_equals_2darray_complex_message

  subroutine get_total_count_ (count)
    implicit none
    integer, intent (out) :: count

    count = successfulAssertCount + failedAssertCount

  end subroutine get_total_count_

  subroutine getFailedCount (count)
    implicit none
    integer, intent (out) :: count

    count = failedAssertCount

  end subroutine getFailedCount

  subroutine warning_ (message)
    character (*), intent (in), optional :: message
    write (*,*) "start: ---!!!--------WARNING from FRUIT------!!!----"
    write (*,*) message
    write (*,*) "end: ---!!!--------WARNING from FRUIT------!!!----"
  end subroutine warning_

  subroutine success_case_action_
    successfulAssertCount = successfulAssertCount + 1
    last_case_passed = .true.
    call success_mark_  
  end subroutine success_case_action_

  subroutine failed_case_action_
    failedAssertCount = failedAssertCount + 1
    last_case_passed = .false.
    call failed_mark_
  end subroutine failed_case_action_
end module fruit
