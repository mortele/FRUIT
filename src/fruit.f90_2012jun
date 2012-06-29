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
! The method used most are: assert_true, assert_equals
!
! Coding convention:
!   1) All methods must be exposed by interface.  i.e. interface init_fruit
!   2) Variable and methods are lower case connected with underscores.  i.e. init_fruit, and
!      failed_assert_count
!
module fruit
  use fruit_util
  implicit none
  private

  integer, parameter ::  STDOUT_DEFAULT = 6
  integer :: stdout = STDOUT_DEFAULT

  integer, parameter :: XML_OPEN = 20
  integer, parameter :: XML_WORK = 21
  integer, parameter :: NUMBER_LENGTH = 10

  integer, private, save :: message_index = 1
  integer, private, save :: message_index_from = 1
  integer, private, save :: current_max = 50

  character (len = *), parameter :: xml_filename     = "result.xml"
  character (len = *), parameter :: xml_filename_work = "result_tmp.xml"

  integer, parameter :: MSG_LENGTH = 256
  integer, parameter :: MAX_MSG_STACK_SIZE = 2000
  integer, parameter :: MSG_ARRAY_INCREMENT = 50
  integer, parameter :: MAX_MARKS_PER_LINE = 78

  character(*), parameter :: DEFAULT_CASE_NAME = '_not_set_'

  integer, private, save :: successful_assert_count = 0
  integer, private, save :: failed_assert_count = 0
  character (len = MSG_LENGTH), private, allocatable :: message_array(:)
  character (len = MSG_LENGTH), private, save :: msg = '[unit name not set from set_name]: '
  character (len = MSG_LENGTH), private, save :: case_name  = DEFAULT_CASE_NAME

  integer, private, save :: successful_case_count = 0
  integer, private, save :: failed_case_count = 0
  integer, private, save :: testCaseIndex = 1
  logical, private, save :: last_passed = .false.
  logical, private, save :: case_passed = .false.

  integer, private, save :: case_time_from = 0

  type ty_stack
    integer :: successful_assert_count
    integer :: failed_assert_count

    integer :: message_index
    integer :: message_index_from
    integer :: current_max
    character (len = MSG_LENGTH), pointer :: message_array(:)

    integer :: successful_case_count
    integer :: failed_case_count
    integer :: testCaseIndex

    character (len = MSG_LENGTH) :: case_name !  = DEFAULT_CASE_NAME
    logical :: last_passed
    logical :: case_passed
    integer :: case_time_from
  end type ty_stack
  type(ty_stack), save :: stashed_suite

  public :: &
    init_fruit, initializeFruit, fruit_summary, getTestSummary, get_last_message, &
    is_last_passed, assert_true, assertTrue, assert_equals, assertEquals, &
    is_case_passed, &
    init_fruit_xml, &
    fruit_summary_xml, &
    case_passed_xml, &
    case_failed_xml, &
    assert_not_equals, assertNotEquals, add_success, addSuccess, &
    addFail, add_fail, &
    set_unit_name, get_unit_name, &
    set_case_name, get_case_name, &
    failed_assert_action, get_total_count, getTotalCount, &
    get_failed_count, getFailedCount, is_all_successful, isAllSuccessful, &
    run_test_case, runTestCase
  public :: override_stdout, end_override_stdout
  public :: stash_test_suite, restore_test_suite
  public :: get_messages

  interface initializeFruit
     module procedure obsolete_initializeFruit_
  end interface

  interface getTestSummary
     module procedure obsolete_getTestSummary_
  end interface

  interface assertTrue
     module procedure obsolete_assert_true_logical_
  end interface

  interface assert_equals
     module procedure assert_eq_int_
     module procedure assert_eq_double_
     module procedure assert_eq_real_
     module procedure assert_eq_logical_
     module procedure assert_eq_string_
     module procedure assert_eq_complex_
     module procedure assert_eq_real_in_range_
     module procedure assert_eq_double_in_range_

     module procedure assert_eq_1d_int_
     module procedure assert_eq_1d_double_
     module procedure assert_eq_1d_real_
     module procedure assert_eq_1d_string_
     module procedure assert_eq_1d_complex_
     module procedure assert_eq_1d_real_in_range_
     module procedure assert_eq_1d_double_in_range_

     module procedure assert_eq_2d_int_
     module procedure assert_eq_2d_double_
     module procedure assert_eq_2d_real_
     module procedure assert_eq_2d_complex_
  end interface

  interface assertEquals
     module procedure assert_eq_int_
     module procedure assert_eq_double_
     module procedure assert_eq_real_
     module procedure assert_eq_logical_
     module procedure assert_eq_string_
     module procedure assert_eq_complex_
     module procedure assert_eq_real_in_range_
     module procedure assert_eq_double_in_range_

     module procedure assert_eq_1d_int_
     module procedure assert_eq_1d_double_
     module procedure assert_eq_1d_real_
     module procedure assert_eq_1d_string_
     module procedure assert_eq_1d_complex_
     module procedure assert_eq_1d_real_in_range_
     module procedure assert_eq_1d_double_in_range_

     module procedure assert_eq_2d_int_
     module procedure assert_eq_2d_double_
     module procedure assert_eq_2d_real_
     module procedure assert_eq_2d_complex_
  end interface

  interface assert_not_equals
     module procedure assert_not_equals_int_
     module procedure assert_not_equals_real_
     module procedure assert_not_equals_1d_real_
     module procedure assert_not_equals_double_
  end interface

  interface assertNotEquals
     module procedure assert_not_equals_real_
     module procedure assert_not_equals_1d_real_
     module procedure assert_not_equals_double_
  end interface

  interface addSuccess
     module procedure obsolete_addSuccess_
  end interface

  interface add_fail
     module procedure add_fail_
     module procedure add_fail_unit_
  end interface

  interface addFail
     module procedure add_fail_
     module procedure add_fail_unit_
  end interface

  interface getTotalCount
     module procedure obsolete_getTotalCount_
  end interface

  interface getFailedCount
     module procedure obsolete_getFailedCount_
  end interface

  interface isAllSuccessful
     module procedure obsolete_isAllSuccessful_
  end interface

  interface run_test_case
     module procedure run_test_case_
     module procedure run_test_case_named_
  end interface

  interface runTestCase
     module procedure run_test_case_
     module procedure run_test_case_named_
  end interface

  interface init_fruit_xml
    module procedure init_fruit_xml_
  end interface

  interface fruit_summary_xml
    module procedure fruit_summary_xml_
  end interface

  interface case_passed_xml
    module procedure case_passed_xml_
  end interface

  interface case_failed_xml
    module procedure case_failed_xml_
  end interface

  interface override_stdout
    module procedure override_stdout_
  end interface

  interface end_override_stdout
    module procedure end_override_stdout_
  end interface


  interface get_messages
    module procedure get_messages_
  end interface

  interface set_unit_name
    module procedure set_case_name_
  end interface
  interface set_case_name
    module procedure set_case_name_
  end interface

  interface get_unit_name
    module procedure get_case_name_
  end interface
  interface get_case_name
    module procedure get_case_name_
  end interface
contains

  subroutine init_fruit
    successful_assert_count = 0
    failed_assert_count = 0
    message_index = 1
    message_index_from = 1
    write (stdout,*)
    write (stdout,*) "Test module initialized"
    write (stdout,*)
    write (stdout,*) "   . : successful assert,   F : failed assert "
    write (stdout,*)
    if ( .not. allocated(message_array) ) then
      allocate(message_array(MSG_ARRAY_INCREMENT))
    end if
  end subroutine init_fruit

  subroutine init_fruit_xml_
    open (XML_OPEN, file = xml_filename)
    write(XML_OPEN, '("<?xml version=""1.0"" encoding=""UTF-8""?>")')
    write(XML_OPEN, '("<testsuites>")')
    close(XML_OPEN)
    open (XML_WORK, FILE = xml_filename_work, status='replace')
    close (XML_WORK)
  end subroutine init_fruit_xml_

  function  case_delta_t()
    integer, parameter :: STRLEN_T = 12
    character(len = STRLEN_T) :: case_delta_t
    real :: delta_t
    integer :: case_time_to, time_rate, time_max

    call system_clock(case_time_to, time_rate, time_max)
    if (time_rate > 0) then
      delta_t = real(case_time_to - case_time_from) / real(time_rate)
      if (delta_t < 0) then
        delta_t = delta_t + real(time_max) / real(time_rate)
      endif
    else
      delta_t = 0
    endif

    write(case_delta_t, '(g12.4)') delta_t
    case_delta_t = adjustl(case_delta_t)
  end function case_delta_t

  subroutine case_passed_xml_(tc_name, classname)
    character(*), intent(in) :: tc_name
    character(*), intent(in) :: classname

    open (XML_WORK, FILE = xml_filename_work, position='append')
    write(XML_WORK, &
   &  '("    <testcase name=""", a, """ classname=""", a, """ time=""", a, """/>")') &
   &  trim(tc_name), trim(classname), trim(case_delta_t())
    close (XML_WORK)
  end subroutine case_passed_xml_

  subroutine case_failed_xml_(tc_name, classname)
    character(*), intent(in) :: tc_name
    character(*), intent(in) :: classname
    integer :: i

    open (XML_WORK, FILE = xml_filename_work, position='append')
    write(XML_WORK, &
   &  '("    <testcase name=""", a, """ classname=""", a, """ time=""", a, """>")') &
   &  trim(tc_name), trim(classname), trim(case_delta_t())

    write(XML_WORK, '("      <failure type=""failure"" message=""")', advance = "no")
    do i = message_index_from, message_index - 1
      write(XML_WORK, '(a)', advance = "no") trim(strip(message_array(i)))
      if (i == message_index - 1) then
        continue
      else
        write(XML_WORK, '("&#xA;")', advance="no")
      endif
    enddo
    write(XML_WORK, '("""/>")')

    write(XML_WORK, &
   &  '("    </testcase>")')
    close(XML_WORK)
  end subroutine case_failed_xml_

  subroutine fruit_summary_xml_
    character(len = 1000) :: whole_line

    open (XML_OPEN, FILE = xml_filename, position='append')
    write(XML_OPEN, '("  <testsuite errors=""0"" ")', advance = "no")
    write(XML_OPEN, '("tests=""", a, """ ")', advance = "no") &
   &  trim(int_to_str(successful_case_count + failed_case_count))
    write(XML_OPEN, '("failures=""", a, """ ")', advance = "no") &
   &  trim(int_to_str(failed_case_count))
    write(XML_OPEN, '("name=""", a, """ ")', advance = "no") &
   &  "name of test suite"
    write(XML_OPEN, '("id=""1"">")')

    open (XML_WORK, FILE = xml_filename_work)
    do
      read(XML_WORK, '(a)', end = 999) whole_line
      write(XML_OPEN, '(a)') trim(whole_line)
    enddo
999 continue
    close(XML_WORK)

    write(XML_OPEN, '("  </testsuite>")')
    write(XML_OPEN, '("</testsuites>")')
    close(XML_OPEN)
  end subroutine fruit_summary_xml_

  function int_to_str(i)
    integer, intent(in) :: i
    character(LEN = NUMBER_LENGTH) :: int_to_str

    write(int_to_str, '(i10)') i
    int_to_str = adjustl(int_to_str)
  end function int_to_str

  subroutine obsolete_initializeFruit_
    call obsolete_ ("initializeFruit is OBSOLETE.  replaced by init_fruit")
    call init_fruit
  end subroutine obsolete_initializeFruit_

  subroutine obsolete_getTestSummary_
    call obsolete_ ( "getTestSummary is OBSOLETE.  replaced by fruit_summary")
    call fruit_summary
  end subroutine obsolete_getTestSummary_

  ! Run a named test case
  subroutine run_test_case_named_( tc, tc_name )
    interface
       subroutine tc()
       end subroutine
    end interface
    character(*), intent(in) :: tc_name

    integer :: initial_failed_assert_count

    initial_failed_assert_count = failed_assert_count

    ! Set the name of the unit test
    call set_case_name( tc_name )

    last_passed = .true.
    case_passed = .true.
    message_index_from = message_index
    call system_clock(case_time_from)

    call tc()

    if ( initial_failed_assert_count .eq. failed_assert_count ) then
       ! If no additional assertions failed during the run of this test case
       ! then the test case was successful
       successful_case_count = successful_case_count+1
    else
       failed_case_count = failed_case_count+1
       case_passed = .false.
    end if

    testCaseIndex = testCaseIndex+1

    ! Reset the name of the unit test back to the default
    call set_case_name( DEFAULT_CASE_NAME )

  end subroutine run_test_case_named_

  ! Run an 'unnamed' test case
  subroutine run_test_case_( tc )
    interface
       subroutine tc()
       end subroutine
    end interface

    call run_test_case_named_( tc, '_unnamed_' )

  end subroutine run_test_case_

  subroutine fruit_summary
    integer :: i

    write (stdout,*)
    write (stdout,*)
    write (stdout,*) '    Start of FRUIT summary: '
    write (stdout,*)

    if (failed_assert_count > 0) then
       write (stdout,*) 'Some tests failed!'
    else
       write (stdout,*) 'SUCCESSFUL!'
    end if

    write (stdout,*)
    if ( message_index > 1) then
       write (stdout,*) '  -- Failed assertion messages:'

       do i = 1, message_index - 1
          write (stdout,"(A)") '   '//trim(strip(message_array(i)))
       end do

       write (stdout,*) '  -- end of failed assertion messages.'
       write (stdout,*)
    else
       write (stdout,*) '  No messages '
    end if

    if (successful_assert_count + failed_assert_count /= 0) then

       write (stdout,*) 'Total asserts :   ', successful_assert_count + failed_assert_count
       write (stdout,*) 'Successful    :   ', successful_assert_count
       write (stdout,*) 'Failed        :   ', failed_assert_count
       write (stdout,'("Successful rate:   ",f6.2,"%")')  real(successful_assert_count) * 100.0 / &
            real (successful_assert_count + failed_assert_count)
       write (stdout, *)
       write (stdout,*) 'Successful asserts / total asserts : [ ',&
            successful_assert_count, '/', successful_assert_count + failed_assert_count, ' ]'
       write (stdout,*) 'Successful cases   / total cases   : [ ', successful_case_count, '/', &
            successful_case_count + failed_case_count, ' ]'
       write (stdout, *) '  -- end of FRUIT summary'

    end if
  end subroutine fruit_summary

  subroutine obsolete_addSuccess_
    call obsolete_ ("addSuccess is OBSOLETE.  replaced by add_success")
    call add_success
  end subroutine obsolete_addSuccess_

  subroutine add_fail_ (message)
    character (*), intent (in), optional :: message
    call failed_assert_action('none', 'none', message)
  end subroutine add_fail_

  subroutine add_fail_unit_ (unitName, message)
    character (*), intent (in) :: unitName
    character (*), intent (in) :: message

    call add_fail_ ("[in " //  unitName // "(fail)]: " //  message)
  end subroutine add_fail_unit_

  subroutine obsolete_isAllSuccessful_(result)
    logical, intent(out) :: result
    call obsolete_ ('subroutine isAllSuccessful is changed to function is_all_successful.')
    result = (failed_assert_count .eq. 0 )
  end subroutine obsolete_isAllSuccessful_

  subroutine is_all_successful(result)
    logical, intent(out) :: result
    result= (failed_assert_count .eq. 0 )
  end subroutine is_all_successful

  ! Private, helper routine to wrap lines of success/failed marks
  subroutine output_mark_( chr )
    character(1), intent(in) :: chr
    integer, save :: linechar_count = 0

    linechar_count = linechar_count + 1
    if ( linechar_count .lt. MAX_MARKS_PER_LINE ) then
       write(stdout,"(A1)",ADVANCE='NO') chr
    else
       write(stdout,"(A1)",ADVANCE='YES') chr
       linechar_count = 0
    endif

  end subroutine output_mark_

  subroutine success_mark_
    call output_mark_( '.' )
  end subroutine success_mark_

  subroutine failed_mark_
    call output_mark_( 'F' )
  end subroutine failed_mark_

  subroutine increase_message_stack_
    character(len=MSG_LENGTH) :: msg_swap_holder(current_max)

    if (message_index > MAX_MSG_STACK_SIZE) then
       write(stdout,*) "Stop because there are too many error messages to put into stack."
       write(stdout,*) "Try to increase MAX_MSG_STACK_SIZE if you really need so."
       call getTestSummary ()
       stop 1
    end if

    if (message_index > current_max) then
      msg_swap_holder(1:current_max) = message_array(1:current_max)
      deallocate(message_array)
      current_max = current_max + MSG_ARRAY_INCREMENT
      allocate(message_array(current_max))
      message_array(1:current_max - MSG_ARRAY_INCREMENT) &
                   = msg_swap_holder(1: current_max - MSG_ARRAY_INCREMENT)
    end if

    message_array (message_index) = msg
    message_index = message_index + 1
  end subroutine increase_message_stack_

  function get_last_message()
    character(len=MSG_LENGTH) :: get_last_message
    if (message_index > 1) then
       get_last_message = strip(message_array(message_index-1), MSG_LENGTH)
    else
       get_last_message = ''
    end if
  end function get_last_message

  subroutine get_messages_(msgs)
    character(len = *), intent(out) :: msgs(:)
    integer :: i, j

    msgs(:) = ""
    do i = message_index_from, message_index - 1
      j = i - message_index_from + 1
      if (j > ubound(msgs, 1)) exit
      msgs(j) = trim(strip(message_array(i)))
    enddo
  end subroutine get_messages_

  subroutine obsolete_getTotalCount_ (count)
    integer, intent (out) :: count
    call obsolete_ (' getTotalCount subroutine is replaced by function get_total_count')
    call get_total_count(count)
  end subroutine obsolete_getTotalCount_

  subroutine get_total_count(count)
    integer, intent(out) :: count

    count = successful_assert_count + failed_assert_count
  end subroutine get_total_count

  subroutine obsolete_getFailedCount_ (count)
    integer, intent (out) :: count

    call obsolete_ (' getFailedCount subroutine is replaced by function get_failed_count')
    call get_failed_count (count)

  end subroutine obsolete_getFailedCount_

  subroutine get_failed_count (count)
    integer, intent(out) :: count
    count = failed_assert_count
  end subroutine get_failed_count

  subroutine obsolete_ (message)
    character (*), intent (in), optional :: message
    write (stdout,*)
    write (stdout,*) "<<<<<<<<<<<<<<<<<<<<<<<<<< WARNING from FRUIT >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write (stdout,*) message
    write (stdout,*)
    write (stdout,*) " old calls will be replaced in the next release in Jan 2009"
    write (stdout,*) " Naming convention for all the method calls are changed to: first_name from"
    write (stdout,*) " firstName.  Subroutines that will be deleted: assertEquals, assertNotEquals,"
    write (stdout,*) " assertTrue, addSuccessful, addFail, etc."
    write (stdout,*) "<<<<<<<<<<<<<<<<<<<<<<<<<< WARNING from FRUIT >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    write (stdout,*)
  end subroutine obsolete_

  subroutine add_success
    successful_assert_count = successful_assert_count + 1
    last_passed = .true.
    case_passed = .true.
    call success_mark_
  end subroutine add_success

  subroutine failed_assert_action (expected, got, message)
    character(*), intent(in) :: expected, got
    character(*), intent(in), optional :: message

    call make_error_msg_ (expected, got, message)
    call increase_message_stack_
    failed_assert_count = failed_assert_count + 1
    last_passed = .false.
    case_passed = .false.
    call failed_mark_
  end subroutine failed_assert_action

  subroutine set_case_name_(value)
    character(*), intent(in) :: value
    case_name = strip(value, MSG_LENGTH)
  end subroutine set_case_name_

  subroutine get_case_name_(value)
    character(*), intent(out) :: value
    value = strip(case_name)
  end subroutine get_case_name_

  subroutine make_error_msg_ (var1, var2, message)
    character(*), intent(in) :: var1, var2
    character(*), intent(in), optional :: message
    msg = '[' // trim(strip(case_name)) // ']: Expected [' // trim(strip(var1)) &
          // '], Got [' // trim(strip(var2)) // ']'
    if (present(message)) then
       msg = trim(msg) // '; User message: [' // message // ']'
    endif
  end subroutine make_error_msg_

  function is_last_passed()
    logical:: is_last_passed
    is_last_passed = last_passed
  end function is_last_passed

  function is_case_passed()
    logical:: is_case_passed
    is_case_passed = case_passed
  end function is_case_passed

  subroutine override_stdout_(write_unit, filename)
    integer, intent(in) ::    write_unit
    character(len = *), intent(in) :: filename

    stdout = write_unit
    open(stdout, file = filename)
  end subroutine override_stdout_

  subroutine stash_test_suite
    stashed_suite%successful_assert_count = successful_assert_count
                  successful_assert_count = 0

    stashed_suite%failed_assert_count     = failed_assert_count
                  failed_assert_count = 0

    allocate(stashed_suite%message_array(current_max))
             stashed_suite%message_array(1:message_index) = &
                         & message_array(1:message_index)
    deallocate(message_array)
      allocate(message_array(MSG_ARRAY_INCREMENT))

    stashed_suite%message_index = message_index
                  message_index = 1
    stashed_suite%message_index_from = message_index_from
                  message_index_from = 1

    stashed_suite%current_max = current_max
                  current_max = 50
    stashed_suite%successful_case_count = successful_case_count
                  successful_case_count = 0
    stashed_suite%failed_case_count     = failed_case_count
                  failed_case_count = 0
    stashed_suite%testCaseIndex         = testCaseIndex
                  testCaseIndex = 1
    stashed_suite%case_name = case_name
                  case_name = DEFAULT_CASE_NAME

    stashed_suite%last_passed = last_passed
                  last_passed = .false.
    stashed_suite%case_passed = case_passed
                  case_passed = .false.
    stashed_suite%case_time_from = case_time_from
                  case_time_from = 0
  end subroutine stash_test_suite

  subroutine restore_test_suite
    successful_assert_count = stashed_suite%successful_assert_count
    failed_assert_count     = stashed_suite%failed_assert_count

    deallocate(message_array)
    message_index = stashed_suite%message_index
    message_index_from = stashed_suite%message_index_from
    current_max  = stashed_suite%current_max
    allocate(message_array(current_max))
    message_array(1:message_index) = stashed_suite%message_array(1:message_index)

    successful_case_count = stashed_suite%successful_case_count
    failed_case_count     = stashed_suite%failed_case_count
    testCaseIndex         = stashed_suite%testCaseIndex

    case_name          = stashed_suite%case_name
    last_passed        = stashed_suite%last_passed
    case_passed        = stashed_suite%case_passed
    case_time_from     = stashed_suite%case_time_from
  end subroutine restore_test_suite

  subroutine end_override_stdout_
    close(stdout)
    stdout = STDOUT_DEFAULT
  end subroutine end_override_stdout_

  !--------------------------------------------------------------------------------
  ! all assertions
  !--------------------------------------------------------------------------------
  subroutine obsolete_assert_true_logical_(var1, message)
    logical, intent (in) :: var1
    character (*), intent (in), optional :: message

    call obsolete_ ('assertTrue subroutine is replaced by function assert_true')
    call assert_true(var1, message)
  end subroutine obsolete_assert_true_logical_

  subroutine assert_true (var1, message)
    logical, intent (in) :: var1
    character (*), intent (in), optional :: message

    if ( var1 .eqv. .true.) then
       call add_success
    else
       call failed_assert_action(to_s(.true.), to_s(var1), message)
    end if
  end subroutine assert_true

  subroutine assert_eq_int_ (var1, var2, message)
    integer, intent(in) :: var1, var2
    character (*), intent(in), optional :: message

    if ( var1 .eq. var2) then
       call add_success
    else
       call failed_assert_action (to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_eq_int_

  subroutine assert_eq_logical_ (var1, var2, message)
    logical, intent (in)  :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .eqv. var2 ) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_eq_logical_

  subroutine assert_eq_string_ (var1, var2, message)
    character(*), intent (in)  :: var1, var2
    character (*), intent (in), optional :: message

    if ( trim(strip(var1)) == trim(strip(var2))) then
       call add_success
    else
       call failed_assert_action(var1, var2, message)
    end if
  end subroutine assert_eq_string_

  subroutine assert_eq_real_ (var1, var2, message)
    real, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .eq. var2) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_eq_real_

  subroutine assert_eq_double_ (var1, var2, message)
    double precision, intent (in) :: var1, var2
    character(*), intent(in), optional :: message

    if ( var1 .eq. var2) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_eq_double_

  subroutine assert_eq_complex_ (var1, var2, message)
    complex(kind=kind(1.0D0)), intent(IN) :: var1, var2
    character (*),             intent(IN), optional :: message

    if ( var1 .ne. var2) then
       call failed_assert_action(to_s(var1), to_s(var2), message)
    else
       call add_success
    end if

  end subroutine assert_eq_complex_

  subroutine assert_eq_real_in_range_(var1, var2, var3, message)
    real, intent (in) :: var1, var2, var3
    character(*), intent(in), optional :: message

    if ( abs( var1 - var2) .le. var3) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if

  end subroutine assert_eq_real_in_range_

  subroutine assert_eq_double_in_range_(var1, var2, var3, message)
    double precision, intent (in) :: var1, var2, var3
    character(*), intent(in), optional :: message

    if ( abs( var1 - var2) .le. var3) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_eq_double_in_range_

  subroutine assert_eq_1d_int_ (var1, var2, n, message)
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_assert_action(to_s(var1(count)), to_s(var2(count)), message)
          return
       end if
    end do loop_dim1

    call add_success
  end subroutine assert_eq_1d_int_

  subroutine assert_eq_1d_string_ (var1, var2, n, message)
    integer, intent (in) :: n
    character(*), intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message
    integer count

    loop_dim1: do count = 1, n
       if ( strip(var1(count)) .ne. strip(var2(count))) then
          call failed_assert_action(var1(count), var2(count), message)
          return
       end if
    end do loop_dim1

    call add_success
  end subroutine assert_eq_1d_string_

  subroutine assert_eq_1d_real_in_range_(var1, var2, n, var3, message)
    integer, intent(in) :: n
    real, intent (in) :: var1(n), var2(n), var3
    character(*), intent(in), optional :: message

    if ( maxval( abs( var1 - var2)) .le. var3) then
       call add_success
    else
       call failed_assert_action(to_s(var1(1)), to_s(var2(1)), &
                                 '1D array real has difference' // ' ' // message)
    end if
  end subroutine assert_eq_1d_real_in_range_

  subroutine assert_eq_1d_double_in_range_(var1, var2, n, var3, message)
    integer, intent(in) :: n
    double precision, intent (in) :: var1(n), var2(n), var3
    character(*), intent(in), optional :: message

    if ( maxval( abs( var1 - var2)) .le. var3) then
       call add_success
    else
       call failed_assert_action(to_s(var1(1)), to_s(var2(1)), message)
    end if
  end subroutine assert_eq_1d_double_in_range_

!  subroutine assert_eq_1d_double (var1, var2, n, message)
!    integer, intent (in) :: n
!    double precision, intent (in) :: var1(n), var2(n)
!    character(*), intent(in), optional :: message
!
!    integer count
!
!    loop_dim1: do count = 1, n
!       if ( var1(count) .ne. var2(count)) then
!          call failed_assert_action(to_s(var1(count)), to_s(var2(count)), &
!               'Array different at count: ' // to_s(count) // ' ' // message)
!          return
!       end if
!    end do loop_dim1
!
!    call add_success
!  end subroutine assert_eq_1d_double

  subroutine assert_eq_2d_real (var1, var2, n, m)
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_assert_action(to_s(var1(count1, count2)), to_s(var2(count1, count2)),&
                  'Array (' // to_s(count1) // ',' // to_s( count2) //')')
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call add_success
  end subroutine assert_eq_2d_real

!  subroutine assert_eq_2d_double (var1, var2, n, m)
!    integer, intent (in) :: n, m
!    double precision, intent (in) :: var1(n,m), var2(n,m)
!
!    integer count1, count2
!
!    loop_dim2: do count2 = 1, m
!       loop_dim1: do count1 = 1, n
!          if ( var1(count1,count2) .ne. var2(count1,count2)) then
!             call failed_assert_action(to_s(var1(count1, count2)), to_s(var2(count1, count2)), &
!                  'Array difference at (' // to_s(count1) // ',' // to_s(count2) // ')')
!             return
!          end if
!       end do loop_dim1
!    end do loop_dim2
!
!    call add_success
!  end subroutine assert_eq_2d_double

  subroutine assert_eq_2d_int_ (var1, var2, n, m, message)
    integer, intent (in) :: n, m
    integer, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in), optional :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_assert_action(to_s(var1(count1, count2)), &
                                       to_s(var2(count1, count2)), message)
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call add_success
  end subroutine assert_eq_2d_int_

  subroutine assert_eq_1d_real_ (var1, var2, n, message)
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_assert_action(to_s(var1(count)), to_s(var2(count)), message)
          return
       end if
    end do loop_dim1
    call add_success
  end subroutine assert_eq_1d_real_

  subroutine assert_eq_2d_real_ (var1, var2, n, m, message)
    integer, intent (in) :: n, m
    real, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent(in), optional :: message

    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_assert_action(to_s(var1(count1, count2)), &
                                       to_s(var2(count1, count2)), message)
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call add_success
  end subroutine assert_eq_2d_real_

  subroutine assert_eq_1d_double_ (var1, var2, n, message)
    integer, intent (in) :: n
    double precision, intent (in) :: var1(n), var2(n)
    character (*), intent (in), optional :: message
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_assert_action(to_s(var1(count)), to_s(var2(count)), message)
          return
       end if
    end do loop_dim1

    call add_success
  end subroutine assert_eq_1d_double_

  subroutine assert_eq_2d_double_ (var1, var2, n, m, message)
    integer, intent (in) :: n, m
    double precision, intent (in) :: var1(n,m), var2(n,m)
    character (*), intent (in), optional :: message
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_assert_action(to_s(var1(count1, count2)), &
                                       to_s(var2(count1, count2)), message)
             return
          end if
       end do loop_dim1
    end do loop_dim2

    call add_success
  end subroutine assert_eq_2d_double_

  subroutine assert_eq_1d_complex_ (var1, var2, n, message)
    integer,                   intent(IN) :: n
    complex(kind=kind(1.0D0)), intent(IN) :: var1(n), var2(n)
    character (*),             intent(IN), optional :: message
    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_assert_action(to_s(var1(count)), to_s(var2(count)), message)
          return
       end if
    enddo loop_dim1

    call add_success
  end subroutine assert_eq_1d_complex_

  subroutine assert_eq_2d_complex_ (var1, var2, n, m, message)
    integer,                   intent(IN) :: n, m
    complex(kind=kind(1.0D0)), intent(IN) :: var1(n,m), var2(n,m)
    character (*),             intent(IN), optional :: message
    integer count1, count2

    loop_dim2: do count2 = 1, m
       loop_dim1: do count1 = 1, n
          if ( var1(count1,count2) .ne. var2(count1,count2)) then
             call failed_assert_action(to_s(var1(count1, count2)), &
                                       to_s(var2(count1, count2)), message)
             return
          endif
       enddo loop_dim1
    enddo loop_dim2

    call add_success
  end subroutine assert_eq_2d_complex_

  subroutine assert_not_equals_int_(var1, var2, message)
    integer, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .ne. var2) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_not_equals_int_

  subroutine assert_not_equals_real_ (var1, var2, message)
    real, intent (in) :: var1, var2
    character (*), intent (in), optional :: message

    if ( var1 .ne. var2) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_not_equals_real_

  subroutine assert_not_equals_double_ (var1, var2, message)
    double precision, intent (in) :: var1, var2
    character(*), intent(in), optional :: message

    if ( var1 .ne. var2) then
       call add_success
    else
       call failed_assert_action(to_s(var1), to_s(var2), message)
    end if
  end subroutine assert_not_equals_double_

  subroutine assert_not_equals_1d_real_ (var1, var2, n)
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)

    integer count

    loop_dim1: do count = 1, n
       if ( var1(count) .ne. var2(count)) then
          call failed_assert_action(to_s(var1(count)), to_s(var2(count)),&
               'Array (' // to_s(count)//')')
          return
       end if
    end do loop_dim1

    call add_success

  end subroutine assert_not_equals_1d_real_

end module fruit
