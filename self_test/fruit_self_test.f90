! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module fruit_self_test
  use fruit
  implicit none

  character(len = *), parameter :: STDOUTNAME = "override_stdout.txt"
contains
  subroutine test_case_passed_xml
    character(len = 256) :: a_line
    character(len = 256) :: a_line_trim
    character(len = 256) :: orig_prefix
    character(len = *), parameter :: tmp_file = "tmp.xml"
    character(len = *), parameter :: tmp_file2 = "tmp2.xml"

    character(len = *), parameter :: expected_1 = &
    & "<testcase name=""testcase_name"" classname=""class_name"" time="""

    character(len = *), parameter :: expected_2 = &
    & "<testcase name=""testcase_name"" "//&
    & "classname=""fruit_self_test.class_name"" time="""

    call get_prefix(orig_prefix)
    !--------------
    call override_xml_work(30, tmp_file)
      call set_prefix("")
      call case_passed_xml("testcase_name", "class_name")
    call end_override_xml_work

    open(30, file = tmp_file, action = "read")
      read(30, '(a)') a_line
    close(30, status = "delete")

    a_line_trim = adjustl(a_line)
    a_line_trim = a_line_trim(1:len(expected_1))
    call assert_equals(expected_1, a_line_trim, "without prefix")
    !--------------

    !--------------
    call override_xml_work(30, tmp_file2)
      call set_prefix("fruit_self_test.")
      call case_passed_xml("testcase_name", "class_name")
    call end_override_xml_work

    open(30, file = tmp_file2, action = "read")
      read(30, '(a)') a_line
    close(30, status = "delete")

    a_line_trim = adjustl(a_line)
    a_line_trim = a_line_trim(1:len(expected_2))
    call assert_equals(expected_2, a_line_trim, "with prefix")
    !--------------
    call set_prefix(orig_prefix)
  end subroutine test_case_passed_xml


  subroutine test_set_prefix
    character(len = 40) :: str40
    character(len = 90) :: str90
    character(len = 3) :: str3
    character(len = FRUIT_PREFIX_LEN_MAX) :: prefix_got
    integer :: i
    character(len = 256) :: orig_prefix

    call get_prefix(orig_prefix)
      !--------------
      str40 = " abcdef. "
      call set_prefix(str40)
      call get_prefix(prefix_got)
      !--------------
    call set_prefix(orig_prefix)
    call assert_equals("abcdef.", prefix_got, "prefix is abcdef.")

    call get_prefix(orig_prefix)
      !--------------
      do i = 1, 90
        str90(i:i) = " "
      enddo
      str90( 1:10) = "     head."
      str90(86:90) = "tail."
      call set_prefix(str90)
      call get_prefix(prefix_got)
      call get_prefix(str3)
      !--------------
    call set_prefix(orig_prefix)

    call assert_equals("head.", prefix_got, "prefix is head.")
    call assert_equals("hea", str3, "prefix(1:3) is hea")
  end subroutine test_set_prefix


  subroutine test_linechar_count
    character(len = 100) :: line_read(1:5)
    character(len = 100) :: expected
    integer :: i

    do i = 1, 39
      expected(i * 2 - 1: i * 2) = ".F"
    enddo

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        do i = 1, 100 !200 = 78 * 2 + 44, 78 is MAX_MARKS_PER_LINE
          call assert_equals(.true., .true.)
          call assert_equals(.true., .false.)
        enddo
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
    do i = 1, 3
      read (20, '(a)') line_read(i)
    enddo
    close(20)
    call assert_equals(78, len_trim(line_read(1)))
    call assert_equals(78, len_trim(line_read(2)))
    call assert_equals(44, len_trim(line_read(3)))
    call assert_equals(expected(1:78), line_read(1))
    call assert_equals(expected(1:78), line_read(2))
    call assert_equals(expected(1:44), line_read(3))
  end subroutine test_linechar_count

  subroutine test_fruit_hide_dots
    character(len = 500) :: line_read
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call fruit_hide_dots
        call assert_equals(.true., .true.)
        call assert_equals(.true., .false.)
        call assert_equals(-5, -5)
        call assert_equals(-5, -6)
        call fruit_show_dots
        call assert_equals(1.000, 1.001, 0.002)
        call assert_equals(1.000, 1.003, 0.002)

        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(3, failed_count, "Number of failed assertions")
    call assert_equals(6, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals("FF.F", line_read)
    close(20)
  end subroutine test_fruit_hide_dots


  subroutine test_assert_equals
    character(len = 500) :: line_read
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(.true., .true.)
        call assert_equals(.true., .false.)
        call assert_equals(-5, -5)
        call assert_equals(-5, -6)
        call assert_equals(1.000, 1.001, 0.002)
        call assert_equals(1.000, 1.003, 0.002)

        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(3, failed_count, "Number of failed assertions")
    call assert_equals(6, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.F.F", line_read)
    close(20)
  end subroutine test_assert_equals

  subroutine test_should_add_success_or_failed_actions
    character(len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call add_success
        write(20, *)

        call add_success
        call add_success
        write(20, *)

        call failed_assert_action('','')
        write(20, *)

        call failed_assert_action('','')
        call failed_assert_action('','')
        write(20, *)
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals (".", line_read, "Should see . here")
      read (20, '(a)') line_read
      call assert_equals ("..", line_read, "Should see .. here")
      read (20, '(a)') line_read
      call assert_equals ("F", line_read, "Should see F here")
      read (20, '(a)') line_read
      call assert_equals ("FF", line_read, "Should see FF here")
    close (20)
  end subroutine test_should_add_success_or_failed_actions

  subroutine test_assert_true
    character(len = 500) :: line_read
    integer :: failed_count, total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_true (.true.) ! 'Should see 1 successful case'
        call assert_true (.FALSE.) ! write (*,*) 'Should see 1 failed case'

        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(1, failed_count, "Number of failed assertions")
    call assert_equals(2, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals (".F", line_read, "Should see .F here")
    close (20)
  end subroutine test_assert_true


  ! Test assert_false with message
  ! ----------------------------
  subroutine test_assert_false_message
    character(len = 500) :: line_read
    integer :: failed_count, total_count

    integer :: try

    do try = 1, 2
      call override_stdout(20, STDOUTNAME)
        call stash_test_suite
          if (try == 1) call init_fruit
          call assert_false (.true. , 'message in assert_false_message_test true test')
          call assert_false (.false., 'message in assert_false_message_test false test')
          call get_failed_count(failed_count)
          call get_total_count(total_count)
          if (try == 1) call fruit_summary
        call restore_test_suite
      call end_override_stdout

      call assert_equals(1, failed_count, "Number of failed assertions")
      call assert_equals(2, total_count, "Number of total assertions")

      open (20, file = STDOUTNAME)
        read (20, '(a)') line_read

        if (try == 1) then
          call read_until_string(20, 'User message:', line_read)
          call assert_string_has_string(line_read, &
          & "message in assert_false_message_test true test")
        else
          call assert_equals ("F.", line_read, "Should see F. here")
        endif
      close (20)
    enddo
  end subroutine test_assert_false_message


  ! Test assert_true with message
  ! ----------------------------
  subroutine test_assert_true_message
    character(len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call assert_true (.true., 'message in assert_true_message_test true test')

        call assert_true (.FALSE., 'message in assert_true_message_test false test')

        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'User message:', line_read)

      call assert_string_has_string(line_read, &
      &  "message in assert_true_message_test false test")
    close (20)
  end subroutine test_assert_true_message


  subroutine test_assert_true_message_nospace
    character(len = 500) :: line_read
    character(len = 100) :: message

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit

        message = "short message." !this variable holds many space characters.
        call assert_true (.false., message)

        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'User message:', line_read)

      call assert_string_has_string(line_read, &
      &  "[short message.]")
    close (20)
  end subroutine test_assert_true_message_nospace


  ! Test add_success and is_all_successful
  ! -----------------------------------
  subroutine test_add_success
    logical :: result = .FALSE.

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_success
        call is_all_successful (result)
        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    call assert_equals(.true., result, "is_all_successful should be .true.")
  end subroutine test_add_success


  subroutine test_add_fail
    logical :: result = .FALSE.

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_fail
        call is_all_successful (result)
        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    call assert_equals(.false., result, "is_all_successful should be .false.")
  end subroutine test_add_fail


  subroutine read_until_string(unit, string, last_line)
    integer, intent(in) :: unit
    character(len = *), intent(in) :: string
    character(len = *), intent(out) :: last_line

    character(len = 500) :: line_read

    do
      read(unit, '(a)', end = 999) line_read
      if (index(line_read, string) /= 0) exit
    enddo
    999 continue
    last_line = trim(line_read)
  end subroutine read_until_string

  subroutine cat_file(filename)
    character(len = *), intent(in) :: filename
    character(len = 500) :: line_read

    print *, ""
    open(20, file = filename)
      do
        read (20, '(a)', end = 1232) line_read
        print *, ">>>", trim(line_read)
      enddo
    1232 continue
    close(20)
  end subroutine cat_file

  ! Test add_success and is_all_successful
  ! -----------------------------------
  subroutine test_add_fail_message
    logical :: result = .FALSE.
    character(len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_fail ('Add a failed case')
        call add_fail ('test_add_fail_message', 'Add a failed case')
        call is_all_successful (result)
        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    call assert_equals(.false., result, "is_all_successful should be .false.")

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'FF', line_read)
      call assert_equals("FF", line_read)

      call read_until_string(20, 'User message:', line_read)
      call assert_string_has_string(line_read, "[Add a failed case]")

      read(20, '(a)') line_read

      call assert_string_has_string(line_read, "test_add_fail_message")
      call assert_string_has_string(line_read, "Add a failed case")
    close (20)
  end subroutine test_add_fail_message


  ! Test successful and failed summary
  ! ----------------------------------
  subroutine test_fruit_summary_1
    character(len = 500) :: line_read
    integer :: failed_count, total_count

    !FRUIT_SPEC Summary for successful cases without message: Should see 1 successful case

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_success
        call fruit_summary

        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'Successful', line_read)
      call assert_string_has_string(line_read, " 1")
    close (20)

    call assert_equals(0, failed_count, "Number of failed assertions")
    call assert_equals(1, total_count, "Number of total assertions")
  end subroutine test_fruit_summary_1


  subroutine test_fruit_summary_2
    character(len = 500) :: line_read
    integer :: failed_count, total_count
    !FRUIT_SPEC Summary for successful cases and 2 messages: Should see 3 successful cases

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_success
        call add_success
        call add_success
        call fruit_summary

        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(0, failed_count, "Number of failed assertions")
    call assert_equals(3, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'Successful', line_read)
      call assert_string_has_string(line_read, " 3")
    close (20)
  end subroutine test_fruit_summary_2

  subroutine test_fruit_summary_3
    character(len = 500) :: line_read
    integer :: failed_count, total_count
    !FRUIT_SPEC Summary for failed cases: Should see 2 failed case amd 1 message
    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        call add_fail
        call add_fail('Fail message from test case.')
        call fruit_summary
        call get_failed_count(failed_count)
        call get_total_count(total_count)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(2, failed_count, "Number of failed assertions")
    call assert_equals(2, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'FF', line_read)
      call assert_equals("FF", line_read)

      call read_until_string(20, 'Failed assertion messages:', line_read)
      read (20, '(a)') line_read
      call assert_equals(0, &
      & index(line_read, "User message:"), &
      &          "String 'User message:' should not appear")

      read(20, '(a)') line_read
      call assert_string_has_string(line_read, "Fail message from test case.")
    close (20)
    !call cat_file(STDOUTNAME)
  end subroutine test_fruit_summary_3

  subroutine test_show_output
    character(len = 500) :: last_message_got1
    character(len = 500) :: last_message_got2
    character(len = 500) :: expected
    integer :: i

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call init_fruit
        DO i=1,5
           call assert_true (.true.)
        END DO
        last_message_got1 = trim(get_last_message())

        DO i=1,5
           call assert_true (.false.)
        END DO
        last_message_got2 = trim(get_last_message())
        call fruit_summary
      call restore_test_suite
    call end_override_stdout

    call assert_equals (last_message_got1, '')

    expected = "Expected [T], Got [F]"
    call assert_string_has_string(last_message_got2, expected)
  end subroutine test_show_output

  subroutine test_showOutputForReport
    logical :: trueValue = .TRUE.
    logical :: falseValue = .FALSE.
    integer :: i
    integer :: count_total
    integer :: count_failed
    character (len = 100), DIMENSION (3)  :: msgs
    character (len = 100) :: expected

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
       call init_fruit
         DO i=1,2
           call assert_true (trueValue, 'msg_trueValue')
         END DO

         DO i=1,2
           call assert_true (falseValue, 'msg falseValue')
         END DO
         call get_total_count (count_total)
         call get_failed_count (count_failed)
         call get_messages(msgs)
       call fruit_summary
      call restore_test_suite
    call end_override_stdout

    call assert_equals(4, count_total)
    call assert_equals(2, count_failed)

    expected = "msg falseValue"
    call assert_string_has_string(msgs(1), expected)

    expected = "msg falseValue"
    call assert_string_has_string(msgs(2), expected)

    call assert_equals("", msgs(3))
  end subroutine test_showOutputForReport

  subroutine assert_string_has_string(str1, str2)
    character (len = *), intent(in) :: str1, str2
    logical :: has_string

    if (index(trim(str1), trim(str2)) /= 0) then
      has_string = .true.
    else
      has_string = .false.
    endif
    call assert_true(has_string, &
   &  "String '" // trim(str1) // "' should contain " // trim(str2) &
   &)
  end subroutine assert_string_has_string

  subroutine test_obsolete_message
    character (len = 500) :: expected
    character (len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assertTrue(.true.)
      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      call read_until_string(20, 'assertTrue subroutine', line_read)

      expected = "replaced by function assert_true"
      call assert_string_has_string(line_read, expected)
    close(20)

  end subroutine test_obsolete_message

  subroutine subtest_multiple_cases_1
    call assert_true (.true.)
    call assert_true (.false.)
  end subroutine subtest_multiple_cases_1

  subroutine subtest_multiple_cases_2
    call assert_true (.true.)
  end subroutine subtest_multiple_cases_2

  subroutine test_demonstrate_case_summary
    character (len = 500) :: line_read

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite

        call run_test_case(subtest_multiple_cases_1)
        call run_test_case(subtest_multiple_cases_2)

      call restore_test_suite
    call end_override_stdout

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.", line_read)
    close(20)
  end subroutine test_demonstrate_case_summary
end module fruit_self_test

