module various_asserts_test
  use fruit
  implicit none

  character(len = *), parameter :: STDOUTNAME = "override_stdout.txt"
contains
  subroutine test_assert_equals_1d
    character(len = 500) :: line_read
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(&
        & (/ .true., .true. /), &
        & (/ .true., .true. /), &
        & 2, "1d logical")

        call assert_equals(&
        & (/ .true.,  .true. /), &
        & (/ .false., .true. /), &
        & 2, "1d logical")

        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), 2, "1d complex")

        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 5.d0) /), 2, "1d complex")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
      call restore_test_suite
    call end_override_stdout

    call assert_equals(2, failed_count, "Number of failed assertions")
    call assert_equals(4, total_count, "Number of total assertions")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.F", line_read)
    close(20)
  end subroutine test_assert_equals_1d

end module various_asserts_test
