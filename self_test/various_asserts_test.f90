! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module various_asserts_test
  use fruit
  implicit none

  character(len = *), parameter :: STDOUTNAME = "override_stdout.txt"
  integer, parameter :: MSG_LEN = 256
contains
  subroutine test_assert_not_equals_0d
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_not_equals(.false., .true. ,  "0d logical, neq, A")
        call assert_not_equals(.true.,  .true. , "0d logical, neq, B")
        call assert_not_equals(.false., .false.,  "0d logical, neq, C")
        call assert_not_equals(3, 4, "0d integer, neq, D")
        call assert_not_equals(3, 3, "0d integer, neq, E")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_string_has_string_tmp(msgs(1), "Expected Not [T], Got [T]")
    call assert_string_has_string_tmp(msgs(2), "Expected Not [F], Got [F]")
    call assert_string_has_string_tmp(msgs(3), "Expected Not [3], Got [3]")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".FF.F", line_read)
    close(20)
  end subroutine test_assert_not_equals_0d


  subroutine test_assert_equals_0d
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(.true.,  .true.,  "0d logical A")
        call assert_equals(.true.,  .false., "0d logical B")
        call assert_equals(.false., .true.,  "0d logical C")

        call assert_not_equals(.true.,  .true.,  "0d logical, neq, A")
        call assert_not_equals(.true.,  .false., "0d logical, neq, B")
        call assert_not_equals(.false., .true.,  "0d logical, neq, C")

        call assert_equals(3, 3, "0d integer A")
        call assert_equals(3, 4, "0d integer B")
        call assert_not_equals(3, 3, "0d integer, neq, A")
        call assert_not_equals(3, 4, "0d integer, neq, B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 5, failed_count, "Number of failed assertions")
    call assert_equals(10, total_count,  "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "0d logical B")
    call assert_string_has_string_tmp(msgs(2), "0d logical C")
    call assert_string_has_string_tmp(msgs(3), "0d logical, neq, A")
    call assert_string_has_string_tmp(msgs(4), "0d integer B")
    call assert_string_has_string_tmp(msgs(5), "0d integer, neq, A")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".FFF...FF.", line_read)
    close(20)
  end subroutine test_assert_equals_0d

  subroutine test_assert_equals_1d
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(&
        & (/ .true., .true. /), (/ .true., .true. /), 2, "1d logical A")

        call assert_equals(&
        & (/ .true.,  .true. /), (/ .false., .true. /), 2, "1d logical B")

        call assert_not_equals(&
        & (/ .true., .true. /), (/ .true., .true. /), 2, "1d neq logical A")

        call assert_not_equals(&
        & (/ .true.,  .true. /), (/ .false., .true. /), 2, "1d neq logical B")

        call assert_equals((/ -3, 3 /), (/ -3, 3 /), 2, "1d integer A")

        call assert_equals((/ -3, 3 /), (/ -3, 4 /), 2, "1d integer B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 3, failed_count, "Number of failed assertions")
    call assert_equals( 6, total_count, "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "1d logical B")
    call assert_string_has_string_tmp(msgs(2), "1d neq logical A")
    call assert_string_has_string_tmp(msgs(3), "1d integer B")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".FF..F", line_read)
    close(20)
  end subroutine test_assert_equals_1d

  subroutine test_assert_equals_1d_realdouble
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(&
        & (/ -1.0, 1.0, 3.0, -5.0 /), &
        & (/ -1.0, 1.0, 3.0, -5.0 /), 4, "1d real A")

        call assert_equals(&
        & (/ 1.0, 2.0, 3.0, 4.0 /), &
        & (/ 1.0, 2.0, 3.0, 5.0 /), 4, "1d real B")

        call assert_equals(&
        & (/ 1.0, 2.0, 3.0, 4.0 /), &
        & (/ 1.0, 2.0, 3.0, 4.010 /), 4, 0.10, "1d real, range A")

        call assert_equals(&
        & (/ 1.0, 2.0, 3.0, 4.0 /), &
        & (/ 1.0, 2.0, 3.0, 4.110 /), 4, 0.10, "1d real, range B")

        call assert_not_equals(&
        & (/ 1.0, 2.0, 3.0, 4.0 /), &
        & (/ 1.0, 2.0, 3.0, 4.010 /), 4, 0.10, "1d real, neq, range A")

        call assert_not_equals(&
        & (/ 1.0, 2.0, 3.0, 4.0 /), &
        & (/ 1.0, 2.0, 3.0, 4.110 /), 4, 0.10, "1d real, neq, range B")

        call assert_equals(&
        & (/ -1.d0, 1.d0, 3.d0, -5.d0 /), &
        & (/ -1.d0, 1.d0, 3.d0, -5.d0 /), 4, "1d double A")

        call assert_equals(&
        & (/ 1.d0, 2.d0, 3.d0, 4.d0 /), &
        & (/ 1.d0, 2.d0, 3.d0, 5.d0 /), 4, "1d double B")

        call assert_equals(&
        & (/ 1.d0, 2.d0, 3.d0, 4.d0 /), &
        & (/ 1.d0, 2.d0, 3.d0, 4.01d0 /), 4, 0.1d0, "1d double, range A")

        call assert_equals(&
        & (/ 1.d0, 2.d0, 3.d0, 4.d0 /), &
        & (/ 1.d0, 2.d0, 3.d0, 4.11d0 /), 4, 0.1d0, "1d double, range B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 5, failed_count, "Number of failed assertions")
    call assert_equals(10, total_count, "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "1d real B")
    call assert_string_has_string_tmp(msgs(2), "1d real, range B")
    call assert_string_has_string_tmp(msgs(3), "1d real, neq, range A")
    call assert_string_has_string_tmp(msgs(4), "1d double B")
    call assert_string_has_string_tmp(msgs(5), "1d double, range B")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.FF..F.F", line_read)
    close(20)
  end subroutine test_assert_equals_1d_realdouble

  subroutine test_assert_equals_1d_complex
    character(len = MSG_LEN) :: line_read
    integer :: failed_count
    integer :: total_count
    character(len = MSG_LEN) :: msgs(10)

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), 2, "1d complex A")

        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 4.1d0) /), 2, "1d complex B")

        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 4.01d0) /), 2, 0.1d0, "1d complex, range 1")

        call assert_equals(&
        & (/ (1.d0, 2.d0), (3.d0, 4.d0) /), &
        & (/ (1.d0, 2.d0), (3.d0, 4.11d0) /), 2, 0.1d0,  "1d complex, range 2")

        call assert_equals(&
        & (/ (1.d0,   2.d0), (3.d0, 4.d0) /), &
        & (/ (1.01d0, 2.d0), (3.d0, 4.d0) /), 2, 0.1d0,  "1d complex, range 3")

        call assert_equals(&
        & (/ (1.d0,   2.d0), (3.d0, 4.d0) /), &
        & (/ (1.11d0, 2.d0), (3.d0, 4.d0) /), 2, 0.1d0,  "1d complex, range 4")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals(3, failed_count, "Number of failed assertions")
    call assert_equals(6, total_count, "Number of total assertions")
    call assert_string_has_string_tmp(msgs(1), "1d complex B")
    call assert_string_has_string_tmp(msgs(2), "1d complex, range 2")
    call assert_string_has_string_tmp(msgs(3), "1d complex, range 4")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.F.F", line_read)
    close(20)
  end subroutine test_assert_equals_1d_complex

  subroutine test_assert_equals_2d
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count
    logical :: var1(4, 5)
    logical :: var2(4, 5)
    logical :: var3(4, 5)
    integer :: var1i(4, 5)
    integer :: var2i(4, 5)
    integer :: var3i(4, 5)

    var1(:, :) = .true.
    var1(2, 3) = .false.

    var2(:, :) = var1(:, :)
    var3(:, :) = var1(:, :)
    var3(2, 2) = .false.

    var1i(:, :) = 123
    var1i(1, 2) = 3
    var1i(2, 4) = 6
    var2i(:, :) = var1i(:, :)
    var3i(:, :) = var1i(:, :)
    var3i(3, 3) = 4321

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(var1, var2, 4, 5, "2d logical A")
        call assert_equals(var1, var3, 4, 5, "2d logical B")
        call assert_not_equals(var1, var2, 4, 5, "2d logical, neq, A")
        call assert_not_equals(var1, var3, 4, 5, "2d logical, neq, B")

        call assert_equals(var1i, var2i, 4, 5, "2d integer A")
        call assert_equals(var1i, var3i, 4, 5, "2d integer B")
        call assert_not_equals(var1i, var2i, 4, 5, "2d integer, neq, A")
        call assert_not_equals(var1i, var3i, 4, 5, "2d integer, neq, B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 4, failed_count, "Number of failed assertions")
    call assert_equals( 8, total_count, "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "2d logical B")
    call assert_string_has_string_tmp(msgs(2), "2d logical, neq, A")
    call assert_string_has_string_tmp(msgs(3), "2d integer B")
    call assert_string_has_string_tmp(msgs(4), "2d integer, neq, A")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".FF." // ".FF.", line_read)
    close(20)
  end subroutine test_assert_equals_2d

  subroutine test_assert_equals_2d_real
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count
    real    :: var1(4, 5)
    real    :: var2(4, 5)
    real    :: var3(4, 5)
    double precision :: var1d(4, 5)
    double precision :: var2d(4, 5)
    double precision :: var3d(4, 5)

    var1(:, :) = 2.0
    var1(2, 3) = 4.0

    var2(:, :) = var1(:, :)
    var3(:, :) = var1(:, :)
    var3(2, 2) = 2.01

    var1d(:, :) = 2.d0
    var1d(1, 2) = 3.d0
    var1d(2, 4) = 6.d0
    var2d(:, :) = var1d(:, :)
    var3d(:, :) = var1d(:, :)
    var3d(3, 3) = 2.01d0

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(var1, var2, 4, 5, "2d real A")
        call assert_equals(var1, var3, 4, 5, "2d real B")
        call assert_equals(var1, var3, 4, 5, 0.1, "2d real C")
        call assert_equals(var1, var3, 4, 5, 0.001, "2d real D")
        call assert_not_equals(var1, var2, 4, 5, "2d real, neq, A")
        call assert_not_equals(var1, var3, 4, 5, "2d real, neq, B")

        call assert_equals(var1d, var2d, 4, 5, "2d double A")
        call assert_equals(var1d, var3d, 4, 5, "2d double B")
        call assert_equals(var1d, var3d, 4, 5, 0.1d0,  "2d double C")
        call assert_equals(var1d, var3d, 4, 5, 0.001d0, "2d double D")
        call assert_not_equals(var1d, var2d, 4, 5, "2d double, neq, A")
        call assert_not_equals(var1d, var3d, 4, 5, "2d double, neq, B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 6, failed_count, "Number of failed assertions")
    call assert_equals(12, total_count,  "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "2d real B")
    call assert_string_has_string_tmp(msgs(2), "2d real D")
    call assert_string_has_string_tmp(msgs(3), "2d real, neq, A")
    call assert_string_has_string_tmp(msgs(4), "2d double B")
    call assert_string_has_string_tmp(msgs(5), "2d double D")
    call assert_string_has_string_tmp(msgs(6), "2d double, neq, A")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.FF." // ".F.FF.", line_read)
    close(20)
  end subroutine test_assert_equals_2d_real




  subroutine test_assert_equals_2d_complex
    character(len = MSG_LEN) :: line_read
    character(len = MSG_LEN) :: msgs(10)
    integer :: failed_count
    integer :: total_count
    complex(kind = kind(1.d0)) :: var1(4, 5)
    complex(kind = kind(1.d0)) :: var2(4, 5)
    complex(kind = kind(1.d0)) :: var3(4, 5)

    var1(:, :) = (2.d0, -2.d0)
    var1(2, 3) = (4.d0,  7.d0)

    var2(:, :) = var1(:, :)
    var3(:, :) = var1(:, :)
    var3(2, 2) = (2.d0, -2.01d0)

    call override_stdout(20, STDOUTNAME)
      call stash_test_suite
        call assert_equals(var1, var2, 4, 5, "2d complex A")
        call assert_equals(var1, var3, 4, 5, "2d complex B")
        call assert_equals(var1, var3, 4, 5, 0.1d0, "2d complex C")
        call assert_equals(var1, var3, 4, 5, 0.001d0, "2d complex D")
        call assert_not_equals(var1, var2, 4, 5, "2d complex, neq, A")
        call assert_not_equals(var1, var3, 4, 5, "2d complex, neq, B")

        call get_failed_count(failed_count)
        call get_total_count( total_count )
        call get_messages(msgs)
      call restore_test_suite
    call end_override_stdout

    call assert_equals( 3, failed_count, "Number of failed assertions")
    call assert_equals( 6, total_count,  "Number of total assertions")

    call assert_string_has_string_tmp(msgs(1), "2d complex B")
    call assert_string_has_string_tmp(msgs(2), "2d complex D")
    call assert_string_has_string_tmp(msgs(3), "2d complex, neq, A")

    open (20, file = STDOUTNAME)
      read (20, '(a)') line_read
      call assert_equals(".F.FF.", line_read)
    close(20)
  end subroutine test_assert_equals_2d_complex


  subroutine assert_string_has_string_tmp(str1, str2)
    character (len = *), intent(in) :: str1, str2
    logical :: has_string
    character (len = MSG_LEN) :: str_message

    if (index(trim(str1), trim(str2)) /= 0) then
      has_string = .true.
    else
      has_string = .false.
    endif

    str_message = "String '" // trim(str1) // "' should contain " // trim(str2)
    call assert_true(has_string, str_message)
  end subroutine assert_string_has_string_tmp

end module various_asserts_test
