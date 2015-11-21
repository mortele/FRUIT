module subs_test
!$ use omp_lib
  use fruit
  implicit none
contains
  subroutine    test_just_sum
    use subs, only : just_sum

    integer :: result
    integer, parameter :: MAX_THREAD_NUM = 100
    integer :: flag(0:MAX_THREAD_NUM)
    character(50) :: str
    integer :: thread_num
    integer :: num_threads

    flag(:) = 0
    num_threads = -1

!$omp parallel default(none) shared(flag) private(thread_num) shared(num_threads) &
!$omp   &      private(str) private(result)
    !$omp single
    num_threads = omp_get_num_threads()
    !$omp end single

    thread_num = omp_get_thread_num()

    if (thread_num <= MAX_THREAD_NUM) then
      flag(thread_num) = flag(thread_num) + 1
      call assert_equals(1, flag(thread_num), "Each thread should come only once")
    endif

    write(str, '("thread", i3, " / ", i3)') thread_num + 1, omp_get_num_threads()

    result = just_sum(2, 3)
    call assert_equals(6, result, " it should fail, " // trim(str))

    result = just_sum(4, 5)
    call assert_equals(9, result, " it should success, " // trim(str))
!$omp end parallel

    call assert_equals(&
    & min(MAX_THREAD_NUM + 1, num_threads), &
    & sum(flag(:)), &
    & "Each thread once")
  end subroutine test_just_sum
end module subs_test
