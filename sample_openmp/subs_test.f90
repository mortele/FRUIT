module subs_test
!$ use omp_lib
  use fruit
  implicit none
contains
  subroutine    test_just_sum
    use subs, only : just_sum

    integer :: result
    character(50) :: str

!$omp parallel
    write(str, '("thread", i3, " / ", i3)') omp_get_thread_num() + 1, omp_get_num_threads()
    result = just_sum(2, 3)
    call assert_equals(6, result, " it should fail, " // trim(str))

    result = just_sum(4, 5)
    call assert_equals(9, result, " it should success, " // trim(str))
!$omp end parallel
  end subroutine test_just_sum
end module subs_test
