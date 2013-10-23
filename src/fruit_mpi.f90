module fruit_mpi
  use fruit
  use mpi
  implicit none
  private

  public ::          fruit_summary_mpi
  interface          fruit_summary_mpi
    module procedure fruit_summary_mpi_
  end interface

  public ::          fruit_summary_mpi_xml
  interface          fruit_summary_mpi_xml
    module procedure fruit_summary_mpi_xml_
  end interface
contains
  subroutine     fruit_summary_mpi_(size, rank)
    integer, intent(in) :: size, rank
    integer :: fail_assert_sum
    integer :: succ_assert_sum
    integer :: fail_case_sum
    integer :: succ_case_sum

    integer :: fail_assert
    integer :: succ_assert
    integer :: fail_case
    integer :: succ_case

    integer :: message_index
    integer :: num_msgs
    integer :: num_msgs_sum
    integer, allocatable :: num_msgs_rank(:)

    integer :: ierr
    integer :: i
    integer :: imsg
    integer :: status(MPI_STATUS_SIZE)

    integer, parameter :: MSG_LENGTH_HERE = 256
    character(len = MSG_LENGTH_HERE), allocatable :: msgs(:)
    character(len = MSG_LENGTH_HERE), allocatable :: msgs_all(:)

    call get_assert_and_case_count(&
    & fail_assert,  succ_assert, &
    & fail_case,    succ_case)

    call get_message_index(message_index)
    num_msgs = message_index - 1
    allocate(msgs(num_msgs))
    call get_message_array(msgs)

    allocate(num_msgs_rank(size))
    call MPI_Allgather(&
    & num_msgs,      1, MPI_INTEGER, &
    & num_msgs_rank, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)

    num_msgs_sum = sum(num_msgs_rank(:))
    allocate(msgs_all(num_msgs_sum))

    ! array msgs_all:
    !
    ! | msgs(:) of rank 0  | msgs(:) of rank 1   | msgs(:) of rank 2  |
    ! |                    |                     |                    |
    ! | num_msgs_rank(1)   |  num_msgs_rank(2)   | num_msgs_rank(3)   |
    ! |                    |                     |                    |
    ! |                    |                     |                    |
    !                       A                     A                  A
    !                       |                     |                  |
    !              sum(num_msgs_rank(1:1))+1      |             num_msgs_sum
    !                                    sum(num_msgs_rank(1:2))+1

    if (rank == 0) then
      msgs_all(1:num_msgs) = msgs(1:num_msgs)
      do i = 1, size - 1
        imsg = sum(num_msgs_rank(1:i)) + 1
        call MPI_RECV(&
        & msgs_all(imsg), &
        & num_msgs_rank(i + 1) * MSG_LENGTH_HERE, MPI_CHARACTER, &
        & i, 7, MPI_COMM_WORLD, status, ierr)
      enddo
    else
      call MPI_Send(&
      & msgs, &
      & num_msgs * MSG_LENGTH_HERE               , MPI_CHARACTER, &
      & 0, 7, MPI_COMM_WORLD, ierr)
    endif

    call MPI_REDUCE(&
    & fail_assert    , &
    & fail_assert_sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    call MPI_REDUCE(&
    & succ_assert    , &
    & succ_assert_sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    call MPI_REDUCE(&
    & fail_case    , &
    & fail_case_sum,   1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    call MPI_REDUCE(&
    & succ_case    , &
    & succ_case_sum,   1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
      write (*,*)
      write (*,*)
      write (*,*) '    Start of FRUIT summary: '
      write (*,*)
  
      if (fail_assert_sum > 0) then
         write (*,*) 'Some tests failed!'
      else
         write (*,*) 'SUCCESSFUL!'
      end if

      write (*,*)
      write (*,*) '  -- Failed assertion messages:'

      do i = 1, num_msgs_sum
        write (*, "(A)") '   '//trim(msgs_all(i))
      end do
  
      write (*, *) '  -- end of failed assertion messages.'
      write (*, *)

      if (succ_assert_sum + fail_assert_sum /= 0) then
        call fruit_summary_table(&
        & succ_assert_sum, fail_assert_sum, &
        & succ_case_sum  , fail_case_sum    &
        &)
      endif
      write(*, *) '  -- end of FRUIT summary'
    endif
  end subroutine fruit_summary_mpi_

  subroutine     fruit_summary_mpi_xml_(size, rank)
    integer, intent(in) :: size, rank
    if (rank == 0) then
      call         fruit_summary_xml
    endif
    if (size < 0) print *, "size < 0"
  end subroutine fruit_summary_mpi_xml_

end module fruit_mpi
