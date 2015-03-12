module mpi_hello_world_test
  use fruit
  use mpi
  implicit none
contains
  subroutine test_hello_world
    use mpi_hello_world, only : hello_world
    integer :: ierror
    integer :: size, rank
    integer, parameter :: len_host = 50
    character(len = len_host) :: host
    character(len = len_host), allocatable :: host_rank(:)
    integer :: i
    character(len = 100) :: message

    call MPI_COMM_SIZE (MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierror)
  
    call hello_world(size, rank, host)

    allocate(host_rank(0:size - 1))

    call MPI_Gather(&
    & host,         len_host, MPI_CHARACTER, &
    & host_rank(0), len_host, MPI_CHARACTER, &
    & 0, MPI_COMM_WORLD, ierror &
    &)
  
    if (rank == 0) then
      do i = 0, size - 1
        write(*, '("rank:", i3, " hostname:", a)') i, trim(host_rank(i))
      enddo
    endif

    ! call assert_not_equals(host_rank(0), host_rank(1))

    write(message, '("rank=", i3, " in size=", i3)') rank, size
    call assert_false(.true., "expected to fail on each rank, " // trim(message))
  end subroutine test_hello_world
end module mpi_hello_world_test
