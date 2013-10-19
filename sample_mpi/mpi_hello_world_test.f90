module mpi_hello_world_test
  use fruit
  implicit none
  include 'mpif.h'
contains
  subroutine test_hello_world
    use mpi_hello_world, only : hello_world
    integer :: ierror
    integer :: size, rank
    integer, parameter :: len_host = 50
    character(len = len_host) :: host
    character(len = len_host), allocatable :: host_rank(:)
    integer :: i

    call MPI_INIT(ierror)
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

      call assert_not_equals(host_rank(0), host_rank(1))
    else
      call assert_false(.true.)
    endif

    call MPI_FINALIZE(ierror)
  end subroutine test_hello_world
end module mpi_hello_world_test
