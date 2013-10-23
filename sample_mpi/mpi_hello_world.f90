module mpi_hello_world
  use mpi
  implicit none
contains
  subroutine hello_world(size, rank, myhostname)
    integer, intent(in) :: size, rank
    character(len = *), intent(out) :: myhostname

    integer :: stat
    
    stat = hostnm(myhostname)
    
    write(*, *) "Hello world FORTRAN rank:", rank, " size:", size, &
    & "hostname:", trim(myhostname)
  
  end subroutine hello_world
end module mpi_hello_world
