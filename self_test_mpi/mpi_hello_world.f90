module mpi_hello_world
  use mpi
  implicit none
contains
  subroutine hello_world(size, rank, myhostname)
    integer, intent(in) :: size, rank
    character(len = *), intent(out) :: myhostname

    integer :: stat
    
    myhostname = "(no host name)"
#ifdef __INTEL_COMPILER
    call get_environment_variable("COMPUTERNAME", myhostname)
#else
    stat = hostnm(myhostname)
#endif
    
    write(*, *) "Hello world FORTRAN rank:", rank, " size:", size, &
    & "hostname:", trim(myhostname)
  
  end subroutine hello_world
end module mpi_hello_world
