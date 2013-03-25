module def_upper_lower
  character(len = *), parameter :: lower = "abcdefghijklmnopqrstuvwxyz"
  character(len = *), parameter :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
contains
  subroutine dummy_writeout
    print *, lower
    print *, upper
  end subroutine dummy_writeout
end module def_upper_lower

