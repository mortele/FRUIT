
! sample of external subprogram

subroutine uppercase(ch_orig, ch_upper)
  use some_module

  character(len = 1), intent(in)  :: ch_orig
  character(len = 1), intent(out) :: ch_upper
  character(len = 26), parameter :: &
  & atoz_lower = "abcdefghijklmnopqrstuvwxyz"
  character(len = 26), parameter :: &
  & atoz_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  integer :: ith

  ith = scan(ch_orig, atoz_lower)
  if (ith == 0) then
    ch_upper = ch_orig
  else
    ch_upper = atoz_upper(ith:ith)
  endif

  call print_one
end

