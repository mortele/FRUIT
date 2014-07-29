subroutine another_ext_sub(i)
  integer, intent(in) :: i

  print *, i, "th in alphabet"

  call ext_sub_3
end

subroutine it_uses_uppercase
  character(len = *), parameter :: message = "no longer lowercase"
  character(len = 1) :: ch_upper

  integer :: i

  do i = 1, len_trim(message)
    call uppercase(message(1:1), ch_upper)
    write(*, '(a1, $)') ch_upper
  enddo

end subroutine it_uses_uppercase
