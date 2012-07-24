module test_mod
  use sample08
  implicit none
contains
  subroutine abc
    print *, "abc"
  end subroutine abc
end module test_mod
