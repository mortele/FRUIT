

module mod_c
  implicit none
contains
  subroutine     sub3
    continue
  end subroutine sub3
end module mod_c

module mod_b
  use mod_c
  implicit none
contains
  subroutine     sub2
    continue
  end subroutine sub2
end module mod_b

module mod_a
  use mod_b
  implicit none
contains
  subroutine     sub1
    print *, "sub1"
  end subroutine sub1
end module mod_a

