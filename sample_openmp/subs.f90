module subs
  implicit none
contains
  integer function just_sum(a, b)
    integer, intent(in) :: a, b

    just_sum = a + b
  end function just_sum
end module subs

