
integer function add_one(i)
  integer, intent(in) :: i

  add_one = i + 1
end  !! not "end function"

function add_three(i)
  integer, intent(in) :: i
  integer :: add_three

  add_three = i + 3
end function add_three

