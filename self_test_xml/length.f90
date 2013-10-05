module length
contains
  real function length_xy(xy_1, xy_2)
    real :: xy_1(2)
    real :: xy_2(2)

    print *, "function should return length from ", xy_1, " to ", xy_2

    length_xy = -1.0
  end function length_xy
end module length
