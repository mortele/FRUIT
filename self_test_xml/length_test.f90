module length_test
  use fruit
contains
  subroutine test_length_xy
    use length, only : length_xy
    real :: xy_1(2)
    real :: xy_2(2)

    xy_1(1:2) = (/ 0.5, 1.0 /)
    xy_2(1:2) = (/ 3.5, 5.0 /)
    
    call assert_equals(5.0, length_xy(xy_1, xy_2), 1.e-5, "length (0.5,1)to(3.5,5.0)")

     stop   !! code for debug. What if tester code stops?
  end subroutine test_length_xy
end module length_test
