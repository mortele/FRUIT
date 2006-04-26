module DataType

! Copied from FORTRAN 90/95 For Scientists and Engineers book
!    by Stephen J. Chapman
!

  implicit none
  public
    integer, parameter :: single = selected_real_kind(p = 6, r = 37)   
    integer, parameter :: double = selected_real_kind(p = 13,r = 200)

end module DataType
