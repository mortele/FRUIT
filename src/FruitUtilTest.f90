!------------------------
!
! Test code for FruitUtils
!
! $Id$
! Version : $Revision$
!------------------------
module TestFruitUtil
  implicit none;

contains

  subroutine allTestFruitUtil
    
    call testFloatEuqal
    
  end subroutine allTestFruitUtil
  
  subroutine testFloatEquals
  
    use FruitUtil
    
    real(kind=single) :: number1 = 3.001_single
    real(kind=single) :: number2 = 3.002_single
    real(kind=single) :: number3 = 3.2_single

    write (*,*) Equals (number1, number2 )
    !write (*,*) .not. Equals (number1, number3)
    
  end subroutine testFloatEquals

end module TestFruitUtil
