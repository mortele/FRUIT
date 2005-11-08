!------------------------
!
! Author: Andrew H. Chen chen@meihome.com
! Last modified: 2004/01/12
! Version : $Revision$
!------------------------
module util_test

contains

  subroutine allUtilTest
    implicit none;
  
    call floatEuqalTest

  end subroutine allUtilTest
  
  
  subroutine floatEuqalTest
    use util
    use fruit
    implicit none;
    
    real :: number1 = 3.001
    real :: number2 = 3.001
    real :: number3 = 3.2

   call assertTrue (floatEqual (number1, number2), "floatEuqalTest" )
   call assertTrue ( .not. floatEqual (number1, number3), "floatEuqalTest")
    
  end subroutine floatEuqalTest

end module util_test
