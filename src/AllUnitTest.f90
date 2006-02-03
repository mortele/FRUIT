!------------------------
! Driver of all unit tests
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module AllUnitTest

contains

  subroutine runAllUnitTest
    
    use fruit
    use fruitTest
    implicit none
    
    call initializeFruit
    
    call allFruitTest()

    call getTestSummary
    
  end subroutine runAllUnitTest

  
end module AllUnitTest
