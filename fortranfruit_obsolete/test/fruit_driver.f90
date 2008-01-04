!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
PROGRAM fruit_driver

  USE fruit
  USE fruit_utility
  USE fruit_test
  USE fruit_data_test
  
  call initializeFruit
  
  call allFruitTest

  call allFruitDataTest

  call getTestSummary
     
END PROGRAM fruit_driver
