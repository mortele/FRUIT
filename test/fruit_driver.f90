!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
program fruit_driver

  use fruit
  use fruit_utility
  use fruit_test
  use fruit_data_test
  
  call initializeFruit
  
  call fruit_test_package

  call allFruitDataTest

  call getTestSummary
     
end program fruit_driver
