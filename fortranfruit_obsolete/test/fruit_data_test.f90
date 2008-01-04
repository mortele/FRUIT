!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module fruit_data_test
  use FRUIT
  implicit none

contains
  ! Run all the test sub routines 
  ! -----------------------------
  subroutine allFruitDataTest  
    call testAssertEqualsFloat()
    call test1DArrayString()

  end subroutine allFruitDataTest
  
  subroutine testAssertEqualsFloat
    
    real :: variable = 2.3
    real :: result = 2.3
    
    call initializeFruit
    call assertEquals (variable, result)
    call assertNotEquals (variable + 0.1, result)
    call getTestSummary
    
  end subroutine testAssertEqualsFloat
  
  subroutine test1DArrayString
    
    character(LEN=5), dimension (2) :: variable
    character(LEN=5), dimension (2) :: RESULT

    variable = (/'a', 'b'/)
    RESULT = (/'a', 'b'/)
    call assertEquals (variable, result, 2)
    call assertEquals (variable, result, 2, "string comp")
    
  end subroutine test1DArrayString
  
end module fruit_data_test
