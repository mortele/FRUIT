!------------------------
!
! Utility module for comparison routines.
!
! $Id$
! Version : $Revision$
!------------------------
module FruitUtil

  use DataType
  
  ! --------------------------------------------------
  ! Compare
  ! --------------------------------------------------
  interface Equals
     module procedure EqualsDouble
     module procedure EqualsFloat
     module procedure EqualsInteger
     module procedure EqualsString
     module procedure EqualsLogical
     module procedure EqualsWithTolDouble
     module procedure EqualsWithTolFloat
     module procedure EqualsWithTolInteger
  end interface Equals

contains

  ! ------------------------------------
  ! Compare two double precision numbers
  ! ------------------------------------
  logical function EqualsDouble(dval1, dval2)
    real(kind=double), intent(in) :: dval1
    real(kind=double), intent(in) :: dval2
    
    EqualsDouble = (dval1 == dval2)
    
  end function EqualsDouble

  ! ------------------------------------
  ! Compare two single precision numbers
  ! ------------------------------------
  logical function EqualsFloat(fval1, fval2)
    real(kind=single), intent(in) :: fval1
    real(kind=single), intent(in) :: fval2
    
    EqualsFloat = (fval1 == fval2)
  
  end function EqualsFloat

  ! --------------------
  ! Compare two integers
  ! --------------------
  logical function EqualsInteger(ival1, ival2)
    integer, intent(in) :: ival1
    integer, intent(in) :: ival2
    
    EqualsInteger = (ival1 == ival2)
  
  end function EqualsInteger

  ! ---------------
  ! Compare strings
  ! ---------------
  logical function EqualsString(sval1, sval2)
    character(*), intent(in) :: sval1
    character(*), intent(in) :: sval2
    
    EqualsString = (sval1 == sval2)
  
  end function EqualsString

  ! ---------------------------
  ! Compare logicals (booleans)
  ! ---------------------------
  logical function EqualsLogical(bval1, bval2)
    logical, intent(in) :: bval1
    logical, intent(in) :: bval2
    
    EqualsLogical = (bval1 .eqv. bval2)
  
  end function EqualsLogical

  ! ----------------------------------------------------
  ! Compare two double precision numbers, with tolerance
  ! ----------------------------------------------------
  logical function EqualsWithTolDouble(dval1, dval2, tol)
    real(kind=double), intent(in) :: dval1
    real(kind=double), intent(in) :: dval2
    real(kind=double), intent(in) :: tol
    
    EqualsWithTolDouble = (abs(dval1 - dval2) <= tol)
  
  end function EqualsWithTolDouble

  ! ----------------------------------------------------
  ! Compare two single precision numbers, with tolerance
  ! ----------------------------------------------------
  logical function EqualsWithTolFloat(fval1, fval2, tol)
    real(kind=single), intent(in) :: fval1
    real(kind=single), intent(in) :: fval2
    real(kind=single), intent(in) :: tol
    
    EqualsWithTolFloat = (abs(fval1 - fval2) <= tol)
  
  end function EqualsWithTolFloat

  ! -------------------------------------------
  ! Compare two INTEGER numbers, with tolerance
  ! -------------------------------------------
  logical function EqualsWithTolInteger(ival1, ival2, tol)
    integer, intent(in) :: ival1
    integer, intent(in) :: ival2
    integer, intent(in) :: tol
    
    EqualsWithTolInteger = (abs(ival1 - ival2) <= tol)
  
  end function EqualsWithTolInteger

end module FruitUtil
