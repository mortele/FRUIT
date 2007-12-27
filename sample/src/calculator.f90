module calculator
  implicit none

  real, save :: result =0.0

contains

  subroutine reset
    result = 0.0
  end subroutine reset

  subroutine add (a, b, output)
    real, intent (in) :: a, b
    real, intent (inout) :: output
    result=a+b
    output=result
  end subroutine add

  real function get_result
    get_result=result
  end function get_result

end module calculator









