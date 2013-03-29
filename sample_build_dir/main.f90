! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

program main
  use atoz
  implicit none

  character(len = 15) :: from
  character(len = 15) :: result

  from = "main program"
  call uppercase(from, result)

  print *, """", trim(from), """ -> """, trim(result), """"
end program main

