! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

program fruit_driver

  use fruit
  use fruit_util_test
  use fruit_test
  use fruit_data_test

  call init_fruit

  call fruit_test_package
!moved to self_test!  call fruit_util_test_package
!moved to self_test!  call fruit_data_test_package

  call fruit_summary

end program fruit_driver
