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
