
module myvector_test
  use mod_myvector
  use fruit
  implicit none


contains
  subroutine test_init
      type(ty_myvector) :: vec
    character :: spec = "double quoted spec contains 'single quoted'"

    call vec%init()
    call vec%push(1.23)
    call vec%push(3.45)
    call vec%push(5.55)

    !call assert_equals(5.55, vec%last_elem())
    call assert_equals(5.55, vec%last_elem())

    call vec%close
  end subroutine test_init

  subroutine test_always_ok
    call assert_equals(1.0, 1.0)
  end subroutine test_always_ok

  subroutine test_always_two_fails_one_success
    call assert_equals(1.0, 2.0)
    call assert_equals(2.0, 3.0)
    call assert_equals(1.0, 1.0)
  end subroutine test_always_two_fails_one_success

  subroutine test_all_elems
    type(ty_myvector) :: vec
    real, pointer :: ptr_elems(:)

    call vec%init()
    call vec%push(1.11)
    call vec%push(2.22)
    call vec%push(3.33)
    call vec%push(4.44)
    call vec%push(5.55)

    call vec%all_elems(ptr_elems)

    call assert_equals(1.11, ptr_elems(1))
    call assert_equals(2.22, ptr_elems(2))
    call assert_equals(3.33, ptr_elems(3))
    call assert_equals(4.44, ptr_elems(4))
    call assert_equals(5.55, ptr_elems(5))

    call assert_equals(&
   &  (/ 1.11, 9.99, 3.33, 4.44, 5.50 /), ptr_elems(:), 5 &
   &)


  end subroutine test_all_elems

end module myvector_test
