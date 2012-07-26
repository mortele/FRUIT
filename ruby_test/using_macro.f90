module using_macro
#ifdef USE_TEST_MOD
  use test_mod
#endif
  implicit none

contains
  subroutine call_or_not_call_abc
    print *, "start subroutine call_or_not_abc"
#ifdef USE_TEST_MOD
    call abc
#else
    print *, "not calling abc"
#endif
    print *, "end subroutine call_or_not_abc"
  end subroutine call_or_not_call_abc
end module using_macro
