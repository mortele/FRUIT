module nested_ifdef
#ifdef OUTER
  use mod_1
#ifdef INNER
  use mod_2
#endif
#endif
contains
  subroutine sub1
    print *, "in sub1"
  end subroutine sub1
end module nested_ifdef

