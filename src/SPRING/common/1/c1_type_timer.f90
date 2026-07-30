module c1_type_timer
  use lib_const
  use lib_time
  implicit none
  public

  type ctimer_
    type(timer_) :: timer
  end type

end module c1_type_timer
