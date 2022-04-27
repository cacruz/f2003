module timer_class
  implicit none
  
  integer, parameter :: dbl = selected_real_kind(p=14)
  
  type, public :: timer_t
     private      ! hide data
     real(kind=dbl) :: saved_time ! in ms
   contains
     procedure, public :: start_timer => start_timer_sub
     procedure, public :: elapsed_time => elapsed_timer_fn
  end type timer_t

  ! restrict access to actual procedures
  private :: start_timer_sub, elapsed_timer_fn
  
contains
  
  subroutine start_timer_sub(self)
    class(timer_t) :: self
    integer, dimension(8) :: value ! time value array

    ! get date and time information from the real-time system clock.
    call DATE_AND_TIME ( VALUES=value )
    self%saved_time = 86400.D0 * value(3) + 3600.D0 * value(5) &
         & + 60.D0 * value(6) + value(7) + 0.001D0 * value(8)
  end subroutine start_timer_sub

  real function elapsed_timer_fn(self)
    class(timer_t) :: self
    integer, dimension(8) :: value ! time value array
    real(kind=dbl) :: current_time ! in ms
    
    ! Get time
    call DATE_AND_TIME ( VALUES=value )
    current_time = 86400.D0 * value(3) + 3600.D0 * value(5) &
         & + 60.D0 * value(6) + value(7) + 0.001D0 * value(8)
    ! Get elapsed time in seconds
    elapsed_timer_fn = current_time - self%saved_time
  end function elapsed_timer_fn
  
end module timer_class

program foo

  use timer_class
  implicit none
  integer :: i, j, k
  type(timer_t) :: t  ! timer object

  ! start timers
  call t%start_timer()

  ! perform some calculations
  do i = 1,100000
     do j = 1,100000
        k = i+j
     end do
  end do

  ! Get the elapsed time
  write (*,'(a,f8.3,a)') 'Time =', t%elapsed_time(), ' s'
  
end program foo

