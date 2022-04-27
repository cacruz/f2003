module hourly_employee_class
  
  use employee_class  ! use parent class
  implicit none

  type, public, extends(employee) :: hourly_employee
     ! Additional instance variables.
     private
     real :: rate = 0 ! hourly rate
   contains
     procedure, public :: set_pay_rate => set_pay_rate_sub
     procedure, public :: calc_pay => calc_pay_func
  end type hourly_employee

  private :: calc_pay_func, set_pay_rate_sub

contains
  
  subroutine set_pay_rate_sub(this, rate)
    class(hourly_employee) :: this
    real,intent(in) :: rate
    this%rate = rate
  end subroutine set_pay_rate_sub
  
  real function calc_pay_func(this,hours)
    ! Function to calculate the salaried employee pay. This
    ! function overrides the one in the parent class.
    class(hourly_employee) :: this
    real,intent(in) :: hours ! return pay
    this%pay = hours * this%rate
    calc_pay_func = this%pay
  end function calc_pay_func
  
end module hourly_employee_class
