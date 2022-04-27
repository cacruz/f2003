module salaried_employee_class
  
  use employee_class  ! use parent class
  implicit none

  type, public, extends(employee) :: salaried_employee
     ! Additional instance variables.
     private
     real :: salary = 0 ! monthly salary
   contains
     procedure, public :: set_salary => set_salary_sub
     procedure, public :: calc_pay => calc_pay_func
  end type salaried_employee

  private :: calc_pay_func, set_salary_sub

contains
  
  subroutine set_salary_sub(this, salary)
    class(salaried_employee) :: this
    real,intent(in) :: salary
    this%pay = salary
    this%salary = salary
  end subroutine set_salary_sub
  
  real function calc_pay_func(this,hours)
    ! Function to calculate the salaried employee pay. This
    ! function overrides the one in the parent class.
    class(salaried_employee) :: this
    real, intent(in) :: hours ! return pay
    calc_pay_func = this%salary
  end function calc_pay_func
  
end module salaried_employee_class
