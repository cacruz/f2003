module employee_class
  implicit none

  type, ABSTRACT, public :: employee
     character(len=30) :: first_name
     character(len=30) :: last_name
     character(len=11) :: ssn
     real :: pay = 0  ! monthly pay
   contains
     procedure, public :: set_employee => set_employee_sub
     procedure, public :: set_name => set_name_sub
     procedure, public :: set_ssn => set_ssn_sub
     procedure, public :: get_first_name => get_first_name_fn
     procedure, public :: get_last_name => get_last_name_fn
     procedure, public :: get_ssn => get_ssn_fn
     PROCEDURE(CALC_PAY_FN), PUBLIC, DEFERRED :: calc_pay
  end type employee

  ABSTRACT INTERFACE
     real function calc_pay_fn(this, hours)
       ! this function will be overridden by different subclasses.
       import :: employee
       class(employee) :: this
       real, intent(in) :: hours
     end function calc_pay_fn
  END INTERFACE

  private :: set_employee_sub, set_name_sub, set_ssn_sub
  private :: get_first_name_fn, get_last_name_fn, get_ssn_fn
  

contains

  subroutine set_employee_sub(this, first, last, ssn)
    class(employee) :: this
    character(len=*) :: first
    character(len=*) :: last
    character(len=*) :: ssn
    this%first_name = first
    this%last_name = last
    this%ssn = ssn
    this%pay = 0
  end subroutine set_employee_sub
  
  subroutine set_name_sub(this, first, last)
    class(employee) :: this
    character(len=*), intent(in) :: first
    character(len=*), intent(in) :: last
    this%first_name = first
    this%last_name = last
  end subroutine set_name_sub
  
  subroutine set_ssn_sub(this, ssn)
    class(employee) :: this
    character(len=*), intent(in) :: ssn
    this%ssn = ssn
  end subroutine set_ssn_sub
  
  character(len=30) function get_first_name_fn(this)
    class(employee) :: this
    get_first_name_fn = this%first_name
  end function get_first_name_fn
  
  character(len=30) function get_last_name_fn(this)
    class(employee) :: this
    get_last_name_fn = this%last_name
  end function get_last_name_fn
  
  character(len=30) function get_ssn_fn(this)
    class(employee) :: this
    get_ssn_fn = this%ssn
  end function get_ssn_fn
  
end module employee_class
