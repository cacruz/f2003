module employee_class
  implicit none

  ! Note that the instance variables in this class are not
  ! declared to be PRIVATE. These instance variables will
  ! need to be accessed by subclasses of the employee class.
  ! This is because the subclasses are in different modules and
  ! they would not be able to access the instance variables if
  ! they were declared to be PRIVATE.
  ! This is a Fortran limitation!
  type, public :: employee
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
     procedure, public :: calc_pay => calc_pay_fn
  end type employee

  private :: set_employee_sub, set_name_sub, set_ssn_sub
  private :: get_first_name_fn, get_last_name_fn, get_ssn_fn
  private :: calc_pay_fn
  
  ! Note that the calling arguments in each bound method include
  ! the object itself as the first parameter. This is necessary,
  ! because whenever a bound method with the PASS attribute is
  ! referenced by an object using the format obj%method(), the
  ! object itself is passed to the method as its first argument. 

contains

  ! The CLASS keyword in this list means that this subroutine
  ! will work with either an object of class employee or with an
  ! object of any subclass of employee.
  ! In Fortran terms, the declared type of the argument this is
  ! employee, but the dynamic type at runtime can be employee or
  ! any subclass of employee.
  
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
  
  ! The method calc_pay in this class returns a zero instead of
  ! calculating a valid pay, since the method of calculating the
  ! pay will depend on the type of employee, and we don’t know
  ! that information yet in this class.
  real function calc_pay_fn(this, hours)
    ! Note: this function will be overridden by different subclasses. 
    class (employee) :: this
    real, intent(in) :: hours
    calc_pay_fn = 0
  end function calc_pay_fn
  
end module employee_class
