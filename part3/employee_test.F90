program foo
  use hourly_employee_class
  use salaried_employee_class
  implicit none

  class(employee), pointer :: emp1, emp2
  type(salaried_employee), pointer :: sal_emp
  type(hourly_employee), pointer :: hourly_emp
  integer :: istat

  ! Create an object of type "salaried_employee"
  allocate( sal_emp, STAT=istat )
  
  ! Initialize the data in this object
  call sal_emp%set_employee('John','Jones','111-11-1111')
  call sal_emp%set_salary(3000.00)
  
  ! Create an object of type "hourly_employee"
  allocate( hourly_emp, STAT=istat )
  
  ! Initialize the data in this object
  call hourly_emp%set_employee('Jane','Jones','222-22-2222')
  call hourly_emp%set_pay_rate(12.50)
  
  ! Now create pointers to "employees".
  emp1 => sal_emp
  emp2 => hourly_emp

  ! Calculate pay using subclass pointers
  write (*,'(a)') 'Pay using subclass pointers:'
  write (*,'(a,f6.1)') 'Emp 1 pay = ', sal_emp%calc_pay(160.)
  write (*,'(a,f6.1)') 'Emp 2 pay = ', hourly_emp%calc_pay(160.)

  ! Calculate pay using superclass pointers
  write (*,'(a)') 'Pay using superclass pointers (polymorphism):'
  write (*,'(a,f6.1)') 'Emp 1 pay = ', emp1%calc_pay(160.)
  write (*,'(a,f6.1)') 'Emp 2 pay = ', emp2%calc_pay(160.)
  
  ! List employee information using superclass pointers
  write (*,*) 'Employee information:'
  write (*,*) 'Emp 1 Name / SSN = ', TRIM(emp1%get_first_name()) // &
       ' ' // TRIM(emp1%get_last_name()) // ' ', TRIM(emp1%get_ssn())
  write (*,*) 'Emp 2 Name / SSN = ', TRIM(emp2%get_first_name()) // &
       ' ' // TRIM(emp2%get_last_name()) // ' ', TRIM(emp2%get_ssn())
end program foo

! Notes:

! This test program creates one salaried_employee object and one hourly_employee
! object and assigns them to pointers of the same types.

! Then it creates polymorphic pointers to employee objects, and assigns
! the two subtype objects to the employee pointers.

! Normally, it is illegal to assign an object of one type to a pointer of
! another type. However, it is ok here because the objects of the subclassess
! salaried_ employee and hourly_employee are also objects of the superclass employee.

! The pointers were declared with the CLASS keyword, which allows them to
! match objects whose dynamic type is the declared type or any subclass of the
! declared type.

! Once the program assigns the objects to the employee pointers, it uses both
! the original pointers and the employee pointers to access some methods.

! It is possible to freely assign an object of a subclass to a pointer of a
! superclass type, since the object of the subclass is also an object of the superclass.
! However, the converse is not true. An object of a superclass type is not an object of
! its subclass types.

! Here, we were working with employee objects, but this program automatically
! selected the proper method to apply to each given object based on the subclass
! that it also belonged to. This ability to automatically vary methods depending
! on the subclass that an object belongs to is known as polymorphism.
