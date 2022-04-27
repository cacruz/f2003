module complex_class
  implicit none

  ! only export the derived type
  private

  ! type definition
  type, public :: complex_t
     real :: re   ! real part
     real :: im   ! imaginary part
  end type complex_t
  
contains

  ! add methods here
  
end module complex_class

program foo

  use complex_class
  implicit none

  type (complex_t) :: n
  
  n = complex_t(1., 2.)
  print *, n
  
end program foo
 
