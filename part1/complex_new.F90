module complex_class
  implicit none

  ! only export the derived type
  private

  ! type definition
  type, public :: complex_t
     real :: re   ! real part
     real :: im   ! imaginary part
  end type complex_t
  
  ! Writing an interface overloading 'complex_t' allows us to
  ! overload the type constructor
  interface complex_t
     procedure :: new_complex
  end interface complex_t
  
contains
  
  type (complex_t) function new_complex(re, im)
    real, intent(in) :: re
    real, intent(in) :: im
    new_complex%re = re
    new_complex%im = im
  end function new_complex
    
end module complex_class

program foo

  use complex_class
  implicit none

  type (complex_t) :: n
  
  n = complex_t(3., 2.)
  
  print *, n
  
end program foo
