module complex_class
  implicit none

  ! only export the derived type, and not any of the
  ! constructors themselves
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

  public add_complex
  
contains
  
  type (complex_t) function new_complex(re, im)
    real, intent(in) :: re
    real, intent(in) :: im
    new_complex%re = re
    new_complex%im = im
  end function new_complex

  type (complex_t) function add_complex(x, y)
    type (complex_t), intent(in) :: x 
    type (complex_t), intent(in) :: y
    add_complex%re = x%re + y%re
    add_complex%im = x%im + y%im
  end function add_complex
  
end module complex_class

program foo

  use complex_class
  implicit none

  type (complex_t) :: n1, n2
  
  n1 = complex_t(3., 2.)
  n2 = complex_t(1., 7.)
  print *, add_complex(n1, n2)       ! 4 + 9i
  
end program foo
