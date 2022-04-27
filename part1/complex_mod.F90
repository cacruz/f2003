module complex_class
  implicit none

  private

  type, public :: complex_t
     real :: re   ! real part
     real :: im   ! imaginary part
   contains
     procedure :: add => add_complex_to_complex
  end type complex_t
  
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

  function add_complex_to_complex(self, num1) result (num2)
    class(complex_t), intent(in) :: self
    type(complex_t), intent(in) :: num1
    type(complex_t) :: num2
    num2%re = self%re + num1%re
    num2%im = self%im + num1%im
  end function add_complex_to_complex
  
end module complex_class

program foo

  use complex_class
  implicit none

  type (complex_t) :: n1, n2
  
  n1 = complex_t(3., 2.)
  n2 = complex_t(1., 7.)

  print *, n1%re, n1%im
  print *, n1%add(n2)       ! 4 + 9i
  
end program foo

