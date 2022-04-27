module ellipse_mod
   use shapes_mod   
   implicit none
   private
   
   type, extends(shape), public :: ellipse
      real :: a, b
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type ellipse

   interface ellipse
      procedure new_ellipse
   end interface ellipse
   
   real, parameter, public :: PI = 4.0*atan(1.0)
   
contains

   function new_ellipse(a, b, name) result(a_ellipse)
      real, intent(in) :: a, b
      character(len=:), allocatable :: name
      type(ellipse) :: a_ellipse

      a_ellipse%a = a
      a_ellipse%b = b
      a_ellipse%name = name
      
   end function new_ellipse
   
   real function get_area( this )
      class(ellipse), intent(in) :: this

      get_area = PI * this%a * this%b

   end function get_area
   
   real function get_perimeter( this )
      class(ellipse), intent(in) :: this
      real :: h, terms
      integer :: i
      
      h = ( (this%a - this%b)**2 ) / ( (this%a + this%b)**2 )
      terms = 1.0 + 0.25*h + (1./64.)*h**2 + (1./256.)*h**3 + &
           (25./16384.)*h**4 + (49./65536.)*h**5 + (441./1048576.)*h**6  
      get_perimeter = PI * (this%a + this%b) * terms

      ! ! Ramanujan's approximation
      ! associate (a=>this%a, b=>this%b)
      !   get_perimeter = PI * (3.0*(a+b) - sqrt( (3.0*a+b)*(a+3.0*b) ))
      ! end associate
      
   end function get_perimeter

end module ellipse_mod
