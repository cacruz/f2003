module circle_mod

   use ellipse_mod 
   implicit none
   private
   public circle

   type, extends(ellipse) :: circle
      real :: radius
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type circle

   interface circle
      procedure new_circle
   end interface circle
   
contains

   function new_circle(radius, name) result(a_circle)
      real, intent(in) :: radius
      character(len=:), allocatable :: name
      type(circle) :: a_circle
      
      a_circle%radius = radius
      a_circle%name = name
      
   end function new_circle
   
   real function get_area( this )
      class(circle), intent(in) :: this

      get_area =  PI * this%radius**2

   end function get_area
   
   real function get_perimeter( this )
      class(circle), intent(in) :: this

      get_perimeter =  2.0 * PI * this%radius

   end function get_perimeter

end module circle_mod
