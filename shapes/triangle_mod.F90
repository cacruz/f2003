module triangle_mod

   use polygon_mod   
   implicit none
   private
   public triangle
   
   type, extends(polygon) :: triangle
      real :: a, b, c       ! triangle sides
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type triangle

   interface triangle
      procedure new_triangle
   end interface triangle
   
contains
   
   function new_triangle(a, b, c, name) result(a_triangle)
      real, intent(in) :: a, b, c
      character(len=:), allocatable :: name
      type(triangle) :: a_triangle
      
      a_triangle%a = a
      a_triangle%b = b
      a_triangle%c = c
      a_triangle%name = name
      
   end function new_triangle
   
   real function get_area( this )
      class(triangle), intent(in) :: this
      real :: s
      ! Heron's formula
      s = (this%a + this%b + this%c) / 2.0
      get_area = sqrt(s*(s-this%a)*(s-this%b)*(s-this%c))

   end function get_area
   
   real function get_perimeter( this )
      class(triangle), intent(in) :: this
      get_perimeter = this%a + this%b + this%c

   end function get_perimeter
   
end module triangle_mod
