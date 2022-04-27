module rectangle_mod

   use polygon_mod   
   implicit none
   private
   public rectangle
   
   type, extends(polygon) :: rectangle
      real :: length, width
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type rectangle

   interface rectangle
      procedure new_rectangle
   end interface rectangle
   
contains
   
   function new_rectangle(length, width, name) result(a_rectangle)
      real, intent(in) :: length, width
      character(len=:), allocatable :: name
      type(rectangle) :: a_rectangle
      
      a_rectangle%length = length
      a_rectangle%width = width
      a_rectangle%name = name
    
   end function new_rectangle
   
   real function get_area( this )
      class(rectangle), intent(in) :: this

      get_area = this%width * this%length

   end function get_area
   
   real function get_perimeter( this )
      class(rectangle), intent(in) :: this

      get_perimeter = 2.0 * (this%width + this%length)

   end function get_perimeter

end module rectangle_mod
