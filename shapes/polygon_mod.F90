module polygon_mod

   use shapes_mod   
   implicit none
   private
   public polygon
   
   type, extends(shape) :: polygon
      ! arrays of x and y coordinates traced in a clockwise
      ! direction, starting at any vertex.
      real, pointer :: x_coords(:) => null()
      real, pointer :: y_coords(:) => null()
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type polygon

   interface polygon
      procedure new_polygon
   end interface polygon
   
contains

   function new_polygon(xc, yc, name)
      real, target :: xc(:)
      real, target :: yc(:)
      character(len=:), allocatable :: name
      type(polygon) :: new_polygon
      
      new_polygon%x_coords => xc
      new_polygon%y_coords => yc
      new_polygon%name = name
      
   end function new_polygon

   ! "simple" polygon
   function get_area(this) result (area)
      class(polygon), intent(in) :: this
      real :: area
      integer :: i, num_points

      area = 0.0
      num_points = size(this%x_coords)
      
      associate ( x=>this%x_coords, y=>this%y_coords, n=>num_points )
        do i = 1, n-1
           area = area + (x(i)*y(i+1)) - (x(i+1)*y(i))
        end do
        area = area + x(n)*y(1) - x(1)*y(n)
      end associate
      area = abs(area)/2.0
      
   end function get_area

   function get_perimeter(this) result (perimeter)
      class(polygon), intent(in) :: this
      real :: perimeter
      integer :: i, num_points

      perimeter = 0.0
      num_points = size(this%x_coords)
      
      associate ( x=>this%x_coords, y=>this%y_coords, n=>num_points )
        do i = 1, n-1
           perimeter = perimeter + sqrt( (x(i+1)-x(i))**2 + (y(i+1)-y(i))**2 )
        end do
        ! last point
        perimeter = perimeter + sqrt( (x(1)-x(n))**2 + (y(1)-y(n))**2 )
      end associate
      
   end function get_perimeter
   
end module polygon_mod
