module shapes_mod
   implicit none
   private
   
   type, abstract, public :: shape
      character(len=:), allocatable :: name
   contains
      procedure(get_shape_area), deferred  :: get_area
      procedure(get_shape_perimeter), deferred  :: get_perimeter
      procedure, non_overridable :: get_name
   end type shape

   abstract interface
      real function get_shape_area( this )
         import                   :: shape
         class(shape), intent(in) :: this
      end function get_shape_area
   end interface

   abstract interface
      real function get_shape_perimeter( this )
         import                   :: shape
         class(shape), intent(in) :: this
      end function get_shape_perimeter
   end interface

contains

   function get_name( this ) result(name)
      class(shape), intent(in) :: this
      character(len=:), allocatable :: name
      name = this%name
   end function get_name

end module shapes_mod
