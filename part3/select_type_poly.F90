program foo 
  implicit none
  
  ! Declare a 2D point type
  type :: point
     real :: x
     real :: y
  end type point
  
  ! declare a 3D point type
  type, extends(point) :: point3d
     real :: z
  end type point3d
  
  ! declare a 2D point with temperature data
  type, extends(point) :: point_temp
     real :: temp
  end type point_temp

  type, extends(point) :: data_point
     real, allocatable :: data_value(:)
  end type data_point

  type(point), target :: p2
  type(point3d), target :: p3
  type(point_temp), target :: pt
  type(data_point), target :: points
  
  class(point), pointer :: p

  ! initialize
  p2%x = 1.
  p2%y = 2.
  
  p3%x = -1.
  p3%y = 7.
  p3%z = -2.
  
  pt%x = 10.
  pt%y = 0.
  pt%temp = 700.

  allocate(points%data_value(3))
  points%data_value(1) = 1.
  points%data_value(2) = 2.
  points%data_value(3) = 3.
  
  ! assign one of the objects to "p"
  p => points

  ! now access the data in that object
  select type (p)
  type is ( point3d )
     write (*,*) 'type is point3d'
     write (*,*) p%x, p%y, p%z
  type is ( point_temp )
     write (*,*) 'type is point_temp'
     write (*,*) p%x, p%y, p%temp
  type is ( data_point )
     write (*,*) 'type is data _point'
     write (*,*) p%data_value
  class is ( point )
     write (*,*) 'class is point'
     write (*,*) p%x, p%y     
  end select

  print *, distance(points, points)
  
contains
  
  real function distance(a, b)
    class(point) :: a, b
    distance = sqrt((a%x-b%x)**2 + (a%y-b%y)**2)
  end function distance


end program foo
