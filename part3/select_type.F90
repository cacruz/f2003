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
  
  type(point), target :: p2
  type(point3d), target :: p3
  type(point_temp), target :: pt
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
  
  ! assign one of the objects to "p"
  p => pt

  ! now access the data in that object
  select type (p)
  type is ( point3d )
     write (*,*) 'type is point3d'
     write (*,*) p%x, p%y, p%z
  type is ( point_temp )
     write (*,*) 'type is point_temp'
     write (*,*) p%x, p%y, p%temp
  class is ( point )
     write (*,*) 'class is point'
     write (*,*) p%x, p%y     
  end select
  
end program foo
