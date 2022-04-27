program test_shapes

   use shapes_mod
   use polygon_mod
   use rectangle_mod
   use square_mod
   use triangle_mod
   use ellipse_mod
   use circle_mod
   implicit none

   type(ellipse)  , target :: elli
   type(circle)   , target :: circ
   type(polygon)  , target :: poly1, poly2, poly3
   type(rectangle), target :: rect
   type(square)   , target :: sq
   type(triangle) , target :: etri, rtri
   
   type list_of_shapes
      class(shape), pointer :: shap_p
   end type list_of_shapes
   type(list_of_shapes), dimension(9) :: list
    
   ! For an irregular polygon, we need coordinates
   ! Test #1 Area=8, perim=12
   real, dimension(4), target :: x_coords1 = [2.0,  2.0,  0.0, 0.0]
   real, dimension(4), target :: y_coords1 = [2.0, -2.0, -2.0, 2.0]
   ! Test #2 Area=6, perim=12
   real, dimension(3), target :: x_coords2 = [0.0,  3.0,  3.0] ! a=6
   real, dimension(3), target :: y_coords2 = [0.0,  4.0,  0.0] ! p=12
   ! Test #3 Area=128, perim=52
   real, dimension(6), target :: x_coords3 = [4.0,  4.0,  8.0,  8.0, -4.0, -4.0]
   real, dimension(6), target :: y_coords3 = [6.0, -4.0, -4.0, -8.0, -8.0,  6.0]
   
   real, pointer :: xc1(:), yc1(:), xc2(:), yc2(:), xc3(:), yc3(:)
   character(len=:), allocatable :: name_ptr
   integer i
  
   ! For formatting
   character(len=2) :: sl, fl
   character(len=16) :: fmt
   integer il

   ! Ellipses:
   elli =  ellipse( a=10.0, b=5.0, name='ellipse(a=10,b=5)')
   name_ptr = 'circle(r=1)'
   circ =  circle( 1.0, name_ptr)

   ! Polygons
   xc1 => x_coords1
   yc1 => y_coords1
   poly1 = polygon(x_coords=xc1, y_coords=yc1, name='polygon1')  
   xc2 => x_coords2
   yc2 => y_coords2
   poly2 = polygon(x_coords=xc2, y_coords=yc2, name='polygon2')  
   xc3 => x_coords3
   yc3 => y_coords3
   poly3 = polygon(x_coords=xc3, y_coords=yc3, name='polygon3')  

   name_ptr = 'rectangle(3x2)'
   rect =  rectangle( length=3.0, width=2.0, name=name_ptr)
   name_ptr = 'square(3x3)'
   sq   =  square( 3.0, name_ptr)
   name_ptr = 'triangle(5x5x5)'
   etri =  triangle( a=5.0, b=5.0, c=5.0, name=name_ptr)
   name_ptr = 'triangle(3x4x5)'
   rtri =  triangle( a=3.0, b=4.0, c=5.0, name=name_ptr)  ! same as polygon2
   
   list(1)%shap_p => elli
   list(2)%shap_p => circ
   list(3)%shap_p => poly1
   list(4)%shap_p => poly2
   list(5)%shap_p => poly3
   list(6)%shap_p => rect
   list(7)%shap_p => sq
   list(8)%shap_p => etri
   list(9)%shap_p => rtri

   write(6,'(1x,a,a)')'Object                  Area        Perimeter'
   write(6,'(1x,a)'  )'---------------------------------------------'

   do i = 1, size(list)
      
      write(fl, '(i0)') len(trim(list(i)%shap_p%get_name()))
      il = 24 - len(trim(list(i)%shap_p%get_name()))
      write(sl, '(i2)') il
      write(fmt, "(a)") '(1x,a'//fl//','//sl//'x)'
      write(6, fmt, advance='no') adjustl(list(i)%shap_p%get_name())
      write(6, '(2(f8.4,4x))') &
           list(i)%shap_p%get_area(), list(i)%shap_p%get_perimeter()

   end do

end program test_shapes
