module math_functions
   implicit none
   
   private  ! control data access
   public :: my_sum, concat
   
   interface my_sum    ! polymorphism
      module procedure real_sum
      module procedure int_sum
   end interface

   interface operator(+)
      module procedure concat
   end interface

contains

   real function real_sum (a, b)
      real, intent(in) :: a,b
      real_sum = a + b
   end function real_sum
   
   integer function int_sum (a, b)
      integer, intent(in) :: a,b
      int_sum = a + b
   end function int_sum

   function concat(cha, chb)
      implicit none
      character (len=*), intent(in) :: cha, chb
      character (len = (LEN_TRIM(cha) + LEN_TRIM(chb))) :: concat
      concat = TRIM(cha)//TRIM(chb)

   end function concat
   
 end module math_functions

 program foo
   use math_functions
   implicit none
   print *, my_sum(2, 3)
   print *, my_sum(2., 3.)
   print *, concat('Fortran','90')
 end program foo
