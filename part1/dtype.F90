program foo
  implicit none
  
  ! Derived types
  type person
     character (len=64):: name
     integer :: age
     real :: weight
  end type person
    
  ! Supertype
  type corridor
     type(person), dimension(:), allocatable :: rooms(:)
     integer :: number_rooms
  end type corridor
  
  type(person) :: you, me
  type(corridor) :: a1

  you = person("Albert Einstein", 50, 75.0)

  print *, you%name ! contains the name of you
  print *, you%age  ! contains the age of you
  print *, you%weight  ! contains the weight of you

  a1%number_rooms = 10   ! 10 rooms in corridor a1
  allocate(a1%rooms(20)) ! 20 people in corridor a1
  
  a1%rooms(1) = you
  print *, a1%rooms(1)%name  ! you are in corridor a1   

end program foo
