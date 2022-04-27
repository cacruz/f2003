module vector_class

  implicit none
  
  type, public :: vector_t
     private
     real, dimension(:), pointer :: v
     logical :: v_allocated = .false.
   contains
     procedure, public :: set_vector => set_vector_sub
     procedure, public :: get_vector => get_vector_sub
     final :: clean_vector
  end type vector_t

  ! Restrict access to the actual procedure names
  private :: set_vector_sub, get_vector_sub

contains
  
  subroutine set_vector_sub(this, array)
    class(vector_t) :: this
    real, dimension(:), intent(in) :: array ! input data
    integer :: istat ! allocate status
    
    ! save data, for deleting any data that might have been
    ! stored in this object.
    if ( this%v_allocated ) then
       deallocate(this%v,stat=istat)
    end if
    allocate(this%v(size(array,1)),stat=istat)
    this%v = array
    this%v_allocated = .true.

    ! Open a formatted file, call it array.dat, using unit number 99
    ! and write the data in this%v into it
    ! Note: you have to add unit_number to derived type definition

  end subroutine set_vector_sub
  
  subroutine get_vector_sub(this, array) !
    ! subroutine to get data in the vector !
    class(vector_t) :: this
    real,dimension(:),intent(out) :: array
    integer :: array_length
    integer :: data_length
    integer :: istat
    
    ! Retrieve data. If the size of the stored data does
    ! not match the array size, then return only a subset
    ! of the data or else pad the real data with zeros.
    if ( this%v_allocated ) then
       ! Return as much data as possible, truncating or
       ! zero padding as necessary.
       array_length = SIZE(array,1)
       data_length = SIZE(this%v,1)
       if ( array_length > data_length ) then
          array(1:data_length) = this%v
          array(data_length+1:array_length) = 0
       else if ( array_length == data_length ) then
          array = this%v
       else
          array = this%v(1:array_length)
       end if
    else
       ! no data--return zeros.
       array = 0
    end if
  end subroutine get_vector_sub

  subroutine clean_vector(this)
    type (vector_t), intent(inout) :: this
    integer :: istat
    ! Debugging message
    write (*,*) 'In finalizer ...'
    ! Save data, for deleting any data that might have been
    ! stored in this object.
    if ( this%v_allocated ) then
       deallocate(this%v, stat=istat)
    end if
    
    ! Another use of finalizer is to close files
    ! inquire (unit=this%unit_number, opened=file_opened)
    ! if ( file_opened ) close ( this%unit_number )
    
  end subroutine clean_vector
  
end module vector_class
   
program foo
  use vector_class
  implicit none
  real, dimension(6) :: array
  integer :: istat
  type(vector_t), pointer :: my_vec
  
  ! Create an object of type "vector" using the pointer
  allocate( my_vec, stat=istat )
  ! save an array of data in this object.
  array = [ 1., 2., 3., 4., 5., 6. ]
  call my_vec%set_vector(array)
  ! retrieve the data from this vector. array = 0
  call my_vec%get_vector(array)
  write (*,'(a,6f6.1)') 'vector = ', array
  ! destroy this object
  write (*,*) 'deallocating vector object ...'
  deallocate( my_vec, stat=istat )
end program foo
