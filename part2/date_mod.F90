module date_class
  ! This module implements a date class, which stores
  ! and manipulates dates on the Gregorian calendar.

  implicit none
  
  type, public :: date
     ! Instance variables.
     ! Note ! date is January 1, 1900. that the default
     private
     integer :: year = 1900
     integer :: month = 1
     integer :: day = 1
   contains
     ! Year (0 - xxxx) ! Month (1-12)
     ! Day (1-31)
     ! Bound procedures
     procedure, public :: set_date => set_date_sub
     procedure, public :: get_day => get_day_fn
     procedure, public :: get_month => get_month_fn
     procedure, public :: get_year => get_year_fn
     procedure, public :: is_leap_year => is_leap_year_fn
     procedure, public :: is_equal => is_equal_fn
     procedure, public :: is_earlier_than => is_earlier_fn
     procedure, public :: is_later_than => is_later_fn
     procedure, public :: to_string => to_string_fn
  end type date

  ! Restrict access to the actual procedure names
  private :: set_date_sub, get_day_fn, get_month_fn, get_year_fn
  private :: is_leap_year_fn, is_equal_fn, is_earlier_fn
  private :: is_later_fn, to_string_fn
  
contains
  
  subroutine set_date_sub(this, day, month, year) !
    ! Subroutine to set the initial date
    class(date) :: this  ! Date object
    integer,intent(in) :: day ! Day (1-31)
    integer,intent(in) :: month ! Month (1-12)
    integer,intent(in) :: year ! Year (0 - xxxx)

    this%day = day
    this%month = month
    this%year = year
  end subroutine set_date_sub
  
  integer function get_day_fn(this)
    ! Function to return the day from this object 
    class(date),intent(in) :: this
    get_day_fn = this%day
  end function get_day_fn

  integer function get_month_fn(this)
    ! function to return the month from this object
    class(date) :: this
    ! get month
    get_month_fn = this%month
  end function get_month_fn
  
  integer function get_year_fn(this)
    ! function to return the year from this object
    class(date),intent(in) :: this
    get_year_fn = this%year
  end function get_year_fn
  
  logical function is_leap_year_fn(this) 
    ! is this year a leap year?
    class(date),intent(in) :: this

    if ( mod(this%year, 400) == 0 ) then
       is_leap_year_fn = .true.
    else if ( mod(this%year, 100) == 0 ) then
       is_leap_year_fn = .false.
    else if ( mod(this%year, 4) == 0 ) then
       is_leap_year_fn = .true.
    else
       is_leap_year_fn = .false.
    end if
  end function is_leap_year_fn
     
  logical function is_equal_fn(this,that) !
    ! are these two dates equal?
    class(date),intent(in) :: this ! date object
    class(date),intent(in) :: that ! another date for comparison
    
    if ( (this%year == that%year) .and. &
         (this%month == that%month) .and. &
         (this%day == that%day) ) then
       is_equal_fn = .true.
    else
       is_equal_fn = .false.
    end if
  end function is_equal_fn

  logical function is_earlier_fn(this,that)
    ! Is the date in "that" earlier than the date stored in the object?
    class(date),intent(in) :: this
    class(date),intent(in) :: that
    
    if ( that%year > this%year ) then
       is_earlier_fn = .false.
    else if ( that%year < this%year ) then
       is_earlier_fn = .true.
    else
       if ( that%month > this%month) then
          is_earlier_fn = .false.
       else if ( that%month < this%month ) then
          is_earlier_fn = .true.
       else
          if ( that%day >= this%day ) then
             is_earlier_fn = .false.
          else
             is_earlier_fn = .true.
          end if
       end if
    end if
  end function is_earlier_fn

  logical function is_later_fn(this, that)
    ! Is the date in "that" later than the date stored in the object?
    class(date),intent(in) :: this
    class(date),intent(in) :: that

    if ( that%year > this%year ) then
       is_later_fn = .true.
    else if ( that%year < this%year ) then
       is_later_fn = .false.
    else
       if ( that%month > this%month ) then
          is_later_fn = .true.
       else if ( that%month < this%month ) then
          is_later_fn = .false.
       else
          if ( that%day > this%day ) then
             is_later_fn = .true.
          else
             is_later_fn = .false.
          end if
       end if
    end if
  end function is_later_fn

  character(len=10) function to_string_fn(this) !
    ! Represent the date as a string: mm/dd/yyyy. !
    ! Declare calling arguments
    class(date),intent(in) :: this
    ! Declare local variables
    character(len=2) :: dd
    character(len=2) :: mm
    character(len=4) :: yy

    write (dd,'(i2.2)') this%day
    write (mm,'(i2.2)') this%month
    write (yy,'(i4)') this%year

    to_string_fn = mm // '/' // dd // '/' // yy
  end function to_string_fn
end module date_class

program test_date
  use date_class
  implicit none
  ! Declare local variables
  type(date) :: d1
  type(date) :: d2
  type(date) :: d3
  type(date) :: d4
  character(len=10) :: str1
  character(len=10) :: str2
  character(len=10) :: str3
  character(len=10) :: str4
  ! Initialize dates d1, d2, and d3 (d4 defaults)
  call d1%set_date(4,1,2016)
  call d2%set_date(1,3,2018)
  call d3%set_date(3,1,2016)
  ! Write out the dates
  str1 = d1%to_string()
  str2 = d2%to_string()
  str3 = d3%to_string()
  str4 = d4%to_string()
  write (*,'(a,a)') 'date 1 = ', str1
  write (*,'(a,a)') 'date 2 = ', str2
  write (*,'(a,a)') 'date 3 = ', str3
  write (*,'(a,a)') 'date 4 = ', str4
  ! Check for leap years
  if ( d1%is_leap_year() ) then
     write (*,'(i4,a)') d1%get_year(), ' is a leap year.'
  else
     write (*,'(i4,a)') d1%get_year(), ' is a not leap year.'
  end if
  if ( d2%is_leap_year() ) then
     write (*,'(i4,a)') d2%get_year(), ' is a leap year.'
  else
     write (*,'(i4,a)') d2%get_year(), ' is a not leap year.'
  end if
  ! Check for equality
  if ( d1%is_equal(d3) ) then
     write (*,'(3a)') str3, ' is equal to ', str1
  else
     write (*,'(3a)') str3, 'is not equal to ', str1
  end if
  ! Check is_earlier
  if ( d1%is_earlier_than(d3) ) then
     write (*,'(3a)') str3, ' is earlier than ', str1 
  else
     write (*,'(3a)') str3, ' is not earlier than ', str1
  end if
  ! Check is_later
  if ( d1%is_later_than(d3) ) then
     write (*,'(3a)') str3, ' is later than ', str1
  else
     write (*,'(3a)') str3, ' is not later than ', str1
  end if
end program test_date

  
