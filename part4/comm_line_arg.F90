program foo

   use ISO_FORTRAN_ENV
   implicit none

   integer, parameter :: MAXLEN_ARG=16
   character(len=MAXLEN_ARG) :: arg1, arg2
   character(len=64) :: myShell
   real, dimension(5) :: x = [1., 2., 3., 4. ,5.]
   real, parameter :: pi = 9801./(1103*sqrt(8.))
   integer :: nx, ny
   complex :: C = (1.0, pi)
   integer(int64) :: count_rate, count_max

   ! get argument count
   print *, 'Number of args: ',command_argument_count()

   ! get the arguments
   call get_command_argument(1, VALUE=arg1)
   call get_command_argument(2, VALUE=arg2)
   read(arg1,'(i2)') nx
   read(arg2,'(i2)') ny
   
   write(*,*) repeat('-', 60)
   write(6,'(1x,a,i2,a,i2)') 'User input: nx = ',nx,' ny = ',ny
   write(*,*) repeat('-', 60)
  
   ! get environment variable 
   call get_environment_variable('SHELL', myShell)
   write(6,*)'Value of SHELL: ',trim(myShell)

   ! new units   
   write(*,*) repeat('-', 60)
   write(6,'(1x,a,3(i5))')'ISO_FORTRAN_ENV constants IO units: ', &
        INPUT_UNIT, &
        OUTPUT_UNIT, ERROR_UNIT
   write(6,'(1x,a,3(i5))')'ISO_FORTRAN_ENV size constants: ', &
        NUMERIC_STORAGE_SIZE, &
        CHARACTER_STORAGE_SIZE, FILE_STORAGE_SIZE
   
   write(*,*) repeat('-', 60)
   write(6,'(1x,a,5(1x,f4.2))')'x: ',x
   write(*,*) repeat('-', 60)
   write(6,"(1x,a,f0.0,sp,f12.7,'i')") 'complex C: ',C
   write(*,*) repeat('-', 60)
  
   ! Access system clock 
   call system_clock(count_rate, count_max)
   print *, 'Clock resolution: ',real(count_max, real64) / count_rate

end program
