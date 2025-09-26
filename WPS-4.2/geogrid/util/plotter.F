program plotter

   implicit none

   integer :: nx, ny
   integer :: i, j
   real :: start_r, start_g, start_b, end_r, end_g, end_b
   real :: lu, val, xlat, xlon, left, right, bottom, top, maxter, minter

   call opngks

   call gopwk(13, 41, 3)

   call gscr(1, 0, 1.00, 1.00, 1.00)
   call gscr(1, 1, 0.00, 0.00, 0.00)
   call gscr(1, 2, 0.25, 0.25, 0.25)
   call gscr(1, 3, 1.00, 1.00, 0.50)
   call gscr(1, 4, 0.50, 1.00, 0.50)
   call gscr(1, 5, 1.00, 1.00, 0.00)
   call gscr(1, 6, 1.00, 1.00, 0.00)
   call gscr(1, 7, 0.50, 1.00, 0.50)
   call gscr(1, 8, 1.00, 1.00, 0.50)
   call gscr(1, 9, 0.50, 1.00, 0.50)
   call gscr(1,10, 0.50, 1.00, 0.50)
   call gscr(1,11, 1.00, 1.00, 0.50)
   call gscr(1,12, 0.00, 1.00, 0.00)
   call gscr(1,13, 0.00, 0.50, 0.00)
   call gscr(1,14, 0.00, 1.00, 0.00)
   call gscr(1,15, 0.00, 0.50, 0.00)
   call gscr(1,16, 0.00, 1.00, 0.00)
   call gscr(1,17, 0.50, 0.50, 1.00)
   call gscr(1,18, 0.00, 1.00, 0.00)
   call gscr(1,19, 0.00, 1.00, 0.00)
   call gscr(1,20, 0.75, 0.75, 0.75)
   call gscr(1,21, 0.75, 0.75, 0.75)
   call gscr(1,22, 0.00, 0.50, 0.00)
   call gscr(1,23, 0.75, 0.75, 0.75)
   call gscr(1,24, 0.75, 0.75, 0.75)
   call gscr(1,25, 1.00, 1.00, 1.00)

   start_r = 0.00
   end_r   = 0.50
   start_g = 1.00
   end_g   = 0.25
   start_b = 0.00
   end_b   = 0.00
   do i=26,76
     call gscr(1,i,start_r+((end_r-start_r)/50.)*real(i-26),start_g+((end_g-start_g)/50.)*real(i-26),start_b+((end_b-start_b)/50.)*real(i-26))
   end do

   start_r = 0.50
   end_r   = 1.00
   start_g = 0.25
   end_g   = 1.00
   start_b = 0.00
   end_b   = 1.00
   do i=77,126
     call gscr(1,i,start_r+((end_r-start_r)/50.)*real(i-77),start_g+((end_g-start_g)/50.)*real(i-77),start_b+((end_b-start_b)/50.)*real(i-77))
   end do

   nx = 5324
   ny = 3344

   left = 0.1
   right = 0.9
   bottom = 0.1
   top = 0.9
   call mappos(left,right,bottom,top)
   call mapstc('OU','US')
   call maproj('LC', 30., -98.00, 60.)
   call mapset('CO', 20.144764, -122.505325, 48.201309, -59.35916)
   call mapint()

   open(42,file='lu.dat',form='formatted')
   open(43,file='lat.dat',form='formatted')
   open(44,file='lon.dat',form='formatted')

   do j=1,ny 
      do i=1,nx 
         read(42,*) val
         read(43,*) xlat
         read(44,*) xlon
         call map_square(xlat, xlon, (right-left)/real(nx), (top-bottom)/real(nx), nint(val)+1)
      end do
   end do

   close(42)
   close(43)
   close(44)

   call maplot() 

   call frame()

   open(41,file='lu.dat',form='formatted')
   open(42,file='ter.dat',form='formatted')
   open(43,file='lat.dat',form='formatted')
   open(44,file='lon.dat',form='formatted')
   maxter = -1000.
   minter = 10000.
   do j=1,ny 
      do i=1,nx 
         read(42,*) val
         if (val > maxter) maxter = val 
         if (val < minter) minter = val 
      end do
   end do

   rewind(42)

   do j=1,ny 
      do i=1,nx 
         read(41,*) lu
         read(42,*) val
         read(43,*) xlat
         read(44,*) xlon
         val = ((val-minter) * 99.)/(maxter-minter) + 26.
         if (nint(lu) ==  16) then
            call map_square(xlat, xlon, (right-left)/real(nx), (top-bottom)/real(nx), 17)
         else if (nint(lu) ==  1) then
            call map_square(xlat, xlon, (right-left)/real(nx), (top-bottom)/real(nx), 2)
         else
            call map_square(xlat, xlon, (right-left)/real(nx), (top-bottom)/real(nx), nint(val))
         end if
      end do
   end do

   close(41)
   close(42)
   close(43)
   close(44)

   call maplot() 

   call gclwk(13)

   call clsgks

end program plotter


subroutine map_square(rlat, rlon, width, height, colr)

    implicit none

    ! Arguments
    real :: rlat, rlon, width, height
    integer :: colr

    ! Local variables
    real :: u, v
    real, dimension(4) :: xra, yra
    real, dimension(2000) :: dst
    integer, dimension(3000) :: ind

    call maptrn(rlat, rlon, u, v)

    u = u + (width/2.)
    v = v + (height/2.)

    xra(1) = u-(width/2.)
    xra(2) = u+(width/2.)
    xra(3) = u+(width/2.)
    xra(4) = u-(width/2.)

    yra(1) = v-(height/2.)
    yra(2) = v-(height/2.)
    yra(3) = v+(height/2.)
    yra(4) = v+(height/2.)

    call sfsgfa(xra, yra, 4, dst, 2000, ind, 3000, colr)

end subroutine map_square
