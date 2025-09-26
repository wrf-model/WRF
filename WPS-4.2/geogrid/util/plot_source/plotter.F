program plotter

   implicit none

   integer :: nx, ny
   integer :: i, j
   real :: lu, val, xlat, xlon, left, right, bottom, top, maxter, minter
   integer, dimension(1200,1200) :: arr

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

   nx = 1200
   ny = 1200

   left = 0.1
   right = 0.9
   bottom = 0.1
   top = 0.9

   open(42,file='data.dat',form='formatted')
   do j=1,ny
      do i=1,nx
         read(42,*) arr(i,j)
      end do
   end do
   close(42)

   do j=1,ny
      do i=1,nx
         call map_square(real(left)+(real(i)/1200.)*0.8, real(bottom)+(real(j)/1200.)*0.8, 0.8/1200., 0.8/1200., arr(i,j)+1)
      end do
   end do

   call frame()

   call gclwk(13)

   call clsgks

end program plotter


subroutine map_square(u, v, width, height, colr)

    implicit none

    ! Arguments
    real :: width, height
    real :: u, v
    integer :: colr

    ! Local variables
    real, dimension(4) :: xra, yra
    real, dimension(2000) :: dst
    integer, dimension(3000) :: ind

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
