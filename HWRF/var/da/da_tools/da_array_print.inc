subroutine da_array_print(direction, a, ch)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: direction
   real*8, intent(in)             :: a(:,:)
   character (len=*), intent(in)  :: ch

   real                           :: amax
   integer                        :: i, j
   integer                        :: len1, len2
   integer                        :: jstart
   integer                        :: jend
   integer                        :: jump

   len1 = size(a(:,:),dim=1)
   len2 = size(a(:,:),dim=2)

   ! Writes the scalar field a

   write(unit=stdout,fmt='(A)') trim(ch)

   amax = MAXVAL(abs(a))
   write(unit=stdout, fmt='(a, 1pe15.8, 4i8)') &
        '   max(a)=', amax, shape(a)

   write(unit=stdout,fmt='(a, 1pe15.8, a)') &
        '   max(a)=', amax, ', i down, j horiz.'

   write(unit=stdout,fmt='(6x,288i3)') (i,i=1,len2)

   ! Direction indicates the order of the rows of the print out:

   if (direction == 1) then
      jstart = 1
      jend = len1
      jump = 1
   else
      jstart = len1
      jend = 1
      jump = -1
   end if

   if (amax.ne.0.0)then
      do j=jstart,jend,jump
         write(unit=stdout,fmt='(1x,i5,288i3)') &
            j, (inT(a(j,i)/amax*99.0),i=1,len2)
      end do
   end if

   write (unit=stdout,fmt='(A)') " "

end subroutine da_array_print


