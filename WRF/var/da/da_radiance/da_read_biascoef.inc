subroutine da_read_biascoef(string,nchan,nscan,nband,npred,global, &
      scanbias,scanbias_b,coef,coef0, vstd_dep)

   integer,           intent(in)    :: nchan,nscan,nband,npred
   logical,           intent(in)    :: global
   character(len=20), intent(in)    :: string
   real,              intent(inout) :: scanbias(nchan,nscan)
   real,              intent(inout) :: scanbias_b(nchan,nscan,nband)
   real,              intent(inout) :: coef(nchan,npred),coef0(nchan)
   real,              intent(inout) :: vstd_dep(nchan)

   integer :: iunit,j,i, ii,jj, iost
   integer :: nobs(nchan)
   real    :: vmean_abs(nchan)
   real    :: vstd_abs(nchan)
   real    :: vmean_dep(nchan)

   character(len=80) :: filename

   call da_trace_entry("da_read_biascoef")

   call da_get_unit(iunit)
   filename='biascorr/'//trim(adjustl(string))//'.bcor'
   open(unit=iunit,file=filename, form='formatted',iostat = iost, status='old')
   if (iost /= 0) then
      message(1)="Cannot open radiance biascorr file "//adjustl(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ! read (iunit,'(4i6)') nchan,nscan,nband,npred
   read (iunit,'(4i6)')
   do i=1, nchan
      read (iunit,'(i5,i10,4F8.2)') ii,nobs(i),vmean_abs(i),vstd_abs(i), vmean_dep(i),vstd_dep(i)
   end do

   do i=1, nchan
      read (iunit,'(i5,5F12.5)') ii,(coef(i,j),j=1,npred),coef0(i)
   end do

   read (iunit,*)
   read (iunit,*)
   if (global) then   ! global coefs not available now, use regional one
      do j=1, nchan
         read(iunit,'(i5,90F7.2)') jj, scanbias(j,1:nscan)
      end do
   else
      do j=1, nchan
         read(iunit,'(i5,90F7.2)') jj, scanbias(j,1:nscan)
      end do
   end if

   close(iunit)
   call da_free_unit(iunit)

   call da_trace_exit("da_read_biascoef")

end subroutine da_read_biascoef


