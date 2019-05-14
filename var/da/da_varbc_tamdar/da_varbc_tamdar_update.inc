   subroutine da_varbc_tamdar_update (cv_size, cv, iv)

   !-----------------------------------!
   !  Update and write out parameters  !
   !-----------------------------------!

   implicit none

   integer,        intent(in)      :: cv_size
   real,           intent(in)      :: cv(1:cv_size)
   type (iv_type), intent(inout)   :: iv   

   integer                         :: i,ipred,iflt,iphase,npred
   integer                         :: iunit,iost
   character(len=filename_len)     :: filename
   character(len=99)               :: fmt_param
   character(len=3)                :: cphz(3)
   character(len=30)               :: stringn

   character(len=5), allocatable   :: tail_id(:)
   real, allocatable               :: sum_tl(:),varbc_param_tl(:,:,:),param(:,:,:)
   integer, allocatable            :: ifuse(:,:)


   if (trace_use) call da_trace_entry("da_varbc_tamdar_update")

   if (rootproc) write(unit=varbc_tamdar_unit,fmt='(//5X,A)') 'Updating parameters'
      
   npred = iv%varbc_tamdar%npred 
   cphz = (/'des','asc','cru'/)

   write(stringn,'(I3)') iv%varbc_tamdar%npred 
   stringn = '(9X,I5,2X,A,2X,A,'//trim(ADJUSTL(stringn))//'f9.3)'
   stringn = trim(adjustl(stringn))

   allocate( sum_tl(npred) )
   allocate( varbc_param_tl(npred,iv%varbc_tamdar%nphase,iv%varbc_tamdar%nair) )

   write(unit=varbc_tamdar_unit,fmt='(/10X,A)') &
         " ID  Phase  Parameters updated (npred)"

   varbc_param_tl(:,:,:) = 0.0

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase 
         if (iv%varbc_tamdar%nobs_sum(iphase,iflt) >= varbc_tamdar_nobsmin) then

             if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                 do ipred = 1, npred
                    varbc_param_tl(ipred,iphase,iflt) = &
                              SUM( cv(iv%varbc_tamdar%index(ipred,iphase,iflt)) * &
                                   iv%varbc_tamdar%vtox(ipred,1:npred,iphase,iflt) )
                 end do
             end if

             do ipred = 1, npred
                sum_tl(ipred) = wrf_dm_sum_real( varbc_param_tl(ipred,iphase,iflt) )
                iv%varbc_tamdar%param(ipred,iphase,iflt) = iv%varbc_tamdar%param(ipred,iphase,iflt) + sum_tl(ipred)
             end do

             if (rootproc .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                 write(unit=varbc_tamdar_unit,fmt=stringn) &
                       iv%varbc_tamdar%tail_id(iflt),cphz(iphase),' parameters updated : ',sum_tl(:)
             end if
         end if
      end do
   end do

   if (.not. rootproc) then
       deallocate(varbc_param_tl)
       if (trace_use) call da_trace_exit("da_varbc_tamdar_update")
       return
   end if

   !-----------------------------------!
   !  Write VARBC_TAMDAR.tbl.out file  !
   !-----------------------------------!
   
   allocate( tail_id (iv%varbc_tamdar%nair) )
   allocate( ifuse   (iv%varbc_tamdar%nphase,iv%varbc_tamdar%nair) )
   allocate( param   (iv%varbc_tamdar%nmaxpred,iv%varbc_tamdar%nphase,iv%varbc_tamdar%nair) )

   call da_get_unit(iunit)
   open(unit=iunit,file='VARBC_TAMDAR.tbl', form='formatted', status='old')

   do i = 1, 13
      read(iunit, *)
   end do
   do iflt = 1, iv%varbc_tamdar%nair
      read(iunit, *) tail_id(iflt), & 
                     ifuse(1:iv%varbc_tamdar%nphase,iflt), &
                   ( param(1:iv%varbc_tamdar%nmaxpred,iphase,iflt), iphase=1,iv%varbc_tamdar%nphase )
   end do

   close(iunit)
   call da_free_unit(iunit)


 ! write updated parameters

   call da_get_unit(iunit)
   filename = 'VARBC_TAMDAR.tbl.out'

   open(unit=iunit,file=trim(adjustl(filename)),iostat=iost,status='replace')
   if (iost /= 0) then
      message(1)="Cannot open TAMDAR bias correction file "//adjustl(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   write(iunit, '(A)')'*'
   write(iunit, '(A)')'  PARAMETER TABLE FOR TAMDAR VARBC'
   write(iunit, '(A)')' '
   write(iunit, '(A)')'  Table format:'
   write(iunit, '(A)')'- ID (1X,A5)'
   write(iunit, '(A)')'- Ifuse(ascent/desent/cruise) 1X,3(1X,I1)'
   write(iunit, '(A)')'- Parameters 3(5F7.3)'
   write(iunit, '(A)')' '
   write(iunit, '(A)')'  Preditors(1:5): 1.0, w, (optional: Mach, dT/dt, T)'
   write(iunit, '(A)')' '
   write(iunit, '(A)')'  Number of aircrafts currently involved in this table'
   write(iunit, '(I5)') iv%varbc_tamdar%nair
   write(iunit, '(A)')'*'

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase
         param(1:npred,iphase,iflt) = iv%varbc_tamdar%param(1:npred,iphase,iflt)
      end do

      write(iunit,fmt=trim(adjustl(iv%varbc_tamdar%fmt_param))) &
            tail_id(iflt), ifuse(iflt,1:iv%varbc_tamdar%nphase), &
          ( param(1:iv%varbc_tamdar%nmaxpred,iphase,iflt), iphase=1,iv%varbc_tamdar%nphase )
   end do

   close(iunit)
   call da_free_unit(iunit)

   deallocate(sum_tl, tail_id, ifuse, param, varbc_param_tl)

   if (rootproc) write(unit=varbc_tamdar_unit,fmt='(//5X,A/)') 'VARBC_TAMDAR is done'

   if (trace_use) call da_trace_exit("da_varbc_tamdar_update")

   end subroutine da_varbc_tamdar_update
