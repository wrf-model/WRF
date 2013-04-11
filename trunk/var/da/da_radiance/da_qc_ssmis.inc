subroutine da_qc_ssmis (it,i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ssmis data.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.


   ! local variables
   integer   :: n,k,isflg,ios,fgat_rad_unit
   logical   :: lmix
   real      :: si37, q19, q37, term22v
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),      &
                nrej_mixsurface,nrej_windowchanl, nrej_rain, nrej_si,    &
                nrej_clw,nrej_clw_rv,nrej_topo,num_proc_domain

   character(len=30)  :: filename

   if (trace_use) call da_trace_entry("da_qc_ssmis")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_mixsurface = 0
   nrej_windowchanl= 0
   nrej_rain       = 0
   nrej_si         = 0
   nrej_clw        = 0
   nrej_clw_rv     = 0
   nrej_topo       = 0
   num_proc_domain = 0

   do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      !  0.0  initialise QC by flags assuming good obs
      !---------------------------------------------
      iv%instid(i)%tb_qc(:,n) = qc_good

      !  a.  reject all channels over mixture surface type
      !------------------------------------------------------
      isflg = iv%instid(i)%isflg(n)
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) then
         iv%instid(i)%tb_qc(:,n)  =  qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_mixsurface = nrej_mixsurface + 1
      end if
      !  b.  reject channels 1~2,8 over land/sea-ice/snow
      !------------------------------------------------------
      if (isflg > 0) then 
         iv%instid(i)%tb_qc(1:2,n)  = qc_bad
         iv%instid(i)%tb_qc(8,n)    = qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_windowchanl = nrej_windowchanl + 1
         if (only_sea_rad) iv%instid(i)%tb_qc(:,n)  = qc_bad
      end if

      !  c. reject rain_flagged data
      !------------------------------------------------------
      if (iv%instid(i)%rain_flag(n) == 1) then
         iv%instid(i)%tb_qc(:,n) = qc_bad
         iv%instid(i)%cloud_flag(:,n) = qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_rain = nrej_rain + 1
      end if

      !  d. check precipitation 
      !  Ferraro, 1997: Journal of Geophysical Research Vol 102, 16715-16735
      !  SI37 = 62.18 + 0.773 * TB19v - TB37v
      !  Q19 = -2.70 * ( ln(290-TB19v) - 2.84 - 0.40 * ln(290-TB22v) )
      !  Q37 = -1.15 * ( ln(290-TB37v) - 2.99 - 0.32 * ln(290-TB22v) )
      !-----------------------------------------------------------

      if ( isflg >= 2 ) then
         si37 = 62.18 + 0.773 * ob%instid(i)%tb(13,n) - ob%instid(i)%tb(16,n)
         if ( si37 >= 5.0 ) then
            if ( ob%instid(i)%tb(14,n) >= 264.0 ) then  ! snow check
               if ( ob%instid(i)%tb(13,n)-ob%instid(i)%tb(12,n) <= 20.0 ) then  ! desert check
                  if ( ob%instid(i)%tb(16,n) <= 253.0 .and.   &
                       ob%instid(i)%tb(13,n)-ob%instid(i)%tb(12,n) <= 7.0 ) then  ! arid soil check
                     iv%instid(i)%tb_qc(:,n) = qc_bad
                     iv%instid(i)%cloud_flag(:,n) = qc_bad
                     if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_si = nrej_si + 1
                   end if
               end if
            end if
          end if
      end if

      if ( isflg <= 1 ) then
         if ( ob%instid(i)%tb(14,n) < 290.0 ) then  ! assure positive for log
            term22v = log(290.0-ob%instid(i)%tb(14,n))
            if ( ob%instid(i)%tb(13,n) < 290.0 ) then  ! assure positive for log
               q19 = -2.70*(log(290.0-ob%instid(i)%tb(13,n))-2.84-0.40*term22v)
            end if
            if ( ob%instid(i)%tb(16,n) < 290.0 ) then  ! assure positive for log
               q37 = -1.15*(log(290.0-ob%instid(i)%tb(16,n))-2.99-0.32*term22v)
            end if
            if ( q19 >= 0.60 .or. q37 >= 0.20 ) then
               if ( ob%instid(i)%tb(14,n) >= 44.0+0.85*ob%instid(i)%tb(13,n) .or.  &
                    (ob%instid(i)%tb(14,n) <= 264.0 .and.    &
                     (ob%instid(i)%tb(14,n)-ob%instid(i)%tb(13,n)) >= 2.0) ) then
                  iv%instid(i)%tb_qc(:,n) = qc_bad
                  iv%instid(i)%cloud_flag(:,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_clw_rv = nrej_clw_rv + 1      ! clw retrieval
               end if
            end if
         end if
      end if
       
      if (iv%instid(i)%clwp(n) >= 0.2) then
         iv%instid(i)%tb_qc(:,n) = qc_bad
         iv%instid(i)%cloud_flag(:,n) = qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_clw = nrej_clw + 1               ! clw model
      end if

!      if ((isflg .ne. 0) .and. (iv%instid(i)%ps(n) < 850.0)) then
!         iv%instid(i)%tb_qc(5,n)  = qc_bad
!         if (iv%instid(i)%info%proc_domain(1,n)) &
!            nrej_topo = nrej_topo + 1
!      end if

      !  g. check iuse
      !-----------------------------------------------------------
      do k = 1, nchan
         if (satinfo(i)%iuse(k) .eq. -1) &
            iv%instid(i)%tb_qc(k,n)  = qc_bad
      end do

      !  f. check innovation
      !-----------------------------------------------------------
      do k = 1, nchan

         ! absolute departure check
         if (abs(iv%instid(i)%tb_inv(k,n)) > 15.0) then
            iv%instid(i)%tb_qc(k,n)  = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_omb_abs(k) = nrej_omb_abs(k) + 1
         end if

         ! relative departure check
         if (use_error_factor_rad) then
            iv%instid(i)%tb_error(k,n) = &
                satinfo(i)%error_std(k)*satinfo(i)%error_factor(k)
         else
            iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
         end if

         if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*iv%instid(i)%tb_error(k,n)) then
             iv%instid(i)%tb_qc(k,n)  = qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
                nrej_omb_std(k) = nrej_omb_std(k) + 1
         end if

         ! final QC decsion
         if (iv%instid(i)%tb_qc(k,n) == qc_bad) then
            iv%instid(i)%tb_error(k,n) = 500.0
            if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
            else
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
            end if

      end do ! chan
   end do ! end loop pixel
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (num_proc_domain)
   call da_proc_sum_int (nrej_mixsurface)
   call da_proc_sum_int (nrej_windowchanl)
   call da_proc_sum_int (nrej_rain )
   call da_proc_sum_int (nrej_si )
   call da_proc_sum_int (nrej_clw_rv)
   call da_proc_sum_int (nrej_clw)
!   call da_proc_sum_int (nrej_topo)
   call da_proc_sum_ints (nrej_omb_abs(:))
   call da_proc_sum_ints (nrej_omb_std(:))
   call da_proc_sum_ints (nrej(:))
   call da_proc_sum_ints (ngood(:))

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(i2.2,a,i2.2)') it,'_qcstat_'//trim(iv%instid(i)%rttovid_string)//'_',iv%time
      else
         write(filename,'(i2.2,a)') it,'_qcstat_'//trim(iv%instid(i)%rttovid_string)
      end if

      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(i)%rttovid_string
      write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchanl = ', nrej_windowchanl
      write(fgat_rad_unit,'(a20,i7)') ' nrej_rain        = ', nrej_rain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_si          = ', nrej_si
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw_rv      = ', nrej_clw_rv
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
!      write(fgat_rad_unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_abs(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_abs(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_std(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_std(:)
      write(fgat_rad_unit,'(a20)')    ' nrej(:)          = '
      write(fgat_rad_unit,'(10i7)')     nrej(:)
      write(fgat_rad_unit,'(a20)')    ' ngood(:)         = '
      write(fgat_rad_unit,'(10i7)')     ngood(:)

      close(fgat_rad_unit)
      call da_free_unit(fgat_rad_unit)
   end if

   if (trace_use) call da_trace_exit("da_qc_ssmis")

end subroutine da_qc_ssmis


