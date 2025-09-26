subroutine da_qc_amsr2 (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for amsr2  data.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.

   real, parameter  :: clwcutofx(1:14) =  &
          (/0.350, 0.350, 0.350, 0.350, 0.350, 0.350, 0.300, &
            0.300, 0.250, 0.250, 0.100, 0.100, 0.020, 0.020  /)

   ! local variables
   integer   :: n,k,isflg,ios,fgat_rad_unit
   real      :: si, coscon, sincon, bearaz, sun_zenith, sun_azimuth, sun_glint
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),      &
                nrej_clw(nchan), num_proc_domain,  &
                nrej_land, nrej_glint

   character(len=30)  :: filename
   real      :: c37_mean

   if (trace_use) call da_trace_entry("da_qc_amsr2")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_clw(:)     = 0
   nrej_land       = 0
   nrej_glint      = 0
   num_proc_domain = 0

   coscon = cos( (90.0 - 55.0)*deg_to_rad)
   sincon = sin( (90.0 - 55.0)*deg_to_rad)

   do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2
      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      if ( crtm_cloud ) then
         ! calculate c37_mean
         c37_mean = 1.0-(ob%instid(i)%tb(11,n)-ob%instid(i)%tb(12,n)+  &
                    iv%instid(i)%tb_xb(11,n)-iv%instid(i)%tb_xb(12,n))/ &
                    (2.0*(iv%instid(i)%tb_xb_clr(11,n)-iv%instid(i)%tb_xb_clr(12,n)))
      end if

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      iv%instid(i)%tb_qc(:,n) = qc_good

      !  1.0 reject all channels over land
      !-----------------------------------------------------------------
      isflg = iv%instid(i)%isflg(n) !model surface type at ob location
      if (isflg > 0 .or. iv%instid(i)%landsea_mask(n) == 0) then
         iv%instid(i)%tb_qc(:,n)  = qc_bad
         nrej_land = nrej_land + 1
      end if

      !  2.0 check sun_glint angle for 6.9, 7.3 and 10.6 GHz
      !      It should be >=  25 degrees
      !-----------------------------------------------------------------
      bearaz      = 270.0 - iv%instid(i)%satazi(n)
      sun_zenith  = iv%instid(i)%solzen(n)
      sun_azimuth = 90.0 - iv%instid(i)%solazi(n)

      sun_glint = acos(coscon * cos( bearaz ) * cos( sun_zenith ) * cos( sun_azimuth ) + &
                  coscon * sin( bearaz ) * cos( sun_zenith ) * sin( sun_azimuth ) +  &
                  sincon * sin( sun_zenith )) * rad_to_deg

      if (sun_glint < 25.0) then
         ! apply only to 6.9,7.3 and 10.6 GHz for both V & H polarizations
         iv%instid(i)%tb_qc(1:6,n)  = qc_bad
         nrej_glint = nrej_glint + 1
      end if

      !  3.0 check iuse
      !-----------------------------------------------------------------
      do k = 1, nchan
         if (satinfo(i)%iuse(k) .eq. -1) &
               iv%instid(i)%tb_qc(k,n)  = qc_bad
      end do

      !  4.0 check cloud
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            ! clw check
            ! channel dependent criteria
            if( iv%instid(i)%clw(n) < 0 .or.             &  !bad obs retrieved clw
                iv%instid(i)%clw(n)  > clwcutofx(k) .or. &  !obs retrieved clw
                iv%instid(i)%clwp(n) > clwcutofx(k) ) then  !model/guess clwp
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_clw(k) = nrej_clw(k) + 1
            end if
         end do
      end if

      ! assigning obs errors
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            if (use_error_factor_rad) then
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error_std(k)*satinfo(i)%error_factor(k)
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            end if
         end do ! nchan

      else !crtm_cloud
         ! symmetric error model, Geer and Bauer (2011)
         do k = 1, nchan
            if (c37_mean.lt.0.05) then
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)
            else if (c37_mean.ge.0.05.and.c37_mean.lt.0.5) then
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)+ &
                  (c37_mean-0.05)*(satinfo(i)%error_cld(k)-satinfo(i)%error_std(k))/(0.5-0.05)
            else
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_cld(k)
            end if
         end do ! nchan

      end if

      !  5.0 check innovation
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         ! absolute departure check
         do k = 1, nchan
            if ( k <= 7 .or. k == 11 .or. k == 12) then
               if (abs(iv%instid(i)%tb_inv(k,n)) > 6.0) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
               end if
            else if ( k == 8 .or. k == 9 ) then
               if (abs(iv%instid(i)%tb_inv(k,n)) > 8.0) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
               end if
            else
               if (abs(iv%instid(i)%tb_inv(k,n)) > 10.0) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
               end if
            end if
         end do ! nchan
      end if

      do k = 1, nchan
         ! relative departure check
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
      end do      ! nchan

   end do ! end loop pixel
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (num_proc_domain)
   call da_proc_sum_ints (nrej_clw)
   call da_proc_sum_int (nrej_land)
   call da_proc_sum_int (nrej_glint)
   call da_proc_sum_ints (nrej_omb_abs)
   call da_proc_sum_ints (nrej_omb_std)
   call da_proc_sum_ints (nrej)
   call da_proc_sum_ints (ngood)

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
      if(num_proc_domain > 0) write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      if(nrej_land > 0)write(fgat_rad_unit,'(a20,i7)') ' nrej_land        = ', nrej_land
      if(nrej_glint> 0)write(fgat_rad_unit,'(a20,i7)') ' nrej_glint       = ', nrej_glint
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_abs(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_abs(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_std(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_std(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_clw(:)      = '
      write(fgat_rad_unit,'(10i7)')     nrej_clw(:)
      write(fgat_rad_unit,'(a20)')    ' nrej(:)          = '
      write(fgat_rad_unit,'(10i7)')     nrej(:)
      write(fgat_rad_unit,'(a20)')    ' ngood(:)         = '
      write(fgat_rad_unit,'(10i7)')     ngood(:)

      close(fgat_rad_unit)
      call da_free_unit(fgat_rad_unit)
   end if
   if (trace_use) call da_trace_exit("da_qc_amsr2")

end subroutine da_qc_amsr2
