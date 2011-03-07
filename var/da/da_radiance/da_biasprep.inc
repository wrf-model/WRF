subroutine da_biasprep(inst,ob,iv)

   !-----------------------------------------------------------------------
   ! Purpose: Output information files for bias correction progs
   !-----------------------------------------------------------------------

   implicit none

   integer       ,  intent(in)      :: inst
   type (y_type) ,  intent(in)      :: ob         ! O structure.
   type (iv_type),  intent(in)      :: iv         ! O-B structure.

   integer  :: n,jx,npred,nchan,num_rad,nlevels
   character(len=80)  :: filename
   character(len=1)   :: s1
   real               :: pred(6)
   type (bias_type)   :: radbias
   real,allocatable      :: q(:), temp(:), hum(:), pf(:)

   num_rad = iv%instid(inst)%info%n2-iv%instid(inst)%info%n1+1

   if (num_rad < 1) return

   if (trace_use) call da_trace_entry("da_biasprep")

#ifdef DM_PARALLEL
   write(filename, '(a,i4.4)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)//'.', myproc
#else
   write(filename, '(a)') 'biasprep_'//trim(iv%instid(inst)%rttovid_string)
#endif

   call da_get_unit(biasprep_unit)
   open(unit=biasprep_unit,FILE=filename,FORM='unformatted')

   !---------------------------------------------------------------------------
   npred = 4
   nchan = iv%instid(inst)%nchan 
   nlevels = iv%instid(inst)%nlevels-1
   allocate(q(nlevels))
   allocate(temp(nlevels))
   allocate(hum(nlevels))
   allocate(pf(0:nlevels))

   allocate(radbias%tb(nchan))
   allocate(radbias%omb(nchan))
   allocate(radbias%bias(nchan))
   allocate(radbias%qc_flag(nchan))
   allocate(radbias%cloud_flag(nchan))
   allocate(radbias%pred(npred))

   do n=iv%instid(inst)%info%n1,iv%instid(inst)%info%n2
      if (iv%instid(inst)%info%proc_domain(1,n)) then 

        if (rtm_option==rtm_option_rttov) then
#ifdef RTTOV
         q(1:nlevels) = iv%instid(inst)%mr(1:nlevels,n)/q2ppmv
         call da_predictor_rttov(inst, pred(1:npred), npred, nlevels, &
            iv%instid(inst)%t(1:nlevels,n), q(1:nlevels), iv%instid(inst)%ts(n))
#endif
        else if (rtm_option==rtm_option_crtm) then
#ifdef CRTM
! FIX? problems with IBM AIX COMPILER
         temp(1:nlevels) = iv%instid(inst)%tm(1:nlevels,n)
         hum(1:nlevels) = iv%instid(inst)%qm(1:nlevels,n)
         pf(0:nlevels) = iv%instid(inst)%pf(0:nlevels,n)
         call da_predictor_crtm(pred(1:npred), npred, nlevels,temp, &
            hum, iv%instid(inst)%ts(n), pf)
#endif
        end if

         ! transfer information to bias structure
         radbias%platform_id  = iv%instid(inst)%platform_id
         radbias%satellite_id = iv%instid(inst)%satellite_id
         radbias%sensor_id    = iv%instid(inst)%sensor_id

         read(iv%instid(inst)%info%date_char(n),'(i4,5(a1,i2))') &
                                   radbias%year,s1, radbias%month,s1, radbias%day, &
                                   s1,radbias%hour, s1,radbias%min, s1,radbias%sec

         radbias%scanline     = iv%instid(inst)%scanline(n)    ! not available
         radbias%scanpos      = iv%instid(inst)%scanpos(n)
         radbias%landmask     = iv%instid(inst)%landsea_mask(n)
         radbias%elevation    = iv%instid(inst)%info%elv(n)
         radbias%lat          = iv%instid(inst)%info%lat(1,n)
         radbias%lon          = iv%instid(inst)%info%lon(1,n)
         radbias%surf_flag    = iv%instid(inst)%isflg(n)
         radbias%ps           = iv%instid(inst)%ps(n)
         radbias%t2m          = iv%instid(inst)%t2m(n)
         radbias%q2m          = iv%instid(inst)%mr2m(n)/q2ppmv
         radbias%tsk          = iv%instid(inst)%ts(n)
         radbias%clwp         = iv%instid(inst)%clwp(n)  ! in mm

         radbias%nchan        = nchan 
         radbias%tb(1:nchan)  = ob%instid(inst)%tb(1:nchan,n)
         radbias%omb(1:nchan) = ob%instid(inst)%tb(1:nchan,n)-iv%instid(inst)%tb_xb(1:nchan,n)
         radbias%bias(1:nchan) = 0.0

         radbias%npred         = npred
         radbias%pred(1:npred) = pred(1:npred)

         radbias%qc_flag(1:nchan)= iv%instid(inst)%tb_qc(1:nchan,n)
         radbias%cloud_flag(1:nchan)= iv%instid(inst)%cloud_flag(1:nchan,n)

         ! set missing data and bad data to missing
         do jx=1,nchan   
            if (radbias%tb(jx) < 150.0 .or. radbias%tb(jx) > 400.0 ) then
               radbias%tb(jx)   = missing_r
               radbias%omb(jx)  = missing_r 
            end if
         end do

         !write(unit=biasprep_unit) radbias ! can not compiled with pointer

         call da_write_biasprep(radbias)

      end if
   end do
  
   close(unit=biasprep_unit)
   call da_free_unit(biasprep_unit)

   deallocate(q)
   deallocate(temp)
   deallocate(hum)
   deallocate(pf)

   deallocate(radbias%tb)
   deallocate(radbias%omb)
   deallocate(radbias%bias)
   deallocate(radbias%qc_flag)
   deallocate(radbias%cloud_flag)
   deallocate(radbias%pred)

   if (trace_use) call da_trace_exit("da_biasprep")

end subroutine da_biasprep


