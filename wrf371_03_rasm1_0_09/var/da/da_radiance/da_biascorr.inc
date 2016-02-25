subroutine da_biascorr ( i, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform bias correction for radiance data.
   !
   ! METHOD:  omb(corrected)=omb-scanbias-airbias
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: i       ! sensor index.
   type (y_type),  intent(in)    :: ob      ! Observation structure.
   type (iv_type), intent(inout) :: iv      ! O-B structure.

   ! Local variables
   integer   :: k,iband,iscan, n,j,npred,nlevels, num_rad
   real      :: pred(4),airbias
   real,allocatable      :: q(:), temp(:), hum(:), pf(:)

   num_rad = iv%instid(i)%info%n2-iv%instid(i)%info%n1+1
   if (num_rad < 1) return

   if (trace_use) call da_trace_entry("da_biascorr")

   npred=4
   nlevels=iv%instid(i)%nlevels-1
   allocate(temp(nlevels))
   allocate(hum(nlevels))
   allocate(pf(0:nlevels))

   do n=iv%instid(i)%info%n1,iv%instid(i)%info%n2
      ! get airmass predictors
      !-------------------------
      if (rtm_option==rtm_option_rttov) then
#ifdef RTTOV
         q(1:nlevels) = iv%instid(i)%mr(1:nlevels,n)/q2ppmv
         call da_predictor_rttov(i, pred(1:npred), npred, nlevels, &
              iv%instid(i)%t(1:nlevels,n), q(1:nlevels), iv%instid(i)%ts(n))
#endif
      else if (rtm_option==rtm_option_crtm) then
#ifdef CRTM
! FIX? problems with IBM AIX COMPILER
         temp(1:nlevels) = iv%instid(i)%tm(1:nlevels,n)
         hum(1:nlevels) = iv%instid(i)%qm(1:nlevels,n)
         pf(0:nlevels) = iv%instid(i)%pf(0:nlevels,n)
         call da_predictor_crtm(pred(1:npred), npred, nlevels,temp, &
            hum, iv%instid(i)%ts(n), pf)
#endif
      end if
      iscan = iv%instid(i)%scanpos(n)
      iband = floor(iv%instid(i)%info%lat(1,n)/10.0001) + 10
      do k=1,iv%instid(i)%nchan
         ! only do bias correction for used channels
         if ( (satinfo(i)%iuse(k) == 1) .and. (iv%instid(i)%tb_inv(k,n) > missing_r) ) then
            ! scan bias correction
            !-----------------------
            if (global) then
               iv%instid(i)%tb_inv(k,n) = iv%instid(i)%tb_inv(k,n) - satinfo(i)%scanbias_b(k,iscan,iband)
            else
               iv%instid(i)%tb_inv(k,n) = iv%instid(i)%tb_inv(k,n) - satinfo(i)%scanbias(k,iscan) 
            end if
            ! airmass bias correction
            !----------------------------
            airbias = satinfo(i)%bcoef0(k)
            do j=1,npred
               airbias= airbias + satinfo(i)%bcoef(k,j)*pred(j)
            end do
            iv%instid(i)%tb_inv(k,n) = iv%instid(i)%tb_inv(k,n)-airbias
         end if
      end do
   end do

   deallocate(q)
   deallocate(temp)
   deallocate(hum)
   deallocate(pf)

   if (trace_use) call da_trace_exit("da_biascorr")

end subroutine da_biascorr

