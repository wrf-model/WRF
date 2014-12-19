subroutine da_get_innov_vector_radar (it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)

   real, allocatable :: model_rv(:,:)
   real, allocatable :: model_rf(:,:)
   real, allocatable :: model_ps(:)

   real, allocatable :: model_qv(:,:)
   real, allocatable :: model_qs(:,:)
   real, allocatable :: model_tc(:,:)

   real, allocatable :: model_rho(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qcl(:,:)
   real, allocatable :: model_qci(:,:)
   real, allocatable :: model_qsn(:,:)
   real, allocatable :: model_qgr(:,:)

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   real    :: xr,yr,zr
   integer :: irv, irvf
   integer :: irf, irff

   real    :: alog_10, czr,czs,czg, zrr,zds,zws,zg,rze
   alog_10 = alog(10.0)

   ! Ze=zv*(ro*v)**1.75
   ! Zdb=10*log10(Ze)
   zrr = 3.63*1.00e+9  ! rainwater
   zds = 9.80*1.00e+8  ! dry snow
   zws = 4.26*1.00e+11 ! wet snow
   zg  = 4.33*1.00e+10 ! grauple

   
   if (trace_use) call da_trace_entry("da_get_innov_vector_radar")

   irv = 0; irvf = 0; irf = 0; irff = 0


   ! No point in going through and allocating all these variables if we're just going to quit anyway

   if ( use_radar_rf .and. use_radar_rhv ) then
      write(unit=message(1),fmt='(A)') "Both 'use_radar_rf' and 'use_radar_rhv' are set to true"
      write(unit=message(2),fmt='(A)') "You must only choose one of these options"
      call da_error(__FILE__,__LINE__,message(1:2))
   end if


   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_rv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_rf(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_qv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qs(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_tc(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_rho(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qcl(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qci(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qsn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qgr(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   model_p(:,:)    = 0.
   model_u(:,:)    = 0.
   model_v(:,:)    = 0.
   model_w(:,:)    = 0.

   model_rv(:,:)   = 0.
   model_rf(:,:)   = 0.
   model_ps(:)     = 0.

   model_qv(:,:)   = 0.
   model_qs(:,:)   = 0.
   model_tc(:,:)   = 0.

   model_rho(:,:)  = 0.
   model_qrn(:,:)  = 0.
   model_qcl(:,:)  = 0.
   model_qci(:,:)  = 0.
   model_qsn(:,:)  = 0.
   model_qgr(:,:)  = 0.

   if ( it > 1 ) then
     do n=iv%info(radar)%n1,iv%info(radar)%n2
        do k=1,iv%info(radar)%levels(n)
           if (iv%radar(n)%rv(k)%qc == fails_error_max) iv%radar(n)%rv(k)%qc = 0
           if (iv%radar(n)%rf(k)%qc == fails_error_max) iv%radar(n)%rf(k)%qc = 0
        end do
     end do
   end if

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      if (iv%info(radar)%levels(n) < 1) cycle

      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
      end do

      do k=1, iv%info(radar)%levels(n)
         call da_to_zk(iv%radar(n)%height(k), v_h, v_interp_h, iv%info(radar)%zk(k,n))

         if (iv%info(radar)%zk(k,n) < 1.0) then
            iv%radar(n)%height_qc(k) = below_model_surface
         else if (iv%info(radar)%zk(k,n) > mkz) then
            iv%radar(n)%height_qc(k) = above_model_lid
         end if
      end do
   end do

   call da_convert_zk (iv%info(radar))

   ! [2.0] Interpolate horizontally to ob points:

   call da_interp_lin_3d (grid%xb % p,   iv%info(radar), model_p)
#ifdef A2C
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u,'u')
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u)
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v)
#endif
   call da_interp_lin_3d (grid%xb % wh,  iv%info(radar), model_w)
   call da_interp_lin_3d (grid%xb % rho, iv%info(radar), model_rho)
   call da_interp_lin_3d (grid%xb % qrn, iv%info(radar), model_qrn)
   call da_interp_lin_3d (grid%xb % qcw, iv%info(radar), model_qcl)
   call da_interp_lin_3d (grid%xb % qci, iv%info(radar), model_qci)
   call da_interp_lin_3d (grid%xb % qsn, iv%info(radar), model_qsn)
IF ( ASSOCIATED( grid%xb%qgr ) ) THEN
   call da_interp_lin_3d (grid%xb % qgr, iv%info(radar), model_qgr)
END IF
   call da_interp_lin_3d (grid%xb % q,  iv%info(radar), model_qv)
   call da_interp_lin_3d (grid%xb % qs, iv%info(radar), model_qs)
   call da_interp_lin_3d (grid%xb % t,  iv%info(radar), model_tc)
   ! whl to TC
   model_tc = model_tc - 273.15

   ! Test 5.0E-5 as critical value. It can not be smaller.
   do n=iv%info(radar)%n1,iv%info(radar)%n2
      do k=1,iv%info(radar)%levels(n)
         ! for Xiao's default scheme
         if(use_radar_rf) model_qrn(k,n)=amax1(5.0E-5,model_qrn(k,n))
      end do

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)


      model_ps(n) = dxm *(dym * grid%xb % psac(i,  j) + dy * grid%xb%psac(i+1,  j)) + &
                 dx  *(dym * grid%xb % psac(i,j+1) + dy * grid%xb%psac(i+1,j+1)) + &
                 grid%xb % ptop

      iv%radar(n)%model_p(:)   = model_p(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_rho(:) = model_rho(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qrn(:) = model_qrn(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qcl(:) = model_qcl(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qci(:) = model_qci(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qsn(:) = model_qsn(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qgr(:) = model_qgr(1:iv%info(radar)%levels(n),n)

      iv%radar(n)%model_ps     = model_ps(n)

      ! [3.0] Calculate rv, rf at OBS location and initialise components of &
      ! innovation vector:

      if (fg_format == fg_format_wrf_arw_regional .or. &
          fg_format == fg_format_wrf_arw_global ) then
         call da_llxy_wrf(map_info, &
            iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      else
         call da_llxy_default( iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      end if

      xr = grid%xb%ds *(iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds *(iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      do k=1, iv%info(radar)%levels(n)
         iv % radar(n) % rv(k) % inv = 0.0
         iv % radar(n) % rf(k) % inv = 0.0

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
             iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rv) then
               if (abs(iv % radar(n) % rv(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rv(k) - missing_r) > 1.0 .AND. &
                       iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                  ! Rf can be used to control rv
                     zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                     call da_radial_velocity(model_rv(k,n), model_p(k,n),  &
                        model_u(k,n), model_v(k,n), model_w(k,n),          &
                        model_qrn(k,n), model_ps(n), xr, yr, zr)

                     iv % radar(n) % rv(k) % inv = ob % radar(n) % rv(k) - model_rv(k,n)
                  end if
               end if
            end if

            if (use_radar_rf) then
               if (abs(iv % radar(n) % rf(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rf(k) - missing_r) > 1.0 .AND. &
                     iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     model_rf(k,n) = leh1 + leh2 * alog10(model_rho(k,n) * model_qrn(k,n) * 1.0e+3)
                     iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - model_rf(k,n)
                  end if
               end if
            end if

            ! calculate retrieved hydrometeorological variables
            ! Jidong Gao JAS 2013
            if (use_radar_rhv) then
               if (abs(iv % radar(n) % rf(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rf(k) - missing_r) > 1.0 .AND. &
                     iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then

                     ! compute retrieved hydrometeoro varibles rhv 
                     if (it.eq.1) then
                        if (model_tc(k,n).ge.5.0) then
                           !JAMC 
                           !iv % radar(n) % rrno(k) = 10.0** ( (ob % radar(n) % rf(k)-leh1)/leh2 ) /model_rho(k,n)*0.001
                           !JAS
                           rze = 10.0**(ob % radar(n) % rf(k)/10.0)
                           iv % radar(n) % rrno(k) = exp ( log(rze/zrr)/1.75 )/model_rho(k,n)
                           ! rrn and rrno were assighed to missing values in read_obs_radar_ascii.inc
                           ! maximum value check, use the data under threshold 15g/kg
                           call da_radar_rf (model_qrn(k,n),model_qsn(k,n),model_qgr(k,n),model_tc(k,n),model_rho(k,n),rze)
                           if (iv % radar(n) % rrno(k) .ge. 0.015.or.ob % radar(n) % rf(k).ge.55) then
                              iv % radar(n) % rrn(k) % qc = -5  ! old -5 
                           else
                              iv % radar(n) % rrn(k) % qc = 0
                           end if
                        else if (model_tc(k,n).lt.5.0 .and. model_tc(k,n).ge.-5.0 ) then
                             czr=(model_tc(k,n)+5)/10.0
                             if (model_tc(k,n).le.0.0) then
                             czs = (1.0-czr)*zds/(zds+zg)
                             czg = (1.0-czr)*zg/(zds+zg)
                             else
                             czs = (1.0-czr)*zws/(zws+zg)
                             czg = (1.0-czr)*zg/(zws+zg)
                             end if
                             !iv % radar(n) % rrno(k) = 10.0** ( (czr*ob % radar(n) % rf(k)-leh1)/leh2 ) /model_rho(k,n)*0.001
                             rze = 10.0**(czr*ob % radar(n) % rf(k)/10.0)
                             iv % radar(n) % rrno(k) = exp ( log(rze/zrr)/1.75 )/model_rho(k,n)
                             rze = 10.0**(czs*ob % radar(n) % rf(k)/10.0)
                             iv % radar(n) % rsno(k) = exp ( log(rze/zds)/1.75 )/model_rho(k,n)
                             rze = 10.0**(czg*ob % radar(n) % rf(k)/10.0)
                             iv % radar(n) % rgro(k) = exp ( log(rze/zg )/1.75 )/model_rho(k,n)
                             iv % radar(n) % rrn(k) % qc = 0
                             iv % radar(n) % rsn(k) % qc = 0
                             iv % radar(n) % rgr(k) % qc = 0
                        else if (model_tc(k,n).le.-5.0) then
                             czs = zds/(zds+zg)
                             czg = 1.0 - czs
                             !   rra  = exp(log(rrze/zgr)/1.75)/ro1
                             rze = 10.0**(czs*ob % radar(n) % rf(k)/10.0) 
                             iv % radar(n) % rsno(k) = exp ( log(rze/zds)/1.75 )/model_rho(k,n)
                             rze = 10.0**(czg*ob % radar(n) % rf(k)/10.0)
                             iv % radar(n) % rgro(k) = exp ( log(rze/zg )/1.75 )/model_rho(k,n)
                             iv % radar(n) % rsn(k) % qc = 0
                             iv % radar(n) % rgr(k) % qc = 0
                        end if  ! temp
                     end if  ! it=1
!                     iv % radar(n) % rsn(k) % qc = -5  ! wenxue
!                     iv % radar(n) % rgr(k) % qc = -5 ! wenxue
                     if (iv % radar(n) % rrn(k) % qc >= obs_qc_pointer) then
                        ! x=b**y; y=logb(x)
                        iv % radar(n) % rrn(k) % inv = iv % radar(n) % rrno(k) - model_qrn(k,n)
                        iv % radar(n) % rrn(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rrno(k) * alog_10/leh2
                        ! rainwater error
                        iv % radar(n) % rrn(k) % error = amax1(0.0005,iv % radar(n) % rrn(k) % error)
                        iv % radar(n) % rrn(k) % error = amin1( 0.001,iv % radar(n) % rrn(k) % error)
                     end if
                     if (iv % radar(n) % rsn(k) % qc >= obs_qc_pointer) then
                        iv % radar(n) % rsn(k) % inv = iv % radar(n) % rsno(k) - model_qsn(k,n)
                        iv % radar(n) % rsn(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rsno(k) * alog_10/leh2
                        ! rainwater error
                        iv % radar(n) % rsn(k) % error = amax1(0.0005,iv % radar(n) % rrn(k) % error)
                        iv % radar(n) % rsn(k) % error = amin1( 0.001,iv % radar(n) % rrn(k) % error)
                     end if
                     if (iv % radar(n) % rgr(k) % qc >= obs_qc_pointer) then
                        iv % radar(n) % rgr(k) % inv = iv % radar(n) % rgro(k) - model_qgr(k,n)
                        iv % radar(n) % rgr(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rgro(k) * alog_10/leh2
                        ! rainwater error
                        iv % radar(n) % rgr(k) % error = amax1(0.0005,iv % radar(n) % rgr(k) % error)
                        iv % radar(n) % rgr(k) % error = amin1( 0.001,iv % radar(n) % rgr(k) % error)
                     end if
                  end if ! ob check
               end if ! iv qc and missing value check
            end if ! rhv

            ! retrieved water vapor
            if (use_radar_rqv) then
               !iv%%rqv and iv%%rqvo were assigned to missing values in read_obs_radar_ascii.inc
               !iter=1, rqv is missing; for second loop, dont change rqv value
               if (it .eq. 1) then
                  iv % radar(n) % rqvo(k) = 1.0*model_qs(k,n)
                  iv % radar(n) % rqv(k) % error = amax1(0.0005,0.1*iv % radar(n) % rqvo(k))
               end if

               !same condtions as use_radar_rf
               if (abs(iv % radar(n) % rf(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rf(k) - missing_r) > 1.0 .AND. &
                       iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv
                     if ( ob % radar(n) % rf(k) .lt. 30.0 .or. zr.lt.1500)then
                        iv % radar(n) % rqv(k) % qc = -5
                     else
                        iv % radar(n) % rqv(k) % qc = 0
                     end if
                     if (iv % radar(n) % rqv(k) % qc >= obs_qc_pointer) then
                        if (ob % radar(n) % rf(k) >= 30.0 .and. ob % radar(n) % rf(k) < 40.0) then
                           iv % radar(n) % rqvo(k)= 0.8*model_qs(k,n)
                        else if (ob % radar(n) % rf(k) >= 40.0 .and. ob % radar(n) % rf(k) < 50.0) then
                           iv % radar(n) % rqvo(k)= 0.9*model_qs(k,n)
                        end if
                        iv % radar(n) % rqv(k) % inv = iv % radar(n) % rqvo(k) - model_qv(k,n)
                        ! iv % radar(n) % rqv(k) % inv must be >= 0.0
                        iv % radar(n) % rqv(k) % inv = amax1(0.0,iv % radar(n) % rqv(k) % inv )
                        iv % radar(n) % rqv(k) % error = amax1(0.001,0.20*iv % radar(n) % rqvo(k))
                     end if
                  end if ! obs qc and mising
               end if  ! iv qc and missing
               if ( ob % radar(n) % rf(k) < -100) then
                  iv % radar(n) % rqvo(k)= amax1(0.85*model_qv(k,n),0.85*model_qs(k,n))
                  iv % radar(n) % rqv(k) % inv = iv % radar(n) % rqvo(k) - model_qv(k,n)
                  iv % radar(n) % rqv(k) % inv = amin1(0.0,iv % radar(n) % rqv(k) % inv )
                  iv % radar(n) % rqv(k) % error = amax1(0.001,0.20*iv % radar(n) % rqvo(k))
               end if
            end if  ! use rqv

         end if  ! not surface or model lid
      end do
   end do

   !------------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:  
   !------------------------------------------------------------------------

   if (check_max_iv)  then
      call da_check_max_iv_radar(iv, it, irv, irf, irvf, irff)
   end if

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(/,A,i5,A)')   &
         'For outer iteration ', it, ', Total Rejections for radar follows:'

      if (use_radar_rv) then
          write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rv observations:     ',irvf, ' on ',irv
      end if

      if (use_radar_rf) then
         write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rf observations:     ',irff, ' on ',irf
      end if
   end if

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)

   deallocate (model_rv)
   deallocate (model_rf)
   deallocate (model_ps)

   deallocate (model_qv)
   deallocate (model_qs)
   deallocate (model_tc)

   deallocate (model_qrn)
   deallocate (model_rho)
   deallocate (model_qcl) 
   deallocate (model_qci)
   deallocate (model_qsn)
   deallocate (model_qgr)
  
   if (trace_use) call da_trace_exit("da_get_innov_vector_radar")

end subroutine da_get_innov_vector_radar


