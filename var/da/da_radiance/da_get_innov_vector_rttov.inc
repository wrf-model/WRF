subroutine da_get_innov_vector_rttov (it, grid, ob, iv)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate grid%xb to obs location
   !       2. call forward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   implicit none
   
!#include "rttov_q2v.interface"

   integer,           intent(in)    :: it       ! External iteration.
   type (domain),     intent(in)    :: grid     ! first guess state.
   type (y_type),     intent(inout) :: ob       ! Observation structure.
   type (iv_type),    intent(inout) :: iv       ! O-B structure.

#if defined(RTTOV)

   integer  :: n        ! Loop counter.
   integer  :: k  ! Index dimension.
   integer  :: nlevels ! Number of obs levels.

   real*8                           :: seap, icep, lndp, snop
   real, allocatable :: v_p(:,:)

   integer :: inst, nchan
   real,    allocatable   :: polar(:)
   real,    allocatable   :: pres(:)
   logical :: amsua, amsub, ssmi
   ! logical :: mhs

   integer :: kn,knchpf,sensor_type,n1,n2,n1n2
   integer, allocatable :: kprof(:), kchan(:),kochan(:),knchan(:),indexn(:,:)
   real   , allocatable :: pemsv(:), pemsh(:), pems5(:)
   real   , allocatable :: zasat(:), zlsat(:)

! FIX?
real,allocatable :: temp(:), temp2(:), temp3(:,:)

   real, allocatable :: tgrn(:)
   real, allocatable :: tsk(:)

   type(con_vars_type),  allocatable :: con_vars(:)
   type(aux_vars_type),  allocatable :: aux_vars(:)

   ! variables for computing clwp
   real, allocatable :: dpf(:,:), clw(:,:), pf(:,:)

   ! variables for tb to radiance conversion
   real :: tstore

   if (trace_use) call da_trace_entry("da_get_innov_vector_rttov")

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      if ( iv%instid(inst)%num_rad < 1 ) cycle
      nlevels  = iv%instid(inst)%nlevels
      nchan    = iv%instid(inst)%nchan

      amsua = trim( rttov_inst_name(rtminit_sensor(inst))) == 'amsua'
      amsub = trim( rttov_inst_name(rtminit_sensor(inst))) == 'amsub'
      ! mhs = trim( rttov_inst_name(rtminit_sensor(inst))) == 'mhs'
      ssmi  = trim( rttov_inst_name(rtminit_sensor(inst))) == 'ssmi'
      sensor_type  = coefs(inst)%id_sensor - 1  ! 0:IR 1:MW

      if (iv%instid(inst)%info%n2 < iv%instid(inst)%info%n1) cycle
      n1 = iv%instid(inst)%info%n1
      n2 = iv%instid(inst)%info%n2

      n1n2=n2-n1+1

      allocate (pres(1:nlevels))
      allocate (con_vars(n1:n2))
      allocate (aux_vars(n1:n2))
      allocate (zasat(n1:n2))
      allocate (zlsat(n1:n2))
      allocate (polar(nchan))
 
      knchpf = n1n2*nchan
      ! knchpf = 1*nchan

      allocate (knchan (n1:n2))
      ! allocate (knchan (1))
      allocate (kprof  (knchpf))
      allocate (kchan  (knchpf))
      allocate (kochan (knchpf))
      allocate (indexn (1:nchan,n1:n2))
      allocate (pemsv  (knchpf))
      allocate (pemsh  (knchpf))
      allocate (pems5  (knchpf))

      pres(1:nlevels) = coefs(inst) % ref_prfl_p(1:nlevels)

      allocate(v_p(kms:kme,n1:n2))
      v_p(:,:)=0.0
      allocate(clw(kms:kme,n1:n2))
      allocate(dpf(kms:kme,n1:n2))
      allocate(pf(kms:kme+1,n1:n2))

      ! horizontal interpolate grid%xb pressure to ob position for every grid%xb levels
      do n=n1,n2
         do k=kts,kte
            ! convert to mb
            v_p(k,n) = 0.01*(iv%instid(inst)%info%dym(k,n)*( &
               iv%instid(inst)%info%dxm(k,n)*grid%xb%p(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n),k) + &
               iv%instid(inst)%info%dx(k,n) *grid%xb%p(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n),k)) + &
               iv%instid(inst)%info%dy(k,n) *( &
               iv%instid(inst)%info%dxm(k,n)*grid%xb%p(iv%instid(inst)%info%i(k,n),  iv%instid(inst)%info%j(k,n)+1,k) + &
               iv%instid(inst)%info%dx(k,n) *grid%xb%p(iv%instid(inst)%info%i(k,n)+1,iv%instid(inst)%info%j(k,n)+1,k)))
         end do
      end do
 
      call da_to_zk_new(pres, v_p(:,n1:n2), v_interp_p, n1n2,nlevels,iv%instid(inst)%info%zk(:,n1:n2))

      call da_convert_zk (iv%instid(inst)%info)

      ! [1.2] Interpolate horizontally to ob:
      call da_interp_lin_3d (grid%xb%t, iv%instid(inst)%info, iv%instid(inst)%t (:,n1:n2))
      call da_interp_lin_3d (grid%xb%q, iv%instid(inst)%info, iv%instid(inst)%mr(:,n1:n2))

      do n= n1,n2
         do k=1, nlevels
            if (iv%instid(inst)%info%zk(k,n) <= 0.0) then
               iv%instid(inst)%t(k,n) = coefs(inst) % ref_prfl_t(k,gas_id_watervapour)  ! outside model level
               iv%instid(inst)%mr(k,n) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
            else
               iv%instid(inst)%mr(k,n) = iv%instid(inst)%mr(k,n) * q2ppmv
               ! call rttov_q2v (&
               !    gas_unit_specconc,  &! in
               !    iv%instid(inst)%q(k,n),         &! in
               !    gas_id_watervapour, &! in
               !    iv%instid(inst)%q(k,n),         &! in
               !    iv%instid(inst)%mr(k,n)     )    ! inout
            end if
            if (pres(k) < 100.0) iv%instid(inst)%mr(k,n) = coefs(inst) % ref_prfl_mr(k,gas_id_watervapour)
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp( grid%xb%snow, grid%xb%xice, grid%xb%landmask,  &
            grid%xb%ivgtyp, grid%xb%isltyp, &
            ims, ime, jms, jme, &
            iv%instid(inst)%info%i(1,n), iv%instid(inst)%info%j(1,n), &
            iv%instid(inst)%info%dx(1,n), iv%instid(inst)%info%dy(1,n), &
            iv%instid(inst)%info%dxm(1,n), iv%instid(inst)%info%dym(1,n), &
            iv%instid(inst)%isflg(n),iv%instid(inst)%vegtyp(n), iv%instid(inst)%soiltyp(n), &
            seap, icep, lndp, snop )
      end do

      call da_interp_lin_2d (grid%xb % u10,  iv%instid(inst)%info, 1,iv%instid(inst)%u10(n1:n2))
      call da_interp_lin_2d (grid%xb % v10,  iv%instid(inst)%info, 1,iv%instid(inst)%v10(n1:n2))
      call da_interp_lin_2d (grid%xb % t2,   iv%instid(inst)%info, 1,iv%instid(inst)%t2m(n1:n2))
      call da_interp_lin_2d (grid%xb % q2,   iv%instid(inst)%info, 1,iv%instid(inst)%q2m(n1:n2))
      call da_interp_lin_2d (grid%xb % psfc, iv%instid(inst)%info, 1,iv%instid(inst)%ps (n1:n2))

      allocate (tgrn(n1:n2))
      allocate (tsk(n1:n2))
      call da_interp_lin_2d (grid%xb % tgrn, iv%instid(inst)%info, 1, tgrn)
      call da_interp_lin_2d (grid%xb % tsk, iv%instid(inst)%info, 1, tsk)

      do n=n1,n2
         if (iv%instid(inst)%isflg(n) == 0) then   ! over sea using SST
            iv%instid(inst)%ts(n) = tgrn(n)
         else
            iv%instid(inst)%ts(n) = tsk(n)
         end if
      end do
      deallocate (tgrn)
      deallocate (tsk)

      call da_interp_lin_2d (grid%xb%terr,  iv%instid(inst)%info, 1, iv%instid(inst)%elevation(n1:n2))

      ! variables for emissivity calculations
      !---------------------------------------- 

      call da_interp_lin_2d (grid%xb%smois,  iv%instid(inst)%info, 1, iv%instid(inst)%smois(n1:n2))
      call da_interp_lin_2d (grid%xb%tslb,   iv%instid(inst)%info, 1, iv%instid(inst)%tslb(n1:n2))
      call da_interp_lin_2d (grid%xb%snowh,  iv%instid(inst)%info, 1, iv%instid(inst)%snowh(n1:n2))
      call da_interp_lin_2d (grid%xb%vegfra, iv%instid(inst)%info, 1, iv%instid(inst)%vegfra(n1:n2))

      ! iv%instid(inst)%snowh(n1:n2) = iv%instid(inst)%snowh(n1:n2)*100.0   ! convert from m to mm
      iv%instid(inst)%vegfra(n1:n2) = 0.01*iv%instid(inst)%vegfra(n1:n2)  ! convert range to 0~1

      ! variables for calculation of cloud affected radiance
      !-------------------------------------------------------
      do k=kts,kte
         call da_interp_lin_2d (grid%xb%t  (:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%tm(k,:))
         call da_interp_lin_2d (grid%xb%q  (:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qm(k,:))
         call da_interp_lin_2d (grid%xb%qrn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qrn(k,:))
         call da_interp_lin_2d (grid%xb%qcw(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qcw(k,:))
       ! call da_interp_lin_2d (grid%xb%qci(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qci(k,:))
       ! call da_interp_lin_2d (grid%xb%qsn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qsn(k,:))
       ! call da_interp_lin_2d (grid%xb%qgr(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%qgr(k,:))
      end do

      iv%instid(inst)%pm(:,n1:n2)  = v_p(:,n1:n2)

      ! convert to hPa
      iv%instid(inst)%ps(n1:n2) = 0.01 * iv%instid(inst)%ps(n1:n2)
      iv%instid(inst)%mr2m(n1:n2) = iv%instid(inst)%q2m(n1:n2) * q2ppmv

      ! ADD for computing cloud liquid water path (mm) from guess

      pf(kts,n1:n2) = 100.0*iv%instid(inst)%ps(n1:n2)
      do k = 2,kte
         pf(k,n1:n2) = 50.0*(v_p(k-1,n1:n2)+v_p(k,n1:n2))
      end do
      pf(kte+1,n1:n2)= 50.0*v_p(kte,n1:n2)

      iv%instid(inst)%clwp(n1:n2) = 0.0
      do k = kts,kte
         dpf(k,n1:n2) = pf(k,n1:n2) - pf(k+1,n1:n2)
         clw(k,n1:n2) = iv%instid(inst)%qcw(k,n1:n2)*dpf(k,n1:n2)/gravity
         where (v_p(k,n1:n2)<100.0) clw (k,n1:n2) = 0.0
         iv%instid(inst)%clwp(n1:n2)  =iv%instid(inst)%clwp(n1:n2) + clw(k,n1:n2)
      end do

      ! call rttov_q2v (&
      !    gas_unit_specconc,  &! in
      !    iv%instid(inst)%q2m(n1:n2),          &! in
      !    gas_id_watervapour, &! in
      !    iv%instid(inst)%q2m(n1:n2),          &! in
      !    iv%instid(inst)%mr2m(n1:n2)    ! inout

      ! Compute surface emissivity for observation
      !-------------------------------------------

      zasat(n1:n2) = iv%instid(inst)%satzen(n1:n2) * deg2rad
      zlsat(n1:n2) = iv%instid(inst)%satazi(n1:n2) * deg2rad

      ! do i = n1,n2
      !    knchan(i) = nchan
      !    do kn=1,nchan 
      !       kprof((i-1)*nchan+kn)  = i
      !       kchan((i-1)*nchan+kn)  = kn
      !       kochan((i-1)*nchan+kn) = kn
      !       indexn(kn,i) = (-1)*nchan+kn 
      !    end do
      ! end do

      pemsv(:)    = 0.0
      pemsh(:)    = 0.0

      if (amsua) then
         polar(1:4) = 0.0  ! polarizition: 0:vertical; 1:horizontal
         polar(5:6) = 1.0
         polar(7)   = 0.0
         polar(8:14)= 1.0
         polar(15)  = 0.0
      end if
      if (amsub) then
         polar(1:5) = 0.0
      end if

      do n=n1,n2
         knchan(n) = nchan
         do kn=1,nchan 
            kprof((n-1)*nchan+kn)  = n
            kchan((n-1)*nchan+kn)  = kn
            kochan((n-1)*nchan+kn) = kn
            indexn(kn,n) = (n-1)*nchan+kn 
         end do
      end do

      if (coefs(inst)%id_sensor == 2) then ! microwave
         call da_trace("da_get_innov_vector_rttov",message="Calling gsi_emiss")
         call gsi_emiss(inst,knchpf,kprof,kchan,knchan,indexn, &
            iv%instid(inst)%satzen * deg2rad, &
            iv%instid(inst)%satazi * deg2rad, polar, &
            iv%instid(inst)%isflg,pemsv,pemsh,pems5, &
            iv%instid(inst)%ts, &
            iv%instid(inst)%soiltyp, &
            iv%instid(inst)%tslb, &
            iv%instid(inst)%smois, &
            iv%instid(inst)%vegtyp, &
            iv%instid(inst)%vegfra, &
            iv%instid(inst)%snowh, &
            sensor_type,n1n2,nchan, &
            iv%instid(inst)%tb_inv(:,:),amsua,amsub,ssmi, &
            iv%instid(inst)%u10, &
            iv%instid(inst)%v10)
      end if

      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( n, temp, temp2, temp3 )
      do n=n1,n2
         con_vars(n) % nlevels = nlevels 
         allocate (con_vars(n) % t(nlevels))
         allocate (con_vars(n) % q(nlevels))

         con_vars(n) % t(1:nlevels) = iv%instid(inst)%t(1:nlevels,n)
         con_vars(n) % q(1:nlevels) = iv%instid(inst)%mr(1:nlevels,n)
         con_vars(n) % ps           = iv%instid(inst)%ps(n)

         aux_vars(n) % t2m      = iv%instid(inst)%t2m(n)
         aux_vars(n) % q2m      = iv%instid(inst)%mr2m(n)
         aux_vars(n) % u10      = iv%instid(inst)%u10(n)
         aux_vars(n) % v10      = iv%instid(inst)%v10(n)
         aux_vars(n) % surftype = iv%instid(inst)%landsea_mask(n) 
         aux_vars(n) % surft    = iv%instid(inst)%ts(n)
         aux_vars(n) % satzen   = iv%instid(inst)%satzen(n)
         aux_vars(n) % satazi   = iv%instid(inst)%satazi(n)

         ! [1.3] Call RTM forward model

         ! da_rttov_direct nominally an array version, but doesn't handle arrays
         ! of surface flags properly
         allocate(temp(nchan),temp2(nchan),temp3(nchan,kts:kte))
         
         !----------------------------------------------------------------
         ! Compute Overcast Radiances for AIRS Cloud Detection(MMR)
         !----------------------------------------------------------------
	 call da_rttov_direct (inst, iv%instid(inst)%isflg(n), nchan, 1, &
            con_vars(n:n), aux_vars(n:n), &
            ! iv%instid(inst)%tb_xb(:,n), pemsv, pemsh, &
!!!            temp, pemsv((n-1)*nchan+1:n*nchan), pemsh((n-1)*nchan+1:n*nchan), &
            temp, temp2, temp3, pemsv((n-1)*nchan+1:n*nchan), pemsh((n-1)*nchan+1:n*nchan), &
            iv%instid(inst)%emiss(:,n))
         
	 iv%instid(inst)%tb_xb(:,n)=temp(:)
         iv%instid(inst)%rad_xb(:,n)=temp2(:)
         iv%instid(inst)%rad_ovc(:,:,n)=temp3(:,:)

         deallocate(temp,temp2,temp3)
      end do
      !$OMP END PARALLEL DO

      if (iv%instid(inst)%nchannels > nchan) then
         iv%instid(inst)%emiss(nchan+1:iv%instid(inst)%nchannels,n1:n2) = 0.0
      end if

      if (coefs(inst)%id_sensor == 2) then ! microwave
         do n=n1,n2
            iv%instid(inst)%emiss(1:nchan,n) = pems5(1+(n-n1)*nchan:(n-n1+1)*nchan)
         end do
      end if

      do n=n1,n2
         deallocate (con_vars(n) % t)
         deallocate (con_vars(n) % q)
      end do

      !----------------------------------------------------------------
      ! [2.0] calculate components of innovation vector:
      !----------------------------------------------------------------

      do n=n1,n2
         do k=1,nchan
            if (iv%instid(inst)%tb_inv(k,n) > missing_r) then
               iv%instid(inst)%tb_inv(k,n) = ob%instid(inst)%tb(k,n) - iv%instid(inst)%tb_xb(k,n)
            else
               iv%instid(inst)%tb_inv(k,n)   = missing_r
            end if
         end do
      end do

      deallocate (v_p)
      deallocate (clw)
      deallocate (dpf)
      deallocate (pf)
      deallocate (pres)
      deallocate (con_vars)
      deallocate (aux_vars)
      deallocate (zasat)
      deallocate (zlsat)
      deallocate (polar)
      deallocate (knchan)
      deallocate (kprof)
      deallocate (kchan)
      deallocate (kochan)
      deallocate (indexn)
      deallocate (pemsv)
      deallocate (pemsh)
      deallocate (pems5)

      call da_convert_zk (iv%instid(inst)%info)
   end do        ! end loop for sensor
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_rttov")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#endif
 
end subroutine da_get_innov_vector_rttov


