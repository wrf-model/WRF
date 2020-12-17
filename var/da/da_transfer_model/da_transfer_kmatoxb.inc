subroutine da_transfer_kmatoxb(xbx, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Transfers fields from KMA to first guess (xb) structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xbx_type), intent(inout)     :: xbx          ! Header & non-gridded vars.

   type(domain), intent(inout)        :: grid

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke
   real    :: tmpvar, earth_radius_sq,conv
   real    :: tmpp,tmp_p,tmpps,tmp_ps
   ! real    :: rgh_fac(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme)
   character(len=19) :: current_date
 
   real :: TV(grid%xp%kms:grid%xp%kme)
   real :: ALPHA(grid%xp%kms:grid%xp%kme)
   real :: PHALF(grid%xp%kms:grid%xp%kme)
   real :: PHALFL(grid%xp%kms:grid%xp%kme)                  

   real :: pu, pd, coef, delp, hydro, rgasg, shgt

   real, dimension(jds:jde) :: loc_latc_mean

   real, allocatable :: arrayglobal(:,:) 

   if (trace_use) call da_trace_entry("da_transfer_kmatoxb")

   if (use_ssmiretrievalobs) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot use ssmi obs with kma global runs"/))
   end if

   conv = pi/180.0
   earth_radius_sq = earth_radius*1000.0 * earth_radius*1000.0 * &
                     conv*(360.0/(coarse_ix-1))*(180.0/(coarse_jy-2))*conv
   COEF=0.6080                                               
   RGASG = gas_constant/gravity                                            

   !---------------------------------------------------------------------------
   ! Set xb array range indices for processor subdomain.
   !---------------------------------------------------------------------------

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte
   ks = grid%xp % kts
   ke = grid%xp % kte

   grid%xb % ds    = grid%dx
   grid%xb % kma_a = grid%kma_a
   grid%xb % kma_b = grid%kma_b

   if (print_detail_xb) then
       write(unit=stdout, fmt='(/a/)') &
          'lvl         kma_a                 kma_b'

       do k=ks,ke
          write(unit=stdout, fmt='(i3,8e20.12)') k, grid%xb%kma_a(k), grid%xb%kma_b(k)
      end do
   end if

   grid%xb % mix = grid%xp % ide - grid%xp % ids + 1
   grid%xb % mjy = grid%xp % jde - grid%xp % jds + 1
   grid%xb % mkz = grid%xp % kde - grid%xp % kds + 1

   !---------------------------------------------------------------------------
   ! KMA-specific fields:
   !---------------------------------------------------------------------------

   ! Fix ptop as 0.4 hPa.
   ptop = 40.0     

   grid%xb % ptop = ptop
      
   !---------------------------------------------------------------------------
   ! Convert KMA fields to xb:
   !---------------------------------------------------------------------------

   grid%xb%lat(is:ie,js:je) =  grid%xlat(is:ie,js:je)
   grid%xb%lon(is:ie,js:je) = grid%xlong(is:ie,js:je)

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif

   tmpvar  = 1.0/real(ide-ids+1)

   !---------------------------------------------------------------
   ! transfer u,v,t & q(specific humidity in g/g)
   !---------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xb%u(i,j,k) = grid%u_2(i,j,k)
            grid%xb%v(i,j,k) = grid%v_2(i,j,k)
            grid%xb%t(i,j,k) = grid%t_2(i,j,k)
            grid%xb%q(i,j,k) = grid%moist(i,j,k,P_QV)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! Fix wind at Poles
   !---------------------------------------------------------------------------

    call da_get_vpoles(grid%xb%u,grid%xb%v,          &
       ids,ide,jds,jde, &
       grid%xp%ims,grid%xp%ime,grid%xp%jms,grid%xp%jme,grid%xp%kms,grid%xp%kme, &
       grid%xp%its,grid%xp%ite,grid%xp%jts,grid%xp%jte,grid%xp%kts,grid%xp%kte )

   if (print_detail_xb) then
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%its, grid%xp%ite=', grid%xp%its, grid%xp%ite
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%jts, grid%xp%jte=', grid%xp%jts, grid%xp%jte
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%kts, grid%xp%kte=', grid%xp%kts, grid%xp%kte
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%ims, grid%xp%ime=', grid%xp%ims, grid%xp%ime
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%jms, grid%xp%jme=', grid%xp%jms, grid%xp%jme
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%kms, grid%xp%kme=', grid%xp%kms, grid%xp%kme
      write(unit=stdout,fmt='(A,2I5)') 'ids, ide=', ids, ide
      write(unit=stdout,fmt='(A,2I5)') 'jds, jde=', jds, jde
      write(unit=stdout,fmt='(A,2I5)') 'grid%xp%kds, grid%xp%kde=', grid%xp%kds, grid%xp%kde

      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%u, dim=1)=', size(grid%xb%u, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%u, dim=2)=', size(grid%xb%u, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%u, dim=3)=', size(grid%xb%u, dim=3)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%v, dim=1)=', size(grid%xb%v, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%v, dim=2)=', size(grid%xb%v, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xb%v, dim=3)=', size(grid%xb%v, dim=3)

      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%u, dim=1)=', size(grid%xa%u, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%u, dim=2)=', size(grid%xa%u, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%u, dim=3)=', size(grid%xa%u, dim=3)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%v, dim=1)=', size(grid%xa%v, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%v, dim=2)=', size(grid%xa%v, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%xa%v, dim=3)=', size(grid%xa%v, dim=3)

      write(unit=stdout,fmt='(A,I5)') 'size(grid%u_2, dim=1)=', size(grid%u_2, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%u_2, dim=2)=', size(grid%u_2, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%u_2, dim=3)=', size(grid%u_2, dim=3)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%v_2, dim=1)=', size(grid%v_2, dim=1)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%v_2, dim=2)=', size(grid%v_2, dim=2)
      write(unit=stdout,fmt='(A,I5)') 'size(grid%v_2, dim=3)=', size(grid%v_2, dim=3)
   end if

   ALPHA(ke) = LOG(2.0)                                           

   do j=js,je
      do i=is,ie
         grid%xb%cori(i,j) = grid%f(i,j)
         grid%xb%psfc(i,j) = grid%psfc(i,j)
         grid%xb%terr(i,j) = grid%ht(i,j)

         ! Zhiquan Liu add some RTTOV variables
         !--------------------------------------
         grid%xb%t2(i,j) = grid%t2(i,j)
         grid%xb%q2(i,j) = grid%q2(i,j)
         grid%xb%u10(i,j) = grid%u10(i,j)
         grid%xb%v10(i,j) = grid%v10(i,j)
         grid%xb%tsk(i,j) = grid%tsk(i,j)
         ! currently KMA guess have no SST, replaced by TSK
         ! grid%xb%tgrn(i,j) = grid%sst(i,j)
         grid%xb%tgrn(i,j) = grid%tsk(i,j)
         grid%xb%landmask(i,j) = grid%landmask(i,j)
         grid%xb%xland(i,j) = grid%xland(i,j)

         grid%xb%smois(i,j) = grid%smois(i,j,1)
         grid%xb%tslb(i,j) = grid%tslb(i,j,1)
         grid%xb%xice(i,j) = grid%xice(i,j)
         grid%xb%ivgtyp(i,j) = grid%ivgtyp(i,j)
         grid%xb%isltyp(i,j) = grid%isltyp(i,j)
         grid%xb%vegfra(i,j) = grid%vegfra(i,j)
         ! convert snow water equivalent kg/m2 to snow depth(mm)
         grid%xb%snowh(i,j) = grid%snow(i,j)*10.0 

         !---------------------------------------------------------------------
         !  Syed RH Rizvi
         ! 
         ! The following code is to be activated after
         ! getting sst, land use, land mask and snow information for KMA grid
         ! This infor needs to be packed in "KMA2NETCDF" convertor
         !                              
         !---------------------------------------------------------------------

         ! grid%xb%tgrn(i,j) = grid%sst(i,j)
         ! if (grid%xb%tgrn(i,j) < 100.0) grid%xb%tgrn(i,j) = tmn(i,j)
         ! grid%xb%lanu(i,j)     = grid%lu_index(i,j)
         ! grid%xb%landmask(i,j) = grid%landmask(i,j)
         ! grid%xb%xland(i,j) = grid%xland(i,j)
         ! grid%xb%snow(i,j)     = grid%snowc(i,j)

         ! Since data is on full levels get full level pr & ht.
         ! Note p = A + B * Psfc formula needs Psfc to be in hPa 

         do K = ks,ke-1    
            PU  = grid%xb%KMA_A(K+1) + grid%xb%KMA_B(K+1)*grid%xb%psfc(i,j)/100.0
            PD  = grid%xb%KMA_A(K ) + grid%xb%KMA_B(K )*grid%xb%psfc(i,j)/100.0
            if (PU == PD)then
               message(1)='PU, PD equal and so denominator will be zero'
               write(unit=message(2),fmt='(A,3I5)') ' i, j, k = ',i,j,k
               write(unit=message(3),fmt='(A,2F10.2)') &
                  ' KMA_A ',grid%KMA_A(k),grid%KMA_A(K+1)
               write(unit=message(4),fmt='(A,2F10.2)') &
                  ' KMA_B ',grid%KMA_B(k),grid%KMA_B(K+1)
               write(unit=message(5),fmt='(A,2F10.2)') &
                  ' psfc(Pa) = ',grid%xb%psfc(i,j)
               call da_error(__FILE__,__LINE__,message(1:5))
            end if
            grid%xb%p(i,j,k)= 100.0 * exp ((PD*LOG(PD)-PU*LOG(PU))/(PD-PU) -1.0)
         end do 
 
         grid%xb%p(i,j,ke) = 100.0*(grid%xb%KMA_A(ke)+grid%xb%KMA_B(ke)*grid%xb%psfc(i,j)/100.0)/2.0

         do k=ks,ke
            if (grid%xb%t(i,j,k) <= 0.0) then
               write(unit=message(1),fmt='(A,3I5,F10.2)') &
                  'Problem in  temp = ',i,j,k,grid%xb%t(i,j,k)   
               call da_error(__FILE__,__LINE__,message(1:1))
            end if

            grid%xb%rho(i,j,k)=grid%xb%p(i,j,k)/(gas_constant*  &
               (1.0+COEF*grid%xb%q(I,J,K))*grid%xb%t(I,J,K))   
         end do

         ! compute full level height

         do K = ks,ke    
            PHALF(K) = grid%xb%KMA_A(K) + grid%xb%KMA_B(K)*grid%xb%psfc(I,J)/100.0                             
            TV   (K) = (1.0+COEF*grid%xb%q(I,J,K))*grid%xb%t(I,J,K)*rgasg    
         end do                                                           

         do K = ks,ke-1                                              
            DELP     = PHALF(K) - PHALF(K+1)                              
            PHALFL(K)= LOG(PHALF(K)/PHALF(K+1))                           
            ALPHA(K) = 1.0-PHALF(K+1)*PHALFL(K)/DELP                     
         end do  

         SHGT = grid%xb%terr(i,j)
         do K = ks, ke                                               
            grid%xb%h(I,J,K) = SHGT + ALPHA(K)*TV(K)                           
         end do 

         HYDRO = 0.0                                                    
         do K = ks+1, ke
            HYDRO = HYDRO + TV(K-1)*PHALFL(K-1)                           
            grid%xb%h(I,J,K) = grid%xb%h(I,J,K) + HYDRO                             
         end do                                                        
      end do
   end do

   !---------------------------------------------------------------------------
   ! Sigma values are needed for interpolating 
   ! background error statistics in vertical
   ! Fix sigmah values based on global average surface pressure 
   !                    and level wise pressure
   !---------------------------------------------------------------------------

   tmp_ps = sum(grid%xb%psfc(is:ie,js:je)) /real((ide-ids+1)*(jde-jds+1))

   tmpps = wrf_dm_sum_real(tmp_ps)

   do k=ks,ke
      tmp_p = sum(grid%xb%p(is:ie,js:je,k)) /real((ide-ids+1)*(jde-jds+1))
      tmpp = wrf_dm_sum_real(tmp_p)

      ! 0.01 is added to see that sigmah should not become negative
      grid%xb%sigmah(ke+ks-k) = (tmpp - ptop+0.01) / (tmpps- ptop+0.01) 
      if (grid%xb%sigmah(ke+ks-k) < 0.0) then
         write(unit=message(1),fmt='(A,F10.2)') &
            ' average surface pressure = ',tmpps  
         write(unit=message(2),fmt='(A,I5,A,F10.2)') &
            ' average pressure at this level= ',k,' = ',tmpp  
         write(unit=message(3),fmt='(A,I5,A,F10.2)') &
            ' negative sigmah(',ke+ks-k,') = ',grid%xb%sigmah(ke+ks-k) 
         call da_error(__FILE__,__LINE__,message(1:3))
      end if
   end do

   ! Fix ztop
   
   grid%xb%ztop = grid%xb%h(is,js,ke)

   if (print_detail_xb) then
      write(unit=stdout, fmt='(/5a/)') &
         'lvl         h                 p                t'

      do k=ks,ke
         write(unit=stdout, fmt='(i3,8e20.12)') k, &
            grid%xb%h(is,js,k), grid%xb%p(is,js,k), grid%xb%t(is,js,k)
      end do

      write (unit=stdout,fmt=*) ' '
      write (unit=stdout,fmt=*) 'grid%xb%u(is,je,ke)=', grid%xb%u(is,je,ke)
      write (unit=stdout,fmt=*) 'grid%xb%v(ie,js,ke)=', grid%xb%v(ie,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%w(is,js,ke)=', grid%xb%w(is,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%t(is,js,ke)=', grid%xb%t(is,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%p(is,js,ke)=', grid%xb%p(is,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%q(is,js,ke)=', grid%xb%q(is,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%hf(is,js,ke)=', grid%xb%hf(is,js,ke)
      write (unit=stdout,fmt=*) 'grid%xb%map_factor(is,js)=', grid%xb%map_factor(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%cori(is,js)=', grid%xb%cori(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%tgrn(is,js)=', grid%xb%tgrn(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%lat(is,js)=', grid%xb%lat(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%lon(is,js)=', grid%xb%lon(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%terr(is,js)=', grid%xb%terr(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%snow(is,js)=', grid%xb%snow(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%lanu(is,js)=', grid%xb%lanu(is,js)
      write (unit=stdout,fmt=*) 'grid%xb%landmask(is,js)=', grid%xb%landmask(is,js)
      write (unit=stdout,fmt=*) ' '
   end if

   write(current_date(1:19), fmt='(i4.4, 5(a1, i2.2))') &
      grid%start_year, '-', &
      grid%start_month, '-', &
      grid%start_day,   '_', &
      grid%start_hour,  ':', &
      grid%start_minute,':', &
      grid%start_second

   write(unit=stdout,fmt='(2A)') 'Current date is ',current_date 

   !---------------------------------------------------------------------------
   ! Syed RH Rizvi
   ! 
   ! Following code for calculating roughness length needs to be activated  
   ! after getting land use data for KMA grid 
   !---------------------------------------------------------------------------

   !
   ! Set proper value for "mminlu" if using KMA info

   !xbx % mminlu = 'USGS'
   !call da_roughness_from_lanu(19, xbx % mminlu, current_date, grid%xp, &
   !   grid%xb % lanu(grid%xp%ims,grid%xp%jms), grid%xb % rough(grid%xp%ims,grid%xp%jms))

   do j=js,je
      do i=is,ie
         if (grid%xb%ztop < grid%xb%hf(i,j,ke+1)) grid%xb%ztop = grid%xb%hf(i,j,ke+1)
         ! Calculate grid_box area and vertical inner product for use in 
         ! vertical transform:
         grid%xb % grid_box_area(i,j) = earth_radius_sq * cos(grid%xlat(i,j)*conv)

         if (vertical_ip == vertical_ip_sqrt_delta_p) then
            ! Vertical inner product is sqrt(Delta p):
            do k=1,grid%xb%mkz
               grid%xb % vertical_inner_product(i,j,k) = &
                  sqrt(grid%xb%p(i,j,k)-grid%xb%p(i,j,k+1))
            end do
         else if (vertical_ip == vertical_ip_delta_p) then
            ! Vertical inner product is Delta p:
            do k=1,grid%xb%mkz
            grid%xb % vertical_inner_product(i,j,k) = grid%xb%p(i,j,k)-grid%xb%p(i,j,k+1)
            end do
         end if

         !------------------------------------------------------------------------------
         !  Syed RH Rizvi
         ! 
         ! Following code for calculating roughness length needs to be activated  
         ! to calculate surface variables (10-m wind, and 2-m T, Q) , 
         ! After testing KMA-WRFVAR for SFC_ASSIM_OPTIONS = 2
         !
         !------------------------------------------------------------------------------

         ! call da_sfc_wtq(grid%xb%psfc(i,j), grid%xb%tgrn(i,j), &
         !    grid%xb%p(i,j,ks), grid%xb%t(i,j,ks), grid%xb%q(i,j,ks), &
         !    grid%xb%u(i,j,ks), grid%xb%v(i,j,ks), &
         !    grid%xb%p(i,j,ks+1), grid%xb%t(i,j,ks+1), grid%xb%q(i,j,ks+1), &
         !    grid%xb%h(i,j,ks), grid%xb%rough(i,j),grid%xb%landmask(i,j), &
         !    grid%xb%u10(i,j), grid%xb%v10(i,j), grid%xb%t2(i,j), grid%xb%q2(i,j), &
         !    grid%xb%regime(i,j))

         ! write (unit=stdout,fmt='(7I5)') &
         !    i,j,grid%xb%psfc(i,j),grid%xb%t2(i,j),grid%xb%q2(i,j), &
         !    grid%xb%u10(i,j),grid%xb%v10(i,j)
         ! ------------------------------------------------------------------------------

      end do
   end do

   !---------------------------------------------------------------------------
   ! Calculate saturation vapour pressure and relative humidity:
   !---------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke
         do i=is,ie
            call da_tpq_to_rh(grid%xb % t(i,j,k), grid%xb % p(i,j,k), &
               grid%xb % q(i,j,k), &
               grid%xb %es(i,j,k), grid%xb %qs(i,j,k), grid%xb %rh(i,j,k))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! Calculate dew point temperature:
   !---------------------------------------------------------------------------

   call da_trh_to_td (grid)

   if (print_detail_xb) then
      i=is; j=js; k=ks

      write (unit=stdout,fmt='(A,3I5)') 'i,j,k=', i,j,k
      write (unit=stdout,fmt='(A,F10.2)') 'grid%xb % td(i,j,k)=', grid%xb % td(i,j,k)
      write (unit=stdout,fmt='(A,F10.2)') 'grid%xb % es(i,j,k)=', grid%xb % es(i,j,k)
      write (unit=stdout,fmt='(A,F10.2)') 'grid%xb % rh(i,j,k)=', grid%xb % rh(i,j,k)
      write (unit=stdout,fmt='(A,F10.2)') 'grid%xb % qs(i,j,k)=', grid%xb % qs(i,j,k)
      write (unit=stdout,fmt='(A)') ' '
   end if

   !---------------------------------------------------------------------------
   ! Sea level pressure and total precipitable water
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------------------
   ! Following code for calculating roughness length needs to be activated  
   ! for calculating roughness length if sea level pressure is desired
   !---------------------------------------------------------------------------

   ! call da_wrf_tpq_2_slp (grid)

   call da_integrat_dz(grid)
   !---------------------------------------------------------------------------
   ! Following code for calculating roughness length needs to be activated  
   ! for surface wind speed for SFC_ASSIM_OPTIONS = 2 
   ! 
   ! It is not working because KMA terrain height output from wave to grid 
   ! transform is negative at many grid points
   !---------------------------------------------------------------------------

   ! tmpvar = log(10.0/0.0001)
   ! do j=js,je
   !    do i=is,ie
   !       if (grid%xb%hf(i,j,ks) <= 0) then
   !          write(unit=message(1), fmt=*) ' zero hf at i/j ',i,j,' ks= ',ks
   !          call da_error(__FILE__,__LINE__,message(1:1))
   !       end if
   !       rgh_fac(i,j) = 1.0/log(grid%xb%hf(i,j,ks)/0.0001)

   !       grid%xb%speed(i,j) = sqrt(grid%xb%u(i,j,ks)*grid%xb%u(i,j,ks) &
   !          + grid%xb%v(i,j,ks)*grid%xb%v(i,j,ks) + 1.0e-6) &
   !          *tmpvar*rgh_fac(i,j)
   !    end do
   ! end do

   !---------------------------------------------------------------------------
   ! Following code for calculating roughness length needs to be activated  
   ! if SSMI brightness temperature are used
   !---------------------------------------------------------------------------

   ! call da_transform_xtotb(xa)

   !---------------------------------------------------------------------------
   ! Calculate means for later use in setting up background errors.
   !---------------------------------------------------------------------------

   allocate (xbx % latc_mean(jds:jde))

   tmpvar = 1.0/real(ide-ids+1)
   loc_latc_mean(:) = 0.0

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not     
   ! testing.  This will always be .false. for a serial or 1-process MPI run.  
   if (test_dm_exact) then
      allocate(arrayglobal(ids:ide, jds:jde))
      call da_patch_to_global(grid, grid%xb%lat, arrayglobal)
      do j=jds,jde
         loc_latc_mean(j) = tmpvar*sum(arrayglobal(ids:ide, j))
      end do
      deallocate(arrayglobal)
      ! Broadcast result from monitor to other tasks.
      call wrf_dm_bcast_real(loc_latc_mean, (jde-jds+1))
      xbx % latc_mean = loc_latc_mean
   else
      do j=js,je
         loc_latc_mean(j) = tmpvar*sum(grid%xb % lat(is:ie, j))
      end do
      call wrf_dm_sum_reals (loc_latc_mean, xbx % latc_mean)
   end if

   ! WHY?
   !  do j=js,je
   !    do i=is,ie
   !       write(unit=stdout,fmt='(2i4,3f12.2,e12.4,2f12.2)') &
   !          j,i,grid%xb%psfc(i,j),grid%xb%tsk(i,j),grid%xb%t2(i,j), &
   !          grid%xb%q2(i,j),grid%xb%u10(i,j),grid%xb%v10(i,j)
   !    end do
   ! end do

   if (trace_use) call da_trace_exit("da_transfer_kmatoxb")

end subroutine da_transfer_kmatoxb


