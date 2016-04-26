!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_WRF_HYDRO

#ifdef MPP_LAND
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                mpp_land_bcast_real, mpp_land_bcast_int1
    use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
#endif
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe

    use module_rt_data, only:  rt_domain
    use module_gw_gw2d_data, only:  gw2d
    use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
    use module_namelist, only: nlst_rt
    USE module_domain, ONLY : domain, domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type
    !yw USE module_configure, only : config_flags
    USE module_configure, only: model_config_rec
 

    implicit none
     
    !yw   added for check soil moisture and soiltype
    integer ::  checkSOIL_flag

#ifndef MPP_LAND
    character(len=19) :: cpl_outdate
#endif
!
! added to consider the adaptive time step from WRF model. 
    real    :: dtrt_ter0  , dtrt_ch0
    integer ::  mm0




CONTAINS

!wrf_cpl_HYDRO will not call the off-line lsm 
!ywGW subroutine wrf_cpl_HYDRO(HYDRO_dt,grid, config_flags, its,ite,jts,jte)
    subroutine wrf_cpl_HYDRO(HYDRO_dt,grid,its,ite,jts,jte)

       implicit none
       TYPE ( domain ), INTENT(INOUT) :: grid
!ywGW       TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
       integer its, ite, jts, jte, ij
       real :: HYDRO_dt


        integer k, ix,jx, mm, nn

        integer ::  did

        integer ntime

        integer :: i,j
        

!output flux and state variable

        did = 1


        ix = ite - its + 1
        jx = jte - jts + 1

        if(HYDRO_dt .le. 0) then
             write(6,*) "WARNING: HYDRO_dt <= 0 from land input. set it to be 1 seconds."
             HYDRO_dt = 1
        endif

        ntime = 1

    
            nlst_rt(did)%dt = HYDRO_dt

  
        if(.not. RT_DOMAIN(did)%initialized) then


           !yw nlst_rt(did)%nsoil = config_flags%num_soil_layers
           !nlst_rt(did)%nsoil = model_config_rec%num_metgrid_soil_levels
           nlst_rt(did)%nsoil = grid%num_soil_layers

         
#ifdef MPP_LAND
           call mpp_land_bcast_int1 (nlst_rt(did)%nsoil)
#endif
           allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
           if(grid%zs(1) <  0) then
              nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = grid%zs(1:nlst_rt(did)%nsoil)
           else
              nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = -1*grid%zs(1:nlst_rt(did)%nsoil)
           endif

            CALL domain_clock_get( grid, current_timestr=cpl_outdate)
            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)


#ifdef MPP_LAND
            call CPL_LAND_INIT(its,ite,jts,jte)
#endif

#ifdef HYDRO_D
               write(6,*) "sf_surface_physics is ", grid%sf_surface_physics
#endif

           if(grid%sf_surface_physics .eq. 5) then    
                ! clm4 
               call HYDRO_ini(ntime,did=did,ix0=1,jx0=1)
           else
               call HYDRO_ini(ntime,did,ix0=ix,jx0=jx,vegtyp=grid%IVGTYP(its:ite,jts:jte),soltyp=grid%isltyp(its:ite,jts:jte))
           endif



            if(nlst_rt(did)%sys_cpl .ne. 2) then
               call hydro_stop("In module_wrf_HYDRO.F wrf_cpl_HYDRO() - "// &
                               "sys_cpl should be 2.  Check hydro.namelist file.")
            endif


            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

            nlst_rt(did)%dt = HYDRO_dt
            if(nlst_rt(did)%dtrt_ter .ge. HYDRO_dt) then
               nlst_rt(did)%dtrt_ter = HYDRO_dt
               mm0 = 1
            else
               mm = HYDRO_dt/nlst_rt(did)%dtrt_ter
               if(mm*nlst_rt(did)%dtrt_ter.lt. HYDRO_dt) nlst_rt(did)%dtrt_ter = HYDRO_dt/mm
               mm0 = mm
            endif

            dtrt_ter0 = nlst_rt(did)%dtrt_ter  

            if(nlst_rt(did)%dtrt_ch .ge. HYDRO_dt) then
               nlst_rt(did)%dtrt_ch = HYDRO_dt
               mm0 = 1
            else
               mm = HYDRO_dt/nlst_rt(did)%dtrt_ch
               if(mm*nlst_rt(did)%dtrt_ch.lt. HYDRO_dt) nlst_rt(did)%dtrt_ch = HYDRO_dt/mm
               mm0 = mm
            endif

            dtrt_ch0 = nlst_rt(did)%dtrt_ch  
        endif

            if((mm0*nlst_rt(did)%dtrt_ter) .ne. HYDRO_dt) then   ! WRF model time step changed.
               if(dtrt_ter0 .ge. HYDRO_dt) then
                  nlst_rt(did)%dtrt_ter = HYDRO_dt
                  mm0 = 1
               else
                  mm = HYDRO_dt/dtrt_ter0
                  if(mm*dtrt_ter0 .lt. HYDRO_dt) nlst_rt(did)%dtrt_ter = HYDRO_dt/mm
                  mm0 = mm
               endif
            endif

            if((mm0*nlst_rt(did)%dtrt_ch) .ne. HYDRO_dt) then   ! WRF model time step changed.
               if(dtrt_ch0 .ge. HYDRO_dt) then
                  nlst_rt(did)%dtrt_ch = HYDRO_dt
                  mm0 = 1
               else
                  mm = HYDRO_dt/dtrt_ch0
                  if(mm*dtrt_ch0 .lt. HYDRO_dt) nlst_rt(did)%dtrt_ch = HYDRO_dt/mm
                  mm0 = mm
               endif
            endif

#ifdef HYDRO_D 
        write(6,*) "mm, nlst_rt(did)%dt = ",mm, nlst_rt(did)%dt
#endif

        if(nlst_rt(did)%rtFlag .eq. 0) return


        nn = nlst_rt(did)%nsoil

        ! get the data from WRF 



       if((.not. RT_DOMAIN(did)%initialized) .and. (nlst_rt(did)%rst_typ .eq. 1) ) then
#ifdef HYDRO_D
           write(6,*) "restart initial data from offline file"
#endif
       else
            do k = 1, nlst_rt(did)%nsoil
                RT_DOMAIN(did)%STC(:,:,k) = grid%TSLB(its:ite,k,jts:jte) 
                RT_DOMAIN(did)%smc(:,:,k) = grid%smois(its:ite,k,jts:jte) 
                RT_DOMAIN(did)%sh2ox(:,:,k) = grid%sh2o(its:ite,k,jts:jte) 
            end do 
            rt_domain(did)%infxsrt = grid%infxsrt(its:ite,jts:jte)
            rt_domain(did)%soldrain = grid%soldrain(its:ite,jts:jte)
       endif  

            call HYDRO_exe(did)


! add for update the WRF state variable.
            do k = 1, nlst_rt(did)%nsoil
                ! grid%TSLB(its:ite,k,jts:jte) = RT_DOMAIN(did)%STC(:,:,k)
                grid%smois(its:ite,k,jts:jte) = RT_DOMAIN(did)%smc(:,:,k)
                grid%sh2o(its:ite,k,jts:jte) = RT_DOMAIN(did)%sh2ox(:,:,k)
            end do 

! update WRF variable after running routing model.
            grid%sfcheadrt(its:ite,jts:jte) = rt_domain(did)%sfcheadrt

! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)            
            if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
!Wei Yu: comment the following two lines. Not ready for WRF3.7 release
!yw             grid%qsgw(its:ite,jts:jte) = gw2d(did)%qsgw
!yw             config_flags%gwsoilcpl = nlst_rt(did)%gwsoilcpl
            end if

!yw not sure for the following
!           grid%xice(its:ite,jts:jte) = rt_domain(did)%sice

            RT_DOMAIN(did)%initialized = .true.
     end subroutine wrf_cpl_HYDRO





!program drive rtland
! This subroutine will be used if the 4-layer Noah lsm is not used.
      subroutine wrf2lsm (z1,v1,kk1,z,vout,ix,jx,kk,vegtyp)
!  input: z1,v1,kk1,z,ix,jx,kk
!  output: vout
!  interpolate based on soil layer: z1 and z 
!  z :  soil layer of output variable.
!  z1: array of soil layers of input variable.
         implicit none
         integer:: i,j,k
         integer:: kk1, ix,jx,kk, vegtyp(ix,jx)
         real :: z1(kk1), z(kk), v1(ix,kk1,jx),vout(ix,jx,kk)

       
         do j = 1, jx
            do i = 1, ix
                do k = 1, kk
                  call interpLayer(Z1,v1(i,1:kk1,j),kk1,Z(k),vout(i,j,k)) 
                end do
            end do
         end do
      end subroutine wrf2lsm

! This subroutine will be used if the 4-layer Noah lsm is not used.
      subroutine lsm2wrf (z1,v1,kk1,z,vout,ix,jx,kk,vegtyp)
!  input: z1,v1,kk1,z,ix,jx,kk
!  output: vout
!  interpolate based on soil layer: z1 and z 
!  z :  soil layer of output variable.
!  z1: array of soil layers of input variable.
         implicit none
         integer:: i,j,k
         integer:: kk1, ix,jx,kk, vegtyp(ix,jx)
         real :: z1(kk1), z(kk), v1(ix,jx,kk1),vout(ix,kk,jx)

       
         do j = 1, jx
            do i = 1, ix
                 do k = 1, kk
                    call interpLayer(Z1,v1(i,j,1:kk1),kk1,Z(k),vout(i,k,j)) 
                 end do
            end do
         end do
      end subroutine lsm2wrf

      subroutine interpLayer(inZ,inV,inK,outZ,outV)
         implicit none
         integer:: k, k1, k2
         integer :: inK
         real:: inV(inK),inZ(inK)
         real:: outV, outZ, w1, w2

         if(outZ .le. inZ(1)) then
             w1 = (inZ(2)-outZ)/(inZ(2)-inZ(1))
             w2 = (inZ(1)-outZ)/(inZ(2)-inZ(1))
             outV = inV(1)*w1-inV(2)*w2
             return
         elseif(outZ .ge. inZ(inK)) then
             w1 = (outZ-inZ(inK-1))/(inZ(inK)-inZ(inK-1)) 
             w2 = (outZ-inZ(inK))  /(inZ(inK)-inZ(inK-1))
             outV = inV(inK)*w1 -inV(inK-1)* w2
             return
         else  
            do k = 2, inK
             if((inZ(k) .ge. outZ).and.(inZ(k-1) .le. outZ) ) then
                k1  = k-1
                k2 = k
                w1 = (outZ-inZ(k1))/(inZ(k2)-inZ(k1))
                w2 = (inZ(k2)-outZ)/(inZ(k2)-inZ(k1))
                outV = inV(k2)*w1 + inV(k1)*w2
                return 
             end if 
            end do
         endif
      end subroutine interpLayer

      subroutine lsm_wrf_input(did,vegtyp,soltyp,ix,jx)
         implicit none
         integer did, leng
         parameter(leng=100)
         integer :: i,j, nn, ix,jx
         integer, dimension(ix,jx) :: soltyp, vegtyp
         real, dimension(leng) :: xdum1, MAXSMC,refsmc,wltsmc


         where(soltyp == 14) VEGTYP = 16
         where(VEGTYP == 16 ) soltyp = 14

         RT_DOMAIN(did)%VEGTYP = vegtyp

!      input OV_ROUGH from OVROUGH.TBL
#ifdef MPP_LAND
       if(my_id .eq. IO_id) then
#endif

#ifndef NCEP_WCOSS
       open(71,file="HYDRO.TBL", form="formatted")
!read OV_ROUGH first
          read(71,*) nn
          read(71,*)
          do i = 1, nn
             read(71,*) RT_DOMAIN(did)%OV_ROUGH(i)
          end do
!read parameter for LKSAT
          read(71,*) nn
          read(71,*)
          do i = 1, nn
             read(71,*) xdum1(i), MAXSMC(i),refsmc(i),wltsmc(i)
          end do
       close(71)

#else
      open(13, form="formatted") 
!read OV_ROUGH first
          read(13,*) nn
          read(13,*)
          do i = 1, nn
             read(13,*) RT_DOMAIN(did)%OV_ROUGH(i)
          end do
!read parameter for LKSAT
          read(13,*) nn
          read(13,*)
          do i = 1, nn
             read(13,*) xdum1(i), MAXSMC(i),refsmc(i),wltsmc(i)
          end do
       close(13)
#endif
#ifdef MPP_LAND
       endif
       call mpp_land_bcast_real(leng,RT_DOMAIN(did)%OV_ROUGH)
       call mpp_land_bcast_real(leng,xdum1)
       call mpp_land_bcast_real(leng,MAXSMC)
       call mpp_land_bcast_real(leng,refsmc)
       call mpp_land_bcast_real(leng,wltsmc)
#endif

       rt_domain(did)%lksat = 0.0
       do j = 1, RT_DOMAIN(did)%jx
             do i = 1, RT_DOMAIN(did)%ix
                rt_domain(did)%lksat(i,j) = xdum1(soltyp(i,j) ) * 1000.0
                IF(rt_domain(did)%VEGTYP(i,j) == 1 ) THEN   ! urban
                    rt_domain(did)%SMCMAX1(i,j) = 0.45
                    rt_domain(did)%SMCREF1(i,j) = 0.42
                    rt_domain(did)%SMCWLT1(i,j) = 0.40
                else
                    rt_domain(did)%SMCMAX1(i,j) = MAXSMC(soltyp(I,J))
                    rt_domain(did)%SMCREF1(i,j) = refsmc(soltyp(I,J))
                    rt_domain(did)%SMCWLT1(i,j) = wltsmc(soltyp(I,J))
                ENDIF
             end do
       end do


      end subroutine lsm_wrf_input

      subroutine  checkSoil(did) 
          implicit none
          integer :: did
          where(rt_domain(did)%smc(:,:,1) <=0) RT_DOMAIN(did)%VEGTYP = 16
          where(rt_domain(did)%sh2ox(:,:,1) <=0) RT_DOMAIN(did)%VEGTYP = 16
          where(rt_domain(did)%smc(:,:,1) >=100) RT_DOMAIN(did)%VEGTYP = 16
          where(rt_domain(did)%sh2ox(:,:,1) >=100) RT_DOMAIN(did)%VEGTYP = 16
      end subroutine checkSoil

end module module_wrf_HYDRO
