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
#endif
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe

    use module_rt_data, only:  rt_domain
    use module_CPL_LAND, only: cpl_outdate
    use module_namelist, only: nlst_rt
    USE module_domain, ONLY : domain, domain_clock_get

    implicit none
     
    !yw   added for check soil moisture and soiltype
    integer ::  checkSOIL_flag

!
! added to consider the adaptive time step from WRF model. 
    real    :: dtrt0  
    integer ::  mm0, itime




CONTAINS

!wrf_cpl_HYDRO_finescale will not call the off-line lsm 
    subroutine wrf_cpl_HYDRO_finescale(HYDRO_dt,grid,its,ite,jts,jte)
       use module_NoahMP_hrldas_driver, only: noah_timestep , land_driver_ini
       implicit none
       TYPE ( domain ), INTENT(INOUT) :: grid
       integer its, ite, jts, jte, ij
       real :: HYDRO_dt


        integer k, ix,jx, mm

        integer ::  did

        integer ntime

        integer :: i,j
        

!output flux and state variable

        did = 1
        ix = ite - its + 1
        jx = jte - jts + 1

        if(HYDRO_dt .le. 0) then
             write(6,*) "Warning: HYDRO_dt <= 0 from land input. set it to be 1 seconds."
             HYDRO_dt = 1
        endif

        ntime = 1

    
            nlst_rt(did)%dt = HYDRO_dt

        itime = itime + 1 
        if(.not. RT_DOMAIN(did)%initialized) then
           itime = 1

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

!yw continue

            call land_driver_ini(nn,its,ite,jts,jte)

#ifdef HYDRO_D
               write(6,*) "sf_surface_physics is ", grid%sf_surface_physics
#endif
            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

            nlst_rt(did)%dt = HYDRO_dt
            noah_timestep = nlst_rt(did)%dt

            if(nlst_rt(did)%dtrt .lt. HYDRO_dt) then
               nlst_rt(did)%dtrt = HYDRO_dt
               mm0 = 1
            else
               mm = HYDRO_dt/nlst_rt(did)%dtrt
               if(mm*nlst_rt(did)%dtrt .lt. HYDRO_dt) nlst_rt(did)%dtrt = HYDRO_dt/mm
               mm0 = mm
            endif

            dtrt0 = nlst_rt(did)%dtrt  
        endif

            if((mm0*nlst_rt(did)%dtrt) .ne. HYDRO_dt) then   ! WRF model time step changed.
               if(dtrt0 .lt. HYDRO_dt) then
                  nlst_rt(did)%dtrt = HYDRO_dt
                  mm0 = 1
               else
                  mm = HYDRO_dt/dtrt0
                  if(mm*dtrt0 .lt. HYDRO_dt) nlst_rt(did)%dtrt = HYDRO_dt/mm
                  mm0 = mm
               endif
            endif

#ifdef HYDRO_D 
        write(6,*) "mm, nlst_rt(did)%dt = ",mm, nlst_rt(did)%dt
#endif

! get forcing data from WRF
         call wrf2l_finemesh(grid,its,ite,jts,jte)

         call HYDRO_land_finemesh_exe(itime)

         call l_finemesh2wrf(grid)

         RT_DOMAIN(did)%initialized = .true.

     end subroutine wrf_cpl_HYDRO_finescale

! get the forcing data from WRF
subroutine wrf2l_finemesh(,its,ite,jts,jte, T_PHY0,U_PHY0,V_PHY0,p_hyd_w0,RAINBL0,QV_CURR0,LAI0,VEGFRA0, &
          emiss0, albedo0   )
       use module_NoahMP_hrldas_driver, only: P8W, T_PHY, U_PHY, V_PHY, QV_CURR, RAINBL_tmp, LAI, VEGFRA, finemesh,finemesh_factor, &
              emiss,albedo
       
       implicit none
       real, domain(:,:),INTENT(IN) :: T_PHY0,U_PHY0,V_PHY0,p_hyd_w0,RAINBL0,QV_CURR0,LAI0,VEGFRA0, &
             emiss0, albedo0, TSK0,HFX0, QFX0,LH0,GRDFLX0,SMSTAV0,SMSTOT0,SFCRUNOFF0, UDRUNOFF0, SNOWC0, SMOIS0, SH2O0, &
             TSLB0, SNOW0,SNOWH0,CANWAT0,ACSNOM0,ACSNOW0,QSFC0,ISNOWXY0,TVXY0,TGXY0,CANICEXY0,CANLIQXY0,EAHXY0,TAHXY0,CMXY0, &
             CHXY0,FWETXY0,SNEQVOXY0,ALBOLDXY0,QSNOWXY0,WSLAKEXY0,ZWTXY0,WAXY0,WTXY0,TSNOXY0,ZSNSOXY0,SNICEXY0,SNLIQXY0, &
             LFMASSXY0,RTMASSXY0,STMASSXY0,WOODXY0,STBLCPXY0,FASTCPXY0,XLAIXY0,XSAIXY0,TAUSSXY0,SMOISEQ0,SMCWTDXY0,DEEPRECHXY0, &
             RECHXY0, &

       integer, intent(in):: its,ite,jts,jte
       call wrf2finegrid(T_PHY0(its:ite,jts:jte), T_PHY(:,1,:),ite-its+1,jte-jts+1,finemesh_factor)
       call wrf2finegrid(U_PHY0(its:ite,jts:jte), U_PHY(:,1,:),ite-its+1,jte-jts+1,finemesh_factor)
       call wrf2finegrid(V_PHY0(its:ite,jts:jte), V_PHY(:,1,:),ite-its+1,jte-jts+1,finemesh_factor)
       call wrf2finegrid(p_hyd_w0(its:ite,jts:jte), P8W(:,1,:),ite-its+1,jte-jts+1,finemesh_factor)
       call wrf2finegrid(RAINBL0(its:ite,jts:jte), RAINBL_tmp,ite-its+1,jte-jts+1,finemesh_factor)
       call wrf2finegrid(QV_CURR0(its:ite,jts:jte), QV_CURR(:,1,:),ite-its+1,jte-jts+1,finemesh_factor)
!  update some varialbes.
       if(finemesh .ne. 1) then   ! update the LAI and VEGFRA for each time step. Note: this is from the WRF grid.
           call wrf2finegrid(albedo0(its:ite,jts:jte), albedo)
           call wrf2finegrid(emiss0(its:ite,jts:jte), emiss)
           call wrf2finegrid(LAI0(its:ite,jts:jte), LAI)
           call wrf2finegrid(VEGFRA0(its:ite,jts:jte), VEGFRA)
       endif
end subroutine wrf2l_finemesh

subroutine l_finemesh2wrf(T_PHY0,U_PHY0,V_PHY0,p_hyd_w0,RAINBL0,QV_CURR0,LAI0,VEGFRA0,its,ite,jts,jte)
   use module_NoahMP_hrldas_driver, only: P8W, T_PHY, U_PHY, V_PHY, QV_CURR, RAINBL_tmp, LAI, VEGFRA, finemesh,finemesh_factor
   implicit none
!variable for output only
   real,dimension(:,:), intent(out)::   T2MVXY0,T2MBXY0,Q2MVXY0,Q2MBXY0,TRADXY0,NEEXY0,GPPXY0,NPPXY0,FVEGXY0,RUNSFXY0,  &
             RUNSBXY0,ECANXY0,EDIRXY0,ETRANXY0,FSAXY0,&
             FIRAXY0,APARXY0,PSNXY0,SAVXY0,SAGXY0,RSSUNXY0,RSSHAXY0,BGAPXY0,WGAPXY0,TGVXY0,TGBXY0,CHVXY0,CHBXY0,SHGXY0,SHCXY0,SHBXY0, &
             EVGXY0,EVBXY0,GHVXY0,GHBXY0,IRGXY0,IRCXY0,IRBXY0,TRXY0,EVCXY0,CHLEAFXY0,CHUCXY0,CHV2XY0,CHB2XY0

         call finegrid2wrf(T2MVXY,T2MVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(T2MBXY,tt0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(FVEGXY,FVEGXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(Q2MVXY,Q2MVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(Q2MBXY,Q2MBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
      if(finemesh .ne. 1) then
         call finegrid2wrf(TRADXY,TRADXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(NEEXY,NEEXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(GPPXY,GPPXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(NPPXY,NPPXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(RUNSFXY,RUNSFXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(RUNSBXY,RUNSBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(ECANXY,ECANXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(EDIRXY,EDIRXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(ETRANXY,ETRANXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(FSAXY,FSAXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(FIRAXY,FIRAXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(APARXY,APARXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(PSNXY,PSNXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(SAVXY,SAVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(SAGXY,SAGXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(RSSUNXY,RSSUNXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(RSSHAXY,RSSHAXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(BGAPXY,BGAPXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(WGAPXY,WGAPXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(TGVXY,TGVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)

         call finegrid2wrf(TGBXY,TGBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHVXY,CHVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHBXY,CHBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(SHGXY,SHGXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(SHCXY,SHCXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(SHBXY,SHBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(EVGXY,EVGXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(EVBXY,EVBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(GHVXY,GHVXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(GHBXY,GHBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(IRGXY,IRGXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(IRCXY,IRCXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(IRBXY,IRBXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(TRXY,TRXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(EVCXY,EVCXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHLEAFXY,CHLEAFXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHUCXY,CHUCXY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHV2XY,CHV2XY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
         call finegrid2wrf(CHB2XY,CHB2XY0(its:ite,jts:jte),ite-its+1,jte-jts+1,finemesh_factor)
      endif
end subroutine l_finemesh2wrf

subroutine wrf2finegrid(wrfGrid,fineGrid,ix,jx,AGGFACTRT)
   implicit none
   real, dimension(:,:), intent(in)::wrfGrid
   real, dimension(:,:), intent(out)::fineGrid
   integer:: i,j,ii,jj,ix,jx, AGGFACTRT
   do j = 1, jx
      do i = 1, ix 
              do ii       =AGGFACTRT-1,0,-1
              do jj       =AGGFACTRT-1,0,-1
                  IXXRT=I*AGGFACTRT-ii       
                  JYYRT=J*AGGFACTRT-jj
                  fineGrid(ixxrt,jyyrt) = wrfGrid(i,j)
              enddo
              enddo
      enddo ! end do loop for ix
   enddo ! end do loop for jx
end subroutine wrf2finegrid

subroutine finegrid2wrf(fineGrid,wrfGrid,ix,jx,AGGFACTRT)
   implicit none
   real, dimension(:,:), intent(out)::wrfGrid
   real, dimension(:,:), intent(in)::fineGrid
   integer:: i,j,ii,jj,ix,jx, AGGFACTRT
   do j = 1, jx
      do i = 1, ix 
              wrfGrid(k,j) = 0.0
              do ii       =AGGFACTRT-1,0,-1
              do jj       =AGGFACTRT-1,0,-1
                  IXXRT=I*AGGFACTRT-ii       
                  JYYRT=J*AGGFACTRT-jj
                  wrfGrid(i,j) = wrfGrid(i,j) + fineGrid(ixxrt,jyyrt)
              enddo
              enddo
              wrfGrid(i,j) = wrfGrid(i,j) / (AGGFACTRT*AGGFACTRT)
      enddo ! end do loop for ix
   enddo ! end do loop for jx
end subroutine finegrid2wrf



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

end module module_wrf_HYDRO
