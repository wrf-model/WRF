!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
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

module module_HYDRO_drv
#ifdef MPP_LAND
   use module_HYDRO_io, only:  output_rt, mpp_output_chrt, mpp_output_lakes, mpp_output_chrtgrd, &
                               restart_out_bi, restart_in_bi, mpp_output_chrt2, mpp_output_lakes2, &
                               hdtbl_in_nc, hdtbl_out
   USE module_mpp_land
#else
   use module_HYDRO_io, only:  output_rt, output_chrt, output_chrt2, output_lakes
#endif
   use module_NWM_io, only: output_chrt_NWM, output_rt_NWM, output_lakes_NWM,&
                            output_chrtout_grd_NWM, output_lsmOut_NWM, &
                            output_frxstPts, output_chanObs_NWM, output_gw_NWM
   use module_HYDRO_io, only: sub_output_gw, restart_out_nc, restart_in_nc,  &
        get_file_dimension , get_file_globalatts, get2d_lsm_real, get2d_lsm_vegtyp, get2d_lsm_soltyp, &
        output_lsm,  output_GW_Diag
   use module_HYDRO_io, only : output_lakes2
   use module_rt_data, only: rt_domain
   use module_GW_baseflow
   use module_gw_gw2d
   use module_gw_gw2d_data, only: gw2d
   use module_channel_routing, only: drive_channel, drive_channel_rsl
   use orchestrator_base
   use config_base, only: nlst, noah_lsm
   use module_routing, only: getChanDim, landrt_ini
   use module_HYDRO_utils
   use module_lsm_forcing, only: geth_newdate
#ifdef WRF_HYDRO_NUDGING
   use module_stream_nudging,  only: init_stream_nudging
#endif
   use module_hydro_stop, only: HYDRO_stop
   use module_UDMAP, only: get_basn_area_nhd
   use netcdf

   implicit none


#ifdef HYDRO_D
  real :: timeOr = 0
  real :: timeSr = 0
  real :: timeCr = 0
  real :: timeGW = 0
  integer :: clock_count_1 = 0
  integer :: clock_count_2 = 0
  integer :: clock_rate    = 0
#endif
  integer :: rtout_factor  = 0

  integer, parameter :: r4 = selected_real_kind(4)
  real,    parameter :: zeroFlt=0.0000000000000000000_r4
  integer, parameter :: r8 = selected_real_kind(8)
  real*8,  parameter :: zeroDbl=0.0000000000000000000_r8

   contains
   subroutine HYDRO_rst_out(did)
#ifdef WRF_HYDRO_NUDGING
      use module_stream_nudging, only: output_nudging_last_obs
#endif
      implicit none
      integer:: rst_out
      integer did, outflag
      character(len=19) out_date
#ifdef MPP_LAND
      character(len=19) str_tmp
#endif
      rst_out = -99
#ifdef MPP_LAND
   if(IO_id .eq. my_id) then
#endif
     if(nlst(did)%dt .gt. nlst(did)%rst_dt*60) then
        call geth_newdate(out_date, nlst(did)%startdate, nint(nlst(did)%dt*rt_domain(did)%rst_counts))
     else
        call geth_newdate(out_date, nlst(did)%startdate, nint(nlst(did)%rst_dt*60*rt_domain(did)%rst_counts))
     endif
     if ( (nlst(did)%rst_dt .gt. 0) .and. (out_date(1:19) == nlst(did)%olddate(1:19)) ) then
           rst_out = 99
           rt_domain(did)%rst_counts = rt_domain(did)%rst_counts + 1
     endif
! restart every month automatically.
     if ( (nlst(did)%olddate(9:10) == "01") .and. (nlst(did)%olddate(12:13) == "00") .and. &
          (nlst(did)%olddate(15:16) == "00").and. (nlst(did)%olddate(18:19) == "00") .and. &
          (nlst(did)%rst_dt .le. 0)  ) then
           if(nlst(did)%startdate(1:16) .ne. nlst(did)%olddate(1:16) ) then
               rst_out = 99
           endif
     endif

#ifdef MPP_LAND
   endif
     call mpp_land_bcast_int1(rst_out)
#endif
    if(rst_out .gt. 0) then
      write(6,*) "yw check output restart at ",nlst(did)%olddate(1:16)
#ifdef MPP_LAND
      if(nlst(did)%rst_bi_out .eq. 1) then
             if(my_id .lt. 10) then
                  write(str_tmp,'(I1)') my_id
             else if(my_id .lt. 100) then
                  write(str_tmp,'(I2)') my_id
             else if(my_id .lt. 1000) then
                  write(str_tmp,'(I3)') my_id
             else if(my_id .lt. 10000) then
                  write(str_tmp,'(I4)') my_id
             else if(my_id .lt. 100000) then
                  write(str_tmp,'(I5)') my_id
             else
                continue
             endif
             call mpp_land_bcast_char(16,nlst(did)%olddate(1:16))
             call   RESTART_OUT_bi(trim("HYDRO_RST."//nlst(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst(did)%hgrid)//"."//trim(str_tmp)),  did)
      else
#endif
             call   RESTART_OUT_nc(trim("HYDRO_RST."//nlst(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst(did)%hgrid)),  did)
#ifdef MPP_LAND
      endif
#endif

#ifdef WRF_HYDRO_NUDGING
      call output_nudging_last_obs
#endif
   endif


   end subroutine HYDRO_rst_out

    subroutine HYDRO_out(did, rstflag)

        implicit none
        integer did, outflag, rtflag, iniflag
        integer rstflag
        character(len=19) out_date
        integer :: Kt, ounit, i
        real, dimension(RT_DOMAIN(did)%NLINKS,2) :: str_out
        real, dimension(RT_DOMAIN(did)%NLINKS) :: vel_out

        !    real, dimension(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx):: soilmx_tmp, &
            !           runoff1x_tmp, runoff2x_tmp, runoff3x_tmp,etax_tmp, &
            !           EDIRX_tmp,ECX_tmp,ETTX_tmp,RCX_tmp,HX_tmp,acrain_tmp, &
            !           ACSNOM_tmp, esnow2d_tmp, drip2d_tmp,dewfall_tmp, fpar_tmp, &
            !           qfx_tmp, prcp_out_tmp, etpndx_tmp

        outflag = -99

#ifdef MPP_LAND
        if(IO_id .eq. my_id) then
#endif
            if(nlst(did)%olddate(1:19) .eq. nlst(did)%startdate(1:19) .and. rt_domain(did)%his_out_counts .eq. 0) then
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
                write(6,*) "output hydrology at time : ",nlst(did)%olddate(1:19), rt_domain(did)%his_out_counts
#else
                write(78,*) "output hydrology at time : ",nlst(did)%olddate(1:19), rt_domain(did)%his_out_counts
#endif
#endif
                outflag = 99
            else
                if(nlst(did)%dt .gt. nlst(did)%out_dt*60) then
                    call geth_newdate(out_date, nlst(did)%startdate, nint(nlst(did)%dt*rt_domain(did)%out_counts))
                else
                    call geth_newdate(out_date, nlst(did)%startdate, nint(nlst(did)%out_dt*60*rt_domain(did)%out_counts))
                endif
                if ( out_date(1:19) == nlst(did)%olddate(1:19) ) then
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
                    write(6,*) "output hydrology at time : ",nlst(did)%olddate(1:19)
#else
                    write(78,*) "output hydrology at time : ",nlst(did)%olddate(1:19)
#endif
#endif
                    outflag = 99
                endif
            endif
#ifdef MPP_LAND
        endif
        call mpp_land_bcast_int1(outflag)
#endif

        if (rstflag .eq. 1) call HYDRO_rst_out(did)

        if (outflag .lt. 0) return

        rt_domain(did)%out_counts = rt_domain(did)%out_counts + 1
        rt_domain(did)%his_out_counts = rt_domain(did)%his_out_counts + 1

        if(nlst(did)%out_dt*60 .gt. nlst(did)%DT) then
            kt = rt_domain(did)%his_out_counts*nlst(did)%out_dt*60/nlst(did)%DT
        else
            kt = rt_domain(did)%his_out_counts
        endif

        ! jump the ouput for the initial time when it has restart file from routing.
        rtflag = -99
        iniflag = -99
#ifdef MPP_LAND
        if(IO_id .eq. my_id) then
#endif
            !       if ( (trim(nlst_rt(did)%restart_file) /= "") .and. ( nlst_rt(did)%startdate(1:19) == nlst_rt(did)%olddate(1:19) ) ) then
            !#ifndef NCEP_WCOSS
            !             print*, "yyyywww restart_file = ", trim(nlst_rt(did)%restart_file)
            !#else
            !             write(78,*) "yyyywww restart_file = ", trim(nlst_rt(did)%restart_file)
            !#endif
            if ( nlst(did)%startdate(1:19) == nlst(did)%olddate(1:19) ) iniflag = 1
            if ( (trim(nlst(did)%restart_file) /= "") .and. ( nlst(did)%startdate(1:19) == nlst(did)%olddate(1:19) ) ) rtflag = 1
            !       endif
#ifdef MPP_LAND
        endif
        call mpp_land_bcast_int1(rtflag)
        call mpp_land_bcast_int1(iniflag)
#endif


        !yw keep the initial time otuput for debug
        if(rtflag == 1) then
            rt_domain(did)%restQSTRM = .false.   !!! do not reset QSTRM.. at initial time.
            if(nlst(did)%t0OutputFlag .eq. 0) return
        endif

        if (iniflag == 1) then
            if(nlst(did)%t0OutputFlag .eq. 0) return
        endif

      if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then
        if(nlst(did)%LSMOUT_DOMAIN .eq. 1)  then
            if(nlst(did)%io_form_outputs .eq. 0) then
                call output_lsm(trim(nlst(did)%olddate(1:4)//nlst(did)%olddate(6:7)//nlst(did)%olddate(9:10)  &
                    //nlst(did)%olddate(12:13)//nlst(did)%olddate(15:16)//  &
                    ".LSMOUT_DOMAIN"//trim(nlst(did)%hgrid)),     &
                    did)
            else
                call output_lsmOut_NWM(did)
            endif
        endif
      end if

        if(nlst(did)%SUBRTSWCRT         .gt. 0 .or. &
                nlst(did)%OVRTSWCRT          .gt. 0 .or. &
                nlst(did)%GWBASESWCRT        .gt. 0 .or. &
   nlst(did)%CHANRTSWCRT        .gt. 0 .or. &
                nlst(did)%channel_only       .gt. 0 .or. &
                nlst(did)%channelBucket_only .gt. 0      ) then


            if(nlst(did)%RTOUT_DOMAIN       .eq. 1 .and. &
                    nlst(did)%channel_only       .eq. 0 .and. &
                    nlst(did)%channelBucket_only .eq. 0       ) then
                if(mod(rtout_factor,3) .eq. 2 .or. &
                   nlst(did)%io_config_outputs .ne. 5 .and. &
                   nlst(did)%io_config_outputs .ne. 3) then
                    ! Output gridded routing variables on National Water Model
                    ! high-res routing grid
                    if(nlst(did)%io_form_outputs .ne. 0) then
                        call output_rt_NWM(did,nlst(did)%igrid)
                    else
                        call output_rt(    &
                            nlst(did)%igrid, nlst(did)%split_output_count, &
                            RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt, &
                            nlst(did)%nsoil, &
                            !                  nlst_rt(did)%startdate, nlst_rt(did)%olddate,
                        !                  rt_domain(did)%subsurface%state%qsubrt,&
                            nlst(did)%sincedate, nlst(did)%olddate, rt_domain(did)%subsurface%state%qsubrt,&
                            rt_domain(did)%subsurface%properties%zwattablrt,RT_DOMAIN(did)%subsurface%grid_transform%smcrt,&
                            RT_DOMAIN(did)%SUB_RESID,       &
                            RT_DOMAIN(did)%q_sfcflx_x,RT_DOMAIN(did)%q_sfcflx_y,&
                            rt_domain(did)%overland%properties%surface_slope_x,rt_domain(did)%overland%properties%surface_slope_y,&
                            RT_DOMAIN(did)%QSTRMVOLRT_ACC,rt_domain(did)%overland%control%surface_water_head_routing, &
                            nlst(did)%geo_finegrid_flnm,nlst(did)%DT,&
                            rt_domain(did)%subsurface%properties%sldpth,RT_DOMAIN(did)%LATVAL,&
                            RT_DOMAIN(did)%LONVAL,rt_domain(did)%overland%properties%distance_to_neighbor,nlst(did)%RTOUT_DOMAIN,&
                            rt_domain(did)%overland%control%boundary_flux, &
                            nlst(did)%io_config_outputs &
                            )
                    endif ! End check for io_form_outputs value
                endif ! End check for rtout_factor
                rtout_factor = rtout_factor + 1
            endif
            !! JLM disable GW output for NWM. Bring this line back when runtime output options avail.
            !! JLM This seems like a more logical place?
            if(nlst(did)%io_form_outputs .ne. 0) then
                if(nlst(did)%GWBASESWCRT .ne. 0) then
                    if(nlst(did)%channel_only .eq. 0) then
                        if(nlst(did)%output_gw .ne. 0) then
                            call output_gw_NWM(did,nlst(did)%igrid)
                        endif
                    endif
                endif
            else
                if((nlst(did)%GWBASESWCRT .eq. 1) .or. (nlst(did)%GWBASESWCRT .ge. 4)) then
                    if(nlst(did)%channel_only .eq. 0) then
                        if(nlst(did)%output_gw  .eq. 1) call output_GW_Diag(did)
                    endif
                end if
            endif


            if(nlst(did)%GWBASESWCRT .eq. 3) then

                if(nlst(did)%output_gw  .eq. 1)  &
                    call sub_output_gw(    &
                    nlst(did)%igrid, nlst(did)%split_output_count, &
                    RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt,          &
                    nlst(did)%nsoil,                               &
                    !               nlst(did)%startdate, nlst(did)%olddate,    &
                    nlst(did)%sincedate, nlst(did)%olddate,    &
                    gw2d(did)%h, rt_domain(did)%subsurface%grid_transform%smcrt,                   &
                    gw2d(did)%convgw, gw2d(did)%excess,                  &
                    gw2d(did)%qsgwrt, gw2d(did)%qgw_chanrt,              &
                    nlst(did)%geo_finegrid_flnm,nlst(did)%DT, &
                    rt_domain(did)%subsurface%properties%sldpth,RT_DOMAIN(did)%LATVAL,       &
                    RT_DOMAIN(did)%LONVAL,rt_domain(did)%overland%properties%distance_to_neighbor,           &
                    nlst(did)%output_gw)

            endif
            ! BF end gw2d output section

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "before call output_chrt"
            call flush(6)
#else
            write(78,*) "before call output_chrt"
#endif
#endif

            if (nlst(did)%CHANRTSWCRT.eq.1.or.nlst(did)%CHANRTSWCRT.eq.2) then

                !ADCHANGE: Change values for within lake reaches to NA
                str_out = RT_DOMAIN(did)%QLINK
                vel_out = RT_DOMAIN(did)%velocity

                if (RT_DOMAIN(did)%NLAKES .gt. 0)  then
                    do i=1,RT_DOMAIN(did)%NLINKS
                        if (RT_DOMAIN(did)%TYPEL(i) .eq. 2) then
                            str_out(i,1) = -9.E15
                            vel_out(i) = -9.E15
                        endif
                    end do
                endif
                !ADCHANGE: End

                if(nlst(did)%io_form_outputs .ne. 0) then
                    ! Call National Water Model output routine for output on NHD forecast points.
                    if(nlst(did)%CHRTOUT_DOMAIN .ne. 0) then
                        call output_chrt_NWM(did)
                    endif
                    ! Call the subroutine to output frxstPts.
                    if(nlst(did)%frxst_pts_out .ne. 0) then
                        call output_frxstPts(did)
                    endif
                    ! Call the subroutine to output CHANOBS
                    if(nlst(did)%CHANOBS_DOMAIN .ne. 0) then
                        call output_chanObs_NWM(did)
                    endif
                else
                    ! Call traditional output routines
                    !ADCHANGE: We suspect this routine is broken so default is now output_chrtout2
                    !             if(nlst_rt(did)%CHRTOUT_DOMAIN  .eq. 1)  then
                    !#ifdef MPP_LAND
                    !                 call mpp_output_chrt( &
                        !                      rt_domain(did)%gnlinks,rt_domain(did)%gnlinksl,rt_domain(did)%map_l2g, &
                        !#else
                    !                 call output_chrt(  &
                        !#endif
                    !                   nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                        !                   RT_DOMAIN(did)%NLINKS,RT_DOMAIN(did)%ORDER, &
                        !                   nlst_rt(did)%sincedate,nlst_rt(did)%olddate,RT_DOMAIN(did)%CHLON,&
                        !                   RT_DOMAIN(did)%CHLAT,                                      &
                        !                   RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ZELEV,                &
                        !                   !RT_DOMAIN(did)%QLINK,nlst_rt(did)%DT,Kt,                   &
                        !                   str_out, nlst_rt(did)%DT,Kt,                   &
                        !                   RT_DOMAIN(did)%STRMFRXSTPTS,nlst_rt(did)%order_to_write,   &
                        !                   RT_DOMAIN(did)%NLINKSL,nlst_rt(did)%channel_option,        &
                        !                   rt_domain(did)%gages, rt_domain(did)%gageMiss,             &
                        !                   nlst_rt(did)%dt                                            &
                        !#ifdef WRF_HYDRO_NUDGING
                    !                   , RT_DOMAIN(did)%nudge                                     &
                        !#endif
                    !                   , RT_DOMAIN(did)%accSfcLatRunoff, RT_DOMAIN(did)%accBucket &
                        !                   , RT_DOMAIN(did)%qSfcLatRunoff,   RT_DOMAIN(did)%qBucket   &
                        !                   , RT_DOMAIN(did)%qin_gwsubbas                              &
                        !                   , nlst_rt(did)%UDMP_OPT                                    &
                        !                   )
                    !              else
                    if(nlst(did)%CHRTOUT_DOMAIN  .gt. 0)  then
#ifdef MPP_LAND
                        call mpp_output_chrt2(&
                            rt_domain(did)%gnlinks,rt_domain(did)%gnlinksl,rt_domain(did)%map_l2g, &
#else
                            call output_chrt2(  &
#endif
                            nlst(did)%igrid, nlst(did)%split_output_count, &
                            RT_DOMAIN(did)%NLINKS,RT_DOMAIN(did)%ORDER,          &
                            nlst(did)%sincedate,nlst(did)%olddate,         &
                            RT_DOMAIN(did)%CHLON, RT_DOMAIN(did)%CHLAT,          &
                            RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ZELEV,          &
                            !RT_DOMAIN(did)%QLINK,nlst_rt(did)%DT,Kt,             &
                            str_out, nlst(did)%DT,Kt,                         &
                            RT_DOMAIN(did)%NLINKSL,nlst(did)%channel_option,  &
                            rt_domain(did)%linkid                                &
#ifdef WRF_HYDRO_NUDGING
                            , RT_DOMAIN(did)%nudge &
#endif
                            !, RT_DOMAIN(did)%QLateral, nlst_rt(did)%io_config_outputs,
                        !RT_DOMAIN(did)%velocity &
                            , RT_DOMAIN(did)%QLateral, nlst(did)%io_config_outputs, vel_out &
                            , RT_DOMAIN(did)%accSfcLatRunoff, RT_DOMAIN(did)%accBucket &
                            , RT_DOMAIN(did)%qSfcLatRunoff,   RT_DOMAIN(did)%qBucket &
                            , RT_DOMAIN(did)%qin_gwsubbas,    nlst(did)%UDMP_OPT &
                            )
                    endif

                endif
            endif ! End of checking for io_form_outputs flag value

#ifdef MPP_LAND
            if(nlst(did)%CHRTOUT_GRID  .eq. 1)  then
                if(nlst(did)%io_form_outputs .eq. 0) then
                    call mpp_output_chrtgrd(nlst(did)%igrid, nlst(did)%split_output_count, &
                        RT_DOMAIN(did)%ixrt,RT_DOMAIN(did)%jxrt, RT_DOMAIN(did)%NLINKS,   &
                        RT_DOMAIN(did)%GCH_NETLNK, &
                        nlst(did)%startdate, nlst(did)%olddate, &
                        !RT_DOMAIN(did)%qlink, nlst_rt(did)%dt, nlst_rt(did)%geo_finegrid_flnm,   &
                        str_out, nlst(did)%dt, nlst(did)%geo_finegrid_flnm,   &
                        RT_DOMAIN(did)%gnlinks,RT_DOMAIN(did)%map_l2g,                   &
                        RT_DOMAIN(did)%g_ixrt,RT_DOMAIN(did)%g_jxrt )
#endif
                else
                    call output_chrtout_grd_NWM(did,nlst(did)%igrid)
                endif
            endif
            if (RT_DOMAIN(did)%NLAKES.gt.0)  then
                if(nlst(did)%io_form_outputs .ne. 0) then
                    ! Output lakes in NWM format
                    if(nlst(did)%outlake .ne. 0) then
                        call output_lakes_NWM(did,nlst(did)%igrid)
                    endif
                else
                    if(nlst(did)%outlake .eq. 1) then
#ifdef MPP_LAND
                        call mpp_output_lakes( RT_DOMAIN(did)%lake_index, &
#else
                            call output_lakes(  &
#endif
                            nlst(did)%igrid, nlst(did)%split_output_count, &
                            RT_DOMAIN(did)%NLAKES, &
                            trim(nlst(did)%sincedate), trim(nlst(did)%olddate), &
                            RT_DOMAIN(did)%LATLAKE,RT_DOMAIN(did)%LONLAKE, &
                            RT_DOMAIN(did)%ELEVLAKE,RT_DOMAIN(did)%QLAKEI, &
                            RT_DOMAIN(did)%QLAKEO, &
                            RT_DOMAIN(did)%RESHT,nlst(did)%DT,Kt)
                    endif
                    if(nlst(did)%outlake .eq. 2) then
#ifdef MPP_LAND
                        call mpp_output_lakes2( RT_DOMAIN(did)%lake_index, &
#else
                            call output_lakes2(  &
#endif
                            nlst(did)%igrid, nlst(did)%split_output_count, &
                            RT_DOMAIN(did)%NLAKES, &
                            trim(nlst(did)%sincedate), trim(nlst(did)%olddate), &
                            RT_DOMAIN(did)%LATLAKE,RT_DOMAIN(did)%LONLAKE, &
                            RT_DOMAIN(did)%ELEVLAKE,RT_DOMAIN(did)%QLAKEI, &
                            RT_DOMAIN(did)%QLAKEO, &
                            RT_DOMAIN(did)%RESHT,nlst(did)%DT,Kt,RT_DOMAIN(did)%LAKEIDM)
                    endif

                endif ! end of check for io_form_outputs value
            endif   ! end if block of rNLAKES .gt. 0
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "end calling output functions"
#else
            write(78,*) "end calling output functions"
#endif
#endif

        endif  ! end of routing switch


    end subroutine HYDRO_out


      subroutine HYDRO_rst_in(did)
        integer :: did
        integer:: flag



   flag = -1
#ifdef MPP_LAND
   if(my_id.eq.IO_id) then
#endif
      if (trim(nlst(did)%restart_file) /= "") then
          flag = 99
          rt_domain(did)%timestep_flag = 99   ! continue run
      endif
#ifdef MPP_LAND
   endif
   call mpp_land_bcast_int1(flag)
#endif

   nlst(did)%sincedate = nlst(did)%startdate

   if (flag.eq.99) then

#ifdef MPP_LAND
     if(my_id.eq.IO_id) then
#endif
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
        write(6,*) "*** read restart data: ",trim(nlst(did)%restart_file)
#else
        write(78,*) "*** read restart data: ",trim(nlst(did)%restart_file)
#endif
#endif
#ifdef MPP_LAND
     endif
#endif

#ifdef MPP_LAND
      if(nlst(did)%rst_bi_in .eq. 1) then
         call RESTART_IN_bi(trim(nlst(did)%restart_file), did)
      else
#endif
         call RESTART_IN_nc(trim(nlst(did)%restart_file), did)
#ifdef MPP_LAND
      endif
#endif

!yw  if (trim(nlst_rt(did)%restart_file) /= "") then
!yw          nlst_rt(did)%restart_file = ""
!yw  endif

  endif
 end subroutine HYDRO_rst_in

     subroutine HYDRO_time_adv(did)
        implicit none
        character(len = 19) :: newdate
        integer did

#ifdef MPP_LAND
   if(IO_id.eq.my_id) then
#endif
         call geth_newdate(newdate, nlst(did)%olddate, nint( nlst(did)%dt))
         nlst(did)%olddate = newdate
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
         write(6,*) "current time is ",newdate
#else
         write(78,*) "current time is ",newdate
#endif
#endif
#ifdef MPP_LAND
   endif
#endif
     end subroutine HYDRO_time_adv

     subroutine HYDRO_exe(did)


        implicit none
        integer:: did
        integer:: rst_out

!       call HYDRO_out(did)


! running land surface model
! cpl: 0--offline run;
!      1-- coupling with WRF but running offline lsm;
!      2-- coupling with WRF but do not run offline lsm
!      3-- coupling with LIS and do not run offline lsm
!      4:  coupling with CLM
!          if(nlst_rt(did)%SYS_CPL .eq. 0 .or. nlst_rt(did)%SYS_CPL .eq. 1 )then
!                  call drive_noahLSF(did,kt)
!          else
!              ! does not run the NOAH LASF model, only read the parameter
!              call read_land_par(did,lsm(did)%ix,lsm(did)%jx)
!          endif


if (nlst(did)%GWBASESWCRT        .ne. 0 .or. &
    nlst(did)%SUBRTSWCRT         .ne. 0 .or. &
    nlst(did)%OVRTSWCRT          .ne. 0 .or. &
    nlst(did)%channel_only       .ne. 0 .or. &
    nlst(did)%channelBucket_only .ne. 0      ) then

   ! step 1) disaggregate specific fields from LSM to Hydro grid
   if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then

      RT_DOMAIN(did)%overland%streams_and_lakes%surface_water_to_channel = zeroFlt
      RT_DOMAIN(did)%LAKE_INFLORT_DUM = rt_domain(did)%overland%streams_and_lakes%surface_water_to_lake

      if(nlst(did)%SUBRTSWCRT .ne. 0 .or. nlst(did)%OVRTSWCRT .ne. 0) then
         call disaggregateDomain_drv(did)
      endif
      if(nlst(did)%OVRTSWCRT .eq. 0) then
         if(nlst(did)%UDMP_OPT .eq. 1) then
            call RunOffDisag(RT_DOMAIN(did)%INFXSRT, RT_DOMAIN(did)%landRunOff,        &
                 rt_domain(did)%dist_lsm(:,:,9),rt_domain(did)%overland%properties%distance_to_neighbor(:,:,9), &
                 RT_DOMAIN(did)%INFXSWGT, nlst(did)%AGGFACTRT, RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx)
         endif
      endif

#ifdef HYDRO_D
      call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
   endif !! channel_only & channelBucket_only == 0


   ! step 2)
   if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then
      if(nlst(did)%SUBRTSWCRT .ne.0) then
         call SubsurfaceRouting_drv(did)
      endif
#ifdef HYDRO_D
      call system_clock(count=clock_count_2, count_rate=clock_rate)
      timeSr = timeSr     + float(clock_count_2-clock_count_1)/float(clock_rate)
#ifndef NCEP_WCOSS
      write(6,*) "Timing: Subsurface Routing  accumulated time--", timeSr
#else
      write(78,*) "Timing: Subsurface Routing  accumulated time--", timeSr
#endif
#endif
   end if !! channel_only & channelBucket_only == 0


   ! step 3) todo split
   if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then
#ifdef HYDRO_D
      call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
      if(nlst(did)%OVRTSWCRT .ne. 0) then
         call OverlandRouting_drv(did)
      else
         !ADCHANGE: Updating landRunoff instead of surface_water_head_routing. This now allows
         !          landRunoff to include both infxsrt from the LSM and exfiltration (if subsfc
         !          is active) and prevents surface_water_head_routing from inadvertently being
         !          passed back to the LSM.
         if (nlst(did)%UDMP_OPT .eq. 1) then
           ! If subsurface is on, we update landRunOff to include the updated term w/ exfiltration.
           ! If subsurface is off, landRunOff does not change from original value so we leave as-is.
           if (nlst(did)%SUBRTSWCRT .ne. 0) then
             rt_domain(did)%landRunOff = rt_domain(did)%overland%control%infiltration_excess
           endif
         else
           ! If overland is off and subsurface is on, we need to update INFXSRT (LSM grid)
           ! since that is what gets fed through the buckets into the channels. So we aggregate
           ! the high-res infiltration_excess back to coarse grid.
           if (nlst(did)%SUBRTSWCRT .ne. 0) then
             call RunoffAggregate(rt_domain(did)%overland%control%infiltration_excess, &
                                  rt_domain(did)%INFXSRT, nlst(did)%AGGFACTRT, &
                                  rt_domain(did)%ix, rt_domain(did)%jx)
           endif
         endif
         ! In either case, if overland is off we need to zero-out surface_water_head since this
         ! water is being scraped into channel and should NOT be passed back to the LSM.
         rt_domain(did)%overland%control%infiltration_excess = 0.
         rt_domain(did)%overland%control%surface_water_head_routing = 0.
      endif
#ifdef HYDRO_D
      call system_clock(count=clock_count_2, count_rate=clock_rate)
      timeOr = timeOr     + float(clock_count_2-clock_count_1)/float(clock_rate)
#ifndef NCEP_WCOSS
      write(6,*) "Timing: Overland Routing  accumulated time--", timeOr
#else
      write(78,*) "Timing: Overland Routing  accumulated time--", timeOr
#endif
#endif

      RT_DOMAIN(did)%QSTRMVOLRT_TS = rt_domain(did)%overland%streams_and_lakes%surface_water_to_channel
      RT_DOMAIN(did)%QSTRMVOLRT_ACC = RT_DOMAIN(did)%QSTRMVOLRT_ACC + RT_DOMAIN(did)%QSTRMVOLRT_TS

      RT_DOMAIN(did)%LAKE_INFLORT_TS = rt_domain(did)%overland%streams_and_lakes%surface_water_to_lake-RT_DOMAIN(did)%LAKE_INFLORT_DUM

#ifdef HYDRO_D
      call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
   end if !! channel_only & channelBucket_only == 0


   ! step 4) baseflow or groundwater physics
   !! channelBucket_only can be anything: the only time you dont run this is if channel_only=1
   if(nlst(did)%channel_only .eq. 0) then
      if (nlst(did)%GWBASESWCRT .gt. 0) then
         call driveGwBaseflow(did)
      endif
#ifdef HYDRO_D
      call system_clock(count=clock_count_2, count_rate=clock_rate)
      timeGw = timeGw     + float(clock_count_2-clock_count_1)/float(clock_rate)
#ifndef NCEP_WCOSS
      write(6,*) "Timing: GwBaseflow  accumulated time--", timeGw
#else
      write(78,*) "Timing: GwBaseflow  accumulated time--", timeGw
#endif
#endif
#ifdef HYDRO_D
      call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
   end if !! channel_only == 0

   ! step 5) river channel physics
   call driveChannelRouting(did)
#ifdef HYDRO_D
   call system_clock(count=clock_count_2, count_rate=clock_rate)
   timeCr = timeCr     + float(clock_count_2-clock_count_1)/float(clock_rate)
#ifndef NCEP_WCOSS
   write(6,*) "Timing: Channel Routing  accumulated time--", timeCr
#else
   write(78,*) "Timing: Channel Routing  accumulated time--", timeCr
#endif
#endif

   !! if not channel_only
   if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then

      ! step 6) aggregate specific fields from Hydro to LSM grid
      if (nlst(did)%SUBRTSWCRT .ne.0  .or. nlst(did)%OVRTSWCRT .ne. 0 ) then
         call aggregateDomain(did)
      endif

   end if !! channel_only & channelBucket_only == 0

end if


!yw  if (nlst_rt(did)%sys_cpl .eq. 2) then
      ! advance to next time step
!          call HYDRO_time_adv(did)
      ! output for history
!          call HYDRO_out(did)
!yw  endif
           call HYDRO_time_adv(did)
           call HYDRO_out(did, 1)


!           write(90 + my_id,*) "finish calling hydro_exe"
!           call flush(90+my_id)
!          call mpp_land_sync()



           !! Under channel-only, these variables are not allocated
           if(allocated(RT_DOMAIN(did)%SOLDRAIN)) RT_DOMAIN(did)%SOLDRAIN = 0
           if(allocated(rt_domain(did)%subsurface%state%qsubrt))   RT_DOMAIN(did)%subsurface%state%qsubrt   = 0



      end subroutine HYDRO_exe



!----------------------------------------------------
subroutine driveGwBaseflow(did)

    implicit none
    integer, intent(in) :: did

    integer :: i, jj, ii

    !------------------------------------------------------------------
    !DJG Begin GW/Baseflow Routines
    !-------------------------------------------------------------------

    if (nlst(did)%GWBASESWCRT.ge.1) then     ! Switch to activate/specify GW/Baseflow

        !  IF (nlst(did)%GWBASESWCRT.GE.1000) THEN     ! Switch to activate/specify GW/Baseflow

        if (nlst(did)%GWBASESWCRT.eq.1 .or. nlst(did)%GWBASESWCRT.eq.2 .or. nlst(did)%GWBASESWCRT.ge.4) then   ! Call simple bucket baseflow scheme

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "*****yw******start simp_gw_buck "
#else
            write(78,*) "*****yw******start simp_gw_buck "
#endif
#endif

            if(nlst(did)%UDMP_OPT .eq. 1) then
                call simp_gw_buck_nhd(                                             &
                    RT_DOMAIN(did)%ix,            RT_DOMAIN(did)%jx,              &
                    RT_DOMAIN(did)%ixrt,          RT_DOMAIN(did)%jxrt,            &
                    RT_DOMAIN(did)%numbasns,      nlst(did)%AGGFACTRT,         &
                    nlst(did)%DT,              RT_DOMAIN(did)%INFXSWGT,        &
                    RT_DOMAIN(did)%INFXSRT,       RT_DOMAIN(did)%SOLDRAIN,        &
                    rt_domain(did)%overland%properties%distance_to_neighbor(:,:,9),   rt_domain(did)%dist_lsm(:,:,9), &
                    RT_DOMAIN(did)%gw_buck_coeff, RT_DOMAIN(did)%gw_buck_exp,     &
              RT_DOMAIN(did)%gw_buck_loss,                                  &
                    RT_DOMAIN(did)%z_max,         RT_DOMAIN(did)%z_gwsubbas,      &
                    RT_DOMAIN(did)%qout_gwsubbas, RT_DOMAIN(did)%qin_gwsubbas,    &
              RT_DOMAIN(did)%qloss_gwsubbas,                                &
                    nlst(did)%GWBASESWCRT,     nlst(did)%OVRTSWCRT,         &
#ifdef MPP_LAND
                RT_DOMAIN(did)%LNLINKSL,                                      &
#else
                    RT_DOMAIN(did)%numbasns,                                      &
#endif
                    rt_domain(did)%basns_area,                                    &
              rt_domain(did)%nhdBuckMask,   nlst(did)%bucket_loss,       &
              nlst(did)%channelBucket_only )

            else
                call simp_gw_buck(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,RT_DOMAIN(did)%ixrt,&
                    RT_DOMAIN(did)%jxrt,RT_DOMAIN(did)%numbasns,RT_DOMAIN(did)%gnumbasns,&
                RT_DOMAIN(did)%basns_area,&
                    RT_DOMAIN(did)%basnsInd, RT_DOMAIN(did)%gw_strm_msk_lind,             &
                    RT_DOMAIN(did)%gwsubbasmsk, RT_DOMAIN(did)%INFXSRT, &
                    RT_DOMAIN(did)%SOLDRAIN, &
                    RT_DOMAIN(did)%z_gwsubbas,&
                    RT_DOMAIN(did)%qin_gwsubbas,RT_DOMAIN(did)%qout_gwsubbas,&
                    RT_DOMAIN(did)%qinflowbase,&
                    RT_DOMAIN(did)%gw_strm_msk,RT_DOMAIN(did)%gwbas_pix_ct, &
                    rt_domain(did)%overland%properties%distance_to_neighbor,nlst(did)%DT,&
                    RT_DOMAIN(did)%gw_buck_coeff,RT_DOMAIN(did)%gw_buck_exp, &
                    RT_DOMAIN(did)%z_max,&
                    nlst(did)%GWBASESWCRT,nlst(did)%OVRTSWCRT)
            endif

            !! JLM: There's *perhaps* a better location for this output above.
            !! If above is better, remove this when runtime output options are avail.
            !#ifndef HYDRO_REALTIME
            !        call output_GW_Diag(did)
            !#endif

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "*****yw******end simp_gw_buck "
#else
            write(78,*) "*****yw******end simp_gw_buck "
#endif
#endif

            !!!For parameter setup runs output the percolation for each basin,
            !!!otherwise comment out this output...
        else if (nlst(did)%gwBaseSwCRT .eq. 3) then

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "*****bf******start 2d_gw_model "
#else
            write(78,*) "*****bf******start 2d_gw_model "
#endif
#endif

            ! 	   compute qsgwrt  between lsm and gw with namelist selected coupling method
            ! 	   qsgwrt is defined on the routing grid  and needs to be aggregated for SFLX
            if (nlst(did)%gwsoilcpl .GT. 0) THEN

                call gwSoilFlux(did)

            end if

            gw2d(did)%excess = 0.

            call gwstep(gw2d(did)%ix, gw2d(did)%jx, gw2d(did)%dx, &
                gw2d(did)%ltype, gw2d(did)%elev, gw2d(did)%bot, &
                gw2d(did)%hycond, gw2d(did)%poros, gw2d(did)%compres, &
                gw2d(did)%ho, gw2d(did)%h, gw2d(did)%convgw, &
                gw2d(did)%excess, &
                gw2d(did)%ebot, gw2d(did)%eocn, gw2d(did)%dt, &
                gw2d(did)%istep)

            gw2d(did)%ho = gw2d(did)%h

            ! put surface exceeding groundwater to surface routing inflow
            rt_domain(did)%overland%control%surface_water_head_routing = rt_domain(did)%overland%control%surface_water_head_routing &
                + gw2d(did)%excess*1000. ! convert to mm

            ! aggregate  qsgw from routing to lsm grid
            call aggregateQsgw(did)

            gw2d(did)%istep =  gw2d(did)%istep + 1

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
            write(6,*) "*****bf******end 2d_gw_model "
#else
            write(78,*) "*****bf******end 2d_gw_model "
#endif
#endif

        end if

    end if    !DJG (End if for RTE SWC activation)
    !------------------------------------------------------------------
    !DJG End GW/Baseflow Routines
    !-------------------------------------------------------------------
end subroutine driveGwBaseflow




!-------------------------------------------
      subroutine driveChannelRouting(did)

       implicit none
       integer, intent(in) :: did

!-------------------------------------------------------------------
!-------------------------------------------------------------------
!DJG,DNY  Begin Channel and Lake Routing Routines
!-------------------------------------------------------------------

if (nlst(did)%CHANRTSWCRT.eq.1 .or. nlst(did)%CHANRTSWCRT.eq.2) then

 if(nlst(did)%UDMP_OPT .eq. 1) then
     !!! for user defined Reach based Routing method.

    call drive_CHANNEL_RSL(did, nlst(did)%UDMP_OPT,RT_DOMAIN(did)%timestep_flag, RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT,  &
       RT_DOMAIN(did)%LAKE_INFLORT_TS, RT_DOMAIN(did)%QSTRMVOLRT_TS, RT_DOMAIN(did)%TO_NODE, RT_DOMAIN(did)%FROM_NODE, &
       RT_DOMAIN(did)%TYPEL, RT_DOMAIN(did)%ORDER, RT_DOMAIN(did)%MAXORDER,   RT_DOMAIN(did)%CH_LNKRT, &
       rt_domain(did)%overland%streams_and_lakes%lake_mask, nlst(did)%DT, nlst(did)%DTCT, nlst(did)%DTRT_CH, &
       RT_DOMAIN(did)%MUSK, RT_DOMAIN(did)%MUSX, RT_DOMAIN(did)%QLINK, &
       RT_DOMAIN(did)%CHANLEN, RT_DOMAIN(did)%MannN, RT_DOMAIN(did)%So, RT_DOMAIN(did)%ChSSlp,RT_DOMAIN(did)%Bw, &
       RT_DOMAIN(did)%Tw,  RT_DOMAIN(did)%Tw_CC, RT_DOMAIN(did)%n_CC, &
       RT_DOMAIN(did)%ChannK,&
       RT_DOMAIN(did)%RESHT, &
       RT_DOMAIN(did)%CVOL, RT_DOMAIN(did)%QLAKEI, &
       RT_DOMAIN(did)%QLAKEO, RT_DOMAIN(did)%LAKENODE, &
       RT_DOMAIN(did)%QINFLOWBASE, RT_DOMAIN(did)%CHANXI, RT_DOMAIN(did)%CHANYJ, nlst(did)%channel_option,  &
       RT_DOMAIN(did)%nlinks, RT_DOMAIN(did)%NLINKSL, RT_DOMAIN(did)%LINKID, RT_DOMAIN(did)%node_area,         &
       RT_DOMAIN(did)%qout_gwsubbas, &
       RT_DOMAIN(did)%LAKEIDA, RT_DOMAIN(did)%LAKEIDM, RT_DOMAIN(did)%NLAKES, RT_DOMAIN(did)%LAKEIDX   &
#ifdef MPP_LAND
       , RT_DOMAIN(did)%nlinks_index, RT_DOMAIN(did)%mpp_nlinks, RT_DOMAIN(did)%yw_mpp_nlinks  &
       , RT_DOMAIN(did)%LNLINKSL   &
       , RT_DOMAIN(did)%gtoNode, RT_DOMAIN(did)%toNodeInd, RT_DOMAIN(did)%nToInd      &
#endif
       , RT_DOMAIN(did)%CH_LNKRT_SL, RT_DOMAIN(did)%landRunOff   &
#ifdef WRF_HYDRO_NUDGING
       , RT_DOMAIN(did)%nudge &
#endif
       , rt_domain(did)%accSfcLatRunoff,    rt_domain(did)%accBucket &
       , rt_domain(did)%qSfcLatRunoff,        rt_domain(did)%qBucket &
       , rt_domain(did)%QLateral,            rt_domain(did)%velocity &
       ,                                     rt_domain(did)%qloss    &
       ,                                     RT_DOMAIN(did)%HLINK    &
       , rt_domain(did)%nlinksize,            nlst(did)%OVRTSWCRT &
       ,                                     nlst(did)%SUBRTSWCRT &
       , nlst(did)%channel_only , nlst(did)%channelBucket_only &
       , nlst(did)%channel_bypass )

else

    call drive_CHANNEL(did, RT_DOMAIN(did)%latval,RT_DOMAIN(did)%lonval, &
       RT_DOMAIN(did)%timestep_flag,RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT, &
       nlst(did)%SUBRTSWCRT, rt_domain(did)%subsurface%state%qsubrt, &
       RT_DOMAIN(did)%LAKE_INFLORT_TS, RT_DOMAIN(did)%QSTRMVOLRT_TS,&
       RT_DOMAIN(did)%TO_NODE, RT_DOMAIN(did)%FROM_NODE, RT_DOMAIN(did)%TYPEL,&
       RT_DOMAIN(did)%ORDER, RT_DOMAIN(did)%MAXORDER, RT_DOMAIN(did)%NLINKS,&
       RT_DOMAIN(did)%CH_NETLNK, rt_domain(did)%overland%streams_and_lakes%ch_netrt,RT_DOMAIN(did)%CH_LNKRT,&
       rt_domain(did)%overland%streams_and_lakes%lake_mask, nlst(did)%DT, nlst(did)%DTCT, nlst(did)%DTRT_CH,&
       RT_DOMAIN(did)%MUSK, RT_DOMAIN(did)%MUSX,  RT_DOMAIN(did)%QLINK, &
       RT_DOMAIN(did)%QLateral, &
       RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ELRT,RT_DOMAIN(did)%CHANLEN,&
       RT_DOMAIN(did)%MannN,RT_DOMAIN(did)%So, RT_DOMAIN(did)%ChSSlp, &
       RT_DOMAIN(did)%Bw,RT_DOMAIN(did)%Tw,RT_DOMAIN(did)%Tw_CC, RT_DOMAIN(did)%n_CC, &
       RT_DOMAIN(did)%ChannK,&
       RT_DOMAIN(did)%RESHT, &
       RT_DOMAIN(did)%ZELEV, RT_DOMAIN(did)%CVOL, &
       RT_DOMAIN(did)%NLAKES, RT_DOMAIN(did)%QLAKEI, RT_DOMAIN(did)%QLAKEO,&
       RT_DOMAIN(did)%LAKENODE, rt_domain(did)%overland%properties%distance_to_neighbor, &
       RT_DOMAIN(did)%QINFLOWBASE, RT_DOMAIN(did)%CHANXI, &
       RT_DOMAIN(did)%CHANYJ, nlst(did)%channel_option, &
       RT_DOMAIN(did)%RETDEP_CHAN, RT_DOMAIN(did)%NLINKSL, RT_DOMAIN(did)%LINKID, &
       RT_DOMAIN(did)%node_area  &
#ifdef MPP_LAND
       ,RT_DOMAIN(did)%lake_index,RT_DOMAIN(did)%link_location,&
       RT_DOMAIN(did)%mpp_nlinks,RT_DOMAIN(did)%nlinks_index, &
       RT_DOMAIN(did)%yw_mpp_nlinks  &
       , RT_DOMAIN(did)%LNLINKSL &
       , rt_domain(did)%gtoNode,rt_domain(did)%toNodeInd,rt_domain(did)%nToInd  &
#endif
       , rt_domain(did)%CH_LNKRT_SL   &
       ,nlst(did)%GwBaseSwCRT, gw2d(did)%ho, gw2d(did)%qgw_chanrt, &
       nlst(did)%gwChanCondSw, nlst(did)%gwChanCondConstIn, &
       nlst(did)%gwChanCondConstOut, rt_domain(did)%velocity, rt_domain(did)%qloss &
       )
endif

    if((nlst(did)%gwBaseSwCRT == 3) .and. (nlst(did)%gwChanCondSw .eq. 1)) then

           ! add/rm channel-aquifer exchange contribution

           gw2d(did)%ho =  gw2d(did)%ho  &
                        +(((gw2d(did)%qgw_chanrt*(-1)) * gw2d(did)%dt / gw2d(did)%dx**2) &
                        /  gw2d(did)%poros)

    endif
  endif

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
           write(6,*) "*****yw******end drive_CHANNEL "
#else
           write(78,*) "*****yw******end drive_CHANNEL "
#endif
#endif

      end subroutine driveChannelRouting



!------------------------------------------------
      subroutine aggregateDomain(did)

#ifdef MPP_LAND
        use module_mpp_land, only:  sum_real1, my_id, io_id, numprocs
#endif

       implicit none
       integer, intent(in) :: did

       integer :: i, j, krt, ixxrt, jyyrt, &
                  AGGFACYRT, AGGFACXRT

#ifdef HYDRO_D
! ADCHANGE: Water balance variables
       integer :: kk
       real    :: smcrttot1,smctot2,sicetot2
       real    :: suminfxsrt1,suminfxs2
#endif

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
 	print *, "Beginning Aggregation..."
#else
       write(78,*) "Beginning Aggregation..."
#endif
#endif

#ifdef HYDRO_D
! ADCHANGE: START Initial water balance variables
! ALL VARS in MM
        suminfxsrt1 = 0.
        smcrttot1 = 0.
        do i=1,RT_DOMAIN(did)%IXRT
         do j=1,RT_DOMAIN(did)%JXRT
            suminfxsrt1 = suminfxsrt1 + rt_domain(did)%overland%control%surface_water_head_routing(I,J) &
                               / float(RT_DOMAIN(did)%IXRT * RT_DOMAIN(did)%JXRT)
            do kk=1,nlst(did)%NSOIL
                smcrttot1 = smcrttot1 + rt_domain(did)%subsurface%grid_transform%smcrt(I,J,KK)*RT_DOMAIN(did)%subsurface%properties%sldpth(KK)*1000. &
                               / float(RT_DOMAIN(did)%IXRT * RT_DOMAIN(did)%JXRT)
            end do
         end do
        end do
#ifdef MPP_LAND
! not tested
        CALL sum_real1(suminfxsrt1)
        CALL sum_real1(smcrttot1)
        suminfxsrt1 = suminfxsrt1/float(numprocs)
        smcrttot1 = smcrttot1/float(numprocs)
#endif
! END Initial water balance variables
#endif

        do J=1,RT_DOMAIN(did)%JX
          do I=1,RT_DOMAIN(did)%IX

             RT_DOMAIN(did)%SFCHEADAGGRT = 0.
!DJG Subgrid weighting edit...
             RT_DOMAIN(did)%LSMVOL=0.
             do KRT=1,nlst(did)%NSOIL
!                SMCAGGRT(KRT) = 0.
               RT_DOMAIN(did)%SH2OAGGRT(KRT) = 0.
             end do


             do AGGFACYRT=nlst(did)%AGGFACTRT-1,0,-1
              do AGGFACXRT=nlst(did)%AGGFACTRT-1,0,-1


                IXXRT=I*nlst(did)%AGGFACTRT-AGGFACXRT
                JYYRT=J*nlst(did)%AGGFACTRT-AGGFACYRT
#ifdef MPP_LAND
       if(left_id.ge.0) IXXRT=IXXRT+1
       if(down_id.ge.0) JYYRT=JYYRT+1
#else
!yw ????
!       IXXRT=IXXRT+1
!       JYYRT=JYYRT+1
#endif

!State Variables
                RT_DOMAIN(did)%SFCHEADAGGRT = RT_DOMAIN(did)%SFCHEADAGGRT &
                                            + rt_domain(did)%overland%control%surface_water_head_routing(IXXRT,JYYRT)
!DJG Subgrid weighting edit...
                RT_DOMAIN(did)%LSMVOL = RT_DOMAIN(did)%LSMVOL &
                                      + rt_domain(did)%overland%control%surface_water_head_routing(IXXRT,JYYRT) &
                                      * rt_domain(did)%overland%properties%distance_to_neighbor(IXXRT,JYYRT,9)

                do KRT=1,nlst(did)%NSOIL
!DJG               SMCAGGRT(KRT)=SMCAGGRT(KRT)+SMCRT(IXXRT,JYYRT,KRT)
                   RT_DOMAIN(did)%SH2OAGGRT(KRT) = RT_DOMAIN(did)%SH2OAGGRT(KRT) &
                                                 + rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT)
                end do

              end do
             end do



            rt_domain(did)%overland%control%surface_water_head_lsm(I,J) = RT_DOMAIN(did)%SFCHEADAGGRT &
                                          / (nlst(did)%AGGFACTRT**2)

            do KRT=1,nlst(did)%NSOIL
!DJG              SMC(I,J,KRT)=SMCAGGRT(KRT)/(AGGFACTRT**2)
               RT_DOMAIN(did)%SH2OX(I,J,KRT) = RT_DOMAIN(did)%SH2OAGGRT(KRT) &
                                             / (nlst(did)%AGGFACTRT**2)
            end do



!DJG Calculate subgrid weighting array...

              do AGGFACYRT=nlst(did)%AGGFACTRT-1,0,-1
                do AGGFACXRT=nlst(did)%AGGFACTRT-1,0,-1
                  IXXRT=I*nlst(did)%AGGFACTRT-AGGFACXRT
                  JYYRT=J*nlst(did)%AGGFACTRT-AGGFACYRT
#ifdef MPP_LAND
       if(left_id.ge.0) IXXRT=IXXRT+1
       if(down_id.ge.0) JYYRT=JYYRT+1
#else
!yw ???
!       IXXRT=IXXRT+1
!       JYYRT=JYYRT+1
#endif
                  if (RT_DOMAIN(did)%LSMVOL.gt.0.) then
                    RT_DOMAIN(did)%INFXSWGT(IXXRT,JYYRT) &
                                          = rt_domain(did)%overland%control%surface_water_head_routing(IXXRT,JYYRT) &
                                          * rt_domain(did)%overland%properties%distance_to_neighbor(IXXRT,JYYRT,9) &
					  / RT_DOMAIN(did)%LSMVOL
                  else
                    RT_DOMAIN(did)%INFXSWGT(IXXRT,JYYRT) &
                                          = 1./FLOAT(nlst(did)%AGGFACTRT**2)
                  end if

                  do KRT=1,nlst(did)%NSOIL

!!!yw added for debug
                   if(rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT) .lt. 0) then
#ifndef NCEP_WCOSS
                      print*, "Error negative SMCRT", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                      write(78,*) "WARNING: negative SMCRT", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
                   endif
                   if(RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) .lt. 0) then
#ifndef NCEP_WCOSS
                      print *, "Error negative SH2OWGT", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                      write(78,*) "WARNING: negative SH2OWGT", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
                   endif

                    IF ( (rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT) - &
                          rt_domain(did)%subsurface%grid_transform%smcmaxrt(IXXRT,JYYRT,KRT)) .GT. 0.000001 ) THEN
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
                      print *, "SMCMAX exceeded upon aggregation...", &
                           rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),  &
                           rt_domain(did)%subsurface%grid_transform%smcmaxrt(IXXRT,JYYRT,KRT)
#else
                     write(78,*) "FATAL ERROR: SMCMAX exceeded upon aggregation...", &
                           rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),  &
                           rt_domain(did)%subsurface%grid_transform%smcmaxrt(IXXRT,JYYRT,KRT)
#endif
#endif
                      call hydro_stop("In module_HYDRO_drv.F aggregateDomain() - "// &
                                      "SMCMAX exceeded upon aggregation.")
                    END IF
                    IF(RT_DOMAIN(did)%SH2OX(I,J,KRT).LT.0.) THEN
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
                      print *, "Erroneous value of SH2O...", &
                                RT_DOMAIN(did)%SH2OX(I,J,KRT),I,J,KRT
                      print *, "Error negative SH2OX", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                     write(78,*) "Erroneous value of SH2O...", &
                                RT_DOMAIN(did)%SH2OX(I,J,KRT),I,J,KRT
                      write(78,*) "FATAL ERROR: negative SH2OX", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
#endif
                      call hydro_stop("In module_HYDRO_drv.F aggregateDomain() "// &
                                      "- Error negative SH2OX")
                    END IF

		    IF ( RT_DOMAIN(did)%SH2OX(I,J,KRT) .gt. 0 ) THEN
                    	RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) &
                                 = rt_domain(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT) &
                                 / RT_DOMAIN(did)%SH2OX(I,J,KRT)
                    ELSE
#ifdef HYDRO_D
                         print *, "Error zero SH2OX", rt_domain(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%subsurface%grid_transform%smcrt(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
                         RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) = 0.0
                    ENDIF
!?yw
                    RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) = max(1.0E-05, RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT))
                  end do

                end do
              end do

         end do
        end do


#ifdef MPP_LAND
        call MPP_LAND_COM_REAL(RT_DOMAIN(did)%INFXSWGT, &
                               RT_DOMAIN(did)%IXRT,    &
                               RT_DOMAIN(did)%JXRT, 99)

        do i = 1, nlst(did)%NSOIL
           call MPP_LAND_COM_REAL(RT_DOMAIN(did)%SH2OWGT(:,:,i), &
                                  RT_DOMAIN(did)%IXRT, &
				  RT_DOMAIN(did)%JXRT, 99)
        end do
#endif

!DJG Update SMC with SICE (unchanged) and new value of SH2O from routing...
	RT_DOMAIN(did)%SMC = RT_DOMAIN(did)%SH2OX + RT_DOMAIN(did)%SICE

#ifdef HYDRO_D
! ADCHANGE: START Final water balance variables
! ALL VARS in MM
        suminfxs2 = 0.
        smctot2 = 0.
        sicetot2 = 0.
        do i=1,RT_DOMAIN(did)%IX
         do j=1,RT_DOMAIN(did)%JX
            suminfxs2 = suminfxs2 + rt_domain(did)%overland%control%surface_water_head_lsm(I,J) &
                               / float(RT_DOMAIN(did)%IX * RT_DOMAIN(did)%JX)
            do kk=1,nlst(did)%NSOIL
                smctot2 = smctot2 + rt_domain(did)%SMC(I,J,KK)*RT_DOMAIN(did)%subsurface%properties%sldpth(KK)*1000. &
                               / float(RT_DOMAIN(did)%IX * RT_DOMAIN(did)%JX)
               sicetot2 = sicetot2 + rt_domain(did)%SICE(I,J,KK)*RT_DOMAIN(did)%subsurface%properties%sldpth(KK)*1000. &
                                / float(RT_DOMAIN(did)%IX * RT_DOMAIN(did)%JX)
            end do
         end do
        end do

#ifdef MPP_LAND
! not tested
        CALL sum_real1(suminfxs2)
        CALL sum_real1(smctot2)
        CALL sum_real1(sicetot2)
        suminfxs2 = suminfxs2/float(numprocs)
        smctot2 = smctot2/float(numprocs)
        sicetot2 = sicetot2/float(numprocs)
#endif

#ifdef MPP_LAND
       if (my_id .eq. IO_id) then
#endif
         print *, "Agg Mass Bal: "
         print *, "WB_AGG!InfxsDiff", suminfxs2-suminfxsrt1
         print *, "WB_AGG!Infxs1", suminfxsrt1
         print *, "WB_AGG!Infxs2", suminfxs2
         print *, "WB_AGG!SMCDiff", smctot2-smcrttot1-sicetot2
         print *, "WB_AGG!SMC1", smcrttot1
         print *, "WB_AGG!SMC2", smctot2
         print *, "WB_AGG!SICE2", sicetot2
         print *, "WB_AGG!Residual", (suminfxs2-suminfxsrt1) + &
                         (smctot2-smcrttot1-sicetot2)
#ifdef MPP_LAND
	endif
#endif
! END Final water balance variables
#endif

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
 	print *, "Finished Aggregation..."
#else
       write(78,*) "Finished Aggregation..."
#endif
#endif


      end subroutine aggregateDomain

      subroutine RunOffDisag(runoff1x_in, runoff1x, area_lsm,cellArea, infxswgt, AGGFACTRT, ix,jx)
        implicit none
        real, dimension(:,:) :: runoff1x_in, runoff1x, area_lsm, cellArea, infxswgt
        integer :: i,j,ix,jx,AGGFACYRT, AGGFACXRT, AGGFACTRT, IXXRT, JYYRT

        do J=1,JX
        do I=1,IX
             do AGGFACYRT=AGGFACTRT-1,0,-1
             do AGGFACXRT=AGGFACTRT-1,0,-1
               IXXRT=I*AGGFACTRT-AGGFACXRT
               JYYRT=J*AGGFACTRT-AGGFACYRT
#ifdef MPP_LAND
       if(left_id.ge.0) IXXRT=IXXRT+1
       if(down_id.ge.0) JYYRT=JYYRT+1
#endif
!DJG Implement subgrid weighting routine...
               if( (runoff1x_in(i,j) .lt. 0) .or. (runoff1x_in(i,j) .gt. 1000) ) then
                    runoff1x(IXXRT,JYYRT) = 0
               else
                    runoff1x(IXXRT,JYYRT)=runoff1x_in(i,j)*area_lsm(I,J)     &
                        *INFXSWGT(IXXRT,JYYRT)/cellArea(IXXRT,JYYRT)
               endif

             enddo
             enddo
        enddo
        enddo

      end subroutine RunOffDisag


! This routine was extracted from the aggregateDomain routine above to do simple depth aggregation.
! There might be a simpler way.
subroutine RunoffAggregate(runoff_in, runoff_out, aggfactrt, ix, jx)
  implicit none
  ! Input variables
  integer, intent(in) :: aggfactrt, ix, jx
  real, intent(in), dimension(:,:) :: runoff_in
  real, intent(inout), dimension(:,:) :: runoff_out
  ! Local variables
  integer :: i, j, aggfacyrt, aggfacxrt, ixxrt, jyyrt
  real :: runoffagg
  do j=1,jx
    do i=1,ix
    runoffagg = 0.
    do aggfacyrt=aggfactrt-1,0,-1
      do aggfacxrt=aggfactrt-1,0,-1
        ixxrt = i * aggfactrt - aggfacxrt
        jyyrt = j * aggfactrt - aggfacyrt
#ifdef MPP_LAND
        if(left_id.ge.0) ixxrt = ixxrt+1
        if(down_id.ge.0) jyyrt = jyyrt+1
#endif
        runoffagg = runoffagg + runoff_in(ixxrt,jyyrt)
      end do
    end do
   runoff_out(i,j) = runoffagg / (aggfactrt**2)
  end do
end do
end subroutine RunoffAggregate

subroutine HYDRO_ini(ntime, did,ix0,jx0, vegtyp,soltyp)
implicit none
integer ntime, did
integer rst_out, ix,jx
!        integer, OPTIONAL:: ix0,jx0
integer:: ix0,jx0
integer, dimension(ix0,jx0),optional :: vegtyp, soltyp
integer            :: iret, ncid, ascIndId



! read the namelist
! the lsm namelist will be read by rtland sequentially again.
!call read_rt_nlst(nlst(did) )

! Some field of this structure are already initialized by the CPL component (e.g. DT)
call orchestrator%config%init_nlst(did)

if(nlst(did)%rtFlag .eq. 0) return

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! get the dimension
call get_file_dimension(trim(nlst(did)%geo_static_flnm), ix,jx)


#ifdef MPP_LAND

if (nlst(did)%sys_cpl .eq. 1 .or. nlst(did)%sys_cpl .eq. 4) then
   !sys_cpl: 1-- coupling with HRLDAS but running offline lsm;
   !         2-- coupling with WRF but do not run offline lsm
   !         3-- coupling with LIS and do not run offline lsm
   !         4:  coupling with CLM

   ! create 2 dimensiaon logical mapping of the CPUs for coupling with CLM or HRLDAS.
   call log_map2d()

   global_nx = ix  ! get from land model
   global_ny = jx  ! get from land model

   call mpp_land_bcast_int1(global_nx)
   call mpp_land_bcast_int1(global_ny)

!!! temp set global_nx to ix
   rt_domain(did)%ix = global_nx
   rt_domain(did)%jx = global_ny

   ! over write the ix and jx
   call MPP_LAND_PAR_INI(1,rt_domain(did)%ix,rt_domain(did)%jx,&
        nlst(did)%AGGFACTRT)
else
   !  coupled with WRF, LIS
   numprocs = node_info(1,1)

   call wrf_LAND_set_INIT(node_info,numprocs,nlst(did)%AGGFACTRT)

   rt_domain(did)%ix = local_nx
   rt_domain(did)%jx = local_ny
endif


rt_domain(did)%g_IXRT=global_rt_nx
rt_domain(did)%g_JXRT=global_rt_ny
rt_domain(did)%ixrt = local_rt_nx
rt_domain(did)%jxrt = local_rt_ny

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
write(6,*) "rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt"
write(6,*)  rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt
write(6,*) "rt_domain(did)%ix, rt_domain(did)%jx "
write(6,*) rt_domain(did)%ix, rt_domain(did)%jx
write(6,*) "global_nx, global_ny, local_nx, local_ny"
write(6,*) global_nx, global_ny, local_nx, local_ny
#else
write(78,*) "rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt"
write(78,*)  rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt
write(78,*) "rt_domain(did)%ix, rt_domain(did)%jx "
write(78,*) rt_domain(did)%ix, rt_domain(did)%jx
write(78,*) "global_nx, global_ny, local_nx, local_ny"
write(78,*) global_nx, global_ny, local_nx, local_ny
#endif
#endif
#else
! sequential
rt_domain(did)%ix = ix
rt_domain(did)%jx = jx
rt_domain(did)%ixrt = ix*nlst(did)%AGGFACTRT
rt_domain(did)%jxrt = jx*nlst(did)%AGGFACTRT
#endif


!      allocate rt arrays


call getChanDim(did)

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
write(6,*) "finish getChanDim "
#else
write(78,*) "finish getChanDim "
#endif
#endif

! ADCHANGE: get global attributes
! need to set these after getChanDim since it allocates rt_domain vals to 0
     call get_file_globalatts(trim(nlst(did)%geo_static_flnm), &
          rt_domain(did)%iswater, rt_domain(did)%islake, rt_domain(did)%isurban, rt_domain(did)%isoilwater)

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
      write(6,*) "hydro_ini: rt_domain(did)%iswater, rt_domain(did)%islake, rt_domain(did)%isurban, rt_domain(did)%isoilwater"
      write(6,*) rt_domain(did)%iswater, rt_domain(did)%islake, rt_domain(did)%isurban, rt_domain(did)%isoilwater
#endif
#endif

if(nlst(did)%GWBASESWCRT .eq. 3 ) then
   call gw2d_allocate(did,&
        rt_domain(did)%ixrt,&
        rt_domain(did)%jxrt,&
        nlst(did)%nsoil)
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
   write(6,*) "finish gw2d_allocate"
#else
   write(78,*) "finish gw2d_allocate"
#endif
#endif
endif

! calculate the distance between grids for routing.
! decompose the land parameter/data


!      ix0= rt_domain(did)%ix
!      jx0= rt_domain(did)%jx
if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then
   if(present(vegtyp)) then
      call lsm_input(did,ix0=ix0,jx0=jx0,vegtyp0=vegtyp,soltyp0=soltyp)
   else
      call lsm_input(did,ix0=ix0,jx0=jx0)
   endif
endif


#ifdef HYDRO_D
#ifndef NCEP_WCOSS
write(6,*) "finish decomposion"
#else
write(78,*) "finish decomposion"
#endif
#endif

if((nlst(did)%channel_only .eq. 1 .or. nlst(did)%channelBucket_only .eq. 1) .and. &
     nlst(1)%io_form_outputs .ne. 0) then
   !! This is the "decoder ring" for reading channel-only forcing from io_form_outputs=1,2 CHRTOUT files.
   !! Only needed on io_id
   if(my_id .eq. io_id) then
      allocate(rt_domain(did)%ascendIndex(rt_domain(did)%gnlinksl))
      iret = nf90_open(trim(nlst(1)%route_link_f),NF90_NOWRITE,ncid=ncid)
      !if(iret .ne. 0) call hdyro_stop
      if(iret .ne. 0) call hydro_stop('ERROR: Unable to open RouteLink file for index extraction')
      iret = nf90_inq_varid(ncid,'ascendingIndex',ascIndId)
      if(iret .ne. 0) call hydro_stop('ERROR: Unable to find ascendingIndex from RouteLink file.')
      iret = nf90_get_var(ncid,ascIndId,rt_domain(did)%ascendIndex)
      if(iret .ne. 0) call hydro_stop('ERROR: Unable to extract ascendingIndex from RouteLink file.')
      iret = nf90_close(ncid)
      if(iret .ne. 0) call hydro_stop('ERROR: Unable to close RouteLink file.')
   else
      allocate(rt_domain(did)%ascendIndex(1))
      rt_domain(did)%ascendIndex(1)=-9
   endif
endif


call get_dist_lsm(did) !! always needed (channel_only and channelBucket_only)
if(nlst(did)%channel_only .ne. 1)  call get_dist_lrt(did) !! needed forchannelBucket_only

! rt model initilization
call LandRT_ini(did)

if(nlst(did)%GWBASESWCRT .eq. 3 ) then

   call gw2d_ini(did,&
        nlst(did)%dt,&
        nlst(did)%dxrt0)
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
   write(6,*) "finish gw2d_ini"
#else
   write(78,*) "finish gw2d_ini"
#endif
#endif
endif
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
write(6,*) "finish LandRT_ini"
#else
write(78,*) "finish LandRT_ini"
#endif
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if(nlst(did)%channel_only .eq. 0 .and. nlst(did)%channelBucket_only .eq. 0) then

   if (nlst(did)%TERADJ_SOLAR.eq.1 .and. nlst(did)%CHANRTSWCRT.ne.2) then   ! Perform ter rain adjustment of incoming solar
#ifdef MPP_LAND
      call MPP_seq_land_SO8(rt_domain(did)%SO8LD_D,rt_domain(did)%SO8LD_Vmax,&
           rt_domain(did)%TERRAIN, rt_domain(did)%dist_lsm,&
           rt_domain(did)%ix,rt_domain(did)%jx,global_nx,global_ny)
#else
      call seq_land_SO8(rt_domain(did)%SO8LD_D,rt_domain(did)%SO8LD_Vmax,&
           rt_domain(did)%TERRAIN,rt_domain(did)%dist_lsm,&
           rt_domain(did)%ix,rt_domain(did)%jx)
#endif
   endif
endif

if (nlst(did)%GWBASESWCRT .gt. 0) then
   if(nlst(did)%UDMP_OPT .eq. 1) then
      call get_basn_area_nhd(rt_domain(did)%basns_area)
   else
      call get_basn_area(did)
   endif
endif

if (nlst(did)%CHANRTSWCRT.eq.1 .or. nlst(did)%CHANRTSWCRT .eq. 2 ) then
   call get_node_area(did)
endif


#ifdef WRF_HYDRO_NUDGING
if(nlst(did)%CHANRTSWCRT .ne. 0) call init_stream_nudging
#endif


!    if (trim(nlst_rt(did)%restart_file) == "") then
! output at the initial time
!        call HYDRO_out(did)
!        return
!    endif

! restart the file

        ! jummp the initial time output
!        rt_domain(did)%out_counts = rt_domain(did)%out_counts + 1
!        rt_domain(did)%his_out_counts = rt_domain(did)%his_out_counts + 1


call HYDRO_rst_in(did)
!#ifdef HYDRO_REALTIME
if (trim(nlst(did)%restart_file) == "") then
  call HYDRO_out(did, 0)
else
  call HYDRO_out(did, 1)
endif
!! JLM: This is only currently part 1/2 of a better accumulation tracking strategy.
!! The parts:
!! 1) (this) zero accumulations on restart/init after any t=0 outputs are written.
!! 2) introduce a variable in the output files which indicates if accumulations are
!!    reset after this output.
!! Here we move zeroing to after AFTER outputing the accumulations at the initial time.
!! This was previously done in HYDRO_rst_in and so output accumulations at time
!! zero were getting zeroed and then writtent to file, which looses information.
!! Note that nlst_rt(did)%rstrt_swc is not changed at any point in between here and the rst_in.
if(nlst(did)%rstrt_swc.eq.1) then  !Switch for rest of restart accum vars...
   print *, "Resetting RESTART Accumulation Variables to 0...",nlst(did)%rstrt_swc
   ! Under channel-only , these first three variables are not allocated.
   if(allocated(rt_domain(did)%overland%streams_and_lakes%surface_water_to_lake))    rt_domain(did)%overland%streams_and_lakes%surface_water_to_lake = zeroFlt
   if(allocated(rt_domain(did)%QSTRMVOLRT_ACC))  rt_domain(did)%QSTRMVOLRT_ACC   = zeroFlt
   ! These variables are optionally allocated, if their output is requested.
   if(allocated(rt_domain(did)%accSfcLatRunoff)) rt_domain(did)%accSfcLatRunoff = zeroDbl
   if(allocated(rt_domain(did)%accBucket))       rt_domain(did)%accBucket    = zeroDbl
end if

end subroutine HYDRO_ini

      subroutine lsm_input(did,ix0,jx0,vegtyp0,soltyp0)
         implicit none
         integer did, leng, ncid, ierr_flg
         parameter(leng=100)
         integer :: i,j, nn
         integer, allocatable, dimension(:,:) :: soltyp
         real, dimension(leng) :: xdum1, MAXSMC,refsmc,wltsmc

        integer :: ix0,jx0
        integer, dimension(ix0,jx0),OPTIONAL :: vegtyp0, soltyp0
        integer :: iret, istmp

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
         write(6,*) RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx
#else
         write(78,*) RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx
#endif
#endif

         allocate(soltyp(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx) )

         soltyp = 0
         call get2d_lsm_soltyp(soltyp,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst(did)%geo_static_flnm))


         call get2d_lsm_real("HGT",RT_DOMAIN(did)%TERRAIN,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst(did)%geo_static_flnm))

         call get2d_lsm_real("XLAT",RT_DOMAIN(did)%lat_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst(did)%geo_static_flnm))
         call get2d_lsm_real("XLONG",RT_DOMAIN(did)%lon_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst(did)%geo_static_flnm))
         call get2d_lsm_vegtyp(RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst(did)%geo_static_flnm))


            if(nlst(did)%sys_cpl .eq. 2 ) then
              ! coupling with WRF
                if(present(soltyp0) ) then
                  where(VEGTYP0 == rt_domain(did)%iswater .or. VEGTYP0 == rt_domain(did)%islake) soltyp0 = rt_domain(did)%isoilwater
                  where(soltyp0 == rt_domain(did)%isoilwater) VEGTYP0 = rt_domain(did)%iswater
                   soltyp = soltyp0
                   RT_DOMAIN(did)%VEGTYP = VEGTYP0
                endif
            endif

         where(RT_DOMAIN(did)%VEGTYP == rt_domain(did)%iswater .or. RT_DOMAIN(did)%VEGTYP == rt_domain(did)%islake) soltyp = rt_domain(did)%isoilwater
         where(soltyp == rt_domain(did)%isoilwater) RT_DOMAIN(did)%VEGTYP = rt_domain(did)%iswater

! LKSAT,
! temporary set
       RT_DOMAIN(did)%SMCRTCHK = 0
       RT_DOMAIN(did)%SMCAGGRT = 0
       RT_DOMAIN(did)%STCAGGRT = 0
       RT_DOMAIN(did)%SH2OAGGRT = 0


       rt_domain(did)%subsurface%properties%zsoil(1:nlst(did)%nsoil) = nlst(did)%zsoil8(1:nlst(did)%nsoil)

       rt_domain(did)%subsurface%properties%sldpth(1) = abs( RT_DOMAIN(did)%subsurface%properties%zsoil(1) )
       do i = 2, nlst(did)%nsoil
          rt_domain(did)%subsurface%properties%sldpth(i) = RT_DOMAIN(did)%subsurface%properties%zsoil(i-1)-RT_DOMAIN(did)%subsurface%properties%zsoil(i)
       enddo
       rt_domain(did)%subsurface%properties%soldeprt = -1.0*RT_DOMAIN(did)%subsurface%properties%zsoil(nlst(did)%NSOIL)

       ierr_flg = 99
       if(trim(nlst(did)%hydrotbl_f) == "") then
           call hydro_stop("FATAL ERROR: hydrotbl_f is empty. Please input a netcdf file. ")
       endif

#ifdef MPP_LAND
       if(my_id .eq. IO_id) then
#endif
          ierr_flg = nf90_open(trim(nlst(did)%hydrotbl_f), nf90_NOWRITE, ncid)
#ifdef MPP_LAND
       endif
       call mpp_land_bcast_int1(ierr_flg)
#endif
       if( ierr_flg .ne. 0) then
          ! input from HYDRO.tbl FILE
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
                !yw rt_domain(did)%lksat(i,j) = xdum1(soltyp(i,j) ) * 1000.0
                rt_domain(did)%lksat(i,j) = xdum1(soltyp(i,j) )
                rt_domain(did)%SMCMAX1(i,j) = MAXSMC(soltyp(I,J))
                rt_domain(did)%SMCREF1(i,j) = refsmc(soltyp(I,J))
                rt_domain(did)%SMCWLT1(i,j) = wltsmc(soltyp(I,J))
                !ADCHANGE: Add some sanity checks in case calibration knocks the order of these out of sequence.
                !The min diffs were pulled from the existing HYDRO.TBL defaults.
                !Currently water is 0, so enforcing 0 as the absolute min.
                rt_domain(did)%SMCMAX1(i,j) = min(rt_domain(did)%SMCMAX1(i,j), 1.0)
                rt_domain(did)%SMCREF1(i,j) = max(min(rt_domain(did)%SMCREF1(i,j), rt_domain(did)%SMCMAX1(i,j) - 0.01), 0.0)
                rt_domain(did)%SMCWLT1(i,j) = max(min(rt_domain(did)%SMCWLT1(i,j), rt_domain(did)%SMCREF1(i,j) - 0.01), 0.0)
                IF(rt_domain(did)%VEGTYP(i,j) > 0 ) THEN   ! created 2d ov_rough
                    rt_domain(did)%OV_ROUGH2d(i,j) = RT_DOMAIN(did)%OV_ROUGH(rt_domain(did)%VEGTYP(I,J))
                endif
             end do
       end do

       call hdtbl_out(did)
    else
       ! input from HYDRO.TBL.nc file
       print*, "reading from hydrotbl_f(HYDRO.TBL.nc)  file ...."
       call hdtbl_in_nc(did)
       if (noah_lsm%imperv_option .eq. 9) then
         !ADCHANGE: For consistency, mirror urban and param value checks used in table read
         where (rt_domain(did)%VEGTYP == rt_domain(did)%isurban)
           rt_domain(did)%SMCMAX1 = 0.45
           rt_domain(did)%SMCREF1 = 0.42
           rt_domain(did)%SMCWLT1 = 0.40
         endwhere
       endif
       where (rt_domain(did)%SMCMAX1 .gt. 1.0) rt_domain(did)%SMCMAX1 = 1.0
       rt_domain(did)%SMCREF1 = max(min(rt_domain(did)%SMCREF1, rt_domain(did)%SMCMAX1 - 0.01), 0.0)
       rt_domain(did)%SMCWLT1 = max(min(rt_domain(did)%SMCWLT1, rt_domain(did)%SMCREF1 - 0.01), 0.0)
    endif

       rt_domain(did)%soiltyp = soltyp

       if(allocated(soltyp)) deallocate(soltyp)


      end subroutine lsm_input


end module module_HYDRO_drv

! stop the job due to the fatal error.
subroutine HYDRO_finish()
#ifdef MPP_LAND
    USE module_mpp_land
#endif
#ifdef WRF_HYDRO_NUDGING
    use module_stream_nudging,  only: finish_stream_nudging
#endif

    integer :: ierr

#ifdef WRF_HYDRO_NUDGING
    call finish_stream_nudging()
#endif
#ifndef NCEP_WCOSS
    print*, "The model finished successfully......."
#else
    write(78,*) "The model finished successfully......."
#endif
#ifdef MPP_LAND
!         call mpp_land_abort()
#ifndef NCEP_WCOSS
    call flush(6)
#else
    call flush(78)
    close(78)
#endif
    call mpp_land_sync()
    call MPI_finalize(ierr)
    stop
#else

#ifndef WRF_HYDRO_NUDGING
    stop  !!JLM want to time at the top NoahMP level.
#endif

#endif

    return
end  subroutine HYDRO_finish
