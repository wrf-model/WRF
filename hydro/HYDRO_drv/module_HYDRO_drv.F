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
                               restart_out_bi, restart_in_bi, mpp_output_chrt2, mpp_output_lakes2
   USE module_mpp_land
#else
   use module_HYDRO_io, only:  output_rt, output_chrt, output_chrt2, output_lakes
#endif
   use module_HYDRO_io, only: sub_output_gw, restart_out_nc, restart_in_nc,  &
        get_file_dimension ,get2d_lsm_real, get2d_lsm_vegtyp, get2d_lsm_soltyp, &
        output_lsm,  output_GW_Diag
   use module_HYDRO_io, only : output_lakes2
   use module_rt_data, only: rt_domain
   use module_GW_baseflow
   use module_gw_gw2d
   use module_gw_gw2d_data, only: gw2d
   use module_channel_routing, only: drive_channel, drive_channel_rsl
   use module_namelist, only: nlst_rt, read_rt_nlst
   use module_routing, only: getChanDim, landrt_ini
   use module_HYDRO_utils
!   use module_namelist
   use module_lsm_forcing, only: geth_newdate
#ifdef WRF_HYDRO_NUDGING
   use module_stream_nudging,  only: init_stream_nudging
#endif

   use module_UDMAP, only: get_basn_area_nhd

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
     if(nlst_rt(did)%dt .gt. nlst_rt(did)%rst_dt*60) then
        call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%dt*rt_domain(did)%rst_counts))
     else
        call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%rst_dt*60*rt_domain(did)%rst_counts))
     endif
     if ( (nlst_rt(did)%rst_dt .gt. 0) .and. (out_date(1:19) == nlst_rt(did)%olddate(1:19)) ) then
           rst_out = 99
           rt_domain(did)%rst_counts = rt_domain(did)%rst_counts + 1
     endif
! restart every month automatically.
     if ( (nlst_rt(did)%olddate(9:10) == "01") .and. (nlst_rt(did)%olddate(12:13) == "00") .and. &
          (nlst_rt(did)%olddate(15:16) == "00").and. (nlst_rt(did)%olddate(18:19) == "00") .and. &
          (nlst_rt(did)%rst_dt .le. 0)  ) rst_out = 99

#ifdef MPP_LAND
   endif
     call mpp_land_bcast_int1(rst_out)
#endif
    if(rst_out .gt. 0) then 
#ifdef MPP_LAND
      if(nlst_rt(did)%rst_bi_out .eq. 1) then
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
             call mpp_land_bcast_char(16,nlst_rt(did)%olddate(1:16))
             call   RESTART_OUT_bi(trim("HYDRO_RST."//nlst_rt(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst_rt(did)%hgrid)//"."//trim(str_tmp)),  did)
      else
#endif
             call   RESTART_OUT_nc(trim("HYDRO_RST."//nlst_rt(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst_rt(did)%hgrid)),  did)
#ifdef MPP_LAND
      endif
#endif

#ifdef WRF_HYDRO_NUDGING
      call output_nudging_last_obs  !! only does something if temporalPersistence==TRUE
#endif      
   endif


   end subroutine HYDRO_rst_out

   subroutine HYDRO_out(did)
   
      implicit none
      integer did, outflag, rtflag
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
      if(nlst_rt(did)%olddate(1:19) .eq. nlst_rt(did)%startdate(1:19) .and. rt_domain(did)%his_out_counts .eq. 0) then
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
         write(6,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19), rt_domain(did)%his_out_counts
#else
         write(78,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19), rt_domain(did)%his_out_counts
#endif
#endif
         outflag = 99
      else
         if(nlst_rt(did)%dt .gt. nlst_rt(did)%out_dt*60) then
             call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%dt*rt_domain(did)%out_counts))
         else
             call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%out_dt*60*rt_domain(did)%out_counts))
         endif
         if ( out_date(1:19) == nlst_rt(did)%olddate(1:19) ) then
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
             write(6,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19)
#else
             write(78,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19)
#endif
#endif
             outflag = 99
         endif
      endif
#ifdef MPP_LAND
   endif
     call mpp_land_bcast_int1(outflag)
#endif

     call HYDRO_rst_out(did) 

     if (outflag .lt. 0) return

     rt_domain(did)%out_counts = rt_domain(did)%out_counts + 1

     rt_domain(did)%his_out_counts = rt_domain(did)%his_out_counts + 1

     if(nlst_rt(did)%out_dt*60 .gt. nlst_rt(did)%DT) then
        kt = rt_domain(did)%his_out_counts*nlst_rt(did)%out_dt*60/nlst_rt(did)%DT
     else
        kt = rt_domain(did)%his_out_counts
     endif

! jump the ouput for the initial time when it has restart file from routing.
   rtflag = -99
#ifdef MPP_LAND
   if(IO_id .eq. my_id) then
#endif
       if ( (trim(nlst_rt(did)%restart_file) /= "") .and. ( nlst_rt(did)%startdate(1:19) == nlst_rt(did)%olddate(1:19) ) ) then
#ifndef NCEP_WCOSS
             print*, "yyyywww restart_file = ", trim(nlst_rt(did)%restart_file) 
#else
             write(78,*) "yyyywww restart_file = ", trim(nlst_rt(did)%restart_file) 
#endif
             rtflag = 1
       endif
#ifdef MPP_LAND
   endif  
   call mpp_land_bcast_int1(rtflag)
#endif


!yw keep the initial time otuput for debug
      if(rtflag == 1) then 
          rt_domain(did)%restQSTRM = .false.   !!! do not reset QSTRM.. at initial time.
#ifndef HYDRO_REALTIME
          return ! jump the initial time output for routing restart
#endif
      endif


     if(nlst_rt(did)%LSMOUT_DOMAIN .eq. 1)  &
     call output_lsm(trim(nlst_rt(did)%olddate(1:4)//nlst_rt(did)%olddate(6:7)//nlst_rt(did)%olddate(9:10)  &
                 //nlst_rt(did)%olddate(12:13)//nlst_rt(did)%olddate(15:16)//  &
                 ".LSMOUT_DOMAIN"//trim(nlst_rt(did)%hgrid)),     &
                 did)

    

        if(nlst_rt(did)%SUBRTSWCRT .gt. 0 &
             .or. nlst_rt(did)%OVRTSWCRT .gt. 0 &
             .or. nlst_rt(did)%GWBASESWCRT .gt. 0 ) then

           

              if(nlst_rt(did)%RTOUT_DOMAIN .eq. 1)  &
              call output_rt(    &
                nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt, &
                nlst_rt(did)%nsoil, &
!               nlst_rt(did)%startdate, nlst_rt(did)%olddate, RT_DOMAIN(did)%QSUBRT,&
                nlst_rt(did)%sincedate, nlst_rt(did)%olddate, RT_DOMAIN(did)%QSUBRT,&
                RT_DOMAIN(did)%ZWATTABLRT,RT_DOMAIN(did)%SMCRT,&
                RT_DOMAIN(did)%SUB_RESID,       &
                   RT_DOMAIN(did)%q_sfcflx_x,RT_DOMAIN(did)%q_sfcflx_y,&
                RT_DOMAIN(did)%soxrt,RT_DOMAIN(did)%soyrt,&
                RT_DOMAIN(did)%QSTRMVOLRT,RT_DOMAIN(did)%SFCHEADSUBRT, &
                nlst_rt(did)%geo_finegrid_flnm,nlst_rt(did)%DT,&
                RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%LATVAL,&
                RT_DOMAIN(did)%LONVAL,RT_DOMAIN(did)%dist,nlst_rt(did)%RTOUT_DOMAIN,&
                RT_DOMAIN(did)%QBDRYRT &
#ifdef HYDRO_REALTIME
		, nlst_rt(did)%iocflag &
#endif
		 )




           if(nlst_rt(did)%GWBASESWCRT .eq. 3) then
	     
              if(nlst_rt(did)%output_gw  .eq. 1)  &
              call sub_output_gw(    &
                nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt,          &
                nlst_rt(did)%nsoil,                               &
!               nlst_rt(did)%startdate, nlst_rt(did)%olddate,    &
                nlst_rt(did)%sincedate, nlst_rt(did)%olddate,    &
                gw2d(did)%h, RT_DOMAIN(did)%SMCRT,                   &
                gw2d(did)%convgw, gw2d(did)%excess,                  &
                gw2d(did)%qsgwrt, gw2d(did)%qgw_chanrt,              &
                nlst_rt(did)%geo_finegrid_flnm,nlst_rt(did)%DT, &
                RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%LATVAL,       &
                RT_DOMAIN(did)%LONVAL,rt_domain(did)%dist,           &
                nlst_rt(did)%output_gw)

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
     
           if (nlst_rt(did)%CHANRTSWCRT.eq.1.or.nlst_rt(did)%CHANRTSWCRT.eq.2) then 

!ADCHANGE: Change values for within lake reaches to NA
             str_out = RT_DOMAIN(did)%QLINK
             vel_out = RT_DOMAIN(did)%velocity

#ifdef HYDRO_REALTIME
             if (RT_DOMAIN(did)%NLAKES .gt. 0)  then
                    do i=1,RT_DOMAIN(did)%NLINKS
                        if (RT_DOMAIN(did)%TYPEL(i) .eq. 2) then
                            str_out(i,1) = -9.E15
                            vel_out(i) = -9.E15
                        endif
                    end do
             endif
#endif
!ADCHANGE: End

             if(nlst_rt(did)%CHRTOUT_DOMAIN  .eq. 1)  then
#ifdef MPP_LAND
                 call mpp_output_chrt(rt_domain(did)%gnlinks,rt_domain(did)%gnlinksl,rt_domain(did)%map_l2g, &
#else
                 call output_chrt(  &
#endif
                   nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                   RT_DOMAIN(did)%NLINKS,RT_DOMAIN(did)%ORDER, &
                   nlst_rt(did)%sincedate,nlst_rt(did)%olddate,RT_DOMAIN(did)%CHLON,&
                   RT_DOMAIN(did)%CHLAT,                                      &
                   RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ZELEV,                &
                   !RT_DOMAIN(did)%QLINK,nlst_rt(did)%DT,Kt,                   &
                   str_out, nlst_rt(did)%DT,Kt,                               &
                   RT_DOMAIN(did)%STRMFRXSTPTS,nlst_rt(did)%order_to_write,   &
                   RT_DOMAIN(did)%NLINKSL,nlst_rt(did)%channel_option,        &
                   rt_domain(did)%gages, rt_domain(did)%gageMiss,             &
                   nlst_rt(did)%dt                                            &
#ifdef WRF_HYDRO_NUDGING
                   , RT_DOMAIN(did)%nudge                                     &
#endif
                   , RT_DOMAIN(did)%accLndRunOff, RT_DOMAIN(did)%accQLateral, &
                   RT_DOMAIN(did)%accStrmvolrt,                               &
                   RT_DOMAIN(did)%accBucket, nlst_rt(did)%UDMP_OPT            &
                   )
              else
                if(nlst_rt(did)%CHRTOUT_DOMAIN  .eq. 2)  then
#ifdef MPP_LAND
                     call mpp_output_chrt2(rt_domain(did)%gnlinks,rt_domain(did)%gnlinksl,rt_domain(did)%map_l2g, &
#else
                     call output_chrt2(  &
#endif
                     nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                     RT_DOMAIN(did)%NLINKS,RT_DOMAIN(did)%ORDER, &
                     nlst_rt(did)%sincedate,nlst_rt(did)%olddate,RT_DOMAIN(did)%CHLON,&
                     RT_DOMAIN(did)%CHLAT,                                      &
                     RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ZELEV,                &
                     !RT_DOMAIN(did)%QLINK,nlst_rt(did)%DT,Kt,                   &
                     str_out, nlst_rt(did)%DT,Kt,                               &
                     RT_DOMAIN(did)%NLINKSL,nlst_rt(did)%channel_option,        &
                     rt_domain(did)%linkid                                      &
#ifdef WRF_HYDRO_NUDGING
                     , RT_DOMAIN(did)%nudge                                     &
#endif
		     !, RT_DOMAIN(did)%QLateral, nlst_rt(did)%iocflag, RT_DOMAIN(did)%velocity &          
                     , RT_DOMAIN(did)%QLateral, nlst_rt(did)%iocflag, vel_out &
                     , RT_DOMAIN(did)%accLndRunOff,                             &
                     RT_DOMAIN(did)%accQLateral,                                &
                     RT_DOMAIN(did)%accStrmvolrt,                               &
                     RT_DOMAIN(did)%accBucket,                                  &
                     nlst_rt(did)%UDMP_OPT            &
		     )
                 endif

              endif

      
#ifdef MPP_LAND
              if(nlst_rt(did)%CHRTOUT_GRID  .eq. 1)  &
              call mpp_output_chrtgrd(nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                  RT_DOMAIN(did)%ixrt,RT_DOMAIN(did)%jxrt, RT_DOMAIN(did)%NLINKS,   &
                  RT_DOMAIN(did)%GCH_NETLNK, &
                  nlst_rt(did)%startdate, nlst_rt(did)%olddate, &
                  !RT_DOMAIN(did)%qlink, nlst_rt(did)%dt, nlst_rt(did)%geo_finegrid_flnm,   &
                  str_out, nlst_rt(did)%dt, nlst_rt(did)%geo_finegrid_flnm,   &
                  RT_DOMAIN(did)%gnlinks,RT_DOMAIN(did)%map_l2g,                   &
                  RT_DOMAIN(did)%g_ixrt,RT_DOMAIN(did)%g_jxrt )
#endif

               if (RT_DOMAIN(did)%NLAKES.gt.0)  then
                      if(nlst_rt(did)%outlake .eq. 1) then
#ifdef MPP_LAND
                          call mpp_output_lakes( RT_DOMAIN(did)%lake_index, &
#else
                          call output_lakes(  &
#endif
                               nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                               RT_DOMAIN(did)%NLAKES, &
                               trim(nlst_rt(did)%sincedate), trim(nlst_rt(did)%olddate), &
                               RT_DOMAIN(did)%LATLAKE,RT_DOMAIN(did)%LONLAKE, &
                               RT_DOMAIN(did)%ELEVLAKE,RT_DOMAIN(did)%QLAKEI, &
                               RT_DOMAIN(did)%QLAKEO, &
                               RT_DOMAIN(did)%RESHT,nlst_rt(did)%DT,Kt)
                      endif
                      if(nlst_rt(did)%outlake .eq. 2) then
#ifdef MPP_LAND
                          call mpp_output_lakes2( RT_DOMAIN(did)%lake_index, &
#else
                          call output_lakes2(  &
#endif
                               nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                               RT_DOMAIN(did)%NLAKES, &
                               trim(nlst_rt(did)%sincedate), trim(nlst_rt(did)%olddate), &
                               RT_DOMAIN(did)%LATLAKE,RT_DOMAIN(did)%LONLAKE, &
                               RT_DOMAIN(did)%ELEVLAKE,RT_DOMAIN(did)%QLAKEI, &
                               RT_DOMAIN(did)%QLAKEO, &
                               RT_DOMAIN(did)%RESHT,nlst_rt(did)%DT,Kt,RT_DOMAIN(did)%LAKEIDM)
                      endif

               endif   ! end if block of rNLAKES .gt. 0
        endif 
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
      if (trim(nlst_rt(did)%restart_file) /= "") then
          flag = 99
          rt_domain(did)%timestep_flag = 99   ! continue run
      endif 
#ifdef MPP_LAND
   endif 
   call mpp_land_bcast_int1(flag)
#endif

   nlst_rt(did)%sincedate = nlst_rt(did)%startdate
   
   if (flag.eq.99) then

#ifdef MPP_LAND
     if(my_id.eq.IO_id) then
#endif
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
        write(6,*) "*** read restart data: ",trim(nlst_rt(did)%restart_file)
#else
        write(78,*) "*** read restart data: ",trim(nlst_rt(did)%restart_file)
#endif
#endif
#ifdef MPP_LAND
     endif 
#endif

#ifdef MPP_LAND
      if(nlst_rt(did)%rst_bi_in .eq. 1) then
         call RESTART_IN_bi(trim(nlst_rt(did)%restart_file), did)
      else
#endif
         call RESTART_IN_nc(trim(nlst_rt(did)%restart_file), did)
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
         call geth_newdate(newdate, nlst_rt(did)%olddate, nint( nlst_rt(did)%dt))
         nlst_rt(did)%olddate = newdate
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





           if (nlst_rt(did)%GWBASESWCRT .ne. 0     &
               .or. nlst_rt(did)%SUBRTSWCRT .NE.0  &
               .or. nlst_rt(did)%OVRTSWCRT .NE. 0 ) THEN


              RT_DOMAIN(did)%QSTRMVOLRT_DUM = RT_DOMAIN(did)%QSTRMVOLRT
              RT_DOMAIN(did)%LAKE_INFLORT_DUM = RT_DOMAIN(did)%LAKE_INFLORT



                ! step 1) disaggregate specific fields from LSM to Hydro grid
                if(nlst_rt(did)%SUBRTSWCRT .NE.0 .or. nlst_rt(did)%OVRTSWCRT .NE. 0) then
                    call disaggregateDomain_drv(did)
                endif
                if(nlst_rt(did)%OVRTSWCRT .eq. 0) then
                   if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                      call RunOffDisag(RT_DOMAIN(did)%INFXSRT, RT_DOMAIN(did)%landRunOff,        &
                          rt_domain(did)%dist_lsm(:,:,9),RT_DOMAIN(did)%dist(:,:,9), &
                          RT_DOMAIN(did)%INFXSWGT, nlst_rt(did)%AGGFACTRT, RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx)
                   endif
                endif


#ifdef HYDRO_D
                call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
               ! step 2)
                if(nlst_rt(did)%SUBRTSWCRT .NE.0) then
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
		
		! step 3) todo split
#ifdef HYDRO_D
                call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
                if(nlst_rt(did)%OVRTSWCRT .NE. 0) then
  		    call OverlandRouting_drv(did)
                else
                    RT_DOMAIN(did)%SFCHEADSUBRT = RT_DOMAIN(did)%INFXSUBRT
                    RT_DOMAIN(did)%INFXSUBRT = 0.
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

              RT_DOMAIN(did)%QSTRMVOLRT_TS = RT_DOMAIN(did)%QSTRMVOLRT-RT_DOMAIN(did)%QSTRMVOLRT_DUM
              RT_DOMAIN(did)%LAKE_INFLORT_TS = RT_DOMAIN(did)%LAKE_INFLORT-RT_DOMAIN(did)%LAKE_INFLORT_DUM

			
#ifdef HYDRO_D
                call system_clock(count=clock_count_1, count_rate=clock_rate)
#endif
		! step 4) baseflow or groundwater physics
                if (nlst_rt(did)%GWBASESWCRT .gt. 0) then
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
	
		! step 6) aggregate specific fields from Hydro to LSM grid
               if (nlst_rt(did)%SUBRTSWCRT .NE.0  .or. nlst_rt(did)%OVRTSWCRT .NE. 0 ) THEN
		      call aggregateDomain(did)
               endif


           end if


!yw  if (nlst_rt(did)%sys_cpl .eq. 2) then
      ! advance to next time step
!          call HYDRO_time_adv(did)
      ! output for history 
!          call HYDRO_out(did)
!yw  endif
           call HYDRO_time_adv(did)
           call HYDRO_out(did)


!           write(90 + my_id,*) "finish calling hydro_exe"
!           call flush(90+my_id)
!          call mpp_land_sync()


            
           RT_DOMAIN(did)%SOLDRAIN = 0
           RT_DOMAIN(did)%QSUBRT = 0



      end subroutine HYDRO_exe

      
      
!----------------------------------------------------      
      subroutine driveGwBaseflow(did)
       
       implicit none
       integer, intent(in) :: did
       
       integer :: i, jj, ii

!------------------------------------------------------------------
!DJG Begin GW/Baseflow Routines
!-------------------------------------------------------------------

  IF (nlst_rt(did)%GWBASESWCRT.GE.1) THEN     ! Switch to activate/specify GW/Baseflow

!  IF (nlst_rt(did)%GWBASESWCRT.GE.1000) THEN     ! Switch to activate/specify GW/Baseflow

    If (nlst_rt(did)%GWBASESWCRT.EQ.1.OR.nlst_rt(did)%GWBASESWCRT.EQ.2) Then   ! Call simple bucket baseflow scheme

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
           write(6,*) "*****yw******start simp_gw_buck "
#else
          write(78,*) "*****yw******start simp_gw_buck " 
#endif
#endif

       if(nlst_rt(did)%UDMP_OPT .eq. 1) then
          call simp_gw_buck_nhd(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,RT_DOMAIN(did)%ixrt,RT_DOMAIN(did)%jxrt,  &
             RT_DOMAIN(did)%numbasns,nlst_rt(did)%AGGFACTRT, nlst_rt(did)%DT, RT_DOMAIN(did)%INFXSWGT,      &
             RT_DOMAIN(did)%INFXSRT, RT_DOMAIN(did)%SOLDRAIN, RT_DOMAIN(did)%dist(:,:,9),rt_domain(did)%dist_lsm(:,:,9), &
             RT_DOMAIN(did)%gw_buck_coeff, RT_DOMAIN(did)%gw_buck_exp, RT_DOMAIN(did)%z_max,     &
             RT_DOMAIN(did)%z_gwsubbas, RT_DOMAIN(did)%qout_gwsubbas,RT_DOMAIN(did)%qin_gwsubbas,    &
             nlst_rt(did)%GWBASESWCRT,nlst_rt(did)%OVRTSWCRT,          & 
#ifdef MPP_LAND
             RT_DOMAIN(did)%LNLINKSL &
#else
             RT_DOMAIN(did)%numbasns &
#endif
             , rt_domain(did)%basns_area, rt_domain(did)%nhdBuckMask &
             )
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
             RT_DOMAIN(did)%dist,nlst_rt(did)%DT,&
             RT_DOMAIN(did)%gw_buck_coeff,RT_DOMAIN(did)%gw_buck_exp, &
             RT_DOMAIN(did)%z_max,&
             nlst_rt(did)%GWBASESWCRT,nlst_rt(did)%OVRTSWCRT)
        endif

        if(nlst_rt(did)%GWBASESWCRT .gt. 0 .and. nlst_rt(did)%output_gw .eq. 2) then
           ! ouput of bucket information for NCAR GW option.
            call output_GW_Diag(did)
        endif

#ifdef HYDRO_D 
#ifndef NCEP_WCOSS
           write(6,*) "*****yw******end simp_gw_buck "
#else
          write(78,*) "*****yw******end simp_gw_buck " 
#endif
#endif

!!!For parameter setup runs output the percolation for each basin,
!!!otherwise comment out this output...
    else if (nlst_rt(did)%gwBaseSwCRT .eq. 3) then

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
           write(6,*) "*****bf******start 2d_gw_model "
#else
          write(78,*) "*****bf******start 2d_gw_model " 
#endif
#endif

	   ! 	   compute qsgwrt  between lsm and gw with namelist selected coupling method
	   ! 	   qsgwrt is defined on the routing grid  and needs to be aggregated for SFLX
	   if (nlst_rt(did)%gwsoilcpl .GT. 0) THEN
	     
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
          RT_DOMAIN(did)%SFCHEADSUBRT = RT_DOMAIN(did)%SFCHEADSUBRT &
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
      
    End if

  END IF    !DJG (End if for RTE SWC activation)
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

  IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT.EQ.2) THEN


    if(rt_domain(did)%restQSTRM) then
       RT_DOMAIN(did)%QSTRMVOLRT_TS = 0.000001
       rt_domain(did)%restQSTRM = .false.
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
       write(6,*) "***** set QSTRMVOLRT_TS = 0.000001 *********"
#else
       write(78,*) "***** set QSTRMVOLRT_TS = 0.000001 *********" 
#endif
       call flush(6)
#endif
    endif 
101  continue

 if(nlst_rt(did)%UDMP_OPT .eq. 1) then
     !!! for user defined Reach based Routing method.

    call drive_CHANNEL_RSL(nlst_rt(did)%UDMP_OPT,RT_DOMAIN(did)%timestep_flag, RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT,  &
       RT_DOMAIN(did)%LAKE_INFLORT_TS, RT_DOMAIN(did)%QSTRMVOLRT_TS, RT_DOMAIN(did)%TO_NODE, RT_DOMAIN(did)%FROM_NODE, &
       RT_DOMAIN(did)%TYPEL, RT_DOMAIN(did)%ORDER, RT_DOMAIN(did)%MAXORDER,   RT_DOMAIN(did)%CH_LNKRT, &
       RT_DOMAIN(did)%LAKE_MSKRT, nlst_rt(did)%DT, nlst_rt(did)%DTCT, nlst_rt(did)%DTRT_CH, &
       RT_DOMAIN(did)%MUSK, RT_DOMAIN(did)%MUSX, RT_DOMAIN(did)%QLINK, &
       RT_DOMAIN(did)%CHANLEN, RT_DOMAIN(did)%MannN, RT_DOMAIN(did)%So, RT_DOMAIN(did)%ChSSlp,RT_DOMAIN(did)%Bw, &
       RT_DOMAIN(did)%RESHT, RT_DOMAIN(did)%HRZAREA, RT_DOMAIN(did)%LAKEMAXH, RT_DOMAIN(did)%WEIRH, RT_DOMAIN(did)%WEIRC, &
       RT_DOMAIN(did)%WEIRL, RT_DOMAIN(did)%ORIFICEC, RT_DOMAIN(did)%ORIFICEA, &
       RT_DOMAIN(did)%ORIFICEE,  RT_DOMAIN(did)%CVOL, RT_DOMAIN(did)%QLAKEI, &
       RT_DOMAIN(did)%QLAKEO, RT_DOMAIN(did)%LAKENODE, &
       RT_DOMAIN(did)%QINFLOWBASE, RT_DOMAIN(did)%CHANXI, RT_DOMAIN(did)%CHANYJ, nlst_rt(did)%channel_option,  &
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
       , rt_domain(did)%accLndRunOff, rt_domain(did)%accQLateral, rt_domain(did)%accStrmvolrt, rt_domain(did)%accBucket &
       , rt_domain(did)%QLateral, rt_domain(did)%velocity &
       , rt_domain(did)%nlinksize, nlst_rt(did)%OVRTSWCRT, nlst_rt(did)%SUBRTSWCRT)
else
    call drive_CHANNEL(RT_DOMAIN(did)%latval,RT_DOMAIN(did)%lonval, &
       RT_DOMAIN(did)%timestep_flag,RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT, &
       nlst_rt(did)%SUBRTSWCRT, RT_DOMAIN(did)%QSUBRT, &
       RT_DOMAIN(did)%LAKE_INFLORT_TS, RT_DOMAIN(did)%QSTRMVOLRT_TS,&
       RT_DOMAIN(did)%TO_NODE, RT_DOMAIN(did)%FROM_NODE, RT_DOMAIN(did)%TYPEL,&
       RT_DOMAIN(did)%ORDER, RT_DOMAIN(did)%MAXORDER, RT_DOMAIN(did)%NLINKS,&
       RT_DOMAIN(did)%CH_NETLNK, RT_DOMAIN(did)%CH_NETRT,RT_DOMAIN(did)%CH_LNKRT,&
       RT_DOMAIN(did)%LAKE_MSKRT, nlst_rt(did)%DT, nlst_rt(did)%DTCT, nlst_rt(did)%DTRT_CH,&
       RT_DOMAIN(did)%MUSK, RT_DOMAIN(did)%MUSX,  RT_DOMAIN(did)%QLINK, &
       RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ELRT,RT_DOMAIN(did)%CHANLEN,&
       RT_DOMAIN(did)%MannN,RT_DOMAIN(did)%So, RT_DOMAIN(did)%ChSSlp, &
       RT_DOMAIN(did)%Bw,&
       RT_DOMAIN(did)%RESHT, RT_DOMAIN(did)%HRZAREA, RT_DOMAIN(did)%LAKEMAXH,&
       RT_DOMAIN(did)%WEIRH,    &
       RT_DOMAIN(did)%WEIRC, RT_DOMAIN(did)%WEIRL, RT_DOMAIN(did)%ORIFICEC, &
       RT_DOMAIN(did)%ORIFICEA, &
       RT_DOMAIN(did)%ORIFICEE, RT_DOMAIN(did)%ZELEV, RT_DOMAIN(did)%CVOL, &
       RT_DOMAIN(did)%NLAKES, RT_DOMAIN(did)%QLAKEI, RT_DOMAIN(did)%QLAKEO,&
       RT_DOMAIN(did)%LAKENODE, RT_DOMAIN(did)%dist, &
       RT_DOMAIN(did)%QINFLOWBASE, RT_DOMAIN(did)%CHANXI, &
       RT_DOMAIN(did)%CHANYJ, nlst_rt(did)%channel_option, &
       RT_DOMAIN(did)%RETDEP_CHAN, RT_DOMAIN(did)%NLINKSL, RT_DOMAIN(did)%LINKID, &
       RT_DOMAIN(did)%node_area  &
#ifdef MPP_LAND
       ,RT_DOMAIN(did)%lake_index,RT_DOMAIN(did)%link_location,&
       RT_DOMAIN(did)%mpp_nlinks,RT_DOMAIN(did)%nlinks_index, &
       RT_DOMAIN(did)%yw_mpp_nlinks  &
       , RT_DOMAIN(did)%LNLINKSL,RT_DOMAIN(did)%LLINKID &
       , rt_domain(did)%gtoNode,rt_domain(did)%toNodeInd,rt_domain(did)%nToInd  &
#endif
       , rt_domain(did)%CH_LNKRT_SL   &
       ,nlst_rt(did)%GwBaseSwCRT, gw2d(did)%ho, gw2d(did)%qgw_chanrt, &
       nlst_rt(did)%gwChanCondSw, nlst_rt(did)%gwChanCondConstIn, &
       nlst_rt(did)%gwChanCondConstOut &
       )
endif
    
    if((nlst_rt(did)%gwBaseSwCRT == 3) .and. (nlst_rt(did)%gwChanCondSw .eq. 1)) then
      
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
            suminfxsrt1 = suminfxsrt1 + RT_DOMAIN(did)%SFCHEADSUBRT(I,J) &
                               / float(RT_DOMAIN(did)%IXRT * RT_DOMAIN(did)%JXRT)
            do kk=1,nlst_rt(did)%NSOIL
                smcrttot1 = smcrttot1 + RT_DOMAIN(did)%SMCRT(I,J,KK)*RT_DOMAIN(did)%SLDPTH(KK)*1000. &
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
             do KRT=1,nlst_rt(did)%NSOIL
!                SMCAGGRT(KRT) = 0.
               RT_DOMAIN(did)%SH2OAGGRT(KRT) = 0.
             end do


             do AGGFACYRT=nlst_rt(did)%AGGFACTRT-1,0,-1
              do AGGFACXRT=nlst_rt(did)%AGGFACTRT-1,0,-1


                IXXRT=I*nlst_rt(did)%AGGFACTRT-AGGFACXRT
                JYYRT=J*nlst_rt(did)%AGGFACTRT-AGGFACYRT
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
                                            + RT_DOMAIN(did)%SFCHEADSUBRT(IXXRT,JYYRT)
!DJG Subgrid weighting edit...
                RT_DOMAIN(did)%LSMVOL = RT_DOMAIN(did)%LSMVOL &
                                      + RT_DOMAIN(did)%SFCHEADSUBRT(IXXRT,JYYRT) &
                                      * RT_DOMAIN(did)%dist(IXXRT,JYYRT,9)

                do KRT=1,nlst_rt(did)%NSOIL
!DJG               SMCAGGRT(KRT)=SMCAGGRT(KRT)+SMCRT(IXXRT,JYYRT,KRT)
                   RT_DOMAIN(did)%SH2OAGGRT(KRT) = RT_DOMAIN(did)%SH2OAGGRT(KRT) &
                                                 + RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT)
                end do

              end do
             end do



            RT_DOMAIN(did)%SFCHEADRT(I,J) = RT_DOMAIN(did)%SFCHEADAGGRT &
                                          / (nlst_rt(did)%AGGFACTRT**2)

            do KRT=1,nlst_rt(did)%NSOIL
!DJG              SMC(I,J,KRT)=SMCAGGRT(KRT)/(AGGFACTRT**2)
               RT_DOMAIN(did)%SH2OX(I,J,KRT) = RT_DOMAIN(did)%SH2OAGGRT(KRT) &
                                             / (nlst_rt(did)%AGGFACTRT**2)
            end do



!DJG Calculate subgrid weighting array...

              do AGGFACYRT=nlst_rt(did)%AGGFACTRT-1,0,-1
                do AGGFACXRT=nlst_rt(did)%AGGFACTRT-1,0,-1
                  IXXRT=I*nlst_rt(did)%AGGFACTRT-AGGFACXRT
                  JYYRT=J*nlst_rt(did)%AGGFACTRT-AGGFACYRT
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
                                          = RT_DOMAIN(did)%SFCHEADSUBRT(IXXRT,JYYRT) &
                                          * RT_DOMAIN(did)%dist(IXXRT,JYYRT,9) &
					  / RT_DOMAIN(did)%LSMVOL
                  else
                    RT_DOMAIN(did)%INFXSWGT(IXXRT,JYYRT) &
                                          = 1./FLOAT(nlst_rt(did)%AGGFACTRT**2)
                  end if

                  do KRT=1,nlst_rt(did)%NSOIL

!!!yw added for debug
                   if(RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT) .lt. 0) then
#ifndef NCEP_WCOSS
                      print*, "Error negative SMCRT", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                      write(78,*) "WARNING: negative SMCRT", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
                   endif
                   if(RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) .lt. 0) then
#ifndef NCEP_WCOSS
                      print *, "Error negative SH2OWGT", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                      write(78,*) "WARNING: negative SH2OWGT", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
                   endif

!end 
                    IF (RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT) .GT. &
                        RT_DOMAIN(did)%SMCMAXRT(IXXRT,JYYRT,KRT)) THEN

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
                      print *, "SMCMAX exceeded upon aggregation...", &
                           RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),  &
                           RT_DOMAIN(did)%SMCMAXRT(IXXRT,JYYRT,KRT)
#else
                     write(78,*) "FATAL ERROR: SMCMAX exceeded upon aggregation...", &
                           RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),  &
                           RT_DOMAIN(did)%SMCMAXRT(IXXRT,JYYRT,KRT) 
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
                      print *, "Error negative SH2OX", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#else
                     write(78,*) "Erroneous value of SH2O...", &
                                RT_DOMAIN(did)%SH2OX(I,J,KRT),I,J,KRT
                      write(78,*) "FATAL ERROR: negative SH2OX", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
#endif
#endif
                      call hydro_stop("In module_HYDRO_drv.F aggregateDomain() "// &
                                      "- Error negative SH2OX")
                    END IF

		    IF ( RT_DOMAIN(did)%SH2OX(I,J,KRT) .gt. 0 ) THEN
                    	RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT) &
                                 = RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT) &
                                 / RT_DOMAIN(did)%SH2OX(I,J,KRT)
                    ELSE
#ifdef HYDRO_D
                         print *, "Error zero SH2OX", RT_DOMAIN(did)%SH2OWGT(IXXRT,JYYRT,KRT), RT_DOMAIN(did)%SMCRT(IXXRT,JYYRT,KRT),RT_DOMAIN(did)%SH2OX(I,J,KRT)
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
	
        do i = 1, nlst_rt(did)%NSOIL
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
            suminfxs2 = suminfxs2 + RT_DOMAIN(did)%SFCHEADRT(I,J) &
                               / float(RT_DOMAIN(did)%IX * RT_DOMAIN(did)%JX)
            do kk=1,nlst_rt(did)%NSOIL
                smctot2 = smctot2 + RT_DOMAIN(did)%SMC(I,J,KK)*RT_DOMAIN(did)%SLDPTH(KK)*1000. &
                               / float(RT_DOMAIN(did)%IX * RT_DOMAIN(did)%JX)
               sicetot2 = sicetot2 + RT_DOMAIN(did)%SICE(I,J,KK)*RT_DOMAIN(did)%SLDPTH(KK)*1000. &
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
      

      subroutine HYDRO_ini(ntime, did,ix0,jx0, vegtyp,soltyp)
        implicit none
        integer ntime, did
        integer rst_out, ix,jx
!        integer, OPTIONAL:: ix0,jx0
        integer:: ix0,jx0
        integer, dimension(ix0,jx0),OPTIONAL :: vegtyp, soltyp


#ifdef MPP_LAND
    call  MPP_LAND_INIT()
#endif


! read the namelist
! the lsm namelist will be read by rtland sequentially again.
     call read_rt_nlst(nlst_rt(did) )

     if(nlst_rt(did)%rtFlag .eq. 0) return

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! get the dimension 
     call get_file_dimension(trim(nlst_rt(did)%geo_static_flnm), ix,jx)
   
       
#ifdef MPP_LAND

  if (nlst_rt(did)%sys_cpl .eq. 1 .or. nlst_rt(did)%sys_cpl .eq. 4) then
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
              nlst_rt(did)%AGGFACTRT)
   else  
!  coupled with WRF, LIS
         numprocs = node_info(1,1)

         call wrf_LAND_set_INIT(node_info,numprocs,nlst_rt(did)%AGGFACTRT)


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
      rt_domain(did)%ixrt = ix*nlst_rt(did)%AGGFACTRT 
      rt_domain(did)%jxrt = jx*nlst_rt(did)%AGGFACTRT
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

      if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
          call gw2d_allocate(did,&
                             rt_domain(did)%ixrt,&
                             rt_domain(did)%jxrt,&
                             nlst_rt(did)%nsoil)
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
      if(present(vegtyp)) then
           call lsm_input(did,ix0=ix0,jx0=jx0,vegtyp0=vegtyp,soltyp0=soltyp)
      else
           call lsm_input(did,ix0=ix0,jx0=jx0)
      endif


#ifdef HYDRO_D
#ifndef NCEP_WCOSS
       write(6,*) "finish decomposion"
#else
       write(78,*) "finish decomposion"
#endif
#endif


     call get_dist_lsm(did) 
     call get_dist_lrt(did)


! rt model initilization
      call LandRT_ini(did)

       
      if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
	
          call gw2d_ini(did,&
                        nlst_rt(did)%dt,&
                        nlst_rt(did)%dxrt0)
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

     IF (nlst_rt(did)%TERADJ_SOLAR.EQ.1 .and. nlst_rt(did)%CHANRTSWCRT.NE.2) THEN   ! Perform ter rain adjustment of incoming solar
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


     IF (nlst_rt(did)%GWBASESWCRT .gt. 0) then
        if(nlst_rt(did)%UDMP_OPT .eq. 1) then
            call get_basn_area_nhd(rt_domain(did)%basns_area)
        else
            call get_basn_area(did)
        endif
     endif

     IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2 ) then
        call get_node_area(did)
     endif
     

#ifdef WRF_HYDRO_NUDGING
     if(nlst_rt(did)%CHANRTSWCRT .ne. 0) call init_stream_nudging
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

       call HYDRO_out(did)

      end subroutine HYDRO_ini

      subroutine lsm_input(did,ix0,jx0,vegtyp0,soltyp0)
         implicit none
         integer did, leng
         parameter(leng=100)
         integer :: i,j, nn
         integer, allocatable, dimension(:,:) :: soltyp
         real, dimension(leng) :: xdum1, MAXSMC,refsmc,wltsmc

        integer :: ix0,jx0
        integer, dimension(ix0,jx0),OPTIONAL :: vegtyp0, soltyp0

#ifdef HYDRO_D
#ifndef NCEP_WCOSS
         write(6,*) RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx
#else
         write(78,*) RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx
#endif
#endif

         allocate(soltyp(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx) )

         soltyp = 0
         call get2d_lsm_soltyp(soltyp,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))


         call get2d_lsm_real("HGT",RT_DOMAIN(did)%TERRAIN,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))

         call get2d_lsm_real("XLAT",RT_DOMAIN(did)%lat_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))
         call get2d_lsm_real("XLONG",RT_DOMAIN(did)%lon_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))
         call get2d_lsm_vegtyp(RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))



            if(nlst_rt(did)%sys_cpl .eq. 2 ) then
              ! coupling with WRF
                if(present(soltyp0) ) then
                   where(soltyp0 == 14) VEGTYP0 = 16
                   where(VEGTYP0 == 16 ) soltyp0 = 14
                   soltyp = soltyp0
                   RT_DOMAIN(did)%VEGTYP = VEGTYP0
                endif
            endif

         where(soltyp == 14) RT_DOMAIN(did)%VEGTYP = 16
         where(RT_DOMAIN(did)%VEGTYP == 16 ) soltyp = 14

! LKSAT, 
! temporary set
       RT_DOMAIN(did)%SMCRTCHK = 0
       RT_DOMAIN(did)%SMCAGGRT = 0
       RT_DOMAIN(did)%STCAGGRT = 0
       RT_DOMAIN(did)%SH2OAGGRT = 0
     

       RT_DOMAIN(did)%zsoil(1:nlst_rt(did)%nsoil) = nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil)

       RT_DOMAIN(did)%sldpth(1) = abs( RT_DOMAIN(did)%zsoil(1) )
       do i = 2, nlst_rt(did)%nsoil
          RT_DOMAIN(did)%sldpth(i) = RT_DOMAIN(did)%zsoil(i-1)-RT_DOMAIN(did)%zsoil(i)
       enddo
       RT_DOMAIN(did)%SOLDEPRT = -1.0*RT_DOMAIN(did)%ZSOIL(nlst_rt(did)%NSOIL)

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


       rt_domain(did)%soiltyp = soltyp
       
       if(allocated(soltyp)) deallocate(soltyp)


      end subroutine lsm_input


end module module_HYDRO_drv

! stop the job due to the fatal error.
      subroutine HYDRO_stop(msg)
#ifdef MPP_LAND
        use module_mpp_land
#endif
        character(len=*) :: msg
        integer :: ierr
        ierr = 1
#ifndef NCEP_WCOSS
!#ifdef HYDRO_D  !! PLEASE NEVER UNCOMMENT THIS IFDEF, it's just one incredibly useful string.
      write(6,*) "The job is stopped due to the fatal error. ", trim(msg)
      call flush(6)
!#endif
#else
     write(78,*) "FATAL ERROR: ", trim(msg)
      call flush(78)
      close(78)
#endif
#ifdef MPP_LAND
#ifndef HYDRO_D
      print*, "---"
      print*, "FATAL ERROR! Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
      print*, ""
#endif

!        call mpp_land_sync()
!        write(my_id+90,*) msg
!        call flush(my_id+90)

         call mpp_land_abort()
         call MPI_finalize(ierr)
#else
         stop "FATAL ERROR: Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
#endif

     return
     end  subroutine HYDRO_stop  


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
