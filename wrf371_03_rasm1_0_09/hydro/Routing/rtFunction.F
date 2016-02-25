      subroutine exeRouting (did)
         use module_RT_data, only: rt_domain
         use module_GW_baseflow_data, only: gw2d
         use module_GW_baseflow, only: simp_gw_buck, gwstep
         use module_channel_routing, only: drive_channel
         use module_namelist, only: nlst_rt

#ifdef MPP_LAND  
         use module_mpp_land 
#endif

       
         implicit none
         integer did, i
         real, dimension(RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT):: &
                   QSTRMVOLRT_DUM,LAKE_INFLORT_DUM, &
                   QSTRMVOLRT_TS, LAKE_INFLORT_TS

         real :: dx
         integer ii,jj,kk


           IF (nlst_rt(did)%SUBRTSWCRT.EQ.1 .or. nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) THEN

              QSTRMVOLRT_DUM = RT_DOMAIN(did)%QSTRMVOLRT
              LAKE_INFLORT_DUM = RT_DOMAIN(did)%LAKE_INFLORT

#ifdef HYDRO_D
               write(6,*) "*****yw******start drive_RT "
#endif



!          write(6,*) "yyww RT_DOMAIN(did)%SH2OX(15,1,7)= ", RT_DOMAIN(did)%SH2OX(15,1,7)

         call drive_RT( RT_DOMAIN(did)%IX,RT_DOMAIN(did)%JX,nlst_rt(did)%NSOIL,&
             RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT,  &
             RT_DOMAIN(did)%SMC,RT_DOMAIN(did)%STC,RT_DOMAIN(did)%SH2OX, &
             RT_DOMAIN(did)%INFXSRT,RT_DOMAIN(did)%SFCHEADRT,RT_DOMAIN(did)%SMCMAX1,&
             RT_DOMAIN(did)%SMCREF1,RT_DOMAIN(did)%LKSAT,  &
             RT_DOMAIN(did)%SMCWLT1, RT_DOMAIN(did)%SMCRTCHK,RT_DOMAIN(did)%DSMC,&
             RT_DOMAIN(did)%ZSOIL, RT_DOMAIN(did)%SMCAGGRT,&
             RT_DOMAIN(did)%STCAGGRT,RT_DOMAIN(did)%SH2OAGGRT, &
             RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%SOLDEPRT,&
             RT_DOMAIN(did)%INFXSAGGRT,RT_DOMAIN(did)%DHRT,RT_DOMAIN(did)%QSTRMVOLRT, &
             RT_DOMAIN(did)%QBDRYRT,RT_DOMAIN(did)%LAKE_INFLORT,&
             RT_DOMAIN(did)%SFCHEADSUBRT,RT_DOMAIN(did)%INFXSWGT,&
             RT_DOMAIN(did)%LKSATRT, &
             RT_DOMAIN(did)%INFXSUBRT,RT_DOMAIN(did)%OVROUGHRT,&
             RT_DOMAIN(did)%QSUBRT,RT_DOMAIN(did)%ZWATTABLRT, &
             RT_DOMAIN(did)%QSUBBDRYRT,   &
             RT_DOMAIN(did)%RETDEPRT,RT_DOMAIN(did)%SOXRT,RT_DOMAIN(did)%SOYRT,&
             RT_DOMAIN(did)%SUB_RESID,RT_DOMAIN(did)%SMCRT,&
             RT_DOMAIN(did)%SMCMAXRT,RT_DOMAIN(did)%SMCWLTRT, &
             RT_DOMAIN(did)%SH2OWGT,RT_DOMAIN(did)%LAKE_MSKRT,&
             RT_DOMAIN(did)%CH_NETRT, RT_DOMAIN(did)%dist, &
             RT_DOMAIN(did)%LSMVOL,RT_DOMAIN(did)%DSMCTOT, &
             RT_DOMAIN(did)%SMCTOT1,&
             RT_DOMAIN(did)%SMCTOT2,RT_DOMAIN(did)%suminfxs1, &
             RT_DOMAIN(did)%suminfxsrt,RT_DOMAIN(did)%SO8RT, &
             RT_DOMAIN(did)%SO8RT_D,nlst_rt(did)%AGGFACTRT,  &
             nlst_rt(did)%SUBRTSWCRT,nlst_rt(did)%OVRTSWCRT, &
             RT_DOMAIN(did)%LAKE_CT, RT_DOMAIN(did)%STRM_CT,    &
             nlst_rt(did)%RT_OPTION,RT_DOMAIN(did)%OV_ROUGH, &
             RT_DOMAIN(did)%INFXSAGG1RT,RT_DOMAIN(did)%SFCHEADAGG1RT,&
             RT_DOMAIN(did)%SFCHEADAGGRT,&
             nlst_rt(did)%DTRT, &
             nlst_rt(did)%DT,RT_DOMAIN(did)%LAKE_INFLOTRT,&
             RT_DOMAIN(did)%QBDRYTRT,RT_DOMAIN(did)%QSUBBDRYTRT,&
             RT_DOMAIN(did)%QSTRMVOLTRT,RT_DOMAIN(did)%q_sfcflx_x,&
             RT_DOMAIN(did)%q_sfcflx_y,RT_DOMAIN(did)%LKSATFAC,&
             RT_DOMAIN(did)%OVROUGHRTFAC,rt_domain(did)%dist_lsm(:,:,9) )

            QSTRMVOLRT_TS = RT_DOMAIN(did)%QSTRMVOLRT-QSTRMVOLRT_DUM
            LAKE_INFLORT_TS = RT_DOMAIN(did)%LAKE_INFLORT-LAKE_INFLORT_DUM

#ifdef HYDRO_D
           write(6,*) "*****yw******end drive_RT "
#endif
        end if



!------------------------------------------------------------------
!DJG Begin GW/Baseflow Routines
!-------------------------------------------------------------------

  IF (nlst_rt(did)%GWBASESWCRT.GE.1) THEN     ! Switch to activate/specify GW/Baseflow

!  IF (nlst_rt(did)%GWBASESWCRT.GE.1000) THEN     ! Switch to activate/specify GW/Baseflow

    If (nlst_rt(did)%GWBASESWCRT.EQ.1.OR.nlst_rt(did)%GWBASESWCRT.EQ.2) Then   ! Call simple bucket baseflow scheme

#ifdef HYDRO_D
           write(6,*) "*****yw******start simp_gw_buck "
#endif

       call simp_gw_buck(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,RT_DOMAIN(did)%ixrt,&
             RT_DOMAIN(did)%jxrt,RT_DOMAIN(did)%numbasns,RT_DOMAIN(did)%basns_area,&
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


#ifdef MPP_LAND
      if(my_id .eq. IO_id) then
#endif

       open (unit=51,file='GW_inflow.txt',form='formatted',&
             status='unknown',position='append')
       open (unit=52,file='GW_outflow.txt',form='formatted',&
             status='unknown',position='append')
       open (unit=53,file='GW_zlev.txt',form='formatted',&
             status='unknown',position='append')
       do i=1,RT_DOMAIN(did)%numbasns
          write (51,951) i,nlst_rt(did)%olddate,rt_domain(did)%qin_gwsubbas(i)
951        FORMAT(I3,1X,A19,1X,F11.3)
          write (52,951) i,nlst_rt(did)%olddate,rt_domain(did)%qout_gwsubbas(i)
          write (53,951) i,nlst_rt(did)%olddate,rt_domain(did)%z_gwsubbas(i)
       end do
       close(51)
       close(52)
       close(53)
#ifdef MPP_LAND
     endif
#endif

#ifdef HYDRO_D 
           write(6,*) "*****yw******end simp_gw_buck "
#endif

!!!For parameter setup runs output the percolation for each basin,
!!!otherwise comment out this output...
    else if (nlst_rt(did)%GWBASESWCRT .eq. 3) then

#ifdef HYDRO_D
           write(6,*) "*****bf******start 2d_gw_model "
#endif

           DX = abs(nlst_rt(did)%DXRT0 * nlst_rt(did)%AGGFACTRT)
           
           call gwstep(gw2d(did)%ix, gw2d(did)%jx, gw2d(did)%dx, &
			gw2d(did)%ltype, gw2d(did)%elev, gw2d(did)%bot, &
			gw2d(did)%hycond, gw2d(did)%poros, gw2d(did)%compres, &
			gw2d(did)%ho, gw2d(did)%h, gw2d(did)%convgw, &
			gw2d(did)%ebot, gw2d(did)%eocn, gw2d(did)%dt, &
			gw2d(did)%istep)
           
           
! bftodo head postprocessing block 
! GW-SOIL-CHANNEL interaction section
	  gw2d(did)%ho = gw2d(did)%h

#ifdef HYDRO_D
           write(6,*) "*****bf******end 2d_gw_model "
#endif
      
    End if

  END IF    !DJG (End if for RTE SWC activation)
!------------------------------------------------------------------
!DJG End GW/Baseflow Routines
!-------------------------------------------------------------------

!-------------------------------------------------------------------
!-------------------------------------------------------------------
!DJG,DNY  Begin Channel and Lake Routing Routines
!-------------------------------------------------------------------
  IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT.EQ.2) THEN
 
    call drive_CHANNEL(RT_DOMAIN(did)%timestep_flag,RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT, &
       nlst_rt(did)%SUBRTSWCRT, RT_DOMAIN(did)%QSUBRT, &
       LAKE_INFLORT_TS, QSTRMVOLRT_TS,&
       RT_DOMAIN(did)%TO_NODE, RT_DOMAIN(did)%FROM_NODE, RT_DOMAIN(did)%TYPEL,&
       RT_DOMAIN(did)%ORDER, RT_DOMAIN(did)%MAXORDER, RT_DOMAIN(did)%NLINKS,&
       RT_DOMAIN(did)%CH_NETLNK, RT_DOMAIN(did)%CH_NETRT, &
       RT_DOMAIN(did)%LAKE_MSKRT, nlst_rt(did)%DT, nlst_rt(did)%DTRT, &
       RT_DOMAIN(did)%MUSK, RT_DOMAIN(did)%MUSX,  RT_DOMAIN(did)%QLINK, &
       RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ELRT,RT_DOMAIN(did)%CHANLEN,&
       RT_DOMAIN(did)%MannN,RT_DOMAIN(did)%So, RT_DOMAIN(did)%ChSSlp, &
       RT_DOMAIN(did)%Bw,&
       RT_DOMAIN(did)%RESHT, RT_DOMAIN(did)%HRZAREA, RT_DOMAIN(did)%LAKEMAXH,&
       RT_DOMAIN(did)%WEIRC, RT_DOMAIN(did)%WEIRL, RT_DOMAIN(did)%ORIFICEC, &
       RT_DOMAIN(did)%ORIFICEA, &
       RT_DOMAIN(did)%ORIFICEE, RT_DOMAIN(did)%ZELEV, RT_DOMAIN(did)%CVOL, &
       RT_DOMAIN(did)%NLAKES, RT_DOMAIN(did)%QLAKEI, RT_DOMAIN(did)%QLAKEO,&
       RT_DOMAIN(did)%LAKENODE, RT_DOMAIN(did)%dist, &
       RT_DOMAIN(did)%QINFLOWBASE, RT_DOMAIN(did)%CHANXI, &
       RT_DOMAIN(did)%CHANYJ, nlst_rt(did)%channel_option, &
       RT_DOMAIN(did)%RETDEP_CHAN &
       , RT_DOMAIN(did)%node_area &
#ifdef MPP_LAND
       ,RT_DOMAIN(did)%lake_index,RT_DOMAIN(did)%link_location,&
       RT_DOMAIN(did)%mpp_nlinks,RT_DOMAIN(did)%nlinks_index, &
       RT_DOMAIN(did)%yw_mpp_nlinks  &
#endif
       )
  endif

#ifdef HYDRO_D
           write(6,*) "*****yw******end drive_CHANNEL "
#endif

      end subroutine  exeRouting

      subroutine time_seconds(i3)
          integer time_array(8)
          real*8 i3
          call date_and_time(values=time_array)
          i3 = time_array(4)*24*3600+time_array(5) * 3600 + time_array(6) * 60 + &
                time_array(7) + 0.001 * time_array(8)
          return
      end subroutine time_seconds


