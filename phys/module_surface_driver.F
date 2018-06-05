!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_surface_driver
CONTAINS

   SUBROUTINE surface_driver(                                         &
     &           HYDRO_dt,sfcheadrt,INFXSRT,soldrain,                 &
     &           acgrdflx,achfx,aclhf                                 &
     &          ,acsnom,acsnow,snowfallac,akhs,akms,albedo,br,canwat  &
     &          ,chklowq,dt,dx,dz8w,dzs,glw                           &
     &          ,grdflx,gsw,swdown,gz1oz0,hfx,ht,ifsnow,isfflx        &
     &          ,fractional_seaice,seaice_albedo_opt                  &
     &          ,seaice_albedo_default,seaice_thickness_opt,          &
     &          seaice_thickness_default                              &
     &          ,seaice_snowdepth_opt,seaice_snowdepth_max            &
     &          ,seaice_snowdepth_min,tice2tsk_if2cold                &
     &          ,ifndalbsi, ifndicedepth, ifndsnowsi                  &
     &          ,isltyp,itimestep,julian_in,ivgtyp,lowlyr,mavail,rmol &
     &          ,num_soil_layers,p8w,pblh,pi_phy,pshltr,fm,fhh,psih   &
#if (NMM_CORE==1)
     &          ,psim,p_phy,q10,q2,qfx,taux,tauy,qsfc,qshltr,qz0      &
     &          ,zkmax,ribn,charn,msang,scurx,scury,icoef_sf,iwavecpl,lcurr_sf & !for gfdl-sf drag
     &          ,pert_Cd, ens_random_seed, ens_Cdamp                  &
     &          ,cd_out,ch_out                                        &
#else
     &          ,psim,p_phy,q10,q2,qfx,qsfc,qshltr,qz0                &
#endif
     &          ,raincv,rho,sfcevp,sfcexc,sfcrunoff ,acrunoff         &
     &          ,smois,smstav,smstot,snoalb,snow,snowc,snowh,stepbl   &
     &          ,smcrel                                               &
     &          ,th10,th2,thz0,th_phy,tmn,tshltr,tsk,tslb             &
     &          ,tyr,tyra,tdly,tlag,lagday,nyear,nday,tmn_update,yr   &
     &          ,t_phy,u10,udrunoff,ust,uz0                           &
     &          ,u_frame,u_phy,v10,vegfra,uoce,voce                   &
     &          ,vz0,v_frame,v_phy,warm_rain,wspd,xice,xland,z,znt    &
     &          ,max_edom,cplmask                                     &
#if (HWRF==1)
     &          ,mznt                                                 &
#endif
     &          ,zs                                                   & 
     &          ,albsi, icedepth,snowsi                               &
#if (NMM_CORE==1)
     &          ,xicem,isice,iswater,ct,tke_pbl,sfenth                &
#else
     &          ,xicem,isice,iswater,ct,tke_pbl                       &
#endif
     &          ,albbck,embck,lh,sh2o,shdmax,shdmin,z0                &
     &          ,flqc,flhc,psfc,sst,sst_input,sstsk,dtw,sst_update,sst_skin     &
     &          ,scm_force_skintemp,scm_force_flux,t2,emiss           &
     &          ,sf_sfclay_physics,sf_surface_physics,ra_lw_physics   & 
     &          ,mosaic_lu,mosaic_soil                                &
     &          ,landusef,soilctop,soilcbot,ra,rs,nlcat,nscat,vegf_px & ! PX-LSM
     &          ,snowncv, anal_interval, lai, imperv, canfra          & ! PX-LSM
     &          ,pxlsm_smois_init, pxlsm_soil_nudge                   & ! PX-LSM
     &          ,idveg    ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz     &
     &          ,iopt_inf ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot ,iopt_stc     &
     &          ,iopt_gla ,iopt_rsf  ,iopt_soil ,iopt_pedo ,iopt_crop               &
     &          ,soilcomp  , soilcl1, soilcl2, soilcl3, soilcl4                     &
     &          ,isnowxy  ,tvxy      ,tgxy      ,canicexy  ,canliqxy  ,eahxy        &
     &          ,tahxy    ,cmxy      ,chxy      ,fwetxy    ,sneqvoxy  ,alboldxy     &
     &          ,qsnowxy  ,wslakexy  ,zwtxy     ,waxy      ,wtxy      ,tsnoxy       &
     &          ,zsnsoxy  ,snicexy   ,snliqxy   ,lfmassxy  ,rtmassxy  ,stmassxy     &
     &          ,woodxy   ,stblcpxy  ,fastcpxy  ,xsaixy    ,taussxy                 &
     &          ,grainxy  ,gddxy     ,cropcat   ,pgsxy                              &
     &          ,planting ,harvest   ,season_gdd                                    &
     &          ,t2mvxy   ,t2mbxy    ,q2mvxy    ,q2mbxy                             &
     &          ,tradxy   ,neexy     ,gppxy     ,nppxy     ,fvegxy    ,runsfxy      &
     &          ,runsbxy  ,ecanxy    ,edirxy    ,etranxy   ,fsaxy     ,firaxy       &
     &          ,aparxy   ,psnxy     ,savxy     ,sagxy     ,rssunxy   ,rsshaxy      &
     &          ,bgapxy   ,wgapxy    ,tgvxy     ,tgbxy     ,chvxy     ,chbxy        &    
     &          ,shgxy    ,shcxy     ,shbxy     ,evgxy     ,evbxy     ,ghvxy        &
     &          ,ghbxy    ,irgxy     ,ircxy     ,irbxy     ,trxy      ,evcxy        &
     &          ,chleafxy ,chucxy    ,chv2xy    ,chb2xy    ,chstarxy                &                      
           ! Noah-MP ground water
     &          ,smcwtdxy ,rechxy   ,deeprechxy,fdepthxy,areaxy   ,rivercondxy, riverbedxy &
     &          ,eqzwt    ,pexpxy   ,qrfxy    ,qspringxy,qslatxy  ,qrfsxy   ,qspringsxy &
     &          ,smoiseq  ,wtddt    ,stepwtd                                            &
                ,gecros_state                                   & ! Optional gecros crop
     &          ,opt_thcnd                                                 &
           ! Noah UA changes
     &          ,ua_phys,flx4,fvb,fbur,fgsn                                  &
#if (EM_CORE==1)
     &          ,ch,tsq,qsq,cov,Sh3d,el_pbl,bl_mynn_cloudpdf          & ! MYNN
     &          ,icloud_bl,qc_bl,cldfra_bl                            & ! MYNN
     &          ,fgdp,dfgdp,vdfg,grav_settling                        & ! Katata - fog dep
#endif
     &          ,lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d       & !lake
     &          ,snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d & !lake
     &          ,z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d   & !lake
     &          ,h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d           & !lake
     &          ,zi3d,         watsat3d,     csol3d,     tkmg3d         & !lake
     &          ,tkdry3d,      tksatu3d,     LakeModel,  lake_min_elev     & !lake
#if (EM_CORE==1)
 !    &          ,lakemask,  lakeflag                                  & !lake
     &          ,lakemask                                  & !lake
#endif
            !  cyl ocean variable
                ,OM_TMP,OM_S,OM_U,OM_V,OM_DEPTH,OM_ML,OM_LON          &
     &          ,OM_LAT,okms,okme,rdx,rdy,msfu,msfv,msft              &
     &          ,XTIME,OM_TINI,OM_SINI,id,omdt                        &
! CLM variables
     &          ,numc,nump,sabv,sabg,lwup,snl,history_interval &
     &          ,snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
     &           h2ocan,h2ocan_col,t2m_max,t2m_min,t2clm ,         &
     &           t_ref2m,h2osoi_liq_s1,                 &
     &           h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
     &           h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
     &           h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
     &           h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
     &           h2osoi_ice_s1,h2osoi_ice_s2,                        &
     &           h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
     &           h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
     &           h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
     &           h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
     &           t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
     &           t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
     &           t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
     &           t_soisno8,t_soisno9,t_soisno10,                     &
     &           dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
     &           snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
     &           t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
     &           t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
     &           h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
     &           h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
     &           h2osoi_vol7,h2osoi_vol8,                            &
     &           h2osoi_vol9,h2osoi_vol10,                           &
     &           maxpatch,inest,                                     &
     &           ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
     &           Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
     &           SWUPsubgrid,LHsoi,LHveg,LHtran                      &
#ifdef CN
!ADD_NEW_VAR for crop and cn
     &           ,dyntlai,dyntsai,dyntop,dynbot                              &
     &           ,htmx,croplive,gdd1020,gdd820,gdd020,grainc,grainc_storage  &
     &           ,grainc_xfer,grainn,grainn_storage,grainn_xfer,days_active  &
     &           ,onset_flag,onset_counter,onset_gddflag,onset_fdd,onset_gdd &
     &           ,onset_swi,offset_flag,offset_counter,offset_fdd,offset_swi &
     &           ,dayl,annavg_t2m,tempavg_t2m,tempsum_potential_gpp          &
     &           ,annsum_potential_gpp,tempmax_retransn,annmax_retransn      &
     &           ,prev_leafc_to_litter,prev_frootc_to_litter,tempsum_npp     &
     &           ,annsum_npp,leafc,leafc_storage,leafc_xfer,frootc           &
     &           ,frootc_storage,frootc_xfer,livestemc,livestemc_storage     &
     &           ,livestemc_xfer,deadstemc,deadstemc_storage,deadstemc_xfer  &
     &           ,livecrootc,livecrootc_storage,livecrootc_xfer,deadcrootc   &
     &           ,deadcrootc_storage,deadcrootc_xfer,cpool,pft_ctrunc        &
     &           ,leafn,leafn_storage,leafn_xfer,frootn,frootn_storage       &
     &           ,frootn_xfer,livestemn,livestemn_storage,livestemn_xfer     &
     &           ,deadstemn,deadstemn_storage,deadstemn_xfer,livecrootn      &
     &           ,livecrootn_storage,livecrootn_xfer,deadcrootn              &
     &           ,deadcrootn_storage,deadcrootn_xfer,npool,pft_ntrunc        &
     &           ,gresp_storage,gresp_xfer,xsmrpool,annsum_counter           &
     &           ,cannsum_npp,cannavg_t2m,wf,me,mean_fire_prob,cwdc,litr1c   &
     &           ,litr2c,litr3c,soil1c,soil2c,soil3c,soil4c,seedc,col_ctrunc &
     &           ,prod10c,prod100c,cwdn,litr1n,litr2n,litr3n,soil1n,soil2n   &
     &           ,soil3n,soil4n,seedn,col_ntrunc,prod10n,prod100n,sminn      &
     &           ,totlitc,dwt_seedc_to_leaf,dwt_seedc_to_deadstem,dwt_conv_cflux &
     &           ,dwt_prod10c_gain,dwt_prod100c_gain,prod100c_loss,dwt_frootc_to_litr1c &
     &           ,dwt_frootc_to_litr2c,dwt_frootc_to_litr3c,dwt_livecrootc_to_cwdc &
     &           ,dwt_deadcrootc_to_cwdc,dwt_seedn_to_leaf,dwt_seedn_to_deadstem &
     &           ,dwt_conv_nflux,dwt_prod10n_gain,dwt_prod100n_gain,prod100n_loss &
     &           ,dwt_frootn_to_litr1n,dwt_frootn_to_litr2n, dwt_frootn_to_litr3n &
     &           ,dwt_livecrootn_to_cwdn,dwt_deadcrootn_to_cwdn,retransn &
#endif
            !  Optional urban
     &          ,slope_rad,topo_shading,shadowmask                    & !I solar
     &          ,swnorm,slope,slp_azi,diffuse_frac                    & !I solar
     &          ,declin,solcon,coszen,hrang,xlat_urb2d                & !I solar/urban
     &          ,num_roof_layers, num_wall_layers                     & !I urban
     &          ,num_road_layers, dzr, dzb, dzg                       & !I urban
     &          ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d         & !H urban
     &          ,uc_urb2d                                             & !H urban
     &          ,xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d          & !H urban
     &          ,cmcr_urb2d,tgr_urb2d,tgrl_urb3d,smr_urb3d            & !H urban
     &          ,julian,julyr,drelr_urb2d,drelb_urb2d,drelg_urb2d     & !H urban
     &          ,flxhumr_urb2d,flxhumb_urb2d,flxhumg_urb2d            & !H urban
     &          ,trl_urb3d,tbl_urb3d,tgl_urb3d                        & !H urban
     &          ,sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d          & !H urban
     &          ,frc_urb2d, utype_urb2d                               & !H urban
     &          ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif          &
     &          ,cmgr_sfcdif,chgr_sfcdif                              &
!-----SSiB LSM (fds 06/2010)---------------------------------------------------
     &          ,alswvisdir, alswvisdif, alswnirdir, alswnirdif       & ! ssib
     &          ,swvisdir, swvisdif, swnirdir, swnirdif               & ! ssib
     &          ,ssib_br  ,ssib_fm  ,ssib_fh  ,ssib_cm  ,ssibxdd      & ! ssib
     &          ,ssib_lhf ,ssib_shf ,ssib_ghf ,ssib_egs ,ssib_eci     & ! ssib
     &          ,ssib_ect ,ssib_egi ,ssib_egt ,ssib_sdn ,ssib_sup     & ! ssib
     &          ,ssib_ldn ,ssib_lup ,ssib_wat ,ssib_shc ,ssib_shg     & ! ssib
     &          ,ssib_lai ,ssib_vcf ,ssib_z00 ,ssib_veg               & ! ssib
     &          ,ISNOW ,SWE ,SNOWDEN ,SNOWDEPTH ,TKAIR                & ! ssib-snow
     &          ,DZO1 ,WO1 ,TSSN1 ,TSSNO1 ,BWO1 ,BTO1                 & ! ssib-snow
     &          ,CTO1 ,FIO1 ,FLO1 ,BIO1 ,BLO1 ,HO1                    & ! ssib-snow
     &          ,DZO2 ,WO2 ,TSSN2 ,TSSNO2 ,BWO2 ,BTO2                 & ! ssib-snow
     &          ,CTO2 ,FIO2 ,FLO2 ,BIO2 ,BLO2 ,HO2                    & ! ssib-snow
     &          ,DZO3 ,WO3 ,TSSN3 ,TSSNO3 ,BWO3 ,BTO3                 & ! ssib-snow
     &          ,CTO3 ,FIO3 ,FLO3 ,BIO3 ,BLO3 ,HO3                    & ! ssib-snow
     &          ,DZO4 ,WO4 ,TSSN4 ,TSSNO4 ,BWO4 ,BTO4                 & ! ssib-snow
     &          ,CTO4 ,FIO4 ,FLO4 ,BIO4 ,BLO4 ,HO4                    & ! ssib-snow
     &          ,ra_sw_physics                                        & ! ssib 
!------------------------------------------------------------------------------
     &          , ids,ide,jds,jde,kds,kde                             &
     &          , ims,ime,jms,jme,kms,kme                             &
     &          , ips,ipe,jps,jpe,kps,kpe                             &
     &          , i_start,i_end,j_start,j_end,kts,kte,num_tiles       &
             !  Optional moisture tracers
     &           ,qv_curr, qc_curr, qr_curr                           &
     &           ,qi_curr, qs_curr, qg_curr                           &
             !  Optional moisture tracer flags
     &           ,f_qv,f_qc,f_qr                                      &
     &           ,f_qi,f_qs,f_qg                                      &
             !  Other optionals (more or less em specific)
     &          ,capg,hol,mol                                         &
     &          ,rainncv,rainshv,rainbl,regime,thc,graupelncv,hailncv &
     &          ,qsg,qvg,qcg,soilt1,tsnav                             &
     &          ,smfr3d,keepfr3dflag,dew,rhosnf,precipfr              &
             !  Other optionals (more or less nmm specific)
     &          ,potevp,snopcx,soiltb,sr                              &
             !  Optional observation PX LSM surface nudging
     &          ,t2_ndg_old, q2_ndg_old, t2_ndg_new, q2_ndg_new       &
     &          ,sn_ndg_old, sn_ndg_new                               &
     &          ,t2obs, q2obs                                         &
             ! OPTIONAL, Required by TEMF surface layer 1/7/09 WA
     &          ,hd_temf,te_temf,fCor,exch_temf,wm_temf               &
             ! Required by ideal SCM surface layer 1/6/10 WA
     &          ,hfx_force,lh_force,tsk_force                         &
     &          ,hfx_force_tend,lh_force_tend,tsk_force_tend          &
             !  Optional observation nudging
     &          ,uratx,vratx,tratx                                    &
             !  Optional ocean model
     &          ,sf_ocean_physics,oml_hml0,oml_gamma                  &
     &          ,tml,t0ml,hml,h0ml,huml,hvml,f,tmoml                  &
     &          ,oml_relaxation_time                                  &
     &          ,ustm,ck,cka,cd,cda,isftcflx,iz0tlnd                  &
     &         ,isurban, mminlu                                       &
     &          ,snotime                                              &
     &           ,rdlai2d                                             &
     &          ,usemonalb                                            &
     &          ,noahres                                              &
             !  Optional adaptive time step
     &          ,bldt,curr_secs,adapt_step_flag,bldtacttime           & 
         ! Optional urban with BEP
     &          ,sf_urban_physics,gmt,xlat,xlong,julday               &
     &          ,num_urban_layers                                     & !multi-layer urban
     &          ,num_urban_hi                                         & !multi-layer urban
     &          ,tsk_rural                                            & !multi-layer urban
     &          ,trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d              & !multi-layer urban
     &          ,tlev_urb3d,qlev_urb3d                                & !multi-layer urban
     &          ,tw1lev_urb3d,tw2lev_urb3d                            & !multi-layer urban
     &          ,tglev_urb3d,tflev_urb3d                              & !multi-layer urban
     &          ,sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d                  & !multi-layer urban
     &          ,sfvent_urb3d,lfvent_urb3d                            & !multi-layer urban 
     &          ,sfwin1_urb3d,sfwin2_urb3d                            & !multi-layer urban       
     &          ,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d            & !multi-layer urban
     &          ,lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d                 & !multi-layer urban
     &          ,mh_urb2d,stdh_urb2d,lf_urb2d                         &           
     &          ,a_u_bep,a_v_bep,a_t_bep,a_q_bep                      &
     &          ,b_u_bep,b_v_bep,b_t_bep,b_q_bep                      &
     &          ,sf_bep,vl_bep                                        &
     &          ,a_e_bep,b_e_bep,dlg_bep                              &
     &          ,dl_u_bep                                             &                          
     &          ,tsk_save                                             & !for fractional seaice
     &          ,cldfra                                               & !ssib
     &          ,sf_surface_mosaic,mosaic_cat,mosaic_cat_index                                    & !danli mosaic
     &          ,landusef2,TSK_mosaic,QSFC_mosaic,TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic            & !danli mosaic
     &          ,CANWAT_mosaic,SNOW_mosaic,SNOWH_mosaic,SNOWC_mosaic                              & !danli mosaic
     &          ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   & !danli mosaic
     &          ,HFX_mosaic,QFX_mosaic, LH_mosaic, GRDFLX_mosaic,SNOTIME_mosaic                   & !danli mosaic
     &          ,RS_mosaic,LAI_mosaic                                 &  !mosaic
     &          ,TR_URB2D_mosaic,TB_URB2D_mosaic                      &  !danli mosaic 
     &          ,TG_URB2D_mosaic,TC_URB2D_mosaic                      &  !danli mosaic 
     &          ,QC_URB2D_mosaic,UC_URB2D_mosaic                      &  !danli mosaic                  
     &          ,TRL_URB3D_mosaic,TBL_URB3D_mosaic                    &  !danli mosaic 
     &          ,TGL_URB3D_mosaic                                     &  !danli mosaic 
     &          ,SH_URB2D_mosaic,LH_URB2D_mosaic                      &  !danli mosaic 
     &          ,G_URB2D_mosaic,RN_URB2D_mosaic                       &  !danli mosaic 
     &          ,TS_URB2D_mosaic                                      &  !danli mosaic 
     &          ,TS_RUL2D_mosaic                                      &  !danli mosaic     
     &          ,ZOL                                                  &  !ckay
     &          ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas  &  !fasdas
     &          ,spp_lsm,pattern_spp_lsm,field_sf                     &  !SPP
     &          ,spp_pbl,pattern_spp_pbl                              &  !SPP
     &          ,XLAIDYN                                              &
     &                                                             )

#if ( ! NMM_CORE == 1 )
   USE module_state_description, ONLY : SFCLAYSCHEME              &
                                       ,SFCLAYREVSCHEME           &
                                       ,MYJSFCSCHEME              &
                                       ,QNSESFCSCHEME             &
                                       ,GFSSFCSCHEME              &
                                       ,PXSFCSCHEME               &
                                       ,NOAHMPSCHEME              &
                                       ,TEMFSFCSCHEME             &
                                       ,IDEALSCMSFCSCHEME         &
                                       ,SLABSCHEME                &
                                       ,LSMSCHEME                 &
                                       ,RUCLSMSCHEME              &
                                       ,PXLSMSCHEME               &
                                       ,CLMSCHEME                 &
                                       ,SSIBSCHEME                & !ssib
                                       ,MYNNSFCSCHEME             &
                                       ,OMLSCHEME                 &
                                       ,PWP3DSCHEME
#else
   USE module_state_description, ONLY : SFCLAYSCHEME              &
                                       ,SFCLAYREVSCHEME           &
                                       ,MYJSFCSCHEME              &
                                       ,QNSESFCSCHEME             &
                                       ,GFSSFCSCHEME              &
                                       ,PXSFCSCHEME               &
                                       ,NOAHMPSCHEME              &
                                       ,SLABSCHEME                &
                                       ,LSMSCHEME                 &
                                       ,RUCLSMSCHEME              &
                                       ,CLMSCHEME                 &
                                       ,PXLSMSCHEME               &
                                       ,TEMFSFCSCHEME             &
                                       ,GFDLSFCSCHEME             &
                                       ,SSIBSCHEME                & ! ssib
                                       ,GFDLSLAB 


#endif
   USE module_model_constants
! *** add new modules of schemes here

   USE module_sf_sfclay
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_gfs
   USE module_sf_noahdrv                           ! danli mosaic, the " ,only : lsm " needs to be deleted 
   USE module_sf_noahlsm, only : LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL
   USE module_sf_noahmpdrv, only : noahmplsm, noahmp_urban
   USE module_sf_noahmp_groundwater
   USE module_sf_noah_seaice_drv
#ifdef WRF_USE_CLM
   USE module_sf_clm
#endif
   USE module_sf_ssib  ! ssib
   USE module_sf_ruclsm
   USE module_sf_pxsfclay
   USE module_sf_pxlsm
   USE module_sf_temfsfclay
   USE module_sf_sfclayrev
   USE module_sf_noah_seaice_drv
#if ( EM_CORE==1)
   USE module_sf_mynn
   USE module_sf_fogdes    ! Katata - fog deposition module
   USE module_sf_ocean_driver
   USE module_sf_idealscmsfclay
#endif
   USE module_sf_scmflux       
   USE module_sf_scmskintemp

#if ( NMM_CORE == 1 )
   USE module_sf_gfdl
#endif

   USE module_sf_slab
!
   USE module_sf_sfcdiags
   USE module_sf_sfcdiags_ruclsm
   USE module_sf_sstskin
   USE module_sf_tmnupdate
   USE module_sf_lake
   USE module_cpl, ONLY : coupler_on, cpl_rcv
!
!  This driver calls subroutines for the surface parameterizations.
!
!  surface layer: (between surface and pbl)
!      1. sfclay
!      2. myjsfc
!      7. Pleim surface layer
!      5. MYNN surface layer
!  surface: ground temp/lsm scheme:
!      1. slab
!      2. Noah LSM
!      7. Pleim-Xiu LSM
!     11. Revised sfclay (option 1) 
!
!  surface: ground temp/lsm scheme for urban:
!      2.  BEP
!
!  ocean mixed layer model
!      sf_ocean_physics = 1
!  ocean 3d PWP
!      sf_ocean_physics = 2
!------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------
! In WRF, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  In your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Definitions
!-----------
! Theta      potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           net short wave flux at ground surface (W/m^2)
!-- SWDOWN        downward short wave flux at ground surface (W/m^2)
!-- EMISS         surface emissivity (between 0 and 1)
!-- TSK           surface temperature (K)
!-- TMN           soil temperature at lower boundary (K)
!-- TYR           annual mean surface temperature of previous year (K)
!-- TYRA          accumulated surface temperature in the current year (K)
!-- TLAG          mean surface temperature of previous 140 days (K)
!-- TDLY          accumulated daily mean surface temperature of the current day (K)
!-- XLAND         land mask (1 for land, 2 for water)
!-- MAX_EDOM      number of external model domains
!-- CPLMASK       coupling mask (0 for data read in wrflowinput, 1 data received from the coupler)
!-- ZNT           thermal time-varying roughness length (m)
!-- MZNT          momentum time-varying roughness length (m)
!-- Z0            background roughness length (m)
!-- MAVAIL        surface moisture availability (between 0 and 1)
!-- UST           u* in similarity theory (m/s)
!-- MOL           T* (similarity theory) (K)
!-- HOL           PBL height over Monin-Obukhov length
!-- PBLH          PBL height (m)
!-- CAPG          heat capacity for soil (J/K/m^3)
!-- THC           thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC         flag indicating snow coverage (1 for snow cover)
!-- HFX           net upward heat flux at the surface (W/m^2)
!-- QFX           net upward moisture flux at the surface (kg/m^2/s)
!-- TAUX          RHO*U**2 for ocean coupling
!-- TAUY          RHO*U**2 for ocean coupling
!-- LH            net upward latent heat flux at surface (W/m^2)
!-- REGIME        flag indicating PBL regime (stable, unstable, etc.)
!-- tke_pbl       turbulence kinetic energy from PBL schemes (m^2/s^2)
!-- akhs          sfc exchange coefficient of heat/moisture from MYJ
!-- akms          sfc exchange coefficient of momentum from MYJ
!-- thz0          potential temperature at roughness length (K)
!-- uz0           u wind component at roughness length (m/s)
!-- vz0           v wind component at roughness length (m/s)
!-- qsfc          specific humidity at lower boundary (kg/kg)
!-- uratx         ratio of u over u10 (Added for obs-nudging)
!-- vratx         ratio of v over v10 (Added for obs-nudging)
!-- tratx         ratio of t over th2 (Added for obs-nudging)
!-- u10           diagnostic 10-m u component from surface layer
!-- v10           diagnostic 10-m v component from surface layer
!-- UOCE          sea surface zonal currents (m s-1)
!-- VOCE          sea surface meridional currents (m s-1)
!-- th2           diagnostic 2-m theta from surface layer and lsm
!-- t2            diagnostic 2-m temperature from surface layer and lsm
!-- q2            diagnostic 2-m mixing ratio from surface layer and lsm
!-- tshltr        diagnostic 2-m theta from MYJ
!-- th10          diagnostic 10-m theta from MYJ
!-- qshltr        diagnostic 2-m specific humidity from MYJ
!-- q10           diagnostic 10-m specific humidity from MYJ
!-- lowlyr        index of lowest model layer above ground
!-- rr            dry air density (kg/m^3)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- pshltr        diagnostic shelter (2m) pressure from MYJ (Pa)
!-- p8w           pressure at full levels (Pa)
!-- t_phy         temperature (K)
!-- dz8w          dz between full levels (m)
!-- z             height above sea level (m)
!-- DX            horizontal space interval (m)
!-- DT            time step (second)
!-- PSFC          pressure at the surface (Pa)
!-- SST           sea-surface temperature (K)
!-- SST_INPUT     sea-surface temperature read in wrflowinput (K) (= SST if no coupling)
!-- SSTSK         skin sea-surface temperature (K)
!-- DTW           warm layer temp diff (K)
!-- TSLB
!-- ZS
!-- DZS
!-- num_soil_layers number of soil layer
!-- IFSNOW      ifsnow=1 for snow-cover effects
!-- sf_ocean_physics       whether to call ocean model from slab (1 = oml, 2=3d PWP)
!-- oml_hml0      initial mixed layer depth (if real-data not available, default 50 m)
!-- oml_gamma     lapse rate below mixed layer in ocean (default 0.14 K m-1)
!-- oml_relaxation_time   time the oml will take to get back to its original state (seconds)
!-- ck            enthalpy exchange coeff at 10 meters
!-- cd            momentum exchange coeff at 10 meters
!-- cka           enthalpy exchange coeff at the lowest model level
!-- cda           momentum exchange coeff at the lowest model level
!!!!!!!!!!!!!!
!
!
!-- LANDUSEF     Landuse fraction                      ! P-X LSM
!-- SOILCTOP     Top soil fraction                     ! P-X LSM
!-- SOILCBOT     Bottom soil fraction                  ! P-X LSM
!-- RA           Aerodynamic resistence                ! P-X LSM
!-- RS           Stomatal resistence                   ! P-X LSM, also from Noah lsm, lsm_mosaic, or noahmp
!-- VEGF_PX      PX LSM internal LU-based Veg Fraction ! P-X LSM
!-- IMPERV       Impervious surface fraction           ! P-X LSM
!-- CANFRA       Canopy/Tree fraction                  ! P-X LSM
!-- NLCAT        Number of landuse categories          ! P-X LSM
!-- NSCAT        Number of soil categories             ! P-X LSM
!-- ch - drag coefficient for heat/moisture            ! MYNN LSM

!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kps           start index for k in patch
!-- kpe           end index for k in patch
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!******************************************************************
!------------------------------------------------------------------

   INTEGER, INTENT(IN) ::                                             &
     &           ids,ide,jds,jde,kds,kde                              &
     &          ,ims,ime,jms,jme,kms,kme                              &
     &          ,ips,ipe,jps,jpe,kps,kpe                              &
     &          ,kts,kte,num_tiles

   INTEGER, INTENT(IN)::   FRACTIONAL_SEAICE
   INTEGER, INTENT(IN)::   SEAICE_ALBEDO_OPT
   REAL,    INTENT(IN)::   SEAICE_ALBEDO_DEFAULT
   INTEGER, INTENT(IN)::   SEAICE_THICKNESS_OPT
   REAL,    INTENT(IN)::   SEAICE_THICKNESS_DEFAULT
   INTEGER, INTENT(IN)::   SEAICE_SNOWDEPTH_OPT
   REAL,    INTENT(IN)::   SEAICE_SNOWDEPTH_MAX
   REAL,    INTENT(IN)::   SEAICE_SNOWDEPTH_MIN
   INTEGER, INTENT(IN)::   IFNDALBSI
   INTEGER, INTENT(IN)::   IFNDICEDEPTH
   INTEGER, INTENT(IN)::   IFNDSNOWSI

   INTEGER, INTENT(IN)::   NLCAT, mosaic_lu, mosaic_soil
   INTEGER, INTENT(IN)::   NSCAT
   INTEGER,  INTENT(IN )  :: LakeModel
   REAL,     INTENT(IN)   :: lake_min_elev

   INTEGER, INTENT(IN)::   history_interval

   INTEGER, INTENT(IN) :: sf_sfclay_physics, sf_surface_physics,      &
                          sf_urban_physics,ra_lw_physics,sst_update,  &
                          ra_sw_physics                    
   INTEGER, INTENT(IN),OPTIONAL :: sst_skin, tmn_update,              &
                                   scm_force_skintemp, scm_force_flux 

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::  ISLTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   IVGTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   LOWLYR
   INTEGER, INTENT(IN )::   IFSNOW
   INTEGER, INTENT(IN )::   ISFFLX
   INTEGER, INTENT(IN )::   ITIMESTEP
   INTEGER, INTENT(IN )::   NUM_SOIL_LAYERS
   REAL,    INTENT(IN ),OPTIONAL ::   JULIAN_in
   INTEGER, INTENT(IN )::   LAGDAY
   INTEGER, INTENT(IN )::   STEPBL
   INTEGER, INTENT(IN )::   ISICE
   INTEGER, INTENT(IN )::   ISWATER
   INTEGER, INTENT(IN ), OPTIONAL :: ISURBAN
   CHARACTER(LEN=*), INTENT(IN ), OPTIONAL :: MMINLU
   LOGICAL, INTENT(IN )::   WARM_RAIN
   LOGICAL, INTENT(IN)::   tice2tsk_if2cold
   INTEGER, INTENT(INOUT ),OPTIONAL ::   NYEAR
   REAL   , INTENT(INOUT ),OPTIONAL ::   NDAY
   INTEGER, INTENT(IN ),OPTIONAL ::   YR
   REAL , INTENT(IN )::   U_FRAME
   REAL , INTENT(IN )::   V_FRAME

  !added by Wei Yu for WRF_HYDRO 
   real ::  HYDRO_dt
   REAL, DIMENSION( ims:ime , jms:jme ):: sfcheadrt,INFXSRT, soldrain

#if (NMM_CORE==1)
   real , intent(IN )::   SFENTH
   INTEGER, INTENT(IN)::   ICOEF_SF
   INTEGER, INTENT(IN)::   IWAVECPL
   LOGICAL, INTENT(IN)::   LCURR_SF
   logical,intent(in),optional  :: pert_Cd
   integer,intent(in),optional  :: ens_random_seed
   real,intent(in),optional     :: ens_Cdamp
#endif
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMOIS
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   TSLB
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(OUT)  ::   SMCREL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GLW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   GSW,SWDOWN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   HT
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   RAINCV
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SST
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   SSTSK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN    ),OPTIONAL ::   SST_INPUT
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   DTW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   TMN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYR
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TDLY
   REAL, DIMENSION( ims:ime , 1:lagday , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TLAG
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   VEGFRA
!------fds (06/2010)--------------------------
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICE
!---------------------------------------------
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   ALBSI
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   ICEDEPTH
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SNOWSI
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XLAND
   INTEGER,                                          INTENT(IN   ) ::   MAX_EDOM
   REAL, DIMENSION( ims:ime , 1:max_edom, jms:jme ), INTENT(IN   ), OPTIONAL ::   CPLMASK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICEM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   MAVAIL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SNOALB
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ACSNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SNOTIME
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKHS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKMS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   CANWAT

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   GRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   HFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   RMOL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   PBLH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   Q2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QFX
#if (NMM_CORE==1)
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT):: TAUX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT):: TAUY
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT):: cd_out
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT):: ch_out
#endif
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QSFC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SFCRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ACRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTAV
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTOT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TH2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   THZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TSK
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UDRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UST
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   VZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   WSPD
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ZNT
#if (HWRF==1)
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   MZNT
#endif
!-----fds (06/2010)---------------------------------------------
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_LHF ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_SHF ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_GHF ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_EGS ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_ECI ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_ECT ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_EGI ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_EGT ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_SDN ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_SUP ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_LDN ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_LUP ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_WAT ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_SHC ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_SHG ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_LAI ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_VCF ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_Z00 ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   SSIB_VEG ! SSiB output
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   ALSWVISDIR! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   ALSWVISDIF! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   ALSWNIRDIR! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(OUT)::   ALSWNIRDIF! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(IN)::      SWVISDIR! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(IN)::      SWVISDIF! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(IN)::      SWNIRDIR! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(IN)::      SWNIRDIF! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SSiB_BR ! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SSiB_FM ! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SSiB_FH ! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SSiB_CM ! SSiB
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SSiBXDD ! SSiB
   INTEGER, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT):: ISNOW  ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SWE     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SNOWDEN ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT):: SNOWDEPTH ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TKAIR   ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   DZO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   WO1     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSN1   ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSNO1  ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BWO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BTO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   CTO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FIO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FLO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BIO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BLO1    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   HO1     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   DZO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   WO2     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSN2   ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSNO2  ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BWO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BTO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   CTO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FIO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FLO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BIO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BLO2    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   HO2     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   DZO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   WO3     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSN3   ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSNO3  ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BWO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BTO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   CTO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FIO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FLO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BIO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BLO3    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   HO3     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   DZO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   WO4     ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSN4   ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSSNO4  ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BWO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BTO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   CTO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FIO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   FLO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BIO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   BLO4    ! ssib-snow
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   HO4     ! ssib-snow
#if (NMM_CORE==1)
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)::    CHARN, MSANG, SCURX, SCURY
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   RIBN, ZKMAX
#endif
!----------------------------------------------------------
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   BR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   CHKLOWQ
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   GZ1OZ0
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   FHH
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   FM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIH
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   Q10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   QSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TH10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   U10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   V10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   UOCE
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   VOCE
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSFC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   ACSNOM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEVP
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACHFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACLHF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACGRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEXC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLHC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLQC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   CT
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   DZ8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   PI_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   RHO
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   TH_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   T_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   U_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   V_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   Z

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::   TKE_PBL

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(INOUT),OPTIONAL ::   pattern_spp_lsm,field_sf
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(INOUT),OPTIONAL ::   pattern_spp_pbl
   INTEGER, INTENT(IN), OPTIONAL                 ::     spp_lsm,spp_pbl

   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   DZS
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   ZS
   REAL, INTENT(IN )::   DT
   REAL, INTENT(IN )::   DX
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,       INTENT(INOUT),OPTIONAL    ::     bldtacttime  

!  arguments for NCAR surface physics

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   ALBBCK  ! INOUT needed for NMM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   EMBCK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   LH
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SH2O
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMAX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMIN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   Z0

!  NoahMP specific fields

   INTEGER, OPTIONAL, INTENT(IN) ::    idveg, iopt_crs, iopt_btr, iopt_run, iopt_sfc , iopt_frz, &
                                    iopt_inf, iopt_rad, iopt_alb, iopt_snf, iopt_tbot, iopt_stc, &
				    iopt_gla, iopt_rsf, iopt_soil,iopt_pedo,iopt_crop
   REAL,    OPTIONAL, DIMENSION(ims:ime ,8, jms:jme), INTENT(IN) :: SOILCOMP
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme)  , INTENT(IN) :: SOILCL1,SOILCL2,SOILCL3,SOILCL4

   INTEGER, OPTIONAL, DIMENSION(ims:ime , jms:jme),                    INTENT(INOUT) :: ISNOWXY,   PGSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime ,-2:num_soil_layers, jms:jme), INTENT(INOUT) :: ZSNSOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime ,-2:0,               jms:jme), INTENT(INOUT) :: TSNOXY, SNICEXY, SNLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme),                    INTENT(INOUT) ::                    &
           TVXY,    TGXY,CANICEXY,CANLIQXY,   EAHXY,   TAHXY,    CMXY,    CHXY,  FWETXY,SNEQVOXY,ALBOLDXY, &
        QSNOWXY,WSLAKEXY,   ZWTXY,    WAXY,    WTXY,LFMASSXY,RTMASSXY,STMASSXY,  WOODXY,STBLCPXY,FASTCPXY, &
	GRAINXY,   GDDXY,                                                                                  &
	 XSAIXY, TAUSSXY,  T2MVXY,  T2MBXY,  Q2MVXY,  Q2MBXY,  TRADXY,   NEEXY,   GPPXY,                   &
          NPPXY,  FVEGXY, RUNSFXY, RUNSBXY,  ECANXY,  EDIRXY, ETRANXY,   FSAXY,  FIRAXY,  APARXY,   PSNXY, &
	  SAVXY,   SAGXY, RSSUNXY, RSSHAXY,  BGAPXY,  WGAPXY,   TGVXY,   TGBXY,   CHVXY,   CHBXY,   SHGXY, &
	  SHCXY,   SHBXY,   EVGXY,   EVBXY,   GHVXY,   GHBXY,   IRGXY,   IRCXY,   IRBXY,    TRXY,   EVCXY, &
       CHLEAFXY,  CHUCXY,  CHV2XY,  CHB2XY,CHSTARXY                         
   INTEGER,    OPTIONAL, DIMENSION(ims:ime , jms:jme), INTENT(IN) :: CROPCAT
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme), INTENT(IN) :: PLANTING
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme), INTENT(IN) :: HARVEST
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme), INTENT(IN) :: SEASON_GDD

!  NoahMP specific fields - runoff option 5

   INTEGER, OPTIONAL, INTENT(IN)      :: stepwtd
   REAL,    OPTIONAL, INTENT(IN)      :: wtddt
   REAL,    OPTIONAL, DIMENSION(ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::  smoiseq
   REAL,    OPTIONAL, DIMENSION(ims:ime , jms:jme),                    INTENT(INOUT) ::  &
    SMCWTDXY,   RECHXY, DEEPRECHXY, FDEPTHXY,  AREAXY,  RIVERCONDXY, RIVERBEDXY,  &
    EQZWT,      PEXPXY, QRFXY,      QSPRINGXY, QSLATXY, QRFSXY,      QSPRINGSXY                         

   REAL,    OPTIONAL, DIMENSION(ims:ime,60,jms:jme) :: gecros_state                  ! Optional gecros crop

   INTEGER, INTENT(IN )::   OPT_THCND
! Noah UA changes
   LOGICAL, INTENT(IN) :: ua_phys
   REAL, DIMENSION(ims:ime , jms:jme), INTENT(OUT) ::  flx4,fvb,fbur,fgsn

! Variables for multi-layer UCM
   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN )::   NUM_URBAN_LAYERS
   INTEGER, INTENT(IN )::   NUM_URBAN_HI
   REAL, OPTIONAL, DIMENSION( ims:ime,                     jms:jme ), INTENT(INOUT) :: tsk_rural
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN)  :: hi_urb2d  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: lp_urb2d  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: lb_urb2d  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: hgt_urb2d !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: mh_urb2d  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: stdh_urb2d!urban
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(IN)  :: lf_urb2d  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   !Implicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   !Explicit momentum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   !Explicit momentum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   !Explicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    !Fraction air volume in grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   !Height above ground
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  !Fraction air at the face of grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  !Length scale

!  arguments for Ocean Mixed Layer Model
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT )::   TML, T0ML, HML, H0ML, HUML, HVML
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN    )::   F, TMOML
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(OUT   )::   CK, CKA, CD, CDA
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT )::   USTM

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT )::   TSK_SAVE

#if ( EM_CORE==1)
   REAL, DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(INOUT   ):: ch

!Katata-added - extra in-output
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT):: fgdp,dfgdp,vdfg
   INTEGER, OPTIONAL, INTENT(IN)                                :: grav_settling
!Katata-end

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
        &OPTIONAL, INTENT(IN   ):: tsq,qsq,cov,Sh3d,el_pbl,qc_bl,cldfra_bl
   INTEGER, OPTIONAL, INTENT(IN)                                :: bl_mynn_cloudpdf, &
                                                                   icloud_bl
#endif


   INTEGER, OPTIONAL, INTENT(IN )::   slope_rad, topo_shading
   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN):: shadowmask
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT):: swnorm
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN):: slope,slp_azi
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN):: diffuse_frac

   INTEGER, OPTIONAL, INTENT(IN )::   ISFTCFLX,IZ0TLND
   INTEGER, OPTIONAL, INTENT(IN )::   SF_OCEAN_PHYSICS
   REAL   , OPTIONAL, INTENT(IN )::   OML_HML0
   REAL   , OPTIONAL, INTENT(IN )::   OML_GAMMA
   REAL   , OPTIONAL, INTENT(IN )::   OML_RELAXATION_TIME
!
!  Observation nudging
!
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   uratx  !Added for obs-nudging
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   vratx  !Added for obs-nudging
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   tratx  !Added for obs-nudging
!
!  PX LSM Surface Grid Analysis nudging
!
   INTEGER, OPTIONAL, INTENT(IN)    :: pxlsm_smois_init, pxlsm_soil_nudge, ANAL_INTERVAL
   REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   LANDUSEF
   REAL, DIMENSION( ims:ime, NSCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SOILCTOP, SOILCBOT
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT):: IMPERV, CANFRA
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT):: VEGF_PX
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT):: RA
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT):: RS
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT):: LAI
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   T2OBS
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   Q2OBS

   REAL,       DIMENSION( ims:ime,  jms:jme ),                           &
               OPTIONAL, INTENT(INOUT)    ::      t2_ndg_old,            &
                                                  q2_ndg_old,            &
                                                  t2_ndg_new,            &
                                                  q2_ndg_new,            &
                                                  sn_ndg_old,            &
                                                  sn_ndg_new
!
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)   ::   snowncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)   ::   graupelncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)   ::   hailncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   capg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   emiss
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   hol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   mol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   regime
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     rainncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     rainshv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   RAINBL
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   t2
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     thc
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qsg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qvg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qcg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   dew
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soilt1
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   tsnav
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   potevp ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   snopcx ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soiltb ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   sr ! NMM and RUC LSM
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   smfr3d
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   keepfr3dflag
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   rhosnf !  density of snowfall
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT) ::    precipfr ! time-step frozen precip from RUC LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   snowfallac !  density of snowfall

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT), OPTIONAL  ::   NOAHRES
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)          ::   ZOL

   INTEGER, INTENT(IN) :: MAXPATCH, inest

  integer, optional,  dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  real,    optional,  dimension(ims:ime,jms:jme ),intent(inout) :: sabv,sabg,lwup
  integer, optional,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real,    optional,  dimension(ims:ime,jms:jme ),intent(inout) ::t2m_max,t2m_min,t2clm
  real,    optional,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) ::  &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,   &
                t_ref2m,h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,SWUPsubgrid ,&
                LHsoi,LHveg,LHtran
#ifdef CN
!ylu 05/31/2011

!CROP&CN restart and potential output
  integer, optional,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: croplive
  real,optional,dimension(ims:ime,1:maxpatch,jms:jme),intent(inout) :: &
                 dyntlai,dyntsai,dyntop,dynbot,   &
                 htmx,gdd1020,gdd820,gdd020,grainc,grainc_storage  &
                ,grainc_xfer,grainn,grainn_storage,grainn_xfer,days_active  &
                ,onset_flag,onset_counter,onset_gddflag,onset_fdd,onset_gdd &
                ,onset_swi,offset_flag,offset_counter,offset_fdd,offset_swi &
                ,dayl,annavg_t2m,tempavg_t2m,tempsum_potential_gpp          &
                ,annsum_potential_gpp,tempmax_retransn,annmax_retransn      &
                ,prev_leafc_to_litter,prev_frootc_to_litter,tempsum_npp     &
                ,annsum_npp,leafc,leafc_storage,leafc_xfer,frootc           &
                ,frootc_storage,frootc_xfer,livestemc,livestemc_storage     &
                ,livestemc_xfer,deadstemc,deadstemc_storage,deadstemc_xfer  &
                ,livecrootc,livecrootc_storage,livecrootc_xfer,deadcrootc   &
                ,deadcrootc_storage,deadcrootc_xfer,cpool,pft_ctrunc        &
                ,leafn,leafn_storage,leafn_xfer,frootn,frootn_storage       &
                ,frootn_xfer,livestemn,livestemn_storage,livestemn_xfer     &
                ,deadstemn,deadstemn_storage,deadstemn_xfer,livecrootn      &
                ,livecrootn_storage,livecrootn_xfer,deadcrootn              &
                ,deadcrootn_storage,deadcrootn_xfer,npool,pft_ntrunc        &
                ,gresp_storage,gresp_xfer,xsmrpool,annsum_counter           &
                ,cannsum_npp,cannavg_t2m,wf,me,mean_fire_prob,cwdc,litr1c   &
                ,litr2c,litr3c,soil1c,soil2c,soil3c,soil4c,seedc,col_ctrunc &
                ,prod10c,prod100c,cwdn,litr1n,litr2n,litr3n,soil1n,soil2n   &
                ,soil3n,soil4n,seedn,col_ntrunc,prod10n,prod100n,sminn      &
                ,totlitc,dwt_seedc_to_leaf,dwt_seedc_to_deadstem,dwt_conv_cflux &
                ,dwt_prod10c_gain,dwt_prod100c_gain,prod100c_loss,dwt_frootc_to_litr1c &
                ,dwt_frootc_to_litr2c,dwt_frootc_to_litr3c,dwt_livecrootc_to_cwdc &
                ,dwt_deadcrootc_to_cwdc,dwt_seedn_to_leaf,dwt_seedn_to_deadstem &
                ,dwt_conv_nflux,dwt_prod10n_gain,dwt_prod100n_gain,prod100n_loss &
                ,dwt_frootn_to_litr1n,dwt_frootn_to_litr2n, dwt_frootn_to_litr3n &
                , dwt_livecrootn_to_cwdn,dwt_deadcrootn_to_cwdn,retransn
#endif


! Variables for TEMF surface layer
   REAL,OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: te_temf
   REAL,OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: hd_temf, exch_temf, wm_temf
   REAL,OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: fCor

! Variables for ideal SCM surface layer
   REAL,OPTIONAL, INTENT(INOUT) :: hfx_force,lh_force,tsk_force
   REAL,OPTIONAL, INTENT(IN   ) :: hfx_force_tend,lh_force_tend,tsk_force_tend

!  LOCAL  VAR

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::          &
                                                             QGH, &
                                                             CHS, &
                                                             CQS, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2

! SSIB local variables
   REAL ZDIFF
   REAL, DIMENSION( ims:ime , jms:jme ) :: XICE_save
!
   REAL    :: DTMIN,DTBL
!
   INTEGER :: i,J,K,NK,jj,ij
   INTEGER :: gfdl_ntsflg
   LOGICAL :: radiation, myj, frpcpn, isisfc
   LOGICAL, INTENT(in), OPTIONAL :: rdlai2d
   LOGICAL, INTENT(in), OPTIONAL :: usemonalb
   REAL    :: total_depth,mid_point_depth
   REAL    :: tconst,tprior,tnew,yrday,deltat
   REAL    :: SWSAVE
   REAL,       DIMENSION( ims:ime, jms:jme )          ::  GSWSAVE
!-------------------------------------------------
! urban related variables are added to declaration
!-------------------------------------------------
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF
     REAL, OPTIONAL, INTENT(IN) :: DECLIN, SOLCON
     REAL, OPTIONAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZEN
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: HRANG
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D  !urban
     INTEGER,  INTENT(IN) :: num_roof_layers                         !urban
     INTEGER,  INTENT(IN) :: num_wall_layers                         !urban
     INTEGER,  INTENT(IN) :: num_road_layers                         !urban
     INTEGER,  INTENT(IN), OPTIONAL :: julian,julyr                            !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZR          !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZB          !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZG          !urban

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TR_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TB_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TG_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: QC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: UC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXR_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXB_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXG_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXC_URB2D !urban

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TGRL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: SMR_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TRL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TBL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TGL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: SH_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: LH_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: G_URB2D  !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: RN_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TS_URB2D !urban
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: FRC_URB2D  !urban
     INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: UTYPE_URB2D  !urban

     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIM_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIH_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: GZ1OZ0_URB2D  !urban local var
!m     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: AKMS_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: U10_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: V10_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: TH2_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: Q2_URB2D    !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: UST_URB2D  !urban local var
     
!-------------------------------------------------
! Noah-mosaic related variables are added to declaration  (danli)
!-------------------------------------------------
  
  INTEGER, INTENT(IN) :: sf_surface_mosaic
  INTEGER, INTENT(IN) :: mosaic_cat
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT), OPTIONAL :: mosaic_cat_index
  REAL,    DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT), OPTIONAL :: landusef2

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        TSK_mosaic, QSFC_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic,   &
        HFX_mosaic,QFX_mosaic, LH_mosaic,GRDFLX_mosaic,SNOTIME_mosaic, &
        RS_mosaic,LAI_mosaic
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT)::   &
        TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic

   REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::  &
         TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic, UC_URB2D_mosaic, &
         SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic
     
!-------------------------------------------------
! End of Noah-mosaic related variables 
!-------------------------------------------------     
     
!--------fds (06/2010)---------------------------------------------
     REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ),               &
            OPTIONAL, INTENT(IN) ::                                 CLDFRA
     REAL   :: DAY, CLOUDFRAC, UV10
!------------------------------------------------------------------
!
     REAL, DIMENSION( ims:ime, jms:jme ) :: HFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: LH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_SEA

     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CQS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CPM_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLHC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLQC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QGH_SEA
!
     REAL, DIMENSION( ims:ime, jms:jme ) :: PSIH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: UST_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_LOCAL
    ! lake varibles ,inout(14)
    real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: savedtke12d
    real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: snowdp2d,       &
                                                                               h2osno2d,       &
                                                                               snl2d,          &
                                                                               t_grnd2d
 
    real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(inout)          :: t_lake3d,       &
                                                                               lake_icefrac3d
    real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(inout) :: t_soisno3d,     &
                                                                               h2osoi_ice3d,   &
                                                                               h2osoi_liq3d,   &
                                                                               h2osoi_vol3d,   &
                                                                               z3d,            &
                                                                               dz3d
    real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(inout) :: zi3d
    ! in(8)
    real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(in)             :: z_lake3d,       &
                                                                               dz_lake3d
    real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(in)             :: watsat3d,       &
                                                                               csol3d,         &
                                                                               tkmg3d,         &
                                                                               tkdry3d,        &
                                                                               tksatu3d
    real,    dimension(ims:ime,jms:jme ),intent(in)                         :: lakedepth2d
#if (EM_CORE==1)
    real ,    dimension(ims:ime,jms:jme )  ::  lakemask       
!    INTEGER  :: lakeflag
#endif
!    logical, dimension(ims:ime,jms:jme ),intent(in)                         :: lake 
 
!
   REAL   :: xice_threshold

! cyl 3d ocean variable 
   integer :: okms, okme
   real, optional , dimension(ims:ime, okms:okme, jms:jme), INTENT(INOUT):: OM_TMP,OM_S,OM_U,OM_V,OM_DEPTH
   real, optional , dimension(ims:ime, okms:okme, jms:jme), INTENT(IN):: OM_TINI,OM_SINI
   real, optional , dimension(ims:ime, jms:jme),INTENT(INOUT):: OM_ML, OM_LAT, OM_LON
   REAL, OPTIONAL , INTENT(IN   ) :: rdx, rdy,xtime,omdt
   REAL , OPTIONAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: msfu, msfv, msft
   INTEGER , OPTIONAL , INTENT(IN)        :: id
!
  real,    dimension(ims:ime,1:maxpatch,jms:jme ) ::  q_ref2m   ! clm


!------------------------------------------------------------------
   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param , doing_adapt_dt , decided
   LOGICAL :: do_adapt
!
! FASDAS
!
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT), OPTIONAL ::   SDA_HFX,SDA_QFX,HFX_BOTH, QFX_BOTH, QNORM
   INTEGER, INTENT(IN   )                              ::   fasdas
! local vars
   REAL, DIMENSION( ims:ime, jms:jme )                 ::   HFXOLD, QFXOLD
   REAL                                                ::   HFX_KAY, QFX_KAY
! local var for SPP_LSM
   INTEGER                                             ::   spp_lsm_loc

   real,    optional,  dimension(ims:ime,jms:jme ),intent(inout) :: XLAIDYN
!
!------------------------------------------------------------------
! Initialize local variables
  q_ref2m = 0.0
!------------------------------------------------------------------
!
! stop run if using ssib and fractional seaice=0  (fds 12/2010)

  if(sf_surface_physics .eq. SSIBSCHEME .and. fractional_seaice .eq. 0) then
    WRITE( message,* ) 'Please activate fractional seaice option when using SSiB model'
    CALL wrf_error_fatal ( message )
  endif

  if (sf_sfclay_physics .eq. 0) return

  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif

  if ( ( seaice_albedo_opt == 2 ) .and. ( ifndalbsi == 0 ) ) then
      call wrf_error_fatal("Field ALBSI not found in input.  Field ALBSI is required if SEAICE_ALBEDO_OPT=2")
  endif

  if ( ( seaice_thickness_opt == 1 ) .and. ( ifndicedepth == 0 ) ) then
      call wrf_error_fatal("Field ICEDEPTH not found in input.  Field ICEDEPTH is required if SEAICE_THICKNESS_OPT=1")
  endif

  if ( ( seaice_snowdepth_opt == 1 ) .and. ( ifndsnowsi == 0 ) ) then
      call wrf_error_fatal("Field SNOWSI not found in input.  Field SNOWSI is required if SEAICE_SNOWDEPTH_OPT=1")
  endif

  IF ( coupler_on .and. present(cplmask) .and. present(sst_input) ) THEN
     
     CALL cpl_rcv( id, 'SST',            &
        &              ids, ide, jds, jde, kds, kde, &
        &              ims, ime, jms, jme, kms, kme, &
        &              ips, ipe, jps, jpe, kps, kpe, &
        &              max_edom, cplmask, SST, SST_INPUT )
     
     CALL cpl_rcv( id, 'UOCE',            &
        &              ids, ide, jds, jde, kds, kde, &
        &              ims, ime, jms, jme, kms, kme, &
        &              ips, ipe, jps, jpe, kps, kpe, &
        &              max_edom, cplmask, UOCE )
     
     CALL cpl_rcv( id, 'VOCE',            &
        &              ids, ide, jds, jde, kds, kde, &
        &              ims, ime, jms, jme, kms, kme, &
        &              ips, ipe, jps, jpe, kps, kpe, &
        &              max_edom, cplmask, VOCE )
     
  END IF

#if (EM_CORE==1)
     spp_lsm_loc = spp_lsm
#else
     spp_lsm_loc = 0
#endif

  
!$OMP PARALLEL DO &
!$OMP PRIVATE (ij, i, j, k)
  DO ij = 1,num_tiles
    DO j = j_start(ij),j_end(ij)
      DO k = kms,kme
        DO i = i_start(ij),i_end(ij)
          v_phytmp(i, k, j) = 0.
          u_phytmp(i, k, j) = 0.
        ENDDO
      ENDDO
      DO i = i_start(ij),i_end(ij)
         QGH(i,j) = 0.
         CHS(i,j) = 0.
         CPM(i,j) = 0.
         CHS2(i,j) = 0.
#if (NMM_CORE==1)
         Cd_out(i,j) = 0.
         Ch_out(i,j) = 0.
#endif
      ENDDO
    ENDDO
  ENDDO
  DTMIN = 0.
  DTBL = 0.

! RAINBL in mm (Accumulation between PBL calls)

  IF ( PRESENT( rainncv ) .AND. PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j) + RAINNCV(i,j)
         IF ( PRESENT( rainshv ))RAINBL(i,j) = RAINBL(i,j) + RAINSHV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ELSE IF ( PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j)
         IF ( PRESENT( rainshv ))RAINBL(i,j) = RAINBL(i,j) + RAINSHV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF
! Update SST
  IF (sst_update .EQ. 1) THEN
    CALL wrf_debug( 100, 'SST_UPDATE is on' )
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
 ! check for lake model 
#if (EM_CORE==1)
          if ( lakemodel==1) then
            if(lakemask(i,j).eq.1.) then
              if ( xice(i,j).gt.xice_threshold) then   !mchen
                   xice(i,j)=0.0
               endif
             endif
          endif 
#else
          if ( lakemodel==1) then
            if(ht(i,j)>=lake_min_elev) then
              if ( xice(i,j).gt.xice_threshold) then   !mchen
                   xice(i,j)=0.0
               endif
             endif
          endif 
#endif
! end check lake model    
         XICE_save(I,J) = XICEM(I,J) 

         IF ( FRACTIONAL_SEAICE == 1 ) then
            IF ( ( XICE(I,J) .NE. XICEM(I,J) ) .AND. ( XICEM(I,J) .GT. XICE_THRESHOLD ) ) THEN
               ! Fractional values of ALBEDO and EMISSIVITY are valid according to the 
               ! earlier fractional seaice value, XICEM.  Recompute them for the new 
               ! seaice value XICE.
                IF ( SEAICE_ALBEDO_OPT ==2 ) THEN
                    IF ( ALBSI(I,J) < -1.E6 ) THEN
                        call wrf_error_fatal("Field ALBSI not found in input.  Field ALBSI is required if SEAICE_ALBEDO_OPT=2")
                    ENDIF
                    ALBEDO(I,J) = 0.08 + XICE(I,J)/XICEM(I,J) * ( ALBSI(I,J) - 0.08 )
                ELSE
                    ALBEDO(I,J) = 0.08 + XICE(I,J)/XICEM(I,J) * ( ALBEDO(I,J) - 0.08 )
                ENDIF
                EMISS (I,J) = 0.98 + XICE(I,J)/XICEM(I,J) * ( EMISS (I,J) - 0.98 )
! use old tsk from seaice part
                TSK(I,J) = TSK_SAVE(I,J)*XICE(I,J) + (1.-XICE(I,J))*SST(I,J)
            ENDIF
         ENDIF

        IF ( XLAND(i,j) .GT. 1.5 .AND. XICE(I,J) .GE. XICE_THRESHOLD .AND. XICEM(I,J) .LT. XICE_THRESHOLD ) THEN
          ! water point turns to sea-ice point
          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 1.
          IVGTYP(I,J) = ISICE
          ISLTYP(I,J) = 16
          VEGFRA(I,J) = 0.
          TMN(I,J) = 271.4

          ! Over new ice, initial guesses of ALBEDO and EMISS are
          ! based on default water and ice values for albedo and
          ! emissivity.  The land-surface schemes can update these
          ! values

          SELECT CASE ( SEAICE_ALBEDO_OPT )
          CASE ( 0, 1 )

              ALBEDO(I,J) = SEAICE_ALBEDO_DEFAULT * XICE(I,J) + 0.08 * ( 1.0-XICE(I,J) )
              ALBBCK(I,J) = SEAICE_ALBEDO_DEFAULT

          CASE ( 2 ) 
              
              IF ( ALBSI(I,J) < -1.E6 ) THEN
                  call wrf_error_fatal("Field ALBSI not found in input.  Field ALBSI is required if SEAICE_ALBEDO_OPT=2")
              ENDIF

              ALBEDO(I,J) = ALBSI(I,J) * XICE(I,J) + 0.08 * ( 1.0-XICE(I,J) )
              ALBBCK(I,J) = ALBSI(I,J)

          END SELECT

          EMISS(I,J)  = 0.98 * XICE(I,J) + 0.98 * ( 1.0-XICE(I,J) )
          EMBCK(I,J)  = 0.98
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = TSK(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 0.0
          ENDDO
        ENDIF
     IF (lakemodel.ne.1) then
         IF(XLAND(i,j) .GT. 1.5) THEN
           IF ( SST(i,j) .LT. 350. .and. SST(i,j) .GT. 250.) THEN
            TSK(i,j)   =SST(i,j)
            TSLB(i,1,j)=SST(i,j)
           ENDIF
          ENDIF
     ELSE
#if (EM_CORE==1)
!       if(lakeflag.eq.1) then
!         IF(XLAND(i,j) .GT. 1.5.AND.LAKEMASK(I,J).NE.1) THEN
!           IF ( SST(i,j) .LT. 350. .and. SST(i,j) .GT. 250.) THEN
!            TSK(i,j)   =SST(i,j)
!            TSLB(i,1,j)=SST(i,j)
!           ENDIF
!          ENDIF
!       else
!         if(XLAND(i,j) .GT. 1.5.and.ht(i,j).lt.lake_min_elev) then
!           IF ( SST(i,j) .LT. 350. .and. SST(i,j) .GT. 250.) THEN
!            TSK(i,j)   =SST(i,j)
!            TSLB(i,1,j)=SST(i,j)
!           ENDIF
!          ENDIF
!       endif   ! (lakeflag=1)
         IF(XLAND(i,j) .GT. 1.5.AND.LAKEMASK(I,J).NE.1) THEN
           IF ( SST(i,j) .LT. 350. .and. SST(i,j) .GT. 250.) THEN
            TSK(i,j)   =SST(i,j)
            TSLB(i,1,j)=SST(i,j)
           ENDIF
          ENDIF
#else
       IF(XLAND(i,j) .GT. 1.5.and.ht(i,j).lt.lake_min_elev) then
           IF ( SST(i,j) .LT. 350. .and. SST(i,j) .GT. 250.) THEN
            TSK(i,j)   =SST(i,j)
            TSLB(i,1,j)=SST(i,j)
           ENDIF
       ENDIF
#endif
     ENDIF  ! (lakemodel=1)
        IF ( XLAND(i,j) .LT. 1.5 .AND. XICEM(I,J) .GE. XICE_THRESHOLD .AND. XICE(I,J) .LT. XICE_THRESHOLD ) THEN
! sea-ice point turns to water point
          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 2.
          IVGTYP(I,J) = ISWATER
          ISLTYP(I,J) = 14
          VEGFRA(I,J) = 0.
          SNOW(I,J)  = 0.
          SNOWC(I,J) = 0.
          SNOWH(I,J) = 0.
          TMN(I,J) = SST(I,J)
          ALBEDO(I,J) = 0.08
          ALBBCK(I,J) = 0.08
          EMISS(I,J)  = 0.98
          EMBCK(I,J)  = 0.98
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = SST(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 1.0
          ENDDO
        ENDIF

        XICE_save(I,J) = XICEM(I,J)
        XICEM(i,j) = XICE(i,j)
        TSK_SAVE(I,J) = TSK(I, J)

      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF

  IF(PRESENT(SST_SKIN))THEN
    IF (sst_skin .EQ. 1) THEN
! Calculate skin sst based on Zeng and Beljaars (2005)
      CALL wrf_debug( 100, 'in SST_SKIN_UPDATE' )
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij, i, j, k )
      DO ij = 1 , num_tiles
        DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
            IF(XLAND(i,j) .GT. 1.5 .and. sst_update .NE. 1) THEN
              TSK(i,j)   =SST(i,j)
              TSLB(i,1,j)=SST(i,j)
            ENDIF
          ENDDO
        ENDDO
        CALL sst_skin_update(xland,glw,gsw,hfx,qfx,tsk,ust,         &
                emiss,dtw,sstsk,dt,stbolt,                          &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
        DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
            IF(XLAND(i,j) .GT. 1.5)TSK(i,j)=SSTSK(i,j)
          ENDDO
        ENDDO
      ENDDO
    !$OMP END PARALLEL DO
    ENDIF
  ENDIF

  IF(PRESENT(TMN_UPDATE))THEN
  IF (tmn_update .EQ. 1) THEN
      CALL wrf_debug( 100, 'in TMN_UPDATE' )
      CALL tmnupdate(tsk,tmn,tlag,tyr,tyra,tdly,nday,nyear,lagday, &
                julian_in, dt, yr,                                  &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start,i_end, j_start,j_end, kts,kte, num_tiles   )

  ENDIF
  ENDIF
!
! Modified for adaptive time step
!
   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
      END IF
   END IF

!  Do we run through this scheme or not?

!    Test 1:  If this is the initial model time, then yes.
!                ITIMESTEP=1
!    Test 2:  If the user asked for the surface to be run every time step, then yes.
!                BLDT=0 or STEPBL=1
!    Test 3:  If not adaptive dt, and this is on the requested surface frequency, then yes.
!                MOD(ITIMESTEP,STEPBL)=0
!    Test 4:  If using adaptive dt and the current time is past the last requested activate surface time, then yes.
!                CURR_SECS >= BLDTACTTIME

!  If we do run through the scheme, we set the flag run_param to TRUE and we set the decided flag
!  to TRUE.  The decided flag says that one of these tests was able to say "yes", run the scheme.
!  We only proceed to other tests if the previous tests all have left decided as FALSE.

   run_param = .FALSE.
   decided = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( PRESENT(bldt) )THEN
      IF ( ( .NOT. decided ) .AND. &
           ( ( bldt .EQ. 0. ) .OR. ( stepbl .EQ. 1 ) ) ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   ELSE
      IF ( ( .NOT. decided ) .AND. &
                                   ( stepbl .EQ. 1 )   ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepbl) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. bldtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

  IF ( run_param ) then

  radiation = .false.
  frpcpn = .false.
  myj    = ((sf_sfclay_physics .EQ. MYJSFCSCHEME) .OR. &
            (sf_sfclay_physics .EQ. QNSESFCSCHEME) )
  isisfc = ( FRACTIONAL_SEAICE .EQ. 1  .AND. (          &
            (sf_sfclay_physics .EQ. SFCLAYSCHEME ) .OR. &
            (sf_sfclay_physics .EQ. SFCLAYREVSCHEME ) .OR. &
            (sf_sfclay_physics .EQ. PXSFCSCHEME  ) .OR. &
            (sf_sfclay_physics .EQ. MYJSFCSCHEME ) .OR. &
            (sf_sfclay_physics .EQ. QNSESFCSCHEME ) .OR. &  !emt
#if (EM_CORE==1)
            (sf_sfclay_physics .EQ. MYNNSFCSCHEME ) .OR. &
#endif
            (sf_sfclay_physics .EQ. GFSSFCSCHEME ) )    &
           )

  IF (ra_lw_physics .gt. 0) radiation = .true.

  IF( PRESENT(slope_rad).AND. radiation )THEN
! topographic slope effects modify SWDOWN and GSW here
    IF (slope_rad .EQ. 1) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
           CALL TOPO_RAD_ADJ_DRVR (XLAT,XLONG,COSZEN,             &
                    shadowmask,diffuse_frac,                      &
                    declin,                                       &
                    SWDOWN,GSW,SWNORM,GSWSAVE,solcon,hrang,       &
                    slope,slp_azi,                                &
                ids, ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                     &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
    ENDDO
    !$OMP END PARALLEL DO

    ENDIF
  ENDIF
!----
! CALCULATE CONSTANT

     DTMIN=DT/60.
! Surface schemes need PBL time step for updates and accumulations
! Assume these schemes provide no tendencies

    if (PRESENT(adapt_step_flag)) then
       if (adapt_step_flag) then
          do_adapt = .TRUE.
       else
          do_adapt = .FALSE.
       endif
    else
       do_adapt = .FALSE.
    endif

    if (PRESENT(BLDT)) then
       if (bldt .eq. 0) then
          DTBL = dt
       ELSE
          if (do_adapt) then
             IF ( curr_secs .LT. 2. * dt ) THEN
                call wrf_message("WARNING: When using an adaptive time-step the boundary layer"// &
                                 " time-step should be 0 (i.e., equivalent to model time-step)." )
                call wrf_message("In order to proceed, for surface calculations, the "// &
                                 "boundary layer time-step"// &
                                 " will be rounded to the nearest minute," )
                call wrf_message("possibly resulting in innacurate results.")
             END IF
             DTBL=bldt*60
          else
             DTBL=DT*STEPBL
          endif
       endif
    else
       DTBL=DT*STEPBL
    endif

! SAVE OLD VALUES


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
! PSFC : in Pa
          PSFC(I,J)=p8w(I,kts,J)
! REVERSE ORDER IN THE VERTICAL DIRECTION
          DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
          ENDDO
! remove surface currents for atmospheric low-level winds
          u_phytmp(i,kts,j)=u_phytmp(i,kts,j)-uoce(i,j)
          v_phytmp(i,kts,j)=v_phytmp(i,kts,j)-voce(i,j)
       ENDDO
       ENDDO
     ENDDO
     !$OMP END PARALLEL DO

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
     sfclay_select: SELECT CASE(sf_sfclay_physics)

     CASE (SFCLAYSCHEME)
! DX varies spatially in NMM, therefore, SFCLAY cannot be called
! because it takes a scalar DX. NMM passes in a dummy value for this
! scalar.  NEEDS FURTHER ATTENTION. JM 20050215
       IF(PRESENT(SCM_FORCE_FLUX))THEN
         IF (scm_force_flux .EQ. 1) THEN
! surface forcing by observed fluxes
         CALL scmflux(u_phytmp, v_phytmp, t_phy, qv_curr, p_phy, dz8w,   &
                     cp, rcp, xlv, psfc, cpm, xland,                     &
                     psim, psih, hfx, qfx, lh, tsk, flhc, flqc,          &
                     znt, gz1oz0, wspd,                                  &
                     julian_in, karman, p1000mb,                         &
                     itimestep,chklowq,                                  &
                     ids, ide, jds, jde, kds, kde,                       &
                     ims, ime, jms, jme, kms, kme,                       &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
         ENDIF
       ENDIF
       IF(PRESENT(SCM_FORCE_SKINTEMP))THEN
         IF (scm_force_skintemp .EQ. 1) THEN
! surface forcing by observed skin temperature
         CALL scmskintemp(tsk, julian_in, itimestep,                     &
                     ids, ide, jds, jde, kds, kde,                       &
                     ims, ime, jms, jme, kms, kme,                       &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
         ENDIF
!         IF (scm_force_skintemp .EQ. 2) THEN
! surface forcing by gabls2 skin temperature
!         CALL scmgabls2(tsk, itimestep, dt,                              &
!                     ids, ide, jds, jde, kds, kde,                       &
!                     ims, ime, jms, jme, kms, kme,                       &
!                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
!         ENDIF
       ENDIF
       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in SFCLAY' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            CALL SFCLAY_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,fm,fhh, &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,th2,t2,q2,                                  &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
                 P1000mb,                                            &
                 XICE,SST,TSK_SEA,                                                  &
                 CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
                 HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
                 ITIMESTEP,TICE2TSK_IF2COLD,XICE_THRESHOLD,                         &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,                &
                 sf_surface_physics  )
         ELSE
         CALL SFCLAY(u_phytmp,v_phytmp,t_phy,qv_curr,              &
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,fm,fhh, &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,                                  &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
               P1000mb,                                            &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,scm_force_flux  )
#if ( EM_CORE==1)
           DO j = j_start(ij),j_end(ij)
           DO i = i_start(ij),i_end(ij)
             ch(i,j) = chs (i,j)
!!             ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
           end do
           end do
#endif
         ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SFCLAY in surface driver')
       ENDIF

     CASE (SFCLAYREVSCHEME)
! DX varies spatially in NMM, therefore, SFCLAY cannot be called
! because it takes a scalar DX. NMM passes in a dummy value for this
! scalar.  NEEDS FURTHER ATTENTION. JM 20050215
       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in SFCLAY' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            CALL SFCLAYREV_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,fm,fhh, &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,th2,t2,q2,                                  &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
                 P1000mb,                                            &
                 XICE,SST,TSK_SEA,                                                  &
                 CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
                 HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
                 ITIMESTEP,TICE2TSK_IF2COLD,XICE_THRESHOLD,                         &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,                &
                 sf_surface_physics  )
         ELSE
         CALL SFCLAYREV(u_phytmp,v_phytmp,t_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,fm,fhh, &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,                                  &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
               P1000mb,                                            &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd, scm_force_flux    )
#if ( EM_CORE==1)
           DO j = j_start(ij),j_end(ij)
           DO i = i_start(ij),i_end(ij)
             ch(i,j) = chs (i,j)
!!             ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
           end do
           end do
#endif
         ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SFCLAY in surface driver')
       ENDIF

     CASE (PXSFCSCHEME)
#if (NMM_CORE != 1)
       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in PX Surface Layer scheme' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            CALL PXSFCLAY_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,th_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,                                            &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,              &
                 XICE, SST, ITIMESTEP, TICE2TSK_IF2COLD,XICE_THRESHOLD, &
                 CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,FLHC_SEA,FLQC_SEA,&
                 HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, TSK_SEA,  &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
         ELSE
         CALL PXSFCLAY(u_phytmp,v_phytmp,t_phy,th_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,                                            &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,itimestep,    &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
         ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for PX Surface Layer in surface driver')
       ENDIF
#else
       CALL wrf_error_fatal('PX Surface Layer scheme cannot be used with NMM')
#endif

      CASE (MYJSFCSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN

        CALL wrf_debug(100,'in MYJSFC')
        IF ( FRACTIONAL_SEAICE == 1 ) THEN
           CALL MYJSFC_SEAICE_WRAPPER(itimestep,ht,dz8w,             &
                p_phy,p8w,th_phy,t_phy,                              &
                qv_curr,qc_curr,                                     &
                u_phy,v_phy,tke_pbl,                                 &
                tsk,qsfc,thz0,qz0,uz0,vz0,                           &
                lowlyr,                                              &
                xland,ivgtyp,isurban,iz0tlnd,                        &
                TICE2TSK_IF2COLD,                                    & ! Extra for wrapper.
                XICE_THRESHOLD,                                      & ! Extra for wrapper.
                XICE, SST,                                           & ! Extra for wrapper.
                CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,            &
                FLHC_SEA, FLQC_SEA, QSFC_SEA, &
                QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA, LH_SEA,         &
                TSK_SEA,                                             &
                ust,znt,z0,pblh,mavail,rmol,                         &
                akhs,akms,                                           &
                br,                                                 &
                chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
                u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
                p1000mb,                                             &
                ids,ide, jds,jde, kds,kde,                           &
                ims,ime, jms,jme, kms,kme,                           &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
        ELSE
            CALL MYJSFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                      &
              u_phy,v_phy,tke_pbl,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,ivgtyp,isurban,iz0tlnd,                        &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
              p1000mb,                                             &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
#if ( EM_CORE==1)
         DO j = j_start(ij),j_end(ij)
            DO i = i_start(ij),i_end(ij)
               wspd(i,j) = MAX(SQRT(u_phy(i,kts,j)**2+v_phy(i,kts,j)**2),0.001)
               ch(i,j) = chs (i,j)
!!           ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
            END DO
         END DO
#endif         

        ENDIF
#if ( EM_CORE==1)
!       ustm is not available in NMM
!       ustm is needed for LES tke calculation (ustm is ust used in friction)
         DO j = j_start(ij),j_end(ij)
            DO i = i_start(ij),i_end(ij)
               ustm(i,j) = ust(i,j)
            END DO
         END DO
#endif
       ELSE
         CALL wrf_error_fatal('Lacking arguments for MYJSFC in surface driver')
       ENDIF

      CASE (QNSESFCSCHEME)
       IF(PRESENT(SCM_FORCE_FLUX))THEN
         IF (scm_force_flux .EQ. 1) THEN
! surface forcing by observed fluxes
         CALL scmflux(u_phytmp, v_phytmp, t_phy, qv_curr, p_phy, dz8w,   &
                     cp, rcp, xlv, psfc, cpm, xland,                     &
                     psim, psih, hfx, qfx, lh, tsk, flhc, flqc,          &
                     znt, gz1oz0, wspd,                                  &
                     julian_in, karman, p1000mb,                         &
                     itimestep,chklowq,                                  &
                     ids, ide, jds, jde, kds, kde,                       &
                     ims, ime, jms, jme, kms, kme,                       &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
         ENDIF
       ENDIF
       IF(PRESENT(SCM_FORCE_SKINTEMP))THEN
         IF (scm_force_skintemp .EQ. 1) THEN
! surface forcing by observed skin temperature
         CALL scmskintemp(tsk, julian_in, itimestep,                     &
                     ids, ide, jds, jde, kds, kde,                       &
                     ims, ime, jms, jme, kms, kme,                       &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
         ENDIF
       ENDIF

       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN
            CALL wrf_debug(100,'in QNSESFC')
             IF ( FRACTIONAL_SEAICE == 1 ) THEN
           CALL QNSESFC_SEAICE_WRAPPER(itimestep,ht,dz8w,             &
                p_phy,p8w,th_phy,t_phy,                              &
                qv_curr,qc_curr,                                     &
                u_phy,v_phy,tke_pbl,                                 &
                tsk,qsfc,thz0,qz0,uz0,vz0,                           &
                lowlyr,                                              &
                xland,                                               &
                TICE2TSK_IF2COLD,                                    & ! Extra for wrapper.
                XICE_THRESHOLD,                                      & ! Extra for wrapper.
                XICE, SST,                                           & ! Extra for wrapper.
                CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,            &
                FLHC_SEA, FLQC_SEA, QSFC_SEA, &
                QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA, LH_SEA,         &
                TSK_SEA,                                             &
                ust,znt,z0,pblh,mavail,rmol,                         &
                akhs,akms,                                           &
                br,                                                 &
                chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
                u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
                ids,ide, jds,jde, kds,kde,                           &
                ims,ime, jms,jme, kms,kme,                           &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,SCM_FORCE_FLUX    )
           ELSE
            CALL QNSESFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                     &
              u_phy,v_phy,tke_pbl,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,                                               &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij),     &
              kts,kte,scm_force_flux    )
#if ( EM_CORE==1)
         DO j = j_start(ij),j_end(ij)
            DO i = i_start(ij),i_end(ij)
               wspd(i,j) = MAX(SQRT(u_phy(i,kts,j)**2+v_phy(i,kts,j)**2),0.001)
               ch(i,j) = chs (i,j)
!!           ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
            END DO
         END DO
#endif         

        ENDIF
        ELSE
         CALL wrf_error_fatal('Lacking arguments for QNSESFC in surface driver')
       ENDIF

     CASE (GFSSFCSCHEME)
       IF (PRESENT(qv_curr) .AND. .TRUE. ) THEN
       CALL wrf_debug( 100, 'in GFSSFC' )
       IF (FRACTIONAL_SEAICE == 1) THEN
          CALL SF_GFS_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr, &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               TICE2TSK_IF2COLD,                            &
               XICE_THRESHOLD,                              &
               CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,        &
               FLHC_SEA, FLQC_SEA,                          &
               HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, &
               UST_SEA, ZNT_SEA, SST, XICE,                 &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ELSE
         CALL SF_GFS(u_phytmp,v_phytmp,t_phy,qv_curr,              &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ENDIF
        CALL wrf_debug(100,'in SFCDIAGS')
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SF_GFS in surface driver')
      ENDIF

#if ( EM_CORE==1)
    CASE(MYNNSFCSCHEME)

       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr)     &
            & .AND.  PRESENT(qcg) ) THEN
          
          CALL wrf_debug(100,'in MYNNSFC')          

         IF (FRACTIONAL_SEAICE == 1) THEN
          CALL MYNN_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,SNOWH,                            &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,              &
               &itimestep,ch,th_phy,pi_phy,qc_curr,rho,            &
               &tsq,qsq,cov,Sh3d,el_pbl,qcg,                       &
               &icloud_bl,qc_bl,cldfra_bl,                         &
               &spp_pbl,pattern_spp_pbl,                           &
               XICE,SST,TSK_SEA,                                   &
               CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,&
               HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,    &
               TICE2TSK_IF2COLD,XICE_THRESHOLD,                    &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte, &   
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,bl_mynn_cloudpdf)
         ELSE
          CALL SFCLAY_mynn(u_phytmp,v_phytmp,t_phy,qv_curr,        &
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,SNOWH,                            &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,              &
               &itimestep,ch,th_phy,pi_phy,qc_curr,rho,            &
               &tsq,qsq,cov,Sh3D,el_pbl,qcg,                       &
               &icloud_bl,qc_bl,cldfra_bl,                         &
               &spp_pbl,pattern_spp_pbl,                           &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte,&
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,bl_mynn_cloudpdf)
         ENDIF
       ELSE
          CALL wrf_error_fatal('Lacking arguments for SFCLAY_mynn in surface driver')
 
       ENDIF
#endif

#if ( EM_CORE==1)
     CASE (TEMFSFCSCHEME)
       IF (PRESENT(qv_curr).and.PRESENT(hd_temf)) THEN
         CALL wrf_debug( 100, 'in TEMFSFCLAY' )
! WA 9/7/09 must initialize Z0 and ZNT for TEMF in ideal cases
       ! DO J=j_start(ij),j_end(ij)
       ! DO I=i_start(ij),i_end(ij)
       !    CHKLOWQ(i,j) = 1.0
       !    Z0(i,j) = 0.03      ! For GABLS2
       !    ZNT(i,j) = 0.03     ! For GABLS2
       ! ENDDO
       ! ENDDO
         CALL TEMFSFCLAY(u3d=u_phytmp,v3d=v_phytmp,th3d=th_phy,    &
               qv3d=qv_curr,p3d=p_phy,pi3d=pi_phy,rho=rho,z=z,ht=ht, &
               CP=cp,G=g,ROVCP=rcp,R=r_d,XLV=xlv,psfc=psfc,chs=chs,&
               chs2=chs2,cqs2=cqs2,CPM=cpm,znt=znt,ust=ust,        &
               MAVAIL=mavail,XLAND=xland,HFX=hfx,QFX=qfx,LH=lh,   &
               TSK=tsk,FLHC=flhc,FLQC=flqc,QGH=qgh,qsfc=qsfc,      &
               U10=u10,V10=v10,TH2=th2,T2=t2,Q2=q2,                &
               SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0,EP1=ep_1, &
               EP2=ep_2,KARMAN=karman,fCor=fCor,te_temf=te_temf,   &
               hd_temf=hd_temf,exch_temf=exch_temf,wm_temf=wm_temf,&
               ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,  &
               ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,  &
               its=i_start(ij),ite=i_end(ij),                      &
               jts=j_start(ij),jte=j_end(ij), kts=kts,kte=kte )
       ELSE
         CALL wrf_error_fatal('Lacking arguments for TEMFSFCLAY in surface driver')
       ENDIF

     CASE (IDEALSCMSFCSCHEME)
       IF (PRESENT(qv_curr)) THEN
         CALL wrf_debug( 100, 'in IDEALSCMSFCLAY' )
         CALL IDEALSCMSFCLAY(u3d=u_phytmp,v3d=v_phytmp,th3d=th_phy,    &
               qv3d=qv_curr,p3d=p_phy,pi3d=pi_phy,rho=rho,z=z,ht=ht, &
               CP=cp,G=g,ROVCP=rcp,R=r_d,XLV=xlv,psfc=psfc,chs=chs,  &
               chs2=chs2,cqs2=cqs2,CPM=cpm,znt=znt,ust=ust,        &
               MAVAIL=mavail,XLAND=xland,HFX=hfx,QFX=qfx,LH=lh,   &
               TSK=tsk,FLHC=flhc,FLQC=flqc,QGH=qgh,qsfc=qsfc,      &
               U10=u10,V10=v10,TH2=th2,T2=t2,Q2=q2,                &
               SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0,EP1=ep_1, &
               EP2=ep_2,KARMAN=karman,fCor=fCor,   &
               exch_temf=exch_temf,                &
               hfx_force=hfx_force,lh_force=lh_force,tsk_force=tsk_force, &
               hfx_force_tend=hfx_force_tend,                      &
               lh_force_tend=lh_force_tend,                        &
               tsk_force_tend=tsk_force_tend,                      &
               dt=dt,itimestep=itimestep,                          &
               ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,  &
               ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,  &
               its=i_start(ij),ite=i_end(ij),                      &
               jts=j_start(ij),jte=j_end(ij), kts=kts,kte=kte )
       ELSE
         CALL wrf_error_fatal('Lacking arguments for IDEALSCMSFCLAY in surface driver')
       ENDIF
#endif

#if (NMM_CORE==1)

    CASE (GFDLSFCSCHEME)
       CALL wrf_debug( 100, 'in GFDLSFC' )

      IF(sf_surface_physics .eq. 88)THEN
        GFDL_NTSFLG=1
      ELSE
        GFDL_NTSFLG=0
      ENDIF

      CALL SF_GFDL(u_phytmp,v_phytmp,t_phy,qv_curr,p_phy, &
                   CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,                 &
                   DTBL, SMOIS,num_soil_layers,ISLTYP,ZNT,                &
#if (HWRF==1)
                   MZNT,                                                  &
#endif
                   UST,PSIM,PSIH,                                         &  
                   XLAND,HFX,QFX,TAUX,TAUY,LH,GSW,GLW,TSK,FLHC,FLQC,  & ! gopal's doing for Ocean coupling
                   QGH,QSFC,U10,V10,                              &
                   ICOEF_SF,IWAVECPL,LCURR_SF,CHARN, MSANG, SCURX, SCURY, &
                   pert_Cd, ens_random_seed, ens_Cdamp,           &
                   GZ1OZ0,WSPD,BR,ZKMAX, ISFFLX,                  &
                   EP_1,EP_2,KARMAN,GFDL_NTSFLG,SFENTH,           &
                   cd_out, ch_out,                                &
                   ids,ide, jds,jde, kds,kde,                     &
                   ims,ime, jms,jme, kms,kme,                             &
                   i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte    )
           RIBN = BR
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
           ENDDO
           ENDDO

#endif
     CASE DEFAULT

       WRITE( message , * )                                &
   'The sfclay option does not exist: sf_sfclay_physics = ', sf_sfclay_physics
       CALL wrf_error_fatal ( message )

     END SELECT sfclay_select

!  Compute uratx, vratx, tratx for obs nudging
     IF(PRESENT(uratx) .and. PRESENT(vratx) .and. PRESENT(tratx))THEN
        DO J=j_start(ij),j_end(ij)
        DO I=i_start(ij),i_end(ij)
           IF(ABS(U10(I,J)) .GT. 1.E-10) THEN
              uratx(I,J) = U_PHYTMP(I,1,J)/U10(I,J)
           ELSE
              uratx(I,J) = 1.2
           END IF
           IF(ABS(V10(I,J)) .GT. 1.E-10) THEN
              vratx(I,J) = V_PHYTMP(I,1,J)/V10(I,J)
           ELSE
              vratx(I,J) = 1.2
           END IF
! (Quotient P1000mb/P_PHY must be conditioned due to large value of P1000mb)
           tratx(I,J) = (T_PHY(I,1,J)*(P1000mb*0.001/(P_PHY(I,1,J)/1000.))**RCP)  &
                        /TH2(I,J)
        ENDDO
        ENDDO
     ENDIF

#if ( EM_CORE==1)
!Katata-added - fog (cloud) water deposition calculation
     IF ( grav_settling .EQ. 0 ) THEN
        !vdfg = 0.
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
           vdfg(i,j)=0.
        ENDDO
        ENDDO
     ELSE
        IF ( PRESENT(dfgdp) .AND. PRESENT(fgdp) .AND. &
           & PRESENT(rainbl) .AND. PRESENT(vdfg)) THEN
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              dfgdp(i,j)=0.
           ENDDO
           ENDDO

           CALL sf_fogdes(                                  &
                vdfg,fgdp,dfgdp,ivgtyp,lai,wspd,qc_curr,    &
                dtbl,rho,dz8w,grav_settling,nlcat,          &
                ids,ide, jds,jde, kds,kde,                  &
                ims,ime, jms,jme, kms,kme,                  &
                i_start(ij),i_end(ij),                      &
                j_start(ij),j_end(ij),kts,kte               )

           !Add fog dep to RAINBL in mm (Accumulation between PBL calls).
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              RAINBL(i,j) = RAINBL(i,j) + dfgdp(i,j)
              RAINBL(i,j) = MAX(RAINBL(i,j), 0.0)
           ENDDO
           ENDDO

        ELSE
          CALL wrf_error_fatal('Missing args for FGDP in surface driver')
        ENDIF
     ENDIF
!Katata/Joe-END
#endif

     ENDDO
     !$OMP END PARALLEL DO

     IF (ISFFLX.EQ.0 ) GOTO 430
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k ) firstprivate(frpcpn)
     DO ij = 1 , num_tiles

     sfc_select: SELECT CASE(sf_surface_physics)

     CASE (SLABSCHEME)

       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(capg)        .AND.    &
                                                      .TRUE. ) THEN
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
!          CQS2 ACCOUNTS FOR MAVAIL FOR SFCDIAGS 2M Q
              CQS2(I,J)= CQS2(I,J)*MAVAIL(I,J)
           ENDDO
           ENDDO

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              CALL wrf_error_fatal('SLAB scheme cannot be used with fractional seaice')
           ENDIF
        CALL wrf_debug(100,'in SLAB')
          CALL SLAB(t_phy,qv_curr,p_phy,flhc,flqc,  &
             psfc,xland,tmn,hfx,qfx,lh,tsk,qsfc,chklowq,          &
             gsw,glw,capg,thc,snowc,emiss,mavail,                 &
             dtbl,rcp,xlv,dtmin,ifsnow,                           &
             svp1,svp2,svp3,svpt0,ep_2,karman,eomeg,stbolt,       &
             tslb,zs,dzs,num_soil_layers,radiation,               &
             p1000mb,                                             &
             ids,ide, jds,jde, kds,kde,                           &
             ims,ime, jms,jme, kms,kme,                           &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
              IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
              IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
           ENDDO
           ENDDO

        CALL wrf_debug(100,'in SFCDIAGS')
          CALL SFCDIAGS(hfx,qfx,tsk,qsfc,chs2,cqs2,t2,th2,q2,      &
                     psfc,cp,r_d,rcp,CHS,t_phy,qv_curr,ua_phys,    &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

       ENDIF

     CASE (LSMSCHEME)

       IF (PRESENT(qv_curr)    .AND.  PRESENT(rainbl)        .AND.    &
!          PRESENT(emiss)      .AND.  PRESENT(t2)            .AND.    &
!          PRESENT(declin) .AND.  PRESENT(coszen)    .AND.    &
!          PRESENT(hrang)  .AND. PRESENT( xlat_urb2d)    .AND.    &
!          PRESENT(dzr)       .AND.    &
!          PRESENT( dzb)            .AND. PRESENT(dzg)       .AND.    &
!          PRESENT(tr_urb2d) .AND. PRESENT(tb_urb2d)         .AND.    &
!          PRESENT(tg_urb2d) .AND. PRESENT(tc_urb2d) .AND.            &
!          PRESENT(qc_urb2d) .AND. PRESENT(uc_urb2d) .AND.            &
!          PRESENT(xxxr_urb2d) .AND. PRESENT(xxxb_urb2d) .AND.        &
!          PRESENT(xxxg_urb2d) .AND.                                  &
!          PRESENT(xxxc_urb2d) .AND. PRESENT(trl_urb3d) .AND.         &
!          PRESENT(tbl_urb3d)   .AND. PRESENT(tgl_urb3d)  .AND.       &
!          PRESENT(sh_urb2d) .AND. PRESENT(lh_urb2d)  .AND.           &
!          PRESENT(g_urb2d)   .AND. PRESENT(rn_urb2d) .AND.           &
!          PRESENT(ts_urb2d)                          .AND.           &
!          PRESENT(frc_urb2d) .AND. PRESENT(utype_urb2d)   .AND.      &
                                                      .TRUE. ) THEN
!------------------------------------------------------------------
         IF( PRESENT(sr) ) THEN
           frpcpn=.true.
         ENDIF
         IF ( FRACTIONAL_SEAICE == 1) THEN
            ! The fields passed to LSM need to represent the full ice values, not
            ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
            ! to a value representing only the sea-ice portion.   Albedo over open 
            ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
            DO j = j_start(ij) , j_end(ij)
               DO i = i_start(ij) , i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                     ALBEDO(I,J) = (ALBEDO(I,J)-(1.-XICE(I,J))*0.08)/XICE(I,J)
                     EMISS(I,J) = (EMISS(I,J)-(1.-XICE(I,J))*0.98)/XICE(I,J)
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               ! Use surface layer routine values from the ice portion of grid point
            ELSE
               !
               ! We don't have surface layer routine values at this time, so
               ! just use what we have.  Use ice component of TSK
               !
               CALL get_local_ice_tsk( ims, ime, jms, jme,                   &
                                       i_start(ij), i_end(ij),               & 
                                       j_start(ij), j_end(ij),               &
                                       itimestep, .false., tice2tsk_if2cold, &
                                       XICE, XICE_THRESHOLD,                 &
                                       SST, TSK, TSK_SEA, TSK_LOCAL )

               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     TSK(i,j) = TSK_LOCAL(i,j)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

!added for WRF_HYDRO
#ifdef WRF_HYDRO
         if(HYDRO_dt .ge. 0) HYDRO_dt = dtbl
#endif

! added RA population for WRF/Noah-CMAQ RS Consistency
! following Garland et al. (1977) and Nemitz et al., 2009
         IF ( PRESENT(RA) ) THEN
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            RA(I,J) = WSPD(I,J)/UST(I,J)**2.0
         ENDDO
         ENDDO
         END IF

         CALL wrf_debug(100,'in NOAH DRV')
                
         IF (sf_surface_mosaic == 1) THEN
          
           IF ( PRESENT( TSK_mosaic ) .AND. PRESENT( HFX_mosaic ) ) THEN
             CALL lsm_mosaic(dz8w,qv_curr,p8w,t_phy,tsk,                 &
                hfx,qfx,lh,grdflx,qgh,gsw,swdown,glw,smstav,smstot,    &
                sfcrunoff,udrunoff,ivgtyp,isltyp,isurban,isice,vegfra,        &
                albedo,albbck,znt,z0, tmn,xland,xice,emiss, embck,    &
                snowc,qsfc,rainbl,                              &
                mminlu,                                         &
                num_soil_layers,dtbl,dzs,itimestep,             &
                smois,tslb,snow,canwat,                         &
                chs, chs2, cqs2, cpm,rcp,SR,chklowq,lai,qz0,    &
                myj,frpcpn,                                     &
		sh2o,snowh,                                     & !h
                u_phy,v_phy,                                    & !I
                snoalb,shdmin,shdmax,                           & !i
                snotime,                                        & !o
                acsnom,acsnow,                                  & !o
                snopcx,                                         & !o
                potevp,                                         & !o
                smcrel,                                         & !o
                xice_threshold,                                 &
                rdlai2d,usemonalb,                              &
                br,                                             & !?
                NOAHRES,opt_thcnd,                              &
                NLCAT,landusef,landusef2,                       & ! danli mosaic
                sf_surface_mosaic,mosaic_cat,mosaic_cat_index,  & ! danli mosaic 
                TSK_mosaic,QSFC_mosaic,                         & ! danli mosaic 
                TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic,           & ! danli mosaic 
                CANWAT_mosaic,SNOW_mosaic,                      & ! danli mosaic
                SNOWH_mosaic,SNOWC_mosaic,                      & ! danli mosaic 
                ALBEDO_mosaic,ALBBCK_mosaic,                    & ! danli mosaic
                EMISS_mosaic, EMBCK_mosaic,                     & ! danli mosaic
                ZNT_mosaic, Z0_mosaic,                          & ! danli mosaic 
                HFX_mosaic,QFX_mosaic,                          & ! danli mosaic
                LH_mosaic, GRDFLX_mosaic, SNOTIME_mosaic,       & ! danli mosaic
                RS_mosaic, LAI_mosaic,                          & ! mosaic
                ua_phys,flx4,fvb,fbur,fgsn,                     &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                sf_urban_physics                                &
!Optional urban
                ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif    &
                ,cmgr_sfcdif,chgr_sfcdif                        &
                ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d,  & !H urban
                uc_urb2d,                                       & !H urban
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d,    & !H urban
                trl_urb3d,tbl_urb3d,tgl_urb3d,                  & !H urban
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d,    & !H urban
                TR_URB2D_mosaic,TB_URB2D_mosaic,                & !H urban  danli mosaic
                TG_URB2D_mosaic,TC_URB2D_mosaic,                & !H urban  danli mosaic
                QC_URB2D_mosaic,UC_URB2D_mosaic,                & !H urban  danli mosaic                  
                TRL_URB3D_mosaic,TBL_URB3D_mosaic,              & !H urban  danli mosaic
                TGL_URB3D_mosaic,                               & !H urban  danli mosaic
                SH_URB2D_mosaic,LH_URB2D_mosaic,                & !H urban  danli mosaic
                G_URB2D_mosaic,RN_URB2D_mosaic,                 & !H urban  danli mosaic
                TS_URB2D_mosaic,                                & !H urban  danli mosaic
                TS_RUL2D_mosaic,                                & !H urban  danli mosaic                
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d,      & !O urban
                GZ1OZ0_urb2d, AKMS_URB2D,                       & !O urban
                th2_urb2d,q2_urb2d,ust_urb2d,                   & !O urban
                declin,coszen,hrang,                            & !I solar
                xlat_urb2d,                                     & !I urban
                num_roof_layers, num_wall_layers,               & !I urban
                num_road_layers, DZR, DZB, DZG,                 & !I urban
                CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,      & !H urban
                julian,julyr,                                   & !H urban
                DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,            & !H urban
                FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,      & !H urban
                FRC_URB2D, UTYPE_URB2D,                         & !I urban
                num_urban_layers,                               & !I multi-layer urban
                num_urban_hi,                                   & !I multi-layer urban
                tsk_rural,                                      & !H multi-layer urban
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,        & !H multi-layer urban
                tlev_urb3d,qlev_urb3d,                          & !H multi-layer urban
                tw1lev_urb3d,tw2lev_urb3d,                      & !H multi-layer urban
                tglev_urb3d,tflev_urb3d,                        & !H multi-layer urban
                sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,            & !H multi-layer urban
                sfvent_urb3d,lfvent_urb3d,                      & !H multi-layer urban
                sfwin1_urb3d,sfwin2_urb3d,                      & !H multi-layer urban
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,      & !H multi-layer urban
                lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,           & !H multi-layer urban
                mh_urb2d,stdh_urb2D,lf_urb2d,                   & !SLUCM
                th_phy,rho,p_phy,ust,                           & !I multi-layer urban
                gmt,julday,xlong,xlat,                          & !I multi-layer urban
                a_u_bep,a_v_bep,a_t_bep,a_q_bep,                & !O multi-layer urban
                a_e_bep,b_u_bep,b_v_bep,                        & !O multi-layer urban
                b_t_bep,b_q_bep,b_e_bep,dlg_bep,                & !O multi-layer urban
                dl_u_bep,sf_bep,vl_bep                          & !O multi-layer urban
                ,sfcheadrt,INFXSRT, soldrain                    & !hydro
                ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas    & ! fasdas
                ,RS,XLAIDYN)
           ELSE
               CALL wrf_error_fatal('Lack arguments to call lsm_mosaic')
           ENDIF

	  ELSEIF (sf_surface_mosaic == 0) THEN
	                  
!
!  FASDAS
!
             IF( fasdas == 1 ) THEN
               DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)

!ckay2015 only do indirect nudging over land areas 
                     IF(XLAND(i,j) .GT. 1.5) then
                      SDA_QFX(I,J) = 0.0
                      SDA_HFX(I,J) = 0.0
                     END IF

! TWG2015 Removed lines that update fluxes to ensure this section only defines
! the output
                    QFXOLD(I,J)=QFX(I,J)
                    QFX_KAY = SDA_QFX(I,J)*RHO(I,1,J)*DZ8W(I,1,J)
                    QFX_KAY = QFX_KAY * QNORM(I,J)
                    QFX_BOTH(I,J)=QFX(I,J)+QFX_KAY

                    HFXOLD(I,J)=HFX(I,J)
                    HFX_KAY = SDA_HFX(I,J)*RHO(I,1,J)*CPM(I,J)*DZ8W(I,1,J)
                    HFX_BOTH(I,J)=HFX(I,J)+HFX_KAY

               ENDDO
               ENDDO
             END IF
!
!  END FASDAS
!
               CALL lsm(dz8w,qv_curr,p8w,t_phy,tsk,                 &
                hfx,qfx,lh,grdflx,qgh,gsw,swdown,glw,smstav,smstot,    &
                sfcrunoff,udrunoff,ivgtyp,isltyp,isurban,isice,vegfra,        &
                albedo,albbck,znt,z0, tmn,xland,xice,emiss, embck,    &
                snowc,qsfc,rainbl,                              &
                mminlu,                                         &
                num_soil_layers,dtbl,dzs,itimestep,             &
                smois,tslb,snow,canwat,                         &
                chs, chs2, cqs2, cpm,rcp,SR,chklowq,lai,qz0,    &
                myj,frpcpn,                                     &
		sh2o,snowh,                                     & !h
                u_phy,v_phy,                                    & !I
                snoalb,shdmin,shdmax,                           & !i
                snotime,                                        & !o
                acsnom,acsnow,                                  & !o
                snopcx,                                         & !o
                potevp,                                         & !o
                smcrel,                                         & !o
                xice_threshold,                                 &
                rdlai2d,usemonalb,                              &
                br,                                             & !?
                NOAHRES,opt_thcnd,                              &
                ua_phys,flx4,fvb,fbur,fgsn,                     &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                sf_urban_physics                                &
!Optional urban
                ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif    &
                ,cmgr_sfcdif,chgr_sfcdif                        &
                ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d,  & !H urban
                uc_urb2d,                                       & !H urban
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d,    & !H urban
                trl_urb3d,tbl_urb3d,tgl_urb3d,                  & !H urban
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d,    & !H urban
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d,      & !O urban
                GZ1OZ0_urb2d, AKMS_URB2D,                       & !O urban
                th2_urb2d,q2_urb2d,ust_urb2d,                   & !O urban
                declin,coszen,hrang,                            & !I solar
                xlat_urb2d,                                     & !I urban
                num_roof_layers, num_wall_layers,               & !I urban
                num_road_layers, DZR, DZB, DZG,                 & !I urban
                CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,      & !H urban
                DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,            & !H urban
                FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,      & !H urban
                julian, julyr,                                  & !H urban
                FRC_URB2D, UTYPE_URB2D,                         & !I urban
                num_urban_layers,                               & !I multi-layer urban
                num_urban_hi,                                   & !I multi-layer urban
                tsk_rural,                                      & !H multi-layer urban
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,        & !H multi-layer urban
                tlev_urb3d,qlev_urb3d,                          & !H multi-layer urban
                tw1lev_urb3d,tw2lev_urb3d,                      & !H multi-layer urban
                tglev_urb3d,tflev_urb3d,                        & !H multi-layer urban
                sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,            & !H multi-layer urban
                sfvent_urb3d,lfvent_urb3d,                      & !H multi-layer urban
                sfwin1_urb3d,sfwin2_urb3d,                      & !H multi-layer urban
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,      & !H multi-layer urban
                lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,           & !H multi-layer urban
                mh_urb2d,stdh_urb2D,lf_urb2d,                   & !SLUCM
                th_phy,rho,p_phy,ust,                           & !I multi-layer urban
                gmt,julday,xlong,xlat,                          & !I multi-layer urban
                a_u_bep,a_v_bep,a_t_bep,a_q_bep,                & !O multi-layer urban
                a_e_bep,b_u_bep,b_v_bep,                        & !O multi-layer urban
                b_t_bep,b_q_bep,b_e_bep,dlg_bep,                & !O multi-layer urban
                dl_u_bep,sf_bep,vl_bep                          & !O multi-layer urban
                ,sfcheadrt,INFXSRT, soldrain &
!
!  FASDAS
!
                ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas    &
!
!  END FASDAS
!
                ,RS,XLAIDYN)
         ENDIF

         call seaice_noah( SEAICE_ALBEDO_OPT, SEAICE_ALBEDO_DEFAULT, SEAICE_THICKNESS_OPT, &
              &            SEAICE_THICKNESS_DEFAULT, SEAICE_SNOWDEPTH_OPT,             &
              &            SEAICE_SNOWDEPTH_MAX, SEAICE_SNOWDEPTH_MIN,                 &
              &            t_phy, qv_curr, p8w, dz8w, num_soil_layers, dt, frpcpn, sr, &
              &            glw, swdown, rainbl, snoalb, qgh, xice, xice_threshold,     &
              &            albsi, icedepth, snowsi,                                    &
              &            tslb, emiss, albedo, z0, tsk, snow, snowc, snowh,           &
              &            chs, chs2, cqs2,                                            &
              &            br, znt, lh, hfx, qfx, potevp, grdflx, qsfc, acsnow,        &
              &            acsnom, snopcx, sfcrunoff, noahres,                         &
              &            sf_urban_physics, b_t_bep, b_q_bep, rho,                    &
              &            ids,ide, jds,jde, kds,kde,                                  &
              &            ims,ime, jms,jme, kms,kme,                                  &
              &            i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte )

         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            ! LSM Returns full land/ice values, no fractional values.
            ! We return to a fractional component here.  SFLX currently hard-wires
            ! emissivity over sea ice to 0.98, the same value as over open water, so
            ! the fractional consideration doesn't have any effect for emissivity.
            DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                     albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                     emiss(i,j) = ( emiss(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        !  Weighted average of fields between ice-cover values and open-water values.
                        flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                        flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                        cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                        cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                        chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                        chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                        qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qsfc_sea(i,j) )
                        qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qgh_sea(i,j)  )
                        qz0(i,j)  = ( qz0(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qz0_sea(i,j)  )
                                                            !  print *,'hfx =',hfx_sea(170,20)
                                                            !   print *,'XICE =',XICE(170,20)
                                                            !    print *,'QSFC =',QSFC(170,20)
                        hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                        qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                        lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
!save old tsk_ice
                        tsk_save(i,j)  = tsk(i,j)
                        tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        ! Compute TSK as the open-water and ice-cover average
                        tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
!              CHKLOWQ(I,J)= 1.0
               SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
               SFCEXC(I,J)= CHS(I,J)
               IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
               IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
               IF(PRESENT(ACGRDFLX))ACGRDFLX(I,J)=ACGRDFLX(I,J) + GRDFLX(I,J)*DT
           ENDDO
           ENDDO

          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
                     PSFC,CP,R_d,RCP,CHS,t_phy,qv_curr,ua_phys,    &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
!urban
     IF(SF_URBAN_PHYSICS.eq.1) THEN
       DO j=j_start(ij),j_end(ij)                             !urban
         DO i=i_start(ij),i_end(ij)                           !urban
          IF( IVGTYP(I,J) == ISURBAN .or. &
	      IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. & !urban
              IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or.&
	      IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN  !urban
             U10(I,J)  = U10_URB2D(I,J)                       !urban
             V10(I,J)  = V10_URB2D(I,J)                       !urban
             PSIM(I,J) = PSIM_URB2D(I,J)                      !urban
             PSIH(I,J) = PSIH_URB2D(I,J)                      !urban
             GZ1OZ0(I,J) = GZ1OZ0_URB2D(I,J)                  !urban
!m             AKHS(I,J) = AKHS_URB2D(I,J)                    !urban
             AKHS(I,J) = CHS(I,J)                             !urban
             AKMS(I,J) = AKMS_URB2D(I,J)                      !urban
           END IF                                             !urban
         ENDDO                                                !urban
       ENDDO                                                  !urban
     ENDIF
! urban BEP
     IF((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.eq.3)) THEN
       DO j=j_start(ij),j_end(ij)                             !urban
         DO i=i_start(ij),i_end(ij)                           !urban
          IF( IVGTYP(I,J) == ISURBAN .or. &
	      IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. & !urban
              IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. &
	      IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN !urban
            T2(I,J)   = TH_PHY(i,1,j)/((1.E5/PSFC(I,J))**RCP) !urban
            TH2(I,J) = TH_PHY(i,1,j) !urban
            Q2(I,J)   = qv_curr(i,1,j)  !urban
            U10(I,J)  = U_phy(I,1,J)                       !urban
            V10(I,J)  = V_phy(I,1,J)                       !urban
           END IF                                             !urban
         ENDDO                                                !urban
       ENDDO                                                  !urban
     ENDIF

!------------------------------------------------------------------

       ELSE
         CALL wrf_error_fatal('Lacking arguments for LSM in surface driver')
       ENDIF

     CASE (NOAHMPSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(rainbl)        .AND.    &
!          PRESENT(emiss)      .AND.  PRESENT(t2)            .AND.    &
!          PRESENT(declin) .AND.  PRESENT(coszen)    .AND.    &
!          PRESENT(hrang)  .AND. PRESENT( xlat_urb2d)    .AND.    &
!          PRESENT(dzr)       .AND.    &
!          PRESENT( dzb)            .AND. PRESENT(dzg)       .AND.    &
!          PRESENT(tr_urb2d) .AND. PRESENT(tb_urb2d)         .AND.    &
!          PRESENT(tg_urb2d) .AND. PRESENT(tc_urb2d) .AND.            &
!          PRESENT(qc_urb2d) .AND. PRESENT(uc_urb2d) .AND.            &
!          PRESENT(xxxr_urb2d) .AND. PRESENT(xxxb_urb2d) .AND.        &
!          PRESENT(xxxg_urb2d) .AND.                                  &
!          PRESENT(xxxc_urb2d) .AND. PRESENT(trl_urb3d) .AND.         &
!          PRESENT(tbl_urb3d)   .AND. PRESENT(tgl_urb3d)  .AND.       &
!          PRESENT(sh_urb2d) .AND. PRESENT(lh_urb2d)  .AND.           &
!          PRESENT(g_urb2d)   .AND. PRESENT(rn_urb2d) .AND.           &
!          PRESENT(ts_urb2d)                          .AND.           &
!          PRESENT(frc_urb2d) .AND. PRESENT(utype_urb2d)   .AND.      &
#if (EM_CORE==1)
           PRESENT(smcwtdxy)       .AND.                              &
           PRESENT(rechxy)         .AND.                              &   
           PRESENT(deeprechxy)     .AND.                              &
           PRESENT(fdepthxy)       .AND.                              &
           PRESENT(areaxy)         .AND.                              &   
           PRESENT(rivercondxy)    .AND.                              &
           PRESENT(riverbedxy)     .AND.                              & 
           PRESENT(eqzwt)          .AND.                              &    
           PRESENT(pexpxy)         .AND.                              &   
           PRESENT(qrfxy)          .AND.                              &    
           PRESENT(qspringxy)      .AND.                              &
           PRESENT(qslatxy)        .AND.                              &  
           PRESENT(qrfsxy)         .AND.                              &   
           PRESENT(qspringsxy)     .AND.                              & 
           PRESENT(smoiseq)        .AND.                              &  
           PRESENT(wtddt)          .AND.                              &    
           PRESENT(stepwtd)        .AND.                              &
#endif
                                                      .TRUE. ) THEN
!------------------------------------------------------------------


         IF ( FRACTIONAL_SEAICE == 1) THEN
            ! The fields passed to LSM need to represent the full ice values, not
            ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
            ! to a value representing only the sea-ice portion.   Albedo over open 
            ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
            DO j = j_start(ij) , j_end(ij)
               DO i = i_start(ij) , i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                     ALBEDO(I,J) = (ALBEDO(I,J)-(1.-XICE(I,J))*0.08)/XICE(I,J)
                     EMISS(I,J) = (EMISS(I,J)-(1.-XICE(I,J))*0.98)/XICE(I,J)
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               ! Use surface layer routine values from the ice portion of grid point
            ELSE
               !
               ! We don't have surface layer routine values at this time, so
               ! just use what we have.  Use ice component of TSK
               !
               CALL get_local_ice_tsk( ims, ime, jms, jme,                   &
                                       i_start(ij), i_end(ij),               &
                                       j_start(ij), j_end(ij),               &
                                       itimestep, .false., tice2tsk_if2cold, &
                                       XICE, XICE_THRESHOLD,                 &
                                       SST, TSK, TSK_SEA, TSK_LOCAL )

               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     TSK(i,j) = TSK_LOCAL(i,j)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

!added for WRF_HYDRO
#ifdef WRF_HYDRO
         if(HYDRO_dt .ge. 0) HYDRO_dt = dtbl
#endif
         CALL wrf_debug(100,'in NOAHMP DRV')
         CALL noahmplsm(ITIMESTEP,       YR, JULIAN_IN,   COSZEN, XLAT,XLONG, &
	           DZ8W,     DTBL,      DZS,     NUM_SOIL_LAYERS,         DX, &
		 IVGTYP,   ISLTYP,   VEGFRA,   SHDMAX,       TMN,             &
		  XLAND,     XICE,     XICE_THRESHOLD,   CROPCAT,             &
               PLANTING,  HARVEST,SEASON_GDD,                               &
                  IDVEG, IOPT_CRS, IOPT_BTR, IOPT_RUN,  IOPT_SFC,   IOPT_FRZ, &
	       IOPT_INF, IOPT_RAD, IOPT_ALB, IOPT_SNF, IOPT_TBOT,   IOPT_STC, &
	       IOPT_GLA, IOPT_RSF, IOPT_SOIL,IOPT_PEDO,IOPT_CROP,             &
                IZ0TLND, SF_URBAN_PHYSICS,                                    &
	       SOILCOMP,  SOILCL1,  SOILCL2,  SOILCL3,   SOILCL4,             &
		  T_PHY,  QV_CURR,    U_PHY,    V_PHY,    SWDOWN,        GLW, &
		    P8W,   RAINBL,       SR,                                  &
		    TSK,      HFX,      QFX,       LH,    GRDFLX,     SMSTAV, &
		 SMSTOT,SFCRUNOFF, UDRUNOFF,   ALBEDO,     SNOWC,      SMOIS, &
		   SH2O,     TSLB,     SNOW,    SNOWH,    CANWAT,     ACSNOM, &
		 ACSNOW,    EMISS,     QSFC,                                  &
 		     Z0,      ZNT,                                            & ! IN/OUT LSM eqv
		ISNOWXY,     TVXY,     TGXY, CANICEXY,  CANLIQXY,      EAHXY, &
		  TAHXY,     CMXY,     CHXY,   FWETXY,  SNEQVOXY,   ALBOLDXY, &
		QSNOWXY, WSLAKEXY,    ZWTXY,     WAXY,      WTXY,     TSNOXY, &
		ZSNSOXY,  SNICEXY,  SNLIQXY, LFMASSXY,  RTMASSXY,   STMASSXY, &
		 WOODXY, STBLCPXY, FASTCPXY,      LAI,    XSAIXY,    TAUSSXY, &
	        SMOISEQ, SMCWTDXY,DEEPRECHXY,  RECHXY,   GRAINXY,      GDDXY,PGSXY, & ! IN/OUT Noah MP only
               GECROS_STATE,                                                  & ! IN/OUT gecros model
	         T2MVXY,   T2MBXY,   Q2MVXY,   Q2MBXY,                        &
                 TRADXY,    NEEXY,    GPPXY,    NPPXY,    FVEGXY,    RUNSFXY, &
	        RUNSBXY,   ECANXY,   EDIRXY,  ETRANXY,     FSAXY,     FIRAXY, &
                 APARXY,    PSNXY,    SAVXY,    SAGXY,   RSSUNXY,    RSSHAXY, &
                 BGAPXY,   WGAPXY,    TGVXY,    TGBXY,     CHVXY,      CHBXY, &
		  SHGXY,    SHCXY,    SHBXY,    EVGXY,     EVBXY,      GHVXY, &
		  GHBXY,    IRGXY,    IRCXY,    IRBXY,      TRXY,      EVCXY, &
	       CHLEAFXY,   CHUCXY,   CHV2XY,   CHB2XY,    RS,                 &
#ifdef WRF_HYDRO
                 sfcheadrt,INFXSRT,soldrain,                          &    !O
#endif
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,        &
! variables below are optional
                MP_RAINC =  RAINCV, MP_RAINNC =    RAINNCV, MP_SHCV = RAINSHV,&
		MP_SNOW  = SNOWNCV, MP_GRAUP  = GRAUPELNCV, MP_HAIL = HAILNCV )

         IF(SF_URBAN_PHYSICS > 0 ) THEN  !urban
         
	   call noahmp_urban (sf_urban_physics,     NUM_SOIL_LAYERS,     IVGTYP,ITIMESTEP,  & ! IN : Model configuration 
                                 DT,         COSZEN,     XLAT_URB2D,                        & ! IN : Time/Space-related
                              T_PHY,        QV_CURR,          U_PHY,      V_PHY,   SWDOWN,  & ! IN : Forcing
		                GLW,            P8W,         RAINBL,       DZ8W,      ZNT,  & ! IN : Forcing
                                TSK,            HFX,            QFX,         LH,   GRDFLX,  & ! IN/OUT : LSM 
		             ALBEDO,          EMISS,           QSFC,                        & ! IN/OUT : LSM 
                            ids,ide,        jds,jde,        kds,kde,                        &
                            ims,ime,        jms,jme,        kms,kme,                        &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,                        &
                         cmr_sfcdif,     chr_sfcdif,     cmc_sfcdif,                        &
	                 chc_sfcdif,    cmgr_sfcdif,    chgr_sfcdif,                        &
                           tr_urb2d,       tb_urb2d,       tg_urb2d,                        & !H urban
	                   tc_urb2d,       qc_urb2d,       uc_urb2d,                        & !H urban
                         xxxr_urb2d,     xxxb_urb2d,     xxxg_urb2d, xxxc_urb2d,            & !H urban
                          trl_urb3d,      tbl_urb3d,      tgl_urb3d,                        & !H urban
                           sh_urb2d,       lh_urb2d,        g_urb2d,   rn_urb2d,  ts_urb2d, & !H urban
                         psim_urb2d,     psih_urb2d,      u10_urb2d,  v10_urb2d,            & !O urban
                       GZ1OZ0_urb2d,     AKMS_URB2D,                                        & !O urban
                          th2_urb2d,       q2_urb2d,      ust_urb2d,                        & !O urban
                             declin,          hrang,                                        & !I urban
                    num_roof_layers,num_wall_layers,num_road_layers,                        & !I urban
                                dzr,            dzb,            dzg,                        & !I urban
                         cmcr_urb2d,      tgr_urb2d,     tgrl_urb3d,  smr_urb3d,            & !H urban
                        drelr_urb2d,    drelb_urb2d,    drelg_urb2d,                        & !H urban
                      flxhumr_urb2d,  flxhumb_urb2d,  flxhumg_urb2d,                        & !H urban
                             julian,          julyr,                                        & !H urban
                          frc_urb2d,    utype_urb2d,                                        & !I urban
                                chs,           chs2,           cqs2,                        & !H
                   num_urban_layers,                                                        & !I multi-layer urban
                       num_urban_hi,                                                        & !I multi-layer urban
                          trb_urb4d,      tw1_urb4d,      tw2_urb4d,  tgb_urb4d,            & !H multi-layer urban
                         tlev_urb3d,     qlev_urb3d,                                        & !H multi-layer urban
                       tw1lev_urb3d,   tw2lev_urb3d,                                        & !H multi-layer urban
                        tglev_urb3d,    tflev_urb3d,                                        & !H multi-layer urban
                        sf_ac_urb3d,    lf_ac_urb3d,    cm_ac_urb3d,                        & !H multi-layer urban
                       sfvent_urb3d,   lfvent_urb3d,                                        & !H multi-layer urban
                       sfwin1_urb3d,   sfwin2_urb3d,                                        & !H multi-layer urban
                         sfw1_urb3d,     sfw2_urb3d,      sfr_urb3d,  sfg_urb3d,            & !H multi-layer urban
                           lp_urb2d,       hi_urb2d,       lb_urb2d,  hgt_urb2d,            & !H multi-layer urban
                           mh_urb2d,     stdh_urb2d,       lf_urb2d,                        & !SLUCM
                             th_phy,            rho,          p_phy,        ust,            & !I multi-layer urban
                                gmt,         julday,          xlong,       xlat,            & !I multi-layer urban
                            a_u_bep,        a_v_bep,        a_t_bep,    a_q_bep,            & !O multi-layer urban
                            a_e_bep,        b_u_bep,        b_v_bep,                        & !O multi-layer urban
                            b_t_bep,        b_q_bep,        b_e_bep,    dlg_bep,            & !O multi-layer urban
                           dl_u_bep,         sf_bep,         vl_bep)                          !O multi-layer urban
	 
	 ENDIF 

  if(iopt_run.eq.5.and.mod(itimestep,STEPWTD).eq.0)then
           CALL wrf_debug( 100, 'calling WTABLE' )

!gmm update wtable from lateral flow and shed water to rivers

           CALL WTABLE_mmf_noahmp(num_soil_layers,xland,xice, xice_threshold, isice,        &
                                  isltyp,smoiseq,dzs,wtddt,                                 &
                                  fdepthxy,areaxy,ht,isurban,ivgtyp,                         &
                                  rivercondxy,riverbedxy,eqzwt,pexpxy,                      &
                                  smois,sh2o,smcwtdxy,zwtxy,qrfxy,deeprechxy,qspringxy,     &
                                  qslatxy,qrfsxy,qspringsxy,rechxy,                        &
                                  ids,ide, jds,jde, kds,kde,                             &
                                  ims,ime, jms,jme, kms,kme,                             &
                                  i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte )

  endif

         call seaice_noah( SEAICE_ALBEDO_OPT, SEAICE_ALBEDO_DEFAULT, SEAICE_THICKNESS_OPT, &
              &            SEAICE_THICKNESS_DEFAULT, SEAICE_SNOWDEPTH_OPT,             &
              &            SEAICE_SNOWDEPTH_MAX, SEAICE_SNOWDEPTH_MIN,                 &
              &            t_phy, qv_curr, p8w, dz8w, num_soil_layers, dt, frpcpn, sr, &
              &            glw, swdown, rainbl, snoalb, qgh, xice, xice_threshold,     &
              &            albsi, icedepth, snowsi,                                    &
              &            tslb, emiss, albedo, z0, tsk, snow, snowc, snowh,           &
              &            chs, chs2, cqs2,                                            &
              &            br, znt, lh, hfx, qfx, potevp, grdflx, qsfc, acsnow,        &
              &            acsnom, snopcx, sfcrunoff, noahres,                         &
              &            sf_urban_physics, b_t_bep, b_q_bep, rho,                    &
              &            ids,ide, jds,jde, kds,kde,                                  &
              &            ims,ime, jms,jme, kms,kme,                                  &
              &            i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte )

         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            ! LSM Returns full land/ice values, no fractional values.
            ! We return to a fractional component here.  SFLX currently hard-wires
            ! emissivity over sea ice to 0.98, the same value as over open water, so
            ! the fractional consideration doesn't have any effect for emissivity.
            DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                     albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                     emiss(i,j) = ( emiss(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        !  Weighted average of fields between ice-cover values and open-water values.
                        flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                        flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                        cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                        cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                        chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                        chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                        qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qsfc_sea(i,j) )
                        qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qgh_sea(i,j)  )
                        qz0(i,j)  = ( qz0(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qz0_sea(i,j)  )
                        hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                        qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                        lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
!save old tsk_ice
                        tsk_save(i,j)  = tsk(i,j)
                        tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        ! Compute TSK as the open-water and ice-cover average
                        tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
               SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
               SFCEXC(I,J)= CHS(I,J)
               IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
               IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
               IF(PRESENT(ACGRDFLX))ACGRDFLX(I,J)=ACGRDFLX(I,J) + GRDFLX(I,J)*DT

               ! Check that SFCDIAGS can declare these as intent(out)
               T2(I,J)  = -1.E36
               TH2(I,J) = -1.E36
               Q2(I,J)  = -1.E36
           ENDDO
           ENDDO
           
!jref: sfc diagnostics
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
!             IF (IVGTYP(I,J) == ISWATER .OR. XICE(I,J) .GE. XICE_THRESHOLD) THEN
              IF (IVGTYP(I,J) == ISWATER .OR. (IVGTYP(I,J) == ISICE .AND. XICE(I,J) .GE. XICE_THRESHOLD)) THEN
                 IF(CQS2(I,J).lt.1.E-5) then
                   Q2(I,J)=QSFC(I,J)
                 ELSE
                   Q2(I,J) = QSFC(I,J) - QFX(I,J)/(PSFC(I,J)/(R_d * TSK(I,J))*CQS2(I,J))
                 ENDIF
                 IF(CHS2(I,J).lt.1.E-5) then
                   T2(I,J) = TSK(I,J) 
                 ELSE
                   T2(I,J) = TSK(I,J) - HFX(I,J)/(PSFC(I,J)/(R_d * TSK(I,J))*CP*CHS2(I,J))
                 ENDIF
                   TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**RCP
!             ELSEIF (IVGTYP(I,J) == ISURBAN .OR. IVGTYP(I,J) == ISICE .OR. FVEGXY(I,J) == 0.0 ) THEN
              ELSEIF (IVGTYP(I,J) == ISURBAN                   .or. &
	              IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL   .or. & !urban
                      IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL  .or. & !urban
	              IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL .or. & !urban
	              (IVGTYP(I,J) == ISICE .AND. XICE(I,J) .LT. XICE_THRESHOLD)) THEN
                   Q2(I,J)  = Q2MBXY(I,J)
                   T2(I,J)  = T2MBXY(I,J)
                   TH2(I,J) = T2(i,j)*(1.E5/PSFC(i,j))**RCP
              ELSE
                 T2(I,J)  = FVEGXY(I,J)*T2MVXY(I,J) + (1.-FVEGXY(I,J))*T2MBXY(I,J)
                 Q2(I,J)  = FVEGXY(I,J)*Q2MVXY(I,J) + (1.-FVEGXY(I,J))*Q2MBXY(I,J)
                 TH2(I,J) = T2(i,j)*(1.E5/PSFC(i,j))**RCP
              ENDIF
           ENDDO
           ENDDO
           
!          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
!                     PSFC,CP,R_d,RCP,CHS,t_phy,qv_curr,ua_phys,    &
!                     ids,ide, jds,jde, kds,kde,                    &
!                     ims,ime, jms,jme, kms,kme,                    &
!             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

!jref: sfc diagnostics end

         IF(SF_URBAN_PHYSICS.eq.1) THEN
           DO j=j_start(ij),j_end(ij)                             !urban
             DO i=i_start(ij),i_end(ij)                           !urban
              IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &  !urban
                  IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL ) THEN !urban
                 Q2(I,J)  = (FVEGXY(I,J)*Q2MVXY(I,J) + (1.-FVEGXY(I,J))*Q2MBXY(I,J))*(1.-FRC_URB2D(I,J)) +   &
                             Q2_URB2D(I,J)*FRC_URB2D(I,J)
                 T2(I,J)  = (FVEGXY(I,J)*T2MVXY(I,J) + (1.-FVEGXY(I,J))*T2MBXY(I,J))*(1.-FRC_URB2D(I,J)) +   &
                             (TH2_URB2D(i,j)/((1.E5/PSFC(i,j))**RCP))*FRC_URB2D(I,J)
                 TH2(I,J) = T2(i,j)*(1.E5/PSFC(i,j))**RCP
                 U10(I,J)  = U10_URB2D(I,J)                       !urban
                 V10(I,J)  = V10_URB2D(I,J)                       !urban
                 PSIM(I,J) = PSIM_URB2D(I,J)                      !urban
                 PSIH(I,J) = PSIH_URB2D(I,J)                      !urban
                 GZ1OZ0(I,J) = GZ1OZ0_URB2D(I,J)                  !urban
                 AKHS(I,J) = CHS(I,J)                             !urban
                 AKMS(I,J) = AKMS_URB2D(I,J)                      !urban
               END IF                                             !urban
             ENDDO                                                !urban
           ENDDO                                                  !urban
         ENDIF

         IF((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.eq.3)) THEN
           DO j=j_start(ij),j_end(ij)                             !urban
             DO i=i_start(ij),i_end(ij)                           !urban
              IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &  !urban
                  IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL ) THEN !urban
                T2(I,J)   = TH_PHY(i,1,j)/((1.E5/PSFC(I,J))**RCP) !urban
                TH2(I,J) = TH_PHY(i,1,j) !urban
                Q2(I,J)   = qv_curr(i,1,j)  !urban
                U10(I,J)  = U_phy(I,1,J)                       !urban
                V10(I,J)  = V_phy(I,1,J)                       !urban
               END IF                                             !urban
             ENDDO                                                !urban
           ENDDO                                                  !urban
         ENDIF

! added RA population for WRF/Noah-CMAQ RS Consistency
! following Garland et al. (1977) and Nemitz et al., 2009
         IF ( PRESENT(RA) ) THEN
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            RA(I,J) = WSPD(I,J)/UST(I,J)**2.0
         ENDDO
         ENDDO
         END IF
!------------------------------------------------------------------

       ELSE
         CALL wrf_error_fatal('Lacking arguments for NOAHMPLSM in surface driver')
       ENDIF

     CASE (RUCLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
!           PRESENT(emiss)      .AND.  PRESENT(t2)      .AND.    &
           PRESENT(qsg)        .AND.  PRESENT(qvg)     .AND.    &
           PRESENT(qcg)        .AND.  PRESENT(soilt1)  .AND.    &
           PRESENT(tsnav)      .AND.  PRESENT(smfr3d)  .AND.    &
           PRESENT(keepfr3dflag) .AND. PRESENT(rainbl) .AND.    &
           PRESENT(dew)                                 .AND.   &
                                                      .TRUE. ) THEN

           IF( PRESENT(sr) ) THEN
               frpcpn=.true.
           ELSE
               SR = 1.
           ENDIF
           CALL wrf_debug(100,'in RUC LSM')
           DO j = j_start(ij) , j_end(ij)
              DO i = i_start(ij) , i_end(ij)
                 IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1. ) ) THEN
                    ALBBCK(I,J) = SEAICE_ALBEDO_DEFAULT
                 ENDIF
              ENDDO
           ENDDO
           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              ! The fields passed to LSMRUC need to represent the full ice values, not
              ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
              ! to a value representing only the sea-ice portion.   Albedo over open 
              ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
              DO j = j_start(ij) , j_end(ij)
                 DO i = i_start(ij) , i_end(ij)
                    IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                       ALBEDO(I,J) = (ALBEDO(I,J) - (1.-XICE(I,J))*0.08) / XICE(I,J)
                       EMISS(I,J)  = (EMISS(I,J)  - (1.-XICE(I,J))*0.98) / XICE(I,J)
! also set skin temperature to saved sea-ice portion only
                       TSK(I,J) = TSK_SAVE(I,J)
                    ENDIF
                 ENDDO
              ENDDO

              IF ( isisfc ) THEN
                 !
                 ! use surface layer routine values from the ice portion of grid point
                 !
              ELSE
                 !
                 ! don't have srfc layer routine values at this time, so just use what you have
                 ! use ice component of TSK
                 !
                 CALL get_local_ice_tsk( ims, ime, jms, jme,                   &
                                         i_start(ij), i_end(ij),               &
                                         j_start(ij), j_end(ij),               &
                                         itimestep, .false., tice2tsk_if2cold, &
                                         XICE, XICE_THRESHOLD,                 &
                                         SST, TSK, TSK_SEA, TSK_LOCAL )
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       TSK(i,j) = TSK_LOCAL(i,j)
                    ENDDO
                 ENDDO
              ENDIF
           ENDIF

           CALL LSMRUC( spp_lsm_loc,                            &
#if (EM_CORE==1)
                pattern_spp_lsm,field_sf,                       &
#endif
                dtbl,itimestep,num_soil_layers,                 &
#if (EM_CORE==1)
                lakemodel,lakemask,                             &
                graupelncv,snowncv,rainncv,                     &
#endif
                zs,rainbl,snow,snowh,snowc,sr,frpcpn,           &
                rhosnf,precipfr,                                &
                dz8w,p_phy,t_phy,qv_curr,qc_curr,rho,           & !p_phy in [pa]
                glw,gsw,emiss,chklowq,                          &
                chs,flqc,flhc,mavail,canwat,vegfra,albedo,znt,  &
                z0,snoalb, albbck, lai,                         &   !new
                mminlu, landusef, nlcat, mosaic_lu,             &
                mosaic_soil, soilctop, nscat,                   &   !new
                qsfc,qsg,qvg,qcg,dew,soilt1,tsnav,              &
                tmn,ivgtyp,isltyp,xland,                        &
                iswater,isice,xice,xice_threshold,              &
                cp ,rcp,g,xlv,stbolt,                           &
                smois,sh2o,smstav,smstot,tslb,tsk,hfx,qfx,lh,   &
                sfcrunoff,udrunoff,acrunoff,sfcexc,             &
                sfcevp,grdflx,snowfallac,acsnow,acsnom,         &
                smfr3d,keepfr3dflag,                            &
                myj,shdmin,shdmax,rdlai2d,                      &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              ! LSMRUC Returns full land/ice values, no fractional values.
              ! We return to a fractional component here.
              DO j=j_start(ij),j_end(ij)
                 DO i=i_start(ij),i_end(ij)
                    IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                       albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                       emiss(i,j)  = ( emiss(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                    ENDIF
                 ENDDO
              ENDDO
              if ( isisfc ) then
                 !
                 !  back to ice and ocean average
                 !
                 DO j=j_start(ij),j_end(ij)
                    DO i=i_start(ij),i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flhc_sea(i,j) )
                          flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flqc_sea(i,j) )
                          cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * cpm_sea(i,j)  )
                          cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * cqs2_sea(i,j) )
                          chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs2_sea(i,j) )
                          chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs_sea(i,j)  )
                          qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * QSFC_SEA(i,j) )
                          qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * qgh_sea(i,j)  )
                          hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * HFX_SEA(i,j)  )
                          qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * QFX_SEA(i,j)  )
                          lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.-XICE(i,j)) * LH_SEA(i,j)   )
!save old tsk_ice
                          tsk_save(i,j)  = tsk(i,j)
                          tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j)  )
                       ENDIF
                    ENDDO
                 ENDDO
              else
                 !
                 ! tsk back to liquid and ice average
                 !
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j) )
                       ENDIF
                    ENDDO
                 ENDDO
              endif
           ENDIF

! Compute CHS and CQS that will be used in 2-m diagnostics
            DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)
                     cqs(i,j)=flqc(i,j)/(mavail(i,j)*rho(i,kts,j))
                     chs(i,j)=flhc(i,j)/(cpm(i,j)*rho(i,kts,j) )
               ENDDO
            ENDDO

          CALL SFCDIAGS_RUCLSM(HFX,QFX,TSK,QSFC,CQS,CQS2,CHS,CHS2,T2,TH2,Q2,  &
                     T_PHY,QV_CURR,RHO,P_PHY,PSFC,SNOW,                       &
                     CP,R_d,RCP,                                              &
                     ids,ide, jds,jde, kds,kde,                               &
                     ims,ime, jms,jme, kms,kme,                               &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )

       ELSE
         CALL wrf_error_fatal('Lacking arguments for RUCLSM in surface driver')
       ENDIF

     CASE (PXLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
           PRESENT(emiss)      .AND.  PRESENT(t2)      .AND.    &
           PRESENT(rainbl) .AND.    &
                                                      .TRUE. ) THEN
          IF ( FRACTIONAL_SEAICE == 1 ) THEN

             IF ( isisfc ) THEN
                !
                ! use surface layer routine values from the ice portion of grid point
                !
             ELSE
                !
                ! don't have srfc layer routine values at this time, so just use what you have
                ! use ice component of TSK
                !
                CALL get_local_ice_tsk( ims, ime, jms, jme,                   &
                                        i_start(ij), i_end(ij),               &
                                        j_start(ij), j_end(ij),               &
                                        itimestep, .false., tice2tsk_if2cold, &
                                        XICE, XICE_THRESHOLD,                 &
                                        SST, TSK, TSK_SEA, TSK_LOCAL )
                DO j = j_start(ij) , j_end(ij)
                   DO i=i_start(ij) , i_end(ij)
                      TSK(i,j) = TSK_LOCAL(i,j)
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
          CALL wrf_debug(100,'in P-X LSM')
          CALL PXLSM(u_phy, v_phy, dz8w, qv_curr, t_phy, th_phy, rho,&
                     psfc, gsw, glw, rainbl, emiss,                  &
                     ITIMESTEP, curr_secs, num_soil_layers, DT,      &
                     anal_interval, xland, xice, albbck, albedo,     &
                     snoalb, smois, tslb, mavail,T2, Q2, qsfc,       &
                     zs, dzs, psih,                                  &
                     landusef,soilctop,soilcbot,vegfra, vegf_px,     &
                     isltyp,ra,rs,lai,imperv,canfra,nlcat,nscat,     &
                     hfx,qfx,lh,tsk,sst,znt,canwat,                  &
                     grdflx,shdmin,shdmax,                           &
                     snowc,pblh,rmol,ust,capg,dtbl,                  &
                     t2_ndg_old,t2_ndg_new,q2_ndg_old,q2_ndg_new,    &
                     sn_ndg_old, sn_ndg_new, snow, snowh,snowncv,    &
                     t2obs, q2obs,pxlsm_smois_init,pxlsm_soil_nudge, &
                     ids,ide, jds,jde, kds,kde,                      &
                     ims,ime, jms,jme, kms,kme,                      &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)
          IF ( FRACTIONAL_SEAICE == 1 ) THEN
             IF ( isisfc ) THEN
                !
                !  back to ice and ocean average
                !
                DO j = j_start(ij) , j_end(ij)
                   DO i = i_start(ij) , i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                         flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                         cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                         cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                         chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                         chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                         qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QSFC_SEA(i,j) )
                         qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QGH_SEA(i,j)  )
                         hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * HFX_SEA(i,j)  )
                         qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QFX_SEA(i,j)  )
                         lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * LH_SEA(i,j)   )
!save old tsk_ice
                         tsk_save(i,j)  = tsk(i,j)
                         tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * TSK_SEA(i,j)  )
                         pblh(i,j) = ( pblh(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PBLH_SEA(i,j) )
                      ENDIF
                   ENDDO
                ENDDO
             ELSE
                !
                ! tsk back to liquid and ice average
                !
                DO j=j_start(ij),j_end(ij)
                   DO i=i_start(ij),i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
!save old tsk_ice
                         tsk_save(i,j)  = tsk(i,j)
                         tsk(i,j)=tsk(i,j)*XICE(i,j)+(1.0-XICE(i,j))*TSK_SEA(i,j)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
              TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**RCP
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
           ENDDO
           ENDDO

       ELSE
         CALL wrf_error_fatal('Lacking arguments for P-X LSM in surface driver')
       ENDIF
#ifdef WRF_USE_CLM
!---------------------------------------------------------------------
! CLM coupling currently version 4 added by Yaqiong Lu and Jiming Jin

     CASE (CLMSCHEME)
     CALL wrf_debug(100,'in CLM')

     IF (MYJ) call wrf_error_fatal('CLM is not currently compatible with MYJ.  Please pick different PBL Schemes')

     IF (present(qv_curr) .and.  present(rainbl) .and.    &
                                                      .true. ) then

       ! print *, "itimestep = ", itimestep
      !  print *," in module_surface_driver.F :  dz8w(i,1,j) = ",dz8w(:,1,:)
         IF( PRESENT(sr) ) THEN
           frpcpn=.true.
         ENDIF
         IF ( FRACTIONAL_SEAICE == 1) THEN
            ! The fields passed to LSM need to represent the full ice values, not
            ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
            ! to a value representing only the sea-ice portion.   Albedo over open 
            ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
            DO j = j_start(ij) , j_end(ij)
               DO i = i_start(ij) , i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE.  1 ) ) THEN
                     ALBEDO(I,J) = (ALBEDO(I,J)-(1.-XICE(I,J))*0.08)/XICE(I,J)
                     EMISS(I,J) = (EMISS(I,J)-(1.-XICE(I,J))*0.98)/XICE(I,J)
                  ENDIF
               ENDDO
            ENDDO
            IF ( isisfc ) THEN
              ! Use surface layer routine values from the ice portion of grid
              ! point
            ELSE
               !    
               ! We don't have surface layer routine values at this time, so
               ! just use what we have.  Use ice component of TSK
               !    
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                        IF ( SST(i,j) .LT. 271.4 ) THEN
                           SST(i,j) = 271.4
                        ENDIF
                        TSK_SEA(i,j) = SST(i,j)
                        ! Convert TSK from our ice/water average value to value
                        ! good for solid-ice surface.
                        TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                        IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
                           TSK(i,j) = 253.15
                        ENDIF
                        IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
                           TSK(i,j) = 263.15
                        ENDIF
                     ELSE
                        TSK_SEA(i,j) = TSK(i,j)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF


       CALL wrf_debug(100,'in clmdrv')

       CALL clmdrv(dz8w,qv_curr,p8w, t_phy,tsk,                   &
                hfx,qfx,lh,grdflx,qgh,gsw,swdown,               &
                ra_sw_physics,history_interval,glw,smstav,smstot, &
                sfcrunoff,udrunoff,ivgtyp,isltyp,vegfra,        &
                albedo,znt,z0, tmn,xland,xice, emiss,           &
                snowc,qsfc,rainbl,maxpatch,                     &
                num_soil_layers,dtbl,xtime, dt,dzs,             &
                smois,tslb,snow,canwat,                         &
                chs,chs2,sh2o,snowh,                            &
                u_phy,v_phy,                                    &
                shdmin,shdmax,                                  &
                acsnom,acsnow,                                  &
                dx,xlat,xlong,ht,                               &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte, &
                inest,sf_urban_physics, nlcat                   &
!Optional urban
               ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif    &
               ,cmgr_sfcdif,chgr_sfcdif                        &
               ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d,  & !H urban
                uc_urb2d,                                       & !H urban
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d,    & !H urban
                trl_urb3d,tbl_urb3d,tgl_urb3d,                  & !H urban
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d,    & !H urban
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d,      & !O urban
                GZ1OZ0_urb2d, AKMS_URB2D,                       & !O urban
                th2_urb2d,q2_urb2d,ust_urb2d,                   & !O urban
                declin,coszen,hrang,                            & !I urban   !  by hongping Gu
                xlat_urb2d,                                     & !I urban
                num_roof_layers, num_wall_layers,               & !I urban
                num_road_layers, DZR, DZB, DZG,                 & !I urban
                FRC_URB2D, UTYPE_URB2D,                         & !I urban
                cmcr_urb2d,tgr_urb2d,tgrl_urb3d,smr_urb3d,      & ! urban
                drelr_urb2d,drelb_urb2d,drelg_urb2d,            & ! urban
                flxhumr_urb2d,flxhumb_urb2d,flxhumg_urb2d,      &
! CLM subgrids
                numc,nump,sabv,sabg,lwup,snl, &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t2clm ,    &
                t_ref2m,h2osoi_liq_s1,                 &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                q_ref2m,                                   &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,SWUPsubgrid,&
                LHsoi,LHveg,LHtran, &
                alswvisdir, alswvisdif, alswnirdir, alswnirdif,      & ! clm
                swvisdir, swvisdif, swnirdir, swnirdif               & ! clm
#ifdef CN
!CROP&CN RESTART AND OUTPUTS
                ,dyntlai,dyntsai,dyntop,dynbot &
                ,htmx,croplive,gdd1020,gdd820,gdd020,grainc,grainc_storage  &
                ,grainc_xfer,grainn,grainn_storage,grainn_xfer,days_active  &
                ,onset_flag,onset_counter,onset_gddflag,onset_fdd,onset_gdd &
                ,onset_swi,offset_flag,offset_counter,offset_fdd,offset_swi &
                ,dayl,annavg_t2m,tempavg_t2m,tempsum_potential_gpp          &
                ,annsum_potential_gpp,tempmax_retransn,annmax_retransn      &
                ,prev_leafc_to_litter,prev_frootc_to_litter,tempsum_npp     &
                ,annsum_npp,leafc,leafc_storage,leafc_xfer,frootc           &
                ,frootc_storage,frootc_xfer,livestemc,livestemc_storage     &
                ,livestemc_xfer,deadstemc,deadstemc_storage,deadstemc_xfer  &
                ,livecrootc,livecrootc_storage,livecrootc_xfer,deadcrootc   &
                ,deadcrootc_storage,deadcrootc_xfer,cpool,pft_ctrunc        &
                ,leafn,leafn_storage,leafn_xfer,frootn,frootn_storage       &
                ,frootn_xfer,livestemn,livestemn_storage,livestemn_xfer     &
                ,deadstemn,deadstemn_storage,deadstemn_xfer,livecrootn      &
                ,livecrootn_storage,livecrootn_xfer,deadcrootn              &
                ,deadcrootn_storage,deadcrootn_xfer,npool,pft_ntrunc        &
                ,gresp_storage,gresp_xfer,xsmrpool,annsum_counter           &
                ,cannsum_npp,cannavg_t2m,wf,me,mean_fire_prob,cwdc,litr1c   &
                ,litr2c,litr3c,soil1c,soil2c,soil3c,soil4c,seedc,col_ctrunc &
                ,prod10c,prod100c,cwdn,litr1n,litr2n,litr3n,soil1n,soil2n   &
                ,soil3n,soil4n,seedn,col_ntrunc,prod10n,prod100n,sminn      &
                ,totlitc,dwt_seedc_to_leaf,dwt_seedc_to_deadstem,dwt_conv_cflux &
                ,dwt_prod10c_gain,dwt_prod100c_gain,prod100c_loss,dwt_frootc_to_litr1c &
                ,dwt_frootc_to_litr2c,dwt_frootc_to_litr3c,dwt_livecrootc_to_cwdc &
                ,dwt_deadcrootc_to_cwdc,dwt_seedn_to_leaf,dwt_seedn_to_deadstem &
                ,dwt_conv_nflux,dwt_prod10n_gain,dwt_prod100n_gain,prod100n_loss &
                ,dwt_frootn_to_litr1n,dwt_frootn_to_litr2n, dwt_frootn_to_litr3n &
                ,dwt_livecrootn_to_cwdn,dwt_deadcrootn_to_cwdn,retransn &
#endif
                 )

         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE.  1.0 ) ) THEN
                     albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                     emiss(i,j) = ( emiss(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        !  Weighted average of fields between ice-cover values
                        !  and open-water values.
                        flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                        flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                        cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                        cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                        chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                        chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                        qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qsfc_sea(i,j) )
                        qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qgh_sea(i,j)  )
                        qz0(i,j)  = ( qz0(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qz0_sea(i,j)  )
                        hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                        qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                        lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
!save old tsk_ice
                        tsk_save(i,j)  = tsk(i,j)
                        tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        ! Compute TSK as the open-water and ice-cover average
                        tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
                     PSFC,CP,R_d,RCP,CHS,t_phy,qv_curr,ua_phys,    &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL

! update land variables from CLM
              IF(XLAND(I,J).LT.1.5) then
                  Q2(I,J) = sum(q_ref2m(i,1:nump(i,j),j)*wtp(i,1:nump(i,j),j))

! convert specific humidty to mixing ratio unit: kg/kg)
                  Q2(I,J) = Q2(I,J)/(1.0-Q2(I,J))

                  T2(I,J) = sum(t_ref2m(i,1:nump(i,j),j)*wtp(i,1:nump(i,j),j))
                  TH2(I,J)= T2(I,J)*(1.E5/PSFC(I,J))**RCP
              END IF
           ENDDO
           ENDDO

       ELSE
         CALL wrf_error_fatal('Lacking arguments for CLM in surface driver')
       ENDIF

! end of CLM scehme
! -------------------------------------------------------------------
#endif

     CASE (SSIBSCHEME)
     IF(PRESENT(alswvisdir))THEN
!---Fernando De Sales (fds 06/2010)--------------------------------------
     CALL wrf_debug(100,'in SSIB')
!
       IF ( FRACTIONAL_SEAICE == 1) THEN
          ! The fields passed to SSIB need to represent the full ice values, not
          ! the fractional values.  Convert ALBEDO from the blended value
          ! to a value representing only the sea-ice portion.   Albedo over open
          ! water is taken to be 0.08.
          DO j = j_start(ij) , j_end(ij)
             DO i = i_start(ij) , i_end(ij)
                IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                   ALBEDO(I,J) = (ALBEDO(I,J)-(1.-XICE(I,J))*0.08)/XICE(I,J)
                ENDIF
             ENDDO
          ENDDO
       ELSE
!  we shouldn't be here. must have fractional seaice for SSIB to work properly (fds 12/2010)
       ENDIF
!
!This stuff is not needed anymore since isisfc is always TRUE for SSIB
!Keep it for later use when code is adapted for isisfc=FALSE
!          IF ( isisfc ) THEN
!             ! Use surface layer routine values from the ice portion of grid point
!          ELSE
!             !
!             ! We don't have surface layer routine values at this time, so
!             ! just use what we have.  Use ice component of TSK
!             !
!             DO j = j_start(ij) , j_end(ij)
!                DO i = i_start(ij) , i_end(ij)
!                   IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
!                      IF ( SST(i,j) .LT. 271.4 ) THEN
!                         SST(i,j) = 271.4
!                      ENDIF
!                      TSK_SEA(i,j) = SST(i,j)
!                      ! Convert TSK from our ice/water average value to value good for solid-ice surface.
!                      TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
!                      IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
!                         TSK(i,j) = 253.15
!                      ENDIF
!                      IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
!                         TSK(i,j) = 263.15
!                      ENDIF
!                   ELSE
!                      TSK_SEA(i,j) = TSK(i,j)
!                   ENDIF
!                ENDDO
!             ENDDO
!          ENDIF
!
       day=float(int(julian_in+0.01))+1.
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)

!check land mask and land-use map !fds (02/2012)
!       IF(itimestep .EQ. 1 ) THEN
!          IF(IVGTYP(i,j).NE.ISWATER)THEN
!            XLAND(I,J)=1.0
!          ELSE
!            XLAND(I,J)=2.0
!          ENDIF
!          IF (IVGTYP(I,J).LE.0 .AND. XLAND(I,J).NE.ISWATER ) IVGTYP(I,J) = 7.0
!       ENDIF

       IF(XLAND(I,J).LT.1.5) THEN ! seaice and land points

           CLOUDFRAC=0.
           IF(PRESENT(CLDFRA))THEN
           DO K=KMS,KME
             CLOUDFRAC=AMAX1(CLOUDFRAC,AMIN1(CLDFRA(I,K,J),1.0))
           ENDDO
           ENDIF

         IF (XICE(I,J) .GE. XICE_THRESHOLD) THEN !sea ice points only

           CALL ssib_seaice                                                        &
                    ( i, j, DTBL, itimestep, xlat_urb2d(i,j), coszen(i,j),         &
                      rainncv(i,j), raincv(i,j), glw(i,j), dz8w(i,1,j),            &
                      smois(i,1,j), smois(i,2,j), smois(i,3,j),                    &
                      tslb(i,1,j), tslb(i,2,j), tslb(i,3,j),                       &
                      snow(i,j), sfcrunoff(i,j), xice_save(i,j),                   &
                      u_phytmp(i,1,j),v_phytmp(i,1,j),qv_curr(i,1,j),t_phy(i,1,j), &
                      p_phy(i,1,j), psfc(i,j),                                     &
                      swdown(i,j), canwat(i,j),                                    &
                 alswvisdir(i,j),alswvisdif(i,j),alswnirdir(i,j),alswnirdif(i,j),  &
                      swvisdir(i,j), swvisdif(i,j), swnirdir(i,j), swnirdif(i,j),  &
                      hfx(i,j), lh(i,j), grdflx(i,j), qfx(i,j), tsk(i,j),          &
                      ust(i,j), ssib_br(i,j), ssib_fm(i,j), ssib_fh(i,j), ssib_cm(i,j), &
                      ssib_lhf(i,j), ssib_shf(i,j), ssib_ghf(i,j),                 &
                      ssib_sdn(i,j), ssib_sup(i,j), ssib_ldn(i,j), ssib_lup(i,j),  &
                      ssib_wat(i,j),                                               &
                                     ssib_z00(i,j), ssib_veg(i,j),                 &
                      day, cloudfrac, q2(i,j), t2(i,j), albedo(i,j), uv10,         &
                      ra_sw_physics,xice_threshold                                 &
                                                                                   )
         ELSE  !land points only (including land ice)

           CALL ssib( i, j, DTBL, itimestep, xlat_urb2d(i,j), coszen(i,j),        &
                     rainncv(i,j), raincv(i,j), glw(i,j), dz8w(i,1,j),            &
                     smois(i,1,j), smois(i,2,j), smois(i,3,j),                    &
                     tslb(i,1,j), tslb(i,2,j), tslb(i,3,j),                       &
                     snow(i,j), sfcrunoff(i,j),                                   &
                     u_phytmp(i,1,j),v_phytmp(i,1,j),qv_curr(i,1,j),t_phy(i,1,j), &
                     p_phy(i,1,j), psfc(i,j), ivgtyp(i,j),                        &
                     swdown(i,j), canwat(i,j),                                    &
                alswvisdir(i,j),alswvisdif(i,j),alswnirdir(i,j),alswnirdif(i,j),  &
                     swvisdir(i,j), swvisdif(i,j), swnirdir(i,j), swnirdif(i,j),  &
                     hfx(i,j), lh(i,j), grdflx(i,j), qfx(i,j), tsk(i,j),          &
                     ust(i,j), ssib_br(i,j), ssib_fm(i,j), ssib_fh(i,j), ssib_cm(i,j), &
                     ssib_lhf(i,j), ssib_shf(i,j), ssib_ghf(i,j), ssib_egs(i,j),  & 
                     ssib_eci(i,j), ssib_ect(i,j), ssib_egi(i,j), ssib_egt(i,j),  &
                     ssib_sdn(i,j), ssib_sup(i,j), ssib_ldn(i,j), ssib_lup(i,j),  &
                     ssib_wat(i,j), ssib_shc(i,j), ssib_shg(i,j), ssib_lai(i,j),  &
                     ssib_vcf(i,j), ssib_z00(i,j), ssib_veg(i,j), ssibxdd(i,j),   &
                     isnow(i,j), swe(i,j), snowden(i,j), snowdepth(i,j),tkair(i,j),  &
                     dzo1(i,j),  wo1(i,j),   tssn1(i,j), tssno1(i,j), bwo1(i,j), bto1(i,j),  &
                     cto1(i,j), fio1(i,j),    flo1(i,j),   bio1(i,j), blo1(i,j),  ho1(i,j),  &
                     dzo2(i,j),  wo2(i,j),   tssn2(i,j), tssno2(i,j), bwo2(i,j), bto2(i,j),  &
                     cto2(i,j), fio2(i,j),    flo2(i,j),   bio2(i,j), blo2(i,j),  ho2(i,j),  &
                     dzo3(i,j),  wo3(i,j),   tssn3(i,j), tssno3(i,j), bwo3(i,j), bto3(i,j),  &
                     cto3(i,j), fio3(i,j),    flo3(i,j),   bio3(i,j), blo3(i,j),  ho3(i,j),  &
                     dzo4(i,j),  wo4(i,j),   tssn4(i,j), tssno4(i,j), bwo4(i,j), bto4(i,j),  &
                     cto4(i,j), fio4(i,j),    flo4(i,j),   bio4(i,j), blo4(i,j),  ho4(i,j),  &
                     day, cloudfrac, q2(i,j), t2(i,j), albedo(i,j), uv10,          &
                     ra_sw_physics, mminlu                                        &
                                                                                  )
         ENDIF
!
         BR(i,j)=ssib_br(i,j)
         ZNT(i,j) = ssib_z00(i,j)
         SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
         t2(i,j) = tsk(i,j)  !keep this
         IF (itimestep .ne. 1) THEN
           ZDIFF=(0.5*dz8w(i,1,j))-SSiBXDD(I,J)
           IF(ZDIFF.LE.ZNT(I,J)) ZDIFF=ZNT(I,J)+0.2
           GZ1OZ0(I,J)=ALOG(ZDIFF/ZNT(I,J))
         ENDIF
         IF (XICE(I,J) .GE. XICE_THRESHOLD) THEN
           snowh(i,j) = 0.0
         ELSE
           snowh(i,j) = snowdepth(i,j)
         ENDIF
         U10(i,j) = UV10*u_phytmp(i,1,j)/SQRT(u_phytmp(i,1,j)**2+v_phytmp(i,1,j)**2)
         V10(i,j) = UV10*v_phytmp(i,1,j)/SQRT(u_phytmp(i,1,j)**2+v_phytmp(i,1,j)**2)
!        Overwrite WSPD to remove convective velocity (wspd=wspd1 in YSU)
!        WSPD(I,J)=sqrt( u_phytmp(i,1,j)*u_phytmp(i,1,j) +      &
!                        v_phytmp(i,1,j)*v_phytmp(i,1,j) ) + 1.e-9
!
       ENDIF
!
       ENDDO
       ENDDO
!
       IF ( FRACTIONAL_SEAICE == 1 ) THEN
          ! SSIB_seaice returns full land/ice albedo values, no fractional values.
          ! We return to a fractional component here. 
          DO j=j_start(ij),j_end(ij)
             DO i=i_start(ij),i_end(ij)
                IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                   albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                ENDIF
             ENDDO
          ENDDO
!
          IF ( isisfc ) THEN
             DO j=j_start(ij),j_end(ij)
                DO i=i_start(ij),i_end(ij)
                   IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                      !  Weighted average of fields between ice-cover values and open-water values.
                      hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                      qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                      lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
!save old tsk_ice
                      tsk_save(i,j)  = tsk(i,j)
                      tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                   ENDIF
                ENDDO
             ENDDO
          ELSE
             DO j = j_start(ij) , j_end(ij)
                DO i = i_start(ij) , i_end(ij)
                   IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                      ! Compute TSK as the open-water and ice-cover average
                      tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                   ENDIF
                ENDDO
             ENDDO
          ENDIF
       ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SSIB in surface driver')
       ENDIF
!end ssib
!-------------------------------------------------------------------

     CASE DEFAULT

       IF ( itimestep .eq. 1 ) THEN
       WRITE( message , * ) &
        'No land surface physics option is used: sf_surface_physics = ', sf_surface_physics
        CALL wrf_message ( message )
       ENDIF

     END SELECT sfc_select

     ENDDO
     !$OMP END PARALLEL DO

 430 CONTINUE

#if ( EM_CORE==1)
   IF (sf_ocean_physics .EQ. OMLSCHEME .or. sf_ocean_physics .EQ. PWP3DSCHEME) THEN
! simple ocean mixed layer model based Pollard, Rhines and Thompson (1973)
     CALL wrf_debug( 100, 'Call OCEANML' )
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
        CALL ocean_driver(tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy, &
                     tmoml,f,g,oml_gamma,                         &
                     xland,hfx,lh,tsk,gsw,glw,emiss,              &
                     dtbl,STBOLT,oml_relaxation_time,             &
                     ids,ide, jds,jde, kds,kde,                   &
                     ims,ime, jms,jme, kms,kme,                   &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte, & 
                     sf_ocean_physics,okms, okme,                 & !cyl
                     om_tmp,om_s,om_u, om_v,  om_depth, om_ml,    & !cyl
                     om_lat, om_lon,                              & !cyl
                     QFX,                                         & !cyl 
                     rdx, rdy, msfu, msfv, msft,xtime,            & !cyl 
                     om_tini,om_sini,id,omdt,                     & !cyl 
                     itimestep )                                    !cyl
     ENDDO
     !$OMP END PARALLEL DO
   ENDIF
#endif
! adding a lake model -- 07/02/2010
   IF ( LakeModel == 1 ) THEN
 
      CALL wrf_debug( 100, 'Call LakeModel' )
 
      DO ij = 1 , num_tiles
 
         CALL Lake(  t_phy        ,p8w            ,dz8w         ,qv_curr         ,&  !i
                     u_phy        ,v_phy          , glw         ,emiss           ,&
                     rainbl       ,dtbl           ,swdown       ,albedo          ,&
                     xlat_urb2d   ,z_lake3d       ,dz_lake3d    ,lakedepth2d     ,&
                     watsat3d     ,csol3d         ,tkmg3d       ,tkdry3d         ,&
                     tksatu3d     ,ivgtyp         ,ht           ,xland           ,&
                     iswater      ,xice           ,xice_threshold, lake_min_elev    ,&
                     ids          ,ide            ,jds          ,jde             ,&
                     kds          ,kde            ,ims          ,ime             ,&
                     jms          ,jme            ,kms          ,kme             ,&
                     i_start(ij)  ,i_end(ij)      ,j_start(ij)  ,j_end(ij)       ,&
                     kts          ,kte                                           ,&
                     h2osno2d     ,snowdp2d       ,snl2d        ,z3d             ,&  !h
                     dz3d         ,zi3d           ,h2osoi_vol3d ,h2osoi_liq3d    ,&
                     h2osoi_ice3d ,t_grnd2d       ,t_soisno3d   ,t_lake3d        ,&
                     savedtke12d  ,lake_icefrac3d                                ,&
#if ( EM_CORE==1)
  !                   lakemask  ,lakeflag                                         ,&
                     lakemask                                           ,&
#endif
                     hfx          ,lh             ,grdflx       ,tsk             ,&  !o
                     qfx          ,t2             ,th2          ,q2 )
 
 
      ENDDO
 
   ENDIF

! Reset RAINBL in mm (Accumulation between PBL calls)

     IF ( PRESENT( rainbl ) ) THEN
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            RAINBL(i,j) = 0.
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO
     ENDIF

! Limit Q2 diagnostic to no more than 5 per cent higher than lowest level value
!     This prevents unrealistic values when QFX is not mostly surface flux 
!          because calculation is based on surface flux only
!     Problems occurred in transition periods and weak winds and vegetation source
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            IF (XLAND(I,J).LT.1.5) THEN
            Q2(i,j) = MIN(Q2(i,j),1.05*QV_CURR(i,1,j))
            END IF
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

     IF( PRESENT(slope_rad).AND. radiation )THEN
! topographic slope effects removed from SWDOWN and GSW here for output
       IF (slope_rad .EQ. 1) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
         IF(SWNORM(I,J) .GT. 1.E-3)THEN  ! daytime
            SWSAVE = SWDOWN(i,j)
! SWDOWN contains unaffected SWDOWN in output
            SWDOWN(i,j) = SWNORM(i,j)
! SWNORM contains slope-affected SWDOWN in output
            SWNORM(i,j) = SWSAVE
            GSW(i,j) = GSWSAVE(i,j)
         ENDIF
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

       ENDIF
     ENDIF

   ENDIF

   END SUBROUTINE surface_driver

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   subroutine myjsfc_seaice_wrapper(ITIMESTEP,HT,DZ,      &
        &     PMID,PINT,TH,T,QV,QC,U,V,Q2,                &
        &     TSK,QSFC,THZ0,QZ0,UZ0,VZ0,                  &
        &     LOWLYR,XLAND,IVGTYP,ISURBAN,IZ0TLND,        &
        &     TICE2TSK_IF2COLD,                           &  ! Extra for wrapper
        &     XICE_THRESHOLD,                             &  ! Extra for wrapper
        &     XICE,SST,                                   &  ! Extra for wrapper
        &     CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,       &  ! Extra for wrapper
        &     FLHC_SEA, FLQC_SEA, QSFC_SEA,               &  ! Extra for wrapper
        &     QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA,         &  ! Extra for wrapper
        &     FLX_LH_SEA, TSK_SEA,                        &  ! Extra for wrapper
        &     USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL,          &
        &     AKHS,AKMS,                                  &
        &     BR,                                         &
        &     CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,     &
        &     QGH,CPM,CT,                                 &
        &     U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR,          &
        &     P1000,                                        &
        &     IDS,IDE,JDS,JDE,KDS,KDE,                        &
        &     IMS,IME,JMS,JME,KMS,KME,                        &
        &     ITS,ITE,JTS,JTE,KTS,KTE )
!     USE module_model_constants
     USE module_sf_myjsfc

     IMPLICIT NONE

     INTEGER,                                INTENT(IN)    :: ITIMESTEP
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: HT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: DZ
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PMID
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PINT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: TH
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: T
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QV
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QC
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: U
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: V
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: Q2   ! Q2 is TKE?

     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: TSK
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT)    :: TSK

     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QSFC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: THZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: UZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: VZ0
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: LOWLYR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XLAND
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: IVGTYP
     INTEGER                                               :: ISURBAN
     INTEGER                                               :: IZ0TLND
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XICE       ! Extra for wrapper
     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: BR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QZ0_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSFC_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH_SEA ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSK_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: USTAR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: ZNT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: Z0BASE
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: PBLH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: MAVAIL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: RMOL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKMS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: U10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: V10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: T02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: PSHLTR
     REAL,                                   INTENT(IN)    :: P1000
     REAL,                                   INTENT(IN)    :: XICE_THRESHOLD
     LOGICAL,                                INTENT(IN)    :: TICE2TSK_IF2COLD
     INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,       &
          &                IMS,IME,JMS,JME,KMS,KME,       &
          &                ITS,ITE,JTS,JTE,KTS,KTE


     ! Local
     INTEGER :: i
     INTEGER :: j
     REAL, DIMENSION( ims:ime, jms:jme ) :: ct_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: u10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: v10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: t02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: tshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: qshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: thz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: uz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: vz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: ustar_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pblh_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: rmol_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akhs_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akms_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: xland_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: mavail_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: znt_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: z0base_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: br_sea

     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: THZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: UZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: VZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: USTAR_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKMS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_LOCAL
     REAL :: PSFC

     ! Set things up for the frozen-surface call to myjsfc
     ! Is SST local here, or are the changes to be fed back to the calling routines?

     ! We want a TSK valid for the ice-covered regions of the grid cell.

     CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )
     DO j = JTS , JTE
        DO i = ITS , ITE
           TSK(i,j) = TSK_LOCAL(i,j)
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN

              ! Over fractional sea-ice points, back out an ice portion of QSFC as well.
              ! QSFC_SEA calculation as done in myjsfc for open water points
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(i,j) = PQ0SEA/PSFC*EXP(A2S*(TSK(i,j)-A3S)/(TSK(i,j)-A4S))
              QSFC(i,j) = QSFC(i,j) - (1.0-XICE(i,j)) * QSFC_SEA(i,j) / XICE(i,j)
!
              HFX_SEA(i,j)  = HFX(i,j)
              QFX_SEA(i,j)  = QFX(i,j)
              FLX_LH_SEA(i,j)   = FLX_LH(i,j)
           ENDIF
        ENDDO
     ENDDO

!
! frozen ocean call for sea ice points
!

! Strictly INTENT(IN) to MYJSFC, should be unchanged by call.

     ! DZ
     ! HT
     ! LOWLYR
     ! MAVAIL
     ! PINT
     ! PMID
     ! QC
     ! QV
     ! Q2
     ! T
     ! TH
     ! TSK
     ! U
     ! V
     ! XLAND
     ! Z0BASE

! INTENT (INOUT),  updated by MYJSFC.  Values will need to be saved before the first call to MYJSFC, so that
! the second call to MYJSFC does not double-count the effect.

     ! Save INTENT(INOUT) variables before the frozen-water/true-land call to MYJSFC:
     QSFC_HOLD  = QSFC
     QZ0_HOLD   = QZ0
     THZ0_HOLD  = THZ0
     UZ0_HOLD   = UZ0
     VZ0_HOLD   = VZ0
     USTAR_HOLD = USTAR
     ZNT_HOLD   = ZNT
     PBLH_HOLD  = PBLH
     RMOL_HOLD  = RMOL
     AKHS_HOLD  = AKHS
     AKMS_HOLD  = AKMS

! Strictly INTENT(OUT):  Set by MYJSFC

     ! CHS
     ! CHS2
     ! CPM
     ! CQS2
     ! CT
     ! FLHC
     ! FLQC
     ! FLX_LH
     ! HFX
     ! PSHLTR
     ! QFX
     ! QGH
     ! QSHLTR
     ! Q02
     ! Q10
     ! TH02
     ! TH10
     ! TSHLTR
     ! T02
     ! U10
     ! V10

     ! Frozen-water/true-land call.
     CALL MYJSFC ( ITIMESTEP, HT, DZ,                              &  ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,            &  ! I,I,I,I,I,I,I,I,I,
          &        TSK, QSFC, THZ0, QZ0, UZ0, VZ0,                 &  ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND, IVGTYP, ISURBAN, IZ0TLND,        &  ! I,I,I,I,I
          &        USTAR, ZNT, Z0BASE, PBLH, MAVAIL, RMOL,         &  ! IO,IO,I,IO,I,IO,
          &        AKHS, AKMS,                                     &  ! IO,IO,
          &        BR,                                             &  ! O
          &        CHS, CHS2, CQS2, HFX, QFX, FLX_LH, FLHC, FLQC,  &  ! O,O,O,0,0,0,0,0,
          &        QGH, CPM, CT, U10, V10, T02,                    &  ! 0,0,0,0,0,0,
          &        TH02, TSHLTR, TH10, Q02,                        &  ! 0,0,0,0,
          &        QSHLTR, Q10, PSHLTR,                            &  ! 0,0,0,
          &        P1000,                                        &  ! I
          &        ids,ide, jds,jde, kds,kde,                      &
          &        ims,ime, jms,jme, kms,kme,                      &
          &        its,ite, jts,jte, kts,kte    )

     ! Set up things for the open ocean call.
     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  = 1.
              ZNT_SEA(I,J) = 0.0001
              Z0BASE_SEA(I,J) = ZNT_SEA(I,J)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(I,J) = PQ0SEA/PSFC*EXP(A2S*(TSK_SEA(i,j)-A3S)/(TSK_SEA(i,j)-A4S))
           ELSE
              ! This should be a land point or a true open water point
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)    = ZNT_HOLD(I,J)
              Z0BASE_SEA(I,J) = Z0BASE(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
              QSFC_SEA(i,j) = QSFC_HOLD(i,j)
           ENDIF
        ENDDO
     ENDDO

     QZ0_SEA  = QZ0_HOLD
     THZ0_SEA = THZ0_HOLD
     UZ0_SEA  = UZ0_HOLD
     VZ0_SEA  = VZ0_HOLD
     USTAR_SEA = USTAR_HOLD
     PBLH_SEA = PBLH_HOLD
     RMOL_SEA = RMOL_HOLD
     AKHS_SEA = AKHS_HOLD
     AKMS_SEA = AKMS_HOLD

!
! open water call
!
     CALL MYJSFC ( ITIMESTEP, HT, DZ,                                                          & ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,                                        & ! I,I,I,I,I,I,I,I,I,
          &        TSK_SEA, QSFC_SEA, THZ0_SEA, QZ0_SEA, UZ0_SEA, VZ0_SEA,                     & ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND_SEA, IVGTYP, ISURBAN, IZ0TLND,                                & ! I,I,I,I,I,
          &        USTAR_SEA, ZNT_SEA, Z0BASE_SEA, PBLH_SEA, MAVAIL_SEA, RMOL_SEA,             & ! IO,IO,I,IO,I,IO,
          &        AKHS_SEA, AKMS_SEA,                                                         & ! IO,IO,
          &        BR_SEA,                                                                     & ! dummy space holder
          &        CHS_SEA, CHS2_SEA, CQS2_SEA, HFX_SEA, QFX_SEA, FLX_LH_SEA, FLHC_SEA,        & ! 0,0,0,0,0,0,0,
          &        FLQC_SEA, QGH_SEA, CPM_SEA, CT_SEA, U10_SEA, V10_SEA, T02_SEA, TH02_SEA,    & ! 0,0,0,0,0,0,0,0,
          &        TSHLTR_SEA, TH10_SEA, Q02_SEA, QSHLTR_SEA, Q10_SEA, PSHLTR_SEA,             & ! 0,0,0,0,0,0,
          &        p1000,                                                                    & ! I
          &        ids,ide, jds,jde, kds,kde,                                                  &
          &        ims,ime, jms,jme, kms,kme,                                                  &
          &        its,ite, jts,jte, kts,kte    )

!
! Scale the appropriate terms between open-water values and ice-covered values
!

     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Over sea-ice points, blend the results.

              ! INTENT(OUT) from MYJSFC
              ! CHS  wait
              ! CHS2 wait
              ! CPM  wait
              ! CQS2 wait
              CT(i,j)     = CT(i,j)     * XICE(i,j) + (1.0-XICE(i,j)) * CT_SEA (i,j)
              ! FLHC(i,j)   = FLHC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLHC_SEA (i,j)
              ! FLQC(i,j)   = FLQC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLQC_SEA (i,j)
              ! FLX_LH wait
              ! HFX  wait
              PSHLTR(i,j) = PSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * PSHLTR_SEA(i,j)
              ! QFX  wait
              ! QGH  wait
              QSHLTR(i,j) = QSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * QSHLTR_SEA(i,j)
              Q02(i,j)    = Q02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q02_SEA(i,j)
              Q10(i,j)    = Q10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q10_SEA(i,j)
              TH02(i,j)   = TH02(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH02_SEA(i,j)
              TH10(i,j)   = TH10(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH10_SEA(i,j)
              TSHLTR(i,j) = TSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * TSHLTR_SEA(i,j)
              T02(i,j)    = T02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * T02_SEA(i,j)
              U10(i,j)    = U10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * U10_SEA(i,j)
              V10(i,j)    = V10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * V10_SEA(i,j)

              ! INTENT(INOUT):  updated by MYJSFC
              ! QSFC:  wait
              THZ0(i,j)   = THZ0(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * THZ0_SEA(i,j)
              ! qz0 wait
              UZ0(i,j)    = UZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * UZ0_SEA(i,j)
              VZ0(i,j)    = VZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * VZ0_SEA(i,j)
              USTAR(i,j)  = USTAR(i,j)  * XICE(i,j) + (1.0-XICE(i,j)) * USTAR_SEA(i,j)
              ! ZNT wait
              PBLH(i,j)   = PBLH(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * PBLH_SEA(i,j)
              RMOL(i,j)   = RMOL(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * RMOL_SEA(i,j)
              AKHS(i,j)   = AKHS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKHS_SEA(i,j)
              AKMS(i,j)   = AKMS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKMS_SEA(i,j)

              !         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
           ELSE
              ! We're not over sea ice.  Take the results from the first call.
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE myjsfc_seaice_wrapper

!------------------------------------------------------------------------

 subroutine qnsesfc_seaice_wrapper(ITIMESTEP,HT,DZ,      &
        &     PMID,PINT,TH,T,QV,QC,U,V,Q2,                &
        &     TSK,QSFC,THZ0,QZ0,UZ0,VZ0,                  &
        &     LOWLYR,XLAND,       &
        &     TICE2TSK_IF2COLD,                           &  ! Extra for wrapper
        &     XICE_THRESHOLD,                             &  ! Extra for wrapper
        &     XICE,SST,                                   &  ! Extra for wrapper
        &     CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,       &  ! Extra for wrapper
        &     FLHC_SEA, FLQC_SEA, QSFC_SEA,               &  ! Extra for wrapper
        &     QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA,         &  ! Extra for wrapper
        &     FLX_LH_SEA, TSK_SEA,                        &  ! Extra for wrapper
        &     USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL,          &
        &     AKHS,AKMS,                                  &
        &     BR,                                         &
        &     CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,     &
        &     QGH,CPM,CT,                                 &
        &     U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR,          &
        &     IDS,IDE,JDS,JDE,KDS,KDE,                        &
        &     IMS,IME,JMS,JME,KMS,KME,                        &
        &     ITS,ITE,JTS,JTE,KTS,KTE,SCM_FORCE_FLUX )
!     USE module_model_constants
     USE module_sf_qnsesfc

     IMPLICIT NONE

     INTEGER,                                INTENT(IN)    :: ITIMESTEP
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: HT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: DZ
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PMID
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PINT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: TH
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: T
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QV
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QC
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: U
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: V
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: Q2   ! Q2 is TKE?

     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: TSK
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT)    :: TSK

     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QSFC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: THZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: UZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: VZ0
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: LOWLYR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XLAND
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XICE       ! Extra for wrapper
     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: BR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QZ0_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSFC_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH_SEA ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSK_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: USTAR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: ZNT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: Z0BASE
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: PBLH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: MAVAIL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: RMOL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKMS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: U10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: V10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: T02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: PSHLTR
     REAL,                                   INTENT(IN)    :: XICE_THRESHOLD
     LOGICAL,                                INTENT(IN)    :: TICE2TSK_IF2COLD
     INTEGER,                                INTENT(IN)    :: SCM_FORCE_FLUX
     INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,       &
          &                IMS,IME,JMS,JME,KMS,KME,       &
          &                ITS,ITE,JTS,JTE,KTS,KTE


     ! Local
     INTEGER :: i
     INTEGER :: j
     REAL, DIMENSION( ims:ime, jms:jme ) :: ct_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: u10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: v10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: t02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: tshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: qshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: thz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: uz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: vz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: ustar_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pblh_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: rmol_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akhs_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akms_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: xland_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: mavail_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: znt_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: z0base_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: br_sea

     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: THZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: UZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: VZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: USTAR_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKMS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_LOCAL
     REAL :: PSFC

     ! Set things up for the frozen-surface call to qnsesfc

     ! We want a TSK valid for the ice-covered regions of the grid cell.

     CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )
     DO j = JTS , JTE
        DO i = ITS , ITE
           TSK(i,j) = TSK_LOCAL(i,j)
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN

              ! Over fractional sea-ice points, back out an ice portion of QSFC as well.
              ! QSFC_SEA calculation as done in qnsesfc for open water points
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(i,j) = PQ0SEA/PSFC*EXP(A2S*(TSK(i,j)-A3S)/(TSK(i,j)-A4S))
              QSFC(i,j) = QSFC(i,j) - (1.0-XICE(i,j)) * QSFC_SEA(i,j) / XICE(i,j)
!
              HFX_SEA(i,j)  = HFX(i,j)
              QFX_SEA(i,j)  = QFX(i,j)
              FLX_LH_SEA(i,j)   = FLX_LH(i,j)
           ENDIF
        ENDDO
     ENDDO

!
! frozen ocean call for sea ice points
!

! Strictly INTENT(IN) to QNSESFC, should be unchanged by call.

     ! DZ
     ! HT
     ! LOWLYR
     ! MAVAIL
     ! PINT
     ! PMID
     ! QC
     ! QV
     ! Q2
     ! T
     ! TH
     ! TSK
     ! U
     ! V
     ! XLAND
     ! Z0BASE

! INTENT (INOUT),  updated by QNSESFC.  Values will need to be saved before the first call to QNSESFC, so that
! the second call to QNSESFC does not double-count the effect.

     ! Save INTENT(INOUT) variables before the frozen-water/true-land call to QNSESFC:
     QSFC_HOLD  = QSFC
     QZ0_HOLD   = QZ0
     THZ0_HOLD  = THZ0
     UZ0_HOLD   = UZ0
     VZ0_HOLD   = VZ0
     USTAR_HOLD = USTAR
     ZNT_HOLD   = ZNT
     PBLH_HOLD  = PBLH
     RMOL_HOLD  = RMOL
     AKHS_HOLD  = AKHS
     AKMS_HOLD  = AKMS

! Strictly INTENT(OUT):  Set by QNSESFC

     ! CHS
     ! CHS2
     ! CPM
     ! CQS2
     ! CT
     ! FLHC
     ! FLQC
     ! FLX_LH
     ! HFX
     ! PSHLTR
     ! QFX
     ! QGH
     ! QSHLTR
     ! Q02
     ! Q10
     ! TH02
     ! TH10
     ! TSHLTR
     ! T02
     ! U10
     ! V10

     ! Frozen-water/true-land call.
     CALL QNSESFC ( ITIMESTEP, HT, DZ,                              &  ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,            &  ! I,I,I,I,I,I,I,I,I,
          &        TSK, QSFC, THZ0, QZ0, UZ0, VZ0,                 &  ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND,      &  ! I,I
          &        USTAR, ZNT, Z0BASE, PBLH, MAVAIL, RMOL,         &  ! IO,IO,I,IO,I,IO,
          &        AKHS, AKMS,                                     &  ! IO,IO,
          &        BR,                                             &  ! O
          &        CHS, CHS2, CQS2, HFX, QFX, FLX_LH, FLHC, FLQC,  &  ! O,O,O,0,0,0,0,0,
          &        QGH, CPM, CT, U10, V10,T02,TH02,                    &  ! 0,0,0,0,0,0,0
          &        TSHLTR, TH10, Q02,                    &  ! 0,0,0
          &        QSHLTR, Q10, PSHLTR,                            &  ! 0,0,0,
          &        ids,ide, jds,jde, kds,kde,                      &
          &        ims,ime, jms,jme, kms,kme,                      &
          &        its,ite, jts,jte, kts,kte, SCM_FORCE_FLUX    )

     ! Set up things for the open ocean call.
     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  = 1.
              ZNT_SEA(I,J) = 0.0001
              Z0BASE_SEA(I,J) = ZNT_SEA(I,J)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(I,J) = PQ0SEA/PSFC*EXP(A2S*(TSK_SEA(i,j)-A3S)/(TSK_SEA(i,j)-A4S))
           ELSE
              ! This should be a land point or a true open water point
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)    = ZNT_HOLD(I,J)
              Z0BASE_SEA(I,J) = Z0BASE(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
              QSFC_SEA(i,j) = QSFC_HOLD(i,j)
           ENDIF
        ENDDO
     ENDDO

     QZ0_SEA  = QZ0_HOLD
     THZ0_SEA = THZ0_HOLD
     UZ0_SEA  = UZ0_HOLD
     VZ0_SEA  = VZ0_HOLD
     USTAR_SEA = USTAR_HOLD
     PBLH_SEA = PBLH_HOLD
     RMOL_SEA = RMOL_HOLD
     AKHS_SEA = AKHS_HOLD
     AKMS_SEA = AKMS_HOLD

!
! open water call
!
     CALL QNSESFC ( ITIMESTEP, HT, DZ,                                                          & ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,                                        & ! I,I,I,I,I,I,I,I,I,
          &        TSK_SEA, QSFC_SEA, THZ0_SEA, QZ0_SEA, UZ0_SEA, VZ0_SEA,                     & ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND_SEA,                             & ! I,I,
          &        USTAR_SEA, ZNT_SEA, Z0BASE_SEA, PBLH_SEA, MAVAIL_SEA, RMOL_SEA,             & ! IO,IO,I,IO,I,IO,
          &        AKHS_SEA, AKMS_SEA,                                                         & ! IO,IO,
          &        BR_SEA,                                                                     & ! dummy space holder
          &        CHS_SEA, CHS2_SEA, CQS2_SEA, HFX_SEA, QFX_SEA, FLX_LH_SEA, FLHC_SEA,        & ! 0,0,0,0,0,0,0,
          &        FLQC_SEA, QGH_SEA, CPM_SEA, CT_SEA, U10_SEA, V10_SEA,T02_SEA,TH02_SEA,   & ! 0,0,0,0,0,0,0,0
          &        TSHLTR_SEA, TH10_SEA, Q02_SEA, QSHLTR_SEA, Q10_SEA, PSHLTR_SEA,             & ! 0,0,0,0,0,0
          &        ids,ide, jds,jde, kds,kde,                                                  &
          &        ims,ime, jms,jme, kms,kme,                                                  &
          &        its,ite, jts,jte, kts,kte, SCM_FORCE_FLUX    )

!
! Scale the appropriate terms between open-water values and ice-covered values
         

     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Over sea-ice points, blend the results.

              ! INTENT(OUT) from QNSESFC
              ! CHS  wait
              ! CHS2 wait
              ! CPM  wait
              ! CQS2 wait
              CT(i,j)     = CT(i,j)     * XICE(i,j) + (1.0-XICE(i,j)) * CT_SEA (i,j)
              ! FLHC(i,j)   = FLHC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLHC_SEA (i,j)
              ! FLQC(i,j)   = FLQC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLQC_SEA (i,j)
              ! FLX_LH wait
              ! HFX  wait
              PSHLTR(i,j) = PSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * PSHLTR_SEA(i,j)
              ! QFX  wait
              ! QGH  wait
              QSHLTR(i,j) = QSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * QSHLTR_SEA(i,j)
              Q10(i,j)    = Q10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q10_SEA(i,j)
              Q02(i,j)    = Q02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q02_SEA(i,j)
              TH10(i,j)   = TH10(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH10_SEA(i,j)
               TH02(i,j)   = TH02(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH02_SEA(i,j)
              TSHLTR(i,j) = TSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * TSHLTR_SEA(i,j)
              T02(i,j)    = T02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * T02_SEA(i,j)
              U10(i,j)    = U10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * U10_SEA(i,j)
              V10(i,j)    = V10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * V10_SEA(i,j)

              ! INTENT(INOUT):  updated by QNSESFC
              ! QSFC:  wait
              THZ0(i,j)   = THZ0(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * THZ0_SEA(i,j)
              ! qz0 wait
              UZ0(i,j)    = UZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * UZ0_SEA(i,j)
              VZ0(i,j)    = VZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * VZ0_SEA(i,j)
              USTAR(i,j)  = USTAR(i,j)  * XICE(i,j) + (1.0-XICE(i,j)) * USTAR_SEA(i,j)
              ! ZNT wait
              PBLH(i,j)   = PBLH(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * PBLH_SEA(i,j)
              RMOL(i,j)   = RMOL(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * RMOL_SEA(i,j)
              AKHS(i,j)   = AKHS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKHS_SEA(i,j)
              AKMS(i,j)   = AKMS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKMS_SEA(i,j)

              !         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
           ELSE
              ! We're not over sea ice.  Take the results from the first call.
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE qnsesfc_seaice_wrapper


!-------------------------------------------------------------------------

   SUBROUTINE mynn_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,dz8w,     &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,SNOWH,                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
               &itimestep,ch,th3d,pi3d,qc3d,rho,                   &
               &tsq,qsq,cov,Sh3d,el_pbl,qcg,                       &
               &icloud_bl,qc_bl,cldfra_bl,                         &
               &spp_pbl,pattern_spp_pbl,                           &
XICE,SST,TSK_SEA,                                                  &
CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
TICE2TSK_IF2COLD,XICE_THRESHOLD,                                   &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,bl_mynn_cloudpdf)

     USE module_sf_mynn, ONLY: sfclay_mynn
     implicit none

     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX,bl_mynn_cloudpdf,icloud_bl
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN

     INTEGER,  INTENT(IN)      ::     spp_pbl
     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )             &
                             ::   pattern_spp_pbl

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D,rho

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                             TH2, &
                                                              T2, &
                                                              Q2, &
                                                            QSFC,SNOWH
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL,TSK

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                                 &
                                                              QGH

     REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX
! from mynn subroutine
     INTEGER, INTENT(in) :: itimestep
     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: qcg
     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ch
     REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::     &
                                                             &QC3D,&
                            &th3d,pi3d,tsq,qsq,cov,Sh3d,el_pbl,&
                                              &qc_bl,cldfra_bl

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(OUT)     ::                   ck,cka,cd,cda
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(INOUT)   ::                            ustm
     INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX,IZ0TLND

!--------------------------------------------------------------------
!    New for wrapper
!--------------------------------------------------------------------
     LOGICAL,  INTENT(IN)               ::      TICE2TSK_IF2COLD
     REAL,     INTENT(IN)               ::      XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(IN)               ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(INOUT)            ::      SST
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(OUT)              ::      TSK_SEA,          &
                                                CHS2_SEA,         &
                                                CHS_SEA,          &
                                                CPM_SEA,          &
                                                CQS2_SEA,         &
                                                FLHC_SEA,         &
                                                FLQC_SEA,         &
                                                HFX_SEA,          &
                                                LH_SEA,           &
                                                QFX_SEA,          &
                                                QGH_SEA,          &
                                                QSFC_SEA,         &
                                                ZNT_SEA

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAND_SEA,        &
                                                MAVAIL_sea,       &
                                                TSK_LOCAL,        &
                                                BR_HOLD,          &
                                                CHS2_HOLD,        &
                                                CHS_HOLD,         &
                                                CPM_HOLD,         &
                                                CQS2_HOLD,        &
                                                FLHC_HOLD,        &
                                                FLQC_HOLD,        &
                                                GZ1OZ0_HOLD,      &
                                                HFX_HOLD,         &
                                                LH_HOLD,          &
                                                MOL_HOLD,         &
                                                PSIH_HOLD,        &
                                                PSIM_HOLD,        &
                                                QFX_HOLD,         &
                                                QGH_HOLD,         &
                                                REGIME_HOLD,      &
                                                RMOL_HOLD,        &
                                                UST_HOLD,         &
                                                WSPD_HOLD,        &
                                                ZNT_HOLD,         &
                                                CH_HOLD,          & ! new
                                                ZOL_HOLD,         &
                                                Q2_SEA,           &
                                                T2_SEA,           &
                                                TH2_SEA,          &
                                                U10_SEA,          &
                                                V10_SEA,          &
                                                CD_SEA,           &
                                                CDA_SEA,          &
                                                CK_SEA,           &
                                                CKA_SEA,          &
                                                USTM_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::                   &
                                                BR_SEA,           &
                                                GZ1OZ0_SEA,       &
                                                MOL_SEA,          &
                                                PSIH_SEA,         &
                                                PSIM_SEA,         &
                                                REGIME_SEA,       &
                                                RMOL_SEA,         &
                                                UST_SEA,          &
                                                WSPD_SEA,         &
                                                CH_SEA,           & ! new
                                                ZOL_SEA
! INTENT(IN) to SFCLAY; unchanged by the call
      ! ISFFLX
      ! SVP1,SVP2,SVP3,SVPT0
      ! EP1,EP2,KARMAN,EOMEG,STBOLT
      ! CP,G,ROVCP,R,XLV,DX
      ! dz8w
      ! QV3D
      ! P3D
      ! T3D
      ! MAVAIL
      ! PBLH
      ! XLAND
      ! TSK
      ! U3D
      ! V3D
      ! PSFC

    CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )

! DFS 8/25/10 Set TSK to ice value
    DO j = JTS , JTE
        DO i = ITS , ITE
            TSK(i,j) = TSK_LOCAL(i,j)
        ENDDO
    ENDDO

! INTENT (INOUT) to SFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
     BR_HOLD   = BR
     CHS2_HOLD = CHS2
     CHS_HOLD  = CHS
     CPM_HOLD  = CPM
     CQS2_HOLD = CQS2
     FLHC_HOLD = FLHC
     FLQC_HOLD = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD  = HFX
     LH_HOLD   = LH
     MOL_HOLD  = MOL
     PSIH_HOLD = PSIH
     PSIM_HOLD = PSIM
     QFX_HOLD  = QFX
     QGH_HOLD  = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD = RMOL
     UST_HOLD  = UST
     WSPD_HOLD = WSPD
     ZNT_HOLD  = ZNT
     ZOL_HOLD  = ZOL
     CH_HOLD   = CH

! INTENT(OUT) from SFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to SFCLAY.
     ! Q2
     ! QSFC
     ! T2
     ! TH2
     ! U10
     ! V10

     ! land/frozen-water call
!     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
!                 CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      & !
!                 I,I,I,I,I,I,IO,IO,IO,IO,
!                 ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
!                 XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
!                 U10,V10,TH2,T2,Q2,                            &
!                 GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
!                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
!                 KARMAN,EOMEG,STBOLT,                          &
!                 P1000,                                      &
!                 ids,ide, jds,jde, kds,kde,                    &
!                 ims,ime, jms,jme, kms,kme,                    &
!                 its,ite, jts,jte, kts,kte,                    &
!                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )

          CALL SFCLAY_mynn(U3D,V3D,T3D,QV3D,P3D,dz8w,              &
               CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,            &
               ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,       &
               XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
               U10,V10,TH2,T2,Q2,SNOWH,                            &
               GZ1OZ0,WSPD,BR,ISFFLX,DX,                           &
               SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,                &
               &itimestep,ch,th3d,pi3d,qc3d,rho,                   &
               &tsq,qsq,cov,sh3d,el_pbl,qcg,                       &
               &icloud_bl,qc_bl,cldfra_bl,                         &
               &spp_pbl,pattern_spp_pbl,                           &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte,                          &
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,bl_mynn_cloudpdf)

     ! Set up for open-water call
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j) = SST(i,j)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF
           ELSE
              XLAND_SEA(i,j) = XLAND(i,j)
              MAVAIL_SEA(i,j) = MAVAIL(i,j)
              ZNT_SEA(i,j)  = ZNT_HOLD(i,j)
              TSK_SEA(i,j) = TSK_LOCAL(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Restore the values from before the land/frozen-water call
     BR_SEA   = BR_HOLD
     CHS2_SEA = CHS2_HOLD
     CHS_SEA  = CHS_HOLD
     CPM_SEA  = CPM_HOLD
     CQS2_SEA = CQS2_HOLD
     FLHC_SEA = FLHC_HOLD
     FLQC_SEA = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA  = HFX_HOLD
     LH_SEA   = LH_HOLD
     MOL_SEA  = MOL_HOLD
     PSIH_SEA = PSIH_HOLD
     PSIM_SEA = PSIM_HOLD
     QFX_SEA  = QFX_HOLD
     QGH_SEA  = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA = RMOL_HOLD
     UST_SEA  = UST_HOLD
     WSPD_SEA = WSPD_HOLD
     ZOL_SEA  = ZOL_HOLD
     CH_SEA   = CH_HOLD

     ! open-water call
!     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
!                 CP,G,ROVCP,R,XLV,PSFC,                        & ! I
!                 CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,            & ! I/O
!                 ZNT_SEA,UST_SEA,                              & ! I/O
!                 PBLH,MAVAIL_SEA,                              & ! I
!                 ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, & ! I/O
!                 XLAND_SEA,                              & ! I
!                 HFX_SEA,QFX_SEA,LH_SEA,                       & ! I/O
!                 TSK_SEA,                                      & ! I
!                 FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,  & ! I/O
!                 U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,        & ! O
!                 GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                   & ! I/O
!                 ISFFLX,DX,                                    &
!                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
!                 KARMAN,EOMEG,STBOLT,
!                 P1000,                                      &
!                 ids,ide, jds,jde, kds,kde,                    &
!                 ims,ime, jms,jme, kms,kme,                    &
!                 its,ite, jts,jte, kts,kte,                    & ! 0
!                 ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx,iz0tlnd )
          CALL SFCLAY_mynn(U3D,V3D,T3D,QV3D,P3D,dz8w,              &
               CP,G,ROVCP,R,XLV,PSFC,                              &
               CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,                  &
               ZNT_SEA,UST_SEA,                                    &
               PBLH,MAVAIL_SEA,                                    &
               ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA,       &
               XLAND_SEA,                                          &
               HFX_SEA,QFX_SEA,LH_SEA,                             &
               TSK_SEA,                                            &
               FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,        &
               U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,SNOWH,        &
               GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                         &
               ISFFLX,DX,                                          &
               SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,                &
               &itimestep,CH_SEA,th3d,pi3d,qc3d,rho,               &
               &tsq,qsq,cov,sh3d,el_pbl,qcg,                       &
               &icloud_bl,qc_bl,cldfra_bl,                         &
               &spp_pbl,pattern_spp_pbl,                           &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte,                          &
               ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx,    &
               iz0tlnd,bl_mynn_cloudpdf )

     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD )  .and.( XICE(i,j) .LE. 1.0 ) ) THEN
              ! weighted average for sea ice points
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              ! CHS2 -- wait
              ! CHS  -- wait
              ! CPM  -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              ! HFX  -- wait
              ! LH   -- wait
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              ! QFX  -- wait
              ! QGH  -- wait
              if ( XICE(i,j).GE. 0.5 ) regime(i,j) = regime_hold(i,j)
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ch(i,j)     = ( ch(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ch_sea(i,j)    )
              ! INTENT(OUT)
              ! --------------------------------------------------------------------
              IF ( PRESENT ( CD ) ) THEN
                 CD(i,j)  = ( CD(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CD_sea(i,j)    )
              ENDIF
              IF ( PRESENT ( CDA ) ) THEN
                 CDA(i,j) = ( CDA(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CDA_sea(i,j)   )
              ENDIF
              IF ( PRESENT ( CK ) ) THEN
                 CK(i,j)  = ( CK(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CK_sea(i,j)    )
              ENDIF
              IF ( PRESENT ( CKA ) ) THEN
                 CKA(i,j) = ( CKA(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CKA_sea(i,j)   )
              ENDIF
              q2(i,j)     = ( q2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * q2_sea(i,j)     )
              ! QSFC -- wait
              t2(i,j)     = ( t2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * t2_sea(i,j)     )
              th2(i,j)    = ( th2(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * th2_sea(i,j)    )
              u10(i,j)    = ( u10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              IF ( PRESENT ( USTM ) ) THEN
                 USTM(i,j) = ( USTM(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * USTM_sea(i,j)   )
              ENDIF
              v10(i,j)    = ( v10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
           ENDIF
        END DO
     END DO
!
!         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
!
   END SUBROUTINE mynn_seaice_wrapper

!-------------------------------------------------------------------------

   SUBROUTINE sf_gfs_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,        &
		 CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,          &
                     ZNT,UST,PSIM,PSIH,                          &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,             &
                     QGH,QSFC,U10,V10,                           &
                     GZ1OZ0,WSPD,BR,ISFFLX,                      &
                     EP1,EP2,KARMAN,itimestep,                   &
                     TICE2TSK_IF2COLD,                           &
                     XICE_THRESHOLD,                             &
                     CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,       &
                     FLHC_SEA, FLQC_SEA,                         &
                     HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA,&
                     UST_SEA, ZNT_SEA, SST, XICE,                &
                     ids,ide, jds,jde, kds,kde,                  &
                     ims,ime, jms,jme, kms,kme,                  &
                     its,ite, jts,jte, kts,kte                   )
     USE module_sf_gfs
     implicit none

     INTEGER, INTENT(IN) ::             ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ISFFLX,itimestep

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        EP1,                            &
                                        EP2,                            &
                                        KARMAN,                         &
                                        R,                              &
                                        ROVCP,                          &
                                        XLV

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        P3D,                            &
                                        QV3D,                           &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        TSK,                            &
                                        PSFC,                           &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        UST,                            &
                                        ZNT

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        BR,                             &
                                        CHS,                            &
                                        CHS2,                           &
                                        CPM,                            &
                                        CQS2,                           &
                                        FLHC,                           &
                                        FLQC,                           &
                                        GZ1OZ0,                         &
                                        HFX,                            &
                                        LH,                             &
                                        PSIM,                           &
                                        PSIH,                           &
                                        QFX,                            &
                                        QGH,                            &
                                        QSFC,                           &
                                        U10,                            &
                                        V10,                            &
                                        WSPD

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) ::                  &
                                        XICE
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        CHS_SEA,                        &
                                        CHS2_SEA,                       &
                                        CPM_SEA,                        &
                                        CQS2_SEA,                       &
                                        FLHC_SEA,                       &
                                        FLQC_SEA,                       &
                                        HFX_SEA,                        &
                                        LH_SEA,                         &
                                        QFX_SEA,                        &
                                        QGH_SEA,                        &
                                        QSFC_SEA,                       &
                                        UST_SEA,                        &
                                        ZNT_SEA
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::               &
                                        SST

      REAL,                              INTENT(IN)    ::               &
                                        XICE_THRESHOLD
      LOGICAL,                          INTENT(IN)     :: TICE2TSK_IF2COLD

!-------------------------------------------------------------------------
!   Local
!-------------------------------------------------------------------------
      INTEGER :: I
      INTEGER :: J
      REAL, DIMENSION(ims:ime, jms:jme) ::                              &
                                        BR_SEA,                         &
                                        GZ1OZ0_SEA,                     &
                                        PSIM_SEA,                       &
                                        PSIH_SEA,                       &
                                        U10_SEA,                        &
                                        V10_SEA,                        &
                                        WSPD_SEA,                       &
                                        XLAND_SEA,                &
                                        TSK_SEA,                        &
                                        UST_HOLD,                       &
                                        ZNT_HOLD,                       &
                                        TSK_LOCAL

      CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                              itimestep, .true., tice2tsk_if2cold,     &
                              XICE, XICE_THRESHOLD,                    &
                              SST, TSK, TSK_SEA, TSK_LOCAL )

!
! Set up for frozen ocean call for sea ice points
!

! Strictly INTENT(IN), Should be unchanged by SF_GFS:
!     CP
!     EP1
!     EP2
!     KARMAN
!     R
!     ROVCP
!     XLV
!     P3D
!     QV3D
!     T3D
!     U3D
!     V3D
!     TSK
!     PSFC
!     XLAND
!     ISFFLX
!     ITIMESTEP


! Intent (INOUT), original value is used and changed by SF_GFS.
!     UST
!     ZNT

     ZNT_HOLD = ZNT
     UST_HOLD = UST

! Strictly INTENT (OUT), set by SF_GFS:
!     BR
!     CHS     -- used by LSM routines
!     CHS2    -- used by LSM routines
!     CPM     -- used by LSM routines
!     CQS2    -- used by LSM routines
!     FLHC
!     FLQC
!     GZ1OZ0
!     HFX     -- used by LSM routines
!     LH      -- used by LSM routines
!     PSIM
!     PSIH
!     QFX     -- used by LSM routines
!     QGH     -- used by LSM routines
!     QSFC    -- used by LSM routines
!     U10
!     V10
!     WSPD

!
! Frozen ocean / true land call.
!
     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM_SEA,    &
          ZNT,UST,PSIM,PSIH,                            &
          XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,         &
          QGH,QSFC,U10,V10,                             &
          GZ1OZ0,WSPD,BR,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )

! Set up for open-water call

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Sets up things for open ocean fraction of sea-ice points
              XLAND_SEA(i,j)=2.
              ZNT_SEA(I,J) = 0.0001
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
           ELSE
              ! Fully open ocean or true land points
              XLAND_SEA(i,j)=xland(i,j)
              ZNT_SEA(I,J) = ZNT_HOLD(I,J)
              UST_SEA(i,j) = UST_HOLD(i,j)
              TSK_SEA(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Open-water call
     ! _SEA variables are held for later use as the result of the open-water call.
     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM,        &
          ZNT_SEA,UST_SEA,PSIM_SEA,PSIH_SEA,                        &
          XLAND,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,   &
          QGH_SEA,QSFC_SEA,U10_SEA,V10_SEA,                         &
          GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )

! Weighting, after our two calls to SF_GFS

     DO j = JTS , JTE
        DO i = ITS , ITE
           ! Over sea-ice points, weight the results.  Otherwise, just take the results from the
           ! first call to SF_GFS_
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Weight a number of fields (between open-water results
              ! and full ice results) by sea-ice fraction.

              BR(i,j)     = ( BR(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * BR_SEA(i,j)     )
              ! CHS, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CHS2, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CPM, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CQS2, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! FLHC(i,j) = ( FLHC(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * FLHC_SEA(i,j) )
              ! FLQC(i,j) = ( FLQC(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * FLQC_SEA(i,j) )
              GZ1OZ0(i,j) = ( GZ1OZ0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * GZ1OZ0_SEA(i,j) )
              ! HFX, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! LH, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              PSIM(i,j)   = ( PSIM(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIM_SEA(i,j)   )
              PSIH(i,j)   = ( PSIH(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIH_SEA(i,j)   )
              ! QFX, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! QGH, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! QSFC, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              U10(i,j)    = ( U10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * U10_SEA(i,j)    )
              V10(i,j)    = ( V10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * V10_SEA(i,j)    )
              WSPD(i,j)   = ( WSPD(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * WSPD_SEA(i,j)   )
              ! UST, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! ZNT, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables

           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE sf_gfs_seaice_wrapper

!-------------------------------------------------------------------------

!-------------------------------------------------------------------------

   SUBROUTINE sfclayrev_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,dz8w,     &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     FM,FH,                                        &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000,                                      &
                     XICE,SST,TSK_SEA,                                                  &
                     CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
                     HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
                     ITIMESTEP,TICE2TSK_IF2COLD,XICE_THRESHOLD,                         &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,          &
                     sf_surface_physics                             )

     USE module_sf_sfclayrev
     implicit none

     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
     REAL,     INTENT(IN )   ::        P1000

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                             TH2, &
                                                              T2, &
                                                              Q2, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                 PSIM,PSIH,FM,FH

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                                 &
                                                              QGH

     REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(OUT)     ::                   ck,cka,cd,cda
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(INOUT)   ::                            ustm

     INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX,IZ0TLND

!--------------------------------------------------------------------
!    New for wrapper
!--------------------------------------------------------------------
     INTEGER,  INTENT(IN)          ::    ITIMESTEP, sf_surface_physics
     LOGICAL,  INTENT(IN)               ::      TICE2TSK_IF2COLD
     REAL,     INTENT(IN)               ::      XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(IN)               ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(INOUT)            ::      SST
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(OUT)              ::      TSK_SEA,          &
                                                CHS2_SEA,         &
                                                CHS_SEA,          &
                                                CPM_SEA,          &
                                                CQS2_SEA,         &
                                                FLHC_SEA,         &
                                                FLQC_SEA,         &
                                                HFX_SEA,          &
                                                LH_SEA,           &
                                                QFX_SEA,          &
                                                QGH_SEA,          &
                                                QSFC_SEA,         &
                                                ZNT_SEA

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAND_SEA,        &
                                                MAVAIL_sea,       &
                                                TSK_LOCAL,        &
                                                BR_HOLD,          &
                                                CHS2_HOLD,        &
                                                CHS_HOLD,         &
                                                CPM_HOLD,         &
                                                CQS2_HOLD,        &
                                                FLHC_HOLD,        &
                                                FLQC_HOLD,        &
                                                GZ1OZ0_HOLD,      &
                                                HFX_HOLD,         &
                                                LH_HOLD,          &
                                                MOL_HOLD,         &
                                                PSIH_HOLD,        &
                                                PSIM_HOLD,        &
                                                FH_HOLD,          &
                                                FM_HOLD,          &
                                                QFX_HOLD,         &
                                                QGH_HOLD,         &
                                                REGIME_HOLD,      &
                                                RMOL_HOLD,        &
                                                UST_HOLD,         &
                                                WSPD_HOLD,        &
                                                ZNT_HOLD,         &
                                                ZOL_HOLD,         &
                                                TH2_HOLD,         & !ssib
                                                T2_HOLD,          & !ssib
                                                Q2_HOLD,          & !ssib
                                                TSK_HOLD,         & !ssib
                                                CD_SEA,           &
                                                CDA_SEA,          &
                                                CK_SEA,           &
                                                CKA_SEA,          &
                                                Q2_SEA,           &
                                                T2_SEA,           &
                                                TH2_SEA,          &
                                                U10_SEA,          &
                                                USTM_SEA,         &
                                                V10_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::                   &
                                                BR_SEA,           &
                                                GZ1OZ0_SEA,       &
                                                MOL_SEA,          &
                                                PSIH_SEA,         &
                                                PSIM_SEA,         &
                                                FH_SEA,           &
                                                FM_SEA,           &
                                                REGIME_SEA,       &
                                                RMOL_SEA,         &
                                                UST_SEA,          &
                                                WSPD_SEA,         &
                                                ZOL_SEA

! INTENT(IN) to SFCLAY; unchanged by the call
      ! ISFFLX
      ! SVP1,SVP2,SVP3,SVPT0
      ! EP1,EP2,KARMAN,EOMEG,STBOLT
      ! CP,G,ROVCP,R,XLV,DX
      ! ISFTCFLX,IZ0TLND
      ! P1000
      ! dz8w
      ! QV3D
      ! P3D
      ! T3D
      ! MAVAIL
      ! PBLH
      ! XLAND
      ! TSK
      ! U3D
      ! V3D
      ! PSFC

     CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )


! INTENT (INOUT) to SFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
     BR_HOLD   = BR
     CHS2_HOLD = CHS2
     CHS_HOLD  = CHS
     CPM_HOLD  = CPM
     CQS2_HOLD = CQS2
     FLHC_HOLD = FLHC
     FLQC_HOLD = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD  = HFX
     LH_HOLD   = LH
     MOL_HOLD  = MOL
     PSIH_HOLD = PSIH
     PSIM_HOLD = PSIM
     FH_HOLD   = FH
     FM_HOLD   = FM
     QFX_HOLD  = QFX
     QGH_HOLD  = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD = RMOL
     UST_HOLD  = UST
     WSPD_HOLD = WSPD
     ZNT_HOLD  = ZNT
     ZOL_HOLD  = ZOL
!also save these variables for SSIB (fds 12/2010)
     TH2_HOLD = TH2
     T2_HOLD = T2
     Q2_HOLD = Q2
     TSK_HOLD = TSK
     
! INTENT(OUT) from SFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to SFCLAY.
     ! CD
     ! CDA
     ! CK
     ! CKA
     ! Q2
     ! QSFC
     ! T2
     ! TH2
     ! U10
     ! USTM
     ! V10


     ! land/frozen-water call
     call sfclayrev(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      & ! I,I,I,I,I,I,IO,IO,IO,IO,
                 ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                 FM,FH,                                        &
                 XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                 U10,V10,TH2,T2,Q2,                            &
                 GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )
!
!Restore land-point values calculated by SSiB (fds 12/2010)
     IF (itimestep .gt. 1 .and. sf_surface_physics .EQ. 8) then
     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( XLAND(I,J) .LT. 1.5 ) THEN
              BR(I,J) = BR_HOLD(I,J)
              TH2(I,J) = TH2_HOLD(I,J)
              T2(I,J) = T2_HOLD(I,J)
              Q2(I,J) = Q2_HOLD(I,J)
              HFX(I,J) = HFX_HOLD(I,J)
              QFX(I,J) = QFX_HOLD(I,J)
              LH(I,J) = LH_HOLD(I,J)
              GZ1OZ0(I,J) = GZ1OZ0_HOLD(I,J)
              WSPD(I,J) = WSPD_HOLD(I,J)
              ZNT(I,J) = ZNT_HOLD(I,J)
              UST(I,J) = UST_HOLD(I,J)
!             TSK(I,J) = TSK_HOLD(I,J)
           ENDIF
        ENDDO
     ENDDO
     ENDIF
!
     ! Set up for open-water call
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j) = SST(i,j)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF
           ELSE
              XLAND_SEA(i,j) = XLAND(i,j)
              MAVAIL_SEA(i,j) = MAVAIL(i,j)
              ZNT_SEA(i,j)  = ZNT_HOLD(i,j)
              TSK_SEA(i,j) = TSK_LOCAL(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Restore the values from before the land/frozen-water call
     BR_SEA   = BR_HOLD
     CHS2_SEA = CHS2_HOLD
     CHS_SEA  = CHS_HOLD
     CPM_SEA  = CPM_HOLD
     CQS2_SEA = CQS2_HOLD
     FLHC_SEA = FLHC_HOLD
     FLQC_SEA = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA  = HFX_HOLD
     LH_SEA   = LH_HOLD
     MOL_SEA  = MOL_HOLD
     PSIH_SEA = PSIH_HOLD
     PSIM_SEA = PSIM_HOLD
     FH_SEA   = FH_HOLD
     FM_SEA   = FM_HOLD
     QFX_SEA  = QFX_HOLD
     QGH_SEA  = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA = RMOL_HOLD
     UST_SEA  = UST_HOLD
     WSPD_SEA = WSPD_HOLD
     ZOL_SEA  = ZOL_HOLD
!
     ! open-water call
     call sfclayrev(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,                        & ! I
                 CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,            & ! I/O
                 ZNT_SEA,UST_SEA,                              & ! I/O
                 PBLH,MAVAIL_SEA,                              & ! I
                 ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, & ! I/O
                 FM_SEA,FH_SEA,                                &
                 XLAND_SEA,                              & ! I
                 HFX_SEA,QFX_SEA,LH_SEA,                       & ! I/O
                 TSK_SEA,                                      & ! I
                 FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,  & ! I/O
                 U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,        & ! O
                 GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                   & ! I/O
                 ISFFLX,DX,                                    &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    & ! 0
                 ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx,iz0tlnd )
!
     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD )  .and.( XICE(i,j) .LE. 1.0 ) ) THEN
              ! weighted average for sea ice points
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              ! CHS2 -- wait
              ! CHS  -- wait
              ! CPM  -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              ! HFX  -- wait
              ! LH   -- wait
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              fh(i,j)    = ( fh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * fh_sea(i,j)   )
              fm(i,j)    = ( fm(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * fm_sea(i,j)   )
              ! QFX  -- wait
              ! QGH  -- wait
              if ( XICE(i,j).GE. 0.5 ) regime(i,j) = regime_hold(i,j)
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ! INTENT(OUT) --------------------------------------------------------------------
              IF ( PRESENT ( CD ) ) THEN
                 CD(i,j)     = ( CD(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CD_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CDA ) ) THEN
                 CDA(i,j)     = ( CDA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CDA_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CK ) ) THEN
                 CK(i,j)     = ( CK(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CK_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CKA ) ) THEN
                 CKA(i,j)     = ( CKA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CKA_sea(i,j)     )
              ENDIF
              q2(i,j)     = ( q2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * q2_sea(i,j)     )
              ! QSFC -- wait
              t2(i,j)     = ( t2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * t2_sea(i,j)     )
              th2(i,j)    = ( th2(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * th2_sea(i,j)    )
              u10(i,j)    = ( u10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              IF ( PRESENT ( USTM ) ) THEN
                 USTM(i,j)    = ( USTM(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * USTM_sea(i,j)    )
              ENDIF
              v10(i,j)    = ( v10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
           ENDIF
        END DO
     END DO
!
!         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
!
   END SUBROUTINE sfclayrev_seaice_wrapper

!-------------------------------------------------------------------------

   SUBROUTINE sfclay_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,dz8w,     &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     FM,FH,                                        &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000,                                      &
XICE,SST,TSK_SEA,                                                  &
CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
ITIMESTEP,TICE2TSK_IF2COLD,XICE_THRESHOLD,                         &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,          &
                     sf_surface_physics                             )

     USE module_sf_sfclay
     implicit none

     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
     REAL,     INTENT(IN )   ::        P1000

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                             TH2, &
                                                              T2, &
                                                              Q2, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                 PSIM,PSIH,FM,FH

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                                 &
                                                              QGH

     REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(OUT)     ::                   ck,cka,cd,cda
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(INOUT)   ::                            ustm

     INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX,IZ0TLND

!--------------------------------------------------------------------
!    New for wrapper
!--------------------------------------------------------------------
     INTEGER,  INTENT(IN)          ::    ITIMESTEP, sf_surface_physics
     LOGICAL,  INTENT(IN)               ::      TICE2TSK_IF2COLD
     REAL,     INTENT(IN)               ::      XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(IN)               ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(INOUT)            ::      SST
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(OUT)              ::      TSK_SEA,          &
                                                CHS2_SEA,         &
                                                CHS_SEA,          &
                                                CPM_SEA,          &
                                                CQS2_SEA,         &
                                                FLHC_SEA,         &
                                                FLQC_SEA,         &
                                                HFX_SEA,          &
                                                LH_SEA,           &
                                                QFX_SEA,          &
                                                QGH_SEA,          &
                                                QSFC_SEA,         &
                                                ZNT_SEA

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAND_SEA,        &
                                                MAVAIL_sea,       &
                                                TSK_LOCAL,        &
                                                BR_HOLD,          &
                                                CHS2_HOLD,        &
                                                CHS_HOLD,         &
                                                CPM_HOLD,         &
                                                CQS2_HOLD,        &
                                                FLHC_HOLD,        &
                                                FLQC_HOLD,        &
                                                GZ1OZ0_HOLD,      &
                                                HFX_HOLD,         &
                                                LH_HOLD,          &
                                                MOL_HOLD,         &
                                                PSIH_HOLD,        &
                                                PSIM_HOLD,        &
                                                FH_HOLD,          &
                                                FM_HOLD,          &
                                                QFX_HOLD,         &
                                                QGH_HOLD,         &
                                                REGIME_HOLD,      &
                                                RMOL_HOLD,        &
                                                UST_HOLD,         &
                                                WSPD_HOLD,        &
                                                ZNT_HOLD,         &
                                                ZOL_HOLD,         &
                                                TH2_HOLD,         & !ssib
                                                T2_HOLD,          & !ssib
                                                Q2_HOLD,          & !ssib
                                                TSK_HOLD,         & !ssib
                                                U10_HOLD,         & !ssib
                                                V10_HOLD,         & !ssib
                                                CD_SEA,           &
                                                CDA_SEA,          &
                                                CK_SEA,           &
                                                CKA_SEA,          &
                                                Q2_SEA,           &
                                                T2_SEA,           &
                                                TH2_SEA,          &
                                                U10_SEA,          &
                                                USTM_SEA,         &
                                                V10_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::                   &
                                                BR_SEA,           &
                                                GZ1OZ0_SEA,       &
                                                MOL_SEA,          &
                                                PSIH_SEA,         &
                                                PSIM_SEA,         &
                                                FH_SEA,           &
                                                FM_SEA,           &
                                                REGIME_SEA,       &
                                                RMOL_SEA,         &
                                                UST_SEA,          &
                                                WSPD_SEA,         &
                                                ZOL_SEA

! INTENT(IN) to SFCLAY; unchanged by the call
      ! ISFFLX
      ! SVP1,SVP2,SVP3,SVPT0
      ! EP1,EP2,KARMAN,EOMEG,STBOLT
      ! CP,G,ROVCP,R,XLV,DX
      ! ISFTCFLX,IZ0TLND
      ! P1000
      ! dz8w
      ! QV3D
      ! P3D
      ! T3D
      ! MAVAIL
      ! PBLH
      ! XLAND
      ! TSK
      ! U3D
      ! V3D
      ! PSFC

     CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )


! INTENT (INOUT) to SFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
     BR_HOLD   = BR
     CHS2_HOLD = CHS2
     CHS_HOLD  = CHS
     CPM_HOLD  = CPM
     CQS2_HOLD = CQS2
     FLHC_HOLD = FLHC
     FLQC_HOLD = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD  = HFX
     LH_HOLD   = LH
     MOL_HOLD  = MOL
     PSIH_HOLD = PSIH
     PSIM_HOLD = PSIM
     FH_HOLD   = FH
     FM_HOLD   = FM
     QFX_HOLD  = QFX
     QGH_HOLD  = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD = RMOL
     UST_HOLD  = UST
     WSPD_HOLD = WSPD
     ZNT_HOLD  = ZNT
     ZOL_HOLD  = ZOL
!also save these variables for SSIB (fds 12/2010)
     TH2_HOLD = TH2
     T2_HOLD = T2
     Q2_HOLD = Q2
     TSK_HOLD = TSK
     U10_HOLD = U10 !fds (01/2014)
     V10_HOLD = V10 !fds (01/2014)
     
! INTENT(OUT) from SFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to SFCLAY.
     ! CD
     ! CDA
     ! CK
     ! CKA
     ! Q2
     ! QSFC
     ! T2
     ! TH2
     ! U10
     ! USTM
     ! V10


     ! land/frozen-water call
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      & ! I,I,I,I,I,I,IO,IO,IO,IO,
                 ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                 FM,FH,                                        &
                 XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                 U10,V10,TH2,T2,Q2,                            &
                 GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )
!
!Restore land-point values calculated by SSiB (fds 12/2010)
     IF (itimestep .gt. 1 .and. sf_surface_physics .EQ. 8) then
     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( XLAND(I,J) .LT. 1.5 ) THEN
              BR(I,J) = BR_HOLD(I,J)
              TH2(I,J) = TH2_HOLD(I,J)
              T2(I,J) = T2_HOLD(I,J)
              Q2(I,J) = Q2_HOLD(I,J)
              HFX(I,J) = HFX_HOLD(I,J)
              QFX(I,J) = QFX_HOLD(I,J)
              LH(I,J) = LH_HOLD(I,J)
              GZ1OZ0(I,J) = GZ1OZ0_HOLD(I,J)
              WSPD(I,J) = WSPD_HOLD(I,J)
              ZNT(I,J) = ZNT_HOLD(I,J)
              UST(I,J) = UST_HOLD(I,J)
!             TSK(I,J) = TSK_HOLD(I,J)
              U10(I,J) = U10_HOLD(I,J) !fds (01/2014)
              V10(I,J) = V10_HOLD(I,J) !fds (01/2014)
           ENDIF
        ENDDO
     ENDDO
     ENDIF
!
     ! Set up for open-water call
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j) = SST(i,j)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF
           ELSE
              XLAND_SEA(i,j) = XLAND(i,j)
              MAVAIL_SEA(i,j) = MAVAIL(i,j)
              ZNT_SEA(i,j)  = ZNT_HOLD(i,j)
              TSK_SEA(i,j) = TSK_LOCAL(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Restore the values from before the land/frozen-water call
     BR_SEA   = BR_HOLD
     CHS2_SEA = CHS2_HOLD
     CHS_SEA  = CHS_HOLD
     CPM_SEA  = CPM_HOLD
     CQS2_SEA = CQS2_HOLD
     FLHC_SEA = FLHC_HOLD
     FLQC_SEA = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA  = HFX_HOLD
     LH_SEA   = LH_HOLD
     MOL_SEA  = MOL_HOLD
     PSIH_SEA = PSIH_HOLD
     PSIM_SEA = PSIM_HOLD
     FH_SEA   = FH_HOLD
     FM_SEA   = FM_HOLD
     QFX_SEA  = QFX_HOLD
     QGH_SEA  = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA = RMOL_HOLD
     UST_SEA  = UST_HOLD
     WSPD_SEA = WSPD_HOLD
     ZOL_SEA  = ZOL_HOLD
!
     ! open-water call
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,                        & ! I
                 CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,            & ! I/O
                 ZNT_SEA,UST_SEA,                              & ! I/O
                 PBLH,MAVAIL_SEA,                              & ! I
                 ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, & ! I/O
                 FM_SEA,FH_SEA,                                &
                 XLAND_SEA,                              & ! I
                 HFX_SEA,QFX_SEA,LH_SEA,                       & ! I/O
                 TSK_SEA,                                      & ! I
                 FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,  & ! I/O
                 U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,        & ! O
                 GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                   & ! I/O
                 ISFFLX,DX,                                    &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    & ! 0
                 ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx,iz0tlnd )
!
     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD )  .and.( XICE(i,j) .LE. 1.0 ) ) THEN
              ! weighted average for sea ice points
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              ! CHS2 -- wait
              ! CHS  -- wait
              ! CPM  -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              ! HFX  -- wait
              ! LH   -- wait
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              fh(i,j)    = ( fh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * fh_sea(i,j)   )
              fm(i,j)    = ( fm(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * fm_sea(i,j)   )
              ! QFX  -- wait
              ! QGH  -- wait
              if ( XICE(i,j).GE. 0.5 ) regime(i,j) = regime_hold(i,j)
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ! INTENT(OUT) --------------------------------------------------------------------
              IF ( PRESENT ( CD ) ) THEN
                 CD(i,j)     = ( CD(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CD_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CDA ) ) THEN
                 CDA(i,j)     = ( CDA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CDA_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CK ) ) THEN
                 CK(i,j)     = ( CK(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CK_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CKA ) ) THEN
                 CKA(i,j)     = ( CKA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CKA_sea(i,j)     )
              ENDIF
              q2(i,j)     = ( q2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * q2_sea(i,j)     )
              ! QSFC -- wait
              t2(i,j)     = ( t2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * t2_sea(i,j)     )
              th2(i,j)    = ( th2(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * th2_sea(i,j)    )
              u10(i,j)    = ( u10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              IF ( PRESENT ( USTM ) ) THEN
                 USTM(i,j)    = ( USTM(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * USTM_sea(i,j)    )
              ENDIF
              v10(i,j)    = ( v10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
           ENDIF
        END DO
     END DO
!
!         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
!
   END SUBROUTINE sfclay_seaice_wrapper

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   SUBROUTINE pxsfclay_seaice_wrapper(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w, &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
XICE, SST, ITIMESTEP, TICE2TSK_IF2COLD,XICE_THRESHOLD,             &
CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA, FLHC_SEA, FLQC_SEA,          &
HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, TSK_SEA,  &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
     USE module_sf_pxsfclay
     implicit none
     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     LOGICAL,  INTENT(IN )   ::        TICE2TSK_IF2COLD
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D, &
                                                            TH3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     INTENT(IN   )                  ::   CP,G,ROVCP,R,XLV,DX

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                       PSIM,PSIH

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            QGH

!--------------------------------------------------------------------
!    For wrapper
!--------------------------------------------------------------------

     INTEGER,  INTENT(IN)                           :: ITIMESTEP
     REAL,     INTENT(IN)                           :: XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN)                           ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)                        ::     TSK_SEA
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)              ::                 SST

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)    ::                         CHS_SEA, &
                                                        CHS2_SEA, &
                                                         CPM_SEA, &
                                                        CQS2_SEA, &
                                                        FLHC_SEA, &
                                                        FLQC_SEA, &
                                                         HFX_SEA, &
                                                          LH_SEA, &
                                                         QFX_SEA, &
                                                         QGH_SEA, &
                                                        QSFC_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::          BR_HOLD, &
                                                        CHS_HOLD, &
                                                       CHS2_HOLD, &
                                                        CPM_HOLD, &
                                                       CQS2_HOLD, &
                                                       FLHC_HOLD, &
                                                       FLQC_HOLD, &
                                                     GZ1OZ0_HOLD, &
                                                        HFX_HOLD, &
                                                         LH_HOLD, &
                                                        MOL_HOLD, &
                                                       PSIH_HOLD, &
                                                       PSIM_HOLD, &
                                                        QFX_HOLD, &
                                                        QGH_HOLD, &
                                                     REGIME_HOLD, &
                                                       RMOL_HOLD, &
                                                        UST_HOLD, &
                                                       WSPD_HOLD, &
                                                        ZNT_HOLD, &
                                                        ZOL_HOLD, &
                                                       TSK_LOCAL

     REAL,     DIMENSION( ims:ime, jms:jme ) ::        XLAND_SEA, &
                                                      MAVAIL_SEA, &
                                                          BR_SEA, &
                                                      GZ1OZ0_SEA, &
                                                         MOL_SEA, &
                                                        PSIH_SEA, &
                                                        PSIM_SEA, &
                                                      REGIME_SEA, &
                                                        RMOL_SEA, &
                                                         UST_SEA, &
                                                        WSPD_SEA, &
                                                         ZNT_SEA, &
                                                         ZOL_SEA, &
                                                         U10_SEA, &
                                                         V10_SEA

     CALL get_local_ice_tsk( ims, ime, jms, jme, its, ite, jts, jte,  &
                             itimestep, .true., tice2tsk_if2cold,     &
                             XICE, XICE_THRESHOLD,                    &
                             SST, TSK, TSK_SEA, TSK_LOCAL )
!
! INTENT (INOUT) to PXSFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
!
     BR_HOLD     = BR
     CHS_HOLD    = CHS
     CHS2_HOLD   = CHS2
     CPM_HOLD    = CPM
     CQS2_HOLD   = CQS2
     FLHC_HOLD   = FLHC
     FLQC_HOLD   = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD    = HFX
     LH_HOLD     = LH
     MOL_HOLD    = MOL
     PSIH_HOLD   = PSIH
     PSIM_HOLD   = PSIM
     QFX_HOLD    = QFX
     QGH_HOLD    = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD   = RMOL
     UST_HOLD    = UST
     WSPD_HOLD   = WSPD
     ZNT_HOLD    = ZNT
     ZOL_HOLD    = ZOL

! INTENT(OUT) from PXSFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to PXSFCLAY.
     ! U10
     ! V10
     ! QSFC

! Land/frozen-water call.
     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,ITIMESTEP,&
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i= ITS , ITE
           IF( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Sets up things for open ocean.
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j)  = SST(i,j)
              if ( SST(i,j) .LT. 271.4 ) then
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              endif
           ELSE
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)  = ZNT_HOLD(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! INTENT(INOUT) variables held over from before the first call to PXSFCLAY:
     BR_SEA     = BR_HOLD
     CHS_SEA    = CHS_HOLD
     CHS2_SEA   = CHS2_HOLD
     CPM_SEA    = CPM_HOLD
     CQS2_SEA   = CQS2_HOLD
     FLHC_SEA   = FLHC_HOLD
     FLQC_SEA   = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA    = HFX_HOLD
     LH_SEA     = LH_HOLD
     MOL_SEA    = MOL_HOLD
     PSIH_SEA   = PSIH_HOLD
     PSIM_SEA   = PSIM_HOLD
     QFX_SEA    = QFX_HOLD
     QGH_SEA    = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA   = RMOL_HOLD
     UST_SEA    = UST_HOLD
     WSPD_SEA   = WSPD_HOLD
     ZOL_SEA    = ZOL_HOLD

! Open-water call.
     ! Variables newly set (INTENT(OUT)) or changed (INTENT(INOUT)) by
     ! PXSFCLAY are here appended with the "_SEA" label.
     ! Special intent(IN) variables here:  XLAND_SEA, MAVAIL_SEA, TSK_SEA
     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,      &
                     ZNT_SEA,UST_SEA,PBLH,MAVAIL_SEA,ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, &
                     XLAND_SEA,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_SEA,RMOL_SEA, &
                     U10_SEA,V10_SEA,                              &
                     GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,DX,         &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,ITIMESTEP,&
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! INTENT (INOUT) for PXSFCLAY:
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ! REGIME:  Special case for this variable.  Just take the land values.
              ! CHS -- wait
              ! CHS2 -- wait
              ! CPM -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              ! HFX -- wait
              ! LH -- wait
              ! QFX -- wait
              ! QGH -- wait

              ! INTENT (OUT) from PXSFCLAY:
              u10(i,j) = ( u10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              v10(i,j) = ( v10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
              ! QSFC -- wait
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE pxsfclay_seaice_wrapper

!-------------------------------------------------------------------------

   SUBROUTINE TOPO_RAD_ADJ_DRVR (XLAT,XLONG,COSZEN,               &
                    shadowmask, diffuse_frac,                     &
                    declin,                                       &
                    SWDOWN,GSW,SWNORM,GSWSAVE,solcon,hrang2d,     &
                    slope_in,slp_azi_in,                          &
                ids, ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                     &
                its, ite, jts, jte, kts, kte                      )
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   INTEGER, INTENT(IN)   ::       its,ite,jts,jte,kts,kte,        &
                                  ims,ime,jms,jme,kms,kme,        &
                                  ids,ide,jds,jde,kds,kde
   INTEGER, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN)      ::       shadowmask
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN)      ::       diffuse_frac
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )   ::       XLAT,XLONG
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)   ::       SWDOWN,GSW,SWNORM,GSWSAVE
   real,intent(in)  :: solcon   
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: hrang2d,coszen 


   REAL, INTENT(IN    )  ::       declin
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: slope_in,slp_azi_in


! LOCAL VARS
   integer    :: i,j
   real       :: pi,degrad
   integer    :: shadow
   real       :: swdown_teradj,swdown_in,xlat1,xlong1

!------------------------------------------------------------------

     pi = 4.*atan(1.)
     degrad=pi/180.

       DO J=jts,jte
       DO I=its,ite
         SWNORM(i,j) = SWDOWN(i,j)     ! save
         IF(SWDOWN(I,J) .GT. 1.E-3)THEN  ! daytime
             shadow = shadowmask(i,j)

         SWDOWN_IN = SWDOWN(i,j)
         XLAT1 = XLAT(i,j)
         XLONG1 = XLONG(i,j)
         CALL TOPO_RAD_ADJ (XLAT1,XLONG1,COSZEN(i,j),             &
                    diffuse_frac(i,j),DECLIN,DEGRAD,              &
                    SWDOWN_IN,solcon,hrang2d(i,j),SWDOWN_teradj,  &
                    kts,kte,                                      &
                    slope_in(i,j),slp_azi_in(i,j),                &
                    shadow , i,j                                  &
                    )

         GSWSAVE(I,J) = GSW(I,J)       ! save
         GSW(I,J) = GSW(I,J)*SWDOWN_teradj/SWDOWN(i,j)
         SWDOWN(i,j) = SWDOWN_teradj

         ENDIF ! daytime
       ENDDO  ! i_loop
       ENDDO  ! j_loop


   END SUBROUTINE TOPO_RAD_ADJ_DRVR
!------------------------------------------------------------------
!------------------------------------------------------------------
   SUBROUTINE TOPO_RAD_ADJ (XLAT1,XLONG1,COSZEN,                 &
                    diffuse_frac_in,DECLIN,DEGRAD,               &
                    SWDOWN_IN,solcon,hrang,SWDOWN_teradj,        &
                    kts,kte,                                     &
                    slope,slp_azi,                               &
                    shadow                                       &
                    ,i,j)

!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
  INTEGER, INTENT(IN)       :: kts,kte
  REAL, INTENT(IN)          :: COSZEN,DECLIN,              &
                               XLAT1,XLONG1,DEGRAD
  REAL, INTENT(IN)          :: SWDOWN_IN,solcon,hrang
  INTEGER, INTENT(IN)       :: shadow
  REAL, INTENT(IN)          :: slp_azi,slope
  REAL, INTENT(IN)          :: diffuse_frac_in

  REAL, INTENT(OUT)         :: SWDOWN_teradj

! LOCAL VARS
   REAL            :: XT24,TLOCTM,CSZA,XXLAT
   REAL            :: diffuse_frac,corr_fac,csza_slp
   integer         :: i,j


!------------------------------------------------------------------

     SWDOWN_teradj=SWDOWN_IN

     CSZA=COSZEN
     XXLAT=XLAT1*DEGRAD

! RETURN IF NIGHT
         IF(CSZA.LE.1.E-4) return 
        
!  Parameterize diffuse fraction of global solar radiation as a function of the ratio 
!    between TOA radiation and surface global radiation
!             diffuse_frac = min(1.,1./(max(0.1,2.1-2.8*log(log(csza*solcon/max(SWDOWN_IN,1.e-3))))))
              diffuse_frac = diffuse_frac_in
        if ((slope.eq.0).or.(diffuse_frac.eq.1).or.(csza.le.1.e-4)) then
!  no topographic effects when all radiation diffuse or sun too close to horizon
          corr_fac = 1
          if(shadow.eq.1) corr_fac = diffuse_frac
          goto 140
        endif

! cosine of zenith angle over sloping topography
        csza_slp = ((SIN(XXLAT)*COS(HRANG))*                                          &
                    (-cos(slp_azi)*sin(slope))-SIN(HRANG)*(sin(slp_azi)*sin(slope))+  &
                    (COS(XXLAT)*COS(HRANG))*cos(slope))*                              &
                   COS(DECLIN)+(COS(XXLAT)*(cos(slp_azi)*sin(slope))+                 &
                   SIN(XXLAT)*cos(slope))*SIN(DECLIN)
        IF(csza_slp.LE.1.E-4) csza_slp = 0

! Topographic shading
        if (shadow.eq.1) csza_slp = 0

! Correction factor for sloping topography; the diffuse fraction of solar radiation 
!   is assumed to be unaffected by the slope
        corr_fac = diffuse_frac + (1-diffuse_frac)*csza_slp/csza

 140        continue

      SWDOWN_teradj=(1.)*SWDOWN_IN*corr_fac

   END SUBROUTINE TOPO_RAD_ADJ

!=======================================================================

   SUBROUTINE get_local_ice_tsk ( ims, ime, jms, jme,     &
                                  its, ite, jts, jte,     &
                                  itimestep,              &
                                  sfc_layer_values,       &
                                  tice2tsk_if2cold,       &
                                  XICE, XICE_THRESHOLD,   &
                                  SST, TSK, TSK_SEA, TSK_ICE )
!<DESCRIPTION>
!
! For grid cells with a fractional ice area, derive the ice surface 
! temperature from the area-averaged surface temperature (the blended
! result of the open-water values (SST) and the ice-covered value).
!
!</DESCRIPTION>

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: ims, ime, jms, jme    !-- start/end index for i/j in memory
      INTEGER, INTENT(IN) :: its, ite, jts, jte    !-- start/end index for i/j in tile
      INTEGER, INTENT(IN) :: itimestep             !-- timestep
      LOGICAL, INTENT(IN) :: sfc_layer_values      !-- True if there are surface layer routine values
                                                   !-- available from the ice portion of the grid point
                                                   !-- (i.e. called from a seaice_wrapper subroutine)
      LOGICAL, INTENT(IN) :: tice2tsk_if2cold      !-- True to set TSK_ICE to TSK.  This may be
                                                   !-- necessary to avoid unphysically low ice
                                                   !-- temperatures is there is a mis-match between
                                                   !-- ice fraction and surface temperature.

      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)    :: XICE        ! Ice fraction
      REAL                                , INTENT(IN)    :: XICE_THRESHOLD 
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)    :: TSK         ! Surface temperature (K)
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: SST         ! Sea surface temperature (K)
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT)   :: TSK_SEA     ! Sfc temp of open water portion of grid cell 
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT)   :: TSK_ICE     ! Sfc temp of ice oprtion of grid cell

! Local
      INTEGER :: i,j
      REAL    :: TICE_MIN

      TICE_MIN = 221.4

      DO j = JTS , JTE
         DO i = ITS , ITE
            IF ( ( XICE(i,j) >= XICE_THRESHOLD ) .AND. ( XICE(I,J) <= 1.0 ) ) THEN
 
               IF ( SST(i,j) < 271.4 ) THEN
                  SST(i,j) = 271.4
               ENDIF
 
               IF (sfc_layer_values) THEN
                  IF ( SST(i,j) > 273. .AND. itimestep <= 3) then
                     ! Why the dependence on the time step count, here?
                     IF ( XICE(i,j) >= 0.6 ) THEN
                        SST(i,j) = 271.4
                     ELSEIF ( XICE(i,j) >= 0.4 ) THEN
                        SST(i,j) = 273.
                     ELSEIF (XICE(i,j) >= 0.2 .AND. SST(i,j) > 275.) THEN
                        SST(i,j) = 275.
                     ELSEIF (SST(i,j) > 278.) THEN
                        SST(i,j) = 278.
                     ENDIF
                  ENDIF
               ENDIF
               TSK_SEA(i,j) = SST(i,j)
 
               IF ( tice2tsk_if2cold ) THEN
!------------------------------------------------------------------------------------
! This avoids unphysically low ice temperatures for grid cells with low ice fractions
! and low area-averaged temperatures.  This can happen when the initial ice fraction 
! and surface temperature come from different data sets.
!------------------------------------------------------------------------------------
                  TSK_ICE(i,j) = MIN( TSK(i,j), 273.15 )
               ELSE
                  TSK_ICE(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) * SST(i,j) ) / XICE(i,j)
                  IF ( TSK_ICE(i,j) < TICE_MIN ) TSK_ICE(i,j) = TICE_MIN
               ENDIF
 
               IF ( ( XICE(i,j) < 0.2 ) .AND. ( TSK(i,j) < 253.15 ) ) THEN
                  TSK_ICE(i,j) = 253.15
               ENDIF
               IF ( ( XICE(i,j) < 0.1 ) .AND. ( TSK(i,j) < 263.15 ) ) THEN
                  TSK_ICE(i,j) = 263.15
               ENDIF
            ELSE
               ! land/open-water point
               TSK_SEA(i,j) = TSK(i,j)
               TSK_ICE(i,j) = TSK(i,j)
            ENDIF
         ENDDO
      ENDDO

   END SUBROUTINE get_local_ice_tsk

!=======================================================================
!=======================================================================

END MODULE module_surface_driver
