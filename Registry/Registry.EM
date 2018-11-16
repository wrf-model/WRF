#  Registry file, EM

#INCLUDES
include registry.dimspec
include registry.em_shared_collection

# added to output 5 for ESMF
state   real   landmask            ij    misc          1     -     i0125rh056d=(interp_fcnm_imask)u=(copy_fcnm)   "LANDMASK"      "LAND MASK (1 FOR LAND, 0 FOR WATER)"  ""
state   real   lakemask            ij    misc          1     -     i012rhd=(interp_fcnm_imask)u=(copy_fcnm)      "LAKEMASK"      "LAKE MASK (1 FOR LAKE, 0 FOR NON-LAKE)"  ""

# Masked SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c_mask:lu_index,tslb,num_soil_layers,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"
# Simple SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state    real   SST              ij     misc        1         -     i01245rh0d=(interp_mask_field:lu_index,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state    real   SST_INPUT        ij     misc        1         -     rh   "SST_INPUT"              "SEA SURFACE TEMPERATURE FROM WRFLOWINPUT FILE" "K"


# Registry entries that are exclusive to Registry.EM

# Chem Scalars
rconfig   integer     chem_opt            namelist,physics	max_domains    0       rh       "chem_opt"              ""      ""
state   real    -              ikjftb   chem        1         -     -    -                                       
#
# Tracer Scalars: If you want full resolved and non-resolved dispersion, compile WRF-Chem
#
state   real    -          ikjftb  tracer        1         -     -    -                                       
state   real    tr17_1     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_1"         "tr17_1"     "Dimensionless"
state   real    tr17_2     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_2"         "tr17_2"     "Dimensionless"
state   real    tr17_3     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_3"         "tr17_3"     "Dimensionless"
state   real    tr17_4     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_4"         "tr17_4"     "Dimensionless"
state   real    tr17_5     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_5"         "tr17_5"     "Dimensionless"
state   real    tr17_6     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_6"         "tr17_6"     "Dimensionless"
state   real    tr17_7     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_7"         "tr17_7"     "Dimensionless"
state   real    tr17_8     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_8"         "tr17_8"     "Dimensionless"

package   tracer_test1  tracer_opt==2       -             tracer:tr17_1,tr17_2,tr17_3,tr17_4,tr17_5,tr17_6,tr17_7,tr17_8

package   restofwrf      use_wps_input==0            -             state:acfrcv,acfrst,acgrdflx,achfx,aclhf,aclwdnb,aclwdnbc,aclwdnt,aclwdntc,aclwupb,aclwupbc,aclwupt,aclwuptc,acsnom,acsnow,acswdnb,acswdnbc,acswdnt,acswdntc,acswupb,acswupbc,acswupt,acswuptc,activefrac,advh_t,advz_t,aerasy2d,aerocu,aerod,aeropcu,aerosolc_1,aerosolc_2,aerssa2d,akhs,akms,akpbl,alswnirdif,alswnirdir,alswvisdif,alswvisdir,angexp2d,aod5502d,aodtot,apr_as,apr_capma,apr_capme,apr_capmi,apr_gr,apr_mc,apr_st,apr_w,bb,bn2,br,bx,capg,cd,cda,cfrach,cfracl,cfracm,ch,chc_sfcdif,chr_sfcdif,ck,cka,cldefi,cldfra,cldfra_dp,cldfra_sh,cldt,cmc_sfcdif,cmr_sfcdif,coszen,coszen_ref,cplmask,ct,ctopo,ctopo2,cubot,cupflag,cuppt,cutop,czmean,defor11,defor12,defor13,defor22,defor23,defor33,delta_ysu,dew,dfi_moist,dfi_scalar,diffuse_frac,div,dmudt,dpsdt,dtaux3d,dtauy3d,dtw,dusfcg,dvsfcg,edt_out,el_pbl,embck,exch_h,exch_m,fbur,fgsn,fh,flhc,flqc,flx4,fm,fvb,g_urb2d,gg,glw,glw_dtmp,graupelnc,graupelncv,grdflx,grpl_colint,gsw,gx,hailcast_wdur,hailcast_wup_mask,hailnc,hailncv,hbot,hbotr,hfx,hfx_dtmp,hrang,ht_coarse,ht_input,ht_int,ht_shad,htop,htopr,iccg_in_den,iccg_in_num,imask_nostag,imask_xstag,imask_xystag,imask_ystag,infxsrt,kpbl,lap_hgt,lh,lh_dtmp,lh_urb2d,lnint,lowlyr,lu_state,lwcf,lwdnb,lwdnbc,lwdnt,lwdnt_dtmp,lwdntc,lwupb,lwupb_dtmp,lwupbc,lwupt,lwupt_dtmp,lwuptc,m_ps_1,m_ps_2,mass_flux,mol,mp_restart_state,mu_2m,mub_fine,mub_save,mudf,mut,muts,muu,muus,muv,muvs,nba_mij,nba_rij,nca,ncfrcv,ncfrst,nest_mask,nest_pos,noahres,numcolpts,o3rad,olr,ozmixm,p_hyd_w,pblh,pc_1,pc_2,pcb,phb_fine,pk1m,pr_ens,pratec,pratesh,psfc_dtmp,pshltr,q10,q2_dtmp,q2obs,qc_cu,qc_urb2d,qcg,qfx,qi_cu,qsg,qshltr,qvg,qvold,qz0,ra,rainbl,rainc,raincv,raincv_a,raincv_b,rainnc,rainncv,rainsh,rainshv,rdz,rdzw,regime,rlwtoa,rmol,rn_urb2d,rqcblten,rqccuten,rqcncuten,rqiblten,rqicuten,rqincuten,rqniblten,rqrcuten,rqscuten,rqvblten,rqvcuten,rqvften,rs,rswtoa,rthblten,rthcuten,rthften,rthraten,rthratenlw,rthratensw,ru,ru_m,ru_tend,rublten,rucuten,rv,rv_m,rv_tend,rvblten,rvcuten,rw,sfcevp,sfcexc,sfcheadrt,sfcrunoff,sh_urb2d,shadowmask,shall,sigmaez,sigmasfc,slopeez,slopesfc,slp_azi,smcrel,smstav,smstot,sn_ndg_new,sn_ndg_old,snotime,snownc,snowncv,sp_amp5,spforcc5,spforcs5,sst_input,sstsk,stnidprt,swcf,swddif,swddir,swddir_ref,swddirc,swddni,swddnic,swdnb,swdnb_dtmp,swdnbc,swdnt,swdnt_dtmp,swdntc,swdown,swdown_ref,swdownc,swnirdif,swnirdir,swnorm,swupb,swupb_dtmp,swupbc,swupt,swupt_dtmp,swuptc,swvisdif,swvisdir,t2_dtmp,t2obs,t_phy,t_save,tactive,taucloud,tb_urb2d,tbpvs0_state,tbpvs_state,tc_urb2d,tcloud_cup,tdly,tg_urb2d,th10,th2_dtmp,thz0,tke_pbl,tkesfcf,tlag,tr_urb2d,tratx,ts_qv_profile,ts_urb2d,tshltr,tsk_dtmp,tsnav,tstar,tyr,tyra,u10_dtmp,u_phy,u_save,uc_urb2d,udrunoff,uh,uratx,ust,ustm,utype_urb2d,uz0,v10_dtmp,v_phy,v_save,vdfg,vratx,vz0,w_colmean,wact_cup,wcloudbase,wspd,wstar,wstar_ysu,wulcl_cup,ww_m,xf_ens,xicem,xkhh,xkhv,xkmh,xkmv,xlaidyn,xxxb_urb2d,xxxc_urb2d,xxxg_urb2d,xxxr_urb2d,z_at_w,zol,zx,zy
 
package   realonly       use_wps_input==1            -            state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,lai12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,cl_gc,cf_gc,qv_gc,qr_gc,qc_gc,qs_gc,qi_gc,qg_gc,qh_gc,qni_gc,qnc_gc,icefrac_gc,prho_gc,pdrho_gc,qnr_gc,qns_gc,qng_gc,qnh_gc,soil_layers,soil_levels,st,sm,sw,soilt,soilm,sm000007,sm007028,sm028100,sm100255,st000007,st007028,st028100,st100255,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300,sw000010,sw010040,sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100,st100200,st010200,soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,fad0_urb2d,fad135_urb2d,fad45_urb2d,pad_urb2d,fad90_urb2d,rad_urb2d,car_urb2d,h2w_urb2d,svf_urb2d,z0s_urb2d,z0r_urb2d,z0m_urb2d,zds_urb2d,zdm_urb2d,zdr_urb2d,qnwfa_gc,qnwfa_now,qnwfa_jan,qnwfa_feb,qnwfa_mar,qnwfa_apr,qnwfa_may,qnwfa_jun,qnwfa_jul,qnwfa_aug,qnwfa_sep,qnwfa_oct,qnwfa_nov,qnwfa_dec,qnifa_gc,qnifa_now,qnifa_jan,qnifa_feb,qnifa_mar,qnifa_apr,qnifa_may,qnifa_jun,qnifa_jul,qnifa_aug,qnifa_sep,qnifa_oct,qnifa_nov,qnifa_dec,qntemp,qntemp2,hgtmaxw,pmaxw,tmaxw,umaxw,vmaxw,hgttrop,ptrop,ttrop,utrop,vtrop,urb_param

package   tconly         use_wps_input==2            -             state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,qv_gc,qr_gc,qc_gc,qs_gc,qi_gc,qg_gc,qh_gc,qni_gc,qnc_gc,qnr_gc,qns_gc,qng_gc,qnh_gc,icefrac_gc
