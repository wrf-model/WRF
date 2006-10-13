!WRF:MODEL_LAYER:INITIALIZATION
!
   subroutine chem_init (id,chem,dt,bioemdt,photdt,chemdt,stepbioe,  &
               stepphot,stepchem,z_at_w,g,aerwrf,config_flags,       &
               alt,t,p,CONVFAC,                                      &
               tauaer1,tauaer2,tauaer3,tauaer4,                      &
               gaer1,gaer2,gaer3,gaer4,                              &
               waer1,waer2,waer3,waer4,                              &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,chem_in_opt,       &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

!  IMPLICIT NONE
   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_phot_mad
   USE module_aerosols_sorgam
   USE module_dep_simple
   USE module_cbmz_initmixrats, only:  cbmz_init_wrf_mixrats
   USE module_mosaic_driver, only:  init_data_mosaic_asect
   USE module_mosaic_initmixrats, only:  mosaic_init_wrf_mixrats
   USE module_input_chem_data, only:  get_last_gas, gasprofile_init_pnnl

   real  , intent(in) :: bioemdt,photdt,chemdt,dt
   INTEGER,      INTENT(IN   ) :: chem_in_opt
   INTEGER,      INTENT(IN   ) :: id,                                &
                                  ids,ide, jds,jde, kds,kde,         &
                                  ims,ime, jms,jme, kms,kme,         &
                                  its,ite, jts,jte, kts,kte
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,   &
                               tauaer1,tauaer2,tauaer3,tauaer4,      &
                               gaer1,gaer2,gaer3,gaer4,              &
                               waer1,waer2,waer3,waer4
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               z_at_w,t,p,alt,convfac
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) ,       &
          INTENT(INOUT   ) ::                                        &
                              chem 
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               aerwrf
   real, INTENT (IN) :: g
   integer, intent(out) :: stepbioe,stepphot,stepchem
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
!
! local stuff
!
   integer :: i,j,k,l,numgas

#ifdef CHEM_DBG_I
    call print_chem_species_index( config_flags%chem_opt )
#endif
    numgas = get_last_gas(config_flags%chem_opt)

    stepbioe=nint(bioemdt*60./dt)
    stepphot=nint(photdt*60./dt)
    stepchem=nint(chemdt*60./dt)
    stepbioe=max(stepbioe,1)
    stepphot=max(stepphot,1)
    stepchem=max(stepchem,1)
    call wrf_debug( 15, 'in chem_init' )
!    print *,'stepbioe,stepphot,stepchem',stepbioe,stepphot,stepchem
!
! An alternative ozone profile option
!
    if ( (config_flags%gas_bc_opt == GAS_BC_PNNL) .or.   &
         (config_flags%gas_ic_opt == GAS_IC_PNNL) ) then
       call gasprofile_init_pnnl
    end if
!
! we are doing initialization here, first photolysis
!
   phot_select: SELECT CASE(config_flags%phot_opt)
     CASE (PHOTMAD)
     CALL wrf_debug(15,'call madronich phot initialization')
       call photmad_init(z_at_w,aerwrf,g,                            &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
!!$ Moved initialization of the mie vars to start_em since they are still
!!$ passed around in the met only version and when not using Fast-J.
!!$     CASE (PHOTFASTJ)
!!$       if( .NOT. config_flags%restart ) then
!!$          do j=jts,jte
!!$             do k=kts,kte
!!$                do i=its,ite
!!$                   tauaer1(i,k,j) = 0.
!!$                   tauaer2(i,k,j) = 0.
!!$                   tauaer3(i,k,j) = 0.
!!$                   tauaer4(i,k,j) = 0.
!!$                   gaer1(i,k,j) = 0.
!!$                   gaer2(i,k,j) = 0.
!!$                   gaer3(i,k,j) = 0.
!!$                   gaer4(i,k,j) = 0.
!!$                   waer1(i,k,j) = 0.
!!$                   waer2(i,k,j) = 0.
!!$                   waer3(i,k,j) = 0.
!!$                   waer4(i,k,j) = 0.
!!$                end do
!!$             end do
!!$          end do
!!$       end if
     END SELECT phot_select
!
! initialization for aerosols
!
   kpp_select: SELECT CASE(config_flags%chem_opt)
     CASE (RACM_KPP,RACMSORG_KPP, RACM_MIM_KPP)
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=370.
                   chem(i,k,j,p_ch4)=1.7
                   chem(i,k,j,p_ete)=chem(i,k,j,p_olt)
                   chem(i,k,j,p_ete)=epsilc
                   chem(i,k,j,p_udd)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_hket)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_api)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_lim)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_dien)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_macr)=chem(i,k,j,p_ete)
                enddo
             enddo
          enddo
   END SELECT kpp_select
   aer_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2SORG, RADM2SORG_KPP, RACMSORG,RACMSORG_KPP)
       CALL wrf_debug(15,'call MADE/SORGAM aerosols initialization')
#if ( NMM_CORE == 1 )
!  do j=jts,jte
!     jj=min(jde-1,j)
!     do k=kts,kte-1
!         kk=min(kde-1,k)
!         do i=its,ite
!             ii=min(ide-1,i)
!             convfac(i,k,j) = p(ii,kk,jj)/rgasuniv/t(ii,kk,jj)
!             if(i.eq.its.and.j.eq.jts)write(0,*) 'before aerinit',k,p(ii,kk,jj),convfac(i,k,j),t(ii,kk,jj)
!         enddo
!     enddo
!  enddo
#endif
#if ( EM_CORE == 1 )
!  do j=jts,jte
!     jj=min(jde-1,j)
!     do k=kts,kte-1
!         kk=min(kde-1,k)
!         do i=its,ite
!             ii=min(ide-1,i)
!             tempfac=(t(ii,kk,jj) + t0)*((p(ii,kk,jj) + pb(ii,kk,jj))/p1000mb)**rcp
!             convfac(i,k,j) = (p(ii,kk,jj)+pb(ii,kk,jj))/rgasuniv/tempfac
!             if(i.eq.its.and.j.eq.jts)print *,k,kk,alt(i,kk,j),convfac(i,k,j)
!         enddo
!     enddo
!  enddo
#endif

       call aerosols_sorgam_init(chem,convfac,z_at_w,                &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               chem_in_opt,config_flags%aer_ic_opt,                  &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
!...Convert aerosols to mixing ratio
        do l=p_so4aj,num_chem
           do j=jts,jte
              do k=kts,kte
                 kk = min(k,kte-1)
                 do i=its,ite
                    chem(i,k,j,l)=chem(i,kk,j,l)*alt(i,kk,j)
                 enddo
              enddo
           enddo
        enddo

        chem=max(chem,epsilc)


     CASE (CBMZ_MOSAIC_AA, CBMZ_MOSAIC_BB) 
       call wrf_debug(15,'call MOSAIC aerosols initialization')
       call init_data_mosaic_asect
       if( .NOT. config_flags%restart ) &
            call mosaic_init_wrf_mixrats(                            &
               0, config_flags,                                      &
               chem, alt, z_at_w, g,                                 &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

   END SELECT aer_select

   do nv=1,num_chem
      do j=jms,jme
         do i=ims,ime
            chem(i,kde,j,nv)=chem(i,kde-1,j,nv)
         enddo
      enddo
   enddo

!
! initialization for wesely (gas) dry deposition
!
   drydep_select: SELECT CASE(config_flags%drydep_opt)
     CASE (WESELY)
       CALL wrf_debug(15,'initializing dry dep (wesely)')
       call dep_init(id,config_flags,numgas)

   END SELECT drydep_select
!
! initialization for cbmz gas-phase chemistry 
!
   cbmz_select: SELECT CASE(config_flags%chem_opt)
     CASE (CBMZ, CBMZ_BB, CBMZ_MOSAIC_AA, CBMZ_MOSAIC_BB)
       CALL wrf_debug(15,'initializing cbmz gas-phase chemistry')
       if( .NOT. config_flags%restart ) &
            call cbmz_init_wrf_mixrats(config_flags,   &
               z_at_w, g,                              &
               chem, numgas,                           &
               ids,ide, jds,jde, kds,kde,              &
               ims,ime, jms,jme, kms,kme,              &
               its,ite, jts,jte, kts,kte               )
   END SELECT cbmz_select

    END SUBROUTINE chem_init

!-----------------------------------------------------------------------
#ifdef CHEM_DBG_I
subroutine print_chem_species_index( chem_opt )
  use module_state_description
  implicit none

  integer, intent(in) :: chem_opt

  print*,'Index numbers for chemistry species:'
!
! Gas species...
!
  select case (chem_opt)
  case (RADM2, RADM2SORG, RADM2_KPP,RADM2SORG_KPP)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
  case (RACM, RACMSORG,RACM_KPP,RACMSORG_KPP)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ete,"ete"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_co2,"co2"
     print*,p_ch4,"ch4"
     print*,p_udd,"udd"
     print*,p_hket,"hket"
     print*,p_api,"api"
     print*,p_lim,"lim"
     print*,p_dien,"dien"
     print*,p_macr,"macr"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
  case (CBMZ)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
     print*,p_hcl,"hcl"
     print*,p_ch3o2,"ch3o2"
     print*,p_ethp,"ethp"
     print*,p_ch3oh,"ch3oh"
     print*,p_c2h5oh,"c2h5oh"
     print*,p_par,"par"
     print*,p_to2,"to2"
     print*,p_cro,"cro"
     print*,p_open,"open"
     print*,p_op3,"op3"
     print*,p_c2o3,"c2o3"
     print*,p_ro2,"ro2"
     print*,p_ano2,"ano2"
     print*,p_nap,"nap"
     print*,p_xo2,"xo2"
     print*,p_xpar,"xpar"
     print*,p_isoprd,"isoprd"
     print*,p_isopp,"isopp"
     print*,p_isopn,"isopn"
     print*,p_isopo2,"isopo2"
     print*,p_dms,"dms"
     print*,p_msa,"msa"
     print*,p_dmso,"dmso"
     print*,p_dmso2,"dmso2"
     print*,p_ch3so2h,"ch3so2h"
     print*,p_ch3sch2oo,"ch3sch2oo"
     print*,p_ch3so2,"ch3so2"
     print*,p_ch3so3,"ch3so3"
     print*,p_ch3so2oo,"ch3so2oo"
     print*,p_ch3so2ch2oo,"ch3so2ch2oo"
     print*,p_mtf,"mtf"
  case (CBMZ_BB, CBMZ_MOSAIC_AA, CBMZ_MOSAIC_BB)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
     print*,p_hcl,"hcl"
     print*,p_ch3o2,"ch3o2"
     print*,p_ethp,"ethp"
     print*,p_ch3oh,"ch3oh"
     print*,p_c2h5oh,"c2h5oh"
     print*,p_par,"par"
     print*,p_to2,"to2"
     print*,p_cro,"cro"
     print*,p_open,"open"
     print*,p_op3,"op3"
     print*,p_c2o3,"c2o3"
     print*,p_ro2,"ro2"
     print*,p_ano2,"ano2"
     print*,p_nap,"nap"
     print*,p_xo2,"xo2"
     print*,p_xpar,"xpar"
     print*,p_isoprd,"isoprd"
     print*,p_isopp,"isopp"
     print*,p_isopn,"isopn"
     print*,p_isopo2,"isopo2"
  end select
!
! Aerosol species...
!
  select case (chem_opt)
  case (RADM2SORG, RADM2SORG_KPP, RACMSORG, RACMSORG_KPP)
     print*,p_so4aj,"so4aj"
     print*,p_so4ai,"so4ai"
     print*,p_nh4aj,"nh4aj"
     print*,p_nh4ai,"nh4ai"
     print*,p_no3aj,"no3aj"
     print*,p_no3ai,"no3ai"
     print*,p_orgaro1j,"orgaro1j"
     print*,p_orgaro1i,"orgaro1i"
     print*,p_orgaro2j,"orgaro2j"
     print*,p_orgaro2i,"orgaro2i"
     print*,p_orgalk1j,"orgalk1j"
     print*,p_orgalk1i,"orgalk1i"
     print*,p_orgole1j,"orgole1j"
     print*,p_orgole1i,"orgole1i"
     print*,p_orgba1j,"orgba1j"
     print*,p_orgba1i,"orgba1i"
     print*,p_orgba2j,"orgba2j"
     print*,p_orgba2i,"orgba2i"
     print*,p_orgba3j,"orgba3j"
     print*,p_orgba3i,"orgba3i"
     print*,p_orgba4j,"orgba4j"
     print*,p_orgba4i,"orgba4i"
     print*,p_orgpaj,"orgpaj"
     print*,p_orgpai,"orgpai"
     print*,p_ecj,"ecj"
     print*,p_eci,"eci"
     print*,p_p25j,"p25j"
     print*,p_p25i,"p25i"
     print*,p_antha,"antha"
     print*,p_seas,"seas"
     print*,p_soila,"soila"
     print*,p_nu0,"nu0"
     print*,p_ac0,"ac0"
     print*,p_corn,"corn"
  case (CBMZ_MOSAIC_AA)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
  case (CBMZ_MOSAIC_BB)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
     print*,p_so4_a05,"so4_a05"
     print*,p_no3_a05,"no3_a05"
     print*,p_cl_a05,"cl_a05"
     print*,p_nh4_a05,"nh4_a05"
     print*,p_na_a05,"na_a05"
     print*,p_oin_a05,"oin_a05"
     print*,p_oc_a05,"oc_a05"
     print*,p_bc_a05,"bc_a05"
     print*,p_hysw_a05,"hysw_a05"
     print*,p_water_a05,"water_a05"
     print*,p_num_a05,"num_a05"
     print*,p_so4_a06,"so4_a06"
     print*,p_no3_a06,"no3_a06"
     print*,p_cl_a06,"cl_a06"
     print*,p_nh4_a06,"nh4_a06"
     print*,p_na_a06,"na_a06"
     print*,p_oin_a06,"oin_a06"
     print*,p_oc_a06,"oc_a06"
     print*,p_bc_a06,"bc_a06"
     print*,p_hysw_a06,"hysw_a06"
     print*,p_water_a06,"water_a06"
     print*,p_num_a06,"num_a06"
     print*,p_so4_a07,"so4_a07"
     print*,p_no3_a07,"no3_a07"
     print*,p_cl_a07,"cl_a07"
     print*,p_nh4_a07,"nh4_a07"
     print*,p_na_a07,"na_a07"
     print*,p_oin_a07,"oin_a07"
     print*,p_oc_a07,"oc_a07"
     print*,p_bc_a07,"bc_a07"
     print*,p_hysw_a07,"hysw_a07"
     print*,p_water_a07,"water_a07"
     print*,p_num_a07,"num_a07"
     print*,p_so4_a08,"so4_a08"
     print*,p_no3_a08,"no3_a08"
     print*,p_cl_a08,"cl_a08"
     print*,p_nh4_a08,"nh4_a08"
     print*,p_na_a08,"na_a08"
     print*,p_oin_a08,"oin_a08"
     print*,p_oc_a08,"oc_a08"
     print*,p_bc_a08,"bc_a08"
     print*,p_hysw_a08,"hysw_a08"
     print*,p_water_a08,"water_a08"
     print*,p_num_a08,"num_a08"
  end select
end subroutine print_chem_species_index
#endif
