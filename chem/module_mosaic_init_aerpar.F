module module_mosaic_init_aerpar
  
  use module_data_mosaic_kind, only: r8

  implicit none

  private

  public:: mosaic_init_aer_params

  contains


!-------------------------------------------------------------------------------
  subroutine mosaic_init_aer_params
    !BSINGH - All initialzations for Mosiac model

    call load_mosaic_parameters
    
  end subroutine mosaic_init_aer_params


!-------------------------------------------------------------------------------
  subroutine load_mosaic_parameters
  !---------------------------------------------------------------------------------------!
  !BSINGH: load_mosaic_parameters subroutine is directly copied form the mosaic_box.25.f90 
  !        code
  !---------------------------------------------------------------------------------------!

  !---------------------------------------------------------------------------------------!
  ! Called only once per entire simulation to load gas and aerosol
  ! indices, parameters, physico-chemical constants, polynomial coeffs, etc.
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !---------------------------------------------------------------------------------------!

    use module_mosaic_soa_vbs, only: soa_vbs_load_params

    use module_data_mosaic_aero, only: ipmcmos_aero, no_aerosol, all_solid, all_liquid,   &
         mixed, nelectrolyte, naercomp, naer, Ncation, Nanion,                            &
         ngas_aerchtot, ngas_volatile, nsalt,                                             &
         jsulf_poor_NUM, jsulf_rich_NUM, MDRH_T_NUM, d_mdrh_DIM2, phasestate, aer_name,   &
         gas_name, ename, jnh4so4, jlvcite, jnh4hso4, jnh4msa, jnh4no3, jnh4cl, jna2so4,  &
         jna3hso4, jnahso4, jnamsa, jnano3, jnacl, jcano3, jcacl2, jcamsa2, jh2so4, jmsa, &
         jhno3, jhcl, jhhso4, jcaso4, jcaco3, joc, jbc, join, jaro1, jaro2, jalk1, jole1, &
         japi1, japi2, jlim1, jlim2, jh2o, jc_h, jc_nh4, jc_na, jc_ca, ja_hso4, ja_so4,   &
         ja_no3, ja_cl, ja_msa, ih2so4_g, ihno3_g, ihcl_g, inh3_g, imsa_g, iaro1_g,       &
         iaro2_g, ialk1_g, iole1_g, iapi1_g, iapi2_g, ilim1_g, ilim2_g, iso4_a, ino3_a,   &
         icl_a, inh4_a, imsa_a, iaro1_a, iaro2_a, ialk1_a, iole1_a, iapi1_a, iapi2_a,     &
         ilim1_a, ilim2_a, ico3_a, ina_a, ica_a, ioin_a, ioc_a, ibc_a,                    &
         isoa_first, jsoa_first, nmax_ASTEM, b_mtem,                                      &
         zc, za, b_zsr, a_zsr, aw_min, mw_electrolyte, dens_electrolyte,                  &
         partial_molar_vol, MW_c, MW_a, mw_aer_mac,dens_aer_mac, kappa_aer_mac,           &
         dens_comp_a, mw_comp_a, ref_index_a, mw_gas, v_molar_gas,                        &
         rtol_mesa, jsalt_index, jsulf_poor, jsulf_rich, Nmax_mesa, d_mdrh, msoa_flag1,   &
         use_cam5mam_soa_params

    ! local variables
    integer :: ia, iaer, ig, igas, ibin, ja, je, j_index
    logical :: use_mos31e_rz1_densities
    logical :: use_uniform_densities !BSINGH - 05/28/2013(RCE updates)
    logical :: use_sorgam_soa_species
    real(r8), dimension(nelectrolyte) :: G_MX,K_MX

    !BSINGH - 05/28/2013(RCE updates)
    use_mos31e_rz1_densities = .true.
    if ( use_mos31e_rz1_densities ) then
       use_uniform_densities = .false.
    else
       use_uniform_densities = .true.
       if (ipmcmos_aero > 0) use_uniform_densities = .false.
    end if
    !BSINGH - 05/28/2013(RCE updates ENDS)

    if (msoa_flag1 == 1) then
       use_sorgam_soa_species = .true.
    else
       use_sorgam_soa_species = .false.
    end if

    ! rce 2013-07-31 -
    !   using a local saved variable like "first" no longer works
    !   the calling routine needs to determine if/when this routine is needed
    !   if(first)then
    !      first=.false.

    !----------------------------------------------------------------
    ! control settings
    ! *** do not change mSIZE_FRAMEWORK here ***
    !      mSIZE_FRAMEWORK = mSECTIONAL        ! mMODAL or mSECTIONAL
    !      mDYNAMIC_SOLVER = mASTEM            ! mASTEM, mLSODES
    !      mGAS_AER_XFER   = mON               ! mON, mOFF

    ! ASTEM parameters
    nmax_ASTEM      = 301              ! max number of time steps in ASTEM
    !      alpha_ASTEM     = 1.0               ! choose a value between 0.01 and 1.0
    !      rtol_eqb_ASTEM = 0.01               ! equilibrium tolerance in ASTEM
    !      ptol_mol_ASTEM = 0.01               ! mol percent tolerance in ASTEM

    ! MESA parameters
    Nmax_MESA       = 80               ! max number of iterations in MESA_PTC
    rtol_mesa       = 0.01             ! MESA equilibrium tolerance
    !----------------------------------------------------------------

    !
    ! set species indices
    !    ixxx_a are for aerosol     species - they apply to aer and total arrays
    !    ixxx_g are for gas         species - they apply to gas array
    !    jyyy   are for electrolyte species - they apply to electrolyte and comp_a arrays
    !
    ! *** CRITICAL "RULES" ABOUT THESE INDICES ***
    !
    ! *** THE CODE WILL FAIL IF THEY ARE NOT FOLLOWED  ***
    !
    ! for the volatile inorganic and organic species (1 thru ngas_volatile)
    !    the ixxx_a and ixxx_g must match (be indentical) for each aerosol-gas pair
    !
    ! for the soa species, the ordering of the ixxx_a, ixxx_g, and jyyy
    !    must be identical
    !
    
    ! electrolyte indices (used for water content calculations)
    ! these indices are order sensitive
    ! inorganic species first
    jnh4so4    =  1    ! soluble
    jlvcite    =  2    ! soluble
    jnh4hso4   =  3    ! soluble
    jnh4msa    =  4    ! soluble: new
    jnh4no3    =  5    ! soluble
    jnh4cl     =  6    ! soluble
    jna2so4    =  7    ! soluble
    jna3hso4   =  8    ! soluble
    jnahso4    =  9    ! soluble
    jnamsa     = 10    ! soluble: new
    jnano3     = 11    ! soluble
    jnacl      = 12    ! soluble
    jcano3     = 13    ! soluble
    jcacl2     = 14    ! soluble
    jcamsa2    = 15    ! soluble     nsalt
    jh2so4     = 16    ! soluble
    jmsa       = 17    ! soluble
    jhno3      = 18    ! soluble
    jhcl       = 19    ! soluble
    jhhso4     = 20    ! soluble
    jcaso4     = 21    ! insoluble
    jcaco3     = 22    ! insoluble
    joc        = 23    ! insoluble - part of naercomp
    jbc        = 24    ! insoluble - part of naercomp
    join       = 25    ! insoluble - part of naercomp

    ! aerosol and gas indices for inorganic species
    iso4_a     =  1 ;  ih2so4_g   =  1     
    ino3_a     =  2 ;  ihno3_g    =  2     
    icl_a      =  3 ;  ihcl_g     =  3     
    inh4_a     =  4 ;  inh3_g     =  4     
    imsa_a     =  5 ;  imsa_g     =  5     

    ia = imsa_a ; ig = imsa_g ; je = join

    ! aerosol, gas, and electrolyte indices for sorgam secondary organic species
    if ( use_sorgam_soa_species ) then
       isoa_first = ia+1 ; jsoa_first = je+1
       iaro1_a    = ia+1 ;  iaro1_g    = ig+1 ;  jaro1      = je+1     
       iaro2_a    = ia+2 ;  iaro2_g    = ig+2 ;  jaro2      = je+2     
       ialk1_a    = ia+3 ;  ialk1_g    = ig+3 ;  jalk1      = je+3     
       iole1_a    = ia+4 ;  iole1_g    = ig+4 ;  jole1      = je+4     
       iapi1_a    = ia+5 ;  iapi1_g    = ig+5 ;  japi1      = je+5     
       iapi2_a    = ia+6 ;  iapi2_g    = ig+6 ;  japi2      = je+6     
       ilim1_a    = ia+7 ;  ilim1_g    = ig+7 ;  jlim1      = je+7     
       ilim2_a    = ia+8 ;  ilim2_g    = ig+8 ;  jlim2      = je+8     
       ia = ia+8 ; ig = ig+8 ; je = je+8
    else
       isoa_first = ia+1 ; jsoa_first = je+1
       iaro1_a    = -999888777 ;  iaro1_g    = -999888777 ;  jaro1      = -999888777     
       iaro2_a    = -999888777 ;  iaro2_g    = -999888777 ;  jaro2      = -999888777     
       ialk1_a    = -999888777 ;  ialk1_g    = -999888777 ;  jalk1      = -999888777     
       iole1_a    = -999888777 ;  iole1_g    = -999888777 ;  jole1      = -999888777     
       iapi1_a    = -999888777 ;  iapi1_g    = -999888777 ;  japi1      = -999888777     
       iapi2_a    = -999888777 ;  iapi2_g    = -999888777 ;  japi2      = -999888777     
       ilim1_a    = -999888777 ;  ilim1_g    = -999888777 ;  jlim1      = -999888777     
       ilim2_a    = -999888777 ;  ilim2_g    = -999888777 ;  jlim2      = -999888777     
       ia = ia+1 ; ig = ig+1 ; je = je+1
    end if

    ! electrolyte indices for other species
    jh2o       = je+1    ! water - part of naercomp

    ! aerosol indices for other species
    ico3_a     = ia+1
    ina_a      = ia+2
    ica_a      = ia+3
    ioin_a     = ia+4
    ioc_a      = ia+5
    ibc_a      = ia+6

    ! gas indices for other species
    ! ico2_g   = 14  ! *** currently not used
                     ! *** if co3_a was treated as a volatile inorganic, 
                     !     then ico3_a and ico2_g would have to be 6 

    ! local aerosol ions
    ! cations
    jc_h       =  1
    jc_nh4     =  2
    jc_na      =  3
    jc_ca      =  4
    !
    ! anions
    ja_hso4    =  1
    ja_so4     =  2
    ja_no3     =  3
    ja_cl      =  4
    ja_msa     =  5
    ! ja_co3   =  6   ! *** currently not used


    if (msoa_flag1 >= 1000) call soa_vbs_load_params( 1 )


    !--------------------------------------------------------------------
    ! phase state names
    phasestate(no_aerosol) = "NOAERO"
    phasestate(all_solid)  = "SOLID "
    phasestate(all_liquid) = "LIQUID"
    phasestate(mixed)      = "MIXED "

    ! names of aer species
    do iaer = 1, naer
       write( aer_name(iaer), '(a,i4.4)' ) 'aer', iaer  ! default
    end do

    aer_name(iso4_a) = "SO4"
    aer_name(ino3_a) = "NO3"
    aer_name(icl_a)  = "Cl "
    aer_name(inh4_a) = "NH4"
    aer_name(ioc_a)  = "OC "
    aer_name(imsa_a) = "MSA"
    aer_name(ico3_a) = "CO3"
    aer_name(ina_a)  = "Na "
    aer_name(ica_a)  = "Ca "
    aer_name(ibc_a)  = "BC "
    aer_name(ioin_a) = "OIN"
    if ( use_sorgam_soa_species ) then
    aer_name(iaro1_a)= "ARO1"
    aer_name(iaro2_a)= "ARO2"
    aer_name(ialk1_a)= "ALK1"
    aer_name(iole1_a)= "OLE1"
    aer_name(iapi1_a)= "API1"
    aer_name(iapi2_a)= "API2"
    aer_name(ilim1_a)= "LIM1"
    aer_name(ilim2_a)= "LIM2"
    end if

    ! names of gas species
    do igas = 1, ngas_aerchtot
       write( gas_name(igas), '(a,i4.4)' ) 'gas', igas  ! default
    end do

    gas_name(ih2so4_g) = "H2SO4"
    gas_name(ihno3_g)  = "HNO3 "
    gas_name(ihcl_g)   = "HCl  "
    gas_name(inh3_g)   = "NH3  "
    gas_name(imsa_g)   = "MSA  "
    if ( use_sorgam_soa_species ) then
    gas_name(iaro1_g)   = "ARO1 "
    gas_name(iaro2_g)   = "ARO2 "
    gas_name(ialk1_g)   = "ALK1 "
    gas_name(iole1_g)   = "OLE1 "
    gas_name(iapi1_g)   = "API1 "
    gas_name(iapi2_g)   = "API2 "
    gas_name(ilim1_g)   = "LIM1 "
    gas_name(ilim2_g)   = "LIM2 "
    end if

    ! names of electrolytes
    ename(jnh4so4) = "AmSO4"
    ename(jlvcite) = "(NH4)3H(SO4)2"
    ename(jnh4hso4)= "NH4HSO4"
    ename(jnh4msa) = "CH3SO3NH4"
    ename(jnh4no3) = "NH4NO3"
    ename(jnh4cl)  = "NH4Cl"
    ename(jnacl)   = "NaCl"
    ename(jnano3)  = "NaNO3"
    ename(jna2so4) = "Na2SO4"
    ename(jna3hso4)= "Na3H(SO4)2"
    ename(jnamsa)  = "CH3SO3Na"
    ename(jnahso4) = "NaHSO4"
    ename(jcaso4)  = "CaSO4"
    ename(jcamsa2) = "(CH3SO3)2Ca"
    ename(jcano3)  = "Ca(NO3)2"
    ename(jcacl2)  = "CaCl2"
    ename(jcaco3)  = "CaCO3"
    ename(jh2so4)  = "H2SO4"
    ename(jhhso4)  = "HHSO4"
    ename(jhno3)   = "HNO3"
    ename(jhcl)    = "HCl"
    ename(jmsa)    = "CH3SO3H"

    ! molecular weights of electrolytes
    mw_electrolyte(jnh4so4) = 132.0
    mw_electrolyte(jlvcite) = 247.0
    mw_electrolyte(jnh4hso4)= 115.0
    mw_electrolyte(jnh4msa) = 113.0
    mw_electrolyte(jnh4no3) = 80.0
    mw_electrolyte(jnh4cl)  = 53.5
    mw_electrolyte(jnacl)   = 58.5
    mw_electrolyte(jnano3)  = 85.0
    mw_electrolyte(jna2so4) = 142.0
    mw_electrolyte(jna3hso4)= 262.0
    mw_electrolyte(jnahso4) = 120.0
    mw_electrolyte(jnamsa)  = 118.0
    mw_electrolyte(jcaso4)  = 136.0
    mw_electrolyte(jcamsa2) = 230.0
    mw_electrolyte(jcano3)  = 164.0
    mw_electrolyte(jcacl2)  = 111.0
    mw_electrolyte(jcaco3)  = 100.0
    mw_electrolyte(jh2so4)  = 98.0
    mw_electrolyte(jhno3)   = 63.0
    mw_electrolyte(jhcl)    = 36.5
    mw_electrolyte(jmsa)    = 96.0


    ! molecular weights of ions [g/mol]
    MW_c(jc_h)  =  1.0
    MW_c(jc_nh4)= 18.0
    MW_c(jc_na) = 23.0
    MW_c(jc_ca) = 40.0

    MW_a(ja_so4) = 96.0
    MW_a(ja_hso4)= 97.0
    MW_a(ja_no3) = 62.0
    MW_a(ja_cl)  = 35.5
    MW_a(ja_msa) = 95.0


    ! magnitude of the charges on ions
    zc(jc_h)   = 1
    zc(jc_nh4) = 1
    zc(jc_na)  = 1
    zc(jc_ca)  = 2

    za(ja_hso4)= 1
    za(ja_so4) = 2
    za(ja_no3) = 1
    za(ja_cl)  = 1
    za(ja_msa) = 1


    ! densities of pure electrolytes in g/cc
    dens_electrolyte(jnh4so4)  = 1.8
    dens_electrolyte(jlvcite)  = 1.8
    dens_electrolyte(jnh4hso4) = 1.8
    dens_electrolyte(jnh4msa)  = 1.8 ! assumed same as nh4hso4
    dens_electrolyte(jnh4no3)  = 1.8
    dens_electrolyte(jnh4cl)   = 1.8
    dens_electrolyte(jnacl)    = 2.2
    dens_electrolyte(jnano3)   = 2.2
    dens_electrolyte(jna2so4)  = 2.2
    dens_electrolyte(jna3hso4) = 2.2
    dens_electrolyte(jnahso4)  = 2.2
    dens_electrolyte(jnamsa)   = 2.2 ! assumed same as nahso4
    dens_electrolyte(jcaso4)   = 2.6
    dens_electrolyte(jcamsa2)  = 2.6   ! assumed same as caso4
    dens_electrolyte(jcano3)   = 2.6
    dens_electrolyte(jcacl2)   = 2.6
    dens_electrolyte(jcaco3)   = 2.6
    dens_electrolyte(jh2so4)   = 1.8
    dens_electrolyte(jhhso4)   = 1.8
    dens_electrolyte(jhno3)    = 1.8
    dens_electrolyte(jhcl)     = 1.8
    dens_electrolyte(jmsa)     = 1.8 ! assumed same as h2so4
    if ( use_uniform_densities ) then!BSINGH - 05/28/2013(RCE updates)
       do je = 1, nelectrolyte
          dens_electrolyte(je) = 1.6
       enddo
    endif!BSINGH - 05/28/2013(RCE updates)

    ! densities of compounds in g/cc
    dens_comp_a(jnh4so4)  = 1.8
    dens_comp_a(jlvcite)  = 1.8
    dens_comp_a(jnh4hso4) = 1.8
    dens_comp_a(jnh4msa)  = 1.8        ! assumed same as nh4hso4
    dens_comp_a(jnh4no3)  = 1.7
    dens_comp_a(jnh4cl)   = 1.5
    dens_comp_a(jnacl)    = 2.2
    dens_comp_a(jnano3)   = 2.2
    dens_comp_a(jna2so4)  = 2.2
    dens_comp_a(jna3hso4) = 2.2
    dens_comp_a(jnahso4)  = 2.2
    dens_comp_a(jnamsa)   = 2.2        ! assumed same as nahso4
    dens_comp_a(jcaso4)   = 2.6
    dens_comp_a(jcamsa2)  = 2.6        ! assumed same as caso4
    dens_comp_a(jcano3)   = 2.6
    dens_comp_a(jcacl2)   = 2.6
    dens_comp_a(jcaco3)   = 2.6
    dens_comp_a(jh2so4)   = 1.8
    dens_comp_a(jhhso4)   = 1.8
    dens_comp_a(jhno3)    = 1.8
    dens_comp_a(jhcl)     = 1.8
    dens_comp_a(jmsa)     = 1.8        ! assumed same as h2so4
    dens_comp_a(joc)      = 1.0
    dens_comp_a(jbc)      = 1.8
    dens_comp_a(join)     = 2.6
    if ( use_sorgam_soa_species ) then
    dens_comp_a(jaro1)    = 1.0
    dens_comp_a(jaro2)    = 1.0
    dens_comp_a(jalk1)    = 1.0
    dens_comp_a(jole1)    = 1.0
    dens_comp_a(japi1)    = 1.0
    dens_comp_a(japi2)    = 1.0
    dens_comp_a(jlim1)    = 1.0
    dens_comp_a(jlim2)    = 1.0
    end if
    dens_comp_a(jh2o)     = 1.0
    !BSINGH - 05/28/2013(RCE updates)
    ! following for comparison with mos31d_bs2 and. mos31e_rz1
    if ( use_mos31e_rz1_densities ) then
       dens_comp_a(joc)      = 1.4
       if ( use_sorgam_soa_species ) then
       dens_comp_a(jaro1)    = 1.4
       dens_comp_a(jaro2)    = 1.4
       dens_comp_a(jalk1)    = 1.4
       dens_comp_a(jole1)    = 1.4
       dens_comp_a(japi1)    = 1.4
       dens_comp_a(japi2)    = 1.4
       dens_comp_a(jlim1)    = 1.4
       dens_comp_a(jlim2)    = 1.4
       end if
    end if

    if ( use_uniform_densities ) then
       !BSINGH - 05/28/2013(RCE updates ENDS)
       do je = 1, naercomp
          dens_comp_a(je) = 1.6
       enddo
       !BSINGH - 05/28/2013(RCE updates)
    endif

    if (ipmcmos_aero > 0) then
       dens_comp_a(jnh4no3)  = 1.8
       dens_comp_a(jnh4cl)   = 1.8
       if ( use_sorgam_soa_species ) then
       dens_comp_a(jaro1)    = 1.4
       dens_comp_a(jaro2)    = 1.4
       dens_comp_a(jalk1)    = 1.4
       dens_comp_a(jole1)    = 1.4
       dens_comp_a(japi1)    = 1.4
       dens_comp_a(japi2)    = 1.4
       dens_comp_a(jlim1)    = 1.4
       dens_comp_a(jlim2)    = 1.4
       end if
    endif
    !BSINGH - 05/28/2013(RCE updates ENDS)

    ! molecular weights of generic aerosol species
    mw_aer_mac(1:naer) = 200.0  ! default

    mw_aer_mac(iso4_a) = 96.0
    mw_aer_mac(ino3_a) = 62.0
    mw_aer_mac(icl_a)  = 35.5
    mw_aer_mac(imsa_a) = 95.0  ! CH3SO3
    mw_aer_mac(ico3_a) = 60.0
    mw_aer_mac(inh4_a) = 18.0
    mw_aer_mac(ina_a)  = 23.0
    mw_aer_mac(ica_a)  = 40.0
    mw_aer_mac(ioin_a) = 1.0           ! not used
    mw_aer_mac(ibc_a)  = 1.0           ! not used
    mw_aer_mac(ioc_a)  = 1.0   ! 200 assumed for primary organics
    if ( use_sorgam_soa_species ) then
    mw_aer_mac(iaro1_a)= 150.0
    mw_aer_mac(iaro2_a)= 150.0
    mw_aer_mac(ialk1_a)= 140.0
    mw_aer_mac(iole1_a)= 140.0
    mw_aer_mac(iapi1_a)= 184.0
    mw_aer_mac(iapi2_a)= 184.0
    mw_aer_mac(ilim1_a)= 200.0
    mw_aer_mac(ilim2_a)= 200.0
    end if

    ! molecular weights of compounds
    mw_comp_a(jnh4so4) = 132.0
    mw_comp_a(jlvcite) = 247.0
    mw_comp_a(jnh4hso4)= 115.0
    mw_comp_a(jnh4msa) = 113.0
    mw_comp_a(jnh4no3) =  80.0
    mw_comp_a(jnh4cl)  =  53.5
    mw_comp_a(jnacl)   =  58.5
    mw_comp_a(jnano3)  =  85.0
    mw_comp_a(jna2so4) = 142.0
    mw_comp_a(jna3hso4)= 262.0
    mw_comp_a(jnahso4) = 120.0
    mw_comp_a(jnamsa)  = 118.0
    mw_comp_a(jcaso4)  = 136.0
    mw_comp_a(jcamsa2) = 230.0
    mw_comp_a(jcano3)  = 164.0
    mw_comp_a(jcacl2)  = 111.0
    mw_comp_a(jcaco3)  = 100.0
    mw_comp_a(jh2so4)  =  98.0
    mw_comp_a(jhhso4)  =  98.0
    mw_comp_a(jhno3)   =  63.0
    mw_comp_a(jhcl)    =  36.5
    mw_comp_a(jmsa)    =  96.0
    mw_comp_a(joc)     =   1.0
    mw_comp_a(jbc)     =   1.0
    mw_comp_a(join)    =   1.0
    if ( use_sorgam_soa_species ) then
    mw_comp_a(jaro1)   = 150.0
    mw_comp_a(jaro2)   = 150.0
    mw_comp_a(jalk1)   = 140.0
    mw_comp_a(jole1)   = 140.0
    mw_comp_a(japi1)   = 184.0
    mw_comp_a(japi2)   = 184.0
    mw_comp_a(jlim1)   = 200.0
    mw_comp_a(jlim2)   = 200.0
    end if
    mw_comp_a(jh2o)    = 18.0

    !BSINGH - 05/28/2013(RCE updates)
    ! partmc-2.2.1 jun-2012
    !#     dens (kg/m^3)   ions in soln (1)    molec wght (kg/mole)   kappa (1)
    ! SO4            1800                0                   96d-3      0.65
    ! NO3            1800                0                   62d-3      0.65
    ! Cl             2200                0                   35.5d-3    0.53
    ! NH4            1800                0                   18d-3      0.65
    ! MSA            1800                0                   95d-3      0.53
    ! ARO1           1400                0                   150d-3     0.1
    ! ARO2           1400                0                   150d-3     0.1
    ! ALK1           1400                0                   140d-3     0.1
    ! OLE1           1400                0                   140d-3     0.1
    ! API1           1400                0                   184d-3     0.1
    ! API2           1400                0                   184d-3     0.1
    ! LIM1           1400                0                   200d-3     0.1
    ! LIM2           1400                0                   200d-3     0.1
    ! CO3            2600                0                   60d-3      0.53
    ! Na             2200                0                   23d-3      0.53
    ! Ca             2600                0                   40d-3      0.53
    ! OIN            2600                0                   1d-3       0.1
    ! OC             1000                0                   1d-3       0.001
    ! BC             1800                0                   1d-3       0
    ! H2O            1000                0                   18d-3      0
    !BSINGH - 05/28/2013(RCE updates ENDS)

    ! densities of generic aerosol species
    dens_aer_mac(1:naer) = 1.0   ! default

    dens_aer_mac(iso4_a) = 1.8 ! used
    dens_aer_mac(ino3_a) = 1.8 ! used
    dens_aer_mac(icl_a)  = 2.2 ! used
    dens_aer_mac(imsa_a) = 1.8 ! used
    dens_aer_mac(ico3_a) = 2.6 ! used
    dens_aer_mac(inh4_a) = 1.8 ! used
    dens_aer_mac(ina_a)  = 2.2 ! used
    dens_aer_mac(ica_a)  = 2.6 ! used
    dens_aer_mac(ioin_a) = 2.6 ! used
    dens_aer_mac(ioc_a)  = 1.0 ! used
    dens_aer_mac(ibc_a)  = 1.8 ! used
    if ( use_sorgam_soa_species ) then
    dens_aer_mac(iaro1_a)= 1.0
    dens_aer_mac(iaro2_a)= 1.0
    dens_aer_mac(ialk1_a)= 1.0
    dens_aer_mac(iole1_a)= 1.0
    dens_aer_mac(iapi1_a)= 1.0
    dens_aer_mac(iapi2_a)= 1.0
    dens_aer_mac(ilim1_a)= 1.0
    dens_aer_mac(ilim2_a)= 1.0
    end if
    !BSINGH - 05/28/2013(RCE updates)
    ! following for comparison with mos31d_bs2 and. mos31e_rz1
    if ( use_mos31e_rz1_densities ) then
       dens_aer_mac(ioc_a)  = 1.4
       if ( use_sorgam_soa_species ) then
       dens_aer_mac(iaro1_a)= 1.4
       dens_aer_mac(iaro2_a)= 1.4
       dens_aer_mac(ialk1_a)= 1.4
       dens_aer_mac(iole1_a)= 1.4
       dens_aer_mac(iapi1_a)= 1.4
       dens_aer_mac(iapi2_a)= 1.4
       dens_aer_mac(ilim1_a)= 1.4
       dens_aer_mac(ilim2_a)= 1.4
       end if
    end if

    if ( use_uniform_densities ) then
       !BSINGH - 05/28/2013(RCE updates ENDS)

       do iaer = 1, naer
          dens_aer_mac(iaer) = 1.6
       enddo
    endif!BSINGH - 05/28/2013(RCE updates)

    if (ipmcmos_aero > 0) then
       ! use partmc-mosaic densities
       dens_aer_mac(1:19) = (/ &
            1.80, 1.80, 2.20, 1.80, 1.80, 1.40, 1.40, 1.40, 1.40, 1.40, &
            1.40, 1.40, 1.40, 2.60, 2.20, 2.60, 2.60, 1.00, 1.80 /)!BSINGH - 05/28/2013(RCE updates)
       !           so4   no3   cl    nh4   msa   aro1  aro2  alk1  ole1  api1
       !           api2  lim1  lim2  co3   na    ca    oin   oc    bc
    end if

    if ( use_cam5mam_soa_params > 0 ) then
       dens_aer_mac(ioc_a)   = 1.0
       dens_comp_a(joc)      = 1.0
       ! for oc, leave mw=1 because some of the mosaic code requires this
       if ( 1 <= ilim2_a .and. ilim2_a <= naer ) then
       dens_aer_mac(ilim2_a) = 1.0
       dens_comp_a(jlim2)    = 1.0
       mw_aer_mac(ilim2_a)   = 150.0
       mw_comp_a(jlim2)      = 150.0
       end if
    end if

    ! kappa values (hygroscopicities) of generic aerosol species
    !
    ! for calculation of ccn properties, kappa of electrolytes
    !    should be used
    ! the multi-dimensional sectional code needs a "fixed" kappa
    !    for each generic aerosol species, just as the older
    !    1d sectional code needs a "fixed" dry density
    kappa_aer_mac(1:naer)  = 0.1  ! default

    kappa_aer_mac(iso4_a)  = 0.65
    kappa_aer_mac(ino3_a)  = 0.65
    kappa_aer_mac(imsa_a)  = 0.65
    kappa_aer_mac(inh4_a)  = 0.65
    kappa_aer_mac(icl_a)   = 0.65
    kappa_aer_mac(ina_a)   = 0.65
    kappa_aer_mac(ico3_a)  = 0.001  ! ??
    kappa_aer_mac(ica_a)   = 0.001  ! ??
    kappa_aer_mac(ioin_a)  = 0.001
    kappa_aer_mac(ioc_a)   = 0.001
    kappa_aer_mac(ibc_a)   = 0.001
    if ( use_sorgam_soa_species ) then
    kappa_aer_mac(iaro1_a) = 0.1
    kappa_aer_mac(iaro2_a) = 0.1
    kappa_aer_mac(ialk1_a) = 0.1
    kappa_aer_mac(iole1_a) = 0.1
    kappa_aer_mac(iapi1_a) = 0.1
    kappa_aer_mac(iapi2_a) = 0.1
    kappa_aer_mac(ilim1_a) = 0.1
    kappa_aer_mac(ilim2_a) = 0.1
    end if
    !BSINGH - 05/28/2013(RCE updates)
    if (ipmcmos_aero > 0) then
       ! use partmc-mosaic kappas
       kappa_aer_mac(1:19) = (/ &
            0.65, 0.65, 0.53, 0.65, 0.53, 0.10, 0.10, 0.10, 0.10, 0.10, &
            0.10, 0.10, 0.10, 0.53, 0.53, 0.53, 0.10, 0.001, 0.0 /)
       !           so4   no3   cl    nh4   msa   aro1  aro2  alk1  ole1  api1
       !           api2  lim1  lim2  co3   na    ca    oin   oc    bc
    end if
    !BSINGH - 05/28/2013(RCE updates ENDS)

    ! partial molar volumes of condensing gases
    partial_molar_vol(1:ngas_aerchtot)   = 200.0  ! default

    partial_molar_vol(ih2so4_g) = 51.83
    partial_molar_vol(ihno3_g)  = 31.45
    partial_molar_vol(ihcl_g)   = 20.96
    partial_molar_vol(inh3_g)   = 24.03
    partial_molar_vol(imsa_g)   = 53.33
    if ( use_sorgam_soa_species ) then
    partial_molar_vol(iaro1_g)  = 150.0
    partial_molar_vol(iaro2_g)  = 150.0
    partial_molar_vol(ialk1_g)  = 140.0
    partial_molar_vol(iole1_g)  = 140.0
    partial_molar_vol(iapi1_g)  = 184.0
    partial_molar_vol(iapi2_g)  = 184.0
    partial_molar_vol(ilim1_g)  = 200.0
    partial_molar_vol(ilim2_g)  = 200.0
    end if

    ! molecular weights of condensing gases
    mw_gas(1:ngas_aerchtot) = 200.0  ! default
    mw_gas(ih2so4_g) = 98.0
    mw_gas(ihno3_g)  = 63.0
    mw_gas(ihcl_g)   = 36.5
    mw_gas(inh3_g)   = 17.0
    mw_gas(imsa_g)   = 96.0
    if ( use_sorgam_soa_species ) then
    mw_gas(iaro1_g)  = 150.0
    mw_gas(iaro2_g)  = 150.0
    mw_gas(ialk1_g)  = 140.0
    mw_gas(iole1_g)  = 140.0
    mw_gas(iapi1_g)  = 184.0
    mw_gas(iapi2_g)  = 184.0
    mw_gas(ilim1_g)  = 200.0
    mw_gas(ilim2_g)  = 200.0
    end if

    ! used to calculate diffusivities of condensing gases
    v_molar_gas(1:ngas_aerchtot) = 60.0  ! default
    v_molar_gas(ih2so4_g)= 42.88
    v_molar_gas(ihno3_g) = 24.11
    v_molar_gas(ihcl_g)  = 21.48
    v_molar_gas(inh3_g)  = 14.90
    v_molar_gas(imsa_g)  = 58.00

    ! refractive index
    ref_index_a(jnh4so4) = cmplx(1.52,0.)
    ref_index_a(jlvcite) = cmplx(1.50,0.)
    ref_index_a(jnh4hso4)= cmplx(1.47,0.)
    ref_index_a(jnh4msa) = cmplx(1.50,0.)      ! assumed
    ref_index_a(jnh4no3) = cmplx(1.50,0.)
    ref_index_a(jnh4cl)  = cmplx(1.50,0.)
    ref_index_a(jnacl)   = cmplx(1.45,0.)
    ref_index_a(jnano3)  = cmplx(1.50,0.)
    ref_index_a(jna2so4) = cmplx(1.50,0.)
    ref_index_a(jna3hso4)= cmplx(1.50,0.)
    ref_index_a(jnahso4) = cmplx(1.50,0.)
    ref_index_a(jnamsa)  = cmplx(1.50,0.)      ! assumed
    ref_index_a(jcaso4)  = cmplx(1.56,0.006)
    ref_index_a(jcamsa2) = cmplx(1.56,0.006)   ! assumed
    ref_index_a(jcano3)  = cmplx(1.56,0.006)
    ref_index_a(jcacl2)  = cmplx(1.52,0.006)
    ref_index_a(jcaco3)  = cmplx(1.68,0.006)
    ref_index_a(jh2so4)  = cmplx(1.43,0.)
    ref_index_a(jhhso4)  = cmplx(1.43,0.)
    ref_index_a(jhno3)   = cmplx(1.50,0.)
    ref_index_a(jhcl)    = cmplx(1.50,0.)
    ref_index_a(jmsa)    = cmplx(1.43,0.)      ! assumed
    ref_index_a(joc)      = cmplx(1.45,0.)
    ref_index_a(jbc)      = cmplx(1.82,0.74)
    ref_index_a(join)    = cmplx(1.55,0.006)
    if ( use_sorgam_soa_species ) then
    ref_index_a(jaro1)   = cmplx(1.45,0.)
    ref_index_a(jaro2)   = cmplx(1.45,0.)
    ref_index_a(jalk1)   = cmplx(1.45,0.)
    ref_index_a(jole1)   = cmplx(1.45,0.)
    ref_index_a(japi1)   = cmplx(1.45,0.)
    ref_index_a(japi2)   = cmplx(1.45,0.)
    ref_index_a(jlim1)   = cmplx(1.45,0.)
    ref_index_a(jlim2)   = cmplx(1.45,0.)
    end if
    ref_index_a(jh2o)    = cmplx(1.33,0.)

    ! jsalt_index
    jsalt_index(jnh4so4) = 5           ! AS
    jsalt_index(jlvcite) = 2           ! LV
    jsalt_index(jnh4hso4)= 1           ! AB
    jsalt_index(jnh4no3) = 2           ! AN
    jsalt_index(jnh4cl)  = 1           ! AC
    jsalt_index(jna2so4) = 60          ! SS
    jsalt_index(jnahso4) = 10          ! SB
    jsalt_index(jnano3)  = 40          ! SN
    jsalt_index(jnacl)   = 10          ! SC
    jsalt_index(jcano3)  = 120 ! CN
    jsalt_index(jcacl2)  = 80          ! CC
    jsalt_index(jnh4msa) = 0           ! AM    zero for now
    jsalt_index(jnamsa)  = 0           ! SM    zero for now
    jsalt_index(jcamsa2) = 0           ! CM    zero for now

    ! Aerosol Indices
    !  AC = 1, AN = 2, AS = 5, SC = 10, SN = 40, SS = 60, CC = 80, CN = 120,
    !  AB = 1, LV = 2, SB = 10
    !
    ! SULFATE-POOR DOMAIN
    jsulf_poor(1)   =  1       !       AC
    jsulf_poor(2)   =  2       !       AN
    jsulf_poor(5)   =  3       !       AS
    jsulf_poor(10)  =  4       !       SC
    jsulf_poor(40)  =  5       !       SN
    jsulf_poor(60)  =  6       !       SS
    jsulf_poor(80)  =  7       !       CC
    jsulf_poor(120) =  8       !       CN
    jsulf_poor(3)   =  9       !       AN + AC
    jsulf_poor(6)   =  10      !       AS + AC
    jsulf_poor(7)   =  11      !       AS + AN
    jsulf_poor(8)   =          12      !       AS + AN + AC
    jsulf_poor(11)  =  13      !       SC + AC
    jsulf_poor(41)  =  14      !       SN + AC
    jsulf_poor(42)  =  15      !       SN + AN
    jsulf_poor(43)  =  16      !       SN + AN + AC
    jsulf_poor(50)  =  17      !       SN + SC
    jsulf_poor(51)  =  18      !       SN + SC + AC
    jsulf_poor(61)  =  19      !       SS + AC
    jsulf_poor(62)  =  20      !       SS + AN
    jsulf_poor(63)  =  21      !       SS + AN + AC
    jsulf_poor(65)  =  22      !       SS + AS
    jsulf_poor(66)  =  23      !       SS + AS + AC
    jsulf_poor(67)  =  24      !       SS + AS + AN
    jsulf_poor(68)  =  25      !       SS + AS + AN + AC
    jsulf_poor(70)  =  26      !       SS + SC
    jsulf_poor(71)  =  27      !       SS + SC + AC
    jsulf_poor(100) =  28      !       SS + SN
    jsulf_poor(101) =  29      !       SS + SN + AC
    jsulf_poor(102) =  30      !       SS + SN + AN
    jsulf_poor(103) =  31      !       SS + SN + AN + AC
    jsulf_poor(110) =  32      !       SS + SN + SC
    jsulf_poor(111) =  33      !       SS + SN + SC + AC
    jsulf_poor(81)  =  34      !       CC + AC
    jsulf_poor(90)  =  35      !       CC + SC
    jsulf_poor(91)  =  36      !       CC + SC + AC
    jsulf_poor(121) =  37      !       CN + AC
    jsulf_poor(122) =  38      !       CN + AN
    jsulf_poor(123) =  39      !       CN + AN + AC
    jsulf_poor(130) =  40      !       CN + SC
    jsulf_poor(131) =  41      !       CN + SC + AC
    jsulf_poor(160) =  42      !       CN + SN
    jsulf_poor(161) =  43      !       CN + SN + AC
    jsulf_poor(162) =  44      !       CN + SN + AN
    jsulf_poor(163) =  45      !       CN + SN + AN + AC
    jsulf_poor(170) =  46      !       CN + SN + SC
    jsulf_poor(171) =  47      !       CN + SN + SC + AC
    jsulf_poor(200) =  48      !       CN + CC
    jsulf_poor(201) =  49      !       CN + CC + AC
    jsulf_poor(210) =  50      !       CN + CC + SC
    jsulf_poor(211) =  51      !       CN + CC + SC + AC
    !
    ! SULFATE-RICH DOMAIN
    jsulf_rich(1)   =  52      !       AB
    jsulf_rich(2)   =  53      !       LV
    jsulf_rich(10)  =  54      !       SB
    jsulf_rich(3)   =  55      !       AB + LV
    jsulf_rich(7)   =  56      !       AS + LV
    jsulf_rich(70)  =  57      !       SS + SB
    jsulf_rich(62)  =  58      !       SS + LV
    jsulf_rich(67)  =  59      !       SS + AS + LV
    jsulf_rich(61)  =  60      !       SS + AB
    jsulf_rich(63)  =  61      !       SS + LV + AB
    jsulf_rich(11)  =  62      !       SB + AB
    jsulf_rich(71)  =  63      !       SS + SB + AB
    jsulf_rich(5)   =  3       !       AS
    jsulf_rich(60)  =  6       !       SS
    jsulf_rich(65)  =  22      !       SS + AS



    !
    ! polynomial coefficients for binary molality (used in ZSR equation)
    !
    !
    ! a_zsr for aw < 0.97
    !
    ! (NH4)2SO4
    je = jnh4so4
    a_zsr(1,je)  =  1.30894
    a_zsr(2,je)  = -7.09922
    a_zsr(3,je)  =  20.62831
    a_zsr(4,je)  = -32.19965
    a_zsr(5,je)  =  25.17026
    a_zsr(6,je)  = -7.81632
    aw_min(je)   = 0.1
    !
    ! (NH4)3H(SO4)2
    je = jlvcite
    a_zsr(1,je)  =  1.10725
    a_zsr(2,je)  = -5.17978
    a_zsr(3,je)  =  12.29534
    a_zsr(4,je)  = -16.32545
    a_zsr(5,je)  =  11.29274
    a_zsr(6,je)  = -3.19164
    aw_min(je)   = 0.1
    !
    ! NH4HSO4
    je = jnh4hso4
    a_zsr(1,je)  =  1.15510
    a_zsr(2,je)  = -3.20815
    a_zsr(3,je)  =  2.71141
    a_zsr(4,je)  =  2.01155
    a_zsr(5,je)  = -4.71014
    a_zsr(6,je)  =  2.04616
    aw_min(je)   = 0.1
    !
    ! NH4MSA (assumed same as NH4HSO4)
    je = jnh4msa
    a_zsr(1,je)  =  1.15510
    a_zsr(2,je)  = -3.20815
    a_zsr(3,je)  =  2.71141
    a_zsr(4,je)  =  2.01155
    a_zsr(5,je)  = -4.71014
    a_zsr(6,je)  =  2.04616
    aw_min(je)   = 0.1
    !
    ! NH4NO3
    je = jnh4no3
    a_zsr(1,je)  =  0.43507
    a_zsr(2,je)  =  6.38220
    a_zsr(3,je)  = -30.19797
    a_zsr(4,je)  =  53.36470
    a_zsr(5,je)  = -43.44203
    a_zsr(6,je)  =  13.46158
    aw_min(je)   = 0.1
    !
    ! NH4Cl: revised on Nov 13, 2003. based on Chan and Ha (1999) JGR.
    je = jnh4cl
    a_zsr(1,je)  =  0.45309
    a_zsr(2,je)  =  2.65606
    a_zsr(3,je)  = -14.7730
    a_zsr(4,je)  =  26.2936
    a_zsr(5,je)  = -20.5735
    a_zsr(6,je)  =  5.94255
    aw_min(je)   = 0.1
    !
    ! NaCl
    je = jnacl
    a_zsr(1,je)  =  0.42922
    a_zsr(2,je)  = -1.17718
    a_zsr(3,je)  =  2.80208
    a_zsr(4,je)  = -4.51097
    a_zsr(5,je)  =  3.76963
    a_zsr(6,je)  = -1.31359
    aw_min(je)   = 0.1
    !
    ! NaNO3
    je = jnano3
    a_zsr(1,je)  =  1.34966
    a_zsr(2,je)  = -5.20116
    a_zsr(3,je)  =  11.49011
    a_zsr(4,je)  = -14.41380
    a_zsr(5,je)  =  9.07037
    a_zsr(6,je)  = -2.29769
    aw_min(je)   = 0.1
    !
    ! Na2SO4
    je = jna2so4
    a_zsr(1,je)  =  0.39888
    a_zsr(2,je)  = -1.27150
    a_zsr(3,je)  =  3.42792
    a_zsr(4,je)  = -5.92632
    a_zsr(5,je)  =  5.33351
    a_zsr(6,je)  = -1.96541
    aw_min(je)   = 0.1
    !
    ! Na3H(SO4)2  added on 1/14/2004
    je = jna3hso4
    a_zsr(1,je)  =  0.31480
    a_zsr(2,je)  = -1.01087
    a_zsr(3,je)  =  2.44029
    a_zsr(4,je)  = -3.66095
    a_zsr(5,je)  =  2.77632
    a_zsr(6,je)  = -0.86058
    aw_min(je)   = 0.1
    !
    ! NaHSO4
    je = jnahso4
    a_zsr(1,je)  =  0.62764
    a_zsr(2,je)  = -1.63520
    a_zsr(3,je)  =  4.62531
    a_zsr(4,je)  = -10.06925
    a_zsr(5,je)  =  10.33547
    a_zsr(6,je)  = -3.88729
    aw_min(je)   = 0.1
    !
    ! NaMSA (assumed same as NaHSO4)
    je = jnamsa
    a_zsr(1,je)  =  0.62764
    a_zsr(2,je)  = -1.63520
    a_zsr(3,je)  =  4.62531
    a_zsr(4,je)  = -10.06925
    a_zsr(5,je)  =  10.33547
    a_zsr(6,je)  = -3.88729
    aw_min(je)   = 0.1
    !
    ! Ca(NO3)2
    je = jcano3
    a_zsr(1,je)  =  0.38895
    a_zsr(2,je)  = -1.16013
    a_zsr(3,je)  =  2.16819
    a_zsr(4,je)  = -2.23079
    a_zsr(5,je)  =  1.00268
    a_zsr(6,je)  = -0.16923
    aw_min(je)   = 0.1
    !
    ! CaCl2: Kim and Seinfeld
    je = jcacl2
    a_zsr(1,je)  =  0.29891
    a_zsr(2,je)  = -1.31104
    a_zsr(3,je)  =  3.68759
    a_zsr(4,je)  = -5.81708
    a_zsr(5,je)  =  4.67520
    a_zsr(6,je)  = -1.53223
    aw_min(je)   = 0.1
    !
    ! H2SO4
    je = jh2so4
    a_zsr(1,je) =  0.32751
    a_zsr(2,je) = -1.00692
    a_zsr(3,je) =  2.59750
    a_zsr(4,je) = -4.40014
    a_zsr(5,je) =  3.88212
    a_zsr(6,je) = -1.39916
    aw_min(je)  = 0.1
    !
    ! MSA (assumed same as H2SO4)
    je = jmsa
    a_zsr(1,je) =  0.32751
    a_zsr(2,je) = -1.00692
    a_zsr(3,je) =  2.59750
    a_zsr(4,je) = -4.40014
    a_zsr(5,je) =  3.88212
    a_zsr(6,je) = -1.39916
    aw_min(je)  = 0.1
    !
    ! HHSO4
    je = jhhso4
    a_zsr(1,je) =  0.32751
    a_zsr(2,je) = -1.00692
    a_zsr(3,je) =  2.59750
    a_zsr(4,je) = -4.40014
    a_zsr(5,je) =  3.88212
    a_zsr(6,je) = -1.39916
    aw_min(je)  = 1.0
    !
    ! HNO3
    je = jhno3
    a_zsr(1,je) =  0.75876
    a_zsr(2,je) = -3.31529
    a_zsr(3,je) =  9.26392
    a_zsr(4,je) = -14.89799
    a_zsr(5,je) =  12.08781
    a_zsr(6,je) = -3.89958
    aw_min(je)  = 0.1
    !
    ! HCl
    je = jhcl
    a_zsr(1,je) =  0.31133
    a_zsr(2,je) = -0.79688
    a_zsr(3,je) =  1.93995
    a_zsr(4,je) = -3.31582
    a_zsr(5,je) =  2.93513
    a_zsr(6,je) = -1.07268
    aw_min(je)  = 0.1
    !
    ! CaSO4
    je = jcaso4
    a_zsr(1,je)  =  0.0
    a_zsr(2,je)  =  0.0
    a_zsr(3,je)  =  0.0
    a_zsr(4,je)  =  0.0
    a_zsr(5,je)  =  0.0
    a_zsr(6,je)  =  0.0
    aw_min(je)   = 1.0
    !
    ! Ca(MSA)2 (assumed same as Ca(NO3)2)
    je = jcamsa2
    a_zsr(1,je)  =  0.38895
    a_zsr(2,je)  = -1.16013
    a_zsr(3,je)  =  2.16819
    a_zsr(4,je)  = -2.23079
    a_zsr(5,je)  =  1.00268
    a_zsr(6,je)  = -0.16923
    aw_min(je)   = 0.1
    !
    ! CaCO3
    je = jcaco3
    a_zsr(1,je)  =  0.0
    a_zsr(2,je)  =  0.0
    a_zsr(3,je)  =  0.0
    a_zsr(4,je)  =  0.0
    a_zsr(5,je)  =  0.0
    a_zsr(6,je)  =  0.0
    aw_min(je)   = 1.0



    !-------------------------------------------
    ! b_zsr for aw => 0.97 to 0.99999
    !
    ! (NH4)2SO4
    b_zsr(jnh4so4)  = 28.0811
    !
    ! (NH4)3H(SO4)2
    b_zsr(jlvcite)  = 14.7178
    !
    ! NH4HSO4
    b_zsr(jnh4hso4) = 29.4779
    !
    ! NH4MSA
    b_zsr(jnh4msa)  = 29.4779 ! assumed same as NH4HSO4
    !
    ! NH4NO3
    b_zsr(jnh4no3)  = 33.4049
    !
    ! NH4Cl
    b_zsr(jnh4cl)   = 30.8888
    !
    ! NaCl
    b_zsr(jnacl)    = 29.8375
    !
    ! NaNO3
    b_zsr(jnano3)   = 32.2756
    !
    ! Na2SO4
    b_zsr(jna2so4)  = 27.6889
    !
    ! Na3H(SO4)2
    b_zsr(jna3hso4) = 14.2184
    !
    ! NaHSO4
    b_zsr(jnahso4)  = 28.3367
    !
    ! NaMSA
    b_zsr(jnamsa)   = 28.3367 ! assumed same as NaHSO4
    !
    ! Ca(NO3)2
    b_zsr(jcano3)   = 18.3661
    !
    ! CaCl2
    b_zsr(jcacl2)   = 20.8792
    !
    ! H2SO4
    b_zsr(jh2so4)   = 26.7347
    !
    ! HHSO4
    b_zsr(jhhso4)   = 26.7347
    !
    ! HNO3
    b_zsr(jhno3)    = 28.8257
    !
    ! HCl
    b_zsr(jhcl)     = 27.7108
    !
    ! MSA
    b_zsr(jmsa)     = 26.7347 ! assumed same as H2SO4
    !
    ! CaSO4
    b_zsr(jcaso4)   = 0.0
    !
    ! Ca(MSA)2
    b_zsr(jcamsa2)  = 18.3661 ! assumed same as Ca(NO3)2
    !
    ! CaCO3
    b_zsr(jcaco3)   = 0.0









    !-------------------------------------------
    ! Li and Lu (2001) Surface tension model
    ! G_MX [mol/cm^2]; K_MX [-]
    !
    ! (NH4)2SO4
    G_MX(jnh4so4)  = -8.79e-7*1.e-4
    K_MX(jnh4so4)  =  3.84e+1
    !
    ! (NH4)3H(SO4)2
    G_MX(jlvcite)  = -8.79e-7*1.e-4    ! assumed same as (NH4)2SO4
    K_MX(jlvcite)  =  3.84e+1          ! assumed same as (NH4)2SO4
    !
    ! NH4HSO4
    G_MX(jnh4hso4) = -8.79e-7*1.e-4    ! assumed same as (NH4)2SO4
    K_MX(jnh4hso4) =  3.84e+1          ! assumed same as (NH4)2SO4
    !
    ! NH4MSA
    G_MX(jnh4msa)  = -8.79e-7*1.e-4    ! assumed same as (NH4)2SO4
    K_MX(jnh4msa)  =  3.84e+1          ! assumed same as (NH4)2SO4
    !
    ! NH4NO3
    G_MX(jnh4no3)  = -3.08e-6*1.e-4
    K_MX(jnh4no3)  =  4.89e-1
    !
    ! NH4Cl
    G_MX(jnh4cl)   = -1.01e-6*1.e-4
    K_MX(jnh4cl)   =  1.3
    !
    ! NaCl
    G_MX(jnacl)    = -1.05e-6*1.e-4
    K_MX(jnacl)    =  1.2
    !
    ! NaNO3
    G_MX(jnano3)   = -1.66e-6*1.e-4
    K_MX(jnano3)   =  1.25
    !
    ! Na2SO4
    G_MX(jna2so4)  = -8.37e-7*1.e-4
    K_MX(jna2so4)  =  7.57e+1
    !
    ! Na3H(SO4)2
    G_MX(jna3hso4) = -8.37e-7*1.e-4    ! assumed same as Na2SO4
    K_MX(jna3hso4) =  7.57e+1          ! assumed same as Na2SO4
    !
    ! NaHSO4
    G_MX(jnahso4)  = -8.37e-7*1.e-4    ! assumed same as Na2SO4
    K_MX(jnahso4)  =  7.57e+1          ! assumed same as Na2SO4
    !
    ! NaMSA
    G_MX(jnamsa)   = -8.37e-7*1.e-4
    K_MX(jnamsa)   =  7.57e+1
    !
    ! Ca(NO3)2
    G_MX(jcano3)   = -4.88e-7*1.e-4    ! assumed same as CaCl2
    K_MX(jcano3)   =  1.50e+1          ! assumed same as CaCl2
    !
    ! CaCl2
    G_MX(jcacl2)   = -4.88e-7*1.e-4
    K_MX(jcacl2)   =  1.50e+1
    !
    ! H2SO4
    G_MX(jh2so4)   = -6.75e-8*1.e-4
    K_MX(jh2so4)   =  1.65e+3
    !
    ! HHSO4
    G_MX(jh2so4)   = -6.75e-8*1.e-4    ! assumed same as H2SO4
    K_MX(jh2so4)   =  1.65e+3          ! assumed same as H2SO4
    !
    ! HNO3
    G_MX(jhno3)    =  8.05e-7*1.e-4
    K_MX(jhno3)    =  1.06e-1
    !
    ! HCl
    G_MX(jhcl)     =  4.12e-7*1.e-4
    K_MX(jhcl)     =  4.68e-3
    !&

    ! MSA
    G_MX(jmsa)     =  8.05e-7*1.e-4    ! assumed same as HNO3
    K_MX(jmsa)     =  1.06e-1          ! assumed same as HNO3
    !
    ! CaSO4
    G_MX(jmsa)     =  0.0*1.e-4        ! assumed
    K_MX(jmsa)     =  0.0              ! assumed
    !
    ! Ca(MSA)2
    G_MX(jcamsa2)  =  0.0*1.e-4        ! assumed
    K_MX(jcamsa2)  =  0.0              ! assumed
    !
    ! CaCO3
    G_MX(jcaco3)   =  0.0*1.e-4        ! assumed
    K_MX(jcaco3)   =  0.0              ! assumed







    !----------------------------------------------------------------
    ! parameters for MTEM mixing rule (Zaveri, Easter, and Wexler, 2005)
    ! log_gamZ(jA,jE)   A in E
    !----------------------------------------------------------------
    !
    b_mtem(:,:,:) = 0.0_r8 !BSINGH - Temporarily initialized, please modify if required *Ask dick about it* the code blows up if i initialize it with nan
    ! (NH4)2SO4 in E
    jA = jnh4so4

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.94685
    b_mtem(2,jA,jE) = 17.3328
    b_mtem(3,jA,jE) = -64.8441
    b_mtem(4,jA,jE) = 122.7070
    b_mtem(5,jA,jE) = -114.4373
    b_mtem(6,jA,jE) = 41.6811

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -2.7503
    b_mtem(2,jA,jE) = 4.3806
    b_mtem(3,jA,jE) = -1.1110
    b_mtem(4,jA,jE) = -1.7005
    b_mtem(5,jA,jE) = -4.4207
    b_mtem(6,jA,jE) = 5.1990

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -2.06952
    b_mtem(2,jA,jE) = 7.1240
    b_mtem(3,jA,jE) = -24.4274
    b_mtem(4,jA,jE) = 51.1458
    b_mtem(5,jA,jE) = -54.2056
    b_mtem(6,jA,jE) = 22.0606

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -2.17361
    b_mtem(2,jA,jE) = 15.9919
    b_mtem(3,jA,jE) = -69.0952
    b_mtem(4,jA,jE) = 139.8860
    b_mtem(5,jA,jE) = -134.9890
    b_mtem(6,jA,jE) = 49.8877

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -4.4370
    b_mtem(2,jA,jE) = 24.0243
    b_mtem(3,jA,jE) = -76.2437
    b_mtem(4,jA,jE) = 128.6660
    b_mtem(5,jA,jE) = -110.0900
    b_mtem(6,jA,jE) = 37.7414

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = -1.5394
    b_mtem(2,jA,jE) = 5.8671
    b_mtem(3,jA,jE) = -22.7726
    b_mtem(4,jA,jE) = 47.0547
    b_mtem(5,jA,jE) = -47.8266
    b_mtem(6,jA,jE) = 18.8489

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = -0.35750
    b_mtem(2,jA,jE) = -3.82466
    b_mtem(3,jA,jE) = 4.55462
    b_mtem(4,jA,jE) = 5.05402
    b_mtem(5,jA,jE) = -14.7476
    b_mtem(6,jA,jE) = 8.8009

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = -2.15146
    b_mtem(2,jA,jE) = 5.50205
    b_mtem(3,jA,jE) = -19.1476
    b_mtem(4,jA,jE) = 39.1880
    b_mtem(5,jA,jE) = -39.9460
    b_mtem(6,jA,jE) = 16.0700

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = -2.52604
    b_mtem(2,jA,jE) = 9.76022
    b_mtem(3,jA,jE) = -35.2540
    b_mtem(4,jA,jE) = 71.2981
    b_mtem(5,jA,jE) = -71.8207
    b_mtem(6,jA,jE) = 28.0758

    !
    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -4.13219
    b_mtem(2,jA,jE) = 13.8863
    b_mtem(3,jA,jE) = -34.5387
    b_mtem(4,jA,jE) = 56.5012
    b_mtem(5,jA,jE) = -51.8702
    b_mtem(6,jA,jE) = 19.6232

    !
    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -2.53482
    b_mtem(2,jA,jE) = 12.3333
    b_mtem(3,jA,jE) = -46.1020
    b_mtem(4,jA,jE) = 90.4775
    b_mtem(5,jA,jE) = -88.1254
    b_mtem(6,jA,jE) = 33.4715

    !
    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -3.23425
    b_mtem(2,jA,jE) = 18.7842
    b_mtem(3,jA,jE) = -78.7807
    b_mtem(4,jA,jE) = 161.517
    b_mtem(5,jA,jE) = -154.940
    b_mtem(6,jA,jE) = 56.2252

    !
    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -1.25316
    b_mtem(2,jA,jE) = 7.40960
    b_mtem(3,jA,jE) = -34.8929
    b_mtem(4,jA,jE) = 72.8853
    b_mtem(5,jA,jE) = -72.4503
    b_mtem(6,jA,jE) = 27.7706


    !-----------------
    ! NH4NO3 in E
    jA = jnh4no3

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -3.5201
    b_mtem(2,jA,jE) = 21.6584
    b_mtem(3,jA,jE) = -72.1499
    b_mtem(4,jA,jE) = 126.7000
    b_mtem(5,jA,jE) = -111.4550
    b_mtem(6,jA,jE) = 38.5677

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -2.2630
    b_mtem(2,jA,jE) = -0.1518
    b_mtem(3,jA,jE) = 17.0898
    b_mtem(4,jA,jE) = -36.7832
    b_mtem(5,jA,jE) = 29.8407
    b_mtem(6,jA,jE) = -7.9314

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -1.3851
    b_mtem(2,jA,jE) = -0.4462
    b_mtem(3,jA,jE) = 8.4567
    b_mtem(4,jA,jE) = -11.5988
    b_mtem(5,jA,jE) = 2.9802
    b_mtem(6,jA,jE) = 1.8132

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -1.7602
    b_mtem(2,jA,jE) = 10.4044
    b_mtem(3,jA,jE) = -35.5894
    b_mtem(4,jA,jE) = 64.3584
    b_mtem(5,jA,jE) = -57.8931
    b_mtem(6,jA,jE) = 20.2141

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -3.24346
    b_mtem(2,jA,jE) = 16.2794
    b_mtem(3,jA,jE) = -48.7601
    b_mtem(4,jA,jE) = 79.2246
    b_mtem(5,jA,jE) = -65.8169
    b_mtem(6,jA,jE) = 22.1500

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = -1.75658
    b_mtem(2,jA,jE) = 7.71384
    b_mtem(3,jA,jE) = -22.7984
    b_mtem(4,jA,jE) = 39.1532
    b_mtem(5,jA,jE) = -34.6165
    b_mtem(6,jA,jE) = 12.1283

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = -0.97178
    b_mtem(2,jA,jE) = 6.61964
    b_mtem(3,jA,jE) = -26.2353
    b_mtem(4,jA,jE) = 50.5259
    b_mtem(5,jA,jE) = -47.6586
    b_mtem(6,jA,jE) = 17.5074

    ! in CaCl2 added on 12/22/2003
    jE = jcacl2
    b_mtem(1,jA,jE) = -0.41515
    b_mtem(2,jA,jE) = 6.44101
    b_mtem(3,jA,jE) = -26.4473
    b_mtem(4,jA,jE) = 49.0718
    b_mtem(5,jA,jE) = -44.2631
    b_mtem(6,jA,jE) = 15.3771

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = -1.20644
    b_mtem(2,jA,jE) = 5.70117
    b_mtem(3,jA,jE) = -18.2783
    b_mtem(4,jA,jE) = 31.7199
    b_mtem(5,jA,jE) = -27.8703
    b_mtem(6,jA,jE) = 9.7299

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = -0.680862
    b_mtem(2,jA,jE) = 3.59456
    b_mtem(3,jA,jE) = -10.7969
    b_mtem(4,jA,jE) = 17.8434
    b_mtem(5,jA,jE) = -15.3165
    b_mtem(6,jA,jE) = 5.17123


    !----------
    ! NH4Cl in E
    jA = jnh4cl

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.8850
    b_mtem(2,jA,jE) = 20.6970
    b_mtem(3,jA,jE) = -70.6810
    b_mtem(4,jA,jE) = 124.3690
    b_mtem(5,jA,jE) = -109.2880
    b_mtem(6,jA,jE) = 37.5831

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -1.9386
    b_mtem(2,jA,jE) = 1.3238
    b_mtem(3,jA,jE) = 11.8500
    b_mtem(4,jA,jE) = -28.1168
    b_mtem(5,jA,jE) = 21.8543
    b_mtem(6,jA,jE) = -5.1671

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.9559
    b_mtem(2,jA,jE) = 0.8121
    b_mtem(3,jA,jE) = 4.3644
    b_mtem(4,jA,jE) = -8.9258
    b_mtem(5,jA,jE) = 4.2362
    b_mtem(6,jA,jE) = 0.2891

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 0.0377
    b_mtem(2,jA,jE) = 6.0752
    b_mtem(3,jA,jE) = -30.8641
    b_mtem(4,jA,jE) = 63.3095
    b_mtem(5,jA,jE) = -61.0070
    b_mtem(6,jA,jE) = 22.1734

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -1.8336
    b_mtem(2,jA,jE) = 12.8160
    b_mtem(3,jA,jE) = -42.3388
    b_mtem(4,jA,jE) = 71.1816
    b_mtem(5,jA,jE) = -60.5708
    b_mtem(6,jA,jE) = 20.5853

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = -0.1429
    b_mtem(2,jA,jE) = 2.3561
    b_mtem(3,jA,jE) = -10.4425
    b_mtem(4,jA,jE) = 20.8951
    b_mtem(5,jA,jE) = -20.7739
    b_mtem(6,jA,jE) = 7.9355

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = 0.76235
    b_mtem(2,jA,jE) = 3.08323
    b_mtem(3,jA,jE) = -23.6772
    b_mtem(4,jA,jE) = 53.7415
    b_mtem(5,jA,jE) = -55.4043
    b_mtem(6,jA,jE) = 21.2944

    ! in CaCl2 (revised on 11/27/2003)
    jE = jcacl2
    b_mtem(1,jA,jE) = 1.13864
    b_mtem(2,jA,jE) = -0.340539
    b_mtem(3,jA,jE) = -8.67025
    b_mtem(4,jA,jE) = 22.8008
    b_mtem(5,jA,jE) = -24.5181
    b_mtem(6,jA,jE) = 9.3663

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 2.42532
    b_mtem(2,jA,jE) = -14.1755
    b_mtem(3,jA,jE) = 38.804
    b_mtem(4,jA,jE) = -58.2437
    b_mtem(5,jA,jE) = 43.5431
    b_mtem(6,jA,jE) = -12.5824

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 0.330337
    b_mtem(2,jA,jE) = 0.0778934
    b_mtem(3,jA,jE) = -2.30492
    b_mtem(4,jA,jE) = 4.73003
    b_mtem(5,jA,jE) = -4.80849
    b_mtem(6,jA,jE) = 1.78866



    !----------
    ! Na2SO4 in E
    jA = jna2so4

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.6982
    b_mtem(2,jA,jE) = 22.9875
    b_mtem(3,jA,jE) = -98.9840
    b_mtem(4,jA,jE) = 198.0180
    b_mtem(5,jA,jE) = -188.7270
    b_mtem(6,jA,jE) = 69.0548

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -2.4844
    b_mtem(2,jA,jE) = 6.5420
    b_mtem(3,jA,jE) = -9.8998
    b_mtem(4,jA,jE) = 11.3884
    b_mtem(5,jA,jE) = -13.6842
    b_mtem(6,jA,jE) = 7.7411

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -1.3325
    b_mtem(2,jA,jE) = 13.0406
    b_mtem(3,jA,jE) = -56.1935
    b_mtem(4,jA,jE) = 107.1170
    b_mtem(5,jA,jE) = -97.3721
    b_mtem(6,jA,jE) = 34.3763

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -1.2832
    b_mtem(2,jA,jE) = 12.8526
    b_mtem(3,jA,jE) = -62.2087
    b_mtem(4,jA,jE) = 130.3876
    b_mtem(5,jA,jE) = -128.2627
    b_mtem(6,jA,jE) = 48.0340

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -3.5384
    b_mtem(2,jA,jE) = 21.3758
    b_mtem(3,jA,jE) = -70.7638
    b_mtem(4,jA,jE) = 121.1580
    b_mtem(5,jA,jE) = -104.6230
    b_mtem(6,jA,jE) = 36.0557


    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = 0.2175
    b_mtem(2,jA,jE) = -0.5648
    b_mtem(3,jA,jE) = -8.0288
    b_mtem(4,jA,jE) = 25.9734
    b_mtem(5,jA,jE) = -32.3577
    b_mtem(6,jA,jE) = 14.3924

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = -0.309617
    b_mtem(2,jA,jE) = -1.82899
    b_mtem(3,jA,jE) = -1.5505
    b_mtem(4,jA,jE) = 13.3847
    b_mtem(5,jA,jE) = -20.1284
    b_mtem(6,jA,jE) = 9.93163

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = -0.259455
    b_mtem(2,jA,jE) = -0.819366
    b_mtem(3,jA,jE) = -4.28964
    b_mtem(4,jA,jE) = 16.4305
    b_mtem(5,jA,jE) = -21.8546
    b_mtem(6,jA,jE) = 10.3044

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = -1.84257
    b_mtem(2,jA,jE) = 7.85788
    b_mtem(3,jA,jE) = -29.9275
    b_mtem(4,jA,jE) = 61.7515
    b_mtem(5,jA,jE) = -63.2308
    b_mtem(6,jA,jE) = 24.9542

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -1.05891
    b_mtem(2,jA,jE) = 2.84831
    b_mtem(3,jA,jE) = -21.1827
    b_mtem(4,jA,jE) = 57.5175
    b_mtem(5,jA,jE) = -64.8120
    b_mtem(6,jA,jE) = 26.1986

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -1.16584
    b_mtem(2,jA,jE) = 8.50075
    b_mtem(3,jA,jE) = -44.3420
    b_mtem(4,jA,jE) = 97.3974
    b_mtem(5,jA,jE) = -98.4549
    b_mtem(6,jA,jE) = 37.6104

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -1.95805
    b_mtem(2,jA,jE) = 6.62417
    b_mtem(3,jA,jE) = -31.8072
    b_mtem(4,jA,jE) = 77.8603
    b_mtem(5,jA,jE) = -84.6458
    b_mtem(6,jA,jE) = 33.4963

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -0.36045
    b_mtem(2,jA,jE) = 3.55223
    b_mtem(3,jA,jE) = -24.0327
    b_mtem(4,jA,jE) = 54.4879
    b_mtem(5,jA,jE) = -56.6531
    b_mtem(6,jA,jE) = 22.4956


    !----------
    ! NaNO3 in E
    jA = jnano3

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.5888
    b_mtem(2,jA,jE) = 17.6192
    b_mtem(3,jA,jE) = -63.2183
    b_mtem(4,jA,jE) = 115.3520
    b_mtem(5,jA,jE) = -104.0860
    b_mtem(6,jA,jE) = 36.7390

    ! in NH4NO3
    jE = jnh4no3

    b_mtem(1,jA,jE) = -2.0669
    b_mtem(2,jA,jE) = 1.4792
    b_mtem(3,jA,jE) = 10.5261
    b_mtem(4,jA,jE) = -27.0987
    b_mtem(5,jA,jE) = 23.0591
    b_mtem(6,jA,jE) = -6.0938

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.8325
    b_mtem(2,jA,jE) = 3.9933
    b_mtem(3,jA,jE) = -15.3789
    b_mtem(4,jA,jE) = 30.4050
    b_mtem(5,jA,jE) = -29.4204
    b_mtem(6,jA,jE) = 11.0597

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -1.1233
    b_mtem(2,jA,jE) = 8.3998
    b_mtem(3,jA,jE) = -31.9002
    b_mtem(4,jA,jE) = 60.1450
    b_mtem(5,jA,jE) = -55.5503
    b_mtem(6,jA,jE) = 19.7757

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -2.5386
    b_mtem(2,jA,jE) = 13.9039
    b_mtem(3,jA,jE) = -42.8467
    b_mtem(4,jA,jE) = 69.7442
    b_mtem(5,jA,jE) = -57.8988
    b_mtem(6,jA,jE) = 19.4635

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = -0.4351
    b_mtem(2,jA,jE) = 2.8311
    b_mtem(3,jA,jE) = -11.4485
    b_mtem(4,jA,jE) = 22.7201
    b_mtem(5,jA,jE) = -22.4228
    b_mtem(6,jA,jE) = 8.5792

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = -0.72060
    b_mtem(2,jA,jE) = 5.64915
    b_mtem(3,jA,jE) = -23.5020
    b_mtem(4,jA,jE) = 46.0078
    b_mtem(5,jA,jE) = -43.8075
    b_mtem(6,jA,jE) = 16.1652

    ! in CaCl2
    jE = jcacl2

    b_mtem(1,jA,jE) = 0.003928
    b_mtem(2,jA,jE) = 3.54724
    b_mtem(3,jA,jE) = -18.6057
    b_mtem(4,jA,jE) = 38.1445
    b_mtem(5,jA,jE) = -36.7745
    b_mtem(6,jA,jE) = 13.4529

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = -1.1712
    b_mtem(2,jA,jE) = 7.20907
    b_mtem(3,jA,jE) = -22.9215
    b_mtem(4,jA,jE) = 38.1257
    b_mtem(5,jA,jE) = -32.0759
    b_mtem(6,jA,jE) = 10.6443

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 0.738022
    b_mtem(2,jA,jE) = -1.14313
    b_mtem(3,jA,jE) = 0.32251
    b_mtem(4,jA,jE) = 0.838679
    b_mtem(5,jA,jE) = -1.81747
    b_mtem(6,jA,jE) = 0.873986


    !----------
    ! NaCl in E
    jA = jnacl

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -1.9525
    b_mtem(2,jA,jE) = 16.6433
    b_mtem(3,jA,jE) = -61.7090
    b_mtem(4,jA,jE) = 112.9910
    b_mtem(5,jA,jE) = -101.9370
    b_mtem(6,jA,jE) = 35.7760

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -1.7525
    b_mtem(2,jA,jE) = 3.0713
    b_mtem(3,jA,jE) = 4.8063
    b_mtem(4,jA,jE) = -17.5334
    b_mtem(5,jA,jE) = 14.2872
    b_mtem(6,jA,jE) = -3.0690

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.4021
    b_mtem(2,jA,jE) = 5.2399
    b_mtem(3,jA,jE) = -19.4278
    b_mtem(4,jA,jE) = 33.0027
    b_mtem(5,jA,jE) = -28.1020
    b_mtem(6,jA,jE) = 9.5159

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 0.6692
    b_mtem(2,jA,jE) = 4.1207
    b_mtem(3,jA,jE) = -27.3314
    b_mtem(4,jA,jE) = 59.3112
    b_mtem(5,jA,jE) = -58.7998
    b_mtem(6,jA,jE) = 21.7674

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -1.17444
    b_mtem(2,jA,jE) = 10.9927
    b_mtem(3,jA,jE) = -38.9013
    b_mtem(4,jA,jE) = 66.8521
    b_mtem(5,jA,jE) = -57.6564
    b_mtem(6,jA,jE) = 19.7296

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = 1.17679
    b_mtem(2,jA,jE) = -2.5061
    b_mtem(3,jA,jE) = 0.8508
    b_mtem(4,jA,jE) = 4.4802
    b_mtem(5,jA,jE) = -8.4945
    b_mtem(6,jA,jE) = 4.3182

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = 1.01450
    b_mtem(2,jA,jE) = 2.10260
    b_mtem(3,jA,jE) = -20.9036
    b_mtem(4,jA,jE) = 49.1481
    b_mtem(5,jA,jE) = -51.4867
    b_mtem(6,jA,jE) = 19.9301

    ! in CaCl2 (PSC92: revised on 11/27/2003)
    jE = jcacl2
    b_mtem(1,jA,jE) = 1.55463
    b_mtem(2,jA,jE) = -3.20122
    b_mtem(3,jA,jE) = -0.957075
    b_mtem(4,jA,jE) = 12.103
    b_mtem(5,jA,jE) = -17.221
    b_mtem(6,jA,jE) = 7.50264

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 2.46187
    b_mtem(2,jA,jE) = -12.6845
    b_mtem(3,jA,jE) = 34.2383
    b_mtem(4,jA,jE) = -51.9992
    b_mtem(5,jA,jE) = 39.4934
    b_mtem(6,jA,jE) = -11.7247

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 1.74915
    b_mtem(2,jA,jE) = -4.65768
    b_mtem(3,jA,jE) = 8.80287
    b_mtem(4,jA,jE) = -12.2503
    b_mtem(5,jA,jE) = 8.668751
    b_mtem(6,jA,jE) = -2.50158


    !----------
    ! Ca(NO3)2 in E
    jA = jcano3

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -1.86260
    b_mtem(2,jA,jE) = 11.6178
    b_mtem(3,jA,jE) = -30.9069
    b_mtem(4,jA,jE) = 41.7578
    b_mtem(5,jA,jE) = -33.7338
    b_mtem(6,jA,jE) = 12.7541

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -1.1798
    b_mtem(2,jA,jE) = 25.9608
    b_mtem(3,jA,jE) = -98.9373
    b_mtem(4,jA,jE) = 160.2300
    b_mtem(5,jA,jE) = -125.9540
    b_mtem(6,jA,jE) = 39.5130

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -1.44384
    b_mtem(2,jA,jE) = 13.6044
    b_mtem(3,jA,jE) = -54.4300
    b_mtem(4,jA,jE) = 100.582
    b_mtem(5,jA,jE) = -91.2364
    b_mtem(6,jA,jE) = 32.5970

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = -0.099114
    b_mtem(2,jA,jE) = 2.84091
    b_mtem(3,jA,jE) = -16.9229
    b_mtem(4,jA,jE) = 37.4839
    b_mtem(5,jA,jE) = -39.5132
    b_mtem(6,jA,jE) = 15.8564

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = 0.055116
    b_mtem(2,jA,jE) = 4.58610
    b_mtem(3,jA,jE) = -27.6629
    b_mtem(4,jA,jE) = 60.8288
    b_mtem(5,jA,jE) = -61.4988
    b_mtem(6,jA,jE) = 23.3136

    ! in CaCl2 (PSC92: revised on 11/27/2003)
    jE = jcacl2
    b_mtem(1,jA,jE) = 1.57155
    b_mtem(2,jA,jE) = -3.18486
    b_mtem(3,jA,jE) = -3.35758
    b_mtem(4,jA,jE) = 18.7501
    b_mtem(5,jA,jE) = -24.5604
    b_mtem(6,jA,jE) = 10.3798

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 1.04446
    b_mtem(2,jA,jE) = -3.19066
    b_mtem(3,jA,jE) = 2.44714
    b_mtem(4,jA,jE) = 2.07218
    b_mtem(5,jA,jE) = -6.43949
    b_mtem(6,jA,jE) = 3.66471

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 1.05723
    b_mtem(2,jA,jE) = -1.46826
    b_mtem(3,jA,jE) = -1.0713
    b_mtem(4,jA,jE) = 4.64439
    b_mtem(5,jA,jE) = -6.32402
    b_mtem(6,jA,jE) = 2.78202


    !----------
    ! CaCl2 in E
    jA = jcacl2

    ! in NH4NO3 (PSC92: revised on 12/22/2003)
    jE = jnh4no3
    b_mtem(1,jA,jE) = -1.43626
    b_mtem(2,jA,jE) = 13.6598
    b_mtem(3,jA,jE) = -38.2068
    b_mtem(4,jA,jE) = 53.9057
    b_mtem(5,jA,jE) = -44.9018
    b_mtem(6,jA,jE) = 16.6120

    ! in NH4Cl (PSC92: revised on 11/27/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.603965
    b_mtem(2,jA,jE) = 27.6027
    b_mtem(3,jA,jE) = -104.258
    b_mtem(4,jA,jE) = 163.553
    b_mtem(5,jA,jE) = -124.076
    b_mtem(6,jA,jE) = 37.4153

    ! in NaNO3 (PSC92: revised on 12/22/2003)
    jE = jnano3
    b_mtem(1,jA,jE) = 0.44648
    b_mtem(2,jA,jE) = 8.8850
    b_mtem(3,jA,jE) = -45.5232
    b_mtem(4,jA,jE) = 89.3263
    b_mtem(5,jA,jE) = -83.8604
    b_mtem(6,jA,jE) = 30.4069

    ! in NaCl (PSC92: revised on 11/27/2003)
    jE = jnacl
    b_mtem(1,jA,jE) = 1.61927
    b_mtem(2,jA,jE) = 0.247547
    b_mtem(3,jA,jE) = -18.1252
    b_mtem(4,jA,jE) = 45.2479
    b_mtem(5,jA,jE) = -48.6072
    b_mtem(6,jA,jE) = 19.2784

    ! in Ca(NO3)2 (PSC92: revised on 11/27/2003)
    jE = jcano3
    b_mtem(1,jA,jE) = 2.36667
    b_mtem(2,jA,jE) = -0.123309
    b_mtem(3,jA,jE) = -24.2723
    b_mtem(4,jA,jE) = 65.1486
    b_mtem(5,jA,jE) = -71.8504
    b_mtem(6,jA,jE) = 28.3696

    ! in CaCl2 (PSC92: revised on 11/27/2003)
    jE = jcacl2
    b_mtem(1,jA,jE) = 3.64023
    b_mtem(2,jA,jE) = -12.1926
    b_mtem(3,jA,jE) = 20.2028
    b_mtem(4,jA,jE) = -16.0056
    b_mtem(5,jA,jE) = 1.52355
    b_mtem(6,jA,jE) = 2.44709

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 5.88794
    b_mtem(2,jA,jE) = -29.7083
    b_mtem(3,jA,jE) = 78.6309
    b_mtem(4,jA,jE) = -118.037
    b_mtem(5,jA,jE) = 88.932
    b_mtem(6,jA,jE) = -26.1407

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 2.40628
    b_mtem(2,jA,jE) = -6.16566
    b_mtem(3,jA,jE) = 10.2851
    b_mtem(4,jA,jE) = -12.9035
    b_mtem(5,jA,jE) = 7.7441
    b_mtem(6,jA,jE) = -1.74821


    !----------
    ! HNO3 in E
    jA = jhno3

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -3.57598
    b_mtem(2,jA,jE) = 21.5469
    b_mtem(3,jA,jE) = -77.4111
    b_mtem(4,jA,jE) = 144.136
    b_mtem(5,jA,jE) = -132.849
    b_mtem(6,jA,jE) = 47.9412

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -2.00209
    b_mtem(2,jA,jE) = -3.48399
    b_mtem(3,jA,jE) = 34.9906
    b_mtem(4,jA,jE) = -68.6653
    b_mtem(5,jA,jE) = 54.0992
    b_mtem(6,jA,jE) = -15.1343

    ! in NH4Cl revised on 12/22/2003
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.63790
    b_mtem(2,jA,jE) = -1.67730
    b_mtem(3,jA,jE) = 10.1727
    b_mtem(4,jA,jE) = -14.9097
    b_mtem(5,jA,jE) = 7.67410
    b_mtem(6,jA,jE) = -0.79586

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = 1.3446
    b_mtem(2,jA,jE) = -2.5578
    b_mtem(3,jA,jE) = 1.3464
    b_mtem(4,jA,jE) = 2.90537
    b_mtem(5,jA,jE) = -6.53014
    b_mtem(6,jA,jE) = 3.31339

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = -0.546636
    b_mtem(2,jA,jE) = 10.3127
    b_mtem(3,jA,jE) = -39.9603
    b_mtem(4,jA,jE) = 71.4609
    b_mtem(5,jA,jE) = -63.4958
    b_mtem(6,jA,jE) = 22.0679

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 1.35059
    b_mtem(2,jA,jE) = 4.34557
    b_mtem(3,jA,jE) = -35.8425
    b_mtem(4,jA,jE) = 80.9868
    b_mtem(5,jA,jE) = -81.6544
    b_mtem(6,jA,jE) = 30.4841

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = 0.869414
    b_mtem(2,jA,jE) = 2.98486
    b_mtem(3,jA,jE) = -22.255
    b_mtem(4,jA,jE) = 50.1863
    b_mtem(5,jA,jE) = -51.214
    b_mtem(6,jA,jE) = 19.2235

    ! in CaCl2 (KM) revised on 12/22/2003
    jE = jcacl2
    b_mtem(1,jA,jE) = 1.42800
    b_mtem(2,jA,jE) = -1.78959
    b_mtem(3,jA,jE) = -2.49075
    b_mtem(4,jA,jE) = 10.1877
    b_mtem(5,jA,jE) = -12.1948
    b_mtem(6,jA,jE) = 4.64475

    ! in HNO3 (added on 12/06/2004)
    jE = jhno3
    b_mtem(1,jA,jE) = 0.22035
    b_mtem(2,jA,jE) = 2.94973
    b_mtem(3,jA,jE) = -12.1469
    b_mtem(4,jA,jE) = 20.4905
    b_mtem(5,jA,jE) = -17.3966
    b_mtem(6,jA,jE) = 5.70779

    ! in HCl (added on 12/06/2004)
    jE = jhcl
    b_mtem(1,jA,jE) = 1.55503
    b_mtem(2,jA,jE) = -3.61226
    b_mtem(3,jA,jE) = 6.28265
    b_mtem(4,jA,jE) = -8.69575
    b_mtem(5,jA,jE) = 6.09372
    b_mtem(6,jA,jE) = -1.80898

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 1.10783
    b_mtem(2,jA,jE) = -1.3363
    b_mtem(3,jA,jE) = -1.83525
    b_mtem(4,jA,jE) = 7.47373
    b_mtem(5,jA,jE) = -9.72954
    b_mtem(6,jA,jE) = 4.12248

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -0.851026
    b_mtem(2,jA,jE) = 12.2515
    b_mtem(3,jA,jE) = -49.788
    b_mtem(4,jA,jE) = 91.6215
    b_mtem(5,jA,jE) = -81.4877
    b_mtem(6,jA,jE) = 28.0002

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -3.09464
    b_mtem(2,jA,jE) = 14.9303
    b_mtem(3,jA,jE) = -43.0454
    b_mtem(4,jA,jE) = 72.6695
    b_mtem(5,jA,jE) = -65.2140
    b_mtem(6,jA,jE) = 23.4814

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = 1.22973
    b_mtem(2,jA,jE) = 2.82702
    b_mtem(3,jA,jE) = -17.5869
    b_mtem(4,jA,jE) = 28.9564
    b_mtem(5,jA,jE) = -23.5814
    b_mtem(6,jA,jE) = 7.91153

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = 1.64773
    b_mtem(2,jA,jE) = 0.94188
    b_mtem(3,jA,jE) = -19.1242
    b_mtem(4,jA,jE) = 46.9887
    b_mtem(5,jA,jE) = -50.9494
    b_mtem(6,jA,jE) = 20.2169


    !----------
    ! HCl in E
    jA = jhcl

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.93783
    b_mtem(2,jA,jE) = 20.5546
    b_mtem(3,jA,jE) = -75.8548
    b_mtem(4,jA,jE) = 141.729
    b_mtem(5,jA,jE) = -130.697
    b_mtem(6,jA,jE) = 46.9905

    ! in NH4NO3
    jE = jnh4no3
    b_mtem(1,jA,jE) = -1.69063
    b_mtem(2,jA,jE) = -1.85303
    b_mtem(3,jA,jE) = 29.0927
    b_mtem(4,jA,jE) = -58.7401
    b_mtem(5,jA,jE) = 44.999
    b_mtem(6,jA,jE) = -11.9988

    ! in NH4Cl (revised on 11/15/2003)
    jE = jnh4cl
    b_mtem(1,jA,jE) = -0.2073
    b_mtem(2,jA,jE) = -0.4322
    b_mtem(3,jA,jE) = 6.1271
    b_mtem(4,jA,jE) = -12.3146
    b_mtem(5,jA,jE) = 8.9919
    b_mtem(6,jA,jE) = -2.3388

    ! in NaCl
    jE = jnacl
    b_mtem(1,jA,jE) = 2.95913
    b_mtem(2,jA,jE) = -7.92254
    b_mtem(3,jA,jE) = 13.736
    b_mtem(4,jA,jE) = -15.433
    b_mtem(5,jA,jE) = 7.40386
    b_mtem(6,jA,jE) = -0.918641

    ! in NaNO3
    jE = jnano3
    b_mtem(1,jA,jE) = 0.893272
    b_mtem(2,jA,jE) = 6.53768
    b_mtem(3,jA,jE) = -32.3458
    b_mtem(4,jA,jE) = 61.2834
    b_mtem(5,jA,jE) = -56.4446
    b_mtem(6,jA,jE) = 19.9202

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 3.14484
    b_mtem(2,jA,jE) = 0.077019
    b_mtem(3,jA,jE) = -31.4199
    b_mtem(4,jA,jE) = 80.5865
    b_mtem(5,jA,jE) = -85.392
    b_mtem(6,jA,jE) = 32.6644

    ! in Ca(NO3)2
    jE = jcano3
    b_mtem(1,jA,jE) = 2.60432
    b_mtem(2,jA,jE) = -0.55909
    b_mtem(3,jA,jE) = -19.6671
    b_mtem(4,jA,jE) = 53.3446
    b_mtem(5,jA,jE) = -58.9076
    b_mtem(6,jA,jE) = 22.9927

    ! in CaCl2 (KM) revised on 3/13/2003 and again on 11/27/2003
    jE = jcacl2
    b_mtem(1,jA,jE) = 2.98036
    b_mtem(2,jA,jE) = -8.55365
    b_mtem(3,jA,jE) = 15.2108
    b_mtem(4,jA,jE) = -15.9359
    b_mtem(5,jA,jE) = 7.41772
    b_mtem(6,jA,jE) = -1.32143

    ! in HNO3 (added on 12/06/2004)
    jE = jhno3
    b_mtem(1,jA,jE) = 3.8533
    b_mtem(2,jA,jE) = -16.9427
    b_mtem(3,jA,jE) = 45.0056
    b_mtem(4,jA,jE) = -69.6145
    b_mtem(5,jA,jE) = 54.1491
    b_mtem(6,jA,jE) = -16.6513

    ! in HCl (added on 12/06/2004)
    jE = jhcl
    b_mtem(1,jA,jE) = 2.56665
    b_mtem(2,jA,jE) = -7.13585
    b_mtem(3,jA,jE) = 14.8103
    b_mtem(4,jA,jE) = -21.8881
    b_mtem(5,jA,jE) = 16.6808
    b_mtem(6,jA,jE) = -5.22091

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 2.50179
    b_mtem(2,jA,jE) = -6.69364
    b_mtem(3,jA,jE) = 11.6551
    b_mtem(4,jA,jE) = -13.6897
    b_mtem(5,jA,jE) = 7.36796
    b_mtem(6,jA,jE) = -1.33245

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = 0.149955
    b_mtem(2,jA,jE) = 11.8213
    b_mtem(3,jA,jE) = -53.9164
    b_mtem(4,jA,jE) = 101.574
    b_mtem(5,jA,jE) = -91.4123
    b_mtem(6,jA,jE) = 31.5487

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -2.36927
    b_mtem(2,jA,jE) = 14.8359
    b_mtem(3,jA,jE) = -44.3443
    b_mtem(4,jA,jE) = 73.6229
    b_mtem(5,jA,jE) = -65.3366
    b_mtem(6,jA,jE) = 23.3250

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = 2.72993
    b_mtem(2,jA,jE) = -0.23406
    b_mtem(3,jA,jE) = -10.4103
    b_mtem(4,jA,jE) = 13.1586
    b_mtem(5,jA,jE) = -7.79925
    b_mtem(6,jA,jE) = 2.30843

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = 3.51258
    b_mtem(2,jA,jE) = -3.95107
    b_mtem(3,jA,jE) = -11.0175
    b_mtem(4,jA,jE) = 38.8617
    b_mtem(5,jA,jE) = -48.1575
    b_mtem(6,jA,jE) = 20.4717


    !----------
    ! 2H.SO4 in E
    jA = jh2so4

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 0.76734
    b_mtem(2,jA,jE) = -1.12263
    b_mtem(3,jA,jE) = -9.08728
    b_mtem(4,jA,jE) = 30.3836
    b_mtem(5,jA,jE) = -38.4133
    b_mtem(6,jA,jE) = 17.0106

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -2.03879
    b_mtem(2,jA,jE) = 15.7033
    b_mtem(3,jA,jE) = -58.7363
    b_mtem(4,jA,jE) = 109.242
    b_mtem(5,jA,jE) = -102.237
    b_mtem(6,jA,jE) = 37.5350

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -3.10228
    b_mtem(2,jA,jE) = 16.6920
    b_mtem(3,jA,jE) = -59.1522
    b_mtem(4,jA,jE) = 113.487
    b_mtem(5,jA,jE) = -110.890
    b_mtem(6,jA,jE) = 42.4578

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -3.43885
    b_mtem(2,jA,jE) = 21.0372
    b_mtem(3,jA,jE) = -84.7026
    b_mtem(4,jA,jE) = 165.324
    b_mtem(5,jA,jE) = -156.101
    b_mtem(6,jA,jE) = 57.3101

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = 0.33164
    b_mtem(2,jA,jE) = 6.55864
    b_mtem(3,jA,jE) = -33.5876
    b_mtem(4,jA,jE) = 65.1798
    b_mtem(5,jA,jE) = -63.2046
    b_mtem(6,jA,jE) = 24.1783

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = 3.06830
    b_mtem(2,jA,jE) = -3.18408
    b_mtem(3,jA,jE) = -19.6332
    b_mtem(4,jA,jE) = 61.3657
    b_mtem(5,jA,jE) = -73.4438
    b_mtem(6,jA,jE) = 31.2334

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 2.58649
    b_mtem(2,jA,jE) = 0.87921
    b_mtem(3,jA,jE) = -39.3023
    b_mtem(4,jA,jE) = 101.603
    b_mtem(5,jA,jE) = -109.469
    b_mtem(6,jA,jE) = 43.0188

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 1.54587
    b_mtem(2,jA,jE) = -7.50976
    b_mtem(3,jA,jE) = 12.8237
    b_mtem(4,jA,jE) = -10.1452
    b_mtem(5,jA,jE) = -0.541956
    b_mtem(6,jA,jE) = 3.34536

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 0.829757
    b_mtem(2,jA,jE) = -4.11316
    b_mtem(3,jA,jE) = 3.67111
    b_mtem(4,jA,jE) = 3.6833
    b_mtem(5,jA,jE) = -11.2711
    b_mtem(6,jA,jE) = 6.71421


    !----------
    ! H.HSO4 in E
    jA = jhhso4

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 2.63953
    b_mtem(2,jA,jE) = -6.01532
    b_mtem(3,jA,jE) = 10.0204
    b_mtem(4,jA,jE) = -12.4840
    b_mtem(5,jA,jE) = 7.78853
    b_mtem(6,jA,jE) = -2.12638

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -0.77412
    b_mtem(2,jA,jE) = 14.1656
    b_mtem(3,jA,jE) = -53.4087
    b_mtem(4,jA,jE) = 93.2013
    b_mtem(5,jA,jE) = -80.5723
    b_mtem(6,jA,jE) = 27.1577

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -2.98882
    b_mtem(2,jA,jE) = 14.4436
    b_mtem(3,jA,jE) = -40.1774
    b_mtem(4,jA,jE) = 67.5937
    b_mtem(5,jA,jE) = -61.5040
    b_mtem(6,jA,jE) = 22.3695

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -1.15502
    b_mtem(2,jA,jE) = 8.12309
    b_mtem(3,jA,jE) = -38.4726
    b_mtem(4,jA,jE) = 80.8861
    b_mtem(5,jA,jE) = -80.1644
    b_mtem(6,jA,jE) = 30.4717

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = 1.99641
    b_mtem(2,jA,jE) = -2.96061
    b_mtem(3,jA,jE) = 5.54778
    b_mtem(4,jA,jE) = -14.5488
    b_mtem(5,jA,jE) = 14.8492
    b_mtem(6,jA,jE) = -5.1389

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = 2.23816
    b_mtem(2,jA,jE) = -3.20847
    b_mtem(3,jA,jE) = -4.82853
    b_mtem(4,jA,jE) = 20.9192
    b_mtem(5,jA,jE) = -27.2819
    b_mtem(6,jA,jE) = 11.8655

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 2.56907
    b_mtem(2,jA,jE) = 1.13444
    b_mtem(3,jA,jE) = -34.6853
    b_mtem(4,jA,jE) = 87.9775
    b_mtem(5,jA,jE) = -93.2330
    b_mtem(6,jA,jE) = 35.9260

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 2.00024
    b_mtem(2,jA,jE) = -4.80868
    b_mtem(3,jA,jE) = 8.29222
    b_mtem(4,jA,jE) = -11.0849
    b_mtem(5,jA,jE) = 7.51262
    b_mtem(6,jA,jE) = -2.07654

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 2.8009
    b_mtem(2,jA,jE) = -6.98416
    b_mtem(3,jA,jE) = 14.3146
    b_mtem(4,jA,jE) = -22.0068
    b_mtem(5,jA,jE) = 17.5557
    b_mtem(6,jA,jE) = -5.84917


    !----------
    ! NH4HSO4 in E
    jA = jnh4hso4

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 0.169160
    b_mtem(2,jA,jE) = 2.15094
    b_mtem(3,jA,jE) = -9.62904
    b_mtem(4,jA,jE) = 18.2631
    b_mtem(5,jA,jE) = -17.3333
    b_mtem(6,jA,jE) = 6.19835

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -2.34457
    b_mtem(2,jA,jE) = 12.8035
    b_mtem(3,jA,jE) = -35.2513
    b_mtem(4,jA,jE) = 53.6153
    b_mtem(5,jA,jE) = -42.7655
    b_mtem(6,jA,jE) = 13.7129

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -2.56109
    b_mtem(2,jA,jE) = 11.1414
    b_mtem(3,jA,jE) = -30.2361
    b_mtem(4,jA,jE) = 50.0320
    b_mtem(5,jA,jE) = -44.1586
    b_mtem(6,jA,jE) = 15.5393

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -0.97315
    b_mtem(2,jA,jE) = 7.06295
    b_mtem(3,jA,jE) = -29.3032
    b_mtem(4,jA,jE) = 57.6101
    b_mtem(5,jA,jE) = -54.9020
    b_mtem(6,jA,jE) = 20.2222

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -0.44450
    b_mtem(2,jA,jE) = 3.33451
    b_mtem(3,jA,jE) = -15.2791
    b_mtem(4,jA,jE) = 30.1413
    b_mtem(5,jA,jE) = -26.7710
    b_mtem(6,jA,jE) = 8.78462

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -0.99780
    b_mtem(2,jA,jE) = 4.69200
    b_mtem(3,jA,jE) = -16.1219
    b_mtem(4,jA,jE) = 29.3100
    b_mtem(5,jA,jE) = -26.3383
    b_mtem(6,jA,jE) = 9.20695

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -0.52694
    b_mtem(2,jA,jE) = 7.02684
    b_mtem(3,jA,jE) = -33.7508
    b_mtem(4,jA,jE) = 70.0565
    b_mtem(5,jA,jE) = -68.3226
    b_mtem(6,jA,jE) = 25.2692

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 0.572926
    b_mtem(2,jA,jE) = -2.04791
    b_mtem(3,jA,jE) = 2.1134
    b_mtem(4,jA,jE) = 0.246654
    b_mtem(5,jA,jE) = -3.06019
    b_mtem(6,jA,jE) = 1.98126

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 0.56514
    b_mtem(2,jA,jE) = 0.22287
    b_mtem(3,jA,jE) = -2.76973
    b_mtem(4,jA,jE) = 4.54444
    b_mtem(5,jA,jE) = -3.86549
    b_mtem(6,jA,jE) = 1.13441


    !----------
    ! (NH4)3H(SO4)2 in E
    jA = jlvcite

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = -1.44811
    b_mtem(2,jA,jE) = 6.71815
    b_mtem(3,jA,jE) = -25.0141
    b_mtem(4,jA,jE) = 50.1109
    b_mtem(5,jA,jE) = -50.0561
    b_mtem(6,jA,jE) = 19.3370

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -3.41707
    b_mtem(2,jA,jE) = 13.4496
    b_mtem(3,jA,jE) = -34.8018
    b_mtem(4,jA,jE) = 55.2987
    b_mtem(5,jA,jE) = -48.1839
    b_mtem(6,jA,jE) = 17.2444

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -2.54479
    b_mtem(2,jA,jE) = 11.8501
    b_mtem(3,jA,jE) = -39.7286
    b_mtem(4,jA,jE) = 74.2479
    b_mtem(5,jA,jE) = -70.4934
    b_mtem(6,jA,jE) = 26.2836

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -2.30561
    b_mtem(2,jA,jE) = 14.5806
    b_mtem(3,jA,jE) = -55.1238
    b_mtem(4,jA,jE) = 103.451
    b_mtem(5,jA,jE) = -95.2571
    b_mtem(6,jA,jE) = 34.2218

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -2.20809
    b_mtem(2,jA,jE) = 13.6391
    b_mtem(3,jA,jE) = -57.8246
    b_mtem(4,jA,jE) = 117.907
    b_mtem(5,jA,jE) = -112.154
    b_mtem(6,jA,jE) = 40.3058

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -1.15099
    b_mtem(2,jA,jE) = 6.32269
    b_mtem(3,jA,jE) = -27.3860
    b_mtem(4,jA,jE) = 55.4592
    b_mtem(5,jA,jE) = -54.0100
    b_mtem(6,jA,jE) = 20.3469

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -1.15678
    b_mtem(2,jA,jE) = 8.28718
    b_mtem(3,jA,jE) = -37.3231
    b_mtem(4,jA,jE) = 76.6124
    b_mtem(5,jA,jE) = -74.9307
    b_mtem(6,jA,jE) = 28.0559

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 0.01502
    b_mtem(2,jA,jE) = -3.1197
    b_mtem(3,jA,jE) = 3.61104
    b_mtem(4,jA,jE) = 3.05196
    b_mtem(5,jA,jE) = -9.98957
    b_mtem(6,jA,jE) = 6.04155

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = -1.06477
    b_mtem(2,jA,jE) = 3.38801
    b_mtem(3,jA,jE) = -12.5784
    b_mtem(4,jA,jE) = 25.2823
    b_mtem(5,jA,jE) = -25.4611
    b_mtem(6,jA,jE) = 10.0754


    !----------
    ! NaHSO4 in E
    jA = jnahso4

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = 0.68259
    b_mtem(2,jA,jE) = 0.71468
    b_mtem(3,jA,jE) = -5.59003
    b_mtem(4,jA,jE) = 11.0089
    b_mtem(5,jA,jE) = -10.7983
    b_mtem(6,jA,jE) = 3.82335

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -0.03956
    b_mtem(2,jA,jE) = 4.52828
    b_mtem(3,jA,jE) = -25.2557
    b_mtem(4,jA,jE) = 54.4225
    b_mtem(5,jA,jE) = -52.5105
    b_mtem(6,jA,jE) = 18.6562

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -1.53503
    b_mtem(2,jA,jE) = 8.27608
    b_mtem(3,jA,jE) = -28.9539
    b_mtem(4,jA,jE) = 55.2876
    b_mtem(5,jA,jE) = -51.9563
    b_mtem(6,jA,jE) = 18.6576

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -0.38793
    b_mtem(2,jA,jE) = 7.14680
    b_mtem(3,jA,jE) = -38.7201
    b_mtem(4,jA,jE) = 84.3965
    b_mtem(5,jA,jE) = -84.7453
    b_mtem(6,jA,jE) = 32.1283

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -0.41982
    b_mtem(2,jA,jE) = 4.26491
    b_mtem(3,jA,jE) = -20.2351
    b_mtem(4,jA,jE) = 42.6764
    b_mtem(5,jA,jE) = -40.7503
    b_mtem(6,jA,jE) = 14.2868

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -0.32912
    b_mtem(2,jA,jE) = 1.80808
    b_mtem(3,jA,jE) = -8.01286
    b_mtem(4,jA,jE) = 15.5791
    b_mtem(5,jA,jE) = -14.5494
    b_mtem(6,jA,jE) = 5.27052

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = 0.10271
    b_mtem(2,jA,jE) = 5.09559
    b_mtem(3,jA,jE) = -30.3295
    b_mtem(4,jA,jE) = 66.2975
    b_mtem(5,jA,jE) = -66.3458
    b_mtem(6,jA,jE) = 24.9443

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 0.608309
    b_mtem(2,jA,jE) = -0.541905
    b_mtem(3,jA,jE) = -2.52084
    b_mtem(4,jA,jE) = 6.63297
    b_mtem(5,jA,jE) = -7.24599
    b_mtem(6,jA,jE) = 2.88811

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 1.98399
    b_mtem(2,jA,jE) = -4.51562
    b_mtem(3,jA,jE) = 8.36059
    b_mtem(4,jA,jE) = -12.4948
    b_mtem(5,jA,jE) = 9.67514
    b_mtem(6,jA,jE) = -3.18004


    !----------
    ! Na3H(SO4)2 in E
    jA = jna3hso4

    ! in H2SO4
    jE = jh2so4
    b_mtem(1,jA,jE) = -0.83214
    b_mtem(2,jA,jE) = 4.99572
    b_mtem(3,jA,jE) = -20.1697
    b_mtem(4,jA,jE) = 41.4066
    b_mtem(5,jA,jE) = -42.2119
    b_mtem(6,jA,jE) = 16.4855

    ! in NH4HSO4
    jE = jnh4hso4
    b_mtem(1,jA,jE) = -0.65139
    b_mtem(2,jA,jE) = 3.52300
    b_mtem(3,jA,jE) = -22.8220
    b_mtem(4,jA,jE) = 56.2956
    b_mtem(5,jA,jE) = -59.9028
    b_mtem(6,jA,jE) = 23.1844

    ! in (NH4)3H(SO4)2
    jE = jlvcite
    b_mtem(1,jA,jE) = -1.31331
    b_mtem(2,jA,jE) = 8.40835
    b_mtem(3,jA,jE) = -38.1757
    b_mtem(4,jA,jE) = 80.5312
    b_mtem(5,jA,jE) = -79.8346
    b_mtem(6,jA,jE) = 30.0219

    ! in (NH4)2SO4
    jE = jnh4so4
    b_mtem(1,jA,jE) = -1.03054
    b_mtem(2,jA,jE) = 8.08155
    b_mtem(3,jA,jE) = -38.1046
    b_mtem(4,jA,jE) = 78.7168
    b_mtem(5,jA,jE) = -77.2263
    b_mtem(6,jA,jE) = 29.1521

    ! in NaHSO4
    jE = jnahso4
    b_mtem(1,jA,jE) = -1.90695
    b_mtem(2,jA,jE) = 11.6241
    b_mtem(3,jA,jE) = -50.3175
    b_mtem(4,jA,jE) = 105.884
    b_mtem(5,jA,jE) = -103.258
    b_mtem(6,jA,jE) = 37.6588

    ! in Na3H(SO4)2
    jE = jna3hso4
    b_mtem(1,jA,jE) = -0.34780
    b_mtem(2,jA,jE) = 2.85363
    b_mtem(3,jA,jE) = -17.6224
    b_mtem(4,jA,jE) = 38.9220
    b_mtem(5,jA,jE) = -39.8106
    b_mtem(6,jA,jE) = 15.6055

    ! in Na2SO4
    jE = jna2so4
    b_mtem(1,jA,jE) = -0.75230
    b_mtem(2,jA,jE) = 10.0140
    b_mtem(3,jA,jE) = -50.5677
    b_mtem(4,jA,jE) = 106.941
    b_mtem(5,jA,jE) = -105.534
    b_mtem(6,jA,jE) = 39.5196

    ! in HNO3
    jE = jhno3
    b_mtem(1,jA,jE) = 0.057456
    b_mtem(2,jA,jE) = -1.31264
    b_mtem(3,jA,jE) = -1.94662
    b_mtem(4,jA,jE) = 10.7024
    b_mtem(5,jA,jE) = -14.9946
    b_mtem(6,jA,jE) = 7.12161

    ! in HCl
    jE = jhcl
    b_mtem(1,jA,jE) = 0.637894
    b_mtem(2,jA,jE) = -2.29719
    b_mtem(3,jA,jE) = 0.765361
    b_mtem(4,jA,jE) = 4.8748
    b_mtem(5,jA,jE) = -9.25978
    b_mtem(6,jA,jE) = 4.91773
    !
    !
    !
    !----------------------------------------------------------
    ! Coefficients for %MDRH(T) = d1 + d2*T + d3*T^2 + d4*T^3    (T in Kelvin)
    ! valid Temperature Range: 240 - 320 K
    !----------------------------------------------------------
    !
    ! SULFATE-POOR SYSTEMS
    ! AC
    j_index = 1
    d_mdrh(j_index,1) = -58.00268351
    d_mdrh(j_index,2) = 2.031077573
    d_mdrh(j_index,3) = -0.008281218
    d_mdrh(j_index,4) = 1.00447E-05

    ! AN
    j_index = 2
    d_mdrh(j_index,1) = 1039.137773
    d_mdrh(j_index,2) = -11.47847095
    d_mdrh(j_index,3) = 0.047702786
    d_mdrh(j_index,4) = -6.77675E-05

    ! AS
    j_index = 3
    d_mdrh(j_index,1) = 115.8366357
    d_mdrh(j_index,2) = 0.491881663
    d_mdrh(j_index,3) = -0.00422807
    d_mdrh(j_index,4) = 7.29274E-06

    ! SC
    j_index = 4
    d_mdrh(j_index,1) = 253.2424151
    d_mdrh(j_index,2) = -1.429957864
    d_mdrh(j_index,3) = 0.003727554
    d_mdrh(j_index,4) = -3.13037E-06

    ! SN
    j_index = 5
    d_mdrh(j_index,1) = -372.4306506
    d_mdrh(j_index,2) = 5.3955633
    d_mdrh(j_index,3) = -0.019804438
    d_mdrh(j_index,4) = 2.25662E-05

    ! SS
    j_index = 6
    d_mdrh(j_index,1) = 286.1271416
    d_mdrh(j_index,2) = -1.670787758
    d_mdrh(j_index,3) = 0.004431373
    d_mdrh(j_index,4) = -3.57757E-06

    ! CC
    j_index = 7
    d_mdrh(j_index,1) = -1124.07059
    d_mdrh(j_index,2) = 14.26364209
    d_mdrh(j_index,3) = -0.054816822
    d_mdrh(j_index,4) = 6.70107E-05

    ! CN
    j_index = 8
    d_mdrh(j_index,1) = 1855.413934
    d_mdrh(j_index,2) = -20.29219473
    d_mdrh(j_index,3) = 0.07807482
    d_mdrh(j_index,4) = -1.017887858e-4

    ! AN + AC
    j_index = 9
    d_mdrh(j_index,1) = 1761.176886
    d_mdrh(j_index,2) = -19.29811062
    d_mdrh(j_index,3) = 0.075676987
    d_mdrh(j_index,4) = -1.0116959e-4

    ! AS + AC
    j_index = 10
    d_mdrh(j_index,1) = 122.1074303
    d_mdrh(j_index,2) = 0.429692122
    d_mdrh(j_index,3) = -0.003928277
    d_mdrh(j_index,4) = 6.43275E-06

    ! AS + AN
    j_index = 11
    d_mdrh(j_index,1) = 2424.634678
    d_mdrh(j_index,2) = -26.54031307
    d_mdrh(j_index,3) = 0.101625387
    d_mdrh(j_index,4) = -1.31544547798e-4

    ! AS + AN + AC
    j_index = 12
    d_mdrh(j_index,1) = 2912.082599
    d_mdrh(j_index,2) = -31.8894185
    d_mdrh(j_index,3) = 0.121185849
    d_mdrh(j_index,4) = -1.556534623e-4

    ! SC + AC
    j_index = 13
    d_mdrh(j_index,1) = 172.2596493
    d_mdrh(j_index,2) = -0.511006195
    d_mdrh(j_index,3) = 4.27244597e-4
    d_mdrh(j_index,4) = 4.12797E-07

    ! SN + AC
    j_index = 14
    d_mdrh(j_index,1) = 1596.184935
    d_mdrh(j_index,2) = -16.37945565
    d_mdrh(j_index,3) = 0.060281218
    d_mdrh(j_index,4) = -7.6161E-05

    ! SN + AN
    j_index = 15
    d_mdrh(j_index,1) = 1916.072988
    d_mdrh(j_index,2) = -20.85594868
    d_mdrh(j_index,3) = 0.081140141
    d_mdrh(j_index,4) = -1.07954274796e-4

    ! SN + AN + AC
    j_index = 16
    d_mdrh(j_index,1) = 1467.165935
    d_mdrh(j_index,2) = -16.01166196
    d_mdrh(j_index,3) = 0.063505582
    d_mdrh(j_index,4) = -8.66722E-05

    ! SN + SC
    j_index = 17
    d_mdrh(j_index,1) = 158.447059
    d_mdrh(j_index,2) = -0.628167358
    d_mdrh(j_index,3) = 0.002014448
    d_mdrh(j_index,4) = -3.13037E-06

    ! SN + SC + AC
    j_index = 18
    d_mdrh(j_index,1) = 1115.892468
    d_mdrh(j_index,2) = -11.76936534
    d_mdrh(j_index,3) = 0.045577399
    d_mdrh(j_index,4) = -6.05779E-05

    ! SS + AC
    j_index = 19
    d_mdrh(j_index,1) = 269.5432407
    d_mdrh(j_index,2) = -1.319963885
    d_mdrh(j_index,3) = 0.002592363
    d_mdrh(j_index,4) = -1.44479E-06

    ! SS + AN
    j_index = 20
    d_mdrh(j_index,1) = 2841.334784
    d_mdrh(j_index,2) = -31.1889487
    d_mdrh(j_index,3) = 0.118809274
    d_mdrh(j_index,4) = -1.53007e-4

    ! SS + AN + AC
    j_index = 21
    d_mdrh(j_index,1) = 2199.36914
    d_mdrh(j_index,2) = -24.11926569
    d_mdrh(j_index,3) = 0.092932361
    d_mdrh(j_index,4) = -1.21774e-4

    ! SS + AS
    j_index = 22
    d_mdrh(j_index,1) = 395.0051604
    d_mdrh(j_index,2) = -2.521101657
    d_mdrh(j_index,3) = 0.006139319
    d_mdrh(j_index,4) = -4.43756E-06

    ! SS + AS + AC
    j_index = 23
    d_mdrh(j_index,1) = 386.5150675
    d_mdrh(j_index,2) = -2.4632138
    d_mdrh(j_index,3) = 0.006139319
    d_mdrh(j_index,4) = -4.98796E-06

    ! SS + AS + AN
    j_index = 24
    d_mdrh(j_index,1) = 3101.538491
    d_mdrh(j_index,2) = -34.19978105
    d_mdrh(j_index,3) = 0.130118605
    d_mdrh(j_index,4) = -1.66873e-4

    ! SS + AS + AN + AC
    j_index = 25
    d_mdrh(j_index,1) = 2307.579403
    d_mdrh(j_index,2) = -25.43136774
    d_mdrh(j_index,3) = 0.098064728
    d_mdrh(j_index,4) = -1.28301e-4

    ! SS + SC
    j_index = 26
    d_mdrh(j_index,1) = 291.8309602
    d_mdrh(j_index,2) = -1.828912974
    d_mdrh(j_index,3) = 0.005053148
    d_mdrh(j_index,4) = -4.57516E-06

    ! SS + SC + AC
    j_index = 27
    d_mdrh(j_index,1) = 188.3914345
    d_mdrh(j_index,2) = -0.631345031
    d_mdrh(j_index,3) = 0.000622807
    d_mdrh(j_index,4) = 4.47196E-07

    ! SS + SN
    j_index = 28
    d_mdrh(j_index,1) = -167.1252839
    d_mdrh(j_index,2) = 2.969828002
    d_mdrh(j_index,3) = -0.010637255
    d_mdrh(j_index,4) = 1.13175E-05

    ! SS + SN + AC
    j_index = 29
    d_mdrh(j_index,1) = 1516.782768
    d_mdrh(j_index,2) = -15.7922661
    d_mdrh(j_index,3) = 0.058942209
    d_mdrh(j_index,4) = -7.5301E-05

    ! SS + SN + AN
    j_index = 30
    d_mdrh(j_index,1) = 1739.963163
    d_mdrh(j_index,2) = -19.06576022
    d_mdrh(j_index,3) = 0.07454963
    d_mdrh(j_index,4) = -9.94302E-05

    ! SS + SN + AN + AC
    j_index = 31
    d_mdrh(j_index,1) = 2152.104877
    d_mdrh(j_index,2) = -23.74998008
    d_mdrh(j_index,3) = 0.092256654
    d_mdrh(j_index,4) = -1.21953e-4

    ! SS + SN + SC
    j_index = 32
    d_mdrh(j_index,1) = 221.9976265
    d_mdrh(j_index,2) = -1.311331272
    d_mdrh(j_index,3) = 0.004406089
    d_mdrh(j_index,4) = -5.88235E-06

    ! SS + SN + SC + AC
    j_index = 33
    d_mdrh(j_index,1) = 1205.645615
    d_mdrh(j_index,2) = -12.71353459
    d_mdrh(j_index,3) = 0.048803922
    d_mdrh(j_index,4) = -6.41899E-05

    ! CC + AC
    j_index = 34
    d_mdrh(j_index,1) = 506.6737879
    d_mdrh(j_index,2) = -3.723520818
    d_mdrh(j_index,3) = 0.010814242
    d_mdrh(j_index,4) = -1.21087E-05

    ! CC + SC
    j_index = 35
    d_mdrh(j_index,1) = -1123.523841
    d_mdrh(j_index,2) = 14.08345977
    d_mdrh(j_index,3) = -0.053687823
    d_mdrh(j_index,4) = 6.52219E-05

    ! CC + SC + AC
    j_index = 36
    d_mdrh(j_index,1) = -1159.98607
    d_mdrh(j_index,2) = 14.44309169
    d_mdrh(j_index,3) = -0.054841073
    d_mdrh(j_index,4) = 6.64259E-05

    ! CN + AC
    j_index = 37
    d_mdrh(j_index,1) = 756.0747916
    d_mdrh(j_index,2) = -8.546826257
    d_mdrh(j_index,3) = 0.035798677
    d_mdrh(j_index,4) = -5.06629E-05

    ! CN + AN
    j_index = 38
    d_mdrh(j_index,1) = 338.668191
    d_mdrh(j_index,2) = -2.971223403
    d_mdrh(j_index,3) = 0.012294866
    d_mdrh(j_index,4) = -1.87558E-05

    ! CN + AN + AC
    j_index = 39
    d_mdrh(j_index,1) = -53.18033508
    d_mdrh(j_index,2) = 0.663911748
    d_mdrh(j_index,3) = 9.16326e-4
    d_mdrh(j_index,4) = -6.70354E-06

    ! CN + SC
    j_index = 40
    d_mdrh(j_index,1) = 3623.831129
    d_mdrh(j_index,2) = -39.27226457
    d_mdrh(j_index,3) = 0.144559515
    d_mdrh(j_index,4) = -1.78159e-4

    ! CN + SC + AC
    j_index = 41
    d_mdrh(j_index,1) = 3436.656743
    d_mdrh(j_index,2) = -37.16192684
    d_mdrh(j_index,3) = 0.136641377
    d_mdrh(j_index,4) = -1.68262e-4

    ! CN + SN
    j_index = 42
    d_mdrh(j_index,1) = 768.608476
    d_mdrh(j_index,2) = -8.051517149
    d_mdrh(j_index,3) = 0.032342332
    d_mdrh(j_index,4) = -4.52224E-05

    ! CN + SN + AC
    j_index = 43
    d_mdrh(j_index,1) = 33.58027951
    d_mdrh(j_index,2) = -0.308772182
    d_mdrh(j_index,3) = 0.004713639
    d_mdrh(j_index,4) = -1.19658E-05

    ! CN + SN + AN
    j_index = 44
    d_mdrh(j_index,1) = 57.80183041
    d_mdrh(j_index,2) = 0.215264604
    d_mdrh(j_index,3) = 4.11406e-4
    d_mdrh(j_index,4) = -4.30702E-06

    ! CN + SN + AN + AC
    j_index = 45
    d_mdrh(j_index,1) = -234.368984
    d_mdrh(j_index,2) = 2.721045204
    d_mdrh(j_index,3) = -0.006688341
    d_mdrh(j_index,4) = 2.31729E-06

    ! CN + SN + SC
    j_index = 46
    d_mdrh(j_index,1) = 3879.080557
    d_mdrh(j_index,2) = -42.13562874
    d_mdrh(j_index,3) = 0.155235005
    d_mdrh(j_index,4) = -1.91387e-4

    ! CN + SN + SC + AC
    j_index = 47
    d_mdrh(j_index,1) = 3600.576985
    d_mdrh(j_index,2) = -39.0283489
    d_mdrh(j_index,3) = 0.143710316
    d_mdrh(j_index,4) = -1.77167e-4

    ! CN + CC
    j_index = 48
    d_mdrh(j_index,1) = -1009.729826
    d_mdrh(j_index,2) = 12.9145339
    d_mdrh(j_index,3) = -0.049811146
    d_mdrh(j_index,4) = 6.09563E-05

    ! CN + CC + AC
    j_index = 49
    d_mdrh(j_index,1) = -577.0919514
    d_mdrh(j_index,2) = 8.020324227
    d_mdrh(j_index,3) = -0.031469556
    d_mdrh(j_index,4) = 3.82181E-05

    ! CN + CC + SC
    j_index = 50
    d_mdrh(j_index,1) = -728.9983499
    d_mdrh(j_index,2) = 9.849458215
    d_mdrh(j_index,3) = -0.03879257
    d_mdrh(j_index,4) = 4.78844E-05

    ! CN + CC + SC + AC
    j_index = 51
    d_mdrh(j_index,1) = -803.7026845
    d_mdrh(j_index,2) = 10.61881494
    d_mdrh(j_index,3) = -0.041402993
    d_mdrh(j_index,4) = 5.08084E-05

    !
    ! SULFATE-RICH SYSTEMS
    ! AB
    j_index = 52
    d_mdrh(j_index,1) = -493.6190458
    d_mdrh(j_index,2) = 6.747053851
    d_mdrh(j_index,3) = -0.026955267
    d_mdrh(j_index,4) = 3.45118E-05

    ! LV
    j_index = 53
    d_mdrh(j_index,1) = 53.37874093
    d_mdrh(j_index,2) = 1.01368249
    d_mdrh(j_index,3) = -0.005887513
    d_mdrh(j_index,4) = 8.94393E-06

    ! SB
    j_index = 54
    d_mdrh(j_index,1) = 206.619047
    d_mdrh(j_index,2) = -1.342735684
    d_mdrh(j_index,3) = 0.003197691
    d_mdrh(j_index,4) = -1.93603E-06

    ! AB + LV
    j_index = 55
    d_mdrh(j_index,1) = -493.6190458
    d_mdrh(j_index,2) = 6.747053851
    d_mdrh(j_index,3) = -0.026955267
    d_mdrh(j_index,4) = 3.45118E-05

    ! AS + LV
    j_index = 56
    d_mdrh(j_index,1) = 53.37874093
    d_mdrh(j_index,2) = 1.01368249
    d_mdrh(j_index,3) = -0.005887513
    d_mdrh(j_index,4) = 8.94393E-06

    ! SS + SB
    j_index = 57
    d_mdrh(j_index,1) = 206.619047
    d_mdrh(j_index,2) = -1.342735684
    d_mdrh(j_index,3) = 0.003197691
    d_mdrh(j_index,4) = -1.93603E-06

    ! SS + LV
    j_index = 58
    d_mdrh(j_index,1) = 41.7619047
    d_mdrh(j_index,2) = 1.303872053
    d_mdrh(j_index,3) = -0.007647908
    d_mdrh(j_index,4) = 1.17845E-05

    ! SS + AS + LV
    j_index = 59
    d_mdrh(j_index,1) = 41.7619047
    d_mdrh(j_index,2) = 1.303872053
    d_mdrh(j_index,3) = -0.007647908
    d_mdrh(j_index,4) = 1.17845E-05

    ! SS + AB
    j_index = 60
    d_mdrh(j_index,1) = -369.7142842
    d_mdrh(j_index,2) = 5.512878771
    d_mdrh(j_index,3) = -0.02301948
    d_mdrh(j_index,4) = 3.0303E-05

    ! SS + LV + AB
    j_index = 61
    d_mdrh(j_index,1) = -369.7142842
    d_mdrh(j_index,2) = 5.512878771
    d_mdrh(j_index,3) = -0.02301948
    d_mdrh(j_index,4) = 3.0303E-05

    ! SB + AB
    j_index = 62
    d_mdrh(j_index,1) = -162.8095232
    d_mdrh(j_index,2) = 2.399326592
    d_mdrh(j_index,3) = -0.009336219
    d_mdrh(j_index,4) = 1.17845E-05

    ! SS + SB + AB
    j_index = 63
    d_mdrh(j_index,1) = -735.4285689
    d_mdrh(j_index,2) = 8.885521857
    d_mdrh(j_index,3) = -0.033488456
    d_mdrh(j_index,4) = 4.12458E-05

    !   endif ! first


    if (msoa_flag1 >= 1000) call soa_vbs_load_params( 2 )


    return
  end subroutine load_mosaic_parameters



end module module_mosaic_init_aerpar
