module module_sorgam_aqchem
  
  ! NOTE: This is an initial attempt at the implementation of AQCHEM with
  ! the MADE/SORGAM aerosol scheme. It needs to be checked and tested.
  !
  ! jan.kazil@noaa.gov 2011-08-14 17:15:39 -06:00
  
  REAL, PARAMETER :: epsilc = 1.0E-16
  
  REAL, PARAMETER :: qcldwtr_cutoff = 1.0e-6 ! Cloud threshold (kg/kg)
  
  REAL, PARAMETER :: mwdry = 28.966  ! Molecular mass of dry air (g/mol)
  REAL, PARAMETER :: mwso4 = 96.00   ! Molecular mass of SO4-- (g/mol)
  REAL, PARAMETER :: mwno3 = 62.0    ! Molecular mass of NO3- (g/mol)
  REAL, PARAMETER :: mwnh4 = 18.0985 ! Molecular mass of NH4+ (g/mol)
  REAL, PARAMETER :: mwna  = 22.990  ! Molecular mass of NH4+ (g/mol)
  REAL, PARAMETER :: mwcl  = 35.453  ! Molecular mass of NH4+ (g/mol)
  
  ! AQCHEM parameters
  
  INTEGER, PARAMETER :: NGAS = 12  ! number of gas-phase species for AQCHEM
  INTEGER, PARAMETER :: NAER = 36  ! number of aerosol species for AQCHEM
  INTEGER, PARAMETER :: NLIQS = 41 ! number of liquid-phase species in AQCHEM
  
  ! Indices for the AQCHEM array GAS
  
  INTEGER, PARAMETER :: LSO2    =  1  ! Sulfur Dioxide
  INTEGER, PARAMETER :: LHNO3   =  2  ! Nitric Acid
  INTEGER, PARAMETER :: LN2O5   =  3  ! Dinitrogen Pentoxide
  INTEGER, PARAMETER :: LCO2    =  4  ! Carbon Dioxide
  INTEGER, PARAMETER :: LNH3    =  5  ! Ammonia
  INTEGER, PARAMETER :: LH2O2   =  6  ! Hydrogen Perioxide
  INTEGER, PARAMETER :: LO3     =  7  ! Ozone
  INTEGER, PARAMETER :: LFOA    =  8  ! Formic Acid
  INTEGER, PARAMETER :: LMHP    =  9  ! Methyl Hydrogen Peroxide
  INTEGER, PARAMETER :: LPAA    = 10  ! Peroxyacidic Acid
  INTEGER, PARAMETER :: LH2SO4  = 11  ! Sulfuric Acid
  INTEGER, PARAMETER :: LHCL    = 12  ! Hydrogen Chloride
  
  ! Indices for the AQCHEM array AEROSOL
  
  INTEGER, PARAMETER :: LSO4AKN  =  1  ! Aitken mode Sulfate
  INTEGER, PARAMETER :: LSO4ACC  =  2  ! Accumulation mode Sulfate
  INTEGER, PARAMETER :: LSO4COR  =  3  ! Coarse mode Sulfate
  INTEGER, PARAMETER :: LNH4AKN  =  4  ! Aitken mode Ammonium
  INTEGER, PARAMETER :: LNH4ACC  =  5  ! Accumulation mode Ammonium
  INTEGER, PARAMETER :: LNO3AKN  =  6  ! Aitken mode Nitrate
  INTEGER, PARAMETER :: LNO3ACC  =  7  ! Accumulation mode Nitrate
  INTEGER, PARAMETER :: LNO3COR  =  8  ! Coarse mode Nitrate
  INTEGER, PARAMETER :: LORGAAKN =  9  ! Aitken mode anthropogenic SOA
  INTEGER, PARAMETER :: LORGAACC = 10  ! Accumulation mode anthropogenic SOA
  INTEGER, PARAMETER :: LORGPAKN = 11  ! Aitken mode primary organic aerosol
  INTEGER, PARAMETER :: LORGPACC = 12  ! Accumulation mode primary organic aerosol
  INTEGER, PARAMETER :: LORGBAKN = 13  ! Aitken mode biogenic SOA
  INTEGER, PARAMETER :: LORGBACC = 14  ! Accumulation mode biogenic SOA
  INTEGER, PARAMETER :: LECAKN   = 15  ! Aitken mode elemental carbon
  INTEGER, PARAMETER :: LECACC   = 16  ! Accumulation mode elemental carbon
  INTEGER, PARAMETER :: LPRIAKN  = 17  ! Aitken mode primary aerosol
  INTEGER, PARAMETER :: LPRIACC  = 18  ! Accumulation mode primary aerosol
  INTEGER, PARAMETER :: LPRICOR  = 19  ! Coarse mode primary aerosol
  INTEGER, PARAMETER :: LNAAKN   = 20  ! Aitken mode Sodium
  INTEGER, PARAMETER :: LNAACC   = 21  ! Accumulation mode Sodium
  INTEGER, PARAMETER :: LNACOR   = 22  ! Coarse mode Sodium
  INTEGER, PARAMETER :: LCLAKN   = 23  ! Aitken mode Chloride ion
  INTEGER, PARAMETER :: LCLACC   = 24  ! Accumulation mode Chloride ion
  INTEGER, PARAMETER :: LCLCOR   = 25  ! Coarse mode Chloride ion
  INTEGER, PARAMETER :: LNUMAKN  = 26  ! Aitken mode number
  INTEGER, PARAMETER :: LNUMACC  = 27  ! Accumulation mode number
  INTEGER, PARAMETER :: LNUMCOR  = 28  ! Coarse mode number
  INTEGER, PARAMETER :: LSRFAKN  = 29  ! Aitken mode surface area
  INTEGER, PARAMETER :: LSRFACC  = 30  ! Accumulation mode surface area
  INTEGER, PARAMETER :: LNACL    = 31  ! Sodium Chloride aerosol for AE3 only {depreciated in AE4}
  INTEGER, PARAMETER :: LCACO3   = 32  ! Calcium Carbonate aerosol (place holder)
  INTEGER, PARAMETER :: LMGCO3   = 33  ! Magnesium Carbonate aerosol (place holder)
  INTEGER, PARAMETER :: LA3FE    = 34  ! Iron aerosol (place holder)
  INTEGER, PARAMETER :: LB2MN    = 35  ! Manganese aerosol (place holder)
  INTEGER, PARAMETER :: LK       = 36  ! Potassium aerosol (Cl- tracked separately) (place holder)
	
  ! Indices for the AQCHEM arrays LIQUID and WETDEP

  INTEGER, PARAMETER :: LACL        =  1  ! Hydrogen ion
  INTEGER, PARAMETER :: LNH4L       =  2  ! Ammonium
  INTEGER, PARAMETER :: LCAL        =  3  ! Calcium
  INTEGER, PARAMETER :: LNAACCL     =  4  ! Sodium
  INTEGER, PARAMETER :: LOHL        =  5  ! Hydroxyl radical ion
  INTEGER, PARAMETER :: LSO4ACCL    =  6  ! Sulfate (attributed to accumulation mode)
  INTEGER, PARAMETER :: LHSO4ACCL   =  7  ! bisulfate (attributed to accumulation mode)
  INTEGER, PARAMETER :: LSO3L       =  8  ! sulfite
  INTEGER, PARAMETER :: LHSO3L      =  9  ! bisulfite
  INTEGER, PARAMETER :: LSO2L       = 10  ! sulfur dioxide
  INTEGER, PARAMETER :: LCO3L       = 11  ! carbonate
  INTEGER, PARAMETER :: LHCO3L      = 12  ! bicarbonate
  INTEGER, PARAMETER :: LCO2L       = 13  ! carbon dioxide
  INTEGER, PARAMETER :: LNO3ACCL    = 14  ! nitrate(attributed to accumulation mode)
  INTEGER, PARAMETER :: LNH3L       = 15  ! ammonia
  INTEGER, PARAMETER :: LCLACCL     = 16  ! chloride ion (attributed to accumulation mode)
  INTEGER, PARAMETER :: LH2O2L      = 17  ! hydrogen peroxide
  INTEGER, PARAMETER :: LO3L        = 18  ! ozone
  INTEGER, PARAMETER :: LFEL        = 19  ! iron
  INTEGER, PARAMETER :: LMNL        = 20  ! Manganese
  INTEGER, PARAMETER :: LAL         = 21  ! generalized anion associated with iron
  INTEGER, PARAMETER :: LFOAL       = 22  ! Formic acid
  INTEGER, PARAMETER :: LHCO2L      = 23  ! HCOO- ion
  INTEGER, PARAMETER :: LMHPL       = 24  ! Methyl hydrogen peroxide
  INTEGER, PARAMETER :: LPAAL       = 25  ! Peroxyacidic acid
  INTEGER, PARAMETER :: LHCLL       = 26  ! Hydrogen chloride
  INTEGER, PARAMETER :: LPRIML      = 27  ! primary aerosol
  INTEGER, PARAMETER :: LMGL        = 28  ! Magnesium
  INTEGER, PARAMETER :: LKL         = 29  ! potassium
  INTEGER, PARAMETER :: LBL         = 30  ! generalized anion associated with manganese
  INTEGER, PARAMETER :: LHNO3L      = 31  ! nitric acid
  INTEGER, PARAMETER :: LPRIMCORL   = 32  ! coarse-mode primary aerosol
  INTEGER, PARAMETER :: LNUMCORL    = 33  ! coarse-mode number
  INTEGER, PARAMETER :: LTS6CORL    = 34  ! sulfate (attributed to coarse mode)
  INTEGER, PARAMETER :: LNACORL     = 35  ! sodium (attributed to coarse mode)
  INTEGER, PARAMETER :: LCLCORL     = 36  ! chloride ion (attributed to coarse mode)
  INTEGER, PARAMETER :: LNO3CORL    = 37  ! nitrate (attributed to coarse mode)
  INTEGER, PARAMETER :: LORGAL      = 38  ! anthropogenic SOA
  INTEGER, PARAMETER :: LORGPL      = 39  ! primary organic aerosols
  INTEGER, PARAMETER :: LORGBL      = 40  ! biogenic SOA
  INTEGER, PARAMETER :: LECL        = 41  ! elemental carbon
  
  contains
  
!-------------------------------------------------------------------------------
  
	subroutine sorgam_aqchem_driver(   &
	    id, ktau, ktauc, dtstepc, config_flags,   &
	    p_phy, t_phy, rho_phy, alt, dz8w,  &
	    moist, chem,   &
	    gas_aqfrac, numgas_aqfrac,   &
	    ids,ide, jds,jde, kds,kde,   &
	    ims,ime, jms,jme, kms,kme,   &
	    its,ite, jts,jte, kts,kte )
    
    use module_ctrans_aqchem, only: aqchem
  	
  	use module_configure, only: grid_config_rec_type
    
    use module_state_description, only: &
      num_chem, &
  		num_moist, &
!      p_co2, &
      p_so2, &
      p_sulf, &
      p_nh3, &
      p_h2o2, &
      p_o3, &
      p_op1, &
      p_ora1, &
      p_paa, &
      p_hno3, &
      p_n2o5, &
      p_so4cwi, &
      p_nh4cwi, &
      p_no3cwi, &
      p_so4cwj, &
      p_nh4cwj, &
      p_no3cwj, &
      p_nacwi, &
      p_nacwj, &
      p_clcwi, &
      p_clcwj, &
!      p_so4cwk, &
!      p_no3cwk, &
      p_qv, &
      p_qc, &
       p_facd, &
       p_mepx, &
       p_pacd, &
       CB05_SORG_AQ_KPP

!!! TUCCELLA    
    !use module_data_sorgam, only: cw_phase, nphase_aer

    implicit none
    
    !
    ! Arguments
    !
    
  	! id - domain index
    ! ktau - time step number
    ! ktauc - gas and aerosol chemistry time step number
    ! numgas_aqfrac - last dimension of gas_aqfrac
    
    ! [ids:ide, kds:kde, jds:jde] - spatial (x,z,y) indices for 'domain'
    ! [ims:ime, kms:kme, jms:jme] - spatial (x,z,y) indices for 'memory'
    ! Most arrays that are arguments to chem_driver
    ! are dimensioned with these spatial indices.
    ! [its:ite, kts:kte, jts:jte] - spatial (x,z,y) indices for 'tile'
    ! chem_driver and routines under it do calculations
    ! over these spatial indices.
    
  	integer, intent(in) ::   &
  		id, ktau, ktauc,   &
  		numgas_aqfrac,   &
  		ids, ide, jds, jde, kds, kde,   &
  		ims, ime, jms, jme, kms, kme,   &
  		its, ite, jts, jte, kts, kte
    
    ! Configuration and control parameters:
    type(grid_config_rec_type), intent(in) :: config_flags
    
    ! Time step for gas and aerosol chemistry(s):
    real, intent(in) :: dtstepc
    
    ! p_phy - air pressure (Pa)
    ! t_phy - temperature (K)
    ! rho_phy - moist air density (kg/m^3)
    ! alt - dry air specific volume (m^3/kg)
    ! dz8w - level height (m)
    
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: &
      p_phy, t_phy, rho_phy, alt, dz8w
    
    ! Mixing ratios of moisture species (water vapor,
    ! cloud water, ...) (kg/kg for mass species, #/kg for number species):
    
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: moist
    
    ! Mixing ratios of trace gas and aerosol species (ppm for gases, 
    !	ug/kg for aerosol mass species, #/kg for aerosol number species):
    
    real, intent(inout), dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: chem
    
    ! Fraction (0-1) of gas that is dissolved in cloud water:
    
    real, intent(inout), dimension( ims:ime, kms:kme, jms:jme, numgas_aqfrac ) :: gas_aqfrac
    
    !
    ! I/O for AQCHEM
    !
    
    real, dimension (ngas)  :: gas     ! Mixing ratio of gas phase species (in gas + liquid phase) (mol/mol)
    real, dimension (naer)  :: aerosol ! Mixing ratio of aerosol species (mass/number/surface area in liquid phase) (mol/mol, #/mol)
    real, dimension (nliqs) :: liquid  ! mol/liter
    
    real, dimension (ngas) :: gaswdep ! mm mol/liter
    real, dimension (naer) :: aerwdep ! mm mol/liter
    real                   :: hpwdep  ! mm mol/liter
    
    real :: precip    ! Precipitation rate (mm/h)
    real :: airm      ! Column air number density (mol/m2)
    real :: rho_dry   ! Dry air mass density (kg/m3)
    real :: h2o_aq    ! Liquid water content ! (kg/m3)
    real :: h2o_total ! Total water content ! (kg/m3)
    
    real :: alfa0 ! Scavenging coeffficient for Aitken aerosol number
    real :: alfa2 ! Scavenging coeffficient for Aitken aerosol surface area
    real :: alfa3 ! Scavenging coeffficient for Aitken aerosol mass

    !!!! TUCCELLA 
    ! For cw phase
    integer :: cw_phase, nphase_aer
    !
    ! Other local variables
    !
    
    integer :: it, jt, kt
    
    real :: conv_factor
   
!!! TUCCELLA 
    ! Check that cw_phase is active
    
    !!! Get cw_phase and nphase_aer
    IF (config_flags%chem_opt==109) THEN
       CALL get_cwphase_soa_vbs(config_flags,cw_phase,nphase_aer)
    ELSE
       CALL get_cwphase_sorgam(config_flags,cw_phase,nphase_aer)
    END IF

    if ((cw_phase .le. 0) .or. (cw_phase .gt. nphase_aer)) then
      write(*,*) '*** module_sorgam_aqchem - cw_phase not active'
      return
    endif
    
!  	write(*,'(a,8(1x,i6))') 'entering module_sorgam_aqchem - ktau =', ktau
    
    ! We set the precipitation rate and aerosol scavenging rates to zero,
    ! in order to prevent wet scavenging in AQCHEM (it is treated elswhere):
    
    precip = 0.0 ! mm/hr
    
    alfa0 = 0.0
    alfa2 = 0.0
    alfa3 = 0.0
    
    ! Wet scavenging arrays
    
    gaswdep(:) = 0.0
    aerwdep(:) = 0.0
    hpwdep  = 0.0
    
    ! Loop over tile
    
    do jt = jts, jte
  	do it = its, ite
  	do kt = kts, kte
    
      if (moist(it,kt,jt,p_qc).gt.qcldwtr_cutoff) then
        
        ! Column air number density in layer:
        airm = 1000.0*rho_phy(it,kt,jt)*dz8w(it,kt,jt)/mwdry ! mol/m2
        
        ! Dry air mass density
        rho_dry = 1.0/alt(it,kt,jt) ! kg/m3
        
        ! Liquid water content:
        h2o_aq = moist(it,kt,jt,p_qc)*rho_dry ! (kg/m3)
        
        ! Total water content:
        h2o_total = (moist(it,kt,jt,p_qc)+moist(it,kt,jt,p_qv))*rho_dry ! (kg/m3)
        
        ! Gas phase concentrations before aqueous phase chemistry
        ! (with units conversion ppm -> mol/mol)
        
        gas(:) = 0.0
        
!        if (p_co2 .gt. 1)  then
!          gas(lco2) = chem(it,kt,jt,p_co2)*1.0e-6
!        else 
!              gas(lco2) = 380.0
           gas(lco2) = 380.0e-6
!        endif
        
        if (p_so2 .gt. 1)  gas(lso2)   = chem(it,kt,jt,p_so2)*1.0e-6
        if (p_hno3 .gt. 1) gas(lhno3)  = chem(it,kt,jt,p_hno3)*1.0e-6
        if (p_n2o5 .gt. 1) gas(ln2o5)  = chem(it,kt,jt,p_n2o5)*1.0e-6
        if (p_nh3 .gt. 1)  gas(lnh3)   = chem(it,kt,jt,p_nh3)*1.0e-6
        if (p_h2o2 .gt. 1) gas(lh2o2)  = chem(it,kt,jt,p_h2o2)*1.0e-6
        if (p_o3 .gt. 1)   gas(lo3)    = chem(it,kt,jt,p_o3)*1.0e-6
        if (p_sulf .gt. 1) gas(lh2so4) = chem(it,kt,jt,p_sulf)*1.0e-6

        if (config_flags%chem_opt==CB05_SORG_AQ_KPP) then
         if (p_facd .gt. 1) gas(lfoa)   = chem(it,kt,jt,p_facd)*1.0e-6
         if (p_mepx .gt. 1)  gas(lmhp)   = chem(it,kt,jt,p_mepx)*1.0e-6
         if (p_pacd .gt. 1)  gas(lpaa)   = chem(it,kt,jt,p_pacd)*1.0e-6
        else
         if (p_ora1 .gt. 1) gas(lfoa)   = chem(it,kt,jt,p_ora1)*1.0e-6
         if (p_op1 .gt. 1)  gas(lmhp)   = chem(it,kt,jt,p_op1)*1.0e-6
         if (p_paa .gt. 1)  gas(lpaa)   = chem(it,kt,jt,p_paa)*1.0e-6
        end if
        
        ! Aerosol mass concentrations before aqueous phase chemistry
        ! (with units conversion ug/kg -> mol/mol). Although AQCHEM
        ! accounts for much of the aerosol compounds in MADE, they are
        ! not treated at the moment by AQCHEM, as the mapping between
        ! the organic compound groups in MADE and AQCHEM is not obvious.
        
        aerosol(:) = 0.0
        
        aerosol(lso4akn)  = chem(it,kt,jt,p_so4cwi)*1.0e-9*mwdry/mwso4 ! Aitken mode sulfate
        aerosol(lnh4akn)  = chem(it,kt,jt,p_nh4cwi)*1.0e-9*mwdry/mwnh4 ! Aitken mode ammonium
        aerosol(lno3akn)  = chem(it,kt,jt,p_no3cwi)*1.0e-9*mwdry/mwno3 ! Aitken mode nitrate
!        aerosol(lnaakn)   = chem(it,kt,jt,p_nacwi)*1.0e-9*mwdry/mwna   ! Aitken mode Na
!        aerosol(lclakn)   = chem(it,kt,jt,p_clcwi)*1.0e-9*mwdry/mwcl   ! Aitken mode Cl
        
        aerosol(lorgaakn) = 0.0                                        ! Aitken mode anthropogenic SOA
        aerosol(lorgpakn) = 0.0                                        ! Aitken mode primary organic aerosol
        aerosol(lorgbakn) = 0.0                                        ! Aitken mode biogenic SOA
        aerosol(lecakn)   = 0.0                                        ! Aitken mode elemental carbon
        aerosol(lpriakn)  = 0.0                                        ! Aitken mode primary aerosol
        
        aerosol(lso4acc)  = chem(it,kt,jt,p_so4cwj)*1.0e-9*mwdry/mwso4 ! Accumulation mode sulfate
        aerosol(lnh4acc)  = chem(it,kt,jt,p_nh4cwj)*1.0e-9*mwdry/mwnh4 ! Accumulation mode ammonium
        aerosol(lno3acc)  = chem(it,kt,jt,p_no3cwj)*1.0e-9*mwdry/mwno3 ! Accumulation mode nitrate
        aerosol(lnaacc)   = chem(it,kt,jt,p_nacwj)*1.0e-9*mwdry/mwna   ! Accumulation mode Na
        aerosol(lclacc)   = chem(it,kt,jt,p_clcwj)*1.0e-9*mwdry/mwcl   ! Accumulation mode Cl
        
        aerosol(lorgaacc) = 0.0                                        ! Accumulation mode anthropogenic SOA
        aerosol(lorgpacc) = 0.0                                        ! Accumulation mode primary organic aerosol
        aerosol(lorgbacc) = 0.0                                        ! Accumulation mode biogenic SOA
        aerosol(lecacc)   = 0.0                                        ! Accumulation mode elemental carbon
        aerosol(lpriacc)  = 0.0                                        ! Accumulation mode primary aerosol
        
!        aerosol(lso4cor)  = chem(it,kt,jt,p_so4cwk)*1.0e-9*mwdry/mwso4 ! Coarse mode sulfate
!        aerosol(lno3cor)  = chem(it,kt,jt,p_no3cwk)*1.0e-9*mwdry/mwno3 ! Coarse mode nitrate
        aerosol(lnacor)   = 0.0                                        ! Coarse mode Na
        aerosol(lclcor)   = 0.0                                        ! Coarse mode Cl
        aerosol(lpricor)  = 0.0                                        ! Coarse mode primary aerosol

!based on CMAQ prescribed Fe/Mn
        aerosol(LA3FE) = 0.01*alt(it,kt,jt)*1.0e-9*mwdry/55.8
        aerosol(LB2MN) = 0.005*alt(it,kt,jt)*1.0e-9*mwdry/54.9

        ! Liquid phase concentrations
        
        liquid(:) = 0.0
        
        call aqchem( &
          t_phy(it,kt,jt), &
          p_phy(it,kt,jt), &
          dtstepc, &
          precip, &
          h2o_aq, &
          h2o_total, &
          airm, &
          alfa0, &
          alfa2, &
          alfa3, &
          gas, &
          aerosol, &
          liquid, &
          gaswdep, &
          aerwdep, &
          hpwdep)
        
        ! Gas phase concentrations after aqueous phase chemistry
        ! (with units conversion mol/mol -> ppm)
        
!        if (p_co2 .gt. 1)  chem(it,kt,jt,p_co2)  = gas(lco2)*1.0e6
        if (p_so2 .gt. 1)  chem(it,kt,jt,p_so2)  = gas(lso2)*1.0e6
        if (p_hno3 .gt. 1) chem(it,kt,jt,p_hno3) = gas(lhno3)*1.0e6
        if (p_n2o5 .gt. 1) chem(it,kt,jt,p_n2o5) = gas(ln2o5)*1.0e6
        if (p_nh3 .gt. 1)  chem(it,kt,jt,p_nh3)  = gas(lnh3)*1.0e6
        if (p_h2o2 .gt. 1) chem(it,kt,jt,p_h2o2) = gas(lh2o2)*1.0e6
        if (p_o3 .gt. 1)   chem(it,kt,jt,p_o3)   = gas(lo3)*1.0e6
        if (p_sulf .gt. 1) chem(it,kt,jt,p_sulf) = gas(lh2so4)*1.0e6

        if (config_flags%chem_opt==CB05_SORG_AQ_KPP) then
          if (p_facd .gt. 1) chem(it,kt,jt,p_facd) = gas(lfoa)*1.0e6
          if (p_mepx .gt. 1)  chem(it,kt,jt,p_mepx)  = gas(lmhp)*1.0e6
          if (p_pacd .gt. 1)  chem(it,kt,jt,p_pacd)  = gas(lpaa)*1.0e6
        else
          if (p_ora1 .gt. 1) chem(it,kt,jt,p_ora1) = gas(lfoa)*1.0e6
          if (p_op1 .gt. 1)  chem(it,kt,jt,p_op1)  = gas(lmhp)*1.0e6
          if (p_paa .gt. 1)  chem(it,kt,jt,p_paa)  = gas(lpaa)*1.0e6
        end if
    
        ! Aerosol mass concentrations after aqueous phase chemistry
        ! (with units conversion mol/mol -> ug/kg)
        
        chem(it,kt,jt,p_so4cwi) = aerosol(lso4akn) *1.0e9/mwdry*mwso4 ! Aitken mode sulfate
        chem(it,kt,jt,p_nh4cwi) = aerosol(lnh4akn) *1.0e9/mwdry*mwnh4 ! Aitken mode ammonium
        chem(it,kt,jt,p_no3cwi) = aerosol(lno3akn) *1.0e9/mwdry*mwno3 ! Aitken mode nitrate
        chem(it,kt,jt,p_nacwi)  = aerosol(lnaakn)  *1.0e9/mwdry*mwna  ! Aitken mode Na
        chem(it,kt,jt,p_clcwi)  = aerosol(lclakn)  *1.0e9/mwdry*mwcl  ! Aitken mode Cl
        
!        chem(it,kt,jt,........) = aerosol(lorgaakn)*1.0e9/mwdry*..... ! Aitken mode anthropogenic SOA
!        chem(it,kt,jt,........) = aerosol(lorgpakn)*1.0e9/mwdry*..... ! Aitken mode primary organic aerosol
!        chem(it,kt,jt,........) = aerosol(lorgbakn)*1.0e9/mwdry*..... ! Aitken mode biogenic SOA
!        chem(it,kt,jt,........) = aerosol(lecakn)  *1.0e9/mwdry*..... ! Aitken mode elemental carbon
!        chem(it,kt,jt,........) = aerosol(lpriakn) *1.0e9/mwdry*..... ! Aitken mode primary aerosol
        
        chem(it,kt,jt,p_so4cwj) = aerosol(lso4acc) *1.0e9/mwdry*mwso4 ! Accumulation mode sulfate
        chem(it,kt,jt,p_nh4cwj) = aerosol(lnh4acc) *1.0e9/mwdry*mwnh4 ! Accumulation mode ammonium
        chem(it,kt,jt,p_no3cwj) = aerosol(lno3acc) *1.0e9/mwdry*mwno3 ! Accumulation mode nitrate
        chem(it,kt,jt,p_nacwj)  = aerosol(lnaacc)  *1.0e9/mwdry*mwna  ! Accumulation mode Na
        chem(it,kt,jt,p_clcwj)  = aerosol(lclacc)  *1.0e9/mwdry*mwcl  ! Accumulation mode Cl
        
!        chem(it,kt,jt,........) = aerosol(lorgaacc)*1.0e9/mwdry*..... ! Accumulation mode anthropogenic SOA
!        chem(it,kt,jt,........) = aerosol(lorgpacc)*1.0e9/mwdry*..... ! Accumulation mode primary organic aerosol
!        chem(it,kt,jt,........) = aerosol(lorgbacc)*1.0e9/mwdry*..... ! Accumulation mode biogenic SOA
!        chem(it,kt,jt,........) = aerosol(lecacc)  *1.0e9/mwdry*..... ! Accumulation mode elemental carbon
!        chem(it,kt,jt,........) = aerosol(lpriacc) *1.0e9/mwdry*..... ! Accumulation mode primary aerosol
                                  
!        chem(it,kt,jt,p_so4cwk) = aerosol(lso4cor) *1.0e9/mwdry*mwso4 ! Coarse mode sulfate
!        chem(it,kt,jt,p_no3cwk) = aerosol(lno3cor) *1.0e9/mwdry*mwno3 ! Coarse mode nitrate
!        chem(it,kt,jt,........) = aerosol(lnacor)  *1.0e9/mwdry*..... ! Coarse mode Na
!        chem(it,kt,jt,........) = aerosol(lclcor)  *1.0e9/mwdry*..... ! Coarse mode Cl
!        chem(it,kt,jt,........) = aerosol(lpricor) *1.0e9/mwdry*..... ! Coarse mode primary aerosol
        
        ! Fraction of gas phase species dissolved in liquid water:
        
        gas_aqfrac(it,kt,jt,:) = 0.0
        
        conv_factor = 1.0E-3*moist(it,kt,jt,p_qc)*mwdry ! mol/liter -> mol/mol
        
!        if (p_co2  .gt. 1 .and. gas(lco2)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_co2)  = conv_factor*liquid(lco2l)/gas(lco2)
        if (p_so2  .gt. 1 .and. gas(lso2)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_so2)  = conv_factor*liquid(lso2l)/gas(lso2)
        if (p_nh3  .gt. 1 .and. gas(lnh3)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_nh3)  = conv_factor*liquid(lnh3l)/gas(lnh3)
        if (p_hno3 .gt. 1 .and. gas(lhno3) .gt. epsilc) gas_aqfrac(it,kt,jt,p_hno3) = conv_factor*liquid(lhno3l)/gas(lhno3)
        if (p_h2o2 .gt. 1 .and. gas(lh2o2) .gt. epsilc) gas_aqfrac(it,kt,jt,p_h2o2) = conv_factor*liquid(lh2o2l)/gas(lh2o2)
        if (p_o3   .gt. 1 .and. gas(lo3)   .gt. epsilc) gas_aqfrac(it,kt,jt,p_o3)   = conv_factor*liquid(lo3l)/gas(lo3)

        if (config_flags%chem_opt==CB05_SORG_AQ_KPP) then
          if (p_facd .gt. 1 .and. gas(lfoa)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_facd) = conv_factor*liquid(lfoal)/gas(lfoa)
          if (p_mepx  .gt. 1 .and. gas(lmhp)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_mepx)  = conv_factor*liquid(lmhpl)/gas(lmhp)
          if (p_pacd  .gt. 1 .and. gas(lpaa)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_pacd)  = conv_factor*liquid(lpaal)/gas(lpaa)
        else
          if (p_ora1 .gt. 1 .and. gas(lfoa)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_ora1) = conv_factor*liquid(lfoal)/gas(lfoa)
          if (p_op1  .gt. 1 .and. gas(lmhp)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_op1)  = conv_factor*liquid(lmhpl)/gas(lmhp)
          if (p_paa  .gt. 1 .and. gas(lpaa)  .gt. epsilc) gas_aqfrac(it,kt,jt,p_paa)  = conv_factor*liquid(lpaal)/gas(lpaa)
        end if
        
      endif
    
    enddo
    enddo
    enddo
    
	end subroutine sorgam_aqchem_driver

!!! TUCCELLA
        subroutine get_cwphase_sorgam(config_flags,cw,nphase)

        use module_data_sorgam, only: cw_phase, nphase_aer
        use module_configure, only: grid_config_rec_type

        implicit none

        ! Configuration and control parameters:
        type(grid_config_rec_type), intent(in) :: config_flags
        ! Out paramaeters for cw_phase
        integer, intent(out) :: cw,nphase

        cw = cw_phase
        nphase = nphase_aer

        end subroutine get_cwphase_sorgam


        subroutine get_cwphase_soa_vbs(config_flags,cw,nphase)

        use module_data_soa_vbs, only: cw_phase, nphase_aer
        use module_configure, only: grid_config_rec_type

        implicit none

        ! Configuration and control parameters:
        type(grid_config_rec_type), intent(in) :: config_flags
        ! Out paramaeters for cw_phase
        integer, intent(out) :: cw,nphase

        cw = cw_phase
        nphase = nphase_aer

        end subroutine get_cwphase_soa_vbs  
end module module_sorgam_aqchem
