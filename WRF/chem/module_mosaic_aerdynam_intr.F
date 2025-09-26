  module module_mosaic_aerdynam_intr


  implicit none


  contains


  !-----------------------------------------------------------------------
!
! *** eventually need to pass some performance stats back to mosaic driver
!
  subroutine aerosoldynamics(                     & !intent-ins
     idiagbb_host,                                &
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     swdownbox,                                   &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

! subroutine aerosoldynamics( it, t_out, t_in,    & !intent-ins
!    pr_atm, rh, te, cair_mol_m3,                 &
!    cnn, jaerosolstate, dp_wet_a,                & !intent-inouts
!    aH2O_a, gam_ratio, iter_MESA                 ) !intent-outs

  use module_data_mosaic_kind, only: r8
  use module_data_mosaic_main, only: &
       mw_air, m_partmc_mosaic, ntot_max, ntot_used
  use module_data_mosaic_aero, only : &
       dens_aer_mac, mw_aer_mac, mw_comp_a, &
       jhyst_undefined, msectional, msize_framework, &
       naer, nbin_a, nbin_a_max, ngas_aerchtot, no_aerosol, nsalt

  use module_mosaic_aerchem_intr, only: aerchemistry
  use module_mosaic_sect_intr, only: sectional_interface_1


  !Subroutine arguments
  integer,  intent(in)  :: idiagbb_host
  integer,  intent(in)  :: id, ii, jj, kk
  integer,  intent(in)  :: ktau, ktauc
! integer,  intent(in)  :: it
  integer,  intent(inout), dimension(nbin_a_max) :: jaerosolstate
! integer,  intent(out),   dimension(nbin_a_max) :: iter_mesa
  integer,  intent(inout), dimension(nbin_a_max) :: iter_mesa

  real(r8), intent(in) :: dtchem       ! chemistry time step (s)
! real(r8), intent(in) :: t_out, t_in
! real(r8), intent(in) :: pr_atm, rh, te, cair_mol_m3
  real(r8), intent(in) :: tempbox      ! temperature (deg k)
  real(r8), intent(in) :: presbox      ! pressure (pa)
  real(r8), intent(in) :: airdenbox    ! air density (kg/m3)
  real(r8), intent(in) :: relhumbox    ! relative humidity (0-1)
  real(r8), intent(in) :: swdownbox    ! 

! real(r8), intent(inout), dimension(ntot_max)   :: cnn
  real(r8), intent(inout), dimension(ntot_used)  :: rbox

  real(r8), intent(inout), dimension(nbin_a_max) :: dp_dry_a
  real(r8), intent(inout), dimension(nbin_a_max) :: dp_wet_a
! real(r8), intent(out),   dimension(nbin_a_max) :: aH2O_a, gam_ratio

  !Local variables
  integer :: it_host, it_mosaic
  integer, dimension(6)          :: hostgridinfox
  integer, dimension(nbin_a_max) :: jaerosolstate_bgn
  integer, dimension(nbin_a_max) :: jhyst_leg, jhyst_leg_sv1

  logical :: ldiag1, ldiag2

  real(r8) :: cair_mol_cc, cair_mol_m3
  real(r8) :: fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr
  real(r8) :: pr_atm
  real(r8) :: rh
  real(r8) :: te

  real(r8), dimension(ngas_aerchtot)     :: gas_avg  ! average gas conc. over dtchem time step (nmol/m3)
  real(r8), dimension(ngas_aerchtot)     :: gas_netprod_otrproc
            ! gas_netprod_otrproc = gas net production rate from other processes
            !    such as gas-phase chemistry and emissions (nmol/m3/s)
            ! NOTE - currently in the mosaic box model, gas_netprod_otrproc is set to zero for all
            !        species, so mosaic_aerchemistry does not apply production and condensation together
  real(r8), dimension(nbin_a_max)        :: aH2O_a
  real(r8), dimension(nbin_a_max)        :: gam_ratio
  real(r8), dimension(nbin_a_max)        :: dens_dry_a_bgn, dens_dry_a
! real(r8), dimension(nbin_a_max)        :: dp_dry_a, dp_dry_a_sv1
  real(r8), dimension(nbin_a_max)        :: dp_dry_a_sv1
  real(r8), dimension(nbin_a_max)        :: mass_dry_a_bgn, mass_dry_a
  real(r8), dimension(nbin_a_max)        :: sigmag_a, sigmag_a_sv1

  real(r8), allocatable, dimension(:)    :: rbox_sv1
! real(r8), allocatable, dimension(:)    :: rbox, rbox_sv1

  if (idiagbb_host >= 100) then
     ldiag1 = .true. ; ldiag2 = .true.
  else
     ldiag1 = .false. ; ldiag2 = .false.
  end if

  if ( ldiag2 ) &
  call dump_aerdy( 'dya',                         & !intent-ins
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

  if ( ldiag1 ) write(*,*) 'aerosoldynamics start'
  if ( (m_partmc_mosaic <= 0) .and. &
       (msize_framework == msectional) ) then
     if ( .not. allocated( rbox_sv1 ) ) allocate( rbox_sv1(1:ntot_used) )
!    allocate( rbox_sv1(1:ntot_used) )
  end if
! allocate( rbox(1:ntot_used) )

! dtchem = t_out - t_in
  it_host     = ktau
  it_mosaic   = ktauc
  hostgridinfox(1) = id
  hostgridinfox(2) = ii
  hostgridinfox(3) = jj
  hostgridinfox(4) = kk
  hostgridinfox(5:6) = 0

  te = tempbox
  rh = relhumbox*100.0_r8  ! rh - convert 0-1 value to %

  pr_atm = presbox / 1.01325e5_r8          ! air pressure - convert Pa to atm
  pr_atm = presbox / 1.032e5_r8            ! *** this is used in module_mosaic_therm ***

  cair_mol_m3 = airdenbox*1.0e3_r8/mw_air  ! air conc - convert kg/m3 to mol/m3
  cair_mol_cc = cair_mol_m3*1.e-6_r8       ! air conc - convert mol/m3 to mol/cm3

  iter_mesa(1:nbin_a_max) = 0
  jaerosolstate(1:nbin_a_max) = no_aerosol
  jaerosolstate_bgn(1:nbin_a_max) = no_aerosol
  jhyst_leg(1:nbin_a_max) = jhyst_undefined
  jhyst_leg_sv1(1:nbin_a_max) = jhyst_undefined

  gas_avg(1:ngas_aerchtot) = 0.0_r8
  gas_netprod_otrproc(1:ngas_aerchtot) = 0.0_r8

  ah2o_a(1:nbin_a_max) = 0.0_r8
  gam_ratio(1:nbin_a_max) = 0.0_r8
  dens_dry_a(1:nbin_a_max) = 0.0_r8
  dens_dry_a_bgn(1:nbin_a_max) = 0.0_r8
  dp_wet_a(1:nbin_a_max) = 0.0_r8
  dp_dry_a(1:nbin_a_max) = 0.0_r8
  dp_dry_a_sv1(1:nbin_a_max) = 0.0_r8
  mass_dry_a(1:nbin_a_max) = 0.0_r8
  mass_dry_a_bgn(1:nbin_a_max) = 0.0_r8
  sigmag_a(1:nbin_a_max) = 0.0_r8
  sigmag_a_sv1(1:nbin_a_max) = 0.0_r8


  ! map variables from cnn array to rbox (and other) arrays
  if ( ldiag1 ) write(*,*) 'aerosoldynamics call map 0'
  call aerodynam_map_mosaic_species( 0, &
       fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
       sigmag_a )
! call aerodynam_map_mosaic_species( 0, cair_mol_cc, cair_mol_m3, &
!      fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
!      cnn, rbox, jaerosolstate, jhyst_leg, dp_dry_a, sigmag_a )


  if ( (m_partmc_mosaic <= 0) .and. &
       (msize_framework == msectional) ) then
     rbox_sv1(1:ntot_used) = rbox(1:ntot_used)
     dp_dry_a_sv1(1:nbin_a) = dp_dry_a(1:nbin_a)
     sigmag_a_sv1(1:nbin_a) = sigmag_a(1:nbin_a)
     jhyst_leg_sv1(1:nbin_a) = jhyst_leg(1:nbin_a)
  end if


  if ( ldiag2 ) &
  call dump_aerdy( 'dyc',                         & !intent-ins
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

  if ( ldiag1 ) write(*,*) 'aerosoldynamics call aerchem'
  call aerchemistry(                                          &
     idiagbb_host,                                            &
     hostgridinfox, it_host, it_mosaic, dtchem,               & !intent-ins
     pr_atm, rh, te, cair_mol_m3, cair_mol_cc, swdownbox,     &
     jaerosolstate, jaerosolstate_bgn, jhyst_leg,             & !intent-inouts
     rbox, dp_dry_a, dp_wet_a, sigmag_a,                      & 
     gas_avg, gas_netprod_otrproc,                            & 
     mass_dry_a_bgn, mass_dry_a, dens_dry_a_bgn, dens_dry_a,  &
     aH2O_a, gam_ratio, iter_mesa                             ) !intent-outs


  if ( ldiag2 ) &
  call dump_aerdy( 'dye',                         & !intent-ins
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

  ! for sectional framework, calculate
  !    transfer of particles betweens due to growth/shrinkage
  !    new particle nucleation (optional)
  !    particle coagulation (optional)
  if ( (m_partmc_mosaic <= 0) .and. &
       (msize_framework == msectional) ) then

      if ( ldiag1 ) write(*,*) 'aerosoldynamics call sectiface'
      call sectional_interface_1( &
          idiagbb_host, &
          id, ii, jj, kk, ktau, ktauc, dtchem, &
          rbox_sv1, rbox, &
          it_mosaic, jaerosolstate_bgn, jaerosolstate, &
          jhyst_leg, jhyst_leg_sv1, &
          dp_dry_a, dp_dry_a_sv1, &
          sigmag_a, sigmag_a_sv1, &
          mass_dry_a_bgn, mass_dry_a, dens_dry_a_bgn, dens_dry_a, &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
          pr_atm, rh, te, cair_mol_m3, cair_mol_cc )

!     deallocate( rbox_sv1 )

  !else if (msize_framework == mmodal) then
  !   do similar calculations for modal framework, but this not yet implemented
  end if


  ! map variables back to cnn array from rbox (and other) arrays
  ! not need in wrfchem
! call aerodynam_map_mosaic_species( 1, cair_mol_cc, cair_mol_m3, &
!      fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
!      cnn, rbox, jaerosolstate, jhyst_leg, dp_dry_a, sigmag_a )


! deallocate( rbox )
  if ( ldiag1 ) write(*,*) 'aerosoldynamics end'


  if ( ldiag2 ) &
  call dump_aerdy( 'dyz',                         & !intent-ins
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

  return
  end subroutine aerosoldynamics



  !-----------------------------------------------------------------------
  subroutine aerodynam_map_mosaic_species( imap, &
       fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
       sigmag_a )
! subroutine aerodynam_map_mosaic_species( imap, cair_mol_cc, cair_mol_m3, &
!      fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr, &
!      cnn, rbox, jaerosolstate, jhyst_leg, dp_dry_a, sigmag_a )

    !
    !   in the mosaic offline box model, this subr. maps between cnn array and rbox array
    !      and does a few other things
    !   in wrfchem, this subr. just does a few other things
    !

    use module_data_mosaic_kind
!   use module_data_mosaic_main, only: &
!      avogad, &
!      kdpdry_a, kjhyst_a, knum_a, ksigmag_a, kwater_a, &
!      mw_air, naer_tot, ngas_max, ntot_max, ntot_used
    use module_data_mosaic_aero, only: &
       nbin_a, nbin_a_max
!      jhyst_undefined
!      jhyst_lo, jhyst_up, jhyst_undefined,                                      &
!      mhyst_method, mhyst_force_lo, mhyst_force_up, &
!      mhyst_uporlo_jhyst, mhyst_uporlo_waterhyst, &
!      mmovesect_flag1, mw_aer_mac, &
!      naer, naercomp, nbin_a, nbin_a_max, &
!      all_solid, all_liquid, mixed, no_aerosol
    use module_data_mosaic_asecthp, only: &
       dcen_sect, sigmag_aer, isize_of_ibin, itype_of_ibin
!      ai_phase, &
!      dens_aer, dens_water_aer, &
!      hygro_aer, hyswptr_aer, &
!      isize_of_ibin, itype_of_ibin, &
!      massptr_aer, ncomp_aer 


    ! subr parameters
    integer, intent(in) :: imap
!   integer, intent(in),    dimension(nbin_a_max) :: jaerosolstate
!   integer, intent(inout), dimension(nbin_a_max) :: jhyst_leg

!   real(r8), intent(in)    :: cair_mol_cc, cair_mol_m3
    real(r8), intent(inout) :: fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr
!   real(r8), intent(inout) :: cnn(ntot_max)
!   real(r8), intent(inout) :: rbox(ntot_used)
    real(r8), intent(inout), dimension(nbin_a_max) :: sigmag_a


    ! local variables
    integer :: ibin, iphase, isize, itype
!   integer :: l, ll, noffset
!   real(r8) :: conv_diam, conv_drym, conv_gas, conv_numb, conv_watr
!   real(r8) :: tmpa, tmph, tmpj, tmpr
    character(len=256) :: errmsg

    if ((imap < 0) .or. (imap > 1)) then
       write(errmsg,*) &
       '*** aerchem_map_mosaic_cnn_rbox fatal error - bad imap =', imap
       call wrf_error_fatal(trim(adjustl(errmsg)))
    end if


    ! set rbox units factors
    !
    ! note that the movesect and coag routines expect the aerosol mass units
    !    for rbox to be mass mixing ratios or mass concentrations
    ! they do not apply any molecular weight factors, so they cannot
    !    work with molar mixing ratios or molar concentrations
    !
    ! for trace gases,   rbox = [umol/mol-air = ppmv],     rbox*fact_gasmr = [mol/mol-air]
    fact_gasmr = 1.0e-6_r8

    ! for aerosol mass,  rbox = [ug-AP/kg-air], rbox*fact_apmassmr = [g-AP/g-air]
    fact_apmassmr = 1.0e-9_r8

    ! for aerosol numb,  rbox = [#/kg-air],     rbox*fact_apnumbmr = [#/g-air]
    fact_apnumbmr = 1.0e-3_r8

    ! aerosol densities in mosaic aerchem and sectional routines = [g/cm^3]
    !     (mosaic densities)*fact_apdens = [g/cm^3]
    fact_apdens = 1.0_r8

    ! aerosol diameters in mosaic aerchem and sectional routines = [cm]
    !     (mosaic diameters)*fact_apdiam = [cm]
    fact_apdiam = 1.0_r8


    ! factors for converting cnn values to rbox (and dp_dry_a_ values
!   tmpa = cair_mol_m3*mw_air   ! air_density in ]g/m^3]
    ! gases:             
    ! cnn is [molecules/cm^3-air],  rbox = cnn*conv_gas is [umol/mol-air]
!   conv_gas  = 1.0_r8/(avogad*cair_mol_cc*fact_gasmr)
    ! aerosol number:    
    ! cnn is [#/cm^3-air],          rbox = cnn*conv_numb is [#/kg-air]  when iunits_flagaa=4
!   conv_numb = 1.0e6_r8 /(fact_apnumbmr*tmpa)
    ! aerosol dry mass:  
    ! cnn is [umol/m^3-air],        rbox = cnn*conv_drym*mw_aer is [ug/kg-air]  when iunits_flagaa=4
!   conv_drym = 1.0e-6_r8/(fact_apmassmr*tmpa)
    ! aerosol water:     
    ! cnn is [kg/m^3-air],          rbox = cnn*conv_watr is [ug/kg-air]  when iunits_flagaa=4
!   conv_watr = 1.0e3_r8 /(fact_apmassmr*tmpa)
    ! aerosol diameter:  
    ! cnn is [um],                  dp_dry_a = cnn*conv_apdiam is [cm]
!   conv_diam = 1.0e-4_r8/fact_apdiam


    if (imap == 1) goto 20000


    !
    ! imap = 0 --> map from cnn to rbox
    !
!   rbox(1:ntot_used) = 0.0_r8

!   do l = 1, ngas_max
!      rbox(l) = cnn(l)*conv_gas
!   end do

    do ibin = 1, nbin_a
       isize = isize_of_ibin(ibin)
       itype = itype_of_ibin(ibin)
!      noffset = ngas_max + (ibin-1)*naer_tot

       ! rbox may or may not be able to hold dp_dry_a and sigma_g
       ! (yes in box model, no in wrfchem)
       ! so do not used rbox for holding dp_dry_a and sigma_g
!      dp_dry_a(ibin)          = cnn(kdpdry_a +noffset)*conv_diam
!      sigmag_a(ibin)          = cnn(ksigmag_a+noffset)
       sigmag_a(ibin)          = sigmag_aer(isize,itype)

!      rbox(knum_a   +noffset) = cnn(knum_a   +noffset)*conv_numb
!      rbox(kwater_a +noffset) = cnn(kwater_a +noffset)*conv_watr
!      do ll = 1, naer
!         l = noffset + (naer_tot-naer) + ll
!         rbox(l) = cnn(l)*(conv_drym*mw_aer_mac(ll))
!      end do

!      if (mhyst_method == mhyst_uporlo_waterhyst) then
          ! cnn and rbox hold water_a_hyst (in water conc units)
          ! SO  convert cnn value to rbox value
          ! AND set jhyst_leg to undefined
!         rbox(kjhyst_a +noffset) = cnn(kjhyst_a+noffset)*conv_watr
!         jhyst_leg(ibin) = jhyst_undefined
!      else
          ! cnn holds jhyst_leg, which can have three possible values
          ! rbox will eventually hold a surrogate for water_a_hyst
          ! SO  set jhyst_leg directly from cnn
          ! AND set rbox value to zero
!         rbox(kjhyst_a +noffset) = 0.0_r8
!         tmpa = cnn(noffset + kjhyst_a)
!         if (tmpa < -0.5_r8) then
!            jhyst_leg(ibin) = jhyst_undefined
!         else if (tmpa < 0.5_r8) then
!            jhyst_leg(ibin) = jhyst_lo
!         else
!            jhyst_leg(ibin) = jhyst_up
!         end if
!      end if

    end do

    return


20000 continue
    !
    ! imap = 1 --> map from rbox to cnn
    !
!   do l = 1, ngas_max
!      cnn(l) = rbox(l)/conv_gas
!   end do

!   do ibin = 1, nbin_a
!      isize = isize_of_ibin(ibin)
!      itype = itype_of_ibin(ibin)
!      noffset = ngas_max + (ibin-1)*naer_tot

!      cnn(kdpdry_a +noffset) = dp_dry_a(ibin)/conv_diam
!      cnn(ksigmag_a+noffset) = sigmag_a(ibin)

!      cnn(knum_a   +noffset) = rbox(knum_a   +noffset)/conv_numb
!      cnn(kwater_a +noffset) = rbox(kwater_a +noffset)/conv_watr
!      do ll = 1, naer
!         l = noffset + (naer_tot-naer) + ll
!         cnn(l) = rbox(l)/(conv_drym*mw_aer_mac(ll))
!      end do

!      if (mhyst_method == mhyst_uporlo_waterhyst) then
          ! cnn and rbox hold water_a_hyst (in water conc units)
          ! SO  convert rbox value to cnn value
          ! AND leave jhyst_leg to unchanged
!         cnn(kjhyst_a+noffset) = rbox(kjhyst_a+noffset)/conv_watr
!      else
          ! cnn holds jhyst_leg, which can have three possible values
          ! rbox will eventually hold a surrogate for water_a_hyst
          ! SO  set cnn directly from jhyst_leg
          ! AND leave rbox value unchanged
!         cnn(kjhyst_a+noffset) = jhyst_leg(ibin)*1.0_r8
!      end if

!   end do

    return
  end subroutine aerodynam_map_mosaic_species



  !-----------------------------------------------------------------------
  subroutine dump_aerdy( dumpchaa,                & !intent-ins
     id, ii, jj, kk,                              &
     ktau, ktauc, dtchem,                         &
     tempbox, presbox, airdenbox, relhumbox,      &
     rbox, iter_mesa, jaerosolstate,              & !intent-inouts
     dp_dry_a, dp_wet_a                           ) 

  use module_data_mosaic_kind, only: r8
  use module_data_mosaic_main, only: &
       ntot_used
  use module_data_mosaic_aero, only : &
       nbin_a_max
  use module_data_mosaic_asecthp, only : &
       nsize_aer, ntype_aer, numptr_aer, lptr_bc_aer, lptr_oc_aer, lptr_oin_aer

  use module_mosaic_aerchem_intr, only: aerchemistry
  use module_mosaic_sect_intr, only: sectional_interface_1


  !Subroutine arguments
  character(len=*), intent(in) :: dumpchaa

  integer,  intent(in)  :: id, ii, jj, kk
  integer,  intent(in)  :: ktau, ktauc
  integer,  intent(inout), dimension(nbin_a_max) :: jaerosolstate
  integer,  intent(inout), dimension(nbin_a_max) :: iter_mesa

  real(r8), intent(in) :: dtchem       ! chemistry time step (s)
  real(r8), intent(in) :: tempbox      ! temperature (deg k)
  real(r8), intent(in) :: presbox      ! pressure (pa)
  real(r8), intent(in) :: airdenbox    ! air density (kg/m3)
  real(r8), intent(in) :: relhumbox    ! relative humidity (0-1)

  real(r8), intent(inout), dimension(ntot_used)  :: rbox

  real(r8), intent(inout), dimension(nbin_a_max) :: dp_dry_a
  real(r8), intent(inout), dimension(nbin_a_max) :: dp_wet_a

  !Local variables
  integer :: isize, itype
  real(r8) :: tmpa
  character(len=80) :: fmtaa

  if (ii*jj*kk /= 1) return

  fmtaa = '(2a,1p,e10.3,7e11.3)'
  if (nsize_aer(1)*ntype_aer > 8) fmtaa = '(2a,1p,e10.3,7e11.3/(7x,1p,e10.3,7e11.3))'

! number output units = #/cm3 = (rbox #/kg-air)*(airden kg-air/m3)*(1e-6 m3/cm3)
  tmpa = airdenbox*1.0e-6_r8
  write(131,fmtaa) dumpchaa, ' num', &
     ( ( rbox(numptr_aer(isize,itype,1))*tmpa, &
         isize=1,nsize_aer(itype) ), itype=1,ntype_aer )

! mass output units = nmol/m3 = (rbox ug/kg-air)*(airden kg-air/m3)*(1e3 ng/ug)/(molecwght ng/nmol)
! and mosaic molecwght = 1.0 for oin, oc, and bc
  tmpa = airdenbox*1.0e3_r8
  write(131,fmtaa) dumpchaa, ' oin', &
     ( ( rbox(lptr_oin_aer(isize,itype,1))*tmpa, &
         isize=1,nsize_aer(itype) ), itype=1,ntype_aer )
  write(131,fmtaa) dumpchaa, ' oc ', &
     ( ( rbox(lptr_oc_aer(isize,itype,1))*tmpa, &
         isize=1,nsize_aer(itype) ), itype=1,ntype_aer )
  write(131,fmtaa) dumpchaa, ' bc ', &
     ( ( rbox(lptr_bc_aer(isize,itype,1))*tmpa, &
         isize=1,nsize_aer(itype) ), itype=1,ntype_aer )

  return
  end subroutine dump_aerdy



  end module module_mosaic_aerdynam_intr
