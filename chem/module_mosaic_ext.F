module module_mosaic_ext
contains


      ! scm_temp - subr dumpxx is temporary for scm testing
      subroutine dumpxx( dumpch, dtchem, t_k, p_atm, ah2o, &
         jaerosolstate, jphase, jhyst_leg, &
         aer, gas, num_a, water_a, water_a_hyst, dp_dry_a, &
         mosaic_vars_aa )

      use module_data_mosaic_kind,  only: r8

      use module_data_mosaic_aero,  only: &
         iso4_a,       ino3_a,       icl_a,        inh4_a,       &
         ina_a,        ioin_a,       ioc_a,        ibc_a,        &
         ipcg2_b_c_a,  ipcg2_b_o_a,  ipcg1_b_c_a,  ipcg1_b_o_a,  &
         iopcg1_b_c_a, iopcg1_b_o_a, ipcg2_f_c_a,  ipcg2_f_o_a,  &
         ipcg1_f_c_a,  ipcg1_f_o_a,  iopcg1_f_c_a, iopcg1_f_o_a, &
         iant1_c_a,    iant1_o_a,    ibiog1_c_a,   ibiog1_o_a,   &
         aer_name, gas_name, &
         jtotal, naer, nbin_a, nbin_a_max, ngas_aerchtot, ngas_volatile, &
         mosaic_vars_aa_type

      implicit none

      integer, intent(in), dimension(nbin_a_max) :: jaerosolstate, jphase, jhyst_leg

      real(r8), intent(in) :: dtchem, t_k, p_atm, ah2o
      real(r8), intent(in), dimension(naer,3,nbin_a_max) :: aer
      real(r8), intent(in), dimension(ngas_volatile) :: gas
      real(r8), intent(in), dimension(nbin_a_max) :: num_a, water_a, water_a_hyst, dp_dry_a

      character(len=*), intent(in) :: dumpch

      type (mosaic_vars_aa_type), intent(in) :: mosaic_vars_aa

      integer, parameter :: lunaa = 131
      integer, parameter :: naer_dump = 24
      integer :: ii, iv, ix, jj, kk, ktau
      integer :: iaer_dump(naer_dump) = 24
      character(len=80)  :: fmtaa
      character(len=2)   :: tmpch2
      character(len=256) :: tmpchaa
      character(len= 32) :: tmpchbb


!     return  ! scm_temp - for non-scm might want to activate this line

      if ( mosaic_vars_aa%idiagbb_host < 100 ) return

      ii = mosaic_vars_aa%hostgridinfo(2)
      jj = mosaic_vars_aa%hostgridinfo(3)
      kk = mosaic_vars_aa%hostgridinfo(4)
      if ( ii*jj*kk /= 1 ) return

      ktau = mosaic_vars_aa%it_host
      iaer_dump = &
         (/ iso4_a,       ino3_a,       icl_a,        inh4_a,      &
            ina_a,        ioin_a,       ioc_a,        ibc_a,       &
            ipcg2_b_c_a,  ipcg2_b_o_a,  ipcg1_b_c_a,  ipcg1_b_o_a, &
            iopcg1_b_c_a, iopcg1_b_o_a, ipcg2_f_c_a,  ipcg2_f_o_a, &
            ipcg1_f_c_a,  ipcg1_f_o_a,  iopcg1_f_c_a, iopcg1_f_o_a, &
            iant1_c_a,    iant1_o_a,    ibiog1_c_a,   ibiog1_o_a /)


! qakola
      tmpch2 = dumpch
      write(lunaa,'(/2a,i5,3i3)') tmpch2, 'dump', &
         ktau, ii, jj, kk
      write(lunaa,'(a,f11.2,f11.4,f11.4      )') 't p a ', t_k, p_atm, ah2o
!     if (tmpch2 == 'aa') &
!     write(lunaa,'(a,f11.2                  )') 'cairm3', cair_mol_m3

!     write(lunaa,'(a,1p,8e11.3)') 'gas1:4    ', gas(1:4)

      fmtaa = '(a,8i11)'
      if (nbin_a > 8) fmtaa = '(a,8i11/(10x,8i11))'
      write(lunaa,fmtaa) 'jstate    ', jaerosolstate(1:nbin_a)
      write(lunaa,fmtaa) 'jphase    ', jphase(1:nbin_a)
      write(lunaa,fmtaa) 'jhyst     ', jhyst_leg(1:nbin_a)

      fmtaa = '(a,1p,8e11.3)'
      if (nbin_a > 8) fmtaa = '(a,1p,8e11.3/(10x,1p,8e11.3))'
      write(lunaa,fmtaa) 'num       ', num_a(1:nbin_a)
      write(lunaa,fmtaa) 'dpdry     ', dp_dry_a(1:nbin_a)
      write(lunaa,fmtaa) 'water     ', water_a(1:nbin_a)
      write(lunaa,fmtaa) 'hyswtr    ', water_a_hyst(1:nbin_a)

!     write(lunaa,'(a,1p,8e11.3)') 'so4       ', aer(iso4_a,jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'no3       ', aer(ino3_a,jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'cl        ', aer(icl_a, jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'nh4       ', aer(inh4_a,jtotal,1:nbin_a)
!!!   write(lunaa,'(a,1p,8e11.3)') 'msa       ', aer(imsa_a,jtotal,1:nbin_a)
!!!   write(lunaa,'(a,1p,8e11.3)') 'co3       ', aer(ico3_a,jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'na        ', aer(ina_a, jtotal,1:nbin_a)
!!!   write(lunaa,'(a,1p,8e11.3)') 'ca        ', aer(ica_a, jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'oin       ', aer(ioin_a,jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'oc        ', aer(ioc_a, jtotal,1:nbin_a)
!     write(lunaa,'(a,1p,8e11.3)') 'bc        ', aer(ibc_a, jtotal,1:nbin_a)
       

!need to dump soa species
      do ix = 1, naer_dump
         iv = iaer_dump(ix)
         if (iv < 1 .or. iv > naer) cycle
         fmtaa = '(a,1p,8e11.3)'
         write(tmpchaa,fmtaa) aer_name(iv)(1:10), aer(iv,jtotal,1:min(nbin_a,8))
         if (iv <= ngas_aerchtot) then
            write(tmpchbb,fmtaa) gas_name(iv)(1:10), gas(iv)
            tmpchaa = trim(tmpchaa) // '  ' // trim(tmpchbb)
         end if
         write(lunaa,'(a)') trim(tmpchaa)
         fmtaa = '(10x,1p,8e11.3)'
         if (nbin_a > 8) write(lunaa,fmtaa) aer(iv,jtotal,9:nbin_a)
      end do ! ix


      return
      end subroutine dumpxx





  !***********************************************************************
  ! determines phase state of an aerosol bin. includes kelvin effect.
  !
  ! author: Rahul A. Zaveri
  ! update: Sep 2015
  !-----------------------------------------------------------------------
  subroutine aerosol_phase_state( ibin, jaerosolstate, jphase, aer,  &
       jhyst_leg, electrolyte, epercent, kel, activity, mc, num_a, mass_wet_a, mass_dry_a,    &
       mass_soluble_a, vol_dry_a, vol_wet_a, water_a, water_a_hyst, water_a_up, aH2O_a,     &
       aH2O, ma, gam, log_gamZ, zc, za, gam_ratio,     &
       xeq_a, na_Ma, nc_Mc, xeq_c,       mw_electrolyte, partial_molar_vol, sigma_soln, T_K, & ! RAZ deleted a_zsr
       RH_pc, mw_aer_mac, dens_aer_mac, sigma_water, Keq_ll, Keq_sl, MW_a, MW_c,             &
       growth_factor, MDRH, MDRH_T, molality0, rtol_mesa, jsalt_present, jsalt_index,       &
       jsulf_poor, jsulf_rich, phi_salt_old,                                     &
       kappa_nonelectro, mosaic_vars_aa )

    use module_data_mosaic_aero,  only: r8, nbin_a_max,                                   &
         ngas_aerchtot, ngas_volatile, nelectrolyte,                                      &!Parameters
         Ncation, naer, jtotal, all_solid, jhyst_up, all_liquid, Nanion, nrxn_aer_ll,     &
         nrxn_aer_sl, nsalt, MDRH_T_NUM, jsulf_poor_NUM, jsulf_rich_NUM,                  &!Parameters
         inh4_a, ina_a, ica_a, ico3_a, imsa_a, icl_a, ino3_a, iso4_a,                     & ! TBD
         a_zsr,  b_zsr,  aw_min,                                                          &! RAZ added a_zsr,  b_zsr,  aw_min
         mosaic_vars_aa_type


    implicit none
    !Intent -ins

    integer, intent(in):: ibin
    integer, intent(in), dimension(nsalt) :: jsalt_index
    integer, intent(in), dimension(jsulf_poor_NUM) :: jsulf_poor
    integer, intent(in), dimension(jsulf_rich_NUM) :: jsulf_rich

    real(r8), intent(in) :: aH2O,T_K,RH_pc,rtol_mesa
    real(r8), intent(in), dimension(naer) :: mw_aer_mac,dens_aer_mac
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(in), dimension(Nanion)  :: za,MW_a
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(in), dimension(ngas_aerchtot) :: partial_molar_vol
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    !Intent - inout
    integer, intent(inout), dimension(nsalt) :: jsalt_present
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase,jhyst_leg

    real(r8), intent(inout) :: sigma_water
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(inout), dimension(Nanion)  :: xeq_a,na_Ma
    real(r8), intent(inout), dimension(nbin_a_max) :: num_a,mass_wet_a,mass_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_soluble_a,gam_ratio
    real(r8), intent(inout), dimension(nbin_a_max) :: vol_dry_a,vol_wet_a,water_a
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a_hyst,water_a_up,aH2O_a
    real(r8), intent(inout), dimension(nbin_a_max) :: sigma_soln,growth_factor,MDRH
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0            ! RAZ 5/20/2014
    real(r8), intent(inout), dimension (nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension (nrxn_aer_sl) :: Keq_sl
    real(r8), intent(inout), dimension(MDRH_T_NUM) :: MDRH_T
    real(r8), intent(inout), dimension(ngas_aerchtot,nbin_a_max) :: kel
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(nelectrolyte,nelectrolyte) :: log_gamZ
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: epercent
    real(r8), intent(inout), dimension(nsalt) :: phi_salt_old

    type (mosaic_vars_aa_type), intent(inout) :: mosaic_vars_aa

    ! local variables
    character(len=256) :: errmsg
    integer, parameter :: aer_pha_sta_diagaa = -1 !BALLI- changed from 100 to -1
    integer, parameter :: iter_kelvin_method =  3 
    ! iter_kelvin_method = 1 - use rahuls original iteration method
    ! iter_kelvin_method = 2 - use bisection
    ! iter_kelvin_method = 3 - start with rahuls original iteration method, but if it fails, switch to bisection
    integer, parameter :: iter_kelvin_meth1_max = 10
    integer, parameter :: iter_kelvin_meth2_max = 100
    integer :: iaer, iv, itmpa
    integer :: iter_kelvin, iter_kelvin_meth1, iter_kelvin_state
    integer :: js, je

    real(r8) :: aer_H
    real(r8):: aH2O_range_bisect_toler
    real(r8) :: aH2O_a_new, aH2O_a_old, aH2O_a_oldn, aH2O_a_oldp, aH2O_a_del_state3
    real(r8), dimension(nbin_a_max) :: DpmV
    real(r8), dimension(nbin_a_max) :: kelvin
    real(r8) :: kelvin_old, kelvin_oldn, kelvin_oldp
    real(r8) :: kelvin_toler
    real(r8) :: rel_err, rel_err_old, rel_err_old2, rel_err_oldn, rel_err_oldp
    real(r8) :: term, tmpa
    real(r8) :: water_a_old, water_a_oldn, water_a_oldp


    if (aer_pha_sta_diagaa >= 3) &
    write(*,'(/a,5i5,2f12.8,1p,2e11.3)') 'aer_pha_sta_a', ibin, jhyst_leg(ibin), jaerosolstate(ibin), -1, 0, aH2O, aH2O_a(ibin)
    !aH2O = RH_pc*0.01 !**BALLI, this is already done in init subr
    aH2O_a(ibin) = aH2O
    kelvin(ibin) = 1.0
    do iv = 1, ngas_aerchtot
       kel(iv,ibin) = 1.0
    enddo

!   if(RH_pc .le. 97.0)then     ! RAZ
!      kelvin_toler = 1.e-4
!   else
!      kelvin_toler = 1.e-10    ! RAZ
!   endif
! define error tolerances become stricter as aH2O approaches 1.0
    kelvin_toler = 1.e-6_r8 * max( 1.0_r8-aH2O, 1.0e-4_r8 )
    aH2O_range_bisect_toler = 1.e-6_r8 * max( 1.0_r8-aH2O, 1.0e-4_r8 )


    ! calculate dry mass and dry volume of a bin
    mass_dry_a(ibin) = 0.0          ! initialize to 0.0
    vol_dry_a(ibin)  = 0.0          ! initialize to 0.0

    aer_H = (2.*aer(iso4_a,jtotal,ibin) +   &
         aer(ino3_a,jtotal,ibin) +   &
         aer(icl_a,jtotal,ibin)  +   &
         aer(imsa_a,jtotal,ibin) +   &
         2.*aer(ico3_a,jtotal,ibin))-   &
         (2.*aer(ica_a,jtotal,ibin)  +   &
         aer(ina_a,jtotal,ibin)  +   &
         aer(inh4_a,jtotal,ibin))
    aer_H = max(aer_H, 0.0d0)

    do iaer = 1, naer
       mass_dry_a(ibin) = mass_dry_a(ibin) +   &
            aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)  ! ng/m^3(air)
       vol_dry_a(ibin)  = vol_dry_a(ibin) +   &
            aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)/dens_aer_mac(iaer)       ! ncc/m^3(air)
    enddo
    mass_dry_a(ibin) = mass_dry_a(ibin) + aer_H
    vol_dry_a(ibin) = vol_dry_a(ibin) + aer_H

    mass_dry_a(ibin) = mass_dry_a(ibin)*1.e-15                      ! g/cc(air)
    vol_dry_a(ibin)  = vol_dry_a(ibin)*1.e-15                               ! cc(aer)/cc(air) or m^3/m^3(air)

    ! wet mass and wet volume
    mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3               ! g/cc(air)
    vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3                ! cc(aer)/cc(air) or m^3/m^3(air)


    water_a_up(ibin) = aerosol_water_up(ibin,electrolyte,aer,kappa_nonelectro,a_zsr)   ! for hysteresis curve determination

    iter_kelvin = 0
    iter_kelvin_meth1 = 0

    iter_kelvin_state = 0
    if (iter_kelvin_method == 2) iter_kelvin_state = 2

    aH2O_a_old = aH2O
    kelvin_old = 1.0_r8
    rel_err_old = 1.0e30_r8
    rel_err_old2 = 1.0e30_r8
    water_a_old = 0.0_r8

    aH2O_a_del_state3 = 1.0e-3_r8
    aH2O_a_oldn = aH2O
    aH2O_a_oldp = aH2O
    kelvin_oldp = 1.0_r8
    kelvin_oldn = 1.0_r8
    rel_err_oldn = 1.0e30_r8
    rel_err_oldp = 1.0e30_r8
    water_a_oldp = 0.0_r8
    water_a_oldn = 0.0_r8
    aH2O_a_new = aH2O    


10  iter_kelvin = iter_kelvin + 1
    aH2O_a(ibin) = aH2O_a_new

! RAZ uncommented the next 3 lines
      do je = 1, nelectrolyte
        molality0(je,ibin) = bin_molality(je,ibin,aH2O_a,b_zsr,a_zsr,aw_min)  ! compute aH2O dependent binary molalities  EFFI
      enddo
    call MESA( ibin, jaerosolstate, jphase, aer, jhyst_leg,         &
         electrolyte, epercent, activity, mc, num_a, mass_wet_a, mass_dry_a,              &
         mass_soluble_a, vol_dry_a, vol_wet_a, water_a, water_a_hyst, water_a_up, aH2O_a, &
         aH2O, ma, gam, log_gamZ, zc, za, gam_ratio, &
         xeq_a, na_Ma, nc_Mc, xeq_c, mw_electrolyte, mw_aer_mac, dens_aer_mac, Keq_ll,     &
         Keq_sl, MW_c, MW_a, growth_factor, MDRH, MDRH_T, molality0, rtol_mesa,            &
         jsalt_present, jsalt_index, jsulf_poor, jsulf_rich, phi_salt_old,    &
         kappa_nonelectro, mosaic_vars_aa )

    if(jaerosolstate(ibin) .eq. all_solid)then
       if (aer_pha_sta_diagaa >= 2) &
       write(*,'(a,5i5,2f12.8,1p,2e11.3)') 'aer_pha_sta_b', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
          iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin)
       return
    endif
    ! new wet mass and wet volume
    mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3               ! g/cc(air)
    vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3                ! cc(aer)/cc(air) or m^3/m^3(air)
 
    call calculate_kelvin(ibin,num_a,vol_wet_a,aH2O_a,DpmV,kelvin,sigma_soln,T_K,  &
         sigma_water)
    !      kelvin(ibin) = 1.0
    kelvin(ibin) = max( kelvin(ibin), 1.0_r8 )
    if (water_a(ibin) <= 0.0_r8) kelvin(ibin) = 1.0_r8

    aH2O_a_new = aH2O/kelvin(ibin)

!   if(RH_pc .le. 97.0)then
!     rel_err = abs( (aH2O_a_new - aH2O_a(ibin))/aH2O_a(ibin))
!   else
!     if(water_a(ibin) .gt. 0.0)then
!       rel_err = abs( (water_a(ibin) - water_a_old)/water_a(ibin))
!     else
!       rel_err = 0.0 ! no soluble material is present
!     endif
!   endif
! the above rel_err involve differences between current and previous
!    iteration values, and is not suitable for bisection
! this rel_err below uses error from the exact solution, and is suitable for bisection
    rel_err = (aH2O_a(ibin)*kelvin(ibin) - aH2O) / max( aH2O, 0.01_r8 )

    if (aer_pha_sta_diagaa >= 10) &
    write(*,'(a,2i5, 1p,e10.2, 0p,f14.10, 2x,2f14.10, 2x,1p,2e18.10)') &
       'iter_kelvin', iter_kelvin_state, iter_kelvin, rel_err, kelvin(ibin), &
       aH2O_a(ibin), aH2O_a_new, water_a_old, water_a(ibin)

    if (abs(rel_err) <= kelvin_toler) then
       iter_kelvin_state = iter_kelvin_state + 100
       goto 90
    end if

    if (iter_kelvin_state <= 0) then
       ! doing rahuls original iteration method
       itmpa = 0
       if (iter_kelvin >= iter_kelvin_meth1_max) then
          itmpa = 1
       else if (iter_kelvin >= iter_kelvin_meth1_max) then
          tmpa = min( rel_err_old, rel_err_old2 )
          if (tmpa < 0.0_r8 .and. rel_err <= tmpa) itmpa = 1
          tmpa = max( rel_err_old, rel_err_old2 )
          if (tmpa > 0.0_r8 .and. rel_err >= tmpa) itmpa = 1
       end if

       if (itmpa > 0) then
          if (iter_kelvin_method <= 1) then
             ! quit if number of iterations is too large OR 
             ! rel_err is outside the range of the previous two rel_err values,
             !    and one previous rel_err is positive, and one previous rel_err is negative 
             aH2O_a(ibin) = aH2O_a_new   ! do this to get same output as prev version
             if (aer_pha_sta_diagaa >= 1) &
             write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_err', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
             iter_kelvin_state = 100
             goto 90
          else
             ! switch to method 2 but do not iterate yet
             iter_kelvin_state = 1
             iter_kelvin_meth1 = iter_kelvin
          end if
       else
          ! save current values to old then do next iteration
          aH2O_a_old = aH2O_a(ibin)
          kelvin_old = kelvin(ibin)
          rel_err_old2 = rel_err_old
          rel_err_old = rel_err
          water_a_old  = water_a(ibin)
       !        aH2O = aH2O_a_new
       !        call MTEM_compute_log_gamZ  ! recompute activity coeffs (for surface tension and solid-liquid equilibria)
          goto 10
       end if
    endif

    if (iter_kelvin_state == 1) then
       ! rahuls original iteration method failed, so do some things before switching to bisection
       iter_kelvin_state = 2
       if (rel_err < 0.0_r8) then
          ! current aH2O_a has negative rel_err so must start at the beginning
          aH2O_a_new = aH2O
          goto 10
       else
          ! current aH2O_a has positive rel_err and can be used in bisection
          ! do not iterate yet
          continue
       end if
    end if

    if (iter_kelvin_state == 2) then
       ! this is first "setup" step of bisection, and the algorithm is expecting that 
       !    the current aH2O_a has hel_err be > 0, and can be used as one of the 2 bisection points
       if (rel_err < 0.0_r8) then
          ! error should be positive, so this is a fatal error
          if (aer_pha_sta_diagaa >= 1) &
             write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_er2', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
          iter_kelvin_state = 100
          goto 90
       end if
       ! current aH2O_a will work as one of the two initial bisection points
       ! (the one with a positive error)
       aH2O_a_oldp = aH2O_a(ibin)
       kelvin_oldp = kelvin(ibin)
       rel_err_oldp = rel_err
       water_a_oldp  = water_a(ibin)
       aH2O_a_new = min( aH2O/kelvin(ibin), 0.999999_r8 )   ! is this needed, or should it be 1.0, or ???
       iter_kelvin_state = 3
       goto 10
    end if

    if (iter_kelvin_state == 3) then
       ! this is the second "setup" step of bisection, and the algorithm is looking for an aH2O_a 
       ! that has rel_err < 0, so that the "root" will be bracketed and bisection can begin
       if (rel_err < 0.0_r8) then
          ! current aH2O_a will work as one of the two initial bisection points
          ! (the one with a negative error)
          aH2O_a_oldn = aH2O_a(ibin)
          kelvin_oldn = kelvin(ibin)
          rel_err_oldn = rel_err
          water_a_oldn  = water_a(ibin)
          aH2O_a_new = 0.5_r8*(aH2O_a_oldn + aH2O_a_oldp)
          iter_kelvin_state = 4
          goto 10
       else
          ! need to find a point with a negative error
          if ( (rel_err >= rel_err_oldp) .or. &
               (aH2O_a_del_state3 >= 0.999_r8) ) then
             ! cannot find such a point -- this is a fatal error
             if (aer_pha_sta_diagaa >= 1) &
                write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_er3', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                   iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
             iter_kelvin_state = 200
             goto 90
          else
             ! save current aH2O_a as the initial bisection point with positive error
             ! then calc aH2O_a_new = aH2O_a(ibin) - aH2O_a_del_state3
             ! which will hopefully have a negative error
             aH2O_a_oldp = aH2O_a(ibin)
             kelvin_oldp = kelvin(ibin)
             rel_err_oldp = rel_err
             water_a_oldp  = water_a(ibin)
             aH2O_a_new = aH2O_a(ibin) - aH2O_a_del_state3
             aH2O_a_del_state3 = aH2O_a_del_state3*1.5_r8
             if (aH2O_a_new .le. 0.01_r8) then
                aH2O_a_new = 0.01_r8
                aH2O_a_del_state3 = 1.0_r8
             end if
             goto 10
          end if
       end if
    end if

    if (iter_kelvin_state == 4) then
       ! at this point, the algorithm is doing bisection
       if ( iter_kelvin >= iter_kelvin_meth2_max + iter_kelvin_meth1 ) then
          ! maximum iterations is exceeded
          if (aer_pha_sta_diagaa >= 1) &
             write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_er4', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
          iter_kelvin_state = 301
          goto 90
       else if ( abs(aH2O_a_oldp - aH2O_a_oldn) <= aH2O_range_bisect_toler ) then
          ! the aH2O_a_oldp to aH2O_a_oldn range is very small, which is treated as convergence
!         if (aer_pha_sta_diagaa >= 1) &
!            write(*,'(a,5i5,2f12.8,1p,4e11.3)') 'iter_kelv_er5', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
!               iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler, &
!               aH2O_range_bisect_toler
          iter_kelvin_state = 302
          goto 90
       end if
       ! decide if the current aH2O_a should replace the old negative-error point
       !    or the old positive-error point
       if (rel_err >= 0.0_r8) then
          if (rel_err >= rel_err_oldp) then
             ! current aH2O_a has positive error, but the error is not smaller
             !    than the old positive-error point -- this is a fatal error
             if (aer_pha_sta_diagaa >= 1) &
                write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_er6', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                   iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
             iter_kelvin_state = 303
             goto 90
          else
             ! current aH2O_a has positive error and replaces the the old positive-error point
             aH2O_a_oldp = aH2O_a(ibin)
             kelvin_oldp = kelvin(ibin)
             rel_err_oldp = rel_err
             water_a_oldp  = water_a(ibin)
          end if
       else
          if (rel_err <= rel_err_oldn) then
             ! current aH2O_a has negative error, but the error is not smaller
             !    than the old negative-error point -- this is a fatal error
             if (aer_pha_sta_diagaa >= 1) &
                write(*,'(a,5i5,2f12.8,1p,3e11.3)') 'iter_kelv_er7', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
                   iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, kelvin_toler
             iter_kelvin_state = 304
             goto 90
          else
             ! current aH2O_a has negative error and replaces the the old negative-error point
             aH2O_a_oldn = aH2O_a(ibin)
             kelvin_oldn = kelvin(ibin)
             rel_err_oldn = rel_err
             water_a_oldn  = water_a(ibin)
          end if
       end if
       aH2O_a_new = 0.5_r8*(aH2O_a_oldn + aH2O_a_oldp)
       goto 10
    end if

    write(errmsg,'(a,4i5)') 'iter_kelv fatal err 1', ibin, iter_kelvin, iter_kelvin_state
    call wrf_error_fatal(trim(adjustl(errmsg)))


    ! kelvin iterations completed
90  if (iter_kelvin_state == 200) then
       ! select aH2O_a(ibin) or aH2O_a_oldp, whichever has lowest error
       if (abs(rel_err_oldp) < abs(rel_err)) then
          aH2O_a(ibin) = aH2O_a_oldp
          rel_err = rel_err_oldp
       end if
    else if (iter_kelvin_state >= 300 .and. iter_kelvin_state <= 304) then
       ! select aH2O_a(ibin) or aH2O_a_oldp or aH2O_a_oldn, whichever has lowest error
       tmpa = min( abs(rel_err_oldn), abs(rel_err_oldp), abs(rel_err) )
       if (abs(rel_err_oldp) == tmpa) then
          aH2O_a(ibin) = aH2O_a_oldp
          rel_err = rel_err_oldp
       else if (abs(rel_err_oldn) == tmpa) then
          aH2O_a(ibin) = aH2O_a_oldn
          rel_err = rel_err_oldn
       end if
    end if

    if(jaerosolstate(ibin) .eq. all_liquid)jhyst_leg(ibin) = jhyst_up

    ! now compute kelvin effect terms for condensing species (nh3, hno3, and hcl)
    do iv = 1,  ngas_aerchtot
       term = 4.*sigma_soln(ibin)*partial_molar_vol(iv)/   &
            (8.3144e7*T_K*DpmV(ibin))
       kel(iv,ibin) = 1. + term*(1. + 0.5*term*(1. + term/3.))
    enddo

    if (aer_pha_sta_diagaa >= 2) &
    write(*,'(a,5i5,2f12.8,1p,e11.3,e14.5)') 'aer_pha_sta_c', ibin, jhyst_leg(ibin), jaerosolstate(ibin), &
       iter_kelvin_state, iter_kelvin, aH2O, aH2O_a(ibin), rel_err, water_a(ibin)
    return
  end subroutine aerosol_phase_state



  !**********************************`*************************************
  ! MESA: Multicomponent Equilibrium Solver for Aerosols.
  ! Computes equilibrum solid and liquid phases by integrating
  ! pseudo-transient dissolution and precipitation reactions
  !
  ! author: Rahul A. Zaveri
  ! update: sep 2015
  ! 
  ! 9/3/2015 RAZ: Bugfix - fixed phase state calculations for aerosols that dont contain any salts,
  !               but can still contain water due to presence of BC, OC, SOA, and OIN, which are now
  !               allowed to absorb some water.
  !-----------------------------------------------------------------------
  subroutine MESA( ibin, jaerosolstate, jphase, aer, jhyst_leg,      &
       electrolyte, epercent, activity, mc, num_a, mass_wet_a, mass_dry_a, mass_soluble_a,    &
       vol_dry_a, vol_wet_a, water_a, water_a_hyst, water_a_up, aH2O_a, aH2O,                &
       ma, gam, log_gamZ, zc, za, gam_ratio, xeq_a,     &
       na_Ma, nc_Mc, xeq_c, mw_electrolyte, mw_aer_mac, dens_aer_mac, Keq_ll, Keq_sl, MW_c,    &
       MW_a, growth_factor, MDRH, MDRH_T, molality0, rtol_mesa, jsalt_present, jsalt_index,   &
       jsulf_poor, jsulf_rich, phi_salt_old,                                       &
       kappa_nonelectro, mosaic_vars_aa )

    use module_data_mosaic_aero,  only: r8, nbin_a_max, nelectrolyte, Ncation, naer,        &!Parameters
         jtotal, all_solid, jsolid, all_liquid, jliquid, jhyst_lo, mhyst_uporlo_jhyst,       &!Parameters
         jhyst_up, mhyst_uporlo_waterhyst, nsoluble, nsalt, Nanion, nrxn_aer_sl,            &
         nrxn_aer_ll, MDRH_T_NUM, jsulf_poor_NUM, jsulf_rich_NUM,                         &!Parameters
         ptol_mol_astem,  mhyst_force_lo,  mhyst_force_up,                               &!Input
         jcacl2, jcano3, mhyst_method, ioin_a, ibc_a, jcaco3, jcaso4,                     & !TBD
         mosaic_vars_aa_type



    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nbin_a_max)  :: jhyst_leg
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase
    integer, intent(in), dimension(nsalt) :: jsalt_index
    integer, intent(inout), dimension(nsalt) :: jsalt_present
    integer, intent(in), dimension(jsulf_poor_NUM) :: jsulf_poor
    integer, intent(in), dimension(jsulf_rich_NUM) :: jsulf_rich

    real(r8), intent(in) :: aH2O,rtol_mesa
    real(r8), intent(in), dimension(naer) :: mw_aer_mac,dens_aer_mac
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(in), dimension(Nanion)  :: za,MW_a
    real(r8), intent(inout), dimension(Nanion)  :: xeq_a,na_Ma
    real(r8), intent(inout), dimension(nbin_a_max) :: num_a,mass_wet_a,mass_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_soluble_a,vol_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: vol_wet_a,gam_ratio
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a,water_a_hyst,water_a_up
    real(r8), intent(inout), dimension(nbin_a_max) :: aH2O_a,growth_factor,MDRH
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0 !BSINGH(05/23/2014) - Added dimension nbin_a_max
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nrxn_aer_sl) :: Keq_sl
    real(r8), intent(inout), dimension(MDRH_T_NUM) :: MDRH_T
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(nelectrolyte,nelectrolyte) :: log_gamZ
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: epercent
    real(r8), intent(inout), dimension(nsalt) :: phi_salt_old
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    type (mosaic_vars_aa_type), intent(inout) :: mosaic_vars_aa

    ! local variables
    integer :: idissolved, j_index, jsalt_dum, jdum, js, je ! 9/3/2015 RAZ: added jsalt_dum
    real(r8) :: CRH, solids, sum_soluble, sum_insoluble, XT !BALLI** XT, should it be subr arg?
    !real(r8) :: aerosol_water                               ! mosaic func
    !real(r8) :: drh_mutual                          ! mosaic func
    real(r8) :: H_ion, sum_dum


    !! EFFI
    !! calculate percent composition
    sum_dum = 0.0
    do je = 1, nelectrolyte
       sum_dum = sum_dum + electrolyte(je,jtotal,ibin)
    enddo

    if(sum_dum .eq. 0.)sum_dum = 1.0

    do je = 1, nelectrolyte
       epercent(je,jtotal,ibin) = 100.*electrolyte(je,jtotal,ibin)/sum_dum
    enddo


    call calculate_XT(ibin,jtotal,XT,aer)



!! begin new algorithm - 6/3/2015 RAZ
    jsalt_dum = 0 ! 9/3/2015 RAZ
    do js = 1, nsalt
       jsalt_present(js) = 0                        ! default value - salt absent

       if(epercent(js,jtotal,ibin) .gt. ptol_mol_astem)then
          jsalt_present(js) = 1                     ! salt present
          jsalt_dum = jsalt_dum + jsalt_index(js)   ! 9/3/2015 RAZ
       endif
    enddo


    if( (epercent(jcano3,jtotal,ibin) .gt. ptol_mol_astem) .or. &
        (epercent(jcacl2,jtotal,ibin) .gt. ptol_mol_astem) )then
      CRH = 0.0  ! no crystrallization or efflorescence point
    else
      CRH = 0.35 ! default value
    endif

  ! now diagnose MDRH
    if(jsalt_dum .eq. 0)then			! no salts or acids are present ! 9/3/2015 RAZ: updated algorithm for jsalt_dum = 0

       CRH = 0.0
       MDRH(ibin) = 0.0
       jaerosolstate(ibin) = all_solid
       jphase(ibin)    = jsolid
       jhyst_leg(ibin) = jhyst_lo
       call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,water_a)
       water_a(ibin) = aerosol_water(jtotal,ibin,jaerosolstate,jphase,jhyst_leg,   &	! 9/3/2015 RAZ: water due to nonelectrolytes (OC, BC, SOA, OIN)
        electrolyte,aer,kappa_nonelectro,num_a,mass_dry_a,mass_soluble_a,aH2O,molality0)
       return

    elseif(XT .lt. 1. .and. XT .gt. 0.0)then  	! excess sulfate, always liquid, MDRH=0.0
       MDRH(ibin) = 0.0
    elseif(XT .ge. 2.0 .or. XT .lt. 0.0)then  	! sulfate poor
       j_index = jsulf_poor(jsalt_dum)          ! 9/3/2015 RAZ
       MDRH(ibin) = MDRH_T(j_index)
    else					! sulfate rich
       j_index = jsulf_rich(jsalt_dum)          ! 9/3/2015 RAZ
       MDRH(ibin) = MDRH_T(j_index)
    endif

    CRH = min(CRH, MDRH(ibin)/100.0)		! 6/3/2015 RAZ

!! end new algorithm - 6/3/2015 RAZ


    ! modified step 1: 9/3/2015 RAZ
    ! step 1: check if aH2O is below CRH (crystallization or efflorescence point)
    if( aH2O_a(ibin).lt.CRH )then
       jaerosolstate(ibin) = all_solid
       jphase(ibin)    = jsolid
       jhyst_leg(ibin) = jhyst_lo
       call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,water_a)
       water_a(ibin) = aerosol_water(jtotal,ibin,jaerosolstate,jphase,jhyst_leg,   &	! 9/3/2015 RAZ: water due to nonelectrolytes (OC, BC, SOA, OIN)
        electrolyte,aer,kappa_nonelectro,num_a,mass_dry_a,mass_soluble_a,aH2O,molality0)
       return
    endif


    ! step 2: check mhyst_method for supersaturation/metastable state
    jdum = 0
    if (mhyst_method == mhyst_uporlo_jhyst) then         ! BOX method/logic
       if (jhyst_leg(ibin) == jhyst_up) jdum = 1
    elseif (mhyst_method == mhyst_uporlo_waterhyst) then ! 3-D method/logic
       if (water_a_hyst(ibin) > 0.5*water_a_up(ibin)) jdum = 1
       !BSINGH - 05/28/2013(RCE updates)
    elseif (mhyst_method == mhyst_force_lo) then
       jdum = 0
    elseif (mhyst_method == mhyst_force_up) then
       jdum = 1
       !BSINGH - 05/28/2013(RCE updates ENDS)
    else
       call wrf_error_fatal('*** MESA - bad mhyst_method')
    endif
    if (jdum == 1) then ! the aerosol is fully deliquesced in metastable or subsaturated state
       call do_full_deliquescence(ibin,aer,electrolyte)

       !        call ions_to_electrolytes(jliquid,ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,nc_Mc,xeq_c) ! for Li and Lu surface tension
       !        call compute_activities(ibin,jphase,aer,jhyst_leg,electrolyte, &
       !activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a,aH2O,ma,gam,log_gamZ,gam_ratio)              ! for Li and Lu surface tension




! MODIFIED LOGIC IF SOA, POA, BC, OIN ARE ASSUMED TO BE SLIGHTLY HYGROSCOPIC  RAZ 4/16/2014
!       sum_soluble = 0.0
!       do js = 1, nsoluble
!          sum_soluble = sum_soluble + electrolyte(js,jtotal,ibin)
!       enddo
!
!       solids = electrolyte(jcaso4,jtotal,ibin) +   &
!                electrolyte(jcaco3,jtotal,ibin) +   &
!                aer(ioin_a,jtotal,ibin)         +   &
!                aer(ibc_a,jtotal,ibin)
!
!
!       if(sum_soluble .le. 0.0 .and. solids .gt. 0.0)then ! RAZ modified logic
!
!          jdum = 0
!          jaerosolstate(ibin) = all_solid ! no soluble material present, so go back to solid state
!          jphase(ibin) = jsolid
!          call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,water_a)
!
!          ! new wet mass and wet volume
!          mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3 ! g/cc(air)
!          vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3  ! cc(aer)/cc(air) or m^3/m^3(air)
!          growth_factor(ibin) = mass_wet_a(ibin)/mass_dry_a(ibin)   ! mass growth factor
!
!          return
!
!       elseif(sum_soluble .gt. 0.0)then  ! RAZ modified logic
!
          jaerosolstate(ibin) = all_liquid
          jhyst_leg(ibin) = jhyst_up
          jphase(ibin) = jliquid
          water_a(ibin) = aerosol_water(jtotal,ibin,jaerosolstate,jphase,jhyst_leg,   &
               electrolyte,aer,kappa_nonelectro,num_a,mass_dry_a,mass_soluble_a,aH2O,molality0)
          if(water_a(ibin) .le. 0.0)then     ! one last attempt to catch bad input
             jdum = 0
             jaerosolstate(ibin) = all_solid ! no soluble material present
             jphase(ibin)    = jsolid
             jhyst_leg(ibin) = jhyst_lo
             call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,water_a)
          else
             call adjust_liquid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent)
             call compute_activities(ibin,jaerosolstate,jphase,aer,jhyst_leg,      &
                  electrolyte,activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a, &
                  aH2O,ma,gam,log_gamZ,gam_ratio,Keq_ll,molality0,kappa_nonelectro)
          endif

          ! new wet mass and wet volume
          mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3 ! g/cc(air)
          vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3  ! cc(aer)/cc(air) or m^3/m^3(air)
          growth_factor(ibin) = mass_wet_a(ibin)/mass_dry_a(ibin)   ! mass growth factor

          return

!       endif


    endif ! jdum


    ! step 3: diagnose phase state based on MDRH
    if(aH2O_a(ibin)*100. .lt. MDRH(ibin)) then
       jaerosolstate(ibin) = all_solid
       jphase(ibin) = jsolid
       jhyst_leg(ibin) = jhyst_lo
       call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,water_a)
       return
    endif


    ! step 4: none of the above means it must be sub-saturated or mixed-phase
10  call do_full_deliquescence(ibin,aer,electrolyte)
    call MESA_PTC( ibin, jaerosolstate, jphase, aer, jhyst_leg,                   &
         electrolyte, epercent, activity, mc, num_a, mass_dry_a, mass_wet_a,      &
         mass_soluble_a, vol_dry_a, vol_wet_a, water_a, aH2O,                     &
         ma, gam, log_gamZ, zc, za, gam_ratio, xeq_a, na_Ma, nc_Mc, xeq_c,        &
         mw_electrolyte, mw_aer_mac, dens_aer_mac, Keq_sl, MW_c, MW_a, Keq_ll,    &
         growth_factor, molality0, rtol_mesa, jsalt_present, phi_salt_old,        &
         kappa_nonelectro, mosaic_vars_aa                                    )     ! determines jaerosolstate(ibin)
    return
  end subroutine MESA



  !***********************************************************************
  ! computes kelvin effect term (kelvin => 1.0)
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine calculate_kelvin(ibin,num_a,vol_wet_a,aH2O_a,DpmV,kelvin,sigma_soln,  &
       T_K,sigma_water)
    use module_data_mosaic_constants, only:  pi
    use module_data_mosaic_aero, only: r8,nbin_a_max                                   !Parameters

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(in) :: T_K,sigma_water
    real(r8), intent(in), dimension(nbin_a_max) :: num_a
    real(r8), intent(inout), dimension(nbin_a_max) :: sigma_soln
    real(r8), intent(inout), dimension(nbin_a_max) ::vol_wet_a,aH2O_a,DpmV,kelvin
    ! local variables
    integer je
    real(r8) :: term, sum_dum
    real(r8), dimension(nbin_a_max) :: volume_a

    volume_a(ibin) = vol_wet_a(ibin)                                ! [cc/cc(air)]
    DpmV(ibin)=(6.*volume_a(ibin)/(num_a(ibin)*pi))**(1./3.)        ! [cm]


    ! Li and Lu (2001) surface tension model:
    !      sum_dum = 0.0
    !      do je = 1, nelectrolyte
    !        sum_dum = sum_dum + G_MX(je)*
    !     &                      alog(1./(1.+K_MX(je)*activity(je,ibin)))
    !      enddo
    !      sigma_soln(ibin) = sigma_water + 8.3144e7*T_K*sum_dum


    ! simpler correlation for solution surface tension:
    sigma_soln(ibin) = sigma_water + 49.0*(1. - aH2O_a(ibin))       ! [dyn/cm]



    term = 72.*sigma_soln(ibin)/(8.3144e7*T_K*DpmV(ibin))           ! [-]
!    kelvin(ibin) = exp(term)
    kelvin(ibin) = 1. + term*(1. + 0.5*term*(1. + term/3.))


    return
  end subroutine calculate_kelvin



  !***********************************************************************
  ! computes sulfate ratio
  !
  ! author: Rahul A. Zaveri
  ! update: dec 1999
  !-----------------------------------------------------------------------
  subroutine calculate_XT(ibin,jp,XT,aer)
    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,                         &
         imsa_a,iso4_a,ica_a,ina_a,inh4_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin, jp
    real(r8), intent(inout) :: XT
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer


    if( (aer(iso4_a,jp,ibin)+aer(imsa_a,jp,ibin)) .gt.0.0)then
       XT   = ( aer(inh4_a,jp,ibin) +   &
            aer(ina_a,jp,ibin)  +   &
            2.*aer(ica_a,jp,ibin) )/   &
            (aer(iso4_a,jp,ibin)+0.5*aer(imsa_a,jp,ibin))
    else
       XT   = -1.0
    endif


    return
  end subroutine calculate_XT



  !***********************************************************************
  ! called when aerosol bin is completely solid.
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,  &
       water_a)

    use module_data_mosaic_aero, only: r8,nbin_a_max,naer,nelectrolyte,jsolid,     &!Parameters
         jhyst_lo,jtotal,jliquid,                                                  &!Parameters
         inh4_a,ino3_a,icl_a                                                        !TBD

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nbin_a_max) :: jphase,jhyst_leg
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte,epercent
    ! local variables
    integer iaer, je


    jphase(ibin)    = jsolid
    jhyst_leg(ibin) = jhyst_lo   ! lower curve
    water_a(ibin)   = 0.0

    ! transfer aer(jtotal) to aer(jsolid)
    do iaer = 1, naer
       aer(iaer, jsolid, ibin) = aer(iaer,jtotal,ibin)
       aer(iaer, jliquid,ibin) = 0.0
    enddo

    ! transfer electrolyte(jtotal) to electrolyte(jsolid)
    do je = 1, nelectrolyte
       electrolyte(je,jliquid,ibin) = 0.0
       epercent(je,jliquid,ibin)    = 0.0
       electrolyte(je,jsolid,ibin)  = electrolyte(je,jtotal,ibin)
       epercent(je,jsolid,ibin)     = epercent(je,jtotal,ibin)
    enddo

    ! update aer(jtotal) that may have been affected above
    aer(inh4_a,jtotal,ibin) = aer(inh4_a,jsolid,ibin)
    aer(ino3_a,jtotal,ibin) = aer(ino3_a,jsolid,ibin)
    aer(icl_a,jtotal,ibin)  = aer(icl_a,jsolid,ibin)


    return
  end subroutine adjust_solid_aerosol



  !***********************************************************************
  ! called when aerosol bin is completely liquid.
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine adjust_liquid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent) ! TOUCH

    use module_data_mosaic_aero, only: r8,nbin_a_max,naer,nelectrolyte,jliquid,    &!Parameters
         jhyst_up,jsolid,jtotal,                                                   &!Parameters
         jcaco3,jcaso4,                                                            &
         inh4_a,ina_a,ica_a,imsa_a,icl_a,ino3_a,iso4_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nbin_a_max) :: jphase,jhyst_leg

    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: epercent
    ! local variables
    integer :: iaer, je

    jphase(ibin)    = jliquid
    jhyst_leg(ibin) = jhyst_up   ! upper curve

    ! partition all electrolytes into liquid phase
    do je = 1, nelectrolyte
       electrolyte(je,jsolid,ibin)  = 0.0
       epercent(je,jsolid,ibin)     = 0.0
       electrolyte(je,jliquid,ibin) = electrolyte(je,jtotal,ibin)
       epercent(je,jliquid,ibin)    = epercent(je,jtotal,ibin)
    enddo
    ! except these electrolytes, which always remain in the solid phase
    electrolyte(jcaco3,jsolid,ibin) = electrolyte(jcaco3,jtotal,ibin)
    electrolyte(jcaso4,jsolid,ibin) = electrolyte(jcaso4,jtotal,ibin)
    epercent(jcaco3,jsolid,ibin)    = epercent(jcaco3,jtotal,ibin)
    epercent(jcaso4,jsolid,ibin)    = epercent(jcaso4,jtotal,ibin)
    electrolyte(jcaco3,jliquid,ibin)= 0.0
    electrolyte(jcaso4,jliquid,ibin)= 0.0
    epercent(jcaco3,jliquid,ibin)   = 0.0
    epercent(jcaso4,jliquid,ibin)   = 0.0


    ! partition all the aer species into
    ! solid phase
    do iaer = 1, naer
    aer(iaer,jsolid,ibin)  = aer(iaer,jtotal,ibin)
    end do
    aer(iso4_a,jsolid,ibin) = electrolyte(jcaso4,jsolid,ibin)
    aer(ino3_a,jsolid,ibin) = 0.0
    aer(icl_a,jsolid,ibin)  = 0.0
    aer(inh4_a,jsolid,ibin) = 0.0
    aer(imsa_a,jsolid,ibin) = 0.0
    aer(ina_a,jsolid,ibin)  = 0.0
    aer(ica_a,jsolid,ibin)  = electrolyte(jcaco3,jsolid,ibin) +   &
                              electrolyte(jcaso4,jsolid,ibin)
!   aer(ico3_a,jsolid,ibin) = aer(ico3_a,jtotal,ibin)
!   aer(ioc_a,jsolid,ibin)  = aer(ioc_a,jtotal,ibin)
!   aer(ibc_a,  jsolid,ibin)= aer(ibc_a,  jtotal,ibin)
!   aer(ioin_a, jsolid,ibin)= aer(ioin_a, jtotal,ibin)
!   aer(iaro1_a,jsolid,ibin)= aer(iaro1_a,jtotal,ibin)
!   aer(iaro2_a,jsolid,ibin)= aer(iaro2_a,jtotal,ibin)
!   aer(ialk1_a,jsolid,ibin)= aer(ialk1_a,jtotal,ibin)
!   aer(iole1_a,jsolid,ibin)= aer(iole1_a,jtotal,ibin)
!   aer(iapi1_a,jsolid,ibin)= aer(iapi1_a,jtotal,ibin)
!   aer(iapi2_a,jsolid,ibin)= aer(iapi2_a,jtotal,ibin)
!   aer(ilim1_a,jsolid,ibin)= aer(ilim1_a,jtotal,ibin)
!   aer(ilim2_a,jsolid,ibin)= aer(ilim2_a,jtotal,ibin)

    ! liquid-phase
    do iaer = 1, naer
    aer(iaer,jliquid,ibin)  = 0.0
    end do
    aer(iso4_a,jliquid,ibin) = aer(iso4_a,jtotal,ibin) -   &
                               aer(iso4_a,jsolid,ibin)
    aer(iso4_a,jliquid,ibin) = max(0.d0, aer(iso4_a,jliquid,ibin)) ! RAZ 4/16/2014
    aer(ino3_a,jliquid,ibin) = aer(ino3_a,jtotal,ibin)
    aer(icl_a,jliquid,ibin)  = aer(icl_a,jtotal,ibin)
    aer(inh4_a,jliquid,ibin) = aer(inh4_a,jtotal,ibin)
    aer(imsa_a,jliquid,ibin) = aer(imsa_a,jtotal,ibin)
    aer(ina_a,jliquid,ibin)  = aer(ina_a,jtotal,ibin)
    aer(ica_a,jliquid,ibin)  = aer(ica_a,jtotal,ibin) -   &
                               aer(ica_a,jsolid,ibin)
    aer(ica_a,jliquid,ibin)  = max(0.d0, aer(ica_a,jliquid,ibin)) ! RAZ 4/16/2014
!   aer(ico3_a,jliquid,ibin) = 0.0
!   aer(ioc_a,jliquid,ibin)  = 0.0
!   aer(ibc_a,  jliquid,ibin)= 0.0
!   aer(ioin_a, jliquid,ibin)= 0.0
!   aer(iaro1_a,jliquid,ibin)= 0.0
!   aer(iaro2_a,jliquid,ibin)= 0.0
!   aer(ialk1_a,jliquid,ibin)= 0.0
!   aer(iole1_a,jliquid,ibin)= 0.0
!   aer(iapi1_a,jliquid,ibin)= 0.0
!   aer(iapi2_a,jliquid,ibin)= 0.0
!   aer(ilim1_a,jliquid,ibin)= 0.0
!   aer(ilim2_a,jliquid,ibin)= 0.0

    return
  end subroutine adjust_liquid_aerosol



  !***********************************************************************
  ! this subroutine completely deliquesces an aerosol and partitions
  ! all the soluble electrolytes into the liquid phase and insoluble
  ! ones into the solid phase. It also calculates the corresponding
  ! aer(js,jliquid,ibin) and aer(js,jsolid,ibin) generic species
  ! concentrations
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine do_full_deliquescence(ibin,aer,electrolyte)    ! TOUCH
    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,nelectrolyte,jtotal,jsolid, &!Parameters
         jliquid,                                                                     &!Parameters
         jcacl2,jcano3,ioin_a,jcaco3,jcaso4,                                          &
         inh4_a,ina_a,ica_a,imsa_a,icl_a,ino3_a,iso4_a



    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer ::  iaer, js

    ! partition all electrolytes into liquid phase
    do js = 1, nelectrolyte
       electrolyte(js,jsolid,ibin)  = 0.0
       electrolyte(js,jliquid,ibin) = electrolyte(js,jtotal,ibin)
    enddo
    !
    ! except these electrolytes, which always remain in the solid phase
    electrolyte(jcaco3,jsolid,ibin) = electrolyte(jcaco3,jtotal,ibin)
    electrolyte(jcaso4,jsolid,ibin) = electrolyte(jcaso4,jtotal,ibin)
    electrolyte(jcaco3,jliquid,ibin)= 0.0
    electrolyte(jcaso4,jliquid,ibin)= 0.0


    ! partition all the generic aer species into solid and liquid phases
    ! solid phase
    do iaer = 1, naer
    aer(iaer,jsolid,ibin)= aer(iaer,jtotal,ibin)
    end do
    aer(iso4_a,jsolid,ibin) = electrolyte(jcaso4,jsolid,ibin)
    aer(ino3_a,jsolid,ibin) = 0.0
    aer(icl_a, jsolid,ibin) = 0.0
    aer(inh4_a,jsolid,ibin) = 0.0
    aer(imsa_a,jsolid,ibin) = 0.0
    aer(ina_a, jsolid,ibin) = 0.0
    aer(ica_a, jsolid,ibin) = electrolyte(jcaco3,jsolid,ibin) +   &
                              electrolyte(jcaso4,jsolid,ibin)
!   aer(ico3_a,jsolid,ibin) = aer(ico3_a,jtotal,ibin)
!   aer(ioc_a, jsolid,ibin) = aer(ioc_a,jtotal,ibin)
!   aer(ibc_a, jsolid,ibin) = aer(ibc_a,jtotal,ibin)
!   aer(ioin_a,jsolid,ibin) = aer(ioin_a,jtotal,ibin)
!   aer(iaro1_a,jsolid,ibin)= aer(iaro1_a,jtotal,ibin)
!   aer(iaro2_a,jsolid,ibin)= aer(iaro2_a,jtotal,ibin)
!   aer(ialk1_a,jsolid,ibin)= aer(ialk1_a,jtotal,ibin)
!   aer(iole1_a,jsolid,ibin)= aer(iole1_a,jtotal,ibin)
!   aer(iapi1_a,jsolid,ibin)= aer(iapi1_a,jtotal,ibin)
!   aer(iapi2_a,jsolid,ibin)= aer(iapi2_a,jtotal,ibin)
!   aer(ilim1_a,jsolid,ibin)= aer(ilim1_a,jtotal,ibin)
!   aer(ilim2_a,jsolid,ibin)= aer(ilim2_a,jtotal,ibin)

    ! liquid-phase
    do iaer = 1, naer
    aer(iaer,jliquid,ibin) = 0.0
    end do
    aer(iso4_a,jliquid,ibin) = max(0.0_r8, aer(iso4_a,jtotal,ibin) -   &
                               electrolyte(jcaso4,jsolid,ibin))      ! added max() RAZ 4/16/2014 
    aer(ino3_a,jliquid,ibin) = aer(ino3_a,jtotal,ibin)
    aer(icl_a, jliquid,ibin) = aer(icl_a,jtotal,ibin)
    aer(inh4_a,jliquid,ibin) = aer(inh4_a,jtotal,ibin)
    aer(imsa_a,jliquid,ibin) = aer(imsa_a,jtotal,ibin)
    aer(ina_a, jliquid,ibin) = aer(ina_a,jtotal,ibin)
    aer(ica_a, jliquid,ibin) = electrolyte(jcano3,jtotal,ibin) +   &
                               electrolyte(jcacl2,jtotal,ibin)
!   aer(ioc_a, jliquid,ibin) = 0.0
!   aer(ico3_a,jliquid,ibin) = 0.0
!   aer(ibc_a, jliquid,ibin) = 0.0
!   aer(ioin_a,jliquid,ibin) = 0.0
!   aer(iaro1_a,jliquid,ibin)= 0.0
!   aer(iaro2_a,jliquid,ibin)= 0.0
!   aer(ialk1_a,jliquid,ibin)= 0.0
!   aer(iole1_a,jliquid,ibin)= 0.0
!   aer(iapi1_a,jliquid,ibin)= 0.0
!   aer(iapi2_a,jliquid,ibin)= 0.0
!   aer(ilim1_a,jliquid,ibin)= 0.0
!   aer(ilim2_a,jliquid,ibin)= 0.0

    return
  end subroutine do_full_deliquescence
  
  
  
  !***********************************************************************
  ! MESA: Multicomponent Equilibrium Solver for Aerosol-phase
  ! computes equilibrum solid and liquid phases by integrating
  ! pseudo-transient dissolution and precipitation reactions
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  ! Reference: Zaveri R.A., R.C. Easter, and L.K. Peters, JGR, 2005b
  !-----------------------------------------------------------------------
  subroutine MESA_PTC(ibin, jaerosolstate, jphase, aer, jhyst_leg,  &
       electrolyte, epercent, activity, mc, num_a, mass_dry_a, mass_wet_a, mass_soluble_a,    &
       vol_dry_a, vol_wet_a, water_a, aH2O, ma, gam,   &
       log_gamZ, zc, za, gam_ratio, xeq_a, na_Ma, nc_Mc, xeq_c, mw_electrolyte, mw_aer_mac,     &
       dens_aer_mac, Keq_sl, MW_c, MW_a, Keq_ll, growth_factor, molality0, rtol_mesa,         &
       jsalt_present, phi_salt_old,                                                &
       kappa_nonelectro, mosaic_vars_aa )                ! TOUCH

    use module_data_mosaic_aero,  only: r8, nbin_a_max, nelectrolyte, Ncation, naer, nsalt,  &!Parameters
         jhyst_lo, mixed, all_liquid, jsolid, jliquid, jtotal, mYES,                         &!Parameters
         all_solid, Nanion, nrxn_aer_sl, nrxn_aer_ll,                                     &!Parameters
         ino3_a, iso4_a, ioc_a, ilim1_a, ilim2_a, inh4_a, ina_a, ica_a, ico3_a, imsa_a, icl_a, &
         mosaic_vars_aa_type

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nsalt) :: jsalt_present
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase,jhyst_leg

    real(r8), intent(in) :: aH2O,rtol_mesa
    real(r8), intent(in), dimension(naer) :: mw_aer_mac,dens_aer_mac
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(in), dimension(Nanion)  :: za,MW_a
    real(r8), intent(inout), dimension(Nanion)  :: xeq_a,na_Ma
    real(r8), intent(inout), dimension(nbin_a_max) :: num_a,mass_dry_a,mass_wet_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_soluble_a,vol_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: growth_factor
    real(r8), intent(inout), dimension(nbin_a_max) :: vol_wet_a,water_a,gam_ratio
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0 !BSINGH(05/23/2014) - Added dimension nbin_a_max
    real(r8), intent(inout), dimension(nrxn_aer_sl) :: Keq_sl
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(nelectrolyte,nelectrolyte) :: log_gamZ
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: epercent
    real(r8), intent(inout), dimension(nsalt) :: phi_salt_old
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    type (mosaic_vars_aa_type), intent(inout) :: mosaic_vars_aa

    ! local variables
    integer iaer, iconverge, iconverge_flux, iconverge_mass,   &
         idissolved, itdum, js, je, jp

    real(r8) :: tau_p(nsalt), tau_d(nsalt)
    real(r8) :: frac_solid, sumflux, hsalt_min, alpha, XT, dumdum,   &
         H_ion
    real(r8) :: phi_prod, alpha_fac, sum_dum
    real(r8) :: aer_H,hsalt_max
    real(r8), dimension(nelectrolyte) :: eleliquid
    real(r8), dimension(nbin_a_max) :: mass_dry_salt
    real(r8), dimension(nsalt) :: phi_salt,flux_sl,phi_bar,alpha_salt
    real(r8), dimension(nsalt) :: sat_ratio,hsalt
  
    ! function
    !real(r8) :: aerosol_water

    ! initialize
    itdum = 0               ! initialize time
    hsalt_max = 1.e25



    do js = 1, nsalt
       hsalt(js)     = 0.0
       sat_ratio(js) = 0.0
       phi_salt(js)  = 0.0
       flux_sl(js)   = 0.0
    enddo



    !! EFFI calculate percent composition
    sum_dum = 0.0
    do je = 1, nelectrolyte
       sum_dum = sum_dum + electrolyte(je,jtotal,ibin)
    enddo

    if(sum_dum .eq. 0.)sum_dum = 1.0

    do je = 1, nelectrolyte
       epercent(je,jtotal,ibin) = 100.*electrolyte(je,jtotal,ibin)/sum_dum
    enddo
    !! EFFI



    do js = 1, nsalt
       jsalt_present(js) = 0                        ! default value - salt absent
       if(epercent(js,jtotal,ibin) .gt. 1.0)then
          jsalt_present(js) = 1                     ! salt present
       endif
    enddo


    mass_dry_a(ibin) = 0.0

    aer_H = (2.*aer(iso4_a,jtotal,ibin) +   &
         aer(ino3_a,jtotal,ibin) +   &
         aer(icl_a,jtotal,ibin)  +   &
         aer(imsa_a,jtotal,ibin) +   &
         2.*aer(ico3_a,jtotal,ibin))-   &
         (2.*aer(ica_a,jtotal,ibin)  +   &
         aer(ina_a,jtotal,ibin)  +   &
         aer(inh4_a,jtotal,ibin))
    aer_H = max(aer_H, 0.0d0)

    do iaer = 1, naer
       mass_dry_a(ibin) = mass_dry_a(ibin) +   &
            aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)  ! [ng/m^3(air)]
       vol_dry_a(ibin)  = vol_dry_a(ibin) +   &
            aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)/dens_aer_mac(iaer)       ! ncc/m^3(air)
    enddo
    mass_dry_a(ibin) = mass_dry_a(ibin) + aer_H
    vol_dry_a(ibin) = vol_dry_a(ibin) + aer_H

    mass_dry_a(ibin) = mass_dry_a(ibin)*1.e-15                      ! [g/cc(air)]
    vol_dry_a(ibin) = vol_dry_a(ibin)*1.e-15                                ! [cc(aer)/cc(air)]

    mass_dry_salt(ibin) = 0.0               ! soluble salts only
    do je = 1, nsalt
       mass_dry_salt(ibin) = mass_dry_salt(ibin) +   &
            electrolyte(je,jtotal,ibin)*mw_electrolyte(je)*1.e-15   ! g/cc(air)
    enddo

    mosaic_vars_aa%jMESA_call = mosaic_vars_aa%jMESA_call + 1
    
    !----begin pseudo time continuation loop-------------------------------

    do 500 itdum = 1, mosaic_vars_aa%Nmax_MESA
       
       
       ! compute new salt fluxes
       call MESA_flux_salt(ibin,jaerosolstate,jphase, aer,jhyst_leg,electrolyte, &
            epercent,activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a,aH2O,ma,&
            gam,log_gamZ,zc,za,gam_ratio,xeq_a,na_Ma,nc_Mc,xeq_c,mw_electrolyte, &
            Keq_sl,MW_c,MW_a,Keq_ll,eleliquid,flux_sl,phi_salt,sat_ratio,        &
            molality0,jsalt_present,kappa_nonelectro)

       
       ! check convergence
       call MESA_convergence_criterion(ibin,iconverge_mass,iconverge_flux,idissolved, &
            aer,electrolyte,mass_dry_a,mass_dry_salt,mw_electrolyte,mw_aer_mac, &
            flux_sl,phi_salt,rtol_mesa)
       
       if(iconverge_mass .eq. mYES)then
          mosaic_vars_aa%iter_MESA(ibin) = mosaic_vars_aa%iter_MESA(ibin) + itdum
          mosaic_vars_aa%niter_MESA = mosaic_vars_aa%niter_MESA + float(itdum)
          mosaic_vars_aa%niter_MESA_max = max( mosaic_vars_aa%niter_MESA_max, itdum)
          jaerosolstate(ibin) = all_solid
          call adjust_solid_aerosol(ibin,jphase,aer,jhyst_leg,electrolyte,epercent,   &
               water_a)
          jhyst_leg(ibin) = jhyst_lo
          growth_factor(ibin) = 1.0
          return
       elseif(iconverge_flux .eq. mYES)then
          mosaic_vars_aa%iter_MESA(ibin) = mosaic_vars_aa%iter_MESA(ibin) + itdum
          mosaic_vars_aa%niter_MESA = mosaic_vars_aa%niter_MESA + float(itdum)
          mosaic_vars_aa%niter_MESA_max = max( mosaic_vars_aa%niter_MESA_max, itdum)
          jaerosolstate(ibin) = mixed
          vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3          ! cc(aer)/cc(air) or m^3/m^3(air)
          growth_factor(ibin) = mass_wet_a(ibin)/mass_dry_a(ibin)           ! mass growth factor
          
          if(idissolved .eq. myes)then
             jaerosolstate(ibin) = all_liquid
             !          jhyst_leg(ibin) = jhyst_up  ! ! do this later (to avoid tripping kelvin iterations)
          else
             jaerosolstate(ibin) = mixed
             jhyst_leg(ibin) = jhyst_lo
          endif
             
          ! calculate epercent(jsolid) composition in mixed-phase aerosol EFFI
          !!        sum_dum = 0.0
          !!        jp = jsolid
          !!        do je = 1, nelectrolyte
          !!          electrolyte(je,jp,ibin) = max(0.d0,electrolyte(je,jp,ibin)) ! remove -ve
          !!          sum_dum = sum_dum + electrolyte(je,jp,ibin)
          !!        enddo
          !!        electrolyte_sum(jp,ibin) = sum_dum
          !!        if(sum_dum .eq. 0.)sum_dum = 1.0
          !!        do je = 1, nelectrolyte
          !!          epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/sum_dum
          !!        enddo
          
          return
       endif
       
       ! calculate hsalt(js)        ! time step
       hsalt_min = 1.e25
      
       do js = 1, nsalt
          
          phi_prod = phi_salt(js) * phi_salt_old(js)

          if(itdum .gt. 1 .and. phi_prod .gt. 0.0)then
             phi_bar(js) = (abs(phi_salt(js))-abs(phi_salt_old(js)))/   &
                  alpha_salt(js)
          else
             phi_bar(js) = 0.0                      ! oscillating, or phi_salt and/or phi_salt_old may be zero
          endif

          if(phi_bar(js) .lt. 0.0)then              ! good. phi getting lower. maybe able to take bigger alphas
             phi_bar(js) = max(phi_bar(js), -10.0d0)
             alpha_fac = 3.0*exp(phi_bar(js))
             alpha_salt(js) = min(alpha_fac*abs(phi_salt(js)), 0.9d0)
          elseif(phi_bar(js) .gt. 0.0)then  ! bad - phi is getting bigger. so be conservative with alpha
             alpha_salt(js) = min(abs(phi_salt(js)), 0.5d0)
          else                                      ! very bad - phi is oscillating. be very conservative
             alpha_salt(js) = min(abs(phi_salt(js))/3.0d0, 0.5d0)
          endif
          
          !        alpha_salt(js) = max(alpha_salt(js), 0.01)
          
          phi_salt_old(js) = phi_salt(js)           ! update old array
          

          if(flux_sl(js) .gt. 0.)then
             
             tau_p(js) = eleliquid(js)/flux_sl(js)  ! precipitation time scale
             if(tau_p(js) .eq. 0.0)then
                hsalt(js) = 1.e25
                flux_sl(js) = 0.0
                phi_salt(js)= 0.0
             else
                hsalt(js) = alpha_salt(js)*tau_p(js)
             endif
             
          elseif(flux_sl(js) .lt. 0.)then
             
             tau_p(js) = -eleliquid(js)/flux_sl(js) ! precipitation time scale
             tau_d(js) = -electrolyte(js,jsolid,ibin)/flux_sl(js) ! dissolution time scale
             if(tau_p(js) .eq. 0.0)then
                hsalt(js) = alpha_salt(js)*tau_d(js)
             else
                hsalt(js) = alpha_salt(js)*min(tau_p(js),tau_d(js))
             endif
             
          else
             
             hsalt(js) = 1.e25
             
          endif
          
          hsalt_min = min(hsalt(js), hsalt_min)
          
       enddo

       !---------------------------------
       
       ! integrate electrolyte(solid)
       do js = 1, nsalt
          electrolyte(js,jsolid,ibin) = (   &
               (electrolyte(js,jsolid,ibin))  +   &
               (hsalt(js)) * (flux_sl(js)) )
       enddo
       
       
       ! compute aer(solid) from electrolyte(solid)
       call electrolytes_to_ions(jsolid,ibin,aer,electrolyte)
       
       
       ! compute new electrolyte(liquid) from mass balance
       do iaer = 1, naer
          aer(iaer,jliquid,ibin) = ( (aer(iaer,jtotal,ibin)) -   &
               (aer(iaer,jsolid,ibin)) )
       enddo
       
       !---------------------------------
       

       
500 continue     ! end time continuation loop
    !--------------------------------------------------------------------
    mosaic_vars_aa%jMESA_fail = mosaic_vars_aa%jMESA_fail + 1
    mosaic_vars_aa%iter_MESA(ibin) = mosaic_vars_aa%iter_MESA(ibin) + itdum
    mosaic_vars_aa%niter_MESA = mosaic_vars_aa%niter_MESA + float(itdum)
    jaerosolstate(ibin) = mixed
    jhyst_leg(ibin) = jhyst_lo
    mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3    ! g/cc(air)
    vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3     ! cc(aer)/cc(air) or m^3/m^3(air)
    growth_factor(ibin) = mass_wet_a(ibin)/mass_dry_a(ibin)      ! mass growth factor
   
    return
  end subroutine MESA_PTC



  !***********************************************************************
  ! part of MESA: calculates solid-liquid fluxes of soluble salts
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine MESA_flux_salt(ibin, jaerosolstate,jphase,aer,jhyst_leg,electrolyte,  &
       epercent,activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a,aH2O,ma,gam,   &
       log_gamZ,zc,za,gam_ratio,xeq_a,na_Ma,nc_Mc,xeq_c,mw_electrolyte,Keq_sl,MW_c,&
       MW_a,Keq_ll,eleliquid,flux_sl,phi_salt,sat_ratio,molality0,jsalt_present,   &
       kappa_nonelectro                                                            )      ! TOUCH

    use module_data_mosaic_aero, only: r8,nbin_a_max,nelectrolyte,Ncation,naer,    &!Parameters
         jliquid,nsalt,jsolid,Nanion,nrxn_aer_sl,nrxn_aer_ll,nrxn_aer_sl,          &!Parameter
         jna3hso4,ica_a,jcano3,jcacl2                                               !TBD

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nsalt) :: jsalt_present
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase,jhyst_leg

    real(r8), intent(in) :: aH2O
    real(r8), intent(inout), dimension(nsalt) :: flux_sl,phi_salt,sat_ratio
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(in), dimension(Nanion)  :: za,MW_a
    real(r8), intent(inout), dimension(Nanion)  :: xeq_a,na_Ma
    real(r8), intent(inout), dimension(nbin_a_max) :: num_a,mass_dry_a,gam_ratio
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_soluble_a,water_a
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(inout), dimension(nelectrolyte) :: eleliquid
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0 !BSINGH(05/23/2014) - Added dimension nbin_a_max
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nrxn_aer_sl) :: Keq_sl
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(nelectrolyte,nelectrolyte) :: log_gamZ
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte,epercent
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    ! local variables
    integer js, je
    real(r8) :: XT, calcium, sum_salt, sum_dum !**BALLI XT should it be subr arg??
    real(r8), dimension(nsalt) :: frac_salt_liq,frac_salt_solid


    ! compute activities and water content
    call ions_to_electrolytes(jliquid,ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,   &
         nc_Mc,xeq_c,mw_electrolyte,MW_c,MW_a)
    call compute_activities(ibin,jaerosolstate,jphase,aer,jhyst_leg,electrolyte,   &
         activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a,aH2O,ma,gam,log_gamZ, &
         gam_ratio,Keq_ll,molality0,kappa_nonelectro)
    activity(jna3hso4,ibin)   = 0.0

    if(water_a(ibin) .le. 0.0)then
       do js = 1, nsalt
          flux_sl(js) = 0.0
       enddo
       return
    endif


    call MESA_estimate_eleliquid(ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,nc_Mc,  &
         xeq_c,mw_electrolyte,MW_c,MW_a,eleliquid)

    calcium = aer(ica_a,jliquid,ibin)



    !! EFFI calculate percent composition
    sum_dum = 0.0
    do je = 1, nelectrolyte
       sum_dum = sum_dum + electrolyte(je,jliquid,ibin)
    enddo

    if(sum_dum .eq. 0.)sum_dum = 1.0

    do je = 1, nelectrolyte
       epercent(je,jliquid,ibin) = 100.*electrolyte(je,jliquid,ibin)/sum_dum
    enddo
    !! EFFI



    ! calculate % electrolyte composition in the solid and liquid phases
    sum_salt = 0.0
    do js = 1, nsalt
       sum_salt = sum_salt + electrolyte(js,jsolid,ibin)
    enddo

    if(sum_salt .eq. 0.0)sum_salt = 1.0
    do js = 1, nsalt
       frac_salt_solid(js) = electrolyte(js,jsolid,ibin)/sum_salt
       frac_salt_liq(js)   = epercent(js,jliquid,ibin)/100.
    enddo

    ! compute salt fluxes
    do js = 1, nsalt             ! soluble solid salts

       ! compute new saturation ratio
       sat_ratio(js) = activity(js,ibin)/Keq_sl(js)
       ! compute relative driving force
       phi_salt(js)  = (sat_ratio(js) - 1.0)/max(sat_ratio(js),1.0d0)

       ! check if too little solid-phase salt is trying to dissolve
       if(sat_ratio(js)       .lt. 1.00 .and.   &
            frac_salt_solid(js) .lt. 0.01 .and.   &
            frac_salt_solid(js) .gt. 0.0)then
          call MESA_dissolve_small_salt(ibin,js,aer,electrolyte)
          call MESA_estimate_eleliquid(ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,  &
               nc_Mc,xeq_c,mw_electrolyte,MW_c,MW_a,eleliquid)
          sat_ratio(js) = activity(js,ibin)/Keq_sl(js)
       endif

       ! compute flux
       flux_sl(js) = sat_ratio(js) - 1.0

       ! apply Heaviside function
       if( (sat_ratio(js)               .lt. 1.0 .and.   &
            electrolyte(js,jsolid,ibin) .eq. 0.0) .or.   &
            (calcium .gt. 0.0 .and. frac_salt_liq(js).lt.0.01).or.   &
            (calcium .gt. 0.0 .and. jsalt_present(js).eq.0) )then
          flux_sl(js) = 0.0
          phi_salt(js)= 0.0
       endif

    enddo


    ! force cacl2 and cano3 fluxes to zero
    sat_ratio(jcano3) = 1.0
    phi_salt(jcano3)  = 0.0
    flux_sl(jcano3)   = 0.0

    sat_ratio(jcacl2) = 1.0
    phi_salt(jcacl2)  = 0.0
    flux_sl(jcacl2)   = 0.0


    return
  end subroutine MESA_flux_salt

 !***********************************************************************
  ! computes activities
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2007
  !-----------------------------------------------------------------------
  subroutine compute_activities(ibin,jaerosolstate,jphase,aer,jhyst_leg,           &
       electrolyte,activity,mc,num_a,mass_dry_a,mass_soluble_a,water_a,aH2O,ma,gam,&
       log_gamZ,gam_ratio,Keq_ll,molality0,kappa_nonelectro)

    use module_data_mosaic_aero, only: r8,nbin_a_max,nelectrolyte,Ncation,naer,    &
         jliquid,Nanion,nrxn_aer_ll,                                               &
         iso4_a,ja_so4,ja_hso4,ino3_a,ja_no3,icl_a,ja_cl,imsa_a,ja_msa,ica_a,jc_ca,&
         inh4_a,jc_nh4,ina_a,jc_na,jc_h,jhcl,jhno3,jcacl2,jcano3,jnacl,jnano3,     &
         jna2so4,jnh4so4,jnh4cl,jnh4no3,jlvcite,jnh4hso4,jnh4msa,jna3hso4,jnahso4, &
         jnamsa,jcamsa2,jh2so4,jhhso4,jmsa                                          !TBD

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase,jhyst_leg

    real(r8), intent(in) :: aH2O
    real(r8), intent(in), dimension(nbin_a_max) :: num_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_dry_a,mass_soluble_a
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a,gam_ratio
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0 !BSINGH(05/23/2014) - Added dimension nbin_a_max
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(nelectrolyte,nelectrolyte) :: log_gamZ
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    ! local variables
    real(r8), dimension(nelectrolyte) :: log_gam
    integer jp, jA
    real(r8) :: XT, xmol(Nelectrolyte), sum_elec, dumK, c_bal, a_c !BALLI** should xt be subr arg??
    real(r8) :: quad, aq, bq, cq, xq, dum, mSULF
    !real(r8) :: aerosol_water     ! mosaic function


    water_a(ibin) = aerosol_water(jliquid,ibin,jaerosolstate,jphase, &
         jhyst_leg,electrolyte,aer,kappa_nonelectro,num_a,mass_dry_a,mass_soluble_a,aH2O, &
         molality0)      ! Kg/m^3(air)
    if(water_a(ibin) .eq. 0.0)return


    call calculate_XT(ibin,jliquid,XT,aer)


    if(XT.ge.2.0 .or. XT.lt.0.)then   ! changed .gt. to .ge.   RAZ 4/16/2014
       ! SULFATE POOR: fully dissociated electrolytes


       ! anion molalities (mol/kg water)
       ma(ja_so4,ibin)  = 1.e-9*aer(iso4_a,jliquid,ibin)/water_a(ibin)
       ma(ja_hso4,ibin) = 0.0
       ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
       ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)
       ma(ja_msa,ibin)  = 1.e-9*aer(imsa_a,jliquid,ibin)/water_a(ibin)

       ! cation molalities (mol/kg water)
       mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
       mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
       mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)
       a_c              = (   &
            (2.*ma(ja_so4,ibin)+   &
            ma(ja_no3,ibin)+   &
            ma(ja_cl,ibin) +   &
            ma(ja_msa,ibin)) -   &
            (2.*mc(jc_ca,ibin) +   &
            mc(jc_nh4,ibin)+   &
            mc(jc_na,ibin)) )

       mc(jc_h,ibin) = 0.5*( (a_c) +   &
            (sqrt(a_c**2 + 4.*Keq_ll(3))) )

       if(mc(jc_h,ibin) .le. 0.0)then   ! changed .eq. to .le. RAZ 4/16/2014
          mc(jc_h,ibin) = 1.e-10
       endif


       jp = jliquid


       sum_elec = 2.*electrolyte(jnh4no3,jp,ibin) +   &
            2.*electrolyte(jnh4cl,jp,ibin)  +   &
            3.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jnano3,jp,ibin)  +   &
            2.*electrolyte(jnacl,jp,ibin)   +   &
            3.*electrolyte(jcano3,jp,ibin)  +   &
            3.*electrolyte(jcacl2,jp,ibin)  +   &
            2.*electrolyte(jhno3,jp,ibin)   +   &
            2.*electrolyte(jhcl,jp,ibin)

       if(sum_elec .eq. 0.0)then
          do jA = 1, nelectrolyte
             gam(jA,ibin) = 1.0
          enddo
          goto 10
       endif


       ! ionic mole fractions
       xmol(jnh4no3) = 2.*electrolyte(jnh4no3,jp,ibin)/sum_elec
       xmol(jnh4cl)  = 2.*electrolyte(jnh4cl,jp,ibin) /sum_elec
       xmol(jnh4so4) = 3.*electrolyte(jnh4so4,jp,ibin)/sum_elec
       xmol(jna2so4) = 3.*electrolyte(jna2so4,jp,ibin)/sum_elec
       xmol(jnano3)  = 2.*electrolyte(jnano3,jp,ibin) /sum_elec
       xmol(jnacl)   = 2.*electrolyte(jnacl,jp,ibin)  /sum_elec
       xmol(jcano3)  = 3.*electrolyte(jcano3,jp,ibin) /sum_elec
       xmol(jcacl2)  = 3.*electrolyte(jcacl2,jp,ibin) /sum_elec
       xmol(jhno3)   = 2.*electrolyte(jhno3,jp,ibin)  /sum_elec
       xmol(jhcl)    = 2.*electrolyte(jhcl,jp,ibin)   /sum_elec


       jA = jnh4so4
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jnh4so4,ibin) = mc(jc_nh4,ibin)**2 * ma(ja_so4,ibin) *   &
               gam(jnh4so4,ibin)**3
       endif



! RAZ 11/7/2014
! always calculate gam(jnh4no3), even if xmol(jnh4no3) = 0. this to calculate gam_ratio
       jA = jnh4no3
!      if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jnh4no3,ibin) = mc(jc_nh4,ibin) * ma(ja_no3,ibin) *   &
               gam(jnh4no3,ibin)**2
!      endif


       jA = jnh4cl
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jnh4cl,ibin)  = mc(jc_nh4,ibin) * ma(ja_cl,ibin) *   &
               gam(jnh4cl,ibin)**2
       endif


       jA = jna2so4
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jna2so4,ibin) = mc(jc_na,ibin)**2 * ma(ja_so4,ibin) *   &
               gam(jna2so4,ibin)**3
       endif


       jA = jnano3
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jnano3,ibin)  = mc(jc_na,ibin) * ma(ja_no3,ibin) *   &
               gam(jnano3,ibin)**2
       endif



       jA = jnacl
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jnacl,ibin)   = mc(jc_na,ibin) * ma(ja_cl,ibin) *   &
               gam(jnacl,ibin)**2
       endif



       !c      jA = jcano3
       !c      if(xmol(jA).gt.0.0)then
       !c      gam(jA,ibin) = 1.0
       !c      activity(jcano3,ibin)  = 1.0
       !c      endif



       !c      jA = jcacl2
       !c      if(xmol(jA).gt.0.0)then
       !c      gam(jA,ibin) = 1.0
       !c      activity(jcacl2,ibin)  = 1.0
       !c      endif

       jA = jcano3
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jcano3,ibin)  = mc(jc_ca,ibin) * ma(ja_no3,ibin)**2 *   &
               gam(jcano3,ibin)**3
       endif



       jA = jcacl2
       if(xmol(jA).gt.0.0)then
          log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
               xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
               xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
               xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
               xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
               xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
               xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
               xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
               xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
               xmol(jhcl)   *log_gamZ(jA,jhcl)
          gam(jA,ibin) = 10.**log_gam(jA)
          activity(jcacl2,ibin)  = mc(jc_ca,ibin) * ma(ja_cl,ibin)**2 *   &
               gam(jcacl2,ibin)**3
       endif


       jA = jhno3
       log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
            xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
            xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
            xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
            xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
            xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
            xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
            xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
            xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)   *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)
       activity(jhno3,ibin)   = mc(jc_h,ibin) * ma(ja_no3,ibin) *   &
            gam(jhno3,ibin)**2


       jA = jhcl
       log_gam(jA) = xmol(jnh4no3)*log_gamZ(jA,jnh4no3) +   &
            xmol(jnh4cl) *log_gamZ(jA,jnh4cl)  +   &
            xmol(jnh4so4)*log_gamZ(jA,jnh4so4) +   &
            xmol(jna2so4)*log_gamZ(jA,jna2so4) +   &
            xmol(jnano3) *log_gamZ(jA,jnano3)  +   &
            xmol(jnacl)  *log_gamZ(jA,jnacl)   +   &
            xmol(jcano3) *log_gamZ(jA,jcano3)  +   &
            xmol(jcacl2) *log_gamZ(jA,jcacl2)  +   &
            xmol(jhno3)  *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)   *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)
       activity(jhcl,ibin)    = mc(jc_h,ibin) * ma(ja_cl,ibin) *   &
            gam(jhcl,ibin)**2

       !----
10     gam(jlvcite,ibin) = 1.0

       gam(jnh4hso4,ibin)= 1.0

       gam(jnh4msa,ibin) = 1.0

       gam(jna3hso4,ibin) = 1.0

       gam(jnahso4,ibin) = 1.0

       gam(jnamsa,ibin)  = 1.0

       gam(jcamsa2,ibin) = 1.0

       activity(jlvcite,ibin) = 0.0

       activity(jnh4hso4,ibin)= 0.0

       activity(jnh4msa,ibin) = mc(jc_nh4,ibin) * ma(ja_msa,ibin) *   &
            gam(jnh4msa,ibin)**2

       activity(jna3hso4,ibin)= 0.0

       activity(jnahso4,ibin) = 0.0

       activity(jnamsa,ibin) = mc(jc_na,ibin) * ma(ja_msa,ibin) *   &
            gam(jnamsa,ibin)**2

       activity(jcamsa2,ibin) = mc(jc_ca,ibin) * ma(ja_msa,ibin)**2 *   &
            gam(jcamsa2,ibin)**3

       gam_ratio(ibin) = gam(jnh4no3,ibin)**2/gam(jhno3,ibin)**2


    else
       !  SULFATE-RICH: solve for SO4= and HSO4- ions

       jp = jliquid

       sum_elec = 3.*electrolyte(jh2so4,jp,ibin)    +   &
            2.*electrolyte(jnh4hso4,jp,ibin)  +   &
            5.*electrolyte(jlvcite,jp,ibin)   +   &
            3.*electrolyte(jnh4so4,jp,ibin)   +   &
            2.*electrolyte(jnahso4,jp,ibin)   +   &
            5.*electrolyte(jna3hso4,jp,ibin)  +   &
            3.*electrolyte(jna2so4,jp,ibin)   +   &
            2.*electrolyte(jhno3,jp,ibin)     +   &
            2.*electrolyte(jhcl,jp,ibin)


       if(sum_elec .eq. 0.0)then
          do jA = 1, nelectrolyte
             gam(jA,ibin) = 1.0
          enddo
          goto 20
       endif


       xmol(jh2so4)  = 3.*electrolyte(jh2so4,jp,ibin)/sum_elec
       xmol(jnh4hso4)= 2.*electrolyte(jnh4hso4,jp,ibin)/sum_elec
       xmol(jlvcite) = 5.*electrolyte(jlvcite,jp,ibin)/sum_elec
       xmol(jnh4so4) = 3.*electrolyte(jnh4so4,jp,ibin)/sum_elec
       xmol(jnahso4) = 2.*electrolyte(jnahso4,jp,ibin)/sum_elec
       xmol(jna3hso4)= 5.*electrolyte(jna3hso4,jp,ibin)/sum_elec
       xmol(jna2so4) = 3.*electrolyte(jna2so4,jp,ibin)/sum_elec
       xmol(jhno3)   = 2.*electrolyte(jhno3,jp,ibin)/sum_elec
       xmol(jhcl)    = 2.*electrolyte(jhcl,jp,ibin)/sum_elec


       ! 2H.SO4
       jA = jh2so4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! H.HSO4
       jA = jhhso4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! NH4HSO4
       jA = jnh4hso4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! LETOVICITE
       jA = jlvcite
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! (NH4)2SO4
       jA = jnh4so4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! NaHSO4
       jA = jnahso4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! Na3H(SO4)2
       jA = jna3hso4
       !      log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +
       !     &              xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+
       !     &              xmol(jlvcite) *log_gamZ(jA,jlvcite) +
       !     &              xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +
       !     &              xmol(jnahso4) *log_gamZ(jA,jnahso4) +
       !     &              xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+
       !     &              xmol(jna2so4) *log_gamZ(jA,jna2so4) +
       !     &              xmol(jhno3)   *log_gamZ(jA,jhno3)   +
       !     &              xmol(jhcl)    *log_gamZ(jA,jhcl)
       !      gam(jA,ibin) = 10.**log_gam(jA)
       gam(jA,ibin) = 1.0


       ! Na2SO4
       jA = jna2so4
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! HNO3
       jA = jhno3
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


       ! HCl
       jA = jhcl
       log_gam(jA) = xmol(jh2so4)  *log_gamZ(jA,jh2so4)  +   &
            xmol(jnh4hso4)*log_gamZ(jA,jnh4hso4)+   &
            xmol(jlvcite) *log_gamZ(jA,jlvcite) +   &
            xmol(jnh4so4) *log_gamZ(jA,jnh4so4) +   &
            xmol(jnahso4) *log_gamZ(jA,jnahso4) +   &
            xmol(jna3hso4)*log_gamZ(jA,jna3hso4)+   &
            xmol(jna2so4) *log_gamZ(jA,jna2so4) +   &
            xmol(jhno3)   *log_gamZ(jA,jhno3)   +   &
            xmol(jhcl)    *log_gamZ(jA,jhcl)
       gam(jA,ibin) = 10.**log_gam(jA)


20     gam(jnh4no3,ibin) = 1.0
       gam(jnh4cl,ibin)  = 1.0
       gam(jnano3,ibin)  = 1.0
       gam(jnacl,ibin)   = 1.0
       gam(jcano3,ibin)  = 1.0
       gam(jcacl2,ibin)  = 1.0

       gam(jnh4msa,ibin) = 1.0
       gam(jnamsa,ibin)  = 1.0
       gam(jcamsa2,ibin) = 1.0



       ! compute equilibrium pH
       ! cation molalities (mol/kg water)
       mc(jc_ca,ibin)   = 1.e-9*aer(ica_a,jliquid,ibin)/water_a(ibin)
       mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
       mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)

       ! anion molalities (mol/kg water)
       mSULF            = 1.e-9*aer(iso4_a,jliquid,ibin)/water_a(ibin)
       ma(ja_hso4,ibin) = 0.0
       ma(ja_so4,ibin)  = 0.0
       ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
       ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)
       ma(ja_msa,ibin)  = 1.e-9*aer(imsa_a,jliquid,ibin)/water_a(ibin)

       gam_ratio(ibin)  = gam(jnh4hso4,ibin)**2/gam(jhhso4,ibin)**2
       dumK = Keq_ll(1)*gam(jhhso4,ibin)**2/gam(jh2so4,ibin)**3

       c_bal =  mc(jc_nh4,ibin) + mc(jc_na,ibin) + 2.*mc(jc_ca,ibin)   &
            - ma(ja_no3,ibin) - ma(ja_cl,ibin) - mSULF - ma(ja_msa,ibin)

       aq = 1.0
       bq = dumK + c_bal
       cq = dumK*(c_bal - mSULF)


       !--quadratic solution
       if(bq .ne. 0.0)then
          xq = 4.*(1./bq)*(cq/bq)
       else
          xq = 1.e+6
       endif

       if(abs(xq) .lt. 1.e-6)then
          dum = xq*(0.5 + xq*(0.125 + xq*0.0625))
          quad = (-0.5*bq/aq)*dum
          if(quad .lt. 0.)then
             quad = -bq/aq - quad
          endif
       else
          quad = 0.5*(-bq+sqrt(bq*bq - 4.*cq))
       endif
       !--end of quadratic solution

       mc(jc_h,ibin) = max(quad, 1.d-7)
       ma(ja_so4,ibin) = mSULF*dumK/(mc(jc_h,ibin) + dumK)
       ma(ja_hso4,ibin)= mSULF - ma(ja_so4,ibin)

       activity(jcamsa2,ibin) = mc(jc_ca,ibin) * ma(ja_msa,ibin)**2 *   &
            gam(jcamsa2,ibin)**3

       activity(jnh4so4,ibin) = mc(jc_nh4,ibin)**2 * ma(ja_so4,ibin) *   &
            gam(jnh4so4,ibin)**3

       activity(jlvcite,ibin) = mc(jc_nh4,ibin)**3 * ma(ja_hso4,ibin) *   &
            ma(ja_so4,ibin) * gam(jlvcite,ibin)**5

       activity(jnh4hso4,ibin)= mc(jc_nh4,ibin) * ma(ja_hso4,ibin) *   &
            gam(jnh4hso4,ibin)**2

       activity(jnh4msa,ibin) = mc(jc_nh4,ibin) * ma(ja_msa,ibin) *   &
            gam(jnh4msa,ibin)**2

       activity(jna2so4,ibin) = mc(jc_na,ibin)**2 * ma(ja_so4,ibin) *   &
            gam(jna2so4,ibin)**3

       activity(jnahso4,ibin) = mc(jc_na,ibin) * ma(ja_hso4,ibin) *   &
            gam(jnahso4,ibin)**2

       activity(jnamsa,ibin)  = mc(jc_na,ibin) * ma(ja_msa,ibin) *   &
            gam(jnamsa,ibin)**2

       !      activity(jna3hso4,ibin)= mc(jc_na,ibin)**3 * ma(ja_hso4,ibin) *
       !     &                         ma(ja_so4,ibin) * gam(jna3hso4,ibin)**5

       activity(jna3hso4,ibin)= 0.0

       activity(jhno3,ibin)   = mc(jc_h,ibin) * ma(ja_no3,ibin) *   &
            gam(jhno3,ibin)**2

       activity(jhcl,ibin)    = mc(jc_h,ibin) * ma(ja_cl,ibin) *   &
            gam(jhcl,ibin)**2

       activity(jmsa,ibin)    = mc(jc_h,ibin) * ma(ja_msa,ibin) *   &
            gam(jmsa,ibin)**2


       ! sulfate-poor species
       activity(jnh4no3,ibin) = 0.0

       activity(jnh4cl,ibin)  = 0.0

       activity(jnano3,ibin)  = 0.0

       activity(jnacl,ibin)   = 0.0

       activity(jcano3,ibin)  = 0.0

       activity(jcacl2,ibin)  = 0.0


    endif
    return
  end subroutine compute_activities



  !***********************************************************************
  ! part of MESA: checks MESA convergence
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine MESA_convergence_criterion(ibin,iconverge_mass,iconverge_flux,        &
       idissolved,aer,electrolyte,mass_dry_a,mass_dry_salt,                        &
       mw_electrolyte,mw_aer_mac,flux_sl,phi_salt,rtol_mesa)  ! TOUCH

    use module_data_mosaic_aero, only: r8,nbin_a_max,naer,nelectrolyte,nsalt,      &!Parameters
         jsolid, mYES, xhyst_up_crustal_thresh,                                    &!Parameters
         mno,ioin_a,jcaso4,jcaco3         !TBD

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(inout) :: iconverge_mass, iconverge_flux, idissolved
    real(r8), intent(in) :: rtol_mesa
    real(r8), intent(inout), dimension(nsalt) :: flux_sl,phi_salt
    real(r8), intent(in),    dimension(nbin_a_max) :: mass_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_dry_salt
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(in), dimension(naer) :: mw_aer_mac
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer je, js, iaer
    real(r8) :: mass_solid, mass_solid_salt,frac_solid, XT, H_ion,   &
         crustal_solids, sumflux


    idissolved = mno             ! default = not completely dissolved

    ! check mass convergence
    iconverge_mass = mNO ! default value = no convergence

    !      call electrolytes_to_ions(jsolid,ibin,aer,electrolyte)
    !      mass_solid = 0.0
    !      do iaer = 1, naer
    !        mass_solid = mass_solid +
    !     &               aer(iaer,jsolid,ibin)*mw_aer_mac(iaer)*1.e-15  ! g/cc(air)
    !      enddo

    mass_solid_salt = 0.0
    do je = 1, nsalt
       mass_solid_salt = mass_solid_salt +   &
            electrolyte(je,jsolid,ibin)*mw_electrolyte(je)*1.e-15        ! g/cc(air)
    enddo



    !      frac_solid = mass_solid/mass_dry_a(ibin)


    if(mass_dry_salt(ibin) .le. 0.0)then
      frac_solid = 0.0
    else
      frac_solid = mass_solid_salt/mass_dry_salt(ibin)
    endif


    if(frac_solid .ge. 0.98)then
       iconverge_mass = mYES
       return
    endif



    ! check relative driving force convergence
    iconverge_flux = mYES
    do js = 1, nsalt
       if(abs(phi_salt(js)).gt. rtol_mesa)then
          iconverge_flux = mNO
          return
       endif
    enddo



    ! check if all the fluxes are zero

    sumflux = 0.0
    do js = 1, nsalt
       sumflux = sumflux + abs(flux_sl(js))
    enddo

!   crustal_solids = electrolyte(jcaco3,jsolid,ibin) +   &
!        electrolyte(jcaso4,jsolid,ibin) +   &
!        aer(ioin_a,jsolid,ibin)
    crustal_solids = electrolyte(jcaco3,jsolid,ibin)*mw_electrolyte(jcaco3) +  &
                     electrolyte(jcaso4,jsolid,ibin)*mw_electrolyte(jcaso4) +  &
                     aer(ioin_a,jsolid,ibin)*mw_aer_mac(ioin_a)

!   if(sumflux .eq. 0.0 .and. crustal_solids .eq. 0.0)then
    if ( sumflux .eq. 0.0 .and. &
         crustal_solids .le. xhyst_up_crustal_thresh*(mass_dry_a(ibin)*1.0e15) ) then
       ! crustal_solids is ng/m^3, mass_dry_a is g/cm^3
       idissolved = myes
    endif



    return
  end subroutine MESA_convergence_criterion



  !***********************************************************************
  ! computes ions from electrolytes
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine electrolytes_to_ions(jp,ibin,aer,electrolyte)

    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &!Parameters
         jh2so4,jnh4hso4,jlvcite,jnh4so4,jnahso4,jna3hso4,jna2so4,jcaso4,iso4_a,   &!TBD
         jhno3,jnh4no3,jcano3,jnano3,ino3_a,jhcl,jnh4cl,jcacl2,jnacl,icl_a,jmsa,   &!TBD
         jcamsa2,jnamsa,jnh4msa,imsa_a,jcaco3,ico3_a,ica_a,ina_a,inh4_a             !TBD
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: sum_dum


    aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
         electrolyte(jna2so4,jp,ibin) +   &
         2.*electrolyte(jna3hso4,jp,ibin)+   &
         electrolyte(jnahso4,jp,ibin) +   &
         electrolyte(jnh4so4,jp,ibin) +   &
         2.*electrolyte(jlvcite,jp,ibin) +   &
         electrolyte(jnh4hso4,jp,ibin)+   &
         electrolyte(jh2so4,jp,ibin)

    aer(ino3_a,jp,ibin) = electrolyte(jnano3,jp,ibin)  +   &
         2.*electrolyte(jcano3,jp,ibin)  +   &
         electrolyte(jnh4no3,jp,ibin) +   &
         electrolyte(jhno3,jp,ibin)

    aer(icl_a,jp,ibin)  = electrolyte(jnacl,jp,ibin)   +   &
         2.*electrolyte(jcacl2,jp,ibin)  +   &
         electrolyte(jnh4cl,jp,ibin)  +   &
         electrolyte(jhcl,jp,ibin)

    aer(imsa_a,jp,ibin) = electrolyte(jnh4msa,jp,ibin) +   &
         electrolyte(jnamsa,jp,ibin)  +   &
         2.*electrolyte(jcamsa2,jp,ibin) +   &
         electrolyte(jmsa,jp,ibin)

    aer(ico3_a,jp,ibin) = electrolyte(jcaco3,jp,ibin)

    aer(ica_a,jp,ibin)  = electrolyte(jcaso4,jp,ibin)  +   &
         electrolyte(jcano3,jp,ibin)  +   &
         electrolyte(jcacl2,jp,ibin)  +   &
         electrolyte(jcaco3,jp,ibin)  +   &
         electrolyte(jcamsa2,jp,ibin)

    aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
         electrolyte(jnacl,jp,ibin)   +   &
         2.*electrolyte(jna2so4,jp,ibin) +   &
         3.*electrolyte(jna3hso4,jp,ibin)+   &
         electrolyte(jnahso4,jp,ibin) +   &
         electrolyte(jnamsa,jp,ibin)

    aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
         electrolyte(jnh4cl,jp,ibin)  +   &
         2.*electrolyte(jnh4so4,jp,ibin) +   &
         3.*electrolyte(jlvcite,jp,ibin) +   &
         electrolyte(jnh4hso4,jp,ibin)+   &
         electrolyte(jnh4msa,jp,ibin)


    return
  end subroutine electrolytes_to_ions



  !***********************************************************************
  ! combinatorial method for computing electrolytes from ions
  !
  ! notes:
  !  - to be used for liquid-phase or total-phase only
  !  - transfers caso4 and caco3 from liquid to solid phase
  !
  ! author: Rahul A. Zaveri (based on code provided by A.S. Wexler)
  ! update: apr 2005
  !-----------------------------------------------------------------------
  subroutine ions_to_electrolytes(jp,ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,    &
       nc_Mc,xeq_c,mw_electrolyte,MW_c,MW_a)

    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,nelectrolyte,ncation,    &!Parameters
         nanion,jliquid,jsolid,                                                    &!Parameters
         ica_a,iso4_a,jcaso4,imsa_a,ina_a,inh4_a,ja_hso4,ja_so4,ino3_a,ja_no3,     &
         icl_a,ja_cl,ja_msa,jc_ca,jc_na,jc_nh4,jc_h,jna2so4,jnahso4,jnamsa,jnano3, &
         jnacl,jnh4so4,jnh4hso4,jnh4msa,jnh4no3,jnh4cl,jcano3,jcacl2,jcamsa2,      &
         jh2so4,jhno3,jhcl,jmsa,jlvcite,jna3hso4                                    !TBD
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin, jp
    real(r8), intent(inout) :: XT
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(in), dimension(Nanion) :: za,MW_a
    real(r8), intent(inout), dimension(Nanion) :: xeq_a,na_Ma
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    character(len=256) :: errmsg
    integer iaer, je, jc, ja, icase
    real(r8) :: store(naer), sum_dum, sum_naza, sum_nczc, sum_na_nh4,   &
         f_nh4, f_na, xh, xb, xl, xs, cat_net, rem_nh4, rem_na
    real(r8) :: nc(ncation), na(nanion)




    if(jp .ne. jliquid)then
       call wrf_message(' jp must be jliquid')
       call wrf_message(' in ions_to_electrolytes sub')
       write(errmsg,*)' wrong jp = ', jp
       call wrf_error_fatal(trim(adjustl(errmsg)))
       
    endif

    ! remove negative concentrations, if any
    !      do iaer = 1, naer
    !        aer(iaer,jp,ibin) = max(0.0d0, aer(iaer,jp,ibin))    ! EFFI
    !      enddo


    ! first transfer caso4 from liquid to solid phase (caco3 should not be present here)
    store(ica_a)  = aer(ica_a, jp,ibin)
    store(iso4_a) = aer(iso4_a,jp,ibin)

    call form_caso4(store,jp,ibin,electrolyte)

    if(jp .eq. jliquid)then ! transfer caso4 from liquid to solid phase
       aer(ica_a,jliquid,ibin) = aer(ica_a,jliquid,ibin) -   &
            electrolyte(jcaso4,jliquid,ibin)

       aer(iso4_a,jliquid,ibin)= aer(iso4_a,jliquid,ibin)-   &
            electrolyte(jcaso4,jliquid,ibin)

       aer(ica_a,jsolid,ibin)  = aer(ica_a,jsolid,ibin) +   &
            electrolyte(jcaso4,jliquid,ibin)

       aer(iso4_a,jsolid,ibin) = aer(iso4_a,jsolid,ibin) +   &
            electrolyte(jcaso4,jliquid,ibin)

       electrolyte(jcaso4,jsolid,ibin)=electrolyte(jcaso4,jsolid,ibin)   &
            +electrolyte(jcaso4,jliquid,ibin)
       electrolyte(jcaso4,jliquid,ibin)= 0.0
    endif


    ! calculate sulfate ratio
    !      call calculate_XT(ibin,jp,XT,aer)              ! EFFI

    if( (aer(iso4_a,jp,ibin)+aer(imsa_a,jp,ibin)) .gt.0.0)then
       XT   = ( aer(inh4_a,jp,ibin) +   &
            aer(ina_a,jp,ibin)  +   &
            2.*aer(ica_a,jp,ibin) )/   &
            (aer(iso4_a,jp,ibin)+0.5*aer(imsa_a,jp,ibin))
    else
       XT   = -1.0
    endif




!    if(XT.ge.1.9999 .or. XT.lt.0.)then ! commented out by RAZ 4/16/2014
    if(XT.ge.2.0 .or. XT.lt.0.)then     ! Slightly different logic, consistent with that in compute_activities subr. RAZ 4/16/2014
       icase = 1  ! sulfate poor: near neutral (acidity is caused by HCl and/or HNO3)
    else
       icase = 2  ! sulfate rich: acidic (acidity is caused by excess SO4)
    endif


    ! initialize to zero
    do je = 1, nelectrolyte
       electrolyte(je,jp,ibin) = 0.0
    enddo

    !
    !---------------------------------------------------------
    ! initialize moles of ions depending on the sulfate domain

    if(icase.eq.1)then ! XT >= 2 or XT < 0: SULFATE POOR (OR NO SULFATE) DOMAIN. RAZ 4/16/2014

       na(ja_hso4)= 0.0
       na(ja_so4) = aer(iso4_a,jp,ibin)
       na(ja_no3) = aer(ino3_a,jp,ibin)
       na(ja_cl)  = aer(icl_a, jp,ibin)
       na(ja_msa) = aer(imsa_a,jp,ibin)

       nc(jc_ca)  = aer(ica_a, jp,ibin)
       nc(jc_na)  = aer(ina_a, jp,ibin)
       nc(jc_nh4) = aer(inh4_a,jp,ibin)

       cat_net = (   &
            (2.*na(ja_so4)+na(ja_no3)+na(ja_cl)+na(ja_msa)) -   &
            (2.*nc(jc_ca) +nc(jc_nh4)+nc(jc_na)) )

       if(cat_net .lt. 0.0)then

          nc(jc_h) = 0.0

       else  ! cat_net must be 0.0 or positive

          nc(jc_h) = cat_net

       endif


       ! now compute equivalent fractions
       sum_naza = 0.0
       do ja = 1, nanion
          sum_naza = sum_naza + na(ja)*za(ja)
       enddo

       sum_nczc = 0.0
       do jc = 1, ncation
          sum_nczc = sum_nczc + nc(jc)*zc(jc)
       enddo

       if(sum_naza .eq. 0. .or. sum_nczc .eq. 0.)then ! it's ok. this may happen if the aerosol is assumed to be composed of hygroscopic SOA, POA, BC, OIN, but does not contain any inorganic electrolytes
!          write(6,*)'ionic concentrations are zero in ibin', ibin  ! commented out by RAZ 4/16/2014
!          write(6,*)'sum_naza = ', sum_naza          ! commented out by RAZ 4/16/2014
!          write(6,*)'sum_nczc = ', sum_nczc          ! commented out by RAZ 4/16/2014
          return
       endif

       do ja = 1, nanion
          xeq_a(ja) = na(ja)*za(ja)/sum_naza
       enddo

       do jc = 1, ncation
          xeq_c(jc) = nc(jc)*zc(jc)/sum_nczc
       enddo

       na_Ma(ja_so4) = na(ja_so4) *MW_a(ja_so4)
       na_Ma(ja_no3) = na(ja_no3) *MW_a(ja_no3)
       na_Ma(ja_cl)  = na(ja_cl)  *MW_a(ja_cl)
       na_Ma(ja_msa) = na(ja_msa) *MW_a(ja_msa)
       na_Ma(ja_hso4)= na(ja_hso4)*MW_a(ja_hso4)

       nc_Mc(jc_ca)  = nc(jc_ca) *MW_c(jc_ca)
       nc_Mc(jc_na)  = nc(jc_na) *MW_c(jc_na)
       nc_Mc(jc_nh4) = nc(jc_nh4)*MW_c(jc_nh4)
       nc_Mc(jc_h)   = nc(jc_h)  *MW_c(jc_h)


       ! now compute electrolyte moles
       if(xeq_c(jc_na) .gt. 0. .and. xeq_a(ja_so4) .gt. 0.)then
          electrolyte(jna2so4,jp,ibin) = (xeq_c(jc_na) *na_Ma(ja_so4) +   &
               xeq_a(ja_so4)*nc_Mc(jc_na))/   &
               mw_electrolyte(jna2so4)
       endif

       electrolyte(jnahso4,jp,ibin) = 0.0

       if(xeq_c(jc_na) .gt. 0. .and. xeq_a(ja_msa) .gt. 0.)then
          electrolyte(jnamsa,jp,ibin)  = (xeq_c(jc_na) *na_Ma(ja_msa) +   &
               xeq_a(ja_msa)*nc_Mc(jc_na))/   &
               mw_electrolyte(jnamsa)
       endif

       if(xeq_c(jc_na) .gt. 0. .and. xeq_a(ja_no3) .gt. 0.)then
          electrolyte(jnano3,jp,ibin)  = (xeq_c(jc_na) *na_Ma(ja_no3) +   &
               xeq_a(ja_no3)*nc_Mc(jc_na))/   &
               mw_electrolyte(jnano3)
       endif

       if(xeq_c(jc_na) .gt. 0. .and. xeq_a(ja_cl) .gt. 0.)then
          electrolyte(jnacl,jp,ibin)   = (xeq_c(jc_na) *na_Ma(ja_cl) +   &
               xeq_a(ja_cl) *nc_Mc(jc_na))/   &
               mw_electrolyte(jnacl)
       endif

       if(xeq_c(jc_nh4) .gt. 0. .and. xeq_a(ja_so4) .gt. 0.)then
          electrolyte(jnh4so4,jp,ibin) = (xeq_c(jc_nh4)*na_Ma(ja_so4) +   &
               xeq_a(ja_so4)*nc_Mc(jc_nh4))/   &
               mw_electrolyte(jnh4so4)
       endif

       electrolyte(jnh4hso4,jp,ibin)= 0.0

       if(xeq_c(jc_nh4) .gt. 0. .and. xeq_a(ja_msa) .gt. 0.)then
          electrolyte(jnh4msa,jp,ibin) = (xeq_c(jc_nh4)*na_Ma(ja_msa) +   &
               xeq_a(ja_msa)*nc_Mc(jc_nh4))/   &
               mw_electrolyte(jnh4msa)
       endif

       if(xeq_c(jc_nh4) .gt. 0. .and. xeq_a(ja_no3) .gt. 0.)then
          electrolyte(jnh4no3,jp,ibin) = (xeq_c(jc_nh4)*na_Ma(ja_no3) +   &
               xeq_a(ja_no3)*nc_Mc(jc_nh4))/   &
               mw_electrolyte(jnh4no3)
       endif

       if(xeq_c(jc_nh4) .gt. 0. .and. xeq_a(ja_cl) .gt. 0.)then
          electrolyte(jnh4cl,jp,ibin)  = (xeq_c(jc_nh4)*na_Ma(ja_cl) +   &
               xeq_a(ja_cl) *nc_Mc(jc_nh4))/   &
               mw_electrolyte(jnh4cl)
       endif

       if(xeq_c(jc_ca) .gt. 0. .and. xeq_a(ja_no3) .gt. 0.0)then
          electrolyte(jcano3, jp,ibin) = (xeq_c(jc_ca) *na_Ma(ja_no3) +   &
               xeq_a(ja_no3)*nc_Mc(jc_ca))/   &
               mw_electrolyte(jcano3)
       endif

       if(xeq_c(jc_ca) .gt. 0. .and. xeq_a(ja_cl) .gt. 0.)then
          electrolyte(jcacl2,jp,ibin)  = (xeq_c(jc_ca) *na_Ma(ja_cl) +   &
               xeq_a(ja_cl) *nc_Mc(jc_ca))/   &
               mw_electrolyte(jcacl2)
       endif

       if(xeq_c(jc_ca) .gt. 0. .and. xeq_a(ja_msa) .gt. 0.)then
          electrolyte(jcamsa2,jp,ibin) = (xeq_c(jc_ca) *na_Ma(ja_msa) +   &
               xeq_a(ja_msa) *nc_Mc(jc_ca))/   &
               mw_electrolyte(jcamsa2)
       endif

       electrolyte(jh2so4, jp,ibin) = 0.0

       if(xeq_c(jc_h) .gt. 0. .and. xeq_a(ja_no3) .gt. 0.)then
          electrolyte(jhno3,jp,ibin)     = (xeq_c(jc_h)  *na_Ma(ja_no3) +   &
               xeq_a(ja_no3)*nc_Mc(jc_h))/   &
               mw_electrolyte(jhno3)
       endif

       if(xeq_c(jc_h) .gt. 0. .and. xeq_a(ja_cl) .gt. 0.)then
          electrolyte(jhcl,jp,ibin)    = (xeq_c(jc_h) *na_Ma(ja_cl) +   &
               xeq_a(ja_cl)*nc_Mc(jc_h))/   &
               mw_electrolyte(jhcl)
       endif

       if(xeq_c(jc_h) .gt. 0. .and. xeq_a(ja_msa) .gt. 0.)then
          electrolyte(jmsa,jp,ibin)    = (xeq_c(jc_h) *na_Ma(ja_msa) +   &
               xeq_a(ja_msa)*nc_Mc(jc_h))/   &
               mw_electrolyte(jmsa)
       endif

       !--------------------------------------------------------------------

    elseif(icase.eq.2)then ! XT < 2 : SULFATE RICH DOMAIN

       store(imsa_a) = aer(imsa_a,jp,ibin)
       store(ica_a)  = aer(ica_a, jp,ibin)

       call form_camsa2(store,jp,ibin,electrolyte)

       sum_na_nh4 = aer(ina_a,jp,ibin) + aer(inh4_a,jp,ibin)

       if(sum_na_nh4 .gt. 0.0)then
          f_na  = aer(ina_a,jp,ibin)/sum_na_nh4
          f_nh4 = aer(inh4_a,jp,ibin)/sum_na_nh4
       else
          f_na  = 0.0
          f_nh4 = 0.0
       endif

       ! first form msa electrolytes
       if(sum_na_nh4 .gt. store(imsa_a))then
          electrolyte(jnamsa,jp,ibin)  = f_na *store(imsa_a)
          electrolyte(jnh4msa,jp,ibin) = f_nh4*store(imsa_a)
          rem_na = max(0.0_r8, aer(ina_a,jp,ibin) - electrolyte(jnamsa,jp,ibin))  ! remaining na  RAZ 4/16/2014
          rem_nh4= max(0.0_r8, aer(inh4_a,jp,ibin)- electrolyte(jnh4msa,jp,ibin)) ! remaining nh4 RAZ 4/16/2014
       else
          electrolyte(jnamsa,jp,ibin)  = aer(ina_a,jp,ibin)
          electrolyte(jnh4msa,jp,ibin) = aer(inh4_a,jp,ibin)
          electrolyte(jmsa,jp,ibin)    = max(0.0_r8, store(imsa_a) - sum_na_nh4) ! RAZ 4/16/2014
          rem_nh4 = 0.0  ! remaining nh4
          rem_na  = 0.0  ! remaining na
       endif


       ! recompute XT
       if(aer(iso4_a,jp,ibin).gt.0.0)then
          XT = (rem_nh4 + rem_na)/aer(iso4_a,jp,ibin)
       else
          goto 10
       endif

       if(XT .le. 1.0)then            ! h2so4 + bisulfate
          xh = max(0.0_r8, (1.0_r8 - XT))   ! RAZ 4/16/2014
          xb = XT
          electrolyte(jh2so4,jp,ibin)   = xh*aer(iso4_a,jp,ibin)
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
       elseif(XT .le. 1.5)then    ! bisulfate + letovicite
          xb = max(0.0_r8, 3.0_r8 - 2.0_r8*XT) ! RAZ 4/16/2014
          xl = max(0.0_r8, XT - 1.0_r8)     ! RAZ 4/16/2014
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
       else                       ! letovicite + sulfate
          xl = max(0.0_r8, 2.0_r8 - XT)     ! RAZ 4/16/2014
          xs = max(0.0_r8, 2.0_r8*XT - 3.0_r8) ! RAZ 4/16/2014
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jnh4so4,jp,ibin)  = xs*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna2so4,jp,ibin)  = xs*f_na *aer(iso4_a,jp,ibin)
       endif

       electrolyte(jhno3,jp,ibin) = aer(ino3_a,jp,ibin)
       electrolyte(jhcl,jp,ibin)  = aer(icl_a,jp,ibin)

    endif
    !---------------------------------------------------------
    !
    ! calculate % composition  EFFI
10  sum_dum = 0.0
    !!      do je = 1, nelectrolyte
    !!        sum_dum = sum_dum + electrolyte(je,jp,ibin)
    !!      enddo
    !!
    !!      if(sum_dum .eq. 0.)sum_dum = 1.0
    !!      electrolyte_sum(jp,ibin) = sum_dum
    !!
    !!      do je = 1, nelectrolyte
    !!        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/sum_dum
    !!      enddo
    !!

    return
  end subroutine ions_to_electrolytes



  !***********************************************************************
  ! part of MESA: calculates liquid electrolytes from ions
  !
  ! notes:
  !  - this subroutine is to be used for liquid-phase or total-phase only
  !  - this sub transfers caso4 and caco3 from liquid to solid phase
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine MESA_estimate_eleliquid(ibin,XT,aer,electrolyte,zc,za,xeq_a,na_Ma,    &
       nc_Mc,xeq_c,mw_electrolyte,MW_c,MW_a,eleliquid)    ! TOUCH
    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,nelectrolyte,ncation,    &!Parameters
         nanion,jliquid,                                                           &!Parameters
         jh2so4,jhno3,jhcl,jmsa,jlvcite,jnh4no3,jnh4cl,jcamsa2,jcano3,jcacl2,      &
         jnano3,jnacl,jnh4so4,jnh4hso4,jnh4msa,jna2so4,jnahso4,jnamsa,iso4_a,      &
         ja_so4,ja_no3,ja_cl,imsa_a,ja_msa,jc_ca,ina_a,jc_na,inh4_a,jc_nh4,jc_h,   &
         ica_a,ino3_a,icl_a,ja_hso4

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout) :: XT
    real(r8), intent(in), dimension(Ncation) :: zc,MW_c
    real(r8), intent(inout), dimension(Ncation) :: nc_Mc,xeq_c
    real(r8), intent(in), dimension(Nanion) :: za,MW_a
    real(r8), intent(inout), dimension(Nanion) :: xeq_a,na_Ma
    real(r8), intent(in), dimension(nelectrolyte) :: mw_electrolyte
    real(r8), intent(inout), dimension(nelectrolyte) :: eleliquid
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer iaer, je, jc, ja, icase, jp
    real(r8) :: store(naer), sum_dum, sum_naza, sum_nczc, sum_na_nh4,   &
         f_nh4, f_na, xh, xb, xl, xs, XT_d, XNa_d, XNH4_d,   &
         xdum, dum, cat_net
    real(r8) :: nc(ncation), na(nanion)
    real(r8) :: dum_ca, dum_no3, dum_cl, cano3, cacl2

    !nc(:) = 0.0_r8!BSINGH - initialized to zero

    ! remove negative concentrations, if any
    do iaer =  1, naer
       aer(iaer,jliquid,ibin) = max(0.0d0, aer(iaer,jliquid,ibin))
    enddo


    ! calculate sulfate ratio
    call calculate_XT(ibin,jliquid,XT,aer)

    if(XT .ge. 2.0 .or. XT.lt.0.)then
       icase = 1 ! near neutral (acidity is caused by HCl and/or HNO3)
    else
       icase = 2 ! acidic (acidity is caused by excess SO4)
    endif


    ! initialize to zero
    do je = 1, nelectrolyte
       eleliquid(je) = 0.0
    enddo

    !
    !---------------------------------------------------------
    ! initialize moles of ions depending on the sulfate domain

    jp = jliquid

    if(icase.eq.1)then ! XT >= 2 : SULFATE POOR DOMAIN

       dum_ca  = aer(ica_a,jp,ibin)
       dum_no3 = aer(ino3_a,jp,ibin)
       dum_cl  = aer(icl_a,jp,ibin)

       cano3   = min(dum_ca, 0.5*dum_no3)
       dum_ca  = max(0.d0, dum_ca - cano3)
       dum_no3 = max(0.d0, dum_no3 - 2.*cano3)

       cacl2   = min(dum_ca, 0.5*dum_cl)
       dum_ca  = max(0.d0, dum_ca - cacl2)
       dum_cl  = max(0.d0, dum_cl - 2.*cacl2)

       na(ja_hso4)= 0.0
       na(ja_so4) = aer(iso4_a,jp,ibin)
       na(ja_no3) = aer(ino3_a,jp,ibin)
       na(ja_cl)  = aer(icl_a, jp,ibin)
       na(ja_msa) = aer(imsa_a,jp,ibin)

       nc(jc_ca)  = aer(ica_a, jp,ibin)
       nc(jc_na)  = aer(ina_a, jp,ibin)
       nc(jc_nh4) = aer(inh4_a,jp,ibin)

       cat_net = (   &
            (2.*na(ja_so4)+na(ja_no3)+na(ja_cl)+na(ja_msa)) -   &
            (2.*nc(jc_ca) +nc(jc_nh4)+nc(jc_na)) )   ! RAZ 11/11/2014: bug fix. remove nc(jc_h)

       if(cat_net .lt. 0.0)then

          nc(jc_h) = 0.0

       else  ! cat_net must be 0.0 or positive

          nc(jc_h) = cat_net

       endif


       ! now compute equivalent fractions
       sum_naza = 0.0
       do ja = 1, nanion
          sum_naza = sum_naza + na(ja)*za(ja)
       enddo

       sum_nczc = 0.0
       do jc = 1, ncation
          sum_nczc = sum_nczc + nc(jc)*zc(jc)
       enddo

       if(sum_naza .eq. 0. .or. sum_nczc .eq. 0.)then
          write(6,*)'ionic concentrations are zero in ibin', ibin
          write(6,*)'sum_naza = ', sum_naza
          write(6,*)'sum_nczc = ', sum_nczc
          return
       endif

       do ja = 1, nanion
          xeq_a(ja) = na(ja)*za(ja)/sum_naza
       enddo

       do jc = 1, ncation
          xeq_c(jc) = nc(jc)*zc(jc)/sum_nczc
       enddo

       na_Ma(ja_so4) = na(ja_so4) *MW_a(ja_so4)
       na_Ma(ja_no3) = na(ja_no3) *MW_a(ja_no3)
       na_Ma(ja_cl)  = na(ja_cl)  *MW_a(ja_cl)
       na_Ma(ja_hso4)= na(ja_hso4)*MW_a(ja_hso4)
       na_Ma(ja_msa) = na(ja_msa) *MW_a(ja_msa)

       nc_Mc(jc_ca)  = nc(jc_ca) *MW_c(jc_ca)
       nc_Mc(jc_na)  = nc(jc_na) *MW_c(jc_na)
       nc_Mc(jc_nh4) = nc(jc_nh4)*MW_c(jc_nh4)
       nc_Mc(jc_h)   = nc(jc_h)  *MW_c(jc_h)


       ! now compute electrolyte moles
       eleliquid(jna2so4) = (xeq_c(jc_na) *na_Ma(ja_so4) +   &
            xeq_a(ja_so4)*nc_Mc(jc_na))/   &
            mw_electrolyte(jna2so4)

       eleliquid(jnahso4) = (xeq_c(jc_na) *na_Ma(ja_hso4) +   &
            xeq_a(ja_hso4)*nc_Mc(jc_na))/   &
            mw_electrolyte(jnahso4)

       eleliquid(jnamsa)  = (xeq_c(jc_na) *na_Ma(ja_msa) +   &
            xeq_a(ja_msa)*nc_Mc(jc_na))/   &
            mw_electrolyte(jnamsa)

       eleliquid(jnano3)  = (xeq_c(jc_na) *na_Ma(ja_no3) +   &
            xeq_a(ja_no3)*nc_Mc(jc_na))/   &
            mw_electrolyte(jnano3)

       eleliquid(jnacl)   = (xeq_c(jc_na) *na_Ma(ja_cl) +   &
            xeq_a(ja_cl) *nc_Mc(jc_na))/   &
            mw_electrolyte(jnacl)

       eleliquid(jnh4so4) = (xeq_c(jc_nh4)*na_Ma(ja_so4) +   &
            xeq_a(ja_so4)*nc_Mc(jc_nh4))/   &
            mw_electrolyte(jnh4so4)

       eleliquid(jnh4hso4)= (xeq_c(jc_nh4)*na_Ma(ja_hso4) +   &
            xeq_a(ja_hso4)*nc_Mc(jc_nh4))/   &
            mw_electrolyte(jnh4hso4)

       eleliquid(jnh4msa) = (xeq_c(jc_nh4) *na_Ma(ja_msa) +   &
            xeq_a(ja_msa)*nc_Mc(jc_nh4))/   &
            mw_electrolyte(jnh4msa)

       eleliquid(jnh4no3) = (xeq_c(jc_nh4)*na_Ma(ja_no3) +   &
            xeq_a(ja_no3)*nc_Mc(jc_nh4))/   &
            mw_electrolyte(jnh4no3)

       eleliquid(jnh4cl)  = (xeq_c(jc_nh4)*na_Ma(ja_cl) +   &
            xeq_a(ja_cl) *nc_Mc(jc_nh4))/   &
            mw_electrolyte(jnh4cl)

       eleliquid(jcamsa2) = (xeq_c(jc_ca) *na_Ma(ja_msa) +   &
            xeq_a(ja_msa)*nc_Mc(jc_ca))/   &
            mw_electrolyte(jcamsa2)

       eleliquid(jcano3)  = (xeq_c(jc_ca) *na_Ma(ja_no3) +   &
            xeq_a(ja_no3)*nc_Mc(jc_ca))/   &
            mw_electrolyte(jcano3)

       eleliquid(jcacl2)  = (xeq_c(jc_ca) *na_Ma(ja_cl) +   &
            xeq_a(ja_cl) *nc_Mc(jc_ca))/   &
            mw_electrolyte(jcacl2)

       eleliquid(jh2so4)  = (xeq_c(jc_h)   *na_Ma(ja_hso4) +   &
            xeq_a(ja_hso4)*nc_Mc(jc_h))/   &
            mw_electrolyte(jh2so4)

       eleliquid(jhno3)   = (xeq_c(jc_h)  *na_Ma(ja_no3) +   &
            xeq_a(ja_no3)*nc_Mc(jc_h))/   &
            mw_electrolyte(jhno3)

       eleliquid(jhcl)    = (xeq_c(jc_h) *na_Ma(ja_cl) +   &
            xeq_a(ja_cl)*nc_Mc(jc_h))/   &
            mw_electrolyte(jhcl)

       eleliquid(jmsa)    = (xeq_c(jc_h)  *na_Ma(ja_msa) +   &
            xeq_a(ja_msa)*nc_Mc(jc_h))/   &
            mw_electrolyte(jmsa)

       !--------------------------------------------------------------------

    elseif(icase.eq.2)then ! XT < 2 : SULFATE RICH DOMAIN

       jp = jliquid

       store(iso4_a) = aer(iso4_a,jp,ibin)
       store(imsa_a) = aer(imsa_a,jp,ibin)
       store(inh4_a) = aer(inh4_a,jp,ibin)
       store(ina_a)  = aer(ina_a, jp,ibin)
       store(ica_a)  = aer(ica_a, jp,ibin)

       call form_camsa2(store,jp,ibin,electrolyte)

       sum_na_nh4 = store(ina_a) + store(inh4_a)
       if(sum_na_nh4 .gt. 0.0)then
          f_nh4 = store(inh4_a)/sum_na_nh4
          f_na  = store(ina_a)/sum_na_nh4
       else
          f_nh4 = 0.0
          f_na  = 0.0
       endif

       ! first form msa electrolytes
       if(sum_na_nh4 .gt. store(imsa_a))then
          eleliquid(jnh4msa) = f_nh4*store(imsa_a)
          eleliquid(jnamsa)  = f_na *store(imsa_a)
          store(inh4_a)= store(inh4_a)-eleliquid(jnh4msa) ! remaining nh4
          store(ina_a) = store(ina_a) -eleliquid(jnamsa)  ! remaining na
       else
          eleliquid(jnh4msa) = store(inh4_a)
          eleliquid(jnamsa)  = store(ina_a)
          eleliquid(jmsa)    = store(imsa_a) - sum_na_nh4
          store(inh4_a)= 0.0  ! remaining nh4
          store(ina_a) = 0.0  ! remaining na
       endif

       if(store(iso4_a).eq.0.0)goto 10

       XT_d  = XT
       XNa_d = 1. + 0.5*store(ina_a)/store(iso4_a)
       xdum = store(iso4_a) - store(inh4_a)

       dum = ( (2.*store(iso4_a)) -   &
            (store(ina_a)) )
       if(store(inh4_a) .gt. 0.0 .and. dum .gt. 0.0)then
          XNH4_d = 2.*store(inh4_a)/   &
               (2.*store(iso4_a) - store(ina_a))
       else
          XNH4_d = 0.0
       endif


       IF(store(inh4_a) .gt. 0.0)THEN
          if(XT_d .ge. XNa_d)then
             eleliquid(jna2so4) = 0.5*store(ina_a)

             if(XNH4_d .ge. 5./3.)then
                eleliquid(jnh4so4) = 1.5*store(ina_a)   &
                     - 3.*xdum - store(inh4_a)
                eleliquid(jlvcite) = 2.*xdum + store(inh4_a)   &
                     - store(ina_a)
             elseif(XNH4_d .ge. 1.5)then
                eleliquid(jnh4so4) = store(inh4_a)/5.
                eleliquid(jlvcite) = store(inh4_a)/5.
             elseif(XNH4_d .ge. 1.0)then
                eleliquid(jnh4so4) = store(inh4_a)/6.
                eleliquid(jlvcite) = store(inh4_a)/6.
                eleliquid(jnh4hso4)= store(inh4_a)/6.
             endif

          elseif(XT_d .gt. 1.0)then
             eleliquid(jnh4so4)  = store(inh4_a)/6.
             eleliquid(jlvcite)  = store(inh4_a)/6.
             eleliquid(jnh4hso4) = store(inh4_a)/6.
             eleliquid(jna2so4)  = store(ina_a)/3.
             eleliquid(jnahso4)  = store(ina_a)/3.
          elseif(XT_d .le. 1.0)then
             eleliquid(jna2so4)  = store(ina_a)/4.
             eleliquid(jnahso4)  = store(ina_a)/2.
             eleliquid(jlvcite)  = store(inh4_a)/6.
             eleliquid(jnh4hso4) = store(inh4_a)/2.
          endif

       ELSE

          if(XT_d .gt. 1.0)then
             eleliquid(jna2so4) = store(ina_a) - store(iso4_a)
             eleliquid(jnahso4) = 2.*store(iso4_a) -   &
                  store(ina_a)
          else
             eleliquid(jna2so4) = store(ina_a)/4.
             eleliquid(jnahso4) = store(ina_a)/2.
          endif


       ENDIF



    endif
    !---------------------------------------------------------


10  return
  end subroutine MESA_estimate_eleliquid



  !***********************************************************************
  ! part of MESA: completely dissolves small amounts of soluble salts
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine MESA_dissolve_small_salt(ibin,js,aer,electrolyte)

    use module_data_mosaic_aero, only:r8,naer,nbin_a_max,nelectrolyte,jsolid,      &!Parameters
         jliquid,                                                                  &!Parameters
         jh2so4,jhno3,jhcl,jlvcite,jnh4no3,jnh4cl,jcamsa2,jcano3,jcacl2,jnano3,    &!TBD
         jnacl,jnh4so4,jnh4hso4,jnh4msa,jna2so4,jnahso4,jnamsa,iso4_a,ina_a,       &!TBD
         inh4_a,jna3hso4,jcaso4,jcaco3,ica_a,ino3_a,icl_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin, js
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    !Local variables
    integer :: jp

    jp = jsolid


    if(js .eq. jnh4so4)then
       aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            2.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jnh4msa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jlvcite)then
       aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
            3.*electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            2.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jnh4msa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jnh4hso4)then
       aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            2.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jnh4msa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jna2so4)then
       aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
            electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jna2so4,jp,ibin) +   &
            3.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnamsa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jna3hso4)then
       aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
            3.*electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
            electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jna2so4,jp,ibin) +   &
            3.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnamsa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jnahso4)then
       aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
            electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jna2so4,jp,ibin) +   &
            3.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnamsa,jp,ibin)

       aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jna2so4,jp,ibin) +   &
            2.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnh4so4,jp,ibin) +   &
            2.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jh2so4,jp,ibin)
       return
    endif


    if(js .eq. jnh4no3)then
       aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            2.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jnh4msa,jp,ibin)

       aer(ino3_a,jp,ibin) = electrolyte(jnano3,jp,ibin)  +   &
            2.*electrolyte(jcano3,jp,ibin)  +   &
            electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jhno3,jp,ibin)
       return
    endif


    if(js .eq. jnh4cl)then
       aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            2.*electrolyte(jnh4so4,jp,ibin) +   &
            3.*electrolyte(jlvcite,jp,ibin) +   &
            electrolyte(jnh4hso4,jp,ibin)+   &
            electrolyte(jnh4msa,jp,ibin)

       aer(icl_a,jp,ibin)  = electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jcacl2,jp,ibin)  +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            electrolyte(jhcl,jp,ibin)
       return
    endif


    if(js .eq. jnano3)then
       aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
            electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jna2so4,jp,ibin) +   &
            3.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnamsa,jp,ibin)

       aer(ino3_a,jp,ibin) = electrolyte(jnano3,jp,ibin)  +   &
            2.*electrolyte(jcano3,jp,ibin)  +   &
            electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jhno3,jp,ibin)
       return
    endif


    if(js .eq. jnacl)then
       aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
            electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jna2so4,jp,ibin) +   &
            3.*electrolyte(jna3hso4,jp,ibin)+   &
            electrolyte(jnahso4,jp,ibin) +   &
            electrolyte(jnamsa,jp,ibin)

       aer(icl_a,jp,ibin)  = electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jcacl2,jp,ibin)  +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            electrolyte(jhcl,jp,ibin)
       return
    endif


    if(js .eq. jcano3)then
       aer(ica_a,jliquid,ibin)  = aer(ica_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ica_a,jp,ibin)  = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jcano3,jp,ibin)  +   &
            electrolyte(jcacl2,jp,ibin)  +   &
            electrolyte(jcaco3,jp,ibin)  +   &
            electrolyte(jcamsa2,jp,ibin)

       aer(ino3_a,jp,ibin) = electrolyte(jnano3,jp,ibin)  +   &
            2.*electrolyte(jcano3,jp,ibin)  +   &
            electrolyte(jnh4no3,jp,ibin) +   &
            electrolyte(jhno3,jp,ibin)
       return
    endif


    if(js .eq. jcacl2)then
       aer(ica_a,jliquid,ibin) = aer(ica_a,jliquid,ibin) +   &
            electrolyte(js,jsolid,ibin)
       aer(icl_a,jliquid,ibin) = aer(icl_a,jliquid,ibin) +   &
            2.*electrolyte(js,jsolid,ibin)

       electrolyte(js,jsolid,ibin) = 0.0

       aer(ica_a,jp,ibin)  = electrolyte(jcaso4,jp,ibin)  +   &
            electrolyte(jcano3,jp,ibin)  +   &
            electrolyte(jcacl2,jp,ibin)  +   &
            electrolyte(jcaco3,jp,ibin)  +   &
            electrolyte(jcamsa2,jp,ibin)

       aer(icl_a,jp,ibin)  = electrolyte(jnacl,jp,ibin)   +   &
            2.*electrolyte(jcacl2,jp,ibin)  +   &
            electrolyte(jnh4cl,jp,ibin)  +   &
            electrolyte(jhcl,jp,ibin)
       return
    endif

    return
  end subroutine MESA_dissolve_small_salt



  !***********************************************************************
  ! electrolyte formation subroutines
  !
  ! author: Rahul A. Zaveri
  ! update: june 2000
  !-----------------------------------------------------------------------
  subroutine form_caso4(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,ica_a,jcaso4

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jcaso4,jp,ibin) = min(store(ica_a),store(iso4_a))
    store(ica_a)  = ( (store(ica_a)) -   &
         (electrolyte(jcaso4,jp,ibin)) )
    store(iso4_a) = ( (store(iso4_a)) -   &
         (electrolyte(jcaso4,jp,ibin)) )
    store(ica_a)  = max(0.d0, store(ica_a))
    store(iso4_a) = max(0.d0, store(iso4_a))

    return
  end subroutine form_caso4



  subroutine form_camsa2(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         imsa_a,ica_a,jcamsa2
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jcamsa2,jp,ibin) = min(store(ica_a),0.5*store(imsa_a))
    store(ica_a)  = ( (store(ica_a)) -   &
         (electrolyte(jcamsa2,jp,ibin)) )
    store(imsa_a) = ( (store(imsa_a)) -   &
         (2.*electrolyte(jcamsa2,jp,ibin)) )
    store(ica_a)  = max(0.d0, store(ica_a))
    store(imsa_a) = max(0.d0, store(imsa_a))

    return
  end subroutine form_camsa2



  !***********************************************************************
  ! computes mass transfer coefficients for each condensing species for
  ! all the aerosol bins
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine aerosolmtc( jaerosolstate, num_a, Dp_wet_a, sigmag_a, P_atm, T_K, kg )
    
    use module_data_mosaic_aero,  only: r8, nbin_a_max, nbin_a, naer, naercomp,             &!Parameters
         ngas_aerchtot, ngas_volatile, nelectrolyte, ngas_ioa,                              &
         mMODAL, no_aerosol, mUNSTRUCTURED, mSECTIONAL, mSIZE_FRAMEWORK,                    &!Input
         isoa_first, mw_gas, v_molar_gas,                                                   &!TBD
         i_gas2bin_uptk_flag, m_gas2bin_uptk_flag,                                          &
         use_cam5mam_accom_coefs, mosaic_vars_aa_type


    implicit none
    
    !Subroutine Arguments
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate

    real(r8), intent(in) :: P_atm,T_K
    real(r8), intent(in), dimension(nbin_a_max) :: num_a
    real(r8), intent(in), dimension(nbin_a_max) :: sigmag_a
    real(r8), intent(inout), dimension(nbin_a_max) :: Dp_wet_a
    real(r8), intent(inout), dimension(ngas_aerchtot,nbin_a_max) :: kg

    ! local variables
    integer nghq
    parameter (nghq = 2)         ! gauss-hermite quadrature order
    integer ibin, iq, iv
    real(r8) :: tworootpi, root2, beta
    parameter (tworootpi = 3.5449077, root2 = 1.4142135, beta = 2.0)
    real(r8) :: cdum, Dp, Dp_avg, Fkn, Kn, lnsg, lnDpgn, lnDp, speed,   &
                sumghq, tmpa
    real(r8) :: xghq(nghq), wghq(nghq)                           ! quadrature abscissae and weights
    real(r8) :: accom(ngas_aerchtot)                             ! keep local
    real(r8) :: freepath(ngas_aerchtot)                          ! keep local
    real(r8) :: Dg(ngas_aerchtot)                                ! keep local
    !real(r8) :: fuchs_sutugin                                   ! mosaic func
    !real(r8) :: gas_diffusivity                                 ! mosaic func
    !real(r8) :: mean_molecular_speed                            ! mosaic func

    ! mass accommodation coefficients
    tmpa = 0.1
    if ( use_cam5mam_accom_coefs > 0 ) tmpa = 0.65
    accom(1:ngas_aerchtot) = tmpa  ! default
!   accom(ih2so4_g)  = tmpa
!   accom(ihno3_g)   = tmpa
!   accom(ihcl_g)    = tmpa
!   accom(inh3_g)    = tmpa
!   accom(imsa_g)    = tmpa
!   accom(iaro1_g)   = tmpa
!   accom(iaro2_g)   = tmpa
!   accom(ialk1_g)   = tmpa
!   accom(iole1_g)   = tmpa
!   accom(iapi1_g)   = tmpa
!   accom(iapi2_g)   = tmpa
!   accom(ilim1_g)   = tmpa
!   accom(ilim2_g)   = tmpa

    ! quadrature weights
    xghq(1) =  0.70710678
    xghq(2) = -0.70710678
    wghq(1) =  0.88622693
    wghq(2) =  0.88622693


    ! calculate gas diffusivity and mean free path for condensing gases
    ! ioa
    do iv = 1, ngas_ioa
       speed  = mean_molecular_speed(T_K,mw_gas(iv))     ! cm/s
       Dg(iv) = gas_diffusivity(T_K,P_atm,mw_gas(iv),v_molar_gas(iv)) ! cm^2/s
       freepath(iv) = 3.*Dg(iv)/speed                    ! cm
    enddo

    ! soa
    do iv = isoa_first, ngas_volatile
       speed = mean_molecular_speed(T_K,mw_gas(iv))      ! cm/s
       Dg(iv) = 0.1                                      ! cm^2/s
       freepath(iv) = 3.*Dg(iv)/speed
    enddo

! het-rct gases
    do iv = (ngas_volatile+1), ngas_aerchtot
       speed = mean_molecular_speed(t_k,mw_gas(iv))    ! cm/s
       dg(iv) = gas_diffusivity(t_k,p_atm,mw_gas(iv),v_molar_gas(iv)) ! cm^2/s
       freepath(iv) = 3.*dg(iv)/speed                  ! cm
    enddo


    ! calc mass transfer coefficients for gases over various aerosol bins

    if (mSIZE_FRAMEWORK .eq. mMODAL) then

       ! for modal approach
       do 10 ibin = 1, nbin_a

          if(jaerosolstate(ibin) .eq. no_aerosol)goto 10

          lnsg   = log(sigmag_a(ibin))

          ! following 2 lines were incorrect as Dp_wet_a is wet "average" Dp
          !       Dpgn_a(ibin) = Dp_wet_a(ibin)  ! cm
          !       lnDpgn = log(Dpgn_a(ibin))
          ! do this instead which gives
          ! lnDpgn = ln( wet geometric-mean Dp of number distribution )
          lnDpgn = log(Dp_wet_a(ibin)) - 1.5*lnsg*lnsg

          cdum   = tworootpi*num_a(ibin)*   &
               exp(beta*lnDpgn + 0.5*(beta*lnsg)**2)

          do 20 iv = 1, ngas_aerchtot

             sumghq = 0.0_r8
             do 30 iq = 1, nghq  ! sum over gauss-hermite quadrature points
                lnDp = lnDpgn + beta*lnsg**2 + root2*lnsg*xghq(iq)
                Dp = exp(lnDp)
                Kn = 2.*freepath(iv)/Dp
                Fkn = fuchs_sutugin(Kn,accom(iv))
                sumghq = sumghq + wghq(iq)*Dp*Fkn/(Dp**beta)
30              continue

                kg(iv,ibin) = cdum*Dg(iv)*sumghq         ! 1/s

20              continue
10     continue
                
    elseif ((mSIZE_FRAMEWORK .eq. mSECTIONAL   ) .or. &
         (mSIZE_FRAMEWORK .eq. mUNSTRUCTURED)) then
       
       ! for sectional approach
       do 11 ibin = 1, nbin_a
          
          if(jaerosolstate(ibin) .eq. no_aerosol)goto 11
          
          cdum  = 6.283185*Dp_wet_a(ibin)*num_a(ibin)
          
          do 21 iv = 1, ngas_aerchtot
             Kn = 2.*freepath(iv)/Dp_wet_a(ibin)
             Fkn = fuchs_sutugin(Kn,accom(iv))
             kg(iv,ibin) = cdum*Dg(iv)*Fkn              ! 1/s
21           continue
             
11     continue
            
    else
       call wrf_message('Error in the choice of mSIZE_FRAMEWORK')
       call wrf_error_fatal('Stopping in subr. aerosolmtc')       
    endif


    if (m_gas2bin_uptk_flag <= 0) then
       ! when m_gas2bin_uptk_flag <= 0, some gases cannot condense onto every bin
       do ibin = 1, nbin_a
          do iv = 1, ngas_aerchtot
             if (i_gas2bin_uptk_flag(iv,ibin) <= 0) kg(iv,ibin) = 0.0
          end do
       end do
    end if

    return
  end subroutine aerosolmtc



  !***********************************************************************
  ! calculates dry and wet aerosol properties: density, refractive indices
  !
  ! author: Rahul A. Zaveri
  ! update: jan 2005
  !-----------------------------------------------------------------------
  subroutine calc_dry_n_wet_aerosol_props(                          &
     ibin, jaerosolstate, aer, electrolyte, water_a, num_a,         &  ! input
     dens_comp_a, mw_comp_a, dens_aer_mac, mw_aer_mac, ref_index_a, &  ! input
     Dp_dry_a, Dp_wet_a, dp_core_a,                                 &  ! output
     area_dry_a, area_wet_a, mass_dry_a, mass_wet_a,                &  ! output
     vol_dry_a, vol_wet_a, dens_dry_a, dens_wet_a,                  &  ! output
     ri_shell_a, ri_core_a, ri_avg_a                                )  ! output
    !      include 'v33com9a'

    use module_data_mosaic_constants,  only: piover4,piover6,third
    use module_data_mosaic_aero,  only: r8,nbin_a_max,naer,nelectrolyte,naercomp,  &!Parameters
         ngas_soa,no_aerosol,msectional,                                           &!Parameters
         maeroptic_aero,msize_framework,                                           &!Input
         inh4_a,ina_a,ica_a,ico3_a,imsa_a,icl_a,ino3_a,jtotal,iso4_a,ioc_a,joc,    &!TBD
         ibc_a,jbc,ioin_a,join,jh2o, isoa_first, jsoa_first

    use module_data_mosaic_asecthp, only: dcen_sect,isize_of_ibin,itype_of_ibin

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    integer, intent(in), dimension(nbin_a_max) :: jaerosolstate

    real(r8), intent(in), dimension(nbin_a_max) :: num_a
    real(r8), intent(in), dimension(naer) :: mw_aer_mac,dens_aer_mac
    real(r8), intent(in), dimension(naercomp) :: dens_comp_a,mw_comp_a
    real(r8), intent(inout), dimension(nbin_a_max) :: Dp_dry_a,Dp_wet_a
    real(r8), intent(inout), dimension(nbin_a_max) :: dp_core_a,vol_dry_a
    real(r8), intent(inout), dimension(nbin_a_max) :: vol_wet_a,dens_wet_a,water_a
    real(r8), intent(inout), dimension(nbin_a_max) :: area_dry_a,area_wet_a
    real(r8), intent(inout), dimension(nbin_a_max) :: mass_dry_a,mass_wet_a
    real(r8), intent(inout), dimension(nbin_a_max) :: dens_dry_a
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    complex, intent(in), dimension(naercomp) :: ref_index_a
    complex, intent(inout), dimension(nbin_a_max) :: ri_avg_a,ri_core_a,ri_shell_a
    ! local variables
    integer i, iaer, isize, itype, j, jc, je, k
    real(r8) :: aer_H, duma, vol_core, vol_shell, vol_dum
    real(r8),dimension(naercomp) :: comp_a
    complex rixvol_tot, rixvol_core, rixvol_shell


    ! calculate dry mass and dry volume of a bin
    mass_dry_a(ibin) = 0.0                ! initialize to 0.0
    vol_dry_a(ibin)  = 0.0                ! initialize to 0.0
    area_dry_a(ibin) = 0.0                ! initialize to 0.0

    if(jaerosolstate(ibin) .ne. no_aerosol)then

       aer_H = (2.*aer(iso4_a,jtotal,ibin) +   &
            aer(ino3_a,jtotal,ibin) +   &
            aer(icl_a,jtotal,ibin)  +   &
            aer(imsa_a,jtotal,ibin) +   &
            2.*aer(ico3_a,jtotal,ibin))-   &
            (2.*aer(ica_a,jtotal,ibin)  +   &
            aer(ina_a,jtotal,ibin)  +   &
            aer(inh4_a,jtotal,ibin))
       aer_H = max(aer_H, 0.0d0)

       do iaer = 1, naer
          mass_dry_a(ibin) = mass_dry_a(ibin) +   &
               aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)     ! ng/m^3(air)
          vol_dry_a(ibin) = vol_dry_a(ibin) +   &
               aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)/dens_aer_mac(iaer)          ! ncc/m^3(air)
       enddo
       mass_dry_a(ibin) = mass_dry_a(ibin) + aer_H
       vol_dry_a(ibin) = vol_dry_a(ibin) + aer_H

       mass_dry_a(ibin) = mass_dry_a(ibin)*1.e-15                 ! g/cc(air)
       vol_dry_a(ibin) = vol_dry_a(ibin)*1.e-15                   ! cc(aer)/cc(air)

       ! wet mass and wet volume
       mass_wet_a(ibin) = mass_dry_a(ibin) + water_a(ibin)*1.e-3  ! g/cc(air)
       vol_wet_a(ibin)  = vol_dry_a(ibin) + water_a(ibin)*1.e-3   ! cc(aer)/cc(air)

       ! calculate mean dry and wet particle densities
       dens_dry_a(ibin) = mass_dry_a(ibin)/vol_dry_a(ibin)                ! g/cc(aerosol)
       dens_wet_a(ibin) = mass_wet_a(ibin)/vol_wet_a(ibin)                ! g/cc(aerosol)

       ! calculate mean dry and wet particle diameters
       Dp_dry_a(ibin)=(vol_dry_a(ibin)/(piover6*num_a(ibin)))**third      ! cm
       Dp_wet_a(ibin)=(vol_wet_a(ibin)/(piover6*num_a(ibin)))**third      ! cm

       ! calculate mean dry and wet particle surface areas
       area_dry_a(ibin)= piover4*num_a(ibin)*Dp_dry_a(ibin)**2    ! cm^2/cc(air)
       area_wet_a(ibin)= piover4*num_a(ibin)*Dp_wet_a(ibin)**2    ! cm^2/cc(air)

       ! calculate volume average refractive index
       !   load comp_a array with component mass concentrations

       ! rahul had turned this off, but it is needed
       !        if(1 == 1)go to 100               ! TEMP
       if (maeroptic_aero <= 0) goto 100

       do je = 1, nelectrolyte
          comp_a(je)=electrolyte(je,jtotal,ibin)*mw_comp_a(je)*1.e-15     ! g/cc(air)
       enddo
       comp_a(joc)  = aer(ioc_a,  jtotal,ibin)*mw_comp_a(joc  )*1.e-15    ! g/cc(air)
       comp_a(jbc)  = aer(ibc_a,  jtotal,ibin)*mw_comp_a(jbc  )*1.e-15    ! g/cc(air)
       comp_a(join) = aer(ioin_a, jtotal,ibin)*mw_comp_a(join )*1.e-15    ! g/cc(air)

!      comp_a(jaro1)= aer(iaro1_a,jtotal,ibin)*mw_comp_a(jaro1)*1.e-15    ! g/cc(air)
!      comp_a(jaro2)= aer(iaro2_a,jtotal,ibin)*mw_comp_a(jaro2)*1.e-15    ! g/cc(air)
!      comp_a(jalk1)= aer(ialk1_a,jtotal,ibin)*mw_comp_a(jalk1)*1.e-15    ! g/cc(air)
!      comp_a(jole1)= aer(iole1_a,jtotal,ibin)*mw_comp_a(jole1)*1.e-15    ! g/cc(air)
!      comp_a(japi1)= aer(iapi1_a,jtotal,ibin)*mw_comp_a(japi1)*1.e-15    ! g/cc(air)
!      comp_a(japi2)= aer(iapi2_a,jtotal,ibin)*mw_comp_a(japi2)*1.e-15    ! g/cc(air)
!      comp_a(jlim1)= aer(ilim1_a,jtotal,ibin)*mw_comp_a(jlim1)*1.e-15    ! g/cc(air)
!      comp_a(jlim2)= aer(ilim2_a,jtotal,ibin)*mw_comp_a(jlim2)*1.e-15    ! g/cc(air)
       do k = 1, ngas_soa
       j = jsoa_first + k - 1
       i = isoa_first + k - 1
       comp_a(j) = aer(i,jtotal,ibin)*mw_comp_a(j)*1.e-15    ! g/cc(air)
       end do

       comp_a(jh2o) = water_a(ibin)*1.e-3                         ! g/cc(air)

       rixvol_tot   = (0.0,0.0)
       do jc = 1, naercomp
          comp_a(jc) = max( 0.0d0, comp_a(jc) )
          rixvol_tot = rixvol_tot   &
               + ref_index_a(jc)*comp_a(jc)/dens_comp_a(jc)
       enddo
       ri_avg_a(ibin) = rixvol_tot/vol_wet_a(ibin)

       !
       ! shell/core calcs - first set values to default (corresponding to zero core)
       !
       ri_shell_a(ibin) = ri_avg_a(ibin)
       ri_core_a(ibin)  = (0.0,0.0)
       Dp_core_a(ibin)  = 0.0

       ! sum ri*vol and vol for core species (bc and optionally oin=dust)
       ! currently just bc in core, but what about insoluble oin and dust species ???
       jc = jbc
       rixvol_core  = ref_index_a(jc)*comp_a(jc)/dens_comp_a(jc)
       vol_core = comp_a(jc)/dens_comp_a(jc)
       vol_core = max( 0.0d0, min( vol_core, vol_wet_a(ibin) ) )

       ! neglect core if (core volume) < 1.0d-9*(total volume)
       !              or (core volume) < 1.0d-22 cm3 = (0.58 nm)**3
       ! neglect shell using similar criteria
       vol_dum = max( 1.0d-22, 1.0d-9*vol_wet_a(ibin) )
       vol_shell = vol_wet_a(ibin) - vol_core
       if (vol_core >= vol_dum) then
          if (vol_shell < vol_dum) then
             ri_shell_a(ibin)  = (0.0,0.0)
             ri_core_a(ibin) = ri_avg_a(ibin)
             Dp_core_a(ibin) = Dp_wet_a(ibin)
          else
             ri_core_a(ibin) = rixvol_core/vol_core
             Dp_core_a(ibin) = Dp_wet_a(ibin)   &
                  * (vol_core/vol_wet_a(ibin))**third

             if (vol_shell >= vol_dum) then
                rixvol_shell = rixvol_tot - rixvol_core
                ri_shell_a(ibin) = rixvol_shell/vol_shell
             else
                ri_shell_a(ibin) = (0.0,0.0)
             endif
          endif
       endif

    else
       ! use defaults when (jaerosolstate(ibin) .eq. no_aerosol)

       dens_dry_a(ibin) = 1.0      ! g/cc(aerosol)
       dens_wet_a(ibin) = 1.0      ! g/cc(aerosol)
       !        Dp_dry_a(ibin) = dcen_sect(ibin)  ! cm
       !        Dp_wet_a(ibin) = dcen_sect(ibin)  ! cm
       if (msize_framework == msectional) then
          isize = isize_of_ibin(ibin)
          itype = itype_of_ibin(ibin)
          Dp_dry_a(ibin) = dcen_sect(isize,itype)
          Dp_wet_a(ibin) = Dp_dry_a(ibin)
       end if

       ri_avg_a(ibin) = (1.5,0.0)
       ri_shell_a(ibin) = (1.5,0.0)
       ri_core_a(ibin)  = (0.0,0.0)
       Dp_core_a(ibin)  = 0.0

    endif   ! if(jaerosolstate(ibin) .ne. no_aerosol)then


100 continue

    return
  end subroutine calc_dry_n_wet_aerosol_props



  !***********************************************************************
  ! forms electrolytes from ions
  !
  ! author: Rahul A. Zaveri
  ! update: june 2000
  !-----------------------------------------------------------------------
  subroutine form_electrolytes(jp,ibin,XT,aer,gas,electrolyte,total_species,tot_cl_in)
    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,           &
         ngas_aerchtot,ngas_volatile,nelectrolyte,jsolid,            &
         imsa_a,iso4_a,ica_a,ina_a,inh4_a,ino3_a,icl_a,ico3_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin, jp
    real(r8), intent(inout) :: XT
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer i, iXT_case, j, je
    real(r8) :: sum_dum, XNa_prime, XNH4_prime, XT_prime
    real(r8) :: store(naer)

    ! remove negative concentrations, if any
    !      do i=1,naer
    !        aer(i,jp,ibin) = max(0.0d0, aer(i,jp,ibin))  ! EFFI
    !      enddo


    !      call calculate_XT(ibin,jp,XT,aer)      ! EFFI

    if( (aer(iso4_a,jp,ibin)+aer(imsa_a,jp,ibin)) .gt.0.0)then
       XT   = ( aer(inh4_a,jp,ibin) +   &
            aer(ina_a,jp,ibin)  +   &
            2.*aer(ica_a,jp,ibin) )/   &
            (aer(iso4_a,jp,ibin)+0.5*aer(imsa_a,jp,ibin))
    else
       XT   = -1.0
    endif




!   if(XT .ge. 1.9999 .or. XT.lt.0.)then
    if(XT .ge. 2.0 .or. XT.lt.0.)then         ! RAZ 11/10/2014
       iXT_case = 1       ! near neutral (acidity is caused by HCl and/or HNO3)
    else
       iXT_case = 2       ! acidic (acidity is caused by excess SO4)
    endif

    ! initialize
    !
    ! put total aer(*) into store(*)
    store(iso4_a) = aer(iso4_a,jp,ibin)
    store(ino3_a) = aer(ino3_a,jp,ibin)
    store(icl_a)  = aer(icl_a, jp,ibin)
    store(imsa_a) = aer(imsa_a,jp,ibin)
    store(ico3_a) = aer(ico3_a,jp,ibin)
    store(inh4_a) = aer(inh4_a,jp,ibin)
    store(ina_a)  = aer(ina_a, jp,ibin)
    store(ica_a)  = aer(ica_a, jp,ibin)

    do j=1,nelectrolyte
       electrolyte(j,jp,ibin) = 0.0
    enddo

    !
    !---------------------------------------------------------
    !
    if(iXT_case.eq.1)then

       ! XT >= 2   : sulfate deficient
       call form_caso4(store,jp,ibin,electrolyte)
       call form_camsa2(store,jp,ibin,electrolyte)
       call form_na2so4(store,jp,ibin,electrolyte)
       call form_namsa(store,jp,ibin,electrolyte)
       call form_cano3(store,jp,ibin,electrolyte)
       call form_nano3(store,jp,ibin,electrolyte)
       call form_nacl(store,jp,ibin,aer,gas,electrolyte,total_species,tot_cl_in)
       call form_cacl2(store,jp,ibin,electrolyte)
       call form_caco3(store,jp,ibin,aer,electrolyte)
       call form_nh4so4(store,jp,ibin,electrolyte)
       call form_nh4msa(store,jp,ibin,electrolyte)
       call form_nh4no3(store,jp,ibin,electrolyte)
       call form_nh4cl(store,jp,ibin,electrolyte)
       call form_msa(store,jp,ibin,electrolyte)

       if(jp .eq. jsolid)then
          call degas_hno3(store,jp,ibin,aer,gas,electrolyte)
          call degas_hcl(store,jp,ibin,aer,gas,electrolyte)
          call degas_nh3(store,jp,ibin,aer,gas)
       else
          call form_hno3(store,jp,ibin,electrolyte)
          call form_hcl(store,jp,ibin,electrolyte)
          call degas_nh3(store,jp,ibin,aer,gas)
       endif



    elseif(iXT_case.eq.2)then

       ! XT < 2   : sulfate enough or sulfate excess

       call form_caso4(store,jp,ibin,electrolyte)
       call form_camsa2(store,jp,ibin,electrolyte)
       call form_namsa(store,jp,ibin,electrolyte)
       call form_nh4msa(store,jp,ibin,electrolyte)
       call form_msa(store,jp,ibin,electrolyte)

       if(store(iso4_a).eq.0.0)goto 10


       XT_prime =(store(ina_a)+store(inh4_a))/   &
            store(iso4_a)
       XNa_prime=0.5*store(ina_a)/store(iso4_a) + 1.

       if(XT_prime.ge.XNa_prime)then
          call form_na2so4(store,jp,ibin,electrolyte)
          XNH4_prime = 0.0
          if(store(iso4_a).gt.1.e-15)then
             XNH4_prime = store(inh4_a)/store(iso4_a)
          endif

          if(XNH4_prime .ge. 1.5)then
             call form_nh4so4_lvcite(store,jp,ibin,electrolyte)
          else
             call form_lvcite_nh4hso4(store,jp,ibin,electrolyte)
          endif

       elseif(XT_prime.ge.1.)then
          call form_nh4hso4(store,jp,ibin,electrolyte)
          call form_na2so4_nahso4(store,jp,ibin,electrolyte)
       elseif(XT_prime.lt.1.)then
          call form_nahso4(store,jp,ibin,electrolyte)
          call form_nh4hso4(store,jp,ibin,electrolyte)
          call form_h2so4(store,jp,ibin,electrolyte)
       endif

10     if(jp .eq. jsolid)then
          call degas_hno3(store,jp,ibin,aer,gas,electrolyte)
          call degas_hcl(store,jp,ibin,aer,gas,electrolyte)
          call degas_nh3(store,jp,ibin,aer,gas)
       else
          call form_hno3(store,jp,ibin,electrolyte)
          call form_hcl(store,jp,ibin,electrolyte)
          call degas_nh3(store,jp,ibin,aer,gas)
       endif

    endif ! case 1, 2


    ! re-calculate ions to eliminate round-off errors
    call electrolytes_to_ions(jp, ibin,aer,electrolyte)
    !---------------------------------------------------------
    !
    ! calculate % composition EFFI
    !!      sum_dum = 0.0
    !!      do je = 1, nelectrolyte
    !!        electrolyte(je,jp,ibin) = max(0.d0,electrolyte(je,jp,ibin)) ! remove -ve  EFFI
    !!        sum_dum = sum_dum + electrolyte(je,jp,ibin)
    !!      enddo
    !!
    !!      if(sum_dum .eq. 0.)sum_dum = 1.0
    !!      electrolyte_sum(jp,ibin) = sum_dum
    !!
    !!      do je = 1, nelectrolyte
    !!        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/sum_dum
    !!      enddo


    return
  end subroutine form_electrolytes



   subroutine form_na2so4(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,ina_a,jna2so4
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store(naer)
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    electrolyte(jna2so4,jp,ibin) = min(.5*store(ina_a),   &
         store(iso4_a))
    store(ina_a) =( (store(ina_a)) -   &
         (2.*electrolyte(jna2so4,jp,ibin)) )
    store(iso4_a)=( (store(iso4_a)) -   &
         (electrolyte(jna2so4,jp,ibin)) )
    store(ina_a) =max(0.d0, store(ina_a))
    store(iso4_a)=max(0.d0, store(iso4_a))

    return
  end subroutine form_na2so4



  subroutine form_nahso4(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,ina_a,jnahso4

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnahso4,jp,ibin) = min(store(ina_a),   &
         store(iso4_a))
    store(ina_a)  = ( (store(ina_a)) -   &
         (electrolyte(jnahso4,jp,ibin)) )
    store(iso4_a) = ( (store(iso4_a)) -   &
         (electrolyte(jnahso4,jp,ibin)) )
    store(ina_a)  = max(0.d0, store(ina_a))
    store(iso4_a) = max(0.d0, store(iso4_a))

    return
  end subroutine form_nahso4



  subroutine form_namsa(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         imsa_a,ina_a,jnamsa
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer)  :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnamsa,jp,ibin) = min(store(ina_a),   &
         store(imsa_a))
    store(ina_a)  = ( (store(ina_a)) -   &
         (electrolyte(jnamsa,jp,ibin)) )
    store(imsa_a) = ( (store(imsa_a)) -   &
         (electrolyte(jnamsa,jp,ibin)) )
    store(ina_a)  = max(0.d0, store(ina_a))
    store(imsa_a) = max(0.d0, store(imsa_a))

    return
  end subroutine form_namsa



  subroutine form_nano3(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ino3_a,ina_a,jnano3
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnano3,jp,ibin)=min(store(ina_a),store(ino3_a))
    store(ina_a)  = ( (store(ina_a)) -   &
         (electrolyte(jnano3,jp,ibin)) )
    store(ino3_a) = ( (store(ino3_a)) -   &
         (electrolyte(jnano3,jp,ibin)) )
    store(ina_a)  = max(0.d0, store(ina_a))
    store(ino3_a) = max(0.d0, store(ino3_a))

    return
  end subroutine form_nano3



  subroutine form_cano3(store,jp,ibin,electrolyte)        ! Ca(NO3)2
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ino3_a,ica_a,jcano3
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jcano3,jp,ibin) = min(store(ica_a),0.5*store(ino3_a))

    store(ica_a)  = ( (store(ica_a)) -   &
         (electrolyte(jcano3,jp,ibin)) )
    store(ino3_a) = ( (store(ino3_a)) -   &
         (2.*electrolyte(jcano3,jp,ibin)) )
    store(ica_a)  = max(0.d0, store(ica_a))
    store(ino3_a) = max(0.d0, store(ino3_a))

    return
  end subroutine form_cano3



  subroutine form_cacl2(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         icl_a,ica_a,jcacl2

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jcacl2,jp,ibin) = min(store(ica_a),0.5*store(icl_a))

    store(ica_a)  = ( (store(ica_a)) -   &
         (electrolyte(jcacl2,jp,ibin)) )
    store(icl_a)  = ( (store(icl_a)) -   &
         (2.*electrolyte(jcacl2,jp,ibin)) )
    store(ica_a)  = max(0.d0, store(ica_a))
    store(icl_a)  = max(0.d0, store(icl_a))

    return
  end subroutine form_cacl2

  
  subroutine form_caco3(store,jp,ibin,aer,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,jsolid,     &
         jtotal,                                                                   &
         ica_a,jcaco3,ico3_a

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    if(jp.eq.jtotal .or. jp.eq.jsolid)then
       electrolyte(jcaco3,jp,ibin) = store(ica_a)

       aer(ico3_a,jp,ibin)= electrolyte(jcaco3,jp,ibin)   ! force co3 = caco3

       store(ica_a) = 0.0
       store(ico3_a)= 0.0
    endif

    return
  end subroutine form_caco3
  
  
  
  subroutine form_nacl(store,jp,ibin,aer,gas,electrolyte,total_species,tot_cl_in)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,ngas_volatile,jtotal,jsolid,jliquid,                        &
         ina_a,jnacl,icl_a,ihcl_g

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local

    electrolyte(jnacl,jp,ibin) = store(ina_a)

    store(ina_a) = 0.0
    store(icl_a) = ( (store(icl_a)) -   &
         (electrolyte(jnacl,jp,ibin)) )

    if(store(icl_a) .lt. 0.)then                          ! cl deficit in aerosol. take some from gas
       aer(icl_a,jp,ibin)= aer(icl_a,jp,ibin)- store(icl_a)       ! update aer(icl_a)

       if(jp .ne. jtotal)then
          aer(icl_a,jtotal,ibin)= aer(icl_a,jliquid,ibin)+ &      ! update for jtotal
               aer(icl_a,jsolid,ibin)
       endif

       gas(ihcl_g) = gas(ihcl_g) + store(icl_a)                   ! update gas(ihcl_g)

       if(gas(ihcl_g) .lt. 0.0)then
          total_species(ihcl_g) = total_species(ihcl_g) - gas(ihcl_g)     ! update total_species
          tot_cl_in = tot_cl_in - gas(ihcl_g)                             ! update tot_cl_in
       endif

       gas(ihcl_g) = max(0.d0, gas(ihcl_g))                               ! restrict gas(ihcl_g) to >= 0.
       store(icl_a) = 0.                                          ! force store(icl_a) to 0.

    endif

    store(icl_a) = max(0.d0, store(icl_a))

    return
  end subroutine form_nacl



  subroutine form_nh4so4(store,jp,ibin,electrolyte)       ! (nh4)2so4
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,inh4_a,jnh4so4
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4so4,jp,ibin)= min(.5*store(inh4_a),   &
         store(iso4_a))
    store(inh4_a)= ( (store(inh4_a)) -   &
         (2.*electrolyte(jnh4so4,jp,ibin)) )
    store(iso4_a)= ( (store(iso4_a)) -   &
         (electrolyte(jnh4so4,jp,ibin)) )
    store(inh4_a) = max(0.d0, store(inh4_a))
    store(iso4_a) = max(0.d0, store(iso4_a))

    return
  end subroutine form_nh4so4



  subroutine form_nh4hso4(store,jp,ibin,electrolyte)      ! nh4hso4
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,inh4_a,jnh4hso4
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4hso4,jp,ibin) = min(store(inh4_a),   &
         store(iso4_a))
    store(inh4_a)= ( (store(inh4_a)) -   &
         (electrolyte(jnh4hso4,jp,ibin)) )
    store(iso4_a)= ( (store(iso4_a)) -   &
         (electrolyte(jnh4hso4,jp,ibin)) )
    store(inh4_a) = max(0.d0, store(inh4_a))
    store(iso4_a) = max(0.d0, store(iso4_a))

    return
  end subroutine form_nh4hso4



  subroutine form_nh4msa(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         imsa_a,inh4_a,jnh4msa
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4msa,jp,ibin) = min(store(inh4_a),   &
         store(imsa_a))
    store(inh4_a) = ( (store(inh4_a)) -   &
         (electrolyte(jnh4msa,jp,ibin)) )
    store(imsa_a) = ( (store(imsa_a)) -   &
         (electrolyte(jnh4msa,jp,ibin)) )
    store(inh4_a) = max(0.d0, store(inh4_a))
    store(imsa_a) = max(0.d0, store(imsa_a))

    return
  end subroutine form_nh4msa



  subroutine form_nh4cl(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         icl_a,inh4_a,jnh4cl
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4cl,jp,ibin) = min(store(inh4_a),   &
         store(icl_a))
    store(inh4_a) = ( (store(inh4_a)) -   &
         (electrolyte(jnh4cl,jp,ibin)) )
    store(icl_a)  = ( (store(icl_a)) -   &
         (electrolyte(jnh4cl,jp,ibin)) )
    store(inh4_a) = max(0.d0, store(inh4_a))
    store(icl_a)  = max(0.d0, store(icl_a))

    return
  end subroutine form_nh4cl



  subroutine form_nh4no3(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ino3_a,inh4_a,jnh4no3
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4no3,jp,ibin) = min(store(inh4_a),   &
         store(ino3_a))
    store(inh4_a) = ( (store(inh4_a)) -   &
         (electrolyte(jnh4no3,jp,ibin)) )
    store(ino3_a) = ( (store(ino3_a)) -   &
         (electrolyte(jnh4no3,jp,ibin)) )
    store(inh4_a) = max(0.d0, store(inh4_a))
    store(ino3_a) = max(0.d0, store(ino3_a))

    return
  end subroutine form_nh4no3



  subroutine form_nh4so4_lvcite(store,jp,ibin,electrolyte) ! (nh4)2so4 + (nh4)3h(so4)2
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,inh4_a,jnh4so4,jlvcite
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jnh4so4,jp,ibin)= ( (2.*store(inh4_a)) -   &
         (3.*store(iso4_a)) )
    electrolyte(jlvcite,jp,ibin)= ( (2.*store(iso4_a)) -   &
         (store(inh4_a)) )
    electrolyte(jnh4so4,jp,ibin)= max(0.d0,   &
         electrolyte(jnh4so4,jp,ibin))
    electrolyte(jlvcite,jp,ibin)= max(0.d0,   &
         electrolyte(jlvcite,jp,ibin))
    store(inh4_a) = 0.
    store(iso4_a) = 0.

    return
  end subroutine form_nh4so4_lvcite



  subroutine form_lvcite_nh4hso4(store,jp,ibin,electrolyte) ! (nh4)3h(so4)2 + nh4hso4
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,inh4_a,jlvcite,jnh4hso4
    implicit none

    ! subr arguments
    integer, intent(in) ::  jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jlvcite,jp,ibin) = ( (store(inh4_a)) -   &
         (store(iso4_a)) )
    electrolyte(jnh4hso4,jp,ibin)= ( (3.*store(iso4_a)) -   &
         (2.*store(inh4_a)) )
    electrolyte(jlvcite,jp,ibin) = max(0.d0,   &
         electrolyte(jlvcite,jp,ibin))
    electrolyte(jnh4hso4,jp,ibin)= max(0.d0,   &
         electrolyte(jnh4hso4,jp,ibin))
    store(inh4_a) = 0.
    store(iso4_a) = 0.

    return
  end subroutine form_lvcite_nh4hso4



  subroutine form_na2so4_nahso4(store,jp,ibin,electrolyte) ! na2so4 + nahso4
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,ina_a,jna2so4,jnahso4
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jna2so4,jp,ibin)= ( (store(ina_a)) -   &
         (store(iso4_a)) )
    electrolyte(jnahso4,jp,ibin)= ( (2.*store(iso4_a))-   &
         (store(ina_a)) )
    electrolyte(jna2so4,jp,ibin)= max(0.d0,   &
         electrolyte(jna2so4,jp,ibin))
    electrolyte(jnahso4,jp,ibin)= max(0.d0,   &
         electrolyte(jnahso4,jp,ibin))
    store(ina_a)  = 0.
    store(iso4_a) = 0.

    !     write(6,*)'na2so4 + nahso4'

    return
  end subroutine form_na2so4_nahso4



  subroutine form_h2so4(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         iso4_a,jh2so4
    implicit none

    ! subr arguments
    integer, intent(in) ::  jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jh2so4,jp,ibin) = max(0.0d0, store(iso4_a))
    store(iso4_a) = 0.0

    return
  end subroutine form_h2so4



  subroutine form_msa(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         imsa_a,jmsa
    implicit none

    ! subr arguments
    integer, intent(in) ::  jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jmsa,jp,ibin) = max(0.0d0, store(imsa_a))
    store(imsa_a) = 0.0

    return
  end subroutine form_msa



  subroutine form_hno3(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ino3_a,jhno3
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jhno3,jp,ibin) = max(0.0d0, store(ino3_a))
    store(ino3_a) = 0.0

    return
  end subroutine form_hno3



  subroutine form_hcl(store,jp,ibin,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         icl_a,jhcl
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    electrolyte(jhcl,jp,ibin) = max(0.0d0, store(icl_a))
    store(icl_a) = 0.0

    return
  end subroutine form_hcl



 subroutine degas_hno3(store,jp,ibin,aer,gas,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jtotal,jliquid,jsolid,                                      &
         ino3_a,ihno3_g,jhno3

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout), dimension(naer) :: store
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    store(ino3_a) = max(0.0d0, store(ino3_a))
    gas(ihno3_g) = gas(ihno3_g) + store(ino3_a)
    aer(ino3_a,jp,ibin) = ( (aer(ino3_a,jp,ibin)) -   &
         (store(ino3_a)) )
    aer(ino3_a,jp,ibin) = max(0.0d0,aer(ino3_a,jp,ibin))

    ! also do it for jtotal
    if(jp .ne. jtotal)then
       aer(ino3_a,jtotal,ibin) = aer(ino3_a,jsolid, ibin) +   &
            aer(ino3_a,jliquid,ibin)
    endif

    electrolyte(jhno3,jp,ibin) = 0.0
    store(ino3_a) = 0.0

    return
  end subroutine degas_hno3



  subroutine degas_hcl(store,jp,ibin,aer,gas,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jtotal,jliquid,jsolid,                                      &
         icl_a,ihcl_g,jhcl
    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout) :: store(naer)
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte

    store(icl_a) = max(0.0d0, store(icl_a))
    gas(ihcl_g) = gas(ihcl_g) + store(icl_a)
    aer(icl_a,jp,ibin) = ( (aer(icl_a,jp,ibin)) -   &
         (store(icl_a)) )
    aer(icl_a,jp,ibin) = max(0.0d0,aer(icl_a,jp,ibin))

    ! also do it for jtotal
    if(jp .ne. jtotal)then
       aer(icl_a,jtotal,ibin) = aer(icl_a,jsolid, ibin) +   &
            aer(icl_a,jliquid,ibin)
    endif

    electrolyte(jhcl,jp,ibin) = 0.0
    store(icl_a) = 0.0

    return
  end subroutine degas_hcl



  subroutine degas_nh3(store,jp,ibin,aer,gas)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jtotal,jliquid,jsolid,                                      &
         inh3_g,inh4_a

    implicit none

    ! subr arguments
    integer, intent(in) :: jp, ibin
    real(r8), intent(inout) :: store(naer)
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer

    store(inh4_a) = max(0.0d0, store(inh4_a))
    gas(inh3_g) = gas(inh3_g) + store(inh4_a)
    aer(inh4_a,jp,ibin) = ( (aer(inh4_a,jp,ibin)) -   &
         (store(inh4_a)) )
    aer(inh4_a,jp,ibin) = max(0.0d0,aer(inh4_a,jp,ibin))

    ! also do it for jtotal
    if(jp .ne. jtotal)then
       aer(inh4_a,jtotal,ibin)= aer(inh4_a,jsolid, ibin) +   &
            aer(inh4_a,jliquid,ibin)
    endif

    store(inh4_a) = 0.0

    return
  end subroutine degas_nh3



  !***********************************************************************
  ! subroutines to absorb and degas small amounts of volatile species
  !
  ! author: Rahul A. Zaveri
  ! update: jun 2002
  !-----------------------------------------------------------------------
  !
  ! nh4no3 (liquid)
  subroutine absorb_tiny_nh4no3(ibin,aer,gas,electrolyte,delta_nh3_max,            &
       delta_hno3_max,electrolyte_sum)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jtotal,jliquid,jsolid,                                      &
         inh4_a,ino3_a,inh3_g,ihno3_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: delta_nh3_max,delta_hno3_max
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(3,nbin_a_max) :: electrolyte_sum
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer je
    real(r8) :: small_aer, small_gas, small_amt



    !! EFFI
    electrolyte_sum(jtotal,ibin) = 0.0
    do je = 1, nelectrolyte
       electrolyte_sum(jtotal,ibin) = electrolyte_sum(jtotal,ibin) + &
            electrolyte(je,jtotal,ibin)
    enddo
    !! EFFI


    small_gas = 0.01 * min(delta_nh3_max(ibin),delta_hno3_max(ibin))
    small_aer = 0.01 * electrolyte_sum(jtotal,ibin)
    if(small_aer .eq. 0.0)small_aer = small_gas

    small_amt = min(small_gas, small_aer)

    aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) + small_amt
    aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) + small_amt

    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
         aer(ino3_a,jliquid,ibin)

    ! update gas
    gas(inh3_g)  = ((gas(inh3_g)) - (small_amt))
    gas(ihno3_g) = ((gas(ihno3_g)) - (small_amt))

    return
  end subroutine absorb_tiny_nh4no3



  !--------------------------------------------------------------------
  ! nh4cl (liquid)
  subroutine absorb_tiny_nh4cl(ibin,aer,gas,electrolyte,delta_nh3_max,             &
       delta_hcl_max,electrolyte_sum)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jtotal,jliquid,jsolid,                                      &
         inh4_a,icl_a,inh3_g,ihcl_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: delta_nh3_max,delta_hcl_max
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(3,nbin_a_max) :: electrolyte_sum
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer je
    real(r8) :: small_aer, small_gas, small_amt


    !! EFFI
    electrolyte_sum(jtotal,ibin) = 0.0
    do je = 1, nelectrolyte
       electrolyte_sum(jtotal,ibin) = electrolyte_sum(jtotal,ibin) + &
            electrolyte(je,jtotal,ibin)
    enddo
    !! EFFI



    small_gas = 0.01 * min(delta_nh3_max(ibin), delta_hcl_max(ibin))
    small_aer = 0.01 * electrolyte_sum(jtotal,ibin)
    if(small_aer .eq. 0.0)small_aer = small_gas

    small_amt = min(small_gas, small_aer)

    aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) + small_amt
    aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin)  + small_amt

    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
         aer(icl_a,jliquid,ibin)

    ! update gas
    gas(inh3_g) = ((gas(inh3_g)) - (small_amt))
    gas(ihcl_g) = ((gas(ihcl_g)) - (small_amt))

    return
  end subroutine absorb_tiny_nh4cl



  !--------------------------------------------------------------------
  ! hno3 (liquid)
  subroutine absorb_tiny_hno3(ibin,aer,gas,delta_hno3_max)        ! and degas tiny hcl
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jliquid,jsolid,jtotal,                                      &
         icl_a,ino3_a,ihno3_g,ihcl_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: delta_hno3_max
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    ! local variables
    real(r8) :: small_aer, small_amt, small_gas

    small_gas = 0.01 * delta_hno3_max(ibin)
    small_aer = 0.01 * aer(icl_a,jliquid,ibin)

    small_amt = min(small_gas, small_aer)

    ! absorb tiny hno3
    aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) + small_amt
    aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
         aer(ino3_a,jliquid,ibin)
    gas(ihno3_g) = ((gas(ihno3_g))-(small_amt))

    ! degas tiny hcl
    aer(icl_a,jliquid,ibin)  = ((aer(icl_a,jliquid,ibin))-   &
         (small_amt))
    aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin) +   &
         aer(icl_a,jliquid,ibin)

    ! update gas
    gas(ihcl_g) = gas(ihcl_g) + small_amt

    return
  end subroutine absorb_tiny_hno3



  !--------------------------------------------------------------
  ! hcl (liquid)
  subroutine absorb_tiny_hcl(ibin,aer,gas,delta_hcl_max)  ! and degas tiny hno3
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jliquid,jtotal,jsolid,                                      &
         ino3_a,icl_a,ihcl_g,ihno3_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: delta_hcl_max
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    ! local variables
    real(r8) :: small_aer, small_amt, small_gas

    small_gas = 0.01 * delta_hcl_max(ibin)
    small_aer = 0.01 * aer(ino3_a,jliquid,ibin)

    small_amt = min(small_gas, small_aer)

    ! absorb tiny hcl
    aer(icl_a,jliquid,ibin)= aer(icl_a,jliquid,ibin) + small_amt
    aer(icl_a,jtotal,ibin) = aer(icl_a,jsolid,ibin) +   &
         aer(icl_a,jliquid,ibin)
    gas(ihcl_g) = ((gas(ihcl_g))-(small_amt))

    ! degas tiny hno3
    aer(ino3_a,jliquid,ibin) = ((aer(ino3_a,jliquid,ibin))-   &
         (small_amt))
    aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
         aer(ino3_a,jliquid,ibin)

    ! update gas
    gas(ihno3_g) = gas(ihno3_g) + small_amt

    return
  end subroutine absorb_tiny_hcl
  


  !--------------------------------------------------------------
  ! nh4no3 (liquid)
  subroutine degas_tiny_nh4no3(ibin,aer,gas,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jliquid,jsolid,jtotal,                                      &
         jnh4no3,inh4_a,ino3_a,inh3_g,ihno3_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: small_amt

    small_amt = 0.01 * electrolyte(jnh4no3,jliquid,ibin)

    aer(inh4_a,jliquid,ibin) = ((aer(inh4_a,jliquid,ibin))-   &
         (small_amt))
    aer(ino3_a,jliquid,ibin) = ((aer(ino3_a,jliquid,ibin))-   &
         (small_amt))

    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
         aer(ino3_a,jliquid,ibin)

    ! update gas
    gas(inh3_g)  = gas(inh3_g)  + small_amt
    gas(ihno3_g) = gas(ihno3_g) + small_amt

    return
  end subroutine degas_tiny_nh4no3




  !--------------------------------------------------------------------
  ! nh4cl (liquid)
  subroutine degas_tiny_nh4cl(ibin,aer,gas,electrolyte)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jliquid,jsolid,jtotal,                                      &
         jnh4cl,inh4_a,icl_a,inh3_g,ihcl_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: small_amt


    small_amt = 0.01 * electrolyte(jnh4cl,jliquid,ibin)

    aer(inh4_a,jliquid,ibin) = ((aer(inh4_a,jliquid,ibin))-   &
         (small_amt))
    aer(icl_a,jliquid,ibin)  = ((aer(icl_a,jliquid,ibin))-   &
         (small_amt))

    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
         aer(icl_a,jliquid,ibin)

    ! update gas
    gas(inh3_g) = gas(inh3_g) + small_amt
    gas(ihcl_g) = gas(ihcl_g) + small_amt

    return
  end subroutine degas_tiny_nh4cl



  !***********************************************************************
  ! subroutines to equilibrate volatile acids
  !
  ! author: Rahul A. Zaveri
  ! update: may 2002
  !-----------------------------------------------------------------------
  subroutine equilibrate_acids(ibin,aer,gas,electrolyte,activity,mc,water_a,       &
       total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,ngas_volatile,Ncation,Nanion,nrxn_aer_gl,nrxn_aer_ll,       &
         ihno3_g,ihcl_g
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(nrxn_aer_gl) :: Keq_gl
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables



    if(gas(ihcl_g)*gas(ihno3_g) .gt. 0.)then
       call equilibrate_hcl_and_hno3(ibin,aer,gas,electrolyte,activity,mc,water_a, &
            total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    elseif(gas(ihcl_g) .gt. 0.)then
       call equilibrate_hcl(ibin,aer,gas,electrolyte,activity,mc,water_a,          &
            total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    elseif(gas(ihno3_g) .gt. 0.)then
       call equilibrate_hno3(ibin,aer,gas,electrolyte,activity,mc,water_a,         &
            total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    endif


    return
  end subroutine equilibrate_acids



  ! only hcl
  subroutine equilibrate_hcl(ibin,aer,gas,electrolyte,activity,mc,water_a,         &
       total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,ngas_volatile,Ncation,jliquid,jsolid,jtotal,Nanion,         &
         nrxn_aer_gl,nrxn_aer_ll,                                                  &
         ja_so4,ja_hso4,ihcl_g,icl_a,jhcl,ino3_a,ica_a,inh4_a,ina_a,jc_h,jc_ca,    &
         jc_nh4,jc_na,ja_cl,ja_no3,jhno3,jnh4cl
    
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nrxn_aer_gl) ::Keq_gl
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: a, aerH, aerHSO4, aerSO4, b, c, dum, Kdash_hcl, mH, Tcl,   &
         W, XT, Z
    !real(r8) :: quadratic                                 ! mosaic func

    aerSO4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
    aerHSO4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

    Tcl = aer(icl_a,jliquid,ibin) + gas(ihcl_g)   ! nmol/m^3(air)
    Kdash_hcl = Keq_gl(4)*1.e+18/gam(jhcl,ibin)**2        ! (nmol^2/kg^2)/(nmol/m^3(air))
    Z = (   aer(ina_a, jliquid,ibin) +               &  ! nmol/m^3(air)
         aer(inh4_a,jliquid,ibin) +   &
         2.*aer(ica_a, jliquid,ibin) ) -   &
         (2.*aerSO4  +   &
         aerHSO4 +   &
         aer(ino3_a,jliquid,ibin) )


    W     = water_a(ibin)                         ! kg/m^3(air)

    Kdash_hcl = Keq_gl(4)*1.e+18/gam(jhcl,ibin)**2        ! (nmol^2/kg^2)/(nmol/m^3(air))
    a = 1.0
    b = ((Kdash_hcl*W) + (Z/W))*1.e-9
    c = Kdash_hcl*(Z - Tcl)*1.e-18


    dum = ((b*b)-(4.*a*c))
    if (dum .lt. 0.) return               ! no real root


    if(c .lt. 0.)then
       mH = quadratic(a,b,c)      ! mol/kg(water)
       aerH = mH*W*1.e+9
       aer(icl_a,jliquid,ibin) = ((aerH) + (Z))
    else
       mH = sqrt(Keq_ll(3))
    endif

    call form_electrolytes(jliquid,ibin,XT,aer,gas,electrolyte,total_species,tot_cl_in)

    ! update gas phase concentration
    gas(ihcl_g) = ( (Tcl)  - (aer(icl_a,jliquid,ibin))  )


    ! update the following molalities
    ma(ja_so4,ibin)  = 1.e-9*aerSO4/water_a(ibin)
    ma(ja_hso4,ibin) = 1.e-9*aerHSO4/water_a(ibin)
    ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
    ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

    mc(jc_h,ibin)    = mH
    mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
    mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
    mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


    ! update the following activities
    activity(jhcl,ibin)    = mc(jc_h,ibin)  *ma(ja_cl,ibin)  *   &
         gam(jhcl,ibin)**2

    activity(jhno3,ibin)   = mc(jc_h,ibin)  *ma(ja_no3,ibin) *   &
         gam(jhno3,ibin)**2

    activity(jnh4cl,ibin)  = mc(jc_nh4,ibin)*ma(ja_cl,ibin) *   &
         gam(jnh4cl,ibin)**2


    ! also update xyz(jtotal)
    aer(icl_a,jtotal,ibin) = aer(icl_a,jliquid,ibin) +   &
         aer(icl_a,jsolid,ibin)

    electrolyte(jhcl,jtotal,ibin) = electrolyte(jhcl,jliquid,ibin)

    return
  end subroutine equilibrate_hcl



  ! only hno3
  subroutine equilibrate_hno3(ibin,aer,gas,electrolyte,activity,mc,water_a,        &
       total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,ngas_volatile,Ncation,jliquid,jsolid,jtotal,Nanion,         &
         nrxn_aer_gl,nrxn_aer_ll,                                                  &
         ja_so4,ja_hso4,ihno3_g,ino3_a,jhno3,icl_a,ica_a,inh4_a,ina_a,jc_h,jc_ca,  &
         jc_nh4,jc_na,ja_cl,jhcl,ja_no3,jnh4no3
    
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nrxn_aer_gl) :: Keq_gl
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: a, aerH, aerHSO4, aerSO4, b, c, dum, Kdash_hno3, mH,   &
         Tno3, W, XT, Z
    !real(r8) :: quadratic                                 ! mosaic func

    aerSO4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
    aerHSO4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

    Tno3 = aer(ino3_a,jliquid,ibin) + gas(ihno3_g)        ! nmol/m^3(air)
    Kdash_hno3 = Keq_gl(3)*1.e+18/gam(jhno3,ibin)**2      ! (nmol^2/kg^2)/(nmol/m^3(air))
    Z = (   aer(ina_a, jliquid,ibin) +               &  ! nmol/m^3(air)
         aer(inh4_a,jliquid,ibin) +   &
         2.*aer(ica_a, jliquid,ibin) ) -   &
         (2.*aerSO4  +   &
         aerHSO4 +   &
         aer(icl_a,jliquid,ibin) )


    W     = water_a(ibin)                         ! kg/m^3(air)

    Kdash_hno3 = Keq_gl(3)*1.e+18/gam(jhno3,ibin)**2      ! (nmol^2/kg^2)/(nmol/m^3(air))
    a = 1.0
    b = ((Kdash_hno3*W) + (Z/W))*1.e-9
    c = Kdash_hno3*(Z - Tno3)*1.e-18

    dum = ((b*b)-(4.*a*c))
    if (dum .lt. 0.) return               ! no real root



    if(c .lt. 0.)then
       mH = quadratic(a,b,c)      ! mol/kg(water)
       aerH = mH*W*1.e+9
       aer(ino3_a,jliquid,ibin) = ((aerH) + (Z))
    else
       mH = sqrt(Keq_ll(3))
    endif

    call form_electrolytes(jliquid,ibin,XT,aer,gas,electrolyte,total_species,tot_cl_in)

    ! update gas phase concentration
    gas(ihno3_g)= ( (Tno3) - (aer(ino3_a,jliquid,ibin)) )


    ! update the following molalities
    ma(ja_so4,ibin)  = 1.e-9*aerSO4/water_a(ibin)
    ma(ja_hso4,ibin) = 1.e-9*aerHSO4/water_a(ibin)
    ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
    ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

    mc(jc_h,ibin)    = mH
    mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
    mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
    mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


    ! update the following activities
    activity(jhcl,ibin)    = mc(jc_h,ibin)  *ma(ja_cl,ibin)  *   &
         gam(jhcl,ibin)**2

    activity(jhno3,ibin)   = mc(jc_h,ibin)  *ma(ja_no3,ibin) *   &
         gam(jhno3,ibin)**2

    activity(jnh4no3,ibin) = mc(jc_nh4,ibin)*ma(ja_no3,ibin) *   &
         gam(jnh4no3,ibin)**2


    ! also update xyz(jtotal)
    aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
         aer(ino3_a,jsolid,ibin)

    electrolyte(jhno3,jtotal,ibin) = electrolyte(jhno3,jliquid,ibin)

    return
  end subroutine equilibrate_hno3



  ! both hcl and hno3
  subroutine equilibrate_hcl_and_hno3(ibin,aer,gas,electrolyte,activity,mc,        &
       water_a,total_species,tot_cl_in,ma,gam,Keq_ll,Keq_gl)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,ngas_volatile,Ncation,jliquid,jsolid,jtotal,Nanion,         &
         nrxn_aer_gl,nrxn_aer_ll,                                                  &
         ja_so4,ja_hso4,ihcl_g,icl_a,ihno3_g,ino3_a,jhcl,jhno3,             &
         ica_a,inh4_a,ina_a,jc_h,jc_ca,jc_nh4,jc_na,ja_cl,ja_no3,jnh4no3,   &
         jnh4cl
    
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(nbin_a_max) :: water_a
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(nrxn_aer_ll) :: Keq_ll
    real(r8), intent(inout), dimension(nrxn_aer_gl) :: Keq_gl
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: activity,gam
    real(r8), intent(inout), dimension(Ncation,nbin_a_max) :: mc
    real(r8), intent(inout), dimension(Nanion,nbin_a_max) :: ma
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    real(r8) :: aerH, aerHSO4, aerSO4, Kdash_hcl, Kdash_hno3,   &
         mH, p, q, r, Tcl, Tno3, W, XT, Z
    !real(r8) :: cubic                                     ! mosaic func


    aerSO4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
    aerHSO4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

    Tcl  = aer(icl_a,jliquid,ibin)  + gas(ihcl_g) ! nmol/m^3(air)
    Tno3 = aer(ino3_a,jliquid,ibin) + gas(ihno3_g)        ! nmol/m^3(air)

    Kdash_hcl  = Keq_gl(4)*1.e+18/gam(jhcl,ibin)**2       ! (nmol^2/kg^2)/(nmol/m^3(air))
    Kdash_hno3 = Keq_gl(3)*1.e+18/gam(jhno3,ibin)**2      ! (nmol^2/kg^2)/(nmol/m^3(air))

    Z = (   aer(ina_a, jliquid,ibin) +               &  ! nmol/m^3(air)
         aer(inh4_a,jliquid,ibin) +   &
         2.*aer(ica_a, jliquid,ibin) ) -   &
         (2.*aerSO4 + aerHSO4 )


    W = water_a(ibin)

    Kdash_hcl  = Keq_gl(4)*1.e+18/gam(jhcl,ibin)**2       ! (nmol^2/kg^2)/(nmol/m^3(air))
    Kdash_hno3 = Keq_gl(3)*1.e+18/gam(jhno3,ibin)**2      ! (nmol^2/kg^2)/(nmol/m^3(air))

    p = (Z/W + W*(Kdash_hcl + Kdash_hno3))*1.e-9

    q = 1.e-18*Kdash_hcl*Kdash_hno3*W**2  +   &
         1.e-18*Z*(Kdash_hcl + Kdash_hno3) -   &
         1.e-18*Kdash_hcl*Tcl -   &
         1.e-18*Kdash_hno3*Tno3

    r = 1.e-18*Kdash_hcl*Kdash_hno3*W*(Z - Tcl - Tno3)*1.e-9

    mH = cubic(p,q,r)

    if(mH .gt. 0.0)then
       aerH = mH*W*1.e+9
       aer(ino3_a,jliquid,ibin) = Kdash_hno3*W*W*Tno3/   &
            (aerH + Kdash_hno3*W*W)
       aer(icl_a, jliquid,ibin) = Kdash_hcl*W*W*Tcl/   &
            (aerH + Kdash_hcl*W*W)
    else
       mH = sqrt(Keq_ll(3))
    endif

    call form_electrolytes(jliquid,ibin,XT,aer,gas,electrolyte,total_species,tot_cl_in)

    ! update gas phase concentration
    gas(ihno3_g)= ( (Tno3) - (aer(ino3_a,jliquid,ibin)) )
    gas(ihcl_g) = ( (Tcl)  - (aer(icl_a,jliquid,ibin))  )


    ! update the following molalities
    ma(ja_so4,ibin)  = 1.e-9*aerSO4/water_a(ibin)
    ma(ja_hso4,ibin) = 1.e-9*aerHSO4/water_a(ibin)
    ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
    ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

    mc(jc_h,ibin)    = mH
    mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
    mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
    mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


    ! update the following activities
    activity(jhcl,ibin)    = mc(jc_h,ibin)*ma(ja_cl,ibin)   *   &
         gam(jhcl,ibin)**2

    activity(jhno3,ibin)   = mc(jc_h,ibin)*ma(ja_no3,ibin)  *   &
         gam(jhno3,ibin)**2

    activity(jnh4no3,ibin) = mc(jc_nh4,ibin)*ma(ja_no3,ibin)*   &
         gam(jnh4no3,ibin)**2

    activity(jnh4cl,ibin)  = mc(jc_nh4,ibin)*ma(ja_cl,ibin) *   &
         gam(jnh4cl,ibin)**2


    ! also update xyz(jtotal)
    aer(icl_a,jtotal,ibin)  = aer(icl_a,jliquid,ibin) +   &
         aer(icl_a,jsolid,ibin)

    aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
         aer(ino3_a,jsolid,ibin)

    electrolyte(jhno3,jtotal,ibin) = electrolyte(jhno3,jliquid,ibin)
    electrolyte(jhcl, jtotal,ibin) = electrolyte(jhcl, jliquid,ibin)

    return
  end subroutine equilibrate_hcl_and_hno3



  !***********************************************************************
  ! subroutines to evaporate solid volatile species
  !
  ! author: Rahul A. Zaveri
  ! update: sep 2004
  !-----------------------------------------------------------------------
  !
  ! nh4no3 (solid)
  subroutine degas_solid_nh4no3(ibin,aer,gas,electrolyte,Keq_sg)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jsolid,jliquid,jtotal,nrxn_aer_sg,                          &
         ihno3_g,inh3_g,jnh4no3,inh4_a,ino3_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(nrxn_aer_sg) :: Keq_sg
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer jp
    real(r8) :: a, b, c, xgas, XT
    !real(r8) :: quadratic                                 ! mosaic func


    jp = jsolid

    a = 1.0
    b = gas(inh3_g) + gas(ihno3_g)
    c = gas(inh3_g)*gas(ihno3_g) - Keq_sg(1)
    xgas = quadratic(a,b,c)

    if(xgas .ge. electrolyte(jnh4no3,jp,ibin))then ! degas all nh4no3

       gas(inh3_g) = gas(inh3_g)  + electrolyte(jnh4no3,jp,ibin)
       gas(ihno3_g)= gas(ihno3_g) + electrolyte(jnh4no3,jp,ibin)
       aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
            electrolyte(jnh4no3,jp,ibin)
       aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) -   &
            electrolyte(jnh4no3,jp,ibin)

    else  ! degas only xgas amount of nh4no3

       gas(inh3_g) = gas(inh3_g)  + xgas
       gas(ihno3_g)= gas(ihno3_g) + xgas
       aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
       aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) - xgas
    endif


    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
         aer(ino3_a,jliquid,ibin)

    return
  end subroutine degas_solid_nh4no3



  ! nh4cl (solid)
  subroutine degas_solid_nh4cl(ibin,aer,gas,electrolyte,Keq_sg)
    use module_data_mosaic_aero, only: r8,naer,nelectrolyte,nbin_a_max,            &
         ngas_aerchtot,jsolid,jliquid,jtotal,nrxn_aer_sg,                          &
         ihcl_g,inh3_g,jnh4cl,inh4_a,icl_a
    implicit none

    ! subr arguments
    integer, intent(in) :: ibin
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(nrxn_aer_sg) :: Keq_sg
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer jp
    real(r8) :: a, b, c, xgas, XT
    !real(r8) :: quadratic                                 ! mosaic func


    jp = jsolid

    a = 1.0
    b = gas(inh3_g) + gas(ihcl_g)
    c = gas(inh3_g)*gas(ihcl_g) - Keq_sg(2)
    xgas = quadratic(a,b,c)

    if(xgas .ge. electrolyte(jnh4cl,jp,ibin))then ! degas all nh4cl

       gas(inh3_g) = gas(inh3_g) + electrolyte(jnh4cl,jp,ibin)
       gas(ihcl_g) = gas(ihcl_g) + electrolyte(jnh4cl,jp,ibin)
       aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
            electrolyte(jnh4cl,jp,ibin)
       aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin) -   &
            electrolyte(jnh4cl,jp,ibin)

    else  ! degas only xgas amount of nh4cl

       gas(inh3_g) = gas(inh3_g) + xgas
       gas(ihcl_g) = gas(ihcl_g) + xgas
       aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
       aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin)  - xgas

    endif


    ! update jtotal
    aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
         aer(inh4_a,jliquid,ibin)
    aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
         aer(icl_a,jliquid,ibin)

    return
  end subroutine degas_solid_nh4cl



  !***********************************************************************
  ! conforms aerosol generic species to a valid electrolyte composition
  !
  ! author: Rahul A. Zaveri
  ! update: june 2000
  !-----------------------------------------------------------------------
  subroutine conform_electrolytes(jp,ibin,XT,aer,gas,electrolyte,total_species,tot_cl_in)

    use module_data_mosaic_aero, only: r8,naer,nbin_a_max,           &
         ngas_aerchtot, ngas_volatile, nelectrolyte,                 &
         imsa_a,iso4_a,ica_a,ina_a,inh4_a,ino3_a,icl_a,ico3_a

    implicit none

    ! subr arguments
    integer, intent(in) :: ibin, jp
    real(r8), intent(inout) :: XT
    real(r8), intent(inout), dimension(ngas_aerchtot) :: gas
    real(r8), intent(inout), dimension(ngas_volatile) :: total_species
    real(r8), intent(inout) :: tot_cl_in
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    ! local variables
    integer i, iXT_case, je
    real(r8) :: sum_dum, XNa_prime, XNH4_prime, XT_prime
    real(r8) :: store(naer)

    ! remove negative concentrations, if any
    !      do i=1,naer
    !      aer(i,jp,ibin) = max(0.0d0, aer(i,jp,ibin))    ! EFFI
    !      enddo


    !      call calculate_XT(ibin,jp,XT,aer)      ! EFFI

    if( (aer(iso4_a,jp,ibin)+aer(imsa_a,jp,ibin)) .gt.0.0)then
       XT   = ( aer(inh4_a,jp,ibin) +   &
            aer(ina_a,jp,ibin)  +   &
            2.*aer(ica_a,jp,ibin) )/   &
            (aer(iso4_a,jp,ibin)+0.5*aer(imsa_a,jp,ibin))
    else
       XT   = -1.0
    endif


!   if(XT .ge. 1.9999 .or. XT.lt.0.)then ! RAZ 11/10/2014
    if(XT .ge. 2.0 .or. XT.lt.0.)then ! RAZ 11/10/2014
       iXT_case = 1       ! near neutral (acidity is caused by HCl and/or HNO3)
    else
       iXT_case = 2       ! acidic (acidity is caused by excess SO4)
    endif

    ! initialize
    !
    ! put total aer(*) into store(*)
    store(iso4_a) = aer(iso4_a,jp,ibin)
    store(ino3_a) = aer(ino3_a,jp,ibin)
    store(icl_a)  = aer(icl_a, jp,ibin)
    store(imsa_a) = aer(imsa_a,jp,ibin)
    store(ico3_a) = aer(ico3_a,jp,ibin)
    store(inh4_a) = aer(inh4_a,jp,ibin)
    store(ina_a)  = aer(ina_a, jp,ibin)
    store(ica_a)  = aer(ica_a, jp,ibin)

    do je=1,nelectrolyte
       electrolyte(je,jp,ibin) = 0.0
    enddo

    !
    !---------------------------------------------------------
    !
    if(iXT_case.eq.1)then

       ! XT >= 2   : sulfate deficient

       call form_caso4(store,jp,ibin,electrolyte)
       call form_camsa2(store,jp,ibin,electrolyte)
       call form_na2so4(store,jp,ibin,electrolyte)
       call form_namsa(store,jp,ibin,electrolyte)
       call form_cano3(store,jp,ibin,electrolyte)
       call form_nano3(store,jp,ibin,electrolyte)
       call form_nacl(store,jp,ibin,aer,gas,electrolyte,total_species,tot_cl_in)
       call form_cacl2(store,jp,ibin,electrolyte)
       call form_caco3(store,jp,ibin,aer,electrolyte)
       call form_nh4so4(store,jp,ibin,electrolyte)
       call form_nh4msa(store,jp,ibin,electrolyte)
       call form_nh4no3(store,jp,ibin,electrolyte)
       call form_nh4cl(store,jp,ibin,electrolyte)
       call form_msa(store,jp,ibin,electrolyte)
       call degas_hno3(store,jp,ibin,aer,gas,electrolyte)
       call degas_hcl(store,jp,ibin,aer,gas,electrolyte)
       call degas_nh3(store,jp,ibin,aer,gas)

    elseif(iXT_case.eq.2)then

       ! XT < 2   : sulfate enough or sulfate excess

       call form_caso4(store,jp,ibin,electrolyte)
       call form_camsa2(store,jp,ibin,electrolyte)
       call form_namsa(store,jp,ibin,electrolyte)
       call form_nh4msa(store,jp,ibin,electrolyte)
       call form_msa(store,jp,ibin,electrolyte)

       if(store(iso4_a).eq.0.0)goto 10


       XT_prime =(store(ina_a)+store(inh4_a))/   &
            store(iso4_a)
       XNa_prime=0.5*store(ina_a)/store(iso4_a) + 1.

       if(XT_prime.ge.XNa_prime)then
          call form_na2so4(store,jp,ibin,electrolyte)
          XNH4_prime = 0.0
          if(store(iso4_a).gt.1.e-15)then
             XNH4_prime = store(inh4_a)/store(iso4_a)
          endif

          if(XNH4_prime .ge. 1.5)then
             call form_nh4so4_lvcite(store,jp,ibin,electrolyte)
          else
             call form_lvcite_nh4hso4(store,jp,ibin,electrolyte)
          endif

       elseif(XT_prime.ge.1.)then
          call form_nh4hso4(store,jp,ibin,electrolyte)
          call form_na2so4_nahso4(store,jp,ibin,electrolyte)
       elseif(XT_prime.lt.1.)then
          call form_nahso4(store,jp,ibin,electrolyte)
          call form_nh4hso4(store,jp,ibin,electrolyte)
          call form_h2so4(store,jp,ibin,electrolyte)
       endif

10     call degas_hno3(store,jp,ibin,aer,gas,electrolyte)
       call degas_hcl(store,jp,ibin,aer,gas,electrolyte)
       call degas_nh3(store,jp,ibin,aer,gas)

    endif ! case 1, 2


    ! re-calculate ions to eliminate round-off errors
    call electrolytes_to_ions(jp, ibin,aer,electrolyte)
    !---------------------------------------------------------
    !
    ! calculate % composition  EFFI
    !!      sum_dum = 0.0
    !!      do je = 1, nelectrolyte
    !!        electrolyte(je,jp,ibin) = max(0.d0,electrolyte(je,jp,ibin)) ! remove -ve
    !!        sum_dum = sum_dum + electrolyte(je,jp,ibin)
    !!      enddo
    !!
    !!      if(sum_dum .eq. 0.)sum_dum = 1.0
    !!      electrolyte_sum(jp,ibin) = sum_dum
    !!
    !!      do je = 1, nelectrolyte
    !!        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/sum_dum
    !!      enddo
    !!
    !!
    return
  end subroutine conform_electrolytes



  !----------------------------------------------------------
  ! solution to x^3 + px^2 + qx + r = 0
  !
  function cubic( psngl, qsngl, rsngl )
    use module_data_mosaic_kind, only:  r8
    implicit none
    real(r8) :: cubic
    ! subr arguments
    real(r8) :: psngl, qsngl, rsngl
    ! local variables
    real(r8) :: p, q, r, A, B, D, M, N, third, y
    real(r8) :: k, phi, thesign, x(3), duma
    integer icase, kk

    third = 1.d0/3.d0

    q = (qsngl)
    p = (psngl)
    r = (rsngl)

    A = (1.d0/3.d0)*((3.d0*q) - (p*p))
    B = (1.d0/27.d0)*((2.d0*p*p*p) - (9.d0*p*q) + (27.d0*r))

    D = ( ((A*A*A)/27.d0) + ((B*B)/4.d0) )

    if(D .gt. 0.)then     !       => 1 real and 2 complex roots
       icase = 1
    elseif(D .eq. 0.)then !       => 3 real roots, atleast 2 identical
       icase = 2
    else  ! D < 0         => 3 distinct real roots
       icase = 3
    endif


    goto (1,2,3), icase

    ! case 1: D > 0
1   thesign = 1.
    if(B .gt. 0.)then
       B = -B
       thesign = -1.
    endif

    M = thesign*((-B/2.d0) + (sqrt(D)))**(third)
    N = thesign*((-B/2.d0) - (sqrt(D)))**(third)

    cubic = ( (M) + (N) - (p/3.d0) )
    return

    ! case 2: D = 0
2   thesign = 1.
    if(B .gt. 0.)then
       B = -B
       thesign = -1.
    endif

    M = thesign*(-B/2.d0)**third
    N = M

    x(1) = ( (M) + (N) - (p/3.d0) )
    x(2) = ( (-M/2.d0) + (-N/2.d0) - (p/3.d0) )
    x(2) = ( (-M/2.d0) + (-N/2.d0) - (p/3.d0) )

    cubic = 0.
    do kk = 1, 3
       if(x(kk).gt.cubic) cubic = x(kk)
    enddo
    return

    ! case 3: D < 0
3   if(B.gt.0.)then
       thesign = -1.
    elseif(B.lt.0.)then
       thesign = 1.
    endif

    ! rce 18-nov-2004 -- make sure that acos argument is between +/-1.0
    !     phi = acos(thesign*sqrt( (B*B/4.d0)/(-A*A*A/27.d0) ))   ! radians
    duma = thesign*sqrt( (B*B/4.d0)/(-A*A*A/27.d0) )
    duma = min( duma, +1.0d0 )
    duma = max( duma, -1.0d0 )
    phi  = acos( duma )   ! radians


    cubic = 0.
    do kk = 1, 3
       k = kk-1
       y = 2.*Sqrt(-A/3.)*cos(phi + 120.*k*0.017453293)
       x(kk) = ((y) - (p/3.d0))
       if(x(kk).gt.cubic) cubic = x(kk)
    enddo
    return

  end function cubic
   !----------------------------------------------------------


  !----------------------------------------------------------
  function quadratic(a,b,c)
    use module_data_mosaic_kind, only:  r8
    implicit none
    real(r8) :: quadratic
    ! subr. arguments
    real(r8) :: a, b, c
    ! local variables
    real(r8) :: x, dum, quad1, quad2


    if(b .ne. 0.0)then
       x = 4.*(a/b)*(c/b)
    else
       x = 1.e+6
    endif

    if(abs(x) .lt. 1.e-6)then
       dum = ( (0.5*x) +   &
            (0.125*x**2) +   &
            (0.0625*x**3) )

       quadratic = (-0.5*b/a)*dum

       if(quadratic .lt. 0.)then
          quadratic = -b/a - quadratic
       endif

    else
       quad1 = ((-b)+sqrt((b*b)-(4.*a*c)))/   &
            (2.*a)
       quad2 = ((-b)-sqrt((b*b)-(4.*a*c)))/   &
            (2.*a)

       quadratic = max(quad1, quad2)
    endif

    return
  end function quadratic
  !----------------------------------------------------------


  !----------------------------------------------------------
  function mean_molecular_speed(T, MW)    ! in cm/s
    use module_data_mosaic_kind, only:  r8
    implicit none
    real(r8) :: mean_molecular_speed
    ! subr. arguments
    real(r8) :: T, MW     ! T(K)

    mean_molecular_speed = 1.455e4 * sqrt(T/MW)

    return
  end function mean_molecular_speed
  !----------------------------------------------------------

  !----------------------------------------------------------
  function gas_diffusivity(T, P, MW, Vm)  ! in cm^2/s
    use module_data_mosaic_kind, only:  r8
    use module_data_mosaic_constants, only:  third
    implicit none
    real(r8) :: gas_diffusivity
    ! subr. arguments
    real(r8) :: MW, Vm, T, P      ! T(K), P(atm)


    gas_diffusivity = (1.0e-3 * T**1.75 * sqrt(1./MW + 0.035))/   &
         (P * (Vm**third + 2.7189)**2)


    return
  end function gas_diffusivity
  !----------------------------------------------------------


  !----------------------------------------------------------
  function fuchs_sutugin(rkn,a)
    use module_data_mosaic_kind, only:  r8
    implicit none
    real(r8) :: fuchs_sutugin
    ! subr. arguments
    real(r8) :: rkn, a
    ! local variables
    real(r8) :: rnum, denom


    rnum  = 0.75*a*(1. + rkn)
    denom = rkn**2 + rkn + 0.283*rkn*a + 0.75*a
    fuchs_sutugin = rnum/denom

    return
  end function fuchs_sutugin
  !----------------------------------------------------------


  !----------------------------------------------------------
  ! ZSR method at 60% RH
  !
  function aerosol_water_up( ibin, electrolyte, aer, kappa_nonelectro, a_zsr ) ! kg (water)/m^3 (air)

    use module_data_mosaic_aero, only: r8, nelectrolyte, naer, nbin_a_max, jtotal, &
        nsalt, ioc_a, ibc_a, ilim2_a, ioin_a, dens_aer_mac, mw_aer_mac ! RAZ 4/16/2014

    use module_data_mosaic_asecthp, only: dens_water_aer

    implicit none

    real(r8) :: aerosol_water_up
    ! subr. arguments
    integer, intent(in) :: ibin
    real(r8), intent(in), dimension (6,nelectrolyte) :: a_zsr
    real(r8), intent(in), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(in), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    ! local variables
    integer :: iaer, jp, je
    real(r8) :: tmpa, tmpb, aH2O_60  ! RAZ 4/16/2014
    ! function
    !real(r8) :: bin_molality_60


    aH2O_60 = 0.6

    jp = jtotal
    tmpa = 0.0_r8

    do je = 1, (nsalt+4)  ! include hno3 and hcl in water calculation
       tmpa = tmpa + electrolyte(je,jp,ibin)/bin_molality_60(je,a_zsr)
    enddo

!   tmpa = tmpa + &
!        ( (aer(ilim2_a,jp,ibin)/dens_aer_mac(ilim2_a))*kappa_nonelectro(ilim2_a) + &                      ! RCE 5/20/2015
!          (aer(ioin_a, jp,ibin)/dens_aer_mac(ioin_a ))*kappa_nonelectro(ioin_a ) + &                      !  "     "
!          (aer(ioc_a,  jp,ibin)/dens_aer_mac(ioc_a  ))*kappa_nonelectro(ioc_a  ) + &                      !  "     "
!          (aer(ibc_a,  jp,ibin)/dens_aer_mac(ibc_a  ))*kappa_nonelectro(ibc_a  ) )*aH2O_60/(1.0-aH2O_60)  ! RCE 5/20/2015

    tmpb = 0.0_r8
    do iaer = 1, naer
       if (kappa_nonelectro(iaer) > 0.0_r8) then
          tmpb = tmpb + (aer(iaer,jp,ibin)*mw_aer_mac(iaer)/dens_aer_mac(iaer))*kappa_nonelectro(iaer)
       end if
    end do
    tmpa = tmpa + tmpb * dens_water_aer * 1.0e-3 * aH2O_60/(1.0-aH2O_60)   ! RCE 1/6/2016 added 1.0e-3 factor

    aerosol_water_up = tmpa*1.e-9  ! kg(water)/m^3(air)

    return
  end function aerosol_water_up
  !----------------------------------------------------------


  !----------------------------------------------------------
  function bin_molality_60(je,a_zsr)            ! TOUCH
    use module_data_mosaic_aero, only: r8,nelectrolyte

    implicit none

    real(r8) :: bin_molality_60
    ! subr. arguments
    integer, intent(in) ::  je
    real(r8), intent(in), dimension (6,nelectrolyte) :: a_zsr
    ! local variables
    real(r8) :: aw, xm


    aw = 0.6_r8

    xm =     a_zsr(1,je) +   &
         aw*(a_zsr(2,je) +   &
         aw*(a_zsr(3,je) +   &
         aw*(a_zsr(4,je) +   &
         aw*(a_zsr(5,je) +   &
         aw* a_zsr(6,je) ))))

    bin_molality_60 = 55.509_r8*xm/(1. - xm)

    return
  end function bin_molality_60
  !----------------------------------------------------------

  
  !----------------------------------------------------------
  ! ZSR method
  function aerosol_water( jp, ibin, jaerosolstate, jphase, jhyst_leg, electrolyte, aer,   &
           kappa_nonelectro, num_a, mass_dry_a, mass_soluble_a, aH2O, molality0 ) ! kg (water)/m^3 (air). RAZ added aer

    use module_data_mosaic_aero, only: r8, nbin_a_max, nelectrolyte, nsoluble, naer,   &
         all_solid, jsolid, jhyst_lo, ioc_a, ibc_a, ilim2_a, ioin_a,   &   ! RAZ 4/16/2014
         jtotal, ah2o_max, dens_aer_mac, mw_aer_mac, ename

    use module_data_mosaic_asecthp, only: dens_water_aer

    implicit none

    real(r8) :: aerosol_water
    ! subr. arguments
    integer, intent(in) :: jp, ibin
    integer, intent(inout), dimension(nbin_a_max) :: jaerosolstate,jphase,jhyst_leg

    real(r8), intent(in) :: aH2O
    real(r8), intent(in), dimension(nbin_a_max) :: num_a,mass_dry_a,mass_soluble_a
    real(r8), intent(inout), dimension(nelectrolyte,nbin_a_max) :: molality0 !BSINGH(05/23/2014) - Added dimension nbin_a_max
    real(r8), intent(inout), dimension(nelectrolyte,3,nbin_a_max) :: electrolyte
    real(r8), intent(inout), dimension(naer,3,nbin_a_max) :: aer
    real(r8), intent(in), dimension(naer) :: kappa_nonelectro

    ! local variables
    integer :: iaer, iclm_aer, jclm_aer, je
    real(r8) :: tmpa, tmpb
    ! function
    real(r8) :: bin_molality



    tmpa = 0.0_r8
    if (jaerosolstate(ibin) .ne. all_solid) then                     ! RAZ 5/24/2016 - added this "if" check
! should this be "do je = 1, nsalt+4" as in aerosol_water_up ?
!             or "do je = 1, jxxxx" where jxxxx is the index of the last electrolyte used here ?
    do je = 1, 19   ! include hno3 and hcl in water calculation
       tmpa = tmpa + electrolyte(je,jp,ibin)/molality0(je,ibin)      ! RAZ 5/20/2014
    enddo
    endif

! note that this only considers the ilim2_a soa species

!   tmpa = tmpa + &
!       ( (aer(ioc_a,  jtotal,ibin)/dens_aer_mac(ioc_a  ))*kappa_nonelectro(ioc_a  ) +   &   ! RCE 5/20/2015
!         (aer(ilim2_a,jtotal,ibin)/dens_aer_mac(ilim2_a))*kappa_nonelectro(ilim2_a) +   &   ! RCE 5/20/2015
!         (aer(ioin_a, jtotal,ibin)/dens_aer_mac(ioin_a ))*kappa_nonelectro(ioin_a ) +   &   ! RCE 5/20/2015
!         (aer(ibc_a,  jtotal,ibin)/dens_aer_mac(ibc_a  ))*kappa_nonelectro(ibc_a  ) )   &   ! RCE 5/20/2015
!       * 1.0e-3 * aH2O/(1.0-min(ah2o,ah2o_max))                                             ! RCE 5/20/2015 - need 1.0e-3 factor

    tmpb = 0.0_r8
    do iaer = 1, naer
       if (kappa_nonelectro(iaer) > 0.0_r8) then
          tmpb = tmpb + (aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)/dens_aer_mac(iaer))*kappa_nonelectro(iaer)
       end if
    end do
    tmpa = tmpa + tmpb * dens_water_aer * 1.0e-3 * ah2o/(1.0-min(ah2o,ah2o_max))  ! ug(water)/m^3(air)

    aerosol_water = tmpa*1.e-9  ! kg(water)/m^3(air)

                 
    if (aerosol_water .le. 0.0) then !BALLI- Commented out to avoid slow runtime.
       iclm_aer = 0 !BSINGH- THIS IS WRONG!!!
       jclm_aer = 0 !BSINGH- THIS IS WRONG!!!
       !write(6,*)'iclm  jclm  ibin  jp = ',   &
       !     iclm_aer, jclm_aer, ibin, jp      !BSINGH- iclm_aer and jclm_aer are never set but they are used here.***

       !write(6,*)'aH2O, water = ', aH2O, aerosol_water
       !write(6,*)'dry mass = ', mass_dry_a(ibin)
       !write(6,*)'soluble mass = ', mass_soluble_a(ibin)
       !write(6,*)'number = ', num_a(ibin)
       !do je = 1, nsoluble
       !   write(6,44)ename(je), electrolyte(je,jp,ibin)
       !enddo
       !write(6,*)'Error in water calculation'
       !write(6,*)'ibin = ', ibin
       !write(6,*)'water content cannot be negative or zero'
       !write(6,*)'setting jaerosolstate to all_solid'

       !        call print_input

       jaerosolstate(ibin) = all_solid
       jphase(ibin)    = jsolid
       jhyst_leg(ibin) = jhyst_lo

    endif

44  format(a7, 2x, e11.3)


    return
  end function aerosol_water





  !----------------------------------------------------------
  function bin_molality(je,ibin,aH2O_a,b_zsr,a_zsr,aw_min)
    use module_data_mosaic_aero, only:r8,  nbin_a_max, nelectrolyte

    implicit none

    real(r8) :: bin_molality
    ! subr. arguments
    integer, intent(in) :: je, ibin
    real(r8), intent(in), dimension(nbin_a_max) :: aH2O_a
    real(r8), intent(in), dimension(nelectrolyte) :: b_zsr,aw_min
    real(r8), intent(in), dimension (6,nelectrolyte) :: a_zsr
    ! local variables
    real(r8) :: aw, xm


    aw = max(aH2O_a(ibin), aw_min(je))
    aw = min(aw, 0.999999_r8)


    if(aw .lt. 0.97_r8)then

       xm =     a_zsr(1,je) +   &
            aw*(a_zsr(2,je) +   &
            aw*(a_zsr(3,je) +   &
            aw*(a_zsr(4,je) +   &
            aw*(a_zsr(5,je) +   &
            aw* a_zsr(6,je) ))))

       bin_molality = 55.509_r8*xm/(1. - xm)

    else

       bin_molality = -b_zsr(je)*log(aw)

    endif


    return
  end function bin_molality
  !----------------------------------------------------------







end module module_mosaic_ext
