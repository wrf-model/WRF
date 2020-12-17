! module_cam_mam_aerchem_driver.F
! created by r.c.easter, june 2010
!
! 2010-07-03 notes:
!
!--------------------------------------------------------------

      module module_cam_mam_aerchem_driver

      private
      public :: cam_mam_aerchem_driver

      contains


!==============================================================
      subroutine cam_mam_aerchem_driver(                            &
         id, curr_secs, ktau, dtstep, ktauc, dtstepc, config_flags, &
         t_phy, rho_phy, p_phy, p8w, alt, z, z_at_w, pbl_h, cldfra, &
         cldfra_mp_all, moist, chem,                                &
         dgnum, dgnumwet, wetdens_ap, del_h2so4_gasprod,            &
         dvmrdt_sv13d, dvmrcwdt_sv13d,                              & !Output from cloud chemistry
         is_CAMMGMP_used,                                           &!BSINGH:01/31/2013: Added is_CAMMGMP_used 
         ids,ide, jds,jde, kds,kde,                                 &
         ims,ime, jms,jme, kms,kme,                                 &
         its,ite, jts,jte, kts,kte                                  )
!-----------------------------------------------------------------------
! DESCRIPTION
!
! cam_mam_aerchem_driver is the interface between wrf-chem and the
!   cam_mam aerosol-chemistry routine cat computes condensation/evaporation
!   of trace gases to/from aerosol particles (AP).  It currently treats
!   water vapor and the 4 inorganic trace gases (nh3, h2so4, hno3, and hcl).
!   The aerosol-chemistry routine can work with either a sectional
!   (multiple size bins) or modal (multiple modes) representation.  
!
!   In both cases, condensation/evaporation to/from each bins/mode is 
!   first computed.  For sectional representation, AP mass and number 
!   are then transferred between size bins as a result of AP 
!   positive/negative growth.  Either a moving-center or two-moment
!   algorithm can be used to compute this transfer.
!
! cam_mam_aerchem_driver is organized as follows
!   loop over j and i
!      call aerchemistry to do the aerosol chemistry calculations
!          for timestep = dtstepc
!
!-----------------------------------------------------------------------

      use module_configure, only:  &
            grid_config_rec_type,                         &
            p_qv

      use module_state_description, only:  num_moist, num_chem, &
                                           param_first_scalar

      use shr_kind_mod, only: r8 => shr_kind_r8
      use module_cam_support, only: pcols, pver, pverp, &
                                    pcnst => pcnst_runtime, &
                                    pcnst_non_chem => pcnst_non_chem_modal_aero, &
                                    gas_pcnst => gas_pcnst_modal_aero, &
                                    gas_pcnst_pos => gas_pcnst_modal_aero_pos, &
                                    endrun
      use constituents, only: cnst_name, cnst_get_ind
      use physconst, only: mwdry, pi

      use modal_aero_calcsize,    only:  modal_aero_calcsize_sub
      use modal_aero_coag,        only:  modal_aero_coag_sub
      use modal_aero_gasaerexch , only:  modal_aero_gasaerexch_sub
      use modal_aero_newnuc,      only:  modal_aero_newnuc_sub
      use modal_aero_wateruptake, only:  modal_aero_wateruptake_sub

      use modal_aero_data, only:  &
            alnsg_amode, dgnum_amode, dgnumlo_amode, dgnumhi_amode, &
            lmassptr_amode, lspectype_amode, &
            lptr_nacl_a_amode, lptr_so4_a_amode, lptr_soa_a_amode, &
            ntot_amode, nspec_amode, ntot_amode, numptr_amode, &
            specdens_amode, spechygro

      use module_data_cam_mam_asect, only:  &
            ai_phase, &
            factconv_chem_to_q, factconv_chem_to_qqcw, &
            lptr_chem_to_q, lptr_chem_to_qqcw, &
            massptr_aer, mw_q_array, mw_q_mo_array, &
            nsize_aer, ntype_aer, numptr_aer, waterptr_aer

      implicit none

!   subr arguments
      type(grid_config_rec_type), intent(in) :: config_flags
!   config_flags - configuration and control parameters
      logical, intent(in) :: is_CAMMGMP_used
      integer, intent(in) ::             &
         id, ktau, ktauc,                &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         its, ite, jts, jte, kts, kte
!   id - domain index
!   ktau - time step number
!   ktauc - gas and aerosol chemistry time step number

!   [ids:ide, kds:kde, jds:jde] - spatial (x,z,y) indices for "domain"
!   [ims:ime, kms:kme, jms:jme] - spatial (x,z,y) indices for "memory"
!      Most arrays that are arguments to chem_driver 
!      are dimensioned with these spatial indices.
!   [its:ite, kts:kte, jts:jte] - spatial (x,z,y) indices for "tile"
!      chem_driver and routines under it do calculations
!      over these spatial indices.

      real(kind=8), intent(in) :: curr_secs
      real, intent(in) :: dtstep, dtstepc
!   dtstep - main model time step (s)
!   dtstepc - time step for gas and aerosol chemistry(s)

      real, intent(in),   &
         dimension( ims:ime, kms:kme, jms:jme ) :: &
         t_phy, rho_phy, p_phy, p8w, alt, &
         z, z_at_w, cldfra, cldfra_mp_all
!   t_phy - temperature at layer center (K)
!   rho_phy - air density at layer center (kg/m^3)
!   p_phy - air pressure at layer center (Pa)
!   p8w - air pressure at layer interfaces (Pa)
!   alt - dry air specific volume at layer center (m^3/kg)
!   z - height at layer center (m msl)
!   z_at_w - height at layer interfaces (m msl)
!   cldfra - cloud fraction of layer (--)
!   cldfra_mp_all- cloud fraction from CAMMGMP

      real, intent(in),   &
         dimension( ims:ime, jms:jme ) :: &
         pbl_h
!   pbl_h - pbl height (m agl)

      real, intent(in),   &
         dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
         moist
!   moist - mixing ratios of moisture species (water vapor, 
!      cloud water, ...) (kg/kg for mass species, #/kg for number species)
 
      real, intent(inout),   &
         dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
         chem
!   chem - mixing ratios of trace gase (ppm) and aerosol species
!      (ug/kg for mass species, #/kg for number species)

      real, intent(inout),   &
         dimension( ims:ime, kms:kme, jms:jme, ntot_amode ) :: &
         dgnum, dgnumwet, wetdens_ap
!   dgnum - median dry-diameter of number distribution for mode (m)
!   dgnumwet - median wet-diameter of number distribution for mode (m)
!   wetdens_ap - wet density of mode (kg/m^3)
      real, intent(in),   &
         dimension( ims:ime, kms:kme, jms:jme ) :: &
         del_h2so4_gasprod
!   del_h2so4_gasprod - h2so4 change from gas-phase chemistry (ppmv)

!
!tendencies:dvmrdt_sv13d,dvmrcwdt_sv13d are the tendencies which are passsed on from the CAM-MAM cloud chemistry
!           to gasaerexch subroutine 
!
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:gas_pcnst_pos )   :: dvmrdt_sv13d,dvmrcwdt_sv13d 

!-----------------------------------------------------------------------
!   local variables
      integer :: aerchem_onoffb, aercoag_onoffb, aernewnuc_onoffb
      integer, parameter :: debug_level=0
      integer, parameter :: icol = 1
      integer, parameter :: idiag_calcsize=0, idiag_wateruptake=0
      integer, parameter :: idiag_gasaerexch=0, idiag_newnuc=0, idiag_coag=0
      integer :: i, icalcaer_flag, istat, itmpa, iwaterup_flag
      integer :: idiagaa, idiagaa_idel, idiagaa_jdel
      integer :: imozart, imozart_m1
      integer :: j
      integer :: k, kcam
      integer :: l, l1, l2, l3, lchnk, loffset
      integer :: l_mo_h2so4, l_mo_soag
      integer :: latndx(pcols), lonndx(pcols)
      integer :: n, ncol, nstep
      integer :: p1st
      
      real(r8) :: cld8(pcols,pver)
      real(r8) :: deltat8
      real(r8) :: del_h2so4_gasprod8(pcols,pver)
      real(r8) :: del_h2so4_aeruptk8(pcols,pver)
      real(r8) :: dgnum8(pcols,pver,ntot_amode)
      real(r8) :: dgnumwet8(pcols,pver,ntot_amode)
      real(r8) :: dqdt8(pcols,pver,pcnst)
      real(r8) :: dvmrdt_sv18(pcols,pver,gas_pcnst_pos), &
                  dvmrcwdt_sv18(pcols,pver,gas_pcnst_pos)
      real(r8) :: fconv
      real(r8) :: pblh8(pcols)
      real(r8) :: pdel8(pcols,pver), pmid8(pcols,pver)
      real(r8) :: qaerwat8(pcols,pver,ntot_amode)
      real(r8) :: q8(pcols,pver,pcnst), qqcw8(pcols,pver,pcnst)
      real(r8) :: qv8(pcols,pver)
      real(r8) :: rh8(pcols,pver)
      real(r8) :: tmpa, tmpb
      real(r8) :: tmpveca(gas_pcnst), tmpvecb(gas_pcnst)
      real(r8) :: tmpvmra(pver,gas_pcnst)
      real(r8) :: tmp_dgnum(ntot_amode), tmp_dstar(ntot_amode)
      real(r8) :: tmp_hygro(pver,ntot_amode)
      real(r8) :: tmp_num(ntot_amode), tmp_vdry(ntot_amode)
      real(r8) :: t8(pcols,pver)
      real(r8) :: vmr8(pcols,pver,gas_pcnst), vmrcw8(pcols,pver,gas_pcnst)
      real(r8) :: wetdens_ap8(pcols,pver,ntot_amode)
      real(r8) :: zm8(pcols,pver)

      logical :: aero_mmr_flag
      logical :: dotend(pcnst)
      logical :: h2o_mmr_flag

      character*100 msg


      itmpa = config_flags%aerchem_onoff
      if (itmpa <= 0) return

      aerchem_onoffb = 1
      aercoag_onoffb = 1
      aernewnuc_onoffb = 1
      if (itmpa == 99) then
         aerchem_onoffb = 0
      else if (itmpa >= 100) then
         aerchem_onoffb   = mod(itmpa/100,10)  ! 100s digit
         aernewnuc_onoffb = mod(itmpa/10, 10)  !  10s digit
         aercoag_onoffb   = mod(itmpa,    10)  !   1s digit
      end if
      if (aerchem_onoffb <= 0) then
         aercoag_onoffb = 0
         aernewnuc_onoffb = 0
      end if

      write(*,'(/a,3i5)') 'cam_mam_aerchem_driver - id, ktau, ktauc =', id, ktau, ktauc
      write(*,'(a,3(4x,2i5))') 'ids/e, j..., k... ', ids,ide, jds,jde, kds,kde
      write(*,'(a,3(4x,2i5))') 'ims/e, j..., k... ', ims,ime, jms,jme, kms,kme
      write(*,'(a,3(4x,2i5))') 'its/e, j..., k... ', its,ite, jts,jte, kts,kte
      write(*,'(a,3i10     )') 'pver, pverp, pcols       ', pver, pverp, pcols
      write(*,'(a,3i10     )') 'aerchem/newnuc/coag_onoffb', &
         aerchem_onoffb, aernewnuc_onoffb, aercoag_onoffb


      p1st = param_first_scalar
      imozart_m1 = pcnst_non_chem
      imozart = imozart_m1 + 1

      call cnst_get_ind( 'h2so4', l_mo_h2so4, .false. )
      l_mo_h2so4 = l_mo_h2so4 - imozart_m1
      if ((l_mo_h2so4 < 1) .or. (l_mo_h2so4 > gas_pcnst)) &
         call endrun( &
            'cam_mam_aerchem_driver error -- no h2so4 species' )
      write(*,*) 'l_mo_h2so4 = ', l_mo_h2so4

      call cnst_get_ind( 'soag', l_mo_soag, .false. )
      l_mo_soag = l_mo_soag - imozart_m1
      if ((l_mo_soag < 1) .or. (l_mo_soag > gas_pcnst)) &
         call endrun( &
            'cam_mam_aerchem_driver error -- no soag species' )
      write(*,*) 'l_mo_soag = ', l_mo_soag



!--------------------------------------------------------------------
!
! Main loop over the points in the tile and treat them each as a CAM chunk
!
!--------------------------------------------------------------------
      do 2920 j = jts, jte
!     write( msg, '(a,3i5)' ) 'cam_mam_aerchem_driver 2920 loop - j =', j
!     call wrf_debug( 100, msg )
      do 2910 i = its, ite

      lchnk = (j-jts)*(ite-its+1) + (i-its+1) !1-D index location from the 2-D tile
      ncol = 1

      latndx(icol) = j
      lonndx(icol) = i

      ! initialize these arrays to zero
      q8(:,:,:) = 0.0_r8
      qqcw8(:,:,:) = 0.0_r8
      qaerwat8(:,:,:) = 0.0_r8
      
      dgnum8(:,:,:) = 0.0_r8
      dgnumwet8(:,:,:) = 0.0_r8
      wetdens_ap8(:,:,:) = 0.0_r8

      ! pblh8 is msl, pbl_h is agl
      pblh8(icol) = pbl_h(i,j) + z_at_w(i,kts,j)
      ! force it to be at least the top of lowest layer
      pblh8(icol) = max( pblh8(icol), 1.0_r8*z_at_w(i,kts+1,j) )

      ! load the cam layer-center variables
      ! (reverse the vertical index, and convert units as needed)
      do k = kts, kte
         kcam = kte-k+1

!        cld8(icol,kcam)  = cldfra(i,k,j)
!        qh8(icol,kcam,1) = max( qv(i,k,j)/(1.+qv(i,k,j)), 1e-30 ) !values of 0 cause a crash in entropy
!        if( present(qc) ) then
!           ql8(icol,kcam) = qc(i,k,j)/(1.+qv(i,k,j)) !Convert to moist mix ratio
!        else
!           ql8(icol,kcam) = 0.
!        end if
!        if( present(qi) ) then
!           qi8(icol,kcam) = qi(i,k,j)/(1.+qv(i,k,j)) !Used in convtran, ditto for conversion
!        else
!           qi8(icol,kcam) = 0.
!        end if
!        zm8(icol,kcam)   = z(i,k,j)

         t8(icol,kcam)    = t_phy(i,k,j)
         pdel8(icol,kcam) = p8w(i,k,j) - p8w(i,k+1,j)
         pmid8(icol,kcam) = p_phy(i,k,j)
         zm8(icol,kcam)   = z(i,k,j)
         if(is_CAMMGMP_used) then
            cld8(icol,kcam) = cldfra_mp_all(i,k,j)
         else
            cld8(icol,kcam) = cldfra(i,k,j)
         endif
            
!         cld8(icol,kcam) = 0.0   ! *** temporary

         qv8(icol,kcam) = moist(i,k,j,p_qv)/(1.+moist(i,k,j,p_qv)) ! convert from dry to moist mix ratio

         do n = 1, ntype_aer
            dgnum8(icol,kcam,n) = dgnum(i,k,j,n)
            dgnumwet8(icol,kcam,n) = dgnumwet(i,k,j,n)
            wetdens_ap8(icol,kcam,n) = wetdens_ap(i,k,j,n)

            l = waterptr_aer(1,n)
            if ((l >= p1st) .and. (l <= num_chem)) &
               qaerwat8(icol,kcam,n) = chem(i,k,j,l)*factconv_chem_to_q(l)
         end do ! n

         do l = p1st, num_chem
            l2 = lptr_chem_to_q(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then
               q8(icol,kcam,l2) = chem(i,k,j,l)*factconv_chem_to_q(l)
            end if
            if (i*j==1 .and. k==kts .and. l2==l_mo_h2so4+pcnst_non_chem) &
               write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 chem & q8', &
               chem(i,k,j,l), q8(icol,kcam,l2)
            if (i*j==1 .and. k==kts .and. l2==l_mo_soag+pcnst_non_chem) &
               write(*,'(a,1p,2e11.3)') '1,1,1 soag  chem & q8', &
               chem(i,k,j,l), q8(icol,kcam,l2)
!            cycle ! this is temporary

            l2 = lptr_chem_to_qqcw(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then
               qqcw8(icol,kcam,l2) = chem(i,k,j,l)*factconv_chem_to_q(l)
            end if
         end do ! l

         del_h2so4_gasprod8(icol,kcam) = del_h2so4_gasprod(i,k,j)
         if(config_flags%cldchem_onoff == 1 .and. config_flags%chem_opt >= 501 .AND. config_flags%chem_opt <= 504) then
            do l = 1 , gas_pcnst
               dvmrdt_sv18(1,kcam,l)   = dvmrdt_sv13d(i,k,j,l)
               dvmrcwdt_sv18(1,kcam,l) = dvmrcwdt_sv13d(i,k,j,l)
            enddo
         else
            dvmrdt_sv18(1,kcam,:)   = 0.0_r8
            dvmrcwdt_sv18(1,kcam,:) = 0.0_r8 
         endif
      end do ! k

      ! load the cam layer-interface variables
      ! (reverse the vertical index, and convert units as needed)
      do k = kts, kte+1
         kcam = kte-k+2

!        pint8(icol,kcam) = p8w(i,k,j)
!        zi8(icol,kcam)   = z_at_w(i,k,j)
      end do



!--------------------------------------------------------------------
!
! do modal_aero_calcsize_sub
!
!--------------------------------------------------------------------
      idiagaa = 1 ; idiagaa_idel = 100 ; idiagaa_jdel = 100

      nstep = ktau
      icalcaer_flag = 1
      loffset = 0             ! using entire q array, not the mozart "chem subset"
      aero_mmr_flag = .true.  ! q values are mass mixing ratios (kg/kg)

      deltat8 = dtstepc

      dotend(:) = .false.
      dqdt8(:,:,:) = 0.0_r8

      ! diagnostics for initial testing
      if (idiag_calcsize > 0) then
      if ((i==its) .and. (j==jts)) then
!        deltat8 = 86400.0*0.1   ! for testing, use bigger step size to see bigger changes
         do kcam = kts, kte, (kte-kts)
            k = kte-kcam+1
            tmp_num(1:ntot_amode) = q8(icol,kcam,numptr_amode(1:ntot_amode))
            ! adjust number mixrat here for testing
!           q8(icol,k,numptr_amode(1)) = q8(icol,k,numptr_amode(1)) * 1.0_r8
!           q8(icol,k,numptr_amode(2)) = q8(icol,k,numptr_amode(2)) / 8.0_r8
!           q8(icol,k,numptr_amode(3)) = q8(icol,k,numptr_amode(3)) * 8.0_r8
            do n = 1, ntot_amode
               ! calc dry volume
               tmp_vdry(n) = 0.0
               tmp_hygro(kcam,n) = 0.0
               do l1 = 1, nspec_amode(n)
                  l2 = lmassptr_amode(l1,n)
                  l3 = lspectype_amode(l1,n)
                  tmpa = q8(icol,kcam,l2)/specdens_amode(l3)
                  tmp_vdry(n) = tmp_vdry(n) + tmpa
                  tmp_hygro(kcam,n) = tmp_hygro(kcam,n) + tmpa*spechygro(l3)
               end do
               tmp_hygro(kcam,n) = tmp_hygro(kcam,n)/max(tmp_vdry(n),1.0e-35_r8)
               ! calc volume mean and number median diameter
               tmpa = tmp_vdry(n)*6.0/( pi*q8(icol,kcam,numptr_amode(n)) )
               tmp_dstar(n) = tmpa**(1.0/3.0)
               tmpb = alnsg_amode(n)
               tmp_dgnum(n) = tmp_dstar(n) * exp(-1.5*tmpb*tmpb)
            end do

            write(*,'(/a,4i5,1p,e12.4)') &
               'calcsize diagnostics before, i, j, k, kcam, deltat =', &
               i, j, k, kcam, deltat8
            write(*,'(a,1p,7e12.4))') 'tmp_vdry  ', &
               ( tmp_vdry(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'tmp_dstar ', &
               ( tmp_dstar(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'tmp_dgnum ', &
               ( tmp_dgnum(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnumlo   ', &
               ( dgnumlo_amode(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnumhi   ', &
               ( dgnumhi_amode(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnum     ', &
               ( dgnum8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'number    ', &
               ( q8(icol,kcam,numptr_amode(n)), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'tmp_num   ', &
               ( tmp_num(n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'q         '
            write(*,'(5(2x,a,1p,e12.4))') ( cnst_name(l)(1:6), &
               q8(icol,kcam,l), l=lmassptr_amode(1,1), pcnst )
         end do
      end if
      end if !(idiag_calcsize > 0)

      if (idiagaa > 0) then
         if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
            print 93010, 'calling modal_aero_calcsize_sub - i,j =', i, j
      end if

!
! this is the call done in cam
!   call modal_aero_calcsize_sub(                                &
!                      lchnk,   ncol,    nstep,                  &
!                      1,                0,                      &
!                      .true.,           dt,                     &
!                      state%t,          state%pmid,             &
!                      state%pdel,       state%q,                &
!                      ptend%q,          ptend%lq,               &
!                      qqcw,             dgnum_pbuf              )
!
! this is the subr in wrfchem
!   subr------ modal_aero_calcsize_sub(                          &
!                      lchnk,   ncol,    nstep,   icalcaer_flag, &
!                      loffset,                                  &
!                      aero_mmr_flag,    deltat,                 &
!                      t,       pmid,    pdel,    q,             &
!                      dqdt,    dotend,  qqcw,    dgncur_a       )
!
      call modal_aero_calcsize_sub(                  &
           lchnk,   ncol,    nstep,   icalcaer_flag, &
           loffset,                                  &
           aero_mmr_flag,    deltat8,                &
           t8,      pmid8,   pdel8,   q8,            &
           dqdt8,   dotend,  qqcw8,   dgnum8         )

      if (idiagaa > 0) then
         if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
            print 93010, 'backfrm modal_aero_calcsize_sub - i,j =', i, j
      end if

      ! diagnostics for initial testing
      if (idiag_calcsize > 0) then
      if ((i==its) .and. (j==jts)) then
         do kcam = kts, kte, (kte-kts)
            k = kte-kcam+1
            write(*,'(/a,4i5)') &
               'calcsize diagnostics after , i, j, k, kcam =', i, j, k, kcam
            write(*,'(a,1p,7e12.4))') 'dgnum     ', &
               ( dgnum8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'number    ', &
               ( q8(icol,kcam,numptr_amode(n)), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') ' +dt*tend ', &
               ( q8(icol,kcam,numptr_amode(n))+ &
                 deltat8*dqdt8(icol,kcam,numptr_amode(n)), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'q+dt*tend '
            write(*,'(5(2x,a,1p,e12.4))') ( cnst_name(l)(1:6), &
               q8(icol,kcam,l)+deltat8*dqdt8(icol,kcam,l), l=lmassptr_amode(1,1), pcnst )
         end do
         write(*,'(a)') 
      end if
      end if !(idiag_calcsize > 0)

! apply tendencies
      do l = 1, pcnst
         if ( .not. dotend(l) ) cycle
         do k = 1, pver
            q8(icol,k,l) = q8(icol,k,l) + deltat8*dqdt8(icol,k,l)
            q8(icol,k,l) = max( q8(icol,k,l), 0.0_r8 )
         end do
      end do



!--------------------------------------------------------------------
!
! do modal_aero_wateruptake_sub
!
!--------------------------------------------------------------------
#ifdef OBSRH
! the modal_aero_wateruptake_sub OBSRH option is not implemented in wrf-chem
      msg = 'cam_mam_aerchem_driver calling modal_aero_wateruptake_sub' // &
            ' -- cannot do OBSRH'
      call wrf_error_fatal( msg )
#endif

      iwaterup_flag = 1
      h2o_mmr_flag = .true.

      dotend(:) = .false.
      dqdt8(:,:,:) = 0.0_r8
      rh8(:,:) = 0.0_r8

      ! diagnostics for initial testing
      if (idiag_wateruptake > 0) then
      if ((i==its) .and. (j==jts)) then
         do kcam = kts, kte, (kte-kts)
!           qv8(icol,kcam) = qv8(icol,kcam)*2.0   ! for testing
            k = kte-kcam+1
            write(*,'(/a,4i5,1p,e12.4)') &
               'wateruptake diagnostics before, i, j, k, kcam, deltat =', &
               i, j, k, kcam, deltat8
            write(*,'(a,1p,7e12.4))') 'tmp_hygro ', &
               ( tmp_hygro(kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'qaerwat   ', &
               ( qaerwat8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnum     ', &
               ( dgnum8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnumwet  ', &
               ( dgnumwet8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'wetdens_ap', &
               ( wetdens_ap8(icol,kcam,n), n=1,ntot_amode )
         end do
      end if
      end if ! (idiag_wateruptake > 0)

      if (idiagaa > 0) then
         if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
            print 93010, 'calling modal_aero_wateruptake_sub - i,j =', i, j
      end if

!
! this is the call done in cam
!   call modal_aero_wateruptake_sub(                             &
!                      lchnk,   ncol,    nstep,                  &
!                      1,                0,                      &
!                      .true.,           .true.,                 &
!                      dt,               state%q(:,:,1),         &
!                      state%t,          state%pmid,             &
!                      state%pdel, cldn, state%q,                &
!                      ptend%q,          ptend%lq,               &
!                      qaerwat,                                  &
!                      dgnum_pbuf,       dgnumwet_pbuf,          &
!                      wetdens_pbuf                              )
!
! this is the subr in wrfchem
!     subr------ modal_aero_wateruptake_sub(                &
!                lchnk, ncol, nstep,                        &
!                iwaterup_flag, loffset,                    &
!                aero_mmr_flag, h2o_mmr_flag,               &
!                deltat, h2ommr, t, pmid, pdel, cldn,       &
!                raer, raertend, dotend, qaerwat,           &
!                dgncur_a, dgncur_awet, wetdens             &
!#ifdef OBSRH
!                , obsrh                                    &
!#endif
!                                                           )
!
      call modal_aero_wateruptake_sub(                &
           lchnk, ncol, nstep,                        &
           iwaterup_flag, loffset,                    &
           aero_mmr_flag, h2o_mmr_flag,               &
           deltat8, qv8, t8, pmid8, pdel8, cld8,      &
           q8, dqdt8, dotend, qaerwat8,               &
           dgnum8, dgnumwet8, wetdens_ap8             )

      if (idiagaa > 0) then
         if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
            print 93010, 'backfrm modal_aero_wateruptake_sub - i,j =', i, j
      end if

      ! diagnostics for initial testing
      if (idiag_wateruptake > 0) then
      if ((i==its) .and. (j==jts)) then
         do kcam = kts, kte, (kte-kts)
            k = kte-kcam+1
            write(*,'(/a,4i5,1p,e12.4)') &
               'wateruptake diagnostics after , i, j, k, kcam, rh =', &
               i, j, k, kcam, rh8(icol,kcam)
            write(*,'(a,1p,7e12.4))') 'qaerwat   ', &
               ( qaerwat8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnum     ', &
               ( dgnum8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'dgnumwet  ', &
               ( dgnumwet8(icol,kcam,n), n=1,ntot_amode )
            write(*,'(a,1p,7e12.4))') 'wetdens_ap', &
               ( wetdens_ap8(icol,kcam,n), n=1,ntot_amode )
         end do
         write(*,'(a/(10f8.4))') 'rh(pver:1)', &
               ( rh8(icol,kcam), kcam=pver,1,-1 )
      end if
      end if ! (idiag_wateruptake > 0)



!--------------------------------------------------------------------
!
! do modal_aero_gasaerexch_sub
!
!--------------------------------------------------------------------
      if (aerchem_onoffb > 0) then

! transfer mass mixing ratios in q8 and qqcw8 to molar mixing ratios in vmr8 and vmrcw8
         do l2 = pcnst_non_chem+1, pcnst
            l3 = l2 - pcnst_non_chem
            fconv = mwdry/mw_q_array(l2)
            vmr8(  icol,:,l3) = q8(   icol,:,l2)*fconv
            vmrcw8(icol,:,l3) = qqcw8(icol,:,l2)*fconv
            if (i*j==1 .and. k==kts .and. l3==l_mo_h2so4) &
               write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 q8 & vmr8', &
               q8(icol,pver,l2), vmr8(icol,pver,l3)
            if (i*j==1 .and. k==kts .and. l3==l_mo_soag) &
               write(*,'(a,1p,2e11.3)') '1,1,1 soag  q8 & vmr8', &
               q8(icol,pver,l2), vmr8(icol,pver,l3)
         end do

! also convert this 
         fconv = mwdry/mw_q_mo_array(l_mo_h2so4)
         del_h2so4_gasprod8(icol,:) = del_h2so4_gasprod8(icol,:)*fconv

! save current h2so4 vmr
         del_h2so4_aeruptk8(icol,:) = vmr8(icol,:,l_mo_h2so4)
         tmpvmra(:,:) = vmr8(icol,:,:)

! following is the cam code in mo_gas_phase_chemdr.F90
!        if (l_mo_h2so4 > 0) then
!           del_h2so4_aeruptk(1:ncol,:) = vmr(1:ncol,:,l_mo_h2so4)
!        else
!           del_h2so4_aeruptk(:,:) = 0.0_r8
!        endif
!        call modal_aero_gasaerexch_sub(                         &
!                          lchnk,    ncol,     nstep,            &
!                          imozart-1,          delt,             &
!                          latndx,   lonndx,                     &
!                          tfld,     pmid,     pdel,             &
!                          vmr,                vmrcw,            &
!                          dvmrdt_sv1,         dvmrcwdt_sv1,     &
!                          dgnum_pbuf,         dgnumwet_pbuf     )
!        if (l_mo_h2so4 > 0) then
!           del_h2so4_aeruptk(1:ncol,:) = vmr(1:ncol,:,l_mo_h2so4) - del_h2so4_aeruptk(1:ncol,:)
!        endif

! make call
         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'calling modal_aero_gasaerexch_sub - i,j =', i, j
         end if

         call modal_aero_gasaerexch_sub(                            &
                           lchnk,     ncol,       nstep,            &
                           imozart_m1,            deltat8,          &
                           latndx,    lonndx,                       &
                           t8,        pmid8,      pdel8,            &
                           vmr8,                  vmrcw8,           &
                           dvmrdt_sv18,           dvmrcwdt_sv18,    &
                           dgnum8,                dgnumwet8,        &
                           gas_pcnst_pos                             )

         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'backfrm modal_aero_gasaerexch_sub - i,j =', i, j
         end if

! diagnostic output after gasaerexch
         if (idiag_gasaerexch > 0) then
         if ((i == its) .and. (j == jts)) then
         do kcam = 1, pver
            if ((kcam /= pver) .and. (kcam /= pver-10)) cycle

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_so4_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(2) = tmpvmra(kcam,l_mo_h2so4)
            tmpvecb(2) = vmr8(icol,kcam,l_mo_h2so4)
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(/a,i3,1p,e13.5,10e11.3)') 'kcam, old so4 ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'( a,i3,1p,e13.5,10e11.3)') 'kcam, new so4 ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_soa_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(2) = tmpvmra(kcam,l_mo_soag)
            tmpvecb(2) = vmr8(icol,kcam,l_mo_soag)
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old soa ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new soa ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_nacl_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old ncl ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new ncl ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = numptr_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old num ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new num ...', &
               kcam, tmpvecb(1:ntot_amode+2)

         end do ! kcam
         end if ! ((i == its) .and. (j == jts))
         end if ! (idiag_gasaerexch > 0)

! calc change to h2so4 vmr
         del_h2so4_aeruptk8(icol,:) = &
            vmr8(icol,:,l_mo_h2so4) - del_h2so4_aeruptk8(icol,:)

      end if ! (aerchem_onoffb > 0)



!--------------------------------------------------------------------
!
! do modal_aero_newnuc_sub
!
!--------------------------------------------------------------------
      if (aernewnuc_onoffb > 0) then
         tmpvmra(:,:) = vmr8(icol,:,:)

         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'calling modal_aero_newnuc_sub - i,j =', i, j
         end if

!     subr------ modal_aero_newnuc_sub(                             &
!                          lchnk,    ncol,     nstep,               &
!                          pcnstxx,  loffset,  deltat,              &
!                          latndx,   lonndx,                        &
!                          t,        pmid,     pdel,                &
!                          zm,       pblh,                          &
!                          qv,       cld,                           &
!                          q,                                       &
!                          del_h2so4_gasprod,  del_h2so4_aeruptk    )
         call modal_aero_newnuc_sub(                                &
                           lchnk,     ncol,       nstep,            &
                           imozart_m1,            deltat8,          &
                           latndx,    lonndx,                       &
                           t8,        pmid8,      pdel8,            &
                           zm8,       pblh8,                        &
                           qv8,       cld8,                         &
                           vmr8,                                    &
                           del_h2so4_gasprod8,  del_h2so4_aeruptk8, &
                           gas_pcnst                                )

         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'backfrm modal_aero_newnuc_sub - i,j =', i, j
         end if

! diagnostic output after newnuc
         if (idiag_newnuc > 0) then
         if ((i == its) .and. (j == jts)) then
         do kcam = 1, pver
            if ((kcam /= pver) .and. (kcam /= pver-10)) cycle

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_so4_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(2) = tmpvmra(kcam,l_mo_h2so4)
            tmpvecb(2) = vmr8(icol,kcam,l_mo_h2so4)
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(/a,i3,1p,e13.5,10e11.3)') 'kcam, old so4 ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'( a,i3,1p,e13.5,10e11.3)') 'kcam, new so4 ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_soa_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(2) = tmpvmra(kcam,l_mo_soag)
            tmpvecb(2) = vmr8(icol,kcam,l_mo_soag)
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old soa ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new soa ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_nacl_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old ncl ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new ncl ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = numptr_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old num ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new num ...', &
               kcam, tmpvecb(1:ntot_amode+2)

         end do ! kcam
         end if ! ((i == its) .and. (j == jts))
         end if ! (idiag_newnuc > 0)

      end if ! (aernewnuc_onoffb > 0)



!--------------------------------------------------------------------
!
! do modal_aero_coag_sub
!
!--------------------------------------------------------------------
      if (aercoag_onoffb > 0) then
         tmpvmra(:,:) = vmr8(icol,:,:)

         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'calling modal_aero_coag_sub - i,j =', i, j
         end if

!     subr------ modal_aero_coag_sub(                               &
!                          lchnk,    ncol,     nstep,               &
!                          pcnstxx,  loffset,  deltat_main,         &
!                          latndx,   lonndx,                        &
!                          t,        pmid,     pdel,                &
!                          q,                                       &
!                          dgncur_a,           dgncur_awet,         &
!                          wetdens_a                                )
         call modal_aero_coag_sub(                                  &
                           lchnk,     ncol,       nstep,            &
                           imozart_m1,            deltat8,          &
                           latndx,    lonndx,                       &
                           t8,        pmid8,      pdel8,            &
                           vmr8,                                    &
                           dgnum8,                dgnumwet8,        &
                           wetdens_ap8,           gas_pcnst         )

         if (idiagaa > 0) then
            if (mod(i-its,idiagaa_idel)==0 .and. mod(j-jts,idiagaa_jdel)==0)   &
               print 93010, 'backfrm modal_aero_coag_sub - i,j =', i, j
         end if

! diagnostic output after coag
         if (idiag_coag > 0) then
         if ((i == its) .and. (j == jts)) then
         do kcam = 1, pver
            if ((kcam /= pver) .and. (kcam /= pver-10)) cycle

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_so4_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(2) = tmpvmra(kcam,l_mo_h2so4)
            tmpvecb(2) = vmr8(icol,kcam,l_mo_h2so4)
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(/a,i3,1p,e13.5,10e11.3)') 'kcam, old so4 ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'( a,i3,1p,e13.5,10e11.3)') 'kcam, new so4 ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = lptr_nacl_a_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old ncl ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new ncl ...', &
               kcam, tmpvecb(1:ntot_amode+2)

            tmpveca(:) = 0.0
            tmpvecb(:) = 0.0
            do n = 1, ntot_amode
               l = numptr_amode(n) - imozart_m1
               if ((l > 0) .and. (l <= gas_pcnst)) then
                  tmpveca(n+2) = tmpvmra(kcam,l)
                  tmpvecb(n+2) = vmr8(icol,kcam,l)
               end if
            end do
            tmpveca(1) = sum( tmpveca(2:ntot_amode+2) )
            tmpvecb(1) = sum( tmpvecb(2:ntot_amode+2) )
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, old num ...', &
               kcam, tmpveca(1:ntot_amode+2)
            write(*,'(a,i3,1p,e13.5,10e11.3)') 'kcam, new num ...', &
               kcam, tmpvecb(1:ntot_amode+2)

         end do ! kcam
         end if ! ((i == its) .and. (j == jts))
         end if ! (idiag_coag > 0)

      end if ! (aercoag_onoffb > 0)



!--------------------------------------------------------------------
!
! after doing modal_aero_gasaerexch_sub (and newnuc, coag, ...),
! transfer final mixing ratios from vmr to q arrays
!
!--------------------------------------------------------------------
      if (aerchem_onoffb > 0) then

! transfer molar mixing ratios in vmr8 and vmrcw8 back to mass mixing ratios in q8 and qqcw8
         do l2 = pcnst_non_chem+1, pcnst
            l3 = l2 - pcnst_non_chem
            fconv = mw_q_array(l2)/mwdry
            q8(   icol,:,l2) = vmr8(  icol,:,l3)*fconv
            qqcw8(icol,:,l2) = vmrcw8(icol,:,l3)*fconv
            if (i*j==1 .and. k==kts .and. l3==l_mo_h2so4) &
               write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 q8 & vmr8', &
               q8(icol,pver,l2), vmr8(icol,pver,l3)
            if (i*j==1 .and. k==kts .and. l3==l_mo_soag) &
               write(*,'(a,1p,2e11.3)') '1,1,1 soag  q8 & vmr8', &
               q8(icol,pver,l2), vmr8(icol,pver,l3)
         end do

      end if ! (aerchem_onoffb > 0)



!--------------------------------------------------------------------
!
! transfer updated data from cam to wrf-chem variables
!
!--------------------------------------------------------------------
      ! unload the cam layer-center variables that have been modified
      ! (reverse the vertical index, and convert units as needed)
      do k = kts, kte
         kcam = kte-k+1

         do n = 1, ntype_aer
            dgnum(i,k,j,n) = dgnum8(icol,kcam,n) 
            dgnumwet(i,k,j,n) = dgnumwet8(icol,kcam,n) 
            wetdens_ap(i,k,j,n) = wetdens_ap8(icol,kcam,n) 

            l = waterptr_aer(1,n)
            if ((l >= p1st) .and. (l <= num_chem)) &
               chem(i,k,j,l) = qaerwat8(icol,kcam,n)/factconv_chem_to_q(l)
         end do ! n

         do l = p1st, num_chem
            l2 = lptr_chem_to_q(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then
               chem(i,k,j,l) = q8(icol,kcam,l2)/factconv_chem_to_q(l)
            end if
            if (i*j==1 .and. k==kts .and. l2==l_mo_h2so4+pcnst_non_chem) &
               write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 chem & q8', &
               chem(i,k,j,l), q8(icol,kcam,l2)
            if (i*j==1 .and. k==kts .and. l2==l_mo_soag+pcnst_non_chem) &
               write(*,'(a,1p,2e11.3)') '1,1,1 soag  chem & q8', &
               chem(i,k,j,l), q8(icol,kcam,l2)
!            cycle

            l2 = lptr_chem_to_qqcw(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then
               chem(i,k,j,l) = qqcw8(icol,kcam,l2)/factconv_chem_to_q(l)
            end if
         end do ! l

      end do



!--------------------------------------------------------------------
!
! End of main loop over the points in the tile (which treats them each as a CAM chunk)
!
!--------------------------------------------------------------------
2910  continue
2920  continue


      print 93010, 'leaving cam_mam_aerchem_driver - ktau =', ktau
93010 format( a, 8(1x,i6) )

      return
      end subroutine cam_mam_aerchem_driver


!-----------------------------------------------------------------------
   subroutine sum_pm_cam_mam (                                         &
         alt, chem,                                                    &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   REAL :: mass

   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte
!
! Sum over bins with center diameter < 2.5e-4 cm for pm2_5_dry,
! pm2_5_dry_ec, and pm2_5_water. All bins go into pm10
!
   pm2_5_dry(its:imax,kts:kmax,jts:jmax)    = 0.
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = 0.
   pm2_5_water(its:imax,kts:kmax,jts:jmax)  = 0.
   pm10(its:imax,kts:kmax,jts:jmax)         = 0.

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype)
      if (dcen_sect(n,itype) .le. 2.5e-4) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  mass = chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_bc_aer(n,itype,iphase))
 
                  pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + mass

                  pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)            &
                                      + chem(i,k,j,lptr_bc_aer(n,itype,iphase))

                  pm2_5_water(i,k,j) = pm2_5_water(i,k,j)              &
                                     + chem(i,k,j,waterptr_aer(n,itype))

                  pm10(i,k,j) = pm10(i,k,j) + mass
               enddo
            enddo
         enddo
      else
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  pm10(i,k,j) = pm10(i,k,j)                              &
                              + chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                          + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                          + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                          + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                          + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                          + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                          + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                          + chem(i,k,j,lptr_bc_aer(n,itype,iphase))
               enddo
            enddo
         enddo
      endif
   enddo ! size
   enddo ! type
   enddo ! phase

   !Convert the units from mixing ratio to concentration (ug m^-3)
   pm2_5_dry(its:imax,kts:kmax,jts:jmax) = pm2_5_dry(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) &
                                              / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_water(its:imax,kts:kmax,jts:jmax) = pm2_5_water(its:imax,kts:kmax,jts:jmax) &
                                             / alt(its:imax,kts:kmax,jts:jmax)
   pm10(its:imax,kts:kmax,jts:jmax) = pm10(its:imax,kts:kmax,jts:jmax) &
                                      / alt(its:imax,kts:kmax,jts:jmax)

   end subroutine sum_pm_cam_mam




!==============================================================
      end module module_cam_mam_aerchem_driver

