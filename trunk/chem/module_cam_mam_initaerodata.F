! module_cam_mam_initaerodata.F
! adapted from cam3 modal_aero_initialize_data.F90 by r.c.easter, june 2010
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!
! 2010-06-17 rce notes
!     > code involving specrefndxsw, specrefndxlw, and spechygro is temporary,
!	and should be changed after cam radiation code is ported
!     > subroutine modal_aero_initialize_q was used for initial development of
!       cam modal aerosols, and it is no longer needed.  
!       I left it in, but it will now halt immediately if called.
!--------------------------------------------------------------
#include "MODAL_AERO_CPP_DEFINES.h"
#define WRF_PORT
#define MODAL_AERO

module modal_aero_initialize_data
#ifndef WRF_PORT
  use cam_logfile,           only : iulog
  use abortutils,            only: endrun
  use spmd_utils,            only: masterproc, iam
  use ppgrid,                only: pcols, pver
  use phys_control,          only: phys_getopts
#else
  use module_cam_support,    only: iulog, endrun, masterproc, iam, &
       pcols, pver
#endif
  use modal_aero_data

  implicit none
  private
  public :: modal_aero_register
  public :: modal_aero_initialize
#ifndef WRF_PORT
  public :: modal_aero_initialize_q
#endif
#ifdef WRF_PORT
  public :: decouple_mam_mp
#endif

contains

  subroutine modal_aero_register
#ifndef WRF_PORT
    use constituents,          only: pcnst, cnst_name
    use phys_buffer, only: pbuf_add
#else
    use module_cam_support,    only: pcnst => pcnst_runtime
    use constituents,          only: cnst_name
#endif

    character(len=8)  :: &
         xname_massptr(maxd_aspectype,ntot_amode), &
         xname_massptrcw(maxd_aspectype,ntot_amode)
    character(len=10) :: xname_spectype(maxd_aspectype,ntot_amode)


    !   input species to hold interstitial & activated number
#if ( defined MODAL_AERO_7MODE )
    character(len=*), parameter :: xname_numptr(ntot_amode)   = (/ 'num_a1  ', 'num_a2  ', 'num_a3  ', &
         'num_a4  ', 'num_a5  ', 'num_a6  ', 'num_a7  ' /)
    character(len=*), parameter ::     xname_numptrcw(ntot_amode) = (/ 'num_c1  ', 'num_c2  ', 'num_c3  ', &
         'num_c4  ', 'num_c5  ', 'num_c6  ', 'num_c7  ' /)
#elif ( defined MODAL_AERO_3MODE )
    character(len=*), parameter ::     xname_numptr(ntot_amode)   = (/ 'num_a1  ', 'num_a2  ', &
         'num_a3  ' /)
    character(len=*), parameter ::     xname_numptrcw(ntot_amode) = (/ 'num_c1  ', 'num_c2  ', &
         'num_c3  ' /)
#endif



    integer :: m, l, iptr
    real pi
    character(len=3) :: trnum       ! used to hold mode number (as characters)

    pi = 4.*atan(1._r8)    

       !   input species to hold aerosol water and "kohler-c"
       !     xname_waterptr(:ntot_amode)   = (/ 'wat_a1  ', 'wat_a2  ', 'wat_a3  ', &
       !                                        'wat_a4  ', 'wat_a5  ', 'wat_a6  ', 'wat_a7  ' /)
       !   input chemical species for the mode
       ! mode 1 (accumulation) species
       ! JPE 02022011: These could also be parameters but a bug in the pathscale compiler prevents
       !               parameter initialization of 2D variables
#if ( defined MODAL_AERO_7MODE )
       xname_massptr(:nspec_amode(1),1)   = (/ 'so4_a1  ', 'nh4_a1  ', &
            'pom_a1  ', 'soa_a1  ', 'bc_a1   ', 'ncl_a1  ' /)
       xname_massptrcw(:nspec_amode(1),1) = (/ 'so4_c1  ', 'nh4_c1  ', &
            'pom_c1  ', 'soa_c1  ', 'bc_c1   ', 'ncl_c1  ' /)
       xname_spectype(:nspec_amode(1),1)  = (/ 'sulfate   ', 'ammonium  ', &
            'p-organic ', 's-organic ', 'black-c   ', 'seasalt   ' /)
#elif ( defined MODAL_AERO_3MODE )
       xname_massptr(:nspec_amode(1),1)   = (/ 'so4_a1  ', &
            'pom_a1  ', 'soa_a1  ', 'bc_a1   ', &
            'dst_a1  ', 'ncl_a1  ' /)
       xname_massptrcw(:nspec_amode(1),1) = (/ 'so4_c1  ', &
            'pom_c1  ', 'soa_c1  ', 'bc_c1   ', &
            'dst_c1  ', 'ncl_c1  ' /)
       xname_spectype(:nspec_amode(1),1)  = (/ 'sulfate   ', &
            'p-organic ', 's-organic ', 'black-c   ', &
            'dust      ', 'seasalt   ' /)
#endif

       ! mode 2 (aitken) species
#if ( defined MODAL_AERO_7MODE )
       xname_massptr(:nspec_amode(2),2)   = (/ 'so4_a2  ', 'nh4_a2  ', &
            'soa_a2  ', 'ncl_a2  ' /)
       xname_massptrcw(:nspec_amode(2),2) = (/ 'so4_c2  ', 'nh4_c2  ', &
            'soa_c2  ', 'ncl_c2  ' /)
       xname_spectype(:nspec_amode(2),2)  = (/ 'sulfate   ', 'ammonium  ', &
            's-organic ', 'seasalt   ' /)
#elif ( defined MODAL_AERO_3MODE )
       xname_massptr(:nspec_amode(2),2)   = (/ 'so4_a2  ', &
            'soa_a2  ', 'ncl_a2  ' /)
       xname_massptrcw(:nspec_amode(2),2) = (/ 'so4_c2  ', &
            'soa_c2  ', 'ncl_c2  ' /)
       xname_spectype(:nspec_amode(2),2)  = (/ 'sulfate   ', &
            's-organic ', 'seasalt   ' /)
#endif

#if ( defined MODAL_AERO_7MODE )
       ! mode 3 (primary carbon) species
       xname_massptr(:nspec_amode(3),3)   = (/ 'pom_a3  ', 'bc_a3   ' /)
       xname_massptrcw(:nspec_amode(3),3) = (/ 'pom_c3  ', 'bc_c3   ' /)
       xname_spectype(:nspec_amode(3),3)  = (/ 'p-organic ', 'black-c   ' /)
#elif ( defined MODAL_AERO_3MODE )
       ! mode 3 (coarse dust & seasalt) species
       xname_massptr(:nspec_amode(3),3)   = (/ 'dst_a3  ', 'ncl_a3  ', 'so4_a3  ' /)
       xname_massptrcw(:nspec_amode(3),3) = (/ 'dst_c3  ', 'ncl_c3  ', 'so4_c3  ' /)
       xname_spectype(:nspec_amode(3),3)  = (/ 'dust      ', 'seasalt   ', 'sulfate   ' /)
#endif


#if ( defined MODAL_AERO_7MODE )
       ! mode 4 (fine seasalt) species
       xname_massptr(:nspec_amode(4),4)   = (/ 'ncl_a4  ', 'so4_a4  ', 'nh4_a4  ' /)
       xname_massptrcw(:nspec_amode(4),4) = (/ 'ncl_c4  ', 'so4_c4  ', 'nh4_c4  ' /)
       xname_spectype(:nspec_amode(4),4)  = (/ 'seasalt   ', 'sulfate   ', 'ammonium  ' /)

       ! mode 5 (fine dust) species
       xname_massptr(:nspec_amode(5),5)   = (/ 'dst_a5  ', 'so4_a5  ', 'nh4_a5  ' /)
       xname_massptrcw(:nspec_amode(5),5) = (/ 'dst_c5  ', 'so4_c5  ', 'nh4_c5  ' /)
       xname_spectype(:nspec_amode(5),5)  = (/ 'dust      ', 'sulfate   ', 'ammonium  ' /)

       ! mode 6 (coarse seasalt) species
       xname_massptr(:nspec_amode(6),6)   = (/ 'ncl_a6  ', 'so4_a6  ', 'nh4_a6  ' /)
       xname_massptrcw(:nspec_amode(6),6) = (/ 'ncl_c6  ', 'so4_c6  ', 'nh4_c6  ' /)
       xname_spectype(:nspec_amode(6),6)  = (/ 'seasalt   ', 'sulfate   ', 'ammonium  ' /)

       ! mode 7 (coarse dust) species
       xname_massptr(:nspec_amode(7),7)   = (/ 'dst_a7  ', 'so4_a7  ', 'nh4_a7  ' /)
       xname_massptrcw(:nspec_amode(7),7) = (/ 'dst_c7  ', 'so4_c7  ', 'nh4_c7  ' /)
       xname_spectype(:nspec_amode(7),7)  = (/ 'dust      ', 'sulfate   ', 'ammonium  ' /)
#endif

    do m = 1, ntot_amode

       if (masterproc) then
          write(iulog,9231) m, modename_amode(m)
          write(iulog,9232)                                          &
               'nspec                       ',                         &
               nspec_amode(m)
          write(iulog,9232)                                          &
               'mprognum, mdiagnum, mprogsfc',                         &
               mprognum_amode(m), mdiagnum_amode(m), mprogsfc_amode(m)
          write(iulog,9232)                                          &
               'mcalcwater                  ',                         &
               mcalcwater_amode(m)
       endif

       !   compute frequently used parameters: ln(sigmag),
       !   volume-to-number and volume-to-surface conversions, ...
       alnsg_amode(m) = log( sigmag_amode(m) )

       voltonumb_amode(m) = 1. / ( (pi/6.)*                            &
            (dgnum_amode(m)**3.)*exp(4.5*alnsg_amode(m)**2.) )
       voltonumblo_amode(m) = 1. / ( (pi/6.)*                          &
            (dgnumlo_amode(m)**3.)*exp(4.5*alnsg_amode(m)**2.) )
       voltonumbhi_amode(m) = 1. / ( (pi/6.)*                          &
            (dgnumhi_amode(m)**3.)*exp(4.5*alnsg_amode(m)**2.) )

       alnv2n_amode(m)   = log( voltonumb_amode(m) )
       alnv2nlo_amode(m) = log( voltonumblo_amode(m) )
       alnv2nhi_amode(m) = log( voltonumbhi_amode(m) )

       !    define species to hold interstitial & activated number
       call search_list_of_names(                                      &
            xname_numptr(m), numptr_amode(m), cnst_name, pcnst )
       if (numptr_amode(m) .le. 0) then
          write(iulog,9061) 'xname_numptr', xname_numptr(m), m
          call endrun()
       end if
       if (numptr_amode(m) .gt. pcnst) then
          write(iulog,9061) 'numptr_amode', numptr_amode(m), m
          write(iulog,9061) 'xname_numptr', xname_numptr(m), m
          call endrun()
       end if

       species_class(numptr_amode(m)) = spec_class_aerosol


       numptrcw_amode(m) = numptr_amode(m)  !use the same index for Q and QQCW arrays
       if (numptrcw_amode(m) .le. 0) then
          write(iulog,9061) 'xname_numptrcw', xname_numptrcw(m), m
          call endrun()
       end if
       if (numptrcw_amode(m) .gt. pcnst) then
          write(iulog,9061) 'numptrcw_amode', numptrcw_amode(m), m
          write(iulog,9061) 'xname_numptrcw', xname_numptrcw(m), m
          call endrun()
       end if
       species_class(numptrcw_amode(m)) = spec_class_aerosol
#ifndef WRF_PORT
       call pbuf_add(xname_numptrcw(m), 'global', 1, pver, 1, iptr)
       call qqcw_set_ptr(numptrcw_amode(m),iptr)
#endif

       !   output mode information
       if ( masterproc ) then
          write(iulog,9233) 'numptr         ',                           &
               numptr_amode(m), xname_numptr(m)
          write(iulog,9233) 'numptrcw       ',                           &
               numptrcw_amode(m), xname_numptrcw(m)
       end if


       !   define the chemical species for the mode
       do l = 1, nspec_amode(m)

          call search_list_of_names(                                  &
               xname_spectype(l,m), lspectype_amode(l,m),              &
               specname_amode, ntot_aspectype )
          if (lspectype_amode(l,m) .le. 0) then
             write(iulog,9062) 'xname_spectype', xname_spectype(l,m), l, m
             call endrun()
          end if

          call search_list_of_names(                                  &
               xname_massptr(l,m), lmassptr_amode(l,m), cnst_name, pcnst )
          if (lmassptr_amode(l,m) .le. 0) then
             write(iulog,9062) 'xname_massptr', xname_massptr(l,m), l, m
             call endrun()
          end if
          species_class(lmassptr_amode(l,m)) = spec_class_aerosol

          lmassptrcw_amode(l,m) = lmassptr_amode(l,m)  !use the same index for Q and QQCW arrays
          if (lmassptrcw_amode(l,m) .le. 0) then
             write(iulog,9062) 'xname_massptrcw', xname_massptrcw(l,m), l, m
             call endrun()
          end if
#ifndef WRF_PORT
          call pbuf_add(xname_massptrcw(l,m), 'global', 1, pver, 1, iptr)
          call qqcw_set_ptr(lmassptrcw_amode(l,m), iptr)
#endif
          species_class(lmassptrcw_amode(l,m)) = spec_class_aerosol

          if ( masterproc ) then
             write(iulog,9236) 'spec, spectype ', l,                    &
                  lspectype_amode(l,m), xname_spectype(l,m)
             write(iulog,9236) 'spec, massptr  ', l,                    &
                  lmassptr_amode(l,m), xname_massptr(l,m)
             write(iulog,9236) 'spec, massptrcw', l,                    &
                  lmassptrcw_amode(l,m), xname_massptrcw(l,m)
          end if

       enddo

       if ( masterproc ) write(iulog,*)


       !   set names for aodvis and ssavis
       write(unit=trnum,fmt='(i3)') m+100
       aodvisname(m) = 'AODVIS'//trnum(2:3)
       aodvislongname(m) = 'Aerosol optical depth for mode '//trnum(2:3)
       ssavisname(m) = 'SSAVIS'//trnum(2:3)
       ssavislongname(m) = 'Single-scatter albedo for mode '//trnum(2:3)
       fnactname(m) = 'FNACT'//trnum(2:3)
       fnactlongname(m) = 'Number faction activated for mode '//trnum(2:3)
       fmactname(m) = 'FMACT'//trnum(2:3)
       fmactlongname(m) = 'Fraction mass activated for mode'//trnum(2:3)
    end do
#ifndef WRF_PORT
       if (masterproc) write(iulog,9230)
#endif
9230   format( // '*** init_aer_modes mode definitions' )
9231   format( 'mode = ', i4, ' = "', a, '"' )
9232   format( 4x, a, 4(1x, i5 ) )
9233   format( 4x, a15, 4x, i7, '="', a, '"' )
9236   format( 4x, a15, i4, i7, '="', a, '"' )
9061   format( '*** subr init_aer_modes - bad ', a /                   &
            5x, 'name, m =  ', a, 5x, i5 )
9062   format( '*** subr init_aer_modesaeromodeinit - bad ', a /                       &
            5x, 'name, l, m =  ', a, 5x, 2i5 )


  end subroutine modal_aero_register


  !==============================================================
  subroutine modal_aero_initialize
#ifndef WRF_PORT
       use cam_history,           only: addfld, add_default, phys_decomp
       use constituents,          only: pcnst
#else
       use module_cam_support,    only: pcnst => pcnst_runtime, &
            addfld, add_default, phys_decomp
#endif
       use physconst,             only: rhoh2o, mwh2o
       use modal_aero_calcsize,   only: modal_aero_calcsize_init
       use modal_aero_coag,       only: modal_aero_coag_init
#ifndef WRF_PORT
       use modal_aero_deposition, only: modal_aero_deposition_init
#endif
       use modal_aero_gasaerexch, only: modal_aero_gasaerexch_init
       use modal_aero_newnuc,     only: modal_aero_newnuc_init
       use modal_aero_rename,     only: modal_aero_rename_init
       use mz_aerosols_intr,      only: modal_aero_bcscavcoef_init
#ifndef WRF_PORT
       use rad_constituents,      only: rad_cnst_get_info, rad_cnst_get_aer_props
#endif

       implicit none

       !--------------------------------------------------------------
       ! ... local variables
       !--------------------------------------------------------------
       integer l, m, i


       character(len=3) :: trnum       ! used to hold mode number (as characters)
       integer :: iaerosol, ibulk
       integer  :: numaerosols     ! number of bulk aerosols in climate list
       character(len=20) :: bulkname
       complex, pointer  :: refindex_aer_sw(:), &
            refindex_aer_lw(:)
       real(r8) :: hygro_aer
       logical  :: history_aerosol      ! Output the MAM aerosol tendencies

       !-----------------------------------------------------------------------
#ifndef WRF_PORT
       call phys_getopts( history_aerosol_out        = history_aerosol   )
#else
       history_aerosol = .false.
#endif



       ! safety check on modal_aero, and modal_aero_3mode, modal_aero_7mode
#if ( defined MODAL_AERO_3MODE ) && ( defined MODAL_AERO_7MODE )
       call endrun( 'Error - when modal_aero defined, just 1 of modal_aero_3/7mode must be defined'
#elif ( ! ( defined MODAL_AERO_3MODE ) ) && ( ! ( defined MODAL_AERO_7MODE ) )
       call endrun( 'Error - when modal_aero defined, at least 1 of modal_aero_3/7mode must be defined'
#endif
       !     values from Koepke, Hess, Schult and Shettle, Global Aerosol Data Set 
       !     Report #243, Max-Planck Institute for Meteorology, 1997a
       !     See also Hess, Koepke and Schult, Optical Properties of Aerosols and Clouds (OPAC)
       !     BAMS, 1998.

       !      specrefndxsw(:ntot_aspectype)     = (/ (1.53,  0.01),   (1.53,  0.01),  (1.53,  0.01), &
       !                                           (1.55,  0.01),   (1.55,  0.01),  (1.90, 0.60), &
       !                                           (1.50, 1.0e-8), (1.50, 0.005) /)
       !      specrefndxlw(:ntot_aspectype)   = (/ (2.0, 0.5),   (2.0, 0.5), (2.0, 0.5), &
       !                                           (1.7, 0.5),   (1.7, 0.5), (2.22, 0.73), &
       !                                           (1.50, 0.02), (2.6, 0.6) /)
       !     get refractive indices from phys_prop files
#ifndef WRF_PORT
       call rad_cnst_get_info(0,naero=numaerosols)
       do l = 1, ntot_aspectype
          ibulk=0
          do iaerosol = 1, numaerosols
             call rad_cnst_get_aer_props(0, iaerosol, aername=bulkname)
             !	    print *,'bulkname=',bulkname
             if(specname_amode(l).eq.'sulfate'.and.bulkname.eq.'SULFATE')ibulk=iaerosol
             if(specname_amode(l).eq.'ammonium'.and.bulkname.eq.'SULFATE')ibulk=iaerosol
             if(specname_amode(l).eq.'nitrate'.and.bulkname.eq.'SULFATE')ibulk=iaerosol
             if(specname_amode(l).eq.'p-organic'.and.bulkname.eq.'OCPHO')ibulk=iaerosol
             if(specname_amode(l).eq.'s-organic'.and.bulkname.eq.'OCPHI')ibulk=iaerosol
             if(specname_amode(l).eq.'black-c'.and.bulkname.eq.'BCPHO')ibulk=iaerosol
             if(specname_amode(l).eq.'seasalt'.and.bulkname.eq.'SSAM')ibulk=iaerosol
             if(specname_amode(l).eq.'dust'.and.bulkname.eq.'DUST4')ibulk=iaerosol
          end do
          if(ibulk.eq.0)then
             write(iulog,*) 'modal species names do not match bulk names for modal species ',specname_amode(l)
             call endrun('endrun modal_aero_initialize')
          endif
          call rad_cnst_get_aer_props(0, ibulk, &
               refindex_aer_sw=refindex_aer_sw, &
               refindex_aer_lw=refindex_aer_lw, &
               density_aer=specdens_amode(l), &
               hygro_aer=hygro_aer )

          spechygro(l)=hygro_aer

          do i=1,nswbands
             specrefndxsw(i,l)=refindex_aer_sw(i)
          end do
          do i=1,nlwbands
             specrefndxlw(i,l)=refindex_aer_lw(i)
          end do
       end do


       if (masterproc) write(iulog,9210)
       do l = 1, ntot_aspectype
          !            spechygro(l) = specnu(l)*specphi(l)*specsolfrac(l)*mwh2o*specdens_amode(l) / &
          !	               (rhoh2o*specmw_amode(l))
          if (masterproc) then
             write(iulog,9211) l
             write(iulog,9212) 'name            ', specname_amode(l)
             write(iulog,9213) 'density, MW     ',                  &
                  specdens_amode(l), specmw_amode(l)
             write(iulog,9213) 'hygro', spechygro(l)
             do i=1,nswbands
                write(iulog,9213) 'ref index sw    ', (specrefndxsw(i,l))
             end do
             do i=1,nlwbands
                write(iulog,9213) 'ref index ir    ', (specrefndxlw(i,l))
             end do
          end if
       end do
#else
       !Balwinder.Singh@pnnl: Comments regarding spechygro:
       !RCE origianl comment:"code involving specrefndxsw, specrefndxlw, and spechygro is temporary,
       !and should be changed after cam radiation code is ported"

       ! 2010/06/16 rce
       ! following code involving specrefndxsw, specrefndxlw, and spechygro is temporary,
       ! and should be changed after cam radiation code is ported
       
       spechygro(:ntot_aspectype) = (/ 0.507, 0.507, 0.507, &
            0.100, 0.140, 1.0e-10, &
            1.160, 0.068 /)
       specrefndxsw(1,:ntot_aspectype)     = (/ (1.53,  0.01),   (1.53,  0.01),  (1.53,  0.01), &
            (1.55,  0.01),   (1.55,  0.01),  (1.90, 0.60), &
            (1.50, 1.0e-8), (1.50, 0.005) /)
       specrefndxlw(1,:ntot_aspectype)   = (/ (2.0, 0.5),   (2.0, 0.5), (2.0, 0.5), &
            (1.7, 0.5),   (1.7, 0.5), (2.22, 0.73), &
            (1.50, 0.02), (2.6, 0.6) /)
       do l = 1, ntot_aspectype
          specrefndxsw(:,l)=specrefndxsw(1,l)
          specrefndxlw(:,l)=specrefndxlw(1,l)
       end do
       
       ! rce - 06-aug-2007 - changed specmw for almost everything to match mozart
       specdens_amode(:ntot_aspectype) = (/1770.0,1770.0,1770.0, 1000.0, 1000.0, 1700.0,1900.0,2600.0 /)
       
#endif
9210   format( // '*** init_aer_modes aerosol species-types' )
9211   format( 'spectype =', i4)
9212   format( 4x, a, 3x, '"', a, '"' )
9213   format( 4x, a, 5(1pe14.5) )





       do i = 1, pcnst
          species_class(i) = spec_class_undefined
       end do



       !   set cnst_name_cw
       call initaermodes_set_cnstnamecw()


       !
       !   set the lptr_so4_a_amode(m), lptr_so4_cw_amode(m), ...
       !
       call initaermodes_setspecptrs

       if ( masterproc ) write(iulog,*)



       !
       !   add to history
       !
       do m = 1, ntot_amode
          write( trnum, '(i3.3)' ) m
          ! note - eventually we should change these from "dgnd_a0N" to "dgnd_aN"
          call addfld( &
               'dgnd_a'//trnum(2:3), 'm', pver, 'A', &
               'dry dgnum, interstitial, mode '//trnum(2:3), phys_decomp )
          call addfld( &
               'dgnw_a'//trnum(2:3), 'm', pver, 'A', &
               'wet dgnum, interstitial, mode '//trnum(2:3), phys_decomp )
          call addfld( &
               'wat_a'//trnum(3:3), 'm', pver, 'A', &
               'aerosol water, interstitial, mode '//trnum(2:3), phys_decomp )
          if ( history_aerosol ) then    
            call add_default( 'dgnd_a'//trnum(2:3), 1, ' ' )
            call add_default( 'dgnw_a'//trnum(2:3), 1, ' ' )
            call add_default( 'wat_a'//trnum(3:3),  1, ' ' )     
          endif

          l = lptr_so4_cw_amode(m)
          if (l > 0) then
             call addfld (&
                  trim(cnst_name_cw(l))//'AQSO4','kg/m2/s ',1,  'A', &
                  trim(cnst_name_cw(l))//' aqueous phase chemistry',phys_decomp)
             call addfld (&
                  trim(cnst_name_cw(l))//'AQH2SO4','kg/m2/s ',1,  'A', &
                  trim(cnst_name_cw(l))//' aqueous phase chemistry',phys_decomp)
             if ( history_aerosol ) then 
                call add_default (trim(cnst_name_cw(l))//'AQSO4', 1, ' ')
                call add_default (trim(cnst_name_cw(l))//'AQH2SO4', 1, ' ')
             endif
          end if

       end do

       call addfld ('AQSO4_H2O2','kg/m2/s ',1,  'A', &
            'SO4 aqueous phase chemistry due to H2O2',phys_decomp)
       call addfld ('AQSO4_O3','kg/m2/s ',1,  'A', &
            'SO4 aqueous phase chemistry due to O3',phys_decomp)
       call addfld( 'XPH_LWC','kg/kg   ',pver, 'A', &
            'pH value multiplied by lwc', phys_decomp)

       if ( history_aerosol ) then    
          call add_default ('AQSO4_H2O2', 1, ' ')
          call add_default ('AQSO4_O3', 1, ' ')    
          call add_default ('XPH_LWC', 1, ' ')
       endif



       !
       !   set threshold for reporting negatives from subr qneg3
       !   for aerosol number species set this to
       !      1e3 #/kg ~= 1e-3 #/cm3 for accum, aitken, pcarbon, ufine modes
       !      3e1 #/kg ~= 3e-5 #/cm3 for fineseas and finedust modes 
       !      1e0 #/kg ~= 1e-6 #/cm3 for other modes which are coarse
       !   for other species, set this to zero so that it will be ignored
       !      by qneg3
       !
#ifndef WRF_PORT
       if ( masterproc ) write(iulog,'(/a)') &
            'mode, modename_amode, qneg3_worst_thresh_amode'
#endif
       qneg3_worst_thresh_amode(:) = 0.0_r8
       do m = 1, ntot_amode
          l = numptr_amode(m)
          if ((l <= 0) .or. (l > pcnst)) cycle

          if      (m == modeptr_accum) then
             qneg3_worst_thresh_amode(l) = 1.0e3_r8
          else if (m == modeptr_aitken) then
             qneg3_worst_thresh_amode(l) = 1.0e3_r8
          else if (m == modeptr_pcarbon) then
             qneg3_worst_thresh_amode(l) = 1.0e3_r8
          else if (m == modeptr_ufine) then
             qneg3_worst_thresh_amode(l) = 1.0e3_r8

          else if (m == modeptr_fineseas) then
             qneg3_worst_thresh_amode(l) = 3.0e1_r8
          else if (m == modeptr_finedust) then
             qneg3_worst_thresh_amode(l) = 3.0e1_r8

          else
             qneg3_worst_thresh_amode(l) = 1.0e0_r8
          end if

          if ( masterproc ) write(iulog,'(i3,2x,a,1p,e12.3)') &
               m, modename_amode(m), qneg3_worst_thresh_amode(l)
       end do


       !
       !   call other initialization routines
       !
       call modal_aero_rename_init
       !   calcsize call must follow rename call
       call modal_aero_calcsize_init
       call modal_aero_gasaerexch_init
       !   coag call must follow gasaerexch call
       call modal_aero_coag_init
       call modal_aero_newnuc_init
       call modal_aero_bcscavcoef_init
#ifndef WRF_PORT
       call modal_aero_deposition_init
#endif

       return
     end subroutine modal_aero_initialize


     !==============================================================
     subroutine search_list_of_names(                                &
          name_to_find, name_id, list_of_names, list_length )
       !
       !   searches for a name in a list of names
       !
       !   name_to_find - the name to be found in the list  [input]
       !   name_id - the position of "name_to_find" in the "list_of_names".
       !       If the name is not found in the list, then name_id=0.  [output]
       !   list_of_names - the list of names to be searched  [input]
       !   list_length - the number of names in the list  [input]
       !
       character(len=*), intent(in):: name_to_find, list_of_names(:)
       integer, intent(in) :: list_length
       integer, intent(out) :: name_id
       
       integer :: i
       name_id = -999888777
       if (name_to_find .ne. ' ') then
          do i = 1, list_length
             if (name_to_find .eq. list_of_names(i)) then
                name_id = i
                exit
             end if
          end do
       end if
     end subroutine search_list_of_names


     !==============================================================
     subroutine initaermodes_setspecptrs
       !
       !   sets the lptr_so4_a_amode(m), lptr_so4_cw_amode(m), ...
       !       and writes them to iulog
       !   ALSO sets the mode-pointers:  modeptr_accum, modeptr_aitken, ...
       !       and writes them to iulog
       !   ALSO sets values of specdens_XX_amode and specmw_XX_amode
       !       (XX = so4, om, bc, dust, seasalt)
       !
       implicit none

       !   local variables
       integer l, l2, m
       character*8 dumname
       integer, parameter :: init_val=-999888777

       !   all processes set the pointers

       modeptr_accum = init_val
       modeptr_aitken = init_val
       modeptr_ufine = init_val
       modeptr_coarse = init_val
       modeptr_pcarbon = init_val
       modeptr_fineseas = init_val
       modeptr_finedust = init_val
       modeptr_coarseas = init_val
       modeptr_coardust = init_val
       do m = 1, ntot_amode
          if (modename_amode(m) .eq. 'accum') then
             modeptr_accum = m
          else if (modename_amode(m) .eq. 'aitken') then
             modeptr_aitken = m
          else if (modename_amode(m) .eq. 'ufine') then
             modeptr_ufine = m
          else if (modename_amode(m) .eq. 'coarse') then
             modeptr_coarse = m
          else if (modename_amode(m) .eq. 'primary carbon') then
             modeptr_pcarbon = m
          else if (modename_amode(m) .eq. 'fine seasalt') then
             modeptr_fineseas = m
          else if (modename_amode(m) .eq. 'fine dust') then
             modeptr_finedust = m
          else if (modename_amode(m) .eq. 'coarse seasalt') then
             modeptr_coarseas = m
          else if (modename_amode(m) .eq. 'coarse dust') then
             modeptr_coardust = m
          end if
       end do

       do m = 1, ntot_amode
          lptr_so4_a_amode(m)   = init_val
          lptr_so4_cw_amode(m)  = init_val
          lptr_msa_a_amode(m)   = init_val
          lptr_msa_cw_amode(m)  = init_val
          lptr_nh4_a_amode(m)   = init_val
          lptr_nh4_cw_amode(m)  = init_val
          lptr_no3_a_amode(m)   = init_val
          lptr_no3_cw_amode(m)  = init_val
          lptr_pom_a_amode(m)   = init_val
          lptr_pom_cw_amode(m)  = init_val
          lptr_soa_a_amode(m)   = init_val
          lptr_soa_cw_amode(m)  = init_val
          lptr_bc_a_amode(m)    = init_val
          lptr_bc_cw_amode(m)   = init_val
          lptr_nacl_a_amode(m)  = init_val
          lptr_nacl_cw_amode(m) = init_val
          lptr_dust_a_amode(m)  = init_val
          lptr_dust_cw_amode(m) = init_val
          do l = 1, nspec_amode(m)
             l2 = lspectype_amode(l,m)
             if ( (specname_amode(l2) .eq. 'sulfate') .and.  &
                  (lptr_so4_a_amode(m) .le. 0) ) then
                lptr_so4_a_amode(m)  = lmassptr_amode(l,m)
                lptr_so4_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'msa') .and.      &
                  (lptr_msa_a_amode(m) .le. 0) ) then
                lptr_msa_a_amode(m)  = lmassptr_amode(l,m)
                lptr_msa_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'ammonium') .and.  &
                  (lptr_nh4_a_amode(m) .le. 0) ) then
                lptr_nh4_a_amode(m)  = lmassptr_amode(l,m)
                lptr_nh4_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'nitrate') .and.  &
                  (lptr_no3_a_amode(m) .le. 0) ) then
                lptr_no3_a_amode(m)  = lmassptr_amode(l,m)
                lptr_no3_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'p-organic') .and.   &
                  (lptr_pom_a_amode(m) .le. 0) ) then
                lptr_pom_a_amode(m)  = lmassptr_amode(l,m)
                lptr_pom_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 's-organic') .and.   &
                  (lptr_soa_a_amode(m) .le. 0) ) then
                lptr_soa_a_amode(m)  = lmassptr_amode(l,m)
                lptr_soa_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'black-c') .and.  &
                  (lptr_bc_a_amode(m) .le. 0) ) then
                lptr_bc_a_amode(m)  = lmassptr_amode(l,m)
                lptr_bc_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'seasalt') .and.  &
                  (lptr_nacl_a_amode(m) .le. 0) ) then
                lptr_nacl_a_amode(m)  = lmassptr_amode(l,m)
                lptr_nacl_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
             if ( (specname_amode(l2) .eq. 'dust') .and.     &
                  (lptr_dust_a_amode(m) .le. 0) ) then
                lptr_dust_a_amode(m)  = lmassptr_amode(l,m)
                lptr_dust_cw_amode(m) = lmassptrcw_amode(l,m)
             end if
          end do
       end do

       !   all processes set values of specdens_XX_amode and specmw_XX_amode
       specdens_so4_amode = 2.0
       specdens_nh4_amode = 2.0
       specdens_no3_amode = 2.0
       specdens_pom_amode = 2.0
       specdens_soa_amode = 2.0
       specdens_bc_amode = 2.0
       specdens_dust_amode = 2.0
       specdens_seasalt_amode = 2.0
       specmw_so4_amode = 1.0
       specmw_nh4_amode = 1.0
       specmw_no3_amode = 1.0
       specmw_pom_amode = 1.0
       specmw_soa_amode = 1.0
       specmw_bc_amode = 1.0
       specmw_dust_amode = 1.0
       specmw_seasalt_amode = 1.0
       do m = 1, ntot_aspectype
          if      (specname_amode(m).eq.'sulfate   ') then
             specdens_so4_amode = specdens_amode(m)
             specmw_so4_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'ammonium  ') then
             specdens_nh4_amode = specdens_amode(m)
             specmw_nh4_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'nitrate   ') then
             specdens_no3_amode = specdens_amode(m)
             specmw_no3_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'p-organic ') then
             specdens_pom_amode = specdens_amode(m)
             specmw_pom_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'s-organic ') then
             specdens_soa_amode = specdens_amode(m)
             specmw_soa_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'black-c   ') then
             specdens_bc_amode = specdens_amode(m)
             specmw_bc_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'dust      ') then
             specdens_dust_amode = specdens_amode(m)
             specmw_dust_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'seasalt   ') then
             specdens_seasalt_amode = specdens_amode(m)
             specmw_seasalt_amode = specmw_amode(m)
          end if
       enddo

       !   masterproc writes out the pointers
       if ( .not. ( masterproc ) ) return
#ifndef WRF_PORT
       write(iulog,9230)
#endif
       write(iulog,*) 'modeptr_accum    =', modeptr_accum
       write(iulog,*) 'modeptr_aitken   =', modeptr_aitken
       write(iulog,*) 'modeptr_ufine    =', modeptr_ufine
       write(iulog,*) 'modeptr_coarse   =', modeptr_coarse
       write(iulog,*) 'modeptr_pcarbon  =', modeptr_pcarbon
       write(iulog,*) 'modeptr_fineseas =', modeptr_fineseas
       write(iulog,*) 'modeptr_finedust =', modeptr_finedust
       write(iulog,*) 'modeptr_coarseas =', modeptr_coarseas
       write(iulog,*) 'modeptr_coardust =', modeptr_coardust

       dumname = 'none'
#ifndef WRF_PORT
       write(iulog,9240)
#endif
       write(iulog,9000) 'sulfate    '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_so4_a_amode(m), lptr_so4_cw_amode(m),  'so4' )
       end do

       write(iulog,9000) 'msa        '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_msa_a_amode(m), lptr_msa_cw_amode(m),  'msa' )
       end do

       write(iulog,9000) 'ammonium   '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_nh4_a_amode(m), lptr_nh4_cw_amode(m),  'nh4' )
       end do

       write(iulog,9000) 'nitrate    '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_no3_a_amode(m), lptr_no3_cw_amode(m),  'no3' )
       end do

       write(iulog,9000) 'p-organic  '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_pom_a_amode(m), lptr_pom_cw_amode(m),  'pom' )
       end do

       write(iulog,9000) 's-organic  '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_soa_a_amode(m), lptr_soa_cw_amode(m),  'soa' )
       end do

       write(iulog,9000) 'black-c    '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_bc_a_amode(m), lptr_bc_cw_amode(m),  'bc' )
       end do

       write(iulog,9000) 'seasalt   '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_nacl_a_amode(m), lptr_nacl_cw_amode(m),  'nacl' )
       end do

       write(iulog,9000) 'dust       '
       do m = 1, ntot_amode
          call initaermodes_setspecptrs_write2( m,                    &
               lptr_dust_a_amode(m), lptr_dust_cw_amode(m),  'dust' )
       end do

9000   format( a )
9230   format(                                                         &
            / 'mode-pointer output from subr initaermodes_setspecptrs' )
9240   format(                                                         &
            / 'species-pointer output from subr initaermodes_setspecptrs' / &
            'mode', 12x, 'id  name_a  ', 12x, 'id  name_cw' )

       return
     end subroutine initaermodes_setspecptrs


     !==============================================================
     subroutine initaermodes_setspecptrs_write2(                     &
          m, laptr, lcptr, txtdum )
       !
       !   does some output for initaermodes_setspecptrs
#ifndef WRF_PORT
       use constituents, only: pcnst, cnst_name
#else
       use module_cam_support,    only: pcnst => pcnst_runtime
       use constituents, only: cnst_name
#endif

       implicit none

       !   subr arguments
       integer m, laptr, lcptr
       character*(*) txtdum

       !   local variables
       character*8 dumnamea, dumnamec

       dumnamea = 'none'
       dumnamec = 'none'
       if (laptr .gt. 0) dumnamea = cnst_name(laptr)
       if (lcptr .gt. 0) dumnamec = cnst_name(lcptr)
       write(iulog,9241) m, laptr, dumnamea, lcptr, dumnamec, txtdum

9241   format( i4, 2( 2x, i12, 2x, a ),                                &
            4x, 'lptr_', a, '_a/cw_amode' )

       return
     end subroutine initaermodes_setspecptrs_write2


     !==============================================================
     subroutine initaermodes_set_cnstnamecw
       !
       !   sets the cnst_name_cw
       !
#ifndef WRF_PORT
       use constituents, only: pcnst, cnst_name
#else
       use module_cam_support,    only: pcnst => pcnst_runtime
       use constituents, only: cnst_name
#endif
       implicit none

       !   subr arguments (none)

       !   local variables
       integer j, l, la, lc, ll, m

       !   set cnst_name_cw
       cnst_name_cw = ' '
       do m = 1, ntot_amode
          do ll = 0, nspec_amode(m)
             if (ll == 0) then
                la = numptr_amode(m)
                lc = numptrcw_amode(m)
             else
                la = lmassptr_amode(ll,m)
                lc = lmassptrcw_amode(ll,m)
             end if
             if ((la < 1) .or. (la > pcnst) .or.   &
                  (lc < 1) .or. (lc > pcnst)) then
                write(*,'(/2a/a,5(1x,i10))')   &
                     '*** initaermodes_set_cnstnamecw error',   &
                     ' -- bad la or lc',   &
                     '    m, ll, la, lc, pcnst =', m, ll, la, lc, pcnst
                call endrun( '*** initaermodes_set_cnstnamecw error' )
             end if
             do j = 2, len( cnst_name(la) ) - 1
                if (cnst_name(la)(j:j+1) == '_a') then
                   cnst_name_cw(lc) = cnst_name(la)
                   cnst_name_cw(lc)(j:j+1) = '_c'
                   exit
                else if (cnst_name(la)(j:j+1) == '_A') then
                   cnst_name_cw(lc) = cnst_name(la)
                   cnst_name_cw(lc)(j:j+1) = '_C'
                   exit
                end if
             end do
             if (cnst_name_cw(lc) == ' ') then
                write(*,'(/2a/a,3(1x,i10),2x,a)')   &
                     '*** initaermodes_set_cnstnamecw error',   &
                     ' -- bad cnst_name(la)',   &
                     '    m, ll, la, cnst_name(la) =',   &
                     m, ll, la, cnst_name(la)
                call endrun( '*** initaermodes_set_cnstnamecw error' )
             end if
          end do   ! ll = 0, nspec_amode(m)
       end do   ! m = 1, ntot_amode

       if ( masterproc ) then
          write(*,'(/a)') 'l, cnst_name(l), cnst_name_cw(l)'
          do l = 1, pcnst
             write(*,'(i4,2(2x,a))') l, cnst_name(l), cnst_name_cw(l)
          end do
       end if

       return
     end subroutine initaermodes_set_cnstnamecw

#ifndef WRF_PORT
     !==============================================================
     subroutine modal_aero_initialize_q( name, q )
       !
       ! this routine is for initial testing of the modal aerosol cam3
       !
       ! it initializes several gas and aerosol species to 
       !    "low background" values, so that very short (e.g., 1 day)
       !    test runs are working with non-zero values
       !
       use constituents, only: pcnst, cnst_name
       use pmgrid,      only: plat, plon, plev

       implicit none

       !--------------------------------------------------------------
       ! ... arguments
       !--------------------------------------------------------------
       character(len=*), intent(in) :: name                   !  constituent name
       real(r8), intent(inout) :: q(plon,plev,plat)           !  mass mixing ratio

       !--------------------------------------------------------------
       ! ... local variables
       !--------------------------------------------------------------
       integer k, l
       real(r8) duma, dumb, dumz


       !
       ! to deactivate this routine, just return here
       !
       !     return


       if ( masterproc ) then
          write( *, '(2a)' )   &
               '*** modal_aero_initialize_q - name = ', name
          if (name == 'H2O2'   ) write( *, '(2a)' ) '    doing ', name
          if (name == 'SO2'    ) write( *, '(2a)' ) '    doing ', name
          if (name == 'H2SO4'  ) write( *, '(2a)' ) '    doing ', name
          if (name == 'DMS'    ) write( *, '(2a)' ) '    doing ', name
          if (name == 'NH3'    ) write( *, '(2a)' ) '    doing ', name
          if (name == 'so4_a1' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'so4_a2' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'pom_a3' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'ncl_a4' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'dst_a5' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'ncl_a6' ) write( *, '(2a)' ) '    doing ', name
          if (name == 'dst_a7' ) write( *, '(2a)' ) '    doing ', name
       end if

       do k = 1, plev

          ! init gases
          dumz = (k+1.0e-5)/(plev+1.0e-5)
          dumb = dumz*1.0e-9/28.966
          if (name == 'H2O2'   ) q(:,k,:) = dumb*34.0*1.0
          if (name == 'SO2'    ) q(:,k,:) = dumb*64.0*0.1
          if (name == 'H2SO4'  ) q(:,k,:) = dumb*98.0*0.001
          if (name == 'DMS'    ) q(:,k,:) = dumb*62.0*0.01
          if (name == 'NH3'    ) q(:,k,:) = dumb*17.0*0.1

          ! init first mass species of each aerosol mode
          duma = dumz*1.0e-10
          if (name == 'so4_a1' ) q(:,k,:) = duma*1.0
          if (name == 'so4_a2' ) q(:,k,:) = duma*0.002
          if (name == 'pom_a3' ) q(:,k,:) = duma*0.3
          if (name == 'ncl_a4' ) q(:,k,:) = duma*0.4
          if (name == 'dst_a5' ) q(:,k,:) = duma*0.5
          if (name == 'ncl_a6' ) q(:,k,:) = duma*0.6
          if (name == 'dst_a7' ) q(:,k,:) = duma*0.7

          ! init aerosol number
          !
          ! at k=plev, duma = 1e-10 kgaero/kgair = 0.1 ugaero/kgair
          !            dumb = duma/(2000 kgaero/m3aero)
          duma = dumz*1.0e-10
          dumb = duma/2.0e3
          ! following produces number 1000X too small, and Dp 10X too big
          !        dumb = dumb*1.0e-3
          ! following produces number 1000X too big, and Dp 10X too small
          !        dumb = dumb*1.0e3
          if (name == 'num_a1' ) q(:,k,:) = dumb*1.0  *3.0e20
          if (name == 'num_a2' ) q(:,k,:) = dumb*0.002*4.0e22
          if (name == 'num_a3' ) q(:,k,:) = dumb*0.3  *5.7e21
          if (name == 'num_a4' ) q(:,k,:) = dumb*0.4  *2.7e19
          if (name == 'num_a5' ) q(:,k,:) = dumb*0.5  *4.0e20
          if (name == 'num_a6' ) q(:,k,:) = dumb*0.6  *2.7e16
          if (name == 'num_a7' ) q(:,k,:) = dumb*0.7  *4.0e17

          !*** modal_aero_calcsize_sub - ntot_amode    7
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    1  1.100E-07  1.847E-07  3.031E+20  4.736E+18  2.635E+21
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    2  2.600E-08  3.621E-08  4.021E+22  5.027E+21  1.073E+24
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    3  5.000E-08  6.964E-08  5.654E+21  7.068E+20  7.068E+23
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    4  2.000E-07  4.112E-07  2.748E+19  2.198E+17  1.758E+21
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    5  1.000E-07  1.679E-07  4.035E+20  3.228E+18  3.228E+21
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    6  2.000E-06  4.112E-06  2.748E+16  3.434E+15  2.198E+17
          !mode, dgn, dp*, v2n, v2nhi, v2nlo    7  1.000E-06  1.679E-06  4.035E+17  5.043E+16  3.228E+18

       end do   ! k

       if ( masterproc ) then
          write( *, '(7x,a,1p,10e10.2)' )   &
               name, (q(1,k,1), k=plev,1,-5) 
       end if

       if (plev > 0) return


       if ( masterproc ) then
          write( *, '(/a,i5)' )   &
               '*** modal_aero_initialize_q - ntot_amode', ntot_amode
          do k = 1, ntot_amode
             write( *, '(/a)' ) 'mode, dgn, v2n',   &
                  k, dgnum_amode(k), voltonumb_amode(k)
          end do
       end if

       return
     end subroutine modal_aero_initialize_q
#endif
#ifdef WRF_PORT
!--------------------------------------------------------------------------------------------------------
     subroutine decouple_mam_mp(CAM_MP_MAM_cpled)
!       
!    Purpose: This subroutine is added so that CAMMGMP scheme can be decoupled from the CAM MAM package
!             If the user choose to decouple CAM MAM package and CAMMGMP scheme then the aerosol variables 
!             for the CAMMGMP scheme are initialized considering prescribed aerosols (ONLY for CAMMGMP)
!
!    *NOTE* The values for the aerosols variables should exactly match with value for prescribed aerosols in
!           phys/module_cam_mp_modal_aero_initialize_data_phys.F
!
!    Called by module_cam_mam_init.F
!    
!--------------------------------------------------------------------------------------------------------
       implicit none
       logical, intent(in):: CAM_MP_MAM_cpled
       integer, parameter :: init_val=-999888777
       integer  :: n
       real(r8) :: pi, tmpsg_mp(ntot_amode)
       
       modeptr_accum_mp         = modeptr_accum
       modeptr_coarse_mp        = modeptr_coarse
       modeptr_coardust_mp      = modeptr_coardust !BSINGH - declared for MAM7 complaince
       modeptr_aitken_mp        = modeptr_aitken

       numptrcw_amode_mp(:)     = numptrcw_amode(:)
       nspec_amode_mp(:)        = nspec_amode(:) 
       numptr_amode_mp(:)       = numptr_amode(:) 
       
       cnst_name_cw_mp(:)       = cnst_name_cw(:)
       sigmag_amode_mp(:)       = sigmag_amode(:)
       dgnum_amode_mp(:)        = dgnum_amode(:)
       
       voltonumbhi_amode_mp(:)  = voltonumbhi_amode(:)
       voltonumblo_amode_mp (:) = voltonumblo_amode(:)
       
       lptr_dust_a_amode_mp(:)  = lptr_dust_a_amode(:)
       lptr_nacl_a_amode_mp(:)  = lptr_nacl_a_amode(:)
       specdens_amode_mp(:)     = specdens_amode(:)
       alnsg_amode_mp(:)        = alnsg_amode(:)
       specmw_amode_mp(:)       = specmw_amode(:)
       spechygro_mp(:)          = spechygro(:)

       lmassptrcw_amode_mp(:,:) = lmassptrcw_amode(:,:) 
       lmassptr_amode_mp(:,:)   = lmassptr_amode(:,:) 
       lspectype_amode_mp(:,:)  = lspectype_amode(:,:)  

       if(.NOT.CAM_MP_MAM_cpled) then
          
          pi = 4.*atan(1._r8)
          
          !
          ! initialize needed variables in module modal_aero_data
          !
          
          modeptr_accum_mp  = 1
          modeptr_aitken_mp = 2
          modeptr_coarse_mp = 3
          
          ! setting portions of these arrays that should not be used
          ! to init_val should cause a seg-fault if they get use
          nspec_amode_mp(:) = init_val
          lspectype_amode_mp(:,:) = init_val
          lmassptr_amode_mp(:,:) = init_val
          numptr_amode_mp(:) = init_val
          lptr_dust_a_amode_mp(:) = init_val
          lptr_nacl_a_amode_mp(:) = init_val
          
          n = modeptr_accum_mp
          nspec_amode_mp(n) = 1
          lspectype_amode_mp(1,n) = 1  ! sulfate
          lmassptr_amode_mp(1,n) = 6   ! species 6 in state%q
          numptr_amode_mp(n) = 7   ! species 7 in state%q
          
          n = modeptr_aitken_mp
          nspec_amode_mp(n) = 1
          lspectype_amode_mp(1,n) = 1  ! sulfate
          lmassptr_amode_mp(1,n) = 8   ! species 8 in state%q
          numptr_amode_mp(n) = 9   ! species 9 in state%q
          
          n = modeptr_coarse_mp
          nspec_amode_mp(n) = 2
          lspectype_amode_mp(1,n) = 2  ! dust
          lspectype_amode_mp(2,n) = 3  ! seasalt
          lmassptr_amode_mp(1,n) = 10  ! species 10 in state%q
          lmassptr_amode_mp(2,n) = 11  ! species 11 in state%q
          numptr_amode_mp(n) = 12  ! species 12 in state%q
          lptr_dust_a_amode_mp(n) = lmassptr_amode_mp(1,n)
          lptr_nacl_a_amode_mp(n) = lmassptr_amode_mp(2,n)
          
          lmassptrcw_amode_mp = lmassptr_amode_mp
          numptrcw_amode_mp = numptr_amode_mp
          
          msectional_mp = 0
          alnsg_amode_mp(:) = log( sigmag_amode_mp(:) )
          tmpsg_mp = exp( 4.5 * (alnsg_amode_mp(:)**2) )
          
          voltonumb_amode_mp(  :) = 1.0/( (pi/6.0) * (dgnum_amode_mp(  :)**3) * tmpsg_mp )
          voltonumblo_amode_mp(:) = 1.0/( (pi/6.0) * (dgnumlo_amode_mp(:)**3) * tmpsg_mp )
          voltonumbhi_amode_mp(:) = 1.0/( (pi/6.0) * (dgnumhi_amode_mp(:)**3) * tmpsg_mp )
          
          specdens_amode_mp(:) = 1.0e3   ! match precribe_aerosol_mixactivate, but units change
          specmw_amode_mp(:) = 132.0     ! match precribe_aerosol_mixactivate
          spechygro_mp(:) = 0.5          ! match precribe_aerosol_mixactivate
       endif
       

     end subroutine decouple_mam_mp
#endif


     !==============================================================
   end module modal_aero_initialize_data

