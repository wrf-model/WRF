      module module_data_mosaic_main
!-----------------------------------------------------------------------
!
! 2015-06-09 r.c.easter - changes to the MOSAIC box model version for use in WRF-Chem  
!
! none so far
!
!-----------------------------------------------------------------------

      use module_data_mosaic_kind, only:  r8
      use module_data_mosaic_constants, only:  &
          avogad, deg2rad, pi, piover4, piover6, third


      implicit none

      integer, parameter ::   &
                ngas_com = 40,   &
                ngas_urb = 19,   &
                ngas_bio =  7,   &
                ngas_mar = 11
      !BSINGH - 05/28/2013(RCE updates)
      integer, parameter ::   &
                  naer_tot = 24 		      ! total num of 3-D variables per bin

      integer, save ::   &
                naerbin  = -999888777         ! number of bins (set at run time)
      !BSINGH - 05/28/2013(RCE updates ENDS)
!               naerbin  = 41760  	      ! ( 48 size)*(29 wbc)*(30 kappa)
!               naerbin  = 3240  	      ! ( 24 size)*(15 wbc)*( 9 kappa)
!               naerbin  = 90000 	      ! (100 size)*(30 wbc)*(30 kappa)

      integer, parameter ::   &
                ncld_tot = 13,		   &  ! + 8 = total num of 3-D variables/bin
                ncldbin  =  4,		   &  ! num of cloud bins
                ncld     = 22		! num of dynamic cloud species/bin

      integer, parameter :: ngas_max = ngas_com + ngas_urb + ngas_bio + ngas_mar
      
      integer, parameter :: ncld_max = ncld_tot*ncldbin
      !BSINGH - 05/28/2013(RCE updates)
      integer, save :: naer_max = -999888777  ! set at run time to naer_tot*naerbin

      integer, save :: ntot_max = -999888777  ! set at run time to (ngas_max + naer_max + ncld_max)
      !BSINGH - 05/28/2013(RCE updates ENDS)

      integer, save ::   &
      		naerbin_used=0,   &   ! num of aerosol bins being used
      		ncldbin_used=0,   &   ! num of  cloud  bins being used
      		ntot_used=ngas_max    ! portion of cnn array being used

      integer, save ::   &
      		ipmcmos = 0        ! if > 0, do emissions, dilution, air density,
      		                   ! and relative humidity as in partmc_mosaic 

      real(r8), parameter :: press0_pa = 1.01325d5  ! pressure of 1 atm [Pa]
      real(r8), parameter :: mw_air = 28.966d0      ! dry-air mean molecular weight [g/mol]


!-------------------------------------------------------------------------

      integer, save ::	m_partmc_mosaic  ! >0 for partmc_mosaic, <=0 for mosaic box model!BSINGH - 05/28/2013(RCE updates)

      integer, save ::	mgas, maer, mcld

      integer, save ::	maeroptic, mshellcore

      integer, save ::	msolar, mphoto


!------------------------------------------------------------------------

      end module module_data_mosaic_main
