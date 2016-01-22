MODULE module_aer_opt_out
! SAM lower and upper wavelength limits (microns) for AFWA band averaging - 2 averaging bins considered here
   REAL,    PARAMETER, PRIVATE ::   afwalowv1   = 3.  ! lower wavelength for first AFWA band average extinction coefficent
   REAL,    PARAMETER, PRIVATE ::   afwahiwv1   = 5.  ! upper wavelength for first AFWA band average extinction coefficent
   REAL,    PARAMETER, PRIVATE ::   afwalowv2   = 8.  ! lower wavelength for second AFWA band average extinction coefficent
   REAL,    PARAMETER, PRIVATE ::   afwahiwv2   = 12. ! upper wavelength for second AFWA band average extinction coefficent
CONTAINS
   SUBROUTINE aer_opt_out(dz8w  &
                   ,ext_coeff,bscat_coeff,asym_par                &
                   ,tauaer300,tauaer400,tauaer600,tauaer999       & ! jcb
                   ,gaer300,gaer400,gaer600,gaer999               & ! jcb
                   ,waer300,waer400,waer600,waer999               & ! jcb
                   ,num_ext_coef,num_bscat_coef,num_asym_par    &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte )
USE module_configure, only:p_extcof3,p_extcof55,p_extcof106,p_extcof3_5,p_extcof8_12,p_bscof3,p_bscof55, &
           p_bscof106,p_asympar3,p_asympar55,p_asympar106

   IMPLICIT NONE
   INTEGER,    INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte, &
                                       num_ext_coef,num_bscat_coef,num_asym_par
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:num_ext_coef ), INTENT (OUT) :: ext_coeff
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:num_bscat_coef ), INTENT (OUT) :: bscat_coeff
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:num_asym_par ), INTENT (OUT) :: asym_par
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        &
         INTENT(IN    ) :: tauaer300,tauaer400,tauaer600,tauaer999, &
                                 gaer300,gaer400,gaer600,gaer999, & 
                              waer300,waer400,waer600,waer999,dz8w
   real :: ang,slope,slopeg,slopessa,onemang    
   integer :: i,j,k


!SAM 10/22/09 AFWA ouput.  Fill following arrays:
! 0.3 micron extinction coefficient (1/km), scattering coefficient (1/km), assymetry coefficient (unitless)
! 0.55 micron extinction coefficient (1/km), scattering coefficient (1/km), assymetry coefficient (unitless)
! 1.06 micron extinction coefficient (1/km), scattering coefficient (1/km), assymetry coefficient (unitless)
! 3. - 5. micron band averaged extinction coefficient (1/km)
! 8. - 12. micron band averaged extinction coefficient (1/km)
! As in PNNL MOSAIC, extrapolate or interpolate based on 300-999 nm Angstrom coefficient,
! or linear interpolation/extrapolation between 300 and 999 nm for assymetry coefficient
      do j = jts,jte
      do k = kts,kte
      do i = its,ite
! convert optical properties at 300,400,600, and 999 to conform to the band wavelengths
! these are: 300, 550 and 1060
! 300 nm already calculated in aerosol_optical_averaging and miecalc
      ext_coeff(i,k,j,p_extcof3)=tauaer300(i,k,j)*1.E3/dz8w(i,k,j)  ! 300nm ext. coeff. (1/km)
      bscat_coeff(i,k,j,p_bscof3)=tauaer300(i,k,j)*waer300(i,k,j)*1.E3/dz8w(i,k,j)  ! 300nm scat. coeff. (1/km)
      asym_par(i,k,j,p_asympar3)=gaer300(i,k,j)  ! 300nm assym. parameter (no units)
! 550 nm done like PNNL
           ang=log(tauaer300(i,k,j)/tauaer999(i,k,j))/log(999./300.)
           slopessa=(waer600(i,k,j)-waer400(i,k,j))/.2
           slopeg=(gaer600(i,k,j)-gaer400(i,k,j))/.2
      ext_coeff(i,k,j,p_extcof55)=tauaer400(i,k,j)*1.E3*((0.4/0.55)**ang)/dz8w(i,k,j)  ! 550nm ext. coeff. (1/km)
      slope= slopessa*(0.55-.6)+waer600(i,k,j) ! slope is scratch variable, = single scat albedo at .55 micron
      slope=AMIN1(1.0,AMAX1(0.4,slope))   ! SSA has same limits as in PNNL
      bscat_coeff(i,k,j,p_bscof55)=ext_coeff(i,k,j,p_extcof55)*slope  ! 550nm scat. coeff. (1/km)
      asym_par(i,k,j,p_asympar55)=AMIN1(1.,AMAX1(0.5,slopeg*(.55-.6)+gaer600(i,k,j)))  ! 550nm assym. parameter (no units)
! 1060 nm done like PNNL
           slopessa=(waer999(i,k,j)-waer600(i,k,j))/.399
           slopeg=(gaer999(i,k,j)-gaer600(i,k,j))/.399
      ext_coeff(i,k,j,p_extcof106)=tauaer400(i,k,j)*1.E3*((0.4/1.06)**ang)/dz8w(i,k,j)  ! 1060nm ext. coeff. (1/km)
      slope= slopessa*(1.06-.999)+waer999(i,k,j) ! slope is scratch variable, = single scat albedo at 1.06 micron
      slope=AMIN1(1.0,AMAX1(0.4,slope))   ! SSA has same limits as in PNNL
      bscat_coeff(i,k,j,p_bscof106)=ext_coeff(i,k,j,p_extcof106)*slope  ! 1060nm scat. coeff. (1/km)
      asym_par(i,k,j,p_asympar106)=AMIN1(1.,AMAX1(0.5,slopeg*(1.06-.999)+gaer600(i,k,j)))  ! 1060nm assym. parameter (no units)
! 3.-5.  and 8. - 12. micron band averages done by extrapolating .3-.999 calculations, like PNNL
      onemang=1.-ang
      if(abs(onemang).gt.1.E-3)then   ! if ang sufficiently different than one, no need to worry about singularity
      slope = tauaer400(i,k,j)*(0.4/afwalowv1)**ang  ! Dummy incrumental tau at afwa lower wavelength for band average
      slopeg = tauaer400(i,k,j)*(0.4/afwahiwv1)**ang  ! Dummy incrumental tau at afwa high wavelength for band average
      ext_coeff(i,k,j,p_extcof3_5) = (slopeg*afwahiwv1-slope*afwalowv1)/(afwahiwv1-afwalowv1)/onemang
      slope = tauaer400(i,k,j)*(0.4/afwalowv2)**ang  ! Dummy incrumental tau at afwa lower wavelength for band average
      slopeg = tauaer400(i,k,j)*(0.4/afwahiwv2)**ang  ! Dummy incrumental tau at afwa high wavelength for band average
      ext_coeff(i,k,j,p_extcof8_12) = (slopeg*afwahiwv2-slope*afwalowv2)/(afwahiwv2-afwalowv2)/onemang
      else                           ! ang is close to 1., avoid singularity
      ext_coeff(i,k,j,p_extcof3_5) = tauaer400(i,k,j)*0.4*log(afwahiwv1/afwalowv1)/(afwahiwv1-afwalowv1)
      ext_coeff(i,k,j,p_extcof8_12) = tauaer400(i,k,j)*0.4*log(afwahiwv2/afwalowv2)/(afwahiwv2-afwalowv2)
      endif
! Convert band average incrumental taus to extinction coefficients (1/km)
      ext_coeff(i,k,j,p_extcof3_5) = ext_coeff(i,k,j,p_extcof3_5)*1.E3/dz8w(i,k,j)
      ext_coeff(i,k,j,p_extcof8_12) = ext_coeff(i,k,j,p_extcof8_12)*1.E3/dz8w(i,k,j)
      end do
      end do
      end do
  END SUBROUTINE AER_OPT_OUT 
END MODULE module_aer_opt_out

