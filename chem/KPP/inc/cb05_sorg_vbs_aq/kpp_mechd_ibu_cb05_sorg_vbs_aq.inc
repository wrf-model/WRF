!
! Temperature and pressure
  tempk = t_phy(i,k,j)
  pressure = p_phy(i,k,j)
!  print*,"in het chem, pressure=",pressure
! Heterogeneous reactions
! Cell number
  NCELL = BLKSIZE

! Rate constants for heterogeneous reactions on particles
! Gas-phase only, no aerosol present
! Update reaction rate constants
  PMCONC ( : ) = 0.0
  PMNUM ( : ) = 0.0
  PMSG (1) = 1.7
  PMSG (2) = 2.0
  PMSG (3) = 2.5
iphase = 1
itype = 1
!     do itype=1,ntype_aer
        DO INASEC = 1, NASECT-1
           l = lptr_so4_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_no3_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_cl_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_nh4_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_na_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_p25_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_orgpa_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_ec_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_asoa1_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_asoa2_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_asoa3_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_asoa4_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_bsoa1_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_bsoa2_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_bsoa3_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = lptr_bsoa4_aer(INASEC,itype,iphase)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)
           l = waterptr_aer(INASEC,itype)
           if (l.gt.1) PMCONC ( INASEC ) = PMCONC ( INASEC ) &
                                         + max ( chem(i,k,j,l), CMIN)

           l = numptr_aer(INASEC,itype,iphase)
           if (l.gt.1) PMNUM ( INASEC ) = PMNUM ( INASEC ) &
                                        + max ( chem(i,k,j,l), CMIN)
        END DO

           itype = 2
           l = numptr_aer(1,itype,iphase)
           if (l.gt.1) PMNUM ( 3 ) = PMNUM ( 3 ) &
                                        + max ( chem(i,k,j,l), CMIN)
           l = lptr_anth_aer(1,itype,iphase)
           if (l.gt.1) PMCONC ( 3 ) = PMCONC ( 3 ) &
                                        + max ( chem(i,k,j,l), CMIN)
           l = lptr_seas_aer(1,itype,iphase)
           if (l.gt.1) PMCONC ( 3 ) = PMCONC ( 3 ) &
                                        + max ( chem(i,k,j,l), CMIN)
           l = lptr_soil_aer(1,itype,iphase)
           if (l.gt.1) PMCONC ( 3 ) = PMCONC ( 3 ) &
                                        + max ( chem(i,k,j,l), CMIN)
!     end do
!  DRAT = ( DPUP / DPLOW ) ** ( 1.0 / REAL ( NASECT, KIND=sp) )
  DO INASEC = 1, NASECT
     PMCONC ( INASEC ) = MAX(PMCONC(INASEC)*rho_phy(i,k,j),CMIN)
     PMNUM ( INASEC ) = MAX(PMNUM(INASEC)*rho_phy(i,k,j),CMIN)
     DPCTR ( INASEC ) = MAX(PMDGMIN,(6.0*PMCONC(INASEC)/DENSP   &
                        /PMNUM(INASEC)/PI*1.0E+6               &
                        /exp(4.5*(log(PMSG(INASEC)))**2.0))   &
                        **(1.0/3.0))
!     DPCTR ( INASEC ) = ( 6.0 * ( PMCONC( INASEC ) / DENSP &
!                      / PMNUM( INASEC )* 1.0E+6 )          &
!                      / PI ) ** ( 1.0 / 3.0 )
!     DPLO = DPLOW * DRAT ** ( REAL ( INASEC, KIND=sp ) - 1.0 )
!     DPHI = DPLOW * DRAT ** REAL ( INASEC, KIND=sp )
!     if ( DPCTR ( INASEC ) .lt. DPLO ) DPCTR ( INASEC ) = DPLO
!     if ( DPCTR ( INASEC ) .gt. DPHI ) DPCTR ( INASEC ) = DPHI
 !    DPCTR ( INASEC ) = ( DPLOW * DRAT ** ( REAL ( INASEC, KIND=sp ) - 1.0 ) &
 !                     * DPLOW * DRAT ** REAL ( INASEC, KIND=sp ) ) ** 0.5
  END DO

  RTDAT_AE ( : ) = 0.0

  CALL AERORATE_SO2( TEMPK, PRESSURE, RTDAT_AE( : ) )

  rtdat_ae_so2 = REAL (RTDAT_AE( ISO2 ), KIND=dp)
