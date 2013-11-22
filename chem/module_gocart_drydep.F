MODULE module_gocart_drydep

CONTAINS

         subroutine gocart_drydep_driver(dtstep,               &
               config_flags,numgas,                            &
               t_phy,moist,p8w,t8w,rmol,aer_res,               &
               p_phy,chem,rho_phy,dz8w,ddvel,xland,hfx,        &
               ivgtyp,tsk,vegfra,pbl,ust,znt,xlat,xlong,       &
               dustdrydep_1,dustdrydep_2,dustdrydep_3,         &
               dustdrydep_4,dustdrydep_5,                      &
               depvelocity,                                    &                     
               ids,ide, jds,jde, kds,kde,                      &
               ims,ime, jms,jme, kms,kme,                      &
               its,ite, jts,jte, kts,kte                       )

  USE module_model_constants
  USE module_configure
  USE module_state_description

  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte,numgas
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,    &
          INTENT(IN   ) ::                                      &
                                                          ivgtyp

   REAL,      INTENT(IN   ) ::                       dtstep
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),     &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),      &
         INTENT(INOUT ) ::                                 chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                      p_phy,    &
                                                      dz8w,     &
                                              t8w,p8w,rho_phy
   REAL, DIMENSION( its:ite, jts:jte, num_chem ),               &
          INTENT(INOUT) ::                            ddvel
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
          INTENT(IN) ::                                         &
                                                     tsk,       &
                                                  vegfra,       &
                                                     pbl,       &
                                                     ust,       &
                                                     xlat,      &
                                                     xlong,     &
                                         rmol,xland,znt,hfx
   REAL, DIMENSION( its:ite, jts:jte ),                         &
         INTENT(IN)         ::                       aer_res
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::        &
                dustdrydep_1, dustdrydep_2, dustdrydep_3,       &
                dustdrydep_4, dustdrydep_5, depvelocity                 

!! .. Local Scalars ..
      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                 n, nr, ipr, jpr, nvr,   &
                 idrydep_onoff,imx,jmx,lmx

      integer :: ii,jj,kk,i,j,k,nv
      integer,dimension (1,1) :: ilwi,ireg

      REAL ::  clwchem,  dvfog, dvpart,  &
               rad, rhchem, ta, vegfrac, z1,zntt
!     real*8, DIMENSION (1,1,1,3) :: erodin
!     real*8, DIMENSION (5) :: tc,bems
!     real*8, dimension (1,1) :: z0,w10m,gwet,airden,airmas,delz_sfc,hflux,ts,pblz,ustar,ps
      real*8, dimension (1,1) :: z0,airden,delz_sfc,hflux,ts,pblz,ustar,ps
      REAL*8  :: dvel(1,1), drydf(1,1)
      LOGICAL :: highnh3, rainflag, vegflag, wetflag

      TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

  do nv=numgas+1,num_chem
    do j=jts,jte
      do i=its,ite
        ddvel(i,j,nv) = 0.
      enddo
    enddo
  enddo

  imx = 1
  jmx = 1
  lmx = 1
  do j = max(jds+1,jts),min(jde-1,jte)
    do i = max(ids+1,its),min(ide-1,ite)
      ipr = 0
      dvel(1,1) = 0._8
      if( xland(i,j) > 1.5 ) then
        ilwi(1,1) = 1
      else
        ilwi(1,1) = 0
      endif
! for aerosols, ii=1 or ii=2
      ii = 1
!     if(ivgtyp(i,j).eq.19.or.ivgtyp(i,j).eq.23)ii=1
      ireg(1,1) = 1
      airden(1,1)   = real( rho_phy(i,kts,j),kind=8 )
      delz_sfc(1,1) = real( dz8w(i,kts,j),kind=8 )
      ustar(1,1) = max( 1.e-1_8,real(ust(i,j),kind=8) )
      hflux(1,1) = real( hfx(i,j),kind=8 )
      pblz(1,1)  = real( pbl(i,j),kind=8 )
      ps(1,1) = real(p8w(i,kts,j),kind=8)*.01_8
      z0(1,1) = real( znt(i,j),kind=8 )
      ts(1,1) = real( tsk(i,j),kind=8 )
!    if(i.eq.23.and.j.eq.74)ipr=1
      call depvel_gocart(config_flags,ipr,ii,imx,jmx,lmx,&
                         airden, delz_sfc, pblz, ts, ustar, hflux, ilwi, &
                         ps, z0, dvel, drydf,g,rmol(i,j),aer_res(i,j))
      do nv = p_p25,num_chem
        ddvel(i,j,nv) = real( dvel(1,1),kind=4 )
      enddo
      ddvel(i,j,p_sulf) = real( dvel(1,1),kind=4 )
      ddvel(i,j,p_msa)  = real( dvel(1,1),kind=4 )
      
      depvelocity(i,j) = ddvel(i,j,p_dust_5)
! drydep [ug/m2/s] = dvel [m/s] * chem [ug/kg] * airden [kg/m3]
      dustdrydep_1(i,j)=-dvel(1,1)*chem(i,1,j,p_dust_1)*airden(1,1)
      dustdrydep_2(i,j)=-dvel(1,1)*chem(i,1,j,p_dust_2)*airden(1,1)
      dustdrydep_3(i,j)=-dvel(1,1)*chem(i,1,j,p_dust_3)*airden(1,1)
      dustdrydep_4(i,j)=-dvel(1,1)*chem(i,1,j,p_dust_4)*airden(1,1)
      dustdrydep_5(i,j)=-dvel(1,1)*chem(i,1,j,p_dust_5)*airden(1,1)   
      
    enddo
  enddo

end subroutine gocart_drydep_driver

SUBROUTINE depvel_gocart( config_flags,ipr,ii,imx,jmx,lmx, &
                          airden, delz_sfc, pblz, ts, ustar, hflux, ilwi, &
                          ps, z0, dvel, drydf,g0,rmol,aer_res)

! ****************************************************************************
! *                                                                          *
! *  Calculate dry deposition velocity.                                      *
! *                                                                          *
! *  Input variables:                                                        *
! *    AEROSOL(k)      - Logical, T = aerosol species, F = gas species       *
! *    IREG(i,j)       - # of landtypes in grid square                       *
! *    ILAND(i,j,ldt)  - Land type ID for element ldt =1,IREG(i,j)           *
! *    IUSE(i,j,ldt)   - Fraction of gridbox area occupied by land type      *
! *                      element ldt                                         *
! *    USTAR(i,j)      - Friction velocity (m s-1)                           *
! *    DELZ_SFC(i,j)   - Thickness of layer above surface                    *
! *    PBLZ(i,j)       - Mixing depth (m)                                    *
! *    Z0(i,j)         - Roughness height (m)                                *
! *                                                                          *
! *  Determined in this subroutine (local):                                  *
! *    OBK             - Monin-Obukhov length (m): set to 1.E5 m under       *
! *                      neutral conditions                                  *
! *    Rs(ldt)         - Bulk surface resistance(s m-1) for species k to     * 
! *                      surface ldt                                         *
! *    Ra              - Aerodynamic resistance.                             *
! *    Rb              - Sublayer resistance.                                *
! *    Rs              - Surface resistance.                                 *
! *    Rttl            - Total deposition resistance (s m-1) for species k   *
! *                      Rttl(k) = Ra + Rb + Rs.                             *
! *                                                                          *
! *  Returned:                                                               *
! *    DVEL(i,j,k)     - Deposition velocity (m s-1) of species k            *
! *    DRYDf(i,j,k)    - Deposition frequency (s-1) of species k,            *
! *                    = DVEL / DELZ_SFC                                     *
! *                                                                          *
! **************************************************************************** 

  USE module_configure

  IMPLICIT NONE

  REAL*8,    INTENT(IN)  :: airden(imx,jmx), delz_sfc(imx,jmx)
  REAL*8,    INTENT(IN)  :: hflux(imx,jmx), ts(imx,jmx)
  REAL*8,    INTENT(IN)  :: ustar(imx,jmx), pblz(imx,jmx)
  REAL*8,    INTENT(IN)  :: ps(imx,jmx)
  INTEGER, INTENT(IN)  :: ilwi(imx,jmx)
  INTEGER, INTENT(IN)  :: imx,jmx,lmx
  REAL*8,    INTENT(IN)  :: z0(imx,jmx)
  REAL,    INTENT(IN)  :: g0,rmol,aer_res
  REAL*8,    INTENT(OUT) :: dvel(imx,jmx), drydf(imx,jmx)

  TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
  
  REAL*8    :: obk, vds, czh, rttl, frac, logmfrac, psi_h, cz, eps
  REAL*8    :: vd, ra, rb, rs  
  INTEGER :: ipr,i, j, k, ldt, iolson, ii
  CHARACTER(LEN=50) :: msg
  REAL*8     :: prss, tempk, tempc, xnu, ckustr, reyno, aird, diam, xm, z
  REAL*8     :: frpath, speed, dg, dw, rt
  REAL*8     :: rad0, rix, gfact, gfaci, rdc, rixx, rluxx, rgsx, rclx
  REAL*8     :: dtmp1, dtmp2, dtmp3, dtmp4
  REAL*8     :: biofit,vk

  ! executable statements
  j_loop: DO j = 1,jmx               
     i_loop: DO i = 1,imx            
        vk    = .4_8
        vd    = 0._8
        ra    = 0._8
        rb    = 0._8 ! only required for gases (SO2)
        rs = 0.0_8

! ****************************************************************************
! *  Compute the the Monin-Obhukov length.                                   *
! *  The direct computation of the Monin-Obhukov length is:                  *
! *                                                                          *
! *           - Air density * Cp * T(surface air) * Ustar^3                  *
! *   OBK =   ----------------------------------------------                 *
! *                    vK   * g  * Sensible Heat flux                        *
! *                                                                          *
! *    Cp = 1000 J/kg/K    = specific heat at constant pressure              *
! *    vK = 0.4            = von Karman's constant                           *
! ****************************************************************************

        IF (abs(hflux(i,j)) <= 1.e-5_8) THEN
           obk = 1.0E5_8
        ELSE
           ! MINVAL(hflux), MINVAL(airden), MINVAL(ustar) =??
           obk = -airden(i,j) * 1000.0_8 * ts(i,j) * (ustar(i,j))**3 &
                / (vk * real(g0,kind=8) * hflux(i,j)) 
! -- debug:
           IF ( obk == 0.0_8 ) WRITE(*,211) obk, i, j
211        FORMAT(1X,'OBK=', E11.2, 1X,' i,j = ', 2I4)
           
        END IF
!       write(0,*)1./obk,rmol
        
        if(rmol.ne.0.)then
          obk=1._8/real(rmol,kind=8)
        else
          obk=1.e5_8
        endif

!       cz = delz_sfc(i,j) / 2.0_8 ! center of the grid box above surface
        cz = 2._8

! ****************************************************************************
! *  (1) Aerosodynamic resistance Ra and sublayer resistance Rb.             *
! *                                                                          *
! *  The Reynolds number REYNO diagnoses whether a surface is                *
! *  aerodynamically rough (REYNO > 10) or smooth.  Surface is               *
! *  rough in all cases except over water with low wind speeds.              *
! *                                                                          *
! *  For gas species over land and ice (REYNO >= 10) and for aerosol         *
! *  species for all surfaces:                                               *
! *                                                                          *
! *      Ra = 1./VT          (VT from GEOS Kzz at L=1, m/s).                 *
! *                                                                          *
! *  The following equations are from Walcek et al, 1986:                    *
! *                                                                          *
! *  For gas species when REYNO < 10 (smooth), Ra and Rb are combined        *
! *  as Ra:                                                                  *
! *                                                                          *
! *      Ra = { ln(ku* z1/Dg) - Sh } / ku*           eq.(13)                 *
! *                                                                          *
! *      where z1 is the altitude at the center of the lowest model layer    *
! *               (CZ);                                                      *
! *            Sh is a stability correction function;                        *
! *            k  is the von Karman constant (0.4, vK);                      *
! *            u* is the friction velocity (USTAR).                          *
! *                                                                          *
! *   Sh is computed as a function of z1 and L       eq ( 4) and (5)):       *
! *                                                                          *
! *    0 < z1/L <= 1:     Sh = -5 * z1/L                                     *
! *    z1/L < 0:          Sh = exp{ 0.598 + 0.39*ln(E) - 0.09(ln(E))^2 }     *
! *                       where E = min(1,-z1/L) (Balkanski, thesis).        *
! *                                                                          *
! *   For gas species when REYNO >= 10,                                      *
! *                                                                          *
! *      Rb = 2/ku* (Dair/Dg)**(2/3)                 eq.(12)                 *
! *      where Dg is the gas diffusivity, and                                *
! *            Dair is the air diffusivity.                                  *
! *                                                                          *
! *  For aerosol species, Rb is combined with surface resistance as Rs.      *
! *                                                                          *
! ****************************************************************************

        frac = cz / obk
        IF (frac > 1.0_8) frac = 1.0_8
        IF (frac > 0.0_8 .AND. frac <= 1.0_8) THEN 
           psi_h = -5.0_8*frac
        ELSE IF (frac < 0.0_8) THEN
           eps = MIN(1.0_8, -frac)
           logmfrac = LOG(eps)
           psi_h = EXP( 0.598_8 + 0.39_8 * logmfrac - 0.09_8 * (logmfrac)**2 )
        END IF
           !--------------------------------------------------------------
           !  Aerosol species, Rs here is the combination of Rb and Rs.

           ra = (LOG(cz/z0(i,j)) - psi_h) / (vk*ustar(i,j))
        
           vds = 0.002_8*ustar(i,j)
           IF (obk < 0.0_8) vds = vds * (1.0_8+(-300.0_8/obk)**0.6667_8)

           czh  = pblz(i,j)/obk
           IF (czh < -30.0_8) vds = 0.0009_8*ustar(i,j)*(-czh)**0.6667_8
           if(ipr.eq.1) write(0,*)ra,aer_res,vds

           IF( config_flags%chem_opt /= CHEM_VASH      .and.                  &
               config_flags%chem_opt /= DUST           )THEN
              ra = real(aer_res,kind=8)
           ENDIF

           ! --Set Vds to be less than VDSMAX (entry in input file divided --
           !   by 1.E4). VDSMAX is taken from Table 2 of Walcek et al. [1986].
           !   Invert to get corresponding R
!          if(ii.eq.1) then
!             rs=1.0_8/MIN(vds,2.0e-2_8)
!          else
              rs=1.0_8/MIN(vds,2.0e-3_8)
!          endif
           

        ! ------ Set max and min values for bulk surface resistances ------

           rs= MAX(1.0_8, MIN(rs, 9.9990e+3_8))

! ****************************************************************************
! *                                                                          *
! *  Compute dry deposition velocity.                                        *
! *                                                                          *
! *  IUSE is the fraction of the grid square occupied by surface ldt in      *
! *  units of per mil (IUSE=500 -> 50% of the grid square).  Add the         *
! *  contribution of surface type ldt to the deposition velocity; this is    *
! *  a loop over all surface types in the gridbox.                           *
! *                                                                          *
! *  Total resistance = Ra + Rb + Rs.
! *                                                                          *
! ****************************************************************************

           rttl = ra + rb + rs
           vd   = vd + 1._8/rttl

        ! ------ Load array DVEL ------
           if(ipr.eq.1) write(0,*)rs,ra,rb,vd
           dvel(i,j) = vd !* 1.2

        ! -- Set a minimum value for DVEL
        !    MIN(VdSO2)      = 2.0e-3 m/s  over ice
        !                    = 3.0e-3 m/s  over land
        !    MIN(vd_aerosol) = 1.0e-4 m/s

           IF (dvel(i,j) < 1.0E-4_8) dvel(i,j) = 1.0E-4_8
        drydf(i,j) = dvel(i,j) / delz_sfc(i,j)

     END DO i_loop
  END DO j_loop

END SUBROUTINE depvel_gocart



END MODULE module_gocart_drydep
