      module module_progtm
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      implicit none
      SAVE
!
      integer,parameter:: NTYPE=9
      integer,parameter:: NGRID=22
      real(kind=kind_phys) B(NTYPE), SATPSI(NTYPE), SATKT(NTYPE),       &
     &                     TSAT(NTYPE),                                 & 
     &                     DFK(NGRID,NTYPE),                            &
     &                     KTK(NGRID,NTYPE),                            &
     &                     DFKT(NGRID,NTYPE)
!
!  the nine soil types are:
!    1  ... loamy sand (coarse)
!    2  ... silty clay loam (medium)
!    3  ... light clay (fine)
!    4  ... sandy loam (coarse-medium)
!    5  ... sandy clay (coarse-fine)
!    6  ... clay loam  (medium-fine)
!    7  ... sandy clay loam (coarse-med-fine)
!    8  ... loam  (organic)
!    9  ... ice (use loamy sand property)
!
!     DATA B/4.05,4.38,4.9,5.3,5.39,7.12,7.75,8.52,
!    &       10.4,10.4,11.4/
!     DATA SATPSI/.121,.09,.218,.786,.478,.299,.356,.63,
!    &            .153,.49,.405/
!     DATA SATKT/1.76E-4,1.5633E-4,3.467E-5,7.2E-6,6.95E-6,
!    &           6.3E-6,1.7E-6,2.45E-6,2.167E-6,1.033E-6,
!    &           1.283E-6/
!     DATA TSAT/.395,.41,.435,.485,.451,.42,.477,.476,
!    &          .426,.492,.482/
      data b/4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
      data satpsi/.04,.62,.47,.14,.10,.26,.14,.36,.04/
      data satkt/1.41e-5,.20e-5,.10e-5,.52e-5,.72e-5,                   &
     &           .25e-5,.45e-5,.34e-5,1.41e-5/
      data tsat/.421,.464,.468,.434,.406,.465,.404,.439,.421/
!
      contains
      subroutine GRDDF
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      implicit none
      integer              i,    k
      real(kind=kind_phys) dynw, f1, f2, theta
!
!  GRDDF SETS UP MOISTURE DIFFUSIVITY AND HYDROLIC CONDUCTIVITY
!  FOR ALL SOIL TYPES
!  GRDDFS SETS UP THERMAL DIFFUSIVITY FOR ALL SOIL TYPES
!
      DO K = 1, NTYPE
        DYNW = TSAT(K) * .05
        F1 = B(K) * SATKT(K) * SATPSI(K) / TSAT(K) ** (B(K) + 3.)
        F2 = SATKT(K) / TSAT(K) ** (B(K) * 2. + 3.)
!
!  CONVERT FROM M/S TO KG M-2 S-1 UNIT
!
        F1 = F1 * 1000.
        F2 = F2 * 1000.
        DO I = 1, NGRID
          THETA = FLOAT(I-1) * DYNW
          THETA = MIN(TSAT(K),THETA)
          DFK(I,K) = F1 * THETA ** (B(K) + 2.)
          KTK(I,K) = F2 * THETA ** (B(K) * 2. + 3.)
        ENDDO
      ENDDO
      END SUBROUTINE
      subroutine GRDKT
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      implicit none
      integer              i,    k
      real(kind=kind_phys) dynw, f1, theta, pf
      DO K = 1, NTYPE
        DYNW = TSAT(K) * .05
        F1 = LOG10(SATPSI(K)) + B(K) * LOG10(TSAT(K)) + 2.
        DO I = 1, NGRID
          THETA = FLOAT(I-1) * DYNW
          THETA = MIN(TSAT(K),THETA)
          IF(THETA.GT.0.) THEN
            PF = F1 - B(K) * LOG10(THETA)
          ELSE
            PF = 5.2
          ENDIF
          IF(PF.LE.5.1) THEN
            DFKT(I,K) = EXP(-(2.7+PF)) * 420.
          ELSE
            DFKT(I,K) = .1744
          ENDIF
        ENDDO
      ENDDO
      END SUBROUTINE
!
      end module module_progtm
