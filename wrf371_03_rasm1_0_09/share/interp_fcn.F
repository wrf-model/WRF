MODULE module_interp_info
     INTEGER , PARAMETER :: NOT_DEFINED_YET    = 0
     INTEGER , PARAMETER :: BILINEAR           = 1
     INTEGER , PARAMETER :: SINT               = 2
     INTEGER , PARAMETER :: NEAREST_NEIGHBOR   = 3
     INTEGER , PARAMETER :: QUADRATIC          = 4
     INTEGER , PARAMETER :: SPLINE             = 5
     INTEGER , PARAMETER :: SINT_NEW           = 12

     INTEGER             :: interp_method_type = 0
CONTAINS
   SUBROUTINE interp_info_init
#if (EM_CORE == 1)
     CALL nl_get_interp_method_type ( 1 , interp_method_type )
#else
     interp_method_type = 2
#endif
   END SUBROUTINE interp_info_init
END MODULE module_interp_info

!WRF:MEDIATION_LAYER:INTERPOLATIONFUNCTION
!
!

!=========================================================================

   SUBROUTINE interp_init
      USE module_interp_info
      CALL interp_info_init
   END SUBROUTINE interp_init

!=========================================================================

#if ! defined(NMM_CORE) || NMM_CORE!=1
   SUBROUTINE interp_fcn    ( cfld,                                 &  ! CD field
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  ! ND field
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  ! stencil half width for interp
                              imask,                                &  ! interpolation mask
                              xstag, ystag,                         &  ! staggering of field
                              ipos, jpos,                           &  ! Position of lower left of nest in CD
                              nri, nrj                              )  ! Nest ratio, i- and j-directions

     USE module_interp_info      

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     IF      ( interp_method_type .EQ. NOT_DEFINED_YET  ) THEN
        interp_method_type = SINT
     END IF

     IF      ( interp_method_type .EQ. BILINEAR         ) THEN
       CALL interp_fcn_blint ( cfld,                                 &  ! CD field
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  ! ND field
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  ! stencil half width for interp
                               imask,                                &  ! interpolation mask
                               xstag, ystag,                         &  ! staggering of field
                               ipos, jpos,                           &  ! Position of lower left of nest in CD
                               nri, nrj)                                ! Nest ratio, i- and j-directions
     ELSE IF ( MOD(interp_method_type,10) .EQ. SINT             ) THEN
       CALL interp_fcn_sint  ( cfld,                                 &  ! CD field
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  ! ND field
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  ! stencil half width for interp
                               imask,                                &  ! interpolation mask
                               xstag, ystag,                         &  ! staggering of field
                               ipos, jpos,                           &  ! Position of lower left of nest in CD
                               nri, nrj)                                ! Nest ratio, i- and j-directions
     ELSE IF ( interp_method_type .EQ. NEAREST_NEIGHBOR ) THEN
       CALL interp_fcn_nn    ( cfld,                                 &  ! CD field
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  ! ND field
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  ! stencil half width for interp
                               imask,                                &  ! interpolation mask
                               xstag, ystag,                         &  ! staggering of field
                               ipos, jpos,                           &  ! Position of lower left of nest in CD
                               nri, nrj)                                ! Nest ratio, i- and j-directions
     ELSE IF ( interp_method_type .EQ. QUADRATIC        ) THEN
       CALL interp_fcn_lagr  ( cfld,                                 &  ! CD field
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  ! ND field
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  ! stencil half width for interp
                               imask,                                &  ! interpolation mask
                               xstag, ystag,                         &  ! staggering of field
                               ipos, jpos,                           &  ! Position of lower left of nest in CD
                               nri, nrj)                                ! Nest ratio, i- and j-directions
     ELSE
        CALL wrf_error_fatal ('Hold on there cowboy, we need to know which interpolation option you want')
     END IF

   END SUBROUTINE interp_fcn

!=========================================================================

! Overlapping linear horizontal iterpolation for mass, u, and v staggerings.

   SUBROUTINE interp_fcn_blint    ( cfld,                                 &  ! CD field
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  ! ND field
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  ! stencil half width for interp
                              imask,                                &  ! interpolation mask
                              xstag, ystag,                         &  ! staggering of field
                              ipos, jpos,                           &  ! Position of lower left of nest in CD
                              nri, nrj)                                ! Nest ratio, i- and j-directions

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, ioff, joff, i, j, k
     REAL :: wx, wy, cfld_ll, cfld_lr, cfld_ul, cfld_ur
     REAL :: cxp0, cxp1, nx, cyp0, cyp1, ny

     !  Fortran functions.  Yes, yes, I know, probably pretty slow.

     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     !  This stag stuff is to keep us away from the outer most row
     !  and column for the unstaggered directions.  We are going to 
     !  consider "U" an xstag variable and "V" a ystag variable.  The
     !  vertical staggering is handled in the actual arguments.  The
     !  ckte and nkte are the ending vertical dimensions for computations
     !  for this particular variable.

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     !  Loop over each j-index on this tile for the nested domain.

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        !  This is the lower-left j-index of the CG.

        !  Example is 3:1 ratio, mass-point staggering.  We have listed six CG values
        !  as an example: A, B, C, D, E, F.  For a 3:1 ratio, each of these CG cells has
        !  nine associated FG points.
        !  |=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |
        !  | -  D  - | -  E  - | -  F  - |
        !  |         |         |         |
        !  | 1  2  3 | 4  5  6 | 7  8  9 |
        !  |=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |
        !  | -  A  - | -  B  - | -  C  - |
        !  |         |         |         |
        !  | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|
        !  To interpolate to FG point 4, we will use CG points: A, B, D, E.  It is adequate to
        !  find the lower left point.  The lower left (LL) point for "4" is "A".  Below
        !  are a few more points.
        !  2 => A
        !  3 => A
        !  4 => A
        !  5 => B
        !  6 => B
        !  7 => B

        cj = compute_CGLL ( nj , jpos , nrj , jstag )
        ny = REAL(nj)
        cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 
        cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

        !  What is the weighting for this CG point to the FG point, j-weight only.

        wy = ( cyp1 - ny ) / ( cyp1 - cyp0 )

        !  Vertical dim of the nest domain.

        k_loop : DO nk = nkts, nkte

          !  Loop over each i-index on this tile for the nested domain.

           i_loop : DO ni = nits, MIN(nide-istag,nite)

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN
 
                 !  The coarse grid location that is to the lower left of the FG point.
   
                 ci = compute_CGLL ( ni , ipos , nri , istag )
                 nx = REAL(ni)
                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 
                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 
   
                 wx = ( cxp1 - nx ) / ( cxp1 - cxp0 )
   
                 !  The four surrounding CG values.
   
                 cfld_ll = cfld(ci  ,nk,cj  )
                 cfld_lr = cfld(ci+1,nk,cj  )
                 cfld_ul = cfld(ci  ,nk,cj+1)
                 cfld_ur = cfld(ci+1,nk,cj+1)

                 !  Bilinear interpolation in horizontal.

                 nfld( ni , nk , nj ) =     wy  * ( cfld_ll * wx + cfld_lr * (1.-wx) ) + &
                                        (1.-wy) * ( cfld_ul * wx + cfld_ur * (1.-wx) )

              END IF
           END DO i_loop
        END DO    k_loop
     END DO       j_loop

   END SUBROUTINE interp_fcn_blint

!=========================================================================

! Lagrange interpolating polynomials, set up as a quadratic, with an average of
! the overlap.

   SUBROUTINE interp_fcn_lagr ( cfld,                                 &  ! CD field
                                cids, cide, ckds, ckde, cjds, cjde,   &
                                cims, cime, ckms, ckme, cjms, cjme,   &
                                cits, cite, ckts, ckte, cjts, cjte,   &
                                nfld,                                 &  ! ND field
                                nids, nide, nkds, nkde, njds, njde,   &
                                nims, nime, nkms, nkme, njms, njme,   &
                                nits, nite, nkts, nkte, njts, njte,   &
                                shw,                                  &  ! stencil half width for interp
                                imask,                                &  ! interpolation mask
                                xstag, ystag,                         &  ! staggering of field
                                ipos, jpos,                           &  ! Position of lower left of nest in CD
                                nri, nrj)                                ! Nest ratio, i- and j-directions

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, i, j, k
     REAL :: nx, x0, x1, x2, x3, x
     REAL :: ny, y0, y1, y2, y3
     REAL :: cxm1, cxp0, cxp1, cxp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2
     REAL :: cym1, cyp0, cyp1, cyp2
     INTEGER :: ioff, joff

     !  Fortran functions.

     REAL, EXTERNAL :: lagrange_quad_avg
     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     !  This stag stuff is to keep us away from the outer most row
     !  and column for the unstaggered directions.  We are going to 
     !  consider "U" an xstag variable and "V" a ystag variable.  The
     !  vertical staggering is handled in the actual arguments.  The
     !  ckte and nkte are the ending vertical dimensions for computations
     !  for this particular variable.

     !  The ioff and joff are offsets due to the staggering.  It is a lot
     !  simpler with ioff and joff if 
     !  u var => ioff=1
     !  v var => joff=1
     !  otherwise zero.
     !  Note that is OPPOSITE of the istag, jstag vars.  The stag variables are
     !  used for the domain dimensions, the offset guys are used in the 
     !  determination of grid points between the CG and FG

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     !  Loop over each j-index on this tile for the nested domain.

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        !  This is the lower-left j-index of the CG.

        !  Example is 3:1 ratio, mass-point staggering.  We have listed sixteen CG values
        !  as an example: A through P.  For a 3:1 ratio, each of these CG cells has
        !  nine associated FG points.

        !  |=========|=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |         |
        !  | -  M  - | -  N  d | -  O  - | -  P  - |
        !  |         |         |         |         |
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |         |
        !  | -  I  - | -  J  c | -  K  - | -  L  - |
        !  |         |         |         |         |
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|=========|
        !  | -  1  2 | 3  4  5 | 6  7  8 | -  -  - |
        !  |         |         |         |         |
        !  | -  E  - | -  F  b | -  G  - | -  H  - |
        !  |         |         |         |         |
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |         |
        !  | -  A  - | -  B  a | -  C  - | -  D  - |
        !  |         |         |         |         |
        !  | -  -  - | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|=========|

        !  To interpolate to FG point 4, 5, or 6 we will use CG points: A through P.  It is
        !  sufficient to find the lower left corner of a 4-point interpolation, and then extend 
        !  each side by one unit.

        !  Here are the lower left hand corners of the following FG points:
        !  1 => E
        !  2 => E
        !  3 => E
        !  4 => F
        !  5 => F
        !  6 => F
        !  7 => G
        !  8 => G

        cj = compute_CGLL ( nj , jpos , nrj , jstag )

        !  Vertical dim of the nest domain.

        k_loop : DO nk = nkts, nkte

          !  Loop over each i-index on this tile for the nested domain.

           i_loop : DO ni = nits, MIN(nide-istag,nite)
 
              !  The coarse grid location that is to the lower left of the FG point.

              ci = compute_CGLL ( ni , ipos , nri , istag )

              !  To interpolate to point "*" (look in grid cell "F"):
              !  1. Use ABC to get a quadratic valid at "a"
              !     Use BCD to get a quadratic valid at "a"
              !     Average these to get the final value for "a"
              !  2. Use EFG to get a quadratic valid at "b"
              !     Use FGH to get a quadratic valid at "b"
              !     Average these to get the final value for "b"
              !  3. Use IJK to get a quadratic valid at "c"
              !     Use JKL to get a quadratic valid at "c"
              !     Average these to get the final value for "c"
              !  4. Use MNO to get a quadratic valid at "d"
              !     Use NOP to get a quadratic valid at "d"
              !     Average these to get the final value for "d"
              !  5. Use abc to get a quadratic valid at "*"
              !     Use bcd to get a quadratic valid at "*"
              !     Average these to get the final value for "*"

              !  |=========|=========|=========|=========|
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |         |         |         |         |
              !  | -  M  - | -  N  d | -  O  - | -  P  - |
              !  |         |         |         |         |
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |=========|=========|=========|=========|
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |         |         |         |         |
              !  | -  I  - | -  J  c | -  K  - | -  L  - |
              !  |         |         |         |         |
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |=========|=========|=========|=========|
              !  | -  -  - | -  -  * | -  -  - | -  -  - |
              !  |         |         |         |         |
              !  | -  E  - | -  F  b | -  G  - | -  H  - |
              !  |         |         |         |         |
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |=========|=========|=========|=========|
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |         |         |         |         |
              !  | -  A  - | -  B  a | -  C  - | -  D  - |
              !  |         |         |         |         |
              !  | -  -  - | -  -  - | -  -  - | -  -  - |
              !  |=========|=========|=========|=========|

              !  Overlapping quadratic interpolation.

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN

                 !  I-direction location of "*"

                 nx = REAL(ni)

                 !  I-direction location of "A", "E", "I", "M"

                 cxm1 = nest_loc_of_cg ( ci-1 , ipos , nri , ioff ) 

                 !  I-direction location of "B", "F", "J", "N"

                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 

                 !  I-direction location of "C", "G", "K", "O"

                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 

                 !  I-direction location of "D", "H", "L", "P"

                 cxp2 = nest_loc_of_cg ( ci+2 , ipos , nri , ioff ) 

                 !  Value at "a"

                 nfld_m1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj-1), cfld(ci+0,nk,cj-1), cfld(ci+1,nk,cj-1), cfld(ci+2,nk,cj-1) )

                 !  Value at "b"

                 nfld_p0 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+0), cfld(ci+0,nk,cj+0), cfld(ci+1,nk,cj+0), cfld(ci+2,nk,cj+0) )

                 !  Value at "c"

                 nfld_p1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+1), cfld(ci+0,nk,cj+1), cfld(ci+1,nk,cj+1), cfld(ci+2,nk,cj+1) )

                 !  Value at "d"

                 nfld_p2 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+2), cfld(ci+0,nk,cj+2), cfld(ci+1,nk,cj+2), cfld(ci+2,nk,cj+2) )

                 !  J-direction location of "*"

                 ny = REAL(nj)

                 !  J-direction location of "A", "B", "C", "D"

                 cym1 = nest_loc_of_cg ( cj-1 , jpos , nrj , joff ) 

                 !  J-direction location of "E", "F", "G", "H" 

                 cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 

                 !  J-direction location of "I", "J", "K", "L"

                 cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

                 !  J-direction location of "M", "N", "O", "P"

                 cyp2 = nest_loc_of_cg ( cj+2 , jpos , nrj , joff ) 

                 !  Value at "*"

                 nfld(ni,nk,nj) = lagrange_quad_avg ( ny, cym1, cyp0, cyp1, cyp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2 )

              END IF

           END DO i_loop
        END DO    k_loop
     END DO       j_loop

   END SUBROUTINE interp_fcn_lagr

!=================================================================================

   REAL FUNCTION lagrange_quad ( x , x0, x1, x2, y0, y1, y2 )

     IMPLICIT NONE 

     REAL :: x , x0, x1, x2, y0, y1, y2

     !  Lagrange = sum     prod    ( x  - xj )
     !             i=0,n ( j=0,n    ---------  * yi )
     !                     j<>i    ( xi - xj ) 
     
     !  For a quadratic, in the above equation, we are setting n=2. Three points
     !  required for a quadratic, points x0, x1, x2 (hence n=2).

     lagrange_quad = &
            (x-x1)*(x-x2)*y0 / ( (x0-x1)*(x0-x2) ) + &
            (x-x0)*(x-x2)*y1 / ( (x1-x0)*(x1-x2) ) + &
            (x-x0)*(x-x1)*y2 / ( (x2-x0)*(x2-x1) ) 

   END FUNCTION lagrange_quad

!=================================================================================

   REAL FUNCTION lagrange_quad_avg ( x , x0, x1, x2, x3, y0, y1, y2, y3 )

     IMPLICIT NONE

     REAL, EXTERNAL :: lagrange_quad
     REAL :: x , x0, x1, x2, x3, y0, y1, y2, y3

     !  Since there are three points required for a quadratic, we compute it twice 
     !  (once with x0, x1, x2 and once with x1, x2, x3), and then average those values.  This will 
     !  reduce overshoot.  The "x" point is where we are interpolating TO.
    
     !         x0         x1    x    x2
     !                    x1    x    x2         x3

     lagrange_quad_avg = & 
!      ( lagrange_quad ( x , x0, x1, x2,     y0, y1, y2     )               + &
!        lagrange_quad ( x ,     x1, x2, x3,     y1, y2, y3 )               ) / &
!        2.
       ( lagrange_quad ( x , x0, x1, x2,     y0, y1, y2     ) * ( x2 - x  ) + &
         lagrange_quad ( x ,     x1, x2, x3,     y1, y2, y3 ) * ( x  - x1 ) ) / &
         ( x2 - x1 )

   END FUNCTION lagrange_quad_avg

!=================================================================================

   REAL FUNCTION nest_loc_of_cg ( ci , ipos , nri , ioff )

     !  I and J direction equations for mass and momentum values for even
     !  and odd ratios: Given that the starting value of the nest in the
     !  CG grid cell is defined as (1,1), what is the location of the CG
     !  location in FG index units.  Example, for a 2:1 ratio, the location 
     !  of the mass point T is 1.5 (3:1 ratio = 2, 4:1 ratio = 2.5, etc).
     !  Note that for momentum points, the CG U point is defined as "1", the
     !  same as the I-direction of the (1,1) location of the FG U point.
     !  Same for V, but in the J-direction.

     IMPLICIT NONE 

     INTEGER :: ci , ipos , nri , ioff

     nest_loc_of_cg = & 
       ( ci - ipos ) * nri + ( 1 - ioff ) * REAL ( nri + 1 ) / 2. + ioff

   END FUNCTION nest_loc_of_cg

!=================================================================================

   FUNCTION compute_CGLL ( ni , ipos , nri , istag ) RESULT ( CGLL_loc )

      IMPLICIT NONE

      INTEGER , INTENT(IN ) :: ni , ipos , nri , istag
      INTEGER :: CGLL_loc

      !  Local vars

      INTEGER :: starting_position , increments_of_CG_cells
      INTEGER :: location_of_LL_wrt_this_CG
      INTEGER :: ioff
      INTEGER , PARAMETER :: MOMENTUM_STAG   = 0
      INTEGER , PARAMETER :: MASS_POINT_STAG = 1

      starting_position = ipos
      increments_of_CG_cells = ( ni - 1 ) / nri
      ioff = MOD ( nri , 2 )

      IF      ( istag .EQ. MOMENTUM_STAG   ) THEN
         location_of_LL_wrt_this_CG =   MOD ( ( ni - 1 ) , nri )          /   ( nri + ioff )       - istag ! zero
      ELSE IF ( istag .EQ. MASS_POINT_STAG ) THEN
         location_of_LL_wrt_this_CG = ( MOD ( ( ni - 1 ) , nri ) + ioff ) / ( ( nri + ioff ) / 2 ) - istag
      ELSE
         CALL wrf_error_fatal ( 'Hold on there pard, there are only two staggerings I accept.' )
      END IF

      CGLL_loc = starting_position + increments_of_CG_cells + location_of_LL_wrt_this_CG
!      WRITE ( 6 , '(a,i4, i4, i4, i4)') 'ni   ipos nri  stag', ni, ipos, nri, istag
!      WRITE ( 6 , '(a,i4, i4, i4, i4)') 'strt inc  loc  CGLL', starting_position , increments_of_CG_cells , location_of_LL_wrt_this_CG , CGLL_loc
!      print *,' '

   END FUNCTION compute_CGLL

!=================================================================================

! Smolarkiewicz positive definite, monotonic transport.

   SUBROUTINE interp_fcn_sint ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, nioff, njoff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER nrio2, nrjo2

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

     ioff  = 0 ; joff  = 0
     nioff = 0 ; njoff = 0
     IF ( xstag ) THEN 
       ioff = (nri-1)/2
       nioff = nri 
     ENDIF
     IF ( ystag ) THEN
       joff = (nrj-1)/2
       njoff = nrj
     ENDIF

     nrio2 = nri/2
     nrjo2 = nrj/2

     nfx = nri * nrj
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ci,cj,ip,jp,nk,ck,nf,icmask,psca )
     DO k = ckts, ckte
        icmask = .FALSE.
        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrjo2 + 1 )  ! j point on nest
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nrio2 + 1 )    ! i point on nest
                if ( ni .ge. nits-nioff-nrio2 .and. &
                     ni .le. nite+nioff+nrio2 .and. &
                     nj .ge. njts-njoff-nrjo2 .and. &
                     nj .le. njte+njoff+nrjo2 ) then
                  if ( ni.ge.nims.and.ni.le.nime.and.nj.ge.njms.and.nj.le.njme) then
                    if ( imask(ni,nj) .eq. 1 ) then
                      icmask( i, j ) = .TRUE.
                    endif
                  endif
                  if ( ni-nioff.ge.nims.and.ni.le.nime.and.nj-njoff.ge.njms.and.nj.le.njme) then
                    if (ni .ge. nits-nioff .and. nj .ge. njts-njoff ) then
                      if ( imask(ni-nioff,nj-njoff) .eq. 1) then
                        icmask( i, j ) = .TRUE.
                      endif
                    endif
                  endif
                endif
                psca(i,j,nf) = cfld(i,k,j)
              ENDDO           ! i
           ENDDO              ! j
        ENDDO                 ! nf

! tile dims in this call to sint are 1-over to account for the fact
! that the number of cells on the nest local subdomain is not 
! necessarily a multiple of the nest ratio in a given dim.
! this could be a little less ham-handed.

        CALL sint( psca,                     &
                   cims, cime, cjms, cjme, icmask,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )

        DO nj = njts, njte+joff
           cj = jpos + (nj-1) / nrj ! j coord of CD point 
           jp = mod ( nj-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni = nits, nite+ioff
               ci = ipos + (ni-1) / nri      ! i coord of CD point 
               ip = mod ( ni-1 , nri )  ! coord of ND w/i CD point
               if ( ( ni-ioff .ge. nits ) .and. ( nj-joff .ge. njts ) ) then
                  if ( imask ( ni, nj ) .eq. 1 .or. imask ( ni-ioff, nj-joff ) .eq. 1  ) then
                    nfld( ni-ioff, nk, nj-joff ) = psca( ci , cj, ip+1 + (jp)*nri )
                  endif
               endif
           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE interp_fcn_sint

!=========================================================================

!  Nearest neighbor interpolation.

   SUBROUTINE interp_fcn_nn ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     INTEGER ci, cj, ck, ni, nj, nk

     ! Iterate over the ND tile and assign the values
     ! from the CD tile.  This is a trivial implementation 
     ! of the interp_fcn; just copies the values from the CD into the ND

     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              if ( imask ( ni, nj ) .eq. 1 ) then
                ci = ipos + (ni-1) / nri      ! i coord of CD point 
                nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO

   END SUBROUTINE interp_fcn_nn

!=========================================================================

   SUBROUTINE interp_fcn_bl ( cfld,                                 &  ! CD field
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  ! ND field
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  ! stencil half width for interp
                              imask,                                &  ! interpolation mask
                              xstag, ystag,                         &  ! staggering of field
                              ipos, jpos,                           &  ! Position of lower left of nest in CD
                              nri, nrj,                             &  ! Nest ratio, i- and j-directions
                              cht, nht,                             &  ! topography for CG and FG
                              ct_max_p,nt_max_p,                    &  ! temperature (K) at max press, want CG value
                              cght_max_p,nght_max_p,                &  ! height (m) at max press, want CG value
                              cmax_p,nmax_p,                        &  ! max pressure (Pa) in column, want CG value
                              ct_min_p,nt_min_p,                    &  ! temperature (K) at min press, want CG value
                              cght_min_p,nght_min_p,                &  ! height (m) at min press, want CG value
                              cmin_p,nmin_p,                        &  ! min pressure (Pa) in column, want CG value
                              zn, p_top                             )  ! eta levels
     USE module_timing
!    USE module_configure
     USE module_model_constants , ONLY : g , r_d, cp, p1000mb, t0

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cht, ct_max_p, cght_max_p, cmax_p, ct_min_p, cght_min_p, cmin_p
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nht, nt_max_p, nght_max_p, nmax_p, nt_min_p, nght_min_p, nmin_p
     REAL, DIMENSION ( ckms:ckme ) :: zn
     REAL :: p_top
     REAL, EXTERNAL :: v_interp_col

     ! Local

     INTEGER ci, cj, ni, nj, nk, istag, jstag, i, j, k
     REAL :: wx, wy, nprs, cfld_ll, cfld_lr, cfld_ul, cfld_ur
     REAL , DIMENSION(ckms:ckme) :: cprs
     REAL    :: p00 , t00 , a , tiso , p_surf

     !  Yes, memory sized to allow "outside the tile" indexing for horiz interpolation.  This
     !  is really an intermediate domain that has quite a bit of usable real estate surrounding
     !  the tile dimensions.

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cpb
  
     !  A bit larger than tile sized to allow horizontal interpolation on the CG.

     REAL, DIMENSION ( cits-2:cite+2, cjts-2:cjte+2 ) :: cfld_max_p, cfld_min_p

     !  The usual tile size for the FG local array.

     REAL, DIMENSION ( nits:nite, nkts:nkte, njts:njte ) :: npb

     !  Get base state constants

     CALL nl_get_base_pres  ( 1 , p00 )
     CALL nl_get_base_temp  ( 1 , t00 )
     CALL nl_get_base_lapse ( 1 , a   )
     CALL nl_get_iso_temp   ( 1 , tiso )

     !  This stag stuff is to keep us away from the outer most row
     !  and column for the unstaggered directions.  We are going to 
     !  consider "U" an xstag variable and "V" a ystag variable.  The
     !  vertical staggering is handled in the actual arguments.  The
     !  ckte and nkte are the ending vertical dimensions for computations
     !  for this particular variable.

     IF ( xstag ) THEN
        istag = 0 
     ELSE
        istag = 1
     END IF

     IF ( ystag ) THEN
        jstag = 0 
     ELSE
        jstag = 1
     END IF

     !  Compute the reference pressure for the CG, function only of constants and elevation.
     !  We extend the i,j range to allow us to do horizontal interpolation.  We only need
     !  one extra grid cell surrounding the nest, and the intermediate domain has plenty of
     !  room with the halos set up for higher-order interpolations.  For intermediate domains,
     !  it turns out that the "domain" size actually fits within the "tile" size.  Yeppers,
     !  that is backwards from what usually happens.  That intermediate domain size is a couple
     !  grid points larger than necessary, and the tile is a couple of grid cells larger still.
     !  For our low-order interpolation, we can use the tile size for the CG, and we will have
     !  plenty of data on our boundaries.

     DO j = cjts-2 , cjte+2
        DO i = cits-2 , cite+2
           p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*cht(i,j)/a/r_d ) **0.5 )
           DO k = ckts , ckte
              cpb(i,k,j) = zn(k)*(p_surf - p_top) + p_top  
           END DO
           IF ( ckte .EQ. ckme ) THEN
              cfld_max_p(i,j) = cght_max_p(i,j) * g
              cfld_min_p(i,j) = cght_min_p(i,j) * g
           ELSE
              cfld_max_p(i,j) = ct_max_p(i,j) * (p1000mb/cmax_p(i,j))**(r_d/cp) - t0
              cfld_min_p(i,j) = ct_min_p(i,j) * (p1000mb/cmin_p(i,j))**(r_d/cp) - t0
           END IF
        END DO
     END DO

     !  Compute the reference pressure for the FG. This is actually the size of the entire
     !  domain, not some chopped down piece of intermediate domain, as in the parent
     !  grid.  We do the traditional MAX(dom end -1,tile end), since we know a priori that the
     !  pressure is a mass point field (because the topo elevation is a mass point field).

     DO j = njts , MIN(njde-1,njte)
        DO i = nits , MIN(nide-1,nite)
           p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*nht(i,j)/a/r_d ) **0.5 )
           DO k = nkts , nkte
              npb(i,k,j) = zn(k)*(p_surf - p_top) + p_top  
           END DO
        END DO
     END DO

     !  Loop over each j-index on this tile for the nested domain.

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        !  This is the lower-left j-index of the CG.

        !  Example is 3:1 ratio, mass-point staggering.  We have listed six CG values
        !  as an example: A, B, C, D, E, F.  For a 3:1 ratio, each of these CG cells has
        !  nine associated FG points.
        !  |=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |
        !  | -  D  - | -  E  - | -  F  - |
        !  |         |         |         |
        !  | 1  2  3 | 4  5  6 | 7  8  9 |
        !  |=========|=========|=========|
        !  | -  -  - | -  -  - | -  -  - |
        !  |         |         |         |
        !  | -  A  - | -  B  - | -  C  - |
        !  |         |         |         |
        !  | -  -  - | -  -  - | -  -  - |
        !  |=========|=========|=========|
        !  To interpolate to FG point 4, we will use CG points: A, B, D, E.  It is adequate to
        !  find the lower left point.  The lower left (LL) point for "4" is "A".  Below
        !  are a few more points.
        !  2 => A
        !  3 => A
        !  4 => A
        !  5 => B
        !  6 => B
        !  7 => B
        !  We want an equation that returns the CG LL:
        !  CG LL =   ipos                          (the starting point of the nest in the CG) 
        !          + (ni-1)/nri                    (gives us the CG cell, based on the nri-groups of FG cells
        !          - istag                         (a correction term, this is either zero for u in the x-dir, 
        !                                           since we are doing an "i" example, or 1 for anything else)
        !          + (MOD(ni-1,nri)+1 + nri/2)/nri (gives us specifically related CG point for each of the nri
        !                                           FG points, for example, we want points "1", "4", and "7" all
        !                                           to point to the CG at the left for the LL point)
        !  For grid points 4, 5, 6, we want the CG LL (sans the first two terms) to be -1, 0, 0 (which
        !  means that the CG point for "4" is to the left, and the CG LL point for "5" and "6"
        !  is in the current CG index.  

        cj = jpos + (nj-1)/nrj - jstag + (MOD(nj-1,nrj)+1 + nrj/2)/nrj


        !  What is the weighting for this CG point to the FG point, j-weight only.

        IF ( ystag ) THEN
           wy = 1. - ( REAL(MOD(nj+(nrj-1)/2,nrj)) / REAL(nrj) +  1. / REAL (2 * nrj) )
        ELSE
           wy = 1. - ( REAL(MOD(nj+(nrj-1)/2,nrj)) / REAL(nrj)                        )
        END IF

        !  Vertical dim of the nest domain.

        k_loop : DO nk = nkts, nkte

          !  Loop over each i-index on this tile for the nested domain.

           i_loop : DO ni = nits, MIN(nide-istag,nite)
 
              !  The coarse grid location that is to the lower left of the FG point.

              ci = ipos + (ni-1)/nri - istag + (MOD(ni-1,nri)+1 + nri/2)/nri

              !  Weights in the x-direction.

              IF ( xstag ) THEN
                 wx = 1. - ( REAL(MOD(ni+(nri-1)/2,nri)) / REAL(nri) +  1. / REAL (2 * nri) )
              ELSE
                 wx = 1. - ( REAL(MOD(ni+(nri-1)/2,nri)) / REAL(nri)                        )
              END IF

              !  The pressure of the FG point.

              IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
                 nprs =   npb(  ni  , nk , nj  ) 
              ELSE IF ( xstag ) THEN
                 nprs = ( npb(  ni-1, nk , nj  ) + npb(  ni  , nk , nj  ) ) * 0.5
              ELSE IF ( ystag ) THEN
                 nprs = ( npb(  ni  , nk , nj-1) + npb(  ni  , nk , nj  ) ) * 0.5
              END IF

              !  The four surrounding CG values.

              IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
                 cprs(:) =   cpb(ci  ,:,cj  )
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) =   cpb(ci+1,:,cj  )
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) =   cpb(ci  ,:,cj+1)
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) =   cpb(ci+1,:,cj+1)
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )

              ELSE IF ( xstag ) THEN
                 cprs(:) = ( cpb(ci  ,:,cj  ) + cpb(ci-1,:,cj  ) )*0.5 
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) = ( cpb(ci+1,:,cj  ) + cpb(ci  ,:,cj  ) )*0.5
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) = ( cpb(ci  ,:,cj+1) + cpb(ci-1,:,cj+1) )*0.5
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) = ( cpb(ci+1,:,cj+1) + cpb(ci  ,:,cj+1) )*0.5
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )
              ELSE IF ( ystag ) THEN
                 cprs(:) = ( cpb(ci  ,:,cj  ) + cpb(ci  ,:,cj-1) )*0.5
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) = ( cpb(ci+1,:,cj  ) + cpb(ci+1,:,cj-1) )*0.5
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) = ( cpb(ci  ,:,cj+1) + cpb(ci  ,:,cj  ) )*0.5
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) = ( cpb(ci+1,:,cj+1) + cpb(ci+1,:,cj  ) )*0.5
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )
              END IF

              !  Bilinear interpolation in horizontal with vertically corrected CG field values.

              nfld( ni , nk , nj ) =     wy  * ( cfld_ll * wx + cfld_lr * (1.-wx) ) + &
                                     (1.-wy) * ( cfld_ul * wx + cfld_ur * (1.-wx) )

           END DO i_loop
        END DO    k_loop
     END DO       j_loop

     !  If this is ph_2, make the values at k=1 all zero

     IF ( ckme .EQ. ckte ) THEN
        DO nj = njts,njte
           DO ni = nits, nite
              nfld(ni,nkts,nj) = 0.0
           END DO
        END DO
     END IF

   END SUBROUTINE interp_fcn_bl

!==================================

   FUNCTION v_interp_col ( cfld_orig , cprs_orig , ckms , ckme , ckte , nprs, ni, nj, nk, ci, cj, &
                           cfld_max_p , cmax_p , cfld_min_p , cmin_p ) RESULT ( cfld_interp )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ni, nj, nk, ci, cj
      INTEGER , INTENT(IN) :: ckms , ckme , ckte
      REAL , DIMENSION(ckms:ckme) , INTENT(IN) :: cfld_orig , cprs_orig
      REAL , INTENT(IN) :: cfld_max_p , cmax_p , cfld_min_p , cmin_p
      REAL , INTENT(IN) :: nprs
      REAL :: cfld_interp

      !  Local

      INTEGER :: ck
      LOGICAL :: found
      CHARACTER(LEN=256) :: joe_mess
      REAL , DIMENSION(ckms:ckme+1+1) :: cfld , cprs

      !  Fill input arrays

      cfld(1) = cfld_max_p
      cprs(1) = cmax_p

      cfld(ckte+2) = cfld_min_p
      cprs(ckte+2) = cmin_p

      DO ck = ckms , ckte
         cfld(ck+1) = cfld_orig(ck)
         cprs(ck+1) = cprs_orig(ck)
      END DO

      found = .FALSE.

      IF      ( cprs(ckms) .LT. nprs ) THEN
         cfld_interp = cfld(ckms)
         RETURN
      ELSE IF ( cprs(ckte+2) .GE. nprs ) THEN
         cfld_interp = cfld(ckte+2)
         RETURN
      END IF

      DO ck = ckms , ckte+1
         IF ( ( cprs(ck  ) .GE. nprs ) .AND. &
              ( cprs(ck+1) .LT. nprs ) ) THEN
            cfld_interp = ( cfld(ck  ) * ( nprs     - cprs(ck+1) ) + &
                            cfld(ck+1) * ( cprs(ck) - nprs       ) ) / &
                                         ( cprs(ck) - cprs(ck+1) )
            RETURN
         END IF
      END DO

print *,'Hey we should not be here'
print *,'nest pres to find = ',nprs
print *,'column of cg pres = ',cprs
      CALL wrf_error_fatal ( 'ERROR -- vertical interpolation for nest interp cannot find trapping pressures' )
   
   END FUNCTION v_interp_col

!==================================
! this is the default function used in feedback.

   SUBROUTINE copy_fcn ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ),INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)  :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     !  Loop over the coarse grid in the area of the fine mesh.  Do not
     !  process the coarse grid values that are along the lateral BC
     !  provided to the fine grid.  Since that is in the specified zone
     !  for the fine grid, it should not be used in any feedback to the
     !  coarse grid as it should not have changed.

     !  Due to peculiarities of staggering, it is simpler to handle the feedback
     !  for the staggerings based upon whether it is a even ratio (2::1, 4::1, etc.) or
     !  an odd staggering ratio (3::1, 5::1, etc.). 

     !  Though there are separate grid ratios for the i and j directions, this code
     !  is not general enough to handle aspect ratios .NE. 1 for the fine grid cell.
 
     !  These are local integer increments in the looping.  Basically, istag=1 means
     !  that we will assume one less point in the i direction.  Note that ci and cj
     !  have a maximum value that is decreased by istag and jstag, respectively.  

     !  Horizontal momentum feedback is along the face, not within the cell.  For a
     !  3::1 ratio, temperature would use 9 pts for feedback, while u and v use
     !  only 3 points for feedback from the nest to the parent.

     CALL nl_get_spec_zone( 1 , spec_zone )
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                   cfld( ci, ck, cj ) =  1./9. * &
!                                         ( nfld( ni-1, nk , nj-1) + &
!                                           nfld( ni  , nk , nj-1) + &
!                                           nfld( ni+1, nk , nj-1) + &
!                                           nfld( ni-1, nk , nj  ) + &
!                                           nfld( ni  , nk , nj  ) + &
!                                           nfld( ni+1, nk , nj  ) + &
!                                           nfld( ni-1, nk , nj+1) + &
!                                           nfld( ni  , nk , nj+1) + &
!                                           nfld( ni+1, nk , nj+1) )
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                   cfld( ci, ck, cj ) =  1./3. * &
!                                         ( nfld( ni  , nk , nj-1) + &
!                                           nfld( ni  , nk , nj  ) + &
!                                           nfld( ni  , nk , nj+1) )
                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                   cfld( ci, ck, cj ) =  1./3. * &
!                                         ( nfld( ni-1, nk , nj  ) + &
!                                           nfld( ni  , nk , nj  ) + &
!                                           nfld( ni+1, nk , nj  ) )
                 ENDDO
              ENDDO
           ENDDO

        END IF

     !  Even refinement ratio

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        !  This is a simple schematic of the feedback indexing used in the even
        !  ratio nests.  For simplicity, a 2::1 ratio is depicted.  Only the 
        !  mass variable staggering is shown. 
        !                                                                  Each of
        !  the boxes with a "T" and four small "t" represents a coarse grid (CG)
        !  cell, that is composed of four (2::1 ratio) fine grid (FG) cells.
   
        !  Shown below is the area of the CG that is in the area of the FG.   The
        !  first grid point of the depicted CG is the starting location of the nest
        !  in the parent domain (ipos,jpos - i_parent_start and j_parent_start from
        !  the namelist).  
   
        !  For each of the CG points, the feedback loop is over each of the FG points
        !  within the CG cell.  For a 2::1 ratio, there are four total points (this is 
        !  the ijpoints loop).  The feedback value to the CG is the arithmetic mean of 
        !  all of the FG values within each CG cell.

!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
! jpos+        |             ||             |                          |             ||             |
! (njde-njds)- |      T      ||      T      |                          |      T      ||      T      |
! jstag        |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
!              |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |-------------||-------------|                          |-------------||-------------|
!
!                   ...
!                   ...
!                   ...
!                   ...
!                   ...

!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!              |      T      ||      T      |                          |      T      ||      T      |
!              |             ||             |                          |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!  nj=3        |-------------||-------------|                          |-------------||-------------|
!              |-------------||-------------|                          |-------------||-------------|
! jpoints = 1  |  t      t   ||  t      t   |                          |  t      t   ||  t      t   |
!              |             ||             |                          |             ||             |
!    jpos      |      T      ||      T      |          ...             |      T      ||      T      |
!              |             ||             |          ...             |             ||             |
! jpoints = 0, |  t      t   ||  t      t   |          ...             |  t      t   ||  t      t   |
!  nj=1        |-------------||-------------|                          |-------------||-------------|
!                     ^                                                                      ^
!                     |                                                                      |
!                     |                                                                      |
!                   ipos                                                                   ipos+
!     ni =        1              3                                                         (nide-nids)/nri
! ipoints=        0      1       0      1                                                  -istag
!

           !  For performance benefits, users can comment out the inner most loop (and cfld=0) and
           !  hardcode the loop feedback.  For example, it is set up to run a 2::1 ratio
           !  if uncommented.  This lacks generality, but is likely to gain timing benefits
           !  with compilers unable to unroll inner loops that do not have parameterized sizes.
   
           !  The extra +1 ---------/ and the extra -1 ----\  (both for ci and cj) 
           !                       /                        \   keeps the feedback out of the 
           !                      /                          \  outer row/col, since that CG data
           !                     /                            \ specified the nest boundary originally
           !                    /                              \   This
           !                   /                                \    is just
           !                  /                                  \   a sentence to not end a line
           !                 /                                    \   with a stupid backslash
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                   cfld( ci, ck, cj ) =  1./4. * &
!                                         ( nfld( ni  , nk , nj  ) + &
!                                           nfld( ni+1, nk , nj  ) + &
!                                           nfld( ni  , nk , nj+1) + &
!                                           nfld( ni+1, nk , nj+1) )
                 END DO
              END DO
           END DO

        !  U

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!              U               |
!              |               |
! jpoints = 0, u       u       |
!  nj=3        |               |
!              |---------------|
!              |---------------|
!              |               |
! jpoints = 1  u       u       |
!              |               |
!    jpos      U               |
!              |               |
! jpoints = 0, u       u       |
! nj=1         |               |
!              |---------------|
! 
!              ^               
!              |              
!              |             
!            ipos           
!     ni =     1               3
! ipoints=     0       1       0 
!

           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                cfld( ci, ck, cj ) =  1./2. * &
!                                      ( nfld( ni  , nk , nj  ) + &
!                                        nfld( ni  , nk , nj+1) )
                 ENDDO
              ENDDO
           ENDDO

        !  V 

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO
!                cfld( ci, ck, cj ) =  1./2. * &
!                                      ( nfld( ni  , nk , nj  ) + &
!                                        nfld( ni+1, nk , nj  ) )
                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn

!==================================
! this is the 1pt function used in feedback.

   SUBROUTINE copy_fcnm (  cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm

!==================================
! this is the 1pt function used in feedback for integers

   SUBROUTINE copy_fcni ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN)  :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcni

!==================================

   SUBROUTINE vert_interp_vert_nesting ( cfld,                                 &  ! CD field
                                         ids, ide, kds, kde, jds, jde,         & 
                                         ims, ime, kms, kme, jms, jme,         &
                                         its, ite, kts, kte, jts, jte,         &   
                                         pgrid_s_vert, pgrid_e_vert,           &  ! vertical dimensions of parent grid
					 cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c,   &  ! coarse grid extrapolation constants
                                         alt_u_c, alt_u_n)

!KAL vertical interpolation for u, v, and mass points (w is below in a different subroutine) for vertical nesting 

   IMPLICIT NONE
   REAL, DIMENSION ( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                ! vertical dimensions of the parent grid			  
   REAL, INTENT(IN)    :: cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1), INTENT(IN) :: alt_u_c
   REAL, DIMENSION(kde+1), INTENT(IN) :: alt_u_n

   
   !local
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1) :: pro_u_c           ! variable in 1D on the coarse grid
   REAL, DIMENSION(kde+1) :: pro_u_n
   
   DO j = jms,jme
   DO i = ims,ime
   
      ! pro_u_c is u on the 1D coarse grid 
    
      do k = pgrid_s_vert,pgrid_e_vert-1
      pro_u_c(k+1) = cfld(i,k,j)
      enddo 
      
      !KAL fill in the surface value and the top value using extrapolation

      pro_u_c(1      ) = cf1_c*cfld(i,1,j) &
                       + cf2_c*cfld(i,2,j) &
                       + cf3_c*cfld(i,3,j)
   
      pro_u_c(pgrid_e_vert+1) = cfn_c *cfld(i,pgrid_e_vert-1,j) &
                              + cfn1_c*cfld(i,pgrid_e_vert-2,j)  
		       
      call inter_wrf_copy(pro_u_c, alt_u_c, pgrid_e_vert+1, pro_u_n, alt_u_n, kde+1)
      
      do k = 1,kde-1
         cfld(i,k,j) = pro_u_n(k+1)
      enddo	
            
   ENDDO
   ENDDO
  
   
   END SUBROUTINE vert_interp_vert_nesting
   
 !==================================

   SUBROUTINE vert_interp_vert_nesting_w ( cfld,                                 &  ! CD field
                                           ids, ide, kds, kde, jds, jde,         & 
                                           ims, ime, kms, kme, jms, jme,         &
                                           its, ite, kts, kte, jts, jte,         &   
                                           pgrid_s_vert, pgrid_e_vert,           &  ! vertical dimensions of parent grid
                                           alt_w_c, alt_w_n)

!KAL vertical interpolation at w points for vertical nesting

   IMPLICIT NONE
   REAL, DIMENSION ( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                ! vertical dimensions of the parent grid			  
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert), INTENT(IN) :: alt_w_c
   REAL, DIMENSION(kde), INTENT(IN) :: alt_w_n

   
   !local
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert) :: pro_w_c           ! variable in 1D on the coarse grid
   REAL, DIMENSION(kde) :: pro_w_n
   
   DO j = jms,jme
   DO i = ims,ime
   
      ! pro_w_c is w on the 1D coarse grid 
    
      do k = pgrid_s_vert,pgrid_e_vert
      pro_w_c(k) = cfld(i,k,j)
      enddo 
	       
      call inter_wrf_copy(pro_w_c, alt_w_c, pgrid_e_vert, pro_w_n, alt_w_n, kde)
      
      do k = 1,kde
         cfld(i,k,j) = pro_w_n(k)
      enddo	
            
   ENDDO
   ENDDO
  
   
   END SUBROUTINE vert_interp_vert_nesting_w

!-----------------------------------------------------------------------------------------   

   SUBROUTINE vert_interp_vert_nesting_1d ( cfld,                                 &  ! CD field
                                            ids, ide, kds, kde, jds, jde,         & 
                                            ims, ime, kms, kme, jms, jme,         &
                                            its, ite, kts, kte, jts, jte,         &   
                                            pgrid_s_vert, pgrid_e_vert,           &  ! vertical dimensions of parent grid
					    cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c,   &  ! coarse grid extrapolation constants
                                            alt_u_c, alt_u_n)

!KAL vertical interpolation for u, v, and mass points (w is below in a different subroutine) for vertical nesting 

   IMPLICIT NONE
   REAL, DIMENSION (kms:kme),INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                ! vertical dimensions of the parent grid			  
   REAL, INTENT(IN)    :: cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1), INTENT(IN) :: alt_u_c
   REAL, DIMENSION(kde+1), INTENT(IN) :: alt_u_n

   
   !local
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1) :: pro_u_c           ! variable in 1D on the coarse grid
   REAL, DIMENSION(kde+1) :: pro_u_n
   

      ! pro_u_c is u on the 1D coarse grid 
    
      do k = pgrid_s_vert,pgrid_e_vert-1
      pro_u_c(k+1) = cfld(k)
      enddo 
      
      !KAL fill in the surface value and the top value using extrapolation

      pro_u_c(1      ) = cf1_c*cfld(1) &
                       + cf2_c*cfld(2) &
                       + cf3_c*cfld(3)
   
      pro_u_c(pgrid_e_vert+1) = cfn_c *cfld(pgrid_e_vert-1) &
                              + cfn1_c*cfld(pgrid_e_vert-2)  
		       
      call inter_wrf_copy(pro_u_c, alt_u_c, pgrid_e_vert+1, pro_u_n, alt_u_n, kde+1)
      
      do k = 1,kde-1
         cfld(k) = pro_u_n(k+1)
      enddo	
            
   
   END SUBROUTINE vert_interp_vert_nesting_1d   
   
     
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!KAL this is a direct copy of a subroutine from ndown, but a dependency on ndown will not work because it is not always compiled (for ideal cases), and most likely not compliled in the order needed.

  SUBROUTINE inter_wrf_copy(pro_c,alt_c,kde_c,pro_n,alt_n,kde_n)
  
!KAL this routine has been added for vertical nesting  

   IMPLICIT NONE
   INTEGER , INTENT(IN) :: kde_c,kde_n
   REAL , DIMENSION(kde_c) , INTENT(IN ) :: pro_c,alt_c
   REAL , DIMENSION(kde_n) , INTENT(IN ) :: alt_n
   REAL , DIMENSION(kde_n) , INTENT(OUT) :: pro_n

      real ,dimension(kde_c) :: a,b,c,d
      real :: p
      integer :: i,j


      call coeff_mon_wrf_copy(alt_c,pro_c,a,b,c,d,kde_c)

       do i = 1,kde_n-1

          do j=1,kde_c-1

               if ( (alt_n(i) .ge. alt_c(j)).and.(alt_n(i) .lt. alt_c(j+1)) ) then
                p =  alt_n(i)-alt_c(j)
                pro_n(i) = p*( p*(a(j)*p+b(j))+c(j)) + d(j)
                goto 20
               endif
           enddo
20             continue
       enddo

       pro_n(kde_n) = pro_c(kde_c)


   END SUBROUTINE inter_wrf_copy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
!KAL this is a direct copy of a subroutine from ndown, but a dependency on ndown will not work because it is not always compiled (for ideal cases), and most likely not compliled in the order needed.

     subroutine  coeff_mon_wrf_copy(x,y,a,b,c,d,n)
     
!KAL this routine has been added for vertical nesting     

      implicit none

      integer :: n
      real ,dimension(n) :: x,y,a,b,c,d
      real ,dimension(n) :: h,s,p,yp

       integer :: i


      do i=1,n-1
      h(i) = (x(i+1)-x(i))
      s(i) = (y(i+1)-y(i)) / h(i)
      enddo

      do i=2,n-1
      p(i) = (s(i-1)*h(i)+s(i)*h(i-1)) / (h(i-1)+h(i))
      enddo

      p(1) = s(1)
      p(n) = s(n-1)

      do i=1,n
      yp(i) = p(i)
      enddo
!!!!!!!!!!!!!!!!!!!!!

      do i=2,n-1
      yp(i) = (sign(1.,s(i-1))+sign(1.,s(i)))* min( abs(s(i-1)),abs(s(i)),0.5*abs(p(i)))
      enddo

      do i = 1,n-1
      a(i) = (yp(i)+yp(i+1)-2.*s(i))/(h(i)*h(i))
      b(i) = (3.*s(i)-2.*yp(i)-yp(i+1))/h(i)
      c(i) = yp(i)
      d(i) = y(i)
      enddo

      end  subroutine  coeff_mon_wrf_copy
      
  !-----------------------------------------------------------------------------------------
  

!================================== 

   SUBROUTINE p2c ( cfld,                                 &  ! CD field
                    cids, cide, ckds, ckde, cjds, cjde,   &
                    cims, cime, ckms, ckme, cjms, cjme,   &
                    cits, cite, ckts, ckte, cjts, cjte,   &
                    nfld,                                 &  ! ND field
                    nids, nide, nkds, nkde, njds, njde,   &
                    nims, nime, nkms, nkme, njms, njme,   &
                    nits, nite, nkts, nkte, njts, njte,   &
                    shw,                                  &  ! stencil half width
                    imask,                                &  ! interpolation mask
                    xstag, ystag,                         &  ! staggering of field
                    ipos, jpos,                           &  ! Position of lower left of nest in CD
                    nri, nrj                              &  ! nest ratios
                    )   
     USE module_configure
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     CALL  interp_fcn  (cfld,                             &  ! CD field
                        cids, cide, ckds, ckde, cjds, cjde,   &
                        cims, cime, ckms, ckme, cjms, cjme,   &
                        cits, cite, ckts, ckte, cjts, cjte,   &
                        nfld,                             &  ! ND field
                        nids, nide, nkds, nkde, njds, njde,   &
                        nims, nime, nkms, nkme, njms, njme,   &
                        nits, nite, nkts, nkte, njts, njte,   &
                        shw,                                  &  ! stencil half width for interp
                        imask,                                &  ! interpolation mask
                        xstag, ystag,                         &  ! staggering of field
                        ipos, jpos,                           &  ! Position of lower left of nest in CD
                        nri, nrj                             )   ! nest ratios

   END SUBROUTINE p2c

!==================================

   SUBROUTINE c2f_interp ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &  
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
!                          cbdy_xs, nbdy_xs,                           &
!                          cbdy_xe, nbdy_xe,                           &
!                          cbdy_ys, nbdy_ys,                           &
!                          cbdy_ye, nbdy_ye,                           &
!                          cbdy_txs, nbdy_txs,                       &
!                          cbdy_txe, nbdy_txe,                       &
!                          cbdy_tys, nbdy_tys,                       &
!                          cbdy_tye, nbdy_tye,                       &
                           parent_id,nest_id                        &!cyl    
                           )   ! boundary arrays
     USE module_configure
     IMPLICIT NONE

!------------------------------------------------------------
! Subroutine c2f_interp interpolate field from coarse resolution domain
! to its nested domain. It is written by Dave Gill in NCAR for the purpose 
! running phys/module_sf_oml.F-DPWP in only d01 and d02 
!                                       Chiaying Lee RSMAS/UM
!------------------------------------------------------------
                            
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj,parent_id,nest_id            !cyl

     LOGICAL, INTENT(IN) :: xstag, ystag
     
     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
!    REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs, nbdy_xs, nbdy_txs
!    REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe, nbdy_xe, nbdy_txe
!    REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys, nbdy_ys, nbdy_tys
!    REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye, nbdy_ye, nbdy_tye
     REAL cdt, ndt
     
     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,'("mask cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte, cjts,cjte
!write(0,'("mask nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte, njts,njte
!     write(0,*)'cyl parentid',parent_id
!     write(0,*)'cyl nestid',nest_id
!     If ( nest_id .le. 2 .and. (1.0/rdx .ge. 3000.0 .and. 1.0/rdy .ge. 3000.0)  ) then ! cyl: only run it in the nest domain with dx, dy < 3 km
     If ( nest_id .eq. 3 ) then 
     DO nj = njts, njte
     
        cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO
     ENDIF  ! cyl
     RETURN

   END SUBROUTINE c2f_interp

!==================================

   SUBROUTINE bdy_interp ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           cbdy_xs, nbdy_xs,                     &  ! Boundary components, for the CG and FG,
                           cbdy_xe, nbdy_xe,                     &  ! for each of the four sides (x start, e end, y start, y end)
                           cbdy_ys, nbdy_ys,                     &  ! and also for the two components of the LBC:
                           cbdy_ye, nbdy_ye,                     &  ! the "starting" value and the tendency
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  ! The CG is at a parent time step ahead of the FG
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              )  ! Time step size for CG and FG

     USE module_interp_info

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs, nbdy_xs, nbdy_txs
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe, nbdy_xe, nbdy_txe
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys, nbdy_ys, nbdy_tys
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye, nbdy_ye, nbdy_tye
     REAL cdt, ndt

     ! Local

     INTEGER nijds, nijde, spec_bdy_width

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

     IF      ( interp_method_type .EQ. NOT_DEFINED_YET  ) THEN
        interp_method_type = SINT
     END IF

     IF      ( interp_method_type .EQ. SINT      ) THEN
         CALL bdy_interp1( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde ,                        &  ! start and end of nest LBC size in the LONG direction
                           spec_bdy_width ,                      &  ! width of the LBC, the SHORT direction
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                     &  ! Boundary components, for the CG and FG,
                           cbdy_xe, nbdy_xe,                     &  ! for each of the four sides (x start, e end, y start, y end)
                           cbdy_ys, nbdy_ys,                     &  ! and also for the two components of the LBC:
                           cbdy_ye, nbdy_ye,                     &  ! the "starting" value and the tendency
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  ! The CG is at a parent time step ahead of the FG
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              &  ! Time step size for CG and FG
                                                                 )

     ELSE IF      ( ( interp_method_type .EQ. BILINEAR         ) .OR. &
                    ( interp_method_type .EQ. NEAREST_NEIGHBOR ) .OR. &
                    ( interp_method_type .EQ. QUADRATIC        ) .OR. &
                    ( interp_method_type .EQ. SPLINE           ) .OR. &
                    ( interp_method_type .EQ. SINT_NEW         ) ) THEN 
         CALL bdy_interp2( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde ,                        &  ! start and end of nest LBC size in the LONG direction
                           spec_bdy_width ,                      &  ! width of the LBC, the SHORT direction
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                     &  ! Boundary components, for the CG and FG,
                           cbdy_xe, nbdy_xe,                     &  ! for each of the four sides (x start, e end, y start, y end)
                           cbdy_ys, nbdy_ys,                     &  ! and also for the two components of the LBC:
                           cbdy_ye, nbdy_ye,                     &  ! the "starting" value and the tendency
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  ! The CG is at a parent time step ahead of the FG
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              &  ! Time step size for CG and FG
                                                                 )

     ELSE
        CALL wrf_error_fatal ('Hold on there cowboy #2, we need to know which nested lateral boundary interpolation option you want')
     END IF

   END SUBROUTINE bdy_interp

!==================================

   SUBROUTINE bdy_interp1( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde, spec_bdy_width ,          &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cbdy_xs, bdy_xs,                           &
                           cbdy_xe, bdy_xe,                           &
                           cbdy_ys, bdy_ys,                           &
                           cbdy_ye, bdy_ye,                           &
                           cbdy_txs, bdy_txs,                       &
                           cbdy_txe, bdy_txe,                       &
                           cbdy_tys, bdy_tys,                       &
                           cbdy_tye, bdy_tye,                       &
                           cdt, ndt                              &
                                        )

!    USE module_configure , ONLY : nl_get_spec_zone, nl_get_relax_zone
     USE module_state_description

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  ! ignore
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye   ! not used
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xs, bdy_txs
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xe, bdy_txe
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ys, bdy_tys
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ye, bdy_tye

     ! Local

     REAL*8 rdt
     INTEGER ci, cj, ck, ni, nj, nk, ni1, nj1, nk1, ip, jp, ioff, joff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca1(cims:cime,cjms:cjme,nri*nrj)
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER shw
     INTEGER spec_zone 
     INTEGER relax_zone
     INTEGER sz
     INTEGER n2ci,n
     INTEGER n2cj

! statement functions for converting a nest index to coarse
     n2ci(n) = (n+ipos*nri-1)/nri
     n2cj(n) = (n+jpos*nrj-1)/nrj

     rdt = 1.D0/cdt

     shw = 0

     ioff  = 0 ; joff  = 0
     IF ( xstag ) THEN 
       ioff = MAX((nri-1)/2,1)
     ENDIF
     IF ( ystag ) THEN
       joff = MAX((nrj-1)/2,1)
     ENDIF

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )
     sz = MIN(MAX( spec_zone, relax_zone + 1 ),spec_bdy_width)

     nfx = nri * nrj

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ni1,nj1,ci,cj,ip,jp,nk,ck,nf,icmask,psca,psca1 )
     DO k = ckts, ckte

        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrj / 2 + 1 )  ! j point on nest
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nri / 2 + 1 )   ! i point on nest
                psca1(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO
! hopefully less ham handed but still correct and more efficient
! sintb ignores icmask so it does not matter that icmask is not set
!
! SOUTH BDY
               IF   ( njts .ge. njds .and. njts .le. njds + sz + joff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njds)), n2cj(MIN(njte,njds+sz+joff)), nrj*nri, xstag, ystag )
               ENDIF
! NORTH BDY
               IF   ( njte .le. njde .and. njte .ge. njde - sz - joff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njde-sz-joff)), n2cj(MIN(njte,njde-1+joff)), nrj*nri, xstag, ystag )
               ENDIF
! WEST BDY
               IF   ( nits .ge. nids .and. nits .le. nids + sz + ioff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nids)), n2ci(MIN(nite,nids+sz+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF
! EAST BDY
               IF   ( nite .le. nide .and. nite .ge. nide - sz - ioff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nide-sz-ioff)), n2ci(MIN(nite,nide-1+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF

        DO nj1 = MAX(njds,njts-1), MIN(njde+joff,njte+joff+1) 
           cj = jpos + (nj1-1) / nrj     ! j coord of CD point 
           jp = mod ( nj1-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni1 = MAX(nids,nits-1), MIN(nide+ioff,nite+ioff+1)
               ci = ipos + (ni1-1) / nri      ! j coord of CD point 
               ip = mod ( ni1-1 , nri )  ! coord of ND w/i CD point

               ni = ni1-ioff
               nj = nj1-joff

               IF ( ( ni.LT.nids) .OR. (nj.LT.njds) ) THEN
                  CYCLE
               END IF

!bdy contains the value at t-dt. psca contains the value at t
!compute dv/dt and store in bdy_t
!afterwards store the new value of v at t into bdy
        ! WEST
               IF   ( ni .ge. nids .and. ni .lt. nids + sz ) THEN
                 bdy_txs( nj,k,ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_xs( nj,k,ni ) = nfld(ni,k,nj)
               ENDIF

        ! SOUTH
               IF   ( nj .ge. njds .and. nj .lt. njds + sz ) THEN
                 bdy_tys( ni,k,nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_ys( ni,k,nj ) = nfld(ni,k,nj)
               ENDIF

        ! EAST
               IF ( xstag ) THEN
                 IF   ( ni .ge. nide - sz + 1 .AND. ni .le. nide ) THEN
                   bdy_txe( nj,k,nide-ni+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni+1 ) = nfld(ni,k,nj)
                 ENDIF
               ELSE
                 IF   ( ni .ge. nide - sz .AND. ni .le. nide-1 ) THEN
                   bdy_txe( nj,k,nide-ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni ) = nfld(ni,k,nj)
                 ENDIF
               ENDIF

        ! NORTH
               IF ( ystag ) THEN
                 IF   ( nj .ge. njde - sz + 1 .AND. nj .le. njde  ) THEN
                   bdy_tye( ni,k,njde-nj+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj+1 ) = nfld(ni,k,nj)
                 ENDIF
               ELSE
                 IF   (  nj .ge. njde - sz .AND. nj .le. njde-1 ) THEN
                   bdy_tye(ni,k,njde-nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj ) = nfld(ni,k,nj)
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO
                  
     RETURN

   END SUBROUTINE bdy_interp1

!==================================

   SUBROUTINE bdy_interp2( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde, spec_bdy_width ,        &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cbdy_xs, bdy_xs,                      &
                           cbdy_xe, bdy_xe,                      &
                           cbdy_ys, bdy_ys,                      &
                           cbdy_ye, bdy_ye,                      &
                           cbdy_txs, bdy_txs,                    &
                           cbdy_txe, bdy_txe,                    &
                           cbdy_tys, bdy_tys,                    &
                           cbdy_tye, bdy_tye,                    &
                           cdt, ndt                              &
                                                                 )

!    USE module_configure , ONLY : nl_get_spec_zone, nl_get_relax_zone
!    USE module_state_description
     USE module_interp_info

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  ! ignore
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys   ! not used
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye   ! not used
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xs, bdy_txs
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xe, bdy_txe
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ys, bdy_tys
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ye, bdy_tye

     ! Local

     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld_horiz_interp ! mem dimensioned on purpose
                                                                              ! to allow interpolating routine
                                                                              ! to assume this is a mem
                                                                              ! sized array
     INTEGER ni, nj, nk, istag, jstag
     INTEGER shw
     INTEGER spec_zone 
     INTEGER relax_zone
     INTEGER sz
     REAL*8 rdt

     shw = 0  ! dummy, not used, but needed for the calling interface

     !  Horizontally interpolate the CG to the FG, store in nfld_horiz_interp

     CALL interp_fcn ( cfld,                                 &  ! CD field
                       cids, cide, ckds, ckde, cjds, cjde,   &
                       cims, cime, ckms, ckme, cjms, cjme,   &
                       cits, cite, ckts, ckte, cjts, cjte,   &
                       nfld_horiz_interp,                    &  ! ND field
                       nids, nide, nkds, nkde, njds, njde,   &
                       nims, nime, nkms, nkme, njms, njme,   &
                       MAX(nits-nri,nids),MIN(nite+nri,nide),&
                       nkts, nkte,                           &
                       MAX(njts-nrj,njds),MIN(njte+nrj,njde),&
                       shw,                                  &  ! stencil half width for interp
                       imask,                                &  ! interpolation mask
                       xstag, ystag,                         &  ! staggering of field
                       ipos, jpos,                           &  ! Position of lower left of nest in CD
!                      ipos-1, jpos-1,                       &  ! Position of lower left of nest in CD
                       nri, nrj                              )  ! Nest ratio, i- and j-directions

     !  Staggering, to determine loop indexes

     IF ( xstag ) THEN 
        istag = 0
     ELSE
        istag = 1
     END IF
     IF ( ystag ) THEN
        jstag = 0
     ELSE 
        jstag = 1
     END IF

     !  CG time step reciprocal, for computing tendencies.

     rdt = 1.D0/cdt

     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )

     !  Belt and suspenders ... sz is just spec_bdy_width.

     sz = MIN(MAX( spec_zone, relax_zone + 1 ),spec_bdy_width)

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ni,nj,nk )
     
     DO nj = MAX ( njts-nrj, njds ) , MIN ( njte+nrj, njde-jstag )
        DO nk = nkts, nkte
           DO ni = MAX( nits-nri, nids ) , MIN ( nite+nri, nide-istag )

              !  WEST boundary

              IF ( ni .LT. nids + sz ) THEN
                  bdy_txs(nj,nk,ni) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                  bdy_xs (nj,nk,ni) =      nfld(ni,nk,nj)
              END IF

               !  SOUTH boundary

               IF ( nj .LT. njds + sz ) THEN
                  bdy_tys(ni,nk,nj) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                  bdy_ys (ni,nk,nj) =      nfld(ni,nk,nj)
               END IF

               !  EAST boundary

               IF ( xstag ) THEN
                  IF ( ( ni .GE. nide - sz + 1 ) .AND. ( ni .LE. nide ) ) THEN
                     bdy_txe(nj,nk,nide-ni+1) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_xe (nj,nk,nide-ni+1) =      nfld(ni,nk,nj)
                  END IF
               ELSE
                  IF ( ( ni .GE. nide - sz ) .AND. ( ni .LE. nide-1 ) ) THEN
                     bdy_txe(nj,nk,nide-ni  ) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_xe (nj,nk,nide-ni  ) =      nfld(ni,nk,nj)
                  END IF
               END IF

               !  NORTH boundary

               IF ( ystag ) THEN
                  IF ( ( nj .GE. njde - sz + 1 ) .AND. ( nj .LE. njde  ) ) THEN
                     bdy_tye(ni,nk,njde-nj+1) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_ye (ni,nk,njde-nj+1) =      nfld(ni,nk,nj)
                  END IF
               ELSE
                  IF ( (  nj .GE. njde - sz ) .AND. ( nj .LE. njde-1 ) ) THEN
                     bdy_tye(ni,nk,njde-nj  ) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_ye (ni,nk,njde-nj  ) =      nfld(ni,nk,nj)
                  END IF
               END IF

           END DO      ! nest i
        END DO         ! nest k
     END DO            ! nest j

   !$OMP END PARALLEL DO

   END SUBROUTINE bdy_interp2

!==================================

   SUBROUTINE interp_fcni( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,'("cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte, cjts,cjte
!write(0,'("nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte, njts,njte

     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              if ( imask(ni,nj) .NE. 1 ) cycle
              ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcni

   SUBROUTINE interp_fcnm( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,'("mask cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte, cjts,cjte
!write(0,'("mask nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte, njts,njte

     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm

   SUBROUTINE interp_fcnm_lu( cfld,                              &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           cxlat,    nxlat,                      &
                           cxlong,   nxlong,                     &
                           cdx, ndx,                             &
                           cid, nid                            ) 
     USE module_configure

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj,                             &
                            cid, nid
     LOGICAL, INTENT(IN) :: xstag, ystag 

     REAL,    INTENT(IN) :: cdx, ndx

     REAL,    INTENT(IN),  DIMENSION ( cims:cime, cjms:cjme ) :: cxlat, cxlong
     REAL,    INTENT(IN),  DIMENSION ( nims:nime, njms:njme ) :: nxlat, nxlong


     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER i, ci, cj, ck, ni, nj, nk, ip, jp, ierr

#ifdef TERRAIN_AND_LANDUSE
     INTEGER, DIMENSION(256) :: ipath  ! array for integer coded ascii for passing path down to get_landuse

     REAL , ALLOCATABLE, DIMENSION(:,:) :: xlat_g, xlon_g, landuse_g
     CHARACTER*256 :: message 
     CHARACTER*256 :: rsmas_data_path

     LOGICAL :: input_from_hires, input_from_file

     INTEGER, EXTERNAL :: get_landuse
     LOGICAL, EXTERNAL :: wrf_dm_on_monitor

     CALL nl_get_input_from_hires( nid , input_from_hires)
     CALL nl_get_input_from_file ( nid , input_from_file )

     IF ( input_from_file .AND. input_from_hires ) THEN
       Write(message, '(a,i3,a)') & 
          "Warning : input_from_file turned on for domain ", nid, ", input_from_hires disabled"
       CALL wrf_message(message)
     END IF

     IF ( .NOT. input_from_file .AND. input_from_hires ) THEN

       allocate(xlat_g(nids:nide,njds:njde))
       allocate(xlon_g(nids:nide,njds:njde))
       allocate(landuse_g(nids:nide,njds:njde))

       CALL nl_get_rsmas_data_path(1,rsmas_data_path)

       DO i = 1, LEN(TRIM(rsmas_data_path))
          ipath(i) = ICHAR(rsmas_data_path(i:i))
       ENDDO

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
       CALL wrf_patch_to_global_real ( nxlat, xlat_g , nid, ' ' , 'xy' ,   &
                                       nids, nide-1 , njds , njde-1 , 1 , 1 ,             &
                                       nims, nime   , njms , njme   , 1 , 1 ,             &
                                       nits, nite   , njts , njte   , 1 , 1   )
       CALL wrf_patch_to_global_real ( nxlong, xlon_g, nid, ' ' , 'xy' ,   &
                                       nids, nide-1 , njds , njde-1 , 1 , 1 ,             &
                                       nims, nime   , njms , njme   , 1 , 1 ,             &
                                       nits, nite   , njts , njte   , 1 , 1   )
       IF ( wrf_dm_on_monitor() ) THEN
         ierr = get_landuse ( ndx/1000., xlat_g, xlon_g, &
                              landuse_g,                                        &
                              nide-nids+1,njde-njds+1,nide-nids+1,njde-njds+1,  &
                              ipath, LEN(TRIM(rsmas_data_path)) )
         IF ( ierr == 1 ) THEN
            WRITE(message,fmt='(a)') 'get_landuse : aborted!'
            CALL wrf_error_fatal(TRIM(message))
         ENDIF
       ENDIF

       CALL wrf_global_to_patch_real ( landuse_g , nfld(:,1,:), nid, ' ' , 'xy' ,  &
                                      nids, nide-1 , njds , njde-1 , 1 , 1 ,    &
                                      nims, nime   , njms , njme   , 1 , 1 ,    &
                                      nits, nite   , njts , njte   , 1 , 1   )

#else
       ierr = get_landuse ( ndx/1000., nxlat(nids:nide,njds:njde), nxlong(nids:nide,njds:njde),  &
                            nfld(nids:nide,1,njds:njde),                                         &
                            nide-nids+1,njde-njds+1,nide-nids+1,njde-njds+1,                     &
                            ipath, LEN(TRIM(rsmas_data_path)) )
#endif
       deallocate(xlat_g)
       deallocate(xlon_g)
       deallocate(landuse_g)
     ELSE
#endif
   ! Iterate over the ND tile and compute the values
   ! from the CD tile.
     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     ! j coord of CD point
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      ! j coord of CD point
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
              if ( imask(ni,nj) .eq. 1 ) then
               nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO
#ifdef TERRAIN_AND_LANDUSE
     END IF
#endif
     RETURN

   END SUBROUTINE interp_fcnm_lu


   SUBROUTINE interp_fcnm_imask( cfld,                           &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,'("mask cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte,cjts,cjte
!write(0,'("mask nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte,njts,njte

     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
              if ( imask(ni,nj) .eq. 1 ) then
               nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm_imask
#endif
! end of first block of ARW-only routines

! NMM: We still allow interp_mask_land_field because it is needed, but no
! equivalent exists.  Use of this in WRF-NMM is an error and will have
! unintended consequences.
   SUBROUTINE interp_mask_land_field ( enable,                   &  ! says whether to allow interpolation or just the bcasts
                                       cfld,                     &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      ! Local
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER(LEN=255) :: message
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
   
      !  Find out what the water value is.
   
      CALL nl_get_iswater(1,iswater)

      !  Right now, only mass point locations permitted.
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         !  Loop over each i,k,j in the nested domain.

       IF ( enable ) THEN

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  END IF
   



                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)  
                  !
                  !                        |--|--------|
                  !                         dx  1-dx         


                  !  For odd ratios, at (nri+1)/2, we are on the coarse grid point, so dx = 0

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  !  This is a "land only" field.  If this is a water point, no operations required.

                  IF      ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) =  cfld(ci  ,ck,cj  )

                  !  If this is a nested land point, and the surrounding coarse values are all land points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested land point and there are NO coarse land values surrounding,
                  !  we temporarily punt.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = -1

                  !  If there are some water points and some land points, take an average. 
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .NE. iswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO


         !  Get an average of the whole domain for problem locations.

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .NE. iswater ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -1) ) THEN

         !  OK, if there were any of those island situations, we try to search a bit broader
         !  of an area in the coarse grid.

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( imask(ni, nj) .NE. 1 ) cycle
                    IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .NE. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                          Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - island (', ni, ',', nj, '), using average ', avg_n(nk)
                          CALL wrf_message ( message )
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( enable,                  &  ! says whether to allow interpolation or just the bcasts
                                        cfld,                    &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu, cflag, nflag                )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj, cflag, nflag
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      ! Local
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
      CHARACTER(LEN=255) :: message

      !  Right now, only mass point locations permitted.
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

       IF ( enable ) THEN
         !  Loop over each i,k,j in the nested domain.

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
!dave             IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  END IF
   



                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)  
                  !
                  !                        |--|--------|
                  !                         dx  1-dx         


                  !  At ni=2, we are on the coarse grid point, so dx = 0

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  !  This is a "water only" field.  If this is a land point, no operations required.

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = cfld(ci  ,ck,cj  )

                  !  If this is a nested water point, and the surrounding coarse values are all water points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested water point and there are NO coarse water values surrounding,
                  !  we temporarily punt.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = -4

                  !  If there are some land points and some water points, take an average. 
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

         !  Get an average of the whole domain for problem locations.

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .EQ. nflag ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -4) ) THEN

           !  OK, if there were any of those lake situations, we try to search a bit broader
           !  of an area in the coarse grid.

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
!dave               IF ( imask(ni, nj) .NE. 1 ) cycle
                    IF ( nfld(ni,nk,nj) .EQ. -4 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .EQ. nflag ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                         Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - lake (', ni, ',', nj, '), using average ', avg_n(nk)
                         CALL wrf_message ( message )                         
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_water_field

! Begin second block of ARW-only routines
#if ! defined(NMM_CORE) || NMM_CORE!=1
   SUBROUTINE p2c_mask (   cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu,                             &  ! land use categories
                           ctslb,ntslb,                          &  ! soil temps
                           cnum_soil_layers,nnum_soil_layers,    &  ! number of soil layers for tslb
                           ciswater, niswater                    )  ! iswater category

      USE module_configure
      USE module_wrf_error

      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj,                             &
                             cnum_soil_layers, nnum_soil_layers,   &
                             ciswater, niswater 

      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      REAL, DIMENSION ( cims:cime, 1:cnum_soil_layers, cjms:cjme ) :: ctslb
      REAL, DIMENSION ( nims:nime, 1:nnum_soil_layers, njms:njme ) :: ntslb

      ! Local
   
      INTEGER ci, cj, ck, ni, nj, nk
      INTEGER :: icount 
      REAL :: sum , dx , dy

      !  Right now, only mass point locations permitted.
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         !  Loop over each i,k,j in the nested domain.

         DO nj = njts, MIN(njde-1,njte)
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 ! first coarse position equal to or below nest point
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, MIN(nide-1,nite)
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 ! first coarse position equal to or to the left of nest point
                  END IF

                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)  
                  !
                  !                        |--|--------|
                  !                         dx  1-dx         


                  !  At ni=2, we are on the coarse grid point, so dx = 0

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  !  This is a "water only" field.  If this is a land point, no operations required.

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. niswater ) ) THEN
                     nfld(ni,nk,nj) = 273.18

                  !  If this is a nested water point, and the surrounding coarse values are all water points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. niswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested water point and there are NO coarse water values surrounding,
                  !  we manufacture something from the deepest CG soil temp.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. niswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * ctslb(ci  ,cnum_soil_layers,cj  )   + &
                                                             dy   * ctslb(ci  ,cnum_soil_layers,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * ctslb(ci+1,cnum_soil_layers,cj  )   + &
                                                             dy   * ctslb(ci+1,cnum_soil_layers,cj+1) )

                  !  If there are some land points and some water points, take an average of the water points.
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE p2c_mask

   SUBROUTINE none
   END SUBROUTINE none

   SUBROUTINE smoother ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      xstag, ystag,                         &  ! staggering of field
                      ipos, jpos,                           &  ! Position of lower left of nest in
                      nri, nrj                              &
                      )
 
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
      INTEGER             :: smooth_option, feedback , spec_zone
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld

      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN
      CALL nl_get_spec_zone ( 1, spec_zone )

      !  These are the 2d smoothers used on the fedback data.  These filters
      !  are run on the coarse grid data (after the nested info has been
      !  fedback).  Only the area of the nest in the coarse grid is filtered.

      CALL nl_get_smooth_option  ( 1, smooth_option  )

      IF      ( smooth_option == 0 ) THEN
! no op
      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
      END IF

   END SUBROUTINE smoother 

   SUBROUTINE sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  Simple 1-2-1 smoother.
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckts , ckte
   
            !  Initialize dummy cfldnew

            DO i = MAX(ipos,cits-3) , MIN(ipos+(nide-nids)/nri,cite+3)
               DO j = MAX(jpos,cjts-3) , MIN(jpos+(njde-njds)/nrj,cjte+3)
                  cfldnew(i,j) = cfld(i,k,j) 
               END DO
            END DO

            !  1-2-1 smoothing in the j direction first, 
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,k,j+1) + 2.*cfld(i,k,j) + cfld(i,k,j-1) )
               END DO
            END DO

            !  then 1-2-1 smoothing in the i direction last
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121

   SUBROUTINE smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  ! staggering of field
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  ! Position of lower left of nest in 
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 ! More passes requires a larger stencil (currently 48 pt)

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      !  The odd number passes of this are the "smoother", the even
      !  number passes are the "de-smoother" (note the different signs on xnu).
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckts , ckte
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i,k,j+1) + cfld(i,k,j-1)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i+1,k,j) + cfld(i-1,k,j)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm

!==================================
! this is used to modify a field over the nest so we can see where the nest is

   SUBROUTINE mark_domain (  cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
        nj = (cj-jpos)*nrj + jstag + 1
        DO ck = ckts, ckte
           nk = ck
           DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
              ni = (ci-ipos)*nri + istag + 1
              cfld( ci, ck, cj ) =  9021000.  !magic number: Beverly Hills * 100.
           ENDDO
        ENDDO
     ENDDO

   END SUBROUTINE mark_domain
#endif 
! end of second block of WRF-ARW-specific interpolation schemes

#if ( NMM_CORE == 1 )
!=======================================================================================
!  Old circa 2007 interpolation schemes that are still in use
!  This is gopal's doing
!=======================================================================================
  SUBROUTINE force_sst_nmm    (cfld,                                 &  ! CD field
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  ! ND field
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  ! stencil half width for interp
                               imask,                                &  ! interpolation mask
                               xstag, ystag,                         &  ! staggering of field
                               ipos, jpos,                           &  ! Position of lower left of nest in CD
                               nri, nrj,                             &  ! nest ratios                         
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  ! south-western grid locs and weights 
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  ! note that "C"ourse grid ones are
                               CBWGT4, HBWGT4, CCSST, CSST           )  ! just dummys
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     INTEGER , INTENT(IN) :: csst(*), ccsst(*)
!    local

     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT

     if(csst(1) /= 1) return

!
!*** INDEX CONVENTIONS
!***                     NBWGT4=0
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            NBWGT1=1           NBWGT2=0
!***
!***
!***                      3
!***                     NBWGT3=0

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
            NBWGT(1,I,J)=HBWGT1(I,J)
            NBWGT(2,I,J)=HBWGT2(I,J)
            NBWGT(3,I,J)=HBWGT3(I,J)
            NBWGT(4,I,J)=HBWGT4(I,J)
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
          AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO

          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal ( "horizontal interp error - interp_hnear_nmm" )
          ENDDO
      ENDDO
     ENDDO

       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7 
                NFLD(I,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ) &
                          + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ) &
                          + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1) &
                          + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1)
            ELSE
                NFLD(I,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ) &
                          + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ) &
                          + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1) &
                          + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1)
            ENDIF
        ENDDO
       ENDDO


     END SUBROUTINE force_sst_nmm

   SUBROUTINE nmm_smoother_ikj ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      ! Local

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGH
      CHARACTER (LEN=256) :: a_message

      RETURN
      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
       if(mod(cj,2) .eq. 0)THEN
        is=0 ! even rows for mass points (2,4,6,8)
       else
        is=1 ! odd rows for mass points  (1,3,5,7)
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
            IF(IS==0)THEN    ! (2,4,6,8)
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGH + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
       if(mod(cj,2) .eq. 0)THEN
        is=0 ! even rows for mass points (2,4,6,8)
       else
        is=1 ! odd rows for mass points  (1,3,5,7)
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO   ! do npass

   END SUBROUTINE nmm_smoother_ikj


   SUBROUTINE nmm_smoother_ijk ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      ! Local

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGH
      CHARACTER (LEN=256) :: a_message

      RETURN
      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO ck = ckts, ckte
       DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
        if(mod(cj,2) .eq. 0)THEN
         is=0 ! even rows for mass points (2,4,6,8)
        else
         is=1 ! odd rows for mass points  (1,3,5,7)
        endif
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
            IF(IS==0)THEN    ! (2,4,6,8)
             AVGH = CFLD(CI,CJ+1,CK) + CFLD(CI,CJ-1,CK) + CFLD(CI+1,CJ+1,CK) + CFLD(CI+1,CJ-1,CK)
            ELSE
             AVGH = CFLD(CI,CJ+1,CK) + CFLD(CI,CJ-1,CK) + CFLD(CI-1,CJ+1,CK) + CFLD(CI-1,CJ-1,CK)
            ENDIF
            CFLDNEW(CI,CJ,CK) = (AVGH + 4*CFLD(CI,CJ,CK)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO ck = ckts, ckte
       DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
        if(mod(cj,2) .eq. 0)THEN
         is=0 ! even rows for mass points (2,4,6,8)
        else
         is=1 ! odd rows for mass points  (1,3,5,7)
        endif
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
           CFLD(CI,CJ,CK) = CFLDNEW(CI,CJ,CK)
        ENDDO
       ENDDO
      ENDDO

      ENDDO   ! do npass

   END SUBROUTINE nmm_smoother_ijk


   SUBROUTINE nmm_vsmoother_ikj ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      ! Local

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGV
      CHARACTER (LEN=256) :: a_message

      RETURN
      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(a_message,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR VELOCITY'
      CALL wrf_message ( a_message )                       

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
       if(mod(cj,2) .eq. 0)THEN
        is=1 ! even rows for mass points (2,4,6,8)
       else
        is=0 ! odd rows for mass points  (1,3,5,7)
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
            IF(IS==0)THEN    ! (2,4,6,8)
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGV + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
       if(mod(cj,2) .eq. 0)THEN
        is=1 ! even rows for mass points (2,4,6,8)
       else
        is=0 ! odd rows for mass points  (1,3,5,7)
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO

   END SUBROUTINE nmm_vsmoother_ikj
!======================================================================================
!   End of gopal's doing
!======================================================================================
!======================================================================================
!   New NMM Interpolation Routines; wrappers around module_interp_nmm (Sam's doing)
!======================================================================================

!--------------------------------------------------------------------------------------
   
   subroutine NoInterpMany(cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios                         
        cpint,npint, cpd,npd, cq,nq, ct,nt,   &
        cfis,nfis)
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK

     !  parent domain

     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD,cpint,ct,cq
     REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: cpd,cfis

     !  nested domain

     REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: nFIS,npd
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)           :: NFLD,npint,nt,nq

   end subroutine NoInterpMany

   subroutine DownAged2D(junk,                &
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj, &
        c_age,n_age, cfld)

     use module_interp_nmm, only: c2n_copy2d_nomask
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     integer, intent(in) :: c_age
     integer, intent(inout) :: n_age

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD,junk
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)        :: nfld

     logical bad
     integer i,j
     ! Skip if the nest is up-to-date with the parent.  Special age
     ! of 0 means the values are invalid (parent just moved, nest
     ! just moved or one was initialized).
     if(n_age==c_age .and. n_age/=0 .and. c_age/=0) then
        !write(0,*) 'Grid ',grid_id,' not storing pdyn in DownAged2D'
        !write(0,*) '  reason: n_age=',n_age,' c_age=',c_age
        return
     end if
     n_age=c_age
     !write(0,*) 'Storing grid ',parent_grid_id,' pdyn_smooth in grid ',grid_id,' pdyn_parent'

     call c2n_copy2d_nomask(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
          cfld,nfld,                &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte, .true.)

   end subroutine DownAged2D

   subroutine ForceNearSST   (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj, &
        cactivate, nactivate)
     use module_interp_nmm, only: c2n_sst
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     integer, intent(In), dimension(*) :: cactivate, nactivate

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)        :: nfld

     if(nactivate(1)/=1) return

     call c2n_sst(hnear_i,hnear_j,          &
          cfld,nfld,                &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine ForceNearSST

   subroutine DownNear      (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios         
     use module_interp_nmm, only: c2n_near2d, c2n_near3d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_near2d(hnear_i,hnear_j,   &
             cfld,nfld,imask,                       &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte)
     else
        call c2n_near3d(hnear_i,hnear_j,   &
             cfld,nfld,imask,                       &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine DownNear

   subroutine DownNearIKJ   (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios         
     use module_interp_nmm, only: c2n_near3dikj
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(INOUT)          :: nfld
     
     if(nkts==nkte) &
          call wrf_error_fatal('IJ interpolation of an IKJ variable is not supported and makes no sense anyway.  Use DownNear instead.')

     call c2n_near3dikj(hnear_i,hnear_j,   &
          cfld,nfld,imask,                       &
          cims, cime, cjms, cjme, ckms, ckme,  &
          nids, nide, njds, njde, nkds, nkde,  &
          nims, nime, njms, njme, nkms, nkme,  &
          nits, nite, njts, njte, nkts, nkte)
   end subroutine DownNearIKJ

   subroutine UpNear(cfld,                    &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         
     use module_interp_store
     use module_interp_nmm, only: n2c_near2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme),   INTENT(IN)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal('Up nearest neighbor interpolation is not implemented.')

     call n2c_near2d( cfld,nfld,ipos,jpos,                  &
          cids, cide, cjds, cjde,   &
          cims, cime, cjms, cjme,   &
          cits, cite, cjts, cjte,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine UpNear

   subroutine DownINear      (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios         
     use module_interp_nmm, only: c2n_inear2d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     INTEGER,DIMENSION(cims:cime,cjms:cjme),   INTENT(IN)           :: CFLD
     INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal('3D integer nearest neighbor interpolation is not implemented.')

     call c2n_inear2d(hnear_i,hnear_j,   &
          cfld,nfld,imask,                       &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine DownINear

   subroutine UpINear        (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         
     use module_interp_store
     use module_interp_nmm, only: n2c_inear2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     INTEGER,DIMENSION(cims:cime,cjms:cjme),   INTENT(OUT)           :: CFLD
     INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(IN)          :: nfld

     if(nkts/=nkte) &
          call wrf_error_fatal('3D integer nearest neighbor interpolation is not implemented.')

     call n2c_inear2d( cfld,nfld,ipos,jpos,                  &
          cids, cide, cjds, cjde,   &
          cims, cime, cjms, cjme,   &
          cits, cite, cjts, cjte,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)
   end subroutine UpINear

   subroutine DownMass      (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        emethod, evalue)                         ! extrapolation method
     use module_interp_nmm, only: c2n_mass, c2n_copy2d
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call c2n_mass(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,   &
             cfld,nfld,iinfo,winfo,imask,          &
             emethod,evalue,                       &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine DownMass

   subroutine DownMassIKJ   (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        emethod, evalue)                         ! extrapolation method
     use module_interp_nmm, only: c2n_massikj
     use module_interp_store
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj,emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) &
          call wrf_error_fatal('IKJ 2D interpolation of an IJ array is not implemented (and makes no sense anyway).  Use DownCopy instead.')
     call c2n_massikj(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,   &
          cfld,nfld,iinfo,winfo,imask,          &
          emethod, evalue,                      &
          cims, cime, cjms, cjme, ckms, ckme,   &
          nids, nide, njds, njde, nkds, nkde,   &
          nims, nime, njms, njme, nkms, nkme,   &
          nits, nite, njts, njte, nkts, nkte)
   end subroutine DownMassIKJ

   subroutine UpMassIKJ     (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios                         
        emethod, evalue)                         ! extrapolation method
     use module_interp_store
     use module_interp_nmm, only: n2c_massikj, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,ckms:ckme,cjms:cjme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,nkms:nkme,njms:njme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_massikj(&
             cfld,nfld,parent_iinfo,parent_winfo,  &
             ipos,jpos,emethod, evalue,            &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine UpMassIKJ

   subroutine UpMass        (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios                         
        emethod, evalue)                         ! extrapolation method
     use module_interp_store
     use module_interp_nmm, only: n2c_mass, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj, emethod
     real, intent(in) :: evalue

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_mass(&
             cfld,nfld,parent_iinfo,parent_winfo,  &
             ipos,jpos,emethod, evalue,            &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   end subroutine UpMass

   subroutine UpCopy         (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         

     use module_interp_nmm, only: n2c_copy3d, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_copy3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine UpCopy

   subroutine UpMax          (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         

     use module_interp_nmm, only: n2c_max3d, n2c_max2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_max2d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call n2c_max3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine UpMax

   subroutine DownCopy      (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         
     use module_interp_store
     use module_interp_nmm, only: c2n_copy3d, c2n_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .true.)
     else
        call c2n_copy3d(IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .true.)
     endif
   end subroutine DownCopy

   subroutine UpVel   (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         

     use module_interp_nmm, only: n2c_copy3d, n2c_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(OUT)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(IN)          :: nfld

     if(nkts==nkte) then
        call n2c_copy2d( cfld,nfld,ipos,jpos,             &
             cids, cide, cjds, cjde,   &
             cims, cime, cjms, cjme,   &
             cits, cite, cjts, cjte,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call n2c_copy3d(&
             cfld,nfld,ipos,jpos,                   &
             cids, cide, cjds, cjde, ckds, ckde,   &
             cims, cime, cjms, cjme, ckms, ckme,   &
             cits, cite, cjts, cjte, ckts, ckte,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   end subroutine UpVel

   subroutine DownVel      (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width for interp
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj)                                ! nest ratios                         
     use module_interp_store
     use module_interp_nmm, only: c2n_copy3d, c2n_copy2d
     implicit none
     LOGICAL,INTENT(IN) :: xstag, ystag
     INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,ipos,jpos,nri,nrj

     INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
     REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
     REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)          :: nfld

     if(nkts==nkte) then
        call c2n_copy2d(IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2n_copy3d(IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4,  &
             cfld,nfld,imask,                &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   end subroutine DownVel

   SUBROUTINE BdyMass (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye,                        &
        emethod,evalue)                          ! Extrapolation information

     !     use module_state_description
     USE module_configure
     USE module_wrf_error
     use module_interp_store
     use module_interp_nmm, only: c2b_mass, c2b_copy2d

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj, emethod
     real, intent(in) :: evalue

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld,ccwm
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld,ncwm
     !
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Output field boundary info:
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     ! Unused parameters:
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iih,jjh,                   &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte,   &
             .true.)
     else
        call c2b_mass(iih,jjh,                     &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             iinfo_bxs,iinfo_bxe,iinfo_bys,iinfo_bye,          &
             winfo_bxs,winfo_bxe,winfo_bys,winfo_bye,          &
             n_bxs, n_bxe, n_bys, n_bye,           &
             emethod, evalue,                      &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte)
     endif
   END SUBROUTINE BdyMass

   !
   !--------------------------------------------------------------------------------------
   !
   SUBROUTINE BdyCopy  (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     !     use module_state_description
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_copy3d, c2b_copy2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     !
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Output field boundary info:
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     ! Nest-parent horizontal interpolation information:
     ! Unused parameters:
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iiv,jjv,                   &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2b_copy3d(iih,jjh,                     &
             hbwgt1,hbwgt2,hbwgt3,hbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif

   END SUBROUTINE BdyCopy
   !
   !--------------------------------------------------------------------------------------
   !
   subroutine NoInterp()
   end subroutine NoInterp
   !
   !--------------------------------------------------------------------------------------
   !
   SUBROUTINE BdyVel  (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     !     use module_state_description
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_copy3d, c2b_copy2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     !
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Output field boundary info:
     real,dimension(nims:nime,nkms:nkme,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,nkms:nkme,bdyw) :: n_bxs,n_bxe

     ! Nest-parent horizontal interpolation information:
     ! Unused parameters:
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts==nkte) then
        call c2b_copy2d(iiv,jjv,                     &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme,   &
             nids, nide, njds, njde,   &
             nims, nime, njms, njme,   &
             nits, nite, njts, njte, .false.)
     else
        call c2b_copy3d(iiv,jjv,                     &
             vbwgt1,vbwgt2,vbwgt3,vbwgt4,          &
             cfld,                                 &
             n_bxs, n_bxe, n_bys, n_bye,           &
             cims, cime, cjms, cjme, ckms, ckme,   &
             nids, nide, njds, njde, nkds, nkde,   &
             nims, nime, njms, njme, nkms, nkme,   &
             nits, nite, njts, njte, nkts, nkte, .false.)
     endif
   END SUBROUTINE BdyVel
   !
   !--------------------------------------------------------------------------------------
   !
   SUBROUTINE BdyNear (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     !     use module_state_description
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_near2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nfld

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Output field boundary info:
     real,dimension(nims:nime,1,bdyw) :: n_bys,n_bye
     real,dimension(njms:njme,1,bdyw) :: n_bxs,n_bxe

     ! Unused parameters:
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts/=nkte) &
          call wrf_error_fatal('3D boundary nearest neighbor interpolation is not implemented.')
     call c2b_near2d(hnear_i,hnear_j,cfld,      &
          n_bxs, n_bxe, n_bys, n_bye,           &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)

   END SUBROUTINE BdyNear

   SUBROUTINE BdyINear (cfld,                                 &  ! CD field
        cids, cide, ckds, ckde, cjds, cjde,   &
        cims, cime, ckms, ckme, cjms, cjme,   &
        cits, cite, ckts, ckte, cjts, cjte,   &
        nfld,                                 &  ! ND field
        nids, nide, nkds, nkde, njds, njde,   &
        nims, nime, nkms, nkme, njms, njme,   &
        nits, nite, nkts, nkte, njts, njte,   &
        shw,                                  &  ! stencil half width
        imask,                                &  ! interpolation mask
        xstag, ystag,                         &  ! staggering of field
        ipos, jpos,                           &  ! Position of lower left of nest in CD
        nri, nrj,                             &  ! nest ratios
        c_bxs,n_bxs,                          &
        c_bxe,n_bxe,                          &
        c_bys,n_bys,                          &
        c_bye,n_bye,                          &
        c_btxs,n_btxs,                        &
        c_btxe,n_btxe,                        &
        c_btys,n_btys,                        &
        c_btye,n_btye)

     !     use module_state_description
     USE module_configure
     USE module_wrf_error
     use module_interp_nmm, only: c2b_inear2d
     use module_interp_store

     IMPLICIT NONE
     integer, parameter :: bdyw = 1

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
          cims, cime, ckms, ckme, cjms, cjme,   &
          cits, cite, ckts, ckte, cjts, cjte,   &
          nids, nide, nkds, nkde, njds, njde,   &
          nims, nime, nkms, nkme, njms, njme,   &
          nits, nite, nkts, nkte, njts, njte,   &
          shw,                                  &
          ipos, jpos,                           &
          nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: nfld

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     ! Output field boundary info:
     integer,dimension(nims:nime,1,bdyw) :: n_bys,n_bye
     integer,dimension(njms:njme,1,bdyw) :: n_bxs,n_bxe

     ! Unused parameters:
     integer,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,c_bxe,c_bys,c_bye
     integer,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

     if(nkts/=nkte) &
          call wrf_error_fatal('3D boundary nearest neighbor interpolation is not implemented.')
     call c2b_inear2d(hnear_i,hnear_j,cfld,      &
          n_bxs, n_bxe, n_bys, n_bye,           &
          cims, cime, cjms, cjme,   &
          nids, nide, njds, njde,   &
          nims, nime, njms, njme,   &
          nits, nite, njts, njte)

   END SUBROUTINE BdyINear

!--------------------------------------------------------------------------------------
! End of Sam's doing
!--------------------------------------------------------------------------------------

#endif

! Third block of ARW-specific routines
#if ! defined(NMM_CORE) || NMM_CORE!=1

   SUBROUTINE interp_mask_field ( enable,                  &  ! says whether to allow interpolation or just the bcasts
                                       cfld,                     &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu, cflag, nflag ) 

      USE module_configure
      USE module_wrf_error
      USE module_dm , only :  wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE


      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &  ! stencil half width
                             ipos, jpos,                           &
                             nri, nrj, cflag, nflag
      LOGICAL, INTENT(IN) :: xstag, ystag

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      ! Local

      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount, ii , jj , ist , ien , jst , jen , iswater, ierr
      REAL :: avg, sum, dx , dy
      INTEGER :: icount_water(nkts:nkte), icount_land(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_water(nkts:nkte), avg_land(nkts:nkte), sum_water(nkts:nkte), sum_land(nkts:nkte), dummy(nkts:nkte)
      CHARACTER (len=256) :: message
      CHARACTER (len=256) :: a_mess

      !  Find out what the water value is.

      !CALL nl_get_iswater(1,iswater)
      iswater = nflag

      !  Right now, only mass point locations permitted.

      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

        !  Loop over each i,k,j in the nested domain.

        IF ( enable ) THEN
          DO nj = njts, njte
            ! first coarse position equal to or below nest point
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  ! first coarse position equal to or to the left of nest point
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1
                  END IF

                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)
                  !
                  !                        |--|--------|
                  !                         dx  1-dx

                  !  For odd ratios, at (nri+1)/2, we are on the coarse grid point, so dx = 0

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri )
                  ELSE
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri )
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj )
                  ELSE
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj )
                  END IF

                  ! Nested cell is a water cell.
                  IF ( ( NINT(nlu(ni, nj)) .EQ. iswater ) ) THEN

                     ! If the surrounding coarse values are all WATER points,
                     ! i.e. open water, this is a simple 4-pt interpolation. 
                     ! If the surrounding coarse values are all LAND points,
                     ! i.e. this is a 1-cell lake, we have no better way to 
                     ! come up with the value than to do a simple 4-pt interpolation.

                     IF ( ALL( clu(ci:ci+1,cj:cj+1) == iswater ) .OR. &
                          ALL( clu(ci:ci+1,cj:cj+1) /= iswater ) ) THEN

                       nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                               dy   * cfld(ci  ,ck,cj+1) ) + &
                                               dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                               dy   * cfld(ci+1,ck,cj+1) )

                     !  If there are some land points and some water points, take an average.
                     ELSE
                       icount = 0
                       sum = 0
                       IF ( NINT(clu(ci  ,cj  )) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci  ,ck,cj  )
                       END IF
                       IF ( NINT(clu(ci+1,cj  )) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci+1,ck,cj  )
                       END IF
                       IF ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci  ,ck,cj+1)
                       END IF
                       IF ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci+1,ck,cj+1)
                       END IF
                       nfld(ni,nk,nj) = sum / REAL ( icount )
                     END IF

                  ! Nested cell is a land cell.
                   ELSE IF ( ( NINT(nlu(ni, nj)) .NE. iswater ) ) THEN

                     ! If the surrounding coarse values are all LAND points,
                     ! this is a simple 4-pt interpolation. 
                     ! If the surrounding coarse values are all WATER points,
                     ! i.e. this is a 1-cell island, we have no better way to 
                     ! come up with the value than to do a simple 4-pt interpolation.

                     IF ( ALL( clu(ci:ci+1,cj:cj+1) == iswater ) .OR. &
                          ALL( clu(ci:ci+1,cj:cj+1) /= iswater ) ) THEN

                       nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                               dy   * cfld(ci  ,ck,cj+1) ) + &
                                               dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                               dy   * cfld(ci+1,ck,cj+1) )

                    !  If there are some water points and some land points, take an average.                  
                    ELSE
                      icount = 0
                      sum = 0
                      IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci  ,ck,cj  )
                      END IF
                      IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci+1,ck,cj  )
                      END IF
                      IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci  ,ck,cj+1)
                      END IF
                      IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci+1,ck,cj+1)
                      END IF
                      nfld(ni,nk,nj) = sum / REAL ( icount )

                    END IF
                  END IF

               END DO
            END DO
          END DO

        END IF
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_field


   SUBROUTINE interp_mask_soil ( enable,                  &  ! says whether to allow interpolation or just the bcasts
                                       cfld,                     &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           imask,                                &  ! interpolation mask
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu )

      USE module_configure
      USE module_wrf_error
      USE module_dm , only : wrf_dm_sum_real, wrf_dm_sum_integer

      IMPLICIT NONE


      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &  ! stencil half width
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag

      INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      ! Local

      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount, ii , jj , ist , ien , jst , jen , iswater, num_soil_cat, ierr
      REAL :: avg, sum, dx , dy
      INTEGER , ALLOCATABLE :: icount_water(:,: ), icount_land(:,:)
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER*120 message
      INTEGER, PARAMETER :: isoilwater = 14

      CALL nl_get_iswater(1,iswater)
      CALL nl_get_num_soil_cat(1,num_soil_cat)

      allocate (icount_water(nkms:nkme,1:num_soil_cat))
      allocate ( icount_land(nkms:nkme,1:num_soil_cat))

      !  Right now, only mass point locations permitted.

      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

        !  Loop over each i,k,j in the nested domain.

        IF ( enable ) THEN

          DO nj = njts, njte
             cj = jpos + (nj-1) / nrj     ! j coord of CD point 
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  ci = ipos + (ni-1) / nri      ! j coord of CD point 

                  IF ( imask(ni, nj) .NE. 1 ) cycle

                  IF ( ( NINT(nlu(ni, nj)) .EQ. iswater ) ) then

                     IF ( ( NINT(clu(ci  ,cj  )) .EQ. iswater ) ) then 
                       nfld(ni,nk,nj) = cfld(ci,ck,cj)
                     ELSE 
                       nfld(ni,nk,nj) = -1
                     ENDIF

                  ELSE IF ( ( NINT(nlu(ni, nj)) .NE. iswater ) ) THEN

                      IF ( ( NINT(clu(ci  ,cj  )) .NE. iswater ) ) THEN 
                         nfld(ni,nk,nj) = cfld(ci,ck,cj)
                      ELSE 
                         nfld(ni,nk,nj) = -1
                      ENDIF

                  END IF
               END DO
            END DO
          END DO

          DO nj = njts, njte
             DO nk = nkts, nkte
                DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .EQ. iswater ) THEN 
                        nfld(ni,nk,nj) = isoilwater
                     END IF
                  END IF
               END DO
             END DO
          END DO
#if 0
          IF ( ANY(nfld .EQ. -1) ) THEN

            !  Get an average of the whole domain for problem locations.

            sum_water    = 0
            icount_water = 0
            sum_land     = 0
            icount_land  = 0
            avg_water    = 0
            avg_land     = 0
           
            DO nj = njts, njte
               cj = jpos + (nj-1) / nrj     ! j coord of CD point 
               DO nk = nkts, nkte
                  DO ni = nits, nite
                     ci = ipos + (ni-1) / nri      ! j coord of CD point 
                     IF ( imask(ni, nj) .NE. 1 ) cycle
                     IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                        ist = MAX (ci-max_search,cits)
                        ien = MIN (ci+max_search,cite,cide-1)
                        jst = MAX (cj-max_search,cjts)
                        jen = MIN (cj+max_search,cjte,cjde-1)
                        DO jj = jst,jen
                           DO ii = ist,ien
                              IF ( NINT(clu(ii,jj)) .NE. iswater ) THEN
                                 icount_land(nk,cfld(ii,nk,jj)) = icount_land(nk,cfld(ii,nk,jj)) +1
                              END IF
                           END DO
                        END DO
                        IF ( maxval(icount_land(nk,:)) .GT. 0 .and. maxloc(icount_land(nk,:)) .ne. isoilwater ) then
                            nfld(ni,nk,nj) = maxloc(icount_land(nk,:))
                        END IF
                     END IF
                  END DO
               END DO
            END DO

          END IF ! nfld = -1


          IF ( ANY(nfld .EQ. -1) ) THEN
            sum_water    = 0
            icount_water = 0
            sum_land     = 0
            icount_land  = 0
            avg_water    = 0
            avg_land     = 0

            DO nj = njts, njte
               DO nk = nkts, nkte
                  DO ni = nits, nite
                     IF ( nlu(ni,nj ) .NE. iswater ) THEN
                        icount_land(nk,nfld(ni,nk,nj)) = icount_land(nk,nfld(ni,nk,nj)) +1
                     END IF
                  ENDDO
               ENDDO
            ENDDO

            DO nj = njts, njte
               DO nk = nkts, nkte
                  DO ni = nits, nite
                     IF ( imask(ni, nj) .NE. 1 ) cycle
                     IF ( nfld(ni,nk,nj) .EQ. -1 .and. maxloc(icount_land(nk,:)) .ne. isoilwater) THEN
                        nfld(ni,nk,nj) = MAXLOC(icount_land(nk,:))
                     END IF
                  ENDDO
               ENDDO
            ENDDO
          END IF ! nfld = -1
#endif

          IF ( ANY(nfld .EQ. -1) ) THEN
            DO nj = njts, njte
               DO nk = nkts, nkte
                  DO ni = nits, nite
                     IF ( imask(ni, nj) .NE. 1 ) cycle
                     IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                        nfld(ni,nk,nj) = 8
                     END IF
                  ENDDO
               ENDDO
            ENDDO
          END IF ! nfld = -1

        END IF  ! enable
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF


      deallocate (icount_water)
      deallocate (icount_land)

   END SUBROUTINE interp_mask_soil
#endif 
! End of third block of ARW-only routines
