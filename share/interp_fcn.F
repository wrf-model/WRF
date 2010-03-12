!WRF:MEDIATION_LAYER:INTERPOLATIONFUNCTION
!

#if (DA_CORE != 1)
#define MM5_SINT
#endif
!#define DUMBCOPY

! Note, NMM-specific routines moved to end. 20080612. JM

   SUBROUTINE interp_fcn ( cfld,                                 &  ! CD field
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
     USE module_timing
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

!logical first

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, nioff, njoff
#ifdef MM5_SINT
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER nrio2, nrjo2
#endif

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

#ifdef MM5_SINT

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
!                 if ( imask(ni,nj) .eq. 1 .or. imask(ni-nioff,nj-njoff) .eq. 1 ) then
!                   icmask( i, j ) = .TRUE.
!                 endif
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
              ENDDO
           ENDDO
        ENDDO

! tile dims in this call to sint are 1-over to account for the fact
! that the number of cells on the nest local subdomain is not 
! necessarily a multiple of the nest ratio in a given dim.
! this could be a little less ham-handed.

!call start_timing

        CALL sint( psca,                     &
                   cims, cime, cjms, cjme, icmask,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )

!call end_timing( ' sint ' )

        DO nj = njts, njte+joff
           cj = jpos + (nj-1) / nrj ! j coord of CD point 
           jp = mod ( nj-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni = nits, nite+ioff
               ci = ipos + (ni-1) / nri      ! i coord of CD point 
               ip = mod ( ni-1 , nri )  ! coord of ND w/i CD point
               if ( imask ( ni, nj ) .eq. 1 .or. imask ( ni-ioff, nj-joff ) .eq. 1  ) then
                 nfld( ni-ioff, nk, nj-joff ) = psca( ci , cj, ip+1 + (jp)*nri )
               endif
           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO
#endif

#ifdef DUMBCOPY
!write(0,'(") cims:cime, ckms:ckme, cjms:cjme ",6i4)')cims,cime, ckms,ckme, cjms,cjme
!write(0,'(") nims:nime, nkms:nkme, njms:njme ",6i4)')nims,nime, nkms,nkme, njms,njme
!write(0,'(") cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte, cjts,cjte
!write(0,'(") nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte, njts,njte

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
              if ( imask ( ni, nj ) .eq. 1 ) then
                nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO
#endif

     RETURN

   END SUBROUTINE interp_fcn

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
                           cbdy_xs, nbdy_xs,                           &
                           cbdy_xe, nbdy_xe,                           &
                           cbdy_ys, nbdy_ys,                           &
                           cbdy_ye, nbdy_ye,                           &
                           cbdy_txs, nbdy_txs,                       &
                           cbdy_txe, nbdy_txe,                       &
                           cbdy_tys, nbdy_tys,                       &
                           cbdy_tye, nbdy_tye,                       &
                           cdt, ndt                              &
                           )   ! boundary arrays
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

     CALL bdy_interp1( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde , spec_bdy_width ,       &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                           &
                           cbdy_xe, nbdy_xe,                           &
                           cbdy_ys, nbdy_ys,                           &
                           cbdy_ye, nbdy_ye,                           &
                           cbdy_txs, nbdy_txs,                       &
                           cbdy_txe, nbdy_txe,                       &
                           cbdy_tys, nbdy_tys,                       &
                           cbdy_tye, nbdy_tye,                       &
                           cdt, ndt                              &
                                        )

     RETURN

   END SUBROUTINE bdy_interp

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

     USE module_configure
     use module_state_description
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
#ifdef MM5_SINT
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca1(cims:cime,cjms:cjme,nri*nrj)
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
#endif
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
       ioff = (nri-1)/2
     ENDIF
     IF ( ystag ) THEN
       joff = (nrj-1)/2
     ENDIF

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

#ifdef MM5_SINT
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
                 bdy_xs( nj,k,ni ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

        ! SOUTH
               IF   ( nj .ge. njds .and. nj .lt. njds + sz ) THEN
                 bdy_tys( ni,k,nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_ys( ni,k,nj ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

        ! EAST
               IF ( xstag ) THEN
                 IF   ( ni .ge. nide - sz + 1 .AND. ni .le. nide ) THEN
                   bdy_txe( nj,k,nide-ni+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni+1 ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( ni .ge. nide - sz .AND. ni .le. nide-1 ) THEN
                   bdy_txe( nj,k,nide-ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

        ! NORTH
               IF ( ystag ) THEN
                 IF   ( nj .ge. njde - sz + 1 .AND. nj .le. njde  ) THEN
                   bdy_tye( ni,k,njde-nj+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj+1 ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   (  nj .ge. njde - sz .AND. nj .le. njde-1 ) THEN
                   bdy_tye(ni,k,njde-nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO
#endif

     RETURN

   END SUBROUTINE bdy_interp1



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
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER*120 message
   
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
                     ! noop
!                    nfld(ni,nk,nj) =  1.e20
                     nfld(ni,nk,nj) =  -1

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
!                    nfld(ni,nk,nj) = -1.e20
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

         sum = 0
         icount = 0 
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( ( nfld(ni,nk,nj) .GT. -1.e19 ) .AND. (  nfld(ni,nk,nj) .LT. 1.e19 ) ) THEN
                     icount = icount + 1
                     sum = sum + nfld(ni,nk,nj)
                  END IF
               END DO
            END DO
         END DO
       ELSE
         sum = 0.
         icount = 0
       ENDIF
       CALL wrf_dm_bcast_real( sum, 1 )
       CALL wrf_dm_bcast_integer( icount, 1 )
       IF ( enable ) THEN
         IF ( icount .GT. 0 ) THEN
           avg = sum / REAL ( icount ) 

         !  OK, if there were any of those island situations, we try to search a bit broader
         !  of an area in the coarse grid.

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
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
!                         CALL wrf_error_fatal ( "horizontal interp error - island" )
                          write(message,*) 'horizontal interp error - island, using average ', avg
                          CALL wrf_message ( message )
                          nfld(ni,nk,nj) = avg
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
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error

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
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
   
      !  Find out what the water value is.
   
      CALL nl_get_iswater(1,iswater)

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

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) ) THEN
                     ! noop
!                    nfld(ni,nk,nj) =  1.e20
                     nfld(ni,nk,nj) = -1

                  !  If this is a nested water point, and the surrounding coarse values are all water points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested water point and there are NO coarse water values surrounding,
                  !  we temporarily punt.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
!                    nfld(ni,nk,nj) = -1.e20
                     nfld(ni,nk,nj) = -1

                  !  If there are some land points and some water points, take an average. 
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) THEN
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
               END DO
            END DO
         END DO

         !  Get an average of the whole domain for problem locations.

         sum = 0
         icount = 0 
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( ( nfld(ni,nk,nj) .GT. -1.e19 ) .AND. (  nfld(ni,nk,nj) .LT. 1.e19 ) ) THEN
                     icount = icount + 1
                     sum = sum + nfld(ni,nk,nj)
                  END IF
               END DO
            END DO
         END DO
       ELSE
         sum = 0.
         icount = 0
       ENDIF
       CALL wrf_dm_bcast_real( sum, 1 )
       CALL wrf_dm_bcast_integer( icount, 1 )
       IF ( enable ) THEN
         IF ( icount .NE. 0 ) THEN
           avg = sum / REAL ( icount ) 


           !  OK, if there were any of those lake situations, we try to search a bit broader
           !  of an area in the coarse grid.

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
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
                             IF ( NINT(clu(ii,jj)) .EQ. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
  !                       CALL wrf_error_fatal ( "horizontal interp error - lake" )
                          print *,'horizontal interp error - lake, using average ',avg
                          nfld(ni,nk,nj) = avg
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

#if ( NMM_CORE == 1 )
!=======================================================================================
!  E grid interpolation for mass with addition of terrain adjustments. First routine
!  pertains to initial conditions and the next one corresponds to boundary conditions 
!  This is gopal's doing
!=======================================================================================

 SUBROUTINE interp_mass_nmm (cfld,                                 &  ! CD field
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
                             CBWGT4, HBWGT4,                       &  ! dummys for weights
                             CZ3d, Z3d,                            &  ! Z3d interpolated from CZ3d
                             CFIS,FIS,                             &  ! CFIS dummy on fine domain
                             CSM,SM,                               &  ! CSM is dummy
                             CPDTOP,PDTOP,                         &
                             CPTOP,PTOP,                           &
                             CPSTD,PSTD,                           &
                             CKZMAX,KZMAX                          )

   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN) :: xstag, ystag
   INTEGER,INTENT(IN) :: ckzmax,kzmax
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj

   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK

!  parent domain

   INTEGER,DIMENSION(cims:cime,cjms:cjme),          INTENT(IN)           :: CII,CJJ     ! dummy
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT1,CBWGT2,CBWGT3
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT4,CFIS,CSM
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,1:KZMAX),     INTENT(IN)           :: CZ3d
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: CPSTD
   REAL,INTENT(IN)                                                       :: CPDTOP,CPTOP

!  nested domain

   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT1,HBWGT2,HBWGT3
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: FIS,SM
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)        :: NFLD
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme,1:KZMAX),     INTENT(OUT)          :: Z3d
   REAL,INTENT(IN)                                                       :: PDTOP,PTOP

!  local

   INTEGER,PARAMETER                                          :: JTB=134
   REAL, PARAMETER                                            :: LAPSR=6.5E-3,GI=1./G, D608=0.608
   REAL, PARAMETER                                            :: COEF3=R_D*GI*LAPSR
   INTEGER                                                    :: I,J,K,IDUM
   REAL                                                       :: dlnpdz,tvout,pmo
   REAL,DIMENSION(nims:nime,njms:njme)                        :: ZS,DUM2d
   REAL,DIMENSION(JTB)                                        :: PIN,ZIN,Y2,PIO,ZOUT,DUM1,DUM2
!-----------------------------------------------------------------------------------------------------
!
!*** CHECK DOMAIN BOUNDS BEFORE INTERPOLATION
!
     DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal ('mass points:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal ('mass points:check domain bounds along y' )
     ENDDO
    ENDDO

    IF(KZMAX .GT. (JTB-10)) &
        CALL wrf_error_fatal ('mass points: increase JTB in interp_mass_nmm')

!    WRITE(21,*)'------------- MED NEST INITIAL 1 ----------------'
!    DO J=NJTS,MIN(NJTE,NJDE-1)
!      DO I=NITS,MIN(NITE,NIDE-1)
!         WRITE(21,*)I,J,IMASK(I,J),NFLD(I,1,J)
!      ENDDO
!    ENDDO
!    WRITE(21,*)

!
!*** DEFINE LOCAL TOPOGRAPHY ON THE BASIS OF FIS. ALSO CHECK IF SM IS LAND (SM=0) OVER TOPO
!*** YOU DON'T WANT MOUNTAINS INSIDE WATER BODIES! 
!

    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
         ZS(I,J)=FIS(I,J)/G
      ENDDO
    ENDDO

!
!*** Interpolate GPMs DERIVED FROM STANDARD ATMOSPHERIC LAPSE RATE FROM THE PARENT TO
!*** THE NESTED DOMAIN
!
!*** INDEX CONVENTIONS
!***                     HBWGT4
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            HBWGT1             HBWGT2
!***
!***
!***                      3
!***                     HBWGT3

    Z3d=0.0
    DO K=NKTS,KZMAX                ! Please note that we are still in isobaric surfaces 
      DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
!
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
               Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)

           ENDIF
!
        ENDDO
      ENDDO
    ENDDO

!  RECONSTRUCT PDs ON THE BASIS OF TOPOGRAPHY AND THE INTERPOLATED HEIGHTS

    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
!
          IF (ZS(I,J) .LT. Z3d(I,J,1)) THEN
            dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(i,j,1)-Z3d(i,j,2))
            dum2d(i,j) = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(i,j,1)))
            dum2d(i,j) = dum2d(i,j) - PDTOP -PTOP
          ELSE                                           ! target level bounded by input levels
            DO K =NKTS,KZMAX-1                           ! still in the isobaric surfaces
             IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
               dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
               dum2d(i,j) = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
               dum2d(i,j) = dum2d(i,j) - PDTOP -PTOP
             ENDIF
            ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',KZMAX,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal3 ( "interp_fcn.b" , 176 , "MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF
!       
      ENDDO
    ENDDO

    DO K=NKDS,NKDE                       ! NKTE is 1, nevertheless let us pretend religious 
      DO J=NJTS,MIN(NJTE,NJDE-1)
       DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           NFLD(I,J,K)= dum2d(i,j)         ! PD defined in the nested domain
         ENDIF
       ENDDO
      ENDDO
    ENDDO

!
  END SUBROUTINE interp_mass_nmm
!
!--------------------------------------------------------------------------------------

 SUBROUTINE nmm_bdymass_hinterp ( cfld,                              &  ! CD field
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
                               CTEMP_B,NTEMP_B,                      &  ! These temp arrays should be removed
                               CTEMP_BT,NTEMP_BT,                    &  ! later on
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  ! south-western grid locs and weights
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  ! note that "C"ourse grid ones are
                               CBWGT4, HBWGT4,                       &  ! dummys
                               CZ3d, Z3d,                            &  ! Z3d dummy on nested domain
                               CFIS,FIS,                             &  ! CFIS dummy on fine domain
                               CSM,SM,                               &  ! CSM is dummy
                               CPDTOP,PDTOP,                         &
                               CPTOP,PTOP,                           &
                               CPSTD,PSTD,                           &
                               CKZMAX,KZMAX                          )


     USE MODULE_MODEL_CONSTANTS
     USE module_configure
     USE module_wrf_error

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: ckzmax,kzmax
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj


   REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: ctemp_b,ctemp_bt
   REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(OUT) :: ntemp_b,ntemp_bt
   LOGICAL, INTENT(IN) :: xstag, ystag
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye

!  parent domain

   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
   INTEGER,DIMENSION(cims:cime,cjms:cjme),          INTENT(IN)           :: CII,CJJ     ! dummy
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT1,CBWGT2,CBWGT3
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT4,CFIS,CSM
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,1:KZMAX),     INTENT(IN)           :: CZ3d
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: CPSTD
   REAL,INTENT(IN)                                                       :: CPDTOP,CPTOP

!  nested domain

   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT1,HBWGT2,HBWGT3
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: FIS,SM
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)        :: NFLD
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme,1:KZMAX),     INTENT(OUT)          :: Z3d
   REAL,INTENT(IN)                                                       :: PDTOP,PTOP

! Local

     INTEGER                                     :: nijds, nijde, spec_bdy_width,i,j,k
     REAL                                        :: dlnpdz,dum2d
     REAL,DIMENSION(nims:nime,njms:njme)         :: zs

  INTEGER,PARAMETER                                                :: JTB=134
  INTEGER                                                          :: ii,jj
  REAL, DIMENSION (nims:nime,njms:njme)                            :: CWK1,CWK2,CWK3,CWK4

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

!
!*** DEFINE LOCAL TOPOGRAPHY ON THE BASIS OF FIS. ASLO CHECK IF SM IS LAND (SM=0) OVER TOPO
!*** YOU DON'T WANT MOUNTAINS INSIDE WATER BODIES!
!
    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
         ZS(I,J)=FIS(I,J)/G
      ENDDO
    ENDDO

!    X start boundary

       NMM_XS: IF(NITS .EQ. NIDS)THEN
!      WRITE(0,*)'ENTERING X1 START BOUNDARY AT MASS POINTS',NJTS,MIN(NJTE,NJDE-1)
        I = NIDS

        DO K=NKTS,KZMAX
          DO J = NJTS,MIN(NJTE,NJDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
              Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                         + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                         + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                         + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
              Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                         + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                         + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                         + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
          END DO
        END DO

        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(J,2) .NE. 0)THEN
            IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              ! level 2 has to be changed
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK1(I,J)  = dum2d -PDTOP -PTOP
            ELSE ! target level bounded by input levels
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK1(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
            ENDIF
            IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
               WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
               CALL wrf_error_fatal("BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
            ENDIF
          ELSE
           CWK1(I,J)=0.
          ENDIF
        ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
         DO K = NKDS,NKDE
           ntemp_b(i,j,k)     = CWK1(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS

!    X end boundary

       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN
!       WRITE(0,*)'ENTERING X END BOUNDARY AT MASS POINTS',NJTS,MIN(NJTE,NJDE-1)
       I = NIDE-1
       II = NIDE - I

       DO K=NKTS,KZMAX
         DO J=NJTS,MIN(NJTE,NJDE-1)
             IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
             ELSE
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
             ENDIF
         ENDDO
       ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(J,2) .NE.0)THEN                ! 1,3,5,7 of nested domain
            IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              ! level 2 has to be changed
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK2(I,J)  = dum2d -PDTOP -PTOP
            ELSE ! target level bounded by input levels
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK2(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
            ENDIF
            IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
               WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
               CALL wrf_error_fatal("BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
            ENDIF
          ELSE
              CWK2(I,J) = 0.0
          ENDIF
        ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
         DO K = NKDS,NKDE
           ntemp_b(i,j,k)     = CWK2(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE

!  Y start boundary

       NMM_YS: IF(NJTS .EQ. NJDS)THEN
!       WRITE(20,*)'ENTERING Y START BOUNDARY AT MASS POINTS',NITS,MIN(NITE,NIDE-1)
        J = NJDS
        DO K=NKTS,KZMAX
         DO I = NITS,MIN(NITE,NIDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
                Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
         END DO
        END DO

        DO I = NITS,MIN(NITE,NIDE-1)
          IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              ! level 2 has to be changed
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK3(I,J)  = dum2d -PDTOP -PTOP
          ELSE ! target level bounded by input levels
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK3(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal("BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF
        ENDDO

        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
           ntemp_b(i,j,k)     = CWK3(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       END IF NMM_YS

! Y end boundary

       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN
!        WRITE(20,*)'ENTERING Y END BOUNDARY AT MASS POINTS',NITS,MIN(NITE,NIDE-1)
        J = NJDE-1
        JJ = NJDE - J
        DO K=NKTS,KZMAX
         DO I = NITS,MIN(NITE,NIDE-1)
             IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
             ELSE
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
             ENDIF
         END DO
        END DO

        DO I = NITS,MIN(NITE,NIDE-1)
          IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              ! level 2 has to be changed
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK4(I,J)  = dum2d -PDTOP -PTOP
          ELSE ! target level bounded by input levels
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK4(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal("BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF
        ENDDO

        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              ntemp_b(i,j,k)     = CWK4(I,J)
              ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdymass_hinterp
!
!=======================================================================================
!
!  ADDED FOR INCLUDING MOISTURE AND THERMODYNAMIC ENERGY BALANCE
!
!=======================================================================================

 SUBROUTINE interp_scalar_nmm (cfld,                               &  ! CD field
                               cids,cide,ckds,ckde,cjds,cjde,      &
                               cims,cime,ckms,ckme,cjms,cjme,      &
                               cits,cite,ckts,ckte,cjts,cjte,      &
                               nfld,                               &  ! ND field
                               nids,nide,nkds,nkde,njds,njde,      &
                               nims,nime,nkms,nkme,njms,njme,      &
                               nits,nite,nkts,nkte,njts,njte,      &
                               shw,                                &  ! stencil half width for interp
                               imask,                              &  ! interpolation mask
                               xstag,ystag,                        &  ! staggering of field
                               ipos,jpos,                          &  ! Position of lower left of nest in CD
                               nri,nrj,                            &  ! nest ratios
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1, &  ! south-western grid locs and weights
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,     &  ! note that "C"ourse grid ones are
                               CBWGT4, HBWGT4,                     &  ! dummys for weights
                               CC3d,C3d,                           &
                               CPD,PD,                             &
                               CPSTD,PSTD,                         &
                               CPDTOP,PDTOP,                       &
                               CPTOP,PTOP,                         &
                               CETA1,ETA1,CETA2,ETA2               )

   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN) :: xstag, ystag
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj

   INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(IN)      :: IMASK

!  parent domain

   INTEGER,DIMENSION(cims:cime,cjms:cjme),        INTENT(IN) :: CII,CJJ   ! dummy
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT1,CBWGT2
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT3,CBWGT4

   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CC3d  ! scalar input on constant pressure levels
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CPSTD
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CPD
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CETA1,CETA2
   REAL,                                          INTENT(IN) :: CPDTOP,CPTOP

!  nested domain

   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT1,HBWGT2
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT3,HBWGT4

   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: NFLD  ! This is scalar on hybrid levels
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: C3d   ! Scalar on constant pressure levels
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: PD
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: ETA1,ETA2
   REAL,INTENT(IN)                                           :: PDTOP,PTOP

!  local

   INTEGER,PARAMETER                                         :: JTB=134
   INTEGER                                                   :: I,J,K
   REAL,DIMENSION(JTB)                                       :: PIN,CIN,Y2,PIO,PTMP,COUT,DUM1,DUM2

!-----------------------------------------------------------------------------------------------------
!
!
!   *** CHECK VERTICAL BOUNDS BEFORE USING SPLINE OR LINEAR INTERPOLATION
!
    IF(nkme .GT. (JTB-10) .OR. NKDE .GT. (JTB-10)) &
      CALL wrf_error_fatal ('mass points: increase JTB in interp_mass_nmm')

!
!   FIRST, HORIZONTALLY INTERPOLATE MOISTURE NOW AVAILABLE ON CONSTANT PRESSURE SURFACE (LEVELS) FROM THE
!   PARENT TO THE NESTED DOMAIN
!
!*** INDEX CONVENTIONS
!***                     HBWGT4
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            HBWGT1             HBWGT2
!***
!***
!***                      3
!***                     HBWGT3

    C3d=0.0
    DO K=NKDS,NKDE-1                ! Please note that we are still in isobaric surfaces
      DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
               C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)

           ELSE
               C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

           ENDIF
         ENDIF
        ENDDO
      ENDDO
    ENDDO

!
!   RECOVER THE SCALARS FROM CONSTANT PRESSURE SURFACES (LEVELS) ON TO HYBRID SURFACES
!
    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
      IF(IMASK(I,J) .NE. 1)THEN

!        clean local array before use of spline or linear interpolation

         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0.

         DO K=NKDS+1,NKDE                    ! inputs at standard  levels
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO

         Y2(1   )=0.
         Y2(NKDE-1)=0.

         DO K=NKDS,NKDE                         ! target points in model interface levels (pint)
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO

         DO K=NKDS,NKDE-1                        ! target points in model levels
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO

         IF(PTMP(1) .GE. PSTD(1))THEN           ! if lower boundary is higher than PMSL(1) re-set lower boundary
           PIN(NKDE-1) = PIO(1)                 ! be consistent with target. This may not happen at all
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  ! interpolate

         DO K=1,NKDE-1
           NFLD(I,J,K)= COUT(K)  ! scalar in the nested domain
         ENDDO

      ENDIF
     ENDDO
    ENDDO

 END SUBROUTINE interp_scalar_nmm
!
!===========================================================================================
!
 SUBROUTINE  nmm_bdy_scalar (cfld,                               &  ! CD field
                             cids,cide,ckds,ckde,cjds,cjde,      &
                             cims,cime,ckms,ckme,cjms,cjme,      &
                             cits,cite,ckts,ckte,cjts,cjte,      &
                             nfld,                               &  ! ND field
                             nids,nide,nkds,nkde,njds,njde,      &
                             nims,nime,nkms,nkme,njms,njme,      &
                             nits,nite,nkts,nkte,njts,njte,      &
                             shw,                                &  ! stencil half width for interp
                             imask,                              &  ! interpolation mask
                             xstag,ystag,                        &  ! staggering of field
                             ipos,jpos,                          &  ! Position of lower left of nest in CD
                             nri,nrj,                            &  ! nest ratios
                               c_bxs,n_bxs,                          &
                               c_bxe,n_bxe,                          &
                               c_bys,n_bys,                          &
                               c_bye,n_bye,                          &
                               c_btxs,n_btxs,                        &
                               c_btxe,n_btxe,                        &
                               c_btys,n_btys,                        &
                               c_btye,n_btye,                        &
                             cdt, ndt,                           &
                             CTEMP_B,NTEMP_B,                    &  ! to be removed
                             CTEMP_BT,NTEMP_BT,                  &
                             CII, IIH, CJJ, JJH, CBWGT1, HBWGT1, &  ! south-western grid locs and weights
                             CBWGT2, HBWGT2, CBWGT3, HBWGT3,     &  ! note that "C"ourse grid ones are
                             CBWGT4, HBWGT4,                     &  ! dummys for weights
                             CC3d,C3d,                           &
                             CPD,PD,                             &
                             CPSTD,PSTD,                         &
                             CPDTOP,PDTOP,                       &
                             CPTOP,PTOP,                         &
                             CETA1,ETA1,CETA2,ETA2               )
   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN)                                               :: xstag, ystag
   REAL, INTENT(INOUT)                                              :: cdt, ndt
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj
   REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: ctemp_b,ctemp_bt
   REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(OUT) :: ntemp_b,ntemp_bt
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye


   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IMASK

!  parent domain

   INTEGER,DIMENSION(cims:cime,cjms:cjme),        INTENT(IN) :: CII,CJJ   ! dummy
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT1,CBWGT2
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT3,CBWGT4
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CC3d ! scalar input on constant pressure levels
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CPSTD
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CPD
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CETA1,CETA2
   REAL,                                          INTENT(IN) :: CPDTOP,CPTOP

!  nested domain

   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT1,HBWGT2
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT3,HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: NFLD 
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: C3d   !Scalar on constant pressure levels
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: PD
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: ETA1,ETA2
   REAL,INTENT(IN)                                           :: PDTOP,PTOP

!  local

   INTEGER,PARAMETER                                       :: JTB=134
   INTEGER                                                 :: I,J,K,II,JJ
   REAL,DIMENSION(JTB)                                     :: PIN,CIN,Y2,PIO,PTMP,COUT,DUM1,DUM2
   REAL, DIMENSION (nims:nime,njms:njme,nkms:nkme)         :: CWK1,CWK2,CWK3,CWK4
!-----------------------------------------------------------------------------------------------------
!
!
!   *** CHECK VERTICAL BOUNDS BEFORE USING SPLINE INTERPOLATION 
!
    IF(nkme .GT. (JTB-10) .OR. NKDE .GT. (JTB-10)) &
      CALL wrf_error_fatal ('mass points: increase JTB in interp_mass_nmm')

!   X start boundary

    NMM_XS: IF(NITS .EQ. NIDS)THEN
!     WRITE(0,*)'ENTERING X1 START BOUNDARY AT T POINTS',NJTS,MIN(NJTE,NJDE-1)
      I = NIDS
      DO K=NKDS,NKDE-1                ! Please note that we are still in isobaric surfaces
        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)
          ENDIF
        ENDDO
      ENDDO
!
      DO J=NJTS,MIN(NJTE,NJDE-1)
       IF(MOD(J,2) .NE. 0)THEN
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. ! clean up local array
         DO K=NKDS+1,NKDE                    ! inputs at standard  levels
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         ! target points in model interface levels (pint)
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        ! target points in model levels
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           ! if lower boundary is higher than PMSL(1) re-set lower boundary
           PIN(NKDE-1) = PIO(1)                 ! be consistent with target. This may not happen at all
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  ! interpolate

         DO K=1,NKDE-1
           CWK1(I,J,K)= COUT(K)  ! scalar in the nested domain
         ENDDO
       ELSE
         DO K=NKDS,NKDE-1
          CWK1(I,J,K)=0.0
         ENDDO
       ENDIF
      ENDDO

      DO J = NJTS,MIN(NJTE,NJDE-1)
       DO K = NKDS,NKDE-1
         ntemp_b(i,j,k)     = CWK1(I,J,K)
         ntemp_bt(i,j,k)    = 0.0
       END DO
      END DO

    ENDIF NMM_XS


!   X end boundary

    NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN
!    WRITE(0,*)'ENTERING X END BOUNDARY AT T POINTS',NJTS,MIN(NJTE,NJDE-1)
     I = NIDE-1
      DO K=NKDS,NKDE-1                ! Please note that we are still in isobaric surfaces
        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)
          ENDIF
        ENDDO
      ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      IF(MOD(J,2) .NE. 0)THEN
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. ! clean up local array
         DO K=NKDS+1,NKDE                    ! inputs at standard  levels
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         ! target points in model interface levels (pint)
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        ! target points in model levels
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           ! if lower boundary is higher than PMSL(1) re-set lower boundary
           PIN(NKDE-1) = PIO(1)                 ! be consistent with target. This may not happen at all
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  ! interpolate

         DO K=1,NKDE-1
           CWK2(I,J,K)= COUT(K)  ! scalar in the nested domain
         ENDDO
      ELSE
         DO K=NKDS,NKDE-1
           CWK2(I,J,K)=0.0
         ENDDO
      ENDIF
     ENDDO

       DO J = NJTS,MIN(NJTE,NJDE-1)
        DO K = NKDS,MIN(NKTE,NKDE-1)
          ntemp_b(i,j,k)     = CWK2(I,J,K)
          ntemp_bt(i,j,k)    = 0.0
        END DO
       END DO

    ENDIF NMM_XE

!  Y start boundary

    NMM_YS: IF(NJTS .EQ. NJDS)THEN
!    WRITE(0,*)'ENTERING Y START BOUNDARY AT T POINTS',NITS,MIN(NITE,NIDE-1)
     J = NJDS
      DO K=NKDS,NKDE-1
       DO I = NITS,MIN(NITE,NIDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

          ENDIF
        ENDDO
      ENDDO
!
     DO I=NITS,MIN(NITE,NIDE-1)
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. ! clean up local array
         DO K=NKDS+1,NKDE                    ! inputs at standard  levels
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         ! target points in model interface levels (pint)
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        ! target points in model levels
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           ! if lower boundary is higher than PMSL(1) re-set lower boundary
           PIN(NKDE-1) = PIO(1)                 ! be consistent with target. This may not happen at all
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  ! interpolate

         DO K=1,NKDE-1
           CWK3(I,J,K)= COUT(K)  ! scalar in the nested domain
         ENDDO
     ENDDO

     DO K = NKDS,NKDE-1
      DO I = NITS,MIN(NITE,NIDE-1)
        ntemp_b(i,J,K)     = CWK3(I,J,K)
        ntemp_bt(i,J,K)    = 0.0
      ENDDO
      ENDDO

    ENDIF NMM_YS

! Y end boundary

    NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN
!    WRITE(0,*)'ENTERING Y END BOUNDARY AT T POINTS',NITS,MIN(NITE,NIDE-1)
     J = NJDE-1
      DO K=NKDS,NKDE-1
        DO I = NITS,MIN(NITE,NIDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

          ENDIF
        ENDDO
      ENDDO

     DO I=NITS,MIN(NITE,NIDE-1)
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. ! clean up local array
         DO K=NKDS+1,NKDE                    ! inputs at standard  levels
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         ! target points in model interface levels (pint)
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        ! target points in model levels
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           ! if lower boundary is higher than PMSL(1) re-set lower boundary
           PIN(NKDE-1) = PIO(1)                 ! be consistent with target. This may not happen at all
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  ! interpolate

         DO K=1,NKDE-1
           CWK4(I,J,K)= COUT(K)  ! scalar in the nested domain
         ENDDO
     ENDDO

     DO K = NKDS,NKDE-1
      DO I = NITS,MIN(NITE,NIDE-1)
        ntemp_b(i,J,K)     = CWK4(I,J,K)
        ntemp_bt(i,J,K)    = 0.0
      END DO
      END DO

    ENDIF NMM_YE

  END SUBROUTINE nmm_bdy_scalar
!
!
!=======================================================================================
 SUBROUTINE SPLINE2(I,J,JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!
!   ******************************************************************
!   *                                                                *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE        *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                         *
!   *                                                                *
!   *  PROGRAMER Z. JANJIC                                           *
!   *                                                                *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3. *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.       *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.   *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE      *
!   *         SPECIFIED.                                             *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.     *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)   *
!   *         AND LE XOLD(NOLD).                                     *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.           *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                *
!   *                                                                *
!   ******************************************************************
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: I,J,JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW
!
      INTEGER :: II,JJ,K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                 &
             ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
!---------------------------------------------------------------------

!     debug

      II=9999
      JJ=9999
      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(0,*)'DEBUG in SPLINE2: I,J',I,J
        WRITE(0,*)'DEBUG in SPLINE2:HSO= ',xnew(1:nold)
        DO K=1,NOLD
         WRITE(0,*)'DEBUG in SPLINE2:L,ZETAI,PINTI= ' &
                        ,K,YOLD(K),XOLD(K)
        ENDDO
      ENDIF
!
      NOLDM1=NOLD-1
!
      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)
!
      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR
!
      IF(NOLD.EQ.3)GO TO 150
!---------------------------------------------------------------------
      K=3
!
  100 DXL=DXR
      DYDXL=DYDXR
      DXR=XOLD(K+1)-XOLD(K)
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR
      DXC=DXL+DXR
      DEN=1./(DXL*Q(K-2)+DXC+DXC)
!
      P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
      Q(K-1)=-DEN*DXR
!
      K=K+1
      IF(K.LT.NOLD)GO TO 100
!-----------------------------------------------------------------------
  150 K=NOLDM1
!
  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
!
      K=K-1
      IF(K.GT.1)GO TO 200
!-----------------------------------------------------------------------
      K1=1
!
  300 XK=XNEW(K1)
!
      DO 400 K2=2,NOLD
!
      IF(XOLD(K2).GT.XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF
!
  400 CONTINUE
!
      YNEW(K1)=YOLD(NOLD)
      GO TO 600
!
  450 IF(K1.EQ.1)GO TO 500
      IF(K.EQ.KOLD)GO TO 550
!
  500 K=KOLD
!
      Y2K=Y2(K)
      Y2KP1=Y2(K+1)
      DX=XOLD(K+1)-XOLD(K)
      RDX=1./DX
!
      AK=.1666667*RDX*(Y2KP1-Y2K)
      BK=0.5*Y2K
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
!
  550 X=XK-XOLD(K)
      XSQ=X*X
!
      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)

!  debug

      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(0,*) 'DEBUG:: k1,xnew(k1),ynew(k1): ', K1,XNEW(k1),YNEW(k1)
      ENDIF 

!
  600 K1=K1+1
      IF(K1.LE.NNEW)GO TO 300

      RETURN

      END SUBROUTINE SPLINE2

!=======================================================================================
!  E grid interpolation for H and V points 
!=======================================================================================

  SUBROUTINE interp_h_nmm (cfld,                                 &  ! CD field
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
                           CBWGT4, HBWGT4                        )  ! dummys for weights
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local
     INTEGER i,j,k
!
!*** CHECK DOMAIN BOUNDS BEFORE INTERPOLATION
!
    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal ('hpoints:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal ('hpoints:check domain bounds along y' )
     ENDDO
    ENDDO
!
!*** INDEX CONVENTIONS
!***                     HBWGT4
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            HBWGT1             HBWGT2
!***
!***
!***                      3
!***                     HBWGT3

     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
!
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7 
               NFLD(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               NFLD(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
           ENDIF
!     
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_h_nmm
!
  SUBROUTINE interp_v_nmm (cfld,                                 &  ! CD field
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
                           CII, IIV, CJJ, JJV, CBWGT1, VBWGT1,   &  ! south-western grid locs and weights
                           CBWGT2, VBWGT2, CBWGT3, VBWGT3,       &  ! note that "C"ourse grid ones are
                           CBWGT4, VBWGT4                        )  ! dummys
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIV,JJV
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local
     INTEGER i,j,k


!
!*** CHECK DOMAIN BOUNDS BEFORE INTERPOLATION
!
    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIV(i,j).LT.(CIDS-shw) .OR. IIV(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal ('vpoints:check domain bounds along x' )
       IF(JJV(i,j).LT.(CJDS-shw) .OR. JJV(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal ('vpoints:check domain bounds along y' )
     ENDDO
    ENDDO
!
!*** INDEX CONVENTIONS
!***                     VBWGT4
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            VBWGT1             VBWGT2
!***
!***
!***                      3
!***                     VBWGT3

     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN

            IF(MOD(JJV(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                NFLD(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                            + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                            + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                            + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
            ELSE
                NFLD(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                            + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                            + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                            + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
            ENDIF

         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_v_nmm
!
!=======================================================================================
!  E grid nearest neighbour interpolation for H points.
!  This routine assumes cfld and nfld are in IJK
!=======================================================================================
!
  SUBROUTINE interp_hnear_nmm (cfld,                                 &  ! CD field
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
                               CBWGT4, HBWGT4                        )  ! just dummys
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local

     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT

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
       IF(IMASK(I,J) .NE. 1)THEN
         NBWGT(1,I,J)=HBWGT1(I,J)
         NBWGT(2,I,J)=HBWGT2(I,J)
         NBWGT(3,I,J)=HBWGT3(I,J)
         NBWGT(4,I,J)=HBWGT4(I,J)
       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

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

       ENDIF
      ENDDO
     ENDDO

     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7 
                NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
                NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_hnear_nmm
!
!=======================================================================================
!  E grid nearest neighbour interpolation for H points.
!  This routine assumes cfld and nfld are in IKJ or ILJ
!=======================================================================================
!
  SUBROUTINE interp_hnear_ikj_nmm (cfld,                                 &  ! CD field
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
                               CBWGT4, HBWGT4                        )  ! just dummys
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

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local

     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT

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
       IF(IMASK(I,J) .NE. 1)THEN
         NBWGT(1,I,J)=HBWGT1(I,J)
         NBWGT(2,I,J)=HBWGT2(I,J)
         NBWGT(3,I,J)=HBWGT3(I,J)
         NBWGT(4,I,J)=HBWGT4(I,J)
       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

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

       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKDS,NKDE
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                NFLD(I,K,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)  ) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)  ) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)-1) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)+1)
            ELSE
                NFLD(I,K,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)  ) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)  ) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)-1) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)+1)
            ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_hnear_ikj_nmm
!
!=======================================================================================
!  E grid nearest neighbour interpolation for integer H points
!=======================================================================================
!
  SUBROUTINE interp_int_hnear_nmm (cfld,                                 &  ! CD field; integers
                                   cids, cide, ckds, ckde, cjds, cjde,   &
                                   cims, cime, ckms, ckme, cjms, cjme,   &
                                   cits, cite, ckts, ckte, cjts, cjte,   &
                                   nfld,                                 &  ! ND field; integers
                                   nids, nide, nkds, nkde, njds, njde,   &
                                   nims, nime, nkms, nkme, njms, njme,   &
                                   nits, nite, nkts, nkte, njts, njte,   &
                                   shw,                                  &  ! stencil half width for interp
                                   imask,                                &  ! interpolation mask
                                   xstag, ystag,                         &  ! staggering of field
                                   ipos, jpos,                           &  ! lower left of nest in CD
                                   nri, nrj,                             &  ! nest ratios                      
                                   CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  ! s-w grid locs and weights 
                                   CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  ! note that "C"ourse grid ones are
                                   CBWGT4, HBWGT4                        )  ! just dummys
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

     INTEGER, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local

     LOGICAL  FLIP 
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT

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
        IF(IMASK(I,J) .NE. 1)THEN
          NBWGT(1,I,J)=HBWGT1(I,J)
          NBWGT(2,I,J)=HBWGT2(I,J)
          NBWGT(3,I,J)=HBWGT3(I,J)
          NBWGT(4,I,J)=HBWGT4(I,J)
        ENDIF
       ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

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
! 
       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKTS,NKTS
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7 
               NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                           + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
           ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_int_hnear_nmm
!
!--------------------------------------------------------------------------------------
!
   SUBROUTINE nmm_bdy_hinterp (cfld,                                 &  ! CD field
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
                               CTEMP_B,NTEMP_B,                      &  ! These temp arrays should be removed
                               CTEMP_BT,NTEMP_BT,                    &  ! later on
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  ! south-western grid locs and weights
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  ! note that "C"ourse grid ones are
                               CBWGT4, HBWGT4                        )  ! dummys

!     use module_state_description
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
!
     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: ctemp_b,ctemp_bt
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: ntemp_b,ntemp_bt
!
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH

! Local

     INTEGER :: i,j,k
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme )    :: cwk1,cwk2,cwk3,cwk4

!    X start boundary

       NMM_XS: IF(NITS .EQ. NIDS)THEN
!        WRITE(0,*)'ENTERING X1 START BOUNDARY AT MASS POINTS',NJTS,MIN(NJTE,NJDE-1)
        I = NIDS
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .NE.0)THEN                ! 1,3,5,7 of nested domain
                IF(MOD(JJH(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
                   CWK1(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)


                ELSE
                   CWK1(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
                ENDIF
              ELSE
                CWK1(I,J,K) = 0.0      ! even rows at mass points of the nested domain
              ENDIF
              ntemp_b(i,J,K)     = CWK1(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS

!    X end boundary

       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN
!       WRITE(0,*)'ENTERING X END BOUNDARY AT MASS POINTS',NJTS,MIN(NJTE,NJDE-1)
        I = NIDE-1
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .NE.0)THEN                ! 1,3,5,7 of the nested domain 
                IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7 of the parent domain
                   CWK2(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
                ELSE
                   CWK2(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
                ENDIF
              ELSE
                CWK2(I,J,K) = 0.0      ! even rows at mass points
              ENDIF
              ntemp_b(i,J,K)     = CWK2(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE

!  Y start boundary

       NMM_YS: IF(NJTS .EQ. NJDS)THEN
!        WRITE(0,*)'ENTERING Y START BOUNDARY AT MASS POINTS',NITS,MIN(NITE,NIDE-1)
        J = NJDS
        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 CWK3(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
              ELSE
                 CWK3(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK3(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YS

! Y end boundary

       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN
!        WRITE(0,*)'ENTERING Y END BOUNDARY AT MASS POINTS',NITS,MIN(NITE,NIDE-1)
        J = NJDE-1
        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 CWK4(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
              ELSE
                 CWK4(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)

              ENDIF
              ntemp_b(i,J,K)     = CWK4(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdy_hinterp

!--------------------------------------------------------------------------------------

   SUBROUTINE nmm_bdy_vinterp ( cfld,                                 &  ! CD field
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
                               CTEMP_B,NTEMP_B,                      &  ! These temp arrays should be removed
                               CTEMP_BT,NTEMP_BT,                    &  ! later on
                               CII, IIV, CJJ, JJV, CBWGT1, VBWGT1,   &  ! south-western grid locs and weights
                               CBWGT2, VBWGT2, CBWGT3, VBWGT3,       &  ! note that "C"ourse grid ones are
                               CBWGT4, VBWGT4                        )  ! dummys

!     use module_state_description
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
!
     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: ctemp_b,ctemp_bt
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: ntemp_b,ntemp_bt
!
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIV,JJV

! Local

     INTEGER :: i,j,k
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme )    :: cwk1,cwk2,cwk3,cwk4

!    X start boundary

       NMM_XS: IF(NITS .EQ. NIDS)THEN
!      WRITE(0,*)'ENTERING X START BOUNDARY AT VELOCITY POINTS',NITS,NIDS,NJTS,MIN(NJTE,NJDE-1)
        I = NIDS
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .EQ.0)THEN                ! 1,3,5,7 of nested domain
                IF(MOD(JJV(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
                   CWK1(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
                ELSE
                   CWK1(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
                ENDIF
              ELSE
                CWK1(I,J,K) = 0.0 ! odd rows along J, at mass points have zero velocity  
              ENDIF
              ntemp_b(i,J,K)     = CWK1(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS

!    X end boundary

       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN
!        WRITE(0,*)'ENTERING X END BOUNDARY AT VELOCITY POINTS',NITE-1,NIDE-1,NJTS,MIN(NJTE,NJDE-1)
        I = NIDE-1
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .EQ.0)THEN                ! 1,3,5,7 of the nested domain
                IF(MOD(JJV(I,J),2) .NE. 0)THEN      ! 1,3,5,7 of the parent domain
                   CWK2(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
                ELSE
                   CWK2(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
                ENDIF
              ELSE
                CWK2(I,J,K) = 0.0      ! odd rows at mass points
              ENDIF
              ntemp_b(i,J,K)     = CWK2(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE

!  Y start boundary

       NMM_YS: IF(NJTS .EQ. NJDS)THEN
!        WRITE(0,*)'ENTERING Y START BOUNDARY AT VELOCITY POINTS',NJTS,NJDS,NITS,MIN(NITE,NIDE-1)
        J = NJDS
        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-2)     ! NIDE-1 SHOULD NOT MATTER IF WE FILL UP PHANTOM CELL 
              IF(MOD(JJV(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 CWK3(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
              ELSE
                 CWK3(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK3(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YS

! Y end boundary

       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN
!       WRITE(0,*)'ENTERING Y END BOUNDARY AT VELOCITY POINTS',NJTE-1,NJDE-1,NITS,MIN(NITE,NIDE-1)
        J = NJDE-1
        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-2)   ! NIDE-1 SHOULD NOT MATTER IF WE FILL UP PHANTOM CELL
              IF(MOD(JJV(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                 CWK4(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
              ELSE
                 CWK4(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK4(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdy_vinterp

!
!=======================================================================================
! E grid interpolation: simple copy from parent to mother domain
!=======================================================================================
!

   SUBROUTINE nmm_copy      ( cfld,                                 &  ! CD field
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
                              CII, IIH, CJJ, JJH                    )

     USE module_timing
     IMPLICIT NONE

     LOGICAL, INTENT(IN) :: xstag, ystag
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(IN)    :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH

!    local
     INTEGER i,j,k

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKTS,NKTE
        DO I=NITS,MIN(NITE,NIDE-1)
           NFLD(I,J,K) = CFLD(IIH(I,J),JJH(I,J),K)
        ENDDO
       ENDDO
     ENDDO

  RETURN

  END SUBROUTINE nmm_copy
!
!=======================================================================================
!  E grid test for mass point coincidence
!=======================================================================================
!
  SUBROUTINE test_nmm (cfld,                                 &  ! CD field
                       cids, cide, ckds, ckde, cjds, cjde,   &
                       cims, cime, ckms, ckme, cjms, cjme,   &
                       cits, cite, ckts, ckte, cjts, cjte,   &
                       nfld,                                 &  ! ND field
                       nids, nide, nkds, nkde, njds, njde,   &
                       nims, nime, nkms, nkme, njms, njme,   &
                       nits, nite, nkts, nkte, njts, njte,   &
                       shw,                                  & ! stencil half width for interp
                       imask,                                & ! interpolation mask
                       xstag, ystag,                         & ! staggering of field
                       ipos, jpos,                           & ! Position of lower left of nest in CD
                       nri, nrj,                             & ! nest ratios                        
                       CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   & ! south-western grid locs and weights 
                       CBWGT2, HBWGT2, CBWGT3, HBWGT3,       & ! note that "C"ourse grid ones are
                       CBWGT4, HBWGT4                        ) ! dummys for weights
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

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    ! dummy
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        ! dummy
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

!    local
     INTEGER i,j,k
     REAL,PARAMETER                                :: error=0.0001,error1=1.0
     REAL                                          :: diff
!
!*** CHECK DOMAIN BOUNDS BEFORE INTERPOLATION
!
    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal ('hpoints:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal ('hpoints:check domain bounds along y' )
     ENDDO
    ENDDO

!
!*** INDEX CONVENTIONS
!***                     HBWGT4
!***                      4
!***
!***
!***
!***                   h
!***             1                 2
!***            HBWGT1             HBWGT2
!***
!***
!***                      3
!***                     HBWGT3


!    WRITE(0,*)NITS,MIN(NITE,NIDE-1),CITS,CITE
     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKDS,NKDE
        DO I=NITS,MIN(NITE,NIDE-1)
          IF(ABS(1.0-HBWGT1(I,J)) .LE. ERROR)THEN
             DIFF=ABS(NFLD(I,J,K)-CFLD(IIH(I,J),JJH(I,J),K))
             IF(DIFF .GT. ERROR)THEN
              CALL wrf_debug(1,"dyn_nmm: NON-COINCIDENT, NESTED MASS POINT") 
              WRITE(0,*)I,IIH(I,J),J,JJH(I,J),HBWGT1(I,J),NFLD(I,J,K),CFLD(IIH(I,J),JJH(I,J),K),DIFF 
             ENDIF
             IF(DIFF .GT. ERROR1)THEN
              WRITE(0,*)I,IIH(I,J),J,JJH(I,J),HBWGT1(I,J),NFLD(I,J,K),CFLD(IIH(I,J),JJH(I,J),K),DIFF
              CALL wrf_error_fatal ('dyn_nmm: NON-COINCIDENT, NESTED MASS POINT')
             ENDIF
          ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE test_nmm

!==================================
! this is the default function used in nmm feedback at mass points.

   SUBROUTINE nmm_feedback ( cfld,                                 &  ! CD field
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
                           CII, IIH, CJJ, JJH,                   &
                           CBWGT1, HBWGT1, CBWGT2, HBWGT2,       &
                           CBWGT3, HBWGT3, CBWGT4, HBWGT4        )
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
     INTEGER,DIMENSION(cims:cime,cjms:cjme),  INTENT(IN)    :: CII,CJJ     ! dummy
     INTEGER,DIMENSION(nims:nime,njms:njme),  INTENT(IN)    :: IIH,JJH
     REAL,DIMENSION(cims:cime,cjms:cjme),     INTENT(IN)    :: CBWGT1,CBWGT2,CBWGT3,CBWGT4
     REAL,DIMENSION(nims:nime,njms:njme),     INTENT(IN)    :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     LOGICAL, INTENT(IN)                                    :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)           :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: is, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     REAL    :: AVGH

!=====================================================================================
!

   IF(nri .ne. 3 .OR. nrj .ne. 3)               &
    CALL wrf_error_fatal ('Feedback works for only 1:3 ratios, currently. Modify the namelist' )

!  WRITE(0,*)'SIMPLE FEED BACK IS SWITCHED ON FOR MASS'

   CFLD = 9999.0

   DO ck = ckts, ckte
     nk = ck
     DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
       nj = (cj-jpos)*nrj + 1
       if(mod(cj,2) .eq. 0)THEN
         is=0 ! even rows for mass points (2,4,6,8)
       else
         is=1 ! odd rows for mass points  (1,3,5,7)
       endif
       DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
         ni = (ci-ipos)*nri + 2 -is
         IF(IS==0)THEN    ! (2,4,6,8)
!           AVGH = NFLD(NI,NJ+1,NK)  + NFLD(NI,NJ-1,NK)  + NFLD(NI+1,NJ+1,NK)+ NFLD(NI+1,NJ-1,NK)  &
!                + NFLD(NI+1,NJ,NK)  + NFLD(NI-1,NJ,NK)  + NFLD(NI,NJ+2,NK)  + NFLD(NI,NJ-2,NK)    &
!                + NFLD(NI+1,NJ+2,NK)+ NFLD(NI-1,NJ+2,NK)+ NFLD(NI+1,NJ-2,NK)+ NFLD(NI-1,NJ-2,NK)

          AVGH =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI  ,NJ+1,NK)              + NFLD(NI+1,NJ+1,NK)                   &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI  ,NJ-1,NK)              + NFLD(NI+1,NJ-1,NK)                   &
               +                          NFLD(NI,NJ-2,NK)

         ELSE
!           AVGH = NFLD(NI,NJ+1,NK)  + NFLD(NI,NJ-1,NK)  + NFLD(NI-1,NJ+1,NK)+ NFLD(NI-1,NJ-1,NK)  &
!                + NFLD(NI+1,NJ,NK)  + NFLD(NI-1,NJ,NK)  + NFLD(NI,NJ+2,NK)  + NFLD(NI,NJ-2,NK)    &
!                + NFLD(NI+1,NJ+2,NK)+ NFLD(NI-1,NJ+2,NK)+ NFLD(NI+1,NJ-2,NK)+ NFLD(NI-1,NJ-2,NK)

          AVGH =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI-1,NJ+1,NK)              + NFLD(NI,NJ+1,NK)                     &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI-1,NJ-1,NK)              + NFLD(NI,NJ-1,NK)                     &
               +                          NFLD(NI,NJ-2,NK)

         ENDIF
!dusan         CFLD(CI,CK,CJ) = 0.5*CFLD(CI,CK,CJ) + 0.5*(NFLD(NI,NK,NJ)+AVGH)/13.0
!         CFLD(CI,CJ,CK) = (NFLD(NI,NJ,NK)+AVGH)/13.0
       CFLD(CI,CJ,CK) = AVGH/9.0
       ENDDO
     ENDDO
   ENDDO

   END SUBROUTINE nmm_feedback

!===========================================================================================

   SUBROUTINE nmm_vfeedback ( cfld,                              &  ! CD field
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
                           CII, IIV, CJJ, JJV,                   &
                           CBWGT1, VBWGT1, CBWGT2, VBWGT2,       &
                           CBWGT3, VBWGT3, CBWGT4, VBWGT4        )
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
     INTEGER,DIMENSION(cims:cime,cjms:cjme),  INTENT(IN)    :: CII,CJJ     ! dummy
     INTEGER,DIMENSION(nims:nime,njms:njme),  INTENT(IN)    :: IIV,JJV
     REAL,DIMENSION(cims:cime,cjms:cjme),     INTENT(IN)    :: CBWGT1,CBWGT2,CBWGT3,CBWGT4
     REAL,DIMENSION(nims:nime,njms:njme),     INTENT(IN)    :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     LOGICAL, INTENT(IN)                                    :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)           :: imask

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: is, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     REAL :: AVGV

!=====================================================================================
!

    IF(nri .ne. 3 .OR. nrj .ne. 3)               &
      CALL wrf_error_fatal ('Feedback works for only 1:3 ratios, currently. Modify the namelist')

!   WRITE(0,*)'SIMPLE FEED BACK IS SWITCHED ON FOR VELOCITY'

   CFLD = 9999.0

   DO ck = ckts, ckte
    nk = ck
    DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  ! exclude top and bottom BCs
     nj = (cj-jpos)*nrj + 1
     if(mod(cj,2) .eq. 0)THEN
      is=1 ! even rows for velocity points (2,4,6,8) 
     else
      is=0 ! odd rows for velocity points (1,3,5,7) 
     endif
     DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) ! excludes LBCs
       ni = (ci-ipos)*nri + 2 -is
         IF(IS==0)THEN    ! (1,3,5,7)
!          AVGV = NFLD(NI,NK,NJ+1)  + NFLD(NI,NK,NJ-1)  + NFLD(NI+1,NK,NJ+1)+ NFLD(NI+1,NK,NJ-1)  &
!               + NFLD(NI+1,NK,NJ)  + NFLD(NI-1,NK,NJ)  + NFLD(NI,NK,NJ+2)  + NFLD(NI,NK,NJ-2)    &
!               + NFLD(NI+1,NK,NJ+2)+ NFLD(NI-1,NK,NJ+2)+ NFLD(NI+1,NK,NJ-2)+ NFLD(NI-1,NK,NJ-2)

          AVGV =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI  ,NJ+1,NK)              + NFLD(NI+1,NJ+1,NK)                   &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI  ,NJ-1,NK)              + NFLD(NI+1,NJ-1,NK)                   &
               +                          NFLD(NI,NJ-2,NK)

         ELSE
!          AVGV = NFLD(NI,NK,NJ+1)  + NFLD(NI,NK,NJ-1)  + NFLD(NI-1,NK,NJ+1)+ NFLD(NI-1,NK,NJ-1)  &
!               + NFLD(NI+1,NK,NJ)  + NFLD(NI-1,NK,NJ)  + NFLD(NI,NK,NJ+2)  + NFLD(NI,NK,NJ-2)    &
!               + NFLD(NI+1,NK,NJ+2)+ NFLD(NI-1,NK,NJ+2)+ NFLD(NI+1,NK,NJ-2)+ NFLD(NI-1,NK,NJ-2)

          AVGV =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI-1,NJ+1,NK)              + NFLD(NI,NJ+1,NK)                     &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI-1,NJ-1,NK)              + NFLD(NI,NJ-1,NK)                     &
               +                          NFLD(NI,NJ-2,NK)

         ENDIF
!dusan         CFLD(CI,CK,CJ) = 0.5*CFLD(CI,CK,CJ) + 0.5*(NFLD(NI,NK,NJ)+AVGV)/13.0
!         CFLD(CI,CK,CJ) = (NFLD(NI,NK,NJ)+AVGV)/13.0
       CFLD(CI,CJ,CK) = AVGV/9.0
     ENDDO
    ENDDO
   ENDDO

   END SUBROUTINE nmm_vfeedback 


   SUBROUTINE nmm_smoother ( cfld , &
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

      RETURN
      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(0,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'

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

   END SUBROUTINE nmm_smoother


   SUBROUTINE nmm_vsmoother ( cfld , &
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

      RETURN
      !  If there is no feedback, there can be no smoothing.

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(0,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR VELOCITY'

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

   END SUBROUTINE nmm_vsmoother
!======================================================================================
!   End of gopal's doing
!======================================================================================
#endif
