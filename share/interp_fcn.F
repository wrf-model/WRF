!WRF:MEDIATION_LAYER:INTERPOLATIONFUNCTION
!

#define MM5_SINT
!#define DUMBCOPY

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
#endif

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,'("cids:cide, ckds:ckde, cjds:cjde ",6i4)')cids,cide, ckds,ckde, cjds,cjde
!write(0,'("cims:cime, ckms:ckme, cjms:cjme ",6i4)')cims,cime, ckms,ckme, cjms,cjme
!write(0,'("cits:cite, ckts:ckte, cjts:cjte ",6i4)')cits,cite, ckts,ckte, cjts,cjte
!write(0,'("nims:nime, nkms:nkme, njms:njme ",6i4)')nims,nime, nkms,nkme, njms,njme
!write(0,'("nits:nite, nkts:nkte, njts:njte ",6i4)')nits,nite, nkts,nkte, njts,njte

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

     nfx = nri * nrj
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ci,cj,ip,jp,nk,ck,nf,icmask,psca )
     DO k = ckts, ckte
        icmask = .FALSE.
        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + 2   ! j point on nest
              DO i = cims,cime
                ni = (i-ipos) * nri + 2   ! i point on nest
                if ( ni .ge. nits-nioff-1 .and. ni .le. nite+nioff+1 .and. nj .ge. njts-njoff-1 .and. nj .le. njte+njoff+1 ) then
                  if ( imask(ni,nj) .eq. 1 ) then
                    icmask( i, j ) = .TRUE.
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
               ci = ipos + (ni-1) / nri      ! j coord of CD point 
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

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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
           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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
           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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
           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + jstag
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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

           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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
           DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio, pick nearest neighbor on SW corner
        DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  ! odd refinement ratio

        DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  ! even refinement ratio
        DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
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
                           cbdy, nbdy,                           &
                           cbdy_t, nbdy_t,                       &
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
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy, cbdy_t, nbdy, nbdy_t
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
                           cbdy, nbdy,                           &
                           cbdy_t, nbdy_t,                       &
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
                           cbdy, bdy,                            &
                           cbdy_t, bdy_t,                        &
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
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy, cbdy_t   ! not used
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( nijds:nijde, nkms:nkme, spec_bdy_width, 4 ), INTENT(INOUT) :: bdy, bdy_t

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

     ioff = 0 ; joff = 0
     IF ( xstag ) ioff = (nri-1)/2
     IF ( ystag ) joff = (nrj-1)/2

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

#ifdef MM5_SINT
     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )
     sz = MIN(MAX( spec_zone, relax_zone ),spec_bdy_width)

     nfx = nri * nrj

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ni1,nj1,ci,cj,ip,jp,nk,ck,nf,icmask,psca,psca1 )
     DO k = ckts, ckte

        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + 2   ! j point on nest
              DO i = cims,cime
                ni = (i-ipos) * nri + 2   ! i point on nest
                psca1(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO
! hopefully less ham handed but still correct and more efficient
! sintb ignores icmask so it does not matter that icmask is not set
!
! SOUTH BDY
               IF   ( njts .ge. njds .and. njts .le. njds + sz - 1  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njds)), n2cj(MIN(njte,njds+sz-1)), nrj*nri, xstag, ystag )
               ENDIF
! NORTH BDY
               IF ( ystag ) THEN
                 IF   ( njte .le. njde .and. njte .ge. njde - sz + 1 ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njde-sz+1)), n2cj(MIN(njte,njde)), nrj*nri, xstag, ystag )
                 ENDIF
               ELSE
                 IF   ( njte .le. njde .and. njte .ge. njde - sz ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njde-sz)), n2cj(MIN(njte,njde-1)), nrj*nri, xstag, ystag )
                 ENDIF
               ENDIF
! WEST BDY
               IF   ( nits .ge. nids .and. nits .le. nids + sz - 1 ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nids)), n2ci(MIN(nite,nids+sz-1)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF
! EAST BDY
               IF ( xstag ) THEN
                 IF   ( nite .le. nide .and. nite .ge. nide - sz + 1 ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nide-sz+1)), n2ci(MIN(nite,nide)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
                 ENDIF
               ELSE
                 IF   ( nite .le. nide .and. nite .ge. nide - sz ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nide-sz)), n2ci(MIN(nite,nide-1)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
                 ENDIF
               ENDIF

        DO nj1 = njts, njte+joff
           cj = jpos + (nj1-1) / nrj     ! j coord of CD point 
           jp = mod ( nj1-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni1 = nits, nite+ioff
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

               IF   ( ni .ge. nids .and. ni .lt. nids + sz ) THEN
                 bdy_t( nj,k,ni, P_XSB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy( nj,k,ni, P_XSB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

               IF   ( nj .ge. njds .and. nj .lt. njds + sz ) THEN
                 bdy_t( ni,k,nj, P_YSB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy( ni,k,nj, P_YSB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

               IF ( xstag ) THEN
                 IF   ( ni .le. nide .and. ni .ge. nide - sz + 1 ) THEN
                   bdy_t( nj,k,nide-ni+1, P_XEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( nj,k,nide-ni+1, P_XEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( ni .le. nide-1 .and. ni .ge. nide - sz ) THEN
                   bdy_t( nj,k,nide-ni, P_XEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( nj,k,nide-ni, P_XEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

               IF ( ystag ) THEN
                 IF   ( nj .le. njde .and. nj .ge. njde - sz + 1 ) THEN
                   bdy_t(ni,k,njde-nj+1,P_YEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( ni,k,njde-nj+1,P_YEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( nj .le. njde-1 .and. nj .ge. njde - sz ) THEN
                   bdy_t(ni,k,njde-nj,P_YEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( ni,k,njde-nj,P_YEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO
#endif

#ifdef DUMBCOPY
!write(0,'("cims:cime, ckms:ckme, cjms:cjme ",6i4)')cims,cime, ckms,ckme, cjms,cjme
!write(0,'("nims:nime, nkms:nkme, njms:njme ",6i4)')nims,nime, nkms,nkme, njms,njme
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

   SUBROUTINE interp_mask_land_field ( cfld,                     &  ! CD field
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
         CALL wrf_dm_bcast_real( sum, 1 )
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
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( cfld,                    &  ! CD field
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
   
            DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
            DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,k,j+1) + 2.*cfld(i,k,j) + cfld(i,k,j-1) )
               END DO
            END DO

            !  then 1-2-1 smoothing in the i direction last
       
            DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
            DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
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
   
            DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
               DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i,k,j+1) + cfld(i,k,j-1)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
               DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
               DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i+1,k,j) + cfld(i-1,k,j)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO j = MAX(jpos+1,cjts-2) , MIN(jpos+(njde-njds)/nrj-1-jstag,cjte+2)
               DO i = MAX(ipos+1,cits-2) , MIN(ipos+(nide-nids)/nri-1-istag,cite+2)
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
