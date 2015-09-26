
MODULE MODULE_PER_TYPE

!-----------------------------------------------------------------------------!
! Count and Print observation per type (synop, sound, etc.)
!
!  HISTORY: 
!         F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!         06/30/2006 -   Updated for AIRS retrievals       Syed  RH  Rizvi
!         11/09/2006 -   Updated for GPS RO                Y.-R. Guo
!------------------------------------------------------------------------------

   INTEGER                              :: fm
   INTEGER                              :: icor
   CHARACTER (LEN = 40)                 :: platform


   INTEGER,  DIMENSION (0:9)             :: nsynops, nshipss, nmetars, &
                                            npilots, nsounds, nsatems, &
                                            nsatobs, naireps, nothers, &
                                            ngpspws, nssmt1s, nssmt2s, &
                                            nssmis,  ntovss,  namdars, &
                                            nqscats, nprofls, nbuoyss, &
                                            ngpsztd, ngpsref, ngpseph, &
                                            nboguss, nairss , ntamdar, ntotals

CONTAINS
!-----------------------------------------------------------------------------!

SUBROUTINE print_per_type (title, caption,iend)

!-----------------------------------------------------------------------------!
   IMPLICIT NONE

   CHARACTER (LEN =  *), INTENT (in) :: title
   CHARACTER (LEN =  *), INTENT (in) :: caption
   INTEGER,              INTENT (in) :: iend

   INTEGER              :: i,iend1
   CHARACTER (LEN = 80) :: fmt_obs

!------------------------------------------------------------------------------!

   IF ((iend .LT. 0) .OR. (iend .GT. 7)) THEN
       WRITE (UNIT = 0, FMT = '(/,A)') " ERROR: MAXIMUM PRINT PER OBS TYPE IS 7"
       STOP                             "in print_per_type.F"
   ENDIF

   ! Output obs

   nsynops (iend) = 0.
   nmetars (iend) = 0.
   nshipss (iend) = 0.
   nsounds (iend) = 0.
   namdars (iend) = 0.
   naireps (iend) = 0.
   npilots (iend) = 0.
   nsatems (iend) = 0.
   nsatobs (iend) = 0.
   ngpspws (iend) = 0.
   ngpsztd (iend) = 0.
   ngpsref (iend) = 0.
   ngpseph (iend) = 0
   nssmt1s (iend) = 0.
   nssmt2s (iend) = 0.
   nssmis  (iend) = 0.
   ntovss  (iend) = 0.
   nqscats (iend) = 0.
   nprofls (iend) = 0.
   nbuoyss (iend) = 0.
   nboguss (iend) = 0.
   nairss  (iend) = 0.
   ntamdar (iend) = 0.
   nothers (iend) = 0.

   DO i = 1, iend-1
      nsynops (iend) = nsynops (iend) + nsynops (i)
      nmetars (iend) = nmetars (iend) + nmetars (i)
      nshipss (iend) = nshipss (iend) + nshipss (i)
      nsounds (iend) = nsounds (iend) + nsounds (i)
      namdars (iend) = namdars (iend) + namdars (i)
      naireps (iend) = naireps (iend) + naireps (i)
      npilots (iend) = npilots (iend) + npilots (i)
      nsatems (iend) = nsatems (iend) + nsatems (i)
      nsatobs (iend) = nsatobs (iend) + nsatobs (i)
      ngpspws (iend) = ngpspws (iend) + ngpspws (i)
      ngpsztd (iend) = ngpsztd (iend) + ngpsztd (i)
      ngpsref (iend) = ngpsref (iend) + ngpsref (i)
      ngpseph (iend) = ngpseph (iend) + ngpseph (i)
      nssmt1s (iend) = nssmt1s (iend) + nssmt1s (i)
      nssmt2s (iend) = nssmt2s (iend) + nssmt2s (i)
      nssmis  (iend) = nssmis  (iend) + nssmis  (i)
      ntovss  (iend) = ntovss  (iend) + ntovss  (i)
      nqscats (iend) = nqscats (iend) + nqscats (i)
      nprofls (iend) = nprofls (iend) + nprofls (i)
      nbuoyss (iend) = nbuoyss (iend) + nbuoyss (i)
      nboguss (iend) = nboguss (iend) + nboguss (i)
      nairss  (iend) = nairss  (iend) + nairss  (i)
      ntamdar (iend) = ntamdar (iend) + ntamdar (i)
      nothers (iend) = nothers (iend) + nothers (i)
   ENDDO

   nsynops (iend) = nsynops (0) - nsynops (iend)
   nmetars (iend) = nmetars (0) - nmetars (iend)
   nshipss (iend) = nshipss (0) - nshipss (iend)
   nsounds (iend) = nsounds (0) - nsounds (iend)
   namdars (iend) = namdars (0) - namdars (iend)
   naireps (iend) = naireps (0) - naireps (iend)
   npilots (iend) = npilots (0) - npilots (iend)
   nsatems (iend) = nsatems (0) - nsatems (iend)
   nsatobs (iend) = nsatobs (0) - nsatobs (iend)
   ngpspws (iend) = ngpspws (0) - ngpspws (iend)
   ngpsztd (iend) = ngpsztd (0) - ngpsztd (iend)
   ngpsref (iend) = ngpsref (0) - ngpsref (iend)
   ngpseph (iend) = ngpseph (0) - ngpseph (iend)
   nssmt1s (iend) = nssmt1s (0) - nssmt1s (iend)
   nssmt2s (iend) = nssmt2s (0) - nssmt2s (iend)
   nssmis  (iend) = nssmis  (0) - nssmis  (iend)
   ntovss  (iend) = ntovss  (0) - ntovss  (iend)
   nqscats (iend) = nqscats (0) - nqscats (iend)
   nprofls (iend) = nprofls (0) - nprofls (iend)
   nbuoyss (iend) = nbuoyss (0) - nbuoyss (iend)
   nboguss (iend) = nboguss (0) - nboguss (iend)
   nairss  (iend) = nairss  (0) - nairss  (iend)
   ntamdar (iend) = ntamdar (0) - ntamdar (iend)
   nothers (iend) = nothers (0) - nothers (iend)

   !  Total  obs

   ntotals = nsynops + nshipss + nmetars + npilots + nsounds + &
             nsatems + nsatobs + naireps + ngpspws + nssmt1s + &
             nssmt2s + ntovss  + nssmis  + namdars + nqscats + &
             nprofls + nbuoyss + nothers + ngpsztd + ngpsref + &
             nairss  + nboguss + ngpseph + ntamdar

   !  Format

   IF (iend .LT. 8) THEN 
   WRITE (fmt_obs,'(A,I1,A)') "(A,",iend+1,"(I7,1X))"
   ELSE
   WRITE (fmt_obs,'(A,I2,A)') "(A,",iend+1,"(I7,1X))"
   ENDIF

   ! Print title

   WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
   WRITE (UNIT = 0, FMT = '(A,/)') TRIM (title)

   WRITE (UNIT = 0, FMT = '(15X,A7)',ADVANCE='no') caption (1:7)

   DO i = 1, iend
      WRITE (UNIT = 0, FMT = '(1X,A7)',ADVANCE='no') caption (7*i+1:7*(i+1))
   ENDDO

   IF (caption (7*(iend+1)+1:7*(iend+2)) .EQ. "INGESTD") THEN
      WRITE (UNIT = 0, FMT = '(1X,A7)',ADVANCE='no') &
      caption (7*(iend+1)+1:7*(iend+2))
!     iend1 = iend + 1
      iend1 = iend
   ELSE
      iend1 = iend
   ENDIF

   WRITE (UNIT = 0, FMT = '(A)') ' '

   !   Print observations per type


   WRITE (UNIT =   0, FMT = fmt_obs) " SYNOP reports:",nsynops (0),&
         (nsynops (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SHIPS reports:",nshipss (0), &
         (nshipss (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " BUOYS reports:",nbuoyss (0), &
         (nbuoyss (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " BOGUS reports:",nboguss (0), &
         (nboguss (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " METAR reports:",nmetars (0), &
         (nmetars (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " PILOT reports:",npilots (0), &
         (npilots (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SOUND reports:",nsounds (0), &
         (nsounds (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " AMDAR reports:",namdars (0), &
         (namdars (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SATEM reports:",nsatems (0), &
         (nsatems (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SATOB reports:",nsatobs (0), &
         (nsatobs (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " AIREP reports:",naireps (0), &
         (naireps (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " GPSPW reports:",ngpspws (0), &
         (ngpspws (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " GPSRF reports:",ngpsref (0), &
         (ngpsref (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " GPSEP reports:",ngpseph (0), &
         (ngpseph (i),  i = 1, iend1)
   WRITE (UNIT =   0, FMT = fmt_obs) " SSMT1 reports:",nssmt1s (0), &
         (nssmt1s (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SSMT2 reports:",nssmt2s (0), &
         (nssmt2s (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " SSMI  reports:",nssmis  (0), &
         (nssmis  (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " TOVS  reports:",ntovss  (0), &
         (ntovss  (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " QSCAT reports:",nqscats (0), &
         (nqscats (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " RPOFL reports:",nprofls (0), &
         (nprofls (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " AIRS  reports:",nairss  (0), &
         (nairss  (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) "TAMDAR reports:",ntamdar (0), &
         (ntamdar (i),  i = 1, iend1)
   WRITE (UNIT =   0, FMT = fmt_obs) " OTHER reports:",nothers (0), &
         (nothers (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = fmt_obs) " Total reports:",ntotals (0), &
         (ntotals (i),  i = 1, iend1) 
   WRITE (UNIT =   0, FMT = '(A)')  ""

END SUBROUTINE print_per_type

END MODULE module_per_type
