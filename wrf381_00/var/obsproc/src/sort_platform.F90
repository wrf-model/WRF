SUBROUTINE sort_platform (nobs_max, obs, number_of_obs,  &
                          nsynops, nshipss, nmetars, npilots, nsounds, &
                          nsatems, nsatobs, naireps, ngpspws, ngpsztd, &
                          ngpsref, ngpseph, nssmt1s,                   &
                          nssmt2s, nssmis,  ntovss,  nothers, namdars, &
                          nqscats, nprofls, nbuoyss, nboguss, nairss ,ntamdar, title)

!------------------------------------------------------------------------------
! Count observations per type
!
!------------------------------------------------------------------------------
!
!  HISTORY: 
!
!          F. VANDENBERGHE, March 2001
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

  USE module_type

  IMPLICIT NONE

  CHARACTER (LEN =  *), INTENT (in)                         :: title
  INTEGER,              INTENT (in)                         :: nobs_max
  TYPE (report),        INTENT (inout), DIMENSION (nobs_max):: obs
  INTEGER,              INTENT (in)                         :: number_of_obs
  INTEGER,              INTENT (inout)                      :: nsynops,nshipss,&
                                                               nmetars,npilots,&
                                                               nsounds,nsatems,&
                                                               nsatobs,naireps,&
                                                               ngpspws,nssmt1s,&
                                                               nssmt2s,nssmis, &
                                                               ntovss, namdars,&
                                                               nqscats,nprofls,&
                                                               nbuoyss,nothers,&
                                                               nboguss,nairss, &
                                                               ngpsztd,ngpsref,&
                                                               ngpseph,ntamdar

  TYPE (measurement ) ,POINTER                              :: current
  INTEGER                                                   :: loop_index

  CHARACTER (LEN = 40)                                      :: platform
  INTEGER                                                   :: fm, is_sound
  INTEGER                                                   :: nvalids,nmultis,&
                                                               nsingls,nlevels
  INTEGER                                                   :: nuppers

  INCLUDE 'platform_interface.inc'

!------------------------------------------------------------------------------!

              WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE ( UNIT = 0, FMT = '(A)') TRIM (title)


! 0.  RESET
! =========

      nvalids = 0
      nmultis = 0
      nsingls = 0
      nlevels = 0

      nsynops = 0
      nmetars = 0
      nshipss = 0
      npilots = 0
      nsounds = 0
      namdars = 0
      nsatems = 0
      nsatobs = 0
      ntamdar = 0
      naireps = 0
      ngpspws = 0
      ngpsztd = 0
      ngpsref = 0
      ngpseph = 0
      nssmt1s = 0
      nssmt2s = 0
      nssmis  = 0
      ntovss  = 0
      nqscats = 0
      nprofls = 0
      nbuoyss = 0
      nboguss = 0
      nairss  = 0
      nothers = 0


! 1. LOOP OVER STATIONS
! ====================

stations: &
      DO loop_index = 1, number_of_obs


! 1.1 Check if record is valid
!     ------------------------

stations_valid: &
      IF (obs(loop_index)%info%discard ) THEN

      CYCLE  stations

      ELSE stations_valid

      nvalids = nvalids + 1

! 1.2 Platform code xx
!     ----------------

       READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

       IF (fm .LE. 0)  THEN
           WRITE (0,'(A,A,I3,A)') obs (loop_index) % info % platform, &
                                  "FM =",fm," IS INVALID."
           CYCLE  stations
       ENDIF


! 1.3 interpret code
!     ---------------
       CALL  fm_decoder (fm, platform, &
                         synop=nsynops, ship =nshipss, metar=nmetars, &
                         pilot=npilots, sound=nsounds, satem=nsatems, &
                         satob=nsatobs, airep=naireps, gpspw=ngpspws, &
                         gpszd=ngpsztd, gpsrf=ngpsref, gpsep=ngpseph, &
                         ssmt1=nssmt1s, ssmt2=nssmt2s, ssmi =nssmis,  &
                         tovs =ntovss,  amdar=namdars, qscat=nqscats, &
                         profl=nprofls, buoy =nbuoyss, bogus=nboguss, &
                         airs=nairss, tamdar=ntamdar, other=nothers )

! 1.4 Initialise pointer to surface level
!     -----------------------------------

      current => obs (loop_index) % surface

! 2.  LOOP ON UPPER LEVELS
! ========================

      is_sound  = -1
      nuppers   =  0

levels: DO WHILE (ASSOCIATED (current))

! 2.1 Found one level, increment
!     --------------------------

      is_sound = is_sound + 1
      nuppers  = nuppers  + 1
      nlevels  = nlevels  + 1

! 2.2 Go to next level
!     ----------------

      current => current%next

      ENDDO levels

! 2.3 Count surface obs and sounding
!     ------------------------------

      if (is_sound .gt. 0) then
          nmultis = nmultis + 1
      else
          nsingls = nsingls + 1
      endif

! 3. ASSIGN THE NUMBER OF UPPER-AIR LEVELS TO STATION
! ===================================================

      obs (loop_index) % info % levels = nuppers


! 4.  GO TO NEXT STATION
! ======================

! 4.1 Go to next valid station
!     ------------------------

      ENDIF  stations_valid

! 4.2 Go to next station
!     ------------------
      ENDDO  stations


! 5.  PRINT OUT
! =============

      WRITE (0, '(A)')
      WRITE (0, '(A,I7)') ' SYNOP reports:',nsynops
      WRITE (0, '(A,I7)') ' SHIPS reports:',nshipss
      WRITE (0, '(A,I7)') ' BUOY  reports:',nbuoyss
      WRITE (0, '(A,I7)') ' BUGUS reports:',nboguss
      WRITE (0, '(A,I7)') ' METAR reports:',nmetars
      WRITE (0, '(A,I7)') ' PILOT reports:',npilots
      WRITE (0, '(A,I7)') ' SOUND reports:',nsounds
      WRITE (0, '(A,I7)') ' AMDAR reports:',namdars
      WRITE (0, '(A,I7)') ' SATEM reports:',nsatems
      WRITE (0, '(A,I7)') ' SATOB reports:',nsatobs
      WRITE (0, '(A,I7)') ' AIREP reports:',naireps
      WRITE (0, '(A,I7)') 'TAMDAR reports:',ntamdar
      WRITE (0, '(A,I7)') ' GPSPW reports:',ngpspws
      WRITE (0, '(A,I7)') ' GPSZD reports:',ngpsztd
      WRITE (0, '(A,I7)') ' GPSRF reports:',ngpsref
      WRITE (0, '(A,I7)') ' GPSEP reports:',ngpseph
      WRITE (0, '(A,I7)') ' SSMT1 reports:',nssmt1s
      WRITE (0, '(A,I7)') ' SSMT2 reports:',nssmt2s
      WRITE (0, '(A,I7)') ' SSMI  reports:',nssmis
      WRITE (0, '(A,I7)') ' TOVS  reports:',ntovss
      WRITE (0, '(A,I7)') ' QSCAT reports:',nqscats
      WRITE (0, '(A,I7)') ' PROFL reports:',nprofls 
      WRITE (0, '(A,I7)') ' AIRST reports:',nairss  
      WRITE (0, '(A,I7)') ' OTHER reports:',nothers
      WRITE (0, '(A,3(I7,A),/)') &
                          ' Total reports:',nsynops+nshipss+nmetars+&
                                            npilots+nsounds+nsatems+&
                                            nsatobs+naireps+ngpspws+&
                                            ngpsztd+ngpsref+ngpseph+&
                                            nboguss+&
                                            nssmt1s+nssmt2s+nssmis +&
                                            ntovss +namdars+nqscats+&
                                            nprofls+nbuoyss+nairss+ntamdar+nothers, &
      " = ",nsingls," single + ",nmultis," multi-level reports."


! 4.  END
! =======
      RETURN

      END SUBROUTINE sort_platform
