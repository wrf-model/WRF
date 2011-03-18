!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_tmnupdate


CONTAINS

   SUBROUTINE tmnupdate(tsk,tmn,tlag,tyr,tyra,tdly,nday,nyear,lagday, &
                julian_in, dt, yr,                                  &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start,i_end, j_start,j_end, kts,kte, num_tiles   )


   IMPLICIT NONE

!---------------------------------------------------------------------
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     kts, kte, num_tiles, lagday

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER, INTENT(INOUT ) ::   NYEAR
   REAL   , INTENT(INOUT ) ::   NDAY
   INTEGER, INTENT(IN ) ::   YR

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)::   TSK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   TMN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ) ::   TYR
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ) ::   TYRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ) ::   TDLY
   REAL, DIMENSION( ims:ime , 1:lagday , jms:jme ), INTENT(INOUT ) ::   TLAG
   REAL,                                INTENT(IN)  :: julian_in, dt

!... Local Variables

!... Integer
      INTEGER :: ij, i, j, n
!... Real
      REAL, PARAMETER      :: tconst = 0.6
      REAL  :: julian, yrday, tprior, deltat

#ifdef NO_LEAP_CALENDAR
! no leap year when coupled with CCSM
      yrday=365.      ! number of days in a non-leap year
#else
      yrday=365.      ! number of days in a non-leap year
      if(mod(yr,4).eq.0) yrday=366.
#endif

! accumulate tsk of current day
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
             tdly(i,j)=tdly(i,j)+tsk(i,j)*dt
      ENDDO
      ENDDO
     ENDDO
    nday=nday+1.*dt


! Update deep soil temperature
! if it is the end of a day, update variables
    !! deltat=(julian_in-int(julian_in))*24.*3600.
    !! IF(nint(deltat).lt.dt) THEN
    deltat=(julian_in-nint(julian_in))*24.*3600.
    IF(abs(deltat).le.dt/2.) THEN
      julian=(julian_in-1.)+(dt/(60.*60.*24.))
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij, i, j, n )
      DO ij = 1 , num_tiles
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
! update tmn
               tprior=0.0
               do n=1,lagday
                 tprior=tprior+tlag(i,n,j)
               end do
               tprior=tprior/lagday
               tmn(i,j)=tconst*tyr(i,j)+(1.-tconst)*tprior
! update tlag and tyra
               do n=1,lagday-1
                 tlag(i,n,j)=tlag(i,n+1,j)
               end do
               tlag(i,lagday,j)=tdly(i,j)/nday
               tdly(i,j)=0.0
        ENDDO
        ENDDO
      ENDDO
      nday=0.
! update tyr if it is the end of a year
      if((yrday-julian).le.1.) then
        DO ij = 1 , num_tiles
          DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
                tyr(i,j)=tyra(i,j)/nyear
                tyra(i,j)=0.0
          ENDDO
          ENDDO
        ENDDO
        nyear=0
      else
        DO ij = 1 , num_tiles
          DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
                tyra(i,j)=tyra(i,j)+tlag(i,lagday,j)
          ENDDO
          ENDDO
        ENDDO
        nyear=nyear+1
      endif
    ENDIF

!
      return

   END SUBROUTINE tmnupdate


END MODULE module_sf_tmnupdate
