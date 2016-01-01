module module_HIFREQ

#if ( HWRF == 1 )
  ! This module implements the high-frequency output requested by the
  ! National Hurricane Center in 2010.  The hifreq_write routine will
  ! write a file that contains max. 10m wind, min. MSLP, their locations,
  ! and the nest center location once per timestep.  The hifreq_read
  ! routine is a sample routine for reading that output.  The hifreq_open
  ! routine is a convenience routine that can generate a nice-looking
  ! filename using WRF filename generation routines.

  !------------------------------------------------------------------------------------------------------

  private
  public HIFREQ_WRITE, HIFREQ_READ, HIFREQ_OPEN

CONTAINS

  !----------------------------------------------------------------------------------
  ! These two simple routines return an N, S, E or W for the hemisphere of
  ! a latitude or longitude:

  character(1) function get_lat_ns(lat)
    implicit none ; real lat
    if(lat>=0) then
       get_lat_ns='N'
    else
       get_lat_ns='S'
    endif
  end function get_lat_ns
  character(1) function get_lon_ew(lon)
    implicit none ; real lon
    if(lon>=0) then
       get_lon_ew='E'
    else
       get_lon_ew='W'
    endif
  end function get_lon_ew


  !------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE HIFREQ_READ(LUN,mingbl_mslp,maxgbl_wind,plat,plon,wlat,wlon,clat,clon,tm,ierr)
    !**********************************************************************
    !$$$  SUBPROGRAM DOCUMENTATION BLOCK
    !                .      .    .
    !   PRGRMMR: Sam Trahan
    !
    ! ABSTRACT:
    !         Call this routine to write one line to read in the values
    !         written out by hifreq_write.  Call this routine repeatedly
    !         to retrieve all lines.
    !
    ! PROGRAM HISTORY LOG:
    !   05-2011  : Sam Trahan
    !
    ! ATTRIBUTES:
    !   LANGUAGE: FORTRAN 90
    !   MACHINE : IBM SP
    !
    ! INPUT ARGUMENTS:
    !     LUN -- logical unit to read from
    !
    ! OUTPUT ARGUMENTS:
    !     IERR -- 0 on success, 1 on error (integer)
    !     TM -- forecast second (real)
    !     MINGBL_MSLP -- min. MSLP in mbar (real)
    !     MAXGBL_MSLP -- max. 10m wind in knots (real)
    !     plat, plon -- lat & lon of MSLP minimum (degrees, real)
    !     wlat, wlon -- lat & lon of wind maximum (degrees, real)
    !     clat, clon -- lat & lon of nest center (degrees, real)
    !$$$
    !**********************************************************************
    !
    implicit none
    real, intent(out) :: MINGBL_MSLP, MAXGBL_WIND
    real, intent(out) :: plat, plon
    real, intent(out) :: wlat, wlon
    real, intent(out) :: clat, clon
    real, intent(out) :: tm
    integer, intent(in) :: lun
    integer, intent(out) :: ierr
    character*1 :: pns,pew,wns,wew,cns,cew

    ierr=0

3131 format(F11.2,", ", &
         F9.4,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F6.3,A1,", ",F7.3,A1)
    read(lun,3131,err=3132) tm, &
         MINGBL_MSLP,plat,pns,plon,pew, &
         MAXGBL_WIND,wlat,wns,wlon,wew, &
         clat,cns,clon,cew

    if(pns == 'S') plat=-plat
    if(pew == 'W') plon=-plon
    if(wns == 'S') wlat=-wlat
    if(wew == 'W') wlon=-wlon
    if(cns == 'S') clat=-clat
    if(cew == 'W') clon=-clon

    return
3132 continue  ! I/O error or end of file.
    ierr=1
  END SUBROUTINE HIFREQ_READ


  SUBROUTINE HIFREQ_WRITE (LUN,NTSD,DT,HLAT,HLON              &
       ,U10,V10,PINT,T,Q                      &
       ,FIS,PD,PDTOP                          &
       ,DETA1,DETA2                           &
       ,IDS,IDE,JDS,JDE,KDS,KDE               &
       ,IMS,IME,JMS,JME,KMS,KME               &
       ,ITS,ITE,JTS,JTE,KTS,KTE            )

    !**********************************************************************
    !$$$  SUBPROGRAM DOCUMENTATION BLOCK
    !                .      .    .
    !   PRGRMMR: Original by Young Kwon, modified by Sam Trahan
    !
    ! ABSTRACT:
    !         Call this routine to write one line to the given LUN, 
    !         containing the minimum MSLP, max 10m wind, their locations,
    !         and the nest center location.
    ! PROGRAM HISTORY LOG:
    !   05-2011  : Young Kwon
    !   05-2011  : Sam Trahan -- Modified for efficiency, eliminated need
    !                for an external parser script.
    !
    ! ATTRIBUTES:
    !   LANGUAGE: FORTRAN 90
    !   MACHINE : IBM SP
    !$$$
    !**********************************************************************
    !
#ifdef DM_PARALLEL
    use mpi ! , only: MPI_MAXLOC, MPI_Allreduce, MPI_Bcast, MPI_2REAL, MPI_REAL
    USE MODULE_DM, only : wrf_dm_minloc_real, wrf_dm_maxloc_real, mytask, local_communicator
#endif
    USE MODULE_NEST_UTIL, only : MSLP_DIAG

    IMPLICIT NONE
    !
    LOGICAL, EXTERNAL :: wrf_dm_on_monitor
    INTEGER,INTENT(IN) :: NTSD, LUN
    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
         &                     ,IMS,IME,JMS,JME,KMS,KME                    &
         &                     ,ITS,ITE,JTS,JTE,KTS,KTE
    !
    REAL,                                     INTENT(IN)    :: PDTOP, DT
    REAL, DIMENSION(KMS:KME),                 INTENT(IN)    :: DETA1,DETA2
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)    :: FIS,PD,HLAT,HLON
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)    :: U10,V10
    REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)    :: PINT,T,Q

    !----------------------------------------------------------------------

    REAL, DIMENSION(IMS:IME,JMS:JME) :: WIND10SQ, MSLP

    REAL                     :: MINGBL_MSLP, MAXGBL_WIND, ZDUM, PREF
    REAL                     :: CLAT,CLON,PLAT,PLON,WLAT,WLON, WREF, HAVE_CEN
    INTEGER                  :: IWIND,JWIND, IMSLP,JMSLP
    INTEGER                  :: ICEN,JCEN,I,J,ITF,JTF,ierr,grank,myrank
    REAL                     :: comm(6),reduced(6),bcast(4)

    !----------------------------------------------------------------------

    ITF=MIN(ITE,IDE-1)
    JTF=MIN(JTE,JDE-1)

    ! Get the MSLP and the square of the 10m wind:
    WIND10SQ(its:itf,jts:jtf) = U10(its:itf,jts:jtf)**2+ &
         V10(its:itf,jts:jtf)**2
    call MSLP_DIAG (MSLP,PINT,T,Q               &
         ,FIS,PD,DETA1,DETA2,PDTOP    &
         ,IDS,IDE,JDS,JDE,KDS,KDE     &
         ,IMS,IME,JMS,JME,KMS,KME     &
         ,ITS,ITE,JTS,JTE,KTS,KTE     )

    ! Find the location of the wind & pressure extrema in this tile:
    imslp=its; jmslp=jts
    iwind=its; jwind=jts

    pref=MSLP(imslp,jmslp)   ! min mslp
    wref=WIND10SQ(iwind,jwind) ! max wind
    do j=jts,jtf
       do i=its,itf
          if(MSLP(i,j) < pref) then
             imslp=i ; jmslp=j
             pref=MSLP(imslp,jmslp)
          endif
          if(WIND10SQ(i,j) > wref) then
             iwind=i ; jwind=j
             wref=WIND10SQ(iwind,jwind)
          end if
       enddo
    enddo
    MINGBL_MSLP=pref             ;        MAXGBL_WIND=sqrt(wref)/0.514444
    PLAT=HLAT(imslp,jmslp)       ;        WLAT=HLAT(iwind,jwind)
    PLON=HLON(imslp,jmslp)       ;        WLON=HLON(iwind,jwind)
    zdum=0

    ! Get the center of the domain:
    ICEN=(IDE-1)/2
    JCEN=(JDE-1)/2
    HAVE_CEN=0
    if(ICEN>=its .and. ICEN<=itf .and. JCEN>=jts .and. JCEN<=jtf) then
       HAVE_CEN=1
       CLAT=HLAT(ICEN,JCEN)
       CLON=HLON(ICEN,JCEN)
    end if

#ifdef DM_PARALLEL
    ! Get grid-wide extrema:
    call MPI_Comm_rank(local_communicator,myrank,ierr)
    comm(1)=have_cen
    comm(2)=myrank
    comm(3)=-mingbl_mslp
    comm(4)=myrank
    comm(5)=maxgbl_wind
    comm(6)=myrank
    call MPI_Allreduce(comm,reduced,3,MPI_2REAL,MPI_MAXLOC,local_communicator,ierr)

    have_cen=reduced(1)
    grank=reduced(2)
    if(myrank==grank) then
       bcast=(/ clat,clon,real(icen),real(jcen) /)
    endif
    call MPI_Bcast(bcast,4,MPI_REAL,grank,local_communicator,ierr)
    if(myrank/=grank) then
       clat=bcast(1)
       clon=bcast(2)
       icen=bcast(3)
       jcen=bcast(4)
    endif

    mingbl_mslp=-reduced(3)
    grank=reduced(4)
    if(myrank==grank) then
       bcast=(/ plat,plon,real(imslp),real(jmslp) /)
    endif
    call MPI_Bcast(bcast,4,MPI_REAL,grank,local_communicator,ierr)
    if(myrank/=grank) then
       plat=bcast(1)
       plon=bcast(2)
       imslp=bcast(3)
       jmslp=bcast(4)
    endif

    maxgbl_wind=reduced(5)
    grank=reduced(6)
    if(myrank==grank) then
       bcast=(/ wlat,wlon,real(iwind),real(jwind) /)
    endif
    call MPI_Bcast(bcast,4,MPI_REAL,grank,local_communicator,ierr)
    if(myrank/=grank) then
       wlat=bcast(1)
       wlon=bcast(2)
       iwind=bcast(3)
       jwind=bcast(4)
    endif
#endif

    ! Monitor process writes out values.
    if(wrf_dm_on_monitor()) then
       ! Write out in a standard format (use hifreq_read to read it):
1313   format(F11.2,", ", &
            F8.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
            F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
            F6.3,A1,", ",F7.3,A1)
       write(LUN,1313) &
            dt*ntsd, &
            MINGBL_MSLP/100,abs(plat),get_lat_ns(plat),abs(plon),get_lon_ew(plon), &
            MAXGBL_WIND,abs(wlat),get_lat_ns(wlat),abs(wlon),get_lon_ew(wlon), &
            abs(clat),get_lat_ns(clat),abs(clon),get_lon_ew(clon)
       if(mod(ntsd,126)==125) then
          ! bug fix for IBM: will not write unless a flush is done periodically
          flush(lun)
       endif
    endif
    RETURN
  END SUBROUTINE hifreq_write


  SUBROUTINE hifreq_open ( grid , config_flags, atcf )
    ! Driver layer
    USE module_domain	, ONLY : domain, domain_clock_get
    USE module_configure	, ONLY : grid_config_rec_type

    IMPLICIT NONE

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor
    logical, intent(in), optional :: atcf
    ! Arguments
    TYPE(domain)                               :: grid
    TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

    ! Local
    CHARACTER*256                          :: outname
    INTEGER                                :: fid
    LOGICAL                                :: opened
    CHARACTER*80                           :: timestr

    character*255 :: message

    integer, parameter :: unitbase = 93, giveup=unitbase+1000
    logical is_atcf

    INTERFACE
       SUBROUTINE construct_filename2a( result , basename , fld1 , len1 , date_char )
         IMPLICIT NONE
         CHARACTER*(*) :: result
         CHARACTER*(*) :: basename
         CHARACTER*(*) :: date_char
         INTEGER , INTENT(IN) :: fld1 , len1
       END SUBROUTINE construct_filename2a
    END INTERFACE

    if(present(atcf)) then
       is_atcf=atcf
       call wrf_message('hifreq open: is atcf')
    else
       is_atcf=.false.
       call wrf_message('hifreq open: is not atcf')
    endif

    CALL domain_clock_get( grid, current_timestr=timestr )
    if(is_atcf) then
       CALL construct_filename2a ( outname ,config_flags%partial_atcf_outname, grid%id , 2 , timestr )
    else
       CALL construct_filename2a ( outname ,config_flags%high_freq_outname, grid%id , 2 , timestr )
    endif

#ifdef DM_PARALLEL
    if(wrf_dm_on_monitor()) then
#endif
       ! Find an unused unit number
       fid = unitbase + grid%id
       fid_loop:do while(fid <= giveup)
          write(message,'("HIFREQ OPEN TRY FID = ",I0)') fid
          call wrf_message(message)
          inquire(unit=fid,opened=opened)
          if(.not.opened) then
             write(message,'("HIFREQ OPEN UNUSED!!  ",I0)') fid
             call wrf_message(message)
             exit fid_loop
          end if
          fid=fid+1
       enddo fid_loop
       if(fid>giveup) then
          call wrf_error_fatal('Could not find an unused LUN in highfreq_open')
       endif

       write(message,'("HIFREQ APPEND  ",A1,A80,A1)') '"',trim(outname),'"'
       call wrf_message(message)
       open(unit=fid,file=trim(outname),position='append',form='formatted')

308    format(A,' output unit is now ',I0)
       if(is_atcf) then
          grid%outatcf_lun=fid
          write(message,308) 'Partial ATCF',grid%outatcf_lun
       else
          grid%hifreq_lun=fid
          write(message,308) 'Partial ATCF',grid%outatcf_lun
       endif
       call wrf_message(message)
#ifdef DM_PARALLEL
   else
      if(is_atcf) then
         grid%outatcf_lun=-99  ! must be non-zero but invalid
      else
         grid%hifreq_lun=-99  ! must be non-zero but invalid
      endif
   endif
#endif
  END SUBROUTINE hifreq_open

! only used by HWRF...  
#endif
end module module_HIFREQ
