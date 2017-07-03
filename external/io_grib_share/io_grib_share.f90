!
! Todd Hutchinson
! WSI
! August 17, 2005
!
! Routines in this file are shared by io_grib1 and io_grib2
!

!*****************************************************************************

SUBROUTINE get_dims(MemoryOrder, Start, End, ndim, x_start, x_end, y_start, &
     y_end, z_start, z_end)
  IMPLICIT NONE
  CHARACTER (LEN=*)    ,INTENT(IN)    :: MemoryOrder
  INTEGER              ,INTENT(OUT)   :: ndim,x_start,x_end,y_start
  INTEGER              ,INTENT(OUT)   :: y_end,z_start,z_end
  integer ,dimension(*),intent(in)    :: Start, End
  CHARACTER (LEN=1)                   :: char
  INTEGER                             :: idx
  CHARACTER (LEN=3)                   :: MemoryOrderLcl

  x_start = 1
  x_end   = 1
  y_start = 1
  y_end   = 1
  z_start = 1
  z_end   = 1

  !
  ! Note: Need to add "char == 'S'" for boundary conditions
  !

  ndim = 0

  ! Fix for out-of-bounds references
  MemoryOrderLcl = '   '
  do idx=1,len_trim(MemoryOrder)
     MemoryOrderLcl(idx:idx) = MemoryOrder(idx:idx)
  enddo
  !
  ! First, do the special boundary cases.  These do not seem to 
  !    
  if ((MemoryOrderLcl(1:3) .eq. 'XSZ') &
       .or. (MemoryOrderLcl(1:3) .eq. 'XEZ')) then
     x_start = Start(3)
     x_end = End(3)
     y_start = Start(1)
     y_end = End(1)
     z_start = Start(2)
     z_end = End(2)
     ndim = 3
  else if ((MemoryOrderLcl(1:3) .eq. 'YSZ') .or. &
       (MemoryOrderLcl(1:3) .eq. 'YEZ')) then
     x_start = Start(1)
     x_end = End(1)
     y_start = Start(3)
     y_end = End(3)
     z_start = Start(2)
     z_end = End(2)
     ndim = 3
  else if ((MemoryOrderLcl(1:2) .eq. 'YS') .or. &
       (MemoryOrderLcl(1:2) .eq. 'YE')) then
     x_start = Start(1)
     x_end = End(1)
     y_start = Start(2)
     y_end = End(2)
     ndim = 2
  else if ((MemoryOrderLcl(1:2) .eq. 'XS') .or. &
       (MemoryOrderLcl(1:2) .eq. 'XE')) then
     x_start = Start(2)
     x_end = End(2)
     y_start = Start(1)
     y_end = End(1)
     ndim = 2
  else if ((MemoryOrderLcl(1:1) .eq. 'C') .or. (MemoryOrderLcl(1:1) .eq. 'c')) then 
     ! This is for "non-decomposed" fields
     x_start = Start(1)
     x_end = End(1)
!     y_start = Start(2)
!     y_end = End(2)
!     z_start = Start(3)
!     z_end = End(3)
     ndim = 3
  else
     do idx=1,len_trim(MemoryOrderLcl)
        char = MemoryOrderLcl(idx:idx)
        if ((char == 'X') .or. (char == 'x')) then
           x_start = Start(idx)
           x_end   = End(idx)
           ndim = ndim + 1
        else if ((char == 'Y') .or. (char == 'y')) then
           y_start = Start(idx)
           y_end   = End(idx)
           ndim = ndim + 1
        else if ((char == 'Z') .or. (char == 'z')) then
           z_start = Start(idx)
           z_end   = End(idx)
           ndim = ndim + 1
        else if (char == '0') then
           ! Do nothing, this indicates field is a scalar.
           ndim = 0
        else
           call wrf_message('Invalid Dimension in get_dims: '//char)
        endif
     enddo
  endif

END SUBROUTINE get_dims

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE geth_idts (ndate, odate, idts)

  IMPLICIT NONE

  !  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
  !  compute the time difference.

  !  on entry     -  ndate  -  the new hdate.
  !                  odate  -  the old hdate.

  !  on exit      -  idts    -  the change in time in seconds.

  CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
  REAL              , INTENT(OUT)   :: idts

  !  Local Variables

  !  yrnew    -  indicates the year associated with "ndate"
  !  yrold    -  indicates the year associated with "odate"
  !  monew    -  indicates the month associated with "ndate"
  !  moold    -  indicates the month associated with "odate"
  !  dynew    -  indicates the day associated with "ndate"
  !  dyold    -  indicates the day associated with "odate"
  !  hrnew    -  indicates the hour associated with "ndate"
  !  hrold    -  indicates the hour associated with "odate"
  !  minew    -  indicates the minute associated with "ndate"
  !  miold    -  indicates the minute associated with "odate"
  !  scnew    -  indicates the second associated with "ndate"
  !  scold    -  indicates the second associated with "odate"
  !  i        -  loop counter
  !  mday     -  a list assigning the number of days in each month

  CHARACTER (LEN=24) :: tdate
  INTEGER :: olen, nlen
  INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
  INTEGER :: yrold, moold, dyold, hrold, miold, scold
  INTEGER :: mday(12), i, newdys, olddys
  LOGICAL :: npass, opass
  INTEGER :: isign
  CHARACTER (LEN=300) :: wrf_err_message
  INTEGER :: ndfeb

  IF (odate.GT.ndate) THEN
     isign = -1
     tdate=ndate
     ndate=odate
     odate=tdate
  ELSE
     isign = 1
  END IF

  !  Assign the number of days in a months

  mday( 1) = 31
  mday( 2) = 28
  mday( 3) = 31
  mday( 4) = 30
  mday( 5) = 31
  mday( 6) = 30
  mday( 7) = 31
  mday( 8) = 31
  mday( 9) = 30
  mday(10) = 31
  mday(11) = 30
  mday(12) = 31

  !  Break down old hdate into parts

  hrold = 0
  miold = 0
  scold = 0
  olen = LEN(odate)

  READ(odate(1:4),  '(I4)') yrold
  READ(odate(6:7),  '(I2)') moold
  READ(odate(9:10), '(I2)') dyold
  IF (olen.GE.13) THEN
     READ(odate(12:13),'(I2)') hrold
     IF (olen.GE.16) THEN
        READ(odate(15:16),'(I2)') miold
        IF (olen.GE.19) THEN
           READ(odate(18:19),'(I2)') scold
        END IF
     END IF
  END IF

  !  Break down new hdate into parts

  hrnew = 0
  minew = 0
  scnew = 0
  nlen = LEN(ndate)

  READ(ndate(1:4),  '(I4)') yrnew
  READ(ndate(6:7),  '(I2)') monew
  READ(ndate(9:10), '(I2)') dynew
  IF (nlen.GE.13) THEN
     READ(ndate(12:13),'(I2)') hrnew
     IF (nlen.GE.16) THEN
        READ(ndate(15:16),'(I2)') minew
        IF (nlen.GE.19) THEN
           READ(ndate(18:19),'(I2)') scnew
        END IF
     END IF
  END IF

  !  Check that the dates make sense.

  npass = .true.
  opass = .true.

  !  Check that the month of NDATE makes sense.

  IF ((monew.GT.12).or.(monew.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of NDATE = ', monew
     npass = .false.
  END IF

  !  Check that the month of ODATE makes sense.

  IF ((moold.GT.12).or.(moold.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of ODATE = ', moold
     opass = .false.
  END IF

  !  Check that the day of NDATE makes sense.

  IF (monew.ne.2) THEN
     ! ...... For all months but February
     IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  ELSE IF (monew.eq.2) THEN
     ! ...... For February
     IF ((dynew.GT.ndfeb(yrnew)).OR.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  END IF

  !  Check that the day of ODATE makes sense.

  IF (moold.ne.2) THEN
     ! ...... For all months but February
     IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  ELSE IF (moold.eq.2) THEN
     ! ....... For February
     IF ((dyold.GT.ndfeb(yrold)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  END IF

  !  Check that the hour of NDATE makes sense.

  IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
     npass = .false.
  END IF

  !  Check that the hour of ODATE makes sense.

  IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of ODATE = ', hrold
     opass = .false.
  END IF

  !  Check that the minute of NDATE makes sense.

  IF ((minew.GT.59).or.(minew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of NDATE = ', minew
     npass = .false.
  END IF

  !  Check that the minute of ODATE makes sense.

  IF ((miold.GT.59).or.(miold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of ODATE = ', miold
     opass = .false.
  END IF

  !  Check that the second of NDATE makes sense.

  IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
     npass = .false.
  END IF

  !  Check that the second of ODATE makes sense.

  IF ((scold.GT.59).or.(scold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Second of ODATE = ', scold
     opass = .false.
  END IF

  IF (.not. npass) THEN
     WRITE( wrf_err_message , * ) &
          'module_date_time: geth_idts: Bad NDATE: ', ndate(1:nlen)
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
  END IF

  IF (.not. opass) THEN
     WRITE( wrf_err_message , * ) &
          'module_date_time: geth_idts: Bad ODATE: ', odate(1:olen)
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
  END IF

  !  Date Checks are completed.  Continue.

  !  Compute number of days from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

  newdys = 0
  DO i = yrold, yrnew - 1
     newdys = newdys + (365 + (ndfeb(i)-28))
  END DO

  IF (monew .GT. 1) THEN
     mday(2) = ndfeb(yrnew)
     DO i = 1, monew - 1
        newdys = newdys + mday(i)
     END DO
     mday(2) = 28
  END IF

  newdys = newdys + dynew-1

  !  Compute number of hours from 1 January ODATE, 00:00:00 until odate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until odate

  olddys = 0

  IF (moold .GT. 1) THEN
     mday(2) = ndfeb(yrold)
     DO i = 1, moold - 1
        olddys = olddys + mday(i)
     END DO
     mday(2) = 28
  END IF

  olddys = olddys + dyold-1

  !  Determine the time difference in seconds

  idts = (newdys - olddys) * 86400
  idts = idts + (hrnew - hrold) * 3600
  idts = idts + (minew - miold) * 60
  idts = idts + (scnew - scold)

  IF (isign .eq. -1) THEN
     tdate=ndate
     ndate=odate
     odate=tdate
     idts = idts * isign
  END IF

END SUBROUTINE geth_idts

!*****************************************************************************

SUBROUTINE get_vert_stag(VarName,Stagger,vert_stag)
  
  character (LEN=*) :: VarName
  character (LEN=*) :: Stagger
  logical           :: vert_stag

  if ((index(Stagger,'Z') > 0) .or. (VarName .eq. 'DNW') &
       .or.(VarName .eq. 'RDNW')) then
     vert_stag = .true.
  else
     vert_stag = .false.
  endif
end SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION ndfeb ( year ) RESULT (num_days)
  
  ! Compute the number of days in February for the given year
  
  IMPLICIT NONE
  
  INTEGER :: year
  INTEGER :: num_days
  
  num_days = 28 ! By default, February has 28 days ...
  IF (MOD(year,4).eq.0) THEN  
     num_days = 29  ! But every four years, it has 29 days ...
     IF (MOD(year,100).eq.0) THEN
        num_days = 28  ! Except every 100 years, when it has 28 days ...
        IF (MOD(year,400).eq.0) THEN
           num_days = 29  ! Except every 400 years, when it has 29 days.
        END IF
     END IF
  END IF
  
END FUNCTION ndfeb

!*****************************************************************************

SUBROUTINE get_dimvals(MemoryOrder,x,y,z,dims)

  IMPLICIT NONE
  CHARACTER (LEN=*)    ,INTENT(IN)    :: MemoryOrder
  INTEGER              ,INTENT(IN)    :: x,y,z
  INTEGER, DIMENSION(*),INTENT(OUT)   :: dims
  INTEGER                             :: idx
  CHARACTER (LEN=1) :: char
  CHARACTER (LEN=3) :: MemoryOrderLcl

  dims(1) = 1
  dims(2) = 1
  dims(3) = 1

  ! Fix for out-of-bounds references
  MemoryOrderLcl = '   '
  do idx=1,len_trim(MemoryOrder)
     MemoryOrderLcl(idx:idx) = MemoryOrder(idx:idx)
  enddo

  !
  ! Note: Need to add "char == 'S'" for boundary conditions
  !

  if ((MemoryOrderLcl(1:3) .eq. 'XSZ') &
       .or. (MemoryOrderLcl(1:3) .eq. 'XEZ')) then
     dims(1) = y
     dims(2) = z
     dims(3) = x
  else if ((MemoryOrderLcl(1:3) .eq. 'YSZ') .or. &
       (MemoryOrderLcl(1:3) .eq. 'YEZ')) then
     dims(1) = x
     dims(2) = z
     dims(3) = y
  else if ((MemoryOrderLcl(1:2) .eq. 'YS') .or. &
       (MemoryOrderLcl(1:2) .eq. 'YE')) then
     dims(1) = x
     dims(2) = y
     dims(3) = z
  else if ((MemoryOrderLcl(1:2) .eq. 'XS') .or. &
       (MemoryOrderLcl(1:2) .eq. 'XE')) then
     dims(1) = y
     dims(2) = x
     dims(3) = z
  else if ((MemoryOrderLcl(1:1) .eq. 'C') .or. &
       (MemoryOrderLcl(1:1) .eq. 'c')) then
     ! Non-decomposed field
     dims(1) = x
     dims(2) = y
     dims(3) = z
  else 
     do idx=1,len_trim(MemoryOrderLcl)
        char = MemoryOrderLcl(idx:idx)
        if ((char == 'X') .or. (char == 'x')) then
           dims(idx) = x
        else if ((char == 'Y') .or. (char == 'y')) then
           dims(idx) = y
        else if ((char == 'Z') .or. (char == 'z')) then
           dims(idx) = z
        else if (char == '0') then
           ! This is a scalar, do nothing.
        else
           call wrf_message ('Invalid Dimension in get_dimvals: '//char)
        endif
     enddo
  endif

END SUBROUTINE get_dimvals

!*****************************************************************************

SUBROUTINE get_soil_layers(VarName,soil_layers)
  
  character (LEN=*) :: VarName
  logical           :: soil_layers

  if ((VarName .eq. 'ZS') .or. (VarName .eq. 'DZS') &
       .or.(VarName .eq. 'TSLB') .or. (VarName .eq. 'SMOIS') &
       .or. (VarName .eq. 'SH2O') .or. (VarName .eq. 'KEEPFR3DFLAG') &
       .or. (VarName .eq. 'SMFR3D')) then
     soil_layers = .true.
  else
     soil_layers = .false.
  endif
end SUBROUTINE

!*****************************************************************************

SUBROUTINE Transpose_grib(MemoryOrder, di, FieldType, Field, &
     Start1, End1, Start2, End2, Start3, End3, data, zidx, numrows, numcols)

  IMPLICIT NONE

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110

  CHARACTER (LEN=*),INTENT(IN)    :: MemoryOrder
  INTEGER          ,INTENT(IN)    :: Start1,End1,Start2,End2,Start3,End3
  INTEGER          ,INTENT(IN)    :: di
  integer          ,intent(inout) :: &
       Field(di,Start1:End1,Start2:End2,Start3:End3)
  INTEGER          ,intent(in)    :: FieldType
  real             ,intent(in)    :: data(*)
  INTEGER          ,INTENT(IN)    :: zidx, numcols, numrows
  INTEGER, DIMENSION(3)           :: dims
  INTEGER                         :: col, row
  LOGICAL                         :: logicaltype
  CHARACTER (LEN=1000)            :: msg
     
  if ((FieldType == WRF_REAL) .or. (FieldType == WRF_DOUBLE)) then
     do col=1,numcols
        do row=1,numrows
           call get_dimvals(MemoryOrder,col,row,zidx,dims)
           Field(1:di,dims(1),dims(2),dims(3)) = &
                TRANSFER(data((row-1)*numcols+col),Field,1)
        enddo
     enddo
  else if (FieldType == WRF_INTEGER) then
     do col=1,numcols
        do row=1,numrows
           call get_dimvals(MemoryOrder,col,row,zidx,dims)
           Field(1:di,dims(1),dims(2),dims(3)) = data((row-1)*numcols+col)
        enddo
     enddo
  else
     write (msg,*)'Reading of type ',FieldType,'from grib data not supported'
     call wrf_message(msg)
  endif

!
! This following seciton is for the logical type.  This caused some problems
!   on certain platforms.
! 
!  else if (FieldType == WRF_LOGICAL) then
!     do col=1,numcols
!        do row=1,numrows
!           call get_dimvals(MemoryOrder,col,row,zidx,dims)
!           Field(1:di,dims(1),dims(2),dims(3)) = &
!                TRANSFER(data((row-1)*numcols+col),logicaltype,1)
!        enddo
!     enddo
  
  
end SUBROUTINE

!*****************************************************************************

SUBROUTINE Transpose1D_grib(MemoryOrder, di, FieldType, Field, &
     Start1, End1, Start2, End2, Start3, End3, data, nelems)

  IMPLICIT NONE

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110

  CHARACTER (LEN=*),INTENT(IN)    :: MemoryOrder
  INTEGER          ,INTENT(IN)    :: Start1,End1,Start2,End2,Start3,End3
  INTEGER          ,INTENT(IN)    :: di
  integer          ,intent(inout) :: &
       Field(di,Start1:End1,Start2:End2,Start3:End3)
  INTEGER          ,intent(in)    :: FieldType
  real             ,intent(in)    :: data(*)
  LOGICAL                         :: logicaltype
  CHARACTER (LEN=1000)            :: msg
  integer                         :: elemnum,nelems

  if ((FieldType == WRF_REAL) .or. (FieldType == WRF_DOUBLE)) then
     do elemnum=1,nelems
        Field(1:di,elemnum,1,1) = TRANSFER(data(elemnum),Field,1)
     enddo
  else if (FieldType == WRF_INTEGER) then
     do elemnum=1,nelems
        Field(1:di,elemnum,1,1) = TRANSFER(data(elemnum),Field,1)
     enddo
  else
     write (msg,*)'Reading of type ',FieldType,'from grib1 data not supported'
     call wrf_message(msg)
  endif

!
! This following seciton is for the logical type.  This caused some problems
!   on certain platforms.
! 
!  else if (FieldType == WRF_LOGICAL) then
!     do col=1,numcols
!        do row=1,numrows
!           call get_dimvals(MemoryOrder,col,row,zidx,dims)
!           Field(1:di,dims(1),dims(2),dims(3)) = &
!                TRANSFER(data((row-1)*numcols+col),logicaltype,1)
!        enddo
!     enddo
  
  
end SUBROUTINE Transpose1D_grib

!*****************************************************************************
!
! Takes a starting date (startTime) in WRF format (yyyy-mm-dd_hh:mm:ss), 
!   adds an input number of seconds to the time, and outputs a new date 
!   (endTime) in WRF format.
!
!*****************************************************************************

subroutine advance_wrf_time(startTime, addsecs, endTime)
  implicit none

  integer          , intent(in)  :: addsecs
  character (len=*), intent(in)  :: startTime
  character (len=*), intent(out) :: endTime
  integer :: syear,smonth,sday,shour,smin,ssec
  integer :: days_in_month(12)

  read(startTime,'(I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2)') &
       syear,smonth,sday,shour,smin,ssec
  
  ssec = ssec + addsecs

  do while (ssec .ge. 60) 
     smin = smin + 1
     ssec = ssec - 60
  enddo

  do while (smin .ge. 60)
     shour = shour + 1
     smin = smin - 60
  enddo

  do while (shour .ge. 24)
     sday = sday + 1
     shour = shour - 24
  enddo


  days_in_month(1) = 31
  if (((mod(syear,4) .eq. 0) .and. (mod(syear,100) .ne. 0)) &
       .or. (mod(syear,400) .eq. 0)) then
     days_in_month(2) = 29
  else
     days_in_month(2) = 28
  endif
  days_in_month(3) = 31
  days_in_month(4) = 30
  days_in_month(5) = 31
  days_in_month(6) = 30
  days_in_month(7) = 31
  days_in_month(8) = 31
  days_in_month(9) = 30
  days_in_month(10) = 31
  days_in_month(11) = 30
  days_in_month(12) = 31


  do while (sday .gt. days_in_month(smonth))
     sday = sday - days_in_month(smonth)
     smonth = smonth + 1
     if (smonth .gt. 12) then
        smonth = 1
        syear = syear + 1
     endif
  enddo
  

  write(endTime,'(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
       syear,'-',smonth,'-',sday,'_',shour,':',smin,':',ssec

  return

end subroutine
