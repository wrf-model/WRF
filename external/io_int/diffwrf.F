module read_util_module

contains

   subroutine arguments(v2file, lmore)
     implicit none
     character(len=*) :: v2file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
     integer, external :: iargc
   
     numarg = iargc()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call getarg(i, harg)
        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call getarg(i,harg)
     v2file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call getarg(0, cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     call wrf_error_fatal ( 'usage' )
   end subroutine help
end module read_util_module

 program readv3
  use read_util_module
  use module_ext_internal
  implicit none
  include '../io_netcdf/wrf_status_codes.h'
  include '../io_netcdf/wrf_io_flags.h'
  character(len=120) :: flnm
  character(len=120) :: flnm2
  character(len=120) :: arg3
  character(len=19) :: DateStr
  character(len=19) :: DateStr2
  character(len=31) :: VarName
  character(len=31) :: VarName2
  integer dh1, dh2

  integer :: flag, flag2
  integer :: iunit, iunit2
  integer :: WrfType

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim2
  real :: time, time2
  real :: a, b
  integer, dimension(4) :: start_index, end_index, start_index2, end_index2
  integer , Dimension(3) :: MemS,MemE,PatS,PatE
  character (len= 4) :: staggering,   staggering2
  character (len= 4) :: ordering,     ordering2, ord
  character (len=24) :: start_date,   start_date2
  character (len=24) :: current_date, current_date2
  character (len=31) :: name,         name2,  tmpname
  character (len=25) :: units,        units2
  character (len=46) :: description,  description2

  character (len=80), dimension(3)  ::  dimnames

  integer :: l, n
  integer :: ikdiffs, ifdiffs

  real, allocatable, dimension(:,:,:,:) :: data,data2

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time, Status_next_time2, Status_next_var, Status_next_var_2

  logical :: newtime = .TRUE.
  logical :: justplot, efound

  integer, external :: iargc
  logical, external :: iveceq

  levlim = -1

  call ext_int_ioinit(Status)

  Justplot = .false.
! get arguments
  if ( iargc() .ge. 2 ) then
    call getarg(1,flnm)
    call getarg(2,flnm2)
    ierr = 0
    call ext_int_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
    if ( Status /= 0 ) then 
      print*,'error opening ',flnm, ' Status = ', Status ; call wrf_error_fatal ( 'error opening '// flnm )
    endif
    call ext_int_open_for_read( trim(flnm2), 0, 0, "", dh2, Status)
    if ( Status /= 0 ) go to 923
    goto 924
923    continue

! bounce here if second name is not openable -- this would mean that
! it is a field name instead.

    print*,'could not open ',flnm2
    name = flnm2
    Justplot = .true.
924    continue
  if ( iargc() .eq. 3 ) then
    call getarg(3,arg3)
    read(arg3,*)levlim
    print*,'LEVLIM = ',LEVLIM
  endif
  else
     print*,'Usage: command file1 file2'
     call wrf_error_fatal ( 'Usage: command file1 file2' )
  endif

print*,'Just plot ',Justplot

if ( Justplot ) then
  print*, 'flnm = ', trim(flnm)

   call ext_int_get_next_time(dh1, DateStr, Status_next_time)

   DO WHILE ( Status_next_time .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    call ext_int_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_int_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )

      write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
               VarName, ndim, end_index(1), end_index(2), end_index(3), &
               trim(ordering), trim(DateStr)

      if ( VarName .eq. name ) then
        write(*,*)'Writing fort.88 file for ', trim(name)

        allocate(data(end_index(1), end_index(2), end_index(3), 1))

        if ( ndim .eq. 3 ) then
          ord = 'XYZ'
        else if ( ndim .eq. 2 ) then
          ord = 'XY'
        else if ( ndim .eq. 1 ) then
          ord = 'Z'
        else if ( ndim .eq. 0 ) then
          ord = '0'
        endif

        call ext_int_read_field(dh1,DateStr,TRIM(name),data,WRF_REAL,0,0,0,ord,   &
                            staggering,          &
                            dimnames,         &
                            start_index,end_index,                      & !dom              
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)

        if ( ierr/=0 ) then
             write(*,*)'error reading data record'
             write(*,*)'  ndim = ', ndim
             write(*,*)'  end_index(1) ',end_index(1)
             write(*,*)'  end_index(2) ',end_index(2)
             write(*,*)'  end_index(3) ',end_index(3)
        endif


        IF ( ndim .eq. 3 ) THEN
        do k = start_index(2), end_index(2)
          if ( levlim .eq. -1 .or. k .eq. levlim ) then
            write(88,*)end_index(1),end_index(3),' ',trim(name),' ',k,' time ',n
            do j = 1, end_index(3)
              do i = 1, end_index(1)
                write(88,*) data(i,k,j,1)
              enddo
            enddo
          endif
        enddo
        ELSE IF ( ndim .eq. 2 ) THEN
            k = 1
            write(88,*)end_index(1),end_index(2),' ',trim(name),' ',k,' time ',n
            do j = 1, end_index(2)
              do i = 1, end_index(1)
                write(88,*) data(i,j,1,1)
              enddo
            enddo
        ENDIF
        deallocate(data)
      endif
      call ext_int_get_next_var (dh1, VarName, Status_next_var)
!print*,'  ext_int_get_next_var Status = ',Status_next_time
    enddo
    call ext_int_get_next_time(dh1, DateStr, Status_next_time)
!print*,'ext_int_get_next_time Status = ',Status_next_time, DateStr
!    Status_next_time = 0
  enddo
else
  print*,'Diffing not implemented in this version of diffwrf yet'
  call wrf_error_fatal ( 'Diffing not implemented in this version of diffwrf yet' )
  print*,'Diffing ',trim(flnm),' ',trim(flnm2)

  call ext_int_get_next_time(dh1, DateStr, Status_next_time)
  call ext_int_get_next_time(dh2, DateStr2, Status_next_time2)

  IF ( DateStr .NE. DateStr2 ) THEN
    print*,'They differ big time.  Dates do not match'
    print*,'   ',flnm,' ',DateStr
    print*,'   ',flnm2,' ',DateStr2
    Status_next_time = 1
  ENDIF

  DO WHILE ( Status_next_time .eq. 0 .AND. Status_next_time2 .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    call ext_int_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_int_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      call ext_int_get_var_info (dh2,VarName,ndim2,ordering2,staggering2,start_index2,end_index2, WrfType, ierr )
      IF ( ierr /= 0 ) THEN
	write(*,*)'Big difference: ',VarName,' not found in ',flnm2
      ENDIF
      IF ( ndim /= ndim2 ) THEN
	write(*,*)'Big difference: Number of dimensions for ',Varname,' differs in ',flnm2,'(',ndim,') /= (',ndim2
      ENDIF
!      IF ( ordering /= ordering2 ) THEN
!	write(*,*)'Big difference: Ordering of dimensions for ',Varname,' differs in ',flnm2,'(',ordering,') /= (',ordering2
!      ENDIF

      DO i = 1, ndim
        IF ( end_index(i) /= end_index2(i) ) THEN
	  write(*,*)'Big difference: dim ',i,' lengths differ for ',Varname,' differ in ',flnm2
	  write(*,*)'   ',flnm,'  ',end_index(i)
	  write(*,*)'   ',flnm2,'  ',end_index2(i)
        ENDIF
      ENDDO
      DO i = ndim+1,3
        start_index(i) = 1
        end_index(i) = 1
        start_index2(i) = 1
        end_index2(i) = 1
      ENDDO

      write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
               VarName, ndim, end_index(1), end_index(2), end_index(3), &
               trim(ordering), trim(DateStr)

      allocate(data (end_index(1), end_index(2), end_index(3), 1))
      allocate(data2(end_index(1), end_index(2), end_index(3), 1))

      if ( ndim .eq. 3 ) then
        ord = 'XYZ'
      else if ( ndim .eq. 2 ) then
        ord = 'XY'
      else if ( ndim .eq. 1 ) then
        ord = 'Z'
      else if ( ndim .eq. 0 ) then
        ord = '0'
      endif

      call ext_int_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord,   &
                          staggering,          &
                          dimnames,         &
                          start_index,end_index,                      & !dom              
                          start_index,end_index,                      & !mem
                          start_index,end_index,                      & !pat
                          ierr)

      IF ( ierr /= 0 ) THEN
	write(*,*)'Error reading ',Varname,' from ',flnm
        write(*,*)'  ndim = ', ndim
        write(*,*)'  end_index(1) ',end_index(1)
        write(*,*)'  end_index(2) ',end_index(2)
        write(*,*)'  end_index(3) ',end_index(3)
      ENDIF
      call ext_int_read_field(dh2,DateStr,TRIM(VarName),data2,WRF_REAL,0,0,0,ord,   &
                          staggering,          &
                          dimnames,         &
                          start_index,end_index,                      & !dom              
                          start_index,end_index,                      & !mem
                          start_index,end_index,                      & !pat
                          ierr)
      IF ( ierr /= 0 ) THEN
	write(*,*)'Error reading ',Varname,' from ',flnm2
        write(*,*)'  ndim = ', ndim
        write(*,*)'  end_index(1) ',end_index(1)
        write(*,*)'  end_index(2) ',end_index(2)
        write(*,*)'  end_index(3) ',end_index(3)
      ENDIF

      IFDIFFS=0
      DO K = 1,end_index(3)-start_index(3)+1
       IF (LEVLIM.EQ.-1.OR.K.EQ.LEVLIM.OR.NDIM.eq.2) THEN
        cross = 0 
        IKDIFFS = 0
        do i = 1, end_index(1)-cross
          do j = 1, end_index(2)-cross
            a = data(I,J,K,1)
            b = data2(I,J,K,1)
            IF (a .ne. b) then
              IKDIFFS = IKDIFFS + 1
              IFDIFFS = IFDIFFS + 1
            ENDIF
          ENDDO
        ENDDO
        IF ( IKDIFFS .NE. 0 ) THEN
          EFOUND = .TRUE.
          PRINT*,'LEVEL ',K,' WITH ',IKDIFFS,' DIFFS'
            write(88,*)end_index(2),end_index(1),' ',name,' LEVEL ',K,' TIME ',N
            write(98,*)end_index(2),end_index(1),' ',name,' LEVEL ',K,' TIME ',N
            DO I = 1,end_index(1)
              DO J = 1,end_index(2)
                WRITE(88,*)data(I,J,K,1)
                WRITE(98,*)data2(I,J,K,1)
              ENDDO
            ENDDO
             write(88,*)end_index(2)-cross,end_index(1)-cross,' ',name,' LEVEL ',K,' TIME ',N
             write(98,*)end_index(2)-cross,end_index(1)-cross,' ',name,' LEVEL ',K,' TIME ',N
             DO I = 1,end_index(1)-cross
               DO J = 1,end_index(2)-cross
                 WRITE(88,*)data(I,J,K,1)
                 WRITE(98,*)data2(I,J,K,1)
               ENDDO
             ENDDO
          ENDIF
       ENDIF
      enddo
      IF (IFDIFFS .NE. 0 ) THEN
         PRINT *,'3D FIELD ID= ',name,'    cross= ',cross, '     N DIFFS= ',IFDIFFS
      ENDIF
      deallocate(data)
      deallocate(data2)

      call ext_int_get_next_var (dh1, VarName, Status_next_var)
    enddo
    call ext_int_get_next_time(dh1, DateStr, Status_next_time)
    call ext_int_get_next_time(dh2, DateStr2, Status_next_time2)
    IF ( DateStr .NE. DateStr2 ) THEN
      print*,'They differ big time.  Dates do not match'
      print*,'They differ big time.  Dates do not match'
      print*,'   ',flnm,' ',DateStr
      print*,'   ',flnm2,' ',DateStr2
      Status_next_time = 1
    ENDIF
  enddo

endif

end program readv3

logical function iveceq( a, b, n )
  implicit none
  integer n
  integer a(n), b(n)
  integer i
  iveceq = .true.
  do i = 1,n
    if ( a(i) .ne. b(i) ) iveceq = .false.
  enddo
  return
end function iveceq

! routines to satisfy unresolved references when compiling without WRF
subroutine wrf_error_fatal( str )
character*(*) str
write(0,*)'str'
write(*,*)'str'
stop
end

subroutine wrf_message( str )
character*(*) str
write(0,*)'str'
write(*,*)'str'
return
end


