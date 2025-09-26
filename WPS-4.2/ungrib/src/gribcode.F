!                                                                             !
!*****************************************************************************!
!                                                                             !
! This is a package of subroutines to read GRIB-formatted data.  It is still  !
! under continuous development. It will not be able to read every GRIB dataset!
! you give it, but it will read a good many.                                  !
!                                                                             !
!   Kevin W. Manning                                                          !
!   NCAR/MMM                                                                  !
!   Summer 1998, and continuing                                               !
!   SDG                                                                       !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! The main user interfaces are:                                               !
!                                                                             !
!  SUBROUTINE GRIBGET(NUNIT, IERR, ec_rec_len)                                !
!      Read a single GRIB record from UNIX file-descriptor NUNIT into array   !
!      GREC. No unpacking of any header or data values is performed.          !
!                                                                             !
!  SUBROUTINE GRIBREAD(NUNIT, DATA, NDATA, IERR, ec_rec_len)                  !
!      Read a single GRIB record from UNIX file-descriptor NUNIT, and unpack  !
!      all header and data values into the appropriate arrays.                !
!                                                                             !
!  SUBROUTINE GRIBHEADER                                                      !
!      Unpack the header of a GRIB record                                     !
!                                                                             !
!  SUBROUTINE GRIBDATA(DATARRAY, NDAT)                                        !
!      Unpack the data in a GRIB record into array DATARRAY                   !
!                                                                             !
!  SUBROUTINE GRIBPRINT(ISEC)                                                 !
!      Print the header information from GRIB section ISEC.                   !
!                                                                             !
!  SUBROUTINE GET_SEC1(KSEC1)                                                 !
!      Return the header information from Section 1.                          !
!                                                                             !
!  SUBROUTINE GET_SEC2(KSEC2)                                                 !
!      Return the header information from Section 2.                          !
!                                                                             !
!  SUBROUTINE GET_GRIDINFO(IGINFO, GINFO)                                     !
!      Return the grid information of the previously-unpacked GRIB header.    !
!                                                                             !
!                                                                             !
!*****************************************************************************!
!                                                                             !
!                                                                             !
! The following arrays have meanings as follows:                              !
!                                                                             !
!                                                                             !
! SEC0:    GRIB Header Section 0 information                                  !
!                                                                             !
!       1  :  Length of a complete GRIB record                                !
!       2  :  GRIB Edition number                                             !
!                                                                             !
!                                                                             !
! SEC1:    GRIB Header Section 1 information                                  !
!                                                                             !
!       1  :  Length of GRIB section 1 (bytes)                                !
!       2  :  Parameter Table Version number ????                             !
!       3  :  Center Identifier ????                                          !
!       4  :  Process Identifier ????                                         !
!       5  :  Grid ID number for pre-specified grids.                         !
!       6  :  Binary bitmap flag:                                             !
!       7  :  Parameter ID Number (ON388 Table 2)                             !
!       8  :  Level type (ON388 Table 3)                                      !
!       9  :  Level value, or top value of a layer                            !
!      10  :  Bottom value of a layer ( 0 if NA ??)                           !
!      11  :  Year (00-99)                                                    !
!      12  :  Month (01-12)                                                   !
!      13  :  Day of the month (01-31)                                        !
!      14  :  Hour (00-23)                                                    !
!      15  :  Minute (00-59)                                                  !
!      16  :  Forecast time unit: (ON388 Table 4)                             !
!      17  :  Time period 1:                                                  !
!      18  :  Time period 2:                                                  !
!      19  :  Time range indicator (ON833 Table 5)                            !
!      20  :  Number of ?? in an average ??                                   !
!      21  :  Number of ?? missing from average ??                            !
!      22  :  Century (Years 1999 and 2000 are century 20, 2001 is century 21)!
!      23  :  Sub-center identifier ??                                        !
!      24  :  Decimal scale factor for ???                                    !
!                                                                             !
!                                                                             !
!                                                                             !
!                                                                             !
!                                                                             !
! SEC2:    GRIB Header Section 2 information                                  !
!                                                                             !
!       1  :  Length of GRIB Section 2                                        !
!       2  :  Number of vertical-coordinate parameters ???                    !
!       3  :  Starting-point of the list of vertical-coordinate parameters ?? !
!       4  :  Data-representation type (i.e., grid type)  Table ???           !
!          :      0 = ??                                                      !
!          :      3 = Lambert-conformal grid.                                 !
!          :      5 = Polar-stereographic grid.                               !
!                                                                             !
!     if (SEC2(4) == 0) then            LATITUDE/LONGITUDE GRID               !
!                                                                             !
!         INFOGRID/GRIDINFO:                                                  !
!                                                                             !
!             1  :  I Dimension of the grid                                   !
!             2  :  J Dimension of the grid                                   !
!             3  :  Starting Latitude of the grid.                            !
!             4  :  Starting Longitude of the grid.                           !
!             5  :  Resolution and component flags.                           !
!             6  :  Ending latitude of the grid.                              !
!             7  :  Ending longitude of the grid.                             !
!             8  :  Longitudinal increment.                                   !
!             9  :  Latitudinal incriment.                                    !
!            10  :  Scanning mode (bit 3 from Table 8)                        !
!            21  :  Iscan sign (+1/-1) (bit 1 from Table 8)                   !
!            22  :  Jscan sign (+1/-1) (bit 2 from Table 8)                   !
!                                                                             !
!                                                                             !
!     elseif (SEC2(4) == 3) then        LAMBERT CONFORMAL GRID                !
!                                                                             !
!         INFOGRID/GRIDINFO:                                                  !
!                                                                             !
!             1  :  I Dimension of the grid                                   !
!             2  :  J Dimension of the grid                                   !
!             3  :  Starting Latitude of the grid.                            !
!             4  :  Starting Longitude of the grid.                           !
!             5  :  Resolution and component flags.                           !
!             6  :  Center longitude of the projection.                       !
!             7  :  Grid-spacing in the I direction                           !
!             8  :  Grid-spacing in the J direction                           !
!             9  :  Projection center                                         !
!            10  :  Scanning mode (bit 3 from Table 8)                        !
!            11  :  First TRUELAT value.                                      !
!            12  :  Second TRUELAT value.                                     !
!            13  :  Latitude of the southern pole ??                          !
!            14  :  Longitude of the southern pole ??                         !
!            21  :  Iscan sign (+1/-1) (bit 1 from Table 8)                   !
!            22  :  Jscan sign (+1/-1) (bit 2 from Table 8)                   !
!                                                                             !
!     if (SEC2(4) == 4) then            GAUSSIAN GRID                         !
!                                                                             !
!         INFOGRID/GRIDINFO:                                                  !
!                                                                             !
!             1  :  I Dimension of the grid                                   !
!             2  :  J Dimension of the grid                                   !
!             3  :  Starting Latitude of the grid.                            !
!             4  :  Starting Longitude of the grid.                           !
!             5  :  Resolution and component flags.                           !
!             6  :  Ending latitude of the grid.                              !
!             7  :  Ending longitude of the grid.                             !
!             8  :  Longitudinal increment.                                   !
!             9  :  Number of latitude circles between pole and equator       !
!            10  :  Scanning mode (bit 3 from Table 8)                        !
!            17  :  Original (stored) ending latitude                         !
!            18  :  Original (stored) starting latitude                       !
!            19  :  Approximate delta-latitude                                !
!            21  :  Iscan sign (+1/-1) (bit 1 from Table 8)                   !
!            22  :  Jscan sign (+1/-1) (bit 2 from Table 8)                   !
!                                                                             !
!                                                                             !
!     elseif (SEC2(4) == 5) then        POLAR STEREOGRAPHIC GRID              !
!                                                                             !
!         INFOGRID/GRIDINFO:                                                  !
!                                                                             !
!             1  :  I Dimension of the grid                                   !
!             2  :  J Dimension of the grid                                   !
!             3  :  Starting Latitude of the grid.                            !
!             4  :  Starting Longitude of the grid.                           !
!             5  :  Resolution and component flags.                           !
!             6  :  Center longitude of the projection.                       !
!             7  :  Grid-spacing in the I direction                           !
!             8  :  Grid-spacing in the J direction                           !
!             9  :  Projection center                                         !
!            10  :  Scanning mode (bit 3 from Table 8)                        !
!            21  :  Iscan sign (+1/-1) (bit 1 from Table 8)                   !
!            22  :  Jscan sign (+1/-1) (bit 2 from Table 8)                   !
!                                                                             !
!     elseif (SEC2(4) == 50) then       SPHERICAL HARMONIC COEFFICIENTS       !
!                                                                             !
!         INFOGRID/GRIDINFO:                                                  !
!                                                                             !
!             1  :  J-pentagonal resolution parameter                         !
!             2  :  K-pentagonal resolution parameter                         !
!             3  :  M-pentagonal resolution parameter                         !
!             4  :  Spectral representation type (ON388 Table 9)              !
!             5  :  Coefficient storage mode (ON388 Table 10)                 !
!                                                                             !
!     elseif (SEC2(4) == ?) then        ??                                    !
!                                                                             !
!                                                                             !
! SEC3:    GRIB Header Section 3 information:                                 !
! SEC4:    GRIB Header Section 4 information:                                 !
!
!
!
module module_grib
!
! Machine wordsize must be known for the various unpacking routines to work.
! Machine wordsize is set through CPP Directives.
! Use options -DBIT32 (for 32-bit word-size) or -DBIT64 (for 64-bit wordsize) 
! for the CPP pass of the compiler.
!
#if defined (BIT32)
  integer, parameter :: MWSIZE = 32 ! Machine word size in bits
#elif defined (BIT64)
  integer, parameter :: MWSIZE = 64 ! Machine word size in bits
#endif


! Array GREC holds a single packed GRIB record (header and all).
! Array BITMAP holds the bitmap (if a bitmap was used).
!
! For some reason, the cray does not like grec to be allocatable.
!
#if defined (CRAY) 
  integer, dimension(100000) :: grec
  integer, dimension(100000) :: bitmap
#else
  integer, allocatable, save, dimension(:) :: grec
  integer, allocatable, save, dimension(:) :: bitmap
#endif

! SEC0 holds the Section 0 header information
! SEC1 holds the Section 1 header information
! SEC2 holds the Section 2 header information
! SEC3 holds the Section 3 header information
! SEC4 holds the Section 4 header information
! XEC4 holds floating-point Section 4 header information

  integer, dimension(2) :: sec0
  integer, dimension(100) :: sec1
  integer, dimension(10) :: sec2
  integer, dimension(10) :: sec3
  integer, dimension(10) :: sec4
  real, dimension(1) :: xec4

  integer :: sevencount = 0

! INFOGRID holds integer values defining the grid.
! GRIDINFO holds floating-point values definint the grid

  integer, dimension(40) :: infogrid
  real, dimension(40) :: gridinfo

  integer :: ied
  real, parameter :: pi = 3.1415926534
  real, parameter :: degran = pi/180.
  real, parameter :: raddeg = 1./degran

  real :: glat1, glon1, gclon, gtrue1, gtrue2, grrth, gx1, gy1, gkappa

contains
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  integer function gribsize(trec, ilen, ierr, ec_rec_len)
!-----------------------------------------------------------------------------!
! Return the size of a single GRIB record.                                    !
!                                                                             !
! Input:                                                                      !
!    TREC: At least a portion of the complete GRIB record.                    !
!    ILEN: The size of array TREC.                                            !
!                                                                             !
! Output                                                                      !
!    GRIBSIZE: The size of the full GRIB record                               !
!    IERR    : 0= no errors, 1 = read error
!                                                                             !
! Side Effects:                                                               !
!    * Module variable IED is set to the GRIB Edition number.                 !
!    * STOP, if not GRIB Edition 0 or 1                                      !
!                                                                             !
! Externals                                                                   !
!    GBYTE                                                                    !
!                                                                             !
!-----------------------------------------------------------------------------!
    implicit none
    integer :: ilen, ec_rec_len
    integer, dimension(ilen) :: trec
    integer :: isz0 = 32
    integer :: isz1 = 0
    integer :: isz2 = 0
    integer :: isz3 = 0
    integer :: isz4 = 0
    integer :: isz5 = 32
    integer :: iflag
    integer :: ierr
    character :: pname*132

    ierr = 0
! Unpack the GRIB Edition number, located in the eighth byte (bits 57-64)     !
! of array TREC.                                                              !

    call gbyte_g1(trec, ied, 56, 8)

! GRIB Edition 1 has the size of the whole GRIB record right up front.        !

    if (ied.eq.1) then
       ! Grib1
       ! Find the size of the whole GRIB record
       if ( ec_rec_len .ne. 0 ) then
         gribsize = ec_rec_len
       else
         call gbyte_g1(trec, gribsize, 32, 24)
       endif

! GRIB Edition 0 does not include the total size, so we have to sum up        !
! the sizes of the individual sections                                        !

    elseif (ied.eq.0) then
       ! Grib old edition
       ! Find the size of section 1.
       call gbyte_g1(trec, isz1, isz0, 24)
       isz1 = isz1 * 8
       call gbyte_g1(trec, iflag, isz0+56, 8)
       if ((iflag.eq.128).or.(iflag.eq.192)) then ! section 2 is there
          ! Find the size of section 2.
          call gbyte_g1(trec, isz2, isz0+isz1, 24)
          isz2 = isz2 * 8
       endif
       if ((iflag.eq.64).or.(iflag.eq.192)) then ! Section 3 is there
          ! Find the size of section 3.
          call gbyte_g1(trec, isz3, isz0+isz1+isz2, 24)
          isz3 = isz3 * 8
       endif
       ! Find the size of section 4.
       call gbyte_g1(trec, isz4, isz0+isz1+isz2+isz3, 24)
       isz4 = isz4 * 8

       ! Total the sizes of sections 0 through 5.
       gribsize = (isz0+isz1+isz2+isz3+isz4+isz5) / 8

    elseif (ied.eq.2) then
       ! Grib2
       CALL getarg ( 0 , pname )
       write(*,'("*** stopping in gribcode ***\n")')
       write(*,'("\tI was expecting a Grib1 file, but this is a Grib2 file.")')
       if ( index(pname,'ungrib.exe') .ne. 0 ) then
         write(*,'("\tIt is possible this is because your GRIBFILE.XXX files")')
         write(*,'("\tare not all of the same type.")')
         write(*,'("\tWPS can handle both file types, but a separate ungrib")')
         write(*,'("\tjob must be run for each Grib type.\n")')
       else
         write(*,'("\tUse g2print on Grib2 files\n")')
       endif
       stop 'gribsize in gribcode'
    else
       write(*,'("Error trying to read grib edition number in gribsize.")')
       write(*,'("Possible corrupt grib file.")')
       write(6,*) 'Incorrect edition number  = ',ied
       write(6,*) 'Skipping the rest of the file and continuing.'
       ierr = 1
    endif
  end function gribsize
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  subroutine findgrib(nunit, isize, ierr, ec_rec_len)

!-----------------------------------------------------------------------------!
!                                                                             !
! Find the string "GRIB", which starts off a GRIB record.                     !
!                                                                             !
! Input:                                                                      !
!    NUNIT:  The C unit to read from.  This should already be opened.         !
!                                                                             !
! Output:                                                                     !
!    ISIZE:  The size in bytes of one complete GRIB Record                    !
!    IERR:   Error flag,                                                      !
!              0 : No error or end-of-file on reading                         !
!              1 : Hit the end of file                                        !
!              2 : Error on reading                                           !
!                                                                             !
! Side effects:                                                               !
!   * The pointer to C unit NUNIT is set to the beginning of the next         !
!     GRIB record.                                                            !
!   * The first time FINDGRIB is called, the integer GTEST is set to          !
!     a value equivalent to the string 'GRIB'                                 !
!                                                                             !
! Modules:                                                                    !
!     MODULE_GRIB                                                             !
!                                                                             !
! Externals:                                                                  !
!     BN_READ                                                                 !
!     BN_SEEK                                                                 !
!     GRIBSIZE                                                                !
!                                                                             !
!-----------------------------------------------------------------------------!
    implicit none
    integer, intent(in) :: nunit, ec_rec_len
    integer, intent(out) :: isize
    integer, intent(out) :: ierr

    integer, parameter :: LENTMP=100
    integer, dimension(lentmp) :: trec

    integer :: isz, itest, icnt

    integer, save :: gtest = 0

! Set the integer variable GTEST to hold the integer representation of the
! character string 'GRIB'.   This integer variable is later compared to
! integers we read from the GRIB file, to find the beginning of a GRIB record.

    if (gtest.eq.0) then
       if (mwsize.eq.32) then
          gtest = transfer('GRIB', gtest)
       elseif(mwsize.eq.64) then
          call gbyte_g1(char(0)//char(0)//char(0)//char(0)//'GRIB', gtest, 0, mwsize)
       endif
    endif
    ierr = 0
    icnt = 0

    LOOP : DO
! Read LENTMP bytes into holding array TREC.
       call bn_read(nunit, trec, lentmp, isz, ierr, 0)
       if (ierr.eq.1) then
          return
       elseif (ierr.eq.2) then
          write(*,'("Error reading GRIB: IERR = ", I2)') ierr
          return
       endif
! Reposition the file pointer back to where we started.
       call bn_seek(nunit, -isz, 0, 0)

! Compare the first four bytes of TREC with the string 'GRIB' stored in 
! integer variable GTEST.
       if (mwsize.eq.32) then
          if (trec(1) == gtest) exit LOOP
       elseif (mwsize.eq.64) then
          call gbyte_g1(trec, itest, 0, 32)
          if (itest == gtest) exit LOOP
       endif

! Advance the file pointer one byte.
       call bn_seek(nunit, 1, 0, 0)
       icnt = icnt + 1
       if ( icnt .gt. 100000) then       ! stop if we cannot find the GRIB string
         write(*,'("*** stopping in findgrib in gribcode ***\n")')
         write(*,'("\tI could not find the GRIB string in the input file")')
         write(*,'("\tafter testing the first 100,000 bytes.")')
         write(*,'("\tThe file may be corrupt or it is not a GRIB file.")')
         write(*,'("\tPerhaps a gzipped GRIB file or netcdf?\n")')
         stop 'findgrib'
       endif

    ENDDO LOOP

!#if defined (DEC) || defined (ALPHA) || defined (alpha) || defined (LINUX)
#ifdef BYTESWAP
      call swap4(trec, isz)
#endif
    isize = gribsize(trec, isz, ierr, ec_rec_len)

  end subroutine findgrib
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  subroutine SGUP_NOBITMAP(datarray, ndat)
! Simple grid-point unpacking
    implicit none

    integer :: ndat
    real , dimension(ndat) :: datarray
    integer, dimension(ndat) :: IX
    real :: dfac, bfac
    integer :: iskip

    DFAC = 10.**(-sec1(24))
    BFAC = 2.**sec4(7)
    if (ied.eq.0) then
       iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    elseif (ied.eq.1) then
       iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    endif
! sec4(8) is the number of bits used per datum value.
! If sec4(8) = 255, assume they mean sec4(8) = 0
    if (sec4(8) == 255) then
       sec4(8) = 0
    endif
! If sec4(8) is 0, assume datarray is constant value of xec4(1)

    if (sec4(8).eq.0) then
       !!! HERE IS THE ORIGINAL NCAR CODE: 
       ! datarray = xec4(1)
       !!! HERE IS WHAT FSL CHANGED IT TO:
       datarray = DFAC*xec4(1)
       !!! because even though it is a constant value
       !!! you still need to scale by the decimal scale factor.
    else
       !!! FSL developers MOVED THE CALL TO gbytes FROM line 441 ABOVE 
       !!! BECAUSE IF sec4(8)==0 BEFORE gbytes IS CALLED, THE MASKS ARRAY
       !!! IN gbytes WILL BE INDEXED OUT OF BOUNDS. C HARROP 9/16/04
       call gbytes_g1(grec, IX, iskip, sec4(8), 0, ndat)
       datarray = DFAC * (xec4(1) + (IX*BFAC))
    endif
  end subroutine SGUP_NOBITMAP
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!

  subroutine SGUP_BITMAP(datarray, ndat)
! Simple grid-point unpacking, with a bitmap.
    implicit none

    integer :: ndat ! Number of data points in the final grid.
    real , dimension(ndat) :: datarray ! Array holding the final unpacked data.
    real :: dfac, bfac
    integer :: iskip, nbm, i, nn

    integer, allocatable, dimension(:) :: bmdat

! SEC4(1) : The number of bytes in the whole of GRIB Section 4.
! SEC4(6) : The number of unused bits at the end of GRIB Section 4.
! SEC4(8) : The number of bits per data value.

    datarray = -1.E30

! 1) There are fewer than NDAT data values, because a bitmap was used.  
!    Compute the number of data values (NBM).  There are 11 extra bytes
!    in the header section 4.  NBM equals the total number of data bits (not
!    counting the header bits), minus the number of unused buts, and then
!    divided by the number of bits per data value.

! Compute the parameters involved with packing
    DFAC = 10.**(-sec1(24))
    BFAC = 2.**sec4(7)

! If sec4(8) is 0, assume datarray is constant value of xec4(1) scaled by DFAC

    if (sec4(8).eq.0) then
       where(bitmap(1:ndat).eq.1) datarray = xec4(1) * DFAC
       return
    endif
    nbm = ((sec4(1)-11)*8-sec4(6))/sec4(8)
    allocate(bmdat(nbm))

! Set ISKIP to the beginning of the data.
    if (ied.eq.0) then
       iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    elseif (ied.eq.1) then
       iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    endif

! Read the data from the GREC array
    call gbytes_g1(grec, bmdat, iskip, sec4(8), 0, nbm)
! sec4(8) is the number of bits used per datum value.
! If sec4(8) = 255, assume they mean sec4(8) = 0
    if (sec4(8) == 255) sec4(8) = 0

! Unpack the data according to packing parameters DFAC, BFAC, and XEC4(1), 
! and masked by the bitmap BITMAP.
       nn = 0
       do i = 1, ndat
          if (bitmap(i).eq.1) then
             nn = nn + 1
             datarray(i) = DFAC * (xec4(1) + (bmdat(nn)*BFAC))
          endif
       enddo

! Deallocate the scratch BMDAT array
    deallocate(bmdat)

  end subroutine SGUP_BITMAP
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!

  subroutine CSHUP(pdata, ndat)
! ECMWFs unpacking of ECMWFs Complex Spherical Harmonic packing
! Adapted from ECMWFs GRIBEX package.
    implicit none

    integer :: ndat
    real , dimension(ndat) :: pdata
    integer, dimension(ndat+500) :: IX

    integer :: iskip, isign
    integer :: N1, IPOWER, J, K, M, nval
    real :: zscale, zref

    integer :: ic, jm, iuc, il2, inum, jn
    integer :: inext, ilast, ioff, jrow, index, i, jcol
    real :: bval
    integer, allocatable, dimension(:) :: iexp, imant
    real , dimension(0:400) :: factor
    real :: power
    integer :: N

    index = -1

    if (ied.eq.0) then
       iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    elseif(ied.eq.1) then
       iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    endif

    call gbyte_g1(grec,N1,iskip,16)
    iskip = iskip + 16

    call gbyte_g1(grec,ipower,iskip,16)
    iskip = iskip + 16
    if (ipower.ge.32768) ipower = 32768-ipower

! Unpack the resolution parameters for the initial (small) truncation:
    call gbyte_g1(grec,J,iskip,8)
    iskip = iskip + 8
    call gbyte_g1(grec,K,iskip,8)
    iskip = iskip + 8
    call gbyte_g1(grec,M,iskip,8)
    iskip = iskip + 8

    zscale = 2.**sec4(7)

    iskip = N1*8

    nval = NDAT - (J+1)*(J+2)

    call gbytes_g1(grec, ix, iskip, sec4(8), 0, nval)
! sec4(8) is the number of bits used per datum value.
! If sec4(8) = 255, assume they mean sec4(8) = 0
    if (sec4(8) == 255) sec4(8) = 0

    pdata(1:nval) = (float(ix(1:nval))*zscale)+xec4(1)

    IUC = NDAT+1
    IC  = NVAL+1
    DO JM=INFOGRID(1),0,-1
       IL2=MAX(JM,J+1)
       INUM=2*(INFOGRID(1)-IL2+1)
       pdata(iuc-inum:iuc-1) = pdata(ic-inum:ic-1)
       iuc = iuc - inum
       ic = ic - inum
       IUC = IUC-MAX((IL2-JM)*2,0)
    ENDDO

    if (ied.eq.0) then
       iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 11*8
    elseif (ied.eq.1) then
       iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8 + 18*8
    endif

    allocate(iexp(802))
    allocate(imant(802))
    ilast=j+1
    do jrow=1,ilast
       inext = 2*(ilast-jrow+1)
       ! extract all the exponents
       call gbytes_g1(grec, iexp, iskip, 8, 24, inext)
       ! extract all the mantissas
       ioff = 8
       call gbytes_g1(grec, imant, iskip+8, 24, 8, inext)
       iskip = iskip + inext*32

       ! Build the real values from mantissas and exponents
       bval = 2.**(-24)
       i = 0
       do jcol = jrow, infogrid(1)+1
          index = index + 2
          if (ilast.ge.jcol) then
             i = i + 1
             if ((iexp(i).eq.128.or.iexp(i).eq.0).and.(imant(i).eq.0)) then
                pdata(i) = 0
             else
                if (iexp(i).ge.128) then
                   iexp(i) = iexp(i) - 128
                   isign = -1
                else
                   isign = 1
                endif
                pdata(index) = isign*bval*IMANT(i)*16.**(IEXP(i)-64)
                i = i + 1
                if (iexp(i).ge.128) then
                   iexp(i) = iexp(i) - 128
                   isign = -1
                else
                   isign = 1
                endif
                pdata(index+1) = isign*bval*IMANT(i)*16.**(IEXP(i)-64)
             endif
          endif
       enddo
    enddo

    !Apply power scaling:

    if (ipower.ne.0) then
       power = float(ipower) / 1000.0
       factor(0) = 1.0
       do n = 1 , infogrid(1)
          if( ipower .ne. 1000 ) then
             factor(n) = 1.0 / (n * (n+1) )**power
          else
             factor(n) = 1.0 / (n * (n + 1))
          endif
       enddo
       INDEX = -1
       DO M = 0 , J-1
          DO N = M , INFOGRID(1)
             INDEX = INDEX + 2
             IF ( N .GE. J ) THEN
                PDATA(INDEX:INDEX+1) = PDATA(INDEX:INDEX+1) * FACTOR(N)
             ENDIF
          ENDDO
       ENDDO
       DO M = J , INFOGRID(1)
          DO N = M , INFOGRID(1)
             INDEX = INDEX + 2
             PDATA(INDEX:INDEX+1)   = PDATA(INDEX:INDEX+1)   * FACTOR(N)
          ENDDO
       ENDDO
    endif

  end subroutine CSHUP
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
!
!
! Trigonometric functions which deal with degrees, rather than radians:
!
  real function sind(theta)
    real :: theta
    sind = sin(theta*degran)
  end function sind
  real function cosd(theta)
    real :: theta
    cosd = cos(theta*degran)
  end function cosd
  real function tand(theta)
    real :: theta
    tand = tan(theta*degran)
  end function tand
  real function atand(x)
    real :: x
    atand = atan(x)*raddeg
  end function atand
  real function atan2d(x,y)
    real :: x,y
    atan2d = atan2(x,y)*raddeg
  end function atan2d
  real function asind(x)
    real :: x
    asind = asin(x)*raddeg
  end function asind
  real function acosd(x)
    real :: x
    acosd = acos(x)*raddeg
  end function acosd

!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
end module module_grib
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribget(nunit, ierr, ec_rec_len)
  use module_grib
!-----------------------------------------------------------------------------!
!                                                                             !
! Read a single GRIB record, with no unpacking of any header or data fields.  !
!                                                                             !
! Input:                                                                      !
!     NUNIT:  C unit number to read from.  This should already be open.       !
!                                                                             !
! Output:                                                                     !
!     IERR: Error flag, Non-zero means there was a problem with the read.     !
!                                                                             !
! Side Effects:                                                               !
!        The array GREC is allocated, and filled with one GRIB record.        !
!        The C unit pointer is moved to the end of the GRIB record just read. !
!                                                                             !
! Modules:                                                                    !
!       MODULE_GRIB                                                           !
!                                                                             !
! Externals:                                                                  !
!       FINDGRIB                                                              !
!       BN_READ                                                               !
!                                                                             !
!-----------------------------------------------------------------------------!

  implicit none

  integer :: nunit, ec_rec_len
  integer :: ierr
  integer :: isz, isize

! Position the file pointer at the beginning of the GRIB record.
  call findgrib(nunit, isize, ierr, ec_rec_len)
  if (ierr.ne.0) return

! Allocate the GREC array to be able to hold the data

#if defined (CRAY)
#else
  allocate(grec((isize+(mwsize/8-1))/(mwsize/8)))
#endif

! Read the full GRIB record.

  call bn_read(nunit, grec, isize, isz, ierr, 1)

!#if defined (DEC) || defined (ALPHA) || defined (alpha) || defined (LINUX)
#ifdef BYTESWAP
      call swap4(grec, isz)
#endif


end subroutine gribget
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribread(nunit, data, ndata, debug_level, ierr, ec_rec_len)
!-----------------------------------------------------------------------------!
! Read one grib record, unpack the header and data information.               !
!                                                                             !
! Input:                                                                      !
!    NUNIT:  C Unit to read from.                                             !
!    NDATA:  Size of array DATA (Should be >= NDAT as computed herein.)       !
!                                                                             !
! Output:                                                                     !
!    DATA:  The unpacked data array                                           !
!    IERR:  Error flag, non-zero means there was a problem.                   !
!                                                                             !
! Side Effects:                                                               !
!    * Header arrays SEC0, SEC1, SEC2, SEC3, SEC4, XEC4, INFOGRID and         !
!      INFOGRID are filled.                                                   !
!    * The BITMAP array is filled.                                            !
!    * The C unit pointer is advanced to the end of the GRIB record.          !
!                                                                             !
! Modules:                                                                    !
!      MODULE_GRIB                                                            !
!                                                                             !
! Externals:                                                                  !
!      GRIBGET                                                                !
!      GRIBHEADER                                                             !
!      GRIBDATA                                                               !
!                                                                             !
!-----------------------------------------------------------------------------!
  use module_grib

  implicit none

  integer :: nunit, ec_rec_len
  integer :: debug_level
  integer :: ierr
  real, allocatable, dimension(:) :: datarray
  integer :: ndata
  real, dimension(ndata) :: data

  integer :: ni, nj

  ierr = 0

  call gribget(nunit, ierr, ec_rec_len)
  if (ierr.ne.0) return

! Unpack the header information

  call gribheader(debug_level,ierr, ec_rec_len)

! Determine the size of the data array from the information in the header, 
! and allocate the array DATARRAY to hold that data.

  if (sec2(4).ne.50) then
     ni = infogrid(1)
     nj = infogrid(2)
     allocate(datarray(ni*nj))
  else
     ni = (infogrid(1)+1) * (infogrid(1)+2)
     nj = 1
     allocate(datarray(ni*nj))
  endif

! Unpack the data from the GRIB record, and fill the array DATARRAY.

  call gribdata(datarray, ni*nj) 

  data(1:ni*nj) = datarray(1:ni*nj)
#if defined (CRAY)
#else
  deallocate(grec, datarray)
#endif

end subroutine gribread
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine get_sec1(ksec1)
! Return the GRIB Section 1 header information, which has already been
! unpacked by subroutine GRIBHEADER.
  use module_grib
  integer, dimension(100) :: ksec1
  ksec1 = sec1
end subroutine get_sec1
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine get_sec2(ksec2)
! Return the GRIB Section 2 header information, which has already been
! unpacked by subroutine GRIBHEADER.
  use module_grib
  integer, dimension(10) :: ksec2
  ksec2 = sec2
end subroutine get_sec2
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine get_gridinfo(iginfo, ginfo)
  use module_grib
  integer, dimension(40) :: iginfo
  real, dimension(40) :: ginfo
  iginfo = infogrid
  ginfo = gridinfo
end subroutine get_gridinfo
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribprint(isec)
  use module_grib
  implicit none
  integer :: isec
  integer :: ou = 6
  character(len=12) :: string = ',t45,":",i8)'
  character(len=15) :: rstring = ',t45,":",f12.5)'

  if (isec.eq.0) then
     write(*,'(/,"GRIB SECTION 0:")')
     write(ou,'(5x,"Grib Length"'//string) sec0(1)
     write(ou,'(5x,"Grib Edition"'//string) sec0(2)
  else if (isec.eq.1) then
     write(*,'(/,"GRIB SECTION 1:")')
     write(ou,'(5x,"Length of PDS"'//string) sec1(1)
     write(ou,'(5x,"Parameter Table Version"'//string) sec1(2)
     write(ou,'(5x,"Center ID"'//string) sec1(3)
     write(ou,'(5x,"Process ID"'//string) sec1(4)
     write(ou,'(5x,"Grid ID"'//string) sec1(5)
     if (sec1(25) == 1) then
        write(ou,'(5x,"Is there a Grid Desc. Section (GDS)?",t45,":     Yes")')
     else if (sec1(25) == 0) then
        write(ou,'(5x,"Is there a Grid Desc. Section (GDS)?",t45,":      No")')
     else
        print*, 'Unrecognized sec1(25): ', sec1(25)
     endif
     if (sec1(26) == 1) then
        write(ou,'(5x,"Is there a Bit Map Section (BMS)?",t45,":     Yes")')
     else if (sec1(26) == 0) then
        write(ou,'(5x,"Is there a Bit Map Section (BMS)?",t45,":      No")')
     else
        print*, 'Unrecognized sec1(26): ', sec1(26)
     endif
     write(ou,'(5x,"Parameter"'//string) sec1(7)
     write(ou,'(5x,"Level type"'//string) sec1(8)
     if ( (sec1(8) == 101) .or. (sec1(8) == 104) .or. (sec1(8) == 106) .or. &
          (sec1(8) == 108) .or. (sec1(8) == 110) .or. (sec1(8) == 112) .or. &
          (sec1(8) == 114) .or. (sec1(8) == 116) .or. (sec1(8) == 120) .or. &
          (sec1(8) == 121) .or. (sec1(8) == 128) .or. (sec1(8) == 141) ) then
        write(ou,'(5x,"Hgt, pres, etc. of layer top "'//string) sec1(9)
        write(ou,'(5x,"Hgt, pres, etc. of layer bottom "'//string) sec1(10)
     else
        write(ou,'(5x,"Height, pressure, etc "'//string) sec1(9)
     endif
     write(ou,'(5x,"Year"'//string) sec1(11)
     write(ou,'(5x,"Month"'//string) sec1(12)
     write(ou,'(5x,"Day"'//string) sec1(13)
     write(ou,'(5x,"Hour"'//string) sec1(14)
     write(ou,'(5x,"Minute"'//string) sec1(15)
     write(ou,'(5x,"Forecast time unit"'//string) sec1(16)
     write(ou,'(5x,"P1"'//string) sec1(17)
     write(ou,'(5x,"P2"'//string) sec1(18)
     write(ou,'(5x,"Time Range Indicator"'//string) sec1(19)
     write(ou,'(5x,"Number in Ave?"'//string) sec1(20)
     write(ou,'(5x,"Number missing from ave?"'//string) sec1(21)
     write(ou,'(5x,"Century"'//string) sec1(22)
     write(ou,'(5x,"Sub-center"'//string) sec1(23)
     write(ou,'(5x,"Decimal scale factor"'//string) sec1(24)
  elseif ((isec.eq.2) .and. ((sec1(6).eq.128).or.(sec1(6).eq.192))) then
     write(*,'(/,"GRIB SECTION 2:")')
     write(ou,'(5x,"Length of GRID Desc. Section"'//string) sec2(1)
     if ((sec2(2) /= 0).or.(sec2(3) /= 0) .or. (sec2(4) /= 0)) then
        write(ou,'(5x,"Number of V. Coordinate Parms"'//string) sec2(2)
        write(ou,'(5x,"List Starting point"'//string) sec2(3)
        write(ou,'(5x,"Data Representation type"'//string) sec2(4)
     endif

     if (sec2(4).eq.0) then
        write(ou,'(5x,"Cylindrical Equidistant Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component:", t45,":",B8.8)') infogrid(5)
        write(ou,'(10x,"Lat NI"'//string) infogrid(6)
        write(ou,'(10x,"Lon NJ"'//string) infogrid(7)
        write(ou,'(10x,"Delta-Lon"'//string) infogrid(8)
        write(ou,'(10x,"Delta-Lat"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)

     else if (sec2(4).eq.1) then
        write(ou,'(5x,"Mercator Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Lat NI"'//rstring) gridinfo(6)
        write(ou,'(10x,"Lon NJ"'//rstring) gridinfo(7)
        write(ou,'(10x,"Dx"'//rstring) gridinfo(8)
        write(ou,'(10x,"Dy"'//rstring) gridinfo(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"Latin"'//rstring) gridinfo(11)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)

     else if (sec2(4).eq.4) then
        write(ou,'(5x,"Gaussian Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Original (stored) Lat 1"'//rstring) gridinfo(18)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Original (stored) Lat NI"'//rstring) gridinfo(17)
        write(ou,'(10x,"Lat NI"'//rstring) gridinfo(6)
        write(ou,'(10x,"Lon NJ"'//rstring) gridinfo(7)
        write(ou,'(10x,"Delta-Lon"'//rstring) gridinfo(8)
        write(ou,'(10x,"Delta-Lat"'//rstring) gridinfo(19)
        write(ou,'(10x,"Number of lats (pole - eq)"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)
     elseif (sec2(4).eq.3) then
        write(ou,'(5x,"Lambert Conformal Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//string) infogrid(3)
        write(ou,'(10x,"Lon 1"'//string) infogrid(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Lov"'//string) infogrid(6)
        write(ou,'(10x,"Dx"'//string) infogrid(7)
        write(ou,'(10x,"Dy"'//string) infogrid(8)
        write(ou,'(10x,"Projection center"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)
        write(ou,'(10x,"Latin 1"'//string) infogrid(11)
        write(ou,'(10x,"Latin 2"'//string) infogrid(12)
        write(ou,'(10x,"Lat of southern pole"'//string) infogrid(13)
        write(ou,'(10x,"Lon of southern pole"'//string) infogrid(14)
     elseif (sec2(4).eq.5) then
        write(ou,'(5x,"Polar Stereographic Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//string) infogrid(3)
        write(ou,'(10x,"Lon 1"'//string) infogrid(4)
        write(ou,'(10x,"Resolution and Component", t45,":",B8.8)') infogrid(5)
        write(ou,'(10x,"Lov"'//string) infogrid(6)
        write(ou,'(10x,"Dx"'//string) infogrid(7)
        write(ou,'(10x,"Dy"'//string) infogrid(8)
        write(ou,'(10x,"Projection center"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)
     elseif (sec2(4).eq.50) then
        write(ou,'(5x,"Spherical harmonic components")')
        write(ou,'(10x,"J-Pentagonal resolution parm:"'//string) infogrid(1)
        write(ou,'(10x,"K-Pentagonal resolution parm:"'//string) infogrid(2)
        write(ou,'(10x,"M-Pentagonal resolution parm:"'//string) infogrid(3)
        write(ou,'(10x,"Representation type"'//string) infogrid(4)
        write(ou,'(10x,"Coefficient storage mode"'//string) infogrid(5)
     endif
  elseif ((isec.eq.3) .and. (sec1(26).eq.1)) then
     write(*,'(/,"GRIB SECTION 3:")')
     write(ou,'(5x,"Length of bit map section"'//string) sec3(1)
     write(ou,'(5x,"Number of unused bits"'//string) sec3(2)
     write(ou,'(5x,"Numeric"'//string) sec3(3)

  elseif (isec.eq.4) then
     write(*,'(/,"GRIB SECTION 4:")')
     write(ou,'(5x,"Length of BDS"'//string) sec4(1)
     write(ou,'(5x,"0/1: grid-point or sph. harm. data"'//string) sec4(2)
     write(ou,'(5x,"0/1: simple or complex packing"'//string) sec4(3)
     write(ou,'(5x,"0/1: floating or integer"'//string) sec4(4)
     write(ou,'(5x,"0/1: No addl flags or addl flags"'//string) sec4(5)
     write(ou,'(5x,"Unused bits"'//string) sec4(6)
     write(ou,'(5x,"Binary Scale Factor"'//string) sec4(7)
     write(ou,'(5x,"Reference Value", t45, ":", F18.8)') xec4(1)
     write(ou,'(5x,"Number of bits for packing"'//string) sec4(8)
  endif

end subroutine gribprint
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine get_bitmap(bm8, ndat)
  use module_grib
  integer, dimension(ndat) :: bm8
  if ((sec1(6).eq.64).or.(sec1(6).eq.192)) then
     bm8 = bitmap
  else
     bm8 = 1
  endif
end subroutine get_bitmap
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribxyll(x, y, xlat, xlon)
  use module_grib
  implicit none

  real , intent(in) :: x, y
  real , intent(out) :: xlat, xlon

  real :: r, xkm, ykm, y1
  integer :: iscan, jscan

  if (sec2(4).eq.0) then ! Cylindrical equidistant grid

     xlat = gridinfo(3) + gridinfo(9)*(y-1.)
     xlon = gridinfo(4) + gridinfo(8)*(x-1.)

  elseif (sec2(4) == 1) then      ! Mercator grid
     r = grrth*cosd(gtrue1)
     xkm = (x-1.)*gridinfo(8)
     ykm = (y-1.)*gridinfo(9)
     xlon = gridinfo(4) + (xkm/r)*(180./pi)
     y1 = r*alog((1.+sind(gridinfo(3)))/cosd(gridinfo(3)))/gridinfo(9)
     xlat = 90. - 2. * atan(exp(-gridinfo(9)*(y+y1-1.)/r))*180./pi

  elseif (sec2(4) == 3) then      ! Lambert Conformal grid
     gclon = gridinfo(6)
     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = 90. - 2.*atand(tand(45.-gtrue1/2.)* &
          ((r*gkappa*gridinfo(7))/(grrth*sind(90.-gtrue1)))**(1./gkappa))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1))/gkappa + gclon

  elseif (sec2(4) == 5) then  ! Polar Stereographic grid
     gclon = gridinfo(6)
     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = 90. - 2.*atan2d((r*gridinfo(7)),(grrth*(1.+sind(gtrue1))))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1)) + gclon

  elseif (sec2(4) == 4) then ! Gaussian grid

     xlon = gridinfo(4) + gridinfo(8)*(x-1.)
     xlat = gridinfo(3) + gridinfo(19)*(y-1.)

  else
     write(*,'("Unrecognized projection:", I10)') sec2(4)
     write(*,'("STOP in GRIBXYLL")')
     stop
  endif

end subroutine gribxyll
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribllxy(xlat, xlon, x, y)
  use module_grib
  implicit none
  real , intent(in) :: xlat, xlon
  real , intent(out) :: x, y

  real :: r, y1

  if (sec2(4) == 0) then      ! Cylindrical Equidistant grid

     x = 1. + (xlon-gridinfo(4)) / gridinfo(9)
     y = 1. + (xlat-gridinfo(3)) / gridinfo(8)

  else if (sec2(4) == 1) then      ! Mercator grid

     r = grrth*cosd(gtrue1)
     x = 1.+( (r/gridinfo(8)) * (xlon-gridinfo(4)) * (pi/180.) )
     y1 = (r/gridinfo(9))*alog((1.+sind(gridinfo(3)))/cosd(gridinfo(3)))
     y = 1. + ((r/gridinfo(9))*alog((1.+sind(xlat))/cosd(xlat)))-y1

  else if (sec2(4) == 3) then      ! Lambert Conformal grid
     gclon = gridinfo(6)
     r = grrth/(gridinfo(7)*gkappa)*sind(90.-gtrue1) * &
          (tand(45.-xlat/2.)/tand(45.-gtrue1/2.)) ** gkappa
     x =  r*sind(gkappa*(xlon-gclon)) - gx1 + 1.
     y = -r*cosd(gkappa*(xlon-gclon)) - gy1 + 1.

  elseif (sec2(4) == 5) then  ! Polar Stereographic grid
     gclon = gridinfo(6)
     r = grrth/gridinfo(7) * tand((90.-xlat)/2.) * (1.+sind(gtrue1))
     x = ( r * sind(xlon-gclon)) - gx1 + 1.
     y = (-r * cosd(xlon-gclon)) - gy1 + 1.

  else
     write(*,'("Unrecognized projection:", I10)') sec2(4)
     write(*,'("STOP in GRIBLLXY")')
     stop
  endif

end subroutine gribllxy
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine glccone (fsplat,ssplat,sign,confac)
  use module_grib
  implicit none
  real, intent(in) :: fsplat,ssplat
  integer, intent(in) :: sign
  real, intent(out) :: confac
  if (abs(fsplat-ssplat).lt.1.E-3) then
     confac = sind(fsplat)
  else
     confac = log10(cosd(fsplat))-log10(cosd(ssplat))
     confac = confac/(log10(tand(45.-float(sign)*fsplat/2.))- &
          log10(tand(45.-float(sign)*ssplat/2.)))
  endif
end subroutine glccone
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
subroutine gribheader(debug_level,ierr, ec_rec_len)
!
! IERR non-zero means there was a problem unpacking the grib header.
!
  use module_grib
  implicit none
  integer :: debug_level
  integer :: ierr, ec_rec_len

  integer, parameter :: nsec1 = 24

  integer, dimension(nsec1) :: &
       iw1=(/3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2/)
  integer :: icount, iskip, ibts, nbm, nbz, i9skip, i17skip

  integer :: iman, ichar, isign, iscan

  integer,  allocatable, dimension(:) :: bm8

  real :: r
  integer :: isvns
  integer :: gsvns = 0

  if (gsvns.eq.0) then
     if (mwsize.eq.32) then
        gsvns = transfer('7777', gsvns)
     elseif(mwsize.eq.64) then
        call gbyte_g1(char(0)//char(0)//char(0)//char(0)//'7777', gsvns, 0, mwsize)
     endif
  endif

! Section 0:
  sec0(2) = ied
  if (ied.eq.1) then
     if ( ec_rec_len .ne. 0 ) then
       sec0(1) = ec_rec_len
     else
       call gbyte_g1(grec, sec0(1), 32, 24)
     endif
     iskip = 64
  elseif (ied.eq.0) then
     sec0(1) = gribsize(grec,200, ierr, 0)
     iskip = 32
  endif

! Section 1:
  i9skip = iskip + 80
  i17skip = iskip + 144
  do icount = 1, nsec1 - ((1-ied)*6)
     ibts = iw1(icount)*8
     call gbyte_g1(grec, sec1(icount), iskip, ibts)
     iskip = iskip + ibts
  enddo
  if (ied.eq.0) sec1(22) = 20
  ! Sec1 indices 9 and 10 might actually be one value, not two.
  ! If this is the case, reread sec1(9), and set sec1(10) to zero:
  if ( (sec1(8) == 101) .or. (sec1(8) == 104) .or. (sec1(8) == 106) .or. &
       (sec1(8) == 108) .or. (sec1(8) == 110) .or. (sec1(8) == 112) .or. &
       (sec1(8) == 114) .or. (sec1(8) == 116) .or. (sec1(8) == 120) .or. &
       (sec1(8) == 121) .or. (sec1(8) == 128) .or. (sec1(8) == 141) .or. &
       (sec1(8) == 236) ) then
     ! No action here.
  else
     call gbyte_g1(grec, sec1(9), i9skip, 16)
     sec1(10) = 0.
  endif

  if (sec1(24).ge.32768) sec1(24) = 32768-sec1(24)

  ! If the TIME/RANGE INDICATOR (sec1(19)) indicates that the time P1
  ! is spread over two bytes, then recompute sec1(17) and set sec1(18)
  ! to zero.
  if (sec1(19) == 10) then
     call gbyte_g1(grec, sec1(17), i17skip, 16)
     sec1(18) = 0
  endif

  ! Pull out single bits from sec1(6) for the GDS and BMS flags:
  sec1(25) = sec1(6)/128
  sec1(26) = mod(sec1(6)/64,2)

! Section 2:
! if ((sec1(6) == 128) .or. (sec1(6) == 192)) then
  if (sec1(25) == 1) then

     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8
     endif
     call gbyte_g1(grec, sec2(1), iskip, 24)
     iskip = iskip + 24
     call gbytes_g1(grec, sec2(2), iskip, 8, 0, 3)
     iskip = iskip + 8*3

     if (sec2(4) == 0) then
        ! Lat/Lon Grid:
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes_g1(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbyte_g1(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes_g1(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbytes_g1(grec, infogrid(8), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbyte_g1(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbyte_g1(grec, infogrid(11), iskip, 32)
        iskip = iskip + 32

!MGD        if ( debug_level .gt. 100 ) then
!MGD        print *, "lat/lon grib grid info", infogrid(1), infogrid(3), &
!MGD        infogrid(5), infogrid(6), infogrid(8), infogrid(21), &
!MGD        infogrid(22), infogrid(10), infogrid(11), infogrid(8)
!MGD        end if

        infogrid(8) = infogrid(8) * infogrid(21)
        infogrid(9) = infogrid(9) * infogrid(22)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        gridinfo(3) = float(infogrid(3))*0.001
        gridinfo(4) = infogrid(4) * 0.001
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        if (infogrid(7).ge.8388608) infogrid(7) = 8388608 - infogrid(7)
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(9) = infogrid(9) * 0.001
        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))
     elseif (sec2(4) == 1) then ! Mercator grid
        ! Number of points in X and Y
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 2)
        iskip = iskip + 32
        ! Starting lat and lon
        call gbytes_g1(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        ! "Resolution and component flags"
        call gbyte_g1(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        ! Ending lat and lon
        call gbytes_g1(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        ! Truelat, 3 bytes
        call gbyte_g1(grec, infogrid(11), iskip, 24)
        iskip = iskip + 24
        ! "Reserved", i.e., skip a byte
        iskip = iskip + 8
        ! Scanning mode flags, first three bits of the next byte
        ! and skip the last five bits.
        call gbyte_g1(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        ! Grid increment in X and Y
        call gbytes_g1(grec, infogrid(8), iskip, 24, 0, 2)
        iskip = iskip + 48
        ! Done reading map specifications.
        ! Now do various conversions:

        gridinfo(1) = float(infogrid(1)) ! ok
        gridinfo(2) = float(infogrid(2)) ! ok

        if (infogrid(3) .ge.8388608) infogrid(3)  = 8388608 - infogrid(3)
        if (infogrid(4) .ge.8388608) infogrid(4)  = 8388608 - infogrid(4)
        if (infogrid(6) .ge.8388608) infogrid(6)  = 8388608 - infogrid(6)
        if (infogrid(7) .ge.8388608) infogrid(7)  = 8388608 - infogrid(7)
        if (infogrid(11).ge.8388608) infogrid(11) = 8388608 - infogrid(11)
        gridinfo(3)  = infogrid(3)  * 0.001
        gridinfo(4)  = infogrid(4)  * 0.001
        gridinfo(6)  = infogrid(6)  * 0.001
        gridinfo(7)  = infogrid(7)  * 0.001
        gridinfo(8)  = infogrid(8)  * 0.001
        gridinfo(9)  = infogrid(9)  * 0.001
        gridinfo(11) = infogrid(11) * 0.001

        gridinfo(21) = infogrid(21)
        gridinfo(22) = infogrid(22)

        gridinfo(20) = 6370.949
        grrth = gridinfo(20)
        gtrue1 = gridinfo(11)

     elseif (sec2(4) == 3) then
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
! Lambert Conformal:
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes_g1(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        call gbyte_g1(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes_g1(grec, infogrid(6), iskip, 24, 0, 3)
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        iskip = iskip + 72
        call gbyte_g1(grec, infogrid(9), iskip, 8)
        iskip = iskip + 8
        call gbyte_g1(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbytes_g1(grec, infogrid(11), iskip, 24, 0, 4)
        if (infogrid(11).ge.8388608) infogrid(11) = 8388608 - infogrid(11)
        if (infogrid(12).ge.8388608) infogrid(12) = 8388608 - infogrid(12)
        if (infogrid(13).ge.8388608) infogrid(13) = 8388608 - infogrid(13)
        if (infogrid(14).ge.8388608) infogrid(14) = 8388608 - infogrid(14)
        iskip = iskip + 96
        call gbyte_g1(grec, infogrid(15), iskip, 16)
        iskip = iskip + 16

        infogrid(7) = infogrid(7) * infogrid(21)
        infogrid(8) = infogrid(8) * infogrid(22)


        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        gridinfo(3) = infogrid(3) * 0.001
        gridinfo(4) = infogrid(4) * 0.001
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(9) = infogrid(9) * 0.001
        gridinfo(11) = infogrid(11) * 0.001
        gridinfo(12) = infogrid(12) * 0.001
        gridinfo(13) = infogrid(13) * 0.001
        gridinfo(14) = infogrid(14) * 0.001

        gridinfo(20) = 6370
        ! a priori knowledge:
        if (sec1(5).eq.212) then
           gridinfo(3) = 12.190
           gridinfo(4) = -133.459
           gridinfo(7) = 40.63525
           gridinfo(8) = 40.63525
           gridinfo(20) = 6370
        endif

!=============================================================================!
! More a priori knowledge:                                                    !
! Correct some bad lat/lon numbers coded into some RUC headers.               !
!                                                                             !
        if (sec1(3) == 59) then  ! If FSL
           if (sec1(4) == 86) then  ! and RUC
              if (sec1(5) == 255) then 
           ! Check to correct bad lat/lon numbers.
                 if (infogrid(3) == 16909) then
                    infogrid(3) = 16281
                    gridinfo(3) = 16.281
                 endif
                 if (infogrid(4) == 236809) then
                    infogrid(4) = 2338622
                    gridinfo(4) = 233.8622
                 endif
              endif
           endif
        endif
!=============================================================================!


        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

        ! Map parameters
        glat1 = gridinfo(3)
        glon1 = gridinfo(4)
        gclon = gridinfo(6)
        if (gclon.gt.180.) gclon = -(360.-gclon)
        if ((gclon<0).and.(glon1>180)) glon1 = glon1-360.  
        gtrue1 = gridinfo(11)
        gtrue2 = gridinfo(12)
        grrth = gridinfo(20)
        call glccone(gtrue1, gtrue2, 1, gkappa)
        r = grrth/(gridinfo(7)*gkappa)*sind(90.-gtrue1) * &
             (tand(45.-glat1/2.)/tand(45.-gtrue1/2.)) ** gkappa
        gx1 =  r*sind(gkappa*(glon1-gclon))
        gy1 = -r*cosd(gkappa*(glon1-gclon))

     elseif (sec2(4) == 4) then
        ! Gaussian Grid:
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbytes_g1(grec, infogrid(3), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbyte_g1(grec, infogrid(5), iskip, 8)
        iskip = iskip + 8
        call gbytes_g1(grec, infogrid(6), iskip, 24, 0, 2)
        iskip = iskip + 48
        call gbytes_g1(grec, infogrid(8), iskip, 16, 0, 2)        
        iskip = iskip + 32
        call gbyte_g1(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
        call gbyte_g1(grec, infogrid(11), iskip, 32)
        iskip = iskip + 32

        infogrid(8) = infogrid(8) * infogrid(21)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        gridinfo(3) = float(infogrid(3))*0.001
        gridinfo(4) = infogrid(4) * 0.001
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)
        if (infogrid(7).ge.8388608) infogrid(7) = 8388608 - infogrid(7)
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001
        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

        ! Compute an approximate delta-latitude and starting latitude.
        ! Replace the stored value of starting latitude with approximate one.
        gridinfo(18) = gridinfo(3)
        infogrid(18) = infogrid(3)
        gridinfo(17) = gridinfo(6)
        infogrid(17) = infogrid(6)
!        call griblgg(infogrid(2), gridinfo(3), gridinfo(19))
!        infogrid(19) = nint(gridinfo(19)*1000.)
!        infogrid(3) = nint(gridinfo(3)*1000.)
        gridinfo(6) = -gridinfo(3)
        infogrid(6) = -infogrid(3)

     elseif (sec2(4) == 5) then
! Polar Stereographic Grid
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 2)  ! NX and NY
        iskip = iskip + 32
        call gbytes_g1(grec, infogrid(3), iskip, 24, 0, 2)  ! LAT1 and LON1
        iskip = iskip + 48
        call gbyte_g1(grec, infogrid(5), iskip, 8) ! Resolution and Component
        iskip = iskip + 8
        call gbytes_g1(grec, infogrid(6), iskip, 24, 0, 3) ! LOV, DX, and DY
        iskip = iskip + 72
        call gbyte_g1(grec, infogrid(9), iskip, 8) ! Projection center flag
        iskip = iskip + 8
        call gbyte_g1(grec, infogrid(21), iskip, 1)
        infogrid(21) = 1-(infogrid(21)*2)
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(22), iskip, 1)
        infogrid(22) = (infogrid(22)*2)-1
        iskip = iskip + 1
        call gbyte_g1(grec, infogrid(10), iskip, 1)
        iskip = iskip + 1
        iskip = iskip + 5
!         call gbyte_g1(grec, infogrid(11), iskip, 32) ! Set to 0 (reserved)
        iskip = iskip + 32

        if (infogrid(3).ge.8388608) infogrid(3) = 8388608 - infogrid(3)
        if (infogrid(4).ge.8388608) infogrid(4) = 8388608 - infogrid(4)
        if (infogrid(6).ge.8388608) infogrid(6) = 8388608 - infogrid(6)


        infogrid(7) = infogrid(7) * infogrid(21)
        infogrid(8) = infogrid(8) * infogrid(22)

        gridinfo(1) = float(infogrid(1))
        gridinfo(2) = float(infogrid(2))
        gridinfo(3) = infogrid(3) * 0.001
        gridinfo(4) = infogrid(4) * 0.001
        gridinfo(6) = infogrid(6) * 0.001
        gridinfo(7) = infogrid(7) * 0.001
        gridinfo(8) = infogrid(8) * 0.001

        gridinfo(20) = 6370

        ! a priori knowledge:
        if (sec1(5).eq.240) then
           gridinfo(3) = 22.7736
           gridinfo(4) = -120.376
           gridinfo(7) = 4.7625
           gridinfo(8) = 4.7625
           gridinfo(20) = 6370
        endif

        ! Map parameters
        glat1 = gridinfo(3)
        glon1 = gridinfo(4)
        gclon = gridinfo(6)
        if (gclon.gt.180.) gclon = -(360.-gclon)
        ! GRIB edition 1 Polar Stereographic grids are true at 60 degrees
        ! Which hemisphere depends on infogrid(9), the "Projection Center Flag"
        grrth = gridinfo(20)
        if (infogrid(9) > 127) then
           gtrue1 = -60. 
           r = grrth/gridinfo(7) * tand((-90.-glat1)/2.) * (1.+sind(-gtrue1))
           gx1 = -r * sind(glon1-gridinfo(6))
           gy1 = -r * cosd(glon1-gridinfo(6)) 
        else
           gtrue1 = 60. 
           r = grrth/gridinfo(7) * tand((90.-glat1)/2.) * (1.+sind(gtrue1))
           gx1 = r * sind(glon1-gridinfo(6))
           gy1 = -r * cosd(glon1-gridinfo(6))
        endif

        gridinfo(21) = float(infogrid(21))
        gridinfo(22) = float(infogrid(22))

     elseif (sec2(4) == 50) then
! Spherical harmonic coefficients
        if (ied.eq.0) then
           print '(//,"*** Despair ***"//)'
           stop
        endif
        call gbytes_g1(grec, infogrid(1), iskip, 16, 0, 3)
        iskip = iskip + 48
        call gbytes_g1(grec, infogrid(4), iskip, 8, 0, 2)
        iskip = iskip + 16

        iskip = iskip + 18*8

     else
        call gribprint(0)
        call gribprint(1)
        call gribprint(2)
        call gribprint(3)
        call gribprint(4)
        write(*,'("Unrecognized grid: ", i8)') sec2(4)
        write(*,'("This grid is not currently supported.")')
        write(*,'("Write your own program to put the data to the intermediate format")')
        stop
     endif

  endif

! Section 3
  if ((sec1(6).eq.64).or.(sec1(6).eq.192)) then
     if (ied.eq.0) then
        print '(//,"*** Despair ***"//)'
        stop
     endif

     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8 + sec2(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8 + sec2(1)*8
     endif
     call gbyte_g1(grec, sec3(1), iskip, 24)
     iskip = iskip + 24
     call gbyte_g1(grec, sec3(2), iskip, 8)
     iskip = iskip + 8
     call gbyte_g1(grec, sec3(3), iskip, 16)
     iskip = iskip + 16

#if defined (CRAY)
#else
     allocate(bitmap((sec3(1)-6)*8))
#endif
     allocate(bm8((sec3(1)-6)*8))
     call gbytes_g1(grec, bm8, iskip, 1, 0, (sec3(1)-6)*8)
     bitmap(1:size(bm8)) = bm8(1:size(bm8))
     deallocate(bm8)
     iskip = iskip + sec3(1)-6
  else
     sec3 = 0
  endif

! Section 4
  if ((sec1(6).eq.128).or.(sec1(6).eq.192)) then
     if (ied.eq.0) then
        iskip = 32 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8
     elseif (ied.eq.1) then
        iskip = 64 + sec1(1)*8 + sec2(1)*8 + sec3(1)*8
     endif
     call gbyte_g1(grec, sec4(1), iskip, 24)
     if (sec4(1) > (sec0(1) - sec1(1) - sec2(1) - sec3(1) - 4)) then
        write(*,'(/,"*** I have good reason to believe that this GRIB record is")')
        write(*,'("*** corrupted or miscoded.",/)')
        ierr = 1
        return
     endif
     iskip = iskip + 24
     call gbytes_g1(grec, sec4(2), iskip, 1,0,4)
     iskip = iskip + 4
     call gbyte_g1(grec, sec4(6), iskip, 4)
     iskip = iskip + 4
! Get the binary scale factor
     call gbyte_g1(grec, isign, iskip, 1)
     iskip = iskip + 1
     call gbyte_g1(grec, sec4(7), iskip, 15)
     iskip = iskip + 15
     sec4(7) = sec4(7) * (-2*isign+1)
! Get the reference value:
     call gbyte_g1(grec, isign, iskip, 1)
     iskip = iskip + 1
     isign = -2*isign+1
     call gbyte_g1(grec, ichar, iskip, 7)
     iskip = iskip + 7
     call gbyte_g1(grec, iman, iskip, 24)
     iskip = iskip + 24
     if ( iman .ne. 0 ) then
       xec4(1) = float(isign) * (2.**(-24)) * float(iman) *  &
          (16.**(ichar-64))
     else
       xec4(1) = 0.
     endif

     call gbyte_g1(grec,sec4(8), iskip, 8)
! sec4(8) is the number of bits used per datum value.
! If sec4(8) = 255, assume they mean sec4(8) = 0
     if (sec4(8) == 255) sec4(8) = 0
     iskip = iskip + 8
  endif

! Section 5
  call gbyte_g1(grec, isvns, ((sec0(1)-4)*8), 32)
  if (isvns.ne.gsvns) then
     write(*, '("End-of-record mark (7777) not found", 2I10)') isvns
     write(*, '("Sec0(1) = ", I8, i2)') sec0(1), sevencount
     sevencount = sevencount + 1
     if (sevencount > 10) then
        write(*,'(//," *** Found more than 10 consecutive bad GRIB records")')
        write(*,'(" *** Let''s just stop now.",//)')
        write(*,'(" Perhaps the analysis file should have been converted",/,&
             &" from COS-Blocked format?",//)')
        stop
     endif
  else
     sevencount = 0
  endif

  ierr = 0

end subroutine gribheader
!
!=============================================================================!
!=============================================================================!
!=============================================================================!
!
  subroutine gribdata(datarray, ndat) 

!-----------------------------------------------------------------------------!
!                                                                             !
! Read and unpack the data from a GRIB record.                                !
!                                                                             !
! Input:                                                                      !
!    NDAT:  The size of the data array we expect to unpack.                   !
!                                                                             !
! Output:                                                                     !
!    DATARRAY:  The unpacked data from the GRIB record                        !
!                                                                             !
! Side Effects:                                                               !
!    STOP if it cannot unpack the data.                                       !
!                                                                             !
! Externals:                                                                  !
!     SGUP_BITMAP                                                             !
!     SGUP_NOBITMAP                                                           !
!     CSHUP                                                                   !
!                                                                             !
! Modules:                                                                    !
!     MODULE_GRIB                                                             !
!                                                                             !
!-----------------------------------------------------------------------------!
    use module_grib

    implicit none

    integer :: ndat
    real , dimension(ndat) :: datarray
    integer, dimension(ndat) :: IX

    integer :: iskip, nbm

    if (sec4(2) == 0) then ! Grid-point data
       if (sec4(3).eq.0) then ! Simple unpacking
          if ((sec1(6).eq.64).or.(sec1(6).eq.192)) then ! There is a bitmap
             call SGUP_BITMAP(datarray, ndat)
          else
             call SGUP_NOBITMAP(datarray, ndat)
          endif
       else
          write(*,'(//,"***** No complex unpacking of gridpoint data.")')
          write(*,'("***** Option not yet available.",//)')
!         write(*,'("***** Complain to mesouser@ucar.edu",//)')
          stop
       endif
    else
       if (sec4(3).eq.0) then ! Simple unpacking
          write(*,'(//,"***** No simple unpacking of spherical-harmonic coefficients.")')
          write(*,'("***** Option not yet available.",//)')
!         write(*,'("***** Complain to mesouser@ucar.edu",//)')
          stop
       elseif (sec4(3).eq.1) then
          call CSHUP(datarray, ndat)
       endif
    endif

end subroutine gribdata

subroutine deallogrib
! Deallocates a couple of arrays that may be allocated.
  use module_grib
#if defined (CRAY)
#else
  if (allocated(grec)) deallocate(grec)
  if (allocated(bitmap)) deallocate(bitmap)
#endif
end subroutine deallogrib

SUBROUTINE gribLGG( NLAT, startlat, deltalat )


  implicit none
!
!  LGGAUS finds the Gaussian latitudes by finding the roots of the
!  ordinary Legendre polynomial of degree NLAT using Newtons
!  iteration method.
!
!  On entry:
  integer NLAT ! the number of latitudes (degree of the polynomial)
!
!  On exit: for each Gaussian latitude

  double precision, dimension(NLAT) :: LATG ! Latitude

! Approximations to a regular latitude grid:
  real :: deltalat
  real :: startlat

!-----------------------------------------------------------------------

  integer :: iskip = 15
  double precision :: sum1 = 0.
  double precision :: sum2 = 0.
  double precision :: sum3 = 0.
  double precision :: sum4 = 0.
  double precision :: xn

  integer, save :: SAVE_NLAT = -99
  real, save :: save_deltalat = -99.
  real, save :: save_startlat = -99.

  double precision, dimension(nlat) ::  COSC, SINC
  double precision, parameter :: PI = 3.141592653589793
!
!    -convergence criterion for iteration of cos latitude
  double precision, parameter :: XLIM  = 1.0E-14
  integer :: nzero, i, j
  double precision :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d

  if (nlat == save_nlat) then
     deltalat = save_deltalat
     startlat = save_startlat
     return
  endif
!
!    -the number of zeros between pole and equator
  NZERO = NLAT/2
!
!    -set first guess for cos(colat)
  DO I=1,NZERO
     COSC(I) = SIN( (I-0.5)*PI/NLAT + PI*0.5 )
  ENDDO
!
!    -constants for determining the derivative of the polynomial
  FI  = NLAT
  FI1 = FI+1.0
  A   = FI*FI1 / SQRT(4.0*FI1*FI1-1.0)
  B   = FI1*FI / SQRT(4.0*FI*FI-1.0)
!
!    -loop over latitudes, iterating the search for each root
  DO I=1,NZERO
     J=0
!
!       -determine the value of the ordinary Legendre polynomial for
!       -the current guess root
     LOOP30 : DO 
        CALL LGORD( G, COSC(I), NLAT )
!
!       -determine the derivative of the polynomial at this point
        CALL LGORD( GM, COSC(I), NLAT-1 )
        CALL LGORD( GP, COSC(I), NLAT+1 )
        GT = (COSC(I)*COSC(I)-1.0) / (A*GP-B*GM)
!
!       -update the estimate of the root
        DELTA   = G*GT
        COSC(I) = COSC(I) - DELTA
!
!       -if convergence criterion has not been met, keep trying
        J = J+1
        IF( ABS(DELTA).LE.XLIM ) EXIT LOOP30
     ENDDO LOOP30
  ENDDO
!
!  Determine the sin(colat)
  SINC(1:NZERO) = SIN(ACOS(COSC(1:NZERO)))
!
!    -if NLAT is odd, set values at the equator
  IF( MOD(NLAT,2) .NE. 0 ) THEN
     I       = NZERO+1
     SINC(I) = 1.0
     latg(i) = 0.
  END IF

! Set the latitudes.

  latg(1:NZERO) = dacos(sinc(1:NZERO)) * 180. / pi

! Determine the southern hemisphere values by symmetry
  do i = 1, nzero
     latg(nlat-nzero+i) = -latg(nzero+1-i)
  enddo


! Now that we have the true values, find some approximate values.

  xn = float(nlat-iskip*2)
  do i = iskip+1, nlat-iskip
     sum1 = sum1 + latg(i)*float(i)
     sum2 = sum2 + float(i)
     sum3 = sum3 + latg(i)
     sum4 = sum4 + float(i)**2
  enddo

  b = (xn*sum1 - sum2*sum3) / (xn*sum4 - sum2**2)
  a = (sum3 - b * sum2) / xn

  deltalat = sngl(b)
  startlat = sngl(a + b)

  save_nlat = nlat
  save_deltalat = deltalat
  save_startlat = startlat

contains
  SUBROUTINE LGORD( F, COSC, N )
    implicit none
!
!  LGORD calculates the value of an ordinary Legendre polynomial at a
!  latitude.
!
!  On entry:
!     COSC - cos(colatitude)
!     N      - the degree of the polynomial
!
!  On exit:
!     F      - the value of the Legendre polynomial of degree N at
!              latitude asin(COSC)
    double precision :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
    integer :: n, k

!------------------------------------------------------------------------

    colat = acos(cosc)
    c1 = sqrt(2.0)
    do k=1,n
       c1 = c1 * sqrt( 1.0 - 1.0/(4*k*k) )
    enddo
    fn = n
    ang= fn * colat
    s1 = 0.0
    c4 = 1.0
    a  =-1.0
    b  = 0.0
    do k=0,n,2
       if (k.eq.n) c4 = 0.5 * c4
       s1 = s1 + c4 * cos(ang)
       a  = a + 2.0
       b  = b + 1.0
       fk = k
       ang= colat * (fn-fk-2.0)
       c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
    enddo
    f = s1 * c1
  end subroutine lgord

END SUBROUTINE GRIBLGG

SUBROUTINE REORDER_IT (a, nx, ny, dx, dy, iorder)

      use module_debug

      implicit none
      integer :: nx, ny, iorder
      integer :: i, j, k, m
      real :: dx, dy
      real, dimension(nx*ny) :: a, z

      if (iorder .eq. 0 .and. dx .gt. 0. .and. dy .lt. 0) return
      k = 0
      call mprintf(.true.,DEBUG, &
        "Reordering GRIB array : dx = %f  , dy = %f  , iorder = %i",  &
	 f1=dx,f2=dy,i1=iorder)
      if (iorder .eq. 0 ) then
	if ( dx .lt. 0 .and. dy .lt. 0. ) then
	  do j = 1, ny
	  do i = nx, 1, -1
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        else if ( dx .lt. 0 .and. dy .gt. 0. ) then
	  do j = ny, 1, -1
	  do i = nx, 1, -1
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        else if ( dx .gt. 0 .and. dy .gt. 0. ) then
	  do j = ny, 1, -1
	  do i = 1, nx
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        endif
      else
	if ( dx .gt. 0 .and. dy .lt. 0. ) then
	  do i = 1, nx
	  do j = 1, ny
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        else if ( dx .lt. 0 .and. dy .lt. 0. ) then
	  do i = nx, 1, -1
	  do j = 1, ny
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        else if ( dx .lt. 0 .and. dy .lt. 0. ) then
	  do i = nx, 1, -1
	  do j = ny, 1, -1
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        else if ( dx .gt. 0 .and. dy .gt. 0. ) then
	  do i = 1, nx
	  do j = ny, 1, -1
	    k = k + 1
	    m = i * j
	    z(k) = a(m)
	  enddo
	  enddo
        endif
      endif
!  now put it back in the 1-d array and reset the dx and dy
      do k = 1, nx*ny
        a(k) = z(k)
      enddo
      dx = abs ( dx)
      dy = -1 * abs(dy)
      return
END SUBROUTINE REORDER_IT
