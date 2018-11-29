!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!*****************************************************************************!
!                                                                             !
! Module to facilitate putting fields into GRIB 1 format.                     !
!                                                                             !
! Still needs a lot of work;                                                  !
! still needs a lot of documentation;                                         !
! but I'm fairly pleased with the potential.                                  !
!                                                                             !
! Author:  Kevin W. Manning                                                   !
!          NCAR/MMM                                                           !
!          October 2001 and continuing                                        !
!          October 2010:  Allow a constant field to be bitmasked.             !
!          October 2010:  Don't encode the bitmask if the mask includes the   !
!                         entire grid.  Treat it as a non-bitmasked field.    !
!                                                                             !
!*****************************************************************************!

module engrib_module_test

  integer, private, parameter :: sec1_size = 28
  integer, private :: sec1_ptv
  integer, private :: sec1_cid
  integer, private :: sec1_pid
  integer, private :: sec1_gid
  integer, private :: sec1_gds
  integer, private :: sec1_bms
  integer, private :: sec1_prm
  integer, private :: sec1_lty
  integer, private :: sec1_lv1
  integer, private :: sec1_lv2
  integer, private :: sec1_gyr
  integer, private :: sec1_gmo
  integer, private :: sec1_gdy
  integer, private :: sec1_ghr
  integer, private :: sec1_gmi
  integer, private :: sec1_tmu
  integer, private :: sec1_pr1
  integer, private :: sec1_pr2
  integer, private :: sec1_tri
  integer, private :: sec1_nin
  integer, private :: sec1_nmi
  integer, private :: sec1_cen
  integer, private :: sec1_sub
  integer, private :: sec1_dsf

  integer, private :: sec2_size
  integer, private :: sec2_gty
  integer, private :: sec2_nxp
  integer, private :: sec2_nyp
  integer, private :: sec2_la1
  integer, private :: sec2_lo1
  integer, private :: sec2_la2
  integer, private :: sec2_lo2
  integer, private :: sec2_rac
  integer, private :: sec2_lov
  integer, private :: sec2_dxt
  integer, private :: sec2_dyt
  integer, private :: sec2_tr1
  integer, private :: sec2_tr2
  integer, private :: sec2_pcf  ! Default 0

  integer, private :: sec2_inc  ! Default +1
  integer, private :: sec2_jnc  ! Default +1
  integer, private :: sec2_scm  ! Default 0
  integer, private :: sec2_spl  ! LC:  Latitude of the "southern pole"

  integer, private :: sec3_size
  logical, allocatable, dimension(:,:) :: lbm
  integer, private :: nummappt

  integer, private :: sec4_size
  integer, private :: sec4_bsf
  integer, private :: sec4_rfa
  integer, private :: sec4_rfb
  integer, private :: sec4_rfs
  integer, private :: sec4_nbt
  integer, allocatable, dimension(:,:) :: gribiarr

  integer, dimension(2) :: newsec0  ! 8 bytes
  integer, dimension(7) :: newsec1  ! 28 bytes
  integer, allocatable, dimension(:) :: newsec2
  integer, allocatable, dimension(:) :: newsec3
  integer, allocatable, dimension(:) :: newsec4
  integer, parameter :: newsec5 = Z"37373737"  !encodes "7777"

#if defined (__sgi) || defined (__sun) || defined (__alpha) || defined (__linux)
  logical, parameter :: DO_BYTE_SWAP = .TRUE.
#else
  logical, parameter :: DO_BYTE_SWAP = .FALSE.
#endif

contains

  subroutine open_newgrib(gribunit, flnm)
!*****************************************************************************!
!                                                                             !
! Subroutine OPEN_NEWGRIB:                                                    !
!    Part of Module ENGRIB_MODULE                                             !
!                                                                             !
! Purpose:                                                                    !
!    Open a c FILE stream for output.                                         !
!                                                                             !
! Argument list:                                                              !
!                                                                             !
!    Input:                                                                   !
!       FLNM:     the pathname of the file to create as a GRIB file.          !
!    Output:                                                                  !
!       GRIBUNIT: the FILE* stream referring to new file FLNM.                !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!   ** Opens the FILE* stream GRIBUNIT for output as file FLNM.               !
!                                                                             !
!   ** If file FLNM does not exist, it is created and ready for new contents. !
!                                                                             !
!   ** If file FLNM already exists, it is destroyed, recreated, and ready     !
!      for new contents.                                                      !
!                                                                             !
!   ** If file FLNM for some reason cannot be created (or recreated), an      !
!      error message is printed and the program is halted.                    !
!                                                                             !
! Externals:                                                                  !
!                                                                             !
!   ** Subroutine COPEN                                                       !
!         Where is COPEN? [i.e., what package, module, or source-code file]   !
!                                                                             !
!                                                                             !
!*****************************************************************************!
    implicit none

! Input:
    character(len=*), intent(in) :: flnm 

! Output:
    integer, intent(out) :: gribunit

! Local variables:
    integer :: ierr  ! Error flag returned from call to copen.

    ! Open the file FLNM for output as FILE* stream GRIBUNIT:
    call copen(gribunit, flnm//char(0), 0, ierr, 0)

    ! Check if the open was successful.  If not, write a message and exit:
    if (ierr /= 0) then
       write(*,'(/,"***** ERROR *****")')
       write(*,'(/,"OPEN_NEWGRIB:  Call to COPEN could not open file ", A,/)') &
            flnm
       call abort
    endif

  end subroutine open_newgrib

  subroutine close_newgrib(gribunit)
!*****************************************************************************!
!                                                                             !
! Subroutine CLOSE_NEWGRIB:                                                   !
!    Part of Module ENGRIB_MODULE                                             !
!                                                                             !
! Purpose:                                                                    !
!    Close a previously-opened c FILE* stream.                                !
!                                                                             !
! Argument list:                                                              !
!                                                                             !
!    Input:                                                                   !
!       GRIBUNIT: the FILE* stream to be closed.                              !
!    Output:                                                                  !
!       None                                                                  !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!   ** Closes the FILE* stream GRIBUNIT.                                      !
!                                                                             !
!   ** If for some reason the close fails, an error message is printed        !
!      and the program is halted.                                             !
!                                                                             !
! Externals:                                                                  !
!                                                                             !
!   ** Subroutine CCLOSE                                                      !
!         Where is CCLOSE? [i.e., what package, module, or source-code file]  !
!                                                                             !
!*****************************************************************************!
    implicit none
! Input:
    integer, intent(in) :: gribunit

! Local variables
    integer :: ierr ! Error flag returned from call to CCLOSE.
    
    call cclose(gribunit, 0, ierr)
    if (ierr /= 0) then
       write(*,'(/,"***** ERROR *****")')
       write(*,'(/,"CLOSE_NEWGRIB:  Call to CCLOSE could not close stream ", I12,/)') &
            gribunit
       call abort
    endif
  end subroutine close_newgrib


  subroutine write_grib(gribunit)
!*****************************************************************************!
!                                                                             !
! Subroutine WRITE_GRIB:                                                      !
!    Part of Module ENGRIB_MODULE                                             !
!                                                                             !
! Purpose:                                                                    !
!    Write                                                                    !
!                                                                             !
! Argument list:                                                              !
!                                                                             !
!    Input:                                                                   !
!       GRIBUNIT: the FILE* stream to be written to.                          !
!                                                                             !
!    Output:                                                                  !
!       None                                                                  !
!                                                                             !
! Other variables in ENGRIB_MODULE accessed by this subroutine:               !
!                                                                             !
!   ** NEWSEC0:  The integer array into which GRIB section 0 information is   !
!                to be packed.  Modified under WRITE_GRIB.                    !
!                                                                             !
!   ** NEWSEC1:  The integer array into which GRIB section 1 information is   !
!                to be packed.  Modified under WRITE_GRIB.                    !
!                                                                             !
!   ** NEWSEC2:  The integer array into which GRIB section 2 information is   !
!                to be packed.  Modified under WRITE_GRIB.                    !
!                                                                             !
!   ** NEWSEC3:  The integer array into which GRIB section 3 information is   !
!                to be packed.  Modified under WRITE_GRIB.                    !
!                                                                             !
!   ** NEWSEC4:  The integer array into which GRIB section 4 information is   !
!                to be packed.  Modified under WRITE_GRIB.                    !
!                                                                             !
!   ** NEWSEC5:  The constant character array, with PARAMETER attribute, which!
!                contains GRIB section 5 information, always "7777".          !
!                                                                             !
!   ** SEC1_SIZE: The size (bytes) of GRIB section 1, with PARAMETER          !
!                 attribute; SEC1_SIZE = 28.                                  !
!                                                                             !
!   ** SEC2_SIZE: The size (bytes) of GRIB section 2.                         !
!                                                                             !
!   ** SEC3_SIZE: The size (bytes) of GRIB section 3.                         !
!                                                                             !
!   ** SEC4_SIZE: The size (bytes) of GRIB section 4.                         !
!                                                                             !
!   ** DO_BYTE_SWAP:  Logical parameter, machine dependent, TRUE if           !
!                     byte-swapping is neede upon output.                     !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!   ** Encodes all the GRIB header information into the GRIB header,          !
!      filling integer arrays NEWSEC0, NEWSEC1, NEWSEC2, NEWSEC3, and         !
!      NEWSEC4.                                                               !
!                                                                             !
!   ** Does byte swapping, if necessary, on integer arrays NEWSEC0, NEWSEC1,  !
!      NEWSEC2, NEWSEC3, and, NEWSEC4.                                        !
!                                                                             !
!   ** Writes a complete GRIB record to FILE* stream GRIBUNIT.                !
!                                                                             !
! Other subroutine and functions used from ENGRIB_MODULE:                     !
!                                                                             !
!   ** GRIBSEC0_PACK:  Packs GRIB section 0 information into array NEWSEC0    !
!                                                                             !
!   ** GRIBSEC1_PACK:  Packs GRIB section 1 information into array NEWSEC1    !
!                                                                             !
!   ** GRIBSEC2_PACK:  Packs GRIB section 2 information into array NEWSEC2    !
!                                                                             !
!   ** GRIBSEC4_PACK:  Packs GRIB section 4 information into array NEWSEC4    !
!                                                                             !
! Externals:                                                                  !
!                                                                             !
!   ** SWAP4F                                                                 !
!                                                                             !
!   ** BNWRIT                                                                 !
!                                                                             !
!*****************************************************************************!

    implicit none

! Argument list input:
    integer, intent(in) :: gribunit

! Argument list output: (none)

! Local variables:
    integer :: ierr, iwrt
    integer, allocatable, dimension(:) :: output_buffer
    integer :: output_size
    integer :: ostart
    integer :: ioffs
    integer :: inskip
    integer :: outskip
    integer :: i
    integer :: k
    integer :: iout
    integer :: itmp

    ! Final packing of all sections, before the write.
    ! These should stay in this order:  
    !      gribsec1_pack, gribsec2_pack, gribsec4_pack, gribsec0_pack
    ! because the other packing needs to be done before we know the final
    ! size of the grib record to go into GRIB section 0.
    call gribsec1_pack
    call gribsec2_pack
    call gribsec4_pack
    call gribsec0_pack

! Do the output, byte-swapping, if necessary:

    output_size = 8 + sec1_size + sec2_size + sec3_size + sec4_size + 4
    allocate(output_buffer(output_size))
    output_buffer = 0

    ! Copy GRIB Section 0.  Known to be 2 bytes.
    call sbytes(output_buffer, newsec0, 0, 32, 0, 2)
    ostart = size(newsec0)

    ! Copy GRIB SEction 1
    if (mod(sec1_size,4)/=0) stop "WEIRD SEC1 SIZE FOR SWAPPING" ! We know sec1_size should be 28
    do iwrt = 1, size(newsec1)
       call sbyte(output_buffer(ostart+iwrt), newsec1(iwrt), 0, 32)
    enddo
    ostart = ostart + size(newsec1)

    ! Copy GRIB Section 2
    if (mod(sec2_size,4)/=0) stop "WEIRD SEC2 SIZE SWAP FOR SWAPPING" ! We know sec2_size should be 32
    do iwrt = 1, size(newsec2)
       call sbyte(output_buffer(ostart+iwrt), newsec2(iwrt), 0, 32)
    enddo
    ostart = ostart + size(newsec2)
    ioffs = 0

       ! Copy GRIB Section 3

    ! Bypass <ostart> words + <outskip> bytes in <output_buffer> before we start packing into the <output_buffer> array.
    ! Skip <inskip> bytes in <newsec3> before we start reading from <newsec3>
    inskip = 0
    outskip = 0
    i = 1
    iout = ostart+1
    do k = 1, sec3_size
       call gbyte(newsec3(i), itmp, inskip, 8)
       call sbyte(output_buffer(iout), itmp, outskip, 8)
       inskip = inskip + 8
       outskip = outskip + 8
       if (outskip == 32) then
          outskip = 0
          iout = iout + 1
       endif
       if (inskip == 32) then
          inskip = 0
          i = i + 1
       endif
    enddo

    ! Copy GRIB Section 4
    inskip = 0
    i = 1
    do k = 1, sec4_size
       call gbyte(newsec4(i), itmp, inskip, 8)
       call sbyte(output_buffer(iout), itmp, outskip, 8)
       inskip = inskip + 8
       outskip = outskip + 8
       if (outskip == 32) then
          outskip = 0
          iout = iout + 1
       endif
       if (inskip == 32) then
          inskip = 0
          i = i + 1
       endif
    enddo

    ! Copy GRIB Section 5
    inskip = 0
    i = 1
    do k = 1, 4
       call gbyte(newsec5, itmp, inskip, 8)
       call sbyte(output_buffer(iout), itmp, outskip, 8)
       inskip = inskip + 8
       outskip = outskip + 8
       if (outskip == 32) then
          outskip = 0
          iout = iout + 1
       endif
       if (inskip == 32) then
          inskip = 0
          i = i + 1
       endif
    enddo
    
    if (DO_BYTE_SWAP) then
       if (mod(output_size,4) == 0) then
          call swap4f(output_buffer, output_size)
       else
          call swap4f(output_buffer, output_size+mod(output_size,4))
       endif
    endif

    call bnwrit(gribunit, output_buffer, output_size, iwrt, ierr, 0)

  end subroutine write_grib

  subroutine grib_clear
    call gribsec0_clear
    call gribsec1_clear
    call gribsec2_clear
    call gribsec3_clear
    call gribsec4_clear
  end subroutine grib_clear

  subroutine gribsec0_clear
    implicit none
    newsec0 = 0
  end subroutine gribsec0_clear

  subroutine gribsec1_clear
    implicit none
    newsec1 = 0
    
    sec1_ptv = -999999
    sec1_cid = -999999
    sec1_pid = -999999
    sec1_gid = -999999
    sec1_gds = -999999
    sec1_bms = -999999
    sec1_prm = -999999
    sec1_lty = -999999
    sec1_lv1 = -999999
    sec1_lv2 = -999999
    sec1_gyr = -999999
    sec1_gmo = -999999
    sec1_gdy = -999999
    sec1_ghr = -999999
    sec1_gmi = -999999
    sec1_tmu = -999999
    sec1_pr1 = -999999
    sec1_pr2 = -999999
    sec1_tri = -999999
    sec1_nin = -999999
    sec1_nmi = -999999
    sec1_cen = -999999
    sec1_sub = -999999
    sec1_dsf = -999999

  end subroutine gribsec1_clear

  subroutine gribsec2_clear
    implicit none
    sec2_size = 0
    sec2_pcf = -999999
    sec2_inc = -999999
    sec2_jnc = -999999
    sec2_rac = -999999
    sec2_scm = -999999
    sec2_tr1 = -999999
    sec2_tr2 = -999999
    if (allocated(newsec2)) deallocate(newsec2)

  end subroutine gribsec2_clear

  subroutine gribsec3_clear
    implicit none

    sec3_size = 0
    nummappt = 0
    if (allocated(lbm)) deallocate(lbm)
    if (allocated(newsec3)) deallocate(newsec3)

  end subroutine gribsec3_clear

  subroutine gribsec4_clear
    implicit none
    sec4_size = 0
    sec4_bsf = -999999
    sec4_rfa = -999999
    sec4_rfb = -999999
    sec4_rfs = -999999
    sec4_nbt = -999999
    if (allocated(newsec4)) deallocate(newsec4)
    if (allocated(gribiarr)) deallocate(gribiarr)
  end subroutine gribsec4_clear

  subroutine gribsec0_pack()
!*****************************************************************************!
!                                                                             !
! Subroutine GRIBSEC0_PACK:                                                   !
!    Part of Module ENGRIB_MODULE                                             !
!                                                                             !
! Purpose:                                                                    !
!    Pack GRIB section 0 information into array NEWSEC0                       !
!                                                                             !
! Argument list:                                                              !
!                                                                             !
!    Input:                                                                   !
!       None                                                                  !
!    Output:                                                                  !
!       None                                                                  !
!                                                                             !
! Other variables in ENGRIB_MODULE accessed by this subroutine:               !
!                                                                             !
!   ** NEWSEC0  :  Integer array to hold GRIB section 0 information.          !
!                  Modified under GRIBSEC0_PACK.                              !
!                                                                             !
!   ** SEC1_SIZE:  Size (bytes) of GRIB section 1.                            !
!                                                                             !
!   ** SEC2_SIZE:  Size (bytes) of GRIB section 2.                            !
!                                                                             !
!   ** SEC3_SIZE:  Size (bytes) of GRIB section 3.                            !
!                                                                             !
!   ** SEC4_SIZE:  Size (bytes) of GRIB section 4.                            !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!   ** Packs GRIB section 0 information into array NEWSEC0                    !
!                                                                             !
! Other subroutine and functions used from ENGRIB_MODULE:                     !
!                                                                             !
!   ** SBENC:  Packs groups of bits into an integer array.  Basically         !
!              a wrapper for the SBYTE subroutine.                            !
!                                                                             !
! Externals:                                                                  !
!                                                                             !
!   None.                                                                     !
!                                                                             !
!*****************************************************************************!

    implicit none

! Local variables:
    integer :: iskip
    integer :: totsize 

    totsize = 8 + sec1_size + sec2_size + sec3_size + sec4_size + 4
    iskip = 0

    call sbenc(newsec0, ichar("G"), ichar("G"), iskip, 8)
    call sbenc(newsec0, ichar("R"), ichar("R"), iskip, 8)
    call sbenc(newsec0, ichar("I"), ichar("I"), iskip, 8)
    call sbenc(newsec0, ichar("B"), ichar("B"), iskip, 8)

    call sbenc(newsec0, totsize, -999999, iskip, 24)
    call sbenc(newsec0, 1, 1, iskip, 8)

  end subroutine gribsec0_pack

  subroutine gribsec1_pack()
    implicit none
    integer :: gdsbms, iskip

    gdsbms = 0
    if (sec1_gds > -999998) then
       gdsbms = gdsbms + 128*sec1_gds
    endif
    if (sec1_bms > -999998) then
       gdsbms = gdsbms + 64*sec1_bms
    endif

    iskip = 0
    call sbenc(newsec1,       28,  28, iskip, 24)
    call sbenc(newsec1, sec1_ptv, 255, iskip,  8)
    call sbenc(newsec1, sec1_cid, 255, iskip,  8)
    call sbenc(newsec1, sec1_pid, 255, iskip,  8)
    call sbenc(newsec1, sec1_gid, 255, iskip,  8)
    call sbenc(newsec1,   gdsbms,   0, iskip,  8)
    call sbenc(newsec1, sec1_prm, 255, iskip,  8)
    call sbenc(newsec1, sec1_lty, 255, iskip,  8)
    select case (sec1_lty)
    case (0:99)
       call sbenc(newsec1, 0,   0, iskip, 16)
    case (100,105,107,109,111,113,115,117,119,125,160,200,201)
       call sbenc(newsec1, sec1_lv1, 0, iskip, 16)
    case (101,102,104,106,108,110,112,114,116,120,121,128,141)
       call sbenc(newsec1, sec1_lv1, 0, iskip, 8)
       call sbenc(newsec1, sec1_lv2, 0, iskip, 8)
    case default
       print*, 'SEC1_LTY = ', sec1_lty
       stop "Unrecognized SEC1_LTY in GRIBSEC1_PACK"
    end select
    call sbenc(newsec1, sec1_gyr, 255, iskip,  8)
    call sbenc(newsec1, sec1_gmo, 255, iskip,  8)
    call sbenc(newsec1, sec1_gdy, 255, iskip,  8)
    call sbenc(newsec1, sec1_ghr, 255, iskip,  8)
    call sbenc(newsec1, sec1_gmi, 255, iskip,  8)
    call sbenc(newsec1, sec1_tmu, 255, iskip,  8)
    call sbenc(newsec1, sec1_pr1, 255, iskip,  8)
    call sbenc(newsec1, sec1_pr2, 255, iskip,  8)
    call sbenc(newsec1, sec1_tri, 255, iskip,  8)
    call sbenc(newsec1, sec1_nin,   0, iskip, 16)
    call sbenc(newsec1, sec1_nmi,   0, iskip,  8)
    call sbenc(newsec1, sec1_cen, 255, iskip,  8)
    call sbenc(newsec1, sec1_sub, 255, iskip,  8)
    call sbenc(newsec1, sec1_dsf,   0, iskip,  16)

  end subroutine gribsec1_pack

  subroutine sbenc(buf, in, df, sk, nb)
    implicit none
    integer, dimension(*) :: buf ! Buffer to copy bits to.
    integer, intent(in) :: in             ! word containing bits to copy
    integer, intent(in) :: df             ! Default bits, in case of no-data value
    integer, intent(inout) :: sk          ! Number of bits to skip
    integer, intent(in) :: nb             ! number of bits to encode

    if (in > -999998) then
!KWM       if ((in == 194) .and. (sk == 48)) then
!KWM          print*, 'SBENC:  call sbyte:  in,sk,nb = ', in,sk,nb
!KWM       endif
       call sbyte(buf, in, sk, nb)
!KWM       if ((in == 194) .and. (sk == 48)) then
!KWM          print'("SBENC:  BUF(1) = ",Z8.8)', buf(1)
!KWM          print'("SBENC:  BUF(2) = ",Z8.8)', buf(2)
!KWM       endif
    else
       call sbyte(buf, df, sk, nb)
    endif
    sk = sk + nb
  end subroutine sbenc

  subroutine gribsec1_set(parameter_table_version, center_id, process_id, &
       grid_id, isthere_gds, isthere_bms, parameter_id, level_type, &
       level_1, level_2, year, month, day, hour, minute, time_unit, &
       p1, p2, time_range_indicator, number_included, number_missing, &
       century, subcenter_id, decimal_scale_factor, &
       hdate)
    implicit none
    integer, intent(in), optional :: parameter_table_version, center_id, process_id, &
         grid_id, isthere_gds, isthere_bms, parameter_id, level_type, &
         level_1, level_2, year, month, day, hour, minute, time_unit, &
         p1, p2, time_range_indicator, number_included, number_missing, &
         century, subcenter_id, decimal_scale_factor
    character(len=*), intent(in), optional :: hdate

    integer :: cn, yr, mo, dy, hr, mi
    integer :: ierr

    if (present(parameter_table_version)) sec1_ptv = parameter_table_version
    if (present(center_id))               sec1_cid = center_id
    if (present(process_id))              sec1_pid = process_id
    if (present(grid_id))                 sec1_gid = grid_id
    if (present(isthere_gds))             sec1_gds = isthere_gds
    if (present(isthere_bms))             sec1_bms = isthere_bms
    if (present(parameter_id))            sec1_prm = parameter_id
    if (present(level_type))              sec1_lty = level_type
    if (present(level_1))                 sec1_lv1 = level_1
    if (present(level_2))                 sec1_lv2 = level_2
    if (present(year))                    sec1_gyr = year
    if (present(month))                   sec1_gmo = month
    if (present(day))                     sec1_gdy = day
    if (present(hour))                    sec1_ghr = hour
    if (present(minute))                  sec1_gmi = minute
    if (present(time_unit))               sec1_tmu = time_unit
    if (present(p1))                      sec1_pr1 = p1
    if (present(p2))                      sec1_pr2 = p2
    if (present(time_range_indicator))    sec1_tri = time_range_indicator
    if (present(number_included))         sec1_nin = number_included
    if (present(number_missing))          sec1_nmi = number_missing
    if (present(century))                 sec1_cen = century
    if (present(subcenter_id))            sec1_sub = subcenter_id
    if (present(decimal_scale_factor)) then
       if (decimal_scale_factor < 0) then
          sec1_dsf = (2**15)-decimal_scale_factor
       else
          sec1_dsf = decimal_scale_factor
       endif
    endif

    if (present(hdate)) then
       cn = 0
       yr = 0
       mo = 0
       dy = 0
       hr = 0
       mi = 0
       select case (len(hdate))
       case (16:)
          read(hdate,'(I2,I2,1x,I2,1x,I2,1x,I2,1x,I2)', iostat=ierr) cn, yr, mo, dy, hr, mi
       case (13)
          read(hdate,'(I2,I2,1x,I2,1x,I2,1x,I2)', iostat=ierr) cn, yr, mo, dy, hr
       case (10)
          read(hdate,'(I2,I2,1x,I2,1x,I2)', iostat=ierr) cn, yr, mo, dy
       case (7)
          read(hdate,'(I2,I2,1x,I2)', iostat=ierr) cn, yr, mo
       case (4)
          read(hdate,'(I2,I2)', iostat=ierr) cn, yr
       case default
          print*, "Bad HDATE = "//hdate
          stop "Bad HDATE in GRIBSEC1_SET"
       end select
       if (ierr /= 0) then
          print*, 'Problem reading from hdate #'//hdate//"#"
          stop
       endif

       sec1_gyr = yr
       sec1_gmo = mo
       sec1_gdy = dy
       sec1_ghr = hr
       sec1_gmi = mi
       if (yr > 0) then
          sec1_cen = cn+1
       else
          sec1_cen = cn
       endif
    endif
  end subroutine gribsec1_set

  subroutine gribsec3_create()

    sec3_size = (size(lbm,1)*size(lbm,2))/8
    nover = mod(size(lbm,1)*size(lbm,2), 8)
    if (nover > 0) then
       sec3_size = sec3_size + 1
       numunused = 8-nover
    endif
    if (mod(sec3_size,2) == 1) then
       sec3_size = sec3_size + 1
       numunused = numunused + 8
    endif

    sec3_size = sec3_size + 6

    allocate(newsec3((sec3_size+3)/4))
    newsec3 = 0

    iskip = 0
    call sbenc(newsec3, sec3_size, sec3_size, iskip, 24)
    call sbenc(newsec3, numunused, numunused, iskip, 8)
    call sbenc(newsec3, 0, 0, iskip, 16)

    if ((sec2_inc == 1) .or. (sec2_inc < -999998)) then
       ist = 1
       ien = size(lbm,1)
       inc = 1
    elseif (sec2_inc == -1) then
       ist = size(lbm,1)
       ien = 1
       inc = -1
    endif


    if ((sec2_jnc == 1) .or. (sec2_jnc < -999998)) then
       jst = 1
       jen = size(lbm,2)
       jnc = 1
    elseif (sec2_jnc == -1) then
       jst = size(lbm,2)
       jen = 1
       jnc = -1
    endif

    if ((sec2_scm == 0) .or. (sec2_scm < -999998)) then

       do j = jst, jen, jnc
          do i = ist, ien, inc
             if (lbm(i,j)) then
                lbm(i,j) = .TRUE.
                call sbenc(newsec3, 1, 1, iskip, 1)
             else
                lbm(i,j) = .FALSE.
                call sbenc(newsec3, 0, 0, iskip, 1)
             endif
          enddo
       enddo

    else if (sec2_scm == 1) then
       
       do i = ist, ien, inc
          do j = jst, jen, jnc
             if (lbm(i,j)) then
                lbm(i,j) = .TRUE.
                call sbenc(newsec3, 1, 1, iskip, 1)
             else
                lbm(i,j) = .FALSE.
                call sbenc(newsec3, 0, 0, iskip, 1)
             endif
          enddo
       enddo

    endif

  end subroutine gribsec3_create

  subroutine gribsec2_set(grid_type, nx, ny, la1, lo1, la2, lo2, &
       resolution_and_component, lov, dx, dy, projection_center_flag, &
       i_scanning_increment, j_scanning_increment, scanning_mode, truelat1, &
       truelat2)
    implicit none
    integer, intent(in), optional :: grid_type
    integer, intent(in), optional :: nx
    integer, intent(in), optional :: ny
    real,    intent(in), optional :: la1
    real,    intent(in), optional :: lo1
    real,    intent(in), optional :: la2
    real,    intent(in), optional :: lo2
    integer, intent(in), optional :: resolution_and_component
    real,    intent(in), optional :: lov
    real,    intent(in), optional :: dx
    real,    intent(in), optional :: dy
    real,    intent(in), optional :: truelat1
    real,    intent(in), optional :: truelat2
    integer, intent(in), optional :: projection_center_flag
    integer, intent(in), optional :: i_scanning_increment
    integer, intent(in), optional :: j_scanning_increment
    integer, intent(in), optional :: scanning_mode

    if (present(grid_type))then
       select case(grid_type)
       case (0,5)
          sec2_size = 32
          allocate(newsec2((sec2_size+3)/4))
          newsec2 = 0
       case (1,3)
          sec2_size = 42
          allocate(newsec2((sec2_size+3)/4))
          newsec2 = 0
       case default
          print*, 'grid_type = ', grid_type
          stop "Unrecognized grid type in GRIBSEC2_SET"
       end select

       sec2_gty = grid_type
       call gribsec1_set(isthere_gds=1)
    endif

    if (present(nx)) sec2_nxp = nx
    if (present(ny)) sec2_nyp = ny

    if (present(la1))then
       if ((la1 > 90.0) .or. (la1 < -90.0)) then
          write(*,'(" ***** Latitude problem (la1): ", F20.10)') la1
          stop "GRIBSEC2_SET"
       endif
       if (la1 < 0) then
          sec2_la1 = (2**23) - nint(la1 * 1.E3)
       else
          sec2_la1 = nint(la1 * 1.E3)
       endif
    endif

    if (present(lo1)) then
       if ((lo1 > 360.0) .or. (lo1 < -360.0)) then
          write(*,'(" ***** Longitude problem (lo1): ", F20.10)') lo1
          stop "GRIBSEC2_SET"
       endif
       if (lo1 < 0) then
          sec2_lo1 = (2**23) - nint(lo1 * 1.E3)
       else
          sec2_lo1 = nint(lo1 * 1.E3)
       endif
    endif
    if (present(la2)) then
       if ((la2 > 90.0) .or. (la2 < -90.0)) then
          write(*,'(" ***** Latitude problem (la2): ", F20.10)') la2
          stop "GRIBSEC2_SET"
       endif
       if (la2 < 0) then
          sec2_la2 = (2**23) - nint(la2 * 1.E3)
       else
          sec2_la2 = nint(la2*1.E3)
       endif
    endif
    if (present(lo2)) then
       if ((lo2 > 360.0) .or. (lo2 < -360.0)) then
          write(*,'(" ***** Longitude problem (lo2): ", F20.10)') lo2
          stop "GRIBSEC2_SET"
       endif
       if (lo2 < 0) then
          sec2_lo2 = (2**23) - nint(lo2*1.E3)
       else
          sec2_lo2 = nint(lo2*1.E3)
       endif
    endif

    if (present(resolution_and_component)) sec2_rac = resolution_and_component

    if (present(lov)) then
       if ((lov > 360.0) .or. (lov < -360.0)) then
          write(*,'(" ***** Longitude problem (lov): ", F20.10)') lov
          stop "GRIBSEC2_SET"
       endif
       if (lov < 0) then
          sec2_lov = (2**23) - nint(lov*1.E3)
       else
          sec2_lov = nint(lov*1.E3)
       endif
    endif
    if (present(dx)) then
       sec2_dxt = nint(dx*1.E3)
!       print*,' sec2_dxt = ', sec2_dxt
    endif
    if (present(dy)) then
       sec2_dyt = nint(dy*1.E3)
!       print*,' sec2_dyt = ', sec2_dyt
    endif
    if (present(truelat1)) then
       if ((truelat1 > 90.0) .or. (truelat1 < -90.0)) then
          write(*,'(" ***** Latitude problem (truelat1): ", F20.10)') truelat1
          stop "GRIBSEC2_SET"
       endif
       if (truelat1 < 0) then
          sec2_tr1 = (2**23) - nint(truelat1*1.E3)
       else
          sec2_tr1 = nint(truelat1*1.E3)
       endif
    endif
    if (present(truelat2)) then
       if ((truelat2 > 90.0) .or. (truelat2 < -90.0)) then
          write(*,'(" ***** Latitude problem (truelat2): ", F20.10)') truelat2
          stop "GRIBSEC2_SET"
       endif
       if (truelat2 < 0) then
          sec2_tr2 = (2**23) - nint(truelat2*1.E3)
       else
          sec2_tr2 = nint(truelat2*1.E3)
       endif
       sec2_spl = (2**23) - (-90000)
    endif

    if (present(projection_center_flag)) then
       if ((projection_center_flag < 0) .or. (projection_center_flag > 1)) then
          write(*, '(/,"***** FATAL ERROR.  Invalid input to GRIBSEC2_SET")')
          write(*, '("***** PROJECTION_CENTER_FLAG must be 0 or 1.  Found: ",I12)') &
               projection_center_flag
          stop "***** GRIBSEC2_SET"
       endif
       sec2_pcf = projection_center_flag*128
    endif

    if (present(i_scanning_increment)) then
       if(abs(i_scanning_increment) /= 1) then
          write(*, '(/,"***** FATAL ERROR.  Invalid input to GRIBSEC2_SET")')
          write(*, '("***** I_SCANNING_INCREMENT must be +/- 1.  Found: ",I12)') &
               i_scanning_increment
          stop "***** GRIBSEC2_SET"
       endif
       sec2_inc = i_scanning_increment
    endif

    if (present(j_scanning_increment)) then
       if(abs(j_scanning_increment) /= 1) then
          write(*, '(/,"***** FATAL ERROR.  Invalid input to GRIBSEC2_SET")')
          write(*, '("***** J_SCANNING_INCREMENT must be +/- 1.  Found: ",I12)') &
               j_scanning_increment
          stop "***** GRIBSEC2_SET"
       endif
       sec2_jnc = j_scanning_increment
    endif

    if (present(scanning_mode))then
       if ((scanning_mode < 0) .or. (scanning_mode > 1)) then
          write(*, '(/,"***** FATAL ERROR.  Invalid input to GRIBSEC2_SET")')
          write(*,'("***** SCANNING_MODE must be 0 or 1.  Found: ",I12)') scanning_mode
          stop "***** GRIBSEC2_SET"
       endif
       sec2_scm = scanning_mode
    endif

  end subroutine gribsec2_set

  subroutine gribsec2_pack()
    implicit none
    integer :: smf
    integer :: iskip
    iskip = 0
    call sbenc(newsec2, sec2_size,  sec2_size, iskip, 24)
    call sbenc(newsec2,        0,   0, iskip, 8)
    call sbenc(newsec2,      255, 255, iskip, 8)
    call sbenc(newsec2, sec2_gty, 255, iskip, 8)

    smf = 0
    if (sec2_inc == -1) smf = smf + 128
    if ((sec2_jnc ==  1) .or. (sec2_jnc < -999998)) smf = smf + 64
    if (sec2_scm == 1) smf = smf + 32

    select case (sec2_gty)
    case (0) ! Cylindrical Equidistant

       call sbenc(newsec2, sec2_nxp, 0, iskip, 16)
       call sbenc(newsec2, sec2_nyp, 0, iskip, 16)
       call sbenc(newsec2, sec2_la1, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo1, 0, iskip, 24)
       call sbenc(newsec2, sec2_rac, 8, iskip, 8)
       call sbenc(newsec2, sec2_la2, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo2, 0, iskip, 24)
       call sbenc(newsec2, sec2_dxt, 0, iskip, 16)
       call sbenc(newsec2, sec2_dyt, sec2_dxt, iskip, 16)
       call sbenc(newsec2, smf,      0, iskip, 8)

    case (1) ! Mercator
       call sbenc(newsec2, sec2_nxp, 0, iskip, 16)
       call sbenc(newsec2, sec2_nyp, 0, iskip, 16)
       call sbenc(newsec2, sec2_la1, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo1, 0, iskip, 24)
       call sbenc(newsec2, sec2_rac, 8, iskip, 8)
       call sbenc(newsec2, sec2_la2, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo2, 0, iskip, 24)
       call sbenc(newsec2, sec2_tr1, 0, iskip, 24)
       call sbenc(newsec2, 0,      0, iskip, 8)
       call sbenc(newsec2, smf,      0, iskip, 8)
       call sbenc(newsec2, sec2_dxt, 0, iskip, 24)
       call sbenc(newsec2, sec2_dyt, sec2_dxt, iskip, 24)

    case (3) ! Lambert Conformal
       call sbenc(newsec2, sec2_nxp, 0, iskip, 16)
       call sbenc(newsec2, sec2_nyp, 0, iskip, 16)
       call sbenc(newsec2, sec2_la1, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo1, 0, iskip, 24)
       call sbenc(newsec2, sec2_rac, 8, iskip, 8)
       call sbenc(newsec2, sec2_lov, 0, iskip, 24)
       call sbenc(newsec2, sec2_dxt, 0, iskip, 24)
       call sbenc(newsec2, sec2_dyt, sec2_dxt, iskip, 24)
       call sbenc(newsec2, sec2_pcf, 0, iskip, 8)
       call sbenc(newsec2, smf,      0, iskip, 8)
       call sbenc(newsec2, sec2_tr1, 0, iskip, 24)
       call sbenc(newsec2, sec2_tr2, 0, iskip, 24)
       call sbenc(newsec2, sec2_spl, 0, iskip, 24)
       call sbenc(newsec2, 0, 0, iskip, 24)


    case (5) ! Polar Stereographic

       call sbenc(newsec2, sec2_nxp, 0, iskip, 16)
       call sbenc(newsec2, sec2_nyp, 0, iskip, 16)
       call sbenc(newsec2, sec2_la1, 0, iskip, 24)
       call sbenc(newsec2, sec2_lo1, 0, iskip, 24)
       call sbenc(newsec2, sec2_rac, 8, iskip, 8)
       call sbenc(newsec2, sec2_lov, 0, iskip, 24)
       call sbenc(newsec2, sec2_dxt, 0, iskip, 24)
       call sbenc(newsec2, sec2_dyt, sec2_dxt, iskip, 24)
       call sbenc(newsec2, sec2_pcf, 0, iskip, 8)
       call sbenc(newsec2, smf,      0, iskip, 8)
    case default
       write(*, '("sec2_gty = ", I12)') sec2_gty
       stop "Unrecognized grid type in GRIBSEC2_PACK"
    end select
    
  end subroutine gribsec2_pack



  subroutine gribsec4_set(binary_scale_factor, reference_value_A, reference_value_B, &
       reference_value_sign, nbits)
    implicit none
    integer, optional, intent(in) :: binary_scale_factor, nbits, reference_value_A, &
         reference_value_B, reference_value_sign

    if (present(binary_scale_factor)) sec4_bsf = binary_scale_factor
    if (present(reference_value_A))   sec4_rfa = reference_value_A
    if (present(reference_value_B))   sec4_rfb = reference_value_B
    if (present(reference_value_sign))sec4_rfs = reference_value_sign
    if (present(nbits))               sec4_nbt = nbits
  end subroutine gribsec4_set

  subroutine gribsec4_pack
    implicit none
    integer :: iskip, numunused, bmod, i, j, ist, ien, inc, jst, jen, jnc

    if (sec4_nbt == 0) then
       ! We have a constant field.  Zero bits needed (sec4_nbt==0) 
       ! to pack each datum.
       sec4_size = 12
       allocate(newsec4((sec4_size+3)/4))
       newsec4 = 0
       iskip = 0
       call sbenc(newsec4, sec4_size, -999999, iskip, 24) ! Bytes 1-3
       call sbenc(newsec4, 0, 0, iskip, 4)                ! Byte 4, bits 1-4
       call sbenc(newsec4, 8, 0, iskip, 4)                ! Byte 4, bits 5-8
       call sbenc(newsec4, sec4_bsf, 0, iskip, 16)        ! Bytes 5-6

       if (sec4_rfs == -1) then                          ! Byte 7...
          call sbenc(newsec4, 1, 0, iskip, 1)
       elseif (sec4_rfs == 1) then
          call sbenc(newsec4, 0, 0, iskip, 1)
       else
          print*, "SEC4_RFS (Reference value sign) = ", sec4_rfs
          stop
       endif
       call sbenc(newsec4, sec4_rfa, 0, iskip, 7)
          
       call sbenc(newsec4, sec4_rfb, 0, iskip, 24)        ! Bytes 8-10
       call sbenc(newsec4, 0, 0, iskip, 8)                ! Byte 11
       call sbenc(newsec4, 0, 0, iskip, 8)                ! Byte 12

       if (sec1_bms == 1) then

          ! We have a constant field with a bitmask

          call gribsec3_create()

          bmod = mod(nummappt*sec4_nbt, 8)
          if (bmod == 0) then
             numunused = 0
          else
             numunused = 8 - bmod
          endif

          sec4_size = ((nummappt*sec4_nbt)+numunused) / 8 + 11
          if (mod(sec4_size,2)/=0) then
             sec4_size = sec4_size + 1
             numunused = numunused + 8
          endif
          if (mod(((nummappt*sec4_nbt)+numunused),16) /= 8) then
             print*, 'nummappt = ', nummappt
             print*, 'sec4_nbt = ', sec4_nbt
             print*, 'sec4_size = ', sec4_size
             print*, 'numunused = ', numunused
             stop "Problem A"
          endif
       endif

       return
    endif

    ! There must be an even number of bytes.

    if (sec1_bms == 1) then

       call gribsec3_create()

       bmod = mod(nummappt*sec4_nbt, 8)
       if (bmod == 0) then
          numunused = 0
       else
          numunused = 8 - bmod
       endif

       sec4_size = ((nummappt*sec4_nbt)+numunused) / 8 + 11
       if (mod(sec4_size,2)/=0) then
          sec4_size = sec4_size + 1
          numunused = numunused + 8
       endif
       if (mod(((nummappt*sec4_nbt)+numunused),16) /= 8) then
          print*, 'sec4_size = ', sec4_size
          print*, 'numunused = ', numunused
          stop "Problem A"
       endif

    else

       bmod = mod(size(gribiarr)*sec4_nbt, 8)
       if (bmod == 0) then
          numunused = 0
       else
          numunused = 8 - bmod
       endif
       sec4_size = (size(gribiarr)*sec4_nbt+numunused) / 8 + 11

       if (mod(sec4_size,2) /= 0) then
          sec4_size = sec4_size + 1
          numunused = numunused + 8
       endif
       
       if (mod(((size(gribiarr)*sec4_nbt)+numunused),16) /= 8) then
          print*, 'numused = ', size(gribiarr)*sec4_nbt
          print*, 'sec4_size = ', sec4_size
          print*, 'numunused = ', numunused
          stop "Problem B"
       endif

    endif

    allocate(newsec4((sec4_size+3)/4))
    newsec4 = 0
    iskip = 0
    call sbenc(newsec4, sec4_size, -999999, iskip, 24)   ! Bytes 1-3
    call sbenc(newsec4, 0, 0, iskip, 4)                  ! Byte 4, bits 1-4
    call sbenc(newsec4, numunused, 0, iskip, 4)          ! Byte 4, bits 5-8
    call sbenc(newsec4, sec4_bsf, 0, iskip, 16)          ! Bytes 5-6

    if (sec4_rfs == -1) then                          ! Byte 7...
       call sbenc(newsec4, 1, 0, iskip, 1)
    elseif (sec4_rfs == 1) then
       call sbenc(newsec4, 0, 0, iskip, 1)
    else
       print*, "SEC4_RFS (Reference value sign) = ", sec4_rfs
       stop
    endif

    call sbenc(newsec4, sec4_rfa, 0, iskip, 7)

    call sbenc(newsec4, sec4_rfb, 0, iskip, 24) ! Bytes 8-10
    call sbenc(newsec4, sec4_nbt, 0, iskip, 8)
    
    if ((sec2_inc == 1) .or. (sec2_inc < -999998)) then
       ist = 1
       ien = size(gribiarr,1)
       inc = 1
    elseif (sec2_inc == -1) then
       ist = size(gribiarr,1)
       ien = 1
       inc = -1
    endif

    if ((sec2_jnc == 1) .or. (sec2_jnc < -999998)) then
       jst = 1
       jen = size(gribiarr,2)
       jnc = 1
    elseif (sec2_jnc == -1) then
       jst = size(gribiarr,2)
       jen = 1
       jnc = -1
    endif

    if (sec1_bms == 1) then
       call loop_bitmapped(gribiarr,size(gribiarr,1),size(gribiarr,2), &
            ist,ien,inc,jst,jen,jnc,iskip)
    else
       call loop_not_bitmapped(gribiarr,size(gribiarr,1),size(gribiarr,2), &
            ist,ien,inc,jst,jen,jnc,iskip)
    endif


    do i = 1, numunused
       call sbenc(newsec4, 0, 0, iskip, 1)
    enddo

  end subroutine gribsec4_pack

  subroutine loop_bitmapped(iarr,ix,jx,ist,ien,inc,jst,jen,jnc,iskip)
    implicit none
    integer, intent(in) :: ist,ien,inc,jst,jen,jnc,ix,jx
    integer, dimension(ix,jx), intent(in) :: iarr
    integer, intent(inout) :: iskip
    integer :: i, j

    if ((sec2_scm == 0) .or. (sec2_scm < -999998)) then

       do j = jst, jen, jnc
          do i = ist, ien, inc
             if (lbm(i,j)) then
                call sbenc(newsec4, iarr(i,j), -999999, iskip, sec4_nbt)
             endif
          enddo
       enddo

    else if (sec2_scm == 1) then
       
       do i = ist, ien, inc
          do j = jst, jen, jnc
             if (lbm(i,j)) then
                call sbenc(newsec4, iarr(i,j), -999999, iskip, sec4_nbt)
             endif
          enddo
       enddo

    endif

  end subroutine loop_bitmapped

  subroutine loop_not_bitmapped(iarr,ix,jx,ist,ien,inc,jst,jen,jnc,iskip)
    integer, intent(in) :: ist,ien,inc,jst,jen,jnc,ix,jx
    integer, dimension(ix,jx), intent(in) :: iarr
    integer, intent(inout) :: iskip
    integer :: i, j

    if ((sec2_scm == 0) .or. (sec2_scm < -999998)) then

       do j = jst, jen, jnc
          do i = ist, ien, inc
             call sbenc(newsec4, iarr(i,j), -999999, iskip, sec4_nbt)
          enddo
       enddo

    else if (sec2_scm == 1) then
       
       do i = ist, ien, inc
          do j = jst, jen, jnc
             call sbenc(newsec4, iarr(i,j), -999999, iskip, sec4_nbt)
          enddo
       enddo

    endif

  end subroutine loop_not_bitmapped

  subroutine packarray(input_array, ix, jx, sigdig, eps, &
       bitmap)
!
! Given a floating-point array input_array, returns an integer array gribiarr
! which represents integer values ready for packing into the GRIB
! record.
!
    implicit none
    integer,                   intent(in)  :: ix, jx
    real, dimension(ix,jx),    intent(in)  :: input_array
    integer, optional,         intent(in)  :: sigdig
    real,    optional,         intent(in)  :: eps
    logical, dimension(ix,jx), intent(in), optional :: bitmap
! Local:
    integer :: npb
    integer :: Binary_Scale_Factor
    integer :: Decimal_Scale_Factor
    integer :: reference_value_A
    integer :: reference_value_B
    integer :: reference_value_sign
    real :: xmax, xbmax, xrange, xval
    real :: xmin, dsmin
    real :: dsmax, bsmax, bsmin, testdiff, tnt
    integer :: idcflog
    real :: maxn
    real :: lval
    double precision, allocatable, dimension(:) :: arr
    double precision :: dval
    integer :: nval, BetterD, BetterE
    real :: eval, rval
    logical :: invb
    integer :: i, j, iia, jja

    real,    allocatable, dimension(:) :: xarr
    integer, allocatable, dimension(:) :: iarr

    if (present(sigdig) .and. present(eps)) then
       write(*,'("PACKARRAY: Either SIGDIG or EPS must be provided.")')
       write(*,'("                         You have provided both.")')
       stop "ENGRIB_MODULE : PACKARRAY"
    endif

    if (.not.(present(sigdig)) .and. .not.(present(eps))) then
       write(*,'("PACKARRAY: Either SIGDIG or EPS must be provided.")')
       write(*,'("                         You have provided neither.")')
       stop "ENGRIB_MODULE : PACKARRAY"
    endif

    ! Pack into the vector xarr the desired elements of the 2d input_array.
    if (present(bitmap)) then
       ! print*, 'count(bitmap) = ', count(bitmap)
       if (count(bitmap) > 0) then
          if (count(bitmap) == ix*jx) then
             ! the provided bitmap includes all points in the field.  Effectively, no bitmap.
             allocate(xarr(ix*jx))
             xarr = reshape(input_array, (/ix*jx/))
          else if (count(bitmap) < ix*jx) then
             allocate(xarr(count(bitmap)))
             xarr = pack(input_array, bitmap)
          else
             stop "bitmap confusion"
          endif
       else
          allocate(xarr(1))
          xarr = 0.
       endif
    else
       allocate(xarr(ix*jx))
       xarr = reshape(input_array, (/ix*jx/))
    endif

    ! Clear out the space for a new grib record.
    call grib_clear

    call gribsec2_set(nx = ix)
    call gribsec2_set(ny = jx)

! Copy the bitmask
    if (present(bitmap)) then
       if (count(bitmap) < ix*jx) then
          allocate(lbm(ix,jx))
          lbm = bitmap
          nummappt = count(lbm)
          call gribsec1_set(isthere_bms = 1)
       endif
    endif

! Copy the original data into a work array

    allocate(iarr(size(xarr)))
    allocate(arr(size(xarr)))
    allocate(gribiarr(ix,jx))

    arr = dble(xarr)

! Get the min and max of the original data
    xmin = minval(xarr)
    xmax = maxval(xarr)

    ! print*, 'xmin, xmax = ', xmin, xmax

! Find a decimal scale factor D, such that, when the original data (which is
! assumed to now be in the proper GRIB-recognized, i.e., mostly SI, units) is
! multiplied by 10**D, the integer part of the result will have enough
! precision to contain the appropriate information.

    if (present(sigdig)) then
       if ((xmin == 0.0) .and. (xmax == 0.0)) then
          Decimal_Scale_Factor = 0.0
       else
          Decimal_Scale_Factor = sigdig-(ceiling(log10(max(abs(xmin),abs(xmax)))))
       endif
    endif
    if (present(eps)) then
       Decimal_Scale_Factor = -1 + log10(1./eps) ! 1 + log10(1./eps)
       ! print*, 'eps = ', eps
       ! print*, 'log10(1./eps) = ', log10(1./eps)
       ! print*, 'Decimal Scale Factor = ', decimal_scale_factor
    endif

100 continue
    if (present(eps)) then
       testdiff = 1.E33
       do while ( testdiff > eps )
          Decimal_Scale_Factor = Decimal_Scale_Factor + 1
          ! print*, 'Decimal_Scale_Factor = ', Decimal_Scale_Factor
          arr = dble(xarr) * (10.D0**decimal_scale_factor)
          ! Try a quick comparison between original array (input_array) and work array
          testdiff = maxval(abs(((int(arr))*(10.0**(-Decimal_Scale_Factor))) - xarr))
          ! print*, 'testdiff = ', testdiff
       enddo
    endif

    if (present(sigdig)) then
       arr = dble(xarr) * (10.D0**decimal_scale_factor)
    endif

! Build the reference value integers:
! The reference value is simply the minimum of the decimal-scaled values.

    ! Find the decimal-scaled xmin and xmax
    dsmin = minval(arr)
    dsmax = maxval(arr)

    if (present(bitmap)) then
       if (count(bitmap) == 0) then
          ! Everything is masked out.  Nothing to encode.
          call gribsec1_set(isthere_bms = 0)
          Binary_Scale_Factor = 0
          gribiarr = 0
          call find_pack_ref_val(dsmin, Reference_Value_A, Reference_Value_B, Reference_Value_Sign, rval)

          call gribsec4_set(binary_scale_factor = 0)
          call gribsec4_set(reference_value_A = reference_value_A)
          call gribsec4_set(reference_value_B = reference_value_B)
          call gribsec4_set(reference_value_Sign = reference_value_Sign)
          call gribsec4_set(nbits = 0)
          call gribsec1_set(decimal_scale_factor = Decimal_Scale_Factor)
          return
       endif
    endif

    call find_pack_ref_val(dsmin, Reference_Value_A, Reference_Value_B, Reference_Value_Sign, rval)

! Subtract off the Reference Value:
    arr = arr - rval

! Find a maximum of the new arr
    maxn = maxval(arr)

! We'll not use binary scaling for now.

    ! Number of bits needed for packing depends on the maximum value:
    lval = (log10(maxn+1)/log10(2.0))
    npb = ceiling(abs(lval))
    Binary_Scale_Factor = 0

!KWM! Find a better D and E, perhaps, for a given arrmax and a given number
!KWM! of bits.
!KWM    call better_D_and_E(npb, real(xmax-xmin), BetterD, BetterE, Decimal_Scale_Factor, Binary_Scale_Factor)
!KWM
!KWM    print*, 'Original D, E: ', Decimal_Scale_Factor, Binary_Scale_Factor
!KWM    print*, 'Better   D, E: ', BetterD, BetterE

    iarr = nint(arr) ! Do we want "INT" or "NINT" ?

    ! Double check that IARR is all non-negative
    do i = 1, size(iarr)
       if (iarr(i) < 0) then
          print*,' i, ', i, arr(i), int(arr(i))
          stop "PACKARRAY problem"
       endif
    enddo

    dval = 1.0D1**(-Decimal_Scale_Factor)
!  eval = 2.**Binary_Scale_Factor
    eval = 1.0

! one final test
    testdiff = maxval(abs(((rval+dble(iarr)*eval)*dval)-xarr))
    if (present(eps)) then
       if (testdiff > eps) then
          print*, '      Trying again:', testdiff, eps
          Decimal_Scale_Factor = Decimal_Scale_Factor + 1
          goto 100
       endif
    endif

! Find the maximum difference between the original data and the 
! unpacking of the packed data.
    testdiff = maxval(abs(((rval+dble(iarr)*eval)*dval)-xarr))
    ! print*, '     Maximum abs(value), difference = ', xmax, testdiff

    call gribsec1_set(decimal_scale_factor = Decimal_Scale_Factor)
    call gribsec4_set(binary_scale_factor = binary_scale_factor)
    call gribsec4_set(reference_value_A = reference_value_A)
    call gribsec4_set(reference_value_B = reference_value_B)
    call gribsec4_set(reference_value_Sign = reference_value_Sign)
    call gribsec4_set(nbits = npb)
!    write(*, '(5x,"DSF NB MAX = ",2I4,2I12, 1P, 2E14.4, 0P)') &
!         Decimal_Scale_Factor, npb, eps, testdiff

    ! And finally, unpack IARR into GRIBIARR
    if (present(bitmap)) then
       if (count(bitmap) < ix*jx) then
          gribiarr = unpack(iarr, bitmap, -9999999)
       else
          gribiarr = reshape ( iarr, (/ ix , jx /) )
       endif
    else
       gribiarr = reshape ( iarr, (/ ix , jx /) )
    endif

  end subroutine packarray

  subroutine better_D_and_E(npb, maxn, BetterD, BetterE, OrigD, OrigE)
    implicit none
    integer, intent(in)  :: npb
    real   , intent(in)  :: maxn
    integer, intent(in)  :: OrigD, OrigE
    integer, intent(out) :: BetterD, BetterE

    real :: m, tsave, xd, xm
    integer :: D, E, Dsave, Esave
    logical :: found


    tsave = 1.E35
    found = .FALSE.
    m = log10(((2.0**npb)-1)/maxn)
    ! For a given M and a variety of E values, find D and take an int.

    do E = -10, 10
       xd = m + float(E)*log10(2.0)
       D = floor(xd)
       xm = float(D) - float(E)*log10(2.0)

       ! xm should be <= m
       if (xm > m) then
          print*, 'xd, D = ', xd, D
          stop "problem:  XM > M"
       endif

       ! Save D and E values where m-xm is minimized
       if ((m-xm)<tsave) then
          found  = .TRUE.
          Esave = E
          Dsave = D
          tsave = (m-xm)
       endif
       
    enddo

    if (found) then
       BetterD = D
       BetterE = E
    else
       print*, 'Better D and E not found'
       BetterD = OrigD
       BetterE = OrigE
    endif

  end subroutine better_D_and_E


  subroutine find_pack_ref_val(x, a, b, isign, rval)
    ! Find the A and B parameters of the grib reference value xmin.
    implicit none
    real,    intent(in)  :: x
    integer, intent(out) :: a, b, isign
    real,    intent(out) :: rval

    real :: y, s
    integer :: e

    if (x==0) then
       a = 0
       b = 0
       isign = 1
       rval = 0.
       return
    endif

    y = abs(x)
    s = sign(1.0, x)
    e = exponent(y)
    isign = int(s)

    b = int(scale(fraction(y), 24+mod(e+128,4)))
    a = ((e-mod(e+128,4))/4) + 64

    rval = s * ((2.0**(-24)) *b ) * (16.0**(a-64))

! Check for exact floating-point values.  Pretty stringent
! and probably not terribly portable, but that's life.
    if (rval /= x) then
       print*, 'A and B not quite right: a,b=',a,b
       print*, 'rval = ', rval
       print*, 'x = ', x
       stop "FIND_PACK_REF_VAL"
    endif

! B must fit into 24 bits (3 bytes)
! A must fit into  7 bits
    do while (b > 16777216)
       a = a + 1

       if (a > 127) then
          print*, 'Having trouble packing reference value.'
          stop "FIND_PACK_REF_VAL"
       endif

       if (isign > 0) then
          b = b / 16
       else
          b = (b+15)/16
       endif
       rval = s * ((2.0**(-24)) *b ) * (16.0**(a-64))
       if (rval > x) then
          print*, 'rval > x:', rval, x
          stop "FIND_PACK_REF_VAL"
       endif
    enddo

  end subroutine find_pack_ref_val

end module engrib_module_test

PROGRAM  sw_to_grib
  use engrib_module_test
  use module_date_utilities
  implicit none
!
!       This is an example Fortran program to read the GCIP/SRB archive
!       of current instantaneous fluxes for a particular day.  It reads
!       the data for an observation time by looping over the 51 latitude
!       bands one at a time, and in each pass through the loop, reading
!       data for the 111 longitude bands, and then proceeding to the
!       next time.
!
!       The program also assigns latitude and longitude coordinates to
!       the cell-centers, and gives an example of how to obtain the
!       nominal time of an observation.
!
!       NOTE: Northern latitudes are positive, western longitudes are
!             negative.
!
!  VARIABLES:
!     Fluxi(Lon,Lat,Hour) : Lon=1 to Nlon, Lat=1 to Nlat; Hour=1 to
!                                  Nhours; instantaneous flux (W/m**2)
!     Glat(Lat)  : Lat=1 to Nlat; latitude of cell center
!     Glon(Lon)  : Lon=1 to Nlon; longitude of cell center
!     Hour       : hour of the day (0-23)
!     Lat1       : latitude of center of first cell (degrees)
!     Lon1       : longitude of center of first cell (degrees)
!     Lrec       : record length
!     Minute     : minutes; satellite observation is Minute minutes
!                   after the hour
!     Nhours     : no. of hours in a day
!     Nlat       : max no. of latitudes
!     Nlon       : max no. of longitudes
!     Reslat     : resolution in latitude (degrees)
!     Reslon     : resolution in longitude (degrees)
!     Time       : nominal time (UTC) of satellite observation (hours)


  integer :: Nlat
  integer :: Nlon
  real    :: Lat1
  real    :: Lon1
!     .. Parameters ..
  integer, parameter :: Nhours = 24
  real   , parameter :: Reslat = 0.5
  real   , parameter :: Reslon = 0.5
  integer, parameter :: Minute = 15
  integer :: additional_time_offset

  integer, external :: iargc

!     ..
!     .. Local Scalars ..
  CHARACTER(len=120) :: Fname
  INTEGER  :: Hour,  Lat, Lon
  REAL  :: Time
  integer :: ierr
!     ..
!     .. Local Arrays ..
  REAL, allocatable, dimension(:, :, :) :: Fluxi
  real, allocatable, dimension(:)       :: Glat
  real, allocatable, dimension(:)       :: Glon
  REAL, allocatable, dimension(:, :)    :: tmp

!     ..
  character(len=24) :: hdate, newdate
  integer :: indxs
  integer :: i
  integer :: gribunit

! Get name of GCIP/SRB file to be read from command-line.
!

! This one expect daily files, named according to <yymmdd>sda.i

  if (iargc() < 1) then
     print*, 'Program expects a command-line argument.'
     write(*, '(/,"***** ERROR EXIT *****",/)')
     stop
  endif
  call getarg(1, Fname)

  hdate = "2000-00-00_00:00:00.0000"
  indxs = index(trim(Fname),"/",.TRUE.)
  hdate(3:4)  = Fname(indxs+1:indxs+2) ! yy
  hdate(6:7)  = Fname(indxs+3:indxs+4) ! mm
  hdate(9:10) = Fname(indxs+5:indxs+6) ! dd
  if ( hdate(1:4) > "2050" ) hdate(1:2) = "19"
  
  print*, 'Hdate = ', Hdate

  if (hdate(1:7) < "2001-07") then
     NLat = 51
     NLon = 111
     Lat1 = 25.0
     Lon1 = -125.0
  else
     NLat = 61
     NLon = 121
     Lat1 = 24.0
     Lon1 = -126.0
  endif

  allocate(Fluxi(Nlon, Nlat, Nhours))
  allocate(Glat(Nlat))
  allocate(Glon(Nlon))
  Fluxi = 0.

! Open and read GCIP/SRB file
! Use c I/O routines to avoid the direct-access Fortran I/O

  call getdata(Fname, Fluxi, Nlon, Nlat, Nhours)

  ! print*, 'Minimum/Maximum Flux = ', minval(Fluxi), maxval(fluxi)
!KWM     print*, 'Min/Max fractional part = ', minval(abs(Fluxi-int(fluxi))), &
!KWM          maxval(abs(Fluxi-int(fluxi)))

! Calculate latitude and longitude coordinates of cell centers
!
  DO Lat = 1, Nlat
     Glat( Lat ) = ( Lat-1 ) * Reslat + Lat1
  ENDDO
  DO Lon = 1, Nlon
     Glon( Lon ) = ( Lon-1 ) * Reslon + Lon1
  ENDDO

  where(fluxi < -998)
     fluxi = -1.E30
  end where

  DO Hour = 1, Nhours
     newdate = hdate

     write(*,'("Assuming that the SW Data offset is about 30 minutes, rather than the stated 15 minutes")')
     additional_time_offset = 15

     call geth_newdate(newdate(1:16), hdate(1:16), (hour-1)*60+Minute + additional_time_offset)
     print*, 'newdate = ', newdate(1:16)

     call open_newgrib(gribunit, "SW."//newdate(1:4)//newdate(6:7)//newdate(9:10)//newdate(12:13)//newdate(15:16)//".grb")
     ! Write the data in GRIB format
!     call packarray(fluxi(:,:,hour), Nlon, Nlat, eps=1.00, bitmap=(fluxi(:,:,hour)>=0.0))
     call packarray(fluxi(:,:,hour), Nlon, Nlat, eps=1.00, bitmap=(fluxi(:,:,hour)>=-100.0))
     call gribsec1_set(parameter_id=204)
     call gribsec1_set(level_type=1)
     call gribsec1_set(level_1 = 0)
     call gribsec1_set(hdate=newdate(1:16))
     call gribsec1_set(P1=00)
     call gribsec1_set(P2=00)
     call gribsec1_set(time_unit=1)
     call gribsec1_set(time_range_indicator=0)
     call gribsec1_set(center_id=244)
     call gribsec1_set(process_id=244)
     call gribsec1_set(subcenter_id=244)

     call gribsec2_set(grid_type=0)
     call gribsec2_set(la1=Glat(1))
     call gribsec2_set(lo1=Glon(1))
     call gribsec2_set(la2=Glat(Nlat))
     call gribsec2_set(lo2=Glon(Nlon))
     call gribsec2_set(dx=reslon)
     call gribsec2_set(dy=reslat)
     !call gribsec2_set(resolution_and_component=)
     !call gribsec2_set(scanning_mode=)

     call write_grib(gribunit)
     call close_newgrib(gribunit)

     if (allocated(lbm)) deallocate(lbm)
     if (allocated(gribiarr)) deallocate(gribiarr)
     if (allocated(newsec2)) deallocate(newsec2)
     if (allocated(newsec3)) deallocate(newsec3)
     if (allocated(newsec4)) deallocate(newsec4)
     
  enddo

END PROGRAM Sw_to_grib

subroutine getdata(Fname, Fluxi, Nlon, Nlat, Nhours)
  implicit none
  integer :: Nlon, Nlat, Nhours, icount
  real, dimension(Nlon, Nlat, Nhours) :: Fluxi
  character(len=*) :: Fname
  integer :: munit
  integer :: iread
  integer :: ierr
  integer :: Lrec
  Lrec = Nlon * Nlat * Nhours * 4

! Open and read the data.
  call copen(Munit,trim(Fname)//char(0), 1, ierr, 0)
  if ((ierr /= 0).or.(Munit == -1)) then
     print*, 'Error Opening.'
     write(*, '(/,"***** ERROR EXIT *****",/)')
     stop
  endif

  call bnread(Munit, Fluxi, Lrec, iread, ierr, 10)
  if ((ierr /= 0) .or. (iread /= Lrec)) then
     print*, 'Error Reading.'
     write(*, '(/,"***** ERROR EXIT *****",/)')
     stop
  endif

!KWM  call cclose(Munit,0,ierr)
!KWM  if (ierr /= 0) then
!KWM     print*, 'Error Closing.'
!KWM     write(*, '(/,"***** ERROR EXIT *****",/)')
!KWM     stop
!KWM  endif

end subroutine getdata

