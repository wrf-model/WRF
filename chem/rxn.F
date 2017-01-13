!=============================================================================*
! This file contains the following subroutines, related to reading/loading
! the product (cross section) x (quantum yield) for photo-reactions:
!     r01 through r47
!     r101 through r148, skipped r116,r117, added pxCH2O
!=============================================================================*

      module module_rxn

      use module_params

      IMPLICIT NONE

      private :: fo3qy2, qyacet

      logical, private :: initialize = .true.
      integer, parameter :: max_files = 5

      integer :: npht, npht_tab
      
      type file_specs
        integer            :: nfiles
        integer            :: nskip(max_files)
        integer            :: nread(max_files)
        real               :: xfac(max_files)
        character(len=132) :: filename(max_files)
      end type file_specs

      type xs_qy_tab
        integer :: tpflag
        integer :: channel
        integer :: jndx
        real    :: qyld
        real, allocatable :: sq(:,:)
        character(len=50) :: label
        character(len=50) :: wrf_label
        type(xs_qy_tab), pointer :: next
        type(xs_qy_tab), pointer :: last
        type(file_specs)  :: filespec
      end type xs_qy_tab

      type(xs_qy_tab), allocatable, target :: xsqy_tab(:)
      type(xs_qy_tab), pointer             :: xsqy_tab_head
      type(xs_qy_tab), pointer             :: xsqy_tab_tail

!=====================================================================
!  the following is fortran2003 compliant code
!=====================================================================
      type xsqy_subs
        procedure(xsqy), nopass, pointer :: xsqy_sub
      end type xsqy_subs

      abstract interface
        SUBROUTINE xsqy(nw,wl,wc,nz,tlev,airden,j)

          use module_params

          INTEGER, intent(in) :: nw
          INTEGER, intent(in) :: nz
          REAL, intent(in)    :: wl(kw), wc(kw)
          REAL, intent(in)    :: tlev(kz)
          REAL, intent(in)    :: airden(kz)

          INTEGER, intent(inout) :: j
        end SUBROUTINE xsqy
      end interface

      type(xsqy_subs), allocatable :: the_subs(:)

      CONTAINS

      SUBROUTINE no_z_dep(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!  generic routine
!-----------------------------------------------------------------------------*

      use module_params

! input

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL,    intent(in) :: wl(kw), wc(kw)
      REAL,    intent(in) :: airden(kz)
      REAL,    intent(in) :: tlev(kz)

      integer, PARAMETER :: kdata=500

! local
      REAL :: x1(kdata)
      REAL :: y1(kdata)

      INTEGER :: wn
      REAL    :: yg(kw)

      if( initialize ) then
        CALL readit
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        if( xsqy_tab(j)%qyld == 1. ) then
!*** quantum yield assumed to be unity
          xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1)
        else
          xsqy_tab(j)%sq(1:nw-1,1) = xsqy_tab(j)%qyld * yg(1:nw-1)
        endif
      endif

      CONTAINS 

      SUBROUTINE readit

      INTEGER :: ierr
      integer :: n, fileno
      character(len=132) :: filename

      do fileno = 1,xsqy_tab(j)%filespec%nfiles
        filename = trim( xsqy_tab(j)%filespec%filename(fileno) )
        n = xsqy_tab(j)%filespec%nread(fileno)
        if( xsqy_tab(j)%filespec%nskip(fileno) >= 0 ) then
          CALL base_read( filespec=trim(filename), &
                          skip_cnt=xsqy_tab(j)%filespec%nskip(fileno), &
                          rd_cnt  =n,x=x1,y=y1 )
        else
          CALL base_read( filespec=trim(filename),rd_cnt=n,x=x1,y=y1 )
        endif
        y1(1:n) = y1(1:n) * xsqy_tab(j)%filespec%xfac(fileno)

        CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                             nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      enddo

      END SUBROUTINE readit

      END SUBROUTINE no_z_dep

      LOGICAL FUNCTION get_initialization()

      get_initialization = initialize

      END FUNCTION get_initialization

      SUBROUTINE set_initialization( status )

      LOGICAL, intent(in) :: status

      initialize = status

      END SUBROUTINE set_initialization

      SUBROUTINE rxn_init( nw, wl )
!---------------------------------------------
!  initialize wrf-tuv
!---------------------------------------------

      use module_xsections, only : rdxs_init

      integer, intent(in) :: nw
      real, intent(in)    :: wl(nw)

      integer :: astat, m, n
      character(len=256) :: emsg

      call set_initialization( status=.true. )

      if( .not. allocated( xsqy_tab ) ) then
         allocate( xsqy_tab(kj),stat=astat )
         if( astat /= 0 ) then
           write(emsg,'(''rxn_init: failed to allocate xsqy_tab; error = '',i4)') astat
           call wrf_error_fatal( trim(emsg) )
         endif
      endif
      if( .not. allocated( the_subs ) ) then
         allocate( the_subs(kj),stat=astat )
         if( astat /= 0 ) then
           write(emsg,'(''rxn_init: failed to allocate xsqy_tab subs; error = '',i4)') astat
           call wrf_error_fatal( trim(emsg) )
         endif
      endif

      nullify( xsqy_tab_head )
      nullify( xsqy_tab_tail )
      
      xsqy_tab(1:kj)%tpflag  = 0
      xsqy_tab(1:kj)%channel = 1
      xsqy_tab(1:kj)%label   =  ' '
      xsqy_tab(1:kj)%qyld    =  1.
      xsqy_tab(1:kj)%filespec%nfiles =  1
      do m = 1,max_files
        xsqy_tab(1:kj)%filespec%nskip(m) =  0
        xsqy_tab(1:kj)%filespec%nread(m) =  0
        xsqy_tab(1:kj)%filespec%xfac(m)  =  1.e-20
        xsqy_tab(1:kj)%filespec%filename(m) = ' '
      end do
      do m = 1,kj
        nullify( xsqy_tab(m)%next )
        nullify( xsqy_tab(m)%last )
        the_subs(m)%xsqy_sub => null()
      end do

      npht_tab = 2
      call setup_sub_calls( the_subs,npht_tab )

      call diagnostics

      call rdxs_init( nw, wl )

      END SUBROUTINE rxn_init

      subroutine setup_sub_calls( subr, m )

      integer, intent(inout) :: m
      type(xsqy_subs), intent(inout) :: subr(:)

      xsqy_tab(m)%label   = 'O3 -> O2 + O(1D)'
      xsqy_tab(m+1)%label = 'O3 -> O2 + O(3P)'
      xsqy_tab(m)%wrf_label   = 'j_o1d'
      xsqy_tab(m+1)%wrf_label = 'j_o3p'
      xsqy_tab(m:m+1)%jndx = (/ m,m+1 /)
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      subr(m)%xsqy_sub   => r01
      subr(m+1)%xsqy_sub => r01
      m = m + 2

      xsqy_tab(m)%label = 'NO2 -> NO + O(3P)'
      xsqy_tab(m)%wrf_label = 'j_no2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/YLD/NO2_jpl11.yld'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 25
      subr(m)%xsqy_sub   => r02
      m = m + 1

      xsqy_tab(m)%label   = 'NO3 -> NO + O2'
      xsqy_tab(m+1)%label = 'NO3 -> NO2 + O(3P)'
      xsqy_tab(m)%wrf_label   = 'j_no3_a'
      xsqy_tab(m+1)%wrf_label = 'j_no3_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/NO3_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 289
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/YLD/NO3_jpl2011.qy'
      xsqy_tab(m)%filespec%nskip(2) = 5
      xsqy_tab(m)%filespec%nread(2) = 56
      xsqy_tab(m)%filespec%xfac(2)  = 1.e-3
      subr(m)%xsqy_sub   => r03
      subr(m+1)%xsqy_sub => r03
      m = m + 2

      xsqy_tab(m)%label   = 'N2O5 -> NO3 + NO + O(3P)'
      xsqy_tab(m+1)%label = 'N2O5 -> NO3 + NO2'
      xsqy_tab(m)%wrf_label   = 'j_n2o5_a'
      xsqy_tab(m+1)%wrf_label = 'j_n2o5_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/N2O5_jpl11.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/ABS/N2O5_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1:2) = (/ 4,111 /)
      xsqy_tab(m)%filespec%nread(1:2) = (/ 103,8 /)
      subr(m)%xsqy_sub   => r04
      subr(m+1)%xsqy_sub => r04
      m = m + 2

      xsqy_tab(m)%label = 'HNO2 -> OH + NO'
      xsqy_tab(m)%wrf_label = 'j_hno2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HONO_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 192
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'HNO3 -> OH + NO2'
      xsqy_tab(m)%wrf_label = 'j_hno3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HNO3_burk.abs'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 83
      subr(m)%xsqy_sub   => r06
      m = m + 1

      xsqy_tab(m)%label = 'HNO4 -> HO2 + NO2'
      xsqy_tab(m)%wrf_label = 'j_hno4'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HNO4_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 54
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'H2O2 -> 2 OH'
      xsqy_tab(m)%wrf_label = 'j_h2o2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%nfiles      = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/H2O2_jpl94.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/ABS/H2O2_Kahan.abs'
      xsqy_tab(m)%filespec%nskip(1:2) = (/ -1,0 /)
      xsqy_tab(m)%filespec%nread(2)   = 494
      subr(m)%xsqy_sub   => r08
      m = m + 1

      xsqy_tab(m)%label = 'CHBr3 -> Products'
      xsqy_tab(m)%wrf_label = 'j_chbr3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CHBr3.jpl97'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 87
      subr(m)%xsqy_sub   => r09
      m = m + 1

      xsqy_tab(m)%label   = 'CH3CHO -> CH3 + HCO'
      xsqy_tab(m+1)%label = 'CH3CHO -> CH4 + CO'
      xsqy_tab(m+2)%label = 'CH3CHO -> CH3CO + H'
      xsqy_tab(m)%wrf_label = 'j_ch3cho_a'
      xsqy_tab(m+1)%wrf_label = 'j_ch3cho_b'
      xsqy_tab(m+2)%wrf_label = 'j_ch3cho_c'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1:m+2)%channel = (/ 2,3 /)
      xsqy_tab(m:m+2)%tpflag = (/ 2,0,0 /)
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3CHO/CH3CHO_jpl11.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/CH3CHO/CH3CHO_uip.yld'
      xsqy_tab(m)%filespec%nskip(1:2) = (/ 2,4 /)
      xsqy_tab(m)%filespec%nread(1:2) = (/ 101,12 /)
      subr(m)%xsqy_sub   => r11
      subr(m+1)%xsqy_sub => r11
      subr(m+2)%xsqy_sub => r11
      m = m + 3

      xsqy_tab(m)%label = 'C2H5CHO -> C2H5 + HCO'
      xsqy_tab(m)%wrf_label = 'j_c2h5cho'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 2
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/C2H5CHO/C2H5CHO_iup.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/C2H5CHO/C2H5CHO_iup.yld'
      xsqy_tab(m)%filespec%nskip(1:2) = 4
      xsqy_tab(m)%filespec%nread(1:2) = (/ 106,5 /)
      subr(m)%xsqy_sub   => r12
      m = m + 1

      xsqy_tab(m)%label   = 'CHOCHO -> HCO + HCO'
      xsqy_tab(m+1)%label = 'CHOCHO -> H2 + 2CO'
      xsqy_tab(m+2)%label = 'CHOCHO -> CH2O + CO'
      xsqy_tab(m)%wrf_label = 'j_gly_a'
      xsqy_tab(m+1)%wrf_label = 'j_gly_b'
      xsqy_tab(m+2)%wrf_label = 'j_gly_c'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1:m+2)%channel = (/ 2,3 /)
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CHOCHO/glyoxal_jpl11.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/CHOCHO/glyoxal_jpl11.qy'
      xsqy_tab(m)%filespec%nskip(1:2) = (/ 2,3 /)
      xsqy_tab(m)%filespec%nread(1:2) = (/ 277,40 /)
      subr(m)%xsqy_sub   => r13
      subr(m+1)%xsqy_sub => r13
      subr(m+2)%xsqy_sub => r13
      m = m + 3

      xsqy_tab(m)%label = 'CH3COCHO -> CH3CO + HCO'
      xsqy_tab(m)%wrf_label = 'j_mgly'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3COCHO/CH3COCHO_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 294
      subr(m)%xsqy_sub   => r14
      m = m + 1

      xsqy_tab(m)%label = 'CH3COCH3 -> CH3CO + CH3'
      xsqy_tab(m)%wrf_label = 'j_ch3coch3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 3
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3COCH3/CH3COCH3_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 5
      xsqy_tab(m)%filespec%nread(1) = 135
      subr(m)%xsqy_sub   => r15
      m = m + 1

      xsqy_tab(m)%label = 'CH3OOH -> CH3O + OH'
      xsqy_tab(m)%wrf_label = 'j_ch3ooh'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3OOH/CH3OOH_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 40
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3ONO2 -> CH3O + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch3ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/CH3ONO2_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 65
      subr(m)%xsqy_sub   => r17
      m = m + 1

      xsqy_tab(m)%label   = 'CH3CO(OONO2) -> CH3CO(OO) + NO2'
      xsqy_tab(m+1)%label = 'CH3CO(OONO2) -> CH3CO(O) + NO3'
      xsqy_tab(m)%wrf_label = 'j_pan_a'
      xsqy_tab(m+1)%wrf_label = 'j_pan_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/PAN_talukdar.abs'
      xsqy_tab(m)%filespec%nskip(1) = 14
      xsqy_tab(m)%filespec%nread(1) = 78
      subr(m)%xsqy_sub   => r18
      subr(m+1)%xsqy_sub => r18
      m = m + 2

      xsqy_tab(m)%label = 'CCl2O -> Products'
      xsqy_tab(m)%wrf_label = 'j_ccl2o'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CCl2O_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CCl4 -> Products'
      xsqy_tab(m)%wrf_label = 'j_ccl4'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CCl4_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 5
      xsqy_tab(m)%filespec%nread(1) = 44
      subr(m)%xsqy_sub   => r20
      m = m + 1

      xsqy_tab(m)%label = 'CClFO -> Products'
      xsqy_tab(m)%wrf_label = 'j_cclfo'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CClFO_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF2O -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2o'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CF2O_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 5
      xsqy_tab(m)%filespec%nread(1) = 21
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF2ClCFCl2 (CFC-113) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2clcfcl2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CFC-113_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r23
      m = m + 1

      xsqy_tab(m)%label = 'CF2ClCF2Cl (CFC-114) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2clcf2cl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CFC-114_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r24
      m = m + 1

      xsqy_tab(m)%label = 'CF3CF2Cl (CFC-115) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf3cf2cl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CFC-115_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CCl3F (CFC-11) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ccl3f'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CFC-11_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r26
      m = m + 1

      xsqy_tab(m)%label = 'CCl2F2 (CFC-12) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ccl2f2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CFC-12_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r27
      m = m + 1

      xsqy_tab(m)%label = 'CH3Br -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3br'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3Br_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3CCl3 -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3ccl3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3CCl3_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r29
      m = m + 1

      xsqy_tab(m)%label = 'CH3Cl -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3cl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3Cl_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r30
      m = m + 1

      xsqy_tab(m)%label = 'ClOO -> Products'
      xsqy_tab(m)%wrf_label = 'j_cloo'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/ClOO_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF3CHCl2 (HCFC-123) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf3chcl2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      subr(m)%xsqy_sub   => r32
      m = m + 1

      xsqy_tab(m)%label = 'CF3CHFCl (HCFC-124) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf3chfcl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      subr(m)%xsqy_sub   => r33
      m = m + 1

      xsqy_tab(m)%label = 'CH3CFCl2 (HCFC-141b) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3cfcl2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HCFC-141b_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3CF2Cl (HCFC-142b) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3cf2cl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      subr(m)%xsqy_sub   => r35
      m = m + 1

      xsqy_tab(m)%label = 'CF3CF2CHCl2 (HCFC-225ca) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf3cf2chcl2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HCFC-225ca_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF2ClCF2CHFCl (HCFC-225cb) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2clcf2chfcl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HCFC-225cb_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CHClF2 (HCFC-22) -> Products'
      xsqy_tab(m)%wrf_label = 'j_chclf2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HCFC-22_jpl94.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => r38
      m = m + 1

      xsqy_tab(m)%label = 'HO2 -> OH + O'
      xsqy_tab(m)%wrf_label = 'j_ho2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HO2_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 10
      xsqy_tab(m)%filespec%nread(1) = 15
      subr(m)%xsqy_sub   => r39
      m = m + 1

      xsqy_tab(m)%label = 'CF2Br2 (Halon-1202) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2bf2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Halon-1202_jpl97.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF2BrCl (Halon-1211) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2brcl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Halon-1211_jpl97.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF3Br (Halon-1301) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf3br'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Halon-1301_jpl97.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CF2BrCF2Br (Halon-2402) -> Products'
      xsqy_tab(m)%wrf_label = 'j_cf2brcf2br'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Halon-2402_jpl97.abs'
      xsqy_tab(m)%filespec%nskip(1) = -1
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'N2O -> N2 + O(1D)'
      xsqy_tab(m)%wrf_label = 'j_n2o'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      subr(m)%xsqy_sub   => r44
      m = m + 1

      xsqy_tab(m)%label   = 'ClONO2 -> Cl + NO3'
      xsqy_tab(m+1)%label = 'ClONO2 -> ClO + NO2'
      xsqy_tab(m)%wrf_label = 'j_clono2_a'
      xsqy_tab(m+1)%wrf_label = 'j_clono2_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/ClONO2_jpl97.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 119
      subr(m)%xsqy_sub   => r45
      subr(m+1)%xsqy_sub => r45
      m = m + 2

      xsqy_tab(m)%label   = 'BrONO2 -> BrO + NO2'
      xsqy_tab(m+1)%label = 'BrONO2 -> Br + NO3'
      xsqy_tab(m)%wrf_label = 'j_brono2_a'
      xsqy_tab(m+1)%wrf_label = 'j_brono2_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/BrONO2_jpl03.abs'
      xsqy_tab(m)%filespec%nskip(1) = 13
      xsqy_tab(m)%filespec%nread(1) = 61
      subr(m)%xsqy_sub   => r46
      subr(m+1)%xsqy_sub => r46
      m = m + 2

      xsqy_tab(m)%label = 'Cl2 -> Cl + Cl'
      xsqy_tab(m)%wrf_label = 'j_cl2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      subr(m)%xsqy_sub   => r47
      m = m + 1

      xsqy_tab(m)%label   = 'HOCH2CHO -> CH2OH + HCO'
      xsqy_tab(m+1)%label = 'HOCH2CHO -> CH3OH + CO'
      xsqy_tab(m+2)%label = 'HOCH2CHO -> CH2CHO + OH'
      xsqy_tab(m)%wrf_label = 'j_glyald_a'
      xsqy_tab(m+1)%wrf_label = 'j_glyald_b'
      xsqy_tab(m+2)%wrf_label = 'j_glyald_c'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1:m+2)%channel = (/ 2,3 /)
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH2OHCHO/glycolaldehyde_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 63
      subr(m)%xsqy_sub   => r101
      subr(m+1)%xsqy_sub => r101
      subr(m+2)%xsqy_sub => r101
      m = m + 3

      xsqy_tab(m)%label = 'CH3COCOCH3 -> Products'
      xsqy_tab(m)%wrf_label = 'j_biacetyl'
      xsqy_tab(m)%qyld  = .158
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3COCOCH3/biacetyl_horowitz.abs'
      xsqy_tab(m)%filespec%nskip(1) = 8
      xsqy_tab(m)%filespec%nread(1) = 287
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3COCH=CH2 -> Products'
      xsqy_tab(m)%wrf_label = 'j_mvk'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/MVK_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 146
      subr(m)%xsqy_sub   => r103
      m = m + 1

      xsqy_tab(m)%label = 'CH2=C(CH3)CHO -> Products'
      xsqy_tab(m)%wrf_label = 'j_macr'
      xsqy_tab(m)%qyld  = .01
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Methacrolein_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 7
      xsqy_tab(m)%filespec%nread(1) = 146
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3COCO(OH) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3cocooh'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH3COCOOH/pyruvic_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 139
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3CH2ONO2 -> CH3CH2O + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch3ch2ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/RONO2_talukdar.abs'
      xsqy_tab(m)%filespec%nskip(1) = 10
      xsqy_tab(m)%filespec%nread(1) = 63
      subr(m)%xsqy_sub   => r106
      m = m + 1

      xsqy_tab(m)%label = 'CH3CHONO2CH3 -> CH3CHOCH3 + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch3chono2ch3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/RONO2_talukdar.abs'
      xsqy_tab(m)%filespec%nskip(1) = 10
      xsqy_tab(m)%filespec%nread(1) = 63
      subr(m)%xsqy_sub   => r107
      m = m + 1

      xsqy_tab(m)%label = 'CH2(OH)CH2(ONO2) -> CH2(OH)CH2(O.) + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch2ohch2ono2'
      xsqy_tab(m)%jndx  = m
      subr(m)%xsqy_sub   => r108
      m = m + 1

      xsqy_tab(m)%label = 'CH3COCH2(ONO2) -> CH3COCH2(O.) + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch3coch2ono2'
      xsqy_tab(m)%jndx  = m
      subr(m)%xsqy_sub   => r109
      m = m + 1

      xsqy_tab(m)%label = 'C(CH3)3(ONO2) -> C(CH3)3(O.) + NO2'
      xsqy_tab(m)%wrf_label = 'j_bnit1'
      xsqy_tab(m)%jndx  = m
      subr(m)%xsqy_sub   => r110
      m = m + 1

      xsqy_tab(m)%label = 'ClOOCl -> Cl + ClOO'
      xsqy_tab(m)%wrf_label = 'j_cloocl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/ClOOCl_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 111
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label   = 'CH2(OH)COCH3 -> CH3CO + CH2(OH)'
      xsqy_tab(m+1)%label = 'CH2(OH)COCH3 -> CH2(OH)CO + CH3'
      xsqy_tab(m)%wrf_label = 'j_hyac_a'
      xsqy_tab(m+1)%wrf_label = 'j_hyac_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Hydroxyacetone_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 96
      subr(m)%xsqy_sub   => r112
      subr(m+1)%xsqy_sub => r112
      m = m + 2

      xsqy_tab(m)%label = 'HOBr -> OH + Br'
      xsqy_tab(m)%wrf_label = 'j_hobr'
      xsqy_tab(m)%jndx  = m
      subr(m)%xsqy_sub   => r113
      m = m + 1 

      xsqy_tab(m)%label = 'BrO -> Br + O'
      xsqy_tab(m)%wrf_label = 'j_bro'
      xsqy_tab(m)%jndx  = m
      subr(m)%xsqy_sub   => r114
      m = m + 1 

      xsqy_tab(m)%label = 'Br2 -> Br + Br'
      xsqy_tab(m)%wrf_label = 'j_br2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Br2.abs'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 29
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label   = 'NO3-(aq) -> NO2(aq) + O-'
      xsqy_tab(m+1)%label = 'NO3-(aq) -> NO2-(aq) + O(3P)'
      xsqy_tab(m+2)%label = 'NO3-(aq) with qy=1'
      xsqy_tab(m)%wrf_label = 'j_no3_aq_a'
      xsqy_tab(m+1)%wrf_label = 'j_no3_aq_b'
      xsqy_tab(m+2)%wrf_label = 'j_no3_aq_c'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1:m+2)%channel = (/ 2,3 /)
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/NO3-_CA03.abs'
      xsqy_tab(m)%filespec%nskip(1) = 7
      xsqy_tab(m)%filespec%nread(1) = 43
      subr(m)%xsqy_sub   => r118
      subr(m+1)%xsqy_sub => r118
      subr(m+2)%xsqy_sub => r118
      m = m + 3

      xsqy_tab(m)%label = 'CH3COCH2CH3 -> CH3CO + CH2CH3'
      xsqy_tab(m)%wrf_label = 'j_mek'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Martinez.abs'
      xsqy_tab(m)%filespec%nskip(1) = 4
      xsqy_tab(m)%filespec%nread(1) = 96
      subr(m)%xsqy_sub   => r119
      m = m + 1

      xsqy_tab(m)%label   = 'CH3CH2CO(OONO2) -> CH3CH2CO(OO) + NO2'
      xsqy_tab(m+1)%label = 'CH3CH2CO(OONO2) -> CH3CH2CO(O) + NO3'
      xsqy_tab(m)%wrf_label = 'j_ppn_a'
      xsqy_tab(m+1)%wrf_label = 'j_ppn_b'
      xsqy_tab(m:m+1)%tpflag  = 1
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/PPN_Harwood.txt'
      xsqy_tab(m)%filespec%nskip(1) = 10
      xsqy_tab(m)%filespec%nread(1) = 66
      subr(m)%xsqy_sub   => r120
      subr(m+1)%xsqy_sub => r120
      m = m + 2

      xsqy_tab(m)%label = 'HOCH2OOH -> HOCH2O. + OH'
      xsqy_tab(m)%wrf_label = 'j_hoch2ooh'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HOCH2OOH_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 32
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH2=CHCHO -> Products'
      xsqy_tab(m)%wrf_label = 'j_acrol'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Acrolein.txt'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 55
      subr(m)%xsqy_sub   => r122
      m = m + 1

      xsqy_tab(m)%label = 'CH3CO(OOH) -> Products'
      xsqy_tab(m)%wrf_label = 'j_ch3coooh'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/Peracetic_acid.txt'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 66
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = '(CH3)2NNO -> Products'
      xsqy_tab(m)%wrf_label = 'j_amine'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/dmna.abs'
      xsqy_tab(m)%filespec%nskip(1) = 5
      xsqy_tab(m)%filespec%nread(1) = 132
      xsqy_tab(m)%filespec%xfac(1)  = 1.e-19
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label   = 'ClO -> Cl + O(1D)'
      xsqy_tab(m+1)%label = 'ClO -> Cl + O(3P)'
      xsqy_tab(m)%wrf_label = 'j_clo_a'
      xsqy_tab(m+1)%wrf_label = 'j_clo_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = 1
      subr(m)%xsqy_sub   => r125
      subr(m+1)%xsqy_sub => r125
      m = m + 2

      xsqy_tab(m)%label = 'ClNO2 -> Cl + NO2'
      xsqy_tab(m)%wrf_label = 'j_clno2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/ClNO2.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 26
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'BrNO -> Br + NO'
      xsqy_tab(m)%wrf_label = 'j_brno'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/BrNO.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 27
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'BrNO2 -> Br + NO2'
      xsqy_tab(m)%wrf_label = 'j_brno2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/BrNO2.abs'
      xsqy_tab(m)%filespec%nskip(1) = 6
      xsqy_tab(m)%filespec%nread(1) = 54
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label   = 'BrONO -> Br + NO2'
      xsqy_tab(m+1)%label = 'BrONO -> BrO + NO'
      xsqy_tab(m)%wrf_label = 'j_brono_a'
      xsqy_tab(m+1)%wrf_label = 'j_brono_b'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/BrONO.abs'
      xsqy_tab(m)%filespec%nskip(1) = 8
      xsqy_tab(m)%filespec%nread(1) = 32
      subr(m)%xsqy_sub   => r129
      subr(m+1)%xsqy_sub => r129
      m = m + 2

      xsqy_tab(m)%label = 'HOCl -> HO + Cl'
      xsqy_tab(m)%wrf_label = 'j_hocl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HOCl.abs'
      xsqy_tab(m)%filespec%nskip(1) = 7
      xsqy_tab(m)%filespec%nread(1) = 111
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'NOCl -> NO + Cl'
      xsqy_tab(m)%wrf_label = 'j_nocl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1:2) = 'DATAJ1/ABS/NOCl.abs'
      xsqy_tab(m)%filespec%nskip(1:2) = (/ 7,88 /)
      xsqy_tab(m)%filespec%nread(1:2) = (/ 80,61 /)
      subr(m)%xsqy_sub   => r131
      m = m + 1

      xsqy_tab(m)%label = 'OClO -> Products'
      xsqy_tab(m)%wrf_label = 'j_oclo'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%nfiles      = 3
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/OClO.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/ABS/OClO.abs'
      xsqy_tab(m)%filespec%filename(3) = 'DATAJ1/ABS/OClO.abs'
      xsqy_tab(m)%filespec%nskip(1:3) = (/ 6,1075,2142 /)
      xsqy_tab(m)%filespec%nread(1:3) = (/ 1068,1067,1068 /)
      subr(m)%xsqy_sub   => r132
      m = m + 1

      xsqy_tab(m)%label = 'BrCl -> Br + Cl'
      xsqy_tab(m)%wrf_label = 'j_brcl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/BrCl.abs'
      xsqy_tab(m)%filespec%nskip(1) = 9
      xsqy_tab(m)%filespec%nread(1) = 81
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3(OONO2) -> CH3(OO) + NO2'
      xsqy_tab(m)%wrf_label = 'j_ch3oono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3OONO2.abs'
      xsqy_tab(m)%filespec%nskip(1) = 9
      xsqy_tab(m)%filespec%nread(1) = 26
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'C(CH3)3(ONO) -> C(CH3)3(O) + NO'
      xsqy_tab(m)%wrf_label = 'j_bnit2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/t-butyl-nitrite.abs'
      xsqy_tab(m)%filespec%nskip(1) = 4
      xsqy_tab(m)%filespec%nread(1) = 96
      xsqy_tab(m)%filespec%xfac(1)  = 1.
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'ClONO -> Cl + NO2'
      xsqy_tab(m)%wrf_label = 'j_clono'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/ClONO_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 34
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'HCl -> H + Cl'
      xsqy_tab(m)%wrf_label = 'j_hcl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/HCl_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 31
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label   = 'CH2O -> H + HCO' 
      xsqy_tab(m+1)%label = 'CH2O -> H2 + CO'
      xsqy_tab(m)%wrf_label = 'j_ch2o_r'
      xsqy_tab(m+1)%wrf_label = 'j_ch2o_m'
      xsqy_tab(m)%jndx = m
      xsqy_tab(m+1)%channel = 2
      xsqy_tab(m:m+1)%tpflag = (/ 1,3 /)
      xsqy_tab(m)%filespec%nfiles = 2
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/CH2O/CH2O_jpl11.abs'
      xsqy_tab(m)%filespec%filename(2) = 'DATAJ1/CH2O/CH2O_jpl11.yld'
      xsqy_tab(m)%filespec%nskip(1:2) = 4
      xsqy_tab(m)%filespec%nread(1:2) = (/ 150,112 /)
      subr(m)%xsqy_sub    => pxCH2O
      subr(m+1)%xsqy_sub  => pxCH2O
      m = m + 2

      xsqy_tab(m)%label = 'CH3COOH -> CH3 + COOH'
      xsqy_tab(m)%wrf_label = 'j_ch3cooh'
      xsqy_tab(m)%qyld  = .55
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3COOH_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 18
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CH3OCl -> CH3O + Cl'
      xsqy_tab(m)%wrf_label = 'j_ch3ocl'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CH3OCl_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 83
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'CHCl3 -> Products'
      xsqy_tab(m)%wrf_label = 'j_chcl3'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/CHCl3_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 39
      subr(m)%xsqy_sub   => r140
      m = m + 1

      xsqy_tab(m)%label = 'C2H5ONO2 -> C2H5O + NO2'
      xsqy_tab(m)%wrf_label = 'j_c2h5ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%tpflag = 1
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/C2H5ONO2_iup2006.abs'
      xsqy_tab(m)%filespec%nskip(1) = 4
      xsqy_tab(m)%filespec%nread(1) = 32
      subr(m)%xsqy_sub   => r141
      m = m + 1

      xsqy_tab(m)%label = 'n-C3H7ONO2 -> C3H7O + NO2'
      xsqy_tab(m)%wrf_label = 'j_nc3h7ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/nC3H7ONO2_iup2006.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 32
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = '1-C4H9ONO2 -> 1-C4H9O + NO2'
      xsqy_tab(m)%wrf_label = 'j_1c4h9ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/1C4H9ONO2_iup2006.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 32
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = '2-C4H9ONO2 -> 2-C4H9O + NO2'
      xsqy_tab(m)%wrf_label = 'j_2c4h9ono2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/RONO2/2C4H9ONO2_iup2006.abs'
      xsqy_tab(m)%filespec%nskip(1) = 3
      xsqy_tab(m)%filespec%nread(1) = 15
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'perfluoro 1-iodopropane -> products'
      xsqy_tab(m)%wrf_label = 'j_perfluoro'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/PF-n-iodopropane.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 16
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'I2 -> I + I'
      xsqy_tab(m)%wrf_label = 'j_i2'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/YLD/I2.qy'
      xsqy_tab(m)%filespec%nskip(1) = 4
      xsqy_tab(m)%filespec%nread(1) = 12
      subr(m)%xsqy_sub   => r146
      m = m + 1

      xsqy_tab(m)%label = 'IO -> I + O'
      xsqy_tab(m)%wrf_label = 'j_io'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/IO_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 133
      subr(m)%xsqy_sub   => no_z_dep
      m = m + 1

      xsqy_tab(m)%label = 'IOH -> I + OH'
      xsqy_tab(m)%wrf_label = 'j_ioh'
      xsqy_tab(m)%jndx  = m
      xsqy_tab(m)%filespec%filename(1) = 'DATAJ1/ABS/IOH_jpl11.abs'
      xsqy_tab(m)%filespec%nskip(1) = 2
      xsqy_tab(m)%filespec%nread(1) = 101
      subr(m)%xsqy_sub   => no_z_dep

      end subroutine setup_sub_calls

!-----------------------------------------------------------------------------*
!=  *** ALL the following subroutines have the following arguments
!=  *** except for the routines:
!=      rxn_init, base_read, readit, add_pnts_inter2
!=                                                                           =*
!=  PARAMETERS:                                                              =*
!=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
!=           wavelength grid                                                 =*
!=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
!=           working wavelength grid                                         =*
!=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
!=           working wavelength grid                                         =*
!=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
!=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
!=  AIRDEN - REAL, air density (molec/cc) at each altitude level          (I)=*
!=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
!=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
!=           photolysis reaction defined, at each defined wavelength and     =*
!=           at each defined altitude level                                  =*
!=  JLABEL - CHARACTER(len=50) ::, string identifier for each photolysis  (O)=*
!=           reaction defined                                                =*
!-----------------------------------------------------------------------------*

      SUBROUTINE r01(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product of (cross section) x (quantum yield) for the two     =*
!=  O3 photolysis reactions:                                                 =*
!=             (a) O3 + hv -> O2 + O(1D)                                     =*
!=             (b) O3 + hv -> O2 + O(3P)                                     =*
!=  Cross section:  Combined data from WMO 85 Ozone Assessment (use 273K     =*
!=                  value from 175.439-847.5 nm) and data from Molina and    =*
!=                  Molina (use in Hartley and Huggins bans (240.5-350 nm)   =*
!=  Quantum yield:  Choice between                                           =*
!=                   (1) data from Michelsen et al, 1994                     =*
!=                   (2) JPL 87 recommendation                               =*
!=                   (3) JPL 90/92 recommendation (no "tail")                =*
!=                   (4) data from Shetter et al., 1996                      =*
!=                   (5) JPL 97 recommendation                               =*
!=                   (6) JPL 00 recommendation                               =*
!-----------------------------------------------------------------------------*

      use module_xsections, only : o3xs

! input

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      INTEGER, intent(inout) :: j

! local

      INTEGER :: iw
      REAL    :: xs(nz,nw-1)
      REAL    :: qy1d(nz)

      if( .not. initialize ) then
        call check_alloc( j, nz, nw-1 )

! call cross section read/interpolate routine
! cross sections from WMO 1985 Ozone Assessment
! from 175.439 to 847.500 nm. Using value at 273 K.
! Values are over-written in Hartly and Huggins bands, using different
! options depending on value of mopt:

!     mabs = 1 = mostly Reims grp (Malicet, Brion)
!     mabs = 2 = JPL 2006

        CALL o3xs(nz,tlev,nw,wl, xs)

!****** quantum yield:
! choose quantum yield recommendation:
!    kjpl87:  JPL recommendation 1987                - JPL 87, 90, 92 do not "tail"
!    kjpl92:  JPL recommendations 1990/92 (identical) - still with no "tail"
!    kjpl97:  JPL recommendation 1997, includes tail, similar to Shetter et al.
!    kmich :  Michelsen et al., 1994
!    kshet :  Shetter et al., 1996
!    kjpl00:  JPL 2000
!    kmats:  Matsumi et al., 2002

! compute cross sections and yields at different wavelengths, altitudes:
        DO iw = 1, nw-1
! quantum yields, Matsumi et al.
          CALL fo3qy2(nz,wc(iw),tlev,qy1d)
          if( xsqy_tab(j)%channel == 2 ) then
            qy1d(1:nz) = (1. - qy1d(1:nz))
          endif
          xsqy_tab(j)%sq(1:nz,iw) = qy1d(1:nz)*xs(1:nz,iw)
        END DO
      endif

      END SUBROUTINE r01

!=============================================================================*

      SUBROUTINE r02(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for NO2            =*
!=  photolysis:                                                              =*
!=         NO2 + hv -> NO + O(3P)                                            =*
!=  Cross section from JPL94 (can also have Davidson et al.)                 =*
!=  Quantum yield from Gardiner, Sperry, and Calvert                         =*
!-----------------------------------------------------------------------------*

      use module_xsections, only : no2xs_jpl06a

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)


! data arrays
      INTEGER, parameter :: kdata = 200

      REAL x1(kdata)
      REAL y1(kdata), y2(kdata)

! local
      REAL, save :: yg1(kw), ydel(kw)
      REAL :: yg2(kw)
      REAL :: qy(nz)
      REAL :: t(nz)
      REAL :: no2xs(nz,nw-1)
      INTEGER :: i, iw, n, idum, ierr
      CHARACTER(len=256) :: msg

!*************** NO2 photodissociation

      if( initialize ) then
        CALL readit
        ydel(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

! options for NO2 cross section:
! 1 = Davidson et al. (1988), indepedent of T
! 2 = JPL 1994 (same as JPL 1997, JPL 2002)
! 3 = Harder et al.
! 4 = JPL 2006, interpolating between midpoints of bins
! 5 = JPL 2006, bin-to-bin interpolation

!     mabs = 4

        CALL no2xs_jpl06a(nz,tlev,nw,wl, no2xs)

! quantum yields
!     myld = 1   NO2_calvert.yld  (same as JPL2002)
!     myld = 2   NO2_jpl11.yld (same as jpl2006)

!     myld = 2

! from jpl 2011         

        t(1:nz) = .02*(tlev(1:nz) - 298.)
        DO iw = 1, nw - 1
          qy(1:nz) = yg1(iw) + ydel(iw)*t(1:nz)
          xsqy_tab(j)%sq(1:nz,iw) = no2xs(1:nz,iw)*max( qy(1:nz),0. )
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real    :: xsav(kdata)

      n = 25 ; nsav = 25
      CALL base_read( filespec='DATAJ1/YLD/NO2_jpl11.yld', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1,y1=y2 )
      xsav(1:n) = x1(1:n)
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/y1(1),0./))
      n = nsav
      x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/y2(1),0./))

      END SUBROUTINE readit

      END SUBROUTINE r02

!=============================================================================*

      SUBROUTINE r03(nw,wl,wc,nz,tlev,airden,j)

!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (absorptioon cross section) x (quantum yield) for    =*
!=  both channels of NO3 photolysis:                                         =*
!=          (a) NO3 + hv -> NO2 + O(3P)                                      =*
!=          (b) NO3 + hv -> NO + O2                                          =*
!=  Cross section combined from Graham and Johnston (<600 nm) and JPL 94     =*
!=  Quantum yield from Madronich (1988)                                      =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      INTEGER, PARAMETER :: kdata=350

      REAL x(kdata), x1(kdata)
      REAL y1(kdata)
      real q1_298(kdata), q1_230(kdata), q1_190(kdata)
      real q2_298(kdata), q2_230(kdata), q2_190(kdata)
      real :: sq_wrk(nz)

! local
      real, parameter :: tfac1 = 1./(230. - 190.)
      real, parameter :: tfac2 = 1./(298. - 230.)

      REAL :: qy, qy1, qy2, xsect
      REAL, save :: yg1(kw)
      real, save :: yg_298(kw,2), yg_230(kw,2), yg_190(kw,2)
      real, save :: delabs(kw,2,2)
      real :: t(nz)

      INTEGER i, iw, iz, n, idum, chnl
      INTEGER ierr
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
! yields from JPL2011:
          CALL readit
          delabs(1:nw-1,1,1) = yg_230(1:nw-1,1) - yg_190(1:nw-1,1)
          delabs(1:nw-1,2,1) = yg_298(1:nw-1,1) - yg_230(1:nw-1,1)
          delabs(1:nw-1,1,2) = yg_230(1:nw-1,2) - yg_190(1:nw-1,2)
          delabs(1:nw-1,2,2) = yg_298(1:nw-1,2) - yg_230(1:nw-1,2)
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )

! mabs = 3:  JPL11
!     mabs = 3
! myld = 2  from JPL-2011
!     myld = 2

! compute T-dependent quantum yields
        chnl = xsqy_tab(j)%channel
        DO iw = 1, nw-1
          xsect = yg1(iw)
          where(tlev(1:nz) <= 190. )
            sq_wrk(1:nz) = yg_190(iw,chnl)*xsect
          elsewhere(tlev(1:nz) > 190. .and. tlev(1:nz) <= 230. )
            t(1:nz) = tfac1*(tlev(1:nz) - 190.)
            sq_wrk(1:nz) = yg_190(iw,chnl) + delabs(iw,1,chnl)*t(1:nz)
          elsewhere(tlev(1:nz) > 230. .and. tlev(1:nz) <= 298. )
            t(1:nz) = tfac2*(tlev(1:nz) - 230.)
            sq_wrk(1:nz) = yg_230(iw,chnl) + delabs(iw,2,chnl)*t(1:nz)
          elsewhere(tlev(1:nz) > 298. )
            sq_wrk(1:nz) = yg_298(iw,chnl)
          endwhere
          xsqy_tab(j)%sq(1:nz,iw) = sq_wrk(1:nz)*xsect
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real    :: xsav(kdata)

      n = 289
      CALL base_read( filespec='DATAJ1/ABS/NO3_jpl11.abs', &
                      skip_cnt=6,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n)*1.E-20
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = 56 ; nsav = 56
      CALL base_read( filespec='DATAJ1/YLD/NO3_jpl2011.qy', &
                      skip_cnt=5,rd_cnt=n,x=x,y=q1_298, &
                      y1=q1_230,y2=q1_190,y3=q2_298, &
                      y4=q2_230,y5=q2_190 )
      xsav(1:n) = x(1:n)
      q1_298(1:n) = q1_298(1:n)*.001
      q1_230(1:n) = q1_230(1:n)*.001
      q1_190(1:n) = q1_190(1:n)*.001
      q2_298(1:n) = q2_298(1:n)*.001
      q2_230(1:n) = q2_230(1:n)*.001
      q2_190(1:n) = q2_190(1:n)*.001

      CALL add_pnts_inter2(x,q1_298,yg_298,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x,q1_230,yg_230,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x,q1_190,yg_190,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
     
      n = nsav ; x(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x,q2_298,yg_298(1,2),kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/1.,0./))
      n = nsav ; x(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x,q2_230,yg_230(1,2),kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/1.,0./))
      n = nsav ; x(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x,q2_190,yg_190(1,2),kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/1.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r03

!=============================================================================*

      SUBROUTINE r04(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product of (cross section) x (quantum yiels) for N2O5 photolysis =*
!=  reactions:                                                               =*
!=       (a) N2O5 + hv -> NO3 + NO + O(3P)                                   =*
!=       (b) N2O5 + hv -> NO3 + NO2                                          =*
!=  Cross section from JPL2011: use tabulated values for 300K, correct for   =*
!=  temperature.
!=  Quantum yield: Analysis of data in JPL94 (->DATAJ1/YLD/N2O5.qy)          =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      INTEGER, PARAMETER :: kdata = 200

      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), A(kdata), B(kdata)
      INTEGER :: n, n1, n2

! local
      INTEGER :: iw
      REAL    :: xs
      REAL, save :: yg1(kw), yg2(kw)
      REAL    :: dum(nz)
      REAL    :: t(nz)
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )
        if( xsqy_tab(j)%channel == 1 ) then
          DO iw = 1,nw-1
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDDO
        elseif( xsqy_tab(j)%channel == 2 ) then
! temperature dependence only valid for 233 - 295 K.  Extend to 300.
          t(1:nz) = MAX(233.,MIN(tlev(1:nz),300.))

          DO iw = 1, nw - 1
! Apply temperature correction to 300K values. Do not use A-coefficients 
! because they are inconsistent with the values at 300K.
! quantum yield = 1 for NO2 + NO3, zero for other channels
            dum(1:nz) = 1000.*yg2(iw)*(300. - t(1:nz))/(300.*t(1:nz))
            xsqy_tab(j)%sq(1:nz,iw) = yg1(iw) * 10.**(dum(1:nz))
          ENDDO
        endif
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from jpl2011, at 300 K

      n1 = 103
      CALL base_read( filespec='DATAJ1/ABS/N2O5_jpl11.abs', &
                      skip_cnt=4,rd_cnt=n1,x=x1,y=y1 )
      y1(1:n1) = y1(1:n1) * 1.E-20
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! read temperature dependence coefficients:
      n2 = 8
      CALL base_read( filespec='DATAJ1/ABS/N2O5_jpl11.abs', &
                      skip_cnt=111,rd_cnt=n2,x=x2,y=A,y1=B )

      CALL add_pnts_inter2(x2,B,yg2,kdata,n2, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r04

!=============================================================================*

      SUBROUTINE r06(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product of (cross section) x (quantum yield) for HNO3 photolysis =*
!=        HNO3 + hv -> OH + NO2                                              =*
!=  Cross section: Burkholder et al., 1993                                   =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

! local
      real :: t(nz)
      REAL, save :: yg1(kw), yg2(kw)
      INTEGER i, iw
      INTEGER ierr

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
! quantum yield = 1
! correct for temperature dependence
        t(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw - 1
          xsqy_tab(j)%sq(1:nz,iw) = yg1(iw) * exp( yg2(iw)*t(1:nz) )
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
! HNO3 cross section parameters from Burkholder et al. 1993

      integer :: nsav
      real    :: xsav(kdata)
      real    :: yends(2)

      n1 =  83 ; nsav = 83
      CALL base_read( filespec='DATAJ1/ABS/HNO3_burk.abs', &
                      skip_cnt=6,rd_cnt=n1,x=y1,y=y2 )

      x1(1:n1) = (/ (184. + real(i)*2.,i=1,n1) /)
      xsav(1:n1) = x1(1:n1)

      y1(1:n1) = y1(1:n1) * 1.e-20
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      y2(1:n1) = y2(1:n1) * 1.e-3
      yends(:) = (/ y2(1),y2(n1) /)
      n1 = nsav ; x1(1:n1) = xsav(1:n1)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,yends)

      END SUBROUTINE readit

      END SUBROUTINE r06

!=============================================================================*

      SUBROUTINE r08(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product of (cross section) x (quantum yield) for H2O2 photolysis =*
!=         H2O2 + hv -> 2 OH                                                 =*
!=  Cross section:  From JPL97, tabulated values @ 298K for <260nm, T-depend.=*
!=                  parameterization for 260-350nm                           =*
!=  Quantum yield:  Assumed to be unity                                      =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=600

      REAL x1(kdata)
      REAL y1(kdata)

! local
      real, parameter :: A0 = 6.4761E+04            
      real, parameter :: A1 = -9.2170972E+02        
      real, parameter :: A2 = 4.535649              
      real, parameter :: A3 = -4.4589016E-03        
      real, parameter :: A4 = -4.035101E-05         
      real, parameter :: A5 = 1.6878206E-07
      real, parameter :: A6 = -2.652014E-10
      real, parameter :: A7 = 1.5534675E-13

      real, parameter :: B0 = 6.8123E+03
      real, parameter :: B1 = -5.1351E+01
      real, parameter :: B2 = 1.1522E-01
      real, parameter :: B3 = -3.0493E-05
      real, parameter :: B4 = -1.0924E-07

      INTEGER i, iw, n, idum
      INTEGER ierr
      REAL lambda
      REAL sumA, sumB
      REAL :: t(nz)
      REAL :: chi(nz)
      REAL, save :: yg(kw)

! cross section from Lin et al. 1978

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
! quantum yield = 1
        t(1:nz) = MIN(MAX(tlev(1:nz),200.),400.)            
        chi(1:nz) = 1./(1. + EXP(-1265./t(1:nz)))
        DO iw = 1, nw - 1
! Parameterization (JPL94)
! Range 260-350 nm; 200-400 K
           IF ((wl(iw) .GE. 260.) .AND. (wl(iw) .LT. 350.)) THEN
             lambda = wc(iw)
             sumA = ((((((A7*lambda + A6)*lambda + A5)*lambda +  &
                          A4)*lambda +A3)*lambda + A2)*lambda +  &
                          A1)*lambda + A0
             sumB = (((B4*lambda + B3)*lambda + B2)*lambda +  &
                       B1)*lambda + B0

             xsqy_tab(j)%sq(1:nz,iw) = &
                 (chi(1:nz) * sumA + (1. - chi(1:nz))*sumB)*1.E-21
           ELSE
             xsqy_tab(j)%sq(1:nz,iw) = yg(iw)
           ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from JPL94 (identical to JPL97)
! tabulated data up to 260 nm

      integer :: n1

      CALL base_read( filespec='DATAJ1/ABS/H2O2_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20
      
      n1 = 494
      CALL base_read( filespec='DATAJ1/ABS/H2O2_Kahan.abs', &
                      skip_cnt=0,rd_cnt=n1,x=x1(n+1:),y=y1(n+1:) )

      n = n + n1
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r08

!=============================================================================*

      SUBROUTINE r09(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product of (cross section) x (quantum yield) for CHBr3 photolysis=*
!=          CHBr3 + hv -> Products                                           =*
!=  Cross section: Choice of data from Atlas (?Talukdar???) or JPL97         =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=200

      INTEGER n1
      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, save :: yg(kw)
      real :: t(nz)

      INTEGER i, iw, n
      INTEGER ierr
      INTEGER iz

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! option:

! kopt = 1:  cross section from Elliot Atlas, 1997
! kopt = 2:  cross section from JPL 1997
!     kopt = 2

! quantum yield = 1

        t(1:nz) = 273. - tlev(1:nz)
        DO iw = 1, nw - 1
          IF (wc(iw) .GT. 290. .AND. wc(iw) .LT. 340. ) then
            where( tlev(1:nz) > 210. .AND. tlev(1:nz) < 300. )
              xsqy_tab(j)%sq(1:nz,iw) = &
                   EXP( (.06183 - .000241*wc(iw))*t(1:nz) &
                             - (2.376 + 0.14757*wc(iw)) )
            elsewhere
              xsqy_tab(j)%sq(1:nz,iw) = yg(iw)
            endwhere
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = yg(iw)
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
! jpl97, with temperature dependence formula,
!w = 290 nm to 340 nm, 
!T = 210K to 300 K
!sigma, cm2 = exp((0.06183-0.000241*w)*(273.-T)-(2.376+0.14757*w))

      n1 = 87
      CALL base_read( filespec='DATAJ1/ABS/CHBr3.jpl97', &
                      skip_cnt=6,rd_cnt=n1,x=x1,y=y1 )

      y1(1:n1) = y1(1:n1) * 1.e-20
      CALL add_pnts_inter2(x1,y1,yg,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/y1(1),0./))

      END SUBROUTINE readit

      END SUBROUTINE r09

!=============================================================================*

      SUBROUTINE r11(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH3CHO photolysis: =*
!=      (a)  CH3CHO + hv -> CH3 + HCO                                        =*
!=      (b)  CH3CHO + hv -> CH4 + CO                                         =*
!=      (c)  CH3CHO + hv -> CH3CO + H                                        =*
!=  Cross section:  Choice between                                           =*
!=                   (1) IUPAC 97 data, from Martinez et al.                 =*
!=                   (2) Calvert and Pitts                                   =*
!=                   (3) Martinez et al., Table 1 scanned from paper         =*
!=                   (4) KFA tabulations                                     =*
!=  Quantum yields: Choice between                                           =*
!=                   (1) IUPAC 97, pressure correction using Horowith and    =*
!=                                 Calvert, 1982                             =*
!=                   (2) NCAR data file, from Moortgat, 1986                 =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=150

      INTEGER i, n
      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

! local
      INTEGER :: m, ierr
      INTEGER :: iw
      INTEGER :: chnl
      REAL    :: qy2, qy3
      REAL    :: sig
      REAL    :: dum
      REAL    :: qy1_n0, qy1_0, x
      REAL, save :: yg(kw), yg1(kw), yg2(kw), yg3(kw)
      REAL :: qy1(nz)
      LOGICAL, save :: is_initialized = .false.

      chnl = xsqy_tab(j)%channel
      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
        if( chnl > 1 ) then
          call check_alloc( ndx=j, nz=nw-1, nw=1 )
          if( chnl == 2 ) then
            xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * yg2(1:nw-1)
          elseif( chnl == 3 ) then
            xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * yg3(1:nw-1)
          endif
        endif
      else
        if( xsqy_tab(j)%channel == 1 ) then
          call check_alloc( j, nz, nw-1 )
!     mabs = 5
!     myld = 1
          DO iw = 1, nw - 1
            sig = yg(iw)
! quantum yields:
! input yields at n0 = 1 atm
            qy1_n0 = yg1(iw)
! Pressure correction for CH3 + CHO channel:
! Assume pressure-dependence only for qy1, not qy2 or qy2.
! Assume total yield 1 at zero pressure
            qy1_0 = 1. - (yg2(iw) + yg3(iw))
            
!  compute coefficient:
!  Stern-Volmer:  1/q = 1/q0 + k N  and N0 = 1 atm,
!  then x = K N0 q0 = qy_0/qy_N0 - 1
            if (qy1_n0 > 0.) then
              x = qy1_0/qy1_n0 - 1.
            else
              x = 0.
            endif

            qy1(1:nz) = qy1_n0 * (1. + x) / (1. + x * airden(1:nz)/2.465E19)
            qy1(1:nz) = MIN( 1.,MAX(0.,qy1(1:nz)) )
            xsqy_tab(j)%sq(1:nz,iw) = sig * qy1(1:nz)
          ENDDO
        endif
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real    :: xsav(kdata)

      n = 101
      CALL base_read( filespec='DATAJ1/CH3CHO/CH3CHO_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.e-20

      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! quantum yields

      n = 12 ; nsav = 12
      CALL base_read( filespec='DATAJ1/CH3CHO/CH3CHO_iup.yld', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=y2,y1=y1 )
      xsav(1:n) = x1(1:n)
    
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav
      x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      yg3(1:nw-1) = 0.

      END SUBROUTINE readit

      END SUBROUTINE r11

!=============================================================================*

      SUBROUTINE r12(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for C2H5CHO        =*
!=  photolysis:                                                              =*
!=         C2H5CHO + hv -> C2H5 + HCO                                        =*
!=                                                                           =*
!=  Cross section:  Choice between                                           =*
!=                   (1) IUPAC 97 data, from Martinez et al.                 =*
!=                   (2) Calvert and Pitts, as tabulated by KFA              =*
!=  Quantum yield:  IUPAC 97 recommendation                                  =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      integer, PARAMETER :: kdata=150

      INTEGER i, n
      INTEGER n1
      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, save :: yg(kw), yg1(kw)
      REAL :: qy1(nz)
      REAL sig
      INTEGER ierr
      INTEGER iw

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! Absorption:
! 1:  IUPAC-97 data, from Martinez et al.
! 2:  Calvert and Pitts, as tabulated by KFA.

! Quantum yield
! 1:  IUPAC-97 data

!     mabs = 1
!     myld = 1

        DO iw = 1, nw - 1
! quantum yields:
! use Stern-Volmer pressure dependence:
          IF (yg1(iw) .LT. pzero) THEN
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ELSE
            qy1(1:nz) = 1./(1. + (1./yg1(iw) - 1.)*airden(1:nz)/2.45e19)
            qy1(1:nz) = MIN(qy1(1:nz),1.)
            xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * qy1(1:nz)
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      character(len=256) :: emsg

      n = 106
      CALL base_read( filespec='DATAJ1/C2H5CHO/C2H5CHO_iup.abs', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.e-20

      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! quantum yields

      n = 5
      CALL base_read( filespec='DATAJ1/C2H5CHO/C2H5CHO_iup.yld', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=y1 )

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,340.,0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
        write(emsg,'(''readit: Error '',i5,'' in inter2 for '',a)') ierr,trim(xsqy_tab(j)%label)
        call wrf_error_fatal( trim(emsg) )
      ENDIF

      END SUBROUTINE readit

      END SUBROUTINE r12

!=============================================================================*

      SUBROUTINE r13(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CHOCHO         =*
!=  photolysis:                                                              =*
!=              CHOCHO + hv -> Products                                      =*
!=                                                                           =*
!=  Cross section: Choice between                                            =*
!=                  (1) Plum et al., as tabulated by IUPAC 97                =*
!=                  (2) Plum et al., as tabulated by KFA.                    =*
!=                  (3) Orlando et al.                                       =*
!=                  (4) Horowitz et al., 2001                                =*
!=  Quantum yield: IUPAC 97 recommendation                                   =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=500

      INTEGER i, n
      REAL x(kdata), x1(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

! local
      REAL, save :: yg(kw), yg1(kw), yg2(kw), yg3(kw)
      INTEGER :: ierr
      LOGICAL, save :: is_initialized = .false.

!     mabs = 5
!     myld = 2

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        if( xsqy_tab(j)%channel == 1 ) then
          xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * yg1(1:nw-1)
        elseif( xsqy_tab(j)%channel == 2 ) then
          xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * yg2(1:nw-1)
        elseif( xsqy_tab(j)%channel == 3 ) then
          xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * yg3(1:nw-1)
        endif
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real :: dum(kdata)
      real :: xsav(kdata)
      real :: yends(2)

      n = 277
      CALL base_read( filespec='DATAJ1/CHOCHO/glyoxal_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.e-20
      yends(:) = 0.
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,yends)

! quantum yields

      n = 40 ; nsav = 40
      CALL base_read( filespec='DATAJ1/CHOCHO/glyoxal_jpl11.qy', &
                      skip_cnt=3,rd_cnt=n,x=x,y=dum,y1=y1,y2=y2,y3=y3 )
      xsav(1:n) = x(1:n)
      yends(1) = y1(1)
      CALL add_pnts_inter2(x,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,yends)
      n = nsav ; x(1:n) = xsav(1:n)
      yends(1) = y2(1)
      CALL add_pnts_inter2(x,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,yends)
      n = nsav ; x(1:n) = xsav(1:n)
      yends(1) = y3(1)
      CALL add_pnts_inter2(x,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,yends)

      END SUBROUTINE readit

      END SUBROUTINE r13

!=============================================================================*

      SUBROUTINE r14(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CH3COCHO       =*
!=  photolysis:                                                              =*
!=           CH3COCHO + hv -> CH3CO + HCO                                    =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=500

      INTEGER i, n
      INTEGER n1, n2
      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, save :: yg(kw)
      REAL qy
      REAL sig
      INTEGER ierr
      INTEGER iw
      REAL phi0, kq

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!     mabs = 8
!     myld = 5

        DO iw = 1, nw - 1
          sig = yg(iw)
! quantum yields:
! zero pressure yield:
! 1.0 for wc < 380 nm
! 0.0 for wc > 440 nm
! linear in between:
          phi0 = 1. - (wc(iw) - 380.)/60.
          phi0 = MIN(MAX(0.,phi0),1.)

! Pressure correction: quenching coefficient, torr-1
! in air, Koch and Moortgat:
          kq = 1.36e8 * EXP(-8793./wc(iw))
! in N2, Chen et al:
          IF(phi0 .GT. 0.) THEN
            IF (wc(iw) .GE. 380. .AND. wc(iw) .LE. 440.) THEN
              xsqy_tab(j)%sq(1:nz,iw) = sig * phi0 &
                  / (phi0 + kq * airden(1:nz) * 760./2.456E19)
            ELSE
              xsqy_tab(j)%sq(1:nz,iw) = sig * phi0
            ENDIF
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      n = 294
      CALL base_read( filespec='DATAJ1/CH3COCHO/CH3COCHO_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.e-20
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
         
      END SUBROUTINE readit

      END SUBROUTINE r14

!=============================================================================*

      SUBROUTINE r15(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH3COCH3 photolysis=*
!=          CH3COCH3 + hv -> Products                                        =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      integer, PARAMETER :: kdata=150

      INTEGER :: i, n
      REAL x1(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata)

! local
      REAL, save :: yg(kw), yg2(kw), yg3(kw)
      REAL :: qy(nz)
      REAL :: sig(nz)
      REAL :: T(nz)
      real :: fac(nz)
      INTEGER ierr
      INTEGER iw

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!     mabs = 4
!     myld = 4

        T(1:nz) = MIN(MAX(tlev(1:nz), 235.),298.)
        DO iw = 1, nw - 1
          sig(1:nz) = yg(iw) * (1. + t(1:nz)*(yg2(iw) + t(1:nz)*yg3(iw)))
          CALL qyacet(nz, wc(iw), tlev, airden, fac)
          xsqy_tab(j)%sq(1:nz,iw) = sig(1:nz)*min(max(0.,fac(1:nz)),1.)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real    :: xsav(kdata)

      n = 135 ; nsav = 135
      CALL base_read( filespec='DATAJ1/CH3COCH3/CH3COCH3_jpl11.abs', &
                      skip_cnt=5,rd_cnt=n,x=x1,y=y1,y1=y2,y2=y3,y3=y4 )
      y1(1:n) = y1(1:n) * 1.e-20
      y2(1:n) = y2(1:n) * 1.e-3
      y3(1:n) = y3(1:n) * 1.e-5
      xsav(1:n) = x1(1:n)

      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
         
      END SUBROUTINE readit

      END SUBROUTINE r15

!=============================================================================*

      SUBROUTINE r17(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH3ONO2            =*
!=  photolysis:                                                              =*
!=          CH3ONO2 + hv -> CH3O + NO2                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      integer, PARAMETER :: kdata = 100

      INTEGER i, n
      INTEGER iw
      INTEGER n1, n2
      REAL :: x1(kdata)
      REAL :: y1(kdata), y2(kdata)

! local
      REAL, save :: yg(kw), yg1(kw)
      REAL :: qy
      REAL :: sig
      REAL :: T(nz)
      INTEGER ierr

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!     mabs = 9
! quantum yield = 1

        T(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw - 1
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * exp( yg1(iw) * T(1:nz) )
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: nsav
      real    :: xsav(kdata)

      n = 65 ; nsav = 65
      CALL base_read( filespec='DATAJ1/RONO2/CH3ONO2_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.e-20
      y2(1:n) = y2(1:n) * 1.e-3
      xsav(1:n) = x1(1:n)
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r17

!=============================================================================*

      SUBROUTINE r18(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for PAN photolysis:    =*
!=       PAN + hv -> Products                                                =*
!=                                                                           =*
!=  Cross section: from Talukdar et al., 1995                                =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER iw
      INTEGER n
      REAL :: x1(kdata)
      REAL :: y1(kdata), y2(kdata)

! local

! quantum yield:
! from JPL 2011 values for >300 nm.
!     real, parameter :: qyNO2 = .7
!     real, parameter :: qyNO3 = .3
      real, parameter :: qyld(2) = (/ .7,.3 /)

      INTEGER :: ierr, chnl
      REAL, save :: yg(kw), yg2(kw)
      REAL :: sig(nz), T(nz)
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )

        chnl = xsqy_tab(j)%channel
        T(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw-1
          sig(1:nz) = yg(iw) * EXP( yg2(iw)*T(1:nz) )
          xsqy_tab(j)%sq(1:nz,iw) = qyld(chnl) * sig(1:nz)
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from 
!      Talukdar et al., 1995, J.Geophys.Res. 100/D7, 14163-14174

      integer :: nsav
      real    :: xsav(kdata)

      n = 78 ; nsav = 78
      CALL base_read( filespec='DATAJ1/RONO2/PAN_talukdar.abs', &
                      skip_cnt=14,rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-3
      xsav(1:n) = x1(1:n)
 
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r18

!=============================================================================*

      SUBROUTINE r20(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CCl4 photolysis:   =*
!=      CCl4 + hv -> Products                                                =*
!=  Cross section: from JPL 97 recommendation                                =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      REAL x1(kdata)
      REAL y1(kdata)

! local
      real, parameter :: b0 = 1.0739
      real, parameter :: b1 = -1.6275e-2
      real, parameter :: b2 = 8.8141e-5
      real, parameter :: b3 = -1.9811e-7
      real, parameter :: b4 = 1.5022e-10

      REAL, save :: yg(kw)
      INTEGER i, iw, n, idum
      INTEGER :: ierr
      REAL :: tcoeff, sig
      REAL :: w1
      REAL :: temp(nz)

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! mabs = 1:  jpl 1997 recommendation
! mabs = 2:  jpl 2011 recommendation, with T dependence

!     mabs = 2

! compute temperature correction factors:

!** quantum yield assumed to be unity

        temp(1:nz) = min(max(tlev(1:nz),210.),300.)
        temp(1:nz) = temp(1:nz) - 295.
        DO iw = 1, nw-1
! compute temperature correction coefficients:
           tcoeff = 0.
           IF(wc(iw) .GT. 194. .AND. wc(iw) .LT. 250.) THEN 
             w1 = wc(iw)
             tcoeff = b0 + w1*(b1 + w1*(b2 + w1*(b3 + w1*b4)))
           ENDIF
           xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * 10.**(tcoeff*temp(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!** cross sections from JPL97 recommendation (identical to 94 data)

      n = 44
      CALL base_read( filespec='DATAJ1/ABS/CCl4_jpl11.abs', &
                      skip_cnt=5,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20
         
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r20

!=============================================================================*

      SUBROUTINE r23(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CFC-113 photolysis:=*
!=          CF2ClCFCl2 + hv -> Products                                      =*
!=  Cross section:  from JPL 97 recommendation, linear interp. between       =*
!=                  values at 210 and 295K                                   =*
!=  Quantum yield:  assumed to be unity                                      =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

! local
      real, parameter :: tfac1 = 1./(295. - 210.)

      REAL, save :: yg2(kw), ydel(kw)
      REAL       :: yg1(kw)
      REAL qy
      REAL :: t(nz)
      REAL :: slope(nz)
      INTEGER i, iw, n, idum
      INTEGER iz
      INTEGER ierr

      if( initialize ) then
        CALL readit
        ydel(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

!** quantum yield assumed to be unity

        t(1:nz) = MAX(210.,MIN(tlev(1:nz),295.))
        slope(1:nz) = (t(1:nz) - 210.)*tfac1
        DO iw = 1, nw-1
          xsqy_tab(j)%sq(1:nz,iw) = yg2(iw) + slope(1:nz)*ydel(iw)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!** cross sections from JPL97 recommendation (identical to 94 recommendation)

      integer :: nsav
      real    :: xsav(kdata)

      CALL base_read( filespec='DATAJ1/ABS/CFC-113_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-20
      xsav(1:n) = x1(1:n)
      nsav = n
      
!* sigma @ 295 K
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! sigma @ 210 K
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r23

!=============================================================================*

      SUBROUTINE r24(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CFC-144 photolysis:=*
!=              CF2ClCF2Cl + hv -> Products                                  =*
!=  Cross section: from JPL 97 recommendation, linear interp. between values =*
!=                 at 210 and 295K                                           =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

! local
      real, parameter :: tfac1 = 1./(295. - 210.)

      REAL, save :: yg2(kw), ydel(kw)
      REAL       :: yg1(kw)
      REAL qy
      REAL :: t(nz)
      REAL :: slope(nz)
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

      if( initialize ) then
        CALL readit
        ydel(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

!** quantum yield assumed to be unity

        t(1:nz) = MAX(210.,MIN(tlev(1:nz),295.))
        slope(1:nz) = (t(1:nz) - 210.)*tfac1
        DO iw = 1, nw-1
          xsqy_tab(j)%sq(1:nz,iw) = yg2(iw) + slope(1:nz)*ydel(iw)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      integer :: nsav
      real    :: xsav(kdata)

      CALL base_read( filespec='DATAJ1/ABS/CFC-114_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-20
      xsav(1:n) = x1(1:n)
      nsav = n

!* sigma @ 295 K
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
! sigma @ 210 K
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r24

!=============================================================================*

      SUBROUTINE r26(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CFC-11  photolysis =*
!=          CCl3F + hv -> Products                                           =*
!=  Cross section: from JPL 97 recommendation                                =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      integer, PARAMETER :: kdata=100

      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, save :: yg(kw)
      REAL :: t(nz)
      INTEGER :: iw, n

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        t(1:nz) = 1.E-04 * (tlev(1:nz) - 298.)
        DO iw = 1, nw-1
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * EXP((wc(iw)-184.9) * t(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      CALL base_read( filespec='DATAJ1/ABS/CFC-11_jpl94.abs',rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20

!* sigma @ 298 K

      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r26

!=============================================================================*

      SUBROUTINE r27(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CFC-12  photolysis:=*
!=         CCl2F2 + hv -> Products                                           =*
!=  Cross section: from JPL 97 recommendation                                =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, save :: yg(kw)
      REAL    :: t(nz)
      INTEGER :: iw, n

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
!*** quantum yield assumed to be unity
        t(1:nz) = 1.E-04 * (tlev(1:nz) - 298.) 
        DO iw = 1, nw-1
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * EXP((wc(iw)-184.9) * t(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      CALL base_read( filespec='DATAJ1/ABS/CFC-12_jpl94.abs',rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20

!* sigma @ 298 K
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r27

!=============================================================================*

      SUBROUTINE r29(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH3CCl3 photolysis =*
!=           CH3CCl3 + hv -> Products                                        =*
!=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
!=                 of data at 210, 250, and 295K                             =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

! local
      real, parameter :: tfac1 = 1./(250. - 210.)
      real, parameter :: tfac2 = 1./(295. - 250.)

      REAL, save :: yg2(kw), yg3(kw), ydel1(kw), ydel2(kw)
      REAL       :: yg1(kw)
      REAL qy
      REAL :: t(nz)
      REAL :: slope(nz)
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

      if( initialize ) then
        CALL readit
        ydel2(1:nw-1) = yg2(1:nw-1) - yg3(1:nw-1)
        ydel1(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        t(1:nz) = MIN(295.,MAX(tlev(1:nz),210.))
        DO iw = 1, nw-1
          where( t(1:nz) <= 250. )
            slope(1:nz) = (t(1:nz) - 210.)*tfac1
            xsqy_tab(j)%sq(1:nz,iw) = yg3(iw) + slope(1:nz)*ydel2(iw)
          elsewhere
            slope(1:nz) = (t(1:nz) - 250.)*tfac2
            xsqy_tab(j)%sq(1:nz,iw) = yg2(iw) + slope(1:nz)*ydel1(iw)
          endwhere
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      integer :: nsav
      real    :: xsav(kdata)

      CALL base_read( filespec='DATAJ1/ABS/CH3CCl3_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1,y1=y2,y2=y3 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-20
      y3(1:n) = y3(1:n) * 1.E-20
      xsav(1:n) = x1(1:n)
      nsav = n

!* sigma @ 295 K
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 250 K
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 210 K
      CALL add_pnts_inter2(x1,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r29

!=============================================================================*

      SUBROUTINE r30(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH3Cl photolysis:  =*
!=            CH3Cl + hv -> Products                                         =*
!=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
!=                 from values at 255, 279, and 296K                         =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

! local
      real, parameter :: tfac1 = 1./(279. - 255.)
      real, parameter :: tfac2 = 1./(296. - 279.)

      REAL, save :: yg2(kw), yg3(kw)
      REAL, save :: ydel1(kw), ydel2(kw)
      REAL       :: yg1(kw)
      REAL qy
      REAL :: t(nz)
      REAL :: slope(nz)
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

      if( initialize ) then
        CALL readit
        ydel2(1:nw-1) = yg2(1:nw-1) - yg3(1:nw-1)
        ydel1(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        t(1:nz) = MAX(255.,MIN(tlev(1:nz),296.))
        DO iw = 1, nw-1
          where( t(1:nz) <= 279. )
            slope(1:nz) = (t(1:nz) - 255.)*tfac1
            xsqy_tab(j)%sq(1:nz,iw) = yg3(iw) + slope(1:nz)*ydel2(iw)
          elsewhere
            slope(1:nz) = (t(1:nz) - 279.)*tfac2
            xsqy_tab(j)%sq(1:nz,iw) = yg2(iw) + slope(1:nz)*ydel1(iw)
          endwhere
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      integer :: nsav
      real    :: xsav(kdata)

      CALL base_read( filespec='DATAJ1/ABS/CH3Cl_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1,y1=y2,y2=y3 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-20
      y3(1:n) = y3(1:n) * 1.E-20
      xsav(1:n) = x1(1:n)
      nsav = n

!* sigma @ 296 K
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 279 K
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 255 K
      CALL add_pnts_inter2(x1,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r30

!=============================================================================*

      SUBROUTINE r32(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for HCFC-123 photolysis=*
!=       CF3CHCl2 + hv -> Products                                           =*
!=  Cross section: from Orlando et al., 1991                                 =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      real, parameter :: LBar = 206.214

      INTEGER i, iw, idum
      INTEGER iz, k
      REAL qy
      REAL lambda
      REAL, save :: TBar
      REAL :: t(nz)
      REAL :: sum(nz)
      REAL, save :: coeff(4,3)
      CHARACTER*120 inline

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        DO iw = 1, nw-1
          lambda = wc(iw)
! use parameterization only up to 220 nm, as the error bars associated with
! the measurements beyond 220 nm are very large (Orlando, priv.comm.)
          IF (lambda .GE. 190. .AND. lambda .LE. 220.) THEN
            t(1:nz) = MIN(295.,MAX(tlev(1:nz),203.)) - TBar
            sum(1:nz) = 0.
            DO i = 1, 4
              sum(1:nz) = (coeff(i,1) + t(1:nz)*(coeff(i,2) + t(1:nz)*coeff(i,3))) &
                          * (lambda-LBar)**(i-1) + sum(1:nz)
            ENDDO 
            xsqy_tab(j)%sq(1:nz,iw) = EXP(sum(1:nz))
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(a120)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      END SUBROUTINE readit

      END SUBROUTINE r32

!=============================================================================*

      SUBROUTINE r33(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for HCFC-124 photolysis=*
!=        CF3CHFCl + hv -> Products                                          =*
!=  Cross section: from Orlando et al., 1991                                 =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      real, parameter :: LBar = 206.214

      INTEGER i, iw, n, idum
      INTEGER iz, k
      REAL qy
      REAL lambda
      REAL, save :: TBar
      REAL :: t(nz)
      REAL :: sum(nz)
      REAL, save :: coeff(4,3)
      CHARACTER*120 inline

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        DO iw = 1, nw-1
          lambda = wc(iw)
          IF (lambda .GE. 190. .AND. lambda .LE. 230.) THEN
            t(1:nz) = MIN(295.,MAX(tlev(1:nz),203.)) - TBar
            sum(1:nz) = 0.
            DO i = 1, 4
              sum(1:nz) = (coeff(i,1) + t(1:nz)*(coeff(i,2) + t(1:nz)*coeff(i,3))) &
                          * (lambda-LBar)**(i-1) + sum(1:nz)
            ENDDO 
            xsqy_tab(j)%sq(1:nz,iw) = EXP(sum(1:nz))
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      idum = idum+5
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(a120)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      END SUBROUTINE readit

      END SUBROUTINE r33

!=============================================================================*

      SUBROUTINE r35(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for HCFC-142b          =*
!=  photolysis:                                                              =*
!=          CH3CF2Cl + hv -> Products                                        =*
!=  Cross section: from Orlando et al., 1991                                 =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      real, parameter :: LBar = 206.214

      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz, k
      REAL qy
      REAL lambda
      REAL, save :: Tbar
      REAL :: t(nz)
      REAL :: sum(nz)
      REAL, save :: coeff(4,3)
      CHARACTER*80 inline

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        DO iw = 1, nw-1
          lambda = wc(iw)
          IF (lambda .GE. 190. .AND. lambda .LE. 230.) THEN
            t(1:nz) = MIN(295.,MAX(tlev(1:nz),203.)) - TBar
            sum(1:nz) = 0.
            DO i = 1, 4
              sum(1:nz) = (coeff(i,1) + t(1:nz)*(coeff(i,2) + t(1:nz)*coeff(i,3))) &
                          * (lambda-LBar)**(i-1) + sum(1:nz)
            ENDDO 
! offeset exponent by 40 (exp(-40.) = 4.248e-18) to prevent exp. underflow errors
! on some machines.
            xsqy_tab(j)%sq(1:nz,iw) = 4.248e-18 * EXP(sum(1:nz) + 40.)
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDIF
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      idum = idum+10
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(a80)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      END SUBROUTINE readit

      END SUBROUTINE r35

!=============================================================================*

      SUBROUTINE r38(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for HCFC-22 photolysis =*
!=          CHClF2 + hv -> Products                                          =*
!=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
!=                 from values at 210, 230, 250, 279, and 295 K              =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata), x5(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata), y5(kdata)

! local
      real, parameter :: tfac1 = 1./(230. - 210.)
      real, parameter :: tfac2 = 1./(250. - 230.)
      real, parameter :: tfac3 = 1./(270. - 250.)
      real, parameter :: tfac4 = 1./(295. - 270.)

      REAL qy
      REAL, save :: yg2(kw), yg3(kw), yg4(kw), yg5(kw)
      REAL       :: yg1(kw)
      REAL, save :: ydel1(kw), ydel2(kw), ydel3(kw), ydel4(kw)
      REAL :: t(nz), t1(nz), t2(nz), t3(nz), t4(nz)
      REAL :: slope(nz)
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

      if( initialize ) then
        CALL readit
        ydel4(1:nw-1) = yg4(1:nw-1) - yg5(1:nw-1)
        ydel3(1:nw-1) = yg3(1:nw-1) - yg4(1:nw-1)
        ydel2(1:nw-1) = yg2(1:nw-1) - yg3(1:nw-1)
        ydel1(1:nw-1) = yg1(1:nw-1) - yg2(1:nw-1)
      else
        call check_alloc( j, nz, nw-1 )

!*** quantum yield assumed to be unity

        t(1:nz) = MIN(295.,MAX(tlev(1:nz),210.))
        t1(1:nz) = (t(1:nz) - 210.)*tfac1
        t2(1:nz) = (t(1:nz) - 230.)*tfac2
        t3(1:nz) = (t(1:nz) - 250.)*tfac3
        t4(1:nz) = (t(1:nz) - 270.)*tfac4
        DO iw = 1, nw-1
          where( t(1:nz) <= 230. )
            xsqy_tab(j)%sq(1:nz,iw) = yg5(iw) + t1(1:nz)*ydel4(iw)
          elsewhere( t(1:nz) > 230. .and. t(1:nz) <= 250. )
            xsqy_tab(j)%sq(1:nz,iw) = yg4(iw) + t2(1:nz)*ydel3(iw)
          elsewhere( t(1:nz) > 250. .and. t(1:nz) <= 270. )
            xsqy_tab(j)%sq(1:nz,iw) = yg3(iw) + t3(1:nz)*ydel2(iw)
          elsewhere
            xsqy_tab(j)%sq(1:nz,iw) = yg2(iw) + t4(1:nz)*ydel1(iw)
          endwhere
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      integer :: nsav
      real    :: xsav(kdata)

      CALL base_read( filespec='DATAJ1/ABS/HCFC-22_jpl94.abs', &
                      rd_cnt=n,x=x1,y=y1,y1=y2,y2=y3,y3=y4,y4=y5 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1.E-20
      y3(1:n) = y3(1:n) * 1.E-20
      y4(1:n) = y4(1:n) * 1.E-20
      y5(1:n) = y5(1:n) * 1.E-20
      nsav = n ; xsav(1:n) = x1(1:n)

!* sigma @ 295 K
      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 270 K
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 250 K
      CALL add_pnts_inter2(x1,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 230 K
      CALL add_pnts_inter2(x1,y4,yg4,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
!* sigma @ 210 K
      CALL add_pnts_inter2(x1,y5,yg5,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r38

!=============================================================================*

      SUBROUTINE r39(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for HO2 photolysis:    =*
!=          HO2 + hv -> OH + O                                               =*
!=  Cross section: from JPL 97 recommendation                                =*
!=  Quantum yield: assumed shape based on work by Lee, 1982; normalized      =*
!=                 to unity at 248 nm                                        =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      REAL x1(kdata)
      REAL y1(kdata)

! local
      real, parameter :: tfac1 = 1./(248. - 193.)
      real, parameter :: xfac1 = 1./15.

      REAL :: yg(kw)
      REAL :: qy(nw)
      INTEGER :: n, idum

      if( initialize ) then
        CALL readit
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        WHERE( wc(1:nw-1) >= 248. )
          qy(1:nw-1) = 1.
        ELSEWHERE
          qy(1:nw-1) = max( (1. + (wc(1:nw-1) - 193.)*14.*tfac1)*xfac1,0. )
        ENDWHERE
        xsqy_tab(j)%sq(1:nw-1,1) = qy(1:nw-1) * yg(1:nw-1)
      endif

      CONTAINS

      SUBROUTINE readit
!*** cross sections from JPL11 recommendation

      n = 15
      CALL base_read( filespec='DATAJ1/ABS/HO2_jpl11.abs', &
                      skip_cnt=10,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20

      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r39

!=============================================================================*

      SUBROUTINE r44(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for N2O photolysis:    =*
!=              N2O + hv -> N2 + O(1D)                                       =*
!=  Cross section: from JPL 97 recommendation                                =*
!=  Quantum yield: assumed to be unity, based on Greenblatt and Ravishankara =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      real, parameter :: A0 = 68.21023                
      real, parameter :: A1 = -4.071805               
      real, parameter :: A2 = 4.301146E-02            
      real, parameter :: A3 = -1.777846E-04           
      real, parameter :: A4 = 2.520672E-07

      real, parameter :: B0 = 123.4014
      real, parameter :: B1 = -2.116255
      real, parameter :: B2 = 1.111572E-02
      real, parameter :: B3 = -1.881058E-05

      INTEGER :: iw
      REAL, save :: a(kw), b(kw)
      REAL :: lambda
      REAL :: t(nz)
      REAL :: bt(nz)

      if( initialize ) then
        DO iw = 1, nw-1
          lambda = wc(iw)   
          IF (lambda >= 173. .AND. lambda <= 240.) THEN
            A(iw) = (((A4*lambda+A3)*lambda+A2)*lambda+A1)*lambda+A0
            B(iw) = (((B3*lambda+B2)*lambda+B1)*lambda+B0)
          ENDIF
        ENDDO
      else
        call check_alloc( j, nz, nw-1 )

!*** cross sections according to JPL97 recommendation (identical to 94 rec.)
!*** see file DATAJ1/ABS/N2O_jpl94.abs for detail
!*** quantum yield of N(4s) and NO(2Pi) is less than 1% (Greenblatt and
!*** Ravishankara), so quantum yield of O(1D) is assumed to be unity

        t(1:nz) = MAX(194.,MIN(tlev(1:nz),320.))
        DO iw = 1, nw-1
          lambda = wc(iw)   
          IF (lambda >= 173. .AND. lambda <= 240.) THEN
            BT(1:nz) = (t(1:nz) - 300.)*EXP(B(iw))
            xsqy_tab(j)%sq(1:nz,iw) = EXP(A(iw)+BT(1:nz))
          ELSE
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          ENDIF
        ENDDO
      endif

      END SUBROUTINE r44

!=============================================================================*

      SUBROUTINE r45(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for ClONO2 photolysis: =*
!=        ClONO2 + hv -> Products                                            =*
!=                                                                           =*
!=  Cross section: JPL 97 recommendation                                     =*
!=  Quantum yield: JPL 97 recommendation                                     =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=150

      REAL x1(kdata)
      REAL y1(kdata),y2(kdata),y3(kdata)

! local
      REAL qy1, qy2
      REAL :: xs(nz)
      real :: t(nz)
      REAL, save :: yg1(kw), yg2(kw), yg3(kw)
      INTEGER i, iw, n, idum, chnl
      INTEGER ierr
      INTEGER iz
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )

        t(1:nz) = tlev(1:nz) - 296.
        chnl = xsqy_tab(j)%channel
        DO iw = 1, nw-1
!** quantum yields (from jpl97, same in jpl2011)
          IF( wc(iw) .LT. 308.) THEN
            qy1 = 0.6
          ELSEIF( (wc(iw) .GE. 308) .AND. (wc(iw) .LE. 364.) ) THEN
            qy1 = 7.143e-3 * wc(iw) - 1.6
          ELSEIF( wc(iw) .GT. 364. ) THEN
            qy1 = 1.0
          ENDIF
          IF( chnl == 2 ) then
            qy1 = 1.0 - qy1
          ENDIF
! compute T-dependent cross section
          xs(1:nz) = yg1(iw) * (1. + t(1:nz) &
                   * (yg2(iw) + t(1:nz)*yg3(iw)))
          xsqy_tab(j)%sq(1:nz,iw) = qy1 * xs(1:nz)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
!** cross sections from JPL97 recommendation.  Same in JPL-2011.

      integer :: nsav
      real    :: xsav(kz)

      n = 119 ; nsav = 119
      CALL base_read( filespec='DATAJ1/ABS/ClONO2_jpl97.abs', &
                      skip_cnt=2,rd_cnt=n,x=x1,y=y1,y1=y2,y2=y3 )
      xsav(1:n) = x1(1:n)
      y1(1:n)   = y1(1:n) * 1.E-20

      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y3,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r45

!=============================================================================*

      SUBROUTINE r46(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for BrONO2 photolysis: =*
!=        BrONO2 + hv -> Products                                            =*
!=                                                                           =*
!=  Cross section: JPL 03 recommendation                                     =*
!=  Quantum yield: JPL 03 recommendation                                     =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      REAL x1(kdata)
      REAL y1(kdata)

! local
      REAL, parameter :: qyld(2) = (/ .15,.85 /)

      REAL    :: yg1(kw)
      INTEGER :: n
      INTEGER :: chnl

      if( initialize ) then
        CALL readit
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        chnl = xsqy_tab(j)%channel
        xsqy_tab(j)%sq(1:nw-1,1) = qyld(chnl) * yg1(1:nw-1)
      endif

      CONTAINS

      SUBROUTINE readit
!** cross sections from JPL03 recommendation

      n = 61
      CALL base_read( filespec='DATAJ1/ABS/BrONO2_jpl03.abs', &
                      skip_cnt=13,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20

      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r46

!=============================================================================*

      SUBROUTINE r47(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for Cl2 photolysis:    =*
!=        Cl2 + hv -> 2 Cl                                                   =*
!=                                                                           =*
!=  Cross section: JPL 97 recommendation                                     =*
!=  Quantum yield: 1     (Calvert and Pitts, 1966)                           =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      real :: ex1(nz), ex2(nz)
      real :: alpha(kz)
      INTEGER iz, iw

      real :: aa, bb, bb2, sig

      if( .not. initialize ) then
        call check_alloc( j, nz, nw-1 )

! mabs = 1: Finlayson-Pitts and Pitts
! mabs = 2: JPL2011 formula
      
        DO iz = 1, nz
          aa = 402.7/tlev(iz)
          bb = exp(aa)
          bb2 = bb*bb
          alpha(iz) = (bb2 - 1.)/(bb2 + 1.)
        ENDDO

!** quantum yield = 1 (Calvert and Pitts, 1966)

        DO iw = 1, nw-1
          ex1(1:nz) = 27.3  * exp(-99.0 * alpha(1:nz) * (log(329.5/wc(iw)))**2)
          ex2(1:nz) = .932 * exp(-91.5 * alpha(1:nz) * (log(406.5/wc(iw)))**2)
          xsqy_tab(j)%sq(1:nz,iw) = 1.e-20 * sqrt(alpha(1:nz)) * (ex1(1:nz) + ex2(1:nz))
        ENDDO
      endif

      END SUBROUTINE r47

!=============================================================================*

      SUBROUTINE r101(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CH2(OH)CHO     =*
!=  (glycolaldehye, hydroxy acetaldehyde) photolysis:                        =*
!=           CH2(OH)CHO + hv -> Products                                     =*
!=                                                                           =*
!=  Quantum yield about 50%                                                  =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER :: n
      REAL x(kdata), y(kdata)

! local
      real, parameter :: qyld(3) = (/ .83, .10, .07 /)

      REAL    :: yg(kw)
      INTEGER :: chnl

      if( initialize ) then
        chnl = xsqy_tab(j)%channel
        CALL readit
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * qyld(chnl)
      endif

      CONTAINS

      SUBROUTINE readit

      n = 63
      CALL base_read( filespec='DATAJ1/CH2OHCHO/glycolaldehyde_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x,y=y )
      y(1:n) = y(1:n) * 1.e-20
         
      CALL add_pnts_inter2(x,y,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r101

!=============================================================================*

      SUBROUTINE r103(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CH3COCHCH2     =*
!=  Methyl vinyl ketone photolysis:                                          =*
!=           CH3COCH=CH2 + hv -> Products                                     =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=150

      INTEGER i, n
      REAL x(kdata), y(kdata)

! local
      REAL, save :: yg(kw)
      REAL :: qy(nz)
      INTEGER ierr
      INTEGER iw
      INTEGER mabs

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! mabs = 1: Schneider and moortgat
! mabs = 2: jpl 2011
!     mabs = 2

! quantum yield from
! Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
! and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
! J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
! depends on pressure and wavelength, set upper limit to 1.0

        DO iw = 1, nw - 1
          qy(1:nz) = exp(-0.055*(wc(iw) - 308.)) &
                   / (5.5 + 9.2e-19*airden(1:nz))
          qy(1:nz) = min(qy(1:nz), 1.)
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * qy(1:nz)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      n = 146
      CALL base_read( filespec='DATAJ1/ABS/MVK_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x,y=y )
      y(1:n) = y(1:n) * 1.e-20

      CALL add_pnts_inter2(x,y,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r103

!=============================================================================*

      SUBROUTINE r106(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CH3CH2ONO2     =*
!=  ethyl nitrate       photolysis:                                          =*
!=           CH3CH2ONO2 + hv -> CH3CH2O + NO2                                =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER i, n1, n2
      REAL x1(kdata), y1(kdata)
      REAL x2(kdata), y2(kdata)

! local
      INTEGER ierr
      INTEGER iw
      REAL dum
      REAL qy, sig
      REAL, save :: yg1(kw), yg2(kw)
      real :: t(nz)

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! quantum yield  = 1

        t(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw - 1
          xsqy_tab(j)%sq(1:nz,iw) = yg1(iw)*exp(yg2(iw)*t(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: n
      real :: wrk(kdata)
      character(len=256) :: emsg

      n = 63
      CALL base_read( filespec='DATAJ1/RONO2/RONO2_talukdar.abs', &
                      skip_cnt=10,rd_cnt=n,x=x1,y=wrk,y1=wrk, &
                      y2=y1,y3=y2,y4=wrk,y5=wrk )

      x2(1:n) = x1(1:n)

      n1 = count( y1(1:n) > 0. )
      if( n1 > 0 ) then
        wrk(1:n1) = pack( y1(1:n),mask=y1(1:n) > 0. )
        y1(1:n1)  = wrk(1:n1) * 1.e-20
        wrk(1:n1) = pack( x1(1:n),mask=y1(1:n) > 0. )
        x1(1:n1)  = wrk(1:n1)
        CALL add_pnts_inter2(x1,y1,yg1,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      else
        yg1(:nw) = 0.
      endif


      n2 = count( y2(1:n) > 0. )
      if( n2 > 0 ) then
        wrk(1:n2) = pack( y2(1:n),mask=y2(1:n) > 0. )
        y2(1:n2)  = wrk(1:n2) * 1.e-3
        wrk(1:n2) = pack( x2(1:n),mask=y2(1:n) > 0. )
        x2(1:n2)  = wrk(1:n2)
        CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
        CALL addpnt(x2,y2,kdata,n2,           1.e+38,y2(n2))
        CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
        IF (ierr .NE. 0) THEN
          write(emsg,'(''readit: Error '',i5,'' in inter2 for '',a)') ierr,trim(xsqy_tab(j)%label)
          call wrf_error_fatal( trim(emsg) )
        ENDIF
      else
        yg2(:nw) = 0.
      endif

      END SUBROUTINE readit

      END SUBROUTINE r106

!=============================================================================*

      SUBROUTINE r107(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for CH3CHONO2CH3   =*
!=  isopropyl nitrate   photolysis:                                          =*
!=           CH3CHONO2CH3 + hv -> CH3CHOCH3 + NO2                            =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER i, n1, n2
      REAL x1(kdata), y1(kdata)
      REAL x2(kdata), y2(kdata)

! local
      INTEGER ierr
      INTEGER iw
      REAL dum
      REAL qy, sig
      REAL, save :: yg1(kw), yg2(kw)
      real :: t(nz)

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! quantum yield  = 1

        t(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw - 1
          xsqy_tab(j)%sq(1:nz,iw) = yg1(iw)*exp(yg2(iw)*t(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      integer :: n
      real :: wrk(kdata)
      character(len=256) :: emsg

      n = 63
      CALL base_read( filespec='DATAJ1/RONO2/RONO2_talukdar.abs', &
                      skip_cnt=10,rd_cnt=n,x=x1,y=wrk, &
                      y1=wrk,y2=wrk,y3=wrk,y4=y1,y5=y2 )

      x2(1:n) = x1(1:n)

      n1 = count( y1(1:n) > 0. )
      if( n1 > 0 ) then
        wrk(1:n1) = pack( y1(1:n),mask=y1(1:n) > 0. )
        y1(1:n1)  = wrk(1:n1) * 1.e-20
        wrk(1:n1) = pack( x1(1:n),mask=y1(1:n) > 0. )
        x1(1:n1)  = wrk(1:n1)
        CALL add_pnts_inter2(x1,y1,yg1,kdata,n1, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      else
        yg1(:nw) = 0.
      endif

      n2 = count( y2(1:n) > 0. )
      if( n2 > 0 ) then
        wrk(1:n2) = pack( y2(1:n),mask=y2(1:n) > 0. )
        y2(1:n2)  = wrk(1:n2) * 1.e-3
        wrk(1:n2) = pack( x2(1:n),mask=y2(1:n) > 0. )
        x2(1:n2)  = wrk(1:n2)
        CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
        CALL addpnt(x2,y2,kdata,n2,           1.e+38,y2(n2))
        CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
        IF (ierr .NE. 0) THEN
          write(emsg,'(''readit: Error '',i5,'' in inter2 for '',a)') ierr,trim(xsqy_tab(j)%label)
          call wrf_error_fatal( trim(emsg) )
        ENDIF
      else
        yg2(:nw) = 0.
      endif

      END SUBROUTINE readit

      END SUBROUTINE r107

!=============================================================================*

      SUBROUTINE r108(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for                =*
!=   nitroxy ethanol CH2(OH)CH2(ONO2) + hv -> CH2(OH)CH2(O.) + NO2           =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
! coefficients from Roberts and Fajer 1989, over 270-306 nm
      real, parameter ::a = -2.359E-3
      real, parameter ::b = 1.2478
      real, parameter ::c = -210.4

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
! quantum yield  = 1
        WHERE( wc(1:nw-1) >= 270. .AND. wc(1:nw-1) <= 306. )
          xsqy_tab(j)%sq(1:nw-1,1) = EXP(c + wc(1:nw-1)*(b + wc(1:nw-1)*a))
        ELSEWHERE
          xsqy_tab(j)%sq(1:nw-1,1) = 0.
        ENDWHERE
      endif

      END SUBROUTINE r108

!=============================================================================*

      SUBROUTINE r109(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for                =*
!=   nitroxy acetone CH3COCH2(ONO2) + hv -> CH3COCH2(O.) + NO2               =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
! coefficients from Roberts and Fajer 1989, over 284-335 nm
      real, parameter :: a = -1.365E-3
      real, parameter :: b = 0.7834
      real, parameter :: c = -156.8

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
! quantum yield  = 1
        WHERE( wc(1:nw-1) >= 284. .AND. wc(1:nw-1) <= 335. )
          xsqy_tab(j)%sq(1:nw-1,1) = EXP(c + wc(1:nw-1)*(b + wc(1:nw-1)*a))
        ELSEWHERE
          xsqy_tab(j)%sq(1:nw-1,1) = 0.
        ENDWHERE
      endif

      END SUBROUTINE r109

!=============================================================================*

      SUBROUTINE r110(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for                =*
!=  t-butyl nitrate C(CH3)3(ONO2) + hv -> C(CH3)(O.) + NO2                   =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
! coefficients from Roberts and Fajer 1989, over 270-330 nm
      real, parameter ::a = -0.993E-3
      real, parameter ::b = 0.5307
      real, parameter ::c = -115.5

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
! quantum yield  = 1
        WHERE( wc(1:nw-1) >= 270. .AND. wc(1:nw-1) <= 330. )
          xsqy_tab(j)%sq(1:nw-1,1) = EXP(c + wc(1:nw-1)*(b + wc(1:nw-1)*a))
        ELSEWHERE
          xsqy_tab(j)%sq(1:nw-1,1) = 0.
        ENDWHERE
      endif

      END SUBROUTINE r110

!=============================================================================*

      SUBROUTINE r112(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for hydroxyacetone =*
!=  CH2(OH)COCH3        photolysis:                                          =*
!=           CH2(OH)COCH3  -> CH3CO + CH2OH
!=                         -> CH2(OH)CO + CH3                                =*
!=                                                                           =*
!=  Cross section from Orlando et al. (1999)                                 =*
!=                                                                           =*
!=  Quantum yield assumed 0.325 for each channel (J. Orlando, priv.comm.2003)=*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER :: n
      REAL    :: x(kdata), y(kdata)

! local
      REAL, parameter :: qy = .325

      REAL :: yg(kw)

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        CALL readit
        xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1) * qy
      endif

      CONTAINS

      SUBROUTINE readit

      n = 96
      CALL base_read( filespec='DATAJ1/ABS/Hydroxyacetone_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x,y=y )
      y(1:n) = y(1:n) * 1.e-20

      CALL add_pnts_inter2(x,y,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r112

!=============================================================================*

      SUBROUTINE r113(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for HOBr           =*
!=  HOBr -> OH + Br                                                          =*
!=  Cross section from JPL 2003                                              =*
!=  Quantum yield assumed unity as in JPL2003                                =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      REAL    :: sig(nw)
      REAL    :: xfac1(nw)
      INTEGER :: iw

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        xsqy_tab(j)%sq(1:nw-1,1) = 0.
        WHERE( wc(1:nw-1) >= 250. .and. wc(1:nw-1) <= 550. )
          xfac1(1:nw-1) = 1./wc(1:nw-1)
          sig(1:nw-1) = 24.77 * exp( -109.80*(LOG(284.01*xfac1(1:nw-1)))**2 ) & 
                + 12.22 * exp(  -93.63*(LOG(350.57*xfac1(1:nw-1)))**2 ) & 
                + 2.283 * exp(- 242.40*(LOG(457.38*xfac1(1:nw-1)))**2 )
          xsqy_tab(j)%sq(1:nw-1,1) = sig(1:nw-1) * 1.e-20
        ENDWHERE
      endif

      END SUBROUTINE r113

!=============================================================================*

      SUBROUTINE r114(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for BrO            =*
!=  BrO -> Br + O                                                            =*
!=  Cross section from JPL 2003                                              =*
!=  Quantum yield assumed unity as in JPL2003                                =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! local
      INTEGER :: i, n
      REAL :: x(20), y(20)
      REAL :: dum
      REAL :: yg(kw)

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        OPEN(UNIT=kin,FILE='DATAJ1/ABS/BrO.jpl03',STATUS='old')
        DO i = 1, 14
          READ(kin,*)
        ENDDO
        n = 15
        DO i = 1, n
          READ(kin,*) x(i), dum, y(i)
        ENDDO
        CLOSE(kin)

        y(1:n) = y(1:n) * 1.e-20
        n = n + 1
        x(n) = dum
! use bin-to-bin interpolation
        CALL inter4(nw,wl,yg,n,x,y,1)
        xsqy_tab(j)%sq(1:nw-1,1) = yg(1:nw-1)
      endif

      END SUBROUTINE r114

!=============================================================================*

      SUBROUTINE r118(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!= NO3-(aq) photolysis for snow simulations                                  =*
!=        a) NO3-(aq) + hv -> NO2 + O-                                       =*
!=        b) NO3-(aq) + hv -> NO2- + O(3P)                                   =*
!=  Cross section:                                                           =*
!=  Burley & Johnston, Geophys. Res. Lett., 19, 1359-1362 (1992)             =*
!=  Chu & Anastasio, J. Phys. Chem. A, 107, 9594-9602 (2003)                 =*
!=  Quantum yield:                                                           =*
!=  Warneck & Wurzinger, J. Phys. Chem., 92, 6278-6283 (1988)                =*
!=  Chu & Anastasio, J. Phys. Chem. A, 107, 9594-9602 (2003)                 =*
!-----------------------------------------------------------------------------*
!= NOTE: user may have to manually add these reactions to the end of the     =*
!= reaction list in file usrinp to include these reactions for a snow run:   =*
!= T 74 NO3-(aq) -> NO2 + O-                                                 =*
!= T 75 NO3-(aq) -> NO2- + O(3P)                                             =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=50

      REAL x1(kdata),x2(kdata)
      REAL y1(kdata),y2(kdata)     ! y1 = 20'C, y2 = -20'C

! local
      REAL, parameter :: qyld(2:3) = (/ 1.1e-3,1. /)
!     REAL, parameter :: qy2 = 1.1e-3
!     REAL, parameter :: qy3 = 1.

      REAL, save :: yg2(kw)
      REAL :: qy1(nz)
      INTEGER i, iw, n, idum
      integer :: chnl
      LOGICAL, save :: is_initialized = .false.

      chnl = xsqy_tab(j)%channel
      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
        if( chnl > 1 ) then
          call check_alloc( ndx=j, nz=nw-1, nw=1 )
          xsqy_tab(j)%sq(1:nw-1,1) = qyld(chnl)*yg2(1:nw-1)
        endif
      else
        if( chnl == 1 ) then
          call check_alloc( j, nz, nw-1 )

          qy1(1:nz) = exp(-2400./tlev(1:nz) + 3.6) ! Chu & Anastasio, 2003
          DO iw = 1, nw-1
            xsqy_tab(j)%sq(1:nz,iw) = qy1(1:nz)*yg2(iw)
          ENDDO
        endif
      endif

      CONTAINS

      SUBROUTINE readit
!** NO3-(aq) cross sections from Chu and Anastasio 2003:
! convert from molar abs log10 to cm2 per molec

      real :: wrk(kdata)

      n = 43
      CALL base_read( filespec='DATAJ1/ABS/NO3-_CA03.abs', &
                      skip_cnt=7,rd_cnt=n,x=x1,y=y1,y1=wrk, &
                      y2=wrk,y3=wrk,y4=wrk )
      y1(1:n) = y1(1:n) * 3.82e-21
      CALL add_pnts_inter2(x1,y1,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r118

!=============================================================================*

      SUBROUTINE r119(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide the product (cross section) x (quantum yield) for                =*
!=    methylethylketone                                                      =*
!=  CH3COCH2CH3 photolysis:                                                  =*
!=           CH3COCH2CH3  -> CH3CO + CH2CH3                                  =*
!=                                                                           =*
!=  Cross section from Martinez et al. (1992)                                =*
!=                                                                           =*
!=  Quantum yield assumed 0.325 for each channel (J. Orlando, priv.comm.2003)=*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER i, n
      REAL x(kdata), y(kdata)

! local
      REAL, save :: yg(kw)
      REAL :: ptorr(nz)
      REAL :: qy(nz)
      INTEGER ierr
      INTEGER iw

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! Quantum Yields from 
! Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
! other channels assumed negligible (less than 10%).
! Total quantum yield  = 0.38 at 760 Torr.
! Stern-Volmer form given:  1/phi = 0.96 + 2.22e-3*P(torr)
!     compute local pressure in torr

        ptorr(1:nz) = 760.*airden(1:nz)/2.69e19
        qy(1:nz)    = min( 1./(0.96 + 2.22E-3*ptorr(1:nz)),1. )
        DO iw = 1, nw-1
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * qy(1:nz)
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      real :: wrk(kdata)
      n = 96
      CALL base_read( filespec='DATAJ1/ABS/Martinez.abs', &
                      skip_cnt=4,rd_cnt=n,x=x,y=wrk,y1=y, &
                      y2=wrk,y3=wrk )
      y(1:n) = y(1:n) * 1.e-20

      CALL add_pnts_inter2(x,y,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r119

!=============================================================================*

      SUBROUTINE r120(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for PPN photolysis:    =*
!=       PPN + hv -> Products                                                =*
!=                                                                           =*
!=  Cross section: from JPL 2006 (originally from Harwood et al. 2003)       =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER :: iw
      INTEGER :: n
      REAL    :: x1(kdata), x2(kdata)
      REAL    :: y1(kdata), y2(kdata)

! local
      real, parameter :: qyld(2) = (/ 0.61,0.39 /)

      INTEGER :: chnl
      REAL, save :: yg(kw), yg2(kw)
      real :: t(nz)
      REAL :: sig(nz)
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )
    
        chnl = xsqy_tab(j)%channel
        t(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw-1
          sig(1:nz) = yg(iw) * EXP(yg2(iw)*t(1:nz))
          xsqy_tab(j)%sq(1:nz,iw) = qyld(chnl) * sig(1:nz)
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from JPL 2011 (originally from Harwood et al. 2003)

      integer :: nsav
      real    :: xsav(kdata)

      n = 66 ; nsav = 66
      CALL base_read( filespec='DATAJ1/ABS/PPN_Harwood.txt', &
                      skip_cnt=10,rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.E-20
      y2(1:n) = y2(1:n) * 1E-3
      xsav(1:n) = x1(1:n)
 
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r120

!=============================================================================*

      SUBROUTINE r122(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CH2=CHCHO          =*
!=  (acrolein) photolysis:                                                   =*
!=       CH2=CHCHO + hv -> Products                                          =*
!=                                                                           =*
!=  Cross section: from JPL 2006 (originally from Magneron et al.            =*
!=  Quantum yield: P-dependent, JPL 2006 orig. from Gardner et al.           =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=100

      INTEGER iw
      INTEGER i, n
      INTEGER n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

! local
      REAL, save :: yg(kw)
      real :: qy(nz), qym1(nz)
      REAL sig
      INTEGER ierr

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )

! quantum yields are pressure dependent between air number densities
! of 8e17 and 2.6e19, Gardner et al.:
        DO iw = 1, nw-1
          where( airden(1:nz) > 2.6e19 )
            qy(1:nz) = 0.004
          elsewhere( airden(1:nz) > 8.e17 .and. airden(1:nz) <= 2.6e19 )
            qym1(1:nz) = 0.086 + 1.613e-17 * airden(1:nz)
            qy(1:nz)   = 0.004 + 1./qym1(1:nz)
          elsewhere( airden(1:nz) <= 8.e17 )
            qym1(1:nz) = 0.086 + 1.613e-17 * 8.e17
            qy(1:nz)   = 0.004 + 1./qym1(1:nz)
          endwhere
          xsqy_tab(j)%sq(1:nz,iw) = qy(1:nz) * yg(iw)
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from JPL 2006 (originally from Magneron et al.)

      n = 55
      CALL base_read( filespec='DATAJ1/ABS/Acrolein.txt',skip_cnt=6,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20
 
      CALL add_pnts_inter2(x1,y1,yg,kdata,n,nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r122

!=============================================================================*

      SUBROUTINE r125(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for ClO photolysis     =*
!=       ClO + hv -> Cl + O                                                  =*
!=                                                                           =*
!=  Cross section: from Maric and Burrows 1999                               =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      INTEGER, intent(inout) :: j
! data arrays
      integer, PARAMETER :: kdata=500

      INTEGER iw
      INTEGER i, n
      REAL x1(kdata)
      REAL y1(kdata)
      INTEGER ierr

! local
      REAL :: yg(kw)
      REAL qy1, qy2

      real, save :: tmp(12)
      real, save :: ygt(kw,12)
      real x(kdata), y(kdata,12)
      real tx, xdum
      integer m, nn, ii
      real yy
      INTEGER m1, m2
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          tmp(1)    = 180.
          tmp(2:12) = (/ (190. + 10.*real(m-1),m=2,12) /)
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )

        DO i = 1, nz
          tx = tlev(i)
! locate temperature indices for interpolation:
          m1 = 1 + INT(.1*(tx - 190.))
          m1 = MIN(MAX(1 ,m1),11)
          m2 = m1 + 1
          DO iw = 1, nw-1
            yy = ygt(iw,m1) + (ygt(iw,m2) - ygt(iw,m1)) &
                 * (tx - tmp(m1))/(tmp(m2) - tmp(m1))
! threshold for O(1D) productionis 263.4 nm:
            if(wc(iw) .lt. 263.4) then
               qy1 = 1.
            else
               qy1 = 0.
            endif
            qy2 = 1. - qy1
            if( xsqy_tab(j)%channel == 1 ) then
              xsqy_tab(j)%sq(i,iw) = qy1 * yy
            elseif( xsqy_tab(j)%channel == 2 ) then
              xsqy_tab(j)%sq(i,iw) = qy2 * yy
            endif
          ENDDO
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from 
! Maric D. and J.P. Burrows, J. Quantitative Spectroscopy and 
! Radiative Transfer 62, 345-369, 1999.  Data was downloaded from 
! their web site on 15 September 2009.

      integer :: nsav
      real    :: xsav(kdata)

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/ClO_spectrum.prn',STATUS='OLD')
      DO i = 1, 2
         READ(kin,*)
      ENDDO
      nn = 453 ; nsav = 453
      DO ii = 1, nn
         i = nn - ii + 1
         READ(kin,*) xdum, x(i), xdum, (y(i,m), m = 1, 12)
      ENDDO
      CLOSE(kin)

      xsav(1:nn) = x(1:nn)
      DO m = 1, 12
         nn = nsav
         x1(1:nn) = xsav(1:nn)
         y1(1:nn) = y(1:nn,m)
         CALL add_pnts_inter2(x1,y1,yg,kdata,nn, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
         ygt(1:nw-1,m) = yg(1:nw-1)
      ENDDO

      END SUBROUTINE readit

      END SUBROUTINE r125

!=============================================================================*

      SUBROUTINE r129(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for bromine nitrite    =*
!=       BrONO -> Br + NO2                                                   =*
!=       BrONO -> BrO + NO                                                   =*
!=                                                                           =*
!=  Cross section: from IUPAC (vol.3)                                        =*
!=  Quantum yield: Assumed to be 0.5 for each channel                        =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=50

      INTEGER :: n
      INTEGER :: chnl
      REAL    :: x1(kdata)
      REAL    :: y1(kdata)

! local
      real, parameter :: qyld(2) = 0.5

      REAL :: yg(kw)

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        CALL readit
        chnl = xsqy_tab(j)%channel
        xsqy_tab(j)%sq(1:nw-1,1) = qyld(chnl) * yg(1:nw-1)
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from IUPAC (vol III) 2007

      n = 32
      CALL base_read( filespec='DATAJ1/ABS/BrONO.abs', &
                      skip_cnt=8,rd_cnt=n,x=x1,y=y1 )
 
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r129

!******************************************************************

      SUBROUTINE r131(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for 
!=       NOCl -> NO + Cl                                                     =*
!=  Cross section: from IUPAC (vol.3)                                        =*
!=  Quantum yield: Assumed to be 1                                           =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=150

      INTEGER iw
      INTEGER i, n, ii
      REAL x1(kdata), y1(kdata)
      REAL y223(kdata),y243(kdata),y263(kdata),y298(kdata), &
           y323(kdata), y343(kdata)
      INTEGER ierr

! local
      REAL, save :: yg223(kw),yg243(kw),yg263(kw), &
                    yg298(kw),yg323(kw), yg343(kw)
      REAL qy, sig

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
! quantum yields assumed unity
        DO iw = 1, nw-1
          where( tlev(1:nz) .le. 223. )
            xsqy_tab(j)%sq(1:nz,iw) = yg223(iw)
          elsewhere (tlev(1:nz) .gt. 223. .and. tlev(1:nz) .le. 243. )
            xsqy_tab(j)%sq(1:nz,iw) = yg223(iw) &
                   + (yg243(iw) - yg223(iw))*(tlev(1:nz) - 223.)*.05
          elsewhere (tlev(1:nz) .gt. 243. .and. tlev(1:nz) .le. 263. )
            xsqy_tab(j)%sq(1:nz,iw) = yg243(iw) &
                   + (yg263(iw) - yg243(iw))*(tlev(1:nz) - 243.)*.05
          elsewhere (tlev(1:nz) .gt. 263. .and. tlev(1:nz) .le. 298. )
            xsqy_tab(j)%sq(1:nz,iw) = yg263(iw) &
                   + (yg298(iw) - yg263(iw))*(tlev(1:nz) - 263.)/35.
          elsewhere (tlev(1:nz) .gt. 298. .and. tlev(1:nz) .le. 323. )
            xsqy_tab(j)%sq(1:nz,iw) = yg298(iw) &
                   + (yg323(iw) - yg298(iw))*(tlev(1:nz) - 298.)*.04
          elsewhere (tlev(1:nz) .gt. 323. .and. tlev(1:nz) .le. 343. )
            xsqy_tab(j)%sq(1:nz,iw) = yg323(iw) &
                   + (yg343(iw) - yg323(iw))*(tlev(1:nz) - 323.)*.05
          elsewhere (tlev(1:nz) .gt. 343. )
            xsqy_tab(j)%sq(1:nz,iw) = 0.
          endwhere
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from IUPAC (vol III) 2007

      integer :: nsav
      real    :: xsav(kdata)

      n = 80
      CALL base_read( filespec='DATAJ1/ABS/NOCl.abs', &
                      skip_cnt=7,rd_cnt=n,x=x1,y=y1 )
      y223(1:n) = y1(1:n)
      y243(1:n) = y1(1:n)
      y263(1:n) = y1(1:n)
      y298(1:n) = y1(1:n)
      y323(1:n) = y1(1:n)
      y343(1:n) = y1(1:n)
      ii = 61
      CALL base_read( filespec='DATAJ1/ABS/NOCl.abs', &
                      skip_cnt=88,rd_cnt=ii,x=x1(n+1:),y=y223(n+1:), &
                      y1=y243(n+1:),y2=y263(n+1:),y3=y298(n+1:), &
                      y4=y323(n+1:),y5=y343(n+1:) )
      
      n = n + ii
      nsav = n ; xsav(1:n) = x1(1:n)

      CALL add_pnts_inter2(x1,y223,yg223,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y243,yg243,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y263,yg263,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y298,yg298,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y323,yg323,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y343,yg343,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r131

!******************************************************************

      SUBROUTINE r132(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for 
!=       OClO -> Products                                                    =*
!=  Cross section: from Wahner et al., J. Phys. Chem. 91, 2734, 1987         =*
!=  Quantum yield: Assumed to be 1                                           =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=2000

      INTEGER iw
      INTEGER i, n
      REAL x1(kdata), y1(kdata)
      integer nn, n204, n296, n378
      REAL x204(kdata),x296(kdata),x378(kdata)
      REAL y204(kdata),y296(kdata),y378(kdata)

      INTEGER ierr

! local
      REAL, save :: yg204(kw),yg296(kw),yg378(kw)
      REAL qy, sig

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
! quantum yields assumed unity
        DO iw = 1, nw-1
          where(tlev(1:nz) .le. 204. )
            xsqy_tab(j)%sq(1:nz,iw) = yg204(iw)
          elsewhere (tlev(1:nz) .gt. 204. .and. tlev(1:nz) .le. 296. )
            xsqy_tab(j)%sq(1:nz,iw) = yg204(iw) &
                + (yg296(iw) - yg204(iw))*(tlev(1:nz) - 204.)/92.
          elsewhere (tlev(1:nz) .gt. 296. .and. tlev(1:nz) .le. 378. )
            xsqy_tab(j)%sq(1:nz,iw) = yg296(iw) &
                + (yg378(iw) - yg296(iw))*(tlev(1:nz) - 296.)/82.
          elsewhere (tlev(1:nz) .gt. 378. )
            xsqy_tab(j)%sq(1:nz,iw) = yg378(iw)  
          endwhere
        ENDDO 
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from 
!A. Wahner, G.S. tyndall, A.R. Ravishankara, J. Phys. Chem., 91, 2734, (1987).
!Supplementary Data, as quoted at:
!http://www.atmosphere.mpg.de/enid/26b4b5172008b02407b2e47f08de2fa1,0/Spectra/Introduction_1rr.html

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/OClO.abs',STATUS='OLD')
      DO i = 1, 6
         READ(kin,*)
      ENDDO
      n204 = 1074-6
      DO i = 1, n204
         READ(kin,*) x204(i), y204(i)
      ENDDO

      READ(kin,*)
      n296 = 1067
      do i = 1, n296
         read(kin,*) x296(i), y296(i)
      enddo

      read(kin,*)
      n378 = 1068
      do i = 1, n378
         read(kin,*) x378(i), y378(i)
      enddo

      CLOSE(kin)
      
      CALL add_pnts_inter2(x204,y204,yg204,kdata,n204, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      CALL add_pnts_inter2(x296,y296,yg296,kdata,n296, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      CALL add_pnts_inter2(x378,y378,yg378,kdata,n378, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r132

!******************************************************************

      SUBROUTINE pxCH2O(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  JPL 2011 recommendation.                                                 =*
!=  Provide product of (cross section) x (quantum yield) for CH2O photolysis =*
!=        (a) CH2O + hv -> H + HCO                                           =*
!=        (b) CH2O + hv -> H2 + CO                                           =*
!=  written by s. madronich march 2013
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

      integer, PARAMETER :: kdata=200

! data arrays
      INTEGER iw
      INTEGER n, n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y298(kdata), tcoef(kdata)
      REAL qr(kdata), qm(kdata)

! local
      INTEGER ierr
      REAL ak300
      real qyr300, qym300
      REAL, save :: yg1(kw), yg2(kw), yg3(kw), yg4(kw)
      REAL :: t(nz), t1(nz)
      REAL :: sig(nz)
      REAL :: qymt(nz)
      REAL :: akt(nz)
      LOGICAL, save :: is_initialized = .false.

      if( initialize ) then
        if( .not. is_initialized ) then
          CALL readit
          is_initialized = .true.
        endif
      else
        call check_alloc( j, nz, nw-1 )

        t(1:nz)  = tlev(1:nz) - 298.
        t1(1:nz) = (300. - tlev(1:nz))/80.
        DO iw = 1, nw - 1
! correct cross section for temperature dependence:
          sig(1:nz) = yg1(iw) + yg2(iw) * t(1:nz)
! assign room temperature quantum yields for radical and molecular channels
          qyr300 = yg3(iw)
          qym300 = yg4(iw)
! between 330 ande 360 nm, molecular channel is pressure and temperature dependent.
          IF (wc(iw) .ge. 330. .and. wc(iw) .lt. 360. .and. qym300 .gt. 0.) then
            ak300 = (1. - (qym300+qyr300))/(qym300*(1. - qyr300))
            ak300 = ak300/2.45e19
            akt(1:nz) = ak300 * (1. + 0.05 * (wc(iw) - 329.) * t1(1:nz))
            qymt(1:nz) = 1./(1./(1.-qyr300) + akt(1:nz)*airden(1:nz))
          ELSE
            qymt(1:nz) = qym300
          ENDIF
          if( xsqy_tab(j)%channel == 1 ) then
            xsqy_tab(j)%sq(1:nz,iw) = sig(1:nz) * qyr300
          elseif( xsqy_tab(j)%channel == 2 ) then
            xsqy_tab(j)%sq(1:nz,iw) = sig(1:nz) * qymt(1:nz)
          endif
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
! read JPL2011 cross section data:

      integer :: nsav
      real    :: xsav(kdata)

      n = 150 ; nsav = 150
      CALL base_read( filespec='DATAJ1/CH2O/CH2O_jpl11.abs', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=y298, &
                      y1=tcoef )
      y298(1:n)  = y298(1:n) * 1.e-20
      tcoef(1:n) = tcoef(1:n) * 1.e-24
      xsav(1:n) = x1(1:n)
      
!     terminate endpoints and interpolate to working grid
      CALL add_pnts_inter2(x1,y298,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))
      
      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,tcoef,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! quantum yields: Read, terminate, interpolate:

      n = 112 ; nsav = 112
      CALL base_read( filespec='DATAJ1/CH2O/CH2O_jpl11.yld', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=qr,y1=qm )
      xsav(1:n) = x1(1:n)

      CALL add_pnts_inter2(x1,qr,yg3,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/qr(1),0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,qm,yg4,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/qm(1),0./))

      END SUBROUTINE readit

      END SUBROUTINE pxCH2O

!=============================================================================*

      SUBROUTINE r140(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for CHCl3 photolysis:  =*
!=      CHCL3 + hv -> Products                                               =*
!=  Cross section: from JPL 2011 recommendation                              =*
!=  Quantum yield: assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=50

      REAL x1(kdata)
      REAL y1(kdata)

! local
! temperature correction factors:
      real, parameter :: b0 = 3.7973
      real, parameter :: b1 = -7.0913e-2
      real, parameter :: b2 = 4.9397e-4
      real, parameter :: b3 = -1.5226e-6
      real, parameter :: b4 = 1.7555e-9

      INTEGER :: iw, n
      REAL, save :: yg(kw)
      REAL    :: tcoeff
      REAL    :: w1
      REAL    :: sig(nz)
      REAL    :: temp(nz)

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
      
!** quantum yield assumed to be unity
        temp(1:nz) = min(max(tlev(1:nz),210.),300.) - 295.
        DO iw = 1, nw-1
! compute temperature correction coefficients:
          tcoeff = 0.
          w1 = wc(iw)
          IF(w1 > 190. .AND. w1 < 240.) THEN 
            tcoeff = b0 + w1*(b1 + w1*(b2 + w1*(b3 + w1*b4)))
          ENDIF
          xsqy_tab(j)%sq(1:nz,iw) = yg(iw) * 10.**(tcoeff*temp(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit

      n = 39
      CALL base_read( filespec='DATAJ1/ABS/CHCl3_jpl11.abs', &
                      skip_cnt=3,rd_cnt=n,x=x1,y=y1 )
      y1(1:n) = y1(1:n) * 1.E-20
      
      CALL add_pnts_inter2(x1,y1,yg,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r140

!=============================================================================*

      SUBROUTINE r141(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for C2H5ONO2           =*
!=  photolysis:                                                              =*
!=          C2H5ONO2 + hv -> C2H5O + NO2                                     =*
!=                                                                           =*
!=  Cross section:  IUPAC 2006 (Atkinson et al., ACP, 6, 3625-4055, 2006)    =*
!=  Quantum yield: Assumed to be unity                                       =*
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata = 50

      INTEGER :: iw
      REAL    :: x1(kdata), x2(kdata)
      REAL    :: y1(kdata), y2(kdata)

! local
      REAL, save :: yg1(kw), yg2(kw)
      real :: t(nz)

      if( initialize ) then
        CALL readit
      else
        call check_alloc( j, nz, nw-1 )
! quantum yield = 1
        t(1:nz) = tlev(1:nz) - 298.
        DO iw = 1, nw - 1
          xsqy_tab(j)%sq(1:nz,iw) = yg1(iw) * exp(yg2(iw) * t(1:nz))
        ENDDO
      endif

      CONTAINS

      SUBROUTINE readit
! mabs: absorption cross section options: 1:  IUPAC 2006

      integer :: n, nsav
      real    :: xsav(kdata)

      n = 32 ; nsav = 32
      CALL base_read( filespec='DATAJ1/RONO2/C2H5ONO2_iup2006.abs', &
                      skip_cnt=4,rd_cnt=n,x=x1,y=y1,y1=y2 )
      y1(1:n) = y1(1:n) * 1.e-20
      y2(1:n) = y2(1:n) * 1.e-3
      xsav(1:n) = x1(1:n)

      CALL add_pnts_inter2(x1,y1,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      n = nsav ; x1(1:n) = xsav(1:n)
      CALL add_pnts_inter2(x1,y2,yg2,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r141

      SUBROUTINE r146(nw,wl,wc,nz,tlev,airden,j)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Provide product (cross section) x (quantum yield) for                    =*
!=     molecular Iodine, I2                                                  =*
!=  cross section from JPL2011                                               =*
!=  Quantum yield: wave-dep, from Brewer and Tellinhuisen, 1972              =*
!=  Quantum yield for Unimolecular Dissociation of I2 in Visible Absorption  =*
!=  J. Chem. Phys. 56, 3929-3937, 1972.
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nw
      INTEGER, intent(in) :: nz
      INTEGER, intent(inout) :: j
      REAL, intent(in)    :: wl(kw), wc(kw)
      REAL, intent(in)    :: tlev(kz)
      REAL, intent(in)    :: airden(kz)

! data arrays
      integer, PARAMETER :: kdata=200

      INTEGER :: n
      REAL    :: x(kdata), y(kdata)

! local
      REAL    :: yg1(kw), yg2(kw)

      if( initialize ) then
        call check_alloc( ndx=j, nz=nw-1, nw=1 )
        CALL readit
        xsqy_tab(j)%sq(1:nw-1,1) = yg1(1:nw-1) * yg2(1:nw-1)
      endif

      CONTAINS

      SUBROUTINE readit
! cross section from JPL2011

      n = 104
      CALL base_read( filespec='DATAJ1/ABS/I2_jpl11.abs', &
                      skip_cnt=2,rd_cnt=n,x=x,y=y )
      y(1:n) = y(1:n) * 1.e-20
      
      CALL add_pnts_inter2(x,y,yg1,kdata,n, &
                           nw,wl,xsqy_tab(j)%label,deltax,(/0.,0./))

! quantum yields 

      n = 12
      CALL base_read( filespec='DATAJ1/YLD/I2.qy',skip_cnt=4,rd_cnt=n,x=x,y=y )
      
      CALL add_pnts_inter2(x,y,yg2,kdata,n,nw,wl,xsqy_tab(j)%label,deltax,(/1.,0./))

      END SUBROUTINE readit

      END SUBROUTINE r146

      SUBROUTINE add_pnts_inter2(xin,yin,yout,kdata,n,nw,wl,jlabel,deltax,yends)

      integer, intent(in) :: kdata
      integer, intent(in) :: n
      integer, intent(in) :: nw
      real, intent(in)    :: deltax
      real, intent(in)    :: wl(nw)
      real, intent(in)    :: xin(kdata)
      real, intent(in)    :: yin(kdata)
      real, intent(in)    :: yends(2)
      real, intent(inout) :: yout(kdata)
      character(len=*), intent(in) :: jlabel

      integer :: ierr, m
      real    :: xwrk(kdata), ywrk(kdata)
      character(len=256) :: emsg

      m = n 
      xwrk(1:n) = xin(1:n)
      ywrk(1:n) = yin(1:n)
      CALL addpnt(xwrk,ywrk,kdata,m,xin(1)*(1.-deltax),yends(1))
      CALL addpnt(xwrk,ywrk,kdata,m,              0.,yends(1))
      CALL addpnt(xwrk,ywrk,kdata,m,xin(n)*(1.+deltax),yends(2))
      CALL addpnt(xwrk,ywrk,kdata,m,          1.e+38,yends(2))

      CALL inter2(nw,wl,yout,m,xwrk,ywrk,ierr)

      IF (ierr /= 0) THEN
         write(emsg,'(''add_pnts_inter2: Error '',i5,'' in inter2 for '',a)') ierr,trim(jlabel)
         call wrf_error_fatal( trim(emsg) )
      ENDIF

      END SUBROUTINE add_pnts_inter2

      SUBROUTINE base_read( filespec, skip_cnt, rd_cnt,x, y, y1, y2, y3, y4, y5 )

      integer, optional, intent(in) :: skip_cnt
      integer, intent(inout)        :: rd_cnt
      real, intent(inout)           :: x(:), y(:)
      real, optional, intent(inout) :: y1(:), y2(:), y3(:)
      real, optional, intent(inout) :: y4(:), y5(:)
      character(len=*), intent(in)  :: filespec

      integer :: i, idum
      integer :: y_to_rd
      integer :: ios, err_cnt
      character(len=256) :: emsg

      y_to_rd = 1
      if( present(y5) ) y_to_rd = y_to_rd + 1
      if( present(y4) ) y_to_rd = y_to_rd + 1
      if( present(y3) ) y_to_rd = y_to_rd + 1
      if( present(y2) ) y_to_rd = y_to_rd + 1
      if( present(y1) ) y_to_rd = y_to_rd + 1

      OPEN(UNIT=kin,FILE=trim(filespec),STATUS='old',IOSTAT=ios)
      IF( ios /= 0 ) then
        write(emsg,'(''base_read: failed to open '',a)') trim(filespec)
        call wrf_error_fatal( trim(emsg) )
      ENDIF

      if( present(skip_cnt) ) then
        DO i = 1, skip_cnt
          READ(kin,*,IOSTAT=ios)
          IF( ios /= 0 ) exit
        END DO
      else
        READ(kin,*,IOSTAT=ios) idum,rd_cnt
        IF( ios == 0 ) then
          DO i = 1, idum-2
            READ(kin,*,IOSTAT=ios)
            IF( ios /= 0 ) exit
          ENDDO
        ENDIF
      endif

      IF( ios /= 0 ) then
        write(emsg,'(''base_read: failed to read '',a)') trim(filespec)
        call wrf_error_fatal( trim(emsg) )
      ENDIF

      select case( y_to_rd )
        case( 1 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i)
            IF( ios /= 0 ) exit
          END DO
        case( 2 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i), y1(i)
            IF( ios /= 0 ) exit
          END DO
        case( 3 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i), y1(i), y2(i)
            IF( ios /= 0 ) exit
          END DO
        case( 4 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i), y1(i), y2(i), y3(i)
            IF( ios /= 0 ) exit
          END DO
        case( 5 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i), y1(i), y2(i), y3(i),y4(i)
            IF( ios /= 0 ) exit
          END DO
        case( 6 )
          DO i = 1, rd_cnt
            READ(kin,*,IOSTAT=ios) x(i), y(i), y1(i), y2(i), y3(i),y4(i),y5(i)
            IF( ios /= 0 ) exit
          END DO
      end select

      CLOSE (kin)

      IF( ios /= 0 ) then
        write(emsg,'(''base_read: failed to read '',a)') trim(filespec)
        call wrf_error_fatal( trim(emsg) )
      ENDIF

      END SUBROUTINE base_read

      SUBROUTINE fo3qy2(nz, w, t, qyld)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
! function to calculate the quantum yield O3 + hv -> O(1D) + O2,             =*
! according to:                                                             
! Matsumi, Y., F. J. Comes, G. Hancock, A. Hofzumanhays, A. J. Hynes,
! M. Kawasaki, and A. R. Ravishankara, QUantum yields for production of O(1D)
! in the ultraviolet photolysis of ozone:  Recommendation based on evaluation
! of laboratory data, J. Geophys. Res., 107, 10.1029/2001JD000510, 2002.
!-----------------------------------------------------------------------------*

      INTEGER, intent(in) :: nz
      REAL, intent(in)    :: w
      REAL, intent(in)    :: t(:)
      REAL, intent(inout) :: qyld(:)

      REAL, parameter :: A(3)  = (/ 0.8036, 8.9061, 0.1192/)
      REAL, parameter :: X(3)  = (/ 304.225, 314.957, 310.737/)
      REAL, parameter :: om(3) = (/ 5.576, 6.601, 2.187/)

      REAL, parameter :: q1 = 1.

      REAL :: kt(nz)
      REAL :: q2(nz), qdiv(nz)

      
      kT(1:nz) = 0.695 * t(1:nz)
      q2(1:nz) = exp(-825.518/kT(1:nz))

      kT(1:nz) = t(1:nz)/300.
      qdiv(1:nz) = 1/(q1 + q2(1:nz))
      
      IF(w .LE. 305.) THEN
        qyld(1:nz) = 0.90
      ELSEIF(w .GT. 305. .AND. w .LE. 328.) THEN
        qyld(1:nz) = 0.0765 + a(1)*q1*qdiv(1:nz)*EXP(-((x(1) - w)/om(1))**4) &
                   + kT(1:nz)*(a(2)*kT(1:nz)*q2*qdiv(1:nz)*EXP(-((x(2) - w)/om(2))**2) &
                               + a(3)*sqrt(kT(1:nz))*EXP(-((x(3) - w)/om(3))**2))
      ELSEIF(w .GT. 328. .AND. w .LE. 340.) THEN
         qyld(1:nz) = 0.08
      ELSEIF(w .GT. 340.) THEN
         qyld(1:nz) = 0.
      ENDIF

      END SUBROUTINE fo3qy2

      SUBROUTINE qyacet(nz, w, T, M, fac)
! This file contains subroutines used for calculation of quantum yields for 
! various photoreactions:
!     qyacet - q.y. for acetone, based on Blitz et al. (2004)

! Compute acetone quantum yields according to the parameterization of:
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
!       (2004), Pressure and temperature-dependent quantum yields for the 
!       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.

      IMPLICIT NONE

! input:
! w = wavelength, nm
! T = temperature, K
! m = air number density, molec. cm-3

      INTEGER, intent(in) :: nz
      REAL, intent(in)    :: w
      REAL, intent(in)    :: T(:), M(:)
      REAL, intent(inout) :: fac(:)

! internal:

      REAL :: wfac
      REAL :: a0(nz), a1(nz), a2(nz), a3(nz), a4(nz)
      REAL :: b0(nz), b1(nz), b2(nz), b3(nz), b4(nz)
      REAL :: c3(nz)
      REAL :: cA0(nz), cA1(nz), cA2(nz), cA3(nz), cA4(nz)
      real :: dumexp(nz)

! fac = quantum yield for product CH3CO (acetyl radical)

      REAL :: fco(nz)
      REAL :: tfac(nz)

!** set out-of-range values:
! use low pressure limits for shorter wavelengths
! set to zero beyound 327.5

      IF(w .LT. 279.) THEN
        fac(1:nz) = 0.95
      ELSEIF(w .GT. 327.) THEN
        fac(1:nz) = 0.
      ELSE
        wfac = 1.e7/w
!** CO (carbon monoxide) quantum yields:
        tfac(1:nz) = t(1:nz)/295.
        a0(1:nz) = 0.350 * tfac(1:nz)**(-1.28)
        b0(1:nz) = 0.068 * tfac(1:nz)**(-2.65)
!*SM: prevent exponent overflow in rare cases:

        dumexp(1:nz) = b0(1:nz)*(w - 248.)
        where( dumexp(1:nz) > 80. )
          cA0(1:nz) = 5.e34
        elsewhere
          cA0(1:nz) = exp(dumexp(1:nz)) * a0(1:nz) / (1. - a0(1:nz))
        endwhere

        fco(1:nz) = 1. / (1. + cA0(1:nz))

!** CH3CO (acetyl radical) quantum yields:

        IF(w >= 279. .AND. w < 302.) THEN
          a1(1:nz) = 1.600E-19 * tfac(1:nz)**(-2.38)
          b1(1:nz) = 0.55E-3   * tfac(1:nz)**(-3.19)
          cA1(1:nz) = a1(1:nz) * EXP(-b1(1:nz)*(wfac - 33113.))
          fac(1:nz) = (1. - fco(1:nz)) / (1. + cA1(1:nz) * M(1:nz))
        ELSEIF(w >= 302. .AND. w <= 327.) THEN
         a2(1:nz) = 1.62E-17 * tfac(1:nz)**(-10.03)
         b2(1:nz) = 1.79E-3  * tfac(1:nz)**(-1.364)
         cA2(1:nz) = a2(1:nz) * EXP(-b2(1:nz)*(wfac - 30488.))

         a3(1:nz) = 26.29   * tfac(1:nz)**(-6.59)
         b3(1:nz) = 5.72E-7 * tfac(1:nz)**(-2.93)
         c3(1:nz) = 30006.  * tfac(1:nz)**(-0.064)
         ca3(1:nz) = a3(1:nz) * EXP(-b3(1:nz)*((1.e7/w) - c3(1:nz))**2)

         a4(1:nz) = 1.67E-15 * tfac(1:nz)**(-7.25)
         b4(1:nz) = 2.08E-3  * tfac(1:nz)**(-1.16)
         cA4(1:nz) = a4(1:nz) * EXP(-b4(1:nz)*(wfac - 30488.))

         fac(1:nz) = (1. - fco(1:nz)) * (1. + cA3(1:nz) + cA4(1:nz) * M(1:nz)) &
                     / ((1. + cA3(1:nz) + cA2(1:nz) * M(1:nz)) * (1. + cA4(1:nz) * M(1:nz)))
        ENDIF
      ENDIF

      END SUBROUTINE qyacet

      SUBROUTINE diagnostics

      integer :: m, n, n1

      open( unit=44,file='TUV.diags')

      write(44,*) 'Photolysis diags'
      write(44,*) ' '
      write(44,'(i3,'' Total photorates'')') npht_tab
      write(44,*) ' '
      do m = 2,npht_tab
        write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
      enddo
      write(44,*) ' '
      write(44,'(''Wrf labels'')')
      write(44,*) ' '
      do m = 2,npht_tab
        write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%wrf_label)
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with no p,temp dependence'')') &
              count(xsqy_tab(2:npht_tab)%tpflag == 0)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%tpflag == 0 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with temp dependence'')') &
              count(xsqy_tab(2:npht_tab)%tpflag == 1)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%tpflag == 1 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with press dependence'')') &
              count(xsqy_tab(2:npht_tab)%tpflag == 2)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%tpflag == 2 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with temp,press dependence'')') &
              count(xsqy_tab(2:npht_tab)%tpflag == 3)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%tpflag == 3 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with second channel'')') &
              count(xsqy_tab(2:npht_tab)%channel == 2)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%channel == 2 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with third channel'')') &
              count(xsqy_tab(2:npht_tab)%channel == 3)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%channel == 3 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'(i3,'' Photorate(s) with multiple input files'')') &
              count(xsqy_tab(2:npht_tab)%filespec%nfiles > 1)
      write(44,*) ' '
      do m = 2,npht_tab
        if( xsqy_tab(m)%filespec%nfiles > 1 ) then
          write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
        endif
      enddo

      write(44,*) ' '
      write(44,'('' Photorate(s) with skip == -1'')')
      write(44,*) ' '
      do m = 2,npht_tab
        n = xsqy_tab(m)%filespec%nfiles
        do n1 = 1,n
          if( xsqy_tab(m)%filespec%nskip(n1)  == -1 ) then
            write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
          endif
        enddo
      enddo

      write(44,*) ' '
      write(44,'('' Photorate(s) with skip >= 0'')')
      write(44,*) ' '
      do m = 2,npht_tab
        n = xsqy_tab(m)%filespec%nfiles
        do n1 = 1,n
          if( xsqy_tab(m)%filespec%nskip(n1) >= 0 .and. &
              xsqy_tab(m)%filespec%filename(n1) /= ' ' ) then
            write(44,'(i3,2x,a)') m,trim(xsqy_tab(m)%label)
          endif
        enddo
      enddo

      write(44,*) ' '
      write(44,'('' Photorate(s) with xfac /= 1.e-20'')')
      write(44,*) ' '
      do m = 2,npht_tab
        n = xsqy_tab(m)%filespec%nfiles
        do n1 = 1,n
          if( xsqy_tab(m)%filespec%xfac(n1) /= 1.e-20 ) then
            write(44,'(i3,2x,a,1pg15.7)') &
              m,trim(xsqy_tab(m)%label),xsqy_tab(m)%filespec%xfac(n1)
          endif
        enddo
      enddo

      write(44,*) ' '
      write(44,'('' Filenames'')')
      write(44,*) ' '
      do m = 2,npht_tab
        n = xsqy_tab(m)%filespec%nfiles
        do n1 = 1,n
          if( xsqy_tab(m)%filespec%filename(n1) /= ' ' ) then
            write(44,'(i3,2x,a,3x,i4,3x,i4)') &
               m,trim(xsqy_tab(m)%filespec%filename(n1)), &
               xsqy_tab(m)%filespec%nskip(n1), &
               xsqy_tab(m)%filespec%nread(n1)
          endif
        enddo
      enddo

      close( 44 )

      END SUBROUTINE diagnostics

      INTEGER FUNCTION get_xsqy_tab_ndx( jlabel,wrf_label )

      character(len=*), optional, intent(in) :: jlabel
      character(len=*), optional, intent(in) :: wrf_label

      integer :: m

      get_xsqy_tab_ndx = -1

      if( present(jlabel) ) then
        do m = 2,npht_tab
          if( trim(jlabel) == trim(xsqy_tab(m)%label) ) then
            get_xsqy_tab_ndx = m
            exit
          endif
        enddo
      elseif( present(wrf_label) ) then
        do m = 2,npht_tab
          if( trim(wrf_label) == trim(xsqy_tab(m)%wrf_label) ) then
            get_xsqy_tab_ndx = m
            exit
          endif
        enddo
      endif


      END FUNCTION get_xsqy_tab_ndx

      SUBROUTINE check_alloc( ndx, nz, nw )

      integer, intent(in) :: ndx
      integer, intent(in) :: nz
      integer, intent(in) :: nw

      integer :: astat
      character(len=256)  :: emsg

      if( .not. allocated(xsqy_tab(ndx)%sq) ) then
        allocate( xsqy_tab(ndx)%sq(nz,nw),stat=astat )
      elseif( size(xsqy_tab(ndx)%sq,dim=1) /= nz ) then
        deallocate( xsqy_tab(ndx)%sq )
        allocate( xsqy_tab(ndx)%sq(nz,nw),stat=astat )
      else
        astat = 0
      endif

      if( astat /= 0 ) then
         write(emsg,'(''check_alloc: failed to alloc sq; error = '',i4)') astat
         call wrf_error_fatal( trim(emsg) )
      endif

      END SUBROUTINE check_alloc

      end module module_rxn
