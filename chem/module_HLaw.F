
   module module_HLawConst

   implicit none

   private

   integer :: nHLC

   type HLCnst_type
     real              :: mw
     real              :: hcnst(6)
     character(len=64) :: name
   end type HLCnst_type

   type(HLCnst_type), allocatable :: HLC(:)

   public :: init_HLawConst
   public :: HLCnst_type
   public :: HLC, nHLC

   CONTAINS

   subroutine init_HLawConst( dm )

   integer, intent(in) :: dm                  ! domain index

   integer :: m, n, unitno
   integer :: nNull
   integer :: astat, istat
   integer :: LawType
   real    :: inMw
   real    :: inHeff(6)
   character(len=64)  :: inName
   character(len=256) :: inlin
   character(len=256) :: emsg

   integer, external :: get_unused_unit

top_level_domain: &
   if( dm == 1 .and. .not. allocated(HLC) ) then
     unitno = get_unused_unit()
     if( unitno <= 0 ) then
       call wrf_error_fatal( 'init_HLConst: Failed to get Fortran I/O unit number' )
     endif
     open(unit=unitno,file='HLC.TBL',status='OLD',iostat=istat)
     if( istat /= 0 ) then
       write(emsg,'(''init_HLConst: Failed to open HLC.TBL; error = '',i8)') istat
       call wrf_error_fatal( trim(emsg) )
     endif

     nHLC = 0
     do
       read(unitno,*,iostat=istat) inlin
       if( istat /= 0 ) then
         exit
       else
         nHLC = nHLC + 1
       endif
     end do

     write(emsg,'(''Read '',i4,'' Henrys Law entries'')') nHLC
     call wrf_debug( 0,trim(emsg) )

     if( nHLC > 0 ) then
       allocate( HLC(nHLC),stat=astat )
       if( astat /= 0 ) then 
         write(emsg,'(''init_HLawConst: Failed to allocate HLC; error = '',i8)') astat
         call wrf_error_fatal( trim(emsg) )
       endif
       rewind(unit=unitno)
       nNull = 0 ; m = 0
       do n = 1,nHLC
         read(unitno,*,iostat=istat) inName,LawType,inMw,inHeff
         if( istat /= 0 ) then
           write(emsg,'(''init_HLawConst: Failed to read line '',i3,''; error = '',i8)') n,istat
           call wrf_error_fatal( trim(emsg) )
         else
           if( all( inHeff == 0. ) ) then
             nNull = nNull + 1
             cycle
           else
             m = m + 1
             HLC(m)%name = inName ; HLC(m)%mw = inMw ; HLC(m)%hcnst(:) = inHeff(:)
           endif
         endif
       end do

       write(emsg,'(''There are '',i4,'' Henrys Law null entries'')') nNull
       call wrf_debug( 0,trim(emsg) )
       nHLC = nHLC - nNull
       call wrf_debug( 0, ' ' )
       call wrf_debug( 0, 'HLaw table ' )
       do n = 1,nHLC
         write(emsg,'(''('',i3.3,'')'',a16,1pg15.7,3x,6g15.7)') &
            n,trim(HLC(n)%name),HLC(n)%mw,HLC(n)%hcnst(:)
         call wrf_debug( 0, trim(emsg) )
       end do
     endif

     close(unit=unitno)

   endif top_level_domain

   end subroutine init_HLawConst

   end module module_HLawConst
