subroutine da_read_obs_ssmi_info (iunit, ob, xb, xbx)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a SSMI GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent (in)  :: iunit
   type (xb_type), intent (in)  :: xb
   type (xbx_type),intent (in)  :: xbx     ! Header & non-gridded vars.
   type (iv_type), intent (out) :: ob


   integer                      :: iost, i, ii
   character (LEN = 120)        :: char_ned
   logical                      :: connected

   integer                      :: nssmis,nothers
   integer                      :: ixc, jxc, iproj, idd, maxnes
   integer                      :: nestix(10) , nestjx(10) , numnc(10) , nesti(10) , nestj(10) 
   real                         :: phic   , xlonc  , &
                                   truelat1_3dv, truelat2_3dv, &
                                   local_ts0    , local_ps0    , local_tlp     , ptop
   real                         :: dis(10)

   logical                      :: check_wrong, check_subdomain

   if (trace_use) call da_trace_entry("da_read_obs_ssmi_info")

   iost = -99999

   ! 1. open file
   !    ---------

   if (use_ssmiretrievalobs .or. use_ssmitbobs .or. use_ssmt1obs .or. use_ssmt2obs) then
      open (unit   = iunit,     &
         FORM   = 'FORMATTED',  &
         ACCESS = 'SEQUENTIAL', &
         iostat =  iost,     &
         STATUS = 'OLD')

      if (iost /= 0) then
         call da_warning(__FILE__,__LINE__, (/"Cannot open SSMI file"/))
         use_ssmiretrievalobs = .false.
         use_ssmitbobs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.
         return
      end if
   else
      return
   end if

   rewind (unit = iunit)


   ! 2.  read header
   ! ===============

   ! 2.1 read first line
   !     ---------------

   read (unit = iunit, fmt = '(a)', iostat = iost) char_ned
   if (iost /= 0) then
      use_ssmiretrievalobs = .false.
      use_ssmitbobs  = .false.
      use_ssmt1obs   = .false.
      use_ssmt2obs   = .false.
      call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file"/))
   end if

   ! 2.3 read NUMBER OF REPORTS
   !     ---------------------

   do
      do i = 0, 120-14
         select case (char_ned (I+1:I+5))
         ! Number of observations
         case ('SSMI ') ; 
            if (use_ssmiretrievalobs) &
               read (char_ned (I+9:I+14),'(I6)', iostat = iost) &
                  ob%nlocal(ssmi_rv)
            if (use_ssmitbobs) then
               read (char_ned (I+9:I+14),'(I6)', iostat = iost) ob%nlocal(ssmi_tb)
            end if
         case ('OTHER') ; 
            read (char_ned (I+9:I+14),'(I6)', iostat = iost) nothers

            ! Geographic area and reference atmosphere definition

         case ('MISS.') ; 
             read (char_ned (I+8:I+15),'(F8.0)', iostat = iost) ob % missing
         case ('PHIC ') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) phic
         case ('XLONC') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) xlonc
         case ('true1') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) truelat1_3dv
         case ('true2') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) truelat2_3dv
         case ('TS0  ') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) local_ts0
         case ('TLP  ') ; 
             read (char_ned (I+8:I+14),'(F7.2)', iostat = iost) local_tlp
         case ('PTOP ') ; 
             read (char_ned (I+8:I+14),'(F7.0)', iostat = iost) ptop
         case ('PS0  ') ; 
             read (char_ned (I+8:I+14),'(F7.0)', iostat = iost) local_ps0
         case ('IXC  ') ; 
             read (char_ned (I+8:I+14),'(I7)', iostat = iost) ixc
         case ('JXC  ') ; 
             read (char_ned (I+8:I+14),'(I7)', iostat = iost) jxc
         case ('IPROJ') ; 
             read (char_ned (I+8:I+14),'(I7)', iostat = iost) iproj
         case ('IDD  ') ; 
             read (char_ned (I+8:I+14),'(I7)', iostat = iost) idd
         case ('MAXNE') ; 
             read (char_ned (I+8:I+14),'(I7)', iostat = iost) maxnes
         case default ; read (char_ned (I+9:I+14),'(I6)', iostat = iost) nssmis
         end select
      end do

      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

      if (iost /= 0) then
         use_ssmiretrievalobs = .false.
         use_ssmitbobs  = .false.
         use_ssmt1obs   = .false.
         use_ssmt2obs   = .false.
         call da_warning(__FILE__,__LINE__, &
            (/"Cannot read SSMI file"/))
         return
      end if
      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
      select case (char_ned (1:6))
      ! Model domains definition

      case ('NESTIX') ;
         call da_read_obs_ssmi_integer_array(nestix, maxnes, 8, 9)
      case ('NESTJX') ; 
         call da_read_obs_ssmi_integer_array(nestjx, maxnes, 8, 9)
      case ('NUMC  ') ; 
         call da_read_obs_ssmi_integer_array(numnc , maxnes, 8, 9)
      case ('DIS   ') ; 
         call da_read_obs_ssmi_real_array   (dis   , maxnes, 8, 9)
      case ('NESTI ') ; 
         call da_read_obs_ssmi_integer_array(nesti , maxnes, 8, 9)
      case ('NESTJ ') ; 
         call da_read_obs_ssmi_integer_array(nestj , maxnes, 8, 9)
      end select

      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'INFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   if (trace_use) call da_trace_exit("da_read_obs_ssmi_info")

contains

#include "da_read_obs_ssmi_integer_array.inc"
#include "da_read_obs_ssmi_real_array.inc"

end subroutine da_read_obs_ssmi_info


