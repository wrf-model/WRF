module da_wrf_interfaces
   
   interface
      subroutine disable_quilting
      end subroutine disable_quilting
   end interface 

   interface
      subroutine wrf_dm_bcast_real (buf, n1)
         integer, intent(in) :: n1
         real, intent(inout) :: buf(*)
      end subroutine wrf_dm_bcast_real
   end interface 

   interface
      subroutine wrf_dm_sum_reals (inval, retval)
         real, intent(in)  :: inval(:)
         real, intent(out) :: retval(:)
      end subroutine wrf_dm_sum_reals
   end interface

   interface
      real function wrf_dm_sum_real (inval)
         real, intent(in) :: inval
      end function wrf_dm_sum_real
   end interface

   interface
      subroutine wrf_dm_bcast_integer(buf, n1)
         implicit none
         integer, intent(in)    ::  n1
         integer, intent(inout) :: buf(*)
      end subroutine wrf_dm_bcast_integer
   end interface

   interface
      integer function wrf_dm_sum_integer (inval)
         integer, intent(in) :: inval
      end function wrf_dm_sum_integer
   end interface

   interface
      subroutine wrf_dm_bcast_string(buf, n1)
         implicit none
         integer, intent(in)          ::  n1
         character*(*), intent(inout) :: buf(*)
      end subroutine wrf_dm_bcast_string
   end interface

   interface
      subroutine wrf_dm_bcast_bytes(buf, size)
         implicit none
         integer, intent(in)    :: size
         integer, intent(inout) :: buf(*)
      end subroutine wrf_dm_bcast_bytes
   end interface

   interface
      subroutine wrf_patch_to_global_real (buf,globbuf,domdesc,stagger, &
         ordering,&
         ds1,de1,ds2,de2,ds3,de3,&
         ms1,me1,ms2,me2,ms3,me3,&
         ps1,pe1,ps2,pe2,ps3,pe3 )

         integer, intent(in) ::         ds1,de1,ds2,de2,ds3,de3,&
                                          ms1,me1,ms2,me2,ms3,me3,&
                                          ps1,pe1,ps2,pe2,ps3,pe3
         character *(*), intent(in) :: stagger,ordering
         integer, intent(in) :: domdesc
         real, intent(inout) :: globbuf(*)
         real, intent(in) :: buf(*)
      end subroutine wrf_patch_to_global_real 
   end interface

   interface
      subroutine wrf_dm_xpose_z2y (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_z2y
   end interface

   interface
      subroutine wrf_dm_xpose_y2z (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_y2z
   end interface

   interface
      subroutine wrf_dm_xpose_y2x (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_y2x
   end interface

   interface
      subroutine wrf_dm_xpose_x2y (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_x2y
   end interface

   interface
      subroutine wrf_dm_xpose_x2z (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_x2z
   end interface

   interface
      subroutine wrf_dm_xpose_z2x (domdesc , comms , xpose_id)
         integer, intent(inout) :: domdesc
         integer, intent(inout) :: comms(*)
         integer, intent(in) :: xpose_id
      end subroutine wrf_dm_xpose_z2x
   end interface

   interface
      subroutine set_scalar_indices_from_config (idomain, dummy2, dummy1)
        integer, intent(in) :: idomain
        integer, intent(in) :: dummy1
        integer, intent(in) :: dummy2
      end subroutine set_scalar_indices_from_config
   end interface

   interface
      subroutine init_modules(phase)
         integer, intent(in) :: phase
      end subroutine init_modules
   end interface

   interface
      subroutine init_wrfio
      end subroutine init_wrfio
   end interface

   interface
      subroutine wrf_get_dm_communicator (communicator)
         integer , intent(out) :: communicator
      end subroutine wrf_get_dm_communicator
   end interface

   interface
      subroutine wrf_debug(level , str) 
         character*(*), intent(in) :: str
         integer,          intent(in) :: level 
      end subroutine wrf_debug
   end interface

   interface 
      subroutine setup_timekeeping(grid)
        use module_domain, only : domain
        type(domain), pointer :: grid
      end subroutine setup_timekeeping
   end interface

   interface
      subroutine wrf_message(str)
         character(len=*), intent(in) :: str
      end subroutine wrf_message
   end interface

   interface
      subroutine wrf_error_fatal (str)
         character*(*), intent(in) :: str
      end subroutine wrf_error_fatal
   end interface

   interface
      subroutine wrf_error_fatal3 (file_str, line, str)
        character(len=*), intent(in) :: file_str
        integer,          intent(in) :: line
        character(len=*), intent(in) :: str(:)
      end subroutine wrf_error_fatal3
   end interface

   interface
      subroutine wrf_check_error(expected, actual, str, file_str, line)
        integer,          intent(in) :: expected
        integer,          intent(in) :: actual
        character(len=*), intent(in) :: str(:)
        character(len=*), intent(in) :: file_str(:)
        integer,          intent(in) :: line
      end subroutine wrf_check_error
   end interface 

   interface
      subroutine wrf_abort
      end subroutine wrf_abort
   end interface 

   interface 
      subroutine wrf_shutdown
      end subroutine wrf_shutdown
   end interface

   interface 
      subroutine med_shutdown_io (grid , config_flags)
         use module_domain, only : domain
         use module_configure, only : grid_config_rec_type
         type (domain),               intent(in) :: grid
         type (grid_config_rec_type), intent(in) :: config_flags
      end subroutine med_shutdown_io
   end interface

end module da_wrf_interfaces
