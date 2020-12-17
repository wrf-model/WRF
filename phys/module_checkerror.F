!Modified to use wrf error routines by Eric Kemp, 2 Aug 2011
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 module module_checkerror
 implicit none

!--------------------------------------------------------------------------------------------------
!              = Goddard Satellite Data Simulator Unit =
!
! NASA GSFC makes no representations about the suitability of software for any purpose. 
! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
! government) nor Principal Developers (their organizations) shall be liable for any 
! damages suffered by the user of this software. In addition, please do not distribute 
! the software to third party.
!
! Comments: 
!  This routine checks the presence of anomalous physical value.
!
! 1. How to use 
!
! You can call subroutine checkerror like this. 
!
!                 !    input               input         input     input   input      input/output 
! call checkerror( "subroutine name', 'parameter name', i_index, j_index, k_index, float_array(i,k,j) )  
!
! a) 'subroutine_name' must be the subroutine name that call check_error. 
! 
! 
! b) 'parameter name' can be must be following characters:  
!
!   'temperature_K'       This is temperature in Kelvin.
!   'temperature_degC'    This is temperature in degree Celcius.
!   'pressure_Pa'         This is pressure in Pascal.
!   'radiationflux_W/m2'  This is radiation flux in Watts per square meter.
!   'condensate_g/m3'     This is cloud rain condensates in gram per cubic meter.
!   'condensate_kg/kg'    This is cloud rain condensate in mixing ratio. 
!   'aerosol_g/m3'        This is aerosol in gram per cubic meter. 
!   'aerosol_ug/kg'       This is aerosol in mixing ratio. 
!   'albedo'              This is surface albedo.
!   'emissivity'          This is surface emissivity 
!
! c) float_array can be either single or double precision. But it must be zero dimension for interface. 
!
!
! History:
! 02/2010  Toshi Matsui@NASA GSFC ; Initial
!
!----------------------------------------------------------------------------------------------

!
! Encapsulation control 
!
 private             ! All parameters and subourtines are non accessible.
 public  checkerror  ! Only this function is accesible.

! ################################################################################
! ############################    Module Interface     ###########################
! ################################################################################

  interface checkerror
    module procedure checkerror_single
    module procedure checkerror_double
!    module procedure checkerror_integer
  end interface

 contains

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine checkerror_single(subroutine_name, param_id,i,k,j,input_real)
#ifndef NO_IEEE_MODULE
 use, intrinsic :: ieee_arithmetic
#endif
 implicit none
 character*(*),intent(in) :: subroutine_name
 character*(*),intent(in) :: param_id
 integer,intent(in) :: i,k,j  ! array index
 real(4),intent(in) :: input_real

 character(len=132) :: string

 select case(trim(param_id))
 case('temperature_K')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('temperature_degC')

  if(input_real < -274. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('pressure_Pa')

  if(input_real < 0. .or. input_real > 200000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('radiationflux_W/m2')

  if(input_real < -10000. .or. input_real > 10000. ) then  
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('condensate_g/m3')

  if(input_real < 0. .or. input_real > 10000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('condensate_kg/kg')

  if(input_real < 0. .or. input_real > 10000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.') 
  endif

 case('aerosol_g/m3')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('aerosol_ug/kg')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('albedo')

  if(input_real < 0. .or. input_real > 1. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('emissivity')

  if(input_real < 0. .or. input_real > 1. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif


 case default
!      print*,'MSG checkeror_float: There is no such param_id',trim(param_id)
!      stop
      write(string,*) 'MSG checkerror_float: There is no such param_id',trim(param_id)
      call wrf_message(string)
      call wrf_error_fatal('Terminate run.')
 end select

! EMK...Check for infinity and NaNs
 if (abs(input_real) >= huge(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' Infinity at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal('Terminate run.')
 end if

#ifndef NO_IEEE_MODULE
 if (ieee_is_nan(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' NaN at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal('Terminate run.')
 end if
#endif

 end subroutine checkerror_single

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 subroutine checkerror_double(subroutine_name, param_id,i,k,j,input_real)
#ifndef NO_IEEE_MODULE
 use, intrinsic :: ieee_arithmetic
#endif
 implicit none
 character*(*),intent(in) :: subroutine_name
 character*(*),intent(in) :: param_id
 integer,intent(in) :: i,k,j  ! array index
 real(8),intent(in) :: input_real

 character(len=132) :: string

 select case(trim(param_id))
 case('temperature_K')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('temperature_degC')

  if(input_real < -274. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('pressure_Pa')

  if(input_real < 0. .or. input_real > 200000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('radiationflux_W/m2')

  if(input_real < -10000. .or. input_real > 10000. ) then  
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')

  endif

 case('condensate_g/m3')

  if(input_real < 0. .or. input_real > 10000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('condensate_kg/kg')

  if(input_real < 0. .or. input_real > 10000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('aerosol_g/m3')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('aerosol_ug/kg')

  if(input_real < 0. .or. input_real > 1000. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('albedo')

  if(input_real < 0. .or. input_real > 1. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case('emissivity')

  if(input_real < 0. .or. input_real > 1. ) then
!       print*, 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
!               ' out of range at grid(i,k,j) =',i,k,j
!       stop 'Terminate run.'
       write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' out of range at grid(i,k,j) =',i,k,j
       call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
  endif

 case default
!      print*,'MSG checkerror_double: There is no such param_id',trim(param_id)
!      stop
      write(string,*) 'MSG checkerror_double: There is no such param_id',trim(param_id)
      call wrf_message(string)
       call wrf_error_fatal('Terminate run.')
 end select

! EMK...Check for infinity and NaNs
 if (abs(input_real) >= huge(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' Infinity at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal('Terminate run.')
 end if

#ifndef NO_IEEE_MODULE
 if (ieee_is_nan(input_real)) then
    write(string,*) 'MSG '//trim(subroutine_name)//': '//trim(param_id)//' =',input_real,&
               ' NaN at grid(i,k,j) =',i,k,j
    call wrf_message(string)
    call wrf_error_fatal('Terminate run.')
 end if
#endif

 end subroutine checkerror_double

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 

 end module module_checkerror

!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
!SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU SDSU 
