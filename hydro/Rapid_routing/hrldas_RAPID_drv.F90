program main_program
  use hrldas_RAPID_wrapper , only : hrldas_RAPID_ini,hrldas_RAPID_exe
  implicit none

  integer, parameter :: ii = 224
  integer, parameter :: jj = 242
  real,dimension(ii,jj) :: runoff
  integer ITIME, NTIME
!  character(len=100) :: Qout_nc_file = './RAPID.with.WRF_hydro.0000.nc'

  call hrldas_RAPID_ini(NTIME)

  do ITIME=1,NTIME
    call hrldas_RAPID_exe(runoff,ii,jj)
  end do  
! end loop for calling RAPID programs
   
  end 
