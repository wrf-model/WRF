!*******************************************************************************
!Subroutine - rapid_create_Qout_file
!*******************************************************************************
subroutine rapid_create_Qout_file(Qout_file) 

!Purpose:
!Create Qout_file from Fortran/netCDF.
!Author: 
!Cedric H. David, 2013-2015.


!*******************************************************************************
!Global variables
!*******************************************************************************
use netcdf
use rapid_var, only :                                                          &
                   rank,                                                       &
                   IS_nc_status,IS_nc_id_fil_Qout,                             &
                   IS_nc_id_dim_time,IS_nc_id_dim_comid,IV_nc_id_dim,          &
                   IS_nc_id_var_Qout,IS_nc_id_var_comid,                       &
                   IV_riv_bas_id,IS_riv_bas
implicit none


!*******************************************************************************
!Includes
!*******************************************************************************


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************
character(len=100), intent(in):: Qout_file


!*******************************************************************************
!Open file
!*******************************************************************************
if (rank==0) then 

     IS_nc_status=NF90_CREATE(Qout_file,NF90_CLOBBER,IS_nc_id_fil_Qout)
     IS_nc_status=NF90_DEF_DIM(IS_nc_id_fil_Qout,'Time',NF90_UNLIMITED,        &
                               IS_nc_id_dim_time)
     IS_nc_status=NF90_DEF_DIM(IS_nc_id_fil_Qout,'COMID',IS_riv_bas,           &
                               IS_nc_id_dim_comid)
     IS_nc_status=NF90_DEF_VAR(IS_nc_id_fil_Qout,'COMID',NF90_INT,             &
                               IS_nc_id_dim_comid,IS_nc_id_var_comid)
     IV_nc_id_dim(1)=IS_nc_id_dim_comid
     IV_nc_id_dim(2)=IS_nc_id_dim_time
     IS_nc_status=NF90_DEF_VAR(IS_nc_id_fil_Qout,'Qout',NF90_REAL,             &
                               IV_nc_id_dim,IS_nc_id_var_Qout)
     IS_nc_status=NF90_ENDDEF(IS_nc_id_fil_Qout)
     IS_nc_status=NF90_PUT_VAR(IS_nc_id_fil_Qout,IS_nc_id_var_comid,           &
                               IV_riv_bas_id)
     IS_nc_status=NF90_CLOSE(IS_nc_id_fil_Qout)

end if


!*******************************************************************************
!End 
!*******************************************************************************

end subroutine rapid_create_Qout_file

