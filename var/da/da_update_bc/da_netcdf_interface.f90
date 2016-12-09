module da_netcdf_interface

integer, parameter :: stderr = 0
integer, parameter :: stdout = 6

contains

#include "da_get_times_cdf.inc"
#include "da_get_dims_cdf.inc"
#include "da_get_gl_att_int_cdf.inc"
#include "da_get_gl_att_real_cdf.inc"
#include "da_get_var_3d_real_cdf.inc"
#include "da_get_var_2d_real_cdf.inc"
#include "da_get_var_1d_real_cdf.inc"
#include "da_put_var_3d_real_cdf.inc"
#include "da_put_var_2d_real_cdf.inc"
#include "da_get_var_2d_int_cdf.inc"
#include "da_put_var_2d_int_cdf.inc"
#include "da_get_att_cdf.inc"
#include "da_put_att_cdf.inc"
#include "da_get_bdyfrq.inc"
#include "da_get_bdytimestr_cdf.inc"
#include "da_atotime.inc"

end module da_netcdf_interface

