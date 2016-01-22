module da_module_couple_uv_ad

   ! update_bc

use da_module_couple_uv, only: da_calc_mu_uv

contains

#include "da_couple_uv_ad.inc"
#include "da_calc_mu_uv_ad.inc"
#include "da_couple_ad.inc"

end module da_module_couple_uv_ad
