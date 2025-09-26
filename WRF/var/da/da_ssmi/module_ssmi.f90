module module_ssmi

use da_control, only : t_kelvin,pi, t_roughem

contains

#include "cal_sigma_v.inc"
#include "epsalt.inc"
#include "spemiss.inc"
#include "tb.inc"
#include "tbatmos.inc"
#include "effht.inc"
#include "effang.inc"
#include "roughem.inc"
#include "read_ssmi.inc"
   
end module module_ssmi

