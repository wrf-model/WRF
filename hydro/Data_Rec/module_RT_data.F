Module module_RT_data
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

! define Routing data
#include "rt_include.inc"
   TYPE ( RT_FIELD ), DIMENSION (max_domain) :: RT_DOMAIN
   save RT_DOMAIN
   integer :: cur_did
end module module_RT_data
