Module module_GW_baseflow_data
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

#include "gw_field_include.inc"
      type (gw_field) :: gw2d(max_domain)
      save gw2d

end module module_GW_baseflow_data
