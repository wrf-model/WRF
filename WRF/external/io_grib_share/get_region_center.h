#ifndef CRAY
# ifdef NOUNDERSCORE
#      define GET_REGION_CENTER get_region_center
#      define GET_LL_LATLON get_ll_latlon
# else
#   ifdef F2CSTYLE
#      define GET_REGION_CENTER get_region_center__
#      define GET_LL_LATLON get_ll_latlon__
#   else
#      define GET_REGION_CENTER get_region_center_
#      define GET_LL_LATLON get_ll_latlon_
#   endif
# endif
#endif
