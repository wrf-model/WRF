module module_version

implicit none

character (len=512), parameter :: wrfHydroVersion=WRF_HYDRO_VERSION
character (len=512), parameter :: nwmVersion=NWM_VERSION

private :: wrfHydroVersion, nwmVersion
public  :: get_model_version, get_code_version, get_nwm_version

contains

function get_model_version()
character (len=512) :: get_model_version

#ifdef NCEP_WCOSS
   get_model_version="NWM " // NWM_VERSION
#else
   get_model_version="WRF-Hydro " // WRF_HYDRO_VERSION
#endif

end function get_model_version

function get_code_version()
character (len=512) :: get_code_version

get_code_version=WRF_HYDRO_VERSION

end function get_code_version

function get_nwm_version()
character (len=512) :: get_nwm_version

get_nwm_version=NWM_VERSION

end function get_nwm_version

end module module_version
