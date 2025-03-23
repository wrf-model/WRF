Module module_RT_data
   use module_rt_inc, only: rt_field
   implicit none

   integer, parameter :: max_domain=5
   ! define Routing data
   type ( rt_field ), dimension (max_domain) :: RT_DOMAIN
   save RT_DOMAIN
   integer :: cur_did
end module module_RT_data
