module ccpp_kind_types
#if ( RWORDSIZE == 4 )
   integer, parameter :: kind_phys = selected_real_kind(6)
#else
   integer, parameter :: kind_phys = selected_real_kind(12)
#endif
   contains
end module ccpp_kind_types
