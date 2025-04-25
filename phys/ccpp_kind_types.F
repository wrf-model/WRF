module ccpp_kind_types
#ifndef DOUBLE_PRECISION
   integer, parameter :: kind_phys = selected_real_kind(6)
#else
   integer, parameter :: kind_phys = selected_real_kind(12)
#endif
   contains
end module ccpp_kind_types
