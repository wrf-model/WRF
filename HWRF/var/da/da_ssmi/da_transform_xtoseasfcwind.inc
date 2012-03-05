subroutine da_transform_xtoseasfcwind(U,V,Speed,zhmkz)

   !----------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   implicit none

   real, intent (out) :: Speed
   real, intent (in)  :: U, V, zhmkz

   real :: usfc, vsfc

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind")

   usfc   = U*log(10./0.0001)/log(zhmkz/0.0001) ! roughness = 0.0001
   vsfc   = V*log(10./0.0001)/log(zhmkz/0.0001) ! roughness = 0.0001
   speed  = sqrt(usfc*usfc + vsfc*vsfc)

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind")

end subroutine da_transform_xtoseasfcwind


