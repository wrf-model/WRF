subroutine da_ffdduv_model (F,D,U,V,ID)

   !-------------------------------------------------------------------------
   ! Purpose: TBD

   ! When ID =  1
   ! Convert wind speed (F in m/s) and direction (D in degree 0-360) into
   ! wind (U-V in m/s) components on grid coordinates
   !
   ! When ID = -1
   ! Convert wind (U-V in m/s) components into wind speed (F in m/s) and 
   ! direction (D in degree 0-360) on grid coordinates 
   !
   !-------------------------------------------------------------------------

   implicit none

   real,    intent (inout) :: F,D
   real,    intent (inout) :: U,V
   integer, intent (in)    :: ID
   real                    :: CONV,A

   if (trace_use_frequent) call da_trace_entry("da_ffdduv_model")

   CONV = 180.0 / pi

   select case (ID)

      case (convert_fd2uv);

         U = -F*SIN(D/CONV)
         V = -F*COS(D/CONV)

      case (convert_uv2fd);

         F = sqrt(U*U + V*V)

         if (F .EQ. 0.0) then
            D = 0.0
            if (trace_use_frequent) call da_trace_exit("da_ffdduv_model")
            return
         end if

         if (V .EQ. 0.0) then
            if (U .GT. 0.0) D = 270.0
            if (U .LT. 0.0) D =  90.0
         else
            A = ATAN (U/V)*CONV

            if (U .LE. 0.0 .AND. V .LE. 0.0) D = A
            if (U .LE. 0.0 .AND. V .GE. 0.0) D = A + 180.0
            if (U .GE. 0.0 .AND. V .GE. 0.0) D = A + 180.0
            if (U .GE. 0.0 .AND. V .LE. 0.0) D = A + 360.0

         end if

      case default
         write(unit=message(1),fmt='(A,I2)') ' UNKNOWN OPTION ',ID
         call da_error(__FILE__,__LINE__,message(1:1))

   end select

   if (trace_use_frequent) call da_trace_exit("da_ffdduv_model")

end subroutine da_ffdduv_model


