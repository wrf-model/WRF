subroutine da_ffdduv (F,D,U,V,YLON,ID)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   ! When ID =  1
   ! Convert wind speed (F in m/s) and direction (D in degree 0-360) into
   ! wind (U-V in m/s) components
   !
   ! When ID = -1
   ! Convert wind (U-V in m/s) components into wind speed (F in m/s) and 
   ! direction (D in degree 0-360)
   !
   ! Need map projection parameters from module da_control
   !
   ! PHIC:  Central latitude 
   ! XLONC: Central longitude
   ! XN:    Cone projection
   ! CONV:  180/Pi
   !
   !-------------------------------------------------------------------------

   implicit none

   real,    intent (inout) :: f,d
   real,    intent (inout) :: u, v
   real,    intent (in)    :: ylon
   integer, intent (in)    :: id

   real :: aearth, uearth, vearth
   real :: xlonrt, ang, conv

   if (trace_use_frequent) call da_trace_entry("da_ffdduv")

   conv = 180.0 / pi

   select case (ID)

      case (convert_fd2uv);

         ! convert wind module/direction into u/v wind components on earth,
         ! then convert u/v wind components on earth into lambert conformal or
         ! polar stereographic projection u/v wind components.

         ! projections change requires only a change of the cone constant, xn
         ! equations remain the same.

         AEARTH = D/CONV

         UEARTH = -F*Sin(AEARTH)
         VEARTH = -F*COS(AEARTH)

         ! for conversion to grid coordinates,
         ! see program datamap, subr vect, and
         ! ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

         XLONRT = XLONC-YLON

         if (XLONRT .GT. 180.0) XLONRT=XLONRT-360.0
         if (XLONRT .LT.-180.0) XLONRT=XLONRT+360.0

         ANG=XLONRT*CONE_FACTOR/CONV

         ! for mercator projection, the winds are as in earth coordinates

         if (map_projection.EQ.3) ANG=0.0

         if (PHIC.LT.0.0) ANG=-ANG

         U = VEARTH*Sin(ANG) + UEARTH*COS(ANG)
         V = VEARTH*COS(ANG) - UEARTH*Sin(ANG)


         ! CONVERT LAMBERT CONFORMAL OR POLAR STEREOGRAPHIC PROJECTION U/V
         ! WinD COMPONENTS inTO U/V WinD COMPONENTS ON EART
         ! then CONVERT U/V WinD COMPONENTS ON EARTH inTO WinD module/DIRECTION

         ! PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE_FACTOR

      case (convert_uv2fd);

         XLONRT = XLONC-YLON

         if (XLONRT .GT. 180.0) XLONRT=XLONRT-360.0
         if (XLONRT .LT.-180.0) XLONRT=XLONRT+360.0

         ANG=XLONRT*CONE_FACTOR/CONV

         ! FOR MERCATOR PROJECTION, THE WinDS ARE AS in EARTH COORDinATES

         if (map_projection .EQ.  3) ANG = 0.0
         if (PHIC  .LT. 0.0) ANG = -ANG

         UEARTH = U*COS(ANG) - V*Sin(ANG)
         VEARTH = U*Sin(ANG) + V*COS(ANG)

         F = sqrt(UEARTH*UEARTH + VEARTH*VEARTH)

         if (F .EQ. 0.0) then
            D = 0.0
            if (trace_use_frequent) call da_trace_exit("da_ffdduv")
            return
         end if

         if (VEARTH .EQ. 0.0) then
            if (UEARTH .GT. 0.0) D = 270.0
            if (UEARTH .LT. 0.0) D =  90.0
         else
            AEARTH = ATAN (UEARTH/VEARTH)*CONV

            if (UEARTH .LE. 0.0 .AND. VEARTH .LE. 0.0) D = AEARTH
            if (UEARTH .LE. 0.0 .AND. VEARTH .GE. 0.0) D = AEARTH + 180.0
            if (UEARTH .GE. 0.0 .AND. VEARTH .GE. 0.0) D = AEARTH + 180.0
            if (UEARTH .GE. 0.0 .AND. VEARTH .LE. 0.0) D = AEARTH + 360.0

         end if

      case default
         write(unit=message(1),fmt='(A,I2)') ' UNKNOWN OPTION ',ID
         call da_error(__FILE__,__LINE__,message(1:1))

   end select

   if (trace_use_frequent) call da_trace_exit("da_ffdduv")

end subroutine da_ffdduv


