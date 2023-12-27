subroutine da_trop_wmo (t, z, p, nlev, tropt, tropp, tropk)

   !----------------------------------------------------------------------------
   ! * Computes tropopause T, P, and/or level based on code from Cameron Homeyer
   !     and WMO definition
   !
   ! * WMO tropopause definition:
   !     The boundary between the troposphere and the stratosphere, where an 
   !     abrupt change in lapse rate usually occurs. It is defined as the lowest 
   !     level at which the lapse rate decreases to 2 °C/km or less, provided 
   !     that the average lapse rate between this level and all higher levels 
   !     within 2 km does not exceed 2 °C/km.
   !----------------------------------------------------------------------------

   implicit none 

   ! Assumed shape inputs for single column (size=nlev)
   !  ordered from bottom to top of model
   real, intent(in) :: t(:)             ! Temperature, K. (3D)

!JJG: "z" is supposed to be height, not geopotential height. Does it matter?
   real, intent(in) :: z(:)             ! Geopotential height above m.s.l., m.
   real, intent(in) :: p(:)             ! Pressure, mb.
   real, optional, intent(out) :: tropt ! Tropopause temperature, K.
   real, optional, intent(out) :: tropp ! Tropopause pressure, mb.
   integer, optional, intent(out) :: tropk
   integer, intent(in) :: nlev

   real    :: dtdz,laps  !LAPS
   integer :: dtdztest(nlev), ztest(nlev)
   integer :: i, j, k, kk, ktrop

!   real, parameter :: tropz_min = 5000.0
!   real, parameter :: tropz_max = 19000.0

   if (.not.present(tropt) .and. &
       .not.present(tropp) .and. &
       .not.present(tropk)) return

   if (present(tropt)) tropt = missing_r
   if (present(tropp)) tropp = missing_r

   !Loop over levels to find tropopause (single column)
   ktrop = nlev-1
   trop_loop: do  k = 1, nlev-1
      if ( p(k) .le. 500.0 ) then
         ! Compute lapse rate (-dT/dz)
         dtdz = ( t(k+1) - t(k)   ) / &
                ( z(k)   - z(k+1) )
      else
         ! Set lapse rate for p > 500 hPa
         dtdz = 999.9
      endif
      !Check if local lapse rate <= 2 K/km
      if (dtdz .le. 0.002) then
         ! Initialize lapse rate and altitude test arrays
         dtdztest = 0
         ztest = 0

         ! Compute average lapse rate across levels above current candidate 
         do kk = k+1, nlev-1
            dtdz = ( t(kk+1) - t(k)   ) / &
                   ( z(k)    - z(kk+1) )

            !If avg. lapse rate <= 2 K/km and z <= trop + 2 km, set pass flag
            if ( ( dtdz .le. 0.002 ) .and. &
                 ( (z(k) - z(kk)) .le. 2000. ) ) THEN
               dtdztest(kk) = 1                        
            endif

            ! If z <= trop + 2 km, set pass flag
            IF ( (z(k) - z(kk)) .le. 2000.0 ) THEN
               ztest(kk) = 1                           
            endif
         enddo   !kk loop
         laps=dtdz !LAPS
         IF (SUM(dtdztest) .eq. SUM(ztest)) THEN
            ! If qualified as tropopause, set altitude index and return value
            ktrop = k  
            exit trop_loop
         ENDIF
      ENDIF
   end do trop_loop

!   ! Filter ktrop using tpause height thresholds
!   ztest = 0
!   if ( z(ktrop) .gt. tropz_max ) then
!      where ( z.le.tropz_max )
!         ztest = 1
!      end where
!      do  k = nlev, 1, -1
!         if (ztest(k) .eq. 1) then
!            ktrop = k
!            exit
!         end if
!      end do
!   else if ( z(ktrop) .lt. tropz_min ) then
!      where ( z.ge.tropz_min )
!         ztest = 1
!      end where
!      do  k = 1, nlev
!         if (ztest(k) .eq. 1) then
!            ktrop = k
!            exit
!         end if
!      end do
!   end if
   
   if (present(tropt)) tropt = t(ktrop)
   if (present(tropp)) tropp = p(ktrop)
   if (present(tropk)) tropk = ktrop

end subroutine da_trop_wmo

