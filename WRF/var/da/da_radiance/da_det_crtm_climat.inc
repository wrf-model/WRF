subroutine da_det_crtm_climat (lat, month, crtm_climat)

! determine the CRTM climatology model according to the 
! latitude and month
! CRTM climatology model is categorized in CRTM_Atmosphere_Define.f90 as
! INTEGER, PARAMETER :: INVALID_MODEL          = 0
! INTEGER, PARAMETER :: TROPICAL               = 1
! INTEGER, PARAMETER :: MIDLATITUDE_SUMMER     = 2
! INTEGER, PARAMETER :: MIDLATITUDE_WINTER     = 3
! INTEGER, PARAMETER :: SUBARCTIC_SUMMER       = 4
! INTEGER, PARAMETER :: SUBARCTIC_WINTER       = 5
! INTEGER, PARAMETER :: US_STANDARD_ATMOSPHERE = 6

   implicit none

   real,    intent(in)  :: lat
   integer, intent(in)  :: month
   integer, intent(out) :: crtm_climat

   crtm_climat = 6  ! initialized to be us_standard_atmosphere

   ! 1: Tropical           (-23.4378 ~ 23.4378)
   if ( lat >= -23.4378 .and. lat <= 23.4378  ) crtm_climat = 1

   ! 2: Midlatitude summer (23.4378 ~ 66.561)
   ! 3: Midlatitude winter
   if ( lat > 23.4378 .and. lat <= 66.561  ) then ! North Mid-Lat
      if ( month >= 3 .and. month <= 8 ) then     
         crtm_climat = 2  ! Summer
      else 
         crtm_climat = 3  ! Winter
      end if
   end if

   if ( lat < -23.4378 .and. lat >= -66.561  ) then ! South Mid-Lat
      if ( month >= 3 .and. month <= 8 ) then
         crtm_climat = 3  ! Winter
      else
         crtm_climat = 2  ! Summer
      end if
   end if

   ! 4: Subarctic summer   ( > 66.561)
   ! 5: Subarctic winter
   if ( lat > 66.561  ) then ! Subarctic
      if ( month >= 3 .and. month <= 8 ) then
         crtm_climat = 4  ! Summer
      else
         crtm_climat = 5  ! Winter
      end if
   end if

   if ( lat < -66.561  ) then ! Subantarctic
      if ( month >= 3 .and. month <= 8 ) then
         crtm_climat = 5  ! Winter
      else
         crtm_climat = 4  ! Summer
      end if
   end if

end subroutine da_det_crtm_climat
