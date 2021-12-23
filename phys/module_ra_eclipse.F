module module_ra_eclipse

contains

!--------------------------------------------
! Solar eclipse prediction moduel
!
! Alex Montornes and Bernat Codina. University of Barcelona. 2015
!
! Based on Montornes A., Codina B., Zack J., Sola, Y.: Implementation of the 
! Bessel's method for solar eclipses prediction in the WRF-ARW model. 2015
!
! This key point of this subroutine is the evaluation of the degree of obscuration,
! i.e. the part of the solar disk that is hidden by the Moon. This variable is
! introduced in the SW schemes and used as a correction of the incoming radiation.
! In other words, if obscur is the part of the solar disk hidden by the Moon, then
! (1-obscur) is the part of the solar disk emitting radiation.
!
! Note that if obscur = 0., then we recover the standard code
!
!----------------------------
! History
!
!	amontornes	2015/09		First implementation
!
!----------------------------
! MAIN ROUTINE
!
subroutine solar_eclipse(ims,ime,jms,jme,its,ite,jts,jte,    &
                          julian,gmt,year,xtime,radt,        &
                          degrad,xlon,xlat,Obscur,mask,      &
                          elat_track,elon_track,sw_eclipse   )
!-------------------------------------------------------------
  implicit none
!
!--------------
!----------
! VARIABLES
!----------
!--------------
!
!----------
! Input/output vars
!----------
! Grid indeces
  integer,                             intent(in)    :: ims,ime,jms,jme, &
                                                        its,ite,jts,jte
! Temporal variables
  real,                                intent(in)    :: gmt,degrad,julian,xtime,radt
  integer,                             intent(in)    :: year
! Geographical information
  real,    dimension(ims:ime,jms:jme), intent(in)    :: xlat,xlon
  real,    dimension(ims:ime,jms:jme), intent(inout) :: Obscur
! Eclipse variables
  integer, dimension(ims:ime,jms:jme), intent(inout) :: mask
  real,                                intent(inout) :: elat_track,elon_track
! Namelist option 1 - enabled / 0 - disabled
  integer,                             intent(in)    :: sw_eclipse

!----------
! Local vars
!----------
!
! Constants
  real,    parameter       :: pi      = 3.1415926535897932384626433
  real,    parameter       :: epsil   = 0.0818192 ! Earth excentricity
!
! Besseliam elements from file eclipse_besselian_elements.dat
!	X, Y         :: coordinates of the eclipse's axis in the fundamental plane
!       d, mu        :: declination and azimuth (deg)
!       l1, l2       :: radii of the penumbra and umbra regions in the fundamental 
!                       plane
!	tanf1, tanf2 :: angle of the shadow's conus
!       t0, min, max :: time when the Besselian elements are valid
!       Dt           :: time correction
  integer, parameter       :: NBessel = 4 ! Dimension
  real, dimension(NBessel) :: X, Y, d, l1, l2, mu
  real                     :: tanf1, tanf2, t0, tmin, tmax, Dt
!
! d and mu in radians
  real                     :: rmu, rd
!
! Besselian elements at the current time t1
  real                 :: cx, cy, cd, cl1, cl2, cmu
!
! Current time t1, and hourly angle H
  real                 :: t1, &
                           H   
!
! Latitude and longitude in radians
  real                 :: rlat, rlon
!
!
! Geocentric coordinates
!----------
  real                 :: cos_lat1, sin_lat1 ! cos/sin of the corrected latitude
  real                 :: xi, eta1, zeta1      ! Geocentric coordinates at the observation point
!
! Eclipse variables
  real                 :: DELTA, DELTA2      ! Distance of the observation point
                                             ! to the eclipse axis
  real                 :: LL1, LL2           ! Penumbra and Umbra radius
                                             ! from the point observer's plane
  logical              :: eclipse_exist
!
! Loop indeces
  integer              :: i, j
!
! Internal variables
  real                 :: A, B
  real                 :: da,eot,xt24
  integer              :: julday
!
! Eclipse path
  real :: E, E_1, tan_rd1, rd1, rho1, cy1, tan_gamma, cgamma, sin_beta, &
          tan_C, C, cm, tan_lat1, tan_lat, sin_H,                       &
          cos_H, tan_H, beta, lon, tan_rd2, rd2, rho2
!
! Missing value
  real :: MISSING


!--------------
!----------
! CODE CUSTOMIZATION
!----------
!--------------
!
  MISSING = 1E30

!--------------
!----------
! MAIN
!----------
!--------------
!
!----------
! Check if the eclipse is enabled by the namelist.input
! If eclipse is not enabled -> set eclipse variables to zero and nothing to do
  if(sw_eclipse .eq. 0) then
     Obscur(:,:) = 0.
     mask(:,:)   = 0
     elat_track  = MISSING
     elon_track  = MISSING
     return
  endif
!
!----------
! Set current time
  xt24   = mod(xtime+radt,1440.)
  t1     = gmt+xt24/60
  julday = int(julian)+1
  if(t1 .ge. 24.) then
	t1     = t1 - 24.
  endif 
!
!----------
! Check if the eclipse exists for the current simulation date
  eclipse_exist = .false.
!
! and load the besselian elements
! Note: eclipse_besselian_elements.dat should be in the running directory!
  call load_besselian_elements(t1, julday, year, X, Y, d, l1, l2, mu, & 
                               tanf1, tanf2, t0, tmin, tmax, NBessel, &
                               eclipse_exist,Dt)
!
!----------
! If the eclipse does not exist, then we set all the output vars to zero and finish
  if(.not. eclipse_exist) then
     Obscur(:,:) = 0.
     mask(:,:)   = 0
     elat_track  = MISSING
     elon_track  = MISSING
     return
  endif
!
!----------
! Compute the besselian elements at t1
  call compute_besselian_t(t1, NBessel, X, Y, d, l1, l2, mu, t0, Dt, &
                               cx, cy, cd, cl1, cl2, cmu)
!
!----------
! Convert the beselian elements from deg to radians
  rmu = cmu * degrad
  rd  = cd  * degrad

!---------------------------
! STEP 1: Compute the eclipse track. This is just a diagnosis for the user
!
!-- Initialization
  elat_track=MISSING
  elon_track=MISSING
!
!-- Compute the excentricity correction
  E         = sqrt(1 - epsil**2)
  E_1       = 1/E
!-- Get d1
  tan_rd1   = E_1 * tan(rd)
  rd1       = atan(tan_rd1)
!-- Get rho1
  rho1      = sin(rd)/sin(rd1)
!-- Correct cy1
  cy1       = cy / rho1
!-- Get d2
  tan_rd2   = E * tan(rd)
  rd2       = atan(tan_rd2)
!-- Get rho2
  rho2      = cos(rd)/cos(rd2)
!-- Compute gamma
  tan_gamma = cx / cy1
  cgamma    = atan(tan_gamma)
!-- Compute beta
  sin_beta  = cx/sin(cgamma)
!
!-- If |sin(beta)|>1 then the axis of the eclipse is tangent to the Earth's
!   surface or directly it does not crosses the Earth.
  if(abs(sin_beta) .lt. 1) then
	beta      = asin(sin_beta)
	!-- Compute C
	tan_C     = cy1 / cos(beta)
	C         = atan(tan_C)
	!-- Compute c
	cm        = cy1 / sin(C)
        !-- Compute lat1
        sin_lat1  = cm * sin(C + rd1)
        cos_lat1  = sqrt(1 - sin_lat1**2)
        tan_lat1  = sin_lat1/cos_lat1
        !-- Convert to geographic latitude
        tan_lat   = E_1 * tan_lat1
        elat_track= atan(tan_lat)/degrad
        !-- Compute the hourly angle
        sin_H     = cx / cos_lat1
        cos_H     = cm * cos(C+rd1)/cos_lat1
        tan_H     = sin_H/cos_H
        H         = atan(tan_H)

	if(cos_H .lt. 0) then
		if(sin_H .ge. 0) then
			H = pi - abs(H)
		else
			H = pi + abs(H)
		end if
	end if
        !-- Compute the longitude
        elon_track= (rmu - H)/degrad
        !-- Convert to geopgraphic longitude
        if(elon_track .gt. 360) then
                elon_track = elon_track -360
        endif
        if(elon_track .gt. 180) then
                elon_track = elon_track -360
        endif
        if(elon_track .le. -180) then
                elon_track = elon_track +360
        endif
        !-- Correct the Earth's movement
        elon_track = -elon_track + 1.002738 * 15*Dt/3600
  endif

!---------------------------
! STEP 2: Compute the obscuricity for each grid-point. This step it is important
!         in order to reduce the incoming radiation
! Loop over latitudes
  do j=jts,jte
     ! Loop over longitudes
     do i=its,ite
	!--Correct the Earth's movement
	lon = -xlon(i,j) - 1.002738 * 15*Dt/3600
  	if(lon .lt. 0) then
      	    lon = lon + 360
        endif
        if(lon .gt. 360) then
            lon = lon -360
        endif

        !-- Convert degrees to radians
	rlat = xlat(i,j)*degrad
        rlon = lon*degrad

        !-- Hourly angle
        H    = rmu - rlon

	!-- Adjust the Earth's sphericity
	A    = sqrt( 1 - ( epsil * sin(rlat) )**2 )

	!-- Compute lat1
        cos_lat1 = cos(rlat) / A
        sin_lat1 = sin(rlat) * E / A

        !-- Geocentric coordinates at the observation point
	xi     = cos_lat1 * sin(H)
	eta1   = sin_lat1 * cos(rd1) * E - cos_lat1 * sin(rd1) * cos(H)
	zeta1  = sin_lat1 * sin(rd2) * E + cos_lat1 * cos(rd2) * cos(H)

	!-- Correct cy (sembla que no es necessari)
	cy1  = cy/rho1

        !-- Distance to the shadow axis
        DELTA2 = (xi - cx)**2 + (eta1  - cy1)**2;
	DELTA  = sqrt(DELTA2)

        !-- Penumbra and Umbra cone's circumference over the Earth's surface
	LL1    = (cl1 - zeta1 * tanf1) ! Penumbra
	LL2    = (cl2 - zeta1 * tanf2) ! Umbra

        ! Check if we are inside the eclipse
        if(DELTA .gt. LL1) then ! Without obscuration
              Obscur(i,j) = 0.
              mask(i,j)   = 0
        elseif(DELTA .le. LL1 .and. DELTA .gt. abs(LL2)) then   ! Penumbra
              Obscur(i,j) = (LL1 - DELTA) / (LL1 + LL2)         ! Partial eclipse
              mask(i,j)   = 1
        elseif(DELTA .le. abs(LL2)) then                        ! Umbra
		if(LL2 .lt. 0) then
                      Obscur(i,j) = 1.                          ! Total eclipse
	              mask(i,j)   = 2
                else
                      Obscur(i,j) = (LL1 - DELTA) / (LL1 + LL2) ! Annular eclipse
	              mask(i,j)   = 3
                endif
        endif

        ! Check that the obscuration is not greater than 1
        if(Obscur(i,j) .gt. 1) then
              Obscur(i,j) = 1.
        endif

     enddo
     ! End loop over longitudes
  enddo
! End loop over longitudes
end subroutine
!
!----------------------------
! INTERNAL SUBROUTINES
!	load_besselian_elements --> load the besselian elements from eclipse_besselian_elements.dat
!       compute_besselian_t     --> compute the besselian elements for the current time
!
!----------------------------
subroutine load_besselian_elements(t, julday, year, X, Y, d, l1, l2, mu,  & 
                                   tanf1, tanf2, t0, tmin, tmax, NBessel, &
                                   eclipse_exist,Dt)
  IMPLICIT NONE
!
!--------------
!----------
! VARIABLES
!----------
!--------------
!
!----------
! Input/output vars
!----------
  integer,                             intent(in)    :: julday, year, NBessel
  real,                                intent(in)    :: t
  logical,                             intent(inout) :: eclipse_exist
  real, dimension(NBessel),            intent(inout) :: X, Y, d, l1, l2, mu
  real,                                intent(inout) :: tanf1, tanf2, t0, tmin, tmax, Dt
!----------
! Local vars
!----------
  integer :: ryear, rjulday
  logical :: file_exist

!----------
! Initialization
!----------
  eclipse_exist = .false.

!----------
! Load the information from eclipse_besselian_elements.dat
!----------
  inquire( file="eclipse_besselian_elements.dat", exist=file_exist)
  if (file_exist) then
	 open (20,FILE='eclipse_besselian_elements.dat',status="old",action="read")
  else
	call wrf_error_fatal('load_besselian_elements: eclipse_besselian_elements.dat not found')
	return
  end if

  do
     ! Load the list of eclipse features
     read(20,*,end=1)ryear,rjulday,t0,tmin,tmax,  &
                         X(1), X(2), X(3), X(4),  &
                         Y(1), Y(2), Y(3), Y(4),  &                      
                         d(1), d(2), d(3), d(4),  &
                        l1(1),l1(2),l1(3),l1(4),  &
                        l2(1),l2(2),l2(3),l2(4),  &
                        mu(1),mu(2),mu(3),mu(4),  &
                        tanf1,tanf2,Dt
     ! Check if the current date matches with the loaded episode
     if(ryear .eq. year .and. rjulday .eq. julday .and. tmin .le. t .and. t .le. tmax) then
             eclipse_exist = .true.
             close(20)
             return
     else
     endif
  end do
  1 close(20)
  return
end subroutine
!----------------------------
!----------------------------
!----------------------------
subroutine compute_besselian_t(t1, NBessel, X, Y, d, l1, l2, mu, t0, Dt, &
                               cx, cy, cd, cl1, cl2, cmu)
  IMPLICIT NONE
  integer,                             intent(in)    :: NBessel
  real,                                intent(in)    :: t1
  real, dimension(NBessel),            intent(in)    :: X, Y, d, l1, l2, mu
  real,                                intent(in)    :: t0, Dt
  real,                                intent(inout) :: cx, cy, cd, cl1, cl2, cmu

  real                                               :: t
  integer                                            :: i

  cx  = 0.
  cy  = 0.
  cd  = 0.
  cl1 = 0.
  cl2 = 0.
  cmu = 0.

  t = (t1 + DT/3600) - t0 

  do i=1,NBessel
      cx  = cx  + X(i)  * t**(i-1)
      cy  = cy  + Y(i)  * t**(i-1)
      cd  = cd  + d(i)  * t**(i-1)
      cl1 = cl1 + l1(i) * t**(i-1)
      cl2 = cl2 + l2(i) * t**(i-1)
      cmu = cmu + mu(i) * t**(i-1)
  enddo

end subroutine
!----------------------------
!----------------------------

end module module_ra_eclipse
