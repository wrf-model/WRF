MODULE module_compute_geop

CONTAINS
  SUBROUTINE compute_500mb_height  ( ph, phb, p, pb,                  &
                                   height, track_level,             &
                                   ids, ide, jds, jde, kds, kde,    &
                                   ims, ime, jms, jme, kms, kme,    &
                                   its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE


   !  Input data.

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::           &
                                                                 ph,      &
                                                                 phb,     &
                                                                 pb,      &
                                                                 p

   REAL , DIMENSION( ims:ime , jms:jme ) ,    INTENT(  OUT) :: height

   INTEGER , INTENT(IN) :: track_level

!  local variables

   integer :: i,j,k
   real, dimension(kms:kme) :: pressure,geopotential
   real :: interp_output
   real :: track_level_p

!  slow version of code, we'll call interp routine for each column

   track_level_p = float(track_level)

   do j = jts, min(jde-1,jte)
   do i = its, min(ide-1,ite)

      do k=kds,kde-1
        pressure(k) = p(i,k,j) + pb(i,k,j)
        geopotential(k) = 0.5*( ph(i,k  ,j)+phb(i,k  ,j)  &
                               +ph(i,k+1,j)+phb(i,k+1,j) )
      enddo

!     call interp_p( geopotential, pressure, 70000., interp_output,  &
      call interp_p( geopotential, pressure, track_level_p, interp_output,  &
                     kds,kde-1,kms,kme, i,j )

      height(i,j) = interp_output/9.81  !  500 mb height in meters

   enddo
   enddo

   end subroutine compute_500mb_height

!--------

  subroutine interp_p(a,p,p_loc,a_interp,ks,ke,kms,kme,i,j)
  implicit none

  integer, intent(in) :: ks,ke,kms,kme,i,j
  real, dimension(kms:kme), intent(in) :: a,p
  real, intent(in)  :: p_loc
  real, intent(out) :: a_interp

!---  local variables

  integer :: kp, pk, k
  real    :: wght1, wght2, dp, pressure
  character*256 mess

!cys_change: set high value at below-ground point
    if (p(ks).lt.p_loc) then
       a_interp=9.81e5
    else

    kp = ks+1
    pk = p(kp)
    pressure = p_loc
    do while( pk .gt. pressure )

      kp = kp+1

      if(kp .gt. ke) then
        write(mess,*) ' interp too high: pressure, p(ke), i, j = ',pressure,p(ke),i,j
        call wrf_message ( mess )
        write(mess,*)'p: ',p
        call wrf_error_fatal( mess )
      end if
 
      pk = p(kp)

    enddo

    dp = p(kp-1) - p(kp)
    wght2 = (p(kp-1)-pressure)/dp
    wght1 = 1.-wght2

    a_interp = wght1*a(kp-1) + wght2*a(kp)

    endif   !cys_change

    end subroutine interp_p

END MODULE module_compute_geop
