!------------------------------------------------------------------------------
! NASA/GSFC, Computational and Information Science and Technology Office, 
! Code 606
!------------------------------------------------------------------------------
!
! MODULE: DSCALE_mod
!
! AUTHOR:
! Eric Kemp, NASA CISTO/SSAI
!
! DESCRIPTION:
! Contains diagnostic codes for NASA Regional Climate Downscaling project.
!------------------------------------------------------------------------------

module DSCALE_mod

   implicit none
   private

   public :: cal_tpwtwp
   public :: calc_dz8w
   public :: calc_rho
   public :: ivt_driver
   public :: calc_freezing_level
   public :: calc_t_phy
   public :: calc_z

contains

   ! EMK...Calculate integrated microphysics:
   ! * Total precipitable water (from water vapor)
   ! * Liquid water path (from cloud water and rain)
   ! * Ice water path (from cloud ice, snow, graupel, and hail)
   ! * Cloud liquid water path (from cloud water)
   ! * Rain water path (from rain)
   ! * Cloud ice water path (from cloud ice)
   ! * Frozen precipitation water path (from snow, graupel, and hail)
   ! Based heavily on code from Takamichi Iguchi.
   SUBROUTINE cal_tpwtwp(dz8w, rho, qv, ql, qi, qr, qs, qg, qh,       &
        tpw, lwp, iwp, clwp, rwp, ciwp, fpwp,         &
        ids,ide, jds,jde, kds,kde,                    & ! domain dims
        ims,ime, jms,jme, kms,kme,                    & ! memory dims
        its,ite, jts,jte, kts,kte                     ) ! tile   dims

      implicit none

      ! Arguments
      integer, intent(in) :: ids,ide,jds,jde,kds,kde               
      integer, intent(in) :: ims,ime,jms,jme,kms,kme         
      integer, intent(in) :: its,ite,jts,jte,kts,kte 

      REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(IN )               :: dz8w, rho,                    &
           qv, ql, qi, qr, qs, qg, qh
      REAL,    DIMENSION( ims:ime , jms:jme ),                            &
           INTENT(OUT )               :: tpw, lwp, iwp, clwp, rwp, ciwp, fpwp
      
      ! Local variables
      INTEGER :: i, k, j

      ! Initialize
      do j = jts,jte
         do i = its,ite
            tpw(i,j)  = 0.
            lwp(i,j)  = 0.
            iwp(i,j)  = 0.
            clwp(i,j) = 0.
            rwp(i,j)  = 0.
            ciwp(i,j) = 0.
            fpwp(i,j) = 0.
         end do
      end do

      do j = jts, min(jte,jde-1)
!         do k = kts, kte
         do k = kts, min(kte-1,kde-2)
            do i = its, min(ite,ide-1)
               tpw(i,j) = tpw(i,j) + dz8w(i,k,j) * rho(i,k,j) * qv(i,k,j)
               lwp(i,j) = lwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (ql(i,k,j) + qr(i,k,j))
               iwp(i,j) = iwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (qi(i,k,j) + qs(i,k,j) + qg(i,k,j) + qh(i,k,j))
               clwp(i,j) = clwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (ql(i,k,j))
               rwp(i,j) = rwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (qr(i,k,j))
               ciwp(i,j) = ciwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (qi(i,k,j))
               fpwp(i,j) = fpwp(i,j) + dz8w(i,k,j) * rho(i,k,j) * &
                    (qs(i,k,j) + qg(i,k,j) + qh(i,k,j))
            enddo
         enddo
      enddo

      return
  END SUBROUTINE cal_tpwtwp

  ! EMK NUWRF...Find thickness of each model layer.
  ! Based on subroutine phys_prep in module_big_step_utilities_em
  subroutine calc_dz8w(phb,ph,dz8w, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )

     USE module_model_constants, only: g

     implicit none

     ! Arguments
     INTEGER ,        INTENT(IN   ) ::   ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte
     REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(IN   )                                  ::      ph,phb
     REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  :: dz8w

     ! Local variables
     real, dimension(its:ite, kts:kte, jts:jte) :: z_at_w
     INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
     integer :: i,j,k

     ! Set bounds
     i_start = its
     i_end   = min( ite,ide-1 )
     j_start = jts
     j_end   = min( jte,jde-1 )
     
     k_start = kts
     k_end = min( kte, kde-1 )

     ! Find heights at w levels
     do j = j_start,j_end
        do k = k_start, k_end
           do i = i_start, i_end
              z_at_w(i,k,j) = (phb(i,k,j)+ph(i,k,j))/g
           enddo
        enddo
     enddo
     
     ! Now find thicknesses of each model layer
     do j = j_start,j_end
        do k = k_start, k_end-1
           do i = i_start, i_end
              dz8w(i,k,j) = z_at_w(i,k+1,j)-z_at_w(i,k,j)
           enddo
        enddo
    enddo

    return
  end subroutine calc_dz8w

  ! EMK NUWRF...Calculate air density.
  ! Based on subroutine phy_prep in module_big_step_utilities_em
  subroutine calc_rho(alt,moist,rho,n_moist, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )

     use module_state_description, only: P_QV
     implicit none

     ! Arguments
     INTEGER ,          INTENT(IN   ) :: n_moist
     INTEGER ,        INTENT(IN   ) ::   ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte
     REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(IN   )                                  ::     alt
     REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), &
          INTENT(IN)                                     ::     moist
     REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  ::     rho

     ! Local variables
     INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
     integer :: i,j,k

     ! Set bounds
     i_start = its
     i_end   = min( ite,ide-1 )
     j_start = jts
     j_end   = min( jte,jde-1 )
     
     k_start = kts
     k_end = min( kte, kde-1 )

     ! Calculate air density
     do j = j_start,j_end
        do k = k_start, k_end
           do i = i_start, i_end
              rho(i,k,j) = 1./alt(i,k,j)*(1.+moist(i,k,j,P_QV))
           end do
        end do
     end do

     return
  end subroutine calc_rho

  ! EMK NUWRF
  ! Calculate integrated vapor transport. Initialization step.
  ! Reference:
  ! Zhu, Y., and R. E. Newell, 1998: A proposed algorithm for moisture
  !   fluxes from atmospheric rivers. Monthly Weather Review, 126, 725-735.
  subroutine initialize_ivt(sum_ivt_u,sum_ivt_v,ivt_count, &
       mean_ivt_u, mean_ivt_v, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )

     implicit none

     ! Arguments
     integer, intent(in) :: ids, ide, jds, jde, kds, kde,                &
          ims, ime, jms, jme, kms, kme,                &
          its, ite, jts, jte, kts, kte 
     ! Intermediate sums of integrated vapor transport (u and v winds)
     real,intent(inout) :: sum_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: sum_ivt_v(ims:ime, jms:jme)
     ! Time average integrated vapor transport (u and v winds)
     real,intent(inout) :: mean_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: mean_ivt_v(ims:ime, jms:jme)
     ! Count of time levels in IVT calculation
     integer,intent(inout) :: ivt_count

     ! Local variables
     integer :: i,j,k
     
     do j = jts, min(jte,jde-1)
        do i = its, min(ite,ide-1)
           sum_ivt_u(i,j) = 0.
           sum_ivt_v(i,j) = 0.
           mean_ivt_u(i,j) = 0.
           mean_ivt_v(i,j) = 0.
        end do
     end do

     ivt_count = 0

     return
  end subroutine initialize_ivt

  ! EMK NUWRF
  ! Calculate integrated vapor transport. Update step.
  ! Reference:
  ! Zhu, Y., and R. E. Newell, 1998: A proposed algorithm for moisture
  !   fluxes from atmospheric rivers. Monthly Weather Review, 126, 725-735.
  subroutine update_ivt_sums(sum_ivt_u,sum_ivt_v,ivt_count, &
       u, v, qvapor, rho, dz8w, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )

     implicit none

     ! Arguments
     integer, intent(in) :: ids, ide, jds, jde, kds, kde,                &
          ims, ime, jms, jme, kms, kme,                &
          its, ite, jts, jte, kts, kte
     real,intent(in) :: u(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: v(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: qvapor(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: rho(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: dz8w(ims:ime, kms:kme, jms:jme)
     ! Intermediate sums of integrated vapor transport (u and v winds)
     real,intent(inout) :: sum_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: sum_ivt_v(ims:ime, jms:jme)
     ! Count of time levels in IVT calculation
     integer,intent(inout) :: ivt_count

     ! Local variables
     real :: qu, qv
     integer :: i,k,j

     ! Update sums of IVT_I. Normalization (time-averaging) performed
     ! by another routine.
     do j = jts, min(jte,jde-1)
        do k = kts, min(kte,kde-1)
           do i = its, min(ite,ide-1) 
              qu = 0.5*(u(i,k,j)+u(i+1,k,j))*qvapor(i,k,j)
              sum_ivt_u(i,j) = sum_ivt_u(i,j) + &
                   qu*rho(i,k,j)*dz8w(i,k,j)                   
           end do
        end do
     end do

     ! Update sums of IVT_J. Normalization (time-averaging) performed
     ! by another routine.
     do j = jts, min(jte,jde-1)
        do k = kts, min(kte,kde-1)
           do i = its, min(ite,ide-1) 
              qv = 0.5*(v(i,k,j)+v(i,k,j+1))*qvapor(i,k,j)
              sum_ivt_v(i,j) = sum_ivt_v(i,j) + &
                   qv*rho(i,k,j)*dz8w(i,k,j)              
           end do
        end do
     end do

     ivt_count = ivt_count + 1
     return
  end subroutine update_ivt_sums
  
  ! EMK NUWRF
  ! Calculate integrated vapor transport. Final time averaging step.
  ! Reference:
  ! Zhu, Y., and R. E. Newell, 1998: A proposed algorithm for moisture
  !   fluxes from atmospheric rivers. Monthly Weather Review, 126, 725-735.
  subroutine calc_mean_ivt(mean_ivt_u, mean_ivt_v, &
       sum_ivt_u, sum_ivt_v, ivt_count, &       
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )

     implicit none

     ! Arguments
     integer, intent(in) :: ids, ide, jds, jde, kds, kde,                &
          ims, ime, jms, jme, kms, kme,                &
          its, ite, jts, jte, kts, kte
     ! Intermediate sums of integrated vapor transport (u and v winds)
     real,intent(in) :: sum_ivt_u(ims:ime, jms:jme)
     real,intent(in) :: sum_ivt_v(ims:ime, jms:jme)
     ! Count of time levels in IVT calculation
     integer,intent(in) :: ivt_count
     ! Time average integrated vapor transport (u and v winds)
     real,intent(inout) :: mean_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: mean_ivt_v(ims:ime, jms:jme)

     ! Local variables
     integer :: i,j

     if (ivt_count < 1) then
        do j = jts, min(jte,jde-1)
           do i = its, min(ite,ide-1) 
              mean_ivt_u(i,j) = 0.
              mean_ivt_v(i,j) = 0.
           end do
        end do
     else
        do j = jts, min(jte,jde-1)
           do i = its, min(ite,ide-1) 
              mean_ivt_u(i,j) = sum_ivt_u(i,j)/float(ivt_count)
              mean_ivt_v(i,j) = sum_ivt_v(i,j)/float(ivt_count)
           end do
        end do
     end if

     return
  end subroutine calc_mean_ivt

  ! EMK NUWRF
  ! Calculate integrated vapor transport. Main driver routine.
  ! Reference:
  ! Zhu, Y., and R. E. Newell, 1998: A proposed algorithm for moisture
  !   fluxes from atmospheric rivers. Monthly Weather Review, 126, 725-735.
  subroutine ivt_driver(mean_ivt_u,mean_ivt_v, &
       sum_ivt_u, sum_ivt_v, ivt_count, &       
       u, v, qvapor, rho, dz8w, &
!       xtime, dt, ivt_interval, is_restart, &
       curr_secs2, dt, ivt_interval, is_restart, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )
     implicit none

     ! Arguments
     integer, intent(in) :: ids, ide, jds, jde, kds, kde,                &
          ims, ime, jms, jme, kms, kme,                &
          its, ite, jts, jte, kts, kte
     real,intent(in) :: u(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: v(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: qvapor(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: rho(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: dz8w(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: curr_secs2, dt
     integer,intent(in) :: ivt_interval
     logical,intent(in) :: is_restart

     ! Intermediate sums of integrated vapor transport (x- and y-directions)
     real,intent(inout) :: sum_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: sum_ivt_v(ims:ime, jms:jme)
     ! Count of time levels in IVT calculation
     integer,intent(inout) :: ivt_count
     ! Time average integrated vapor transport (x and y-directions)
     real,intent(inout) :: mean_ivt_u(ims:ime, jms:jme)
     real,intent(inout) :: mean_ivt_v(ims:ime, jms:jme)

     ! Local variables
     integer :: numerator, denominator

!     numerator = NINT(XTIME/dt*60.)
!     denominator = NINT(ivt_interval/dt*60.)
     numerator = NINT(curr_secs2/dt)
     denominator = NINT(ivt_interval*60./dt)

     ! Intel compiler has problems if denominator is zero. 
     if (denominator > 0) then
        if ( (mod(numerator,denominator) == 0 ) .and. &
             (.not. is_restart)) then
           CALL wrf_debug ( 200 , ' Calling initialize_ivt...') 
           call initialize_ivt(sum_ivt_u,sum_ivt_v,ivt_count, &
                mean_ivt_u, mean_ivt_v, &
                ids, ide, jds, jde, kds, kde,                &
                ims, ime, jms, jme, kms, kme,                &
                its, ite, jts, jte, kts, kte                )        
        end if
     ! If denominator is zero, we'll interpret as instantaneous IVT request.
     ! No need to check for restart.
     else if (denominator == 0) then
        CALL wrf_debug ( 200 , ' Calling initialize_ivt...') 
        call initialize_ivt(sum_ivt_u,sum_ivt_v,ivt_count, &
             mean_ivt_u, mean_ivt_v, &
             ids, ide, jds, jde, kds, kde,                &
             ims, ime, jms, jme, kms, kme,                &
             its, ite, jts, jte, kts, kte                )
     end if

     CALL wrf_debug ( 200 , ' Calling update_ivt_sums...') 
     call update_ivt_sums(sum_ivt_u,sum_ivt_v,ivt_count, &
          u, v, qvapor, rho, dz8w, &
          ids, ide, jds, jde, kds, kde,                &
          ims, ime, jms, jme, kms, kme,                &
          its, ite, jts, jte, kts, kte                )

     ! Intel compiler has problems if denominator is zero. 
     if (denominator > 0) then
        if ( (mod(numerator,denominator) == 0 ) ) then

           CALL wrf_debug ( 200 , ' Calling calc_mean_ivt...') 

           call calc_mean_ivt(mean_ivt_u, mean_ivt_v, &
                sum_ivt_u, sum_ivt_v, ivt_count, &       
                ids, ide, jds, jde, kds, kde,                &
                ims, ime, jms, jme, kms, kme,                &
                its, ite, jts, jte, kts, kte                )           
        end if
     ! Copy instantaneous fields to mean values.
     else if (denominator == 0) then
        CALL wrf_debug ( 200 , ' Calling calc_mean_ivt...') 
        
        call calc_mean_ivt(mean_ivt_u, mean_ivt_v, &
             sum_ivt_u, sum_ivt_v, ivt_count, &       
             ids, ide, jds, jde, kds, kde,                &
             ims, ime, jms, jme, kms, kme,                &
             its, ite, jts, jte, kts, kte                )           
     end if

     return
  end subroutine ivt_driver

  ! EMK NUWRF...Calculate freezing level.
  ! Based on AFWA code in WRF 3.6.1.
  subroutine calc_freezing_level(freezing_level, &
       t_phy, z, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )           
     
     implicit none

     ! Arguments
     integer, intent(in) :: ids,ide,jds,jde,kds,kde               
     integer, intent(in) :: ims,ime,jms,jme,kms,kme         
     integer, intent(in) :: its,ite,jts,jte,kts,kte 
     ! Temperature (K) at mid-layer
     real,intent(in) :: t_phy(ims:ime, kms:kme, jms:jme) 
     real,intent(in) :: z(ims:ime, kms:kme, jms:jme)
     real,intent(out) :: freezing_level(ims:ime, jms:jme)

     ! Local variables
     integer :: i,k,j

     ! Initialize to missing
     do j = jts,min(jte,jde-1)
        do i = its,min(ite,ide-1)
           freezing_level(i,j) = -9999           
        end do
     end do

     ! Now loop through and replace missing values
     do j = jts,min(jte,jde-1)
        do k = kts, min(kte,kde-1)
           do i = its,min(ite,ide-1)

              if (freezing_level(i,j) .gt. -9999) cycle
              if (t_phy(i,k,j) .gt. 273.15) cycle

              if (k .eq. 1) then ! Freezing level at the surface
                 freezing_level(i,j) = z(i,1,j)
              else
                 ! Interpolate linearly in height
                 freezing_level(i,j) = z(i,k-1,j) + &
                      ((273.15 - t_phy(i,k-1,j)) &
                      / (t_phy(i,k,j) - t_phy(i,k-1,j))) &
                      *(z(i,k,j) - z(i,k-1,j))
              end if
           end do
        end do
     end do

     return
  end subroutine calc_freezing_level

  ! EMK NUWRF...Based on subroutine phy_prep
  subroutine calc_t_phy(t_phy, &
       t, p, pb, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )           
     
     use module_model_constants, only: P1000MB, t0, rcp
     implicit none

     ! Arguments
     integer, intent(in) :: ids,ide,jds,jde,kds,kde               
     integer, intent(in) :: ims,ime,jms,jme,kms,kme         
     integer, intent(in) :: its,ite,jts,jte,kts,kte 
     real,intent(in) :: t(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: p(ims:ime, kms:kme, jms:jme)
     real,intent(in) :: pb(ims:ime, kms:kme, jms:jme)
     real,intent(inout) :: t_phy(ims:ime, kms:kme, jms:jme)

     ! Local constants
     real, parameter :: RECIPROCAL_P1000MB = 1./P1000MB

     ! Local variables
     real :: th, pi
     integer :: i,k,j

     do j = jts,min(jte,jde-1)
        do k = kts, kte
           do i = its,min(ite,ide-1)
              th = t(i,k,j) + T0 ! Potential temperature (K)
              ! Exner function
              pi = ( (p(i,k,j) + pb(i,k,j))*RECIPROCAL_P1000MB )**RCP
              t_phy(i,k,j) = th*pi ! Temperature (K)
           end do
        end do
     end do

     return
  end subroutine calc_t_phy

  ! EMK NUWRF...Find heights at mid-layers. Based on subroutine phy_prep
  subroutine calc_z(z, &
       phb, ph, &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                )           
     
     use module_model_constants, only: g
     implicit none

     ! Arguments
     integer, intent(in) :: ids,ide,jds,jde,kds,kde               
     integer, intent(in) :: ims,ime,jms,jme,kms,kme         
     integer, intent(in) :: its,ite,jts,jte,kts,kte 
     real, intent(in) :: phb(ims:ime, kms:kme, jms:jme)
     real, intent(in) :: ph(ims:ime, kms:kme, jms:jme)
     real, intent(inout) :: z(ims:ime, kms:kme, jms:jme)

     ! Local variables
     real :: z_at_w_top, z_at_w_bottom
     integer :: i,k,j

     do j = jts, min(jte, jde-1)
        do k = kts, min(kte-1, kde-2)
           do i = its, min(ite, ide-1)
              z_at_w_bottom = (phb(i,k  ,j)+ph(i,k  ,j))/G
              z_at_w_top    = (phb(i,k+1,j)+ph(i,k+1,j))/G
              z(i,k,j) = 0.5*(z_at_w_bottom + z_at_w_top)
           end do
        end do
     end do

     return
  end subroutine calc_z

end module DSCALE_mod
