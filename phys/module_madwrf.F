  module module_madwrf

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !                                          !
  ! Purpose: MAD-WRF model components        !
  !                                          !
  ! Author: Pedro A. Jimenez                 !
  !                                          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use module_model_constants, only : G, T0, RCP
#if ( ! NMM_CORE == 1 )
    use module_soil_pre, only : Skip_middle_points_t
#endif

    implicit none

    private
    public :: Init_madwrf_tracers, Init_madwrf_clouds

  contains

    function Calc_cldtopz_from_brtemp (cldmask, ts, dzs, brtemp, tropoz, ht, kts, kte) result (return_value)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                                                                         !
    ! Purpose: Calculate cloud top height from brigtness temperature                          !
    !                                                                                         !
    ! Authors: Pedro A. Jimenez version of Greg Thompson code                                 !
    !                                                                                         !
    ! Comments: Staring at model top, go downwards first to the level of the tropopause,      !
    !           then keep going until the model temperature is greater (warmer) than the      !
    !           incoming GOES satellite longwave IR tempeature (channel13 on GOES-R series).  !
    !           As version1.0, this will be the highest altitude of potential cloud based on  !
    !           IR temperature whereas an improvement is possible for inversions in which the !
    !           cloud top could be lower altitude that might be detected using the RH field.  !
    !                                                                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
      implicit none

      real :: return_value

      real, intent (in) :: cldmask, brtemp, tropoz, ht
      integer, intent (in) :: kts, kte
      real, dimension (kts:kte), intent (in) :: ts, dzs

      real, parameter :: MISSING_CLDTOPZ = -999.9, MISSING_BRTEMP = -999.9
      real :: this_height
      integer :: k


      return_value = MISSING_CLDTOPZ
      if (brtemp == MISSING_BRTEMP) return
      if (cldmask > 0.0) then
        this_height = sum (dzs(kts:kte - 1)) + ht
        do k = kte - 2, kts, -1
          this_height = this_height - dzs(k + 1)
          if (this_height > tropoz) cycle
          if (ts(k) > brtemp) then
            return_value = this_height + dzs(k) * (brtemp - ts(k)) / (ts(k + 1) - ts(k))
            if ((return_value < this_height) .or. (return_value > this_height + dzs(k))) &
                return_value = tropoz
            exit
          end if
        end do
      end if

    end function Calc_cldtopz_from_brtemp

    subroutine Init_madwrf_clouds (moist, p_qv, p_qc, p_qi, p_qs, p00, t, p, ph_2, phb, alt, xland, cldmask, cldtopz, cldbasez, &
         brtemp, ht,  dx, dy, flag_cldmask, flag_cldtopz, flag_cldbasez, flag_brtemp, em_width, hold_ups, ids, ide, jds, jde,   &
         its, ims, ime, jms, jme, kms, kme, ite, jts, jte, kts, kte, cldfra)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                          !
    ! Purpose: Cloud initialization            !
    !                                          !
    ! Author: Pedro A. Jimenez                 !
    !                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

        ! t, p, ph_2, phb, alt, xland
      real, dimension (ims:ime, kms:kme, jms:jme, *), intent (inout) :: moist
      real, dimension (ims:ime, kms:kme, jms:jme), intent (in) :: t, p, ph_2, phb, alt
      real, dimension (ims:ime, jms:jme), intent (in) :: xland, cldbasez, brtemp, ht
      real, dimension (ims:ime, jms:jme), intent (inout) :: cldmask, cldtopz
      logical, intent (in) :: hold_ups
      integer, intent (in) :: p_qv, p_qc, p_qi, p_qs, flag_cldmask, flag_cldtopz, flag_cldbasez, flag_brtemp, em_width, &
          ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte
      real, intent (in) :: p00, dx, dy
      real, dimension (ims:ime, kms:kme, jms:jme), intent (out) :: cldfra

      real, parameter :: CONVERT_M_TO_KM = 0.001, MISSING_CLDTOPZ = -999.9, MISSING_CLDMASK = -999.9
      real, dimension (kts:kte - 1) :: dzs, ts
      real :: gridkm, tropoz, cldtopz_agl
      integer :: i, j, k, insert_clouds, k_tropo
      logical, parameter :: LOCAL_DEBUG = .false.


        ! Identify the cloud initialization method
      insert_clouds = 0
      if (flag_cldmask == 1 .and. flag_cldtopz == 0 .and. flag_brtemp == 0) insert_clouds = 1
      if ((flag_cldmask == 1 .and. flag_brtemp == 1) .or. flag_cldtopz == 1) insert_clouds = 2
      if (insert_clouds == 2 .and. flag_cldbasez == 1) insert_clouds = 3
      if (LOCAL_DEBUG) then
        print *, 'flag_cldmask = ', flag_cldmask
        print *, 'flag_cldtopz = ', flag_cldtopz
        print *, 'flag_brtemp = ', flag_brtemp
        print *, 'insert_clouds = ', insert_clouds
      end if

      gridkm = sqrt (dx * dy) * CONVERT_M_TO_KM

      Loop_j: do j = jts, min (jte, jde - 1)
        Loop_i: do i = its, min (ite, ide - 1)
#if ( ! NMM_CORE == 1 )
          if (Skip_middle_points_t (ids, ide, jds, jde, i, j, em_width, hold_ups)) cycle
#endif

          if (LOCAL_DEBUG) print *, 'Calculations for i, j = ', i, j

          Loop_k: do k = kts, kte - 1
              ! Convert theta pert to T
            ts(k) = (t(i, k, j) + T0) / ((p00 / p(i, k, j)) ** RCP) 
              ! Calc layer thickness
            dzs(k) = (ph_2(i, k + 1, j) + phb(i, k + 1, j) - (ph_2(i, k, j) + phb(i, k, j))) / G
          end do Loop_k

            ! Calc troposphere height
          call Calc_tropo_height (ts, p(i, :, j), dzs, kts, kte, LOCAL_DEBUG, k_tropo, tropoz)
          tropoz = tropoz + ht(i, j)

            ! Calc cloud top height agl if necessary
          if (insert_clouds > 0) then
            cldtopz_agl = MISSING_CLDTOPZ
            if (flag_cldtopz == 1) then
                ! cloud mask
              if (cldtopz(i, j) > 0.0) then
                cldmask(i, j) = 1.0
              elseif (cldtopz(i, j) == 0.0) then 
                cldmask(i, j) = 0.0
              else
                cldmask(i, j) = MISSING_CLDMASK
              end if
                ! cloud top height
              if (cldtopz(i, j) > 0.0) cldtopz_agl = max (0.0, cldtopz(i, j) -  ht(i, j))
            else if (flag_brtemp == 1) then
                ! Only cloud top height needed
              cldtopz(i, j) = Calc_cldtopz_from_brtemp (cldmask(i, j), ts, dzs, brtemp(i, j), tropoz, ht(i, j), kts, kte)
              if (cldtopz(i, j) > 0.0) cldtopz_agl = max (0.0, cldtopz(i, j) -  ht(i, j))
            end if
          end if

          select_cld_impro: select case (insert_clouds)
            case (0)
                ! Use the analysis field
              call cal_cldfra3_madwrf (cldfra(i, :, j), moist(i, :, j, p_qv), moist(i, :, j, p_qc), &
                  moist(i, :, j, p_qi), moist(i, :, j, p_qs), dzs, p(i, :, j), ts(:),   &
                  xland(i, j), gridkm, .true., 1.5, tropoz, kts, kte, .false.)

            case (1)
                ! Remove clouds if clear (cldmask = 0)
              call cal_cldfra3_madwrf (cldfra(i, :, j), moist(i, :, j, p_qv), moist(i, :, j, p_qc), &
                  moist(i, :, j, p_qi), moist(i, :, j, p_qs), dzs, p(i, :, j), ts(:),   &
                  xland(i, j), gridkm, .true., 1.5, tropoz, kts, kte, .false., &
                  cldmask = cldmask(i, j))

            case (2)
                ! Remove clouds if clear (cldmask = 0)
                ! Reduce / extend cloud top heights to match observations
                ! Add clouds over clear sky regions (cldmask = 1)
              call cal_cldfra3_madwrf (cldfra(i, :, j), moist(i, :, j, p_qv), moist(i, :, j, p_qc), &
                  moist(i, :, j, p_qi), moist(i, :, j, p_qs), dzs, p(i, :, j), ts(:),   &
                  xland(i, j), gridkm, .true., 1.5, tropoz, kts, kte, .false., &
                  cldmask = cldmask(i, j), cldtopz = cldtopz_agl)

            case (3)
                ! Remove clouds if clear (cldmask = 0)
                ! Reduce / extend cloud top / base heights to match observations
              call cal_cldfra3_madwrf (cldfra(i, :, j), moist(i, :, j, p_qv), moist(i, :, j, p_qc), &
                  moist(i, :, j, p_qi), moist(i, :, j, p_qs), dzs, p(i, :, j), ts(:),   &
                  xland(i, j), gridkm, .true., 1.5, tropoz, kts, kte, .false.,  &
                  cldmask = cldmask(i, j), cldtopz = cldtopz_agl, cldbasez = cldbasez(i, j))

          end select select_cld_impro

!               do k = kts, kte - 1
!                  moist(i,k,j,P_QV) = MAX(temp_Qv(k), moist(i,k,j,P_QV))
!                  moist(i,k,j,P_QC) = temp_Qc(k) / temp_R(k)
!                  moist(i,k,j,P_QI) = temp_Qi(k) / temp_R(k)
!                  if (P_QNI .gt. 1) then
!                     scalar(i,k,j,P_QNI) = temp_Ni(k) / temp_R(k)
!                  endif
!                  if (P_QNC .gt. 1) then
!                     scalar(i,k,j,P_QNC) = temp_Nc(k) / temp_R(k)
!                  endif
!               enddo
        end do Loop_i
      end do Loop_j

    end subroutine Init_madwrf_clouds

    subroutine Init_madwrf_tracers (tracer, moist, p_qc, p_qi, p_qs, p_tr_qc, p_tr_qi, p_tr_qs, &
        ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its , ite , jts , jte , kts , kte)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                          !
    ! Purpose: Initializes MAD-WRF tracers     !
    !                                          !
    ! Author: Pedro A. Jimenez                 !
    !                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real, dimension (ims:ime, kms:kme, jms:jme, *), intent (inout) :: tracer
      real, dimension (ims:ime, kms:kme, jms:jme, *), intent (in) :: moist
      integer, intent (in) :: p_qc, p_qi, p_qs, p_tr_qc, p_tr_qi, p_tr_qs, ids, ide, jds, jde, &
          kds, kde, ims, ime, jms, jme, kms, kme, its , ite , jts , jte , kts , kte

      integer :: i, j, k


      do j = jts, min (jte, jde - 1)
        do k = kts, kte - 1
          do i = its, min (ite, ide - 1)
            tracer(i, k, j, p_tr_qc) = moist(i, k, j, p_qc)
            tracer(i, k, j, p_tr_qi) = moist(i, k, j, p_qi)
            tracer(i, k, j, p_tr_qs) = moist(i, k, j, p_qs)
          end do
        end do
      end do

    end subroutine Init_madwrf_tracers

!+---+-----------------------------------------------------------------+

      SUBROUTINE cal_cldfra3_madwrf(CLDFRA, qv, qc, qi, qs, dz,                &
     &                 p, t, XLAND, gridkm,                             &
     &                 modify_qvapor, max_relh,                         &
     &                 tropo_z, kts,kte, debug_flag, k_tropo, cldmask,  &
     &                 cldtopz, cldbasez)
!
      USE module_mp_thompson   , ONLY : rsif, rslf
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: modify_qvapor
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qv, qc, qi, qs, cldfra
      REAL, DIMENSION(kts:kte), INTENT(IN):: p, t, dz
      REAL, INTENT(IN):: gridkm, XLAND, max_relh
      REAL, INTENT(INOUT):: tropo_z
      LOGICAL, INTENT(IN):: debug_flag
      integer, intent(in), optional :: k_tropo
      real, intent(in), optional :: cldmask, cldtopz, cldbasez

!..Local vars.
      REAL:: RH_00L, RH_00O, RH_00
      REAL:: entrmnt=0.5
      INTEGER:: k
      REAL:: TC, qvsi, qvsw, RHUM, delz, cldbasek_tmp
      REAL, DIMENSION(kts:kte):: qvs, rh, rhoa

      character*512 dbg_msg
      logical :: is_tropo_init, impose_cldmask, impose_cldtopz, &
          impose_cldbasetopz, keep_clouds_below_lcl
      INTEGER :: cldtopk, cldbasek, cldthick_bot_k, cldfra_thresh_k
      REAL, PARAMETER :: CLDTHICK_DEF = -999.9  ! Default thickness [m] of new clouds
      REAL, PARAMETER :: CLDFRA_DEF = 0.5  ! Default cloud fraction to insert in new clouds
      REAL, PARAMETER :: CLDFRA_THRESH = 0.0  ! Default threshold cloud fraction to determine presence of clouds
      REAL, PARAMETER :: RH_NOCLOUD = 0.3
      logical, parameter :: LOCAL_DEBUG = .false.


        ! Define logical vars to handle optional
        ! arguments
      if (present(k_tropo)) then
        is_tropo_init = .true.
      else
        is_tropo_init = .false.
      end if

      keep_clouds_below_lcl = .false.
      impose_cldmask = .false.
      impose_cldtopz = .false.
      impose_cldbasetopz = .false.

      if (present(cldmask) .and. .not. present(cldtopz) .and. .not. present(cldbasez)) then
        impose_cldmask = .true.
      elseif (present(cldmask) .and. present(cldtopz) .and. .not. present(cldbasez)) then
        impose_cldtopz = .true.
      elseif (present(cldmask) .and. present(cldtopz) .and. present(cldbasez)) then
        impose_cldbasetopz = .true.
      end if

      !! Initialize variables
      cldtopk = -999
      cldbasek = -999
      cldthick_bot_k = -999
      cldfra_thresh_k = -999

        ! Remove hydrometeors if necessary and
        ! calculate height related variables
      if (impose_cldmask) then
        if (cldmask == 0.0) then
          do k = kts, kte - 1
            cldfra(k) = 0.0
            qc(k) = 0.0
            qi(k) = 0.0
            qs(k) = 0.0
          end do
          return
        end if
      end if

      if_impose_cldtopz: if (impose_cldtopz .or. impose_cldbasetopz) then
         if_cldmask: if (cldmask == 0.0) then
            do k = kts, kte - 1
               cldfra(k) = 0.0
               qc(k) = 0.0
               qi(k) = 0.0
               qs(k) = 0.0
            end do
            return
         else if (cldmask > 0.0 .and. cldmask <= 1.0 .and. cldtopz > 0.0) then
              ! Zero out hydrometeors above cldtopz
            call find_k_from_z_agl(cldtopz, cldtopk, dz, kts, kte)
            if (cldtopk > 0 .and. cldtopk < kte) then
              do k = cldtopk + 1, kte - 1
                 cldfra(k) = 0.0
                 qc(k) = 0.0
                 qi(k) = 0.0
                 qs(k) = 0.0
              end do
            end if

              ! Calculate the cloud bottom k if the specified thickness were to be applied
              ! If that would be below ground, then set the k level to 1
            if (CLDTHICK_DEF > 0.0) then
              if (cldtopz - CLDTHICK_DEF > 0.0) then
                 call find_k_from_z_agl(cldtopz - CLDTHICK_DEF, cldthick_bot_k, dz, kts, kte)
              else
                 cldthick_bot_k = 1
              end if
            end if
         end if if_cldmask
      end if if_impose_cldtopz


      if (impose_cldbasetopz) then
         !! If cloud base information (cldbzin) is also provided in met_em files, then...
         !! Zero out hydrometeors below observed cloud base in columns where cldmask > 0.0
         !! Note that hydrometeors have already been zeroed out in columns where cldmask = 0.0
         if (cldmask > 0.0 .and. cldmask <= 1.0 .and. cldbasez > 0.0) then
            ! First, find out level of observed cloud base
            call find_k_from_z_agl(cldbasez, cldbasek, dz, kts, kte)
            !! Trust satellite estimations of cloud top height more than METAR estimates/extrapolations of cloud base height
            !! Thus, if cldtopk < cldbasek, then pretend we don't have cldbasek
            if (cldtopk < cldbasek) cldbasek = -999
            if (cldbasek > 0 .and. cldbasek > kts) then
               do k = kts, cldbasek-1
                  cldfra(k) = 0.0
                  qc(k) = 0.0
                  qi(k) = 0.0
                  qs(k) = 0.0
               end do
            end if
         end if
      end if

!+---+

!..Initialize cloud fraction, compute RH, and rho-air.

         DO k = kts, kte - 1
            CLDFRA(K) = 0.0
            tc = t(k) - 273.15

            qvsw = rslf(P(k), t(k))
            if (debug_flag) print *, 'k, p, t, qvsw, qv =', k, p(k), t(k), qvsw, qv(k)
            if (tc .lt. 0.0) then
               qvsi = rsif(P(k), t(k)+0.025)    !..To ensure a tiny bit ice supersaturated
            else
               qvsi = qvsw
            endif

            if (tc .ge. -12.0) then
               qvs(k) = qvsw
            elseif (tc .lt. -35.0) then
               qvs(k) = qvsi
            else
               qvs(k) = qvsw - (qvsw-qvsi)*(-12.0-tc)/(-12.0+35.)
            endif

            if (modify_qvapor) then
               if (qc(k).gt.1.E-8) then
                  qv(k) = MAX(qv(k), qvsw)
                  qvs(k) = qvsw
               endif
               if (qc(k).le.1.E-8 .and. qi(k).ge.1.E-9) then
                  qv(k) = MAX(qv(k), qvsi)
                  qvs(k) = qvsi
               endif
            endif


            rh(k) = MAX(0.01, qv(k)/qvs(k))
            if (debug_flag) print *, '  qv, qvs, qvsi, qc, qi, rh =', qv(k), qvs(k), qvsi, qc(k), qi(k), rh(k)
            rhoa(k) = p(k)/(287.0*t(k))
         ENDDO

          ! Over ocean, find the level at which RH is lower than RH_NOCLOUD starting from cloud top
          ! to avoid putting clouds in very dry layers
        if ((impose_cldtopz .or. impose_cldbasetopz) .and. (XLAND - 1.5) > 0.0) then
          if (cldtopk > 0 .and. cldtopk < kte) then
            if (cldbasek < 0 .or. cldbasek > cldtopk) then
              if (rh(cldtopk) < RH_NOCLOUD) then
                cldbasek = cldtopk + 1
              else
                cldbasek = cldtopk
              end if
            else
              cldbasek_tmp = cldtopk + 1
              do k = cldtopk, kts, -1
                if (rh(k) < RH_NOCLOUD) exit
                  cldbasek_tmp = k
              end do
              if (cldbasek_tmp > cldbasek) cldbasek = cldbasek_tmp
            end if
          end if
        end if

!..First cut scale-aware. Higher resolution should require closer to
!.. saturated grid box for higher cloud fraction.  Simple functions
!.. chosen based on Mocko and Cotton (1995) starting point and desire
!.. to get near 100% RH as grid spacing moves toward 1.0km, but higher
!.. RH over ocean required as compared to over land.

      DO k = kts, kte - 1

         delz = MAX(100., dz(k))
         RH_00L = 0.65 + SQRT(1./(25.0+gridkm*gridkm*delz*0.01))
         RH_00O = 0.81 + SQRT(1./(50.0+gridkm*gridkm*delz*0.01))
         RHUM = rh(k)

         if (qc(k).gt.1.E-7 .or. qi(k).ge.1.E-7                         &
     &                    .or. (qs(k).gt.1.E-6 .and. t(k).lt.273.)) then
            CLDFRA(K) = 1.0
         else

            IF ((XLAND-1.5).GT.0.) THEN                                  !--- Ocean
               RH_00 = RH_00O
            ELSE                                                         !--- Land
               RH_00 = RH_00L
            ENDIF

            tc = t(k) - 273.15
            if (tc .lt. -12.0) RH_00 = RH_00L

            if (tc .ge. 25.0) then
               CLDFRA(K) = 0.0
            elseif (tc .ge. -12.0) then
               RHUM = MIN(rh(k), 1.0)
               CLDFRA(K) = MAX(0., 1.0-SQRT((1.005-RHUM)/(1.005-RH_00)))
            else
               if (max_relh.gt.1.12 .or. (.NOT.(modify_qvapor)) ) then
!..For HRRR model, the following look OK.
                  RHUM = MIN(rh(k), 1.45)
                  RH_00 = RH_00 + (1.45-RH_00)*(-12.0-tc)/(-12.0+100.)
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.5-RHUM)/(1.5-RH_00)))
               else
!..but for the GFS model, RH is way lower.
                  RHUM = MIN(rh(k), 1.05)
                  RH_00 = RH_00 + (1.05-RH_00)*(-12.0-tc)/(-12.0+100.)
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.05-RHUM)/(1.05-RH_00)))
               endif
            endif
            if (CLDFRA(K).gt.0.) CLDFRA(K) = MAX(0.01, MIN(CLDFRA(K),0.9))

            if (debug_flag) then
              WRITE (dbg_msg,*) 'DEBUG-GT: cloud fraction: ', RH_00, RHUM, CLDFRA(K)
              CALL wrf_debug (150, dbg_msg)
            endif

         endif
      ENDDO

      if ((impose_cldtopz .or. impose_cldbasetopz) .and. cldtopk > 0 .and. cldtopk < kte) then
          ! Remove clouds above observed cloud top
        CLDFRA(cldtopk + 1:kte) = 0.0

        keep_clouds_below_lcl = .true.

          ! Find the first cloudy level
        call find_thresh_k_downward(cldfra, CLDFRA_THRESH, cldfra_thresh_k, cldtopk, kts, kts, kte)
        if (cldfra_thresh_k > 0) then
             ! Extend cloud until cldtopz
           if (cldfra_thresh_k < cldtopk) cldfra(cldfra_thresh_k + 1:cldtopk) = CLDFRA_DEF
        else
            ! No clouds present
          if (CLDTHICK_DEF > 0.0) then
            cldfra(cldthick_bot_k:cldtopk) = CLDFRA_DEF
!          else
!            if (rh(cldtopk) > RH_NOCLOUD) cldfra(cldtopk) = CLDFRA_DEF
          end if
        end if
      end if

      if (impose_cldbasetopz .and. cldbasek > kts .and. cldbasek <= kte) then
         !! Remove clouds below observed cloud base
         CLDFRA(kts:cldbasek-1) = 0.0

         !! If necessary, add clouds with default CLDFRA from observed cloud base up to base of already-existing cloud
         cldfra_thresh_k = -999  ! reset this variable
         call find_thresh_k_upward(cldfra, CLDFRA_THRESH, cldfra_thresh_k, cldbasek, kte, kts, kte)
         if (cldfra_thresh_k > cldbasek) then
           do k = cldbasek, cldfra_thresh_k - 1
             if (rh(k) > RH_NOCLOUD) cldfra(k) = CLDFRA_DEF
           end do
         end if
      end if


      if (is_tropo_init) then
        call find_cloudLayers(qvs, cldfra, T, P, Dz, entrmnt,             &
       &                      debug_flag, qc, qi, qs, tropo_z, kts,kte, keep_clouds_below_lcl, k_tropo)
      else
        call find_cloudLayers(qvs, cldfra, T, P, Dz, entrmnt,             &
       &                      debug_flag, qc, qi, qs, tropo_z, kts,kte, keep_clouds_below_lcl)
      end if

!..Do a final total column adjustment since we may have added more than 1mm
!.. LWP/IWP for multiple cloud decks.

      call adjust_cloudFinal(cldfra, qc, qi, rhoa, dz, kts,kte)

      if (debug_flag) then
        WRITE (dbg_msg,*) 'DEBUG-GT:  Made-up fake profile of clouds'
        CALL wrf_debug (150, dbg_msg)
        do k = kte - 1, kts, -1
           write(dbg_msg,'(f7.2, 2x, f7.2, 2x, f6.4, 2x, f7.3, 2x,  f15.7, 2x, f15.7)') &
     &          T(k)-273.15, P(k)*0.01, rh(k), cldfra(k)*100., qc(k)*1000.,qi(k)*1000.
           CALL wrf_debug (150, dbg_msg)
        enddo
      endif

      if (modify_qvapor) then
         DO k = kts, kte - 1
            if (cldfra(k).gt.0.20 .and. cldfra(k).lt.1.0) then
               qv(k) = MAX(qv(k),qvs(k))
            endif
         ENDDO
      endif

      END SUBROUTINE cal_cldfra3_madwrf

      SUBROUTINE find_k_from_z_agl(z_agl, k_lev, dz, kts, kte)

      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: kts, kte
      REAL, INTENT(IN)     :: z_agl
      INTEGER, INTENT(OUT) :: k_lev
      REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
      INTEGER :: k
      REAL    :: z_this, z_next, z_full_lev

      !! Identify the k-level of a given height AGL, starting from ground level
      z_full_lev = 0.0
      do k = kts, kte
         z_this = z_full_lev
         z_next = z_this + dz(k)
         if (z_agl > 0.0) then
            if (z_agl > z_this .and. z_agl <= z_next) then
               k_lev = k
            end if
         end if
         z_full_lev = z_next
      end do

      END SUBROUTINE find_k_from_z_agl

      SUBROUTINE find_thresh_k_downward(var, var_thresh, k_lev, k_top, k_bot, kts, kte)

      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: kts, kte
      INTEGER, INTENT(IN)  :: k_top, k_bot
      REAL, DIMENSION(kts:kte), INTENT(IN) :: var
      REAL, INTENT(IN)     :: var_thresh
      INTEGER, INTENT(OUT) :: k_lev
      INTEGER :: k

      !! Identify the k-level where the input variable first exceeds a threshold, searching downward over a given range
      do k = k_top, k_bot, -1
         if (var(k) > var_thresh) then
            k_lev = k
            exit
         end if
      end do

      END SUBROUTINE find_thresh_k_downward

      SUBROUTINE find_thresh_k_upward(var, var_thresh, k_lev, k_bot, k_top, kts, kte)

      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: kts, kte
      INTEGER, INTENT(IN)  :: k_top, k_bot
      REAL, DIMENSION(kts:kte), INTENT(IN) :: var
      REAL, INTENT(IN)     :: var_thresh
      INTEGER, INTENT(OUT) :: k_lev
      INTEGER :: k

      !! Identify the k-level where the input variable first exceeds a threshold, searching upward over a given range
      do k = k_bot, k_top
         if (var(k) > var_thresh) then
            k_lev = k
            exit
         end if
      end do

      END SUBROUTINE

!+---+-----------------------------------------------------------------+
!..From cloud fraction array, find clouds of multi-level depth and compute
!.. a reasonable value of LWP or IWP that might be contained in that depth,
!.. unless existing LWC/IWC is already there.

      SUBROUTINE find_cloudLayers(qvs1d, cfr1d, T1d, P1d, Dz1d, entrmnt,&
     &                            debugfl, qc1d, qi1d, qs1d,            &
     &                            tropo_z, kts,kte, keep_clouds_below_lcl, ktropo)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: debugfl, keep_clouds_below_lcl
      REAL, INTENT(IN):: entrmnt
      REAL, INTENT(INOUT):: tropo_z
      REAL, DIMENSION(kts:kte), INTENT(IN):: qs1d,qvs1d,T1d,P1d,Dz1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: cfr1d, qc1d, qi1d
      integer, intent(in), optional :: ktropo

!..Local vars.
      REAL, DIMENSION(kts:kte):: theta
      REAL:: theta1, theta2, delz
      INTEGER:: k, k2, k_tropo, k_m12C, k_m40C, k_cldb, k_cldt, kbot
      LOGICAL:: in_cloud
      character*512 dbg_msg
      logical :: is_tropo_init

!+---+

      if (present(ktropo)) then
        is_tropo_init = .true.
      else
        is_tropo_init = .false.
      end if

      k_m12C = 0
      k_m40C = 0
      DO k = kte - 1, kts, -1
         theta(k) = T1d(k)*((100000.0/P1d(k))**(287.05/1004.))
         if (T1d(k)-273.16 .gt. -40.0 .and. P1d(k).gt.7000.0) k_m40C = MAX(k_m40C, k)
         if (T1d(k)-273.16 .gt. -12.0 .and. P1d(k).gt.10100.0) k_m12C = MAX(k_m12C, k)
      ENDDO
      if (k_m40C .le. kts) k_m40C = kts
      if (k_m12C .le. kts) k_m12C = kts

      if (k_m40C.gt.kte-2 .OR. k_m12C.gt.kte-3) then
        WRITE (dbg_msg,*) 'DEBUG-GT: WARNING, no possible way neg40 or neg12C can occur this high up: ', k_m40C, k_m12C
        CALL wrf_debug (0, dbg_msg)
        do k = kte - 1, kts, -1
           WRITE (dbg_msg,*) 'DEBUG-GT,   P, T : ', P1d(k)*0.01,T1d(k)-273.16
           CALL wrf_debug (0, dbg_msg)
        enddo
        call wrf_error_fatal ('FATAL ERROR, problem in temperature profile.')
      endif

      if (is_tropo_init) then
       k_tropo = ktropo
      else
        call Calc_tropo_height (T1d, P1d, dz1d, kts, &
          kte, debugfl, k_tropo, tropo_z)
      end if

!..Eliminate possible fractional clouds above supposed tropopause.
      DO k = k_tropo+1, kte - 1
         if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) then
!            print *, 'Removing stratospheric cloud', k
            cfr1d(k) = 0.
         endif
      ENDDO

!..We would like to prevent fractional clouds below LCL in idealized
!.. situation with deep well-mixed convective PBL, that otherwise is
!.. likely to get clouds in more realistic capping inversion layer.
      kbot = kts+2
      DO k = kbot, k_m12C
         if ( (theta(k)-theta(k-1)) .gt. 0.010E-3*Dz1d(k)) EXIT
      ENDDO
      kbot = MAX(kts+1, k-2)
      if (.not. keep_clouds_below_lcl) then
        DO k = kts, kbot
!           print *, 'Reducing cloud below LCL', k
           if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) cfr1d(k) = 0.5*cfr1d(k)
        ENDDO
      else
        kbot = kts + 1
      end if


!..Starting below tropo height, if cloud fraction greater than 1 percent,
!.. compute an approximate total layer depth of cloud, determine a total 
!.. liquid water/ice path (LWP/IWP), then reduce that amount with tuning 
!.. parameter to represent entrainment factor, then divide up LWP/IWP
!.. into delta-Z weighted amounts for individual levels per cloud layer. 

      k_cldb = k_tropo
      in_cloud = .false.
      k = k_tropo
      DO WHILE (.not. in_cloud .AND. k.gt.k_m12C+1)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, k_m12C, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.k_m12C) then
                  k_cldb = k2+1
                  goto 87
               endif
            ENDDO
 87         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: An ice cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            call adjust_cloudIce(cfr1d, qi1d, qs1d, qvs1d, T1d, Dz1d,   &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A single-layer ice cloud layer is found on ', k_cldb, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qi1d(k_cldb)=0.05*qvs1d(k_cldb)*cfr1d(k_cldb)*cfr1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO


      k_cldb = k_m12C + 3
      in_cloud = .false.
      k = k_m12C + 2
      DO WHILE (.not. in_cloud .AND. k.gt.kbot)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, kbot, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.kbot) then
                  k_cldb = k2+1
                  goto 88
               endif
            ENDDO
 88         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A water cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            call adjust_cloudH2O(cfr1d, qc1d, qvs1d, T1d, Dz1d,         &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qc1d(k_cldb)=0.05*qvs1d(k_cldb)*cfr1d(k_cldb)*cfr1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO

      END SUBROUTINE find_cloudLayers

      subroutine Calc_tropo_height (T1d, P1d, dz1d, kts, kte, debugfl, k_tropo, tropo_z)

        !..Find tropopause height, best surrogate, because we would not really
        !.. wish to put fake clouds into the stratosphere.  The 10/1500 ratio
        !.. d(Theta)/d(Z) approximates a vertical line on typical SkewT chart
        !.. near typical (mid-latitude) tropopause height.  Since messy data
        !.. could give us a false signal of such a transition, do the check over 
        !.. three K-level change, not just a level-to-level check.  This method
        !.. has potential failure in arctic-like conditions with extremely low
        !.. tropopause height, as would any other diagnostic, so ensure resulting
        !.. k_tropo level is above 700hPa.

        implicit none

        REAL, DIMENSION(kts:kte), INTENT(IN) :: T1d, P1d, Dz1d
        integer, intent(in) :: kts, kte
        LOGICAL, INTENT(IN):: debugfl
        integer, intent(out) :: k_tropo
        real, intent (out) :: tropo_z

          ! Local vars
        integer :: k, k_p200
        real :: theta1, theta2, delz
        character*512 dbg_msg
        REAL, DIMENSION(kts:kte):: theta


        k_p200 = 0
        DO k = kte - 1, kts, -1
          theta(k) = T1d(k)*((100000.0/P1d(k))**(287.05/1004.))
          if (P1d(k).gt.19999.0 .and. k_p200.eq.0) k_p200 = k
        END DO

        if ( (kte-k_p200) .lt. 3) k_p200 = kte-3
        DO k = k_p200-2, kts, -1
           theta1 = theta(k)
           theta2 = theta(k+2)
           delz = 0.5*dz1d(k) + dz1d(k+1) + 0.5*dz1d(k+2)
           if ( (((theta2-theta1)/delz).lt.10./1500.) .OR. P1d(k).gt.70000.) EXIT
        ENDDO
        k_tropo = MAX(kts+2, MIN(k+2, kte-1))

        if (k_tropo .gt. k_p200) then
           DO k = kte-3, k_p200-2, -1
              theta1 = theta(k)
              theta2 = theta(k+2)
              delz = 0.5*dz1d(k) + dz1d(k+1) + 0.5*dz1d(k+2)
              if ( (((theta2-theta1)/delz).lt.10./1500.) .AND. P1d(k).gt.9500.) EXIT
           ENDDO
           k_tropo = MAX(k_p200-1, MIN(k+2, kte-1))
        endif
        tropo_z = SUM(dz1d(kts:k_tropo))

        if (k_tropo.gt.kte-2) then
          WRITE (dbg_msg,*) 'DEBUG-GT: CAUTION, tropopause appears to be very high up: ', k_tropo
          CALL wrf_debug (150, dbg_msg)
          do k = kte - 1, kts, -1
             WRITE (dbg_msg,*) 'DEBUG-GT,   P, T : ', k,P1d(k)*0.01,T1d(k)-273.16
             CALL wrf_debug (150, dbg_msg)
          enddo
        elseif (debugfl) then
          WRITE (dbg_msg,*) 'DEBUG-GT: FOUND TROPOPAUSE k=', k_tropo
          CALL wrf_debug (150, dbg_msg)
        endif

        if (debugfl) then
          print *, 'FOUND TROPOPAUSE k, height=', k_tropo, tropo_z
        end if

      end subroutine Calc_tropo_height

      SUBROUTINE adjust_cloudIce(cfr,qi,qs,qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qs, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qi
      REAL:: iwc, max_iwc, tdz, this_iwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_iwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_iwc = ', max_iwc, ' over DZ=',tdz

      do k = k1, k2
         max_iwc = MAX(1.E-6, max_iwc - (qi(k)+qs(k)))
      enddo
      max_iwc = MIN(1.E-3, max_iwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_iwc = max_iwc*this_dz/tdz
         iwc = MAX(1.E-6, this_iwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.203.16) then
            qi(k) = qi(k) + cfr(k)*cfr(k)*iwc
         endif
      enddo

      END SUBROUTINE adjust_cloudIce

!+---+-----------------------------------------------------------------+

      SUBROUTINE adjust_cloudH2O(cfr, qc, qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc
      REAL:: lwc, max_lwc, tdz, this_lwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_lwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_lwc = ', max_lwc, ' over DZ=',tdz

      do k = k1, k2
         max_lwc = MAX(1.E-6, max_lwc - qc(k))
      enddo
      max_lwc = MIN(1.E-3, max_lwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_lwc = max_lwc*this_dz/tdz
         lwc = MAX(1.E-6, this_lwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.253.16) then
            qc(k) = qc(k) + cfr(k)*cfr(k)*lwc
         endif
      enddo

      END SUBROUTINE adjust_cloudH2O

!+---+-----------------------------------------------------------------+

!..Do not alter any grid-explicitly resolved hydrometeors, rather only
!.. the supposed amounts due to the cloud fraction scheme.

      SUBROUTINE adjust_cloudFinal(cfr, qc, qi, Rho,dz, kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts,kte
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, Rho, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc, qi
      REAL:: lwp, iwp, xfac
      INTEGER:: k

      lwp = 0.
      iwp = 0.
      do k = kts, kte - 1
         if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
            lwp = lwp + qc(k)*Rho(k)*dz(k)
            iwp = iwp + qi(k)*Rho(k)*dz(k)
         endif
      enddo

      if (lwp .gt. 1.0) then
         xfac = 1.0/lwp
         do k = kts, kte - 1
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qc(k) = qc(k)*xfac
            endif
         enddo
      endif

      if (iwp .gt. 1.0) then
         xfac = 1.0/iwp
         do k = kts, kte - 1
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qi(k) = qi(k)*xfac
            endif
         enddo
      endif

      END SUBROUTINE adjust_cloudFinal

  end module module_madwrf
