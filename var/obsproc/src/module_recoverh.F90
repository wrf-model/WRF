MODULE module_recoverh

!------------------------------------------------------------------------------!
! Recover observation height, when missing, based on pressure.
!
! Y.-R. GUO, September 2000 
!------------------------------------------------------------------------------!

   USE module_type
   USE module_func
   USE module_per_type
   USE module_mm5

   INCLUDE 'missing.inc'

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE recover_height_from_pressure(max_number_of_obs , &
                             obs , number_of_obs, print_hp_recover)

!  This routine recovers the missing heights based on the pressure
!  under the hydrostatic assumption, or the model reference state.
!  for the multi-level OBS data (SOUND, AIREP, etc.).
!

   IMPLICIT NONE

   INTEGER,       INTENT ( IN )                :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs):: obs
   INTEGER , INTENT ( IN )                     :: number_of_obs
   LOGICAL , INTENT ( IN )                     :: print_hp_recover

   TYPE (measurement), POINTER                 :: current
   INTEGER                                     :: iunit     
   INTEGER                                     :: qc_flag
   INTEGER                                     :: i, j, nlevel, k, &
                                                  k_start, k_top
   CHARACTER (LEN = 80)                        :: filename
   CHARACTER (LEN = 80)                        :: proc_name = &
                                                 "recover_height_from_pressure"
   LOGICAL                                     :: connected, correct, failed
   INTEGER                                     :: io_error


   TYPE (field)  , dimension(9000)              :: hh
   REAL          , dimension(9000)              :: pp, tt, qq

   INCLUDE 'platform_interface.inc'

!------------------------------------------------------------------------------!
             WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'HEIGHT RECOVERED FROM P, T, Q,..:'
!

      !  Open diagnostic file

      IF (print_hp_recover) THEN

      filename = 'obs_recover_height.diag'
      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

      IF (print_hp_recover) &
      WRITE (UNIT = IUNIT, FMT = '(/A/)') &
        'HEIGHT RECOVERED FROM PRESSURE FOR MULTI-LEVEL OBS DATA:'

      failed = .false.

! 1.  ESTIMATE H
! ==============

      j = 0

! 1.1 Loop over obs
!     -------------

loop_all: &
      DO i = 1, number_of_obs

         IF ((obs (i) % info % discard)  .OR. .NOT. ASSOCIATED &
             (obs (i) % surface)) THEN

             CYCLE loop_all

         ENDIF


! 2.  SINGLE LEVEL OBS
! ====================

surface:&
         IF ((ASSOCIATED (obs (i) % surface)) .AND. &
        (.NOT.ASSOCIATED (obs (i) % surface % next))) THEN

             ! IF height is missing, pressure should be present

             IF (eps_equal (&
                 obs (i) % surface % meas % height % data, missing_r, 1.)) THEN

                 obs (i) % surface % meas % height   % data = ref_height &
                (obs (i) % surface % meas % pressure % data)
                 obs (i) % surface % meas % height   % qc   = Reference_atmosphere

                 obs (i) % surface % meas % height % data = NINT &
                (obs (i) % surface % meas % height % data + .5)
                 obs (i) % surface % meas % height % data = MAX &
                (obs (i) % surface % meas % height % data, 0.)


                 IF (print_hp_recover) THEN

                     WRITE (UNIT = iunit,FMT = '(/,A,A5,1X,A23,2F9.3)')        &
                    "Recover 1 level  station id = ",                          &
                     TRIM  (obs (i) % location % id ) ,                        &
                     TRIM  (obs (i) % location % name),                        &
                            obs (i) % location % latitude,                     &
                            obs (i) % location % longitude
                     WRITE (UNIT = iunit, FMT = '(2(A,I5),A)')                 &
                    "Use reference state to infer height (",                   &
                     INT (obs (i) % surface % meas % height % data),           &
                     "m) from pressure (",&
                     INT (obs (i) % surface % meas % pressure % data/100.),"hPa)."
                 ENDIF

             ENDIF

             !  Recover station elevation for single level obs

!            IF (eps_equal (&
!                obs (i) % info % elevation, missing_r, 1.)) THEN
!                obs (i) % info % elevation =  &
!                obs (i) % surface % meas % height % data
!            ENDIF

             CYCLE loop_all

         ENDIF surface

! 3. MULTI LEVEL OBS
! ==================
! Y.-R. Guo (10/25/2005):.......
         call reorder(obs(i), i, 'pressure', failed)
         if (failed) then
            obs(i) % info % discard = .true.
            cycle loop_all
         endif

! ..........................................
! 3.1 Get the OBS profile and count the number of levels
!     --------------------------------------------------

         nlevel  = 0
         correct = .FALSE. 
         current => obs(i)%surface

count_level_1:&
         DO WHILE (ASSOCIATED (current))

            nlevel = nlevel + 1

            hh (nlevel) = current%meas%height
            pp (nlevel) = current%meas%pressure%data
            tt (nlevel) = current%meas%temperature%data
            qq (nlevel) = current%meas%qv%data

            IF (eps_equal(current%meas%height%data, missing_r, 1.)) THEN
                correct = .TRUE. 
            ENDIF

            current => current%next

         ENDDO count_level_1

! 3.2 If all levels have height, go to next station
!     ---------------------------------------------

         IF (.not.correct) CYCLE loop_all


! 3.2 Otherwise recover missing height for upper-level
!     ------------------------------------------------

levels:&
         IF (nlevel <= 1) THEN

             IF (print_hp_recover) THEN
                 WRITE (UNIT = iunit , FMT = '(A,A5,1X,A23,2F9.3)')     &
                "No level found for sound id= " ,                       &
                 TRIM  (obs (i) % location % id ) ,                     &
                 TRIM  (obs (i) % location % name),                     &
                        obs (i) % location % latitude,                  &
                        obs (i) % location % longitude
             ENDIF

             STOP 'in recover_height.F90'

         ELSE IF (nlevel > 1) THEN levels

             CALL recover_h_from_ptq (pp, tt, qq, hh, nlevel,k_start,k_top)

             IF (k_start >= 1 .or. k_top <= nlevel) THEN

                 IF (print_hp_recover) &
                     WRITE (UNIT = iunit, FMT = '(/,A,A5,1X,A23,2F9.3)') &
                    "Recover upperair station id = ",                    &
                     TRIM  (obs (i) % location % id ) ,                  &
                     TRIM  (obs (i) % location % name),                  &
                            obs (i) % location % latitude,               &
                            obs (i) % location % longitude

                 ENDIF

         ENDIF levels

! 3.  CORRECT OBS
! ===============

         k = 0 
         current => obs(i)%surface

correct_levels: &
         DO WHILE (ASSOCIATED (current))

            k = k + 1

!            IF (eps_equal(current%meas%height%data, missing_r, 1.)) THEN

                 IF (print_hp_recover) THEN
                    WRITE (UNIT = iunit, FMT = '(2(A,I5),A)')    &
                   "Height missing set to ",INT (hh (k) % data), &
                   "m, pressure = ",INT (pp(k)/100.),"hpa."
                 ENDIF

                 current%meas%height % data = CEILING (hh(k)%data) 
                 current%meas%height % qc   =          hh(k)%qc 

!            ENDIF

            current => current%next


         ENDDO correct_levels

         call reorder(obs(i), i, 'pressure', failed)
         if (failed) then
            obs(i) % info % discard = .true.
            cycle loop_all
         endif

! 3.4 Go to next station
!     ------------------

      ENDDO loop_all

     IF (print_hp_recover) CLOSE (IUNIT)

END SUBROUTINE recover_height_from_pressure

!----------------------------------------------------------------------- 

 SUBROUTINE recover_h_from_ptq(P,T,Q,HGT,KXS,K_START,K_TOP)
!----------------------------------------------------------------------- 
! To recover the missing heights for the MULTI-LEVEL (KXS>1) OBS data
!     
! (1) To compute the heights based on pressure(P), and available
!     temperature(T) and specific humidity(Q) under the hydrostatic
!     assumption.
!
!     Using the available observed heights to calibrate the computed
!     heights, then these calibrated and computed heights are used
!     where the observed heights are missing.
!
! (2) When the hydrostatic assumption can not be applied at the bottom
!     (K_START > 1) or top (K_TOP < KXS) part of the atmosphere, the 
!     model reference state is used to derive the missing heights based 
!     on the observed pressure with the calibration from the available 
!     observed heights.
!
! Note:
!
!   In case (1), the derived heights have very high accuracy since only
!     a high accurate "hydrostatic assumption" used here.
!
!   In case (2), the model reference state parameters are used. These 
!     parameters are consistent with the model BKG, and independent of
!     the model domain settings. Of course, the model reference state
!     atmosphere is not a real atmosphere, but it's consistent with the model.
!     With the observed heights calibrated, these derived heights
!     also included some information from OBS (pressure and height).
!
!   This subroutine can not be applied to the SIGLE-LEVEL(KXS=1) data. 
!   For those OBS, the missing heights can only be derived from the 
!   observed pressure and the model BKG and reference state (subroutine 
!   recover_hpt).
!
!                                    Yong-Run Guo
!                                      11/18/00
!
!-------------------------------------------------------------------------

  IMPLICIT NONE
 
  INTEGER,                      INTENT(in)    :: KXS 
  REAL        , DIMENSION(KXS), INTENT(in)    :: T, Q, P
  TYPE (field), DIMENSION(KXS), INTENT(inout) :: HGT
  INTEGER,                      INTENT(out)   :: k_start,k_top

  INTEGER                :: k, kk, kwk, L, K0, K1, K2
  REAL                   :: height0, height1, diff_hp, &
                            TM1, TM2, ALNP, DDH, DDP, DHH, &
                            dh1, dh2, dp1, dp2, aa, bb, cc
  INTEGER,dimension(9000) :: KP
  REAL   ,DIMENSION(9000) :: TWK, QWK, PWK, HWK, DHGT
  LOGICAL                :: Vert_ok
  
  TYPE (field), DIMENSION(KXS) :: HGT0, HGT1

  include 'constants.inc'
! -------------------------------------------------------------------

     HGT0 = HGT

! .. get data at the first levels where both pressure and height
!    available
!

!    Find the first level with height, pressure and temperature
     K_top   = kxs+1
     K_start = 0

first_level: &
     DO k = 1, kxs

        IF (.NOT. eps_equal(P  (k)     , missing_r, 1.) .AND. &
            .NOT. eps_equal(HGT(k)%data, missing_r, 1.) .AND. &
            .NOT. eps_equal(T  (k)     , missing_r, 1.)) THEN
             K_start = k
             EXIT first_level
        ENDIF

     ENDDO first_level

    !
    ! Obs without temperature and/or height are processed here
    ! 
     IF (K_start == 0) THEN

        DO k = 1, kxs

           IF (.NOT. eps_equal(P  (k)     , missing_r, 1.) .AND. &
                     eps_equal(HGT(k)%data, missing_r, 1.)) THEN

                               HGT(k)%data = ref_height (P  (k) )
                               HGT(k)%qc   = reference_atmosphere
           ENDIF

        ENDDO

    ! To avoid the duplicated heights between the computed and 
    ! observed heights:

1999    Vert_ok = .True.
        
        DO k = 2, kxs
          if ((P(k) < P(K-1)) .and. (HGT(k)%data > HGT(k-1)%data)) then
            cycle
          else
            Vert_ok = .False. 
            if (HGT(k)%qc <= 0) then
            ! HGT at level k-1 is computed:
               if (k > 2) then
                  AA = P(k  )-P(k-2)
                  BB = P(k-1)-P(k-2)
                  CC = AA - BB
                  HGT(k-1)%data = (HGT(k)%data*BB + HGT(k-2)%data * CC) / AA
! Y.-R. Guo (10/25/2005), Y.-R. Guo (01/16/2006):
                  if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc
!     print '(" Computed k=",i3,"  pp,hh,qc:",2f11.2,i8)', k-1,p(k-1),HGT(k-1)%data, HGT(k-1)%qc

               else
                  if ( k <= kxs-1 ) then
                    AA = P(k+1) - P(k)
                    BB = P(k-1) - P(k)
                    CC = AA - BB
                    HGT(k-1)%data = (HGT(k+1)%data*BB + HGT(k)%data * CC) / AA
                  else
! Y.-R. Guo (1/31/2008): must be processed separately when k >= kxs 
                    AA = HGT(k)%data - ref_height (P  (k) )
                    HGT(k-1)%data = HGT(k-1)%data + AA
                  endif
! Y.-R. Guo (10/25/2005), Y.-R. Guo (01/16/2006):
                  if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc
               endif
            else
            ! HGT at level k is computed
               if ( k <= kxs-1 ) then
                 AA = P(k+1) - P(k-1)
                 BB = P(k)   - P(k-1)
                 CC = AA - BB
                 HGT(k)%data = (HGT(k+1)%data*BB + HGT(k-1)%data * CC) / AA
               else
! Y.-R. Guo (1/31/2008): must be processed separately when k >= kxs 
                 AA = HGT(k-1)%data - ref_height (P  (k-1) )
                 HGT(k)%data = HGT(k)%data + AA
               endif
! Y.-R. Guo (10/25/2005), Y.-R. Guo (01/16/2006):
               if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc
            endif
          endif
        ENDDO
        if (.Not. Vert_ok) goto 1999

        K_start = 1
        K_top   = kxs

! Y.-R. Guo (10/25/2005):
        DO k = 1, kxs
           HGT(k)%qc   = abs(HGT(k)%qc)
!           print '(i3,"  p,h,qc:",2f12.2,i8)', k, p(k), HGT(k)%data, HGT(k)%qc
        ENDDO

        RETURN

     ENDIf

!  .. if k_start > 1, using the model reference state to get
!     the height at any level k below the level k_start, 
!     keep the height difference between level k_start and level k
!     same as between the observed height and the model reference height
!     at level k_start

     IF (k_start > 1) THEN

         !  Model reference height

         height1 = Ref_height (p(k_start))

         DO k = k_start-1, 1, -1

           !  Missing height

           IF (eps_equal (hgt(k)%data, missing_r, 1.)) THEN

               IF (p(k)-p(k_start) > 20000.) THEN

               ! too far below (200mb) the level where the OBS height available

                  HGT(k)%data  = Ref_height(P(k))
                  HGT(k)%qc    = reference_atmosphere

               ELSE

                  HGT(k)%data  = HGT(k_start)%data - height1 &
                               + Ref_height(P(k))
                  HGT(k)%qc    = reference_OBS_scaled

               ENDIF

          ENDIF

       ENDDO

     ENDIF

     !  Use the hydrostatic equation to correct the heigt

     kwk = 0

temp_search: &
      DO k = k_start, KXS

       IF (.NOT.eps_equal(T(k), missing_r, 1.)) THEN
            kwk = kwk+1
            pwk (kwk) = P(k)
            twk (kwk) = T(k)
            qwk (kwk) = q(k)
      ENDIF

     ENDDO temp_search

     HWK(1) = HGT(k_start)%data * G

!     ... INTEGRATE HYDROSTATIC EQN
!
hydro_int:  &
      DO K=2,KWK

         ALNP = ALOG (PWK(K-1) /PWK(K)) * gasr

         IF (.NOT.eps_equal(QWK(k), missing_r, 1.)) THEN
              TM2 = TWK(K  )*(1.+0.608*QWK(K  ))
         ELSE
              TM2 = TWK(K  )
         ENDIF

         IF (.NOT.eps_equal(QWK(k-1), missing_r, 1.)) THEN
              TM1 = TWK(K-1)*(1.+0.608*QWK(K-1))
         ELSE
              TM1 = TWK(K-1)
         ENDIF
              HWK(K) = HWK(K-1) + .5*(TM1+TM2) * ALNP
      ENDDO hydro_int

!
! .. CALIBRATION OF THE HWK BASED ON THE AVAILABLE HGT:

      K0 = 1
      KP(1) = 1
      DHGT(1) = 0

calibration: &
      DO K = 1,KXS
         IF (eps_equal(HGT(K)%data, missing_r, 1.)) then
          ! nothing
        ELSE
          DO KK = 1,KWK
          IF (P(K).EQ.PWK(KK)) THEN
            K0 = K0+1
            KP(K0) = KK
            DHGT(K0) = HWK(KK)/G - HGT(K)%data
            CYCLE calibration
          ENDIF
          ENDDO
        ENDIF

     ENDDO calibration

!  .. levels between k_start(K0=1) and K0-1
!
     DO L = 1,K0-1
        K1 = KP(L)
        K2 = KP(L+1)
        DO KK = K1,K2-1
          DDH = DHGT(L+1) - DHGT(L)
          DDP = ALOG(PWK(K2)/PWK(K1))
          DHH = DHGT(L) + ALOG(PWK(KK)/PWK(K1))*DDH/DDP
          HWK(KK) = HWK(KK)/G - DHH
        END DO
     END DO
!
!  .. Above the level KP(K0):

     DO K = KP(K0),KWK
         HWK(K) = HWK(K)/G - DHGT(K0)
     END DO
!     WRITE(0,'(I3,2X,4E15.5)') &
!        (L,PWK(L),TWK(L),QWK(L),HWK(L),L=1,KWK)

!
! .. FILL BACK THE HEIGHTS AT THE LEVELS WHERE TEMPERATURE AVAILABLE:

     height1 = Ref_height(Pwk(kwk))

above_k_start: DO K = k_start,KXS

     if (abs(P(K) - PWK(KWK)) < 0.01) k_top = k

!     if (eps_equal(HGT(k)%data, missing_r, 1.)) then

      IF (P(K) >= PWK(KWK)) then
  
        DO KK = 1,KWK
          IF (P(K).EQ.PWK(KK)) THEN
             HGT(K)%data = HWK(KK)
          ELSE IF (KK.LT.KWK .AND. &
                 P(K).LT.PWK(KK) .AND. P(K).GT.PWK(KK+1)) THEN
!
! .. Get the interpolated heights at the levels temperature unavailable:

            ALNP = ALOG (P(K)/PWK(KK)) / ALOG(PWK(KK+1)/PWK(KK))
            HGT(K)%data = HWK(KK) + ALNP*(HWK(KK+1)-HWK(KK))
          ENDIF
        ENDDO
        HGT(k)%qc    =  Hydrostatic_recover
      ELSE
        if ((PWK(KWK)-P(K)) > 10000.) then

     ! too far above (100mb) the level where the OBS height available

           HGT(k)%data  = Ref_height(P(k))
           HGT(k)%qc    = reference_atmosphere
        else
           HGT(k)%data  = Hwk(kwk) - height1 &
                        + Ref_height(P(k))
           HGT(k)%qc    = reference_OBS_scaled
        endif
      ENDIF

!     endif

    ENDDO above_k_start

!
! Non-hydrastatic adjustment:
!
! In case of only P and H observed without T, TD, the hydrostatic
! height may be different from the observed H. We must keep this
! observed H, and adjust the calculated heights at the adjacent 
! levels to avoid inconsistency between P and H when having
! the dense observed levels.
!                                
!                                 Yong-Run Guo  12/06/2001
! ------------------------------------------------------------  
! .. Find the starting level with the observed height:
!                                     Fixed the bug on 07/08/2004
    k0 = 1
    do k = 1,kxs
      if (HGT0(k) % qc == 0) then
        k0 = k
        exit
      endif
    enddo
! ------------------------------------------------------------   
    k1 = 0
! .. Keep the calculated height:
    HGT1 = HGT
    do k = k0, kxs

! .. Find the starting level (k1+1) without the observed height:
      if (HGT0(k) % qc /= 0 .and. k1 == 0) then
         k1 = k-1
      else if (HGT0(k) % qc == 0 .and. &
               abs(HGT0(k)%data - HGT(k)%data) <= 0.10*HGT(k)%data ) then
         HGT1(k) = HGT0(k)
      endif

! .. Find the ending level (k2-1) without the observed height:
      if (HGT0(k) % qc == 0 .and. k1 > 0 .and. k > k1+1) then
         k2 = k

         if (abs(HGT0(k2)%data - HGT(k2)%data) > 0.10*HGT(k2)%data) then

! ....When the difference between the observed height and retrieved height
!     greater than 10% of Hydro. Retrv. height, ignore the observed height,
!     and no adjustment done:

           HGT0(k) = HGT1(k)

         else

           HGT1(k) = HGT0(k)

! .. Adjust the calculated hydristatic heights from level k1+1 to k2-1:
         dp2 = p(k2) - p(k1)
         dh1 = HGT0(k1)%data - HGT(k1)%data
         dh2 = HGT0(k2)%data - HGT(k2)%data
         do kk = k1+1, k2-1
           dp1 = p(kk) - p(k1) 
           HGT1(kk)%data = HGT(kk)%data + dh1*(1-dp1/dp2) + dh2*dp1/dp2
           HGT1(kk)%qc   = HGT(kk)%qc
           HGT1(kk)%error= HGT(kk)%error 
         enddo
        endif
         k1 = 0
         k2 = 0
      endif

    enddo

! .. Fill back to HGT with HGT1:
    HGT = HGT1

 END subroutine recover_h_from_ptq
!------------------------------------------------------------------------------!

 SUBROUTINE reorder(obs, i, order_component, failed)
!------------------------------------------------------------------------------!

  IMPLICIT NONE

  INTEGER,       INTENT (in)    :: i
  CHARACTER*(*), INTENT (in)    :: order_component 
  TYPE (report), INTENT (inout) :: obs
  logical                       :: failed

  INCLUDE 'missing.inc'

  TYPE (measurement), pointer :: current, new, tmp, pre
!
  integer :: num, ii, num_loop, nlevels
  logical :: need_check

  failed = .false.
 
  ii = 0
  current => obs % surface
  count_levels:  do while (associated(current))
     ii = ii + 1
     current => current%next
  enddo count_levels
  nlevels = ii

  current => obs%surface

!--Missing Check:

   num = 0
   ii  = 0

   missing_check: do while (associated(current))
      num = num + 1
      if(eps_equal(comp_field(current,order_component), &
                                       missing_r, 1.0)) then
! remove the missing data link:
         num=num-1
      end if
      current => current%next
   end do missing_check

   if (num /= nlevels) then
     write(0,'(/I5,A,A,A/3X,A,A,A,A,A,2I5,A,f10.3,A,f10.3)') I, &
        ' There are ',order_component,&
        ' at several levels missing. Reordering can not be done.', &
        ' FM=',obs%info%platform(4:5),' ID=',obs%location%id(1:5), &
        ' nlevels, num:', nlevels, num, &
        ' lat=', obs%location%latitude, ' long=', obs%location%longitude
!     STOP 'in_reorder'
     failed = .true.
   endif

!  Re-set the number of levels to OBS

   obs % info % levels = nlevels

   if (nlevels <= 1) return

   ii = 0
   num_loop = 0

   need_check = .true.

   put2order: do while(need_check)

   num_loop = num_loop + 1

      head_check: do while(need_check)

         current => obs%surface

!--Head Check

         if(comp_field(current,order_component) < &
            comp_field(current%next,order_component)) then
!--------Cut the first on off and put it in pointer tmp:
            tmp => current
            current => current%next
            nullify(tmp%next)

            new  => current

            obs%surface => new

!--------if first pressure is not good, through tmp away

            if(eps_equal(comp_field(current,order_component), &
                                            missing_r, 1.0)) then
               num=num-1
          ! Don't allow happened
               STOP 'in reorder_missing data'
!               cycle head_check
            end if

!--------Now we need to put pointer tmp to a place.

            current => obs%surface

            new => obs%surface%next

            nullify(current%next)

            allocate(current%next)
            current%next => tmp
   
            tmp%next => new
         end if

         need_check = .false.
      end do head_check

      if(num < 3) exit put2order

!     write(0,'(/)')

!     current => obs%surface

!     ii=0

!     do while (associated(current))
!        ii = ii + 1
!        write(0,'(A,I3,A,I3,3f12.2)') 'num_loop=',num_loop, & 
!              '  I,P,H,T:', ii, &
!              current%meas%pressure%data,  current%meas%height%data, &
!              current%meas%temperature%data
!        current => current%next
!     end do

!-----Put others in order

      pre     => obs%surface
      current => obs%surface%next
      new     => obs%surface%next%next

      do while (associated(new))
         if(comp_field(current,order_component) < &
            comp_field(current%next,order_component)) then

            tmp => new%next

            nullify(pre%next)
            nullify(current%next)
            nullify(new%next)

            current%next => tmp
            new%next => current
            pre%next => new

            need_check = .true.

            exit
         end if

         pre     =>     pre%next
         new     =>     new%next
         current => current%next
      end do

!     write(0,'(/)')

!     current => obs%surface

!     ii=0

!     do while (associated(current))
!        ii = ii + 1
!        write(0,'(A,I3,A,I3,3f12.2)') 'num_loop=',num_loop, & 
!              '  I,P,H,T:', ii, &
!              current%meas%pressure%data,  current%meas%height%data, &
!              current%meas%temperature%data
!        current => current%next
!     end do
   end do put2order

!   write(0,'(//)')

!   current => obs%surface

!   ii=0
!   do while (associated(current))
!      ii = ii + 1
!      write(0,'(A,I3,3f12.2)') 'I,P,H,T:', ii, &
!            current%meas%pressure%data,  current%meas%height%data, &
!            current%meas%temperature%data
!       current => current%next
!   end do

 END subroutine reorder
!------------------------------------------------------------------------------!

 FUNCTION comp_field(current, order_component) result(xxx)
!------------------------------------------------------------------------------!

type (measurement), pointer :: current
character*(*),   intent(in) :: order_component
real                        :: xxx
 
     SELECT CASE (order_component)

       CASE ('pressure')

         xxx = current%meas%pressure%data

       CASE ('height')

         xxx =  current%meas%height%data
       
       CASE DEFAULT

         WRITE(0,'(A,A,A)') 'order_component=',order_component, &
                            ' is not defined correctly'
         STOP 'in_reorder'

     END SELECT

 END function comp_field
!------------------------------------------------------------------------------!
  
END MODULE module_recoverh
