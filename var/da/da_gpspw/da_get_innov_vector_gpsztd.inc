SUBROUTINE da_get_innov_vector_gpsztd ( it, num_qcstat_conv, grid, ob, iv )
!----------------------------------------------------------------
! Innovation for Ground-based ZTD.
!
! Because we can ONLY assimilate either GPS PW or GPS ZTD,
! never assimilate both of them simultaneously, here we 
! used the PW structure for ZTD to avoid declaration of the
! another structure.          
!                                 Y.-R. Guo           05/21/2008
! History:
!    2017-06: Jamie Bresch
!             (1) reject obs-model height difference larger than Max_StHeight_Diff_ztd
!             (2) properly write out ztd innov info
!             (3) minor clean-up
!----------------------------------------------------------------
   IMPLICIT NONE

   integer,       intent(in)    :: it      ! External iteration.
   type(domain),  intent(in)    :: grid    ! first guess state
   type(y_type),  intent(inout) :: ob      ! Observation structure.
   type(iv_type), intent(inout) :: iv      ! O-B structure.
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: mdl_ztd  ! Model ztd at ob location.

!--------------------------------------------------------------------------
   INTEGER                :: k            ! Index dimension
   REAL                   :: dzd, ddzd    ! adjustment ztd (ref*dz*10**6)
   REAL                   :: obs_terr     ! real terrain height at GPS site [m]
   REAL                   :: model_terr   ! model terrain height at GPS site[m]
   REAL,DIMENSION(kts:kte):: model_ztd    ! model ref at GPS site [kg/kg]
   REAL,DIMENSION(kts:kte):: model_z      ! model z at GPS site [m]
   INTEGER                :: iunit, ierr
   character(len=256)     :: fname


   if ( iv%info(gpspw)%nlocal <= 0 ) return

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpsztd")

   if ( write_iv_gpsztd ) then
      if ( num_fgat_time > 1 ) then
         write(unit=fname, fmt='(i2.2,a,i2.2,a,i4.4)') &
            it,'_inv_gpsztd_t', iv%time, '.', myproc
      else
         write(unit=fname, fmt='(i2.2,a,i4.4)') it,'_inv_gpsztd.', myproc
      end if
      call da_get_unit(iunit)
      open(unit=iunit,file=trim(fname),form='formatted',iostat=ierr)
      write(unit=iunit,fmt='(a4,1x,a4,12a10)')  'ztd:', '   n',   &
                       '       lat', '       lon', '   obs_ght',  &
                       '   mdl_ght', ' obsh-mdlh', '   obs_ztd',  &
                       '   mdl_ztd', '   O-B ztd', '      Dztd',  &
                       '  O-B+Dztd', '   Obs_err', '        qc'
   end if

   do n = iv%info(gpspw)%n1,iv%info(gpspw)%n2

      if ( iv % gpspw(n) % tpw % qc == fails_error_max .and. it > 1 ) then
         iv % gpspw(n) % tpw % qc = 0
      end if

      ! Get horizontal interpolation weights:

      i   = iv%info(gpspw)%i(1,n)
      j   = iv%info(gpspw)%j(1,n)
      dx  = iv%info(gpspw)%dx(1,n)
      dy  = iv%info(gpspw)%dy(1,n)
      dxm = iv%info(gpspw)%dxm(1,n)
      dym = iv%info(gpspw)%dym(1,n)

      ! xb%ztd (cm) is computed in da_transfer_wrftoxb by calling da_transform_xtoztd
      mdl_ztd   = dym*(dxm*grid%xb%ztd(i,j)   + dx*grid%xb%ztd(i+1,j)) + &
                  dy *(dxm*grid%xb%ztd(i,j+1) + dx*grid%xb%ztd(i+1,j+1))

      if ( .not. pseudo_ztd ) iv % gpspw(n) % tpw % inv = 0.0
      if ( ob % gpspw(n) % tpw > missing_r .and. &
           iv % gpspw(n) % tpw % qc >= obs_qc_pointer ) then

         dzd = 0.0
         obs_terr   = iv%info(gpspw)%elv(n)
         model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
                      dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

         if ( obs_terr <= model_terr ) then

            model_ztd(1) = dym*(dxm*grid%xb%ref(i,j,1)   + dx*grid%xb%ref(i+1,j,1)) + &
                           dy *(dxm*grid%xb%ref(i,j+1,1) + dx*grid%xb%ref(i+1,j+1,1))
            dzd = model_ztd(1) * ( obs_terr - model_terr )

         else

            model_z(1) = dym*(dxm*grid%xb%hf(i,j,1)   + dx*grid%xb%hf(i+1,j,1)) + &
                         dy *(dxm*grid%xb%hf(i,j+1,1) + dx*grid%xb%hf(i+1,j+1,1))

            do k = kts, kte

               if (model_z(k) >= obs_terr ) exit

               model_z(k+1) = dym*(dxm*grid%xb%hf(i,j,k+1)   + dx*grid%xb%hf(i+1,j,k+1)) + &
                              dy *(dxm*grid%xb%hf(i,j+1,k+1) + dx*grid%xb%hf(i+1,j+1,k+1))
               model_ztd(k) = dym*(dxm*grid%xb%ref(i,j,k)   + dx*grid%xb%ref(i+1,j,k)) + &
                              dy *(dxm*grid%xb%ref(i,j+1,k) + dx*grid%xb%ref(i+1,j+1,k))

               if ( model_z(k+1) <= obs_terr ) then
                  ddzd = model_ztd(k) * ( model_z(k+1) - model_z(k) )
               else
                  ddzd = model_ztd(k) * ( obs_terr - model_z(k) )
               endif

               dzd = dzd + ddzd
            end do
         end if

         if ( pseudo_ztd .and. it == 1 ) then

            ! compute ob from input inv for pseudo_ztd
            ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv + mdl_ztd - 1.e-4 * dzd

         else

            ! compute inv for ztd
            iv % gpspw(n) % tpw % inv = ob % gpspw(n) % tpw - mdl_ztd &
                                          + 1.e-4 * dzd
!
! Overwrite the observation error specification (YRG):
!
!           iv % gpspw(n) % tpw % error = 1.0 + 0.02*(ob%gpspw(n)%tpw-200.)

         end if !pseudo_ztd
      end if ! valid obs

      if ( abs(obs_terr - model_terr) > Max_StHeight_Diff_ztd ) then
         iv%gpspw(n)%tpw%qc = -66
      end if

      if ( write_iv_gpsztd ) then
         write(unit=iunit, fmt='(a4,1x,i4,11f10.3,i10)') 'ztd:', n, &
              iv%info(gpspw)%lat(1,n), iv%info(gpspw)%lon(1,n), obs_terr, &
              model_terr, obs_terr - model_terr, ob%gpspw(n)%tpw,   &
              mdl_ztd , ob%gpspw(n)%tpw-mdl_ztd, 1.e-4*dzd,       &
              ob%gpspw(n)%tpw-mdl_ztd+1.e-4*dzd, iv%gpspw(n)%tpw%error,&
              iv%gpspw(n)%tpw%qc
      end if

   end do ! n1-n2 loop

   ! Perform optional maximum error check:
   if ( .not. pseudo_ztd .and. check_max_iv ) &
         call da_check_max_iv_gpspw(iv, it, num_qcstat_conv)

   if ( write_iv_gpsztd ) then
      call da_free_unit(iunit)
      close(iunit)
   end if

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsztd")

end subroutine da_get_innov_vector_gpsztd

