SUBROUTINE da_get_innov_vector_gpsztd ( it, num_qcstat_conv, grid, ob, iv )
!----------------------------------------------------------------
! Innovation for Ground-based ZTD.
!
! Because we can ONLY assimilate either GPS PW or GPS ZTD,
! never assimilate both of them simultaneously, here we 
! used the PW structure for ZTD to avoid declaration of the
! another structure.          
!                                 Y.-R. Guo           05/21/2008
!    Updated for Analysis on Arakawa-C grid
!    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
!----------------------------------------------------------------
   IMPLICIT NONE

!-----
!    INCLUDE 'mpif.h'
!-----

   integer,       intent(in)    :: it      ! External iteration.
   type(domain),  intent(in)    :: grid    ! first guess state
   type(y_type),  intent(inout) :: ob      ! Observation structure.
   type(iv_type), intent(inout) :: iv      ! O-B structure.
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: mdl_ztd  ! Model value u at oblocation.
   INTEGER           :: ittpw,ittpwf

!--------------------------------------------------------------------------
   INTEGER                :: k            ! Index dimension
   REAL                   :: dzd, ddzd    ! adjustment pw [mm]
   REAL                   :: obs_terr     ! real terrain height at GPS site [m]
   REAL                   :: model_terr   ! model terrain height at GPS site[m]
   REAL,DIMENSION(kts:kte):: model_ztd    ! model q at GPS site [kg/kg]
   REAL,DIMENSION(kts:kte):: model_z      ! model z at GPS site [m]
   INTEGER                      :: myrank, ierr, unit_gps

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpsztd")

   myrank=0
   unit_gps = myrank + 140
!---------------------------------------------------------------------------

!
! GPS ZTD Pseudo OBS test:
!
   if ( (pseudo_var(1:3) == 'ztd' .and. num_pseudo > 0) .and. &
        it == 1 ) then

! Deallocate:
     if (iv%info(gpspw)%nlocal > 0) then
       write(unit=stdout, fmt='(a,i4)') 'iv%num_gpsztd =', iv%info(gpspw)%nlocal
       deallocate (iv % gpspw)
       deallocate (ob % gpspw)
     endif
     
     use_gpsztdObs = .true.

! Allocate:
     iv %info(gpspw)%nlocal  = num_pseudo
     iv %info(gpspw)%plocal(1)= num_pseudo
     iv %info(gpspw)%ntotal  = num_pseudo
     iv %info(pseudo)%nlocal = 0

     call da_allocate_observations (iv)
     iv%info(gpspw)%n1 = 1
     iv%info(gpspw)%n2 = 1
     allocate (ob % gpspw (1:num_pseudo))
     ob % gpspw(1) % tpw   = 0.0

      write(unit=message(1),fmt='(a,i2)') '==> GPS ZTD pseudo OBS test: num_pseudo=',num_pseudo
      call da_message(message(1:1))
     
     iv % info(gpspw) % x(:,1)  = pseudo_x
     iv % info(gpspw) % y(:,1)  = pseudo_y
     iv % info(gpspw) % i(:,1)  = int(pseudo_x)
     iv % info(gpspw) % j(:,1)  = int(pseudo_y)
     iv % info(gpspw) % dx(:,1) = pseudo_x-real(iv % info(gpspw) % i(1,1))
     iv % info(gpspw) % dy(:,1) = pseudo_y-real(iv % info(gpspw) % j(1,1))
     iv % info(gpspw) % dxm(:,1)= 1.0 - iv % info(gpspw) % dx(1,1)
     iv % info(gpspw) % dym(:,1)= 1.0 - iv % info(gpspw) % dy(1,1)

     iv % gpspw(1) % tpw % inv   = pseudo_val
     iv % gpspw(1) % tpw % qc    = 0
     iv % gpspw(1) % tpw % error = pseudo_err

! To consider the site elevation effect, set the model terrain height
! to elevation for pseudo OBS:

     i   = iv%info(gpspw)%i(1,1)
     j   = iv%info(gpspw)%j(1,1)
     dx  = iv%info(gpspw)%dx(1,1)
     dy  = iv%info(gpspw)%dy(1,1)
     dxm = iv%info(gpspw)%dxm(1,1)
     dym = iv%info(gpspw)%dym(1,1)

     iv%info(gpspw)%elv(1) = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + & 
                            dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

! Set halo:
     if((iv%info(gpspw)%i(1,1) < its-1) .or. (iv%info(gpspw)%i(1,1) > ite) .or. & 
        (iv%info(gpspw)%j(1,1) < jts-1) .or. (iv%info(gpspw)%j(1,1) > jte)) then 
        call da_error(__FILE__,__LINE__,(/"Should never have obs outside halo by now"/))
        iv%info(gpspw)%proc_domain(:,1) = .false. 
     else 
        iv%info(gpspw)%proc_domain(:,1) = .false.  
     
        if(iv%info(gpspw)%i(1,1) >= its .and. iv%info(gpspw)%i(1,1) <= ite .and. &  
           iv%info(gpspw)%j(1,1) >= jts .and. iv%info(gpspw)%j(1,1) <= jte) then  
           iv%info(gpspw)%proc_domain(:,1) = .true.  
        endif  
     endif 

     write(unit=message(1),fmt='(a,2f15.5)') pseudo_var, pseudo_val, pseudo_err
     write(unit=message(2),fmt='(3f15.5)')   pseudo_x,  pseudo_y,  pseudo_z
     write(unit=message(3),fmt='(a,f8.2)')   'iv%gpsztd: elv=',iv%info(gpspw)%elv(1)
     call da_message(message(1:3))
   end if

! ----------------------------------------------------------------------------

   if ( iv%info(gpspw)%nlocal > 0 ) then

   ittpw   = 0 ; ittpwf  = 0

    write(unit=unit_gps,fmt='(3x,a3,12a10)') ' n ','     lat  ',  &
                       '     lon  ', '  obs ght ', '  mdl ght ',  &
                       ' obsh-mdlh', '   obs ztd', ' model ztd',  &
                       '   O-B ztd', '    Dztd  ', '  O-B+Dztd',  &
                       '   Obs_err', '    qc    ' 

      do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2

         if( iv % gpspw(n) % tpw % qc == fails_error_max .and. it > 1) &
             iv % gpspw(n) % tpw % qc = 0

!        [1.1] Get horizontal interpolation weights:

         i   = iv%info(gpspw)%i(1,n)
         j   = iv%info(gpspw)%j(1,n)
         dx  = iv%info(gpspw)%dx(1,n)
         dy  = iv%info(gpspw)%dy(1,n)
         dxm = iv%info(gpspw)%dxm(1,n)
         dym = iv%info(gpspw)%dym(1,n)

         mdl_ztd   = dym*(dxm*grid%xb%ztd(i,j)   + dx*grid%xb%ztd(i+1,j)) + &
                     dy *(dxm*grid%xb%ztd(i,j+1) + dx*grid%xb%ztd(i+1,j+1))

! To compute the 'inv':
         if ( .not.(pseudo_var(1:3) == 'ztd' .and. num_pseudo > 0) ) &
         iv % gpspw(n) % tpw % inv = 0.0
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

            if ( (pseudo_var(1:3) == 'ztd' .and. num_pseudo > 0) .and. it == 1 ) then

! To compute the 'ob':
              ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv + mdl_ztd - 1.e-4 * dzd

            else


              iv % gpspw(n) % tpw % inv = ob % gpspw(n) % tpw - mdl_ztd &
                                          + 1.e-4 * dzd
!
! Overwrite the observation error specification (YRG):
!
!              iv % gpspw(n) % tpw % error = 1.0 + 0.02*(ob%gpspw(n)%tpw-200.)   

            end if
         endif
!---   
        write(unit=unit_gps, fmt='(i4,11f10.3,i7)') n, &
              iv%info(gpspw)%lat(1,n), iv%info(gpspw)%lon(1,n), obs_terr, &
              model_terr, obs_terr - model_terr, ob%gpspw(n)%tpw,   &
              mdl_ztd , ob%gpspw(n)%tpw-mdl_ztd, 1.e-4*dzd,       &
              ob%gpspw(n)%tpw-mdl_ztd+1.e-4*dzd, iv%gpspw(n)%tpw%error,&
              iv%gpspw(n)%tpw%qc
!---   
      end do

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------
      if ( .not.(pseudo_var(1:3) == 'ztd' .and. num_pseudo > 0) .and. check_max_iv ) &
         call da_check_max_iv_gpspw(iv, it, num_qcstat_conv)
   end if

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsztd")

end subroutine da_get_innov_vector_gpsztd

