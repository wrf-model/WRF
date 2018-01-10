subroutine da_setup_pseudo_obs(grid, iv, ob)

   !-------------------------------------------------------------------------
   ! Purpose: Sets up pseudo ob part of observation structure.
   !-------------------------------------------------------------------------

   implicit none

   type(domain),     intent(in)    :: grid ! Model data
   type(iv_type),    intent(inout) :: iv   ! Obs and header structure.
   type(y_type),     intent(inout) :: ob   ! (Smaller) observation structure.

   integer :: i, j
   logical :: outside
   real    :: dx, dy, dxm, dym

   if ( num_pseudo <= 0 ) return

   if (trace_use_dull) call da_trace_entry("da_setup_pseudo_obs")

   write(unit=message(1),fmt='(a,a)')'==> pseudo OBS test:'
   write(unit=message(2),fmt='(a,a10)')    'pseudo_var = ', trim(pseudo_var)
   write(unit=message(3),fmt='(a,f10.5)') 'pseudo_val = ', pseudo_val
   write(unit=message(4),fmt='(a,f10.5)') 'pseudo_err = ', pseudo_err
   write(unit=message(5),fmt='(a,f10.5)') 'pseudo_x   = ', pseudo_x
   write(unit=message(6),fmt='(a,f10.5)') 'pseudo_y   = ', pseudo_y
   write(unit=message(7),fmt='(a,f10.5)') 'pseudo_z   = ', pseudo_z
   call da_message(message(1:7))

   if ( pseudo_tpw .or. pseudo_ztd .or. pseudo_ref ) then
      ob%nlocal(pseudo)               = 0
      iv%info(pseudo)%nlocal          = 0
      iv%info(pseudo)%ntotal          = 0
      iv%info(pseudo)%plocal(iv%time) = 0
      iv%info(pseudo)%ptotal(iv%time) = 0
   end if

   iv%time = 1

   ! Find out if pseudo ob is local
   outside = .false.
   i = int(pseudo_x)
   j = int(pseudo_y)
   if (fg_format == fg_format_kma_global) then
      if ((j < jts-1) .or. (j > jte)) outside = .true.
   else
      if ((i < ids)   .or. (i >= ide) .or. (j < jds)   .or. (j >= jde)) outside = .true.
      if ((i < its-1) .or. (i >  ite) .or. (j < jts-1) .or. (j >  jte)) outside = .true.
   end if

   ! setup gpsref structure
   if ( pseudo_ref ) then
      iv%info(gpsref)%ptotal(1) = num_pseudo
      iv%info(gpsref)%ntotal    = num_pseudo
      if (outside) then
         ob%nlocal(gpsref)               = 0
         iv%info(gpsref)%nlocal          = 0
         iv%info(gpsref)%plocal(iv%time) = 0
      else
         ob%nlocal(gpsref)         = num_pseudo
         iv%info(gpsref)%nlocal    = num_pseudo
         iv%info(gpsref)%plocal(1) = num_pseudo
         iv%info(gpsref)%n1        = 1
         iv%info(gpsref)%n2        = 1
         iv%info(gpsref)%max_lev   = 1
         allocate(iv%gpsref(1:iv%info(gpsref)%nlocal))
         call da_allocate_obs_info(iv, gpsref)
         iv%info(gpsref)%proc_domain(:,:) = .true.
         iv%info(gpsref)%obs_global_index(iv%info(gpsref)%nlocal) = 1
         allocate(iv%gpsref(num_pseudo)%ref(1:1))
         allocate(iv%gpsref(num_pseudo)%  h(1:1))
         allocate(iv%gpsref(num_pseudo)%  p(1:1))
         allocate(iv%gpsref(num_pseudo)%  t(1:1))
         allocate(iv%gpsref(num_pseudo)%  q(1:1))
         allocate(ob%gpsref(1:num_pseudo))
         allocate(ob%gpsref(num_pseudo)%ref(1:num_pseudo))
         iv%info(gpsref)%levels(1) = 1
         iv%info(gpsref)%x(:,1)   = pseudo_x
         iv%info(gpsref)%y(:,1)   = pseudo_y
         iv%info(gpsref)%zk(:,1)  = pseudo_z
         iv%info(gpsref)%i(:,1)   = int(pseudo_x)
         iv%info(gpsref)%j(:,1)   = int(pseudo_y)
         iv%info(gpsref)%k(:,:)   = int(pseudo_z)
         iv % gpsref(1) %  h(1)   = pseudo_z
         iv%info(gpsref)%dx(:,1)  = pseudo_x-real(iv%info(gpsref)%i(1,1))
         iv%info(gpsref)%dy(:,1)  = pseudo_y-real(iv%info(gpsref)%j(1,1))
         iv%info(gpsref)%dxm(:,1) = 1.0-iv%info(gpsref)%dx(1,1)
         iv%info(gpsref)%dym(:,1) = 1.0-iv%info(gpsref)%dy(1,1)
         i = iv%info(gpsref)%i(1,1)
         j = iv%info(gpsref)%j(1,1)
         iv%info(gpsref)%lat(:,1) = grid%xb%lat(i,j)
         iv%info(gpsref)%lon(:,1) = grid%xb%lon(i,j)
         iv % gpsref(1) %ref(1) % inv   = pseudo_val
         iv % gpsref(1) %ref(1) % error = pseudo_err
         iv % gpsref(1) %ref(1) % qc    = 0
      end if
   end if

   ! setup gpspw structure
   if ( pseudo_tpw .or. pseudo_ztd ) then
      iv%info(gpspw)%ptotal(1) = num_pseudo
      iv%info(gpspw)%ntotal    = num_pseudo
      if (outside) then
         ob%nlocal(gpspw)               = 0
         iv%info(gpspw)%nlocal          = 0
         iv%info(gpspw)%plocal(iv%time) = 0
      else
         ob%nlocal(gpspw)         = num_pseudo
         iv%info(gpspw)%nlocal    = num_pseudo
         iv%info(gpspw)%plocal(1) = num_pseudo
         iv%info(gpspw)%n1        = 1
         iv%info(gpspw)%n2        = 1
         allocate(iv%gpspw(1:iv%info(gpspw)%nlocal))
         call da_allocate_obs_info(iv, gpspw)
         iv%info(gpspw)%proc_domain(:,:) = .true.
         iv%info(gpspw)%obs_global_index(iv%info(gpspw)%nlocal) = 1
         allocate (ob % gpspw (1:num_pseudo))
         ob % gpspw(1) % tpw   = 0.0
         iv%info(gpspw)%x(:,1)   = pseudo_x
         iv%info(gpspw)%y(:,1)   = pseudo_y
         iv%info(gpspw)%zk(:,1)  = pseudo_z
         iv%info(gpspw)%i(:,1)   = int(pseudo_x)
         iv%info(gpspw)%j(:,1)   = int(pseudo_y)
         iv%info(gpspw)%k(:,1)   = int(pseudo_z)
         iv%info(gpspw)%dx(:,1)  = pseudo_x-real(iv%info(gpspw)%i(1,1))
         iv%info(gpspw)%dy(:,1)  = pseudo_y-real(iv%info(gpspw)%j(1,1))
         iv%info(gpspw)%dxm(:,1) = 1.0 - iv%info(gpspw)%dx(1,1)
         iv%info(gpspw)%dym(:,1) = 1.0 - iv%info(gpspw)%dy(1,1)
         iv % gpspw(1) % tpw % inv   = pseudo_val
         iv % gpspw(1) % tpw % error = pseudo_err
         iv % gpspw(1) % tpw % qc    = 0
         i = iv%info(gpspw)%i(1,1)
         j = iv%info(gpspw)%j(1,1)
         iv%info(gpspw)%lat(:,1) = grid%xb%lat(i,j)
         iv%info(gpspw)%lon(:,1) = grid%xb%lon(i,j)
         if ( pseudo_elv > -999.0 ) then
            ! pseudo_elv is set in the namelist
            iv%info(gpspw)%elv      = pseudo_elv
         else
            ! assign model terrain to ob elv
            dx  = iv%info(gpspw)%dx(1,1)
            dy  = iv%info(gpspw)%dy(1,1)
            dxm = iv%info(gpspw)%dxm(1,1)
            dym = iv%info(gpspw)%dym(1,1)
            iv%info(gpspw)%elv(1) =  &
               dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
               dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
         end if
      end if
   end if

   if ( pseudo_uvtpq ) then
      iv%info(pseudo)%ntotal          = num_pseudo
      iv%info(pseudo)%ptotal(iv%time) = num_pseudo
      if (outside) then
         ob%nlocal(pseudo)               = 0
         iv%info(pseudo)%nlocal          = 0
         iv%info(pseudo)%plocal(iv%time) = 0
      else
         ob%nlocal(pseudo)               = num_pseudo
         iv%info(pseudo)%nlocal          = num_pseudo
         iv%info(pseudo)%plocal(iv%time) = num_pseudo
         iv%info(pseudo)%max_lev         = 1
         allocate (iv%pseudo(1:iv%info(pseudo)%nlocal))
         call da_allocate_obs_info(iv, pseudo)
         iv%info(pseudo)%proc_domain(:,:) = .true.
         allocate (ob % pseudo (1:num_pseudo))
         iv%pseudo(:) % u % inv = missing_r
         iv%pseudo(:) % v % inv = missing_r
         iv%pseudo(:) % t % inv = missing_r
         iv%pseudo(:) % p % inv = missing_r
         iv%pseudo(:) % q % inv = missing_r

         iv%pseudo(:) % u % error = missing_r
         iv%pseudo(:) % v % error = missing_r
         iv%pseudo(:) % t % error = missing_r
         iv%pseudo(:) % p % error = missing_r
         iv%pseudo(:) % q % error = missing_r

         iv%pseudo(:) % u % qc  = missing_data
         iv%pseudo(:) % v % qc  = missing_data
         iv%pseudo(:) % t % qc  = missing_data
         iv%pseudo(:) % p % qc  = missing_data
         iv%pseudo(:) % q % qc  = missing_data

         ob%pseudo(:) % u = missing_r
         ob%pseudo(:) % v = missing_r
         ob%pseudo(:) % t = missing_r
         ob%pseudo(:) % p = missing_r
         ob%pseudo(:) % q = missing_r

         iv%info(pseudo)%x(:,:)  = pseudo_x
         iv%info(pseudo)%y(:,:)  = pseudo_y
         iv%info(pseudo)%zk(:,:) = pseudo_z

         iv%info(pseudo)%i(:,:) = int(pseudo_x)
         iv%info(pseudo)%j(:,:) = int(pseudo_y)
         iv%info(pseudo)%k(:,:) = int(pseudo_z)

         iv%info(pseudo)%dx(:,:) = pseudo_x-real(iv%info(pseudo)%i(:,:))
         iv%info(pseudo)%dy(:,:) = pseudo_y-real(iv%info(pseudo)%j(:,:))
         iv%info(pseudo)%dxm(:,:)=1.0-iv%info(pseudo)%dx(:,:)
         iv%info(pseudo)%dym(:,:)=1.0-iv%info(pseudo)%dy(:,:)
         iv%info(pseudo)%levels(:) = 1

         if ( trim(adjustl(pseudo_var)) == 'u' ) then
            iv%pseudo(:) % u % inv = pseudo_val
            iv%pseudo(:) % u % error = pseudo_err
            iv%pseudo(:) % u % qc = 0
         else if ( trim(adjustl(pseudo_var)) == 'v' ) then
            iv%pseudo(:) % v % inv = pseudo_val
            iv%pseudo(:) % v % error = pseudo_err
            iv%pseudo(:) % v % qc = 0
         else if ( trim(adjustl(pseudo_var)) == 't' ) then
            iv%pseudo(:) % t % inv = pseudo_val
            iv%pseudo(:) % t % error = pseudo_err
            iv%pseudo(:) % t % qc = 0
         else if ( trim(adjustl(pseudo_var)) == 'p' ) then
            iv%pseudo(:) % p % inv = pseudo_val
            iv%pseudo(:) % p % error = pseudo_err
            iv%pseudo(:) % p % qc = 0
         !else if ( trim(adjustl(pseudo_var)) == 'q' ) then
         else if ( pseudo_var(1:1) == 'q' ) then
            iv%pseudo(:) % q % inv = pseudo_val
            iv%pseudo(:) % q % error = pseudo_err
            iv%pseudo(:) % q % qc = 0
         end if
      end if
   end if

   call da_allocate_y (iv, ob)

   if (trace_use_dull) call da_trace_exit("da_setup_pseudo_obs")

end subroutine da_setup_pseudo_obs


