subroutine da_setup_flow_predictors_para_read_opt2( ix, jy, kz, ne, ep, its, ite, jts, jte, kts, kte )

   !------------------------------------------------------------------------------
   ! Purpose: Setup structures for flow-dependent information and read it in.
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: ix, jy, kz            ! EP grid dimensions.
   integer, intent(in)         :: its, jts, kts         ! Tile start.
   integer, intent(in)         :: ite, jte, kte         ! Tile end.
   integer, intent(in)         :: ne                    ! Ensemble size.
   type (ep_type), intent(inout):: ep                   ! Flow-dependent info.

   character(len=4), allocatable :: ep_dir_name(:)      ! Input ep dir name
   character*10                :: ce                    ! Member index -> character.
   character(len=filename_len) :: filename              ! Input filename.
   character*10                :: var(1:5)              ! Variable name.
   integer                     :: ni, nj, nk            ! Grid dimensions.
   integer                     :: e                     ! Loop counter
   logical                     :: ldum1, ldum2,nkdum    ! Dummy.
   real                        :: temp3d(1:ix,1:jy,1:kz)! Temporary, real*4 array.
   real                        :: temp2d(1:ix,1:jy)     ! Temporary, real*4 array.

   real                        :: ens_scaling_inv       ! Ensemble scaling of perturbations.

   character*10                :: varcld(1:5)           ! cloud variable name.
   integer                     :: num_var, iv
   integer                     :: ep_unit,t,te,nte,it,ie,ijk
   real, allocatable           :: global_ep(:,:,:,:)
   real, allocatable           :: globbuf(:,:,:)

   if (trace_use) call da_trace_entry("da_setup_flow_predictors_para_read_opt2")

   call da_get_unit(ep_unit)

   call da_message((/"Set up flow-dependent information"/))

   num_var = 5

   ep % ne = ne

   ens_scaling_inv = 1.0
   if (ne > 1) ens_scaling_inv = 1.0 / sqrt(real(ne-1))

   ! Decide which space we are introducing flow-dependence:
   if (alphacv_method == alphacv_method_xa) then    ! xa space (default)
      var(1) = 'u'
      var(2) = 'v'
      var(3) = 't'
      var(4) = 'q'
      var(5) = 'ps'
   else                               ! vp space
      var(1) = 'psi'
      var(2) = 'chi_u'
      var(3) = 't_u'
      var(4) = 'rh'
      var(5) = 'ps_u'
   end if

   varcld(1) = 'qcloud'
   varcld(2) = 'qrain'
   varcld(3) = 'qice'
   varcld(4) = 'qsnow'
   varcld(5) = 'qgraup'

   if ( num_fgat_time > 1 .and. use_4denvar ) then
      nte = num_fgat_time * ne
      allocate(ep_dir_name(num_fgat_time))
      do it = 1, num_fgat_time
         write(ep_dir_name(it), fmt='(a, i2.2)') 'ep', it    !ep01,ep02,...
      end do
   else
      nte = ne
      allocate(ep_dir_name(1))
      ep_dir_name(1) = 'ep'
   end if

   write(unit=message(1),fmt='(a,L,a,i4,a,i4)') &
      '  use_4denvar = ', use_4denvar, ',  num_fgat_time = ', num_fgat_time, ', ensdim_alpha = ', ne
   call da_message(message(1:1))

   allocate(global_ep(1:ix,1:jy,1:kz,1:nte))
   global_ep = 0.0
   allocate(globbuf(1:ix,1:jy,1:kz))

   ijk = ix*jy*kz

   !---------------------------------------------------------------------------
   ! Input ensemble perturbations
   !---------------------------------------------------------------------------

   do iv = 1, num_var

      write(unit=message(1),fmt='(a,a)') &
         '  Reading perturbations for variable ', trim(var(iv))
      call da_message(message(1:1))

      do te = 1, nte !loop over num_fgat_time*ne

         if ( myproc /= MOD((te-1), num_procs) ) cycle

         it = te/ne + 1       ! retrieve time window index from nte
         ie = mod(te, ne)     ! retrieve ensemble memeber index from nte
         if ( ie == 0 ) then
            it = it - 1
            ie = ne
         end if
         !write(6,*) 'it, ie = ', it, ie

         write(unit=ce,fmt='(i3.3)') ie

         write(unit=message(1),fmt='(a,i4.4,a,i2.2,a,i4.4)') &
            '    proc ', myproc, ' reading from time = ', it, ' and member = ', ie
         call da_message(message(1:1))

         filename = trim(ep_dir_name(it))//'/'//trim(var(iv))//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk

         if (ni /= ix .or. nj /= jy .or. nk /= kz) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,3i6)') &
               ' Grid dims for analysis grid: ', ix, jy
            write(unit=message(3),fmt='(a,3i6)') &
               ' Grid dims for flow predictors: ', ni, nj
            call da_warning(__FILE__,__LINE__,message(1:3))
         end if

         if ( iv /= 5 ) then
            ! 3d array
            read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
            global_ep(:,:,:,te) = ens_scaling_inv * temp3d(:,:,:)
         else
            ! 2d array
            read(unit=ep_unit) temp2d(1:ix,1:jy)
            global_ep(:,:,1,te) = ens_scaling_inv * temp2d(:,:)
         end if

         close(unit=ep_unit)

         if ( iv == 4 ) then
            !  Optionally include hydrometeors:
            if( ((use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud)) &
                .and. alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then    ! xa space
               if ( cloud_cv_options == 1 ) then  ! qt control variable
                  ! adding qcloud to v4
                  filename = trim(ep_dir_name(it))//'/'//'qcloud'//'.e'//trim(ce)
                  open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
                  read(unit=ep_unit) ni, nj, nk
                  read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
                  close(unit=ep_unit)
                  global_ep(:,:,:,te) = global_ep(:,:,:,te) +        &
                                        ens_scaling_inv * temp3d(:,:,:)

                  ! adding qrain to v4
                  filename = trim(ep_dir_name(it))//'/'//'qrain'//'.e'//trim(ce)
                  open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
                  read(unit=ep_unit) ni, nj, nk
                  read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
                  close(unit=ep_unit)
                  global_ep(:,:,:,te) = global_ep(:,:,:,te) +        &
                                        ens_scaling_inv * temp3d(:,:,:)
               end if !end if cloud_cv_options=1
            end if
         end if !iv = 4 for q

      end do ! num_fgat_time*ne loop

      !data communication
#ifdef DM_PARALLEL
      do te = 1, nte
         call mpi_allreduce(global_ep(:,:,:,te),globbuf(:,:,:),ijk,true_mpi_real,mpi_sum,comm,ierr)
         select case ( iv )
            case ( 1 )
               ep % v1(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
            case ( 2 )
               ep % v2(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
            case ( 3 )
               ep % v3(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
            case ( 4 )
               ep % v4(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
            case ( 5 )
               ep % v5(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
         end select
      end do
#else
      do te = 1, nte
         select case ( iv )
            case ( 1 )
               ep % v1(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
            case ( 2 )
               ep % v2(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
            case ( 3 )
               ep % v3(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
            case ( 4 )
               ep % v4(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
            case ( 5 )
               ep % v5(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
         end select
      end do
#endif

   end do !num_var loop

!  Optionally include hydrometeors:
#ifdef CLOUD_CV
   if ( cloud_cv_options >= 2 ) then
      if( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) .and. &
          alpha_hydrometeors ) then

         do iv = 1, num_var

            write(unit=message(1),fmt='(a,a)') &
               '  Reading perturbations for variable ', trim(varcld(iv))
            call da_message(message(1:1))

            do te = 1, nte

               if ( myproc /= MOD((te-1), num_procs) ) cycle

               it = te/ne + 1
               ie = mod(te, ne)
               if ( ie == 0 ) then
                  it = it - 1
                  ie = ne
               end if
               !write(6,*) 'it, ie = ', it, ie

               write(unit=ce,fmt='(i3.3)') ie

               filename = trim(ep_dir_name(it))//'/'//trim(varcld(iv))//'.e'//trim(ce)
               open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
               read(unit=ep_unit) ni, nj, nk
               read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
               close(unit=ep_unit)
               global_ep(:,:,:,te) = ens_scaling_inv * temp3d(:,:,:)

            end do ! num_fgat_time*ne loop

            !data communication
#ifdef DM_PARALLEL
            do te = 1, nte
               call mpi_allreduce(global_ep(:,:,:,te),globbuf(:,:,:),ijk,true_mpi_real,mpi_sum,comm,ierr)
               select case ( iv )
                  case ( 1 )
                     ep % cw(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
                  case ( 2 )
                     ep % rn(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
                  case ( 3 )
                     ep % ci(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
                  case ( 4 )
                     ep % sn(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
                  case ( 5 )
                     ep % gr(its:ite,jts:jte,kts:kte,te) = globbuf(its:ite,jts:jte,kts:kte)
               end select
            end do
#else
            do te = 1, nte
               select case ( iv )
                  case ( 1 )
                     ep % cw(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
                  case ( 2 )
                     ep % rn(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
                  case ( 3 )
                     ep % ci(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
                  case ( 4 )
                     ep % sn(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
                  case ( 5 )
                     ep % gr(its:ite,jts:jte,kts:kte,te) = global_ep(its:ite,jts:jte,kts:kte,te)
               end select
            end do
#endif
         end do ! num_var loop
      end if ! hydrometeors
   end if ! end if cloud_cv_options>=2
#endif

   print *,"MAX EP v1 local ",MAXVAL(ep % v1(its:ite,jts:jte,:,:))
   print *,"MAX EP v2 local ",MAXVAL(ep % v2(its:ite,jts:jte,:,:))
   print *,"MAX EP v3 local ",MAXVAL(ep % v3(its:ite,jts:jte,:,:))
   print *,"MAX EP v4 local ",MAXVAL(ep % v4(its:ite,jts:jte,:,:))
   print *,"MAX EP v5 local ",MAXVAL(ep % v5(its:ite,jts:jte,:,:))
#ifdef CLOUD_CV
   print *,"MAX EP cw local ",MAXVAL(ep % cw(its:ite,jts:jte,:,:))
   print *,"MAX EP rn local ",MAXVAL(ep % rn(its:ite,jts:jte,:,:))
   print *,"MAX EP ci local ",MAXVAL(ep % ci(its:ite,jts:jte,:,:))
   print *,"MAX EP sn local ",MAXVAL(ep % sn(its:ite,jts:jte,:,:))
   print *,"MAX EP gr local ",MAXVAL(ep % gr(its:ite,jts:jte,:,:))
#endif

   deallocate(ep_dir_name)
   call da_free_unit(ep_unit)

   deallocate(global_ep)
   deallocate(globbuf)

   if (trace_use) call da_trace_exit("da_setup_flow_predictors_para_read_opt2")

end subroutine da_setup_flow_predictors_para_read_opt2


