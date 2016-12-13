subroutine da_setup_flow_predictors( ix, jy, kz, ne, ep, its, ite, jts, jte, kts, kte )

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

   integer                     :: ep_unit,t,te,nte,it,ie

   if (trace_use) call da_trace_entry("da_setup_flow_predictors")

   call da_get_unit(ep_unit)

   call da_message((/"Set up flow-dependent information"/))

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

   !---------------------------------------------------------------------------
   ! Input ensemble perturbations
   !---------------------------------------------------------------------------
   do te = 1, nte !loop over num_fgat_time*ne

      it = te/ne + 1       ! retrieve time window index from nte
      ie = mod(te, ne)     ! retrieve ensemble memeber index from nte
      if ( ie == 0 ) then
         it = it - 1
         ie = ne
      end if
      !write(6,*) 'it, ie = ', it, ie

      write(unit=ce,fmt='(i3.3)') ie

      ! v1:
      filename = trim(ep_dir_name(it))//'/'//trim(var(1))//'.e'//trim(ce)
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

      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      close(unit=ep_unit)
      ep % v1(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                            temp3d(its:ite,jts:jte,kts:kte)

      ! v2:
      filename = trim(ep_dir_name(it))//'/'//trim(var(2))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v2(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                            temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v3:
      filename = trim(ep_dir_name(it))//'/'//trim(var(3))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v3(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                            temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v4:
      filename = trim(ep_dir_name(it))//'/'//trim(var(4))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v4(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                            temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v5:
      filename = trim(ep_dir_name(it))//'/'//trim(var(5))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nkdum
      read(unit=ep_unit) temp2d(1:ix,1:jy)
      ep % v5(its:ite,jts:jte,1,te) = ens_scaling_inv * temp2d(its:ite,jts:jte)
      close(unit=ep_unit)

   end do ! num_fgat_time*ne loop

  !  Optional include hydrometeors:

   if ( alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then  ! xa space

      do te = 1, nte

         it = te/ne + 1
         ie = mod(te, ne)
         if ( ie == 0 ) then
            it = it - 1
            ie = ne
         end if
         !write(6,*) 'it, ie = ', it, ie

         write(unit=ce,fmt='(i3.3)') ie

         filename = trim(ep_dir_name(it))//'/'//'qcloud'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % cw(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'qrain'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % rn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'qice'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % ci(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'qsnow'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % sn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'qgraup'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % gr(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

      end do ! num_fgat_time*ne loop

   end if ! hydrometeors

   print *,"MAX EP",MAXVAL(ep % v1(its:ite,jts:jte,:,:))
   print *,"MAX EP",MAXVAL(ep % v2(its:ite,jts:jte,:,:))
   print *,"MAX EP",MAXVAL(ep % v3(its:ite,jts:jte,:,:)) 
   print *,"MAX EP",MAXVAL(ep % v4(its:ite,jts:jte,:,:))
   print *,"MAX EP",MAXVAL(ep % v5(its:ite,jts:jte,:,:))

   if ( alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then
      print *,"MAX EP",MAXVAL(ep % cw(its:ite,jts:jte,:,:))
      print *,"MAX EP",MAXVAL(ep % rn(its:ite,jts:jte,:,:))
      print *,"MAX EP",MAXVAL(ep % ci(its:ite,jts:jte,:,:))
      print *,"MAX EP",MAXVAL(ep % sn(its:ite,jts:jte,:,:))
      print *,"MAX EP",MAXVAL(ep % gr(its:ite,jts:jte,:,:))
   end if

   deallocate(ep_dir_name)
   call da_free_unit(ep_unit)

   if (trace_use) call da_trace_exit("da_setup_flow_predictors")

end subroutine da_setup_flow_predictors


