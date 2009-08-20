subroutine da_setup_flow_predictors( ix, jy, kz, ne, ep, &
                                     its, ite, jts, jte, kts, kte )

   !------------------------------------------------------------------------------
   ! Purpose: Setup structures for flow-dependent information and read it in.
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: ix, jy, kz            ! EP grid dimensions.
   integer, intent(in)         :: its, jts, kts         ! Tile start.
   integer, intent(in)         :: ite, jte, kte         ! Tile end.
   integer, intent(in)         :: ne                    ! Ensemble size.
   type (ep_type), intent(inout):: ep                   ! Flow-dependent info.

   character*10                :: ce                    ! Member index -> character.
   character(len=filename_len) :: filename              ! Input filename.
   character*10                :: var(1:5)              ! Variable name.
   integer                     :: ni, nj, nk            ! Grid dimensions.
   integer                     :: e                     ! Loop counter
   logical                     :: ldum1, ldum2,nkdum    ! Dummy.
   real                        :: temp3d(1:ix,1:jy,1:kz)! Temporary, real*4 array.
   real                        :: temp2d(1:ix,1:jy)     ! Temporary, real*4 array.

   real                        :: ens_scaling_inv       ! Ensemble scaling of perturbations.

   integer                     :: ep_unit

   if (trace_use) call da_trace_entry("da_setup_flow_predictors")

   call da_get_unit(ep_unit)

   call da_message(&
      (/"Setup structures for flow-dependent information and read in"/))

   ep % ne = ne

   ens_scaling_inv = 1.0
   if (ne > 1) ens_scaling_inv = 1.0 / sqrt(real(ne-1))

   ! Decide which space we are introducing flow-dependence:
   if (alphacv_method == alphacv_method_xa) then    ! xa space
      var(1) = 'u'
      var(2) = 'v'
      var(3) = 't'
      var(4) = 'q'
      var(5) = 'ps'
   else                               ! vp space (default)
      var(1) = 'psi'
      var(2) = 'chi_u'
      var(3) = 't_u'
      var(4) = 'rh'
      var(5) = 'ps_u'
   end if

   !---------------------------------------------------------------------------
   ! Input ensemble perturbations
   !---------------------------------------------------------------------------

   do e = 1, ne

      write(unit=ce,fmt='(i3.3)')e

      ! v1:
      filename = 'ep/'//trim(var(1))//'.e'//trim(ce)
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
      ep % v1(its:ite,jts:jte,kts:kte,e) = ens_scaling_inv * &
                                           temp3d(its:ite,jts:jte,kts:kte)

      ! v2:
      filename = 'ep/'//trim(var(2))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v2(its:ite,jts:jte,kts:kte,e) = ens_scaling_inv * &
                                           temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v3:
      filename = 'ep/'//trim(var(3))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v3(its:ite,jts:jte,kts:kte,e) = ens_scaling_inv * &
                                           temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v4:
      filename = 'ep/'//trim(var(4))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nk
      read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v4(its:ite,jts:jte,kts:kte,e) = ens_scaling_inv * &
                                           temp3d(its:ite,jts:jte,kts:kte)
      close(unit=ep_unit)

      ! v5:
      filename = 'ep/'//trim(var(5))//'.e'//trim(ce)
      open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) ni, nj, nkdum
      read(unit=ep_unit) temp2d(1:ix,1:jy)
      ep % v5(its:ite,jts:jte,1,e) = ens_scaling_inv * temp2d(its:ite,jts:jte)
      close(unit=ep_unit)

   end do 

!  Optional include hydrometeors:

   if( use_radarobs .and. use_radar_rf .or. use_rad .and. crtm_cloud & ! qt control variable
       .and. alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then    ! xa space

      do e = 1, ne
         write(unit=ce,fmt='(i3.3)')e

         filename = 'ep/'//'qcloud'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % v4(its:ite,jts:jte,kts:kte,e) = ep % v4(its:ite,jts:jte,kts:kte,e) + &
                                              ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)

         filename = 'ep/'//'qrain'//'.e'//trim(ce)
         open(unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
         read(unit=ep_unit) ni, nj, nk
         read(unit=ep_unit) temp3d(1:ix,1:jy,1:kz)
         ep % v4(its:ite,jts:jte,kts:kte,e) = ep % v4(its:ite,jts:jte,kts:kte,e) + &
                                              ens_scaling_inv * temp3d(its:ite,jts:jte,kts:kte)
         close(unit=ep_unit)
      end do
   end if

   call da_free_unit(ep_unit)

   if (trace_use) call da_trace_exit("da_setup_flow_predictors")

end subroutine da_setup_flow_predictors


