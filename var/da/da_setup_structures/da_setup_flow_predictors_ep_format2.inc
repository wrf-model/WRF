subroutine da_setup_flow_predictors_ep_format2( ix, jy, kz, ne, ep, its, ite, jts, jte, kts, kte )

   !------------------------------------------------------------------------------
   ! Purpose: Setup structures for flow-dependent information and read it in.
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: ix, jy, kz            ! EP grid dimensions
   integer, intent(in)         :: its, jts, kts         ! Tile start
   integer, intent(in)         :: ite, jte, kte         ! Tile end
   integer, intent(in)         :: ne                    ! Ensemble size
   type(ep_type), intent(inout):: ep                    ! Flow-dependent info

   character(len=4), allocatable :: ep_dir_name(:)      ! Input ep dir name
   character(len=filename_len) :: filename              ! Input filename
   integer                     :: ni, nj, nk            ! Grid dimensions
   real*4, allocatable         :: temp3d_r4(:,:,:)      ! Temporary, real*4 array
   real*4, allocatable         :: temp2d_r4(:,:)        ! Temporary, real*4 array
   real,   allocatable         :: temp3d(:,:,:)         ! Temporary array
   real,   allocatable         :: temp2d(:,:)           ! Temporary array
   real                        :: ens_scaling_inv       ! Ensemble scaling of perturbations
   integer                     :: vardim                ! dimension of variable
   integer                     :: nens, nens_global(1)  ! number of ensembles
   integer                     :: ep_unit,te,it,ie
   integer                     :: ijk
   integer                     :: ierr

   if (trace_use) call da_trace_entry("da_setup_flow_predictors_ep_format2")

   call da_get_unit(ep_unit)

   call da_message((/"Set up flow-dependent information from ep_format=2"/))

   if (alphacv_method /= alphacv_method_xa) then
      write(unit=message(1),fmt='(a,i2)') &
         ' ep_format=2 is not implemented for alphacv_method= ', alphacv_method
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ep % ne = ne
   ijk = ix * jy * kz

   ens_scaling_inv = 1.0
   if (ne > 1) ens_scaling_inv = 1.0 / sqrt(real(ne-1))

   if ( num_fgat_time > 1 .and. use_4denvar ) then
      allocate(ep_dir_name(num_fgat_time))
      do it = 1, num_fgat_time
         write(ep_dir_name(it), fmt='(a, i2.2)') 'ep', it    !ep01,ep02,...
      end do
   else
      allocate(ep_dir_name(1))
      ep_dir_name(1) = 'ep'
   end if

   if ( rootproc ) then
      allocate(temp3d_r4(1:ix,1:jy,1:kz))
      allocate(temp2d_r4(1:ix,1:jy))
   end if
   allocate(temp3d(1:ix,1:jy,1:kz))
   allocate(temp2d(1:ix,1:jy))

   !---------------------------------------------------------------------------
   ! Input ensemble perturbations
   !---------------------------------------------------------------------------
   do it = 1, num_fgat_time

      ! v1:
      filename = trim(ep_dir_name(it))//'/'//'ep.U'
      if ( rootproc ) then
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ni, nj, nk, nens

         if (ni /= ix .or. nj /= jy .or. nk /= kz) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,3i6)') &
               ' Grid dims for ensemble grid: ', ix, jy, kz
            write(unit=message(3),fmt='(a,3i6)') &
               ' Grid dims from U ep file:    ', ni, nj, nk
            call da_error(__FILE__,__LINE__,message(1:3))
         end if
         nens_global = nens
      end if
      call wrf_dm_bcast_integer(nens_global, 1)
      nens = nens_global(1)

      do ie = 1, min(ne, nens)
         if ( rootproc ) then
            read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
            temp3d = temp3d_r4
         end if
         call wrf_dm_bcast_real(temp3d, ijk)
         te = ie + (it-1)*nens
         ep % v1(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d(its:ite,jts:jte,kts:kte)
      end do
      if ( rootproc ) close(unit=ep_unit)

      ! v2:
      filename = trim(ep_dir_name(it))//'/'//'ep.V'
      if ( rootproc ) then
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ni, nj, nk, nens

         if (ni /= ix .or. nj /= jy .or. nk /= kz) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,3i6)') &
               ' Grid dims for ensemble grid: ', ix, jy, kz
            write(unit=message(3),fmt='(a,3i6)') &
               ' Grid dims from V ep file:    ', ni, nj, nk
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

      end if

      do ie = 1, min(ne, nens)
         if ( rootproc ) then
            read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
            temp3d = temp3d_r4
         end if
         call wrf_dm_bcast_real(temp3d, ijk)
         te = ie + (it-1)*nens
         ep % v2(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d(its:ite,jts:jte,kts:kte)
      end do
      if ( rootproc ) close(unit=ep_unit)

      ! v3:
      filename = trim(ep_dir_name(it))//'/'//'ep.T'
      if ( rootproc ) then
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ni, nj, nk, nens

         if (ni /= ix .or. nj /= jy .or. nk /= kz) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,3i6)') &
               ' Grid dims for ensemble grid: ', ix, jy, kz
            write(unit=message(3),fmt='(a,3i6)') &
               ' Grid dims from T ep file:    ', ni, nj, nk
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

      end if

      do ie = 1, min(ne, nens)
         if ( rootproc ) then
            read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
            temp3d = temp3d_r4
         end if
         call wrf_dm_bcast_real(temp3d, ijk)
         te = ie + (it-1)*nens
         ep % v3(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d(its:ite,jts:jte,kts:kte)
      end do
      if ( rootproc ) close(unit=ep_unit)

      ! v4:
      filename = trim(ep_dir_name(it))//'/'//'ep.QVAPOR'
      if ( rootproc ) then
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ni, nj, nk, nens

         if (ni /= ix .or. nj /= jy .or. nk /= kz) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,3i6)') &
               ' Grid dims for ensemble grid:   ', ix, jy, kz
            write(unit=message(3),fmt='(a,3i6)') &
               ' Grid dims from QVAPOR ep file: ', ni, nj, nk
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

      end if

      do ie = 1, min(ne, nens)
         if ( rootproc ) then
            read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
            temp3d = temp3d_r4
         end if
         call wrf_dm_bcast_real(temp3d, ijk)
         te = ie + (it-1)*nens
         ep % v4(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d(its:ite,jts:jte,kts:kte)
      end do
      if ( rootproc ) close(unit=ep_unit)

      ! v5:
      filename = trim(ep_dir_name(it))//'/'//'ep.PSFC'
      if ( rootproc ) then
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ni, nj, nk, nens

         if (ni /= ix .or. nj /= jy) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,2i6)') &
               ' Grid dims for ensemble grid: ', ix, jy
            write(unit=message(3),fmt='(a,2i6)') &
               ' Grid dims from PSFC ep file: ', ni, nj
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

      end if

      do ie = 1, min(ne, nens)
         if ( rootproc ) then
            read(unit=ep_unit) temp2d_r4(1:ix,1:jy)
            temp2d = temp2d_r4
         end if
         call wrf_dm_bcast_real(temp2d, ix*jy)
         te = ie + (it-1)*nens
         ep % v5(its:ite,jts:jte,1,te) = ens_scaling_inv * &
                                         temp2d(its:ite,jts:jte)
      end do
      if ( rootproc ) close(unit=ep_unit)

   end do ! num_fgat_time loop

  !  Optional include hydrometeors:

   if ( alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then  ! xa space

      do it = 1, num_fgat_time

         filename = trim(ep_dir_name(it))//'/'//'ep.QCLOUD'
         if ( rootproc ) then
            open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
            if ( ierr /= 0 ) then
               write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            read(unit=ep_unit) !varname(len=10)
            read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
            read(unit=ep_unit) vardim
            read(unit=ep_unit) ni, nj, nk, nens

            if (ni /= ix .or. nj /= jy .or. nk /= kz) then
               write(unit=message(1),fmt='(a)') &
                  'Inconsistent grid dimensions'
               write(unit=message(2),fmt='(a,3i6)') &
                  ' Grid dims for ensemble grid:   ', ix, jy, kz
               write(unit=message(3),fmt='(a,3i6)') &
                  ' Grid dims from QCLOUD ep file: ', ni, nj, nk
               call da_error(__FILE__,__LINE__,message(1:3))
            end if

         end if
         do ie = 1, min(ne, nens)
            if ( rootproc ) then
               read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
               temp3d = temp3d_r4
            end if
            call wrf_dm_bcast_real(temp3d, ijk)
            te = ie + (it-1)*nens
            ep % cw(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d(its:ite,jts:jte,kts:kte)
         end do
         if ( rootproc ) close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QRAIN'
         if ( rootproc ) then
            open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
            if ( ierr /= 0 ) then
               write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            read(unit=ep_unit) !varname(len=10)
            read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
            read(unit=ep_unit) vardim
            read(unit=ep_unit) ni, nj, nk, nens

            if (ni /= ix .or. nj /= jy .or. nk /= kz) then
               write(unit=message(1),fmt='(a)') &
                  'Inconsistent grid dimensions'
               write(unit=message(2),fmt='(a,3i6)') &
                  ' Grid dims for ensemble grid: ', ix, jy, kz
               write(unit=message(3),fmt='(a,3i6)') &
                  ' Grid dims from QRAIN ep file:', ni, nj, nk
               call da_error(__FILE__,__LINE__,message(1:3))
            end if

         end if
         do ie = 1, min(ne, nens)
            if ( rootproc ) then
               read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
               temp3d = temp3d_r4
            end if
            call wrf_dm_bcast_real(temp3d, ijk)
            te = ie + (it-1)*nens
            ep % rn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d(its:ite,jts:jte,kts:kte)
         end do
         if ( rootproc ) close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QICE'
         if ( rootproc ) then
            open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
            if ( ierr /= 0 ) then
               write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            read(unit=ep_unit) !varname(len=10)
            read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
            read(unit=ep_unit) vardim
            read(unit=ep_unit) ni, nj, nk, nens

            if (ni /= ix .or. nj /= jy .or. nk /= kz) then
               write(unit=message(1),fmt='(a)') &
                  'Inconsistent grid dimensions'
               write(unit=message(2),fmt='(a,3i6)') &
                  ' Grid dims for ensemble grid: ', ix, jy, kz
               write(unit=message(3),fmt='(a,3i6)') &
                  ' Grid dims from QICE ep file: ', ni, nj, nk
               call da_error(__FILE__,__LINE__,message(1:3))
            end if

         end if
         do ie = 1, min(ne, nens)
            if ( rootproc ) then
               read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
               temp3d = temp3d_r4
            end if
            te = ie + (it-1)*nens
            ep % ci(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d(its:ite,jts:jte,kts:kte)
         end do
         if ( rootproc ) close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QSNOW'
         if ( rootproc ) then
            open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
            if ( ierr /= 0 ) then
               write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            read(unit=ep_unit) !varname(len=10)
            read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
            read(unit=ep_unit) vardim
            read(unit=ep_unit) ni, nj, nk, nens

            if (ni /= ix .or. nj /= jy .or. nk /= kz) then
               write(unit=message(1),fmt='(a)') &
                  'Inconsistent grid dimensions'
               write(unit=message(2),fmt='(a,3i6)') &
                  ' Grid dims for ensemble grid: ', ix, jy, kz
               write(unit=message(3),fmt='(a,3i6)') &
                  ' Grid dims from QSNOW ep file:', ni, nj, nk
               call da_error(__FILE__,__LINE__,message(1:3))
            end if

         end if
         do ie = 1, min(ne, nens)
            if ( rootproc ) then
               read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
               temp3d = temp3d_r4
            end if
            te = ie + (it-1)*nens
            ep % sn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d(its:ite,jts:jte,kts:kte)
         end do
         if ( rootproc ) close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QGRAUP'
         if ( rootproc ) then
            open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
            if ( ierr /= 0 ) then
               write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            read(unit=ep_unit) !varname(len=10)
            read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
            read(unit=ep_unit) vardim
            read(unit=ep_unit) ni, nj, nk, nens

            if (ni /= ix .or. nj /= jy .or. nk /= kz) then
               write(unit=message(1),fmt='(a)') &
                  'Inconsistent grid dimensions'
               write(unit=message(2),fmt='(a,3i6)') &
                  ' Grid dims for ensemble grid:   ', ix, jy, kz
               write(unit=message(3),fmt='(a,3i6)') &
                  ' Grid dims from QGRAUP ep file: ', ni, nj, nk
               call da_error(__FILE__,__LINE__,message(1:3))
            end if

         end if
         do ie = 1, min(ne, nens)
            if ( rootproc ) then
               read(unit=ep_unit) temp3d_r4(1:ix,1:jy,1:kz)
               temp3d = temp3d_r4
            end if
            te = ie + (it-1)*nens
            ep % gr(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d(its:ite,jts:jte,kts:kte)
         end do
         if ( rootproc ) close(unit=ep_unit)

      end do ! num_fgat_time*ne loop

   end if ! hydrometeors

   if ( rootproc ) then
      deallocate(temp3d_r4)
      deallocate(temp2d_r4)
   end if
   deallocate(temp3d)
   deallocate(temp2d)

   write(unit=message(1),fmt='(a,e12.5)') &
      ' Max U    ep: ', maxval(ep % v1(its:ite,jts:jte,:,:))
   write(unit=message(2),fmt='(a,e12.5)') &
      ' Max V    ep: ', maxval(ep % v2(its:ite,jts:jte,:,:))
   write(unit=message(3),fmt='(a,e12.5)') &
      ' Max T    ep: ', maxval(ep % v3(its:ite,jts:jte,:,:))
   write(unit=message(4),fmt='(a,e12.5)') &
      ' Max Q    ep: ', maxval(ep % v4(its:ite,jts:jte,:,:))
   write(unit=message(5),fmt='(a,e12.5)') &
      ' Max Psfc ep: ', maxval(ep % v5(its:ite,jts:jte,1,:))
   call da_message(message(1:5))

   if ( alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then
      write(unit=message(1),fmt='(a,e12.5)') &
         ' Max QCLOUD ep: ', maxval(ep % cw(its:ite,jts:jte,:,:))
      write(unit=message(2),fmt='(a,e12.5)') &
         ' Max QRAIN  ep: ', maxval(ep % rn(its:ite,jts:jte,:,:))
      write(unit=message(3),fmt='(a,e12.5)') &
         ' Max QICE   ep: ', maxval(ep % ci(its:ite,jts:jte,:,:))
      write(unit=message(4),fmt='(a,e12.5)') &
         ' Max QSNOW  ep: ', maxval(ep % sn(its:ite,jts:jte,:,:))
      write(unit=message(5),fmt='(a,e12.5)') &
         ' Max QGRAUP ep: ', maxval(ep % gr(its:ite,jts:jte,:,:))
      call da_message(message(1:5))
   end if

   deallocate(ep_dir_name)
   call da_free_unit(ep_unit)

   if (trace_use) call da_trace_exit("da_setup_flow_predictors_ep_format2")

end subroutine da_setup_flow_predictors_ep_format2

