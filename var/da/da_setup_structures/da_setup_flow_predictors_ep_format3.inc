subroutine da_setup_flow_predictors_ep_format3( ix, jy, kz, ne, ep, its, ite, jts, jte, kts, kte )

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
   real                        :: ens_scaling_inv       ! Ensemble scaling of perturbations
   integer                     :: vardim                ! dimension of variable
   integer                     :: nens                  ! number of ensembles
   integer                     :: ep_unit,te,it,ie
   character(len=6)            :: pe_name
   integer                     :: ips, ipe, jps, jpe, kds, kde
   integer                     :: ierr

   if (trace_use) call da_trace_entry("da_setup_flow_predictors_ep_format3")

   call da_get_unit(ep_unit)

   call da_message((/"Set up flow-dependent information from ep_format=3"/))

   if (alphacv_method /= alphacv_method_xa) then
      write(unit=message(1),fmt='(a,i2)') &
         ' ep_format=3 is not implemented for alphacv_method= ', alphacv_method
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ep % ne = ne

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

   allocate(temp3d_r4(1:ix,1:jy,1:kz))
   allocate(temp2d_r4(1:ix,1:jy))

   write(pe_name,'(a2,i4.4)') 'pe', myproc

   !---------------------------------------------------------------------------
   ! Input ensemble perturbations
   !---------------------------------------------------------------------------
   do it = 1, num_fgat_time

      ! v1:
      filename = trim(ep_dir_name(it))//'/'//'ep.U.'//trim(pe_name)
      open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
      if ( ierr /= 0 ) then
         write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      read(unit=ep_unit) !varname(len=10)
      read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
      read(unit=ep_unit) vardim
      read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

      if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
           kds/=kts .or. kde/=kte ) then
         write(unit=message(1),fmt='(a)') &
            'Inconsistent grid dimensions'
         write(unit=message(2),fmt='(a,6i6)') &
            ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
         write(unit=message(3),fmt='(a,6i6)') &
            ' Grid patch dims from U ep file:     ', ips, ipe, jps, jpe, kds, kde
         call da_error(__FILE__,__LINE__,message(1:3))
      end if

      if ( .not. allocated(temp3d_r4) ) allocate(temp3d_r4(ips:ipe,jps:jpe,kds:kde))
      do ie = 1, min(ne, nens)
         read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
         te = ie + (it-1)*nens
         ep % v1(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d_r4(its:ite,jts:jte,kts:kte)
      end do
      close(unit=ep_unit)

      ! v2:
      filename = trim(ep_dir_name(it))//'/'//'ep.V.'//trim(pe_name)
      open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
      if ( ierr /= 0 ) then
         write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      read(unit=ep_unit) !varname(len=10)
      read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
      read(unit=ep_unit) vardim
      read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

      if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
           kds/=kts .or. kde/=kte ) then
         write(unit=message(1),fmt='(a)') &
            'Inconsistent grid dimensions'
         write(unit=message(2),fmt='(a,6i6)') &
            ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
         write(unit=message(3),fmt='(a,6i6)') &
            ' Grid patch dims from V ep file:     ', ips, ipe, jps, jpe, kds, kde
         call da_error(__FILE__,__LINE__,message(1:3))
      end if

      do ie = 1, min(ne, nens)
         read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
         te = ie + (it-1)*nens
         ep % v2(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d_r4(its:ite,jts:jte,kts:kte)
      end do
      close(unit=ep_unit)

      ! v3:
      filename = trim(ep_dir_name(it))//'/'//'ep.T.'//trim(pe_name)
      open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
      if ( ierr /= 0 ) then
         write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      read(unit=ep_unit) !varname(len=10)
      read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
      read(unit=ep_unit) vardim
      read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

      if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
           kds/=kts .or. kde/=kte ) then
         write(unit=message(1),fmt='(a)') &
            'Inconsistent grid dimensions'
         write(unit=message(2),fmt='(a,6i6)') &
            ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
         write(unit=message(3),fmt='(a,6i6)') &
            ' Grid patch dims from T ep file:     ', ips, ipe, jps, jpe, kds, kde
         call da_error(__FILE__,__LINE__,message(1:3))
      end if

      do ie = 1, min(ne, nens)
         read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
         te = ie + (it-1)*nens
         ep % v3(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d_r4(its:ite,jts:jte,kts:kte)
      end do
      close(unit=ep_unit)

      ! v4:
      filename = trim(ep_dir_name(it))//'/'//'ep.QVAPOR.'//trim(pe_name)
      open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
      if ( ierr /= 0 ) then
         write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      read(unit=ep_unit) !varname(len=10)
      read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
      read(unit=ep_unit) vardim
      read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

      if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
           kds/=kts .or. kde/=kte ) then
         write(unit=message(1),fmt='(a)') &
            'Inconsistent grid dimensions'
         write(unit=message(2),fmt='(a,6i6)') &
            ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
         write(unit=message(3),fmt='(a,6i6)') &
            ' Grid patch dims from QVAPOR ep file:', ips, ipe, jps, jpe, kds, kde
         call da_error(__FILE__,__LINE__,message(1:3))
      end if

      do ie = 1, min(ne, nens)
         read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
         te = ie + (it-1)*nens
         ep % v4(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                               temp3d_r4(its:ite,jts:jte,kts:kte)
      end do
      close(unit=ep_unit)

      ! v5:
      filename = trim(ep_dir_name(it))//'/'//'ep.PSFC.'//trim(pe_name)
      open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
      if ( ierr /= 0 ) then
         write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      read(unit=ep_unit) !varname(len=10)
      read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
      read(unit=ep_unit) vardim
      read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

      if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte ) then
         write(unit=message(1),fmt='(a)') &
            'Inconsistent grid dimensions'
         write(unit=message(2),fmt='(a,4i6)') &
            ' Grid patch dims for this processor: ', its, ite, jts, jte
         write(unit=message(3),fmt='(a,4i6)') &
            ' Grid patch dims from PSFC ep file:  ', ips, ipe, jps, jpe
         call da_error(__FILE__,__LINE__,message(1:3))
      end if

      if ( .not. allocated(temp2d_r4) ) allocate(temp2d_r4(ips:ipe,jps:jpe))

      do ie = 1, min(ne, nens)
         read(unit=ep_unit) temp2d_r4(ips:ipe,jps:jpe)
         te = ie + (it-1)*nens
         ep % v5(its:ite,jts:jte,1,te) = ens_scaling_inv * &
                                         temp2d_r4(its:ite,jts:jte)
      end do
      close(unit=ep_unit)

   end do ! num_fgat_time loop

  !  Optional include hydrometeors:

   if ( alphacv_method == alphacv_method_xa .and. alpha_hydrometeors ) then  ! xa space

      do it = 1, num_fgat_time

         filename = trim(ep_dir_name(it))//'/'//'ep.QCLOUD.'//trim(pe_name)
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

         if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
              kds/=kts .or. kde/=kte ) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,6i6)') &
               ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
            write(unit=message(3),fmt='(a,6i6)') &
               ' Grid patch dims from QCLOUD ep file:', ips, ipe, jps, jpe, kds, kde
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

         do ie = 1, min(ne, nens)
            read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
            te = ie + (it-1)*nens
            ep % cw(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d_r4(its:ite,jts:jte,kts:kte)
         end do
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QRAIN.'//trim(pe_name)
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

         if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
              kds/=kts .or. kde/=kte ) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,6i6)') &
               ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
            write(unit=message(3),fmt='(a,6i6)') &
               ' Grid patch dims from QRAIN ep file: ', ips, ipe, jps, jpe, kds, kde
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

         do ie = 1, min(ne, nens)
            read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
            te = ie + (it-1)*nens
            ep % rn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d_r4(its:ite,jts:jte,kts:kte)
         end do
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QICE.'//trim(pe_name)
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

         if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
              kds/=kts .or. kde/=kte ) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,6i6)') &
               ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
            write(unit=message(3),fmt='(a,6i6)') &
               ' Grid patch dims from QICE ep file:  ', ips, ipe, jps, jpe, kds, kde
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

         do ie = 1, min(ne, nens)
            read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
            te = ie + (it-1)*nens
            ep % ci(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d_r4(its:ite,jts:jte,kts:kte)
         end do
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QSNOW.'//trim(pe_name)
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

         if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
              kds/=kts .or. kde/=kte ) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,6i6)') &
               ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
            write(unit=message(3),fmt='(a,6i6)') &
               ' Grid patch dims from QSNOW ep file: ', ips, ipe, jps, jpe, kds, kde
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

         do ie = 1, min(ne, nens)
            read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
            te = ie + (it-1)*nens
            ep % sn(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d_r4(its:ite,jts:jte,kts:kte)
         end do
         close(unit=ep_unit)

         filename = trim(ep_dir_name(it))//'/'//'ep.QGRAUP.'//trim(pe_name)
         open(unit=ep_unit, file=filename, form='unformatted', status='old', iostat=ierr)
         if ( ierr /= 0 ) then
            write(unit=message(1),fmt='(a,a)') ' Error opening ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         read(unit=ep_unit) !varname(len=10)
         read(unit=ep_unit) !date(len=19) 0000-00-00_00:00:00
         read(unit=ep_unit) vardim
         read(unit=ep_unit) ips, ipe, jps, jpe, kds, kde, nens

         if ( ips/=its .or. ipe/=ite .or. jps/=jts .or. jpe/=jte .or. &
              kds/=kts .or. kde/=kte ) then
            write(unit=message(1),fmt='(a)') &
               'Inconsistent grid dimensions'
            write(unit=message(2),fmt='(a,6i6)') &
               ' Grid patch dims for this processor: ', its, ite, jts, jte, kts, kte
            write(unit=message(3),fmt='(a,6i6)') &
               ' Grid patch dims from QGRAUP ep file:', ips, ipe, jps, jpe, kds, kde
            call da_error(__FILE__,__LINE__,message(1:3))
         end if

         do ie = 1, min(ne, nens)
            read(unit=ep_unit) temp3d_r4(ips:ipe,jps:jpe,kds:kde)
            te = ie + (it-1)*nens
            ep % gr(its:ite,jts:jte,kts:kte,te) = ens_scaling_inv * &
                                                  temp3d_r4(its:ite,jts:jte,kts:kte)
         end do
         close(unit=ep_unit)

      end do ! num_fgat_time*ne loop

   end if ! hydrometeors

   if ( allocated(temp3d_r4) ) deallocate(temp3d_r4)
   if ( allocated(temp2d_r4) ) deallocate(temp2d_r4)

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

   if (trace_use) call da_trace_exit("da_setup_flow_predictors_ep_format3")

end subroutine da_setup_flow_predictors_ep_format3

