subroutine da_readwrite_be_stage4(outunit, nk, uh_method, n_smth_sl, variable)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, parameter          :: spike_tolerance = 1.5      ! Threshold for detecting spikes in data. 

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   integer    , intent(in)  :: n_smth_sl                  ! Number of smoothing. 0: no smoothig
   character*5, intent(in)  :: uh_method
   character*10, intent(in) :: variable                   ! Variable name.

   character(len=filename_len) :: filename                   ! Input filename.
   character*10             :: cvar                       ! Dummy variable name.
   character*2              :: ck                         ! Loop index -> character.
   integer                  :: k                          ! Loop counter.
   integer                  :: kdum                       ! Dummy vertical index.
   integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   real                     :: ml                         ! Gradient (read from file but not used).
   real                     :: mean_scale                 ! Average scale between points.
   logical                  :: first_time                 ! True if first time through loop

   real, allocatable        :: total_power(:)             ! Total Power spectrum.
   real, allocatable        :: scale_length(:)
   real, allocatable        :: sl_smth(:)
   
   integer :: iunit

   first_time = .true.

   if (uh_method .eq. 'power') then
      do k = 1, nk
         write(ck,'(i2)')k
         if (k < 10) ck = '0'//ck(2:2)

         filename = trim(variable)//'/'//trim(variable)
         filename = trim(filename)//'.'//ck//'.spectrum'

         call da_get_unit(iunit)
         open (iunit, file = filename, form='unformatted')
         read(iunit)cvar
         if (trim(cvar) /=  trim(variable)) then
           call da_error(__FILE__,__LINE__, &
             (/"Variable name inconsistency"/))
         end if

         read(iunit)max_wavenumber, kdum
         if (kdum /= k) then
           call da_error(__FILE__,__LINE__, &
             (/"Inconsistent vertical label"/))
         end if

         if (first_time) then
            allocate(total_power(0:max_wavenumber))
         end if

         read(iunit)total_power(:)
         close(iunit)
         call da_free_unit(iunit)

         write(outunit)variable
         write(outunit)max_wavenumber, k
         write(outunit) .false. ! preserve file format
         write(outunit)total_power(:)

         first_time = .false.
      end do

      deallocate(total_power)

   else if (uh_method == 'scale   ') then
      if (first_time) then
         allocate(scale_length(1:nk))
         allocate(sl_smth(1:nk))
      end if

      call da_get_unit(iunit)
      filename = trim(variable)//'/'//'sl_print.'//trim(variable)
      open (iunit, file = filename)

      do k=1, nk
         read(iunit,'(a,2e20.8)') ck, ml, scale_length(k)
         ! If missing value is encountered, use the value from the last
         ! mode or level (YRG, 06/30/2005):
         if (ml == 0.0 .and. scale_length(k) == 0.0) &
             scale_length(k) = scale_length(k-1)
      end do

      ! Remove spikes in lengthscales (extrapolate if spike detected):
      do k = 2, nk-1
         mean_scale = 0.5 * ( scale_length(k-1) + scale_length(k+1) )
         if ( scale_length(k) > spike_tolerance * mean_scale ) then
            scale_length(k) = mean_scale
         end if
      end do

      ! Smoothing the scale_length
      sl_smth =  scale_length
      do kdum = 1, n_smth_sl
         do k = 2, nk-1
            sl_smth(k) = scale_length(k) &
               + 0.25*(scale_length(k-1)+scale_length(k+1)-2.0*scale_length(k))
         end do
         scale_length = sl_smth 
      end do
     
      write(outunit) variable
      write(outunit) scale_length

      close(iunit)
      call da_free_unit(iunit)

      deallocate (scale_length)
   end if

end subroutine da_readwrite_be_stage4


