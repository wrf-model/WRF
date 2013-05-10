subroutine da_setup_obs_structures_ascii( ob, iv, grid )

   !-------------------------------------------------------------------------
   ! Purpose: Define, allocate and read of observation structure.
   !-------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(out)   :: ob  ! Observation structure.
   type (iv_type), intent(inout) :: iv  ! O-B structure.
   type (domain),  intent(inout) :: grid  ! First guess structure

   character(len=filename_len)  :: filename
   integer                      :: n, i, j, k
   logical                      :: outside
   logical                      :: uvq_direct=.false.  !cys_add

   if (trace_use) call da_trace_entry("da_setup_obs_structures_ascii")

   !-------------------------------
   ! Find out if pseudo ob is local
   !-------------------------------
   if (num_pseudo > 0) then
      iv%time = 1
      outside = .false.
      i = int(pseudo_x)
      j = int(pseudo_y)
      if (fg_format == fg_format_kma_global) then
         if ((j < jts-1) .or. (j > jte)) outside = .true.
      else
         if ((i < ids)   .or. (i >= ide) .or. (j < jds)   .or. (j >= jde)) outside = .true.
         if ((i < its-1) .or. (i >  ite) .or. (j < jts-1) .or. (j >  jte)) outside = .true.
      end if
   else
      iv%time = 1
      outside = .true.
   end if

   if (num_pseudo > 0) then
   if (outside) then
      iv%info(pseudo)%nlocal          = 0
      ob%nlocal(pseudo)               = 0
      iv%info(pseudo)%ntotal          = 0
      iv%info(pseudo)%plocal(iv%time) = 0
   else
      iv%info(pseudo)%nlocal          = num_pseudo
      ob%nlocal(pseudo)               = num_pseudo
      iv%info(pseudo)%ntotal          = num_pseudo
      iv%info(pseudo)%max_lev         = 1            
   end if
   end if

   !--------------------------------------------------------------------------
   ! [1.0] Scan GTS observation header and get idea of number of obs:
   !--------------------------------------------------------------------------
  
   if (num_fgat_time > 1) then
!      filename = ' '

      do n=1, num_fgat_time

         iv%time = n
         filename = ' '

        write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.ascii'

         ! scan main body of gts observation file
         call da_scan_obs_ascii (iv, filename)

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! scan SSMI observation file
            write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.ssmi'
            call da_scan_obs_ssmi (iv, filename)
         end if

         if (use_radarobs) then
            ! scan radar observation file
            write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.radar'
            call da_scan_obs_radar(iv, filename)
            print*,'scan radar obs ',n,filename(1:10),iv%info(:)%ntotal, iv%info(:)%nlocal
         end if

         iv%info(:)%plocal(n) = iv%info(:)%nlocal
         iv%info(:)%ptotal(n) = iv%info(:)%ntotal
      end do
   else
      iv%time = 1
      print*,'scan obs ascii'
      call da_scan_obs_ascii(iv, 'ob.ascii')
      print*,'end scan obs ascii'
      !-----------------------------------------------------------------------
      ! read header of ssmi observation file
      !-----------------------------------------------------------------------
      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         call da_scan_obs_ssmi(iv, 'ob.ssmi')
      end if

      ! scan main body of radar observation file
      if (use_radarobs) then
         call da_scan_obs_radar(iv, 'ob.radar')
      end if
      do i=1,num_ob_indexes
         iv%info(i)%plocal(iv%time) = iv%info(i)%nlocal
         iv%info(i)%ptotal(iv%time) = iv%info(i)%ntotal
      end do
   end if
   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then

      do n=1, num_fgat_time
         iv%time = n
         filename = ' '  

         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.ascii'

         ! Read gts observation file
         call da_read_obs_ascii (iv, filename, uvq_direct)  !cys_change

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! read ssmi observation file
            write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.ssmi'
            call da_read_obs_ssmi (iv, filename)
         end if

         if (use_radarobs) then
            ! read radar observation file
            write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.radar'
            call da_read_obs_radar(iv, filename)
         end if

      end do
   else
      iv%time = 1

      call da_read_obs_ascii(iv, 'ob.ascii', uvq_direct)  !cys_change

      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         ! read ssmi observation file
         call da_read_obs_ssmi (iv, 'ob.ssmi')
      end if

      if (use_radarobs) then
         ! read radar observation file
         call da_read_obs_radar(iv, 'ob.radar')
      end if
   end if

   ! Calculate DT for RF DA

   if (use_radarobs .and. use_radar_rf) then
      if (.not. DT_cloud_model) then
         do j = jts,jte
            do i = its, ite
               do k = kts, kte
                   grid%xb%delt(i,j,k) = 0.0
               end do
            end do
        end do

        do n = 1, iv%info(radar)%nlocal
           do i=int(iv%info(radar)%i(1,n)), int(iv%info(radar)%i(1,n))+1
              do j=int(iv%info(radar)%j(1,n)), int(iv%info(radar)%j(1,n))+1
                 do k=kts, kte
                    grid%xb%delt(i,j,k) = 1800.0
                    grid%xb%qrn(i,j,k) = amax1(5.0E-5, grid%xb%qrn(i,j,k))
                    grid%xb%qcw(i,j,k) = amax1(5.0E-12, grid%xb%qcw(i,j,k))
                  end do
                  ! do k=kts, kte
                  !    v_h(k)=grid%xb%h(i,j  ,k)
                  ! end do
                  ! do k = 1, iv%info(radar)%levels(n)
                  !    call zk(iv%radar(n)%height(k), v_h, zkk)
                  !    grid%xb%delt(i,j,int(zkk))=1800.0
                  !    grid%xb%delt(i,j,int(zkk)+1)=1800.0
                  ! end do
               end do
            end do
         end do
      end if
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   ! cys_change
   ! call da_fill_obs_structures(iv, ob)
   if (uvq_direct) then
     call da_fill_obs_structures(iv, ob, uvq_direct)
   else
     call da_fill_obs_structures(iv, ob)
   endif

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_ascii")

end subroutine da_setup_obs_structures_ascii


