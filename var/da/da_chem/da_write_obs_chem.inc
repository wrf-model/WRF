subroutine da_write_obs_chem ( iv, ob, it )
   !-------------------------------------------------------------------------
   ! Purpose:        Print modeled observations to ascii files
   !
   ! Called from da_nl_model
   !
   ! History: 06/2015  Creation (Jonathan Guerrette)
   !
   !-------------------------------------------------------------------------

   type (iv_type),    intent(inout) :: iv
   type (y_type),     intent(inout) :: ob
   integer, intent(in)              :: it
   integer                                    :: iobs, ichem, itime, iplt, iloc, jloc, kloc, &
                                                 ounit, iostat, ij, ilocal
   real, dimension(:,:), allocatable :: dummy_surf
   real, dimension(:,:), allocatable :: dummy_acft

   real :: k_at_p

   character*256         :: surf_file! = "SURFACE_Hx_y.txt"
   character*256         :: acft_file! = "AIRCRAFT_Hx_y.txt"  
   character*5000        :: hx_out  

   character*3         :: dummychar1="", dummychar2=""

   if (num_surf_obs > 0) then

      write(surf_file,'("SURFACE_Hx_y_i",I2.2,".txt")') it
      if(rootproc) then
         ! =========
         ! open file
         ! =========
         call da_get_unit(ounit)  
         open(unit   = ounit,     &
            FILE   = trim(surf_file), &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            iostat =  iostat,     &
            STATUS = 'NEW')
   
         if (iostat /= 0) then
            write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
            call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!            call da_wrf_error_fatal('Error opening surface Hx_y file')
            call wrf_message('Error opening surface Hx_y file')  !!! add !!!
         end if   

         allocate ( dummy_surf (2,num_chem_surf-1) )

         ilocal = 0
         do iobs = 1, iv%info(chem_surf)%ntotal !num_surf_obs
            if ( .NOT. (head_grid%ti_surf(iobs) .GE. 1 .AND. head_grid%tf_surf(iobs) .LE. num_ts) ) cycle
            iloc = INT(head_grid%i_surf(iobs) + 0.5D0)
            jloc = INT(head_grid%j_surf(iobs) + 0.5D0)
            kloc = INT(head_grid%k_surf(iobs) + 0.5D0)

            ilocal = ilocal + 1
            do ichem = 1, num_chem_surf-1
               dummy_surf(1,ichem) = &
                       ob%chem_surf(iobs)%chem(ichem)
!                       iv%chem_surf(iobs)%chem(ichem)%inv
               dummy_surf(2,ichem) = &
                       ( iv%chem_surf(iobs)%chem(ichem)%error ) ** 2

            end do
            write(dummychar1,fmt='(I0)') num_chem_surf-1
            write(dummychar2,fmt='(I0)') num_surf_hx-1
            write(ounit,fmt='(3I6,D14.5,' // trim(dummychar1) // 'D14.5,' // trim(dummychar1) // 'D23.14,' // trim(dummychar2) // 'D23.14)') &
               iloc, jloc, kloc, &
               head_grid%ti_surf(iobs), &
               dummy_surf(1,:), &
               dummy_surf(2,:), &
               head_grid%surf_hx(iobs,2:num_surf_hx)
            if (iostat /= 0 ) then
               write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
               call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!               call da_wrf_error_fatal('Error writing surface chem Hx_y')
               call wrf_message('Error writing surface chem Hx_y')  !!! add !!!
            end if
         end do

         deallocate ( dummy_surf )
         close(ounit)
         call da_free_unit(ounit)
      end if
   end if
#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

   if (num_acft_obs > 0) then  
      write(acft_file,'("AIRCRAFT_Hx_y_i",I2.2,".txt")') it

      if(rootproc) then
         ! =========
         ! open file
         ! =========
         call da_get_unit(ounit)  
         open(unit   = ounit,     &
            FILE   = trim(acft_file), &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            iostat =  iostat,     &
            STATUS = 'NEW')
      
         if (iostat /= 0) then
            write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
            call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!            call da_wrf_error_fatal('Error opening aircraft Hx_y file')
            call wrf_message('Error opening aircraft Hx_y file')  !!! add !!!
         end if   

         allocate ( dummy_acft (2,num_chem_acft-1) )

         do iplt = 1, num_platform
            ilocal = 0
            do itime = 1, iv%info(chem_acft)%ntotal
               if( .NOT. head_grid%acft_obs_count(itime,iplt) > 0 ) cycle
               iloc = INT(head_grid%i_acft(itime,iplt) + 0.5D0)
               jloc = INT(head_grid%j_acft(itime,iplt) + 0.5D0)
               kloc = INT(head_grid%k_acft(itime,iplt) + 0.5D0)
           
               ilocal = ilocal + 1

               do ichem = 1, num_chem_acft-1
                  dummy_acft(1,ichem) = &
                          ob%chem_acft(itime)%chem(ichem,iplt)
!                          iv%chem_acft(itime)%chem(ichem,iplt)%inv
                  dummy_acft(2,ichem) = &
                          ( iv%chem_acft(itime)%chem(ichem,iplt)%error ) ** 2
               end do
               write(dummychar1,fmt='(I0)') num_chem_acft-1
               write(dummychar2,fmt='(I0)') num_acft_hx-1
               write(ounit,fmt='(6I6,' // trim(dummychar1) // 'D14.5,' // trim(dummychar1) // 'D23.14,' // trim(dummychar2) // 'D23.14)') &

                  iloc, jloc, kloc, itime, iplt, &
                  head_grid%acft_obs_count(itime,iplt), &
                  dummy_acft(1,:), &
                  dummy_acft(2,:), &
                  head_grid%acft_hx(itime,iplt,2:num_acft_hx)
   
               if (iostat /= 0 ) then
                  write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
                  call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!                  call da_wrf_error_fatal('Error writing aircraft chem Hx_y')
                  call wrf_message('Error writing aircraft chem Hx_y')  !!! add !!!
               end if
            end do
         end do

         deallocate ( dummy_acft )
         close(ounit)
         call da_free_unit(ounit)
      end if
   end if

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

end subroutine da_write_obs_chem

