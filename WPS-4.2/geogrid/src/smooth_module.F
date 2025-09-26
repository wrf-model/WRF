!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE SMOOTH_MODULE
!
! This module provides routines for smoothing.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module smooth_module

   use parallel_module


   contains

 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: one_two_one
   !
   ! Purpose: Apply the 1-2-1 smoother from the MM5 program TERRAIN 
   !   (found in smth121.F) to array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine one_two_one(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                          start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval, &
                          dom_x,dom_y)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(inout) :: array 
      real, intent(in) :: dom_x,dom_y
      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch
      integer :: counter  
      allocate(scratch(start_x+1:end_x-1, start_y:end_y, start_z:end_z))
 
      !write(*,*) 'I AM HERE !',dom_x,dom_y,start_z,end_z
 
      do ipass=1,npass

      
         counter = 0

         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z

                  if ( (abs( (array(ix,iy,iz)-array(ix-1,iy,iz)) / dom_x ) > 0.999) .or. &
                       (abs( (array(ix,iy,iz)-array(ix+1,iy,iz)) / dom_x ) > 0.999)       ) then
                     write(*,*) 'smoothing - x ',ix,iy,iz,abs( (array(ix,iy,iz)-array(ix-1,iy,iz)) / dom_x ), &
                         abs( (array(ix,iy,iz)-array(ix+1,iy,iz)) / dom_x )             
                     counter = counter + 1
                    scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+0.25*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
                  else
                    scratch(ix,iy,iz) = array(ix,iy,iz)
                  endif

               end do
            end do
         end do
   
         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z

                  if ( (abs( (scratch(ix,iy,iz)-scratch(ix,iy-1,iz)) / dom_y ) > 0.999) .or. &
                       (abs( (scratch(ix,iy,iz)-scratch(ix,iy+1,iz)) / dom_y ) > 0.999)  ) then
                     write(*,*) 'smoothing - y ',ix,iy,iz, abs( (scratch(ix,iy,iz)-scratch(ix,iy-1,iz)) / dom_y ) ,&
                         abs( (scratch(ix,iy,iz)-scratch(ix,iy+1,iz)) / dom_y )         
                     counter = counter + 1
                     array(ix,iy,iz) = 0.50*scratch(ix,iy,iz)+0.25*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
                  else
                     array(ix,iy,iz) = scratch(ix,iy,iz)
                  endif


               end do
             end do
          end do


         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

          if ( counter .eq. 0 ) then
              exit
          endif


      end do
 
      write(*,*) 'special smoothing done in loops: ',ipass
 
      deallocate(scratch)
 
   end subroutine one_two_one 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth
   !
   ! Purpose: Apply the smoother-desmoother from the MM5 program TERRAIN 
   !   (found in smther.F) to array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                          start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(inout) :: array 
  
      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch
  
      allocate(scratch(start_x+1:end_x-1, start_y:end_y, start_z:end_z))
  
      do ipass=1,npass

         !
         ! Smoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 0.5*array(ix,iy,iz) + 0.25*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do
   
         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 0.5*scratch(ix,iy,iz) + 0.25*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)
   
         !
         ! Desmoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 1.52*array(ix,iy,iz) - 0.26*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do
   
         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 1.52*scratch(ix,iy,iz) - 0.26*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

      end do
  
      deallocate(scratch)
 
   end subroutine smth_desmth


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth_special
   !
   ! Purpose: Apply the smoother-desmoother from the MM5 program TERRAIN 
   !   (found in smther.F) to array; however, any grid points that were not
   !   originally negative but which have been smoothed to a negative value
   !   will be restored to their original values.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth_special(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                                  start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval)

      implicit none

      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch, orig_array

      allocate(scratch(start_x+1:end_x-1, start_y:end_y, start_z:end_z))
      allocate(orig_array(start_x:end_x, start_y:end_y, start_z:end_z))

      orig_array = array

      do ipass=1,npass

         !
         ! Smoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 0.5*array(ix,iy,iz) + 0.25*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 0.5*scratch(ix,iy,iz) + 0.25*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

         !
         ! Desmoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 1.52*array(ix,iy,iz) - 0.26*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 1.52*scratch(ix,iy,iz) - 0.26*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

      end do

      ! Remove artificially negative values
      do iy=start_y,end_y
         do ix=start_x,end_x
            do iz=start_z,end_z
               if (array(ix,iy,iz) < 0. .and. orig_array(ix,iy,iz) >= 0.) then
                  array(ix,iy,iz) = orig_array(ix,iy,iz)
               end if
            end do
         end do
      end do

      deallocate(scratch)
      deallocate(orig_array)

   end subroutine smth_desmth_special


   !
   ! Smoothing routines for E-grid, contributed by Matthew Pyle
   !

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: one_two_one_egrid
   !
   ! Purpose: Apply the 1-2-1 smoother from the MM5 program TERRAIN 
   !   (found in smth121.F) to array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine one_two_one_egrid(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                                start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval, hflag)

      implicit none

      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval, hflag
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch
      integer, dimension(start_y:end_y) :: ihe, ihw, istart

      allocate(scratch(start_x:end_x, start_y:end_y, start_z:end_z))

      do iy=start_y,end_y
         if (hflag == 1.0) then
            ihe(iy) = abs(mod(iy+1,2))
            ihw(iy) = ihe(iy)-1
         else
            ! assign ive,ivw equivs to ihe,ihw
            ihe(iy) = abs(mod(iy,2))
            ihw(iy) = ihe(iy)-1
         end if
      end do

      do iy=start_y,end_y
         if (hflag == 1.0) then
            if (mod(iy,2) == 0) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            end if
         else ! v points
            if (abs(mod(iy,2)) == 1) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            end if
         end if
      end do

      do ipass=1,npass

         do iy=start_y,end_y
            do ix=start_x,end_x
               scratch(ix,iy,1) = array(ix,iy,1) ! for points used in 2nd computation but not defined in 1st computation
            end do
         end do

         ! SW-NE direction
         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval == 1.0 .and. array(ix,iy,iz) /= 0.) .or. msgval /= 1.0) then
                     scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+ &
                                      0.25*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         ! NW-SE direction
         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval == 1.0 .and. array(ix,iy,iz) /= 0.) .or. msgval /= 1.0) then
                     array(ix,iy,iz) = 0.50*scratch(ix,iy,iz)+ &
                                    0.25*(scratch(ix+ihe(iy),iy-1,iz)+scratch(ix+ihw(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

      end do

      deallocate(scratch)

   end subroutine one_two_one_egrid


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth_egrid_old
   !
   ! Purpose: Apply the smoother-desmoother for E grid
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth_egrid_old(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                                    start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval, hflag)

      implicit none

      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval, hflag
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
               intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch
      integer, dimension(start_y:end_y) :: ihe, ihw, istart
      real, parameter:: cenwgt = 1.52
      real, parameter:: endwgt = 0.13

      allocate(scratch(start_x:end_x, start_y:end_y, start_z:end_z))

      do iy=start_y,end_y
         if (hflag == 1.0) then
            ihe(iy) = abs(mod(iy+1,2))
            ihw(iy) = ihe(iy)-1
         else
            ! assign ive,ivw equivs to ihe,ihw
            ihe(iy) = abs(mod(iy,2))
            ihw(iy) = ihe(iy)-1
         end if
      end do

      do iy=start_y,end_y
         if (hflag == 1.0) then
            if (mod(iy,2) == 0) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         else ! v points
            if (abs(mod(iy,2)) == 1) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         endif
      end do

      do ipass=1,npass

         !
         ! Smoothing pass
         !

         do iy=start_y,end_y
            do ix=start_x,end_x
               scratch(ix,iy,1) = array(ix,iy,1) 
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval == 1.0 .and. array(ix,iy,iz) /= 0.) .or. msgval /= 1.0) then
                     scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+ &
                                      0.125*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz)+ &
                                             array(ix+ihw(iy),iy+1,iz)+array(ix+ihe(iy),iy-1,iz))
                  end if
               end do
            end do
         end do


         !
         ! Desmoothing pass
         !

         do iy=start_y+2,end_y-2
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval == 1.0 .and. scratch(ix,iy,iz) /= 0.) .or. msgval /= 1.0) then
                     array(ix,iy,iz) = cenwgt*scratch(ix,iy,iz) - &
                                      endwgt*(scratch(ix+ihw(iy),iy-1,iz)+scratch(ix+ihe(iy),iy+1,iz) + &
                                              scratch(ix+ihw(iy),iy+1,iz)+scratch(ix+ihe(iy),iy-1,iz))
                  end if
               end do
            end do
         end do

      end do

      deallocate(scratch)

   end subroutine smth_desmth_egrid_old


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth_egrid
   !
   ! Purpose: Apply the smoother-desmoother for E grid 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth_egrid(array, start_dom_x, end_dom_x, start_dom_y, end_dom_y, &
                                start_x, end_x, start_y, end_y, start_z, end_z, npass, msgval, hflag)

      implicit none

      ! Arguments
      integer, intent(in) :: start_dom_x, start_dom_y, start_x, start_y, start_z
      integer, intent(in) :: end_dom_x, end_dom_y, end_x, end_y, end_z
      integer, intent(in) :: npass
      real, intent(in) :: msgval, hflag
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
               intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch
      integer, dimension(start_y:end_y) :: ihe, ihw, istart
      real, parameter :: cenwgt = 1.52
      real, parameter :: endwgt = 0.26

      allocate(scratch(start_x:end_x, start_y:end_y, start_z:end_z))

      do iy=start_y,end_y

         if (hflag .eq. 1.0) then
            ihe(iy)=abs(mod(iy+1,2))
            ihw(iy)=ihe(iy)-1

         ! assign ive,ivw equivs to ihe,ihw
         else
            ihe(iy)=abs(mod(iy,2))
            ihw(iy)=ihe(iy)-1

         end if

      end do

      do iy=start_y,end_y

         if (hflag .eq. 1.0) then
            if (mod(iy,2) .eq. 0) then
               istart(iy)=start_x
            else
               istart(iy)=start_x+1
            endif

         else ! v points
            if (abs(mod(iy,2)) .eq. 1) then
               istart(iy)=start_x
            else
               istart(iy)=start_x+1
            end if

         end if

      end do


      do ipass=1,npass

         !
         ! Smoothing pass
         !

         do iy=start_y,end_y
         do ix=start_x,end_x
            scratch(ix,iy,1)=array(ix,iy,1) ! for points used in 2nd computation but 
                                            !    not defined in 1st
         end do
         end do

         ! SW-NE direction
         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval .eq. 1.0 .and. array(ix,iy,iz) .ne. 0.) .or. msgval .ne. 1.0) then
                     scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+ &
                     0.25*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         ! NW-SE direction
         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval .eq. 1.0 .and. array(ix,iy,iz) .ne. 0.) .or. msgval .ne. 1.0) then
                     array(ix,iy,iz) = 0.50*scratch(ix,iy,iz)+ &
                     0.25*(scratch(ix+ihe(iy),iy-1,iz)+scratch(ix+ihw(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)



         !
         ! Desmoothing pass
         !

         ! SW-NE direction
         do iy=start_y+2,end_y-2
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval .eq. 1.0 .and. array(ix,iy,iz) .ne. 0.) .or. msgval .ne. 1.0) then
                     scratch(ix,iy,iz) = cenwgt*array(ix,iy,iz) - &
                       endwgt*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         ! NW-SE direction
         do iy=start_y+2,end_y-2
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  if ( (msgval .eq. 1.0 .and. array(ix,iy,iz) .ne. 0.) .or. msgval .ne. 1.0) then
                     array(ix,iy,iz) = cenwgt*scratch(ix,iy,iz) - &
                       endwgt*(scratch(ix+ihe(iy),iy-1,iz)+scratch(ix+ihw(iy),iy+1,iz))
                  end if
               end do
            end do
         end do

         call exchange_halo_r(array, &
                              start_x, end_x, start_y, end_y, start_z, end_z, &
                              start_dom_x, end_dom_x, start_dom_y, end_dom_y, start_z, end_z)

      end do

      deallocate(scratch)

   end subroutine smth_desmth_egrid

end module smooth_module
