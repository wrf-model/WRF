subroutine da_sfc_hori_interp_weights(n, info, sfc_ob, xb, ob_land_type)

   implicit none

   integer        ,  intent(in)    :: n
   type(infa_type),  intent(inout) :: info
   type(synop_type), intent(inout) :: sfc_ob
   type(xb_type),    intent(in)    :: xb
   integer        ,  intent(in)    :: ob_land_type !1:land, 0:water

   integer :: i, j, iloc, ii, jj
   integer :: iloc_min_hdiff
   integer :: landmask(4)
   real    :: dx, dxm, dy, dym
   real    :: hh(4), w(4), hdiff(4)

! 1: (i,   j)
! 2: (i+1, j)
! 3: (i,   j+1)
! 4: (i+1, j+1)
!------------------------------------------------------
!  (i,j+1)
!    -----------------(i+1,j+1)
!    |   w2          |
!    |dym        w1  |
!    |      obs      |
!    |------- *      |
!    |dy  w4  |  w3  |
!    |   dx   | dxm  |
!    |----------------
!   (i,j)            (i+1,j)
!
!--------------------------------------------------------

   ! initialize
   ii = nint(info%x(1,n))
   jj = nint(info%y(1,n))

   i   = info%i(1,n)
   j   = info%j(1,n)
   dx  = info%dx(1,n)
   dy  = info%dy(1,n)
   dxm = info%dxm(1,n)
   dym = info%dym(1,n)

   w(1) = dym*dxm   ! weight for point (i,  j)
   w(2) = dym*dx    ! weight for point (i+1,j)
   w(3) = dy *dxm   ! weight for point (i,  j+1)
   w(4) = dy *dx    ! weight for point (i+1,j+1)

   ! lowest half model level height
   hh(1) = xb%h(i,   j,   kts)
   hh(2) = xb%h(i+1, j,   kts)
   hh(3) = xb%h(i,   j+1, kts)
   hh(4) = xb%h(i+1, j+1, kts)

   ! landmask: 1-land, 0-water
   landmask(1) = int(xb%landmask(i,   j))
   landmask(2) = int(xb%landmask(i+1, j))
   landmask(3) = int(xb%landmask(i,   j+1))
   landmask(4) = int(xb%landmask(i+1, j+1))

   do iloc = 1, 4
      hdiff(iloc) = abs(hh(iloc) - sfc_ob%h)
      if ( landmask(iloc) /= ob_land_type ) then
         hdiff(iloc) = 8888.0 !set a large value to not use it
      end if
   end do
   if ( sum(hdiff)/4.0 == 8888.0 ) then
      ! do not use the ob, because the model land type is not
      ! consistent with the ob land type
      !print*, 'Rejecting ', trim(info%id(n)), ' due to land type mismatch'
      sfc_ob%p%qc = missing_data
      sfc_ob%t%qc = missing_data
      sfc_ob%q%qc = missing_data
      sfc_ob%u%qc = missing_data
      sfc_ob%v%qc = missing_data
      return
   else
      iloc_min_hdiff = 1 !initialize
      do iloc = 2, 4
         if ( hdiff(iloc) > hdiff(iloc_min_hdiff) ) then
            cycle
         else
            if ( hdiff(iloc) < hdiff(iloc_min_hdiff) ) then
               iloc_min_hdiff = iloc
            else
               if ( w(4-iloc+1) < w(4-iloc_min_hdiff+1) ) then
                  iloc_min_hdiff = iloc
               end if
            end if
         end if
      end do
      select case ( iloc_min_hdiff )
         case ( 1 )
            ii = i
            jj = j
         case ( 2 )
            ii = i+1
            jj = j
         case ( 3 )
            ii = i
            jj = j+1
         case ( 4 )
            ii = i+1
            jj = j+1
      end select
   end if

   if ( ii == i .and. jj == j ) then
      dx  = 0.0
      dxm = 1.0
      dy  = 0.0
      dym = 1.0
   else if ( ii == i+1 .and. jj == j ) then
      dx  = 1.0
      dxm = 0.0
      dy  = 0.0
      dym = 1.0
   else if ( ii == i   .and. jj == j+1 ) then
      dx  = 0.0
      dxm = 1.0
      dy  = 1.0
      dym = 0.0
   else if ( ii == i+1 .and. jj == j+1 ) then
      dx  = 1.0
      dxm = 0.0
      dy  = 1.0
      dym = 0.0
   end if

   !print*, 'Resetting i, j for : ', trim(info%id(n)), ii, jj
   ! re-assign the weights, lowest level only
   info%dx(1,n)  = dx
   info%dy(1,n)  = dy
   info%dxm(1,n) = dxm
   info%dym(1,n) = dym

!         call da_detsurtyp ( grid%xb%snow, grid%xb%xice, grid%xb%landmask,  &
!            grid%xb%ivgtyp, grid%xb%isltyp, &
!            ims, ime, jms, jme, &
!            i, j, dx, dy, dxm, dym, &
!            model_isflg,model_ivgtyp, model_isltyp, &
!            Surface(1)%Water_Coverage, Surface(1)%Ice_Coverage, &
!            Surface(1)%Land_Coverage, Surface(1)%Snow_Coverage )

end subroutine da_sfc_hori_interp_weights
