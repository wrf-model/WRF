subroutine da_detsurtyp (snow,ice,landsea, vegtyp, soiltyp, is, ie, js, je, &
   i, j, dx, dy, dxm, dym, isflg,ob_vegtyp,ob_soiltyp, seap, icep, lndp, snop)

   !---------------------------------------------------------------------------
   ! Purpose: determine surface type at obs locations.
   !
   ! METHOD: using the background information 
   !      1. get the background landmask/snow/sea-ice
   !      2. calculate percentage of sea/ice/land/snow
   !      3. determine surface type at obs location
   !      4. find nearest grid vegtype and soil type 
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: is, ie, js, je
   integer, intent(in)  :: i, j
   real   , intent(in)  :: dx, dxm, dy, dym
   real   , intent(in)  :: snow(is:ie,js:je)    ! Input variable
   real   , intent(in)  :: ice(is:ie,js:je)     ! Input variable
   real   , intent(in)  :: landsea(is:ie,js:je) ! Input variable
   integer, intent(in)  :: vegtyp(is:ie,js:je)  ! Input variable
   integer, intent(in)  :: soiltyp(is:ie,js:je) ! Input variable
   integer, intent(out) :: isflg                ! Output variable 
   real,    intent(out) :: ob_vegtyp            ! Output variable
   real,    intent(out) :: ob_soiltyp            ! Output variable
   real,    intent(out) :: seap, icep, lndp, snop ! percentage of surface type
   
   !     isflg    - surface flag
   !                0 sea
   !                1 sea ice
   !                2 land
   !                3 snow
   !                4 mixed predominately sea
   !                5 mixed predominately sea ice
   !                6 mixed predominately land
   !                7 mixed predominately snow

   !  local variables
   integer   ::  n, xbflag(4)   ! surface type at xb location
                               ! 0:sea  1:sea-ice 2:land  3:snow
   real      ::  w(4),minw      ! weight at 4 xb locations

   if (trace_use) call da_trace_entry("da_detsurtyp")    

   ! 1.0 determine surface type of xb at 4 location around obs
   !-------------------------------------------------------
   if ( nint(landsea(i  ,j  )) == 0 ) xbflag(1) = 0 ! sea
   if ( nint(landsea(i+1,j  )) == 0 ) xbflag(2) = 0 ! sea
   if ( nint(landsea(i  ,j+1)) == 0 ) xbflag(3) = 0 ! sea
   if ( nint(landsea(i+1,j+1)) == 0 ) xbflag(4) = 0 ! sea

   if ( nint(landsea(i  ,j  )) == 1 ) xbflag(1) = 2 ! land/snow/sea-ice
   if ( nint(landsea(i+1,j  )) == 1 ) xbflag(2) = 2 ! land/snow/sea-ice
   if ( nint(landsea(i  ,j+1)) == 1 ) xbflag(3) = 2 ! land/snow/sea-ice
   if ( nint(landsea(i+1,j+1)) == 1 ) xbflag(4) = 2 ! land/snow/sea-ice

   if ( nint(snow(i  ,j  )) == 1 ) xbflag(1) = 3 ! snow
   if ( nint(snow(i+1,j  )) == 1 ) xbflag(2) = 3 ! snow
   if ( nint(snow(i  ,j+1)) == 1 ) xbflag(3) = 3 ! snow
   if ( nint(snow(i+1,j+1)) == 1 ) xbflag(4) = 3 ! snow

   if ( nint(ice(i  ,j  )) == 1 ) xbflag(1) = 1 ! sea-ice
   if ( nint(ice(i+1,j  )) == 1 ) xbflag(2) = 1 ! sea-ice
   if ( nint(ice(i  ,j+1)) == 1 ) xbflag(3) = 1 ! sea-ice
   if ( nint(ice(i+1,j+1)) == 1 ) xbflag(4) = 1 ! sea-ice 

   ! 2.0 determine surface type percentage at obs location
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

   seap = 0.0
   icep = 0.0
   snop = 0.0
   lndp = 0.0
   w(1) = dym*dxm   ! weight for point (i,j)
   w(2) = dym*dx    ! weight for point (i+1,j)
   w(3) = dy *dxm   ! weight for point (i,j+1)
   w(4) = dy *dx    ! weight for point (i+1,j+1)

   do n = 1, 4
      if (xbflag(n) == 0) seap = seap+w(n)
      if (xbflag(n) == 1) icep = icep+w(n)
      if (xbflag(n) == 2) lndp = lndp+w(n)
      if (xbflag(n) == 3) snop = snop+w(n)
   end do

   ! fo2d   = dym*(dxm*fm2d(i,j  ) + dx*fm2d(i+1,j  )) &
   !         + dy *(dxm*fm2d(i,j+1) + dx*fm2d(i+1,j+1))

   ! 3.0 determine final surface flag at obs location
   !-----------------------------------------
   if (seap >= 0.99) isflg = 0
   if (icep >= 0.99) isflg = 1
   if (lndp >= 0.99) isflg = 2
   if (snop >= 0.99) isflg = 3
   if ( .not. (seap >= 0.99) .and. &
        .not. (icep >= 0.99) .and. &
        .not. (lndp >= 0.99) .and. &
        .not. (snop >= 0.99) ) then
      if (seap > lndp) then
         if (seap > icep) then
            if (seap > snop) then
               isflg = 4
            else
               isflg = 7
            end if
         else
            if (icep > snop) then
               isflg = 5
            else
               isflg = 7
            end if
         end if
      else
         if (lndp > icep) then
            if (lndp > snop) then
               isflg = 6
            else
               isflg = 7
            end if
         else
            if (icep > snop) then
               isflg = 5
            else
               isflg = 7
            end if
         end if
      end if 
   end if
   
   ! 4.0 find nearest grid vegtype and soil type
   !         at obs location
   !-----------------------------------------
   minw=min(w(1),w(2),w(3),w(4))
   if (minw == w(1)) then
      ob_vegtyp  = float(vegtyp (i+1,j+1))
      ob_soiltyp = float(soiltyp(i+1,j+1))
   else if (minw == w(2)) then
      ob_vegtyp  = float(vegtyp (i,j+1))
      ob_soiltyp = float(soiltyp(i,j+1))
   else if (minw == w(3)) then
      ob_vegtyp  = float(vegtyp (i+1,j))
      ob_soiltyp = float(soiltyp(i+1,j))
   else if (minw == w(4)) then
      ob_vegtyp  = float(vegtyp (i,j))
      ob_soiltyp = float(soiltyp(i,j))
   end if

   if (trace_use) call da_trace_exit("da_detsurtyp")    

end subroutine da_detsurtyp

