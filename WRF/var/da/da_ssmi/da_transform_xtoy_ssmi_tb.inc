subroutine da_transform_xtoy_ssmi_tb(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer                      :: n        ! loop counter.

   ! real, dimension(mix,mjy)     :: TGL_dotspeed
   ! real, dimension(mix,mjy)     :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   ! real, dimension(mix,mjy)     :: TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,TGL_tpw
   ! real, dimension(mix,mjy)     :: TGL_htpw,TGL_speed,TGL_alw,TGL_zcld         

   ! real, dimension(mix,mjy)     :: tb19v,tb19h, &
   !                                  tb22v,       &
   !                                  tb37v,tb37h, &
   !                                  tb85v,tb85h

   ! real, dimension(mix,mjy)     :: TGL_tb19v,TGL_tb19h, &
   !                                  TGL_tb22v,           &
   !                                  TGL_tb37v,TGL_tb37h, &
   !                                  TGL_tb85v,TGL_tb85h

   ! real, dimension(mkz)         :: zh
   ! real, dimension(mkz+1)       :: zf
   !----------------------------------------------------------------------------

   ! Wind at 1st level at dot pivnts

   ! speed, surface wind speed (m/sec)    (0 - 30) , take 10 m wind

   ! total precipitable water in cm

   ! call da_transform_xtotpw(grid%xa, grid%xb)

   ! do j=1,mjy-1
   !    do i=1,mix-1
   !       zf(1) = grid%xb%ztop
   !       do k=1,mkz
   !          zh(k)=grid%xb%h(i,j,k)
   !          zf(k+1)=grid%xb%hf(i,j,k)
   !       end do

   !       ! surface pressure (mb) (940 -1030)

   !       psfc(i,j)     = (grid%xb%p(i,j,mkz)+grid%xb%Psac(i,j)*(1.0-grid%xb % sigmah(mkz)))*0.001
   !       TGL_psfc(i,j) =  grid%xa%p  (i,j,mkz)*0.01

   !       ! sea surface temperature (k) (273 - 303) (doesnot change) 

   !       sst(i,j)      = grid%xb%tgrn(i,j)
   !       TGL_sst(i,j)  = grid%xa%tgrn(1,1)
   !       TGL_sst(i,j)  = 0.0

   !       ! effective surface air temperature (263 - 303)

   !       ta(i,j)     = grid%xb%tgrn(i,j) + &
   !                   (grid%xb%t(i,j,mkz)-grid%xb%tgrn(i,j))*log(2.0/0.0001)/ &
   !                   log(zh(mkz)/0.0001)

   !       TGL_ta(i,j) = grid%xa%tgrn(1,1) + &
   !                   (grid%xa%t(i,j,mkz)-grid%xa%tgrn(1,1))*log(2.0/0.0001)/ &
   !                   log(zh(mkz)/0.0001)

   !       TGL_ta(i,j) = (grid%xa%t(i,j,mkz)-0.)*log(2.0/0.0001)/ &
   !                   log(zh(mkz)/0.0001)

   !       ! gamma is an emperical formula and zcld is given for now

   !       gamma(i,j) = (ta(i,j)-270)*0.023 + 5.03  ! effective lapse rate   (km) (4.0 - 6.5)
   !       zcld(i,j)  = 1                           ! effective cloud height (km)
                                                     ! = 1 if no cloud infomation
   !       TGL_gamma(i,j) = TGL_ta(i,j)*0.023
   !       TGL_zcld(i,j)  = 0.0

   !       ! total precipitable water in (kg/m**2) (0 - 70)

   !       tpw(i,j)     = grid%xb%tpw(i,j)*10.0
   !       TGL_tpw(i,j) = grid%xa%tpw(i,j)*10.0

   !       ! Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

   !       alw(i,j)     = 0.0
   !       TGL_alw(i,j) = 0.0

   !       ! Column height weighted mivsture density on the grid locally 

   !       call da_transform_xtozrhoq(grid%xb, i, j, zh, zf, zrhom)
   !       call da_transform_xtozrhoq_lin(grid%xb, grid%xa, i, j, zh, zf, TGL_zrhom)

   !       ! Column mivsture density on the grid locally

   !       htpw(i,j)     = zrhom/tpw(i,j)/1000.0
   !       TGL_htpw(i,j) = TGL_zrhom/tpw(i,j)/1000.0 &
   !                   - TGL_tpw(i,j)/tpw(i,j)*htpw(i,j)

   !    end do
   ! end do

   ! do j=1,mjy-1
   !    do i=1,mix-1

   !       call tgl_tb(1,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
   !          htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
   !          grid%xb%tb19v(i,j),grid%xb%tb19h(i,j),                           &
   !          TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
   !          TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
   !          TGL_zcld(i,j),TGL_tb19v(i,j),TGL_tb19h(i,j)           )

   !       call TGL_tb(2,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
   !          htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
   !          grid%xb%tb22v(i,j),dum1,                                    &
   !          TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
   !          TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
   !          TGL_zcld(i,j),TGL_tb22v(i,j),dum2                     )

   !       call TGL_tb(3,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
   !          htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
   !          grid%xb%tb37v(i,j),grid%xb%tb37h(i,j),                           &
   !          TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
   !          TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
   !          TGL_zcld(i,j),TGL_tb37v(i,j),TGL_tb37h(i,j)           )

   !       call TGL_tb(4,53.,psfc(i,j),ta(i,j),gamma(i,j),sst(i,j),tpw(i,j),  &
   !          htpw(i,j),speed(i,j),alw(i,j),zcld(i,j),               &
   !          grid%xb%tb85v(i,j),grid%xb%tb85h(i,j),                           &
   !          TGL_psfc(i,j),TGL_ta(i,j),TGL_gamma(i,j),TGL_sst(i,j), &
   !          TGL_tpw(i,j),TGL_htpw(i,j),TGL_speed(i,j),TGL_alw(i,j),&
   !          TGL_zcld(i,j),TGL_tb85v(i,j),TGL_tb85h(i,j)           )
   !    end do
   ! end do

   real, allocatable :: tb19v(:)
   real, allocatable :: tb19h(:)
   real, allocatable :: tb22v(:)
   real, allocatable :: tb37v(:)
   real, allocatable :: tb37h(:)
   real, allocatable :: tb85v(:)
   real, allocatable :: tb85h(:)

   if (trace_use) call da_trace_entry("da_transform_xtoy_ssmi_tb")

   allocate(tb19v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb19h(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb22v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb37v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb37h(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb85v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb85h(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))

   call da_interp_lin_2d (grid%xa%tb19v, iv%info(ssmi_tb), 1, tb19v)
   call da_interp_lin_2d (grid%xa%tb19h, iv%info(ssmi_tb), 1, tb19h)
   call da_interp_lin_2d (grid%xa%tb22v, iv%info(ssmi_tb), 1, tb22v)
   call da_interp_lin_2d (grid%xa%tb37v, iv%info(ssmi_tb), 1, tb37v)
   call da_interp_lin_2d (grid%xa%tb37h, iv%info(ssmi_tb), 1, tb37h)
   call da_interp_lin_2d (grid%xa%tb85v, iv%info(ssmi_tb), 1, tb85v)
   call da_interp_lin_2d (grid%xa%tb85h, iv%info(ssmi_tb), 1, tb85h)

   do n=iv%info(ssmi_tb)%n1,iv%info(ssmi_tb)%n2
      y%ssmi_tb(n)%tb19v = tb19v(n)
      y%ssmi_tb(n)%tb19h = tb19h(n)
      y%ssmi_tb(n)%tb22v = tb22v(n)
      y%ssmi_tb(n)%tb37v = tb37v(n)
      y%ssmi_tb(n)%tb37h = tb37h(n)
      y%ssmi_tb(n)%tb85v = tb85v(n)
      y%ssmi_tb(n)%tb85h = tb85h(n)
   end do

   deallocate(tb19v)
   deallocate(tb19h)
   deallocate(tb22v)
   deallocate(tb37v)
   deallocate(tb37h)
   deallocate(tb85v)
   deallocate(tb85h)
    
   ! WHY?
   !          y%ssmi_tb(n)%tb19v = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb19v(i,j ),TGL_tb19v(i+1,j ), &
   !              TGL_tb19v(i,j+1),TGL_tb19v(i+1,j+1) )

   !          y%ssmi_tb(n)%tb19h = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb19h(i,j ),TGL_tb19h(i+1,j ), &
   !              TGL_tb19h(i,j+1),TGL_tb19h(i+1,j+1) )

   !          y%ssmi_tb(n)%tb22v = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb22v(i,j ),TGL_tb22v(i+1,j ), &
   !              TGL_tb22v(i,j+1),TGL_tb22v(i+1,j+1) )

   !          y%ssmi_tb(n)%tb37v = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb37v(i,j ),TGL_tb37v(i+1,j ), &
   !              TGL_tb37v(i,j+1),TGL_tb37v(i+1,j+1) )

   !          y%ssmi_tb(n)%tb37h = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb37h(i,j ),TGL_tb37h(i+1,j ), &
   !              TGL_tb37h(i,j+1),TGL_tb37h(i+1,j+1) )

   !          y%ssmi_tb(n)%tb85v = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb85v(i,j ),TGL_tb85v(i+1,j ), &
   !              TGL_tb85v(i,j+1),TGL_tb85v(i+1,j+1) )

   !          y%ssmi_tb(n)%tb85h = hor_interp(dxm,dx,dym,dy, &
   !              TGL_tb85h(i,j ),TGL_tb85h(i+1,j ), &
   !              TGL_tb85h(i,j+1),TGL_tb85h(i+1,j+1) )
   !       end if
   !    end if

   ! end do

   if (trace_use) call da_trace_exit("da_transform_xtoy_ssmi_tb")

end subroutine da_transform_xtoy_ssmi_tb


