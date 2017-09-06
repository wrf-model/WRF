
subroutine da_read_lsac_wrfinput(iv, onlyscan)

implicit none

type (iv_type), intent(inout)       ::  iv
logical       , intent(in)          ::  onlyscan

type (multi_level_type)             ::  platform
logical                             ::  outside, outside_all

integer                             ::  i, j, k, ki, ndims, nrecs, nlocal, iunit, nlevels, ilevel
integer                             ::  u_qc, v_qc, t_qc, q_qc, ierror
real                                ::  u_ferr, v_ferr, t_ferr, q_ferr
character(len=512)                  ::  lsac_wrfinput, lsac_output
integer, dimension(4)               ::  dims_u, dims_v, dims_t, dims_p, dims_q
integer, dimension(4)               ::  dims_lat, dims_lon, dims_alt, dims_phb, dims_ph
real, allocatable, dimension(:,:,:) ::  u_lsac, v_lsac, w_lsac, t_lsac, p_lsac, q_lsac, taux_lsac
real, allocatable, dimension(:,:,:) ::  pb_lsac, ph_lsac, phb_lsac, height_lsac, press
real, allocatable, dimension(:,:)   ::  lat_lsac, lon_lsac
logical                             ::  debug
logical, external                   ::  wrf_dm_on_monitor

! If the errors are not calculated from equation, then a minimum value is set.
!Minimum Error
real, parameter                     ::  u_ferrmin=2.5         ! [m/s]
real, parameter                     ::  v_ferrmin=2.5         ! [m/s]
real, parameter                     ::  t_ferrmin=2.0         ! [C]
real, parameter                     ::  q_ferrmin=0.002       ! [Kg/Kg]

! If the errors are calculated from equation, then the errors are a percentage of the full values
!Percentage
real, parameter                     ::  u_err=25.0            ! [%]
real, parameter                     ::  v_err=25.0            ! [%]
real, parameter                     ::  t_err=15.0            ! [%]
real, parameter                     ::  q_err=30.0            ! [%]


if (trace_use) call da_trace_entry("da_read_lsac_util")

lsac_wrfinput  = 'fg_l'
debug=.false.

if (onlyscan) then

   call da_get_dims_cdf( lsac_wrfinput, 'XLAT',   dims_lat,  ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'XLONG',  dims_lon,  ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'T',      dims_t,    ndims, debug)

   allocate(lat_lsac(dims_lat(1), dims_lat(2)))
   allocate(lon_lsac(dims_lon(1), dims_lon(2)))

   nlevels = dims_t(3)/lsac_nvskip

   !---------------------------------------------------------
   ! Reading data from WRF Input file
   !---------------------------------------------------------
   call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLAT',   lat_lsac, dims_lat(1), dims_lat(2), 1, debug)
   call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLONG',  lon_lsac, dims_lon(1), dims_lon(2), 1, debug)

   ! Calculating the errors and fill the iv type
   nlocal=0
   nrecs =0
   do i=1, dims_lon(1), lsac_nhskip
      do j=1, dims_lat(2), lsac_nhskip
         platform%info%lat      = lat_lsac(i,j)
         platform%info%lon      = lon_lsac(i,j)
         platform%info%elv      = 0.0
         platform%info%name     = 'LSAC'
         platform%info%platform = 'FM-???  LSAC'
         platform%info%id       = '?????'
         platform%info%date_char= '????-??-??_??:??:??'
         platform%info%pstar    = 0.000000000000000
         platform%info%levels   = nlevels
         if (platform%info%lon == 180.0  ) platform%info%lon =-180.000
         if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
            platform%info%lon = 0.0
         endif

         call da_llxy (platform%info, platform%loc, outside, outside_all)
         if (.not.outside) then
            nlocal = nlocal+1
         endif
         if (.not.outside_all) then
            nrecs  = nrecs+1
         endif
      enddo
   enddo
   iv%info(bogus)%max_lev = nlevels
   iv%info(bogus)%nlocal  = nlocal
   iv%info(bogus)%ntotal  = nrecs
   deallocate(lat_lsac)
   deallocate(lon_lsac)
else
   !---------------------------------------------------------
   ! Getting information from NETCDF files (WRF Input file)
   !---------------------------------------------------------
   call da_get_dims_cdf( lsac_wrfinput, 'U',      dims_u,    ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'V',      dims_v,    ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'T',      dims_t,    ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'PB',     dims_p,    ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'QVAPOR', dims_q,    ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'XLAT',   dims_lat,  ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'XLONG',  dims_lon,  ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'PHB',    dims_phb,  ndims, debug)
   call da_get_dims_cdf( lsac_wrfinput, 'PH',     dims_ph,   ndims, debug)

   ! It will be assimilated every "lsac_nhskip" data point in the horizontal and "lsac_nvskip"
   ! in the vertical
   nrecs   = ( 1 + ( dims_lat(1) - 1 )/lsac_nhskip ) * ( 1 + ( dims_lat(2) - 1 )/lsac_nhskip )
   nlevels = dims_t(3)/lsac_nvskip

   !---------------------------------------------------------
   ! Allocating memory
   !---------------------------------------------------------
   allocate(u_lsac(dims_u(1),       dims_u(2),   dims_u(3)   ))
   allocate(v_lsac(dims_v(1),       dims_v(2),   dims_v(3)   ))
   allocate(t_lsac(dims_t(1),       dims_t(2),   dims_t(3)   ))
   allocate(taux_lsac(dims_t(1),    dims_t(2),   dims_t(3)   ))
   allocate(p_lsac(dims_p(1),       dims_p(2),   dims_p(3)   ))
   allocate(pb_lsac(dims_p(1),      dims_p(2),   dims_p(3)   ))
   allocate(q_lsac(dims_q(1),       dims_q(2),   dims_q(3)   ))
   allocate(lat_lsac(dims_lat(1),   dims_lat(2)              ))
   allocate(lon_lsac(dims_lon(1),   dims_lon(2)              ))
   allocate(phb_lsac(dims_phb(1),   dims_phb(2), dims_phb(3) ))
   allocate(ph_lsac(dims_ph(1),     dims_ph(2),  dims_ph(3)  ))
   allocate(height_lsac(dims_ph(1), dims_ph(2),  dims_ph(3)  ))
   allocate(press(dims_p(1),        dims_p(2),   dims_p(3)   ))

   !---------------------------------------------------------
   ! Reading data from WRF Input file
   !---------------------------------------------------------
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'U',      u_lsac,   dims_u(1),   dims_u(2),   dims_u(3),   1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'V',      v_lsac,   dims_v(1),   dims_v(2),   dims_v(3),   1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'T',      t_lsac,   dims_t(1),   dims_t(2),   dims_t(3),   1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'P',      p_lsac,   dims_p(1),   dims_p(2),   dims_p(3),   1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'PB',     pb_lsac,  dims_p(1),   dims_p(2),   dims_p(3),   1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'QVAPOR', q_lsac,   dims_q(1),   dims_q(2),   dims_q(3),   1, debug)
   call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLAT',   lat_lsac, dims_lat(1), dims_lat(2),              1, debug)
   call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLONG',  lon_lsac, dims_lon(1), dims_lon(2),              1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'PHB',    phb_lsac, dims_phb(1), dims_phb(2), dims_phb(3), 1, debug)
   call da_get_var_3d_real_cdf( lsac_wrfinput, 'PH',     ph_lsac,  dims_ph(1),  dims_ph(2),  dims_ph(3),  1, debug)

   !---------------------------------------------------------
   !Calculating the height
   !---------------------------------------------------------
   height_lsac= (phb_lsac + ph_lsac)/9.8
   press=(p_lsac+pb_lsac)*0.01

   !Temperature from potential temperature
   taux_lsac=(300.0+t_lsac) * ( ( press/1000.0 )**(287.04/1004.5) )

   if (lsac_print_details .and. wrf_dm_on_monitor() ) then
      call da_get_unit(iunit)
      open(iunit, file='lsac_details')
   endif

   ! Calculating the errors and fill the iv type
   nlocal=0
   do i=1, dims_lon(1), lsac_nhskip
      do j=1, dims_lat(2), lsac_nhskip
         ilevel = 0
         do k=1, dims_t(3), lsac_nvskip
            ilevel = ilevel+1
            u_qc   = 0
            v_qc   = 0
            t_qc   = 0
            q_qc   = 0

            if(lsac_calcerr) then
               u_ferr=max( u_ferrmin , abs((u_lsac(i,j,k            )*u_err)/100.0) )
               v_ferr=max( v_ferrmin , abs((v_lsac(i,j,k            )*v_err)/100.0) )
               t_ferr=max( t_ferrmin , abs(((taux_lsac(i,j,k)-273.15)*t_err)/100.0) )
               q_ferr=max( q_ferrmin , abs((q_lsac(i,j,k            )*q_err)/100.0) )
            else
               u_ferr=u_ferrmin
               v_ferr=v_ferrmin
               t_ferr=t_ferrmin
               q_ferr=q_ferrmin
            endif

            platform%each(ilevel)%height=height_lsac(i,j,k)

            platform%each(ilevel)%u%inv=u_lsac(i,j,k)
            platform%each(ilevel)%u%error=u_ferr
            platform%each(ilevel)%u%qc=u_qc

            platform%each(ilevel)%v%inv=v_lsac(i,j,k)
            platform%each(ilevel)%v%error=v_ferr
            platform%each(ilevel)%v%qc=v_qc

            platform%each(ilevel)%t%inv=taux_lsac(i,j,k)
            platform%each(ilevel)%t%error=t_ferr
            platform%each(ilevel)%t%qc=t_qc

            platform%each(ilevel)%q%inv=q_lsac(i,j,k)
            platform%each(ilevel)%q%error=q_ferr
            platform%each(ilevel)%q%qc=q_qc

            if(lsac_print_details .and. wrf_dm_on_monitor() ) then
               write(iunit,'(3f10.3,x,4(f10.3,x,f10.3,x,i4))') height_lsac(i,j,k), lat_lsac(i,j), lon_lsac(i,j), &
                 u_lsac(i,j,k)           , u_ferr     ,   u_qc, &
                 v_lsac(i,j,k)           , v_ferr     ,   v_qc, &
                 taux_lsac(i,j,k)        , t_ferr     ,   t_qc, &
                 q_lsac(i,j,k)*1000      , q_ferr*1000,   q_qc
            endif
         enddo
         platform%info%lat      = lat_lsac(i,j)
         platform%info%lon      = lon_lsac(i,j)
         platform%info%elv      = height_lsac(i,j,dims_t(3))
         platform%info%name     = 'LSAC'
         platform%info%platform = 'FM-???  LSAC'
         platform%info%id       = '?????'
         platform%info%date_char= '????-??-??_??:??:??'
         platform%info%pstar    = 0.000000000000000
         platform%info%levels   = nlevels
         if (platform%info%lon == 180.0  ) platform%info%lon =-180.000
         if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
            platform%info%lon = 0.0
         endif

         call da_llxy (platform%info, platform%loc, outside, outside_all)
         if (outside) then
            cycle
         endif
         nlocal = nlocal+1

         if (nlevels > 0) then
            allocate (iv%bogus(nlocal)%h (1:nlevels))
            allocate (iv%bogus(nlocal)%p (1:nlevels))
            allocate (iv%bogus(nlocal)%u (1:nlevels))
            allocate (iv%bogus(nlocal)%v (1:nlevels))
            allocate (iv%bogus(nlocal)%t (1:nlevels))
            allocate (iv%bogus(nlocal)%q (1:nlevels))
            do ki = 1, nlevels
               iv%bogus(nlocal)%h(ki) = platform%each(ki)%height
               iv%bogus(nlocal)%p(ki) = missing_r
               iv%bogus(nlocal)%u(ki) = platform%each(ki)%u
               iv%bogus(nlocal)%v(ki) = platform%each(ki)%v
               iv%bogus(nlocal)%t(ki) = platform%each(ki)%t
               iv%bogus(nlocal)%q(ki) = platform%each(ki)%q
               iv%bogus(nlocal)%slp%inv   = 0.
               iv%bogus(nlocal)%slp%qc    = missing_data
               iv%bogus(nlocal)%slp%error = missing_r
            end do
         else
            nlevels = 1
            allocate (iv%bogus(nlocal)%h (1:nlevels))
            allocate (iv%bogus(nlocal)%p (1:nlevels))
            allocate (iv%bogus(nlocal)%u (1:nlevels))
            allocate (iv%bogus(nlocal)%v (1:nlevels))
            allocate (iv%bogus(nlocal)%t (1:nlevels))
            allocate (iv%bogus(nlocal)%q (1:nlevels))
            iv%bogus(nlocal)%h       = missing_r
            iv%bogus(nlocal)%p       = missing_r
            iv%bogus(nlocal)%u%inv   = missing_r
            iv%bogus(nlocal)%u%qc    = missing
            iv%bogus(nlocal)%u%error = abs(missing_r)
            iv%bogus(nlocal)%v       = iv%bogus(nlocal)%u
            iv%bogus(nlocal)%t       = iv%bogus(nlocal)%u
            iv%bogus(nlocal)%q       = iv%bogus(nlocal)%u
         end if

         iv%info(bogus)%name(nlocal)          = platform%info%name
         iv%info(bogus)%platform(nlocal)      = platform%info%platform
         iv%info(bogus)%id(nlocal)            = platform%info%id
         iv%info(bogus)%date_char(nlocal)     = platform%info%date_char
         iv%info(bogus)%levels(nlocal)        = platform%info%levels
         iv%info(bogus)%lat(:,nlocal)         = platform%info%lat
         iv%info(bogus)%lon(:,nlocal)         = platform%info%lon
         iv%info(bogus)%elv(nlocal)           = platform%info%elv
         iv%info(bogus)%pstar(nlocal)         = platform%info%pstar
         iv%info(bogus)%max_lev               = platform%info%levels

         iv%info(bogus)%slp(nlocal)           = platform%loc%slp
         iv%info(bogus)%pw(nlocal)            = platform%loc%pw
         iv%info(bogus)%x(:,nlocal)           = platform%loc%x
         iv%info(bogus)%y(:,nlocal)           = platform%loc%y
         iv%info(bogus)%i(:,nlocal)           = platform%loc%i
         iv%info(bogus)%j(:,nlocal)           = platform%loc%j
         iv%info(bogus)%dx(:,nlocal)          = platform%loc%dx
         iv%info(bogus)%dxm(:,nlocal)         = platform%loc%dxm
         iv%info(bogus)%dy(:,nlocal)          = platform%loc%dy
         iv%info(bogus)%dym(:,nlocal)         = platform%loc%dym
         iv%info(bogus)%proc_domain(:,nlocal) = platform%loc%proc_domain
         ! iv%info(bogus)%proc_domain(:,nlocal) = .true.
         ! iv%info(bogus)%proc_domain  = .true.
         ! iv%info(bogus)%proc_domain(1,1) = .true.

         iv%info(bogus)%obs_global_index(nlocal) = nlocal
      enddo
   enddo
   deallocate(u_lsac)
   deallocate(v_lsac)
   deallocate(t_lsac)
   deallocate(taux_lsac)
   deallocate(p_lsac)
   deallocate(pb_lsac)
   deallocate(q_lsac)
   deallocate(lat_lsac)
   deallocate(lon_lsac)
   deallocate(phb_lsac)
   deallocate(ph_lsac)
   deallocate(height_lsac)
   deallocate(press)
endif

if (lsac_print_details .and. wrf_dm_on_monitor() ) then
   close(iunit)
endif

if (trace_use) call da_trace_exit("da_read_lsac_util")

end subroutine da_read_lsac_wrfinput
