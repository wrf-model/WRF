subroutine da_read_lsac_wrfinput(iv, onlyscan)

   !-----------------------------------------------------------------------
   ! Purpose: extract pseduo-observations from wrfinput and use them as bogus
   ! observations to constrain the large-scale pattern of WRFDA analysis
   !
   !    Author: Xiaowen Tang,  NJU,  Date: 5/2/2016
   !    Update 7/15/2017: fix the problem of staggering variables
   !---------------------------------------------------------------------

implicit none

type (iv_type), intent(inout)       ::  iv
logical       , intent(in)          ::  onlyscan

type (multi_level_type)             ::  platform
logical                             ::  outside, outside_all

integer                             ::  i, j, k, ki, ndims, nrecs, nlocal, iunit, nlevels, ilevel
integer                             ::  u_qc, v_qc, t_qc, q_qc
character(len=512)                  ::  lsac_wrfinput
integer, dimension(4)               ::  dims_u, dims_v, dims_t, dims_p, dims_pb, dims_q
integer, dimension(4)               ::  dims_lat, dims_lon, dims_alt, dims_phb, dims_ph
real, allocatable, dimension(:,:,:) ::  u_lsac, v_lsac, w_lsac, theta_lsac, p_lsac, q_lsac, tk_lsac
real, allocatable, dimension(:,:,:) ::  pb_lsac, ph_lsac, phb_lsac, height_lsac, press
real, allocatable, dimension(:,:)   ::  lat_lsac, lon_lsac
logical                             ::  debug
logical, external                   ::  wrf_dm_on_monitor
logical                             ::  isfile


if (trace_use) call da_trace_entry("da_read_lsac_util")

lsac_wrfinput  = 'fg_l'
debug=.false.

inquire(file=trim(lsac_wrfinput), exist=isfile)
if ( .not. isfile ) then
   write(unit=message(1),fmt='(a,a,a)') 'File ',trim(lsac_wrfinput),' for LSAC is missing.'
   call da_error(__FILE__,__LINE__,message(1:1))
endif

if (onlyscan) then
   if (rootproc) then
      call da_get_dims_cdf( lsac_wrfinput, 'XLAT',   dims_lat,  ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'XLONG',  dims_lon,  ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'T',      dims_t,    ndims, debug)
   endif
   call wrf_dm_bcast_integer(dims_lon, 4)
   call wrf_dm_bcast_integer(dims_lat, 4)
   call wrf_dm_bcast_integer(dims_t,   4)
#ifdef DEBUG_LSAC
   print *, '****SCAN LSAC lat_dims: ', myproc, dims_lat(1), dims_lat(2), dims_lat(3), dims_lat(4)
   print *, '****SCAN LSAC lon_dims: ', myproc, dims_lon(1), dims_lon(2), dims_lon(3), dims_lon(4)
   print *, '****SCAN LSAC t_dims: ', myproc, dims_t(1), dims_t(2), dims_t(3), dims_t(4)
#endif
   allocate(lat_lsac(dims_lat(1), dims_lat(2)))
   allocate(lon_lsac(dims_lon(1), dims_lon(2)))

   nlevels = (dims_t(3)-lsac_nv_start+1)/lsac_nv_step

   !---------------------------------------------------------
   ! Reading data from WRF Input file
   !---------------------------------------------------------
   if (rootproc) then
      call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLAT',   lat_lsac, dims_lat(1), dims_lat(2), 1, debug)
      call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLONG',  lon_lsac, dims_lon(1), dims_lon(2), 1, debug)
   endif
   call wrf_dm_bcast_real(lat_lsac, dims_lat(1)*dims_lat(2))
   call wrf_dm_bcast_real(lon_lsac, dims_lon(1)*dims_lon(2))

   !Assigning max_lev and counts in the iv type in onlyscan mode
   nlocal=0
   nrecs =0
   do i=1, dims_lon(1), lsac_nh_step
      do j=1, dims_lat(2), lsac_nh_step
         platform%info%lat      = lat_lsac(i,j)
         platform%info%lon      = lon_lsac(i,j)
         platform%info%elv      = 0.0
         platform%info%name     = 'LSAC'
         platform%info%platform = 'FM-???  LSAC'
         platform%info%id       = '?????'
         platform%info%date_char= '????-??-??_??:??:??'
         platform%info%pstar    = 0.D0
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
#ifdef DEBUG_LSAC
   print *, '******SCAN LSAC: ', myproc, nlevels, nlocal, nrecs
#endif
   iv%info(bogus)%max_lev = nlevels
   iv%info(bogus)%nlocal  = nlocal
   iv%info(bogus)%ntotal  = nrecs
   deallocate(lat_lsac)
   deallocate(lon_lsac)
else
   !---------------------------------------------------------
   ! Getting information from NETCDF files (WRF Input file)
   !---------------------------------------------------------
   if (rootproc) then
      call da_get_dims_cdf( lsac_wrfinput, 'XLAT',   dims_lat,  ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'XLONG',  dims_lon,  ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'U',      dims_u,    ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'V',      dims_v,    ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'T',      dims_t,    ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'PB',     dims_pb,   ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'P',      dims_p,    ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'QVAPOR', dims_q,    ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'PHB',    dims_phb,  ndims, debug)
      call da_get_dims_cdf( lsac_wrfinput, 'PH',     dims_ph,   ndims, debug)
   endif

#ifdef DEBUG_LSAC
   print *, 'lat_dims: ', myproc, dims_lat(1), dims_lat(2), dims_lat(3), dims_lat(4)
   print *, 'lon_dims: ', myproc, dims_lon(1), dims_lon(2), dims_lon(3), dims_lon(4)
   print *, 'u_dims: ', myproc, dims_u(1), dims_u(2), dims_u(3), dims_u(4)
   print *, 'v_dims: ', myproc, dims_v(1), dims_v(2), dims_v(3), dims_v(4)
   print *, 't_dims: ', myproc, dims_t(1), dims_t(2), dims_t(3), dims_t(4)
   print *, 'p_dims: ', myproc, dims_p(1), dims_p(2), dims_p(3), dims_p(4)
   print *, 'pb_dims: ', myproc, dims_pb(1), dims_pb(2), dims_pb(3), dims_pb(4)
   print *, 'q_dims: ', myproc, dims_q(1), dims_q(2), dims_q(3), dims_q(4)
   print *, 'ph_dims: ', myproc, dims_ph(1), dims_ph(2), dims_ph(3), dims_ph(4)
   print *, 'phb_dims: ', myproc, dims_phb(1), dims_phb(2), dims_phb(3), dims_phb(4)
#endif

   call wrf_dm_bcast_integer(dims_u,   4)
   call wrf_dm_bcast_integer(dims_v,   4)
   call wrf_dm_bcast_integer(dims_t,   4)
   call wrf_dm_bcast_integer(dims_p,   4)
   call wrf_dm_bcast_integer(dims_pb,  4)
   call wrf_dm_bcast_integer(dims_q,   4)
   call wrf_dm_bcast_integer(dims_lat, 4)
   call wrf_dm_bcast_integer(dims_lon, 4)
   call wrf_dm_bcast_integer(dims_phb, 4)
   call wrf_dm_bcast_integer(dims_ph,  4)

   nrecs   = (1 + (dims_lat(1) - 1)/lsac_nh_step) * (1 + (dims_lat(2) - 1) / lsac_nh_step)
   nlevels = (dims_t(3) - lsac_nv_start + 1) / lsac_nv_step

   !---------------------------------------------------------
   ! Allocating memory
   !---------------------------------------------------------
   allocate(lat_lsac(dims_lat(1),   dims_lat(2)))
   allocate(lon_lsac(dims_lon(1),   dims_lon(2)))
   allocate(u_lsac(dims_u(1),       dims_u(2),   dims_u(3)   ))
   allocate(v_lsac(dims_v(1),       dims_v(2),   dims_v(3)   ))
   allocate(theta_lsac(dims_t(1),   dims_t(2),   dims_t(3)   ))
   allocate(p_lsac(dims_p(1),       dims_p(2),   dims_p(3)   ))
   allocate(pb_lsac(dims_pb(1),     dims_pb(2),  dims_pb(3)  ))
   allocate(q_lsac(dims_q(1),       dims_q(2),   dims_q(3)   ))
   allocate(phb_lsac(dims_phb(1),   dims_phb(2), dims_phb(3) ))
   allocate(ph_lsac(dims_ph(1),     dims_ph(2),  dims_ph(3)  ))
   allocate(height_lsac(dims_ph(1), dims_ph(2),  dims_ph(3)  ))
   allocate(press(dims_p(1),        dims_p(2),   dims_p(3)   ))
   allocate(tk_lsac(dims_t(1),      dims_t(2),   dims_t(3)   ))

   !---------------------------------------------------------
   ! Reading data from WRF Input file
   !---------------------------------------------------------
   if (rootproc) then
      call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLAT',   lat_lsac,  dims_lat(1), dims_lat(2), 1, debug)
      call da_get_var_2d_real_cdf( lsac_wrfinput, 'XLONG',  lon_lsac,  dims_lon(1), dims_lon(2), 1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'U',      u_lsac,    dims_u(1),   dims_u(2),   dims_u(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'V',      v_lsac,    dims_v(1),   dims_v(2),   dims_v(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'T',      theta_lsac,dims_t(1),   dims_t(2),   dims_t(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'P',      p_lsac,    dims_p(1),   dims_p(2),   dims_p(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'PB',     pb_lsac,   dims_p(1),   dims_p(2),   dims_p(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'QVAPOR', q_lsac,    dims_q(1),   dims_q(2),   dims_q(3),   1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'PHB',    phb_lsac,  dims_phb(1), dims_phb(2), dims_phb(3), 1, debug)
      call da_get_var_3d_real_cdf( lsac_wrfinput, 'PH',     ph_lsac,   dims_ph(1),  dims_ph(2),  dims_ph(3),  1, debug)
   endif
   call wrf_dm_bcast_real(lat_lsac,   PRODUCT(dims_lat(1:2)))
   call wrf_dm_bcast_real(lon_lsac,   PRODUCT(dims_lon(1:2)))
   call wrf_dm_bcast_real(u_lsac,     PRODUCT(dims_u(1:3))  )
   call wrf_dm_bcast_real(v_lsac,     PRODUCT(dims_v(1:3))  )
   call wrf_dm_bcast_real(theta_lsac, PRODUCT(dims_t(1:3))  )
   call wrf_dm_bcast_real(p_lsac,     PRODUCT(dims_p(1:3))  )
   call wrf_dm_bcast_real(pb_lsac,    PRODUCT(dims_p(1:3))  )
   call wrf_dm_bcast_real(q_lsac,     PRODUCT(dims_q(1:3))  )
   call wrf_dm_bcast_real(phb_lsac,   PRODUCT(dims_phb(1:3)))
   call wrf_dm_bcast_real(ph_lsac,    PRODUCT(dims_ph(1:3)) )

   !---------------------------------------------------------
   !Calculating the height
   !---------------------------------------------------------
   height_lsac = (phb_lsac + ph_lsac) / gravity
   press = (p_lsac + pb_lsac) * 0.01

   !Temperature from potential temperature
   tk_lsac = (t0 + theta_lsac) * ((press / 1000.0) ** (gas_constant / cp))

   if (lsac_print_details .and. rootproc) then
      call da_get_unit(iunit)
      open(iunit, file='lsac_details')
   endif

   ! Assigning errors, heights and inv in the iv type
   nlocal=0
   do i=1, dims_lon(1), lsac_nh_step
      do j=1, dims_lat(2), lsac_nh_step
         ilevel = 0
         do k=lsac_nv_start, dims_t(3), lsac_nv_step
            ilevel = ilevel+1
            if (lsac_use_u) then
               u_qc   = 0
            else
               u_qc   = missing_data
            endif
            if (lsac_use_v) then
               v_qc   = 0
            else
               v_qc   = missing_data
            endif
            if (lsac_use_t) then
               t_qc   = 0
            else
               t_qc   = missing_data
            endif
            if (lsac_use_q) then
               q_qc   = 0
            else
               q_qc   = missing_data
            endif

            platform%each(ilevel)%height= (height_lsac(i,j,k)+height_lsac(i,j,k+1))/2.

            platform%each(ilevel)%u%inv= (u_lsac(i,j,k)+u_lsac(i+1,j,k))/2.
            platform%each(ilevel)%u%error=lsac_u_error
            platform%each(ilevel)%u%qc=u_qc

            platform%each(ilevel)%v%inv= (v_lsac(i,j,k)+v_lsac(i,j+1,k))/2.
            platform%each(ilevel)%v%error=lsac_v_error
            platform%each(ilevel)%v%qc=v_qc

            platform%each(ilevel)%t%inv=tk_lsac(i,j,k)
            platform%each(ilevel)%t%error=lsac_t_error
            platform%each(ilevel)%t%qc=t_qc

            platform%each(ilevel)%q%inv=q_lsac(i,j,k)
            platform%each(ilevel)%q%error=lsac_q_error
            platform%each(ilevel)%q%qc=q_qc

            if(lsac_print_details .and. rootproc) then
               write(iunit,'(3I5,3f10.3,x,4(f10.3,x,f10.3,x,i4))') i, j, k, &
                 (height_lsac(i,j,k) + height_lsac(i,j,k+1))/2., lat_lsac(i,j), lon_lsac(i,j), &
                 (u_lsac(i,j,k)+u_lsac(i+1,j,k))/2.,   lsac_u_error     ,   u_qc, &
                 (v_lsac(i,j,k)+v_lsac(i,j+1,k))/2.,   lsac_v_error     ,   v_qc, &
                 tk_lsac(i,j,k)                    ,   lsac_t_error     ,   t_qc, &
                 q_lsac(i,j,k)*1000.               ,   lsac_q_error*1000.,  q_qc
            endif
         enddo
         platform%info%lat      = lat_lsac(i,j)
         platform%info%lon      = lon_lsac(i,j)
         platform%info%elv      = 0.
         platform%info%name     = 'LSAC'
         platform%info%platform = 'FM-???  LSAC'
         platform%info%id       = '?????'
         platform%info%date_char= '????-??-??_??:??:??'
         platform%info%pstar    = 0.D0
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
   deallocate(theta_lsac)
   deallocate(tk_lsac)
   deallocate(p_lsac)
   deallocate(pb_lsac)
   deallocate(q_lsac)
   deallocate(lat_lsac)
   deallocate(lon_lsac)
   deallocate(phb_lsac)
   deallocate(ph_lsac)
   deallocate(height_lsac)
   deallocate(press)

   if (lsac_print_details .and. rootproc) then
      close(iunit)
      call da_free_unit(iunit)
   endif

endif !onlyscan

if (trace_use) call da_trace_exit("da_read_lsac_util")

end subroutine da_read_lsac_wrfinput
