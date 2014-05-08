module module_tornado_genesis
  implicit none
  private

  public :: init_tornado_genesis, calc_tornado_genesis, &
       reset_tornado_genesis, request_tg_reset

  real, parameter :: wwind_cutoff = 40000.0 ! pascals

contains

  subroutine update_tg_time(grid,init)
    ! Helper function that updates the three time interval variables
    ! based on the grid's clock.  If init=.true. then both times
    ! (interval start and end) are set to the current time, otherwise
    ! only the interval end is updated.  In either case, tg_duration
    ! is set to the length in seconds of the interval.
    use module_domain, only: domain, domain_get_time_since_sim_start
    use module_symbols_util, only: WRFU_TimeIntervalGet, WRFU_TimeInterval
    type(domain), intent(inout) :: grid
    type(WRFU_TimeInterval) :: since_start
    logical, intent(in) :: init
    integer :: s_i, s_n, s_d

    since_start=domain_get_time_since_sim_start(grid)
    s_i=0
    s_n=0
    s_d=1
    call WRFU_TimeIntervalGet(since_start,S=s_i,Sn=s_n,Sd=s_d)
    if(s_d==0) s_d=1
    grid%tg_interval_end=real(s_i) + real(s_n)/real(s_d)
    if(init)  grid%tg_interval_start=grid%tg_interval_end
    grid%tg_duration=grid%tg_interval_end-grid%tg_interval_start
  end subroutine update_tg_time

  subroutine init_tg_vars(grid,config_flags, &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )
    ! Helper function that resets all min/max accumulation arrays to 0
    use module_domain, only: domain, get_ijk_from_grid
    use module_configure, only : grid_config_rec_type
    use module_state_description, only: tg_emc2014spc
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j, istart,iend, jstart,jend
    character*255 message

    if(config_flags%tg_option/=tg_emc2014spc) return
    jstart=max(jds,jps)
    jend=min(jpe,jde-1)
    istart=max(ids,ips)
    iend=min(ipe,ide-1)

3012 format("Grid ",I2,": filling tornado genesis data with zeros")
    write(message,3012) grid%id
    call wrf_debug(1,message)

    do j=jstart,jend
       do i=istart,iend
          grid%tg_max_m10wind(i,j)=0
          grid%tg_max_wwind(i,j)=0
          grid%tg_min_wwind(i,j)=0
          grid%tg_max_zhel_25(i,j)=0
          grid%tg_min_zhel_25(i,j)=0
          grid%tg_max_zhel_03(i,j)=0
          grid%tg_min_zhel_03(i,j)=0
          grid%tg_max_updhel03(i,j)=0
          grid%tg_max_updhel25(i,j)=0
          grid%tg_updhel03(i,j)=0
          grid%tg_updhel25(i,j)=0
          grid%tg_total_precip(i,j)=0
       enddo
    enddo

    if(size(grid%tlow)>1 .and. size(grid%zlow)>1) then
    do j=jstart,jend
       do i=istart,iend
          grid%tlow(i,j)=0
          grid%zlow(i,j)=0
       enddo
    enddo
    endif

    if(size(grid%rotangle)>1) then
       do j=jstart,jend
          do i=istart,iend
             grid%rotangle(i,j)=0
          end do
       end do
    endif

    grid%tg_interval_end=grid%tg_interval_start
    grid%tg_duration=0.0
    grid%tg_want_reset=0
  end subroutine init_tg_vars

  subroutine init_tornado_genesis(grid,config_flags)
    ! Called to initialize tornado genesis data arrays.  Should only
    ! be called at initial time.
    use module_domain, only: domain, get_ijk_from_grid
    use module_state_description, only: tg_emc2014spc
    use module_configure, only : grid_config_rec_type
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE

    grid%tg_want_reset=0  ! to avoid needless calls to reset_tornado_genesis
    if(config_flags%tg_option/=tg_emc2014spc) return

    if(grid%hydro) then
       call wrf_error_fatal('Tornado genesis products require non-hydrostatic integration.')
    endif

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )
    call init_tg_vars(grid,config_flags, &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         ips, ipe, jps, jpe, kps, kpe    )
    call update_tg_time(grid,.true.)
  end subroutine init_tornado_genesis

  subroutine request_tg_reset(grid,config_flags,stream)
    use module_state_description, only: tg_emc2014spc
    use module_domain, only: domain, get_ijk_from_grid
    use module_configure, only : grid_config_rec_type
    use module_io_domain, only: first_history
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer, intent(in) :: stream
    character*255 :: message
    integer :: histnum

    if(config_flags%tg_option/=tg_emc2014spc) return

    histnum=stream-first_history
    if(config_flags%tg_reset_stream == histnum) then
3012   format('Grid ',I2,': resetting tornado genesis data after stream ',I0,' output')
       write(message,3012) grid%id,histnum
       call wrf_message(trim(message))
       grid%tg_want_reset=1
    endif
  end subroutine request_tg_reset

  subroutine reset_tornado_genesis(grid,config_flags)
    ! Called after writing output for a given stream.  Resets all
    ! min/max information for all fields if the stream is the
    ! tg_reset_stream.  
    use module_state_description, only: tg_emc2014spc
    use module_domain, only: domain, get_ijk_from_grid
    use module_configure, only : grid_config_rec_type
    use module_io_domain, only: first_history
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE
    character*255 :: message
    integer :: histnum

    if(config_flags%tg_option/=tg_emc2014spc) return
    if(grid%tg_want_reset==0) return

3012 format('Grid ',I2,': resetting tornado genesis data')
    write(message,3012) grid%id
    call wrf_message(trim(message))

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    ! Previous interval end time is now this interval's start time
    ! since we're entering the next interval:
    grid%tg_interval_start=grid%tg_interval_end

    call init_tg_vars(grid,config_flags, &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         ips, ipe, jps, jpe, kps, kpe    )
  end subroutine reset_tornado_genesis

  subroutine rotate_winds(grid,config_flags, &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         ips, ipe, jps, jpe, kps, kpe    )
    ! Compute wind rotation angle
    use module_model_constants, only: DEGRAD
    use module_domain, only: domain
    use module_configure, only : grid_config_rec_type
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j
    real :: cenlat,cenlon, lmbd0,phi0, cos_phi0,sin_phi0
    real :: big_denom, relm, lat, lon, cos_alpha, sin_alpha

    ! Get the projection center from the MOAD center:
    call nl_get_cen_lat(1,cenlat)
    call nl_get_cen_lon(1,cenlon)
    if(cenlon<0) cenlon=cenlon+360.
    lmbd0=cenlon*DEGRAD
    phi0=cenlat*DEGRAD
    
    cos_phi0=cos(phi0)
    sin_phi0=sin(phi0)
    do j=max(jps,jds),min(jpe,jde-1)
       do i=max(ips,ids),min(ipe,ide-1)
          lon=grid%GLON(i,j)
          lat=grid%GLAT(i,j)
          relm=lon-lmbd0
          big_denom=cos(asin( cos_phi0*sin(lat) - sin_phi0*cos(lat)*cos(relm) ))
          sin_alpha=sin_phi0*sin(relm)/big_denom
          cos_alpha=(cos_phi0*cos(lat)+sin_phi0*sin(lat)*cos(relm))/big_denom
          grid%rotangle(i,j) = atan2(sin_alpha,cos_alpha)
       enddo
    enddo
  end subroutine rotate_winds

  subroutine calc_tornado_genesis(grid,config_flags)
    ! Updates max/min information for tornado genesis wind fields from
    ! grid data at the current time.  The tg_total_precip is handled
    ! in module_PHYSICS_CALLS instead.
    use module_comm_dm, only: HALO_NMM_C_sub
    use module_state_description, only: tg_emc2014spc
    use module_domain, only: domain, get_ijk_from_grid
    use module_configure, only : grid_config_rec_type
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_maxval_real, wrf_dm_minval_real, &
         ntasks_x, ntasks_y, mytask, ntasks, local_communicator
#endif
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j,k, istart,iend, jstart,jend, a, imin,imax
    real :: dudy, dvdx, w, zhel, maxmaxwind, minminw, maxmaxw, sec, updhel03, updhel25
    real :: height, height1, height2, height0, maxmaxzhel, minminzhel, updhelpart
    character*255 :: message

    if(config_flags%tg_option/=tg_emc2014spc) return
    if(grid%hydro) then
       call wrf_error_fatal('Tornado genesis products require non-hydrostatic integration.')
    endif

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )
    jstart=max(jps,jds+1)
    jend=min(jpe,jde-2)
    istart=max(ips,ids+1)
    iend=min(ipe,ide-2)
    imin=max(ips,ids)
    imax=min(ipe,ide-1)

#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif

    if(size(grid%tlow)>1 .and. size(grid%zlow)>1) then
       ! Near surface Z & T for wave model:
       do j=max(jps,jds),min(jpe,jde-1)
          do i=max(ips,ids),min(ipe,ide-1)
             grid%tlow(i,j)=grid%T(i,j,kds)
             grid%zlow(i,j)=(grid%Z(i,j,kds+1)-grid%Z(i,j,kds))/2
          enddo
       enddo
    endif

    if(size(grid%rotangle)>1) then
       call rotate_winds(grid,config_flags, &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         ips, ipe, jps, jpe, kps, kpe    )
    endif

    ! Maximum 10m wind vector magnitude:
    maxmaxwind=0.0
    do j=jstart,jend
       do i=istart,iend
          grid%tg_max_m10wind(i,j)=max(grid%tg_max_m10wind(i,j), &
               sqrt(grid%u10(i,j)*grid%u10(i,j) + grid%v10(i,j)*grid%v10(i,j)))
          maxmaxwind=max(maxmaxwind,grid%tg_max_m10wind(i,j))
       enddo
    enddo

#ifdef DM_PARALLEL
    call wrf_dm_maxval_real(maxmaxwind,i,j)
#endif

    ! Min/max vertical wind below 400mbar:
    minminw=0.0
    maxmaxw=0.0
    do j=jstart,jend
       do i=istart,iend
          kloop: do k=kds+1,kde-1
             if(grid%pint(i,j,k)<wwind_cutoff) exit kloop
             w=grid%w(i,j,k)
             grid%tg_min_wwind(i,j)=min(grid%tg_min_wwind(i,j),w)
             minminw=min(minminw,grid%tg_min_wwind(i,j))
             grid%tg_max_wwind(i,j)=max(grid%tg_max_wwind(i,j),w)
             maxmaxw=max(maxmaxw,grid%tg_max_wwind(i,j))
          enddo kloop
       enddo
    enddo

#ifdef DM_PARALLEL
    call wrf_dm_maxval_real(maxmaxw,i,j)
    call wrf_dm_minval_real(minminw,i,j)
#endif

    ! Min/max helicity for 0-3km layer and 2-5km layer.  Note this is
    ! X km above ground (lowest interface level geopotential height),
    ! not above sea level.
    minminzhel=0.0
    maxmaxzhel=0.0
    do j=jstart,jend
       a=mod(j,2)
       do i=istart,iend
          k=kds
          height0=grid%Z(i,j,k)   ! height0=lowest interface level height
          height1=0.0             ! height1=lower height bound for layer
          updhel03=0
          updhel25=0
          do while(k<kde-1 .and. height1<5000.0)
             height2=grid%Z(i,j,k+1)-height0 ! height2=layer upper height bound

             dvdx = (grid%v(i+1-a,j,k)-grid%v(i-a,j,k))/(2.*grid%dx_nmm(i,j))
             dudy = (grid%u(i,j+1,k)-grid%u(i,j-1,k))/(2.*grid%dy_nmm)
             zhel = (dvdx-dudy) * (grid%w(i,j,k) + grid%w(i,j,k+1))/2
             
             if(height1<3000.0) then
                grid%tg_max_zhel_03(i,j)=max(grid%tg_max_zhel_03(i,j),zhel)
                grid%tg_min_zhel_03(i,j)=min(grid%tg_min_zhel_03(i,j),zhel)
                minminzhel=min(grid%tg_min_zhel_03(i,j),minminzhel)
                maxmaxzhel=max(grid%tg_max_zhel_03(i,j),maxmaxzhel)
                updhelpart=max(zhel*(height2-height1),0.)
                if(grid%glat(i,j)<0) updhelpart=-updhelpart
                if(updhelpart>0) then
                   updhel03=updhel03+updhelpart
                endif
             endif
             if(height2>2000.0) then
                grid%tg_max_zhel_25(i,j)=max(grid%tg_max_zhel_25(i,j),zhel)
                grid%tg_min_zhel_25(i,j)=min(grid%tg_min_zhel_25(i,j),zhel)
                minminzhel=min(grid%tg_min_zhel_25(i,j),minminzhel)
                maxmaxzhel=max(grid%tg_max_zhel_25(i,j),maxmaxzhel)
                updhelpart=max(zhel*(height2-height1),0.)
                if(grid%glat(i,j)<0) updhelpart=-updhelpart
                if(updhelpart>0) then
                   updhel25=updhel25+updhelpart
                endif
             endif

             k=k+1
             height1=height2
          enddo
          grid%tg_updhel25(i,j)=updhel25
          grid%tg_updhel03(i,j)=updhel03
          if(updhel25>grid%tg_max_updhel25(i,j)) &
               grid%tg_max_updhel25(i,j)=updhel25
          if(updhel03>grid%tg_max_updhel03(i,j)) &
               grid%tg_max_updhel03(i,j)=updhel03
       enddo
    enddo

#ifdef DM_PARALLEL
    call wrf_dm_maxval_real(maxmaxzhel,i,j)
    call wrf_dm_minval_real(minminzhel,i,j)
#endif

    ! I boundaries copy from nearest point that has data, excluding corner points:
    if(ips<=ids) then
       grid%tg_max_zhel_25(ids,jstart:jend)=grid%tg_max_zhel_25(ids+1,jstart:jend)
       grid%tg_max_zhel_03(ids,jstart:jend)=grid%tg_max_zhel_03(ids+1,jstart:jend)
       grid%tg_min_zhel_25(ids,jstart:jend)=grid%tg_min_zhel_25(ids+1,jstart:jend)
       grid%tg_min_zhel_03(ids,jstart:jend)=grid%tg_min_zhel_03(ids+1,jstart:jend)
       grid%tg_updhel25(ids,jstart:jend)=grid%tg_updhel25(ids+1,jstart:jend)
       grid%tg_updhel03(ids,jstart:jend)=grid%tg_updhel03(ids+1,jstart:jend)
       grid%tg_max_updhel25(ids,jstart:jend)=grid%tg_max_updhel25(ids+1,jstart:jend)
       grid%tg_max_updhel03(ids,jstart:jend)=grid%tg_max_updhel03(ids+1,jstart:jend)
       grid%tg_max_wwind(ids,jstart:jend)=grid%tg_max_wwind(ids+1,jstart:jend)
       grid%tg_min_wwind(ids,jstart:jend)=grid%tg_min_wwind(ids+1,jstart:jend)
       grid%tg_max_m10wind(ids,jstart:jend)=grid%tg_max_m10wind(ids+1,jstart:jend)
    endif

    if(ipe>=ide-2) then
       grid%tg_max_zhel_25(ide-1,jstart:jend)=grid%tg_max_zhel_25(ide-2,jstart:jend)
       grid%tg_max_zhel_03(ide-1,jstart:jend)=grid%tg_max_zhel_03(ide-2,jstart:jend)
       grid%tg_min_zhel_25(ide-1,jstart:jend)=grid%tg_min_zhel_25(ide-2,jstart:jend)
       grid%tg_min_zhel_03(ide-1,jstart:jend)=grid%tg_min_zhel_03(ide-2,jstart:jend)
       grid%tg_updhel25(ide-1,jstart:jend)=grid%tg_updhel25(ide-2,jstart:jend)
       grid%tg_updhel03(ide-1,jstart:jend)=grid%tg_updhel03(ide-2,jstart:jend)
       grid%tg_max_updhel25(ide-1,jstart:jend)=grid%tg_max_updhel25(ide-2,jstart:jend)
       grid%tg_max_updhel03(ide-1,jstart:jend)=grid%tg_max_updhel03(ide-2,jstart:jend)
       grid%tg_max_wwind(ide-1,jstart:jend)=grid%tg_max_wwind(ide-2,jstart:jend)
       grid%tg_min_wwind(ide-1,jstart:jend)=grid%tg_min_wwind(ide-2,jstart:jend)
       grid%tg_max_m10wind(ide-1,jstart:jend)=grid%tg_max_m10wind(ide-2,jstart:jend)
    endif

    ! J boundaries: copy from nearest point that has data.  We use
    ! imin:imax instead of istart:iend to get the corner points.
    if(jps<=jds) then
       grid%tg_max_zhel_25(imin:imax,jds)=grid%tg_max_zhel_25(imin:imax,jds+1)
       grid%tg_max_zhel_03(imin:imax,jds)=grid%tg_max_zhel_03(imin:imax,jds+1)
       grid%tg_min_zhel_25(imin:imax,jds)=grid%tg_min_zhel_25(imin:imax,jds+1)
       grid%tg_min_zhel_03(imin:imax,jds)=grid%tg_min_zhel_03(imin:imax,jds+1)
       grid%tg_updhel25(imin:imax,jds)=grid%tg_updhel25(imin:imax,jds+1)
       grid%tg_updhel03(imin:imax,jds)=grid%tg_updhel03(imin:imax,jds+1)
       grid%tg_max_updhel25(imin:imax,jds)=grid%tg_max_updhel25(imin:imax,jds+1)
       grid%tg_max_updhel03(imin:imax,jds)=grid%tg_max_updhel03(imin:imax,jds+1)
       grid%tg_max_wwind(imin:imax,jds)=grid%tg_max_wwind(imin:imax,jds+1)
       grid%tg_min_wwind(imin:imax,jds)=grid%tg_min_wwind(imin:imax,jds+1)
       grid%tg_max_m10wind(imin:imax,jds)=grid%tg_max_m10wind(imin:imax,jds+1)
    endif

    if(jpe>=jde-2) then
       grid%tg_max_zhel_25(imin:imax,jde-1)=grid%tg_max_zhel_25(imin:imax,jde-2)
       grid%tg_max_zhel_03(imin:imax,jde-1)=grid%tg_max_zhel_03(imin:imax,jde-2)
       grid%tg_min_zhel_25(imin:imax,jde-1)=grid%tg_min_zhel_25(imin:imax,jde-2)
       grid%tg_min_zhel_03(imin:imax,jde-1)=grid%tg_min_zhel_03(imin:imax,jde-2)
       grid%tg_updhel25(imin:imax,jde-1)=grid%tg_updhel25(imin:imax,jde-2)
       grid%tg_updhel03(imin:imax,jde-1)=grid%tg_updhel03(imin:imax,jde-2)
       grid%tg_max_updhel25(imin:imax,jde-1)=grid%tg_max_updhel25(imin:imax,jde-2)
       grid%tg_max_updhel03(imin:imax,jde-1)=grid%tg_max_updhel03(imin:imax,jde-2)
       grid%tg_max_wwind(imin:imax,jde-1)=grid%tg_max_wwind(imin:imax,jde-2)
       grid%tg_min_wwind(imin:imax,jde-1)=grid%tg_min_wwind(imin:imax,jde-2)
       grid%tg_max_m10wind(imin:imax,jde-1)=grid%tg_max_m10wind(imin:imax,jde-2)
    endif

    call update_tg_time(grid,.false.)

3313 format('TG extrema: max(wind)=',F0.2,' max(w)=',F0.2,' min(w)=',F0.2,'  max(zhel)=',F0.4,' min(zhel)=',F0.4)
    write(message,3313) maxmaxwind,maxmaxw,minminw,maxmaxzhel,minminzhel
    call wrf_debug(1,message)
  end subroutine calc_tornado_genesis

end module module_tornado_genesis

subroutine nmm_request_tg_reset(grid,config_flags,stream)
  ! This subroutine is a wrapper kludge to work around the WRF build
  ! order and limitations of make.  The module_tornado_genesis module
  ! file does not exist when mediation_integrate is compiled, so
  ! med_hist_out has to call a non-module function instead.
  use module_domain, only: domain
  use module_configure, only : grid_config_rec_type 
  use module_tornado_genesis, only: request_tg_reset
  implicit none
  integer, intent(in) :: stream
  type(domain), intent(inout) :: grid
  type(grid_config_rec_type), intent(in) :: config_flags
  call request_tg_reset(grid,config_flags,stream)
end subroutine nmm_request_tg_reset
