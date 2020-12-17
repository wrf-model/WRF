module readwrf_module

  real*4, parameter :: sclht = 287.04 * 256.0 / 9.81 
  real*4, parameter :: eps = 0.622
  real*4, parameter :: ezero = 6.112
  real*4, parameter :: eslcon1 = 17.67
  real*4, parameter :: eslcon2 = 29.65
  real*4, parameter :: gamma = 287.04/1004.
  real*4, parameter :: gammamd = 0.608-0.887
  real*4, parameter :: pi = 3.1415926           ! Value used in WRF.
  real*4, parameter :: earth_radius = 6370.0    ! Value used in WRF.
  real*4, parameter :: deg_to_rad = pi/180.0
  real*4, parameter :: rad_to_deg = 1.0/deg_to_rad

  real*4 :: ptop
  real*4, allocatable, dimension(:) :: znw, znu, znfac
  integer, allocatable, dimension(:,:) :: landmask
  real*4, allocatable, dimension(:,:) :: wrf_psfc, wrf_t2, wrf_q2, wrf_u10, wrf_v10, wrf_rh2, wrf_xlat, wrf_xlong
  real*4, allocatable, dimension(:,:,:) :: wrf_t, wrf_u, wrf_v, wrf_p, wrf_pb, wrf_ph, wrf_phb, wrf_ght, wrf_q, wrf_rh, wrf_tdp

  integer :: kdim_stag

  contains
    function readwrf(filename, west_east_dim, south_north_dim, dx, dy, cen_lat, cen_lon, &
                 stand_lon, true1, true2, mapproj, idim, jdim, kdim, idim_stag, jdim_stag, &
                 ratio, miycors, mjxcors)

    implicit none

#include "netcdf.inc"

    ! Arguments
    integer :: mapproj, west_east_dim, south_north_dim, idim, jdim, kdim, idim_stag, jdim_stag
    real*4 :: dx, dy, cen_lat, cen_lon, stand_lon, true1, true2, ratio, miycors, mjxcors
    character (len=*) :: filename

    ! Local variables
    integer :: readwrf
    integer :: status, ncid, n_dimensions, n_variables, n_attributes, unltd_dim_id
    integer :: xtype, ndims, n_atts, i, j, xtype_att, l, att_num, dimid
    integer :: ii, jj, kk
    integer, dimension(NF_MAX_VAR_DIMS) :: dimids
    real*4 :: q, e, es, elog, gammam
    character (len=NF_MAX_NAME) :: varname, attname, dimname

    ! Set for compatibility with
    ratio = 1.0
    miycors = 1.0
    mjxcors = 1.0

    status = nf_open(trim(filename), 0, ncid) 

    if (status /= NF_NOERR) then
      write(6,*) 'Error :: readwrf() : Could not open file '//trim(filename)
      readwrf = 0
      return
    end if

    status = nf_inq(ncid, n_dimensions, n_variables, n_attributes, unltd_dim_id)

    if (status == NF_NOERR) then

    do i=1,n_attributes
      status = nf_inq_attname(ncid, NF_GLOBAL, i, attname)
      if (status == NF_NOERR) then

        status = nf_inq_att(ncid, NF_GLOBAL, attname, xtype, l)
        if (status == NF_NOERR) then
          if (index(trim(attname),'WEST-EAST_GRID_DIMENSION') /= 0) then
            status = nf_get_att_int(ncid, NF_GLOBAL, attname, west_east_dim) 
            if (status /= NF_NOERR) write(6,*) 'Error getting west-east dimension'
          else if (index(trim(attname),'SOUTH-NORTH_GRID_DIMENSION') /= 0) then
            status = nf_get_att_int(ncid, NF_GLOBAL, attname, south_north_dim) 
            if (status /= NF_NOERR) write(6,*) 'Error getting south-north dimension'
          else if (index(trim(attname),'MAP_PROJ') /= 0) then
            status = nf_get_att_int(ncid, NF_GLOBAL, attname, mapproj) 
            if (status /= NF_NOERR) write(6,*) 'Error getting map_projection'
          else if (index(trim(attname),'DX') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, dx) 
            if (status /= NF_NOERR) write(6,*) 'Error getting dx'
!            dx = dx / 1000.
          else if (index(trim(attname),'DY') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, dy) 
            if (status /= NF_NOERR) write(6,*) 'Error getting dy'
!            dy = dy / 1000.
          else if (index(trim(attname),'CEN_LAT') /= 0 .and. len_trim(attname) == 7) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, cen_lat) 
            if (status /= NF_NOERR) write(6,*) 'Error getting cen_lat'
          else if (index(trim(attname),'CEN_LON') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, cen_lon) 
            if (status /= NF_NOERR) write(6,*) 'Error getting cen_lon'
          else if (index(trim(attname),'STAND_LON') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, stand_lon) 
            if (status /= NF_NOERR) write(6,*) 'Error getting stand_lon'
          else if (index(trim(attname),'TRUELAT1') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, true1) 
            if (status /= NF_NOERR) write(6,*) 'Error getting true1'
          else if (index(trim(attname),'TRUELAT2') /= 0) then
            status = nf_get_att_real(ncid, NF_GLOBAL, attname, true2) 
            if (status /= NF_NOERR) write(6,*) 'Error getting true2'
          end if
        end if
      end if
    end do

    do i=1,n_variables
      status = nf_inq_var(ncid, i, varname, xtype, ndims, dimids, n_atts)

      if (status == NF_NOERR) then
        if (index(trim(varname),'P_TOP') /= 0) then !{

         status = nf_get_var_real(ncid, i, ptop)

        else if (index(trim(varname),'LANDMASK') /= 0) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(landmask(idim, jdim))
         status = nf_get_var_real(ncid, i, landmask)
  
        else if (index(trim(varname),'XLAT') /= 0 .and. len_trim(varname) == 4) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_xlat(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_xlat)
  
        else if (index(trim(varname),'XLONG') /= 0 .and. len_trim(varname) == 5) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_xlong(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_xlong)
  
        else if (index(trim(varname),'T') /= 0 .and. len_trim(varname) == 1) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_t(idim, jdim, kdim))
         status = nf_get_var_real(ncid, i, wrf_t)
  
        else if (index(trim(varname),'U') /= 0 .and. len_trim(varname) == 1) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east_stag') /= 0) then
                idim_stag = l
	      else if (index(trim(dimname),'west-east_stag') /= 0) then
                idim_stag = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_u(idim_stag, jdim, kdim))
         status = nf_get_var_real(ncid, i, wrf_u)
  
        else if (index(trim(varname),'V') /= 0 .and. len_trim(varname) == 1) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north_stag') /= 0) then
                jdim_stag = l
              else if (index(trim(dimname),'south-north_stag') /= 0) then
                jdim_stag = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_v(idim, jdim_stag, kdim))
         status = nf_get_var_real(ncid, i, wrf_v)
  
        else if (index(trim(varname),'PSFC') /= 0 .and. len_trim(varname) == 4) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_psfc(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_psfc)
  
        else if (index(trim(varname),'T2') /= 0 .and. len_trim(varname) == 2) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_t2(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_t2)
  
        else if (index(trim(varname),'Q2') /= 0 .and. len_trim(varname) == 2) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_q2(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_q2)
  
        else if (index(trim(varname),'U10') /= 0 .and. len_trim(varname) == 3) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_u10(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_u10)
  
        else if (index(trim(varname),'V10') /= 0 .and. len_trim(varname) == 3) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              end if
            end if
          end do
  
         allocate(wrf_v10(idim, jdim))
         status = nf_get_var_real(ncid, i, wrf_v10)
  
        else if (index(trim(varname),'QVAPOR') /= 0 .and. len_trim(varname) == 6) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_q(idim, jdim, kdim))
         status = nf_get_var_real(ncid, i, wrf_q)
  
        else if (index(trim(varname),'PB') /= 0 .and. len_trim(varname) == 2) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_pb(idim, jdim, kdim))
         status = nf_get_var_real(ncid, i, wrf_pb)
  
        else if (index(trim(varname),'P') /= 0 .and. len_trim(varname) == 1) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
              else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(wrf_p(idim, jdim, kdim))
         status = nf_get_var_real(ncid, i, wrf_p)
  
        else if (index(trim(varname),'PHB') /= 0 .and. len_trim(varname) == 3) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top_stag') /= 0) then
                kdim_stag = l
              else if (index(trim(dimname),'bottom-top_stag') /= 0) then
                kdim_stag = l
              end if
            end if
          end do
  
         allocate(wrf_phb(idim, jdim, kdim_stag))
         status = nf_get_var_real(ncid, i, wrf_phb)
  
        else if (index(trim(varname),'PH') /= 0 .and. len_trim(varname) == 2) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'west_east') /= 0) then
                idim = l
	      else if (index(trim(dimname),'west-east') /= 0) then
                idim = l
              else if (index(trim(dimname),'south_north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'south-north') /= 0) then
                jdim = l
              else if (index(trim(dimname),'bottom_top_stag') /= 0) then
                kdim_stag = l
              else if (index(trim(dimname),'bottom-top_stag') /= 0) then
                kdim_stag = l
              end if
            end if
          end do
  
         allocate(wrf_ph(idim, jdim, kdim_stag))
         status = nf_get_var_real(ncid, i, wrf_ph)
  
        else if (index(trim(varname),'ZNU') /= 0 .and. len_trim(varname) == 3) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'bottom_top') /= 0) then
                kdim = l
	      else if (index(trim(dimname),'bottom-top') /= 0) then
                kdim = l
              end if
            end if
          end do
  
         allocate(znu(kdim))
         status = nf_get_var_real(ncid, i, znu)
  
        else if (index(trim(varname),'ZNW') /= 0 .and. len_trim(varname) == 3) then !{

          do j=1, ndims
            status = nf_inq_dim(ncid, dimids(j), dimname, l)
            if (status == NF_NOERR) then
              if (index(trim(dimname),'bottom-top_stag') /= 0) then
                kdim_stag = l
              else if (index(trim(dimname),'bottom_top_stag') /= 0) then
                kdim_stag = l
              end if
            end if
          end do
  
         allocate(znw(kdim_stag))
         status = nf_get_var_real(ncid, i, znw)
  
        end if !}

      end if
    end do

    end if

    readwrf = 1

    allocate(znfac(kdim))

    do kk=1,kdim
       znfac(kk)=(znw(kk)-znu(kk))/(znw(kk)-znw(kk+1))
    enddo

    do kk=1,kdim_stag
      do jj=1,jdim
        do ii=1,idim
          wrf_ph(ii,jj,kk)=exp(-(wrf_ph(ii,jj,kk)+wrf_phb(ii,jj,kk))/(9.81*sclht))
        end do
      end do
    end do

    allocate(wrf_ght(idim, jdim, kdim))
    
    do kk=1,kdim
      do jj=1,jdim
        do ii=1,idim
          wrf_ght(ii,jj,kk)=znfac(kk)*wrf_ph(ii,jj,kk+1)+(1.-znfac(kk))*wrf_ph(ii,jj,kk)
          wrf_ght(ii,jj,kk)=-sclht*log(wrf_ght(ii,jj,kk))
        end do
      end do
    end do

!    do kk=1,kdim
!      do jj=1,jdim
!        do ii=1,idim
!           wrf_p(ii,jj,kk)=wrf_p(ii,jj,kk) + wrf_pb(ii,jj,kk)
!        end do
!      end do
!    end do

!    do kk=1,kdim
!      do jj=1,jdim
!        do ii=1,idim
!         gammam=gamma*(1.+gammamd*.001*wrf_q(ii,jj,kk))
!         wrf_t(ii,jj,kk)=(wrf_t(ii,jj,kk)+300.)*(wrf_p(ii,jj,kk)/100000.)**gammam
!        end do
!      end do
!    end do

    allocate(wrf_rh(idim, jdim, kdim))

!    do kk=1,kdim
!      do jj=1,jdim
!        do ii=1,idim
!           e = wrf_q(ii,jj,kk)*(wrf_p(ii,jj,kk)/100.)/(eps+wrf_q(ii,jj,kk))
!           es = ezero * exp( eslcon1*(wrf_t(ii,jj,kk)-273.15)/(wrf_t(ii,jj,kk)-eslcon2) )
!           wrf_rh(ii,jj,kk)=100.*(e*((wrf_p(ii,jj,kk)/100.)-es))/(es*((wrf_p(ii,jj,kk)/100.)-e))
!        end do
!      end do
!    end do

    allocate(wrf_tdp(idim, jdim, kdim))

!    do kk=1,kdim
!      do jj=1,jdim
!        do ii=1,idim
!           q=max(wrf_q(ii,jj,kk),1.e-15)
!           e=q*(wrf_p(ii,jj,kk)/100.)/(eps+q)
!           elog=alog(e/ezero)
!           wrf_tdp(ii,jj,kk)=(eslcon2*elog-eslcon1*273.15)/(elog-eslcon1)
!        end do
!      end do
!    end do

    allocate(wrf_rh2(idim, jdim))

    do jj=1,jdim
      do ii=1,idim
         e = wrf_q2(ii,jj)*(wrf_psfc(ii,jj)/100.)/(eps+wrf_q2(ii,jj))
         es = ezero * exp( eslcon1*(wrf_t2(ii,jj)-273.15)/(wrf_t2(ii,jj)-eslcon2) )
         wrf_rh2(ii,jj)=100.*(e*((wrf_psfc(ii,jj)/100.)-es))/(es*((wrf_psfc(ii,jj)/100.)-e))
      end do
    end do

    end function readwrf

end module readwrf_module
