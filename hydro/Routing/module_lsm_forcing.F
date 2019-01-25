!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_lsm_forcing

#ifdef MPP_LAND
    use module_mpp_land
#endif
    use module_HYDRO_io, only: get_2d_netcdf, get_soilcat_netcdf, get2d_int

implicit none
#include <netcdf.inc>
    integer :: i_forcing 
character(len=19) out_date

interface read_hydro_forcing
#ifdef MPP_LAND
   !yw module procedure read_hydro_forcing_mpp
   module procedure read_hydro_forcing_mpp1
#else
   module procedure read_hydro_forcing_seq
#endif
end interface

Contains

  subroutine READFORC_WRF(flnm,ix,jx,target_date,t,q,u,v,p,lw,sw,pcp,lai,fpar)
    
    implicit none
    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx) :: t,q,u,v,p,lw,sw,pcp,pcpc, lai,fpar
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    tlevel = 1
 
    pcp = 0
    pcpc = 0

    ! Open the NetCDF file.
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_WRF Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_WRF() - Problem opening netcdf file")
    endif

    call get_2d_netcdf_ruc("T2",     ncid, t,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("Q2",     ncid, q,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("U10",    ncid, u,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("V10",    ncid, v,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("PSFC",   ncid, p,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("GLW",    ncid, lw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("SWDOWN", ncid, sw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINC",  ncid, pcpc,  ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINNC", ncid, pcp,   ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("VEGFRA", ncid, fpar,  ix, jx,tlevel, .true., ierr)
    if(ierr == 0) then
        if(maxval(fpar) .gt. 10 .and. (maxval(fpar) .lt. 10000) ) fpar = fpar/100.
    endif
    call get_2d_netcdf_ruc("LAI", ncid, lai,  ix, jx,tlevel, .true., ierr)

    ierr = nf_close(ncid)

!DJG  Add the convective and non-convective rain components (note: conv. comp=0
!for cloud resolving runs...) 
!DJG  Note that for WRF these are accumulated values to be adjusted to rates in
!driver...

    pcp=pcp+pcpc   ! assumes pcpc=0 for resolved convection...

  end subroutine READFORC_WRF

  subroutine read_hrldas_hdrinfo(geo_static_flnm, ix, jx, land_cat, soil_cat)
    ! Simply return the dimensions of the grid.
    implicit none
    character(len=*),          intent(in)  :: geo_static_flnm
    integer, intent(out) :: ix, jx, land_cat, soil_cat ! dimensions

    integer :: iret, ncid, dimid

    ! Open the NetCDF file.
    iret = nf_open(geo_static_flnm, NF_NOWRITE, ncid)
    if (iret /= 0) then
       write(*,'("Problem opening geo_static file: ''", A, "''")') &
            trim(geo_static_flnm)
       call hydro_stop("In read_hrldas_hdrinfo() - Problem opening geo_static file")
    endif

    iret = nf_inq_dimid(ncid, "west_east", dimid)

    if (iret /= 0) then
!       print*, "nf_inq_dimid:  west_east"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimid:  west_east problem")
    endif

    iret = nf_inq_dimlen(ncid, dimid, ix)
    if (iret /= 0) then
!       print*, "nf_inq_dimlen:  west_east"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimlen:  west_east problem")
    endif

    iret = nf_inq_dimid(ncid, "south_north", dimid)
    if (iret /= 0) then
!       print*, "nf_inq_dimid:  south_north"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimid:  south_north problem")
    endif

    iret = nf_inq_dimlen(ncid, dimid, jx)
    if (iret /= 0) then
 !      print*, "nf_inq_dimlen:  south_north"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimlen:  south_north problem")
    endif

    iret = nf_inq_dimid(ncid, "land_cat", dimid)
    if (iret /= 0) then
 !      print*, "nf_inq_dimid:  land_cat"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimid:  land_cat problem")
    endif

    iret = nf_inq_dimlen(ncid, dimid, land_cat)
    if (iret /= 0) then
       print*, "nf_inq_dimlen:  land_cat"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimlen:  land_cat problem")
    endif

    iret = nf_inq_dimid(ncid, "soil_cat", dimid)
    if (iret /= 0) then
 !      print*, "nf_inq_dimid:  soil_cat"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimid:  soil_cat problem")
    endif

    iret = nf_inq_dimlen(ncid, dimid, soil_cat)
    if (iret /= 0) then
 !      print*, "nf_inq_dimlen:  soil_cat"
       call hydro_stop("In read_hrldas_hdrinfo() - nf_inq_dimlen:  soil_cat problem")
    endif

    iret = nf_close(ncid)

  end subroutine read_hrldas_hdrinfo



  subroutine readland_hrldas(geo_static_flnm,ix,jx,land_cat,soil_cat,vegtyp,soltyp, &
                  terrain,latitude,longitude,SOLVEG_INITSWC)
    implicit none
    character(len=*),          intent(in)  :: geo_static_flnm
    integer,                   intent(in)  :: ix, jx, land_cat, soil_cat,SOLVEG_INITSWC
    integer, dimension(ix,jx), intent(out) :: vegtyp, soltyp
    real,    dimension(ix,jx), intent(out) :: terrain, latitude, longitude

    character(len=256) :: units
    integer :: ierr,i,j,jj
    integer :: ncid,varid
    real, dimension(ix,jx) :: xdum
    integer, dimension(ix,jx) :: vegtyp_inv, soiltyp_inv,xdum_int
    integer flag ! flag = 1 from wrfsi, flag =2 from WPS.
    CHARACTER(len=256)       :: var_name
    integer :: istmp, iswater, isoilwater, isother

    ! Open the NetCDF file.
    ierr = nf_open(geo_static_flnm, NF_NOWRITE, ncid)

    if (ierr /= 0) then
       write(*,'("Problem opening geo_static file: ''", A, "''")') trim(geo_static_flnm)
       call hydro_stop("In readland_hrldas() - Problem opening geo_static file") 
    endif

    flag = -99 
    ierr = nf_inq_varid(ncid,"XLAT", varid)
    flag = 1
    if(ierr .ne. 0) then
        ierr = nf_inq_varid(ncid,"XLAT_M", varid)
        if(ierr .ne. 0) then
!            write(6,*) "XLAT not found from wrfstatic file. "
            call hydro_stop("In readland_hrldas() - XLAT not found from wrfstatic file") 
        endif
        flag = 2
    endif

    ! Get Latitude (lat)
    if(flag .eq. 1) then
       call get_2d_netcdf("XLAT", ncid, latitude,  units, ix, jx, .TRUE., ierr)
    else
      call get_2d_netcdf("XLAT_M", ncid, latitude,  units, ix, jx, .TRUE., ierr)
    endif

    ! Get Longitude (lon)
    if(flag .eq. 1) then 
        call get_2d_netcdf("XLONG", ncid, longitude, units, ix, jx, .TRUE., ierr)
    else
        call get_2d_netcdf("XLONG_M", ncid, longitude, units, ix, jx, .TRUE., ierr)
    endif

    ! Get Terrain (avg)
    if(flag .eq. 1) then
       call get_2d_netcdf("HGT", ncid, terrain,   units, ix, jx, .TRUE., ierr)
    else
        call get_2d_netcdf("HGT_M", ncid, terrain,   units, ix, jx, .TRUE., ierr)
    endif


    if (SOLVEG_INITSWC.eq.0) then
!      ! Get Dominant Land Use categories (use)
!      call get_landuse_netcdf(ncid, xdum ,   units, ix, jx, land_cat)
!      vegtyp = nint(xdum)

     var_name = "LU_INDEX"
         call get2d_int(var_name,xdum_int,ix,jx,&
               trim(geo_static_flnm))
         vegtyp = xdum_int

      ! Get Dominant Soil Type categories in the top layer (stl)
      call get_soilcat_netcdf(ncid, xdum ,   units, ix, jx, soil_cat)
      soltyp = nint(xdum)

    else if (SOLVEG_INITSWC.eq.1) then
       var_name = "VEGTYP"
       call get2d_int(var_name,VEGTYP_inv,ix,jx,&
              trim(geo_static_flnm))

       var_name = "SOILTYP"
       call get2d_int(var_name,SOILTYP_inv,ix,jx,&
              trim(geo_static_flnm))
       do i=1,ix
         jj=jx
         do j=1,jx
           VEGTYP(i,j)=VEGTYP_inv(i,jj)
           SOLTYP(i,j)=SOILTYP_inv(i,jj)
           jj=jx-j
         end do
       end do

    endif

! Default values based on USGS
iswater = 16
isoilwater = 14
isother = 28 ! what is this??
  
ierr = NF_GET_ATT_INT(ncid, NF_GLOBAL, 'ISWATER', istmp)
if (ierr .eq. NF_NOERR) iswater = istmp
ierr = NF_GET_ATT_INT(ncid, NF_GLOBAL, 'ISOILWATER', istmp)
if (ierr .eq. NF_NOERR) isoilwater = istmp

    ! Close the NetCDF file
    ierr = nf_close(ncid)
    if (ierr /= 0) then
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  READLAND_HRLDAS:  NF_CLOSE"
       call hydro_stop("In readland_hrldas() - NF_CLOSE problem")
    endif

 write(6, *) "readland_hrldas: ISWATER ISOILWATER", iswater, isoilwater

    ! Make sure vegtyp and soltyp are consistent when it comes to water points,
    ! by setting soil category to water when vegetation category is water, and
    ! vice-versa.
    where (vegtyp == isother) vegtyp = iswater
    where (vegtyp == iswater) soltyp = isoilwater
    where (soltyp == isoilwater) vegtyp = iswater

!DJG test for deep gw function...
!    where (soltyp <> 14) soltyp = 1

  end subroutine readland_hrldas


      subroutine get_2d_netcdf_ruc(var_name,ncid,var, &
            ix,jx,tlevel,fatal_if_error,ierr)
          character(len=*), intent(in) :: var_name
          integer,intent(in) ::  ncid,ix,jx,tlevel
          real, intent(out):: var(ix,jx)
          logical, intent(in) :: fatal_if_error
          integer dims(4), dim_len(4)
          integer ierr,iret
          integer varid
           integer start(4),count(4)
           data count /1,1,1,1/
           data start /1,1,1,1/
          count(1) = ix
          count(2) = jx
          start(4) = tlevel
      ierr = nf_inq_varid(ncid,  var_name,  varid)

      if (ierr /= 0) then
        if (fatal_IF_ERROR) then
           print*, "MODULE_NOAHLSM_HRLDAS_INPUT: get_2d_netcdf_ruc:nf_inq_varid ", trim(var_name)
           call hydro_stop("In get_2d_netcdf_ruc() - nf_inq_varid problem")
        else
          return
        endif
      endif

      ierr = nf_get_vara_real(ncid, varid, start,count,var)
      

      return
      end subroutine get_2d_netcdf_ruc


      subroutine get_2d_netcdf_cows(var_name,ncid,var, &
            ix,jx,tlevel,fatal_if_error,ierr)
          character(len=*), intent(in) :: var_name
          integer,intent(in) ::  ncid,ix,jx,tlevel
          real, intent(out):: var(ix,jx)
          logical, intent(in) :: fatal_if_error
          integer ierr, iret
          integer varid
          integer start(4),count(4)
          data count /1,1,1,1/
          data start /1,1,1,1/
          count(1) = ix
          count(2) = jx
          start(4) = tlevel
      iret = nf_inq_varid(ncid,  var_name,  varid)

      if (iret /= 0) then
        if (fatal_IF_ERROR) then
           print*, "MODULE_NOAHLSM_HRLDAS_INPUT: get_2d_netcdf_cows:nf_inq_varid"
           call hydro_stop("In get_2d_netcdf_cows() - nf_inq_varid problem")
        else
          ierr = iret
          return
        endif
      endif
      iret = nf_get_vara_real(ncid, varid, start,count,var)

      return
      end subroutine get_2d_netcdf_cows





  subroutine readinit_hrldas(netcdf_flnm, ix, jx, nsoil, target_date, &
       smc, stc, sh2o, cmc, t1, weasd, snodep)
    implicit none
    character(len=*),                intent(in)  :: netcdf_flnm
    integer,                         intent(in)  :: ix
    integer,                         intent(in)  :: jx
    integer,                         intent(in)  :: nsoil
    character(len=*),                intent(in)  :: target_date
    real,    dimension(ix,jx,nsoil), intent(out) :: smc
    real,    dimension(ix,jx,nsoil), intent(out) :: stc
    real,    dimension(ix,jx,nsoil), intent(out) :: sh2o
    real,    dimension(ix,jx),       intent(out) :: cmc
    real,    dimension(ix,jx),       intent(out) :: t1
    real,    dimension(ix,jx),       intent(out) :: weasd
    real,    dimension(ix,jx),       intent(out) :: snodep

    character(len=256) :: units
    character(len=8) :: name
    integer :: ix_read, jx_read,i,j

    integer :: ierr, ncid, ierr_snodep
    integer :: idx

    logical :: found_canwat, found_skintemp, found_weasd, found_stemp, found_smois

    ! Open the NetCDF file.
    ierr = nf_open(netcdf_flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READINIT Problem opening netcdf file: ''", A, "''")') &
            trim(netcdf_flnm)
       call hydro_stop("In readinit_hrldas()- Problem opening netcdf file")
    endif

    call get_2d_netcdf("CANWAT",     ncid, cmc,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SKINTEMP",   ncid, t1,      units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("WEASD",      ncid, weasd,   units, ix, jx, .TRUE., ierr)

    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
!       print*, "Unrecognized units on WEASD"
       call hydro_stop("In readinit_hrldas() - Unrecognized units on WEASD")
    endif

    call get_2d_netcdf("SNODEP",     ncid, snodep,   units, ix, jx, .FALSE., ierr_snodep)
    call get_2d_netcdf("STEMP_1",    ncid, stc(:,:,1), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_2",    ncid, stc(:,:,2), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_3",    ncid, stc(:,:,3), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_4",    ncid, stc(:,:,4), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_1",    ncid, smc(:,:,1), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_2",    ncid, smc(:,:,2), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_3",    ncid, smc(:,:,3), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_4",    ncid, smc(:,:,4), units,  ix, jx, .TRUE., ierr)


    if (ierr_snodep /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif


!DJG check for erroneous neg WEASD or SNOWD due to offline interpolation...
       do i=1,ix
         do j=1,jx
           if (WEASD(i,j).lt.0.) WEASD(i,j)=0.0  !set lower bound to correct bi-lin interp err...
           if (snodep(i,j).lt.0.) snodep(i,j)=0.0  !set lower bound to correct bi-lin interp err...
         end do
       end do


    sh2o = smc

    ierr = nf_close(ncid)
  end subroutine readinit_hrldas




  subroutine READFORC_HRLDAS(flnm,ix,jx,target_date, t,q,u,v,p,lw,sw,pcp,lai,fpar)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: t
    real,             dimension(ix,jx), intent(out) :: q
    real,             dimension(ix,jx), intent(out) :: u
    real,             dimension(ix,jx), intent(out) :: v
    real,             dimension(ix,jx), intent(out) :: p
    real,             dimension(ix,jx), intent(out) :: lw
    real,             dimension(ix,jx), intent(out) :: sw
    real,             dimension(ix,jx), intent(out) :: pcp
    real,             dimension(ix,jx), intent(inout) :: lai
    real,             dimension(ix,jx), intent(inout) :: fpar

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.
    ierr = nf_open(trim(flnm), NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_HRLDAS() - Problem opening netcdf file")
    endif

    call get_2d_netcdf("T2D",     ncid, t,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("Q2D",     ncid, q,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("U2D",     ncid, u,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("V2D",     ncid, v,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("PSFC",    ncid, p,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("LWDOWN",  ncid, lw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SWDOWN",  ncid, sw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("RAINRATE",ncid, pcp,   units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("VEGFRA",  ncid, fpar,  units, ix, jx, .FALSE., ierr)
    if (ierr == 0) then
      if(maxval(fpar) .gt. 10 .and. maxval(fpar) .lt. 10000)  fpar = fpar * 1.E-2
    endif
    call get_2d_netcdf("LAI",     ncid, lai,   units, ix, jx, .FALSE., ierr)

    ierr = nf_close(ncid)

  end subroutine READFORC_HRLDAS



  subroutine READFORC_DMIP(flnm,ix,jx,var)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    real,       dimension(ix,jx), intent(out)       :: var
    character(len=13)                               :: head
    integer                          :: ncols, nrows, cellsize
    real                             :: xllc, yllc, no_data
    integer                          :: i,j
    character(len=256)                              ::junk

    open (77,file=trim(flnm),form="formatted",status="old")

!    read(77,732) head,ncols
!    read(77,732) head,nrows
!732        FORMAT(A13,I4)
!    read(77,733) head,xllc
!    read(77,733) head,yllc
!733        FORMAT(A13,F16.9)
!    read(77,732) head,cellsize
!    read(77,732) head,no_data

    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk

    do j=jx,1,-1
      read(77,*) (var(I,J),I=1,ix)
    end do
    close(77)

  end subroutine READFORC_DMIP



  subroutine READFORC_MDV(flnm,ix,jx,pcp,mmflag,ierr_flg)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    integer,                            intent(out)  :: ierr_flg
    integer :: it,jew,zsn
    real,             dimension(ix,jx), intent(out) :: pcp

    character(len=256) :: units
    integer :: ierr,i,j,i2,j2,varid
    integer :: ncid,mmflag
    real, dimension(ix,jx) :: temp

    mmflag = 0   ! flag for units spec. (0=mm, 1=mm/s)


!open NetCDF file...
        ierr_flg = nf_open(flnm, NF_NOWRITE, ncid)
        if (ierr_flg /= 0) then
#ifdef HYDRO_D
          write(*,'("READFORC_MDV Problem opening netcdf file: ''",A,"''")') &
                trim(flnm)
#endif
           return
        end if

        ierr = nf_inq_varid(ncid,  "precip",  varid)
        if(ierr /= 0) ierr_flg = ierr
        if (ierr /= 0) then
          ierr = nf_inq_varid(ncid,  "precip_rate",  varid)   !recheck variable name...
          if (ierr /= 0) then
#ifdef HYDRO_D
            write(*,'("READFORC_MDV Problem reading precip netcdf file: ''", A,"''")') &
                 trim(flnm)
#endif
          end if
          ierr_flg = ierr
          mmflag = 1
        end if
        ierr = nf_get_var_real(ncid, varid, pcp)
        ierr = nf_close(ncid)

        if (ierr /= 0) then
#ifdef HYDRO_D
          write(*,'("READFORC_MDV Problem reading netcdf file: ''", A,"''")') trim(flnm)
#endif
        end if

  end subroutine READFORC_MDV



  subroutine READFORC_NAMPCP(flnm,ix,jx,pcp,k,product)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    integer,                            intent(in)  :: k
    character(len=*),                   intent(in)  :: product
    integer :: it,jew,zsn
    parameter(it =  496,jew = 449, zsn = 499)   ! domain 1
!    parameter(it =  496,jew = 74, zsn = 109)   ! domain 2
    real,             dimension(it,jew,zsn) :: buf
    real,             dimension(ix,jx), intent(out) :: pcp

    character(len=256) :: units
    integer :: ierr,i,j,i2,j2,varid
    integer :: ncid
    real, dimension(ix,jx) :: temp

!      varname = trim(product)

!open NetCDF file...
      if (k.eq.1.) then
        ierr = nf_open(flnm, NF_NOWRITE, ncid)
        if (ierr /= 0) then
          write(*,'("READFORC_NAMPCP1 Problem opening netcdf file: ''",A, "''")') &
              trim(flnm)
          call hydro_stop("In READFORC_NAMPCP() - Problem opening netcdf file")
        end if

        ierr = nf_inq_varid(ncid,  trim(product),  varid)
        ierr = nf_get_var_real(ncid, varid, buf)
        ierr = nf_close(ncid)

        if (ierr /= 0) then
          write(*,'("READFORC_NAMPCP2 Problem reading netcdf file: ''", A,"''")') &
             trim(flnm)
          call hydro_stop("In READFORC_NAMPCP() - Problem reading netcdf file")
        end if
      endif
#ifdef HYDRO_D
      print *, "Data read in...",it,ix,jx,k
#endif

! Extract single time slice from dataset...

      do i=1,ix
        do j=1,jx
          pcp(i,j) = buf(k,i,j)
        end do
      end do

!      call get_2d_netcdf_ruc("trmm",ncid, pcp, jx, ix,k, .true., ierr)

  end subroutine READFORC_NAMPCP




  subroutine READFORC_COWS(flnm,ix,jx,target_date, t,q,u,p,lw,sw,pcp,tlevel)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: t
    real,             dimension(ix,jx), intent(out) :: q
    real,             dimension(ix,jx), intent(out) :: u
    real,             dimension(ix,jx) :: v
    real,             dimension(ix,jx), intent(out) :: p
    real,             dimension(ix,jx), intent(out) :: lw
    real,             dimension(ix,jx), intent(out) :: sw
    real,             dimension(ix,jx), intent(out) :: pcp
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_COWS Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_COWS() - Problem opening netcdf file")
    endif

    call get_2d_netcdf_cows("TA2",     ncid, t,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("QV2",     ncid, q,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("WSPD10",  ncid, u,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("PRES",    ncid, p,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("GLW",     ncid, lw,    ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("RSD",     ncid, sw,    ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("RAIN",    ncid, pcp,   ix, jx,tlevel, .TRUE., ierr)
!yw   call get_2d_netcdf_cows("V2D",     ncid, v,     ix, jx,tlevel, .TRUE., ierr)

    ierr = nf_close(ncid)

  end subroutine READFORC_COWS




  subroutine READFORC_RUC(flnm,ix,jx,target_date,t,q,u,v,p,lw,sw,pcp)
    
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx) :: t,q,u,v,p,lw,sw,pcp,pcpc
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    tlevel = 1

    ! Open the NetCDF file.
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_RUC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_RUC() - Problem opening netcdf file")
    endif

    call get_2d_netcdf_ruc("T2",     ncid, t,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("Q2",     ncid, q,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("U10",    ncid, u,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("V10",    ncid, v,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("PSFC",   ncid, p,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("GLW",    ncid, lw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("SWDOWN", ncid, sw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINC",  ncid, pcpc,  ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINNC", ncid, pcp,   ix, jx,tlevel, .true., ierr)

    ierr = nf_close(ncid)
    

!DJG  Add the convective and non-convective rain components (note: conv. comp=0
!for cloud resolving runs...) 
!DJG  Note that for RUC these are accumulated values to be adjusted to rates in
!driver...

    pcp=pcp+pcpc   ! assumes pcpc=0 for resolved convection...

  end subroutine READFORC_RUC




  subroutine READSNOW_FORC(flnm,ix,jx,weasd,snodep)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    real,             dimension(ix,jx), intent(out) :: weasd
    real,             dimension(ix,jx), intent(out) :: snodep
    real, dimension(ix,jx) :: tmp

    character(len=256) :: units
    integer :: ierr
    integer :: ncid,i,j

    ! Open the NetCDF file.

    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READSNOW Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READSNOW_FORC() - Problem opening netcdf file")
    endif

    call get_2d_netcdf("WEASD",  ncid, tmp,   units, ix, jx, .FALSE., ierr)
    if (ierr /= 0) then
         call get_2d_netcdf("SNOW",  ncid, tmp,   units, ix, jx, .FALSE., ierr)
         if (ierr == 0) then
            units = "mm"
            print *, "read WEASD from wrfoutput ...... "
            weasd = tmp * 1.E-3
         endif
    else
         weasd = tmp
         if (trim(units) == "m") then
            ! No conversion necessary
         else if (trim(units) == "mm") then
            ! convert WEASD from mm to m
            weasd = weasd * 1.E-3
         endif
    endif

    if (ierr /= 0) then
       print *, "!!!!! NO WEASD present in input file...initialize to 0."
    endif

    call get_2d_netcdf("SNODEP",     ncid, tmp,   units, ix, jx, .FALSE., ierr)
    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.
       call get_2d_netcdf("SNOWH",     ncid, tmp,   units, ix, jx, .FALSE., ierr)
       if(ierr .eq. 0) then
            print *, "read snow depth from wrfoutput ... " 
            snodep = tmp
       endif
    else
       snodep = tmp
    endif

    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.
!yw       snodep = weasd * 10.
       where(snodep .lt. weasd) snodep = weasd*10  !set lower bound to correct bi-lin interp err...
    endif

!DJG check for erroneous neg WEASD or SNOWD due to offline interpolation...
       where(snodep .lt. 0) snodep = 0
       where(weasd .lt. 0) weasd = 0
    ierr = nf_close(ncid)

  end subroutine READSNOW_FORC

    subroutine get2d_hrldas(inflnm,ix,jx,nsoil,smc,stc,sh2ox,cmc,t1,weasd,snodep)
          implicit none
          integer :: iret,varid,ncid,ix,jx,nsoil,ierr
          real,dimension(ix,jx):: weasd,snodep,cmc,t1
          real,dimension(ix,jx,nsoil):: smc,stc,sh2ox
          character(len=*), intent(in) :: inflnm
          character(len=256)::   units
          iret = nf_open(trim(inflnm), NF_NOWRITE, ncid)
          if(iret .ne. 0 )then
              write(6,*) "Error: failed to open file :",trim(inflnm)
             call hydro_stop("In get2d_hrldas() - failed to open file")
          endif

          call get2d_hrldas_real("CMC",     ncid, cmc,     ix, jx)
          call get2d_hrldas_real("TSKIN",   ncid, t1,      ix, jx)
          call get2d_hrldas_real("SWE",      ncid, weasd,   ix, jx)
          call get2d_hrldas_real("SNODEP",     ncid, snodep,   ix, jx)

    call get2d_hrldas_real("SOIL_T_1",    ncid, stc(:,:,1),  ix, jx)
    call get2d_hrldas_real("SOIL_T_2",    ncid, stc(:,:,2),  ix, jx)
    call get2d_hrldas_real("SOIL_T_3",    ncid, stc(:,:,3),  ix, jx)
    call get2d_hrldas_real("SOIL_T_4",    ncid, stc(:,:,4),  ix, jx)
    call get2d_hrldas_real("SOIL_T_5",    ncid, stc(:,:,5),  ix, jx)
    call get2d_hrldas_real("SOIL_T_6",    ncid, stc(:,:,6),  ix, jx)
    call get2d_hrldas_real("SOIL_T_7",    ncid, stc(:,:,7),  ix, jx)
    call get2d_hrldas_real("SOIL_T_8",    ncid, stc(:,:,8),  ix, jx)

    call get2d_hrldas_real("SOIL_M_1",    ncid, SMC(:,:,1),  ix, jx)
    call get2d_hrldas_real("SOIL_M_2",    ncid, SMC(:,:,2),  ix, jx)
    call get2d_hrldas_real("SOIL_M_3",    ncid, SMC(:,:,3),  ix, jx)
    call get2d_hrldas_real("SOIL_M_4",    ncid, SMC(:,:,4),  ix, jx)
    call get2d_hrldas_real("SOIL_M_5",    ncid, SMC(:,:,5),  ix, jx)
    call get2d_hrldas_real("SOIL_M_6",    ncid, SMC(:,:,6),  ix, jx)
    call get2d_hrldas_real("SOIL_M_7",    ncid, SMC(:,:,7),  ix, jx)
    call get2d_hrldas_real("SOIL_M_8",    ncid, SMC(:,:,8),  ix, jx)

    call get2d_hrldas_real("SOIL_W_1",    ncid, SH2OX(:,:,1),  ix, jx)
    call get2d_hrldas_real("SOIL_W_2",    ncid, SH2OX(:,:,2),  ix, jx)
    call get2d_hrldas_real("SOIL_W_3",    ncid, SH2OX(:,:,3),  ix, jx)
    call get2d_hrldas_real("SOIL_W_4",    ncid, SH2OX(:,:,4),  ix, jx)
    call get2d_hrldas_real("SOIL_W_5",    ncid, SH2OX(:,:,5),  ix, jx)
    call get2d_hrldas_real("SOIL_W_6",    ncid, SH2OX(:,:,6),  ix, jx)
    call get2d_hrldas_real("SOIL_W_7",    ncid, SH2OX(:,:,7),  ix, jx)
    call get2d_hrldas_real("SOIL_W_8",    ncid, SH2OX(:,:,8),  ix, jx)

          iret = nf_close(ncid)
         return
      end subroutine get2d_hrldas

      subroutine get2d_hrldas_real(var_name,ncid,out_buff,ix,jx)
          implicit none
          integer ::iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          iret = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
         return
      end subroutine get2d_hrldas_real

    subroutine read_stage4(flnm,IX,JX,pcp)
        integer IX,JX,ierr,ncid,i,j
        real pcp(IX,JX),buf(ix,jx)
        character(len=*),  intent(in)  :: flnm
        character(len=256) :: units

        ierr = nf_open(flnm, NF_NOWRITE, ncid)

        if(ierr .ne. 0) then
            call hydro_stop("In read_stage4() - failed to open stage4 file.")
        endif

        call get_2d_netcdf("RAINRATE",ncid, buf,   units, ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)
        do j = 1, jx
        do i = 1, ix
            if(buf(i,j) .lt. 0) then
                 buf(i,j) = pcp(i,j)
            end if
        end do
        end do
        pcp = buf
        return
    END subroutine read_stage4




 subroutine read_hydro_forcing_seq( &
       indir,olddate,hgrid, &
       ix,jx,forc_typ,snow_assim,  & 
       T2,q2x,u,v,pres,xlong,short,prcp1,&
       lai,fpar,snodep,dt,k,prcp_old)
! This subrouting is going to read different forcing.
   implicit none
   ! in variable
   character(len=*) :: olddate,hgrid,indir
   character(len=256) :: filename
   integer :: ix,jx,forc_typ,k,snow_assim  ! k is time loop
   real,dimension(ix,jx):: T2,q2x,u,v,pres,xlong,short,prcp1,&
          prcpnew,weasd,snodep,prcp0,prcp2,prcp_old
   real ::  dt, wrf_dt
   ! tmp variable
   character(len=256) :: inflnm, inflnm2, product
   integer  :: i,j,mmflag,ierr_flg
   real,dimension(ix,jx):: lai,fpar
   character(len=4) nwxst_t
   logical :: fexist

        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid

!!!DJG... Call READFORC_(variable) Subroutine for forcing data...
!!!DJG HRLDAS Format Forcing with hour format filename (NOTE: precip must be in mm/s!!!)
   if(FORC_TYP.eq.1) then
!!Create forcing data filename...
        call geth_newdate(out_date,olddate,nint(dt))
        inflnm = trim(indir)//"/"//&
             out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
             ".LDASIN_DOMAIN"//hgrid

        inquire (file=trim(inflnm), exist=fexist)
        if ( .not. fexist ) then
           print*, "no forcing data found", inflnm
           call hydro_stop("In read_hydro_forcing_seq")
        endif

      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   end if




!!!DJG HRLDAS Forcing with minute format filename (NOTE: precip must be in mm/s!!!)
   if(FORC_TYP.eq.2) then
!!Create forcing data filename...
        call geth_newdate(out_date,olddate,nint(dt))
        inflnm = trim(indir)//"/"//&
             out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
             out_date(15:16)//".LDASIN_DOMAIN"//hgrid
        inquire (file=trim(inflnm), exist=fexist)
        if ( .not. fexist ) then
           print*, "no forcing data found", inflnm
           call hydro_stop("In read_hydro_forcing_seq() - no forcing data found")
        endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   end if





!!!DJG WRF Output File Direct Ingest Forcing...
     if(FORC_TYP.eq.3) then
!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
             "wrfout_d0"//hgrid//"_"//&
             olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
             "_"//olddate(12:13)//":00:00"

        inquire (file=trim(inflnm), exist=fexist)
        if ( .not. fexist ) then
           print*, "no forcing data found", inflnm
           call hydro_stop("In read_hydro_forcing_seq() - no forcing data found")
        endif

        do i_forcing = 1, int(24*3600/dt)
           wrf_dt = i_forcing*dt
           call geth_newdate(out_date,olddate,nint(wrf_dt))
           inflnm2 = trim(indir)//"/"//&
             "wrfout_d0"//hgrid//"_"//&
             out_date(1:4)//"-"//out_date(6:7)//"-"//out_date(9:10)//&
             "_"//out_date(12:13)//":00:00"
           inquire (file=trim(inflnm2), exist=fexist)
           if (fexist ) goto 991
        end do
991     continue

        if(.not. fexist) then
           write(6,*) "FATAL ERROR: could not find file ",trim(inflnm2)
           call hydro_stop("In read_hydro_forcing_seq() - could not find file ")
        endif
#ifdef HYDRO_D
           print*, "read WRF forcing data: ", trim(inflnm)
           print*, "read WRF forcing data: ", trim(inflnm2)
#endif
       CALL READFORC_WRF(inflnm2,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCPnew,lai,fpar)
       CALL READFORC_WRF(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,prcp0,lai,fpar)
        PRCP1=(PRCPnew-prcp0)/wrf_dt   !Adjustment to convert accum to rate...(mm/s)

     end if

!!!DJG CONSTant, idealized forcing...
     if(FORC_TYP.eq.4) then
! Impose a fixed diurnal cycle...
! assumes model timestep is 1 hr
! assumes K=1 is 12z (Ks or ~ sunrise)
! First Precip...
       IF (K.EQ.2) THEN
       PRCP1 =25.4/3600.0      !units mm/s  (Simulates 1"/hr for first time step...)
!       PRCP1 =0./3600.0      !units mm/s  (Simulates <1"/hr for first 10 hours...)
       ELSEIF (K.GT.1) THEN
!        PRCP1 =0./3600.0      !units mm/s
!       ELSE
         PRCP1 = 0.
       END IF
!       PRCP1 = 0.
!       PRCP1 =10./3600.0      !units mm/s
! Other Met. Vars...
       T2=290.0 + 3.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       Q2X = 0.01
       U = 1.0
       V = 1.0
       PRES = 100000.0
       XLONG=400.0 + 25.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       SHORT=450.0 + 450.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))

!      print *, "PCP", PRCP1

    end if

!!!DJG  Idealized Met. w/ Specified Precip. Forcing Data...(Note: input precip units here are in 'mm/hr')
!   This option uses hard-wired met forcing EXCEPT precipitation which is read in
!   from a single, separate input file called 'YYYYMMDDHHMM.PRECIP_FORCING.nc'
!
    if(FORC_TYP.eq.5) then
! Standard Met. Vars...
       T2=290.0 + 3.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       Q2X = 0.01
       U = 1.0
       V = 1.0
       PRES = 100000.0
       XLONG=400.0 + 25.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       SHORT=450.0 + 450.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))

!Get specified precip....
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
!       product = "trmm"
!       inflnm = trim(indir)//"/"//"sat_domain1.nc"
!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
                olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
                olddate(15:16)//".PRECIP_FORCING.nc"
        inquire (file=trim(inflnm), exist=fexist)
        if ( .not. fexist ) then
           print*, "no specified precipitation data found", inflnm
           call hydro_stop("In read_hydro_forcing_seq() - no specified precipitation data found")
        endif

       PRCP1 = 0.
       PRCP_old = PRCP1

#ifdef HYDRO_D
      print *, "Opening supplemental precipitation forcing file...",inflnm
#endif
       CALL READFORC_MDV(inflnm,IX,JX,   &
          PRCP2,mmflag,ierr_flg)

!If radar or spec. data is ok use if not, skip to original NARR data...
      IF (ierr_flg.eq.0) then   ! use spec. precip
!Convert units if necessary
        IF (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
           PRCP1=PRCP2/DT     !convert from mm to mm/s
#ifdef HYDRO_D
           print*, "Supplemental pcp is accumulated pcp/dt. "  
#endif
        else
           PRCP1=PRCP2   !assumes PRCP2 is in mm/s 
#ifdef HYDRO_D
           print*, "Supplemental pcp is rate. "  
#endif
        END IF  ! Endif mmflag
      ELSE   ! either stop or default to original forcing data...
#ifdef HYDRO_D
        print *,"Current RADAR precip data not found !!! Using previous available file..."
#endif
        PRCP1 = PRCP_old
      END IF  ! Endif ierr_flg

! Loop through data to screen for plausible values
       do i=1,ix
         do j=1,jx
           if (PRCP1(i,j).lt.0.) PRCP1(i,j)= PRCP_old(i,j)
           if (PRCP1(i,j).gt.0.138889) PRCP1(i,j)=0.138889  !set max pcp intens = 500 mm/h
         end do
       end do

    end if





!!!DJG HRLDAS Forcing with hourly format filename with specified precipitation forcing...
!   This option uses HRLDAS-formatted met forcing EXCEPT precipitation which is read in
!   from a single, separate input file called 'YYYYMMDDHHMM.PRECIP_FORCING.nc'

   if(FORC_TYP.eq.6) then

!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid

        inquire (file=trim(inflnm), exist=fexist)

        if ( .not. fexist ) then
          do i_forcing = 1, nint(12*3600/dt)
           call geth_newdate(out_date,olddate,nint(i_forcing*dt))
           inflnm = trim(indir)//"/"//&
              olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
              olddate(15:16)//".LDASIN_DOMAIN"//hgrid
           inquire (file=trim(inflnm), exist=fexist)
            if(fexist) goto 201
          end do
201       continue
        endif


        if ( .not. fexist ) then
#ifdef HYDRO_D
           print*, "no ATM forcing data found at this time", inflnm
#endif
        else
#ifdef HYDRO_D
           print*, "reading forcing data at this time", inflnm
#endif
           
           CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
                PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
           PRCP_old = PRCP1  ! This assigns new precip to last precip as a fallback for missing data...
        endif


!Get specified precip....
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
                 olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
                 olddate(15:16)//".PRECIP_FORCING.nc"
        inquire (file=trim(inflnm), exist=fexist)
#ifdef HYDRO_D
        if(fexist) then 
            print*, "using specified pcp forcing: ",trim(inflnm)
        else
            print*, "no specified pcp forcing: ",trim(inflnm)
        endif
#endif
        if ( .not. fexist ) then
           prcp1 = PRCP_old ! for missing pcp data use analysis/model input 
        else
           CALL READFORC_MDV(inflnm,IX,JX,   &
              PRCP2,mmflag,ierr_flg)
!If radar or spec. data is ok use if not, skip to original NARR data...
           if(ierr_flg .ne. 0) then
#ifdef HYDRO_D
               print*, "WARNING: pcp reading problem: ", trim(inflnm)
#endif
               PRCP1=PRCP_old
           else
               PRCP1=PRCP2   !assumes PRCP2 is in mm/s
               IF (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
                PRCP1=PRCP2/DT     !convert from mm to mm/s
               END IF  ! Endif mmflag
#ifdef HYDRO_D
               print*, "replace pcp successfully! ",trim(inflnm)
#endif
           endif
        endif


! Loop through data to screen for plausible values
       where(PRCP1 .lt. 0) PRCP1=PRCP_old
       where(PRCP1 .gt. 10 ) PRCP1= PRCP_old 
       do i=1,ix
         do j=1,jx
           if (PRCP1(i,j).lt.0.) PRCP1(i,j)=0.0
           if (PRCP1(i,j).gt.0.138889) PRCP1(i,j)=0.138889  !set max pcp intens = 500 mm/h
         end do
       end do

   end if


!!!! FORC_TYP 7: uses WRF forcing data plus additional pcp forcing.

   if(FORC_TYP.eq.7) then

!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
             "wrfout_d0"//hgrid//"_"//&
             olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
             "_"//olddate(12:13)//":00:00"

        inquire (file=trim(inflnm), exist=fexist)


        if ( .not. fexist ) then
#ifdef HYDRO_D
           print*, "no forcing data found", inflnm
#endif
        else
           do i_forcing = 1, int(24*3600/dt)
              wrf_dt = i_forcing*dt
              call geth_newdate(out_date,olddate,nint(wrf_dt))
              inflnm2 = trim(indir)//"/"//&
                "wrfout_d0"//hgrid//"_"//&
                out_date(1:4)//"-"//out_date(6:7)//"-"//out_date(9:10)//&
                "_"//out_date(12:13)//":00:00"
              inquire (file=trim(inflnm2), exist=fexist)
              if (fexist ) goto 992
           end do
992        continue

#ifdef HYDRO_D
           print*, "read WRF forcing data: ", trim(inflnm)
           print*, "read WRF forcing data: ", trim(inflnm2)
#endif
           CALL READFORC_WRF(inflnm2,IX,JX,OLDDATE,T2,Q2X,U,V,   &
                   PRES,XLONG,SHORT,PRCPnew,lai,fpar)
           CALL READFORC_WRF(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
                   PRES,XLONG,SHORT,prcp0,lai,fpar)
           PRCP1=(PRCPnew-prcp0)/wrf_dt   !Adjustment to convert accum to rate...(mm/s)
           PRCP_old = PRCP1
        endif

!Get specified precip....
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
                 olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
                 olddate(15:16)//".PRECIP_FORCING.nc"
        inquire (file=trim(inflnm), exist=fexist)
#ifdef HYDRO_D
        if(fexist) then
            print*, "using specified pcp forcing: ",trim(inflnm)
        else
            print*, "no specified pcp forcing: ",trim(inflnm)
        endif
#endif
        if ( .not. fexist ) then
           prcp1 = PRCP_old ! for missing pcp data use analysis/model input 
        else
           CALL READFORC_MDV(inflnm,IX,JX,   &
              PRCP2,mmflag,ierr_flg)
!If radar or spec. data is ok use if not, skip to original NARR data...
           if(ierr_flg .ne. 0) then
#ifdef HYDRO_D
               print*, "WARNING: pcp reading problem: ", trim(inflnm)
#endif
               PRCP1=PRCP_old
           else
               PRCP1=PRCP2   !assumes PRCP2 is in mm/s
               IF (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
#ifdef HYDRO_D
                 write(6,*) "using supplemental pcp time interval ", DT
#endif
                PRCP1=PRCP2/DT     !convert from mm to mm/s
               else
#ifdef HYDRO_D
                 write(6,*) "using supplemental pcp rates "
#endif
               END IF  ! Endif mmflag
#ifdef HYDRO_D
               print*, "replace pcp successfully! ",trim(inflnm)
#endif
           endif
        endif


! Loop through data to screen for plausible values
       where(PRCP1 .lt. 0) PRCP1=PRCP_old
       where(PRCP1 .gt. 10 ) PRCP1= PRCP_old ! set maximum to be 500 mm/h
       where(PRCP1 .gt. 0.138889) PRCP1= 0.138889 ! set maximum to be 500 mm/h
   end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!The other forcing data types below here are obsolete and left for reference...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!DJG HRLDAS Single Input with Multiple Input Times File Forcing...
!     if(FORC_TYP.eq.6) then
!!Create forcing data filename...
!     if (len_trim(range) == 0) then
!      inflnm = trim(indir)//"/"//&
!             startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!             olddate(15:16)//".LDASIN_DOMAIN"//hgrid//"_multiple"
!!        "MET_LIS_CRO_2D_SANTEE_LU_1KM."//&
!!        ".156hrfcst.radar"
!     else
!     endif
!     CALL READFORC_HRLDAS_mult(inflnm,IX,JX,OLDDATE,T2,Q2X,U,   &
!          PRES,XLONG,SHORT,PRCP1,K)
!
!!       IF (K.GT.0.AND.K.LT.10) THEN
!!         PRCP1 = 10.0/3600.0            ! units mm/s
!!          PRCP1 = 0.254/3600.0
!!       ELSE
!!         PRCP1 = 0.
!!       END IF
!      endif



!!!!!DJG  NARR Met. w/ NARR Precip. Forcing Data...
!! Assumes standard 3-hrly NARR data has been resampled to NDHMS grid...
!! Assumes one 3hrly time-step per forcing data file 
!! Input precip units here are in 'mm' accumulated over 3 hrs...
!    if(FORC_TYP.eq.7) then  !NARR Met. w/ NARR Precip.
!!!Create forcing data filename...
!      if (len_trim(range) == 0) then
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid
!      else
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
!      endif
!      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
!          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!      PRCP1=PRCP1/(3.0*3600.0)  ! convert from 3hr accum to mm/s which is what NDHMS expects    
!    end if  !NARR Met. w/ NARR Precip.






!!!!DJG  NARR Met. w/ Specified Precip. Forcing Data...
!    if(FORC_TYP.eq.8) then !NARR Met. w/ Specified Precip.
!
!!Check to make sure if Noah time step is 3 hrs as is NARR...
!
!        PRCP_old = PRCP1
!
!     if(K.eq.1.OR.(MOD((K-1)*INT(DT),10800)).eq.0) then   !if/then 3 hr check
!!!Create forcing data filename...
!      if (len_trim(range) == 0) then
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid
!!        startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!!        ".48hrfcst.ncf"
!      else
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
!      endif
!      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
!          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!!       PRCP1=PRCP1/(3.0*3600.0)     !NARR 3hrly precip product in mm
!       PRCP1=PRCP1     !NAM model data in mm/s
!    end if    !3 hr check
!
!
!!Get spec. precip....
!! NAM Remote sensing...
!!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
!!       product = "trmm"
!!       inflnm = trim(indir)//"/"//"sat_domain1.nc"
!!!       inflnm = trim(indir)//"/"//"sat_domain2.nc"
!!       PRCP1 = 0.
!!       CALL READFORC_NAMPCP(inflnm,IX,JX,   &
!!          PRCP2,K,product)
!!       ierr_flg = 0
!!       mmflag = 0
!!!Convert pcp grid to units of mm/s...
!!       PRCP1=PRCP1/(3.0*3600.0)     !3hrly precip product
!
!!Read from filelist (NAME HE...,others)...
!!        if (K.eq.1) then
!!          open(unit=93,file="filelist.txt",form="formatted",status="old")
!!        end if
!!        read (93,*) filename
!!        inflnm = trim(indir)//"/"//trim(filename)
!!
!!
!!Front Range MDV Radar...
!
!!         inflnm = "/ptmp/weiyu/rt_2008/radar_obs/"//&
!!             inflnm = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/20080809/"//&
!!              olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!!              olddate(15:16)//"_radar.nc"
!!              olddate(15:16)//"_chill.nc"
!
!!        inflnm = "/d2/hydrolab/HRLDAS/forcing/FRNG/Big_Thomp_04/"//&
!!       inflnm = "/d2/hydrolab/HRLDAS/forcing/FRNG/RT_2008/radar_obs/"//&
!!             inflnm = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/20080809/"//&
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!!             olddate(15:16)//"00_Pcp60min.nc"
!!             olddate(15:16)//"00_Pcp30min.nc"
!!             olddate(15:16)//"00_30min.nc"
!             olddate(15:16)//"00_Pcp5min.nc"
!!              olddate(15:16)//"_chill.nc"
!
!!         inflnm = "/d2/hydrolab/HRLDAS/forcing/COWS/"//&
!!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!!             olddate(15:16)//"00_Pcp5min.nc"
!!              olddate(15:16)//"00_5.nc"
!
!!         inflnm = ""     ! use this for NAM frxst runs with 30 min time-step
!!
!
!
!!        if (K.le.6) then   ! use for 30min nowcast...
!!          if (K.eq.1) then
!!             open(unit=94,file="start_file.txt",form="formatted",status="replace")
!!!             inflnm2 = "/d2/hydrolab/HRLDAS/forcing/FRNG/RT_2008/radar_obs/"//&
!!             inflnm2 = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/"//&
!!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!!             olddate(15:16)//"00_"
!!             close(94)
!!             nwxst_t = "5"! calc minutes from timestep and convert to char...
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!          if (K.eq.2) then
!!             nwxst_t = "10" ! calc minutes from timestep and convert to char...
!!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!!             read (94,*) inflnm2
!!             close(94)
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!          if (K.eq.3) then
!!             nwxst_t = "15" ! calc minutes from timestep and convert to char...
!!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!!             read (94,*) inflnm
!!             close(94)
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!          if (K.eq.4) then
!!             nwxst_t = "20" ! calc minutes from timestep and convert to char...
!!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!!             read (94,*) inflnm
!!             close(94)
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!          if (K.eq.5) then
!!             nwxst_t = "25" ! calc minutes from timestep and convert to char...
!!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!!             read (94,*) inflnm
!!             close(94)
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!          if (K.eq.6) then
!!             nwxst_t = "30" ! calc minutes from timestep and convert to char...
!!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!!             read (94,*) inflnm
!!             close(94)
!!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!!          end if
!!        else
!!          inflnm = ""     ! use this for NAM frxst runs with 30 min time-step
!!        end if
!
!!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!!             olddate(15:16)//"00_Pcp30minMerge.nc"
!        
!       CALL READFORC_MDV(inflnm,IX,JX,   &
!          PRCP2,mmflag,ierr_flg)
!
!!If radar or spec. data is ok use if not, skip to original NARR data...
!      IF (ierr_flg.eq.0) then   ! use spec. precip
!         PRCP1=PRCP2   !assumes PRCP2 is in mm/s
!!Convert units if necessary
!        IF (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
!          PRCP1=PRCP2/DT     !convert from mm to mm/s 
!        END IF  ! Endif mmflag
!      ELSE   ! either stop or default to original forcing data...
!        PRCP1 = PRCP_old
!      END IF  ! Endif ierr_flg
!
!! Loop through data to screen for plausible values
!       do i=1,ix
!         do j=1,jx
!           if (PRCP1(i,j).lt.0.) PRCP1(i,j)=0.0
!           if (PRCP1(i,j).gt.0.0555) PRCP1(i,j)=0.0555  !set max pcp intens = 200 mm/h
!!          PRCP1(i,j) = 0.
!!          PRCP1(i,j) = 0.02   !override w/ const. precip for gw testing only...
!         end do
!       end do
!
!!        if (K.eq.1) then  ! quick dump for site specific precip...
!          open(unit=94,file="Christman_accumpcp.txt",form="formatted",status="new")
!        end if
!
!        
!    end if  !NARR Met. w/ Specified Precip.





!!!!DJG  NLDAS Met. w/ NLDAS Precip. Forcing Data...
!! Assumes standard hrly NLDAS data has been resampled to NDHMS grid...
!! Assumes one 1-hrly time-step per forcing data file
!! Input precip units here are in 'mm' accumulated over 1 hr...
!    if(FORC_TYP.eq.9) then  !NLDAS Met. w/ NLDAS Precip.
!!!Create forcing data filename...
!      if (len_trim(range) == 0) then
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!!Use this for minute forcing...             olddate(15:16)//".LDASIN_DOMAIN"//hgrid
!             ".LDASIN_DOMAIN"//hgrid
!      else
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
!      endif
!      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
!          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!      PRCP1=PRCP1/(1.0*3600.0)  ! convert hourly NLDAS hourly accum pcp to mm/s which is what NDHMS expects
!    end if  !NLDAS Met. w/ NLDAS Precip.





!!!!DJG  NARR Met. w/ DMIP Precip. & Temp. Forcing Data...
!    if(FORC_TYP.eq.10) then  ! If/Then for DMIP forcing data...
!!Check to make sure if Noah time step is 3 hrs as is NARR...
!
!     if(K.eq.1.OR.(MOD((K-1)*INT(DT),10800)).eq.0) then   !if/then 3 hr check
!!!Create forcing data filename...
!      if (len_trim(range) == 0) then
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid
!!        startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!!        ".48hrfcst.ncf"
!      else
!        inflnm = trim(indir)//"/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
!      endif
!      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
!          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!          PRCP1=PRCP1/(3.0*3600.0)  ! convert to mm/s which is what HRLDAS expects    
!    end if    !3 hr check
!
!!Get DMIP Precip...
!!       inflnm = "/d3/gochis/HRLDAS/forcing/DMIP_II/PRECIP_HRAP/precip_finished"//"/"//&
!       inflnm = "/d2/hydrolab/HRLDAS/forcing/DMIP_II_AmerR/PRECIP_HRAP"//"/"//&
!           "proj.xmrg"//&
!           olddate(6:7)//olddate(9:10)//olddate(1:4)//olddate(12:13)//&
!           "z.asc"
!        PRCP1 = 0.
!        CALL READFORC_DMIP(inflnm,IX,JX,PRCP1)
!          PRCP1 = PRCP1 / 100.0    ! Convert from native hundreths of mm to mm
!!       IF (K.LT.34) THEN
!!        PRCP1 = 5.0/3600.0            ! units mm/s
!!!       ELSE
!!!         PRCP1 = 0.
!!       END IF
!
!!Get DMIP Temp...
!!       inflnm = "/d3/gochis/HRLDAS/forcing/DMIP_II/TEMP_HRAP/tair_finished"//"/"//&
!       inflnm = "/d2/hydrolab/HRLDAS/forcing/DMIP_II_AmerR/TEMP_HRAP"//"/"//&
!           "proj.tair"//&
!           olddate(6:7)//olddate(9:10)//olddate(1:4)//olddate(12:13)//&
!           "z.asc"
!        CALL READFORC_DMIP(inflnm,IX,JX,T2)
!          T2 = (5./9.)*(T2-32.0) + 273.15         !Convert from deg F to deg K
!
!    end if  !End if for DMIP forcing data...
!
!
!
!! : add reading forcing precipitation data
!!       ywinflnm = "/ptmp/weiyu/hrldas/v2/st4"//"/"//&
!!            olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!!            ".LDASIN_DOMAIN2"
!!       call read_stage4(ywinflnm,IX,JX,PRCP1)
!!end yw
!
!
!!!!DJG Check for snow data assimilation...

   if (SNOW_ASSIM .eq. 1) then

! Every 24 hours, update the snow field from analyses.
     if(forc_typ .ne. 3 .or. forc_typ .ne. 6) then
         if ( OLDDATE(12:13) == "00") then
            CALL READSNOW_FORC(inflnm,IX,JX,WEASD,SNODEP)
         endif
     else
        CALL READSNOW_FORC(inflnm,IX,JX,WEASD,SNODEP)
     endif

   end if

#ifdef PRECIP_DOUBLE
#ifdef HYDRO_D
   print*,'PRECIP DOUBLE'
#endif
   PRCP1 = PRCP1 * 2.0
#endif 

 end subroutine read_hydro_forcing_seq


#ifdef MPP_LAND
    subroutine mpp_readland_hrldas(geo_static_flnm,&
          ix,jx,land_cat,soil_cat,& 
          vegtyp,soltyp,terrain,latitude,longitude,&
          global_nx,global_ny,SOLVEG_INITSWC)
    implicit none
    character(len=*),          intent(in)  :: geo_static_flnm
    integer,                   intent(in)  :: ix, jx, land_cat, soil_cat, &
              global_nx,global_ny,SOLVEG_INITSWC
    integer, dimension(ix,jx), intent(out) :: vegtyp, soltyp
    real,    dimension(ix,jx), intent(out) :: terrain, latitude, longitude
    real, dimension(global_nx,global_ny) ::g_terrain, g_latitude, g_longitude
    integer, dimension(global_nx,global_ny) :: g_vegtyp, g_soltyp

    character(len=256) :: units
    integer :: ierr
    integer :: ncid,varid
    real, dimension(ix,jx) :: xdum
    integer flag ! flag = 1 from wrfsi, flag =2 from WPS.
     if(my_id.eq.IO_id) then
        CALL READLAND_HRLDAS(geo_static_flnm,global_nx,  &
               global_ny,LAND_CAT,SOIL_CAT,      &
               g_VEGTYP,g_SOLTYP,g_TERRAIN,g_LATITUDE,g_LONGITUDE, SOLVEG_INITSWC)
     end if
  ! distribute the data to computation node.
     call mpp_land_bcast_int1(LAND_CAT)
     call mpp_land_bcast_int1(SOIL_CAT)
     call decompose_data_int(g_VEGTYP,VEGTYP)
     call decompose_data_int(g_SOLTYP,SOLTYP)
     call decompose_data_real(g_TERRAIN,TERRAIN)
     call decompose_data_real(g_LATITUDE,LATITUDE)
     call decompose_data_real(g_LONGITUDE,LONGITUDE)
      return 
      end subroutine mpp_readland_hrldas


      subroutine MPP_READSNOW_FORC(flnm,ix,jx,OLDDATE,weasd,snodep,&
                 global_nX, global_ny)
        implicit none

        character(len=*),                   intent(in)  :: flnm,OLDDATE
        integer,  intent(in)  :: ix, global_nx,global_ny
        integer,                            intent(in)  :: jx
        real,             dimension(ix,jx), intent(out) :: weasd
        real,             dimension(ix,jx), intent(out) :: snodep

        real,dimension(global_nX, global_ny):: g_weasd, g_snodep
    
        character(len=256) :: units
        integer :: ierr
        integer :: ncid,i,j

        if(my_id .eq. IO_id) then
          CALL READSNOW_FORC(trim(flnm),global_nX, global_ny,g_WEASD,g_SNODEP)
       endif
       call decompose_data_real(g_WEASD,WEASD)
       call decompose_data_real(g_SNODEP,SNODEP)

        return 
        end  subroutine MPP_READSNOW_FORC

      subroutine MPP_DEEPGW_HRLDAS(ix,jx,in_SMCMAX,&
                 global_nX, global_ny,nsoil,out_SMC,out_SH2OX)
        implicit none

        integer,  intent(in)  :: ix,global_nx,global_ny
        integer,  intent(in)  :: jx,nsoil
        real,             dimension(ix,jx), intent(in) :: in_smcmax
        real,             dimension(ix,jx,nsoil), intent(out) :: out_smc,out_sh2ox

        real,dimension(global_nX, global_ny,nsoil):: g_smc, g_sh2ox
        real,dimension(global_nX, global_ny):: g_smcmax
        integer   :: i,j,k
       

          call write_IO_real(in_smcmax,g_smcmax)  ! get global grid of smcmax

#ifdef HYDRO_D
          write (*,*) "In deep GW...", nsoil
#endif

!loop to overwrite soils to saturation...
        do i=1,global_nx
         do j=1,global_ny
            g_smc(i,j,1:NSOIL) = g_smcmax(i,j)
            g_sh2ox(i,j,1:NSOIL) = g_smcmax(i,j)
         end do 
        end do 

!decompose global grid to parallel tiles...
       do k=1,nsoil
        call decompose_data_real(g_smc(:,:,k),out_smc(:,:,k))
        call decompose_data_real(g_sh2ox(:,:,k),out_sh2ox(:,:,k))
       end do

        return 
        end  subroutine MPP_DEEPGW_HRLDAS


 subroutine read_hydro_forcing_mpp( &
       indir,olddate,hgrid, &
       ix,jx,forc_typ,snow_assim,  & 
       T2,q2x,u,v,pres,xlong,short,prcp1,&
       lai,fpar,snodep,dt,k,prcp_old)
! This subrouting is going to read different forcing.


   implicit none
   ! in variable
   character(len=*) :: olddate,hgrid,indir
   character(len=256) :: filename
   integer :: ix,jx,forc_typ,k,snow_assim  ! k is time loop
   real,dimension(ix,jx):: T2,q2x,u,v,pres,xlong,short,prcp1,&
          prcpnew,lai,fpar,snodep,prcp_old
   real ::  dt
   ! tmp variable
   character(len=256) :: inflnm, product
   integer  :: i,j,mmflag
   real,dimension(global_nx,global_ny):: g_T2,g_Q2X,g_U,g_V,g_XLONG, &
             g_SHORT,g_PRCP1,g_PRES,g_lai,g_snodep,g_prcp_old, g_fpar
   integer flag 
   


     call write_io_real(T2,g_T2)
     call write_io_real(Q2X,g_Q2X)
     call write_io_real(U,g_U)
     call write_io_real(V,g_V)
     call write_io_real(XLONG,g_XLONG)
     call write_io_real(SHORT,g_SHORT)
     call write_io_real(PRCP1,g_PRCP1)
     call write_io_real(PRES,g_PRES)
     call write_io_real(prcp_old,g_PRCP_old)

     call write_io_real(lai,g_lai)
     call write_io_real(fpar,g_fpar)
     call write_io_real(snodep,g_snodep)



   if(my_id .eq. IO_id) then
      call read_hydro_forcing_seq( &
        indir,olddate,hgrid,&
        global_nx,global_ny,forc_typ,snow_assim,  &
        g_T2,g_q2x,g_u,g_v,g_pres,g_xlong,g_short,g_prcp1,&
        g_lai,g_fpar,g_snodep,dt,k,g_prcp_old)
#ifdef HYDRO_D
     write(6,*) "finish read forcing,olddate ",olddate
#endif
   end if

     call decompose_data_real(g_T2,T2)
     call decompose_data_real(g_Q2X,Q2X)
     call decompose_data_real(g_U,U)
     call decompose_data_real(g_V,V)
     call decompose_data_real(g_XLONG,XLONG)
     call decompose_data_real(g_SHORT,SHORT)
     call decompose_data_real(g_PRCP1,PRCP1)
     call decompose_data_real(g_prcp_old,prcp_old)
     call decompose_data_real(g_PRES,PRES)

     call decompose_data_real(g_lai,lai)
     call decompose_data_real(g_fpar,fpar)
     call decompose_data_real(g_snodep,snodep)

     return
   end subroutine read_hydro_forcing_mpp
#endif

  integer function nfeb_yw(year)
    !
    ! Compute the number of days in February for the given year.
    !
    implicit none
    integer, intent(in) :: year ! Four-digit year

    nfeb_yw = 28 ! By default, February has 28 days ...
    if (mod(year,4).eq.0) then
       nfeb_yw = 29  ! But every four years, it has 29 days ...
       if (mod(year,100).eq.0) then
          nfeb_yw = 28  ! Except every 100 years, when it has 28 days ...
          if (mod(year,400).eq.0) then
             nfeb_yw = 29  ! Except every 400 years, when it has 29 days ...
             if (mod(year,3600).eq.0) then
                nfeb_yw = 28  ! Except every 3600 years, when it has 28 days.
             endif
          endif
       endif
    endif
  end function nfeb_yw

  subroutine geth_newdate (ndate, odate, idt)
    implicit none

    !  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and 
    !  delta-time, compute the new date.

    !  on entry     -  odate  -  the old hdate.
    !                  idt    -  the change in time

    !  on exit      -  ndate  -  the new hdate.

    integer, intent(in)           :: idt
    character (len=*), intent(out) :: ndate
    character (len=*), intent(in)  :: odate

    !  Local Variables

    !  yrold    -  indicates the year associated with "odate"
    !  moold    -  indicates the month associated with "odate"
    !  dyold    -  indicates the day associated with "odate"
    !  hrold    -  indicates the hour associated with "odate"
    !  miold    -  indicates the minute associated with "odate"
    !  scold    -  indicates the second associated with "odate"

    !  yrnew    -  indicates the year associated with "ndate"
    !  monew    -  indicates the month associated with "ndate"
    !  dynew    -  indicates the day associated with "ndate"
    !  hrnew    -  indicates the hour associated with "ndate"
    !  minew    -  indicates the minute associated with "ndate"
    !  scnew    -  indicates the second associated with "ndate"

    !  mday     -  a list assigning the number of days in each month

    !  i        -  loop counter
    !  nday     -  the integer number of days represented by "idt"
    !  nhour    -  the integer number of hours in "idt" after taking out
    !              all the whole days
    !  nmin     -  the integer number of minutes in "idt" after taking out
    !              all the whole days and whole hours.
    !  nsec     -  the integer number of minutes in "idt" after taking out
    !              all the whole days, whole hours, and whole minutes.

    integer :: newlen, oldlen
    integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
    integer :: yrold, moold, dyold, hrold, miold, scold, frold
    integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
    logical :: opass
    character (len=10) :: hfrc
    character (len=1) :: sp
    logical :: punct
    integer :: yrstart, yrend, mostart, moend, dystart, dyend
    integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
    integer :: units
    integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
!yw    integer nfeb_yw   

    ! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
    if (odate(5:5) == "-") then
       punct = .TRUE.
    else
       punct = .FALSE.
    endif

    !  Break down old hdate into parts

    hrold = 0
    miold = 0
    scold = 0
    frold = 0
    oldlen = LEN(odate)
    if (punct) then
       yrstart = 1
       yrend = 4
       mostart = 6
       moend = 7
       dystart = 9
       dyend = 10
       hrstart = 12
       hrend = 13
       mistart = 15
       miend = 16
       scstart = 18
       scend = 19
       frstart = 21
       select case (oldlen)
       case (10)
          ! Days
          units = 1
       case (13)
          ! Hours
          units = 2
       case (16)
          ! Minutes
          units = 3
       case (19)
          ! Seconds
          units = 4
       case (21)
          ! Tenths
          units = 5
       case (22)
          ! Hundredths
          units = 6
       case (23)
          ! Thousandths
          units = 7
       case (24)
          ! Ten thousandths
          units = 8
       case default
          write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
          call hydro_stop("In geth_newdate() - error odd length") 
       end select

       if (oldlen.ge.11) then
          sp = odate(11:11)
       else
          sp = ' '
       end if

    else

       yrstart = 1
       yrend = 4
       mostart = 5
       moend = 6
       dystart = 7
       dyend = 8
       hrstart = 9
       hrend = 10
       mistart = 11
       miend = 12
       scstart = 13
       scend = 14
       frstart = 15

       select case (oldlen)
       case (8)
          ! Days
          units = 1
       case (10)
          ! Hours
          units = 2
       case (12)
          ! Minutes
          units = 3
       case (14)
          ! Seconds
          units = 4
       case (15)
          ! Tenths
          units = 5
       case (16)
          ! Hundredths
          units = 6
       case (17)
          ! Thousandths
          units = 7
       case (18)
          ! Ten thousandths
          units = 8
       case default
          write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
           call hydro_stop("In geth_newdate() - error odd length")
       end select
    endif

    !  Use internal READ statements to convert the CHARACTER string
    !  date into INTEGER components.

    read(odate(yrstart:yrend),  '(i4)') yrold
    read(odate(mostart:moend),  '(i2)') moold
    read(odate(dystart:dyend), '(i2)') dyold
    if (units.ge.2) then
       read(odate(hrstart:hrend),'(i2)') hrold
       if (units.ge.3) then
          read(odate(mistart:miend),'(i2)') miold
          if (units.ge.4) then
             read(odate(scstart:scend),'(i2)') scold
             if (units.ge.5) then
                read(odate(frstart:oldlen),*) frold
             end if
          end if
       end if
    end if

    !  Set the number of days in February for that year.

    mday(2) = nfeb_yw(yrold)

    !  Check that ODATE makes sense.

    opass = .TRUE.

    !  Check that the month of ODATE makes sense.

    if ((moold.gt.12).or.(moold.lt.1)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
#endif
       opass = .FALSE.
    end if

    !  Check that the day of ODATE makes sense.

    if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
#endif
       opass = .FALSE.
    end if

    !  Check that the hour of ODATE makes sense.

    if ((hrold.gt.23).or.(hrold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
#endif
       opass = .FALSE.
    end if

    !  Check that the minute of ODATE makes sense.

    if ((miold.gt.59).or.(miold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
#endif
       opass = .FALSE.
    end if

    !  Check that the second of ODATE makes sense.

    if ((scold.gt.59).or.(scold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
#endif
       opass = .FALSE.
    end if

    !  Check that the fractional part  of ODATE makes sense.
    if (.not.opass) then
       write(*,*) 'Crazy ODATE: ', odate(1:oldlen), oldlen
       call hydro_stop("In geth_newdate")
    end if

    !  Date Checks are completed.  Continue.


    !  Compute the number of days, hours, minutes, and seconds in idt

    if (units.ge.5) then !idt should be in fractions of seconds
       ifrc = oldlen-(frstart)+1
       ifrc = 10**ifrc
       nday   = abs(idt)/(86400*ifrc)
       nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
       nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
       nsec   = mod(abs(idt),60*ifrc)/(ifrc)
       nfrac = mod(abs(idt), ifrc)
    else if (units.eq.4) then  !idt should be in seconds
       ifrc = 1
       nday   = abs(idt)/86400 ! integer number of days in delta-time
       nhour  = mod(abs(idt),86400)/3600
       nmin   = mod(abs(idt),3600)/60
       nsec   = mod(abs(idt),60)
       nfrac  = 0
    else if (units.eq.3) then !idt should be in minutes
       ifrc = 1
       nday   = abs(idt)/1440 ! integer number of days in delta-time
       nhour  = mod(abs(idt),1440)/60
       nmin   = mod(abs(idt),60)
       nsec   = 0
       nfrac  = 0
    else if (units.eq.2) then !idt should be in hours
       ifrc = 1
       nday   = abs(idt)/24 ! integer number of days in delta-time
       nhour  = mod(abs(idt),24)
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else if (units.eq.1) then !idt should be in days
       ifrc = 1
       nday   = abs(idt)    ! integer number of days in delta-time
       nhour  = 0
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else
       write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
            oldlen
       write(*,*) '#'//odate(1:oldlen)//'#'
       call hydro_stop("In geth_newdate")
    end if

    if (idt.ge.0) then

       frnew = frold + nfrac
       if (frnew.ge.ifrc) then
          frnew = frnew - ifrc
          nsec = nsec + 1
       end if

       scnew = scold + nsec
       if (scnew .ge. 60) then
          scnew = scnew - 60
          nmin  = nmin + 1
       end if

       minew = miold + nmin
       if (minew .ge. 60) then
          minew = minew - 60
          nhour  = nhour + 1
       end if

       hrnew = hrold + nhour
       if (hrnew .ge. 24) then
          hrnew = hrnew - 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew + 1
          if (dynew.gt.mday(monew)) then
             dynew = dynew - mday(monew)
             monew = monew + 1
             if (monew .gt. 12) then
                monew = 1
                yrnew = yrnew + 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb_yw(yrnew)
             end if
          end if
       end do

    else if (idt.lt.0) then

       frnew = frold - nfrac
       if (frnew .lt. 0) then
          frnew = frnew + ifrc
          nsec = nsec + 1
       end if

       scnew = scold - nsec
       if (scnew .lt. 00) then
          scnew = scnew + 60
          nmin  = nmin + 1
       end if

       minew = miold - nmin
       if (minew .lt. 00) then
          minew = minew + 60
          nhour  = nhour + 1
       end if

       hrnew = hrold - nhour
       if (hrnew .lt. 00) then
          hrnew = hrnew + 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew - 1
          if (dynew.eq.0) then
             monew = monew - 1
             if (monew.eq.0) then
                monew = 12
                yrnew = yrnew - 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb_yw(yrnew)
             end if
             dynew = mday(monew)
          end if
       end do
    end if

    !  Now construct the new mdate

    newlen = LEN(ndate)

    if (punct) then

       if (newlen.gt.frstart) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
19        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

       else if (newlen.eq.miend) then
          write(ndate,16) yrnew, monew, dynew, hrnew, minew
16        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,13) yrnew, monew, dynew, hrnew
13        format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,10) yrnew, monew, dynew
10        format(i4,'-',i2.2,'-',i2.2)

       end if

    else

       if (newlen.gt.frstart) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
119       format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.miend) then
          write(ndate,116) yrnew, monew, dynew, hrnew, minew
116       format(i4,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,113) yrnew, monew, dynew, hrnew
113       format(i4,i2.2,i2.2,i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,110) yrnew, monew, dynew
110       format(i4,i2.2,i2.2)

       end if

    endif

    if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp

  end subroutine geth_newdate


subroutine read_hydro_forcing_mpp1( &
     indir,olddate,hgrid, &
     ix,jx,forc_typ,snow_assim,  & 
     T2,q2x,u,v,pres,xlong,short,prcp1,&
     lai,fpar,snodep,dt,k,prcp_old)
! This subrouting is going to read different forcing.
implicit none
! in variable
character(len=*) :: olddate,hgrid,indir
character(len=256) :: filename
integer :: ix,jx,forc_typ,k,snow_assim  ! k is time loop
real,dimension(ix,jx):: T2,q2x,u,v,pres,xlong,short,prcp1,&
     prcpnew,weasd,snodep,prcp0,prcp2,prcp_old
real ::  dt, wrf_dt
! tmp variable
character(len=256) :: inflnm, inflnm2, product
integer  :: i,j,mmflag,ierr_flg
real,dimension(ix,jx):: lai,fpar
character(len=4) nwxst_t
logical :: fexist

inflnm = trim(indir)//"/"//&
         olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
         ".LDASIN_DOMAIN"//hgrid

!!!DJG... Call READFORC_(variable) Subroutine for forcing data...
!!!DJG HRLDAS Format Forcing with hour format filename (NOTE: precip must be in mm/s!!!)


!!! FORC_TYPE 1 ============================================================================
if(FORC_TYP.eq.1) then
   !!Create forcing data filename...
   call geth_newdate(out_date,olddate,nint(dt))
   inflnm = trim(indir)//"/"//&
        out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
        ".LDASIN_DOMAIN"//hgrid
   
   inquire (file=trim(inflnm), exist=fexist)
   
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
   if ( .not. fexist ) then
      print*, "no forcing data found", inflnm
      call hydro_stop("In read_hydro_forcing_mpp1() - no forcing data found")
   endif

#ifdef HYDRO_D
   print*, "read forcing data at ", OLDDATE,  trim(inflnm)
#endif
   call READFORC_HRLDAS_mpp(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
        PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   
   where(PRCP1 .lt. 0) PRCP1= 0  ! set minimum to be 0
   where(PRCP1 .gt. 0.138889) PRCP1= 0.138889 ! set maximum to be 500 mm/h
   
end if


!!! FORC_TYPE 2 ============================================================================
!!!DJG HRLDAS Forcing with minute format filename (NOTE: precip must be in mm/s!!!)
if(FORC_TYP.eq.2) then

!!Create forcing data filename...
   call geth_newdate(out_date,olddate,nint(dt))
   inflnm = trim(indir)//"/"//&
        out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
        out_date(15:16)//".LDASIN_DOMAIN"//hgrid
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
   if ( .not. fexist ) then
      print*, "no forcing data found", inflnm
      call hydro_stop("In read_hydro_forcing_mpp1() - no forcing data found")
   endif
   call READFORC_HRLDAS_mpp(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
        PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   
   where(PRCP1 .lt. 0) PRCP1= 0  ! set minimum to be 0
   where(PRCP1 .gt. 0.138889) PRCP1= 0.138889 ! set maximum to be 500 mm/h

end if


!!! FORC_TYPE 3 ============================================================================
!!!DJG WRF Output File Direct Ingest Forcing...
if(FORC_TYP.eq.3) then
   !!Create forcing data filename...
   inflnm = trim(indir)//"/"//&
        "wrfout_d0"//hgrid//"_"//&
        olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
        "_"//olddate(12:13)//":00:00"
   
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
   if ( .not. fexist ) then
      print*, "no forcing data found", inflnm
      call hydro_stop("read_hydro_forcing_seq")
   endif
   
   do i_forcing = 1, int(24*3600/dt)
      wrf_dt = i_forcing*dt
      call geth_newdate(out_date,olddate,nint(wrf_dt))
      inflnm2 = trim(indir)//"/"//&
           "wrfout_d0"//hgrid//"_"//&
           out_date(1:4)//"-"//out_date(6:7)//"-"//out_date(9:10)//&
           "_"//out_date(12:13)//":00:00"
      inquire (file=trim(inflnm2), exist=fexist)
#ifdef MPP_LAND
      call mpp_land_bcast_logical(fexist)
#endif
      if (fexist ) goto 991
   end do
991 continue
   
   if(.not. fexist) then
      write(6,*) "Error: could not find file ",trim(inflnm2)
      call hydro_stop("In read_hydro_forcing_mpp1() - could not find WRF forcing file")
   endif
#ifdef HYDRO_D
   print*, "read WRF forcing data: ", trim(inflnm)
   print*, "read WRF forcing data: ", trim(inflnm2)
#endif
   
   
   call READFORC_WRF_mpp(inflnm2,IX,JX,OLDDATE,T2,Q2X,U,V,   &
        PRES,XLONG,SHORT,PRCPnew,lai,fpar)
   call READFORC_WRF_mpp(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
        PRES,XLONG,SHORT,prcp0,lai,fpar)
   PRCP1=(PRCPnew-prcp0)/wrf_dt   !Adjustment to convert accum to rate...(mm/s)
   
end if


!!! FORC_TYPE4 ============================================================================
!!!DJG CONSTant, idealized forcing...
if(FORC_TYP.eq.4) then
   ! Impose a fixed diurnal cycle...
   ! assumes model timestep is 1 hr
   ! assumes K=1 is 12z (Ks or ~ sunrise)
   ! First Precip...
   !       IF (K.GE.1 .and. K.LE.2) THEN
   if (K.eq.1) then
      PRCP1 =25.4/3600.0      !units mm/s  (Simulates 1"/hr for first time step...)
   elseif (K.gt.1) then
      PRCP1 = 0.
   end if
   ! Other Met. Vars...
   T2=290.0 + 3.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
   Q2X = 0.01
   U = 1.0
   V = 1.0
   PRES = 100000.0
   XLONG=400.0 + 25.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
   SHORT=450.0 + 450.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
end if


!!! FORC_TYPE 5 ============================================================================
!!!DJG  Idealized Met. w/ Specified Precip. Forcing Data...(Note: input precip units here are in 'mm/hr')
!   This option uses hard-wired met forcing EXCEPT precipitation which is read in
!   from a single, separate input file called 'YYYYMMDDHHMM.PRECIP_FORCING.nc'
!
if(FORC_TYP.eq.5) then
   ! Standard Met. Vars...
   T2=290.0 + 3.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
   Q2X = 0.01
   U = 1.0
   V = 1.0
   PRES = 100000.0
   XLONG=400.0 + 25.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
   SHORT=450.0 + 450.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
   
   !Get specified precip....
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
   !       product = "trmm"
   !       inflnm = trim(indir)//"/"//"sat_domain1.nc"
   !!Create forcing data filename...
   inflnm = trim(indir)//"/"//&
        olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
        olddate(15:16)//".PRECIP_FORCING.nc"
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
   if ( .not. fexist ) then
      print*, "no specified precipitation data found", inflnm
      call hydro_stop("In read_hydro_forcing_mpp1() - no specified precipitation data found")
   endif
   
   PRCP1 = 0.
   PRCP_old = PRCP1
   
#ifdef HYDRO_D
   print *, "Opening supplemental precipitation forcing file...",inflnm
#endif
   call READFORC_MDV_mpp(inflnm,IX,JX,   &
        PRCP2,mmflag,ierr_flg)
   
   !If radar or spec. data is ok use if not, skip to original NARR data...
   if (ierr_flg.eq.0) then   ! use spec. precip
      !Convert units if necessary
      if (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
         PRCP1=PRCP2/DT     !convert from mm to mm/s
#ifdef HYDRO_D
         print*, "Supplemental pcp is accumulated pcp/dt. "  
#endif
      else
         PRCP1=PRCP2   !assumes PRCP2 is in mm/s 
#ifdef HYDRO_D
         print*, "Supplemental pcp is rate. "  
#endif
      end if  ! Endif mmflag
   else   ! either stop or default to original forcing data...
#ifdef HYDRO_D
      print *,"Current RADAR precip data not found !!! Using previous available file..."
#endif
      PRCP1 = PRCP_old
   end if  ! Endif ierr_flg
   
   ! Loop through data to screen for plausible values
   do i=1,ix
      do j=1,jx
         if (PRCP1(i,j).lt.0.) PRCP1(i,j)= PRCP_old(i,j)
         if (PRCP1(i,j).gt.0.138889) PRCP1(i,j)=0.138889  !set max pcp intens = 500 mm/h
      end do
   end do
   
end if


!!! FORC_TYPE 6 ============================================================================
!!!DJG HRLDAS Forcing with hourly format filename with specified precipitation forcing...
!   This option uses HRLDAS-formatted met forcing EXCEPT precipitation which is read in
!   from a single, separate input file called 'YYYYMMDDHHMM.PRECIP_FORCING.nc'

if(FORC_TYP.eq.6) then
   
   !!Create forcing data filename...
   
#ifdef MPP_LAND
   if(my_id .eq. io_id) then
#endif
      do i_forcing = 1, nint(3600*12/dt)
         call geth_newdate(out_date,olddate,nint(dt*i_forcing))
         inflnm = trim(indir)//"/"//&
              out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
              ".LDASIN_DOMAIN"//hgrid
         
         inquire (file=trim(inflnm), exist=fexist)
         if(fexist) goto 101
      enddo
101   continue
#ifdef MPP_LAND
   endif
   call mpp_land_bcast_logical(fexist)
#endif
   
   if ( .not. fexist ) then
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
#endif
         do i_forcing = 1, nint(3600*12/dt)
            call geth_newdate(out_date,olddate,nint(dt*i_forcing))
            inflnm = trim(indir)//"/"//&
                 out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
                 out_date(15:16)//".LDASIN_DOMAIN"//hgrid
            inquire (file=trim(inflnm), exist=fexist)
            if(fexist) goto 102
         end do
102      continue 
#ifdef MPP_LAND
      endif
      call mpp_land_bcast_logical(fexist)
#endif
   endif
   
   
   if ( .not. fexist ) then
#ifdef HYDRO_D
      print*, "no ATM forcing data found at this time", inflnm
#endif
   else
#ifdef HYDRO_D
      print*, "reading forcing data at this time", inflnm
#endif
      
      call READFORC_HRLDAS_mpp(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
           PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
      PRCP_old = PRCP1  ! This assigns new precip to last precip as a fallback for missing data...
   endif
   
   
   !Get specified precip....
   !VIP, dimensions of grid are currently hardwired in input subroutine!!!
   !!Create forcing data filename...
   call geth_newdate(out_date,olddate,nint(dt))
   inflnm = trim(indir)//"/"//&
        out_date(1:4)//out_date(6:7)//out_date(9:10)//out_date(12:13)//&
        out_date(15:16)//".PRECIP_FORCING.nc"
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
#ifdef HYDRO_D
   if(my_id .eq. io_id) then
      if(fexist) then 
         print*, "using specified pcp forcing: ",trim(inflnm)
      else
         print*, "no specified pcp forcing: ",trim(inflnm)
      endif
   endif
#endif
   if ( .not. fexist ) then
      prcp1 = PRCP_old ! for missing pcp data use analysis/model input 
   else
      call READFORC_MDV_mpp(inflnm,IX,JX,   &
           PRCP2,mmflag,ierr_flg)
      !If radar or spec. data is ok use if not, skip to original NARR data...
      if(ierr_flg .ne. 0) then
#ifdef HYDRO_D
         print*, "WARNING: pcp reading problem: ", trim(inflnm)
#endif
         PRCP1=PRCP_old
      else
         PRCP1=PRCP2   !assumes PRCP2 is in mm/s
         if (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
            PRCP1=PRCP2/DT     !convert from mm to mm/s
         end if  ! Endif mmflag
#ifdef HYDRO_D
         if(my_id .eq. io_id) then
            print*, "replace pcp successfully! ",trim(inflnm)
         endif
#endif
      endif
   endif
   
   
   ! Loop through data to screen for plausible values
   where(PRCP1 .lt. 0) PRCP1=PRCP_old
   where(PRCP1 .gt. 10 ) PRCP1= PRCP_old 
   do i=1,ix
      do j=1,jx
         if (PRCP1(i,j).lt.0.) PRCP1(i,j)=0.0
         if (PRCP1(i,j).gt.0.138889) PRCP1(i,j)=0.138889  !set max pcp intens = 500 mm/h
      end do
   end do
   !       write(80,*) prcp1
   
end if


!!! FORC_TYPE 7 ============================================================================
!!!! FORC_TYP 7: uses WRF forcing data plus additional pcp forcing.

if(FORC_TYP.eq.7) then
   
   !!Create forcing data filename...
   inflnm = trim(indir)//"/"//&
        "wrfout_d0"//hgrid//"_"//&
        olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
        "_"//olddate(12:13)//":00:00"
   
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
   
   
   if ( .not. fexist ) then
#ifdef HYDRO_D
      print*, "no forcing data found", inflnm
#endif
   else
      do i_forcing = 1, int(24*3600/dt)
         wrf_dt = i_forcing*dt
         call geth_newdate(out_date,olddate,nint(wrf_dt))
         inflnm2 = trim(indir)//"/"//&
              "wrfout_d0"//hgrid//"_"//&
              out_date(1:4)//"-"//out_date(6:7)//"-"//out_date(9:10)//&
              "_"//out_date(12:13)//":00:00"
         inquire (file=trim(inflnm2), exist=fexist)
#ifdef MPP_LAND
         call mpp_land_bcast_logical(fexist)
#endif
         if (fexist ) goto 992
      end do
992   continue
      
#ifdef HYDRO_D
      print*, "read WRF forcing data: ", trim(inflnm)
      print*, "read WRF forcing data: ", trim(inflnm2)
#endif
      call READFORC_WRF_mpp(inflnm2,IX,JX,OLDDATE,T2,Q2X,U,V,   &
           PRES,XLONG,SHORT,PRCPnew,lai,fpar)
      call READFORC_WRF_mpp(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
           PRES,XLONG,SHORT,prcp0,lai,fpar)
      PRCP1=(PRCPnew-prcp0)/wrf_dt   !Adjustment to convert accum to rate...(mm/s)
      PRCP_old = PRCP1
   endif
   
   !Get specified precip....
   !VIP, dimensions of grid are currently hardwired in input subroutine!!!
   !!Create forcing data filename...
   inflnm = trim(indir)//"/"//&
        olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
        olddate(15:16)//".PRECIP_FORCING.nc"
   inquire (file=trim(inflnm), exist=fexist)
#ifdef MPP_LAND
   call mpp_land_bcast_logical(fexist)
#endif
#ifdef HYDRO_D
   if(fexist) then
      print*, "using specified pcp forcing: ",trim(inflnm)
   else
      print*, "no specified pcp forcing: ",trim(inflnm)
   endif
#endif
   if ( .not. fexist ) then
      prcp1 = PRCP_old ! for missing pcp data use analysis/model input 
   else
      call READFORC_MDV_mpp(inflnm,IX,JX,   &
           PRCP2,mmflag,ierr_flg)
      !If radar or spec. data is ok use if not, skip to original NARR data...
      if(ierr_flg .ne. 0) then
#ifdef HYDRO_D
         print*, "WARNING: pcp reading problem: ", trim(inflnm)
#endif
         PRCP1=PRCP_old
      else
         PRCP1=PRCP2   !assumes PRCP2 is in mm/s
         if (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
            write(6,*) "using supplemental pcp time interval ", DT
            PRCP1=PRCP2/DT     !convert from mm to mm/s
         else
            write(6,*) "using supplemental pcp rates "
         end if  ! Endif mmflag
#ifdef HYDRO_D
         print*, "replace pcp successfully! ",trim(inflnm)
#endif
      endif
   endif
   
   
   ! Loop through data to screen for plausible values
   where(PRCP1 .lt. 0) PRCP1=PRCP_old
   where(PRCP1 .gt. 10 ) PRCP1= PRCP_old ! set maximum to be 500 mm/h
   where(PRCP1 .gt. 0.138889) PRCP1= 0.138889 ! set maximum to be 500 mm/h
end if



!!!!DJG Check for snow data assimilation...
if (SNOW_ASSIM .eq. 1) then
   
   ! Every 24 hours, update the snow field from analyses.
   if(forc_typ .ne. 3 .or. forc_typ .ne. 6) then
      if ( OLDDATE(12:13) == "00") then
         call READSNOW_FORC_mpp(inflnm,IX,JX,WEASD,SNODEP)
      endif
   else
      call READSNOW_FORC_mpp(inflnm,IX,JX,WEASD,SNODEP)
   endif
   
end if

#ifdef PRECIP_DOUBLE
#ifdef HYDRO_D
print*,'PRECIP DOUBLE'
#endif
PRCP1 = PRCP1 * 2.0
#endif 

end subroutine read_hydro_forcing_mpp1



  subroutine READFORC_HRLDAS_mpp(flnm,ix,jx,target_date, t,q,u,v,p,lw,sw,pcp,lai,fpar)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: t
    real,             dimension(ix,jx), intent(out) :: q
    real,             dimension(ix,jx), intent(out) :: u
    real,             dimension(ix,jx), intent(out) :: v
    real,             dimension(ix,jx), intent(out) :: p
    real,             dimension(ix,jx), intent(out) :: lw
    real,             dimension(ix,jx), intent(out) :: sw
    real,             dimension(ix,jx), intent(out) :: pcp
    real,             dimension(ix,jx), intent(inout) :: lai
    real,             dimension(ix,jx), intent(inout) :: fpar

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.
#ifdef MPP_LAND
    real, allocatable, dimension(:,:):: buf2
    if(my_id .eq. io_id) then
        allocate(buf2(global_nx,global_ny))
    else
        allocate(buf2(1,1))
    endif
    if(my_id .eq. io_id) then
        ierr = nf_open(trim(flnm), NF_NOWRITE, ncid)
    endif
    call mpp_land_bcast_int1(ierr)
    if (ierr /= 0) then
       write(*,'("READFORC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_HRLDAS_mpp() - Problem opening netcdf file")
    endif


    if(my_id .eq. io_id ) call get_2d_netcdf("T2D", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,t)
    if(my_id .eq. io_id ) call get_2d_netcdf("Q2D", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,q)
    if(my_id .eq. io_id ) call get_2d_netcdf("U2D", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,u)
    if(my_id .eq. io_id ) call get_2d_netcdf("V2D", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,v)
    if(my_id .eq. io_id ) call get_2d_netcdf("PSFC", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,p)
    if(my_id .eq. io_id ) call get_2d_netcdf("LWDOWN", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,lw)
    if(my_id .eq. io_id ) call get_2d_netcdf("SWDOWN", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,sw)
    if(my_id .eq. io_id ) call get_2d_netcdf("RAINRATE", ncid,buf2, units, global_nx, global_ny, .TRUE., ierr)
    call decompose_data_real (buf2,pcp)
    if(my_id .eq. io_id ) then 
          call get_2d_netcdf("VEGFRA", ncid,buf2, units, global_nx, global_ny, .FALSE., ierr)
          if (ierr == 0) then
            if(maxval(buf2) .gt. 10 .and. maxval(buf2) .lt. 10000)  buf2 = buf2 * 1.E-2
          endif
    endif
    call mpp_land_bcast_int1(ierr)
    if(ierr == 0) call decompose_data_real (buf2,fpar)
    if(my_id .eq. io_id ) call get_2d_netcdf("LAI",     ncid, buf2,   units, ix, jx, .FALSE., ierr)
    call mpp_land_bcast_int1(ierr)
    if(ierr == 0) call decompose_data_real (buf2,lai)
    
    deallocate(buf2) 
#else
    ierr = nf_open(trim(flnm), NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("READFORC_HRLDAS")
    endif
    call get_2d_netcdf("T2D",     ncid, t,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("Q2D",     ncid, q,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("U2D",     ncid, u,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("V2D",     ncid, v,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("PSFC",    ncid, p,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("LWDOWN",  ncid, lw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SWDOWN",  ncid, sw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("RAINRATE",ncid, pcp,   units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("VEGFRA",  ncid, fpar,  units, ix, jx, .FALSE., ierr)

    if (ierr == 0) then
      if(maxval(fpar) .gt. 10 .and. maxval(fpar) .lt. 10000)  fpar = fpar * 1.E-2
    endif
    call get_2d_netcdf("LAI",     ncid, lai,   units, ix, jx, .FALSE., ierr)
#endif

    ierr = nf_close(ncid)

  end subroutine READFORC_HRLDAS_mpp

  subroutine READFORC_WRF_mpp(flnm,ix,jx,target_date,t,q,u,v,p,lw,sw,pcp,lai,fpar)

    implicit none
    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx) :: t,q,u,v,p,lw,sw,pcp,pcpc, lai,fpar
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid
#ifdef MPP_LAND
    real, allocatable, dimension(:,:) :: buf2
#endif

    tlevel = 1

    pcpc = 0

#ifdef MPP_LAND
    if(my_id .eq. io_id) then
          allocate(buf2(global_nx, global_ny) )
    else
          allocate(buf2(1, 1) )
    endif

    ! Open the NetCDF file.
   
    if(my_id .eq. io_id) ierr = nf_open(flnm, NF_NOWRITE, ncid)
    call mpp_land_bcast_int1(ierr)
    if (ierr /= 0) then
       write(*,'("READFORC_WRF Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_WRF_mpp() - Problem opening netcdf file")
    endif
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("T2",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,t)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("Q2",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,q)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("U10",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,u)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("V10",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,v)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("PSFC",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,p)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("GLW",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,lw)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("SWDOWN",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,sw)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("RAINC",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,pcpc)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("RAINNC",     ncid, buf2, global_nx, global_ny,tlevel, .true., ierr)
    call decompose_data_real (buf2,pcp)
    if(my_id .eq. io_id) call get_2d_netcdf_ruc("LAI",     ncid, buf2, global_nx, global_ny,tlevel, .false., ierr)
    call mpp_land_bcast_int1(ierr)
    if(ierr == 0) call decompose_data_real (buf2,lai)
    if(my_id .eq. io_id) then 
       call get_2d_netcdf_ruc("VEGFRA", ncid, fpar,  ix, jx,tlevel, .true., ierr)
       if(maxval(fpar) .gt. 10 .and. (maxval(fpar) .lt. 10000) ) fpar = fpar/100.
    endif
    call mpp_land_bcast_int1(ierr)
    if(ierr == 0) call decompose_data_real (buf2,fpar)
    deallocate(buf2)
#else

    ! Open the NetCDF file.
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_WRF Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READFORC_WRF_mpp() - Problem opening netcdf file")
    endif
    call get_2d_netcdf_ruc("T2",     ncid, t,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("Q2",     ncid, q,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("U10",    ncid, u,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("V10",    ncid, v,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("PSFC",   ncid, p,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("GLW",    ncid, lw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("SWDOWN", ncid, sw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINC",  ncid, pcpc,  ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINNC", ncid, pcp,   ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("VEGFRA", ncid, fpar,  ix, jx,tlevel, .false., ierr)
    if(ierr == 0) then
        if(maxval(fpar) .gt. 10 .and. (maxval(fpar) .lt. 10000) ) fpar = fpar/100.
    endif
    call get_2d_netcdf_ruc("LAI", ncid, lai,  ix, jx,tlevel, .false., ierr)

#endif


    pcp=pcp+pcpc   ! assumes pcpc=0 for resolved convection...
    ierr = nf_close(ncid)


  end subroutine READFORC_WRF_mpp

  subroutine READFORC_MDV_mpp(flnm,ix,jx,pcp,mmflag,ierr_flg)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    integer,                            intent(out)  :: ierr_flg
    integer :: it,jew,zsn
    real,             dimension(ix,jx), intent(out) :: pcp

    character(len=256) :: units
    integer :: ierr,i,j,i2,j2,varid
    integer :: ncid,mmflag
    real, dimension(ix,jx) :: temp
#ifdef MPP_LAND
    real, allocatable, dimension(:,:) :: buf2
    if(my_id .eq. io_id) then
       allocate(buf2(global_nx, global_ny))
    else
       allocate(buf2(1,1))
    endif
#endif

    mmflag = 0   ! flag for units spec. (0=mm, 1=mm/s)

    
!open NetCDF file...
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
#endif
        ierr_flg = nf_open(flnm, NF_NOWRITE, ncid)
#ifdef MPP_LAND
      endif
      call mpp_land_bcast_int1(ierr_flg)
#endif
        if (ierr_flg /= 0) then
          write(*,'("READFORC_MDV Problem opening netcdf file: ''",A,"''")') &
                trim(flnm)
#ifdef MPP_LAND
           deallocate(buf2)
#endif
           return
        end if

#ifdef MPP_LAND
      if(my_id .eq. io_id) then
#endif
        ierr = nf_inq_varid(ncid,  "precip",  varid)
#ifdef MPP_LAND
      endif
      call mpp_land_bcast_int1(ierr)
#endif
        if(ierr /= 0) ierr_flg = ierr
        if (ierr /= 0) then
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
#endif
          ierr = nf_inq_varid(ncid,  "precip_rate",  varid)   !recheck variable name...
#ifdef MPP_LAND
      endif
      call mpp_land_bcast_int1(ierr)
#endif
          if (ierr /= 0) then
            write(*,'("READFORC_MDV Problem reading precip netcdf file: ''", A,"''")') &
                 trim(flnm)
#ifdef MPP_LAND
              deallocate(buf2)
#endif
              return
          end if
          ierr_flg = ierr
          mmflag = 1
        end if
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
          ierr = nf_get_var_real(ncid, varid, buf2)
      endif
      call mpp_land_bcast_int1(ierr)
      if(ierr ==0) call decompose_data_real (buf2,pcp)
      deallocate(buf2)
#else
        ierr = nf_get_var_real(ncid, varid, pcp)
#endif
        if (ierr /= 0) then
           write(*,'("READFORC_MDV Problem reading netcdf file: ''", A,"''")') trim(flnm)
        end if
        ierr = nf_close(ncid)

  end subroutine READFORC_MDV_mpp

  subroutine READSNOW_FORC_mpp(flnm,ix,jx,weasd,snodep)
    implicit none

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    real,             dimension(ix,jx), intent(out) :: weasd
    real,             dimension(ix,jx), intent(out) :: snodep
    real, dimension(ix,jx) :: tmp

    character(len=256) :: units
    integer :: ierr
    integer :: ncid,i,j
#ifdef MPP_LAND
    real, allocatable, dimension(:,:) :: buf2
    if(my_id .eq. io_id) then
       allocate(buf2(global_nx, global_ny))
    else
       allocate(buf2(1,1))
    endif
#endif

    ! Open the NetCDF file.
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
#endif
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
#ifdef MPP_LAND
      endif
      call mpp_land_bcast_int1(ierr)
#endif
    if (ierr /= 0) then
       write(*,'("READSNOW Problem opening netcdf file: ''", A, "''")') trim(flnm)
       call hydro_stop("In READSNOW_FORC_mpp() - Problem opening netcdf file")
    endif

#ifdef MPP_LAND
      if(my_id .eq. io_id) then
          call get_2d_netcdf("WEASD",  ncid, buf2,   units, ix, jx, .FALSE., ierr)
      endif
      call mpp_land_bcast_int1(ierr)
      if(ierr == 0) call decompose_data_real (buf2,tmp)
#else
    call get_2d_netcdf("WEASD",  ncid, tmp,   units, ix, jx, .FALSE., ierr)
#endif
    if (ierr /= 0) then
         call get_2d_netcdf("SNOW",  ncid, tmp,   units, ix, jx, .FALSE., ierr)
         if (ierr == 0) then
            units = "mm"
#ifdef HYDRO_D
            print *, "read WEASD from wrfoutput ...... "
#endif
            weasd = tmp * 1.E-3
         endif
    else
         weasd = tmp
         if (trim(units) == "m") then
            ! No conversion necessary
         else if (trim(units) == "mm") then
            ! convert WEASD from mm to m
            weasd = weasd * 1.E-3
         endif
    endif

    if (ierr /= 0) then
       print *, "!!!!! NO WEASD present in input file...initialize to 0."
    endif
#ifdef MPP_LAND
      if(my_id .eq. io_id) then
         call get_2d_netcdf("SNODEP",     ncid, buf2,   units, ix, jx, .FALSE., ierr)
      endif
      call mpp_land_bcast_int1(ierr)
      if(ierr == 0) call decompose_data_real (buf2,tmp)
#else
    call get_2d_netcdf("SNODEP",     ncid, tmp,   units, ix, jx, .FALSE., ierr)
#endif
    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.

#ifdef MPP_LAND
      if(my_id .eq. io_id) then
         call get_2d_netcdf("SNOWH",     ncid, buf2,   units, ix, jx, .FALSE., ierr)
      endif
      call mpp_land_bcast_int1(ierr)
      if(ierr == 0) call decompose_data_real (buf2,tmp)
#else
         call get_2d_netcdf("SNOWH",     ncid, tmp,   units, ix, jx, .FALSE., ierr)
#endif
       if(ierr .eq. 0) then
#ifdef HYDRO_D
            print *, "read snow depth from wrfoutput ... "
#endif
            snodep = tmp
       endif
    else
       snodep = tmp
    endif

    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.
!yw       snodep = weasd * 10.
       where(snodep .lt. weasd) snodep = weasd*10  !set lower bound to correct bi-lin interp err...
    endif

!DJG check for erroneous neg WEASD or SNOWD due to offline interpolation...
       where(snodep .lt. 0) snodep = 0
       where(weasd .lt. 0) weasd = 0
    ierr = nf_close(ncid)

  end subroutine READSNOW_FORC_mpp

  subroutine read_ldasout(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)

      implicit none
      logical :: fexist
      integer :: ix,jx
      character(len=*) :: olddate,hgrid,indir
      character(len=19) :: outdate
      character(len=256) :: inflnm, inflnm2
      real :: dt
      real, dimension(ix,jx):: infxsrt,infxsrt2,soldrain,soldrain2
      integer :: ncid, ierr
      character(len=256) :: units
#ifdef MPP_LAND
      real, dimension(global_nx,global_ny) :: gArr
#endif

        ! check for file with hours first 
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASOUT_DOMAIN"//hgrid
        inquire (file=trim(inflnm), exist=fexist)

        if(.not. fexist) then
           ! check for file with minutes
             inflnm = trim(indir)//"/"//&
                  olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//olddate(15:16)//&
                  ".LDASOUT_DOMAIN"//hgrid
             inquire (file=trim(inflnm), exist=fexist)
        endif
        if(.not. fexist) then
            write(6,*) "Error: input file does not exist. Check ", trim(olddate)
            call hydro_stop( "LDASOUT input Error")
        endif

        call geth_newdate(outdate,olddate,nint(dt))
        ! check file for next date 
        ! check for file with hours first 
        inflnm2 = trim(indir)//"/"//&
             outdate(1:4)//outdate(6:7)//outdate(9:10)//outdate(12:13)//&
             ".LDASOUT_DOMAIN"//hgrid
        inquire (file=trim(inflnm2), exist=fexist)

        if(.not. fexist) then
           ! check for file with minutes
             inflnm2 = trim(indir)//"/"//&
                  outdate(1:4)//outdate(6:7)//outdate(9:10)//outdate(12:13)//outdate(15:16)//&
                  ".LDASOUT_DOMAIN"//hgrid
             inquire (file=trim(inflnm2), exist=fexist)
        endif
        if(.not. fexist) then
            write(6,*) "FATAL ERROR: input file does not exist. Check ", trim(outdate)
            call hydro_stop( "LDASOUT input Error")
        endif
!       read file1
#ifdef MPP_LAND
        if(my_id .eq. io_id) then
           ierr = nf_open(trim(inflnm), NF_NOWRITE, ncid)
           call get_2d_netcdf("SFCRNOFF",    ncid, gArr, units,  global_nx, global_ny, .TRUE., ierr)
        endif
        call decompose_data_real (gArr,infxsrt)
        if(my_id .eq. io_id) then
           call get_2d_netcdf("UGDRNOFF",    ncid, gArr, units, global_nx, global_ny, .TRUE., ierr)
        endif
        call decompose_data_real (gArr,soldrain)
        if(my_id .eq. io_id) then
            ierr = nf_close(ncid)
        endif
#else
        ierr = nf_open(trim(inflnm), NF_NOWRITE, ncid)
        call get_2d_netcdf("SFCRNOFF",    ncid, infxsrt, units,  ix, jx, .TRUE., ierr)
        call get_2d_netcdf("UGDRNOFF",    ncid, soldrain, units,  ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)
#endif
!       read file2
#ifdef MPP_LAND
       if(my_id .eq. io_id) then
           ierr = nf_open(trim(inflnm2), NF_NOWRITE, ncid)
           call get_2d_netcdf("SFCRNOFF",    ncid, gArr, units,  global_nx, global_ny, .TRUE., ierr)
        endif
        call decompose_data_real (gArr,infxsrt2)
        if(my_id .eq. io_id) then
           call get_2d_netcdf("UGDRNOFF",    ncid, gArr, units, global_nx, global_ny, .TRUE., ierr)
        endif
        call decompose_data_real (gArr,soldrain2)
        if(my_id .eq. io_id) then
           ierr = nf_close(ncid)
        endif
#else
        ierr = nf_open(trim(inflnm2), NF_NOWRITE, ncid)
        call get_2d_netcdf("SFCRNOFF",    ncid, infxsrt2, units,  ix, jx, .TRUE., ierr)
        call get_2d_netcdf("UGDRNOFF",    ncid, soldrain2, units,  ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)
#endif

        infxsrt = infxsrt2 - infxsrt
        soldrain = soldrain2 - soldrain

   end subroutine read_ldasout

!temporary for Noah model

  subroutine read_ldasout_seq(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)
      implicit none
      logical :: fexist
      integer :: ix,jx
      character(len=*) :: olddate,hgrid,indir
      character(len=19) :: outdate
      character(len=256) :: inflnm, inflnm2
      real :: dt
      real, dimension(ix,jx):: infxsrt,infxsrt2,soldrain,soldrain2
      integer :: ncid, ierr
      character(len=256) :: units

        ! check for file with hours first 
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASOUT_DOMAIN"//hgrid
        inquire (file=trim(inflnm), exist=fexist)

        if(.not. fexist) then
           ! check for file with minutes
             inflnm = trim(indir)//"/"//&
                  olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//olddate(15:16)//&
                  ".LDASOUT_DOMAIN"//hgrid
             inquire (file=trim(inflnm), exist=fexist)
        endif
        if(.not. fexist) then
            write(6,*) "FATAL ERROR: input file does not exist. Check ", trim(olddate)
            call hydro_stop( "LDASOUT input Error")
        endif

        call geth_newdate(outdate,olddate,nint(dt))
        ! check file for next date 
        ! check for file with hours first 
        inflnm2 = trim(indir)//"/"//&
             outdate(1:4)//outdate(6:7)//outdate(9:10)//outdate(12:13)//&
             ".LDASOUT_DOMAIN"//hgrid
        inquire (file=trim(inflnm2), exist=fexist)

        if(.not. fexist) then
           ! check for file with minutes
             inflnm2 = trim(indir)//"/"//&
                  outdate(1:4)//outdate(6:7)//outdate(9:10)//outdate(12:13)//outdate(15:16)//&
                  ".LDASOUT_DOMAIN"//hgrid
             inquire (file=trim(inflnm2), exist=fexist)
        endif
        if(.not. fexist) then
            write(6,*) "FATAL ERROR: input file does not exist. Check ", trim(outdate)
            call hydro_stop( "LDASOUT input Error")
        endif
!       read file1
        ierr = nf_open(trim(inflnm), NF_NOWRITE, ncid)
        call get_2d_netcdf("SFCRNOFF",    ncid, infxsrt, units,  ix, jx, .TRUE., ierr)
        call get_2d_netcdf("UGDRNOFF",    ncid, soldrain, units,  ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)
!       read file2
        ierr = nf_open(trim(inflnm2), NF_NOWRITE, ncid)
        call get_2d_netcdf("SFCRNOFF",    ncid, infxsrt2, units,  ix, jx, .TRUE., ierr)
        call get_2d_netcdf("UGDRNOFF",    ncid, soldrain2, units,  ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)

        infxsrt = infxsrt2 - infxsrt
        soldrain = soldrain2 - soldrain

   end subroutine read_ldasout_seq
end module module_lsm_forcing

     subroutine read_forc_ldasout(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)
      use module_lsm_forcing, only: read_ldasout
      implicit none
      integer :: ix,jx
      character(len=*) :: olddate,hgrid,indir
      real :: dt
      real, dimension(ix,jx):: infxsrt,soldrain
      call read_ldasout(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)
    end subroutine read_forc_ldasout
    
    subroutine read_forc_ldasout_seq(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)
! temporary for Noah model
      use module_lsm_forcing, only: read_ldasout_seq
      implicit none
      integer :: ix,jx
      character(len=*) :: olddate,hgrid,indir
      real :: dt
      real, dimension(ix,jx):: infxsrt,soldrain
      call read_ldasout_seq(olddate,hgrid, indir, dt,ix,jx,infxsrt,soldrain)
    end subroutine read_forc_ldasout_seq

