module module_HYDRO_io
#ifdef MPP_LAND
     use module_mpp_land
#endif
   use module_HYDRO_utils, only: get_dist_ll
   use module_namelist, only: nlst_rt
   use module_RT_data, only: rt_domain

   implicit none
#include <netcdf.inc>

     contains
        integer function get2d_real(var_name,out_buff,ix,jx,fileName)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          get2d_real = -1

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
#ifdef HYDRO_D 
            print*,"failed to open the netcdf file: ",trim(fileName)
#endif
            out_buff = -9999.
            return 
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
            ivar = nf_inq_varid(ncid,trim(var_name//"_M"),  varid)
            if(ivar .ne. 0) then
#ifdef HYDRO_D
               write(6,*) "Read Error: could not find ",var_name
#endif
                 return 
            endif
          end if
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
          get2d_real =  ivar
      end function get2d_real
     
     subroutine get2d_lsm_real(var_name,out_buff,ix,jx,fileName)
         implicit none
         integer ix,jx, status
         character (len=*),intent(in) :: var_name, fileName
         real,dimension(ix,jx):: out_buff
#ifdef MPP_LAND
         real,allocatable, dimension(:,:) :: buff_g

#ifdef HYDRO_D
         write(6,*) "start to read variable ", var_name
#endif
         allocate(buff_g (global_nx,global_ny) )

         if(my_id .eq. IO_id) then
            status = get2d_real(var_name,buff_g,global_nx,global_ny,fileName)
         end if
         call decompose_data_real(buff_g,out_buff)     
         deallocate(buff_g)
#else         
         status = get2d_real(var_name,out_buff,ix,jx,fileName)
#endif
#ifdef HYDRO_D
         write(6,*) "finish reading variable ", var_name
#endif
     end subroutine get2d_lsm_real

     subroutine get2d_lsm_vegtyp(out_buff,ix,jx,fileName)
         implicit none
         integer ix,jx, status,land_cat, iret, dimid,ncid
         character (len=*),intent(in) :: fileName
         character (len=256) units 
         integer,dimension(ix,jx):: out_buff
         real, dimension(ix,jx) :: xdum
#ifdef MPP_LAND
         real,allocatable, dimension(:,:) :: buff_g

         allocate(buff_g (global_nx,global_ny) )

         if(my_id .eq. IO_id) then
#endif
                ! Open the NetCDF file.
              iret = nf_open(fileName, NF_NOWRITE, ncid)
              if (iret /= 0) then
#ifdef HYDRO_D
                 write(*,'("Problem opening geo_static file: ''", A, "''")') &
                      trim(fileName)
                 call hydro_stop()
#endif
              endif

            iret = nf_inq_dimid(ncid, "land_cat", dimid)
            if (iret /= 0) then
#ifdef HYDRO_D
              print*, "nf_inq_dimid:  land_cat"
              call hydro_stop()
#endif
             endif

            iret = nf_inq_dimlen(ncid, dimid, land_cat)
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, "nf_inq_dimlen:  land_cat"
               call hydro_stop()
#endif
            endif

#ifdef MPP_LAND
            call get_landuse_netcdf(ncid, buff_g, units, global_nx ,global_ny, land_cat)
            iret = nf_close(ncid)
         end if

         call decompose_data_real(buff_g,xdum)     
         deallocate(buff_g)
#else         
          call get_landuse_netcdf(ncid, xdum,   units, ix, jx, land_cat)
          iret = nf_close(ncid)
#endif
         out_buff = nint(xdum)
     end subroutine get2d_lsm_vegtyp

     subroutine get_file_dimension(fileName, ix,jx)
            implicit none
            character(len=*) fileName
            integer ncid , iret, ix,jx, dimid
#ifdef MPP_LAND
            if(my_id .eq. IO_id) then
#endif
            iret = nf_open(fileName, NF_NOWRITE, ncid)
            if (iret /= 0) then
#ifdef HYDRO_D
               write(*,'("Problem opening geo_static file: ''", A, "''")') &
                    trim(fileName)
               call hydro_stop()
#endif
            endif
        
            iret = nf_inq_dimid(ncid, "west_east", dimid)
        
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, "nf_inq_dimid:  west_east"
               call hydro_stop()
#endif
            endif
        
            iret = nf_inq_dimlen(ncid, dimid, ix)
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, "nf_inq_dimlen:  west_east"
               call hydro_stop()
#endif
            endif
        
            iret = nf_inq_dimid(ncid, "south_north", dimid)
            if (iret /= 0) then
#ifdef HYDRO_D
                       print*, "nf_inq_dimid:  south_north"
                       call hydro_stop()
#endif
            endif
        
            iret = nf_inq_dimlen(ncid, dimid, jx)
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, "nf_inq_dimlen:  south_north"
               call hydro_stop()
#endif
            endif
            iret = nf_close(ncid)
#ifdef MPP_LAND
            endif
            call mpp_land_bcast_int1(ix)
            call mpp_land_bcast_int1(jx)
#endif

     end subroutine get_file_dimension

     subroutine get2d_lsm_soltyp(out_buff,ix,jx,fileName)
         implicit none
         integer ix,jx, status,land_cat, iret, dimid,ncid
         character (len=*),intent(in) :: fileName
         character (len=256) units 
         integer,dimension(ix,jx):: out_buff
         real, dimension(ix,jx) :: xdum
#ifdef MPP_LAND
         real,allocatable, dimension(:,:) :: buff_g

         allocate(buff_g (global_nx,global_ny) )

         if(my_id .eq. IO_id) then
#endif
                ! Open the NetCDF file.
              iret = nf_open(fileName, NF_NOWRITE, ncid)
              if (iret /= 0) then
#ifdef HYDRO_D
                 write(*,'("Problem opening geo_static file: ''", A, "''")') &
                      trim(fileName)
                 call hydro_stop()
#endif
              endif

            iret = nf_inq_dimid(ncid, "soil_cat", dimid)
            if (iret /= 0) then
#ifdef HYDRO_D
              print*, "nf_inq_dimid:  soil_cat"
              call hydro_stop()
#endif
             endif

            iret = nf_inq_dimlen(ncid, dimid, land_cat)
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, "nf_inq_dimlen:  soil_cat"
               call hydro_stop()
#endif
            endif

#ifdef MPP_LAND
            call get_soilcat_netcdf(ncid, buff_g, units, global_nx ,global_ny, land_cat)
            iret = nf_close(ncid)
         end if

         call decompose_data_real(buff_g,xdum)     
         deallocate(buff_g)
#else         
          call get_soilcat_netcdf(ncid, xdum,   units, ix, jx, land_cat)
          iret = nf_close(ncid)
#endif
          out_buff = nint(xdum)
     end subroutine get2d_lsm_soltyp


   



  subroutine get_landuse_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, l
    character(len=24), parameter :: name = "LANDUSEF"

    units = ""

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf_inq_varid"
       call hydro_stop()
#endif
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf_get_var_real"
       call hydro_stop()
#endif
    endif

    do i = 1, idim
       do j = 1, jdim
          mp = maxloc(xtmp(i,j,:))
          array(i,j) = mp(1)
          do l = 1,ldim
            if(xtmp(i,j,l).lt.0) array(i,j) = -9999.0
          enddo
       enddo
    enddo

  end subroutine get_landuse_netcdf


  subroutine get_soilcat_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
#include <netcdf.inc>

    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j
    character(len=24), parameter :: name = "SOILCTOP"

    units = ""

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_soilcat_netcdf:  nf_inq_varid"
       call hydro_stop()
#endif
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_soilcat_netcdf:  nf_get_var_real"
       call hydro_stop()
#endif
    endif

    do i = 1, idim
       do j = 1, jdim
          mp = maxloc(xtmp(i,j,:))
          array(i,j) = mp(1)
       enddo
    enddo

    where (array == 14) array = 1   ! DJG remove all 'water' soils...

  end subroutine get_soilcat_netcdf


subroutine get_greenfrac_netcdf(ncid, array3, units, idim, jdim, ldim,mm,dd)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid,mm,dd
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim) :: array
    real, dimension(idim,jdim) :: array2
    real, dimension(idim,jdim) :: diff
    real, dimension(idim,jdim), intent(out) :: array3
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, mm2,daytot
    real :: ddfrac
    character(len=24), parameter :: name = "GREENFRAC"

    units = "fraction"

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_inq_varid"
       call hydro_stop()
#endif
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_get_var_real"
       call hydro_stop()
#endif
    endif


    if (mm.lt.12) then 
      mm2 = mm+1
    else
      mm2 = 1
    end if

!DJG_DES Set up dates for daily interpolation...
          if (mm.eq.1.OR.mm.eq.3.OR.mm.eq.5.OR.mm.eq.7.OR.mm.eq.8.OR.mm.eq.10.OR.mm.eq.12) then
             daytot = 31
          else if (mm.eq.4.OR.mm.eq.6.OR.mm.eq.9.OR.mm.eq.11) then 
             daytot = 30
          else if (mm.eq.2) then
             daytot = 28
          end if
          ddfrac = float(dd)/float(daytot)
          if (ddfrac.gt.1.0) ddfrac = 1.0   ! Assumes Feb. 29th change is same as Feb 28th

#ifdef HYDRO_D
    print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac
#endif

    do i = 1, idim
       do j = 1, jdim
          array(i,j) = xtmp(i,j,mm)   !GREENFRAC in geogrid in units of fraction from month 1
          array2(i,j) = xtmp(i,j,mm2)   !GREENFRAC in geogrid in units of fraction from month 1
          diff(i,j) = array2(i,j) - array(i,j)
          array3(i,j) = array(i,j) + ddfrac * diff(i,j) 
       enddo
    enddo

end subroutine get_greenfrac_netcdf



subroutine get_albedo12m_netcdf(ncid, array3, units, idim, jdim, ldim,mm,dd)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid,mm,dd
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim) :: array
    real, dimension(idim,jdim) :: array2
    real, dimension(idim,jdim) :: diff
    real, dimension(idim,jdim), intent(out) :: array3
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, mm2,daytot
    real :: ddfrac
    character(len=24), parameter :: name = "ALBEDO12M"


    units = "fraction"

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_albedo12m_netcdf:  nf_inq_varid"
       call hydro_stop()
#endif
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
#ifdef HYDRO_D
       print*, 'name = "', trim(name)//'"'
       print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_albedo12m_netcdf:  nf_get_var_real"
       call hydro_stop()
#endif
    endif

    if (mm.lt.12) then 
      mm2 = mm+1
    else
      mm2 = 1
    end if

!DJG_DES Set up dates for daily interpolation...
          if (mm.eq.1.OR.mm.eq.3.OR.mm.eq.5.OR.mm.eq.7.OR.mm.eq.8.OR.mm.eq.10.OR.mm.eq.12) then
             daytot = 31
          else if (mm.eq.4.OR.mm.eq.6.OR.mm.eq.9.OR.mm.eq.11) then 
             daytot = 30
          else if (mm.eq.2) then
             daytot = 28
          end if
          ddfrac = float(dd)/float(daytot)
          if (ddfrac.gt.1.0) ddfrac = 1.0   ! Assumes Feb. 29th change is same as Feb 28th

#ifdef HYDRO_D
    print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac
#endif

    do i = 1, idim
       do j = 1, jdim
          array(i,j) = xtmp(i,j,mm) / 100.0   !Convert ALBEDO12M from % to fraction...month 1
          array2(i,j) = xtmp(i,j,mm2) / 100.0   !Convert ALBEDO12M from % to fraction... month 2
          diff(i,j) = array2(i,j) - array(i,j)
          array3(i,j) = array(i,j) + ddfrac * diff(i,j) 
       enddo
    enddo

end subroutine get_albedo12m_netcdf



  subroutine get_2d_netcdf(name, ncid, array, units, idim, jdim, &
       fatal_if_error, ierr)
    implicit none
#include <netcdf.inc>
    character(len=*), intent(in) :: name
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    ! .TRUE._IF_ERROR:  an input code value:
    !      .TRUE. if an error in reading the data should stop the program.
    !      Otherwise the, IERR error flag is set, but the program continues.
    logical, intent(in) :: fatal_if_error 
    integer, intent(out) :: ierr

    units = ""

    iret = nf_inq_varid(ncid,  name,  varid)

    if (iret /= 0) then
       if (fatal_IF_ERROR) then
#ifdef HYDRO_D
          print*, 'name = "', trim(name)//'"'
          print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_inq_varid"
          call hydro_stop()
#endif
       else
          ierr = iret
          return
       endif
    endif


    iret = nf_get_var_real(ncid, varid, array)
    if (iret /= 0) then
       if (fatal_IF_ERROR) then
#ifdef HYDRO_D
          print*, 'name = "', trim(name)//'"'
          print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_get_var_real"
          call hydro_stop()
#endif
       else
          ierr = iret
          return
       endif
    endif

    ierr = 0;
  end subroutine get_2d_netcdf

      subroutine get_2d_netcdf_cows(var_name,ncid,var, &
            ix,jx,tlevel,fatal_if_error,ierr)
#include <netcdf.inc>
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
#ifdef HYDRO_D
           print*, "MODULE_NOAHLSM_HRLDAS_INPUT: get_2d_netcdf_cows:nf_inq_varid"
           call hydro_stop()
#endif
        else
          ierr = iret
          return
        endif
      endif
      iret = nf_get_vara_real(ncid, varid, start,count,var)

      return
      end subroutine get_2d_netcdf_cows

!---------------------------------------------------------
!DJG Subroutinesfor inputting routing fields...
!DNY   first reads the files to get the size of the 
!DNY   LINKS arrays
!DJG   - Currently only hi-res topo is read 
!DJG   - At a future time, use this routine to input
!DJG     subgrid land-use classification or routing
!DJG     parameters 'overland roughness' and 'retention
!DJG     depth'
!
!DJG,DNY - Update this subroutine to read in channel and lake
!           parameters if activated       11.20.2005
!---------------------------------------------------------
       SUBROUTINE READ_ROUTEDIM(IXRT,JXRT,route_chan_f,route_link_f, &
            route_direction_f, route_lake_f, NLINKS, NLAKES, &
            CH_NETLNK, channel_option, geo_finegrid_flnm)

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: I,J,channel_option,iret,jj
        INTEGER, INTENT(INOUT)                       :: NLINKS, NLAKES
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON

!!Dummy read in grids for inverted y-axis
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT_inv   !- binary channel mask
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION_inv  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT_inv
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT_inv, LON_inv


        CHARACTER(len=*)       :: route_chan_f, route_link_f,route_direction_f,route_lake_f
        CHARACTER(len=256)       :: InputLine
        CHARACTER(len=256)       :: geo_finegrid_flnm
        CHARACTER(len=256)       :: var_name
!        external get2d_real
!       integer ::  get2d_real
     
        NLINKS = 0
        NLAKES = 0
        CH_NETRT = -9999
        CH_NETLNK = -9999


        cnt = 0 
#ifdef HYDRO_D
       print *, "Channel Option in Routedim is ", channel_option
#endif

       IF(channel_option.eq.3) then  !get maxnodes and links from grid

         var_name = "CHANNELGRID"
         call get2d_int(var_name,CH_NETRT_inv,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))
         
         var_name = "FLOWDIRECTION"
         call get2d_int(var_name,DIRECTION_inv,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))

         var_name = "LAKEGRID"
         call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))


        var_name = "LATITUDE"
        iret =  get2d_real(var_name,LAT_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        var_name = "LONGITUDE"
        iret = get2d_real(var_name,LON_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))

!!!Flip y-dimension of highres grids from exported Arc files...


        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           CH_NETRT(i,j)=CH_NETRT_inv(i,jj)
           DIRECTION(i,j)=DIRECTION_inv(i,jj)
           LAKE_MSKRT(i,j)=LAKE_MSKRT_inv(i,jj)
           LAT(i,j)=LAT_inv(i,jj)
           LON(i,j)=LON_inv(i,jj)
           jj=jxrt-j
         end do
        end do
          
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do


!DJG inv         do j=jxrt,1,-1
         do j=1,jxrt
             do i = 1, ixrt
               if (CH_NETRT(i,j) .ge.0.AND.CH_NETRT(i,j).lt.100) then 
                 NLINKS = NLINKS + 1
               endif
            end do 
         end do 
#ifdef HYDRO_D
         print *, "NLINKS IS ", NLINKS 
#endif


!DJG inv         DO j = JXRT,1,-1  !rows
         DO j = 1,JXRT  !rows
          DO i = 1 ,IXRT   !colsumns
           If (CH_NETRT(i, j) .ge. 0) then !get its direction
            If ((DIRECTION(i, j) .EQ. 64) .AND. (j+1 .LE. JXRT).AND.(CH_NETRT(i,j+1) .ge.0) ) then !North
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
               .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i+1,j+1) .ge.0)) then !North East
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT).AND.(CH_NETRT(i+1,j) .ge. 0)) then !East
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt 
            else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0).AND.(CH_NETRT(i+1,j-1).ge.0)) then !south east
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 4).AND.(j - 1 .NE. 0).AND.(CH_NETRT(i,j-1).ge.0)) then !due south
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
                    .AND. (j - 1 .NE. 0).AND. (CH_NETRT(i-1,j-1).ge.0) ) then !south west
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0).AND.(CH_NETRT(i-1,j).ge.0)) then !West
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i-1,j+1).ge.0)) then !North West
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt 
           else 
#ifdef HYDRO_D
             write(*,135) "PrPt/LkIn", CH_NETRT(i,j), DIRECTION(i,j), LON(i,j), LAT(i,j),i,j 
135             FORMAT(A9,1X,I3,1X,I3,1X,F10.5,1X,F9.5,1X,I4,1X,I4)
#endif
             if (DIRECTION(i,j) .eq. 0) then
#ifdef HYDRO_D
               print *, "Direction i,j ",i,j," of point ", cnt, "is invalid"
#endif
             endif

           End If
         End If !CH_NETRT check for this node
        END DO
       END DO 
#ifdef HYDRO_D
       print *, "found type 0 nodes", cnt
#endif
      
!Find out if the boundaries are on an edge or flow into a lake
!DJG inv       DO j = JXRT,1,-1
       DO j = 1,JXRT
         DO i = 1 ,IXRT
          If (CH_NETRT(i, j) .ge. 0) then !get its direction

           If ( ((DIRECTION(i, j).EQ. 64) .AND. (j + 1 .GT. JXRT))         & !-- 64's can only flow north
               .OR. ((DIRECTION(i, j) .EQ. 64).and. (j<jxrt) .AND. (CH_NETRT(i,j+1) .lt. 0))) then !North
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point N", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .GT. IXRT))  & !-- 128's can flow out of the North or East edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (j + 1 .GT. JXRT))  & !   this is due north edge     
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (i<ixrt .and. j<jxrt) .AND.(CH_NETRT(i + 1, j + 1).lt.0))) then !North East
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point NE", cnt, CH_NETRT(i,j),i,j
#endif
           else if (((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .GT. IXRT))     & !-- 1's can only flow due east
               .OR. ((DIRECTION(i, j) .EQ. 1) .and. (i<ixrt) .AND. (CH_NETRT(i + 1, j) .lt. 0))) then !East
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point E", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .GT. IXRT))    &      !-- 2's can flow out of east or south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .AND. (j - 1 .EQ. 0))       &      !-- this is the south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .and. (i<ixrt .and. j>1) .AND.(CH_NETRT(i + 1, j - 1) .lt.0))) then !south east
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point SE", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .EQ. 0))       &      !-- 4's can only flow due south
                .OR. ((DIRECTION(i, j) .EQ. 4) .and. (j>1) .AND.(CH_NETRT(i, j - 1) .lt. 0))) then !due south
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point S", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .LE. 0))      &      !-- 8's can flow south or west
               .OR.  ((DIRECTION(i, j) .EQ. 8) .AND. (j - 1 .EQ. 0))      &      !-- this is the south edge
               .OR.  ((DIRECTION(i, j).EQ.8).and. (i>1 .and. j>1) .AND.(CH_NETRT(i - 1, j - 1).lt.0))) then !south west
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point SW", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .LE. 0))       &      !-- 16's can only flow due west 
               .OR.  ((DIRECTION(i, j).EQ.16) .and. (i>1) .AND.(CH_NETRT(i - 1, j).lt.0))) then !West
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt              
#ifdef HYDRO_D
              print *, "Boundary Pour Point W", cnt,CH_NETRT(i,j), i,j
#endif
           else if ( ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .LE. 0))      &      !-- 32's can flow either west or north
               .OR.  ((DIRECTION(i, j) .EQ. 32) .AND. (j + 1 .GT. JXRT))   &      !-- this is the north edge
               .OR.  ((DIRECTION(i, j).EQ.32) .and. (i>1 .and. j<jxrt) .AND.(CH_NETRT(i - 1, j + 1).lt.0))) then !North West
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point NW", cnt,CH_NETRT(i,j), i,j
#endif
           endif
          endif !CH_NETRT check for this node
         END DO
       END DO 

#ifdef HYDRO_D
       print *, "total number of channel elements", cnt
       print *, "total number of NLINKS          ", NLINKS
#endif



      !-- get the number of lakes
       if (cnt .ne. NLINKS) then 
#ifdef HYDRO_D
         print *, "Apparent error in network topology", cnt, NLINKS
         call hydro_stop()
#endif
       endif
!DJG inv       do j=jxrt,1,-1
       do j=1,jxrt
          do i = 1,ixrt
           if (LAKE_MSKRT(i,j) .gt. NLAKES) then 
             NLAKES = LAKE_MSKRT(i,j)
           endif
        end do
       end do
#ifdef HYDRO_D
       write(6,*) "finish read_red ..  nlakes = ", nlakes
#endif

       return

     else  ! get nlinks from the ascii file of links
        open(unit=79,file=trim(route_link_f),          & !link
                   form='formatted',status='old')
#ifdef HYDRO_D
                   write(6,*) "read file ",route_link_f
#endif
1011    read(79,*,end= 1999) InputLine
        NLINKS = NLINKS + 1
        goto 1011
1999   continue
        NLINKS = NLINKS - 1 !-- first line is a comment 
        close(79)
#ifdef HYDRO_D
        print *, "Number of Links on sparse network", NLINKS
#endif
        return
     end if

     END SUBROUTINE READ_ROUTEDIM

!---------------------------------------------------------
       SUBROUTINE READ_CHROUTING(IXRT,JXRT,ELRT,CH_NETRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,LATVAL,LONVAL, &
            STRMFRXSTPTS,geo_finegrid_flnm)
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: ELRT
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION
        INTEGER, DIMENSION(IXRT,JXRT)                :: GSTRMFRXSTPTS
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER,                DIMENSION(IXRT,JXRT) :: GORDER  !-- gridded stream orderk
        INTEGER,                DIMENSION(IXRT,JXRT) :: Link_Location !-- gridded stream orderk
        INTEGER                                      :: I,J,channel_option
        REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)      :: LATVAL, LONVAL
        CHARACTER(len=28)                            :: dir
!Dummy inverted grids from arc
	INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION_inv
        INTEGER, DIMENSION(IXRT,JXRT)                :: GSTRMFRXSTPTS_inv
        INTEGER, DIMENSION(IXRT,JXRT) :: LAKE_MSKRT_inv
        INTEGER,                DIMENSION(IXRT,JXRT) :: GORDER_inv  !-- gridded stream orderk
        REAL, DIMENSION(IXRT,JXRT)      :: LATVAL_inv, LONVAL_inv


!----DJG,DNY New variables for channel and lake routing
        CHARACTER(len=155)	 :: header
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ZELEV
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHLAT,CHLON

        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TYPEL
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TO_NODE,ORDER
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: STRMFRXSTPTS

        INTEGER, INTENT(INOUT)                       :: MAXORDER
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MUSK, MUSX !muskingum
        REAL, INTENT(INOUT),  DIMENSION(NLINKS,2)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE  ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK
        REAL,  DIMENSION(IXRT,JXRT)                  ::  ChSSlpG,BwG,MannNG  !channel properties on Grid


!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: HRZAREA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LAKEMAXH
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(NLINKS)       :: ChSSlp, Bw

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: geo_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid, iret, jj,ct
        real                                         :: gc,n

!---------------------------------------------------------
! End Declarations
!---------------------------------------------------------
        MAXORDER = -9999
!initialize GSTRM
        GSTRMFRXSTPTS = -9999

!yw initialize the array.
        to_node =   MAXORDER
        from_node = MAXORDER

#ifdef HYDRO_D
        print *, "reading routing initialization files..."
        print *, "route direction", route_direction_f
        print *, "route order", route_order_f
        print *, "route linke",route_link_f
        print *, "route lake",route_lake_f
#endif

!DJG Edited code here to retrieve data from hires netcdf file....

   IF((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option.eq.3) then

        var_name = "LATITUDE"
        iret = get2d_real(var_name,LATVAL_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        var_name = "LONGITUDE"
        iret = get2d_real(var_name,LONVAL_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))

    END IF


       IF(CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2) then
!DJG change filename to LAKEPARM.TBL        open(unit=79,file=trim(route_link_f),          &
        open(unit=79,file='LAKEPARM.TBL',          &
                  form='formatted',status='old')
       END IF


       var_name = "LAKEGRID"
       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))
       var_name = "FLOWDIRECTION"
       call get2d_int(var_name,DIRECTION_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))
       var_name = "STREAMORDER"
       call get2d_int(var_name,GORDER_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))
       var_name = "frxst_pts"
       call get2d_int(var_name,GSTRMFRXSTPTS_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

!--1/13/2011 real hi res sfc calibrtion parameters (...)
!       var_name = "LAKEGRID"
!       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
!               trim(geo_finegrid_flnm))
!       var_name = "LAKEGRID"
!       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
!               trim(geo_finegrid_flnm))


!-- real hi res channel properties (not yet implemented...)
!        var_name = "MANNINGS"
!        iret = get2d_real(var_name,MannNG,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))
!        var_name = "SIDE_SLOPE"
!        iret = get2d_real(var_name,ChSSlpG,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))
!        var_name = "BOTTOM_WIDTH"
!        iret = get2d_real(var_name,BwG,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))


!!!Flip y-dimension of highres grids from exported Arc files...


        ct = 0
        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           LAKE_MSKRT(i,j)=LAKE_MSKRT_inv(i,jj)
           DIRECTION(i,j)=DIRECTION_inv(i,jj)
           GORDER(i,j)=GORDER_inv(i,jj)
           GSTRMFRXSTPTS(i,j)=GSTRMFRXSTPTS_inv(i,jj)
           if(GSTRMFRXSTPTS(i,j).ne.-9999) ct = ct+1
           LATVAL(i,j)=LATVAL_inv(i,jj)
           LONVAL(i,j)=LONVAL_inv(i,jj)
           jj=jxrt-j
         end do
        end do
       
!        if(dist(1,1,1) .eq. -999) then 
!           call get_dist_ll(dist,latval,lonval,ixrt,jxrt)
!        end if

        
#ifdef HYDRO_D
	print *, "Number of frxst pts: ",ct
#endif
          
     
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do

      cnt =0
      if ((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option .ne. 3) then ! not routing on grid, read from file
       read(79,*)  header
       do i=1,NLINKS
          read (79,*) tmp, FROM_NODE(i), TO_NODE(i), TYPEL(i),&
                   ORDER(i), QLINK(i,1), MUSK(i), MUSX(i), CHANLEN(i), &
                   MannN(i), So(i), ChSSlp(i), Bw(i), HRZAREA(i),&
                   LAKEMAXH(i), WEIRC(i), WEIRL(i), ORIFICEC(i), &
                   ORIFICEA(i),ORIFICEE(i)

           !-- hardwire QLINK
          QLINK(i,1) = 1.0
          QLINK(i,2) = QLINK(i,1)

          if (So(i).lt.0.005) So(i) = 0.005  !-- impose a minimum slope requireement
 
          if (ORDER(i) .gt. MAXORDER) then
            MAXORDER = ORDER(i)
          endif

        end do

      elseif ((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option.eq.3) then  !-- handle setting up topology on the grid for diffusion scheme

       read(79,*)  header  !-- read the lake file
#ifdef HYDRO_D
       write(*,*) "reading lake file ", header
       write(6,*) "error check read file ",route_link_f
#endif


      if (NLAKES.gt.0) then !read in only if there are lakes
       do i=1, NLAKES
        read (79,*) tmp, HRZAREA(i),LAKEMAXH(i), &
          WEIRC(i), WEIRL(i), ORIFICEC(i), ORIFICEA(i), ORIFICEE(i),&
           LATLAKE(i), LONLAKE(i),ELEVLAKE(i)
#ifdef HYDRO_D
        write (*,*) tmp, HRZAREA(i),LAKEMAXH(i), LATLAKE(i), LONLAKE(i),ELEVLAKE(i),NLAKES
#endif
       enddo
      end if   !end if for NLAKES >0 check

       cnt = 0 

!yw  add temperary to initialize the following two variables.
     BwG = 0.0
     ChSSlpG = 0.0

!DJG inv       DO j = JXRT,1,-1  !rows
       DO j = 1,JXRT  !rows
        DO i = 1 ,IXRT   !colsumns
         If (CH_NETRT(i, j) .ge. 0) then !get its direction and assign its elevation and order
          If ((DIRECTION(i, j) .EQ. 64) .AND. (j + 1 .LE. JXRT) .AND. &
               (CH_NETRT(i,j+1).ge.0) ) then !North
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i, j + 1)
             CHANLEN(cnt) = dist(i,j,1)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i+1,j+1).ge.0) ) then !North East
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j + 1)
             CHANLEN(cnt) = dist(i,j,2)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT) &
                    .AND. (CH_NETRT(i+1,j).ge.0) ) then !East
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j)
             CHANLEN(cnt) = dist(i,j,3)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0) .AND. (CH_NETRT(i+1,j-1).ge.0) ) then !south east
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j - 1)
             CHANLEN(cnt) = dist(i,j,4)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .NE. 0).AND.(CH_NETRT(i,j-1).ge.0) ) then !due south
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i, j - 1)
             CHANLEN(cnt) = dist(i,j,5)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
               .AND. (j - 1 .NE. 0) .AND. (CH_NETRT(i-1,j-1).ge.0)) then !south west
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i,j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j - 1)
             CHANLEN(cnt) = dist(i,j,6)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0).AND.(CH_NETRT(i-1,j).ge.0) ) then !West
             cnt = cnt + 1
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j)
             CHANLEN(cnt) = dist(i,j,7)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i-1,j+1).ge.0) ) then !North West
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j + 1)
             CHANLEN(cnt) = dist(i,j,8)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else 
#ifdef HYDRO_D
             print *, "NO MATCH", i,j,CH_NETLNK(i,j),DIRECTION(i,j),i + 1,j - 1 !south east
#endif
          End If

         End If !CH_NETRT check for this node

        END DO
       END DO 

#ifdef HYDRO_D
       print *, "after exiting the channel, this many nodes", cnt
       write(*,*) " " 
#endif

!Find out if the boundaries are on an edge
!DJG inv       DO j = JXRT,1,-1
       DO j = 1,JXRT
         DO i = 1 ,IXRT
          If (CH_NETRT(i, j) .ge. 0) then !get its direction
           If (((DIRECTION(i, j).EQ. 64) .AND. (j + 1 .GT. JXRT)) .OR. &        !-- 64's can only flow north
              ((DIRECTION(i, j) .EQ. 64) .and. (j < jxrt) .AND. (CH_NETRT(i,j+1) .lt. 0))) then !North
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)

              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(j+1 .GT. JXRT) then !-- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i,j+1).gt.0) then 
               TYPEL(cnt) = 2
               LAKENODE(cnt) = LAKE_MSKRT(i,j+1)
              else
               TYPEL(cnt) = 1 
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,1)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point N", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if ( ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .GT. IXRT))  & !-- 128's can flow out of the North or East edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (j + 1 .GT. JXRT))  & !   this is due north edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (i<ixrt .and. j<jxrt) .AND.(CH_NETRT(i + 1, j + 1).lt.0))) then !North East
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)

              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if((i+1 .GT. IXRT) .OR. (j+1 .GT. JXRT))  then ! an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j+1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j+1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,2)  
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point NE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if (((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .GT. IXRT)) .OR. &    !-- 1's can only flow due east
               ((DIRECTION(i, j) .EQ. 1) .and. (i<ixrt) .AND. (CH_NETRT(i + 1, j) .lt. 0))) then !East
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(i+1 .GT. IXRT) then  !an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,3)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point E", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if ( ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .GT. IXRT))    &      !-- 2's can flow out of east or south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .AND. (j - 1 .EQ. 0))       &      !-- this is the south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .and. (i<ixrt .and. j>1) .AND.(CH_NETRT(i + 1, j - 1) .lt.0))) then !south east
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if((i+1 .GT. IXRT) .OR. (j-1 .EQ. 0)) then !an edge 
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,4)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point SE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if (((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .EQ. 0)) .OR. &       !-- 4's can only flow due south
               ((DIRECTION(i, j) .EQ. 4) .and. (j>1) .AND.(CH_NETRT(i, j - 1) .lt. 0))) then !due south
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(j-1 .EQ. 0) then !- an edge
                TYPEL(cnt) =1
              elseif(LAKE_MSKRT(i,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,5)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point S", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
          else if ( ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .LE. 0))      &      !-- 8's can flow south or west
               .OR.  ((DIRECTION(i, j) .EQ. 8) .AND. (j - 1 .EQ. 0))      &      !-- this is the south edge
               .OR.  ((DIRECTION(i, j).EQ.8).and. (i>1 .and. j>1) .AND.(CH_NETRT(i - 1, j - 1).lt.0))) then !south west
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if( (i-1 .EQ. 0) .OR. (j-1 .EQ. 0) ) then !- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,6) 
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point SW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if (((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .LE.0) ) &                  !16's can only flow due west
               .OR.((DIRECTION(i, j).EQ.16) .and. (i>1) .AND.(CH_NETRT(i - 1, j).lt.0))) then !West
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(i-1 .EQ. 0) then !-- an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,7)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point W", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           else if ( ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .LE. 0))      &      !-- 32's can flow either west or north
               .OR.  ((DIRECTION(i, j) .EQ. 32) .AND. (j + 1 .GT. JXRT))   &      !-- this is the north edge
               .OR.  ((DIRECTION(i, j).EQ.32) .and. (i>1 .and. j<jxrt) .AND.(CH_NETRT(i - 1, j + 1).lt.0))) then !North West
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if( (i-1 .EQ. 0) .OR. (j+1 .GT. JXRT)) then !-- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j+1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j+1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,8)   
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
#ifdef HYDRO_D
              print *, "Pour Point NW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
           endif
          endif !CH_NETRT check for this node
         END DO
       END DO 
      endif

     close(79)
       if (cnt .ne. NLINKS) then 
#ifdef HYDRO_D
         print *, "Apparent error in network topology", cnt, NLINKS
         call hydro_stop()
#endif
       endif

     do i=1,NLINKS
#ifdef HYDRO_D
      if (STRMFRXSTPTS(i).ne.-9999) print *,"Frxst_pt: ",STRMFRXSTPTS(i)
#endif
     end do
     return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_CHROUTING
!DJG -----------------------------------------------------

#ifdef MPP_LAND
!this subroutine mapping the channel network and lakes.
   subroutine MPP_CHROUTING_CONF(g_ixrt,g_jxrt,ixrt,jxrt, NLAKES,NLINKS,&
         lake_mskrt, lake_index,link_location,HRZAREA,LAKEMAXH,WEIRC,WEIRL,&
             ORIFICEC,ORIFICEA,ORIFICEE,LATLAKE,LONLAKE,ELEVLAKE, &
         FROM_NODE,TO_NODE,ZELEV,CHLAT,CHLON,TYPEL, ORDER,CHANLEN, &
         CHANXI,CHANYJ, lakenode,mpp_nlinks, nlinks_index,MAXORDER,yw_mpp_nlinks)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
   implicit none 
#include <netcdf.inc>
   integer :: i,j,ixrt,g_ixrt,jxrt,g_jxrt, nlakes, nlinks
   integer, dimension(ixrt,jxrt)  :: LAKE_MSKRT, lakenode
   integer, INTENT(OUT) ,dimension(ixrt,jxrt):: link_location
   integer, INTENT(OUT) :: mpp_nlinks, yw_mpp_nlinks
   integer, INTENT(OUT),dimension(nlinks) :: nlinks_index, lake_index
   
   INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE,TO_NODE, &
            TYPEL,CHANXI,CHANYJ,ORDER
   REAL, INTENT(INOUT),  DIMENSION(NLINKS)   ::CHANLEN,  ZELEV
   REAL, INTENT(INOUT),  DIMENSION(NLINKS)   ::CHLAT, CHLON
   !yw REAL,  DIMENSION(NLINKS)   ::CHLAT4, CHLON4
   integer,  DIMENSION(NLINKS)   :: node_table 
   integer , dimension(g_ixrt,g_jxrt):: g_tmp
   real ywtest(nlinks)
   integer  maxorder
  
! Lake information 
   REAL, INTENT(INOUT),  DIMENSION(*) :: HRZAREA,LAKEMAXH,WEIRC,WEIRL,&
             ORIFICEC,ORIFICEA,ORIFICEE,LATLAKE,LONLAKE,ELEVLAKE

      call mpp_land_bcast_int(NLINKS,FROM_NODE)
      call mpp_land_bcast_int(NLINKS,TO_NODE)
      call mpp_land_bcast_int(NLINKS,TYPEL)
      call mpp_land_bcast_int(NLINKS,ORDER)
      call mpp_land_bcast_int(NLINKS,LAKENODE)

      call mpp_land_bcast_real(NLINKS,CHANLEN)
      call mpp_land_bcast_real(NLINKS,ZELEV)
      
      call mpp_land_bcast_real(NLINKS,CHLAT)
      call mpp_land_bcast_real(NLINKS,CHLON)

      call mpp_land_max_int1(MAXORDER)
      if(MAXORDER .eq. 0)  MAXORDER = -9999

       lake_index = -99
       do j = 1, jxrt
          do i = 1, ixrt
            if (LAKE_MSKRT(i,j) .gt. 0) then
               lake_index(LAKE_MSKRT(i,j)) = LAKE_MSKRT(i,j)
            endif
          enddo
       enddo

       link_location = 0
       if(my_id .eq. IO_id) then
          g_tmp = -1
          do i = 1, nlinks
             g_tmp( CHANXI(i),CHANYJ(i) )  = i 
          enddo
       endif
       call decompose_RT_int(g_tmp,link_location,g_ixrt, g_jxrt, ixrt, jxrt)

      CHANXI = 0
      CHANYj = 0
      do j = 1, jxrt
          do i = 1, ixrt
             if(link_location(i,j) .gt. 0) then
               CHANXI(link_location(i,j)) = i
               CHANYJ(link_location(i,j)) = j
             endif
          end do
      end do

      node_table = 0
      do j = 1, jxrt
          do i = 1, ixrt
            if(link_location(i,j) .gt. 0) then 
               if( i.eq.1 .and. left_id > 0) then
                    continue 
               elseif ( i.eq. ixrt .and. right_id >0) then
                    continue
               elseif ( j.eq. 1 .and. down_id >0 ) then
                    continue
               elseif ( j.eq. jxrt .and. up_id >0) then
                    continue
               else
                  node_table(link_location(i,j)) = link_location(i,j) 
               endif
            endif 
          end do
      end do
      mpp_nlinks = 0
      do i = 1, nlinks
          if(node_table(i) > 0 ) then
                mpp_nlinks = mpp_nlinks + 1
                nlinks_index(mpp_nlinks) = i
          endif
      enddo

!     mpp_nlinks = 0
!     do j = 1, jxrt
!         do i = 1, ixrt
!           if(link_location(i,j) .gt. 0) then 
!               mpp_nlinks = mpp_nlinks + 1
!               nlinks_index(mpp_nlinks) = link_location(i,j)
!           endif
!         enddo
!     enddo
 
! add the boundary links
      yw_mpp_nlinks = mpp_nlinks  
      do j = 1, jxrt
          do i = 1, ixrt
            if(link_location(i,j) .gt. 0) then 
               if( i.eq.1 .and. left_id > 0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( i.eq. ixrt .and. right_id >0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( j.eq. 1 .and. down_id >0 ) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( j.eq. jxrt .and. up_id >0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               else
                  continue
               endif
            endif 
          end do
      end do


      call mpp_land_bcast_real(NLAKES,HRZAREA)
      call mpp_land_bcast_real(NLAKES,LAKEMAXH)
      call mpp_land_bcast_real(NLAKES,WEIRC)
      call mpp_land_bcast_real(NLAKES,WEIRL)
      call mpp_land_bcast_real(NLAKES,ORIFICEC)
      call mpp_land_bcast_real(NLAKES,ORIFICEA)
      call mpp_land_bcast_real(NLAKES,ORIFICEE)
      call mpp_land_bcast_real(NLAKES,LATLAKE)
      call mpp_land_bcast_real(NLAKES,LONLAKE)
      call mpp_land_bcast_real(NLAKES,ELEVLAKE)

   end subroutine MPP_CHROUTING_CONF
#endif

#ifdef MPP_LAND
  subroutine MPP_READ_SIMP_GW(IX,JX,IXRT,JXRT,GWSUBBASMSK,gwbasmskfil,&
          gw_strm_msk,numbasns,ch_netrt,AGGFACTRT)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
    
    integer, intent(in)                     :: IX,JX,IXRT,JXRT,AGGFACTRT
    integer, intent(out)                    :: numbasns
    integer, intent(out), dimension(IX,JX)  :: GWSUBBASMSK
    integer, intent(out), dimension(IXRT,JXRT)  :: gw_strm_msk
    integer, intent(in), dimension(IXRT,JXRT)  :: ch_netrt
    character(len=256)                      :: gwbasmskfil
    integer,dimension(global_nX,global_ny) ::  g_GWSUBBASMSK
    integer,dimension(global_rt_nx, global_rt_ny) ::  g_gw_strm_msk,g_ch_netrt


     call write_IO_rt_int(ch_netrt,g_ch_netrt)

     if(my_id .eq. IO_id) then
       call READ_SIMP_GW(global_nX,global_ny,global_rt_nx,global_rt_ny,&
             g_GWSUBBASMSK,gwbasmskfil,g_gw_strm_msk,numbasns,&
             g_ch_netrt,AGGFACTRT) 
     endif
     call decompose_data_int(g_GWSUBBASMSK,GWSUBBASMSK)
     call decompose_RT_int(g_gw_strm_msk,gw_strm_msk,  &
          global_rt_nx, global_rt_ny,ixrt,jxrt)
     call mpp_land_bcast_int1(numbasns)
  return
  end subroutine MPP_READ_SIMP_GW
#endif

!DJG -----------------------------------------------------
!   SUBROUTINE READ_SIMP_GW
!DJG -----------------------------------------------------

  subroutine READ_SIMP_GW(IX,JX,IXRT,JXRT,GWSUBBASMSK,gwbasmskfil,&
          gw_strm_msk,numbasns,ch_netrt,AGGFACTRT)
    implicit none
#include <netcdf.inc>

    integer, intent(in)                     :: IX,JX,IXRT,JXRT,AGGFACTRT
    integer, intent(in), dimension(IXRT,JXRT)  :: ch_netrt
    integer, intent(out)                    :: numbasns
    integer, intent(out), dimension(IX,JX)  :: GWSUBBASMSK
    integer, intent(out), dimension(IXRT,JXRT)  :: gw_strm_msk
    character(len=256)                      :: gwbasmskfil
    integer                                 :: i,j,aggfacxrt,aggfacyrt,ixxrt,jyyrt

    numbasns = 0
    gw_strm_msk = -9999

!Open files...
    open(unit=91,file=trim(gwbasmskfil),          &
            form='formatted',status='old')

!Read in sub-basin mask...
    do j=jx,1,-1
          read (91,*) (GWSUBBASMSK(i,j),i=1,ix)
    end do
    close(91)


!Loop through to count number of basins and assign basin indices to chan grid
     do J=1,JX
       do I=1,IX

!Determine max number of basins...(assumes basins are numbered
!   sequentially from 1 to max number of basins...)
        if (GWSUBBASMSK(i,j).gt.numbasns) then
          numbasns = GWSUBBASMSK(i,j)   ! get count of basins...
        end if

!Assign gw basin index values to channel grid...
        do AGGFACYRT=AGGFACTRT-1,0,-1
          do AGGFACXRT=AGGFACTRT-1,0,-1

             IXXRT=I*AGGFACTRT-AGGFACXRT
             JYYRT=J*AGGFACTRT-AGGFACYRT
             IF(ch_netrt(IXXRT,JYYRT).ge.0) then  !If channel grid cell
               gw_strm_msk(IXXRT,JYYRT) = GWSUBBASMSK(i,j)  ! assign coarse grid basn indx to chan grid
             END IF

           end do !AGGFACXRT
         end do !AGGFACYRT

      end do   !I-ix
    end do    !J-jx

    return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_SIMP_GW
!DJG -----------------------------------------------------

  ! BF read the static input fields needed for the 2D GW scheme
  subroutine readGW2d(ix, jx, hc, ihead, botelv, por, ltype)
  implicit none
#include <netcdf.inc>
  integer, intent(in) :: ix, jx
  integer, dimension(ix,jx), intent(inout)::   ltype
  real, dimension(ix,jx), intent(inout)   ::   hc, ihead, botelv, por

#ifdef MPP_LAND
  integer, dimension(:,:), allocatable ::  gLtype
  real, dimension(:,:), allocatable    ::  gHC, gIHEAD, gBOTELV, gPOR
#endif
  integer :: i
!, get2d_real
  
#ifdef MPP_LAND
  allocate(gHC(global_rt_nx, global_rt_ny))
  allocate(gIHEAD(global_rt_nx, global_rt_ny))
  allocate(gBOTELV(global_rt_nx, global_rt_ny))
  allocate(gPOR(global_rt_nx, global_rt_ny))
  allocate(gLtype(global_rt_nx, global_rt_ny))
  
  if(my_id .eq. IO_id) then
#ifdef HYDRO_D
  print*, "2D GW-Scheme selected, retrieving files from gwhires.nc ..."
#endif
#endif


        ! hydraulic conductivity
        i = get2d_real("HC", &
#ifdef MPP_LAND
                       gHC, global_nx, global_ny,  &
#else
                       hc, ix, jx,  &
#endif
                       trim("./gwhires.nc"))

        ! initial head
        i = get2d_real("IHEAD", &
#ifdef MPP_LAND
                       gIHEAD, global_nx, global_ny, &
#else
                       ihead,  ix, jx, &
#endif
                       trim("./gwhires.nc"))
                       
        ! aquifer bottom elevation                
        i = get2d_real("BOTELV", &
#ifdef MPP_LAND
                       gBOTELV, global_nx, global_ny, &
#else
                       botelv, ix, jx,  &
#endif
                       trim("./gwhires.nc"))
                       
	! aquifer porosity
        i = get2d_real("POR", &
#ifdef MPP_LAND
                       gPOR, global_nx, global_ny, &
#else
                       por, ix, jx,  &
#endif
                       trim("./gwhires.nc"))

! bftodo: develop proper landtype mask
 
#ifdef MPP_LAND
       gLtype=1
       gLtype(1,:) = 2
       gLtype(:,1) = 2
       gLtype(global_rt_nx,:) = 2
       gLtype(:,global_rt_ny) = 2 
#else
       ltype=1
       ltype(1,:) =2
       ltype(:,1) =2
       ltype(ix,:)=2
       ltype(:,jx)=2
#endif

#ifdef MPP_LAND  
  endif
     call decompose_rt_int (gLtype, ltype, global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gHC,hc,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gIHEAD,ihead,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gBOTELV,botelv,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gPOR,por,global_rt_nx, global_rt_ny, ix, jx)
     deallocate(gHC, gIHEAD, gBOTELV, gPOR)
#endif
  !bftodo: make filename accessible in namelist
  return
  end subroutine readGW2d
  !BF
 



  subroutine output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
       q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
       geo_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,HIRES_OUT,  &
       QBDRYRT)

!output the routing variables over routing grid.
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    integer,                                  intent(in) :: ixrt,jxrt
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt,jxrt,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT 
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: geo_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d, xcoord
    real*8, allocatable, DIMENSION(:)                   :: ycoord_d, ycoord

    integer, save :: ncid,ncstatic
    integer, save :: output_count
    real,    dimension(nsoil) :: asldpth

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret, dimid_soil, i,j,ii,jj
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=32)  :: convention
    character(len=34)  :: sec_since_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: xdum,QSUBRT,ZWATTABLRT,SUB_RESID
    real, dimension(ixrt,jxrt) :: q_sfcflx_x,q_sfcflx_y
    real, dimension(ixrt,jxrt) :: QSTRMVOLRT
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: soxrt,soyrt
    real, dimension(ixrt,jxrt) :: LATVAL,LONVAL, QBDRYRT
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = int(dt)*output_count

    decimation = 1 !-- decimation factor
    ixrtd = int(ixrt/decimation)
    jxrtd = int(jxrt/decimation)
    allocate(xdumd(ixrtd,jxrtd))
    allocate(xcoord_d(ixrtd))
    allocate(ycoord_d(jxrtd))
    allocate(xcoord(ixrtd))
    allocate(ycoord(jxrtd))
    ii = 0
    jj = 0

!DJG Dump timeseries for channel inflow accum. for calibration...(8/28/09)
    chan_in = 0.0
    do j=1,jxrt
      do i=1,ixrt
        chan_in=chan_in+QSTRMVOLRT(I,J)/1000.0*(dist(i,j,9))  !(units m^3)
      enddo
    enddo
    open (unit=46,file='qstrmvolrt_accum.txt',form='formatted',&
             status='unknown',position='append')
    write (46,713) chan_in
713 FORMAT (F20.7)
    close (46)
!    return
!DJG end dump of channel inflow for calibration....

    if (hires_out.eq.1) return  ! return if hires flag eq 1, if =2 output full grid

    if (output_count == 0) then

   !-- Open the  finemesh static files to obtain projection information
#ifdef HYDRO_D
      write(*,'("geo_finegrid_flnm: ''", A, "''")') trim(geo_finegrid_flnm)
#endif
      iret = nf_open(trim(geo_finegrid_flnm), NF_NOWRITE, ncstatic)

      if (iret /= 0) then
#ifdef HYDRO_D
         write(*,'("Problem opening geo_finegrid file: ''", A, "''")') &
         trim(geo_finegrid_flnm)
         write(*,*) "HIRES_OUTPUT will not be georeferenced..."
#endif
         
        hires_flag = 0
      else
        hires_flag = 1
      endif

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord)
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord)
     else
      xcoord_d = 0.
      ycoord_d = 0.
     end if  !endif hires_georef 

     do j=jxrt,1,-1*decimation
        jj = jj+1
        if (jj<= jxrtd) then
         ycoord_d(jj) = ycoord(j)
        endif
     enddo

!yw     do i = 1,ixrt,decimation
!yw        ii = ii + 1
!yw        if (ii <= ixrtd) then 
!yw         xcoord_d(ii) = xcoord(i)
         xcoord_d = xcoord
!yw        endif
!yw     enddo
       

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)
      if(iret .eq. 0) iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file
     end if  !endif hires_georef 
      iret = nf_close(ncstatic)

!-- create the fine grid routing file
       write(output_flnm, '(A12,".RTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
       print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

       iret = nf_create(trim(output_flnm), 0, ncid)
#ifdef HYDRO_D
       if (iret /= 0) then
         print*, "Problem nf_create"
         call hydro_stop()
       endif
#endif

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils

!--- define variables
!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
        iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/dimid_ix/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
        iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
        iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
          iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/dimid_jx/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- LATITUDE
        iret = nf_def_var(ncid,"LATITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',8,'LATITUDE')
        iret = nf_put_att_text(ncid,varid,'standard_name',8,'LATITUDE')
        iret = nf_put_att_text(ncid,varid,'units',5,'deg North')

       !- LONGITUDE
          iret = nf_def_var(ncid,"LONGITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',9,'LONGITUDE')
          iret = nf_put_att_text(ncid,varid,'standard_name',9,'LONGITUDE')
          iret = nf_put_att_text(ncid,varid,'units',5,'deg east')

       !-- z-level is soil
        iret = nf_def_var(ncid,"depth", NF_FLOAT, 1, (/dimid_soil/),varid)
        iret = nf_put_att_text(ncid,varid,'units',2,'cm')
        iret = nf_put_att_text(ncid,varid,'long_name',19,'depth of soil layer')

         iret = nf_def_var(ncid,  "SOIL_M",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
            iret = nf_put_att_text(ncid,varid,'units',7,'m^3/m^3')
            iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
            iret = nf_put_att_text(ncid,varid,'long_name',26,soilm)
            iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
            iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
            iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"ESNOW2D",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)

!       iret = nf_def_var(ncid,"QSUBRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',15,'subsurface flow')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

          iret = nf_def_var(ncid,"ZWATTABLRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
          iret = nf_put_att_text(ncid,varid,'units',1,'m')
          iret = nf_put_att_text(ncid,varid,'long_name',17,'water table depth')
          iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
          iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
          iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"Q_SFCFLX_X",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'surface flux x')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"Q_SFCFLX_Y",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'surface flux y')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"QSTRMVOLRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',14,'channel inflow')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"SFCHEADSUBRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"SOXRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!      iret = nf_put_att_text(ncid,varid,'units',1,'1')
!      iret = nf_put_att_text(ncid,varid,'long_name',7,'slope x')
!      iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!      iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!      iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"SOYRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!      iret = nf_put_att_text(ncid,varid,'units',1,'1')
!      iret = nf_put_att_text(ncid,varid,'long_name',7,'slope 7')
!      iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!      iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!      iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"SUB_RESID",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)

       iret = nf_def_var(ncid,"QBDRYRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',70, &
          'accumulated value of the boundary flux, + into domain, - out of domain')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!-- place projection information
     if(hires_flag.eq.1) then !if/then hires_georef
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_FLOAT,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_FLOAT,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_FLOAT,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_FLOAT,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_FLOAT,2,sp)
     end if   !endif hires_georef

!      iret = nf_def_var(ncid,"Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)
      iret = nf_put_att_int(ncid,NF_GLOBAL,"output_decimation_factor",NF_INT, 1,decimation)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
!       xdumd = LATVAL
        iret = nf_inq_varid(ncid,"x", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
         iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrtd/), xcoord_d) !-- 1-d array

!       xdumd = LONVAL
        iret = nf_inq_varid(ncid,"y", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrtd/), ycoord_d) !-- 1-d array

        xdumd = LATVAL
        iret = nf_inq_varid(ncid,"LATITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

        xdumd = LONVAL
        iret = nf_inq_varid(ncid,"LONGITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)


#ifdef HYDRO_D 
        write (*,*) "TEST....",LONVAL (1,1),(1,2)
        write (*,*) "TEST....",LATVAL (1,1),(1,2)
#endif




       do n = 1,nsoil
        if(n == 1) then
         asldpth(n) = -sldpth(n)
        else
         asldpth(n) = asldpth(n-1) - sldpth(n)
        endif
       enddo

       iret = nf_inq_varid(ncid,"depth", varid)
       iret = nf_put_vara_real(ncid, varid, (/1/), (/nsoil/), asldpth)
!yw       iret = nf_close(ncstatic)

    endif

    output_count = output_count + 1

!!-- time
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

!-- 3-d soils
     do n = 1, nsoil
!DJG inv      jj = int(jxrt/decimation)
      jj = 1
      ii = 0
!DJG inv      do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
       do i = 1,ixrt,decimation
        ii = ii + 1  
        if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
         xdumd(ii,jj) = smcrt(i,j,n)
        endif
      enddo 
       ii = 0
!DJG inv       jj = jj -1
       jj = jj + 1
     enddo
!       where (vegtyp(:,:) == 16) xdum = -1.E33
          iret = nf_inq_varid(ncid,  "SOIL_M", varid)
          iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ixrtd,jxrtd,1,1/), xdumd)
    enddo !-n soils


!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = QSUBRT(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "QSUBRT", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

!    xdum = ZWATTABLRT
!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = ZWATTABLRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv       jj = jj - 1 
       jj = jj + 1 
      enddo
        iret = nf_inq_varid(ncid,  "ZWATTABLRT", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)


!!    xdum = Q_SFCFLX_X
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = Q_SFCFLX_X(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "Q_SFCFLX_X", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!!
!!    xdum = Q_SFCFLX_Y
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = Q_SFCFLX_Y(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "Q_SFCFLX_Y", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

       jj = 1
       ii = 0
       do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = QBDRYRT(i,j)
         endif
       enddo 
       ii = 0
       jj = jj + 1 
      enddo
     iret = nf_inq_varid(ncid,  "QBDRYRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)


!    xdum = QSTRMVOLRT
!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = QSTRMVOLRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv      jj = jj - 1 
       jj = jj + 1 
      enddo
     iret = nf_inq_varid(ncid,  "QSTRMVOLRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
!    xdum = SFCHEADSUBRT 
!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
      do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = SFCHEADSUBRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv       jj = jj - 1 
       jj = jj + 1 
      enddo
     iret = nf_inq_varid(ncid,  "SFCHEADSUBRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 

!   iret = nf_inq_varid(ncid,  "SOXRT", varid)
!   iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!    iret = nf_inq_varid(ncid,  "SOYRT", varid)
!    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!
!!    xdum = SUB_RESID
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!!    iret = nf_inq_varid(ncid,  "SUB_RESID", varid)
!!    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     deallocate(xdumd)
     deallocate(xcoord_d)
     deallocate(xcoord)
     deallocate(ycoord_d)
     deallocate(ycoord)
    
#ifdef HYDRO_D 
     write(6,*) "end of output_rt" 
#endif

  end subroutine output_rt

!BF output section for gw2d model
!bftodo: clean up an customize for GW usage
  subroutine output_gw(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, HEAD, SMCRT, convgw, SFCHEADSUBRT, &
       geo_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,HIRES_OUT)

#ifdef MPP_LAND
       USE module_mpp_land
#endif
!output the routing variables over routing grid.
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    integer,                                  intent(in) :: ixrt,jxrt
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt,jxrt,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT 
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: geo_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d, xcoord
    real*8, allocatable, DIMENSION(:)                   :: ycoord_d, ycoord

    integer, save :: ncid,ncstatic
    integer, save :: output_count
    real,    dimension(nsoil) :: asldpth

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret, dimid_soil, i,j,ii,jj
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=32)  :: convention
    character(len=34)  :: sec_since_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: head, convgw
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: latval,lonval
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    
#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: gHead, gConvgw, gSFCHEADSUBRT
    real, dimension(global_rt_nx,global_rt_ny) :: gLatval, gLonval
    real, dimension(global_rt_nx,global_rt_ny,nsoil) :: gSMCRT
#endif
    
#ifdef MPP_LAND
    call write_IO_rt_real(latval,gLatval)
    call write_IO_rt_real(lonval,gLonval)
    call write_IO_rt_real(SFCHEADSUBRT,gSFCHEADSUBRT)
    call write_IO_rt_real(head,gHead)
    call write_IO_rt_real(convgw,gConvgw)
    
    do i = 1, NSOIL
     call write_IO_rt_real(SMCRT(:,:,i),gSMCRT(:,:,i))
    end do

   if(my_id.eq.IO_id) then
     

#endif
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = int(dt)*output_count

    decimation = 1 !-- decimation factor
#ifdef MPP_LAND
    ixrtd = int(global_rt_nx/decimation)
    jxrtd = int(global_rt_ny/decimation)
#else
    ixrtd = int(ixrt/decimation)
    jxrtd = int(jxrt/decimation)
#endif
    allocate(xdumd(ixrtd,jxrtd))
    allocate(xcoord_d(ixrtd))
    allocate(ycoord_d(jxrtd))
    allocate(xcoord(ixrtd))
    allocate(ycoord(jxrtd))
    ii = 0
    jj = 0

    if (hires_out.eq.1) return  ! return if hires flag eq 1, if =2 output full grid

    if (output_count == 0) then

   !-- Open the  finemesh static files to obtain projection information
#ifdef HYDRO_D
      write(*,'("geo_finegrid_flnm: ''", A, "''")') trim(geo_finegrid_flnm)

#endif
      iret = nf_open(trim(geo_finegrid_flnm), NF_NOWRITE, ncstatic)

      if (iret /= 0) then
#ifdef HYDRO_D
         write(*,'("Problem opening geo_finegrid file: ''", A, "''")') &
         trim(geo_finegrid_flnm)
         write(*,*) "HIRES_OUTPUT will not be georeferenced..."
#endif
        hires_flag = 0
      else
        hires_flag = 1
      endif

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord)
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord)
     else
      xcoord_d = 0.
      ycoord_d = 0.
     end if  !endif hires_georef 

     do j=jxrt,1,-1*decimation
        jj = jj+1
        if (jj<= jxrtd) then
         ycoord_d(jj) = ycoord(j)
        endif
     enddo

!yw     do i = 1,ixrt,decimation
!yw        ii = ii + 1
!yw        if (ii <= ixrtd) then 
!yw         xcoord_d(ii) = xcoord(i)
         xcoord_d = xcoord
!yw        endif
!yw     enddo
       

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)
      if(iret .eq. 0) iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file
     end if  !endif hires_georef 
      iret = nf_close(ncstatic)

!-- create the fine grid routing file
       write(output_flnm, '(A12,".GW_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
       print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

       iret = nf_create(trim(output_flnm), 0, ncid)
#ifdef HYDRO_D
       if (iret /= 0) then
         print*, "Problem nf_create"
         call hydro_stop()
       endif
#endif

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils

!--- define variables
       !- time definition, timeObs
       iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
       iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/dimid_ix/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
       iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
       iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
       iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
       iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
       iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- LATITUDE
       iret = nf_def_var(ncid,"LATITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',8,'LATITUDE')
       iret = nf_put_att_text(ncid,varid,'standard_name',8,'LATITUDE')
       iret = nf_put_att_text(ncid,varid,'units',5,'deg North')

       !- LONGITUDE
       iret = nf_def_var(ncid,"LONGITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',9,'LONGITUDE')
       iret = nf_put_att_text(ncid,varid,'standard_name',9,'LONGITUDE')
       iret = nf_put_att_text(ncid,varid,'units',5,'deg east')

       !-- z-level is soil
       iret = nf_def_var(ncid,"depth", NF_FLOAT, 1, (/dimid_soil/),varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'cm')
       iret = nf_put_att_text(ncid,varid,'long_name',19,'depth of soil layer')

       iret = nf_def_var(ncid,  "SOIL_M",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
       iret = nf_put_att_text(ncid,varid,'long_name',26,soilm)
       iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"HEAD",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',1,'m')
       iret = nf_put_att_text(ncid,varid,'long_name',17,'groundwater head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"CONVGW",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'channel flux')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"Platzhalter",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!-- place projection information
     if(hires_flag.eq.1) then !if/then hires_georef
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_FLOAT,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_FLOAT,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_FLOAT,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_FLOAT,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_FLOAT,2,sp)
     end if   !endif hires_georef

!      iret = nf_def_var(ncid,"Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)
      iret = nf_put_att_int(ncid,NF_GLOBAL,"output_decimation_factor",NF_INT, 1,decimation)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
!       xdumd = LATVAL
        iret = nf_inq_varid(ncid,"x", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
	iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrtd/), xcoord_d) !-- 1-d array

!       xdumd = LONVAL
        iret = nf_inq_varid(ncid,"y", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrtd/), ycoord_d) !-- 1-d array

#ifdef MPP_LAND
        xdumd = gLATVAL
#else  
        xdumd = LATVAL
#endif
        iret = nf_inq_varid(ncid,"LATITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

#ifdef MPP_LAND
        xdumd = gLONVAL
#else  
        xdumd = LONVAL
#endif
        iret = nf_inq_varid(ncid,"LONGITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

       do n = 1,nsoil
        if(n == 1) then
         asldpth(n) = -sldpth(n)
        else
         asldpth(n) = asldpth(n-1) - sldpth(n)
        endif
       enddo

       iret = nf_inq_varid(ncid,"depth", varid)
       iret = nf_put_vara_real(ncid, varid, (/1/), (/nsoil/), asldpth)
!yw       iret = nf_close(ncstatic)

    endif

    output_count = output_count + 1

!!-- time
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

!-- 3-d soils
     do n = 1, nsoil
#ifdef MPP_LAND
        xdumd = gSMCRT(:,:,n)
#else  
        xdumd = SMCRT(:,:,n)
#endif
! !DJG inv      jj = int(jxrt/decimation)
!       jj = 1
!       ii = 0
! !DJG inv      do j = jxrt,1,-decimation
!        do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = smcrt(i,j,n)
!         endif
!       enddo 
!        ii = 0
! !DJG inv       jj = jj -1
!        jj = jj + 1
!      enddo
!       where (vegtyp(:,:) == 16) xdum = -1.E33
          iret = nf_inq_varid(ncid,  "SOIL_M", varid)
          iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ixrtd,jxrtd,1,1/), xdumd)
    enddo !-n soils

#ifdef MPP_LAND
        xdumd = gHead
#else  
        xdumd = head
#endif

     iret = nf_inq_varid(ncid,  "HEAD", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

#ifdef MPP_LAND
        xdumd = gConvgw
#else  
        xdumd = convgw
#endif
     iret = nf_inq_varid(ncid,  "CONVGW", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     deallocate(xdumd)
     deallocate(xcoord_d)
     deallocate(xcoord)
     deallocate(ycoord_d)
     deallocate(ycoord)
    
#ifdef HYDRO_D 
     write(6,*) "end of output_ge" 
#endif
#ifdef MPP_LAND
    endif
#endif

  end subroutine output_gw

!-- output the channel route in an IDV 'station' compatible format
   subroutine output_chrt(igrid, split_output_count, NLINKS, ORDER, &
        startdate,date,chlon, chlat, hlink,zelev,qlink,dtrt,K,   &
        STRMFRXSTPTS,order_to_write)
     
     implicit none
#include <netcdf.inc>
!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(NLINKS),                  intent(in) :: chlon,chlat
     real, dimension(NLINKS),                  intent(in) :: hlink,zelev
     integer, dimension(NLINKS),               intent(in) :: ORDER
     integer, dimension(NLINKS),               intent(inout) :: STRMFRXSTPTS

     real,                                     intent(in) :: dtrt
     real, dimension(NLINKS,2),                intent(in) :: qlink

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

     real, allocatable, DIMENSION(:)            :: chanlat,chanlon
     real, allocatable, DIMENSION(:)            :: chanlatO,chanlonO

     real, allocatable, DIMENSION(:)            :: elevation
     real, allocatable, DIMENSION(:)            :: elevationO

     integer, allocatable, DIMENSION(:)         :: station_id
     integer, allocatable, DIMENSION(:)         :: station_idO

     integer, allocatable, DIMENSION(:)         :: rec_num_of_station
     integer, allocatable, DIMENSION(:)         :: rec_num_of_stationO

     integer, allocatable, DIMENSION(:)         :: lOrder !- local stream order
     integer, allocatable, DIMENSION(:)         :: lOrderO !- local stream order

     integer, save  :: output_count
     integer, save  :: ncid,ncid2

     integer :: stationdim, dimdata, varid, charid, n
     integer :: obsdim, dimdataO, charidO

     integer :: iret,i, start_pos, prev_pos, order_to_write!-- order_to_write is the lowest stream order to output
     integer :: start_posO, prev_posO

     integer :: previous_pos  !-- used for the station model
     character(len=256) :: output_flnm,output_flnm2
     character(len=19)  :: date19,date19start
     character(len=34)  :: sec_since_date
     integer :: seconds_since,nstations,cnt,ObsStation,nobs
     character(len=32)  :: convention
     character(len=11),allocatable, DIMENSION(:)  :: stname
     character(len=11),allocatable, DIMENSION(:)  :: stnameO

    !--- all this for writing the station id string
     INTEGER   TDIMS, TXLEN
     PARAMETER (TDIMS=2)    ! number of TX dimensions
     PARAMETER (TXLEN = 11) ! length of example string
     INTEGER  TIMEID        ! record dimension id
     INTEGER  TXID          ! variable ID
     INTEGER  TXDIMS(TDIMS) ! variable shape
     INTEGER  TSTART(TDIMS), TCOUNT(TDIMS)

     !--  observation point  ids
     INTEGER   OTDIMS, OTXLEN
     PARAMETER (OTDIMS=2)    ! number of TX dimensions
     PARAMETER (OTXLEN = 11) ! length of example string
     INTEGER  OTIMEID        ! record dimension id
     INTEGER  OTXID          ! variable ID
     INTEGER  OTXDIMS(OTDIMS) ! variable shape
     INTEGER  OTSTART(OTDIMS), OTCOUNT(OTDIMS)

#ifdef HYDRO_D
     write(6,*) "yyww dtrt =", dtrt , "k =", k
#endif
   
     seconds_since = int(dtrt)*K

!    order_to_write = 2  !-- 1 all; 6 feweest

      nstations = 0  ! total number of channel points to display
      nobs      = 0  ! number of observation points

!-- output only the higher oder streamflows  and only observation points
       do i=1,NLINKS
        if(ORDER(i) .ge. order_to_write) then 
         nstations = nstations + 1
        endif
        if(STRMFRXSTPTS(i) .ne. -9999) then 
         nobs = nobs + 1
        endif
       enddo 

       if (nobs .eq. 0) then ! let's at least make one obs point
         nobs = 1
         STRMFRXSTPTS(1) = 1
       endif

       allocate(chanlat(nstations))
       allocate(chanlon(nstations))
       allocate(elevation(nstations))
       allocate(station_id(nstations))
       allocate(lOrder(nstations))
       allocate(rec_num_of_station(nstations))
       allocate(stname(nstations))

       allocate(chanlatO(nobs))
       allocate(chanlonO(nobs))
       allocate(elevationO(nobs))
       allocate(station_idO(nobs))
       allocate(lOrderO(nobs))
       allocate(rec_num_of_stationO(nobs))
       allocate(stnameO(nobs))

       if(output_count == 0) then 
!-- have moved sec_since_date from above here..
        sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

        date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

        nstations = 0
        nobs = 0

        write(output_flnm, '(A12,".CHRTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
        write(output_flnm2,'(A12,".CHANOBS_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid

#ifdef HYDRO_D
        print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

        iret = nf_create(trim(output_flnm), 0, ncid)
#ifdef HYDRO_D
        if (iret /= 0) then
           print*,  "Problem nf_create points"
           call hydro_stop()
        endif
#endif

        iret = nf_create(trim(output_flnm2), 0, ncid2)
#ifdef HYDRO_D
        if (iret /= 0) then
            print*, "Problem nf_create observation"
            call hydro_stop()
        endif
#endif

       do i=1,NLINKS
        if(ORDER(i) .ge. order_to_write) then 
         nstations = nstations + 1
         chanlat(nstations) = chlat(i)
         chanlon(nstations) = chlon(i)
         elevation(nstations) = zelev(i)
         lOrder(nstations) = ORDER(i)
         station_id(nstations) = i
         if(STRMFRXSTPTS(nstations) .eq. -9999) then 
           ObsStation = 0
         else 
           ObsStation = 1
         endif
         write(stname(nstations),'(I6,"_",I1,"_S",I1)') nstations,lOrder(nstations),ObsStation
        endif
       enddo 


       do i=1,NLINKS
        if(STRMFRXSTPTS(i) .ne. -9999) then 
         nobs = nobs + 1
         chanlatO(nobs) = chlat(i)
         chanlonO(nobs) = chlon(i)
         elevationO(nobs) = zelev(i)
         lOrderO(nobs) = ORDER(i)
         station_idO(nobs) = i
         write(stnameO(nobs),'(I6,"_",I1)') nobs,lOrderO(nobs)
#ifdef HYDRO_D
         print *,"stationobservation name",  stnameO(nobs)
#endif
        endif
       enddo 

       iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach


       iret = nf_def_dim(ncid, "station", nstations, stationdim)



       iret = nf_def_dim(ncid2, "recNum", NF_UNLIMITED, dimdataO)  !--for linked list approach
       iret = nf_def_dim(ncid2, "station", nobs, obsdim)


      !- station location definition all,  lat
        iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
#ifdef HYDRO_D
       write(6,*) "iret 2.1,  ", iret, stationdim
#endif
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station latitude')
#ifdef HYDRO_D
       write(6,*) "iret 2.2", iret
#endif
        iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')
#ifdef HYDRO_D
       write(6,*) "iret 2.3", iret
#endif


      !- station location definition obs,  lat
        iret = nf_def_var(ncid2,"latitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',20,'Observation latitude')
        iret = nf_put_att_text(ncid2,varid,'units',13,'degrees_north')


      !- station location definition,  long
        iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',17,'Station longitude')
        iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')


      !- station location definition, obs long
        iret = nf_def_var(ncid2,"longitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',21,'Observation longitude')
        iret = nf_put_att_text(ncid2,varid,'units',12,'degrees_east')


!     !-- elevation is ZELEV
        iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station altitude')
        iret = nf_put_att_text(ncid,varid,'units',6,'meters')


!     !-- elevation is obs ZELEV
        iret = nf_def_var(ncid2,"altitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',20,'Observation altitude')
        iret = nf_put_att_text(ncid2,varid,'units',6,'meters')


!     !--  gage observation
!       iret = nf_def_var(ncid,"gages",NF_FLOAT, 1, (/stationdim/), varid)
!       iret = nf_put_att_text(ncid,varid,'long_name',20,'Stream Gage Location')
!       iret = nf_put_att_text(ncid,varid,'units',4,'none')

!-- parent index
        iret = nf_def_var(ncid,"parent_index",NF_INT,1,(/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',36,'index of the station for this record')

        iret = nf_def_var(ncid2,"parent_index",NF_INT,1,(/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',36,'index of the station for this record')

     !-- prevChild
        iret = nf_def_var(ncid,"prevChild",NF_INT,1,(/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',57,'record number of the previous record for the same station')
!ywtmp        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

        iret = nf_def_var(ncid2,"prevChild",NF_INT,1,(/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',57,'record number of the previous record for the same station')
!ywtmp        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid2,varid,'_FillValue',2,-1)

     !-- lastChild
        iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',30,'latest report for this station')
!ywtmp        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

        iret = nf_def_var(ncid2,"lastChild",NF_INT,1,(/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',30,'latest report for this station')
!ywtmp        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid2,varid,'_FillValue',2,-1)

!     !- flow definition, var
        iret = nf_def_var(ncid, "streamflow", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',10,'River Flow')

        iret = nf_def_var(ncid2, "streamflow", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',10,'River Flow')

!     !- flow definition, var
!       iret = nf_def_var(ncid, "pos_streamflow", NF_FLOAT, 1, (/dimdata/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'abs streamflow')

!     !- head definition, var
        iret = nf_def_var(ncid, "head", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid,varid,'long_name',11,'River Stage')

        iret = nf_def_var(ncid2, "head", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid2,varid,'long_name',11,'River Stage')


!     !- order definition, var
        iret = nf_def_var(ncid, "order", NF_INT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',21,'Strahler Stream Order')
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

        iret = nf_def_var(ncid2, "order", NF_INT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',21,'Strahler Stream Order')
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)


     !-- station  id
     ! define character-position dimension for strings of max length 11
         iret = NF_DEF_DIM(ncid, "id_len", 11, charid)
         TXDIMS(1) = charid   ! define char-string variable and position dimension first
         TXDIMS(2) = stationdim
         iret = nf_def_var(ncid,"station_id",NF_CHAR, TDIMS, TXDIMS, varid)
         iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')

         iret = NF_DEF_DIM(ncid2, "id_len", 11, charidO)
         OTXDIMS(1) = charidO   ! define char-string variable and position dimension first
         OTXDIMS(2) = obsdim
         iret = nf_def_var(ncid2,"station_id",NF_CHAR, OTDIMS, OTXDIMS, varid)
         iret = nf_put_att_text(ncid2,varid,'long_name',14,'Observation id')


!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time_observation",NF_INT, 1, (/dimdata/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid,varid,'long_name',19,'time of observation')

         iret = nf_def_var(ncid2,"time_observation",NF_INT, 1, (/dimdataO/), varid)
         iret = nf_put_att_text(ncid2,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid2,varid,'long_name',19,'time of observation')

         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)

         convention(1:32) = "Unidata Observation Dataset v1.0"
         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19start)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "stationDimension",7, "station")
         iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid, NF_GLOBAL, "stream order output",NF_INT,1,order_to_write)

         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "cdm_datatype",7, "Station")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "time_coverage_start",19, date19start)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "stationDimension",7, "station")
         iret = nf_put_att_real(ncid2, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid2, NF_GLOBAL, "stream order output",NF_INT,1,order_to_write)

         iret = nf_enddef(ncid)
         iret = nf_enddef(ncid2)
        
        !-- write latitudes
         iret = nf_inq_varid(ncid,"latitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chanlat)

         iret = nf_inq_varid(ncid2,"latitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), chanlatO)

        !-- write longitudes
         iret = nf_inq_varid(ncid,"longitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chanlon)

         iret = nf_inq_varid(ncid2,"longitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), chanlonO)

        !-- write elevations
         iret = nf_inq_varid(ncid,"altitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), elevation)

         iret = nf_inq_varid(ncid2,"altitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), elevationO)

      !-- write gage location
!      iret = nf_inq_varid(ncid,"gages", varid)
!      iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), STRMFRXSTPTS)

        !-- write number_of_stations, OPTIONAL
      !!  iret = nf_inq_varid(ncid,"number_stations", varid)
      !!  iret = nf_put_var_int(ncid, varid, nstations)

        !-- write station id's 
         do i=1,nstations
          TSTART(1) = 1
          TSTART(2) = i
          TCOUNT(1) = TXLEN
          TCOUNT(2) = 1
          iret = nf_inq_varid(ncid,"station_id", varid)
          iret = nf_put_vara_text(ncid, varid, TSTART, TCOUNT, stname(i))
         enddo

        !-- write observation id's 
         do i=1, nobs
          OTSTART(1) = 1
          OTSTART(2) = i
          OTCOUNT(1) = OTXLEN
          OTCOUNT(2) = 1
          iret = nf_inq_varid(ncid2,"station_id", varid)
          iret = nf_put_vara_text(ncid2, varid, OTSTART, OTCOUNT, stnameO(i))
         enddo

     endif

     output_count = output_count + 1

     open (unit=999,file='frxst_pts_out.txt',status='unknown',position='append')

     cnt=0
     do i=1,NLINKS

       if(ORDER(i) .ge. order_to_write) then 
         start_pos = (cnt+1)+(nstations*(output_count-1))

         !!--time in seconds since startdate
          iret = nf_inq_varid(ncid,"time_observation", varid)
          iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid,"streamflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlink(i,1))

!        iret = nf_inq_varid(ncid,"pos_streamflow", varid)
!        iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), abs(qlink(i,1)))

         iret = nf_inq_varid(ncid,"head", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), hlink(i))

         iret = nf_inq_varid(ncid,"order", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), ORDER(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid,"parent_index", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), cnt)

          !--record number of previous record for same station
!obsolete format         prev_pos = cnt+(nstations*(output_count-1))
         prev_pos = cnt+(nobs*(output_count-2))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid,"prevChild", varid)
           iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), prev_pos)
         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_station(cnt) = start_pos-1  !-- save position for last child, 0-based!!


       endif
    enddo
!    close(999) 

    !-- output  only observation points
    cnt=0
    do i=1,NLINKS

      if(STRMFRXSTPTS(i) .ne. -9999) then 
#ifdef HYDRO_D
         print *, "Outputting frxst pt. :",STRMFRXSTPTS(i)
#endif
         start_posO = (cnt+1)+(nobs * (output_count-1))

!Write frxst_pts to text file...
!yw          write(999,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
           write(999,117) seconds_since,date(1:10),date(12:19),cnt,chlon(i),chlat(i), &
                    abs(qlink(i,1)), abs(qlink(i,1))*35.315,hlink(i)
!yw 117 FORMAT(I8,1X,A25,1X,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)
117 FORMAT(I8,1X,A10,1X,A8,1x,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)

         !!--time in seconds since startdate
         iret = nf_inq_varid(ncid2,"time_observation", varid)
         iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid2,"streamflow", varid)
         iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), qlink(i,1))

         iret = nf_inq_varid(ncid2,"head", varid)
         iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), hlink(i))

         iret = nf_inq_varid(ncid,"order", varid)
         iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), ORDER(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid2,"parent_index", varid)
         iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), cnt)

          !--record number of previous record for same station
!obsolete format          prev_posO = cnt+(nobs*(output_count-1))
         prev_posO = cnt+(nobs*(output_count-2))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid2,"prevChild", varid)
           iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), prev_posO)

!IF block to add -1 to last element of prevChild array to designate end of list...
!           if(cnt+1.eq.nobs.AND.output_count.eq.split_output_count) then
!             iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), -1)
!           else
!             iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), prev_posO)
!           endif 


         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_stationO(cnt) = start_posO - 1  !-- save position for last child, 0-based!!


      endif

    enddo
    close(999) 


      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid,"lastChild", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), rec_num_of_station)

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid2,"lastChild", varid)
      iret = nf_put_vara_int(ncid2, varid, (/1/), (/nobs/), rec_num_of_stationO)

      iret = nf_redef(ncid)
      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(date)) = date
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)

      iret = nf_redef(ncid2)
      iret = nf_put_att_text(ncid2, NF_GLOBAL, "time_coverage_end", 19, date19)

      iret = nf_enddef(ncid)
      iret = nf_sync(ncid)

      iret = nf_enddef(ncid2)
      iret = nf_sync(ncid2)

      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
        iret = nf_close(ncid2)
     endif

     deallocate(chanlat)
     deallocate(chanlon)
     deallocate(elevation)
     deallocate(station_id)
     deallocate(lOrder)
     deallocate(rec_num_of_station)
     deallocate(stname)

     deallocate(chanlatO)
     deallocate(chanlonO)
     deallocate(elevationO)
     deallocate(station_idO)
     deallocate(lOrderO)
     deallocate(rec_num_of_stationO)
     deallocate(stnameO)
#ifdef HYDRO_D
     print *, "Exited Subroutine output_chrt"
#endif
     close(16)

20 format(i8,',',f12.7,',',f10.7,',',f6.2,',',i3)

end subroutine output_chrt

#ifdef MPP_LAND
!-- output the channel route in an IDV 'station' compatible format
   subroutine mpp_output_chrt(mpp_nlinks,nlinks_index,igrid, &
        split_output_count, NLINKS, ORDER, &
        startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt, &
        K,STRMFRXSTPTS,order_to_write)

       USE module_mpp_land

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(NLINKS),               intent(in) :: chlon,chlat
     real, dimension(NLINKS),                  intent(in) :: hlink,zelev

     integer, dimension(NLINKS),               intent(in) :: ORDER
     integer, dimension(NLINKS),               intent(inout) :: STRMFRXSTPTS

     real,                                     intent(in) :: dtrt
     real, dimension(NLINKS,2),                intent(in) :: qlink

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     integer  :: mpp_nlinks, nlinks_index(nlinks), order_to_write


     call write_chanel_int(order,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(chlon,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(chlat,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(hlink,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(zelev,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,1),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,2),nlinks_index,mpp_nlinks,nlinks)

     if(my_id .eq. IO_id) then
       call output_chrt(igrid, split_output_count, NLINKS, ORDER, &
          startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt,K,&
          STRMFRXSTPTS,order_to_write)
    end if

end subroutine mpp_output_chrt

!---------  lake netcdf output -----------------------------------------
!-- output the ilake info an IDV 'station' compatible format -----------
   subroutine mpp_output_lakes(lake_index,igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt,K)
#ifdef MPP_LAND
   USE module_mpp_land
#endif

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     integer lake_index(nlakes)

     call write_lake_real(latlake,lake_index,nlakes)
     call write_lake_real(lonlake,lake_index,nlakes)
     call write_lake_real(elevlake,lake_index,nlakes)
     call write_lake_real(resht,lake_index,nlakes)
     call write_lake_real(qlakei,lake_index,nlakes)
     call write_lake_real(qlakeo,lake_index,nlakes)
     if(my_id.eq. IO_id) then
        call output_lakes(igrid, split_output_count, NLAKES, &
           startdate, date, latlake, lonlake, elevlake, &
           qlakei,qlakeo, resht,dtrt,K)
     end if
     return
     end subroutine mpp_output_lakes

#endif

!----------------------------------- lake netcdf output
!-- output the ilake info an IDV 'station' compatible format
   subroutine output_lakes(igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt,K)

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

     integer, allocatable, DIMENSION(:)                   :: station_id
     integer, allocatable, DIMENSION(:)                   :: rec_num_of_lake

     integer, save  :: output_count
     integer, save :: ncid

     integer :: stationdim, dimdata, varid, charid, n
     integer :: iret,i, start_pos, prev_pos  !-- 
     integer :: previous_pos        !-- used for the station model
     character(len=256) :: output_flnm
     character(len=19)  :: date19, date19start
     character(len=34)  :: sec_since_date
     integer :: seconds_since,cnt
     character(len=32)  :: convention
     character(len=6),allocatable, DIMENSION(:)  :: stname

    !--- all this for writing the station id string
     INTEGER   TDIMS, TXLEN
     PARAMETER (TDIMS=2)    ! number of TX dimensions
     PARAMETER (TXLEN = 6) ! length of example string
     INTEGER  TIMEID        ! record dimension id
     INTEGER  TXID          ! variable ID
     INTEGER  TXDIMS(TDIMS) ! variable shape
     INTEGER  TSTART(TDIMS), TCOUNT(TDIMS)

!    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
!    seconds_since = int(dtrt)*output_count
     seconds_since = int(dtrt)*K

     allocate(station_id(NLAKES))
     allocate(rec_num_of_lake(NLAKES))
     allocate(stname(NLAKES))

     if (output_count == 0) then

!-- have moved sec_since_date from above here..
      sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

      date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

      write(output_flnm, '(A12,".LAKEOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
      print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

      iret = nf_create(trim(output_flnm), 0, ncid)
#ifdef HYDRO_D
      if (iret /= 0) then
          print*, "Problem nf_create"
          call hydro_stop()
      endif
#endif

      do i=1,NLAKES
         station_id(i) = i
         write(stname(i),'(I6)') i
      enddo 

      iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach
      iret = nf_def_dim(ncid, "station", nlakes, stationdim)

      !- station location definition,  lat
      iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake latitude')
      iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')

      !- station location definition,  long
      iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake longitude')
      iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')

!     !-- lake's phyical elevation
      iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake altitude')
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')

     !-- parent index
      iret = nf_def_var(ncid,"parent_index",NF_INT,1,(/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',33,'index of the lake for this record')

     !-- prevChild
      iret = nf_def_var(ncid,"prevChild",NF_INT,1,(/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',54,'record number of the previous record for the same lake')
!ywtmp      iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
      iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

     !-- lastChild
      iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',27,'latest report for this lake')
!ywtmp      iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
      iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

!     !- water surface elevation
      iret = nf_def_var(ncid, "elevation", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake Elevation')

!     !- inflow to lake
      iret = nf_def_var(ncid, "inflow", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

!     !- outflow to lake
      iret = nf_def_var(ncid, "outflow", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

     !-- station  id
     ! define character-position dimension for strings of max length 6
         iret = NF_DEF_DIM(ncid, "id_len", 6, charid)
         TXDIMS(1) = charid   ! define char-string variable and position dimension first
         TXDIMS(2) = stationdim
         iret = nf_def_var(ncid,"station_id",NF_CHAR, TDIMS, TXDIMS, varid)
         iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')

!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time_observation",NF_INT, 1, (/dimdata/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid,varid,'long_name',19,'time of observation')

!       date19(1:19) = "0000-00-00_00:00:00"
!       date19(1:len_trim(startdate)) = startdate
!       iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
!
        date19(1:19) = "0000-00-00_00:00:00"
        date19(1:len_trim(startdate)) = startdate
        convention(1:32) = "Unidata Observation Dataset v1.0"
        iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
        iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19start)
        iret = nf_put_att_text(ncid, NF_GLOBAL, "stationDimension",7, "station")
!!       iret = nf_put_att_text(ncid, NF_GLOBAL, "observationDimension",6, "recNum")
!!        iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coordinate",16,"time_observation")
        iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
        iret = nf_enddef(ncid)

        !-- write latitudes
        iret = nf_inq_varid(ncid,"latitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LATLAKE)

        !-- write longitudes
        iret = nf_inq_varid(ncid,"longitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LONLAKE)

        !-- write physical height of lake
        iret = nf_inq_varid(ncid,"altitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), elevlake)

        !-- write station id's 
         do i=1,nlakes
          TSTART(1) = 1
          TSTART(2) = i
          TCOUNT(1) = TXLEN
          TCOUNT(2) = 1
          iret = nf_inq_varid(ncid,"station_id", varid)
          iret = nf_put_vara_text(ncid, varid, TSTART, TCOUNT, stname(i))
         enddo

     endif

     output_count = output_count + 1

     cnt=0
     do i=1,NLAKES

         start_pos = (cnt+1)+(nlakes*(output_count-1))

         !!--time in seconds since startdate
         iret = nf_inq_varid(ncid,"time_observation", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid,"elevation", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), resht(i))

         iret = nf_inq_varid(ncid,"inflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlakei(i))

         iret = nf_inq_varid(ncid,"outflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlakeo(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid,"parent_index", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), cnt)

          !--record number of previous record for same station
         prev_pos = cnt+(nlakes*(output_count-1))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid,"prevChild", varid)
           iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), prev_pos)
         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_lake(cnt) = start_pos-1  !-- save position for last child, 0-based!!

    enddo

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid,"lastChild", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/nlakes/), rec_num_of_lake)

     !-- number of children reported for this station, OPTIONAL
     !--  iret = nf_inq_varid(ncid,"numChildren", varid)
     !--  iret = nf_put_vara_int(ncid, varid, (/1/), (/nlakes/), rec_num_of_lake)

    iret = nf_redef(ncid)
    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(date)) = date
    iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
    iret = nf_enddef(ncid)

    iret = nf_sync(ncid)
     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

     deallocate(station_id)
     deallocate(rec_num_of_lake)
     deallocate(stname)
#ifdef HYDRO_D
     print *, "Exited Subroutine output_lakes"
#endif
     close(16)

 end subroutine output_lakes
!----------------------------------- lake netcdf output

#ifdef MPP_LAND

!-- output the channel route in an IDV 'grid' compatible format
   subroutine mpp_output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT_in, CH_NETLNK_in, ORDER, startdate, date, &
        qlink, dt, geo_finegrid_flnm, mpp_nlinks,nlinks_index,g_ixrt,g_jxrt )
#ifdef MPP_LAND
   USE module_mpp_land
#endif
     implicit none
#include <netcdf.inc>
     integer g_ixrt,g_jxrt
     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(NLINKS,2),                intent(in) :: qlink
     integer, dimension(g_IXRT,g_JXRT)         :: CH_NETRT,CH_NETLNK
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETRT_in,CH_NETLNK_in
     integer, dimension(NLINKS),               intent(in) :: ORDER !--currently not used here, see finegrid.f
     character(len=*),          intent(in)     :: geo_finegrid_flnm
     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
    
     integer::  mpp_nlinks , nlinks_index(nlinks)
     call write_chanel_real(qlink(:,1),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,2),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_int(order,nlinks_index,mpp_nlinks,nlinks)
     call write_IO_rt_int(CH_NETRT_in, CH_NETRT)
     call write_IO_rt_int(CH_NETLNK_in, CH_NETLNK)

     call  output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT, CH_NETLNK, ORDER, startdate, date, &
        qlink, dt, geo_finegrid_flnm)
  
     return
     end subroutine mpp_output_chrtgrd
#endif

!-- output the channel route in an IDV 'grid' compatible format
   subroutine output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT, CH_NETLNK, ORDER, startdate, date, &
        qlink, dt, geo_finegrid_flnm)

     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(NLINKS,2),                intent(in) :: qlink
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETRT,CH_NETLNK
     integer, dimension(NLINKS),               intent(in) :: ORDER !--currently not used here, see finegrid.f
     character(len=*),          intent(in)     :: geo_finegrid_flnm
     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     character(len=32)  :: convention
     integer,save  :: output_count
     integer, save :: ncid,ncstatic
     real, dimension(IXRT,JXRT)          :: tmpflow
     real, dimension(IXRT)            :: xcoord
     real, dimension(JXRT)            :: ycoord
     real                                :: long_cm,lat_po,fe,fn
     real, dimension(2)                  :: sp

    integer :: varid, n
    integer :: jxlatdim,ixlondim,timedim !-- dimension ids

    integer :: iret,i,j
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=34)  :: sec_since_date
 

    integer :: seconds_since
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = dt*output_count

    !-- Open the  finemesh static files to obtain projection information
#ifdef HYDRO_D
      write(*,'("geo_finegrid_flnm: ''", A, "''")') trim(geo_finegrid_flnm)
#endif
      iret = nf_open(geo_finegrid_flnm, NF_NOWRITE, ncstatic)

      if (iret /= 0) then
#ifdef HYDRO_D
         write(*,'("Problem opening geo_finegrid file: ''", A, "''")') &
         trim(geo_finegrid_flnm)
         call hydro_stop()
#endif
      endif

      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord) 
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord) 

      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)  
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file

      tmpflow = -9E15

      if (output_count == 0) then
 
        write(output_flnm, '(A12,".CHRTOUT_GRID",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
        print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif
 
!--- define dimension
        iret = nf_create(trim(output_flnm), 0, ncid)
#ifdef HYDRO_D
        if (iret /= 0) then
            print*, "Problem nf_create"
            call hydro_stop()
        endif
#endif

        iret = nf_def_dim(ncid, "time", NF_UNLIMITED, timedim)
        iret = nf_def_dim(ncid, "x", ixrt, ixlondim)
        iret = nf_def_dim(ncid, "y", jxrt, jxlatdim)

!--- define variables
!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time",NF_INT, 1, (/timedim/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
          iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/ixlondim/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
          iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/jxlatdim/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

!     !- flow definition, var
        iret = nf_def_var(ncid,"flow",NF_REAL, 3, (/ixlondim,jxlatdim,timedim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
        iret = nf_put_att_text(ncid,varid,'long_name',15,'water flow rate')
        iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
        iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)


!-- place prjection information
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_DOUBLE,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_DOUBLE,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_DOUBLE,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_DOUBLE,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_DOUBLE,2,sp)


      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
        iret = nf_inq_varid(ncid,"y", varid)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrt/), ycoord) !-- 1-d array

        iret = nf_inq_varid(ncid,"x", varid)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrt/), xcoord) !-- 1-d array

      iret = nf_close(ncstatic)

     endif

    output_count = output_count + 1

!going to output a group at a given time

!DJG inv    do j=jxrt,1,-1
    do j=1,jxrt
     do i=1,ixrt
       if(CH_NETRT(i,j).GE.0) then
         tmpflow(i,j) = qlink(CH_NETLNK(i,j),1) 
       else
         tmpflow(i,j) = -9E15
       endif
     enddo
    enddo

!!time in seconds since startdate
    iret = nf_inq_varid(ncid,"time", varid)
    iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

    iret = nf_inq_varid(ncid,"flow", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrt,jxrt,1/),tmpflow)

     iret = nf_redef(ncid)
     date19(1:19) = "0000-00-00_00:00:00"
     date19(1:len_trim(date)) = date
     iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
     iret = nf_enddef(ncid)

     iret = nf_sync(ncid)

     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

 end subroutine output_chrtgrd



#ifdef MPP_LAND
  subroutine mpp_output_rt(ixrt, jxrt,igrid, split_output_count, &
       ixrt_in, jxrt_in,nsoil, startdate, olddate, &
       QSUBRT_in,ZWATTABLRT_in,SMCRT_in,SUB_RESID_in,       &
       q_sfcflx_x_in,q_sfcflx_y_in,soxrt_in,soyrt_in,       &
       QSTRMVOLRT_in,SFCHEADSUBRT_in, &
       geo_finegrid_flnm,dt,sldpth,LATVAL_in,LONVAL_in,dist,HIRES_OUT, &
       QBDRYRT_in)

!output the routing variables over routing grid.
#ifdef MPP_LAND
   USE module_mpp_land
#endif
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count

! ixrt and jxrt are global. ixrt_in and jxrt_in are local array index.
    integer,                                  intent(in) :: ixrt,jxrt,ixrt_in,jxrt_in
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt_in,jxrt_in,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: olddate
    character(len=*),          intent(in)                :: geo_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth

    real, dimension(ixrt_in,jxrt_in) :: QSUBRT_in,ZWATTABLRT_in,SUB_RESID_in
    real, dimension(ixrt_in,jxrt_in) :: q_sfcflx_x_in,q_sfcflx_y_in
    real, dimension(ixrt_in,jxrt_in) :: QSTRMVOLRT_in
    real, dimension(ixrt_in,jxrt_in) :: SFCHEADSUBRT_in, QBDRYRT_in
    real, dimension(ixrt_in,jxrt_in) :: soxrt_in,soyrt_in
    real, dimension(ixrt_in,jxrt_in,nsoil) :: SMCRT_in
    real, dimension(ixrt_in,jxrt_in) :: LATVAL_in,LONVAL_in

    real, dimension(ixrt,jxrt) :: QSUBRT,ZWATTABLRT,SUB_RESID
    real, dimension(ixrt,jxrt) :: q_sfcflx_x,q_sfcflx_y
    real, dimension(ixrt,jxrt) :: QSTRMVOLRT, QBDRYRT
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: soxrt,soyrt
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT
    real, dimension(ixrt,jxrt,9) :: dist_g
    real, dimension(ixrt,jxrt) :: LATVAL,LONVAL
    integer i


#ifdef HYDRO_D
    write(6,*) "mpp_output_RT output file: ",trim(geo_finegrid_flnm)
#endif

    call write_IO_rt_real(LATVAL_in,LATVAL)
    call write_IO_rt_real(LONVAL_in,LONVAL)
    call write_IO_rt_real(QSUBRT_in,QSUBRT)
   

    call write_IO_rt_real(ZWATTABLRT_in,ZWATTABLRT)


    call write_IO_rt_real(SUB_RESID_in,SUB_RESID)


    call write_IO_rt_real(QSTRMVOLRT_in,QSTRMVOLRT)



    call write_IO_rt_real(SFCHEADSUBRT_in,SFCHEADSUBRT)
    call write_IO_rt_real(soxrt_in,soxrt)

    call write_IO_rt_real(QBDRYRT_in,QBDRYRT)



    call write_IO_rt_real(soyrt_in,soyrt)
    call write_IO_rt_real(q_sfcflx_x_in,q_sfcflx_x)
    call write_IO_rt_real(q_sfcflx_y_in,q_sfcflx_y)




    do i = 1, NSOIL
         call write_IO_rt_real(SMCRT_in(:,:,i),SMCRT(:,:,i))
    end do
    do i = 1, 9    
         call write_IO_rt_real(dist(:,:,i),dist_g(:,:,i))
    end do

!   yyywwww  ! temp test
!   if(my_id.eq. IO_id  ) write(14,*) dist(:,:,9)
!   if(my_id.eq. IO_id  ) write(12,*) dist_g(:,:,9)




    if(my_id.eq.IO_id) then
       call output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
          startdate, olddate, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
          q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
          geo_finegrid_flnm,DT,SLDPTH,latval,lonval,dist_g,HIRES_OUT, &
          QBDRYRT)
    end if

#ifdef HYDRO_D
    write(6,*) "return from mpp_output_RT"
#endif
  end subroutine mpp_output_rt

#endif

 subroutine read_chan_forcing( &
       indir,olddate,startdate,hgrid,&
       ixrt,jxrt,QSTRMVOLRT_ACC,QINFLOWBASE,QSUBRT)
! This subrouting is going to read channel forcing for
!  channel only simulations (ie when CHANRTSWCRT = 2)

   implicit none
#include <netcdf.inc>
   ! in variable
   character(len=*) :: olddate,hgrid,indir,startdate
   character(len=256) :: filename
   integer :: ixrt,jxrt
   real,dimension(ixrt,jxrt):: QSTRMVOLRT_ACC,QINFLOWBASE,QSUBRT
   ! tmp variable
   character(len=256) :: inflnm, product
   integer  :: i,j,mmflag
   character(len=256) :: units
   integer :: ierr
   integer :: ncid


!DJG Create filename...
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             olddate(15:16)//".RTOUT_DOMAIN"//hgrid
#ifdef HYDRO_D
        print *, "Channel forcing file...",inflnm
#endif


!DJG Open NetCDF file...
    ierr = nf_open(inflnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
#ifdef HYDRO_D
       write(*,'("READFORC_chan Problem opening netcdf file: ''", A, "''")') trim(inflnm)
       call hydro_stop()
#endif
    endif

!DJG read data...
    call get_2d_netcdf("QSTRMVOLRT",  ncid, QSTRMVOLRT_ACC, units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)

    ierr = nf_close(ncid)

 end subroutine read_chan_forcing




      subroutine get2d_int(var_name,out_buff,ix,jx,fileName)
          implicit none
#include <netcdf.inc>
          integer :: iret,varid,ncid,ix,jx
          integer out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
#ifdef HYDRO_D
            print*,"aaa failed to open the netcdf file: ",trim(fileName)
            call hydro_stop()
#endif
          endif
          iret = nf_inq_varid(ncid,trim(var_name),  varid)
          if(iret .ne. 0) then
#ifdef HYDRO_D
            print*,"failed to read the variabe: ",trim(var_name)
            print*,"failed to read the netcdf file: ",trim(fileName)
#endif
          endif
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
         return
      end subroutine get2d_int

#ifdef MPP_LAND
       SUBROUTINE MPP_READ_ROUTEDIM(g_IXRT,g_JXRT, IXRT,JXRT, &
            route_chan_f,route_link_f, &
            route_direction_f, route_lake_f, NLINKS, NLAKES, &
            CH_NETLNK, channel_option, geo_finegrid_flnm)


         USE module_mpp_land

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: channel_option
        INTEGER                                      :: g_IXRT,g_JXRT
        INTEGER, INTENT(INOUT)                       :: NLINKS, NLAKES
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, DIMENSION(g_IXRT,g_JXRT) :: g_CH_NETLNK  ! temp array
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON


        CHARACTER(len=256)       :: route_chan_f, route_link_f,route_direction_f,route_lake_f
        CHARACTER(len=256)       :: geo_finegrid_flnm
!       CHARACTER(len=*)       :: geo_finegrid_flnm

        if(my_id .eq. IO_id) then
          CALL READ_ROUTEDIM(g_IXRT, g_JXRT, route_chan_f, route_link_f, &
              route_direction_f, route_lake_f, NLINKS, NLAKES, &
              g_CH_NETLNK, channel_option,geo_finegrid_flnm)
        endif
        

        call mpp_land_bcast_int1(NLAKES)
        call mpp_land_bcast_int1(NLINKS)


        call decompose_RT_int(g_CH_NETLNK,CH_NETLNK,g_IXRT,g_JXRT,ixrt,jxrt)
       

        return 
        end SUBROUTINE MPP_READ_ROUTEDIM

      SUBROUTINE MPP_READ_ROUTING(IXRT,JXRT,ELRT,  &
              CH_NETRT,LKSATFAC,route_topo_f,    &
            route_chan_f, geo_finegrid_flnm,g_IXRT,g_JXRT, &
            OVROUGHRTFAC,RETDEPRTFAC)

        implicit none
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT,g_IXRT,g_JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: ELRT,LKSATFAC
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: OVROUGHRTFAC,RETDEPRTFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT

        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_ELRT
        INTEGER,DIMENSION(g_IXRT,g_JXRT)             :: g1_CH_NETRT
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_LKSATFAC
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_OVROUGHRTFAC
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_RETDEPRTFAC

        CHARACTER(len=256)                           :: route_topo_f,route_chan_f,geo_finegrid_flnm

        if(my_id .eq. IO_id)  then
          CALL READ_ROUTING_seq(g_IXRT,g_JXRT,g1_ELRT,g1_CH_NETRT,g1_LKSATFAC,&
              route_topo_f, route_chan_f,geo_finegrid_flnm,g1_OVROUGHRTFAC,&
               g1_RETDEPRTFAC)
        endif

        call decompose_RT_real(g1_ELRT,ELRT,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_int(g1_CH_NETRT,CH_NETRT,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_LKSATFAC,LKSATFAC,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_RETDEPRTFAC,RETDEPRTFAC,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_OVROUGHRTFAC,OVROUGHRTFAC,g_IXRT,g_JXRT,IXRT,JXRT)

       return
       end SUBROUTINE MPP_READ_ROUTING


      subroutine MPP_DEEPGW_HRLDAS(ix,jx,in_SMCMAX,&
                 global_nX, global_ny,nsoil,out_SMC,out_SH2OX)
        implicit none
#include <netcdf.inc>

        integer,  intent(in)  :: ix,global_nx,global_ny
        integer,  intent(in)  :: jx,nsoil
        real,             dimension(ix,jx), intent(in) :: in_smcmax
        real,             dimension(ix,jx,nsoil), intent(out) :: out_smc,out_sh2ox

        real,dimension(global_nX, global_ny,nsoil):: g_smc, g_sh2ox
        real,dimension(global_nX, global_ny):: g_smcmax
        integer   :: i,j,k
       

          call write_IO_real(in_smcmax,g_smcmax)  ! get global grid of smcmax

          write (*,*) "In deep GW...", nsoil

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


       SUBROUTINE MPP_READ_CHROUTING(IXRT,JXRT,ELRT,CH_NETRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,LATVAL,&
            LONVAL,STRMFRXSTPTS,geo_finegrid_flnm,g_ixrt,g_jxrt)
        implicit none
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT,g_IXRT,g_JXRT
!yw        INTEGER, INTENT(IN)                       :: CHANRTSWCRT, NLINKS, NLAKES
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES
        INTEGER                                      :: I,J,channel_option
        REAL, DIMENSION(g_IXRT,g_JXRT) :: g1_LATVAL, g1_LONVAL
        CHARACTER(len=28)                            :: dir

!----DJG,DNY New variables for channel and lake routing
        CHARACTER(len=155)	 :: header
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ZELEV
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHLAT,CHLON

        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TYPEL
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TO_NODE,ORDER
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: STRMFRXSTPTS

        INTEGER, INTENT(INOUT)                       :: MAXORDER
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MUSK, MUSX !muskingum
        REAL, INTENT(INOUT),  DIMENSION(NLINKS,2)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE  ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)


!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: HRZAREA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LAKEMAXH
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(NLINKS)       :: ChSSlp, Bw

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: geo_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid
        real                                         :: gc,n

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: ELRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: latval,lonval
        real      g1_elrt(g_ixrt,g_jxrt), g_dist(g_ixrt,g_jxrt,9)
        integer   g1_ch_netrt(g_ixrt,g_jxrt)
        INTEGER, DIMENSION(g_IXRT,g_JXRT)            :: g1_LAKE_MSKRT, g1_ch_netlnk
        integer :: k


        call write_IO_rt_real(elrt,g1_elrt)
        call write_IO_rt_int(ch_netrt,g1_ch_netrt)
        call write_IO_rt_int(CH_NETLNK,g1_CH_NETLNK)
!       if(dist(1,1,1) .ne. -999) then
            do k = 1, 9
                call write_IO_rt_real(dist(:,:,k),g_dist(:,:,k))
            end do
!       endif

        if(my_id .eq. IO_id) then
          CALL READ_CHROUTING(g_IXRT,g_JXRT,g1_ELRT,g1_CH_NETRT, g1_LAKE_MSKRT, &
               FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
               NLAKES, MUSK, MUSX, QLINK,CHANLEN, MannN, So, ChSSlp, Bw, &
               HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, &
              ORIFICEA, ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
               route_link_f,route_lake_f, &
               route_direction_f, route_order_f, &
               CHANRTSWCRT,g_dist, ZELEV, LAKENODE, g1_CH_NETLNK, CHANXI, CHANYJ, &
               CHLAT, CHLON, channel_option, g1_latval,g1_lonval,&
               STRMFRXSTPTS,geo_finegrid_flnm)
        endif

        call decompose_RT_int(g1_LAKE_MSKRT,LAKE_MSKRT,g_IXRT,G_JXRT,ixrt,jxrt)
        call decompose_RT_real(g1_latval,latval,g_IXRT,G_JXRT,ixrt,jxrt)
        call decompose_RT_real(g1_lonval,lonval,g_IXRT,G_JXRT,ixrt,jxrt)

!        do k = 1, 9
!           call decompose_RT_real(g_dist(:,:,k),dist(:,:,k),g_IXRT,G_JXRT,ixrt,jxrt)
!        end do

        return 
        end SUBROUTINE MPP_READ_CHROUTING
#endif
        
      SUBROUTINE READ_ROUTING_seq(IXRT,JXRT,ELRT,CH_NETRT,LKSATFAC,route_topo_f,    &
            route_chan_f, geo_finegrid_flnm,OVROUGHRTFAC,RETDEPRTFAC)


#include <netcdf.inc>
        INTEGER, INTENT(IN) :: IXRT,JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: ELRT,LKSATFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
!Dummy inverted grids
        REAL, DIMENSION(IXRT,JXRT) :: ELRT_inv,LKSATFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: RETDEPRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: RETDEPRTFAC_inv
        INTEGER, DIMENSION(IXRT,JXRT) :: CH_NETRT_inv

        INTEGER         :: I,J, iret, jj
        CHARACTER(len=256)        :: var_name
        CHARACTER(len=256)       :: route_topo_f
        CHARACTER(len=256)       :: route_chan_f
        CHARACTER(len=256)       :: geo_finegrid_flnm

        var_name = "TOPOGRAPHY"
        iret = get2d_real(var_name,ELRT_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

!!!DY to be fixed ... 6/27/08
!        var_name = "BED_ELEVATION"
!        iret = get2d_real(var_name,ELRT,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))

        var_name = "CHANNELGRID"
        call get2d_int(var_name,CH_NETRT_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

        var_name = "LKSATFAC"
        LKSATFAC_inv = -9999.9
        iret = get2d_real(var_name,LKSATFAC_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

           where (LKSATFAC_inv == -9999.9) LKSATFAC_inv = 1000.0  !specify LKSAFAC if no term avail...


!1.12.2012...Read in routing calibration factors...
        var_name = "RETDEPRTFAC"
        iret = get2d_real(var_name,RETDEPRTFAC_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (RETDEPRTFAC_inv < 0.) RETDEPRTFAC_inv = 1.0  ! reset grid to = 1.0 if non-valid value exists

        var_name = "OVROUGHRTFAC"
        iret = get2d_real(var_name,OVROUGHRTFAC_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (OVROUGHRTFAC_inv <= 0.) OVROUGHRTFAC_inv = 1.0 ! reset grid to = 1.0 if non-valid value exists



!!!Flip y-dimension of highres grids from exported Arc files...

        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           ELRT(i,j)=ELRT_inv(i,jj)
           CH_NETRT(i,j)=CH_NETRT_inv(i,jj)
           LKSATFAC(i,j)=LKSATFAC_inv(i,jj)
           RETDEPRTFAC(i,j)=RETDEPRTFAC_inv(i,jj)
           OVROUGHRTFAC(i,j)=OVROUGHRTFAC_inv(i,jj)
           jj=jxrt-j
         end do
        end do

#ifdef HYDRO_D
        write(6,*) "finish READ_ROUTING_seq"
#endif

        return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_ROUTING_seq
!DJG _____________________________
   subroutine output_lsm(outFile,did)


   implicit none

   integer did

   character(len=*) outFile

    integer :: ncid,irt, dimid_ix, dimid_jx,  &
             dimid_ixrt, dimid_jxrt, varid, &
             dimid_links, dimid_basns, dimid_soil
    integer :: iret



#ifdef MPP_LAND
     if(IO_id.eq.my_id) &
#endif
       iret = nf_create(trim(outFile), 0, ncid)
#ifdef MPP_LAND
       call mpp_land_bcast_int1(iret)
#endif

#ifdef HYDRO_D
       if (iret /= 0) then
          print*, "Problem nf_create"
          call hydro_stop()
       endif
#endif


#ifdef MPP_LAND
     if(IO_id.eq.my_id) then
#endif
#ifdef HYDRO_D
         write(6,*) "output file ", outFile
#endif
! define dimension for variables 
          iret = nf_def_dim(ncid, "depth", nlst_rt(did)%nsoil, dimid_soil)  !-- 3-d soils
   
#ifdef MPP_LAND
          iret = nf_def_dim(ncid, "ix", global_nx, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", global_ny, dimid_jx)
#else
          iret = nf_def_dim(ncid, "ix", rt_domain(did)%ix, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", rt_domain(did)%jx, dimid_jx)
#endif
    
!define variables
          iret = nf_def_var(ncid,"stc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"smc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"sh2ox",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"smcmax1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcref1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcwlt1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"infxsrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"sfcheadrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)

          iret = nf_enddef(ncid)

#ifdef MPP_LAND
    endif
#endif
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%stc,"stc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%smc,"smc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1") 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1" )
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1"  ) 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt"  ) 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SFCHEADRT,"sfcheadrt" )

#ifdef MPP_LAND
     if(IO_id.eq.my_id) then
#endif

        iret = nf_close(ncid)
#ifdef HYDRO_D
        write(6,*) "finish writing outFile : ", outFile
#endif

#ifdef MPP_LAND
    endif
#endif

        return
        end subroutine output_lsm


   subroutine RESTART_OUT_nc(outFile,did)


   implicit none

   integer did

   character(len=*) outFile

    integer :: ncid,irt, dimid_ix, dimid_jx,  &
             dimid_ixrt, dimid_jxrt, varid, &
             dimid_links, dimid_basns, dimid_soil
    integer :: iret


#ifdef MPP_LAND
     if(IO_id.eq.my_id) &
#endif
       iret = nf_create(trim(outFile), 0, ncid)
#ifdef MPP_LAND
       call mpp_land_bcast_int1(iret)
#endif

#ifdef HYDRO_D
       if (iret /= 0) then
          print*, "Problem nf_create"
          call hydro_stop()
       endif
#endif

#ifdef MPP_LAND
     if(IO_id.eq.my_id) then
#endif
! define dimension for variables 
          iret = nf_def_dim(ncid, "depth", nlst_rt(did)%nsoil, dimid_soil)  !-- 3-d soils
   
#ifdef MPP_LAND
          iret = nf_def_dim(ncid, "ix", global_nx, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", global_ny, dimid_jx)
          iret = nf_def_dim(ncid, "ixrt", global_rt_nx , dimid_ixrt)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iyrt", global_rt_ny, dimid_jxrt)
#else
          iret = nf_def_dim(ncid, "ix", rt_domain(did)%ix, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", rt_domain(did)%jx, dimid_jx)
          iret = nf_def_dim(ncid, "ixrt", rt_domain(did)%ixrt , dimid_ixrt)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iyrt", rt_domain(did)%jxrt, dimid_jxrt)
#endif

          iret = nf_def_dim(ncid, "links", rt_domain(did)%nlinks, dimid_links)
          iret = nf_def_dim(ncid, "basns", rt_domain(did)%numbasns, dimid_basns)

!define variables
          iret = nf_def_var(ncid,"stc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"smc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"sh2ox",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
    
          iret = nf_def_var(ncid,"smcmax1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcref1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcwlt1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"infxsrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
!         iret = nf_def_var(ncid,"soldrain",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"sfcheadrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)

          if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            iret = nf_def_var(ncid,"infxswgt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"QBDRYRT",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"sh2owgt",NF_FLOAT,3,(/dimid_ixrt,dimid_jxrt,dimid_soil/),varid)
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              iret = nf_def_var(ncid,"hlink",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"qlink1",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"qlink2",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"cvol",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"resht",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"qlakeo",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"lake_inflort",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
              iret = nf_def_var(ncid,"qstrmvolrt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
              iret = nf_def_var(ncid,"z_gwsubbas",NF_FLOAT,1,(/dimid_basns/),varid)
            end if
          end if  

!         put global attribute
          iret = nf_put_att_int(ncid,NF_GLOBAL,"his_out_counts",NF_INT, 1,rt_domain(did)%his_out_counts)

          iret = nf_enddef(ncid)

#ifdef MPP_LAND
    endif
#endif
       call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%stc,"stc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%smc,"smc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")


       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1") 
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1" )
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1"  ) 
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt"  ) 
!      call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%soldrain,"soldrain"  ) 
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%sfcheadrt,"sfcheadrt"  ) 


        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT, "infxswgt" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QBDRYRT, "QBDRYRT" )
            call w_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_rt(did)%nsoil,rt_domain(did)%SH2OWGT, "sh2owgt" )
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%HLINK,"hlink" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )

              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%QLINK(:,1),"qlink1" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )


              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%QLINK(:,2),"qlink2" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )


              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%cvol,"cvol" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )

              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%resht,"resht" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )

              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%qlakeo,"qlakeo" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )


              call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")
              call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT, "qstrmvolrt" )

            end if

            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
              call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%z_gwsubbas,"z_gwsubbas" )
            end if
        end if  

#ifdef MPP_LAND
        if(IO_id.eq.my_id) &
#endif
        iret = nf_close(ncid)

        return
        end subroutine RESTART_OUT_nc

        subroutine w_rst_rt_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real, dimension(ix,jx):: inVar
#ifdef MPP_LAND
           real, dimension(global_rt_nx, global_rt_ny):: varTmp
           call write_IO_rt_real(inVar,varTmp) 
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_rt_nx,global_rt_ny/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),inVar)
#endif
           
           return
        end subroutine w_rst_rt_nc2

        subroutine w_rst_rt_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real,dimension(ix,jx,nsoil):: inVar
#ifdef MPP_LAND
           integer k
           real varTmp(global_rt_nx,global_rt_ny,nsoil)
           do k = 1, nsoil
              call write_IO_rt_real(inVar(:,:,k),varTmp(:,:,k)) 
           end do
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/global_rt_nx,global_rt_ny,nsoil/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ix,jx,nsoil/),inVar)
#endif
           return
        end subroutine w_rst_rt_nc3

        subroutine w_rst_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real inVar(ix,jx)

#ifdef MPP_LAND
           real varTmp(global_nx,global_ny)
           call write_IO_real(inVar,varTmp) 
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_nx,global_ny/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),invar)
#endif
           
           return
        end subroutine w_rst_nc2

        subroutine w_rst_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real inVar(ix,jx,nsoil)
           integer k
#ifdef MPP_LAND
           real varTmp(global_nx,global_ny,nsoil)
           do k = 1, nsoil
              call write_IO_real(inVar(:,:,k),varTmp(:,:,k)) 
           end do
           if(my_id .eq. IO_id) then
             iret = nf_inq_varid(ncid,varName, varid)
             iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/global_nx,global_ny,nsoil/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ix,jx,nsoil/),inVar)
#endif
           return
        end subroutine w_rst_nc3

        subroutine w_rst_crt_nc1(ncid,n,inVar,varName &
#ifdef MPP_LAND
                 ,index, mpp_n&
#endif
                  )
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           integer:: index(n),mpp_n
           call write_chanel_real(inVar,index,mpp_n,n)          
           if(my_id .eq. IO_id) then
#endif
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1

        subroutine w_rst_crt_nc1g(ncid,n,inVar,varName)
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           if(my_id .eq. IO_id) then
#endif
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1g

   subroutine RESTART_IN_NC(inFile,did)


   implicit none
   character(len=*) inFile
   integer  :: ierr, iret,ncid, did

    integer :: i, j


#ifdef MPP_LAND
     if(IO_id .eq. my_id) then
#endif
!open a netcdf file 
    iret = nf_open(trim(inFile), NF_NOWRITE, ncid)
#ifdef MPP_LAND
    endif
    call mpp_land_bcast_int1(iret)
#endif
    if (iret /= 0) then
#ifdef HYDRO_D
       write(*,'("Problem opening file: ''", A, "''")') &
            trim(inFile)
       call hydro_stop() 
#endif
    endif

#ifdef MPP_LAND
     if(IO_id .eq. my_id) then
#endif
        iret = NF_GET_ATT_INT(ncid, NF_GLOBAL, 'his_out_counts', rt_domain(did)%his_out_counts) 
#ifdef MPP_LAND
    endif
    call mpp_land_bcast_int1(rt_domain(did)%out_counts)
#endif

#ifdef HYDRO_D
     write(6,*) "nlst_rt(did)%nsoil=",nlst_rt(did)%nsoil
#endif
     if(nlst_rt(did)%rst_typ .eq. 1 ) then 
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%stc,"stc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%smc,"smc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt")
!       call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%soldrain,"soldrain")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%sfcheadrt,"sfcheadrt")
     endif
 
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1")


        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT,"infxswgt")
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QBDRYRT,"QBDRYRT")
            call read_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_rt(did)%nsoil,rt_domain(did)%SH2OWGT,"sh2owgt")
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              call read_rst_crt_nc(ncid,rt_domain(did)%HLINK,rt_domain(did)%NLINKS,"hlink")
              call read_rst_crt_nc(ncid,rt_domain(did)%QLINK(:,1),rt_domain(did)%NLINKS,"qlink1")
              call read_rst_crt_nc(ncid,rt_domain(did)%QLINK(:,2),rt_domain(did)%NLINKS,"qlink2")
              call read_rst_crt_nc(ncid,rt_domain(did)%CVOL,rt_domain(did)%NLINKS,"cvol")
              call read_rst_crt_nc(ncid,rt_domain(did)%RESHT,rt_domain(did)%nlinks,"resht")
              call read_rst_crt_nc(ncid,rt_domain(did)%QLAKEO,rt_domain(did)%nlinks,"qlakeo")
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT,"qstrmvolrt")
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.1.AND.nlst_rt(did)%GW_RESTART.NE.0) then
                 call read_rst_crt_nc(ncid,rt_domain(did)%z_gwsubbas,rt_domain(did)%numbasns,"z_gwsubbas")
            end if
        end if

          if(nlst_rt(did)%rstrt_swc.eq.1) then  !Switch for rest of restart accum vars...
#ifdef HYDRO_D
            print *, "1 Resetting RESTART Accumulation Variables to 0...",nlst_rt(did)%rstrt_swc
#endif
            rt_domain(did)%INFXSRT=0.
            rt_domain(did)%LAKE_INFLORT=0.
            rt_domain(did)%QSTRMVOLRT=0.
          end if

      
#ifdef MPP_LAND
        if(my_id .eq. IO_id) &
#endif
        iret =  nf_close(ncid) 
#ifdef HYDRO_D
        write(6,*) "end of RESTART_IN"
#endif
 
        return
        end subroutine RESTART_IN_nc

      subroutine read_rst_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none 
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_nx,global_ny,nsoil) :: xtmp
         integer i

         if(my_id .eq. IO_id) & 
#endif
           iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif

         if (iret /= 0) then
#ifdef HYDRO_D
            print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
            return
         endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
            iret = nf_get_var_real(ncid, varid, xtmp)

         do i = 1, nsoil
            call decompose_data_real(xtmp(:,:,i), var(:,:,i))
         end do
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif

         return
      end subroutine read_rst_nc3

      subroutine read_rst_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx,ireg, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_nx,global_ny) :: xtmp 
         if(my_id .eq. IO_id) & 
#endif
           iret = nf_inq_varid(ncid,  trim(varStr),  varid)

#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif

         if (iret /= 0) then
#ifdef HYDRO_D
            print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
            return
         endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
            iret = nf_get_var_real(ncid, varid, xtmp)

         call decompose_data_real(xtmp, var)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_nc2

      subroutine read_rst_rt_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny,nsoil) :: xtmp
         integer i
         if(my_id .eq. IO_id) & 
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
         if (iret /= 0) then
#ifdef HYDRO_D
            print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
            return
         endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         iret = nf_get_var_real(ncid, varid, xtmp)
         do i = 1, nsoil
            call decompose_RT_real(xtmp(:,:,i),var(:,:,i),global_rt_nx,global_rt_ny,ix,jx)
         end do
#else
         iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_rt_nc3

      subroutine read_rst_rt_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx,ireg, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny) :: xtmp 
#endif
         iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
         if (iret /= 0) then
#ifdef HYDRO_D
            print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
            return
         endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) &   
             iret = nf_get_var_real(ncid, varid, xtmp)
         call decompose_RT_real(xtmp,var,global_rt_nx,global_rt_ny,ix,jx)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_rt_nc2

      subroutine read_rt_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr

#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny) :: xtmp
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
               return
            endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
            iret = nf_get_var_real(ncid, varid, xtmp)
         endif
         call decompose_RT_real(xtmp,var,global_rt_nx,global_rt_ny,ix,jx)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rt_nc2

      subroutine read_rst_crt_nc(ncid,var,n,varStr)
         implicit none
         integer ::  ireg, ncid, varid, n, iret
         real,dimension(n) ::  var
         character(len=*) :: varStr

#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
               return
            endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
#endif
            iret = nf_get_var_real(ncid, varid, var)
#ifdef MPP_LAND
         endif
         call mpp_land_bcast_real(n,var)
#endif
         return
      end subroutine read_rst_crt_nc 

      subroutine hrldas_out()
      end subroutine hrldas_out

      SUBROUTINE READ_ROUTING_old(IXRT,JXRT,ELRT,CH_NETRT,LKSATFAC,route_topo_f,    &
            route_chan_f, geo_finegrid_flnm,OVROUGHRTFAC,RETDEPRTFAC)


#include <netcdf.inc>
        INTEGER, INTENT(IN) :: IXRT,JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: ELRT,LKSATFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
!Dummy inverted grids
        REAL, DIMENSION(IXRT,JXRT) :: ELRT_inv,LKSATFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: RETDEPRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: RETDEPRTFAC_inv
        INTEGER, DIMENSION(IXRT,JXRT) :: CH_NETRT_inv

        INTEGER         :: I,J, iret, jj
        CHARACTER(len=256)        :: var_name
        CHARACTER(len=256)       :: route_topo_f
        CHARACTER(len=256)       :: route_chan_f
        CHARACTER(len=256)       :: geo_finegrid_flnm

        var_name = "TOPOGRAPHY"
        iret = get2d_real(var_name,ELRT_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

!!!DY to be fixed ... 6/27/08
!        var_name = "BED_ELEVATION"
!        iret = get2d_real(var_name,ELRT,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))

        var_name = "CHANNELGRID"
        call get2d_int(var_name,CH_NETRT_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

        var_name = "LKSATFAC"
        LKSATFAC_inv = -9999.9
        iret = get2d_real(var_name,LKSATFAC_inv,ixrt,jxrt,&
               trim(geo_finegrid_flnm))
#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

           where (LKSATFAC_inv == -9999.9) LKSATFAC_inv = 1000.0  !specify LKSAFAC if no term avail...


!1.12.2012...Read in routing calibration factors...
        var_name = "RETDEPRTFAC"
        iret = get2d_real(var_name,RETDEPRTFAC_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (RETDEPRTFAC_inv < 0.) RETDEPRTFAC_inv = 1.0  ! reset grid to = 1.0 if non-valid value exists

        var_name = "OVROUGHRTFAC"
        iret = get2d_real(var_name,OVROUGHRTFAC_inv,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (OVROUGHRTFAC_inv <= 0.) OVROUGHRTFAC_inv = 1.0 ! reset grid to = 1.0 if non-valid value exists



!!!Flip y-dimension of highres grids from exported Arc files...

        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           ELRT(i,j)=ELRT_inv(i,jj)
           CH_NETRT(i,j)=CH_NETRT_inv(i,jj)
           LKSATFAC(i,j)=LKSATFAC_inv(i,jj)
           RETDEPRTFAC(i,j)=RETDEPRTFAC_inv(i,jj)
           OVROUGHRTFAC(i,j)=OVROUGHRTFAC_inv(i,jj)
           jj=jxrt-j
         end do
        end do

#ifdef HYDRO_D  
        write(6,*) "finish READ_ROUTING_old"
#endif

        return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_ROUTING_old
!DJG _____________________________

end module module_HYDRO_io
