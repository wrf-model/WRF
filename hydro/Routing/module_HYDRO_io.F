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

module module_HYDRO_io
#ifdef MPP_LAND
     use module_mpp_land
     use module_mpp_reachls,  only: ReachLS_decomp, reachls_wreal, ReachLS_write_io, &
                                    ReachLS_wInt, reachls_wreal2, TONODE2RSL, gbcastvalue
     use MODULE_mpp_GWBUCKET, only: gw_write_io_real, gw_write_io_int
#endif
   use Module_Date_utilities_rt, only: geth_newdate
   use module_HYDRO_utils, only: get_dist_ll
   use module_namelist, only: nlst_rt
   use module_RT_data, only: rt_domain
   use module_gw_gw2d_data, only: gw2d   
   use netcdf 

   implicit none
#include <netcdf.inc>

     contains

        integer function get2d_real(var_name,out_buff,ix,jx,fileName, fatalErr)
          implicit none
          integer :: ivar, iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          logical, optional, intent(in) :: fatalErr
          logical :: fatalErr_local
          character(len=256) :: errMsg
          
          fatalErr_local = .false.
          if(present(fatalErr)) fatalErr_local=fatalErr

          get2d_real = -1

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
             errMsg = "get2d_real: failed to open the netcdf file: " // trim(fileName)
             print*, trim(errMsg)
             if(fatalErr_local) call hydro_stop(trim(errMsg))
             out_buff = -9999.
             return 
          endif

          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
             ivar = nf_inq_varid(ncid,trim(var_name//"_M"),  varid)
             if(ivar .ne. 0) then
                errMsg = "get2d_real: failed to find the variables: " //      &
                         trim(var_name) // ' and ' // trim(var_name//"_M") // &
                         ' in ' // trim(fileName)
                write(6,*) errMsg
                if(fatalErr_local) call hydro_stop(errMsg)
                return 
             endif
          end if

          iret = nf_get_var_real(ncid, varid, out_buff)
          if(iret .ne. 0) then
             errMsg = "get2d_real: failed to read the variable: " // &
                      trim(var_name) // ' or ' // trim(var_name//"_M") // &
                      ' in ' // trim(fileName)
             print*,trim(errMsg)
             if(fatalErr_local) call hydro_stop(trim(errMsg))
             return
          endif

          iret = nf_close(ncid)
          if(iret .ne. 0) then
             errMsg = "get2d_real: failed to close the file: " // &
                      trim(fileName)
             print*,trim(errMsg)
             if(fatalErr_local) call hydro_stop(trim(errMsg))
          endif

          get2d_real =  ivar
      end function get2d_real

     
     subroutine get2d_lsm_real(var_name,out_buff,ix,jx,fileName)
         implicit none
         integer ix,jx, status
         character (len=*),intent(in) :: var_name, fileName
         real,dimension(ix,jx):: out_buff


#ifdef MPP_LAND
#ifdef PARALLELIO
         status = get2d_real(var_name,out_buff,ix,jx,fileName)
#else
         real,allocatable, dimension(:,:) :: buff_g


#ifdef HYDRO_D
         write(6,*) "start to read variable ", var_name
#endif
         if(my_id .eq. IO_id) then
            allocate(buff_g (global_nx,global_ny) )
            status = get2d_real(var_name,buff_g,global_nx,global_ny,fileName)
         else
            allocate(buff_g (1,1) )
         end if
         call decompose_data_real(buff_g,out_buff)     
         if(allocated(buff_g)) deallocate(buff_g)
#endif
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


#ifndef PARALLELIO
         if(my_id .eq. IO_id) then
            allocate(buff_g (global_nx,global_ny) )
         else
            allocate(buff_g (1,1) )
         endif
         if(my_id .eq. IO_id) then
#endif
#endif
                ! Open the NetCDF file.
              iret = nf_open(fileName, NF_NOWRITE, ncid)
              if (iret /= 0) then
                 write(*,'("Problem opening geo_static file: ''", A, "''")') &
                      trim(fileName)
                 call hydro_stop("In get2d_lsm_vegtyp() - Problem opening geo_static file")
              endif

            iret = nf_inq_dimid(ncid, "land_cat", dimid)
            if (iret /= 0) then
              call hydro_stop("In get2d_lsm_vegtyp() - nf_inq_dimid:  land_cat problem ")
             endif

            iret = nf_inq_dimlen(ncid, dimid, land_cat)
            if (iret /= 0) then
               call hydro_stop("In get2d_lsm_vegtyp() - nf_inq_dimlen:  land_cat problem")
            endif

#ifdef MPP_LAND
#ifndef PARALLELIO
            call get_landuse_netcdf(ncid, buff_g, units, global_nx ,global_ny, land_cat)
         end if
         call decompose_data_real(buff_g,xdum)     
         if(allocated(buff_g)) deallocate(buff_g)
#else
          call get_landuse_netcdf(ncid, xdum,   units, ix, jx, land_cat)
#endif
          iret = nf_close(ncid)

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
#ifndef PARALLELIO
            if(my_id .eq. IO_id) then
#endif
#endif
            iret = nf_open(fileName, NF_NOWRITE, ncid)
            if (iret /= 0) then
               write(*,'("Problem opening geo_static file: ''", A, "''")') &
                    trim(fileName)
               call hydro_stop("In get_file_dimension() - Problem opening geo_static file")
            endif
        
            iret = nf_inq_dimid(ncid, "west_east", dimid)
        
            if (iret /= 0) then
               call hydro_stop("In get_file_dimension() - nf_inq_dimid:  west_east problem")
            endif
        
            iret = nf_inq_dimlen(ncid, dimid, ix)
            if (iret /= 0) then
               call hydro_stop("In get_file_dimension() - nf_inq_dimlen:  west_east problem")
            endif
        
            iret = nf_inq_dimid(ncid, "south_north", dimid)
            if (iret /= 0) then
               call hydro_stop("In get_file_dimension() - nf_inq_dimid:  south_north problem.")
            endif
        
            iret = nf_inq_dimlen(ncid, dimid, jx)
            if (iret /= 0) then
               call hydro_stop("In get_file_dimension() - nf_inq_dimlen:  south_north problem")
            endif
            iret = nf_close(ncid)
#ifdef MPP_LAND
#ifndef PARALLELIO
            endif
            call mpp_land_bcast_int1(ix)
            call mpp_land_bcast_int1(jx)
#endif
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
#ifndef PARALLELIO
         real,allocatable, dimension(:,:) :: buff_g


         if(my_id .eq. IO_id) then
              allocate(buff_g (global_nx,global_ny) )
#endif
#endif
                ! Open the NetCDF file.
            iret = nf_open(fileName, NF_NOWRITE, ncid)
              if (iret /= 0) then
                 write(*,'("Problem opening geo_static file: ''", A, "''")') &
                      trim(fileName)
                 call hydro_stop("In get2d_lsm_soltyp() - problem to open geo_static file.")
              endif

            iret = nf_inq_dimid(ncid, "soil_cat", dimid)
            if (iret /= 0) then
                call hydro_stop("In get2d_lsm_soltyp() - nf_inq_dimid:  soil_cat problem")
            endif

            iret = nf_inq_dimlen(ncid, dimid, land_cat)
            if (iret /= 0) then
               call hydro_stop("In get2d_lsm_soltyp() - nf_inq_dimlen:  soil_cat problem")
            endif

#ifdef MPP_LAND
#ifndef PARALLELIO
            call get_soilcat_netcdf(ncid, buff_g, units, global_nx ,global_ny, land_cat)
         end if
         call decompose_data_real(buff_g,xdum)     
         if(my_id .eq. io_id) then 
           if(allocated(buff_g)) deallocate(buff_g)
         endif
#else
          call get_soilcat_netcdf(ncid, xdum,   units, ix, jx, land_cat)
#endif
          iret = nf_close(ncid)
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
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_landuse_netcdf() - nf_inq_varid problem")
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_landuse_netcdf() - nf_get_var_real problem")
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
    integer :: i, j, did
    character(len=24), parameter :: name = "SOILCTOP"

    did = 1
    units = ""

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_soilcat_netcdf() - nf_inq_varid problem")
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_soilcat_netcdf() - nf_get_var_real problem")
    endif

    do i = 1, idim
       do j = 1, jdim
          mp = maxloc(xtmp(i,j,:))
          array(i,j) = mp(1)
       enddo
    enddo

     if(nlst_rt(did)%GWBASESWCRT .ne. 3) then
        where (array == 14) array = 1   ! DJG remove all 'water' soils...
     endif

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
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_greenfrac_netcdf() - nf_inq_varid problem")
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_greenfrac_netcdf() - nf_get_var_real problem")
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
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_albedo12m_netcdf() - nf_inq_varid problem")
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       call hydro_stop("In get_albedo12m_netcdf() - nf_get_var_real problem")
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
          print*, 'name = "', trim(name)//'"'
          call hydro_stop("In get_2d_netcdf() - nf_inq_varid problem")
       else
          ierr = iret
          return
       endif
    endif


    iret = nf_get_var_real(ncid, varid, array)
    if (iret /= 0) then
       if (fatal_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          call hydro_stop("In get_2d_netcdf() - nf_get_var_real problem")
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
           call hydro_stop("In get_2d_netcdf_cows() - nf_inq_varid problem")
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
            route_direction_f, NLINKS, &
            CH_NETLNK, channel_option, geo_finegrid_flnm, NLINKSL, UDMP_OPT)

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: I,J,channel_option,jj
        INTEGER, INTENT(INOUT)                       :: NLINKS, NLINKSL
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_LNKRT   !- link routing ID
        integer, INTENT(IN)                          :: UDMP_OPT                                

!!Dummy read in grids for inverted y-axis


        CHARACTER(len=*)         :: route_chan_f, route_link_f,route_direction_f
        CHARACTER(len=256)       :: geo_finegrid_flnm
        CHARACTER(len=256)       :: var_name

        ! variables for handling netcdf dimensions
        integer :: iRet, ncid, dimId
        logical :: routeLinkNetcdf
     
        NLINKS = 0
        CH_NETRT = -9999
        CH_NETLNK = -9999
        
        NLINKSL   = 0
        CH_LNKRT  = -9999



        cnt = 0 
#ifdef HYDRO_D
       print *, "Channel Option in Routedim is ", channel_option
#endif


        if (channel_option .eq. 4) return  ! it will run Rapid


!-- will always read channel grid       IF(channel_option.eq.3) then  !get maxnodes and links from grid

         var_name = "CHANNELGRID"
         call readRT2d_int(var_name,CH_NETRT,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))


!-- new link id variable to handle link routing
         var_name = "LINKID"
#ifdef MPP_LAND
#ifdef HYDRO_D
    write(6,*) "read LINKID for CH_LNKRT from ", trim(geo_finegrid_flnm)
#endif
#endif
!!!! LINKID is used for reach based method.  ?
     IF(channel_option.ne.3 .and. UDMP_OPT.ne.1) then  !get maxnodes and links from grid
         call readRT2d_int(var_name,CH_LNKRT,ixrt,jxrt,&
                   trim(geo_finegrid_flnm), fatalErr=.TRUE.)
     endif


         
         var_name = "FLOWDIRECTION"
         call readRT2d_int(var_name,DIRECTION,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))

!note that this is not used for link routing
         var_name = "LAKEGRID"
         call readRT2d_int(var_name,LAKE_MSKRT,ixrt,jxrt,&
                   trim(geo_finegrid_flnm))


        var_name = "LATITUDE"
        call readRT2d_real(var_name,LAT,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        var_name = "LONGITUDE"
        call readRT2d_real(var_name,LON,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
          
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do

!DJG inv         do j=jxrt,1,-1
         do j=1,jxrt
             do i = 1, ixrt
!               if (CH_NETRT(i,j) .ge.0.AND.CH_NETRT(i,j).lt.100) then 
                if (CH_NETRT(i,j) .ge.0) then
                 NLINKS = NLINKS + 1
                 if( UDMP_OPT .eq. 1) CH_NETLNK(i,j) = 2
               endif
            end do 
         end do 
#ifdef HYDRO_D
         print *, "NLINKS IS ", NLINKS 
#endif
     if( UDMP_OPT .eq. 1) then 
         return
     endif

!DJG inv         DO j = JXRT,1,-1  !rows
         DO j = 1,JXRT  !rows
          DO i = 1 ,IXRT   !colsumns
           If (CH_NETRT(i, j) .ge. 0) then !get its direction
            If ((DIRECTION(i, j) .EQ. 64) .AND. (j+1 .LE. JXRT) ) then !North
               if(CH_NETRT(i,j+1) .ge.0) then 
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
                endif
            else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
               .AND. (j + 1 .LE. JXRT) ) then !North East
                if(CH_NETRT(i+1,j+1) .ge.0) then
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt
                endif
            else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT)) then !East
                if(CH_NETRT(i+1,j) .ge. 0) then
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt 
                endif
            else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0)) then !south east
                 if(CH_NETRT(i+1,j-1).ge.0) then
                     cnt = cnt + 1
                     CH_NETLNK(i,j) = cnt
                 endif
            else if ((DIRECTION(i, j) .EQ. 4).AND.(j - 1 .NE. 0)) then !due south
                 if(CH_NETRT(i,j-1).ge.0) then
                         cnt = cnt + 1
                         CH_NETLNK(i,j) = cnt
                 endif
            else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
                    .AND. (j - 1 .NE. 0)  ) then !south west
                if(CH_NETRT(i-1,j-1).ge.0) then
                     cnt = cnt + 1
                     CH_NETLNK(i,j) = cnt
                endif
            else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0)) then !West
                 if(CH_NETRT(i-1,j).ge.0) then
                       cnt = cnt + 1
                       CH_NETLNK(i,j) = cnt
                 endif
            else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT) ) then !North West
                 if(CH_NETRT(i-1,j+1).ge.0) then
                        cnt = cnt + 1
                        CH_NETLNK(i,j) = cnt 
                 endif
           else 
#ifdef HYDRO_D
             write(*,135) "PrPt/LkIn", CH_NETRT(i,j), DIRECTION(i,j), LON(i,j), LAT(i,j),i,j 
#endif
135             FORMAT(A9,1X,I3,1X,I3,1X,F10.5,1X,F9.5,1X,I4,1X,I4)
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

           If ( (DIRECTION(i, j).EQ. 64) )then 
              if( j + 1 .GT. JXRT) then           !-- 64's can only flow north
                 cnt = cnt + 1
                 CH_NETLNK(i,j) = cnt
              elseif(CH_NETRT(i,j+1) .lt. 0) then !North
                 cnt = cnt + 1
                 CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
                  print *, "Boundary Pour Point N", cnt,CH_NETRT(i,j), i,j
#endif
              endif
           else if ( DIRECTION(i, j) .EQ. 128) then
               if ((i + 1 .GT. IXRT) .or. (j + 1 .GT. JXRT))  then    !-- 128's can flow out of the North or East edge
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt
                                                                      !   this is due north edge     
               elseif(CH_NETRT(i + 1, j + 1).lt.0) then !North East
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point NE", cnt, CH_NETRT(i,j),i,j
#endif
               endif
           else if (DIRECTION(i, j) .EQ. 1) then 
                if (i + 1 .GT. IXRT) then      !-- 1's can only flow due east
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt
                elseif(CH_NETRT(i + 1, j) .lt. 0) then !East
                   cnt = cnt + 1
                   CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
              print *, "Boundary Pour Point E", cnt,CH_NETRT(i,j), i,j
#endif
                endif
           else if (DIRECTION(i, j) .EQ. 2) then
               !-- 2's can flow out of east or south edge
              if( (i + 1 .GT. IXRT) .OR.  (j - 1 .EQ. 0)) then            !-- this is the south edge
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
              elseif(CH_NETRT(i + 1, j - 1) .lt.0) then !south east
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
                  print *, "Boundary Pour Point SE", cnt,CH_NETRT(i,j), i,j
#endif
              endif
           else if ( DIRECTION(i, j) .EQ. 4) then 
              if( (j - 1 .EQ. 0))  then            !-- 4's can only flow due south
                 cnt = cnt + 1
                 CH_NETLNK(i,j) = cnt
              elseif (CH_NETRT(i, j - 1) .lt. 0) then !due south
                 cnt = cnt + 1
                 CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
                 print *, "Boundary Pour Point S", cnt,CH_NETRT(i,j), i,j
#endif
              endif
           else if ( DIRECTION(i, j) .EQ. 8) then
          !-- 8's can flow south or west
              if( (i - 1 .eq. 0) .OR. ( j - 1 .EQ. 0)) then             !-- this is the south edge
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
              elseif  (CH_NETRT(i - 1, j - 1).lt.0) then !south west
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
                  print *, "Boundary Pour Point SW", cnt,CH_NETRT(i,j), i,j
#endif
              endif
           else if ( DIRECTION(i, j) .EQ. 16) then 
              if(i - 1 .eq. 0) then              !-- 16's can only flow due west 
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt              
              elseif (CH_NETRT(i - 1, j).lt.0) then !West
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt              
#ifdef HYDRO_D
              print *, "Boundary Pour Point W", cnt,CH_NETRT(i,j), i,j
#endif
              endif
           else if ( DIRECTION(i, j) .EQ. 32)  then
              if ( (i - 1 .eq. 0)      &      !-- 32's can flow either west or north
               .OR.   (j .eq. JXRT))  then         !-- this is the north edge
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
              elseif (CH_NETRT(i - 1, j + 1).lt.0) then !North West
                  cnt = cnt + 1
                  CH_NETLNK(i,j) = cnt
#ifdef HYDRO_D
                  print *, "Boundary Pour Point NW", cnt,CH_NETRT(i,j), i,j
#endif
              endif
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
         print *, "Apparent error in network topology", cnt, NLINKS
         print* , "ixrt =", ixrt, "jxrt =", jxrt
         call hydro_stop("READ_ROUTEDIM")
       endif

!!-- no longer find the lakes from the 2-d hi res grid
!DJG inv       do j=jxrt,1,-1
!       do j=1,jxrt
!          do i = 1,ixrt
!           if (LAKE_MSKRT(i,j) .gt. NLAKES) then 
!             NLAKES = LAKE_MSKRT(i,j)
!           endif
!        end do
!       end do
!#ifdef HYDRO_D
!       write(6,*) "finish read_red ..  Total Number of Lakes in Domain = ", NLAKES
!#endif


!-- don't return here--!  return

     END SUBROUTINE READ_ROUTEDIM

!!! This subroutine gets the NLINKSL
     subroutine get_NLINKSL(NLINKSL, channel_option, route_link_f)
        implicit none
        CHARACTER(len=*)         :: route_link_f
        integer :: NLINKSL, channel_option
        CHARACTER(len=256)         :: route_link_f_r
        integer :: lenRouteLinkFR
        logical :: routeLinkNetcdf
        CHARACTER(len=256)       :: InputLine
        if (channel_option.ne.3) then  ! overwrite the NLINKS
!-IF is now commented above   else  ! get nlinks from the ascii file of links
#ifdef HYDRO_D
           write(6,*) "read file to get NLINKSL from", trim(route_link_f)
           call flush(6)
#endif
       !! is RouteLink file netcdf (*.nc) or csv (*.csv)
           route_link_f_r = adjustr(route_link_f)
           lenRouteLinkFR = len(route_link_f_r)
           routeLinkNetcdf = route_link_f_r( (lenRouteLinkFR-2):lenRouteLinkFR) .eq.  '.nc'

           if(routeLinkNetcdf) then
              NLINKSL = get_netcdf_dim(trim(route_link_f), 'linkDim',  &
                                   'READ_ROUTEDIM', fatalErr=.true.)
           else
              open(unit=17,file=trim(route_link_f),          & !link
                   form='formatted',status='old')

1011          read(17,*,end= 1999) InputLine
              NLINKSL = NLINKSL + 1
              goto 1011
1999          continue
              NLINKSL = NLINKSL - 1 !-- first line is a comment 
              close(17)
           end if ! routeLinkNetcdf

#ifdef HYDRO_D
            print *, "Number of Segments or Links on sparse network", NLINKSL
            write(6,*) "NLINKSL = ", NLINKSL
            call flush(6)
#endif

      end if !end-if is now for channel_option just above, not IF from further up

          return
     end subroutine get_NLINKSL

     subroutine nreadRT2d_real(var_name, inv, ixrt, jxrt, fileName, fatalErr) 
         implicit none
         INTEGER :: iret
         INTEGER, INTENT(IN) :: ixrt,jxrt
         INTEGER :: i, j, ii,jj
         CHARACTER(len=*):: var_name,fileName
         real, INTENT(OUT), dimension(ixrt,jxrt) :: inv
#ifndef MPP_LAND
         real, dimension(ixrt,jxrt) :: inv_tmp
#endif
         logical, optional, intent(in) :: fatalErr
         logical :: fatalErr_local
#ifdef MPP_LAND
         real, allocatable,dimension(:,:) :: g_inv_tmp, g_inv
#endif
         fatalErr_local = .FALSE.
         if(present(fatalErr)) fatalErr_local=fatalErr

#ifdef MPP_LAND
         if(my_id .eq. io_id) then

              allocate(g_inv_tmp(global_rt_nx,global_rt_ny))
              allocate(g_inv(global_rt_nx,global_rt_ny))


              g_inv_tmp = -9999.9
              iret =  get2d_real(var_name,g_inv_tmp,global_rt_nx,global_rt_ny,&
                     trim(fileName), fatalErr=fatalErr_local)
              do i=1,global_rt_nx
                 jj=global_rt_ny
                 do j=1,global_rt_ny
                   g_inv(i,j)=g_inv_tmp(i,jj)
                   jj=global_rt_ny-j
                 end do
              end do
              if(allocated(g_inv_tmp)) deallocate(g_inv_tmp)
         else
              allocate(g_inv(1,1))
         endif 
         call decompose_RT_real(g_inv,inv,global_rt_nx,global_rt_ny,IXRT,JXRT)
         if(allocated(g_inv)) deallocate(g_inv)
#else
         inv_tmp = -9999.9
         iret =  get2d_real(var_name,inv_tmp,ixrt,jxrt,&
                     trim(fileName), fatalErr=fatalErr_local)
         do i=1,ixrt
            jj=jxrt
         do j=1,jxrt
           inv(i,j)=inv_tmp(i,jj)
           jj=jxrt-j
         end do
        end do
#endif

        
     end SUBROUTINE nreadRT2d_real

     subroutine nreadRT2d_int(var_name, inv, ixrt, jxrt, fileName, fatalErr) 
         implicit none
         INTEGER, INTENT(IN) :: ixrt,jxrt
         INTEGER :: i, j, ii,jj, iret
         CHARACTER(len=*):: var_name,fileName
         integer, INTENT(OUT), dimension(ixrt,jxrt) :: inv
         integer, dimension(ixrt,jxrt) :: inv_tmp
         logical, optional, intent(in) :: fatalErr
         logical :: fatalErr_local
#ifdef MPP_LAND
         integer, allocatable,dimension(:,:) :: g_inv_tmp, g_inv
#endif
         fatalErr_local = .FALSE.
         if(present(fatalErr)) fatalErr_local=fatalErr

#ifdef MPP_LAND
         if(my_id .eq. io_id) then
              allocate(g_inv_tmp(global_rt_nx,global_rt_ny))
              allocate(g_inv(global_rt_nx,global_rt_ny))
              g_inv_tmp = -9999.9
              call  get2d_int(var_name,g_inv_tmp,global_rt_nx,global_rt_ny,&
                     trim(fileName), fatalErr=fatalErr_local)
              do i=1,global_rt_nx
                 jj=global_rt_ny
                do j=1,global_rt_ny
                  g_inv(i,j)=g_inv_tmp(i,jj)
                  jj=global_rt_ny-j
                end do
              end do
         else
              allocate(g_inv_tmp(1,1))
              allocate(g_inv(1,1))
         endif
         call decompose_RT_int(g_inv,inv,global_rt_nx,global_rt_ny,IXRT,JXRT)
         if(allocated(g_inv_tmp)) deallocate(g_inv_tmp)
         if(allocated(g_inv)) deallocate(g_inv)
#else
         call  get2d_int(var_name,inv_tmp,ixrt,jxrt,&
                     trim(fileName), fatalErr=fatalErr_local)
         do i=1,ixrt
            jj=jxrt
         do j=1,jxrt
           inv(i,j)=inv_tmp(i,jj)
           jj=jxrt-j
         end do
        end do
#endif
     end SUBROUTINE nreadRT2d_int
!---------------------------------------------------------
!DJG -----------------------------------------------------

     subroutine readRT2d_real(var_name, inv, ixrt, jxrt, fileName, fatalErr) 
         implicit none
         INTEGER :: iret
         INTEGER, INTENT(IN) :: ixrt,jxrt
         INTEGER :: i, j, ii,jj
         CHARACTER(len=*):: var_name,fileName
         real, INTENT(OUT), dimension(ixrt,jxrt) :: inv
         real, dimension(ixrt,jxrt) :: inv_tmp
         logical, optional, intent(in) :: fatalErr
         logical :: fatalErr_local
         fatalErr_local = .FALSE.
         if(present(fatalErr)) fatalErr_local=fatalErr
         inv_tmp = -9999.9
         iret =  get2d_real(var_name,inv_tmp,ixrt,jxrt,&
                     trim(fileName), fatalErr=fatalErr_local)
         do i=1,ixrt
            jj=jxrt
         do j=1,jxrt
           inv(i,j)=inv_tmp(i,jj)
           jj=jxrt-j
         end do
        end do
     end SUBROUTINE readRT2d_real

     subroutine readRT2d_int(var_name, inv, ixrt, jxrt, fileName, fatalErr) 
         implicit none
         INTEGER, INTENT(IN) :: ixrt,jxrt
         INTEGER :: i, j, ii,jj
         CHARACTER(len=*):: var_name,fileName
         integer, INTENT(OUT), dimension(ixrt,jxrt) :: inv
         integer, dimension(ixrt,jxrt) :: inv_tmp
         logical, optional, intent(in) :: fatalErr
         logical :: fatalErr_local
         fatalErr_local = .FALSE.
         if(present(fatalErr)) fatalErr_local=fatalErr
         call  get2d_int(var_name,inv_tmp,ixrt,jxrt,&
                     trim(fileName), fatalErr=fatalErr_local)
         do i=1,ixrt
            jj=jxrt
         do j=1,jxrt
           inv(i,j)=inv_tmp(i,jj)
           jj=jxrt-j
         end do
        end do
     end SUBROUTINE readRT2d_int

!---------------------------------------------------------
!DJG -----------------------------------------------------

#ifdef MPP_LAND
  subroutine MPP_READ_SIMP_GW(IX,JX,IXRT,JXRT,GWSUBBASMSK,gwbasmskfil,&
          gw_strm_msk,numbasns,ch_netrt,AGGFACTRT)

   USE module_mpp_land
    
    integer, intent(in)                     :: IX,JX,IXRT,JXRT,AGGFACTRT
    integer, intent(out)                    :: numbasns
    integer, intent(out), dimension(IX,JX)  :: GWSUBBASMSK
    integer, intent(out), dimension(IXRT,JXRT)  :: gw_strm_msk
    integer, intent(in), dimension(IXRT,JXRT)  :: ch_netrt
    character(len=256)                      :: gwbasmskfil
    !integer,dimension(global_nX,global_ny) ::  g_GWSUBBASMSK
    !yw integer,dimension(global_rt_nx, global_rt_ny) ::  g_gw_strm_msk,g_ch_netrt

    integer,allocatable,dimension(:,:) ::  g_GWSUBBASMSK
    integer,allocatable,dimension(:, :) ::  g_gw_strm_msk,g_ch_netrt
    
     if(my_id .eq. IO_id) then
          allocate(g_gw_strm_msk(global_rt_nx, global_rt_ny))
          allocate(g_ch_netrt(global_rt_nx, global_rt_ny))
          allocate(g_GWSUBBASMSK(global_nX,global_ny))
     else
          allocate(g_gw_strm_msk(1,1))
          allocate(g_ch_netrt(1,1))
          allocate(g_GWSUBBASMSK(1,1))
     endif


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

     if(allocated(g_gw_strm_msk))  deallocate(g_gw_strm_msk)
     if(allocated(g_ch_netrt)) deallocate(g_ch_netrt)
     if(allocated(g_GWSUBBASMSK))  deallocate(g_GWSUBBASMSK)

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
    open(unit=18,file=trim(gwbasmskfil),          &
            form='formatted',status='old')

!Read in sub-basin mask...
    do j=jx,1,-1
          read (18,*) (GWSUBBASMSK(i,j),i=1,ix)
    end do
    close(18)


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

#ifdef HYDRO_D
      write(6,*) "numbasns = ", numbasns
#endif

    return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_SIMP_GW
!DJG -----------------------------------------------------

!Wei Yu
  subroutine get_gw_strm_msk_lind (ixrt,jxrt,gw_strm_msk,numbasns,basnsInd,gw_strm_msk_lind)
      implicit none
      integer, intent(in) :: ixrt,jxrt, numbasns
      integer, dimension(:,:) :: gw_strm_msk, gw_strm_msk_lind
      integer, dimension(:) :: basnsInd
      integer:: i,j,k,bas
      gw_strm_msk_lind = -999
      do j = 1, jxrt
         do i = 1, ixrt
             if(gw_strm_msk(i,j) .gt. 0) then
                  do k = 1, numbasns
                     if(gw_strm_msk(i,j) .eq. basnsInd(k)) then
                          gw_strm_msk_lind(i,j) = k
                     endif
                  end do
             end if
         end do 
      end do 
         
  end subroutine get_gw_strm_msk_lind

  subroutine SIMP_GW_IND(ix,jx,GWSUBBASMSK,numbasns,gnumbasns,basnsInd)
! create an index of basin mask so that it is faster for parallel computation.
     implicit none
     integer, intent(in) ::  ix,jx
     integer, intent(in),dimension(ix,jx) ::  GWSUBBASMSK
     integer, intent(out):: gnumbasns
     integer, intent(inout):: numbasns
     integer, intent(inout),allocatable,dimension(:):: basnsInd

     integer,dimension(numbasns):: tmpbuf

     integer :: i,j,k
     
     
     gnumbasns = numbasns
     numbasns = 0
     tmpbuf = -999.

     do j = 1,jx
        do i = 1, ix
           if(GWSUBBASMSK(i,j) .gt.0) then
                tmpbuf(GWSUBBASMSK(i,j)) = GWSUBBASMSK(i,j) 
           endif
        end do
     end do
     do k = 1, gnumbasns
         if(tmpbuf(k) .gt. 0) numbasns = numbasns + 1 
     end do

     allocate(basnsInd(numbasns))
 
     i = 1 
     do k = 1, gnumbasns
         if(tmpbuf(k) .gt. 0) then
             basnsInd(i) = tmpbuf(k)
             i = i + 1
         endif
     end do
#ifdef HYDRO_D
     write(6,*) "check numbasns, gnumbasns : ", numbasns, gnumbasns
#endif

     return
  end subroutine SIMP_GW_IND

!Wei Yu
  subroutine read_GWBUCKPARM (numbasns,gnumbasns, basnsInd, &
                 gw_buck_coeff, gw_buck_exp, z_max, &
                 z_gwsubbas, bas_id,basns_area)
! read GWBUCKPARM file

   implicit none
   integer, intent(in) :: gnumbasns, numbasns
   integer, intent(in),dimension(numbasns)  :: basnsInd
   real, intent(out),dimension(numbasns) :: gw_buck_coeff, gw_buck_exp, z_max, &
                 z_gwsubbas, basns_area
   integer, intent(out),dimension(numbasns) :: bas_id
   real, dimension(gnumbasns) :: tmp_buck_coeff, tmp_buck_exp, tmp_z_max, &
                  tmp_z_gwsubbas,  tmp_basns_area
   integer, dimension(gnumbasns) :: tmp_bas_id
   CHARACTER(len=100)                     :: header 
   CHARACTER(len=1)                       :: jnk
   integer :: bas,k

#ifdef MPP_LAND
   if(my_id .eq. IO_id) then
#endif
!Read in GW bucket params and Zinit from input file in Run directory...
#ifndef NCEP_WCOSS
     OPEN(81, FILE='GWBUCKPARM.TBL',FORM='FORMATTED',STATUS='OLD')
     read(81,811) header
#else
     OPEN(24, FORM='FORMATTED',STATUS='OLD')
     read(24,811) header
#endif
811   FORMAT(A19)


#ifndef NCEP_WCOSS
     do bas = 1,gnumbasns

! commented out the new GWBUCKPARM.TBL used for Caralina domain
!new        read(81,812) tmp_bas_id(bas),jnk,tmp_buck_coeff(bas),jnk,tmp_buck_exp(bas) , & 
!new              jnk,tmp_z_max(bas), jnk,tmp_z_gwsubbas(bas),jnk,tmp_basns_area(bas)
!new 812   FORMAT(I8,A1,F6.4,A1,F6.3,A1,F6.2,A1,F7.4,A1,F11.3)

!DJG...change bucket units to mm.... 812   FORMAT(I3,A1,F6.4,A1,F6.3,A1,F6.3,A1,F7.4)
! following is old GWBUCKPARM.TBL
       read(81,812) tmp_bas_id(bas),jnk,tmp_buck_coeff(bas),jnk,tmp_buck_exp(bas) , & 
             jnk,tmp_z_max(bas), jnk,tmp_z_gwsubbas(bas)
812   FORMAT(I8,A1,F6.4,A1,F6.3,A1,F6.2,A1,F7.4)

     end do
     close(81)
#else
     do bas = 1,gnumbasns
         read(24,812) tmp_bas_id(bas),jnk,tmp_buck_coeff(bas),jnk,tmp_buck_exp(bas) , & 
             jnk,tmp_z_max(bas), jnk,tmp_z_gwsubbas(bas)
812   FORMAT(I8,A1,F6.4,A1,F6.3,A1,F6.2,A1,F7.4)

     end do
     close(24)
#endif

#ifdef MPP_LAND
   endif

   if(gnumbasns .gt. 0 ) then
      call mpp_land_bcast_real(gnumbasns,tmp_buck_coeff)
      call mpp_land_bcast_real(gnumbasns,tmp_buck_exp  )
      call mpp_land_bcast_real(gnumbasns,tmp_z_max   )
      call mpp_land_bcast_real(gnumbasns,tmp_z_gwsubbas   )
      call mpp_land_bcast_real(gnumbasns,tmp_basns_area   )
      call mpp_land_bcast_int(gnumbasns,tmp_bas_id)
   endif
#endif

    do k = 1, numbasns
       bas = basnsInd(k)
       gw_buck_coeff(k) = tmp_buck_coeff(bas)
       gw_buck_exp(k) = tmp_buck_exp(bas)
       z_max(k) = tmp_z_max(bas)
       z_gwsubbas(k) = tmp_z_gwsubbas(bas)
       basns_area(k) = tmp_basns_area(bas)
       bas_id(k) = tmp_bas_id(bas)
    end do
  end subroutine read_GWBUCKPARM



  ! BF read the static input fields needed for the 2D GW scheme
  subroutine readGW2d(ix, jx, hc, ihead, botelv, por, ltype, ihShift)
  implicit none
#include <netcdf.inc>
  integer, intent(in) :: ix, jx
  real, intent(in) :: ihShift
  integer, dimension(ix,jx), intent(inout)::   ltype
  real, dimension(ix,jx), intent(inout)   ::   hc, ihead, botelv, por

#ifdef MPP_LAND
  integer, dimension(:,:), allocatable ::  gLtype
  real, dimension(:,:), allocatable    ::  gHC, gIHEAD, gBOTELV, gPOR
#endif
  integer :: i

  
#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
      allocate(gHC(global_rt_nx, global_rt_ny))
      allocate(gIHEAD(global_rt_nx, global_rt_ny))
      allocate(gBOTELV(global_rt_nx, global_rt_ny))
      allocate(gPOR(global_rt_nx, global_rt_ny))
      allocate(gLtype(global_rt_nx, global_rt_ny))
  else
      allocate(gHC(1, 1))
      allocate(gIHEAD(1, 1))
      allocate(gBOTELV(1, 1))
      allocate(gPOR(1, 1))
      allocate(gLtype(1, 1))
  endif
 
#ifndef PARALLELIO 
  if(my_id .eq. IO_id) then
#endif
#ifdef HYDRO_D
  print*, "2D GW-Scheme selected, retrieving files from gwhires.nc ..."
#endif
#endif


        ! hydraulic conductivity
        i = get2d_real("HC", &
#ifdef MPP_LAND
#ifndef PARALLELIO 
                       gHC, global_nx, global_ny,  &
#else
                       hc, ix, jx,  &
#endif
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
#ifndef PARALLELIO 
                       gBOTELV, global_nx, global_ny, &
#else
                       botelv, ix, jx,  &
#endif
#else
                       botelv, ix, jx,  &
#endif
                       trim("./gwhires.nc"))
                       
	! aquifer porosity
        i = get2d_real("POR", &
#ifdef MPP_LAND
#ifndef PARALLELIO 
                       gPOR, global_nx, global_ny, &
#else
                       por, ix, jx,  &
#endif
#else
                       por, ix, jx,  &
#endif
                       trim("./gwhires.nc"))


	! groundwater model mask (0 no aquifer, aquifer > 0
        call get2d_int("LTYPE", &
#ifdef MPP_LAND
#ifndef PARALLELIO 
                       gLtype, global_nx, global_ny, &
#else
                       ltype, ix, jx, &
#endif
#else
                       ltype, ix, jx,  &
#endif
                       trim("./gwhires.nc"))

 
#ifdef MPP_LAND
#ifndef PARALLELIO 
       gLtype(1,:) = 2
       gLtype(:,1) = 2
       gLtype(global_rt_nx,:) = 2
       gLtype(:,global_rt_ny) = 2 
#else
! BF TODO parallel io for gw ltype
#endif
#else
       ltype(1,:) = 2
       ltype(:,1) = 2
       ltype(ix,:)= 2
       ltype(:,jx)= 2
#endif

#ifdef MPP_LAND  
#ifndef PARALLELIO 
  endif
     call decompose_rt_int (gLtype, ltype, global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gHC,hc,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gIHEAD,ihead,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gBOTELV,botelv,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gPOR,por,global_rt_nx, global_rt_ny, ix, jx)
     if(allocated(gLtype)) deallocate(gLtype)
     if(allocated(gHC)) deallocate(gHC)
     if(allocated(gIHEAD)) deallocate(gIHEAD)
     if(allocated(gBOTELV)) deallocate(gBOTELV)
     if(allocated(gPOR)) deallocate(gPOR)
#endif
#endif
 
    
  ihead = ihead + ihShift
  
  where(ltype .eq. 0) 
   hc = 0.
!yw   por = 10**21
   por = 10E21
  end where

  
  !bftodo: make filename accessible in namelist
  return
  end subroutine readGW2d
  !BF
 
  subroutine output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
       q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
       geo_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,CHRTOUT_GRID,  &
       QBDRYRT    &
#ifdef HYDRO_REALTIME
       , iocflag &
#endif
       )

!output the routing variables over routing grid.
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
#ifdef HYDRO_REALTIME
    integer,                                  intent(in) :: iocflag
#endif
    integer,                                  intent(in) :: split_output_count
    integer,                                  intent(in) :: ixrt,jxrt
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt,jxrt,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: CHRTOUT_GRID
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: geo_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d
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
    character(len=34)  :: sec_valid_date

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

    character(len=2) :: strTmp

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
    sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                  //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

    decimation = 1 !-- decimation factor
#ifdef MPP_LAND
    ixrtd = int(global_rt_nx/decimation)
    jxrtd = int(global_rt_ny/decimation)
#else
    ixrtd = int(ixrt/decimation)
    jxrtd = int(jxrt/decimation)
#endif

#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
       allocate(xdumd(ixrtd,jxrtd))
       allocate(xcoord_d(ixrtd))
       allocate(ycoord_d(jxrtd))
       allocate(ycoord(jxrtd))

       xdumd = -999
       xcoord_d = -999 
       ycoord_d = -999
       ycoord = -999
#ifdef MPP_LAND
    else
       allocate(xdumd(1,1))
       allocate(xcoord_d(1))
       allocate(ycoord_d(1))
       allocate(ycoord(1))
    endif
#endif
    ii = 0

!DJG Dump timeseries for channel inflow accum. for calibration...(8/28/09)
    chan_in = 0.0
    do j=1,jxrt
      do i=1,ixrt
        chan_in=chan_in+QSTRMVOLRT(I,J)/1000.0*(dist(i,j,9))  !(units m^3)
      enddo
    enddo
#ifdef MPP_LAND
      call sum_real1(chan_in)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
#ifdef NCEP_WCOSS
       open (unit=54, form='formatted', status='unknown', position='append')
        write (54,713) chan_in
       close (54)
#else
#ifndef HYDRO_REALTIME
       open (unit=46,file='qstrmvolrt_accum.txt',form='formatted',&
             status='unknown',position='append')
        write (46,713) chan_in
       close (46)
#endif
#endif
#ifdef MPP_LAND
    endif
#endif
713 FORMAT (F20.7)
!    return
!DJG end dump of channel inflow for calibration....

    if (CHRTOUT_GRID.eq.0) return  ! return if hires flag eq 1, if =2 output full grid

    if (output_count == 0) then  

   !-- Open the  finemesh static files to obtain projection information
#ifdef HYDRO_D
      write(*,'("geo_finegrid_flnm: ''", A, "''")') trim(geo_finegrid_flnm)
#endif

#ifdef MPP_LAND
   if(my_id .eq. io_id) then
#endif
      iret = nf_open(trim(geo_finegrid_flnm), NF_NOWRITE, ncstatic)
#ifdef MPP_LAND
   endif
   call mpp_land_bcast_int1(iret)
#endif

      if (iret /= 0) then
         write(*,'("Problem opening geo_finegrid file: ''", A, "''")') &
         trim(geo_finegrid_flnm)
         write(*,*) "HIRES_OUTPUT will not be georeferenced..."
        hires_flag = 0
      else
        hires_flag = 1
      endif

#ifdef MPP_LAND
   if(my_id .eq. io_id) then
#endif

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord_d)
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord)
     else
      ycoord_d = 0.
      xcoord_d = 0.
     end if  !endif hires_georef 

     jj = 0
#ifdef MPP_LAND
     do j=global_rt_ny,1,-1*decimation
#else
     do j=jxrt,1,-1*decimation
#endif
        jj = jj+1
        if (jj<= jxrtd) then
         ycoord_d(jj) = ycoord(j)
        endif
     enddo

       
#ifndef HYDRO_REALTIME
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
#endif

!-- create the fine grid routing file
       write(output_flnm, '(A12,".RTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
       print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif
#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       write(6,*) "using normal netcdf file for RTOUT_DOMAIN"
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       write(6,*) "using large netcdf file for RTOUT_DOMAIN"
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif
       if (iret /= 0) then
         call hydro_stop("In output_rt() - Problem nf_create")
       endif

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
#ifndef HYDRO_REALTIME
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils
#endif

!--- define variables
!     !- time definition, timeObs
	 iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
         iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')
         iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)

#ifndef HYDRO_REALTIME
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

         do n = 1, NSOIL
             write(strTmp,'(I2)') n
             iret = nf_def_var(ncid,  "SOIL_M"//trim(strTmp),  NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
         end do
            iret = nf_put_att_text(ncid,varid,'units',7,'m^3/m^3')
            iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
            iret = nf_put_att_text(ncid,varid,'long_name',26,soilm)
!           iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
            iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
            iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"ESNOW2D",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)

!       iret = nf_def_var(ncid,"QSUBRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',15,'subsurface flow')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)
#endif

#ifdef HYDRO_REALTIME
	if ( (iocflag .ge. 0) .and. (iocflag .ne. 4) ) then
#endif
          iret = nf_def_var(ncid,"zwattablrt",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
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

       iret = nf_def_var(ncid,"sfcheadsubrt",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

#ifdef HYDRO_REALTIME
        endif
#endif

#ifndef HYDRO_REALTIME   
       iret = nf_def_var(ncid,"QSTRMVOLRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',14,'channel inflow')
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
#endif

!      iret = nf_def_var(ncid,"Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
      iret = nf_put_att_int(ncid,NF_GLOBAL,"output_decimation_factor",NF_INT, 1,decimation)

       ! iret = nf_redef(ncid)
       iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
       ! iret = nf_enddef(ncid)

      iret = nf_enddef(ncid)

#ifndef HYDRO_REALTIME
!!-- write latitude and longitude locations
         iret = nf_inq_varid(ncid,"x", varid)
         iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrtd/), xcoord_d) !-- 1-d array

         iret = nf_inq_varid(ncid,"y", varid)
         iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrtd/), ycoord_d) !-- 1-d array
#endif

#ifdef MPP_LAND
    endif
#endif

iret = nf_inq_varid(ncid,"time", varid)
iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)

#ifndef HYDRO_REALTIME
#ifdef MPP_LAND
        call write_IO_rt_real(LATVAL,xdumd)
    if( my_id .eq. io_id) then
#else
        xdumd = LATVAL
#endif
        iret = nf_inq_varid(ncid,"LATITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)


#ifdef MPP_LAND
    endif   !!! end if block of my_id .eq. io_id

        call write_IO_rt_real(LONVAL,xdumd)

    if( my_id .eq. io_id) then
#else
        xdumd = LONVAL
#endif
        iret = nf_inq_varid(ncid,"LONGITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

#ifdef MPP_LAND
    endif

    if( my_id .eq. io_id) then
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
#ifdef MPP_LAND
    endif  ! end of my_id .eq. io_id
#endif
#endif

   endif !!! end of if block output_count == 0
    output_count = output_count + 1

#ifndef HYDRO_REALTIME
!-- 3-d soils
     do n = 1, nsoil
#ifdef MPP_LAND
          call write_IO_rt_real(smcrt(:,:,n),xdumd)
#else
          xdumd(:,:) = smcrt(:,:,n)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
          write(strTmp,'(I2)') n
          iret = nf_inq_varid(ncid,  "SOIL_M"//trim(strTmp), varid)
          iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
#ifdef MPP_LAND
    endif
#endif
    enddo !-n soils
#endif

#ifdef HYDRO_REALTIME
   if ( (iocflag .ge. 0) .and. (iocflag .ne. 4)  ) then
#endif
#ifdef MPP_LAND
          call write_IO_rt_real(ZWATTABLRT,xdumd)
#else
          xdumd(:,:) = ZWATTABLRT(:,:)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
        iret = nf_inq_varid(ncid,  "zwattablrt", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
#ifdef MPP_LAND
     endif
#endif
#ifdef HYDRO_REALTIME
   endif
#endif

#ifndef HYDRO_REALTIME
#ifdef MPP_LAND
          call write_IO_rt_real(QBDRYRT,xdumd)
#else
          xdumd(:,:) = QBDRYRT(:,:)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
     iret = nf_inq_varid(ncid,  "QBDRYRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
#ifdef MPP_LAND
     endif
#endif

#ifdef MPP_LAND
          call write_IO_rt_real(QSTRMVOLRT,xdumd)
#else
          xdumd(:,:) = QSTRMVOLRT(:,:)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
     iret = nf_inq_varid(ncid,  "QSTRMVOLRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
#ifdef MPP_LAND
     endif
#endif
#endif
 
#ifdef HYDRO_REALTIME
   if ( (iocflag .ge. 0) .and. (iocflag .ne. 4) ) then
#endif
#ifdef MPP_LAND
          call write_IO_rt_real(SFCHEADSUBRT,xdumd)
#else
          xdumd(:,:) = SFCHEADSUBRT(:,:)
#endif
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
     iret = nf_inq_varid(ncid,  "sfcheadsubrt", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
#ifdef MPP_LAND
     endif
#endif
#ifdef HYDRO_REALTIME
   endif
#endif

#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif


!yw      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif
#ifdef MPP_LAND
     endif
     call mpp_land_bcast_int1(output_count)
#endif

     if(allocated(xdumd))  deallocate(xdumd)
     if(allocated(xcoord_d))  deallocate(xcoord_d)
     if(allocated(ycoord_d)) deallocate(ycoord_d)
     if(allocated(ycoord))  deallocate(ycoord)
    
#ifdef HYDRO_D 
     write(6,*) "end of output_rt" 
#endif

  end subroutine output_rt


!BF output section for gw2d model
!bftodo: clean up an customize for GW usage

  subroutine output_gw_spinup(igrid, split_output_count, ixrt, jxrt, &
       startdate, date, HEAD, convgw, excess, &
       geo_finegrid_flnm,dt,LATVAL,LONVAL,dist,output_gw)

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
    integer,                                  intent(in) ::  output_gw
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: geo_finegrid_flnm
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d, xcoord
    real*8, allocatable, DIMENSION(:)                   :: ycoord_d, ycoord

    integer, save :: ncid,ncstatic
    integer, save :: output_count

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret, dimid_soil, i,j,ii,jj
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=32)  :: convention
    character(len=34)  :: sec_since_date
    character(len=34)  :: sec_valid_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: head, convgw, excess, &
                                  latval, lonval

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    
#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: gHead, gConvgw, gExcess                                                  
    real, dimension(global_rt_nx,global_rt_ny) :: gLatval, gLonval
#endif
    
#ifdef MPP_LAND
    call MPP_LAND_COM_REAL(convgw, ixrt, jxrt, 99)
    call write_IO_rt_real(latval,gLatval)
    call write_IO_rt_real(lonval,gLonval)
    call write_IO_rt_real(head,gHead)
    call write_IO_rt_real(convgw,gConvgw)
    call write_IO_rt_real(excess,gExcess)
    

   if(my_id.eq.IO_id) then
     

#endif
    seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                  //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

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

    if (output_gw.eq.0) return  ! return if hires flag eq 0, if =1 output full grid

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

     do j=jxrtd,1,-1*decimation
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
       write(output_flnm, '(A12,".GW_SPINUP",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
       print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif


#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

       if (iret /= 0) then
         call hydro_stop("In output_gw_spinup() - Problem nf_create")
       endif

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)

!--- define variables
       !- time definition, timeObs
       iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)

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


       iret = nf_def_var(ncid,"GwHead",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',1,'m')
       iret = nf_put_att_text(ncid,varid,'long_name',17,'groundwater head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"GwConv",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'groundwater convergence')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)
       
       iret = nf_def_var(ncid,"GwExcess",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',1,'m')
       iret = nf_put_att_text(ncid,varid,'long_name',17,'surface excess groundwater')
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
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
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


    endif

    output_count = output_count + 1

!!-- time
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)


#ifdef MPP_LAND
        xdumd = gHead
#else  
        xdumd = head
#endif

     iret = nf_inq_varid(ncid,  "GwHead", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

#ifdef MPP_LAND
        xdumd = gConvgw
#else  
        xdumd = convgw
#endif
     iret = nf_inq_varid(ncid,  "GwConv", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

     
#ifdef MPP_LAND
        xdumd = gExcess
#else  
        xdumd = excess
#endif
     iret = nf_inq_varid(ncid,  "GwExcess", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
     
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     if(allocated(xdumd))  deallocate(xdumd)
     if(allocated(xcoord_d)) deallocate(xcoord_d)
     if(allocated(xcoord)) deallocate(xcoord)
     if(allocated(ycoord_d)) deallocate(ycoord_d)
     if(allocated(ycoord)) deallocate(ycoord)
    
#ifdef MPP_LAND
    endif
#endif

  end subroutine output_gw_spinup


subroutine sub_output_gw(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, HEAD, SMCRT, convgw, excess, qsgwrt, qgw_chanrt, &
       geo_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,output_gw)

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
    integer,                                  intent(in) ::  output_gw
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
    character(len=34)  :: sec_valid_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: head, convgw, excess, &
                                  qsgwrt, qgw_chanrt, &
                                  latval, lonval
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    
#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: gHead, gConvgw, gqsgwrt, gExcess, &
                                                  gQgw_chanrt
    real, dimension(global_rt_nx,global_rt_ny) :: gLatval, gLonval
    real, dimension(global_rt_nx,global_rt_ny,nsoil) :: gSMCRT
#endif
    
#ifdef MPP_LAND
    call MPP_LAND_COM_REAL(convgw, ixrt, jxrt, 99)
    call MPP_LAND_COM_REAL(qsgwrt, ixrt, jxrt, 99)
    call MPP_LAND_COM_REAL(qgw_chanrt, ixrt, jxrt, 99)
    call write_IO_rt_real(latval,gLatval)
    call write_IO_rt_real(lonval,gLonval)
    call write_IO_rt_real(qsgwrt,gqsgwrt)
    call write_IO_rt_real(qgw_chanrt,gQgw_chanrt)
    call write_IO_rt_real(head,gHead)
    call write_IO_rt_real(convgw,gConvgw)
    call write_IO_rt_real(excess,gExcess)
    
    do i = 1, NSOIL
     call MPP_LAND_COM_REAL(smcrt(:,:,i), ixrt, jxrt, 99)
     call write_IO_rt_real(SMCRT(:,:,i),gSMCRT(:,:,i))
    end do

   if(my_id.eq.IO_id) then
     

#endif
    seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                  //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

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

    if (output_gw.eq.0) return  ! return if hires flag eq 0, if =1 output full grid

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

     do j=jxrtd,1,-1*decimation
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


#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

       if (iret /= 0) then
         call hydro_stop("In output_gw_spinup() - Problem nf_create")
       endif

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils

!--- define variables
       !- time definition, timeObs
       iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)

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
!      iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
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
       
       iret = nf_def_var(ncid,"GwExcess",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',1,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',17,'surface excess groundwater')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"QSGWRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"QGW_CHANRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'m3 s-1')
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
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
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

     
#ifdef MPP_LAND
        xdumd = gExcess
#else  
        xdumd = excess
#endif
     iret = nf_inq_varid(ncid,  "GwExcess", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

     
#ifdef MPP_LAND
        xdumd = gqsgwrt
#else  
        xdumd = qsgwrt
#endif

     iret = nf_inq_varid(ncid,  "QSGWRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
     
#ifdef MPP_LAND
        xdumd = gQgw_chanrt
#else  
        xdumd = qgw_chanrt
#endif

     iret = nf_inq_varid(ncid,  "QGW_CHANRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
     
     
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     if(allocated(xdumd)) deallocate(xdumd)
     if(allocated(xcoord_d)) deallocate(xcoord_d)
     if(allocated(xcoord)) deallocate(xcoord)
     if(allocated(ycoord_d)) deallocate(ycoord_d)
     if(allocated(ycoord)) deallocate(ycoord)
    
#ifdef HYDRO_D 
     write(6,*) "end of output_ge" 
#endif
#ifdef MPP_LAND
    endif
#endif

  end subroutine sub_output_gw

!NOte: output_chrt is the old version comparing to "output_chrt_bak".
   subroutine output_chrt(igrid, split_output_count, NLINKS, ORDER,             &
        startdate, date, chlon, chlat, hlink, zelev, qlink, dtrt_ch, K,         &
        STRMFRXSTPTS, order_to_write, NLINKSL, channel_option, gages, gageMiss, & 
        lsmDt                                       &
#ifdef WRF_HYDRO_NUDGING
        , nudge                                     &
#endif
        , accLndRunOff, accQLateral, accStrmvolrt, accBucket, UDMP_OPT          &
        )
     
     implicit none
#include <netcdf.inc>
!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K,channel_option
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS, NLINKSL
     real, dimension(:),                  intent(in) :: chlon,chlat
     real, dimension(:),                  intent(in) :: hlink,zelev
     integer, dimension(:),               intent(in) :: ORDER
     integer, dimension(:),               intent(inout) :: STRMFRXSTPTS
     character(len=15), dimension(:),     intent(inout) :: gages
     character(len=15),                        intent(in) :: gageMiss
     real,                                     intent(in) :: lsmDt

     real,                                     intent(in) :: dtrt_ch
     real, dimension(:,:),                intent(in) :: qlink
#ifdef WRF_HYDRO_NUDGING
     real, dimension(:),                  intent(in) :: nudge
#endif
     
     integer, intent(in)  :: UDMP_OPT

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
     integer :: start_posO, prev_posO, nlk

     integer :: previous_pos  !-- used for the station model
     character(len=256) :: output_flnm,output_flnm2
     character(len=19)  :: date19,date19start, hydroTime
     character(len=34)  :: sec_since_date
     integer :: seconds_since,nstations,cnt,ObsStation,nobs
     character(len=32)  :: convention
     character(len=11),allocatable, DIMENSION(:)  :: stname
     character(len=15),allocatable, DIMENSION(:)  :: stnameO

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
     PARAMETER (OTXLEN = 15) ! length of example string
     INTEGER  OTIMEID        ! record dimension id
     INTEGER  OTXID          ! variable ID
     INTEGER  OTXDIMS(OTDIMS) ! variable shape
     INTEGER  OTSTART(OTDIMS), OTCOUNT(OTDIMS)

     real, dimension(:), intent(in) :: accLndRunOff, accQLateral, accStrmvolrt, accBucket  

     !! currently, this is the time of the hydro model, it's
     !! lsm time (olddate) plus one lsm timestep
     !call geth_newdate(hydroTime, date, nint(lsmDt))
     hydroTime=date

     seconds_since = int(dtrt_ch)*(K-1)

!    order_to_write = 2  !-- 1 all; 6 fewest
      nstations = 0  ! total number of channel points to display
      nobs      = 0  ! number of observation points

     if(channel_option .ne. 3) then
        nlk = NLINKSL
     else
        nlk = NLINKS
     endif


!-- output only the higher oder streamflows  and only observation points
     do i=1,nlk
        if(ORDER(i) .ge. order_to_write) nstations = nstations + 1
        if(channel_option .ne. 3) then
           if(trim(gages(i)) .ne. trim(gageMiss)) nobs = nobs + 1
        else 
           if(STRMFRXSTPTS(i) .ne. -9999) nobs = nobs + 1
        endif
     enddo

     if (nobs .eq. 0) then ! let's at least make one obs point
        nobs = 1
        if(channel_option .ne. 3) then 
           !           123456789012345
           gages(1) = '          dummy'
        else 
           STRMFRXSTPTS(1) = 1
        endif
     endif

       allocate(chanlat(nstations))
       allocate(chanlon(nstations))
       allocate(elevation(nstations))
       allocate(lOrder(nstations))
       allocate(stname(nstations))
       allocate(station_id(nstations))
       allocate(rec_num_of_station(nstations))

       allocate(chanlatO(nobs))
       allocate(chanlonO(nobs))
       allocate(elevationO(nobs))
       allocate(lOrderO(nobs))
       allocate(stnameO(nobs))
       allocate(station_idO(nobs))
       allocate(rec_num_of_stationO(nobs))

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

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif
        if (iret /= 0) then
           call hydro_stop("In output_chrt() - Problem nf_create points")
        endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm2), NF_CLOBBER, ncid2)
#else
       iret = nf_create(trim(output_flnm2), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid2)
#endif
        if (iret /= 0) then
            call hydro_stop("In output_chrt() - Problem nf_create observation")
        endif

       do i=1,nlk
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


       do i=1,nlk
          if(channel_option .ne. 3) then
             if(trim(gages(i)) .ne. trim(gageMiss)) then
                nobs = nobs + 1
                chanlatO(nobs) = chlat(i)
                chanlonO(nobs) = chlon(i)
                elevationO(nobs) = zelev(i)
                lOrderO(nobs) = ORDER(i)
                station_idO(nobs) = i
                stnameO(nobs) = gages(i)
             endif
          else 
             if(STRMFRXSTPTS(i) .ne. -9999) then 
                nobs = nobs + 1
                chanlatO(nobs) = chlat(i)
                chanlonO(nobs) = chlon(i)
                elevationO(nobs) = zelev(i)
                lOrderO(nobs) = ORDER(i)
                station_idO(nobs) = i
                write(stnameO(nobs),'(I6,"_",I1)') nobs,lOrderO(nobs)
#ifdef HYDRO_D
                !        print *,"stationobservation name",  stnameO(nobs)
#endif
             endif
          endif
       enddo 

       iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach
       iret = nf_def_dim(ncid, "station", nstations, stationdim)



       iret = nf_def_dim(ncid2, "recNum", NF_UNLIMITED, dimdataO)  !--for linked list approach
       iret = nf_def_dim(ncid2, "station", nobs, obsdim)

      !- station location definition all,  lat
        iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station latitude')
        iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')

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
        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)

        iret = nf_def_var(ncid2,"prevChild",NF_INT,1,(/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',57,'record number of the previous record for the same station')
        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)

     !-- lastChild
        iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',30,'latest report for this station')
        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)

        iret = nf_def_var(ncid2,"lastChild",NF_INT,1,(/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',30,'latest report for this station')
        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)

!     !- flow definition, var

        if(UDMP_OPT .eq. 1) then

           iret = nf_def_var(ncid, "accLndRunOff", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
           iret = nf_put_att_text(ncid,varid,'long_name',28,'ACCUMULATED runoff from land')

           iret = nf_def_var(ncid, "accQLateral", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
           iret = nf_put_att_text(ncid,varid,'long_name',24,'Total ACCUMULATED runoff')

           iret = nf_def_var(ncid, "accStrmvolrt", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
           iret = nf_put_att_text(ncid,varid,'long_name',39,'ACCUMULATED runoff from terrain routing')

           iret = nf_def_var(ncid, "accBucket", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
           iret = nf_put_att_text(ncid,varid,'long_name',32,'ACCUMULATED runoff from gw bucket')

        endif

        iret = nf_def_var(ncid, "streamflow", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',10,'River Flow')

        iret = nf_def_var(ncid2, "streamflow", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',10,'River Flow')

#ifdef WRF_HYDRO_NUDGING
        iret = nf_def_var(ncid, "nudge", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',32,'Amount of stream flow alteration')

        iret = nf_def_var(ncid2, "nudge", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',32,'Amount of stream flow alteration')
#endif

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
        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)

        iret = nf_def_var(ncid2, "order", NF_INT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',21,'Strahler Stream Order')
        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)

     !-- station  id
     ! define character-position dimension for strings of max length 11
         iret = NF_DEF_DIM(ncid, "id_len", 11, charid)
         TXDIMS(1) = charid   ! define char-string variable and position dimension first
         TXDIMS(2) = stationdim
         iret = nf_def_var(ncid,"station_id",NF_CHAR, TDIMS, TXDIMS, varid)
         iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')


         iret = NF_DEF_DIM(ncid2, "id_len", 15, charidO)
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

     open (unit=55, &
#ifndef NCEP_WCOSS
     file='frxst_pts_out.txt', &
#endif
     status='unknown',position='append')

     cnt=0
     do i=1,nlk   

       if(ORDER(i) .ge. order_to_write) then 
         start_pos = (cnt+1)+(nstations*(output_count-1))

         !!--time in seconds since startdate
          iret = nf_inq_varid(ncid,"time_observation", varid)
          iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), seconds_since)

         if(UDMP_OPT .eq. 1) then
            iret = nf_inq_varid(ncid,"accLndRunOff", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accLndRunOff(i))

            iret = nf_inq_varid(ncid,"accQLateral", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accQLateral(i))

            iret = nf_inq_varid(ncid,"accStrmvolrt", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accStrmvolrt(i))

            iret = nf_inq_varid(ncid,"accBucket", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accBucket(i))
         endif

         iret = nf_inq_varid(ncid,"streamflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlink(i,1))

#ifdef WRF_HYDRO_NUDGING
         iret = nf_inq_varid(ncid,"nudge", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), nudge(i))
#endif

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
    do i=1,nlk   
       if(channel_option .ne. 3) then
          ! jlm this verry repetitiuos, oh well.
          if(trim(gages(i)) .ne. trim(gageMiss)) then

             start_posO = (cnt+1)+(nobs * (output_count-1))
             !Write frxst_pts to text file...
             !yw          write(55,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
118          FORMAT(I8,",",A10,1X,A8,", ",A15,",",F10.5,",",F8.5,",",F9.3,",",F12.3,",",F6.3)
             !write(55,118) seconds_since, date(1:10), date(12:19), &
#ifndef HYDRO_REALTIME
             write(55,118) seconds_since, hydroTime(1:10), hydroTime(12:19), &
                  gages(i), chlon(i), chlat(i),                               &
                  qlink(i,1), qlink(i,1)*35.314666711511576, hlink(i)
#endif
             !yw 117 FORMAT(I8,1X,A25,1X,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)
             !yw 117 FORMAT(I8,1X,A10,1X,A8,1x,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)

             !!--time in seconds since startdate
             iret = nf_inq_varid(ncid2,"time_observation", varid)
             iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), seconds_since)

             iret = nf_inq_varid(ncid2,"streamflow", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), qlink(i,1))

#ifdef WRF_HYDRO_NUDGING
             iret = nf_inq_varid(ncid2,"nudge", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), nudge(i))
#endif

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

          
       else !! channel options 3 below

          if(STRMFRXSTPTS(i) .ne. -9999) then 
             start_posO = (cnt+1)+(nobs * (output_count-1))
             !Write frxst_pts to text file...
             !yw          write(55,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
117          FORMAT(I8,",",A10,1X,A8,",",I7,",",F10.5,",",F8.5,",",F9.3,",",F12.3,",",F6.3)
             !write(55,117) seconds_since,date(1:10),date(12:19),cnt,chlon(i),chlat(i), &
             !     qlink(i,1), qlink(i,1)*35.315,hlink(i)
             ! JLM: makes more sense to output the value in frxstpts incase they have meaning,
             ! as below, but I'm not going to make this change until I'm working with gridded
             ! streamflow again.
             write(55,117) seconds_since, hydroTime(1:10), hydroTime(12:19), &
                  strmfrxstpts(i), chlon(i), chlat(i),                        &
                  qlink(i,1), qlink(i,1)*35.314666711511576, hlink(i)

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

       endif

    enddo
    close(55) 

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

     if(allocated(chanlat)) deallocate(chanlat)
     if(allocated(chanlon)) deallocate(chanlon)
     if(allocated(elevation)) deallocate(elevation)
     if(allocated(station_id)) deallocate(station_id)
     if(allocated(lOrder)) deallocate(lOrder)
     if(allocated(rec_num_of_station)) deallocate(rec_num_of_station)
     if(allocated(stname)) deallocate(stname)

     if(allocated(chanlatO)) deallocate(chanlatO)
     if(allocated(chanlonO)) deallocate(chanlonO)
     if(allocated(elevationO)) deallocate(elevationO)
     if(allocated(station_idO)) deallocate(station_idO)
     if(allocated(lOrderO)) deallocate(lOrderO)
     if(allocated(rec_num_of_stationO)) deallocate(rec_num_of_stationO)
     if(allocated(stnameO)) deallocate(stnameO)
#ifdef HYDRO_D
     print *, "Exited Subroutine output_chrt"
#endif
     close(16)

20 format(i8,',',f12.7,',',f10.7,',',f6.2,',',i3)

end subroutine output_chrt

!-- output the channel route in an IDV 'station' compatible format
!Note: This version has pool output performance need to be
!solved. We renamed it from output_chrt to be output_chrt_bak.
   subroutine output_chrt_bak(igrid, split_output_count, NLINKS, ORDER,             &
        startdate, date, chlon, chlat, hlink, zelev, qlink, dtrt_ch, K,         &
        STRMFRXSTPTS, order_to_write, NLINKSL, channel_option, gages, gageMiss, & 
        lsmDt                                       &
#ifdef WRF_HYDRO_NUDGING
        , nudge                                     &
#endif
        , accLndRunOff, accQLateral, accStrmvolrt, accBucket, UDMP_OPT          &
        )
     
     implicit none
#include <netcdf.inc>
!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K,channel_option
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS, NLINKSL
     real, dimension(:),                  intent(in) :: chlon,chlat
     real, dimension(:),                  intent(in) :: hlink,zelev
     integer, dimension(:),               intent(in) :: ORDER
     integer, dimension(:),               intent(inout) :: STRMFRXSTPTS
     character(len=15), dimension(:),     intent(inout) :: gages
     character(len=15),                        intent(in) :: gageMiss
     real,                                     intent(in) :: lsmDt

     real,                                     intent(in) :: dtrt_ch
     real, dimension(:,:),                intent(in) :: qlink
#ifdef WRF_HYDRO_NUDGING
     real, dimension(:),                  intent(in) :: nudge
#endif
     
     integer, intent(in)  :: UDMP_OPT

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
     integer :: timedim, timedim2
     character(len=34) :: sec_valid_date

     integer :: iret,i, start_pos, prev_pos, order_to_write!-- order_to_write is the lowest stream order to output
     integer :: start_posO, prev_posO, nlk

     integer :: previous_pos  !-- used for the station model
     character(len=256) :: output_flnm,output_flnm2
     character(len=19)  :: date19,date19start, hydroTime
     character(len=34)  :: sec_since_date
     integer :: seconds_since,nstations,cnt,ObsStation,nobs
     character(len=32)  :: convention
     character(len=11),allocatable, DIMENSION(:)  :: stname
     character(len=15),allocatable, DIMENSION(:)  :: stnameO

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
     PARAMETER (OTXLEN = 15) ! length of example string
     INTEGER  OTIMEID        ! record dimension id
     INTEGER  OTXID          ! variable ID
     INTEGER  OTXDIMS(OTDIMS) ! variable shape
     INTEGER  OTSTART(OTDIMS), OTCOUNT(OTDIMS)

     real,dimension(:), intent(in) :: accLndRunOff, accQLateral, accStrmvolrt, accBucket  

     !! currently, this is the time of the hydro model, it's
     !! lsm time (olddate) plus one lsm timestep
     !call geth_newdate(hydroTime, date, nint(lsmDt))
     hydroTime=date

     seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
     sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                   //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

!    order_to_write = 2  !-- 1 all; 6 fewest
      nstations = 0  ! total number of channel points to display
      nobs      = 0  ! number of observation points

     if(channel_option .ne. 3) then
        nlk = NLINKSL
     else
        nlk = NLINKS
     endif


!-- output only the higher oder streamflows  and only observation points
     do i=1,nlk
        if(ORDER(i) .ge. order_to_write) nstations = nstations + 1
        if(channel_option .ne. 3) then
           if(trim(gages(i)) .ne. trim(gageMiss)) nobs = nobs + 1
        else 
           if(STRMFRXSTPTS(i) .ne. -9999) nobs = nobs + 1
        endif
     enddo

     if (nobs .eq. 0) then ! let's at least make one obs point
        nobs = 1
        if(channel_option .ne. 3) then 
           !           123456789012345
           gages(1) = '          dummy'
        else 
           STRMFRXSTPTS(1) = 1
        endif
     endif

       allocate(chanlat(nstations))
       allocate(chanlon(nstations))
       allocate(elevation(nstations))
       allocate(lOrder(nstations))
       allocate(stname(nstations))
       allocate(station_id(nstations))
       allocate(rec_num_of_station(nstations))

       allocate(chanlatO(nobs))
       allocate(chanlonO(nobs))
       allocate(elevationO(nobs))
       allocate(lOrderO(nobs))
       allocate(stnameO(nobs))
       allocate(station_idO(nobs))
       allocate(rec_num_of_stationO(nobs))

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

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif
        if (iret /= 0) then
           call hydro_stop("In output_chrt() - Problem nf_create points")
        endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm2), NF_CLOBBER, ncid2)
#else
       iret = nf_create(trim(output_flnm2), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid2)
#endif
        if (iret /= 0) then
            call hydro_stop("In output_chrt() - Problem nf_create observation")
        endif

       do i=1,nlk
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


       do i=1,nlk
          if(channel_option .ne. 3) then
             if(trim(gages(i)) .ne. trim(gageMiss)) then
                nobs = nobs + 1
                chanlatO(nobs) = chlat(i)
                chanlonO(nobs) = chlon(i)
                elevationO(nobs) = zelev(i)
                lOrderO(nobs) = ORDER(i)
                station_idO(nobs) = i
                stnameO(nobs) = gages(i)
             endif
          else 
             if(STRMFRXSTPTS(i) .ne. -9999) then 
                nobs = nobs + 1
                chanlatO(nobs) = chlat(i)
                chanlonO(nobs) = chlon(i)
                elevationO(nobs) = zelev(i)
                lOrderO(nobs) = ORDER(i)
                station_idO(nobs) = i
                write(stnameO(nobs),'(I6,"_",I1)') nobs,lOrderO(nobs)
#ifdef HYDRO_D
                !        print *,"stationobservation name",  stnameO(nobs)
#endif
             endif
          endif
       enddo 

       iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach
       iret = nf_def_dim(ncid, "station", nstations, stationdim)
       iret = nf_def_dim(ncid, "time", 1, timedim)


       iret = nf_def_dim(ncid2, "recNum", NF_UNLIMITED, dimdataO)  !--for linked list approach
       iret = nf_def_dim(ncid2, "station", nobs, obsdim)
       iret = nf_def_dim(ncid2, "time", 1, timedim2)

      !- station location definition all,  lat
        iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station latitude')
        iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')

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

        if(UDMP_OPT .eq. 1) then
           iret = nf_def_var(ncid, "accLndRunOff", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',13,'unknow')
           iret = nf_put_att_text(ncid,varid,'long_name',10,'ACCUMULATED runoff from land')

           iret = nf_def_var(ncid, "accQLateral", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',13,'unknow')
           iret = nf_put_att_text(ncid,varid,'long_name',10,'Total ACCUMULATED runoff')

           iret = nf_def_var(ncid, "accStrmvolrt", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',13,'unknow')
           iret = nf_put_att_text(ncid,varid,'long_name',10,'ACCUMULATED runoff from terrain routing')

           iret = nf_def_var(ncid, "accBucket", NF_FLOAT, 1, (/dimdata/), varid)
           iret = nf_put_att_text(ncid,varid,'units',13,'unknow')
           iret = nf_put_att_text(ncid,varid,'long_name',10,'ACCUMULATED runoff from gw bucket')
        endif

        iret = nf_def_var(ncid, "streamflow", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',10,'River Flow')

        iret = nf_def_var(ncid2, "streamflow", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',10,'River Flow')

#ifdef WRF_HYDRO_NUDGING
        iret = nf_def_var(ncid, "nudge", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',32,'Amount of stream flow alteration')

        iret = nf_def_var(ncid2, "nudge", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',32,'Amount of stream flow alteration')
#endif

!     !- flow definition, var
!       iret = nf_def_var(ncid, "pos_streamflow", NF_FLOAT, 1, (/dimdata/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'abs streamflow')

#ifndef HYDRO_REALTIME 
!     !- head definition, var
        iret = nf_def_var(ncid, "head", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid,varid,'long_name',11,'River Stage')

        iret = nf_def_var(ncid2, "head", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid2,varid,'long_name',11,'River Stage')
#endif

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


         iret = NF_DEF_DIM(ncid2, "id_len", 15, charidO)
         OTXDIMS(1) = charidO   ! define char-string variable and position dimension first
         OTXDIMS(2) = obsdim
         iret = nf_def_var(ncid2,"station_id",NF_CHAR, OTDIMS, OTXDIMS, varid)
         iret = nf_put_att_text(ncid2,varid,'long_name',14,'Observation id')


!     !- time definition, timeObs
	 iret = nf_def_var(ncid,"time",NF_INT, 1, (/timedim/), varid)
	 iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)
         iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')

	 iret = nf_def_var(ncid2,"time",NF_INT, 1, (/timedim2/), varid)
         iret = nf_put_att_text(ncid2,varid,'units',34,sec_valid_date)
         iret = nf_put_att_text(ncid2,varid,'long_name',17,'valid output time')

         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)

         convention(1:32) = "Unidata Observation Dataset v1.0"
         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
#ifndef HYDRO_REALTIME
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
#endif
         iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
         iret = nf_put_att_text(ncid, NF_GLOBAL, "station_dimension",7, "station")
         iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid, NF_GLOBAL, "stream_order_output",NF_INT,1,order_to_write)

         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "cdm_datatype",7, "Station")
#ifndef HYDRO_REALTIME
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
#endif
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "station_dimension",7, "station")
         iret = nf_put_att_real(ncid2, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid2, NF_GLOBAL, "stream_order_output",NF_INT,1,order_to_write)

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

     open (unit=55, &
#ifndef NCEP_WCOSS
     file='frxst_pts_out.txt', &
#endif
     status='unknown',position='append')

     cnt=0
     do i=1,nlk   

       if(ORDER(i) .ge. order_to_write) then 
         start_pos = (cnt+1)+(nstations*(output_count-1))

         !!--time in seconds since startdate
          iret = nf_inq_varid(ncid,"time", varid)
          iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since) 

         if(UDMP_OPT .eq. 1) then
            iret = nf_inq_varid(ncid,"accLndRunOff", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accLndRunOff(i))

            iret = nf_inq_varid(ncid,"accQLateral", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accQLateral(i))

            iret = nf_inq_varid(ncid,"accStrmvolrt", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accStrmvolrt(i))

            iret = nf_inq_varid(ncid,"accBucket", varid)
            iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), accBucket(i))
         endif

         iret = nf_inq_varid(ncid,"streamflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlink(i,1))

#ifdef WRF_HYDRO_NUDGING
         iret = nf_inq_varid(ncid,"nudge", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), nudge(i))
#endif

!        iret = nf_inq_varid(ncid,"pos_streamflow", varid)
!        iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), abs(qlink(i,1)))

#ifndef HYDRO_REALTIME
         iret = nf_inq_varid(ncid,"head", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), hlink(i))
#endif

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
    do i=1,nlk   
       if(channel_option .ne. 3) then
          ! jlm this verry repetitiuos, oh well.
          if(trim(gages(i)) .ne. trim(gageMiss)) then

             start_posO = (cnt+1)+(nobs * (output_count-1))
             !Write frxst_pts to text file...
             !yw          write(55,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
118          FORMAT(I8,",",A10,1X,A8,", ",A15,",",F10.5,",",F8.5,",",F9.3,",",F12.3,",",F6.3)
             !write(55,118) seconds_since, date(1:10), date(12:19), &
#ifndef HYDRO_REALTIME
             write(55,118) seconds_since, hydroTime(1:10), hydroTime(12:19), &
                  gages(i), chlon(i), chlat(i),                               &
                  qlink(i,1), qlink(i,1)*35.314666711511576, hlink(i)
#endif
             !yw 117 FORMAT(I8,1X,A25,1X,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)
             !yw 117 FORMAT(I8,1X,A10,1X,A8,1x,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)

             !!--time in seconds since startdate
             iret = nf_inq_varid(ncid2,"time", varid)
             iret = nf_put_vara_int(ncid2, varid, (/1/), (/1/), seconds_since)

             iret = nf_inq_varid(ncid2,"streamflow", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), qlink(i,1))

#ifdef WRF_HYDRO_NUDGING
             iret = nf_inq_varid(ncid2,"nudge", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), nudge(i))
#endif

#ifndef HYDRO_REALTIME
             iret = nf_inq_varid(ncid2,"head", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), hlink(i))
#endif

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

          
       else !! channel options 3 below

          if(STRMFRXSTPTS(i) .ne. -9999) then 
             start_posO = (cnt+1)+(nobs * (output_count-1))
             !Write frxst_pts to text file...
             !yw          write(55,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
117          FORMAT(I8,",",A10,1X,A8,",",I7,",",F10.5,",",F8.5,",",F9.3,",",F12.3,",",F6.3)
             !write(55,117) seconds_since,date(1:10),date(12:19),cnt,chlon(i),chlat(i), &
             !     qlink(i,1), qlink(i,1)*35.315,hlink(i)
             ! JLM: makes more sense to output the value in frxstpts incase they have meaning,
             ! as below, but I'm not going to make this change until I'm working with gridded
             ! streamflow again.
             write(55,117) seconds_since, hydroTime(1:10), hydroTime(12:19), &
                  strmfrxstpts(i), chlon(i), chlat(i),                        &
                  qlink(i,1), qlink(i,1)*35.314666711511576, hlink(i)

             !!--time in seconds since startdate  
             iret = nf_inq_varid(ncid2,"time", varid)
             iret = nf_put_vara_int(ncid2, varid, (/1/), (/1/), seconds_since)

             iret = nf_inq_varid(ncid2,"streamflow", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), qlink(i,1))

#ifndef HYDRO_REALTIME
             iret = nf_inq_varid(ncid2,"head", varid)
             iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), hlink(i))
#endif

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

       endif

    enddo
    close(55) 

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid,"lastChild", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), rec_num_of_station)

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid2,"lastChild", varid)
      iret = nf_put_vara_int(ncid2, varid, (/1/), (/nobs/), rec_num_of_stationO)

      iret = nf_redef(ncid)
      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(date)) = date
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))

      iret = nf_redef(ncid2)
      iret = nf_put_att_text(ncid2, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))

      iret = nf_enddef(ncid)
      iret = nf_sync(ncid)

      iret = nf_enddef(ncid2)
      iret = nf_sync(ncid2)

      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
        iret = nf_close(ncid2)
     endif

     if(allocated(chanlat))  deallocate(chanlat)
     if(allocated(chanlon))  deallocate(chanlon)
     if(allocated(elevation))  deallocate(elevation)
     if(allocated(station_id))  deallocate(station_id)
     if(allocated(lOrder))  deallocate(lOrder)
     if(allocated(rec_num_of_station))  deallocate(rec_num_of_station)
     if(allocated(stname))  deallocate(stname)

     if(allocated(chanlatO))  deallocate(chanlatO)
     if(allocated(chanlonO))  deallocate(chanlonO)
     if(allocated(elevationO))  deallocate(elevationO)
     if(allocated(station_idO))  deallocate(station_idO)
     if(allocated(lOrderO))  deallocate(lOrderO)
     if(allocated(rec_num_of_stationO))  deallocate(rec_num_of_stationO)
     if(allocated(stnameO))  deallocate(stnameO)
#ifdef HYDRO_D
     print *, "Exited Subroutine output_chrt"
#endif
     close(16)

20 format(i8,',',f12.7,',',f10.7,',',f6.2,',',i3)

end subroutine output_chrt_bak

#ifdef MPP_LAND
!-- output the channel route in an IDV 'station' compatible format
   subroutine mpp_output_chrt(gnlinks,gnlinksl,map_l2g,igrid,                  &
        split_output_count, NLINKS, ORDER,                                     &
        startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt_ch,              &
        K,STRMFRXSTPTS,order_to_write,NLINKSL,channel_option, gages, gageMiss, &
        lsmDt                                       &
#ifdef WRF_HYDRO_NUDGING
        , nudge                                     &
#endif
        , accLndRunOff, accQLateral, accStrmvolrt, accBucket, UDMP_OPT         &
        )

       USE module_mpp_land

       implicit none

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K,channel_option,NLINKSL
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(:),               intent(in) :: chlon,chlat
     real, dimension(:),                  intent(in) :: hlink,zelev

     integer, dimension(:),               intent(in) :: ORDER
     integer, dimension(:),               intent(inout) :: STRMFRXSTPTS
     character(len=15), dimension(:),     intent(inout) :: gages
     character(len=15),                   intent(in) :: gageMiss
     real,                                intent(in) :: lsmDt

     real,                                     intent(in) :: dtrt_ch
     real, dimension(:,:),                intent(in) :: qlink
#ifdef WRF_HYDRO_NUDGING
     real, dimension(:),                  intent(in) :: nudge
#endif

     integer, intent(in) :: UDMP_OPT

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

      integer  :: gnlinks, map_l2g(nlinks), order_to_write, gnlinksl
      real, allocatable,dimension(:) :: g_chlon,g_chlat, g_hlink,g_zelev
#ifdef WRF_HYDRO_NUDGING
      real, allocatable,dimension(:) :: g_nudge
#endif
      integer, allocatable,dimension(:) :: g_order,g_STRMFRXSTPTS
      real,allocatable,dimension(:,:) :: g_qlink
      integer  :: gsize
      character(len=15),allocatable,dimension(:) :: g_gages
      real,allocatable,dimension(:) :: g_accLndRunOff, g_accQLateral, g_accStrmvolrt, g_accBucket
      real, dimension(:), intent(in) :: accLndRunOff, accQLateral, accStrmvolrt, accBucket

        gsize = gNLINKS
        if(gnlinksl .gt. gsize) gsize = gnlinksl
     if(my_id .eq. io_id ) then
        allocate(g_chlon(gsize  ))
        allocate(g_chlat(gsize  ))
        allocate(g_hlink(gsize  ))
        allocate(g_zelev(gsize  ))
        allocate(g_qlink(gsize  ,2))
#ifdef WRF_HYDRO_NUDGING
        allocate(g_nudge(gsize))
#endif
        allocate(g_order(gsize  ))
        allocate(g_STRMFRXSTPTS(gsize  ))
        allocate(g_gages(gsize))

        allocate(g_accLndRunOff(gsize  ))
        allocate(g_accQLateral(gsize  ))
        allocate(g_accStrmvolrt(gsize  ))
        allocate(g_accBucket(gsize  ))

     else

        allocate(g_accLndRunOff(1))
        allocate(g_accQLateral(1))
        allocate(g_accStrmvolrt(1))
        allocate(g_accBucket(1))

        allocate(g_chlon(1))
        allocate(g_chlat(1))
        allocate(g_hlink(1))
        allocate(g_zelev(1))
        allocate(g_qlink(1,2))
#ifdef WRF_HYDRO_NUDGING
        allocate(g_nudge(1))
#endif
        allocate(g_order(1))
        allocate(g_STRMFRXSTPTS(1))
        allocate(g_gages(1))
     endif

     call mpp_land_sync()

     if(channel_option .eq. 1 .or. channel_option .eq. 2) then
        g_qlink = 0
        g_gages = gageMiss
        call ReachLS_write_io(qlink(:,1), g_qlink(:,1))
        call ReachLS_write_io(qlink(:,2), g_qlink(:,2))
#ifdef WRF_HYDRO_NUDGING
        g_nudge=0
        call ReachLS_write_io(nudge,g_nudge)
#endif
        call ReachLS_write_io(order, g_order)
        call ReachLS_write_io(chlon, g_chlon)
        call ReachLS_write_io(chlat, g_chlat)
        call ReachLS_write_io(zelev, g_zelev)
!yw This function does not work correctly for gages
!yw        call ReachLS_write_io(gages, g_gages)
        call ReachLS_write_io(STRMFRXSTPTS, g_STRMFRXSTPTS)
        call ReachLS_write_io(hlink, g_hlink)

        call ReachLS_write_io(accLndRunOff, g_accLndRunOff)
        call ReachLS_write_io(accQLateral, g_accQLateral)
        call ReachLS_write_io(accStrmvolrt, g_accStrmvolrt)
        call ReachLS_write_io(accBucket, g_accBucket)

     else
        call write_chanel_real(qlink(:,1),map_l2g,gnlinks,nlinks,g_qlink(:,1))
        call write_chanel_real(qlink(:,2),map_l2g,gnlinks,nlinks,g_qlink(:,2))
        call write_chanel_int(order,map_l2g,gnlinks,nlinks,g_order)
        call write_chanel_real(chlon,map_l2g,gnlinks,nlinks,g_chlon)
        call write_chanel_real(chlat,map_l2g,gnlinks,nlinks,g_chlat)
        call write_chanel_real(zelev,map_l2g,gnlinks,nlinks,g_zelev)
        call write_chanel_int(STRMFRXSTPTS,map_l2g,gnlinks,nlinks,g_STRMFRXSTPTS)
        call write_chanel_real(hlink,map_l2g,gnlinks,nlinks,g_hlink)
     endif


     if(my_id .eq. IO_id) then
       call output_chrt(igrid, split_output_count, GNLINKS, g_ORDER,                &
          startdate, date, g_chlon, g_chlat, g_hlink,g_zelev,g_qlink,dtrt_ch,K,     &
          g_STRMFRXSTPTS,order_to_write,gNLINKSL,channel_option, g_gages, gageMiss, &
          lsmDt                                                                     &
#ifdef WRF_HYDRO_NUDGING
          , g_nudge                                     &
#endif
          , g_accLndRunOff, g_accQLateral, g_accStrmvolrt, g_accBucket, UDMP_OPT    &
          )

    end if
     call mpp_land_sync()
    if(allocated(g_order)) deallocate(g_order)
    if(allocated(g_STRMFRXSTPTS)) deallocate(g_STRMFRXSTPTS)
    if(allocated(g_chlon)) deallocate(g_chlon)
    if(allocated(g_chlat)) deallocate(g_chlat)
    if(allocated(g_hlink)) deallocate(g_hlink)
    if(allocated(g_zelev)) deallocate(g_zelev)
    if(allocated(g_qlink)) deallocate(g_qlink)
    if(allocated(g_gages)) deallocate(g_gages)
#ifdef WRF_HYDRO_NUDGING
    if(allocated(g_nudge)) deallocate(g_nudge)
#endif
    if(allocated(g_accLndRunOff)) deallocate(g_accLndRunOff)
    if(allocated(g_accQLateral)) deallocate(g_accQLateral)
    if(allocated(g_accStrmvolrt)) deallocate(g_accStrmvolrt)
    if(allocated(g_accBucket)) deallocate(g_accBucket)

end subroutine mpp_output_chrt

!---------  lake netcdf output -----------------------------------------
!-- output the ilake info an IDV 'station' compatible format -----------
   subroutine mpp_output_lakes(lake_index,igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt_ch,K)

   USE module_mpp_land

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt_ch

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
           qlakei,qlakeo, resht,dtrt_ch,K)
     end if
     call mpp_land_sync()
     return
     end subroutine mpp_output_lakes

   subroutine mpp_output_lakes2(lake_index,igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt_ch,K, LAKEIDM)

   USE module_mpp_land

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt_ch
     integer, dimension(NLAKES),               intent(in) :: LAKEIDM     ! lake id

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
        call output_lakes2(igrid, split_output_count, NLAKES, &
           startdate, date, latlake, lonlake, elevlake, &
           qlakei,qlakeo, resht,dtrt_ch,K, LAKEIDM)
     end if
     call mpp_land_sync()
     return
     end subroutine mpp_output_lakes2
#endif

!----------------------------------- lake netcdf output
!-- output the ilake info an IDV 'station' compatible format
   subroutine output_lakes(igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt_ch,K)

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt_ch

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
     integer :: timedim
     character(len=34) :: sec_valid_date

    !--- all this for writing the station id string
     INTEGER   TDIMS, TXLEN
     PARAMETER (TDIMS=2)    ! number of TX dimensions
     PARAMETER (TXLEN = 6) ! length of example string
     INTEGER  TIMEID        ! record dimension id
     INTEGER  TXID          ! variable ID
     INTEGER  TXDIMS(TDIMS) ! variable shape
     INTEGER  TSTART(TDIMS), TCOUNT(TDIMS)

!    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
!    seconds_since = int(dtrt_ch)*output_count
     seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
     sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                     //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'


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

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

      if (iret /= 0) then
          call hydro_stop("In output_lakes() - Problem nf_create")
      endif

      do i=1,NLAKES
         station_id(i) = i
         write(stname(i),'(I6)') i
      enddo 

      iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach
      iret = nf_def_dim(ncid, "station", nlakes, stationdim)
      iret = nf_def_dim(ncid, "time", 1, timedim)

#ifndef HYDRO_REALTIME
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
#endif

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
      iret = nf_put_att_text(ncid,varid,'long_name',23,'Water Surface Elevation')

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
         iret = nf_def_var(ncid,"time", NF_INT, 1, (/timedim/), varid) 
         iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)
         iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')

!       date19(1:19) = "0000-00-00_00:00:00"
!       date19(1:len_trim(startdate)) = startdate
!       iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
!
        date19(1:19) = "0000-00-00_00:00:00"
        date19(1:len_trim(startdate)) = startdate
        convention(1:32) = "Unidata Observation Dataset v1.0"
        iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
        iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
#ifndef HYDRO_REALTIME
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
#endif
        iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
        iret = nf_put_att_text(ncid, NF_GLOBAL, "station_dimension",7, "station")
!!       iret = nf_put_att_text(ncid, NF_GLOBAL, "observation_dimension",6, "recNum")
!!        iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coordinate",16,"time_observation")
        iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
        iret = nf_enddef(ncid)

#ifndef HYDRO_REALTIME
        !-- write latitudes
        iret = nf_inq_varid(ncid,"latitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LATLAKE)

        !-- write longitudes
        iret = nf_inq_varid(ncid,"longitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LONLAKE)

        !-- write physical height of lake
        iret = nf_inq_varid(ncid,"altitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), elevlake)
#endif

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

     iret = nf_inq_varid(ncid,"time", varid)
     iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)

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
    iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
    iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
    iret = nf_enddef(ncid)

    iret = nf_sync(ncid)
     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

     if(allocated(station_id)) deallocate(station_id)
     if(allocated(rec_num_of_lake)) deallocate(rec_num_of_lake)
     if(allocated(stname)) deallocate(stname)
#ifdef HYDRO_D
     print *, "Exited Subroutine output_lakes"
#endif
     close(16)

 end subroutine output_lakes

!----------------------------------- lake netcdf output
!-- output the lake as regular netcdf file format for better performance than point netcdf file.
   subroutine output_lakes2(igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt_ch,K,LAKEIDM)

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     integer, dimension(NLAKES),               intent(in) :: LAKEIDM        !-- LAKE ID
     real,                                     intent(in) :: dtrt_ch

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date


     integer, save  :: output_count
     integer, save :: ncid

     integer :: stationdim, varid,  n
     integer :: iret,i    !-- 
     character(len=256) :: output_flnm
     character(len=19)  :: date19, date19start
     character(len=32)  :: convention
     integer :: timedim
     integer :: seconds_since
     character(len=34) :: sec_valid_date
     sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                         //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'


     seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))

     if (output_count == 0) then

      date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

      write(output_flnm, '(A12,".LAKEOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
      print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

      if (iret /= 0) then
          print*, "Problem nf_create" 
          call hydro_stop("output_lakes") 
      endif 

      iret = nf_def_dim(ncid, "station", nlakes, stationdim)

      iret = nf_def_dim(ncid, "time", 1, timedim)

#ifndef HYDRO_REALTIME
      !- station location definition,  lat
      iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake latitude')
      iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')
#endif

      !- station location definition,  LAKEIDM
      iret = nf_def_var(ncid,"lake_id",NF_INT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake COMMON ID')

#ifndef HYDRO_REALTIME
      !- station location definition,  long
      iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake longitude')
      iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')

!     !-- lake's phyical elevation
      iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake altitude')
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')
#endif

!     !- water surface elevation
      iret = nf_def_var(ncid, "elevation", NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')
      iret = nf_put_att_text(ncid,varid,'long_name',23,'Water Surface Elevation')

!     !- inflow to lake
      iret = nf_def_var(ncid, "inflow", NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

!     !- outflow to lake
      iret = nf_def_var(ncid, "outflow", NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

      ! Time variable
      iret = nf_def_var(ncid, "time", NF_INT, 1, (/timeDim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)
      iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')

        date19(1:19) = "0000-00-00_00:00:00"
        date19(1:len_trim(startdate)) = startdate
#ifndef HYDRO_REALTIME
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
#endif
        iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
        iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
        iret = nf_enddef(ncid)

        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)

#ifndef HYDRO_REALTIME
        !-- write latitudes
        iret = nf_inq_varid(ncid,"latitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LATLAKE)

        !-- write longitudes
        iret = nf_inq_varid(ncid,"longitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LONLAKE)

        !-- write physical height of lake
        iret = nf_inq_varid(ncid,"altitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), elevlake)
#endif

        !-- write elevation  of lake
        iret = nf_inq_varid(ncid,"elevation", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), resht   )

        !-- write elevation  of inflow
        iret = nf_inq_varid(ncid,"inflow", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), qlakei  )

        !-- write elevation  of inflow
        iret = nf_inq_varid(ncid,"outflow", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), qlakeo  )

        !-- write lake id
        iret = nf_inq_varid(ncid,"lake_id", varid)
        iret = nf_put_vara_int(ncid, varid, (/1/), (/NLAKES/), LAKEIDM)

     endif

     output_count = output_count + 1


    iret = nf_redef(ncid)
  
    iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
    iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
    iret = nf_enddef(ncid)

    iret = nf_sync(ncid)
     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

 end subroutine output_lakes2
!----------------------------------- lake netcdf output

#ifdef MPP_LAND

!-- output the channel route in an IDV 'grid' compatible format
   subroutine mpp_output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETLNK_in, startdate, date, &
        qlink, dt, geo_finegrid_flnm, gnlinks,map_l2g,g_ixrt,g_jxrt )

   USE module_mpp_land

     implicit none
#include <netcdf.inc>
     integer g_ixrt,g_jxrt
     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(:,:),                intent(in) :: qlink
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETLNK_in
     character(len=*),          intent(in)     :: geo_finegrid_flnm
     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

     integer::  gnlinks , map_l2g(nlinks)

     integer, allocatable,dimension(:,:)         :: CH_NETLNK
     real, allocatable,dimension(:,:)                :: g_qlink

     if(my_id .eq. io_id) then
        allocate(CH_NETLNK(g_IXRT,g_JXRT))
        allocate(g_qlink(gNLINKS,2) )
     else
        allocate(CH_NETLNK(1,1))
        allocate(g_qlink(1,2) )
     endif

     call write_chanel_real(qlink(:,1),map_l2g,gnlinks,nlinks,g_qlink(:,1))
     call write_chanel_real(qlink(:,2),map_l2g,gnlinks,nlinks,g_qlink(:,2))

     call write_IO_rt_int(CH_NETLNK_in, CH_NETLNK)

    if(my_id.eq.IO_id) then
        call  output_chrtgrd(igrid, split_output_count, g_ixrt,g_jxrt, &
           GNLINKS, CH_NETLNK, startdate, date, &
           g_qlink, dt, geo_finegrid_flnm)
    endif
    
     if(allocated(g_qlink)) deallocate(g_qlink)
     if(allocated(CH_NETLNK)) deallocate(CH_NETLNK)
     return
     end subroutine mpp_output_chrtgrd
#endif

!-- output the channel route in an IDV 'grid' compatible format
   subroutine output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS, CH_NETLNK, startdate, date, &
        qlink, dt, geo_finegrid_flnm)

     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(:,:),                intent(in) :: qlink
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETLNK
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
    integer :: timedim2
    character(len=34) :: sec_valid_date

    integer :: iret,i,j
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=34)  :: sec_since_date
 

    integer :: seconds_since

    seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
    sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                 //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'


      tmpflow = -9E15

 
        write(output_flnm, '(A12,".CHRTOUT_GRID",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
#ifdef HYDRO_D
        print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif
 

!--- define dimension
#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

        if (iret /= 0) then
            call hydro_stop("In output_chrtgrd() - Problem nf_create ")
        endif

        iret = nf_def_dim(ncid, "time", NF_UNLIMITED, timedim)
        iret = nf_def_dim(ncid, "x", ixrt, ixlondim)
        iret = nf_def_dim(ncid, "y", jxrt, jxlatdim)

!--- define variables
!     !- time definition, timeObs

       !- x-coordinate in cartesian system
!yw         iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/ixlondim/), varid)
!yw         iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
!yw         iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
!yw         iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
!yw         iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/jxlatdim/), varid)
!yw         iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
!yw         iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
!yw         iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

!     !- flow definition, var
        iret = nf_def_var(ncid,"streamflow",NF_REAL, 3, (/ixlondim,jxlatdim,timedim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
        iret = nf_put_att_text(ncid,varid,'long_name',15,'water flow rate')
        iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
        iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)
        iret = nf_def_var(ncid,"index",NF_INT, 2, (/ixlondim,jxlatdim/), varid)
        iret = nf_def_var(ncid, "time", NF_INT, 1, (/timedim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')
        iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)


!-- place prjection information


      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate)) 

      iret = nf_enddef(ncid)

      iret = nf_inq_varid(ncid,"time", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)

!!-- write latitude and longitude locations

!DJG inv    do j=jxrt,1,-1
    do j=1,jxrt
     do i=1,ixrt
       if(CH_NETLNK(i,j).GE.0) then
         tmpflow(i,j) = qlink(CH_NETLNK(i,j),1) 
       else
         tmpflow(i,j) = -9E15
       endif
     enddo
    enddo

!!time in seconds since startdate
    iret = nf_inq_varid(ncid,"index", varid)
    iret = nf_put_vara_int(ncid, varid, (/1,1/), (/ixrt,jxrt/),CH_NETLNK)

    iret = nf_inq_varid(ncid,"streamflow", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ixrt,jxrt,1/),tmpflow)

        iret = nf_close(ncid)



 end subroutine output_chrtgrd


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
       write(*,'("READFORC_chan Problem opening netcdf file: ''", A, "''")') trim(inflnm)
       call hydro_stop("In read_chan_forcing() - Problem opening netcdf file")
    endif

!DJG read data...
    call get_2d_netcdf("QSTRMVOLRT",  ncid, QSTRMVOLRT_ACC, units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)

    ierr = nf_close(ncid)

 end subroutine read_chan_forcing



 subroutine get2d_int(var_name,out_buff,ix,jx,fileName, fatalErr)
    implicit none
#include <netcdf.inc>
    integer :: iret,varid,ncid,ix,jx
    integer out_buff(ix,jx)
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: fileName
    logical, optional, intent(in) :: fatalErr
    logical :: fatalErr_local
    character(len=256) :: errMsg

    fatalErr_local = .false.
    if(present(fatalErr)) fatalErr_local=fatalErr
    
    iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
    if (iret .ne. 0) then
       errMsg = "get2d_int: failed to open the netcdf file: " // trim(fileName)
       print*, trim(errMsg)
       if(fatalErr_local) call hydro_stop(trim(errMsg))
    endif
    
    iret = nf_inq_varid(ncid,trim(var_name),  varid)
    if(iret .ne. 0) then
       errMsg = "get2d_int: failed to find the variable: " // &
                 trim(var_name) // ' in ' // trim(fileName)
       print*, trim(errMsg)
       if(fatalErr_local) call hydro_stop(errMsg)
    endif
    
    iret = nf_get_var_int(ncid, varid, out_buff)
    if(iret .ne. 0) then
       errMsg = "get2d_int: failed to read the variable: " // &
                trim(var_name) // " in " //trim(fileName)
       print*,trim(errMsg)
       if(fatalErr_local) call hydro_stop(trim(errMsg))
    endif
    
    iret = nf_close(ncid)
    if(iret .ne. 0) then
       errMsg = "get2d_int: failed to close the file: " // &
                trim(fileName)
       print*,trim(errMsg)
       if(fatalErr_local) call hydro_stop(trim(errMsg))
    endif
    
    return
  end subroutine get2d_int


#ifdef MPP_LAND
      SUBROUTINE MPP_READ_ROUTEDIM(did,g_IXRT,g_JXRT, GCH_NETLNK,GNLINKS,IXRT,JXRT, &
            route_chan_f,route_link_f, &
            route_direction_f, NLINKS, &
            CH_NETLNK, channel_option, geo_finegrid_flnm, NLINKSL, UDMP_OPT)

         USE module_mpp_land

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: channel_option, did
        INTEGER                                      :: g_IXRT,g_JXRT
        INTEGER, INTENT(INOUT)                       :: NLINKS, GNLINKS,NLINKSL
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: GCH_NETLNK  !- each node gets unique id based on global domain
        ! INTEGER, DIMENSION(g_IXRT,g_JXRT) :: g_CH_NETLNK  ! temp array
        INTEGER, allocatable,DIMENSION(:,:) :: g_CH_NETLNK  ! temp array
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON
        INTEGER, INTENT(IN)                          :: UDMP_OPT
        integer:: i,j

        CHARACTER(len=256)       :: route_chan_f, route_link_f,route_direction_f
        CHARACTER(len=256)       :: geo_finegrid_flnm
!       CHARACTER(len=*)       :: geo_finegrid_flnm

!       integer, allocatable, dimension(:) :: tmp_int
        integer :: ywcount



        if(my_id .eq. IO_id) then
           allocate(g_CH_NETLNK(g_IXRT,g_JXRT))
           g_CH_NETLNK = -9999
           CALL READ_ROUTEDIM(g_IXRT, g_JXRT, route_chan_f, route_link_f, &
              route_direction_f, GNLINKS, &
              g_CH_NETLNK, channel_option,geo_finegrid_flnm,NLINKSL, UDMP_OPT)
           call get_NLINKSL(NLINKSL, channel_option, route_link_f)
        else
           allocate(g_CH_NETLNK(1,1))
        endif

        call mpp_land_bcast_int1(GNLINKS)
        call mpp_land_bcast_int1(NLINKSL)


        call decompose_RT_int(g_CH_NETLNK,GCH_NETLNK,g_IXRT,g_JXRT,ixrt,jxrt)
        if(allocated(g_CH_NETLNK)) deallocate(g_CH_NETLNK)
        ywcount = 0 
        CH_NETLNK = -9999
        do j = 1, jxrt
           do i = 1, ixrt
                  if(GCH_NETLNK(i,j) .gt. 0) then
                       ywcount = ywcount + 1
                       CH_NETLNK(i,j) = ywcount
                  endif
           end do
        end do
        NLINKS = ywcount


!ywcheck
!        CH_NETLNK = GCH_NETLNK


        allocate(rt_domain(did)%map_l2g(NLINKS))

        rt_domain(did)%map_l2g = -1
        do j = 1, jxrt
           do i = 1, ixrt
              if(CH_NETLNK(i,j) .gt. 0) then
                  rt_domain(did)%map_l2g(CH_NETLNK(i,j)) = GCH_NETLNK(i,j)
              endif
           end do       
        end do       

        call mpp_chrt_nlinks_collect(NLINKS)
        return 

      end SUBROUTINE MPP_READ_ROUTEDIM




#endif
        
      SUBROUTINE READ_ROUTING_seq(IXRT,JXRT,ELRT,CH_NETRT,CH_LNKRT,LKSATFAC,route_topo_f,    &
            route_chan_f, geo_finegrid_flnm,OVROUGHRTFAC,RETDEPRTFAC,channel_option, UDMP_OPT)


#include <netcdf.inc>
        INTEGER, INTENT(IN) :: IXRT,JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: ELRT,LKSATFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT,CH_LNKRT
!Dummy inverted grids
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: RETDEPRTFAC

        integer         :: I,J, iret, jj, channel_option, UDMP_OPT
        CHARACTER(len=256)        :: var_name
        CHARACTER(len=*)       :: route_topo_f
        CHARACTER(len=*)       :: route_chan_f
        CHARACTER(len=*)       :: geo_finegrid_flnm

        var_name = "TOPOGRAPHY"

        call nreadRT2d_real(var_name,ELRT,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))

     IF(channel_option .ne. 3 .and. UDMP_OPT .ne. 1) then  !get maxnodes and links from grid
        var_name = "LINKID"
        call nreadRT2d_int(var_name,CH_LNKRT,ixrt,jxrt,&
               trim(geo_finegrid_flnm), fatalErr=.true.)
     endif


       
#ifdef HYDRO_D
        write(6,*) "read linkid grid CH_LNKRT ",var_name
#endif

!!!DY to be fixed ... 6/27/08
!        var_name = "BED_ELEVATION"
!        iret = get2d_real(var_name,ELRT,ixrt,jxrt,&
!                     trim(geo_finegrid_flnm))

        var_name = "CHANNELGRID"
        call nreadRT2d_int(var_name,CH_NETRT,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

        var_name = "LKSATFAC"
        LKSATFAC = -9999.9
        call nreadRT2d_real(var_name,LKSATFAC,ixrt,jxrt,&
               trim(geo_finegrid_flnm))

#ifdef HYDRO_D
        write(6,*) "read ",var_name
#endif

           where (LKSATFAC == -9999.9) LKSATFAC = 1000.0  !specify LKSAFAC if no term avail...


!1.12.2012...Read in routing calibration factors...
        var_name = "RETDEPRTFAC"
        call nreadRT2d_real(var_name,RETDEPRTFAC,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (RETDEPRTFAC < 0.) RETDEPRTFAC = 1.0  ! reset grid to = 1.0 if non-valid value exists

        var_name = "OVROUGHRTFAC"
        call nreadRT2d_real(var_name,OVROUGHRTFAC,ixrt,jxrt,&
                     trim(geo_finegrid_flnm))
        where (OVROUGHRTFAC <= 0.) OVROUGHRTFAC = 1.0 ! reset grid to = 1.0 if non-valid value exists


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
    integer :: iret, n
    character(len=2) tmpStr 



#ifdef MPP_LAND
     if(IO_id.eq.my_id) &
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(outFile), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(outFile), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

#ifdef MPP_LAND
       call mpp_land_bcast_int1(iret)
#endif

       if (iret /= 0) then
          call hydro_stop("In output_lsm() - Problem nf_create")
       endif


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
          do n = 1, nlst_rt(did)%nsoil
             if( n .lt. 10) then
                write(tmpStr, '(i1)') n
             else
                write(tmpStr, '(i2)') n
             endif
             iret = nf_def_var(ncid,"stc"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
             iret = nf_def_var(ncid,"smc"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
             iret = nf_def_var(ncid,"sh2ox"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          end do

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
   integer :: n
   character(len=2) :: tmpStr
   character(len=*) outFile

    integer :: ncid,irt, dimid_ix, dimid_jx,  &
             dimid_ixrt, dimid_jxrt, varid, &
             dimid_links, dimid_basns, dimid_soil, dimid_lakes
    integer :: iret


#ifdef MPP_LAND
     if(IO_id.eq.my_id) &
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(outFile), NF_CLOBBER, ncid)
#ifdef HYDRO_D
       write(6,*) "yyywww do not use large netcdf file definition. "
       call flush(6)
#endif
#else
       iret = nf_create(trim(outFile), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#ifdef HYDRO_D
       write(6,*) "yyywww using large netcdf file definition. "
       call flush(6)
#endif
#endif


#ifdef MPP_LAND
       call mpp_land_bcast_int1(iret)
#endif

       if (iret /= 0) then
          call hydro_stop("In RESTART_OUT_nc() - Problem nf_create")
       endif

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

          if(nlst_rt(did)%channel_option .eq. 3) then
              iret = nf_def_dim(ncid, "links", rt_domain(did)%gnlinks, dimid_links)
           else
              iret = nf_def_dim(ncid, "links", rt_domain(did)%gnlinksl, dimid_links)
           endif
           iret = nf_def_dim(ncid, "basns", rt_domain(did)%gnumbasns, dimid_basns)
           if(rt_domain(did)%nlakes .gt. 0) then
                  iret = nf_def_dim(ncid, "lakes", rt_domain(did)%nlakes, dimid_lakes)
           endif

!define variables
          do n = 1, nlst_rt(did)%nsoil
             if( n .lt. 10) then
                write(tmpStr, '(i1)') n
             else
                write(tmpStr, '(i2)') n
             endif
             iret = nf_def_var(ncid,"stc"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
             iret = nf_def_var(ncid,"smc"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
             iret = nf_def_var(ncid,"sh2ox"//trim(tmpStr),NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          end do
    
          iret = nf_def_var(ncid,"smcmax1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcref1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcwlt1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"infxsrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"soldrain",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"sfcheadrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)

          if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            iret = nf_def_var(ncid,"QBDRYRT",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"infxswgt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"sfcheadsubrt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
          do n = 1, nlst_rt(did)%nsoil
             if( n .lt. 10) then
                write(tmpStr, '(i1)') n
             else
                write(tmpStr, '(i2)') n
             endif
             iret = nf_def_var(ncid,"sh2owgt"//trim(tmpStr),NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
          end do
            iret = nf_def_var(ncid,"qstrmvolrt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"RETDEPRT",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)






            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              iret = nf_def_var(ncid,"hlink",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"qlink1",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"qlink2",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"cvol",NF_FLOAT,1,(/dimid_links/),varid)
              if(rt_domain(did)%nlakes .gt. 0) then
                  iret = nf_def_var(ncid,"resht",NF_FLOAT,1,(/dimid_lakes/),varid)
                  iret = nf_def_var(ncid,"qlakeo",NF_FLOAT,1,(/dimid_lakes/),varid)
              endif
              iret = nf_def_var(ncid,"lake_inflort",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
              if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                     iret = nf_def_var(ncid,"accLndRunOff",NF_FLOAT,1,(/dimid_links/),varid)
                     iret = nf_def_var(ncid,"accQLateral",NF_FLOAT,1,(/dimid_links/),varid)
                     iret = nf_def_var(ncid,"accStrmvolrt",NF_FLOAT,1,(/dimid_links/),varid)
                     iret = nf_def_var(ncid,"accBucket",NF_FLOAT,1,(/dimid_links/),varid)
              endif
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then

                 if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                     iret = nf_def_var(ncid,"z_gwsubbas",NF_FLOAT,1,(/dimid_links/),varid)
                 else
                     iret = nf_def_var(ncid,"z_gwsubbas",NF_FLOAT,1,(/dimid_basns/),varid)
                 endif

!yw test bucket model
!             iret = nf_def_var(ncid,"gwbas_pix_ct",NF_FLOAT,1,(/dimid_basns/),varid)
!             iret = nf_def_var(ncid,"gw_buck_exp",NF_FLOAT,1,(/dimid_basns/),varid)
!             iret = nf_def_var(ncid,"z_max",NF_FLOAT,1,(/dimid_basns/),varid)
!             iret = nf_def_var(ncid,"gw_buck_coeff",NF_FLOAT,1,(/dimid_basns/),varid)
!             iret = nf_def_var(ncid,"qin_gwsubbas",NF_FLOAT,1,(/dimid_basns/),varid)
!             iret = nf_def_var(ncid,"qinflowbase",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
!             iret = nf_def_var(ncid,"qout_gwsubbas",NF_FLOAT,1,(/dimid_basns/),varid)
            end if
            if(nlst_rt(did)%gwBaseSwCRT .eq. 3)then
            iret = nf_def_var(ncid,"HEAD",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            end if  
	  end if    
	      
!         put global attribute
          iret = nf_put_att_int(ncid,NF_GLOBAL,"his_out_counts",NF_INT, 1,rt_domain(did)%his_out_counts)
          iret = nf_put_att_text(ncid,NF_GLOBAL,"Restart_Time",19,nlst_rt(did)%olddate(1:19))
          iret = nf_put_att_text(ncid,NF_GLOBAL,"Since_Date",19,nlst_rt(did)%sincedate(1:19))
          iret = nf_put_att_real(ncid,NF_GLOBAL,"DTCT",NF_REAL, 1,nlst_rt(did)%DTCT)
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
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%soldrain,"soldrain"  ) 
       call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%sfcheadrt,"sfcheadrt"  ) 


        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QBDRYRT, "QBDRYRT" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT, "infxswgt" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%SFCHEADSUBRT, "SFCHEADSUBRT" )
            call w_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_rt(did)%nsoil,rt_domain(did)%SH2OWGT, "sh2owgt" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT, "qstrmvolrt" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%RETDEPRT, "RETDEPRT" )

!yw test


!yw test


            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              if(nlst_rt(did)%channel_option .eq. 3) then
                    call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%HLINK,"hlink" &
#ifdef MPP_LAND
                            ,rt_domain(did)%map_l2g, rt_domain(did)%gnlinks  &
#endif
                          )
              else
                    call w_rst_crt_reach(ncid,rt_domain(did)%HLINK, "hlink"  &
#ifdef MPP_LAND
                            ,rt_domain(did)%gnlinksl&
#endif
                          )
              endif

              if(nlst_rt(did)%channel_option .eq. 3) then
                     call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%QLINK(:,1),"qlink1" &
#ifdef MPP_LAND
                        ,rt_domain(did)%map_l2g, rt_domain(did)%gnlinks  &
#endif
                         )
              else
                    call w_rst_crt_reach(ncid,rt_domain(did)%QLINK(:,1), "qlink1"  &
#ifdef MPP_LAND   
                            ,rt_domain(did)%gnlinksl &
#endif
                          )
              endif



              if(nlst_rt(did)%channel_option .eq. 3) then
                    call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%QLINK(:,2),"qlink2" &
#ifdef MPP_LAND
                       ,rt_domain(did)%map_l2g, rt_domain(did)%gnlinks  &
#endif
                        )
              else
                    call w_rst_crt_reach(ncid,rt_domain(did)%QLINK(:,2), "qlink2"  &
#ifdef MPP_LAND   
                            ,rt_domain(did)%gnlinksl &
#endif
                          )
                    if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                        call w_rst_crt_reach(ncid,rt_domain(did)%accLndRunOff, "accLndRunOff"  &
#ifdef MPP_LAND   
                                ,rt_domain(did)%gnlinksl &
#endif
                              )
                        call w_rst_crt_reach(ncid,rt_domain(did)%accQLateral, "accQLateral"  &
#ifdef MPP_LAND   
                                ,rt_domain(did)%gnlinksl &
#endif
                              )
                        call w_rst_crt_reach(ncid,rt_domain(did)%accStrmvolrt, "accStrmvolrt"  &
#ifdef MPP_LAND   
                                ,rt_domain(did)%gnlinksl &
#endif
                              )
                        call w_rst_crt_reach(ncid,rt_domain(did)%accBucket, "accBucket"  &
#ifdef MPP_LAND   
                                ,rt_domain(did)%gnlinksl &
#endif
                              )
                    endif   ! end if of UDMP_OPT .eq. 1
              endif

              


              if(nlst_rt(did)%channel_option .eq. 3) then
                     call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%cvol,"cvol" &
#ifdef MPP_LAND
                        ,rt_domain(did)%map_l2g, rt_domain(did)%gnlinks  &
#endif
                         )
              else
                    call w_rst_crt_reach(ncid,rt_domain(did)%cvol, "cvol"  &
#ifdef MPP_LAND   
                            ,rt_domain(did)%gnlinksl &
#endif
                          )
              endif


!              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%resht,"resht" &
!#ifdef MPP_LAND
!                 ,rt_domain(did)%map_l2g, rt_domain(did)%gnlinks  &
!#endif
!                  )


              call w_rst_crt_nc1_lake(ncid,rt_domain(did)%nlakes,rt_domain(did)%resht,"resht" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index  &
#endif
                  )

              call w_rst_crt_nc1_lake(ncid,rt_domain(did)%nlakes,rt_domain(did)%qlakeo,"qlakeo" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index  &
#endif
                  )


              call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")

            end if


            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
!              call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%z_gwsubbas,"z_gwsubbas" )
              if(nlst_rt(did)%UDMP_OPT .eq. 1) then

                    call w_rst_crt_reach(ncid,rt_domain(did)%z_gwsubbas, "z_gwsubbas"  &
#ifdef MPP_LAND   
                            ,rt_domain(did)%gnlinksl  &
#endif
                          )
              else
                  call w_rst_gwbucket_real(ncid,rt_domain(did)%numbasns,rt_domain(did)%gnumbasns, &
                       rt_domain(did)%basnsInd, rt_domain(did)%z_gwsubbas,"z_gwsubbas" )
              endif
!yw test bucket model
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%gwbas_pix_ct,"gwbas_pix_ct" )
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%gw_buck_exp,"gw_buck_exp" )
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%z_max,"z_max" )
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%gw_buck_coeff,"gw_buck_coeff" )
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%qin_gwsubbas,"qin_gwsubbas" )
!             call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%qinflowbase,"qinflowbase")
!             call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%qout_gwsubbas,"qout_gwsubbas" )
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.3) then
	      call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,gw2d(did)%ho, "HEAD" )
            end if
        end if  

#ifdef MPP_LAND
        if(IO_id.eq.my_id) &
#endif
        iret = nf_close(ncid)

        return
        end subroutine RESTART_OUT_nc

#ifdef MPP_LAND

   subroutine RESTART_OUT_bi(outFile,did)


   implicit none

   integer did

   character(len=*) outFile

    integer :: iunit
    integer  :: i0,ie, i, istep, mkdirStatus
    

    call mpp_land_sync()

    iunit = 81
 istep = 64
 i0 = 0
 ie = istep
 do i = 0, numprocs,istep 
   if(my_id .ge. i0 .and. my_id .lt. ie) then
     open(iunit, file = "restart/"//trim(outFile), form="unformatted",ERR=101, access="sequential")
          write(iunit,ERR=101) rt_domain(did)%his_out_counts
!         write(iunit,ERR=101) nlst_rt(did)%olddate(1:19)
          write(iunit,ERR=101) nlst_rt(did)%sincedate(1:19)
!         write(iunit,ERR=101) nlst_rt(did)%DTCT 
          write(iunit,ERR=101) rt_domain(did)%stc
          write(iunit,ERR=101) rt_domain(did)%smc
          write(iunit,ERR=101) rt_domain(did)%sh2ox
          write(iunit,ERR=101) rt_domain(did)%SMCMAX1
          write(iunit,ERR=101) rt_domain(did)%SMCREF1
          write(iunit,ERR=101) rt_domain(did)%SMCWLT1
          write(iunit,ERR=101) rt_domain(did)%INFXSRT
          write(iunit,ERR=101) rt_domain(did)%soldrain
          write(iunit,ERR=101) rt_domain(did)%sfcheadrt

          if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
                if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
                   write(iunit,ERR=101) rt_domain(did)%HLINK
                   write(iunit,ERR=101) rt_domain(did)%QLINK(:,1)
                   write(iunit,ERR=101) rt_domain(did)%QLINK(:,2)
                   write(iunit,ERR=101) rt_domain(did)%cvol
                   write(iunit,ERR=101) rt_domain(did)%resht
                   write(iunit,ERR=101) rt_domain(did)%qlakeo
                   write(iunit,ERR=101) rt_domain(did)%LAKE_INFLORT
                end if
                if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
                     write(iunit,ERR=101) rt_domain(did)%z_gwsubbas
                end if
                if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1) then
                    write(iunit,ERR=101) rt_domain(did)%QBDRYRT
                    write(iunit,ERR=101) rt_domain(did)%INFXSWGT
                    write(iunit,ERR=101) rt_domain(did)%SFCHEADSUBRT
                    write(iunit,ERR=101) rt_domain(did)%SH2OWGT
                    write(iunit,ERR=101) rt_domain(did)%QSTRMVOLRT
                    write(iunit,ERR=101) rt_domain(did)%RETDEPRT
                endif
          end if  

        close(iunit)
    endif
    call mpp_land_sync()
    i0 = i0 + istep
    ie = ie + istep
  end do ! end do of i loop

        return
101     continue
        call hydro_stop("FATAL ERROR: failed to output the hydro restart file.")
        end subroutine RESTART_OUT_bi

   subroutine RESTART_in_bi(inFileTmp,did)


   implicit none

   integer did

   character(len=*) inFileTmp
   character(len=256) inFile
   character(len=19) str_tmp

    integer :: iunit
    logical :: fexist
    integer  :: i0,ie, i, istep

    iunit = 81

             if(my_id .lt. 10) then
                write(str_tmp,'(I1)') my_id
             else if(my_id .lt. 100) then
                write(str_tmp,'(I2)') my_id
             else if(my_id .lt. 1000) then
                write(str_tmp,'(I3)') my_id
             else if(my_id .lt. 10000) then
                write(str_tmp,'(I4)') my_id
             else if(my_id .lt. 100000) then
                write(str_tmp,'(I5)') my_id
             endif

    inFile = trim(inFileTmp)//"."//str_tmp 

    inquire (file=trim(inFile), exist=fexist)
    if(.not. fexist) then
        call hydro_stop("In RESTART_in_bi()- Could not find restart file "//trim(inFile))
    endif

 istep = 64
 i0 = 0
 ie = istep
 do i = 0, numprocs,istep 
   if(my_id .ge. i0 .and. my_id .lt. ie) then
    open(iunit, file = inFile, form="unformatted",ERR=101,access="sequential")
          read(iunit,ERR=101) rt_domain(did)%his_out_counts
!         read(iunit,ERR=101) nlst_rt(did)%olddate(1:19)
          read(iunit,ERR=101) nlst_rt(did)%sincedate(1:19)
!         read(iunit,ERR=101) nlst_rt(did)%DTCT 
          read(iunit,ERR=101) rt_domain(did)%stc
          read(iunit,ERR=101) rt_domain(did)%smc
          read(iunit,ERR=101) rt_domain(did)%sh2ox
          read(iunit,ERR=101) rt_domain(did)%SMCMAX1
          read(iunit,ERR=101) rt_domain(did)%SMCREF1
          read(iunit,ERR=101) rt_domain(did)%SMCWLT1
          read(iunit,ERR=101) rt_domain(did)%INFXSRT
          read(iunit,ERR=101) rt_domain(did)%soldrain
          read(iunit,ERR=101) rt_domain(did)%sfcheadrt
          if(nlst_rt(did)%SUBRTSWCRT.EQ.0.and.nlst_rt(did)%OVRTSWCRT.EQ.0) rt_domain(did)%sfcheadrt = 0

          if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
                if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
                   read(iunit,ERR=101) rt_domain(did)%HLINK
                   read(iunit,ERR=101) rt_domain(did)%QLINK(:,1)
                   read(iunit,ERR=101) rt_domain(did)%QLINK(:,2)
                   read(iunit,ERR=101) rt_domain(did)%cvol
                   read(iunit,ERR=101) rt_domain(did)%resht
                   read(iunit,ERR=101) rt_domain(did)%qlakeo
                   read(iunit,ERR=101) rt_domain(did)%LAKE_INFLORT
                end if
                if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
                     read(iunit,ERR=101) rt_domain(did)%z_gwsubbas
                end if
                if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1) then
                   read(iunit,ERR=101) rt_domain(did)%QBDRYRT
                   read(iunit,ERR=101) rt_domain(did)%INFXSWGT
                   read(iunit,ERR=101) rt_domain(did)%SFCHEADSUBRT
                   read(iunit,ERR=101) rt_domain(did)%SH2OWGT
                   read(iunit,ERR=101) rt_domain(did)%QSTRMVOLRT
                   !read(iunit,ERR=101) rt_domain(did)%RETDEPRT
                endif
          end if  

        close(iunit)
    endif
    call mpp_land_sync()
    i0 = i0 + istep
    ie = ie + istep
  end do ! end do of i loop

        return
101     continue
        call hydro_stop("In RESTART_in_bi() - failed to read the hydro restart file "//trim(inFile))
        end subroutine RESTART_in_bi
#endif

        subroutine w_rst_rt_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real, dimension(ix,jx):: inVar
#ifdef MPP_LAND
           real, allocatable, dimension(:,:) :: varTmp 
           if(my_id .eq. io_id ) then
               allocate(varTmp(global_rt_nx, global_rt_ny))
           else
               allocate(varTmp(1,1))
           endif
           call write_IO_rt_real(inVar,varTmp) 
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_rt_nx,global_rt_ny/),varTmp)
           endif
           if(allocated(varTmp))  deallocate(varTmp)
#else
           iret = nf_inq_varid(ncid,varName, varid)
           if(iret .eq. 0) then
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),inVar)
           else
              write(6,*) "Error : variable not defined in rst file before write: ", varName
           endif
#endif
           
           return
        end subroutine w_rst_rt_nc2

        subroutine w_rst_rt_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real,dimension(ix,jx,nsoil):: inVar
           character(len=2) tmpStr
           integer k
#ifdef MPP_LAND
           real varTmp(global_rt_nx,global_rt_ny)
           do k = 1, nsoil
              call write_IO_rt_real(inVar(:,:,k),varTmp(:,:)) 
              if(my_id .eq. IO_id) then
                 if( k .lt. 10) then
                    write(tmpStr, '(i1)') k
                 else
                    write(tmpStr, '(i2)') k
                 endif
                 iret = nf_inq_varid(ncid,varName//trim(tmpStr), varid)
                 iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_rt_nx,global_rt_ny/),varTmp)
              endif
           end do
#else
           do k = 1, nsoil
                 if( k .lt. 10) then
                    write(tmpStr, '(i1)') k
                 else
                    write(tmpStr, '(i2)') k
                 endif
              iret = nf_inq_varid(ncid,varName//trim(tmpStr), varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/),(/ix,jx/),inVar(:,:,k)) 
           end do 
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
           character(len=2) tmpStr
           
#ifdef MPP_LAND
           real varTmp(global_nx,global_ny)
           do k = 1, nsoil
              call write_IO_real(inVar(:,:,k),varTmp(:,:)) 
              if(my_id .eq. IO_id) then
                 if( k .lt. 10) then
                    write(tmpStr, '(i1)') k
                 else
                    write(tmpStr, '(i2)') k
                 endif
                iret = nf_inq_varid(ncid,varName//trim(tmpStr), varid)
                iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_nx,global_ny/),varTmp)
              endif
           end do
#else
           do k = 1, nsoil
                 if( k .lt. 10) then
                    write(tmpStr, '(i1)') k
                 else
                    write(tmpStr, '(i2)') k
                 endif
             iret = nf_inq_varid(ncid,varName//trim(tmpStr), varid)
             iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),inVar(:,:,k))
           end do 
#endif
           return
        end subroutine w_rst_nc3

        subroutine w_rst_crt_nc1_lake(ncid,n,inVar,varName &
#ifdef MPP_LAND
                 ,nodelist     &
#endif
                  )
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           integer:: nodelist(n)
           if(n .eq. 0) return

           call write_lake_real(inVar,nodelist,n)          
           if(my_id .eq. IO_id) then
#endif
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1_lake

        subroutine w_rst_crt_reach(ncid,inVar,varName &
#ifdef MPP_LAND
                 , gnlinksl&
#endif
                  )
           implicit none
           integer:: ncid,varid , iret, n   
           character(len=*) varName
           real, dimension(:) :: inVar
    
#ifdef MPP_LAND
           integer:: gnlinksl
           real,allocatable,dimension(:) :: g_var
           if(my_id .eq. io_id) then
                allocate(g_var(gnlinksl))
                g_var  = 0
           else
                allocate(g_var(1) )
           endif
  
           call ReachLS_write_io(inVar, g_var)
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/gnlinksl/),g_var)
           endif
           if(allocated(g_var)) deallocate(g_var)
#else
           n = size(inVar,1) 
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#endif
           return
        end subroutine w_rst_crt_reach

        subroutine w_rst_crt_nc1(ncid,n,inVar,varName &
#ifdef MPP_LAND
                 ,map_l2g, gnlinks&
#endif
                  )
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           integer:: gnlinks, map_l2g(n)
           real g_var(gnlinks)
           call write_chanel_real(inVar,map_l2g,gnlinks,n,g_var)          
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/gnlinks/),g_var)
#else
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#endif
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1

        subroutine w_rst_crt_nc1g(ncid,n,inVar,varName)
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real,dimension(:) ::  inVar
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

   subroutine w_rst_gwbucket_real(ncid,numbasns,gnumbasns, &
                       basnsInd, inV,vName )
      implicit none
      integer :: ncid,numbasns,gnumbasns
      integer, dimension(:) :: basnsInd
      real, dimension(:) :: inV 
      character(len=*) :: vName
      integer i, j, k
      real, allocatable,dimension(:) :: buf
#ifdef MPP_LAND
      if (my_id .eq. IO_id) then 
        allocate(buf(gnumbasns))
      else
        allocate(buf(1))
      endif
      call gw_write_io_real(numbasns,inV,basnsInd,buf)
#else
      allocate(buf(gnumbasns))
      do k = 1, numbasns
        buf(basnsInd(k)) = inV(k)
      end do
#endif
      call w_rst_crt_nc1g(ncid,gnumbasns,buf,vName)
      if(allocated(buf)) deallocate(buf)
   end subroutine w_rst_gwbucket_real

   subroutine read_rst_gwbucket_real(ncid,outV,numbasns,&
                       gnumbasns,basnsInd, vName)
      implicit none
      integer :: ncid,numbasns,gnumbasns
      integer, dimension(:) :: basnsInd
      real, dimension(:) :: outV 
      character(len=*) :: vName
      integer i, j,k
      real, dimension(gnumbasns) :: buf
      call read_rst_crt_nc(ncid,buf,gnumbasns,vName)
      do k = 1, numbasns
         outV(k) = buf(basnsInd(k))
      end do
   end subroutine read_rst_gwbucket_real

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
       write(*,'("Problem opening file: ''", A, "''")') &
            trim(inFile)
       call hydro_stop("In RESTART_IN_NC() - Problem opening file") 
    endif

#ifdef MPP_LAND
     if(IO_id .eq. my_id) then
#endif
        iret = NF_GET_ATT_INT(ncid, NF_GLOBAL, 'his_out_counts', rt_domain(did)%his_out_counts) 
        iret = NF_GET_ATT_REAL(ncid, NF_GLOBAL, 'DTCT', nlst_rt(did)%DTCT)
        iret = nf_get_att_text(ncid,NF_GLOBAL,"Since_Date",nlst_rt(did)%sincedate(1:19))
        if(iret /= 0) nlst_rt(did)%sincedate = nlst_rt(did)%startdate
        if(nlst_rt(did)%DTCT .gt. 0) then
           nlst_rt(did)%DTCT = min(nlst_rt(did)%DTCT, nlst_rt(did)%DTRT_CH)
        else
           nlst_rt(did)%DTCT = nlst_rt(did)%DTRT_CH
        endif
#ifdef MPP_LAND
    endif
    call mpp_land_bcast_int1(rt_domain(did)%out_counts)
    call mpp_land_bcast_real1(nlst_rt(did)%DTCT)
#endif

#ifdef HYDRO_D
     write(6,*) "nlst_rt(did)%nsoil=",nlst_rt(did)%nsoil
#endif

     if(nlst_rt(did)%rst_typ .eq. 1 ) then 
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%stc,"stc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%smc,"smc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_rt(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%sfcheadrt,"sfcheadrt")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%soldrain,"soldrain")
     endif

!yw check
 
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1")


        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) then
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT,"infxswgt")
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%SFCHEADSUBRT,"SFCHEADSUBRT")
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QBDRYRT,"QBDRYRT")
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT,"qstrmvolrt")
            !call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%RETDEPRT,"RETDEPRT")
            call read_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_rt(did)%nsoil,rt_domain(did)%SH2OWGT,"sh2owgt")


            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
                if(nlst_rt(did)%channel_option .eq. 3) then
                    call read_rst_crt_stream_nc(ncid,rt_domain(did)%HLINK,rt_domain(did)%NLINKS,"hlink",rt_domain(did)%GNLINKS,rt_domain(did)%map_l2g)
                    call read_rst_crt_stream_nc(ncid,rt_domain(did)%QLINK(:,1),rt_domain(did)%NLINKS,"qlink1",rt_domain(did)%GNLINKS,rt_domain(did)%map_l2g)
                    call read_rst_crt_stream_nc(ncid,rt_domain(did)%QLINK(:,2),rt_domain(did)%NLINKS,"qlink2",rt_domain(did)%GNLINKS,rt_domain(did)%map_l2g)
                    call read_rst_crt_stream_nc(ncid,rt_domain(did)%CVOL,rt_domain(did)%NLINKS,"cvol",rt_domain(did)%GNLINKS,rt_domain(did)%map_l2g)
                else
                    call read_rst_crt_reach_nc(ncid,rt_domain(did)%HLINK,"hlink",rt_domain(did)%GNLINKSL)
                    call read_rst_crt_reach_nc(ncid,rt_domain(did)%QLINK(:,1),"qlink1",rt_domain(did)%GNLINKSL)
                    call read_rst_crt_reach_nc(ncid,rt_domain(did)%QLINK(:,2),"qlink2",rt_domain(did)%GNLINKSL)
                    call read_rst_crt_reach_nc(ncid,rt_domain(did)%CVOL,"cvol",rt_domain(did)%GNLINKSL)
                    if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                       ! read in the statistic value
                       call read_rst_crt_reach_nc(ncid,rt_domain(did)%accLndRunOff,"accLndRunOff",rt_domain(did)%GNLINKSL)
                       call read_rst_crt_reach_nc(ncid,rt_domain(did)%accQLateral,"accQLateral",rt_domain(did)%GNLINKSL)
                       call read_rst_crt_reach_nc(ncid,rt_domain(did)%accStrmvolrt,"accStrmvolrt",rt_domain(did)%GNLINKSL)
                       call read_rst_crt_reach_nc(ncid,rt_domain(did)%accBucket,"accBucket",rt_domain(did)%GNLINKS)
                    endif
                endif

              if(rt_domain(did)%NLAKES .gt. 0) then
                 call read_rst_crt_nc(ncid,rt_domain(did)%RESHT,rt_domain(did)%NLAKES,"resht")
                 call read_rst_crt_nc(ncid,rt_domain(did)%QLAKEO,rt_domain(did)%NLAKES,"qlakeo")
              endif
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")

            end if

            if(nlst_rt(did)%GWBASESWCRT.EQ.1.AND.nlst_rt(did)%GW_RESTART.NE.0 .and. rt_domain(did)%gnumbasns .gt. 0) then
               if(nlst_rt(did)%UDMP_OPT .eq. 1) then
                   call read_rst_crt_reach_nc(ncid,rt_domain(did)%z_gwsubbas,"z_gwsubbas",rt_domain(did)%GNLINKSL)
               else 
                   call read_rst_gwbucket_real(ncid,rt_domain(did)%z_gwsubbas,rt_domain(did)%numbasns,&
                       rt_domain(did)%gnumbasns,rt_domain(did)%basnsInd, "z_gwsubbas")
               endif
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.3) then
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,gw2d(did)%ho,"HEAD")
	    end if
        end if

          if(nlst_rt(did)%rstrt_swc.eq.1) then  !Switch for rest of restart accum vars...
#ifdef HYDRO_D
            print *, "1 Resetting RESTART Accumulation Variables to 0...",nlst_rt(did)%rstrt_swc
#endif
            rt_domain(did)%INFXSRT=0.
            rt_domain(did)%LAKE_INFLORT=0.
            rt_domain(did)%QSTRMVOLRT=0.
            rt_domain(did)%accLndRunOff = 0.
            rt_domain(did)%accQLateral = 0.
            rt_domain(did)%accStrmvolrt = 0.
            rt_domain(did)%accBucket = 0.
          end if

      
#ifdef MPP_LAND
        if(my_id .eq. IO_id) &
#endif
        iret =  nf_close(ncid) 
#ifdef HYDRO_D
        write(6,*) "end of RESTART_IN"
        call flush(6)
#endif

        return
        end subroutine RESTART_IN_nc

      subroutine read_rst_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none 
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
         character(len=2) :: tmpStr
         integer :: n
         integer i
#ifdef MPP_LAND
         real,dimension(global_nx,global_ny) :: xtmp
#endif

         do i = 1, nsoil
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
#endif
                 if( i .lt. 10) then
                    write(tmpStr, '(i1)') i
                 else
                    write(tmpStr, '(i2)') i
                 endif
           iret = nf_inq_varid(ncid,  trim(varStr)//trim(tmpStr),  varid)
#ifdef MPP_LAND
         endif
         call mpp_land_bcast_int1(iret)
#endif

         if (iret /= 0) then
#ifdef HYDRO_D
            print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
            return
         endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr//trim(tmpStr)
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
            iret = nf_get_var_real(ncid, varid, xtmp)

            call decompose_data_real(xtmp(:,:), var(:,:,i))
#else
            iret = nf_get_var_real(ncid, varid, var(:,:,i))
#endif
         end do

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
            var = 0.0
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_nc2

      subroutine read_rst_rt_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
         character(len=2) :: tmpStr
         integer i
#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny) :: xtmp
#endif
         do i = 1, nsoil
                 if( i .lt. 10) then
                    write(tmpStr, '(i1)') i
                 else
                    write(tmpStr, '(i2)') i
                 endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
#endif
            iret = nf_inq_varid(ncid,  trim(varStr)//trim(tmpStr),  varid)
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
         print*, "read restart variable ", varStr//trim(tmpStr)
#endif
#ifdef MPP_LAND
         iret = nf_get_var_real(ncid, varid, xtmp)
            call decompose_RT_real(xtmp(:,:),var(:,:,i),global_rt_nx,global_rt_ny,ix,jx)
#else
         iret = nf_get_var_real(ncid, varid, var(:,:,i))
#endif
         end do
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
         real,allocatable, dimension(:,:) :: xtmp
!yw         real,dimension(global_rt_nx,global_rt_ny) :: xtmp
         if(my_id .eq. io_id ) then
             allocate(xtmp(global_rt_nx,global_rt_ny))
         else
             allocate(xtmp(1,1))
         endif
         xtmp = 0.0
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

         if(allocated(xtmp)) deallocate(xtmp)

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
        
         if( n .le. 0)  return
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
         if(n .gt. 0) then
             call mpp_land_bcast_real(n,var)
         endif
#endif
         return
      end subroutine read_rst_crt_nc 

      subroutine read_rst_crt_stream_nc(ncid,var_out,n,varStr,gnlinks,map_l2g)
         implicit none
         integer ::  ncid, varid, n, iret, gnlinks
         integer, intent(in), dimension(:) :: map_l2g
         character(len=*) :: varStr
         integer :: l, g
         real,intent(out) , dimension(:) ::  var_out
#ifdef MPP_LAND
         real,dimension(gnlinks) ::  var
#else
         real,dimension(n) ::  var
#endif


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
            var = 0.0
            iret = nf_get_var_real(ncid, varid, var)
#ifdef MPP_LAND
         endif
         if(gnlinks .gt. 0) then
            call mpp_land_bcast_real(gnlinks,var)
         endif
        
         if(n .le. 0) return
         var_out = 0

         do l = 1, n
            g = map_l2g(l)
            var_out(l) = var(g)
         end do
#else
         var_out = var
#endif
         return
      end subroutine read_rst_crt_stream_nc 

      subroutine read_rst_crt_reach_nc(ncid,var_out,varStr,gnlinksl)
         implicit none
         integer ::  ncid, varid, n, iret, gnlinksl
         character(len=*) :: varStr
         integer :: l, g
         real, dimension(:) ::  var_out

         real,allocatable,dimension(:) ::  var

         n = size(var_out,1)

#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
              allocate(var(gnlinksl))
         else
              allocate(var(1))
         endif
#else
              allocate(var(n))
#endif
         

#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
         endif
         call mpp_land_bcast_int1(iret)
         if (iret /= 0) then
#ifdef HYDRO_D
               print*, 'variable not found: name = "', trim(varStr)//'"'
#endif

              if(allocated(var))  deallocate(var)
               return
            endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
         call flush(6)
#endif
         if(my_id .eq. IO_id) then
            var = 0.0
            iret = nf_get_var_real(ncid, varid, var)
         endif
         call ReachLS_decomp(var,   var_out)
         if(allocated(var)) deallocate(var)
#else
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
           if (iret /= 0) then
#ifdef HYDRO_D
               print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
               if(allocated(var)) deallocate(var)
               return
            endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif   
         iret = nf_get_var_real(ncid, varid, var_out)
         if(allocated(var)) deallocate(var)
#endif


         return
      end subroutine read_rst_crt_reach_nc 

      subroutine hrldas_out()
      end subroutine hrldas_out


       SUBROUTINE READ_CHROUTING1(IXRT,JXRT,fgDEM,CH_NETRT,CH_LNKRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            LAKEIDA, HRZAREA, LAKEMAXH,WEIRH,  WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, LAKEIDM, LAKEIDX, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,LATVAL,LONVAL, &
            STRMFRXSTPTS,geo_finegrid_flnm , NLINKSL, LINKID, GNLINKSL,UDMP_OPT     &
#ifdef MPP_LAND
            ,Link_Location &
#endif
            ,gages, gageMiss)
#ifdef MPP_LAND
        use module_mpp_land, only:  my_id, io_id
#endif
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT, UDMP_OPT
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES, NLINKSL, GNLINKSL
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: fgDEM
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION
        INTEGER, DIMENSION(IXRT,JXRT)                :: GSTRMFRXSTPTS
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETRT, CH_LNKRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER,                DIMENSION(IXRT,JXRT) :: GORDER  !-- gridded stream orderk
#ifdef MPP_LAND
        INTEGER,                DIMENSION(IXRT,JXRT) :: Link_Location !-- gridded stream orderk
        INTEGER :: LNLINKSL
!yw        INTEGER,                dimension(LNLINKSL) :: LLINKID
#endif
        INTEGER                                      :: I,J,K,channel_option
        REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)      :: LATVAL, LONVAL
        CHARACTER(len=28)                            :: dir
!Dummy inverted grids from arc


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
        REAL, INTENT(INOUT),  DIMENSION(:,:)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE,LINKID   ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK
        REAL,  DIMENSION(IXRT,JXRT)                  :: ChSSlpG,BwG,MannNG  !channel properties on Grid
        REAL,  DIMENSION(IXRT,JXRT)                  :: chanDepth, elrt


!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        REAL, INTENT(INOUT),  DIMENSION(:)      :: HRZAREA
        INTEGER, INTENT(INOUT),  DIMENSION(:)   :: LAKEIDM    !lake id for LAKES_Modeled in the LAKEPARM table (.nc or .tbl)

        REAL, INTENT(INOUT),  DIMENSION(:)           :: LAKEMAXH, WEIRH
        REAL, INTENT(INOUT),  DIMENSION(:)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(:)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(:)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(:)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(:)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(:)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(:)       :: ChSSlp, Bw

        INTEGER, INTENT(INOUT),  DIMENSION(:)  :: LAKEIDA !the COM lake id for each link on the full nlinks database
        INTEGER, INTENT(INOUT),  DIMENSION(:)  :: LAKEIDX !the sequential index of lakes (1 to Nlakes) mapped to COMID

        INTEGER, DIMENSION(NLAKES,NLINKSL)           :: tmpTO      !a variable to hold hold the to of links for Lake Outlet Iding
        INTEGER, DIMENSION(NLAKES)                   :: LAKELINKID !temporarily store the outlet index for each modeled lake


        character(len=15), intent(inout), dimension(nlinks) :: gages  !! need to respect the default values
        character(len=15), intent(in)                :: gageMiss

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: geo_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid, iret, jj,ct
	INTEGER 			             :: IOstatus, OUTLAKEID

        real                                         :: gc,n
        integer :: did

        did = 1
        
!---------------------------------------------------------
! End Declarations
!---------------------------------------------------------

        LAKEIDX  = -999
        MAXORDER = -9999
        LAKELINKID = 0
!initialize GSTRM
        GSTRMFRXSTPTS = -9999

!yw initialize the array.
        to_node =   MAXORDER
        from_node = MAXORDER
#ifdef MPP_LAND
        Link_location = MAXORDER
#endif

#ifdef HYDRO_D
        print *, "reading routing initialization files..."
        print *, "route direction", route_direction_f
        print *, "route order", route_order_f
        print *, "route linke",route_link_f
        print *, "route lake",route_lake_f
#endif

!DJG Edited code here to retrieve data from hires netcdf file....

!!-- read regardless; commented out on 7/21/14

        var_name = "LATITUDE"
        call nreadRT2d_real  (   &
             var_name,LATVAL,ixrt,jxrt,trim(geo_finegrid_flnm))

        var_name = "LONGITUDE"
        call nreadRT2d_real(   &
             var_name,LONVAL,ixrt,jxrt,trim(geo_finegrid_flnm))

       var_name = "LAKEGRID"
       call nreadRT2d_int(&
             var_name,LAKE_MSKRT,ixrt,jxrt,trim(geo_finegrid_flnm))

       var_name = "FLOWDIRECTION"
       call nreadRT2d_int(& 
              var_name,DIRECTION,ixrt,jxrt,trim(geo_finegrid_flnm))

       var_name = "STREAMORDER"
       call nreadRT2d_int(&
               var_name,GORDER,ixrt,jxrt,trim(geo_finegrid_flnm))


       var_name = "frxst_pts"
       call nreadRT2d_int(&
             var_name,GSTRMFRXSTPTS,ixrt,jxrt,trim(geo_finegrid_flnm))

!!!Flip y-dimension of highres grids from exported Arc files...

        var_name = "CHAN_DEPTH"
        call nreadRT2d_real(   &
             var_name,chanDepth,ixrt,jxrt,trim(geo_finegrid_flnm))

       if(nlst_rt(did)%GWBASESWCRT .eq. 3) then
             elrt = fgDEM - chanDepth
       else
             elrt = fgDEM     !ywtmp
       endif
       
       ct = 0
     
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do


!- read the grid regardless of routing method
   cnt = 0 
   BwG = 0.0
   ChSSlpG = 0.0
   MannNG = 0.0
   TYPEL = 0
   MannN = 0.0
   Bw = 0.0
   ChSSlp = 0.0

        if (UDMP_OPT .eq. 1) goto 299

!DJG inv       DO j = JXRT,1,-1  !rows
       DO j = 1,JXRT  !rows
        DO i = 1 ,IXRT   !colsumns
         If (CH_NETRT(i, j) .ge. 0) then !get its direction and assign its elevation and order
          If ((DIRECTION(i, j) .EQ. 64) .AND. (j + 1 .LE. JXRT) ) then !North
             if(CH_NETRT(i,j+1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j + 1 .LE. JXRT)  ) then !North East
             if(CH_NETRT(i+1,j+1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT) ) then !East
             if(CH_NETRT(i+1,j).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0)  ) then !south east
             if(CH_NETRT(i+1,j-1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .NE. 0) ) then !due south
             if(CH_NETRT(i,j-1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
               .AND. (j - 1 .NE. 0) ) then !south west
             if(CH_NETRT(i-1,j-1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0) ) then !West
             if(CH_NETRT(i-1,j).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT)  ) then !North West
             if(CH_NETRT(i-1,j+1).ge.0) then
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
             endif
          else 
#ifdef HYDRO_D
!            print *, "NO MATCH", i,j,CH_NETLNK(i,j),DIRECTION(i,j),i + 1,j - 1 !south east
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

           If (DIRECTION(i, j).EQ. 64) then
              if( j + 1 .GT. JXRT)  then         !-- 64's can only flow north
                 goto 101
              elseif ( CH_NETRT(i,j+1) .lt. 0) then !North
                 goto 101
              endif
              goto 102
101           continue
#ifdef MPP_LAND
                cnt = CH_NETLNK(i,j)
#else
                cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
                Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!                print *, "Pour Point N", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
102           continue

           else if ( DIRECTION(i, j) .EQ. 128) then
               if ((i + 1 .GT. IXRT)  & !-- 128's can flow out of the North or East edge
               .OR.  (j + 1 .GT. JXRT))  then !   this is due north edge
                   goto 201
               elseif (CH_NETRT(i + 1, j + 1).lt.0) then !North East
                   goto 201
               endif
!#endif
               goto 202 
201            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point NE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
202            continue

           else if (DIRECTION(i, j) .EQ. 1) then 
               if(i + 1 .GT. IXRT) then     !-- 1's can only flow due east
                  goto 301
               elseif(CH_NETRT(i + 1, j) .lt. 0) then !East
                  goto 301
               endif
               goto 302
301            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point E", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
302            continue
           else if (DIRECTION(i, j) .EQ. 2) then 
                if((i + 1 .GT. IXRT)    &      !-- 2's can flow out of east or south edge
                   .OR.  (j - 1 .EQ. 0))   then              !-- this is the south edge
                   goto 401
                elseif (CH_NETRT(i + 1, j - 1) .lt.0) then !south east
                   goto 401
                endif
              goto 402
401            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point SE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
402            continue

           else if (DIRECTION(i, j) .EQ. 4)  then
               if(j - 1 .EQ. 0) then         !-- 4's can only flow due south
                   goto 501
               elseif (CH_NETRT(i, j - 1) .lt. 0) then !due south
                   goto 501
               endif
               goto 502
501            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point S", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
502            continue

          else if ( DIRECTION(i, j) .EQ. 8) then
               if( (i - 1 .LE. 0)      &      !-- 8's can flow south or west
                  .OR.  (j - 1 .EQ. 0)) then        !-- this is the south edge
                   goto 601
               elseif (CH_NETRT(i - 1, j - 1).lt.0) then !south west
                   goto 601
               endif
               goto 602
601            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point SW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
602            continue
           else if (DIRECTION(i, j) .EQ. 16) then
               if( i - 1 .LE.0) then                 !16's can only flow due west
                  goto 701
               elseif( CH_NETRT(i - 1, j).lt.0) then !West
                  goto 701
               endif
               goto 702
701            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point W", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
702            continue

           else if ( DIRECTION(i, j) .EQ. 32) then
               if( (i - 1 .LE. 0)      &      !-- 32's can flow either west or north
                 .OR.  (j + 1 .GT. JXRT)) then     !-- this is the north edge
                   goto 801
               elseif (CH_NETRT(i - 1, j + 1).lt.0) then !North West
                   goto 801
               endif
               goto 802
801            continue
#ifdef MPP_LAND
             cnt = CH_NETLNK(i,j)
#else
             cnt = cnt + 1
#endif
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
#ifdef MPP_LAND
             Link_Location(i,j) = cnt
#endif
#ifdef HYDRO_D
!             print *, "Pour Point NW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
#endif
802            continue
           endif
          endif !CH_NETRT check for this node
         END DO
       END DO 

#ifdef MPP_LAND
#ifdef HYDRO_D
     print*, "my_id=",my_id, "cnt = ", cnt 
#endif
#endif
 
#ifdef MPP_LAND
     Link_location = CH_NETLNK
     call MPP_CHANNEL_COM_INT(Link_location,ixrt,jxrt,TYPEL,NLINKS,99) 
#endif

! jump to here if UDMP_OPT .eq. 1
299    continue

!---- read in link routing data if not routing on grid, but on link network
   IF (channel_option .eq. 3) THEN

#ifdef MPP_LAND
     if(my_id .eq. IO_id) then
#endif

        if (NLAKES.gt.0) then !read in only if there are lakes
           open(unit=79,file='LAKEPARM.TBL', form='formatted',status='old')
           write(6,*) "before read(79)  header for LAKEPARM.TBL"
           call flush(6)
           read(79,*)  header  !-- read the lake file
        endif

           do i=1, NLAKES
             read (79,*,err=5101) tmp, HRZAREA(i),LAKEMAXH(i), &
                WEIRC(i), WEIRL(i), ORIFICEC(i), ORIFICEA(i), ORIFICEE(i),&
                LATLAKE(i), LONLAKE(i),ELEVLAKE(i), WEIRH(i)
           enddo
5101        continue
           close(79)
#ifdef MPP_LAND
     endif

       if(NLAKES .gt. 0) then
          call mpp_land_bcast_real(NLAKES,HRZAREA)
          call mpp_land_bcast_real(NLAKES,LAKEMAXH)
          call mpp_land_bcast_real(NLAKES,WEIRH  )
          call mpp_land_bcast_real(NLAKES,WEIRC  )
          call mpp_land_bcast_real(NLAKES,WEIRL  )
          call mpp_land_bcast_real(NLAKES,ORIFICEC)
          call mpp_land_bcast_real(NLAKES,ORIFICEA)
          call mpp_land_bcast_real(NLAKES,ORIFICEE)
          call mpp_land_bcast_real(NLAKES,LATLAKE )
          call mpp_land_bcast_real(NLAKES,LONLAKE )
          call mpp_land_bcast_real(NLAKES,ELEVLAKE)
       endif
#endif

!!-- if routing on link network, read those data too
   ELSEIF ((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option .ne. 3) then ! not routing on grid, read from file ? do we need the channel switch?

        call readLinkSL( GNLINKSL,NLINKSL,route_link_f, route_lake_f,maxorder, &
                   LINKID, TO_NODE, TYPEL, ORDER , &
                   QLINK,CHLON, CHLAT, ZELEV, MUSK, MUSX, CHANLEN, &
                   MannN, So, ChSSlp, Bw, LAKEIDA, HRZAREA,  &
                   LAKEMAXH, WEIRH, WEIRC, WEIRL, ORIFICEC, &
                   ORIFICEA, ORIFICEE, gages, gageMiss, &
                   LAKEIDM,NLAKES, latlake, lonlake)

!--- get the lake configuration here.
#ifdef MPP_LAND
      call nhdLakeMap_mpp(NLAKES, NLINKSL, TYPEL, LAKELINKID, LAKEIDX, TO_NODE,LINKID, LAKEIDM, LAKEIDA, GNLINKSL)
!      call nhdLakeMap(NLAKES, NLINKSL, TYPEL, LAKELINKID, LAKEIDX, TO_NODE,LINKID, LAKEIDM, LAKEIDA)
#else
       call nhdLakeMap(NLAKES, NLINKSL, TYPEL, LAKELINKID, LAKEIDX, TO_NODE,LINKID, LAKEIDM, LAKEIDA)
#endif


#ifdef MPP_LAND
       if(NLAKES .gt. 0) then
!         call mpp_land_bcast_int(NLINKSL,LAKEIDA) 
!         call mpp_land_bcast_int(NLINKSL,LAKEIDX)

          call mpp_land_bcast_real(NLAKES,HRZAREA)
          call mpp_land_bcast_int(NLAKES,LAKEIDM)
          call mpp_land_bcast_real(NLAKES,LAKEMAXH)
          call mpp_land_bcast_real(NLAKES,WEIRH  )
          call mpp_land_bcast_real(NLAKES,WEIRC  )
          call mpp_land_bcast_real(NLAKES,WEIRL  )
          call mpp_land_bcast_real(NLAKES,ORIFICEC)
          call mpp_land_bcast_real(NLAKES,ORIFICEA)
          call mpp_land_bcast_real(NLAKES,ORIFICEE)
          call mpp_land_bcast_real(NLAKES,LATLAKE )
          call mpp_land_bcast_real(NLAKES,LONLAKE )
          call mpp_land_bcast_real(NLAKES,ELEVLAKE)
       endif
#endif

    ENDIF   !channel option is 1 or 2 (linked routing)

   RETURN !from READ_CHROUTING1

!DJG -----------------------------------------------------
   END SUBROUTINE READ_CHROUTING1

   subroutine readLinkSL( GNLINKSL,NLINKSL,route_link_f, route_lake_f, maxorder, &
                   LINKID, TO_NODE, TYPEL, ORDER , &
                   QLINK,CHLON, CHLAT, ZELEV, MUSK, MUSX, CHANLEN, &
                   MannN, So, ChSSlp, Bw, LAKEIDA, HRZAREA,  &
                   LAKEMAXH,WEIRH,  WEIRC, WEIRL, ORIFICEC, &
                   ORIFICEA, ORIFICEE, gages, gageMiss,& 
                   LAKEIDM,NLAKES, latlake, lonlake)

        implicit none
        character(len=*) :: route_link_f,route_lake_f
        integer  :: GNLINKSL, NLINKSL, tmp_from_node,NLAKES

        INTEGER, INTENT(INOUT)                   :: MAXORDER
        INTEGER, intent(out), dimension(:) :: LAKEIDA, LINKID, TO_NODE, TYPEL, ORDER 

        real,dimension(:,:)  :: QLINK
        REAL, intent(out), dimension(:) ::  CHLON, CHLAT, ZELEV, MUSK, MUSX, CHANLEN, &
                   MannN, So, ChSSlp, Bw, latlake, lonlake

        character(len=15), dimension(:), intent(inout) :: gages
        character(len=15), intent(in) :: gageMiss

!NLAKES
        INTEGER, intent(out), dimension(:)  ::  LAKEIDM
        REAL, intent(out), dimension(:) :: HRZAREA,LAKEMAXH, WEIRC, WEIRL, ORIFICEC,WEIRH, &
                   ORIFICEA, ORIFICEE
!end NLAKES

        INTEGER, dimension(GNLINKSL) ::  tmpLAKEIDA, tmpLINKID,  tmpTO_NODE, tmpTYPEL, tmpORDER 
        character(len=15), dimension(gnlinksl) :: tmpGages
        CHARACTER(len=155)	 :: header
        integer :: i

        character(len=256) :: route_link_f_r,route_lake_f_r
        integer :: lenRouteLinkFR,lenRouteLakeFR ! so the preceeding chan be changed without changing code
        logical :: routeLinkNetcdf, routeLakeNetcdf

#ifdef MPP_LAND
        real :: tmpQLINK(GNLINKSL,2)
        REAL, allocatable, dimension(:) ::  tmpCHLON, tmpCHLAT, tmpZELEV, tmpMUSK, tmpMUSX, tmpCHANLEN, &
                   tmpMannN, tmpSo, tmpChSSlp, tmpBw
#endif 

        !! is RouteLink file netcdf (*.nc) or csv (*.csv)
        route_link_f_r = adjustr(route_link_f)
        lenRouteLinkFR = len(route_link_f_r)
        routeLinkNetcdf = route_link_f_r( (lenRouteLinkFR-2):lenRouteLinkFR) .eq. '.nc'

        !! is RouteLake file netcdf (*.nc) or .TBL
        route_lake_f_r = adjustr(route_lake_f)
        lenRouteLakeFR = len(route_lake_f_r)
        routeLakeNetcdf = route_lake_f_r( (lenRouteLakeFR-2):lenRouteLakeFR) .eq. '.nc'

#ifdef MPP_LAND
       tmpQLINK = 0
       tmpGages = gageMiss
 
       if(my_id .eq. IO_id) then

          allocate(tmpCHLON(GNLINKSL))
          allocate(tmpCHLAT(GNLINKSL))
          allocate(tmpZELEV(GNLINKSL))
          allocate(tmpMUSK(GNLINKSL))
          allocate(tmpMUSX(GNLINKSL))
          allocate(tmpCHANLEN(GNLINKSL))
          allocate(tmpMannN(GNLINKSL))
          allocate(tmpSo(GNLINKSL))
          allocate(tmpChSSlp(GNLINKSL))
          allocate(tmpBw(GNLINKSL))

          if(routeLinkNetcdf) then

             call read_route_link_netcdf(                                &
                  route_link_f,                                          &
                  tmpLINKID,     tmpTO_NODE,   tmpCHLON,                 &
                  tmpCHLAT,      tmpZELEV,     tmpTYPEL,    tmpORDER,    &
                  tmpQLINK(:,1), tmpMUSK,      tmpMUSX,     tmpCHANLEN,  &
                  tmpMannN,      tmpSo,        tmpChSSlp,   tmpBw,       &
                  tmpGages, tmpLAKEIDA)

          else

             open(unit=17,file=trim(route_link_f),form='formatted',status='old')
             read(17,*)  header
#ifdef HYDRO_D
             print *, "header ", header, "NLINKSL = ", NLINKSL, GNLINKSL
#endif
             call flush(6)
             do i=1,GNLINKSL
                read (17,*) tmpLINKID(i),   tmp_from_node,   tmpTO_NODE(i), tmpCHLON(i),    &
                            tmpCHLAT(i),    tmpZELEV(i),     tmpTYPEL(i),   tmpORDER(i),    &
                            tmpQLINK(i,1),  tmpMUSK(i),      tmpMUSX(i),    tmpCHANLEN(i),  &
                            tmpMannN(i),    tmpSo(i),        tmpChSSlp(i),  tmpBw(i)

                ! if (So(i).lt.0.005) So(i) = 0.005  !-- impose a minimum slope requireement
                if (tmpORDER(i) .gt. MAXORDER) MAXORDER = tmpORDER(i)
             end do
             close(17)
          
          end if  ! routeLinkNetcdf 

          if(routeLakeNetcdf) then
             call read_route_lake_netcdf(route_lake_f,HRZAREA, &
                LAKEMAXH, WEIRH, WEIRC,    WEIRL, ORIFICEC,       &
                ORIFICEA,  ORIFICEE, LAKEIDM, latlake, lonlake)
          endif

!!- initialize channel  if missing in input
           do i=1,GNLINKSL
              if(tmpQLINK(i,1) .le. 1e-3) then
                 tmpQLINK(i,1) = 20.0 * (1.0/(float(MAXORDER+1) - float(tmpORDER(i))))**3
                tmpQLINK(i,2) = tmpQLINK(i,1) !## initialize the current flow at each link
              endif
           end do

       endif ! my_id .eq. IO_id

        call ReachLS_decomp(tmpLINKID,  LINKID )
        call ReachLS_decomp(tmpLAKEIDA, LAKEIDA )

        call ReachLS_decomp(tmpTO_NODE, TO_NODE)
        call ReachLS_decomp(tmpCHLON,    CHLON  )
        call ReachLS_decomp(tmpCHLAT,    CHLAT  )
        call ReachLS_decomp(tmpZELEV,    ZELEV  )
        call ReachLS_decomp(tmpTYPEL,   TYPEL  )
        call ReachLS_decomp(tmpORDER,   ORDER  )
        call ReachLS_decomp(tmpQLINK(:,1), QLINK(:,1))
        call ReachLS_decomp(tmpQLINK(:,2), QLINK(:,2))
        call ReachLS_decomp(tmpMUSK,    MUSK   )
        call ReachLS_decomp(tmpMUSX,     MUSX   )
        call ReachLS_decomp(tmpCHANLEN,  CHANLEN)
        call ReachLS_decomp(tmpMannN,       MannN  )
        call ReachLS_decomp(tmpSo,       So     )
        call ReachLS_decomp(tmpChSSlp,   ChSSlp )
        call ReachLS_decomp(tmpBw,       Bw     )


!       call ReachLS_decomp(tmpHRZAREA,  HRZAREA)
!       call ReachLS_decomp(tmpLAKEMAXH, LAKEMAXH)
!       call ReachLS_decomp(tmpWEIRC,    WEIRC  )
!       call ReachLS_decomp(tmpWEIRL,    WEIRL  )
!       call ReachLS_decomp(tmpORIFICEC, ORIFICEC)
!       call ReachLS_decomp(tmpORIFICEA, ORIFICEA)
!       call ReachLS_decomp(tmpORIFICEE, ORIFICEE)

!yw This function does not work correctly for gages.
!yw        call ReachLS_decomp(tmpGages,    gages)
        call mpp_land_bcast_int1(MAXORDER)

        if(NLAKES .gt. 0) then
           call mpp_land_bcast_real(NLAKES, HRZAREA)
           call mpp_land_bcast_real(NLAKES, LAKEMAXH)
           call mpp_land_bcast_real(NLAKES, WEIRH)  
           call mpp_land_bcast_real(NLAKES, WEIRC)  
           call mpp_land_bcast_real(NLAKES, WEIRL)  
           call mpp_land_bcast_real(NLAKES, ORIFICEC)
           call mpp_land_bcast_real(NLAKES, ORIFICEA)
           call mpp_land_bcast_real(NLAKES, ORIFICEE)
           call mpp_land_bcast_int(NLAKES, LAKEIDM)
        endif


        if(my_id .eq. io_id ) then
           if(allocated(tmpCHLON)) deallocate(tmpCHLON)
           if(allocated(tmpCHLAT)) deallocate(tmpCHLAT)
           if(allocated(tmpZELEV)) deallocate(tmpZELEV)
           if(allocated(tmpMUSK)) deallocate(tmpMUSK)
           if(allocated(tmpMUSX)) deallocate(tmpMUSX)
           if(allocated(tmpCHANLEN)) deallocate(tmpCHANLEN)
           if(allocated(tmpMannN)) deallocate(tmpMannN)
           if(allocated(tmpSo)) deallocate(tmpSo)
           if(allocated(tmpChSSlp)) deallocate(tmpChSSlp)
           if(allocated(tmpBw)) deallocate(tmpBw)
!, tmpHRZAREA,&
!                  tmpLAKEMAXH, tmpWEIRC, tmpWEIRL, tmpORIFICEC, &
!                  tmpORIFICEA,tmpORIFICEE)
        endif

#else
       QLINK = 0 
        if(routeLinkNetcdf) then

          call read_route_link_netcdf(                     &
                 route_link_f,                              &
                 LINKID,     TO_NODE, CHLON,                &
                 CHLAT,      ZELEV,     TYPEL,    ORDER,    &
                 QLINK(:,1), MUSK,      MUSX,     CHANLEN,  &
                 MannN,      So,        ChSSlp,   Bw,       &
                 gages, LAKEIDA)
           
        else 

          open(unit=17,file=trim(route_link_f),form='formatted',status='old')
          read(17,*)  header
#ifdef HYDRO_D
          print *, "header ", header, "NLINKSL = ", NLINKSL
#endif
          do i=1,NLINKSL
              read (17,*) LINKID(i), tmp_from_node, TO_NODE(i), CHLON(i),CHLAT(i),ZELEV(i), &
                   TYPEL(i), ORDER(i), QLINK(i,1), MUSK(i), MUSX(i), CHANLEN(i), &
                   MannN(i), So(i), ChSSlp(i), Bw(i)

              ! if (So(i).lt.0.005) So(i) = 0.005  !-- impose a minimum slope requireement
              if (ORDER(i) .gt. MAXORDER) MAXORDER = ORDER(i)
          end do
          close(17)

        end if  ! routeLinkNetcdf 

!!- initialize channel according to order if missing in input
        do i=1,NLINKSL
            if(QLINK(i,1) .le. 1e-3) then
              QLINK(i,1) = 20.0 * (1/(float(MAXORDER+1) - float(ORDER(i))))**3
              QLINK(i,2) = QLINK(i,1) !## initialize the current flow at each link
            endif
        end do
        
!!================================ 
!!! need to add the sequential lake read here
!!=================================


#endif

        do i=1,NLINKSL
!           if(So(i) .lt. 0.001) So(i) = 0.001
           So(i) = max(So(i), 0.00001)
        end do

#ifdef HYDRO_D
       write(6,*) "finish read readLinkSL "
       call flush(6) 

#endif
   end subroutine readLinkSL




#ifdef MPP_LAND

!yw continue

       SUBROUTINE MPP_READ_CHROUTING_new(IXRT,JXRT,ELRT,CH_NETRT, CH_LNKRT,LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            LAKEIDA,HRZAREA, LAKEMAXH, WEIRH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, LAKEIDM, LAKEIDX, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,LATVAL,&
            LONVAL,STRMFRXSTPTS,geo_finegrid_flnm,NLINKSL, LINKID, GNLINKSL,UDMP_OPT,g_ixrt,g_jxrt, &
            gnlinks,GCH_NETLNK, map_l2g, link_location,yw_mpp_nlinks, &
            lake_index, nlinks_index, gages, gageMiss)
        implicit none
        INTEGER, INTENT(IN)                          :: IXRT,JXRT,g_IXRT,g_JXRT, GNLINKS, UDMP_OPT
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES, NLINKSL
        INTEGER                                      :: I,J,channel_option
        CHARACTER(len=28)                            :: dir

!----DJG,DNY New variables for channel and lake routing
        CHARACTER(len=155)	 :: header
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ZELEV
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHLAT,CHLON

        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TYPEL
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TO_NODE,ORDER
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: STRMFRXSTPTS
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKEIDA  !identifies which links in the domain are id'd as lakes
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKEIDX  ! the integer lake id.

        INTEGER, INTENT(INOUT)                       :: MAXORDER
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MUSK, MUSX !muskingum
        REAL, INTENT(INOUT),  DIMENSION(:,:)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE,LINKID  ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: map_l2g

!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        INTEGER, INTENT(INOUT),  DIMENSION(NLAKES)   :: LAKEIDM
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: HRZAREA
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: LAKEMAXH, WEIRH
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(NLAKES)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(NLINKS)       :: ChSSlp, Bw
        character(len=15), intent(inout), dimension(nlinks) :: gages
        character(len=15), intent(in)                :: gageMiss

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: geo_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid
        real                                         :: gc,n

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK,GCH_NETLNK
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: ELRT
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETRT, CH_LNKRT
        INTEGER, INTENT(OUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT, link_location
        REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)    :: latval,lonval
        integer :: k
        INTEGER, DIMENSION(nlinks)            :: node_table, nlinks_index
        INTEGER, DIMENSION(nlakes)            :: lake_index
        integer :: yw_mpp_nlinks , l, mpp_nlinks, GNLINKSL



          call READ_CHROUTING1(IXRT,JXRT,ELRT,CH_NETRT, CH_LNKRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            LAKEIDA,HRZAREA, LAKEMAXH, WEIRH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, LAKEIDM, LAKEIDX, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,LATVAL,LONVAL, &
            STRMFRXSTPTS,geo_finegrid_flnm , NLINKSL, LINKID, GNLINKSL,UDMP_OPT  &
#ifdef MPP_LAND
           ,Link_Location  &
#endif
           ,gages, gageMiss)

      call mpp_land_max_int1(MAXORDER)

      if(MAXORDER .eq. 0)  MAXORDER = -9999

       lake_index = -99
      if(channel_option .eq. 3) then
         do j = 1, jxrt
            do i = 1, ixrt
              if (LAKE_MSKRT(i,j) .gt. 0) then
                 lake_index(LAKE_MSKRT(i,j)) = LAKE_MSKRT(i,j)
              endif
            enddo
         enddo
      endif

  
      CHANXI = 0
      CHANYj = 0
      do j = 1, jxrt
          do i = 1, ixrt
             if(CH_NETLNK(i,j) .gt. 0) then
               CHANXI(CH_NETLNK(i,j)) = i
               CHANYJ(CH_NETLNK(i,j)) = j
             endif
          end do
      end do

      node_table = 0
      yw_mpp_nlinks = 0
      do j = 1, jxrt
          do i = 1, ixrt
            if(CH_NETLNK(i,j) .ge. 0) then
               if( (i.eq.1) .and. (left_id .ge. 0) ) then
                    continue
               elseif ( (i.eq. ixrt) .and. (right_id .ge. 0) ) then
                    continue
               elseif ( (j.eq. 1) .and. (down_id .ge. 0) ) then
                    continue
               elseif ( (j.eq. jxrt) .and. (up_id .ge. 0) ) then
                    continue
               else
                        l = CH_NETLNK(i,j)
                        ! if(from_node(l) .gt. 0 .and. to_node(l) .gt. 0) then
                             yw_mpp_nlinks = yw_mpp_nlinks + 1
                             nlinks_index(yw_mpp_nlinks) = l
                        ! endif
               endif
            endif
          end do
      end do

#ifdef HYDRO_D
       write(6,*) "nlinks=", nlinks, " yw_mpp_nlinks=", yw_mpp_nlinks," nlakes=", nlakes
       call flush(6)
#endif
       if(NLAKES .gt. 0) then
          call mpp_land_bcast_real(NLAKES,HRZAREA)
          call mpp_land_bcast_real(NLAKES,LAKEMAXH)
          call mpp_land_bcast_real(NLAKES,WEIRC)
          call mpp_land_bcast_real(NLAKES,WEIRC)
          call mpp_land_bcast_real(NLAKES,WEIRL)
          call mpp_land_bcast_real(NLAKES,ORIFICEC)
          call mpp_land_bcast_real(NLAKES,ORIFICEA)
          call mpp_land_bcast_real(NLAKES,ORIFICEE)
          call mpp_land_bcast_real(NLAKES,LATLAKE)
          call mpp_land_bcast_real(NLAKES,LONLAKE)
          call mpp_land_bcast_real(NLAKES,ELEVLAKE)
       endif


        link_location = CH_NETLNK

        return 

     end SUBROUTINE MPP_READ_CHROUTING_new

#endif


#ifdef MPP_LAND
       subroutine out_day_crt(dayMean,outFile)
           implicit none
           integer :: did
           real ::  dayMean(:)
           character(len=*) :: outFile
           integer:: ywflag
           ywflag = -999
           did = 1
           if((nlst_rt(did)%olddate(12:13) .eq. "00") .and. (nlst_rt(did)%olddate(15:16) .eq. "00") ) ywflag = 99
           call mpp_land_bcast_int1(ywflag)
           if(ywflag <0) return
           ! output daily
           call out_obs_crt(did,dayMean,outFile)
       end subroutine out_day_crt

       subroutine out_obs_crt(did,dayMean,outFile)
           implicit none
           integer did, i, cnt
           real ::  dayMean(:)
           character(len=*) :: outFile
           real,dimension(rt_domain(did)%gnlinks) :: g_dayMean, chlat, chlon
           integer,dimension(rt_domain(did)%gnlinks) :: STRMFRXSTPTS
           
           g_dayMean = -999
           chlat = -999
           chlon = -999
           STRMFRXSTPTS = 0

           call write_chanel_int(RT_DOMAIN(did)%STRMFRXSTPTS,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,STRMFRXSTPTS)

           call write_chanel_real(dayMean,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,g_dayMean)

           call write_chanel_real(RT_DOMAIN(did)%CHLON,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,chlon)

           call write_chanel_real(RT_DOMAIN(did)%CHLAT,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,chlat)


           open (unit=75,file=outFile,status='unknown',position='append')
           cnt = 0
           do i = 1, rt_domain(did)%gnlinks
              if(STRMFRXSTPTS(i) .gt. 0) then
                   write(75,114) nlst_rt(did)%olddate(1:4),nlst_rt(did)%olddate(6:7),nlst_rt(did)%olddate(9:10), nlst_rt(did)%olddate(12:13), &
                         cnt,chlon(i),chlat(i),g_dayMean(i) 
                   cnt = cnt + 1
              endif
           end do
           close(75)
114 FORMAT(1x,A4,A2,A2,A2,",",I7,", ",F10.5,",",F10.5,",",F12.3)
       end subroutine out_obs_crt
#endif
       
    subroutine outPutChanInfo(fromNode,toNode,chlon,chlat)
        implicit none
        integer, dimension(:) :: fromNode,toNode
        real, dimension(:) :: chlat,chlon
        integer :: iret, nodes, i, ncid, dimid_n, varid

        nodes = size(chlon,1)         
#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create("nodeInfor.nc", NF_CLOBBER, ncid)
#else
       iret = nf_create("nodeInfor.nc", IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif
       iret = nf_def_dim(ncid, "node", nodes, dimid_n)  !-- make a decimated grid
!  define the varialbes
       iret = nf_def_var(ncid,"fromNode",NF_INT,1,(/dimid_n/),varid)
       iret = nf_def_var(ncid,"toNode",NF_INT,1,(/dimid_n/),varid)
       iret = nf_def_var(ncid,"chlat",NF_FLOAT,1,(/dimid_n/),varid)
          iret = nf_put_att_text(ncid,varid,'long_name',13,'node latitude')
       iret = nf_def_var(ncid,"chlon",NF_FLOAT,1,(/dimid_n/),varid)
          iret = nf_put_att_text(ncid,varid,'long_name',14,'node longitude')
       iret = nf_enddef(ncid)
!write to the file
           iret = nf_inq_varid(ncid,"fromNode", varid)
           iret = nf_put_vara_int(ncid, varid, (/1/), (/nodes/), fromNode)
           iret = nf_inq_varid(ncid,"toNode", varid)
           iret = nf_put_vara_int(ncid, varid, (/1/), (/nodes/), toNode)
           iret = nf_inq_varid(ncid,"chlat", varid)
           iret = nf_put_vara_real(ncid, varid, (/1/), (/nodes/), chlat)
           iret = nf_inq_varid(ncid,"chlon", varid)
           iret = nf_put_vara_real(ncid, varid, (/1/), (/nodes/), chlon)
          iret = nf_close(ncid)
    end subroutine outPutChanInfo


!===================================================================================================
! Program Name: read_route_link_netcdf
! Author(s)/Contact(s): James L McCreight <jamesmcc><ucar><edu>
! Abstract: Read in the "RouteLink.nc" netcdf file specifing the channel topology. 
! History Log: 
! 7/17/15 -Created, JLM.
! Usage:
! Parameters: <Specify typical arguments passed>
! Input Files: netcdf file RouteLink.nc or other name.
! Output Files: None.
! Condition codes: Currently incomplete error handling. 
!
! If appropriate, descriptive troubleshooting instructions or 
! likely causes for failures could be mentioned here with the 
! appropriate error code
!
! User controllable options: None. 

subroutine read_route_link_netcdf( route_link_file,                         &
                                   LINKID,   TO_NODE, CHLON,                &
                                   CHLAT,    ZELEV,     TYPEL,    ORDER,    &
                                   QLINK,    MUSK,      MUSX,     CHANLEN,  &
                                   MannN,    So,        ChSSlp,   Bw,       &
                                   gages, LAKEIDA                           )

implicit none
character(len=*),        intent(in)  :: route_link_file
integer, dimension(:),   intent(out) :: LAKEIDA, LINKID, TO_NODE
real,    dimension(:),   intent(out) :: CHLON, CHLAT, ZELEV
integer, dimension(:),   intent(out) :: TYPEL, ORDER 
real,    dimension(:),   intent(out) :: QLINK
real,    dimension(:),   intent(out) :: MUSK, MUSX, CHANLEN
real,    dimension(:),   intent(out) :: MannN,    So,       ChSSlp,   Bw
character(len=15), dimension(:), intent(inout) :: gages

integer :: iRet, ncid, ii, varid
logical :: fatal_if_error
fatal_if_error = .TRUE.  !! was thinking this would be a global variable...could become an input.

#ifdef HYDRO_D
print*,"start read_route_link_netcdf"
#endif

iRet = nf90_open(trim(route_link_file), nf90_nowrite, ncid)
if (iRet /= nf90_noErr) then
   write(*,'("read_route_link_netcdf: Problem opening: ''", A, "''")') trim(route_link_file)
   if (fatal_IF_ERROR) call hydro_stop("read_route_link_netcdf: Problem opening file.")
endif


call get_1d_netcdf_int(ncid,  'link',     LINKID,    'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_int(ncid,  'NHDWaterbodyComID',  LAKEIDA, 'read_route_link_netcdf', .FALSE.)
call get_1d_netcdf_int(ncid,  'to',       TO_NODE,   'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'lon',      CHLON,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'lat',      CHLAT,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'alt',      ZELEV,     'read_route_link_netcdf', .TRUE.)
!yw call get_1d_netcdf_int(ncid,  'type',     TYPEL,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_int(ncid,  'order',    ORDER,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'Qi',       QLINK,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'MusK',     MUSK,      'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'MusX',     MUSX,      'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'Length',   CHANLEN,   'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'n',        MannN,     'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'So',       So,        'read_route_link_netcdf', .TRUE.)
!! impose a minimum as this sometimes fails in the file.
where(So .lt. 0.00001) So=0.00001
call get_1d_netcdf_real(ncid, 'ChSlp',    ChSSlp,    'read_route_link_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'BtmWdth',  Bw,        'read_route_link_netcdf', .TRUE.)

! gages is optional, only get it if it's defined in the file.
iRet = nf90_inq_varid(ncid, 'gages', varid)
if (iret .eq. nf90_NoErr) then
   call get_1d_netcdf_text(ncid, 'gages', gages,  'read_route_link_netcdf', .true.)
end if

iRet = nf90_close(ncId)
if (iRet /= nf90_noErr) then
   write(*,'("read_route_link_netcdf: Problem closing: ''", A, "''")') trim(route_link_file)
   if (fatal_IF_ERROR) call hydro_stop("read_route_link_netcdf: Problem closing file.")
end if

#ifdef HYDRO_D
ii = size(LINKID)
print*,'last index=',ii
print*, 'CHLON', CHLON(ii), 'CHLAT', CHLAT(ii), 'ZELEV', ZELEV(ii)
print*,'TYPEL', TYPEL(ii), 'ORDER', ORDER(ii), 'QLINK', QLINK(ii), 'MUSK', MUSK(ii)
print*, 'MUSX', MUSX(ii), 'CHANLEN', CHANLEN(ii), 'MannN', MannN(ii)
print*,'So', So(ii), 'ChSSlp', ChSSlp(ii), 'Bw', Bw(ii)
print*,'gages(ii): ',trim(gages(ii))
print*,"finish read_route_link_netcdf"
#endif

end subroutine read_route_link_netcdf


!===================================================================================================
! Program Name: read_route_lake_netcdf
! Abstract: Read in the "LAKEPARM.nc" netcdf file specifing the channel topology. 
! History Log: 
! 7/17/15 -Created, JLM., then used by DNY
! Usage:
! Parameters: <Specify typical arguments passed>
! Input Files: netcdf file RouteLink.nc or other name.
! Output Files: None.
! Condition codes: Currently incomplete error handling. 
!
subroutine read_route_lake_netcdf(route_lake_file,                         &
                                   HRZAREA,  LAKEMAXH, WEIRH,  WEIRC,    WEIRL,    &
                                   ORIFICEC, ORIFICEA,  ORIFICEE, LAKEIDM, &
                                   lakelat, lakelon)

implicit none
character(len=*),        intent(in)  :: route_lake_file
integer, dimension(:),   intent(out) :: LAKEIDM
real,    dimension(:),   intent(out) :: HRZAREA,  LAKEMAXH, WEIRC,    WEIRL, WEIRH
real,    dimension(:),   intent(out) :: ORIFICEC, ORIFICEA, ORIFICEE, lakelat, lakelon

integer :: iRet, ncid, ii, varid
logical :: fatal_if_error
fatal_if_error = .TRUE.  !! was thinking this would be a global variable...could become an input.

#ifdef HYDRO_D
print*,"start read_route_lake_netcdf"
#endif
   
iRet = nf90_open(trim(route_lake_file), nf90_nowrite, ncid)
if (iRet /= nf90_noErr) then
   write(*,'("read_route_lake_netcdf: Problem opening: ''", A, "''")') trim(route_lake_file)
   if (fatal_IF_ERROR) call hydro_stop("read_route_lake_netcdf: Problem opening file.")
endif

call get_1d_netcdf_int(ncid,  'lake_id',     LAKEIDM,   'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'LkArea',   HRZAREA,   'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'LkMxH',    LAKEMAXH,  'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'WeirH',    WEIRH,     'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'WeirC',    WEIRC,     'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'WeirL',    WEIRL,     'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'OrificeC', ORIFICEC,  'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'OrificeA', ORIFICEA,  'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'OrificeE', ORIFICEE,  'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'lat', lakelat,  'read_route_lake_netcdf', .TRUE.)
call get_1d_netcdf_real(ncid, 'lon', lakelon,  'read_route_lake_netcdf', .TRUE.)

iRet = nf90_close(ncId)
if (iRet /= nf90_noErr) then
   write(*,'("read_route_lake_netcdf: Problem closing: ''", A, "''")') trim(route_lake_file)
   if (fatal_IF_ERROR) call hydro_stop("read_route_lake_netcdf: Problem closing file.")
end if

#ifdef HYDRO_D
ii = size(LAKEIDM)
print*,'last index=',ii
print*,'HRZAREA', HRZAREA(ii)
print*,'LAKEMAXH', LAKEMAXH(ii), 'WEIRC', WEIRC(ii), 'WEIRL', WEIRL(ii)
print*,'ORIFICEC', ORIFICEC(ii), 'ORIFICEA', ORIFICEA(ii), 'ORIFICEE', ORIFICEE(ii)
print*,"finish read_route_lake_netcdf"
#endif

end subroutine read_route_lake_netcdf

!===================================================================================================
! Program Names: get_1d_netcdf_real, get_1d_netcdf_int, get_1d_netcdf_text
! Author(s)/Contact(s): James L McCreight <jamesmcc><ucar><edu>
! Abstract: Read a variable of real or integer type from an open netcdf file, respectively. 
! History Log: 
! 7/17/15 -Created, JLM.
! Usage:
! Parameters: See definitions.
! Input Files: This file is refered to by it's "ncid" obtained from nc_open
!              prior to calling this routine. 
! Output Files: None.
! Condition codes: hydro_stop is passed "get_1d_netcdf".
!
! If appropriate, descriptive troubleshooting instructions or 
! likely causes for failures could be mentioned here with the 
! appropriate error code
!
! User controllable options: None. 

!! could define an interface for these. 
subroutine get_1d_netcdf_int(ncid, varName, var, callingRoutine, fatal_if_error)
integer,               intent(in)  :: ncid !! the file identifier
character(len=*),      intent(in)  :: varName
integer, dimension(:), intent(out) :: var
character(len=*),      intent(in)  :: callingRoutine
logical,               intent(in)  :: fatal_if_error
integer :: varid, iret
iRet = nf90_inq_varid(ncid, varName, varid)
if (iret /= nf90_noErr) then
   if (fatal_IF_ERROR) then
      print*, trim(callingRoutine) // ": get_1d_netcdf_real: variable: " // trim(varName)
      call hydro_stop("get_1d_netcdf")
   end if
end if
iRet = nf90_get_var(ncid, varid, var)
if (iRet /= nf90_NoErr) then
   print*, trim(callingRoutine) // ": get_1d_netcdf_int: values: " // trim(varName)
   if (fatal_IF_ERROR) call hydro_stop("get_1d_netcdf_int")
end if
end subroutine get_1d_netcdf_int


subroutine get_1d_netcdf_real(ncid, varName, var, callingRoutine, fatal_if_error)
integer,            intent(in)  :: ncid !! the file identifier
character(len=*),   intent(in)  :: varName
real, dimension(:), intent(out) :: var
character(len=*),   intent(in)  :: callingRoutine
logical,            intent(in)  :: fatal_if_error

integer :: varid, iret
iRet = nf90_inq_varid(ncid, varName, varid)
if (iret /= nf90_noErr) then
   if (fatal_IF_ERROR) then
      print*, trim(callingRoutine) // ": get_1d_netcdf_real: variable: " // trim(varName)
      call hydro_stop("get_1d_netcdf")
   end if
end if
iRet = nf90_get_var(ncid, varid, var)
if (iRet /= nf90_NoErr) then
   print*, trim(callingRoutine) // ": get_1d_netcdf_real: values: " // trim(varName)
   if (fatal_IF_ERROR) call hydro_stop("get_1d_netcdf_real")
end if
end subroutine get_1d_netcdf_real

subroutine get_1d_netcdf_text(ncid, varName, var, callingRoutine, fatal_if_error)
integer,                        intent(in)  :: ncid !! the file identifier
character(len=*),               intent(in)  :: varName
character(len=*), dimension(:), intent(out) :: var
character(len=*),               intent(in)  :: callingRoutine
logical,                        intent(in)  :: fatal_if_error
integer :: varId, iRet
iRet = nf90_inq_varid(ncid, varName, varid)
if (iret /= nf90_NoErr) then
   print*, trim(callingRoutine) // ": get_1d_netcdf_text: variable: " // trim(varName)
   if (fatal_IF_ERROR) call hydro_stop("get_1d_netcdf_text")
end if
iRet = nf90_get_var(ncid, varid, var)
if (iret /= nf90_NoErr) then
   print*, trim(callingRoutine) // ": get_1d_netcdf_text: values: " // trim(varName)
   if (fatal_IF_ERROR) call hydro_stop("get_1d_netcdf_text")
end if
end subroutine get_1d_netcdf_text

!===================================================================================================
! Program Names: 
!   get_netcdf_dim
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Get the length of a provided dimension.
! History Log: 
!   7/23/15 -Created, JLM.
! Usage:
! Parameters: 
!   file: character, the file to query
!   dimName: character, the name of the dimension
!   callingRoutine: character, the name of the calling routine for error messages
!   fatalErr: Optional, Logical - all errors are fatal, calling hydro_stop()
! Input Files:  
!   Specified argument. 
! Output Files: 
! Condition codes: 
!   hydro_stop is called. .
! User controllable options:
! Notes: 

function get_netcdf_dim(file, dimName, callingRoutine, fatalErr) 
implicit none
integer :: get_netcdf_dim  !! return value
character(len=*), intent(in)   :: file, dimName, callingRoutine
integer :: ncId, dimId, iRet
logical, optional, intent(in) :: fatalErr
logical :: fatalErr_local
character(len=256) :: errMsg

fatalErr_local = .false.
if(present(fatalErr)) fatalErr_local=fatalErr

write(*,'("getting dimension from file: ", A)') trim(file)
iRet = nf90_open(trim(file), nf90_NOWRITE, ncId)
if (iret /= nf90_noerr) then
   write(*,'("Problem opening file: ", A)') trim(file)
   if(fatalErr_local) call hydro_stop(trim(callingRoutine) // ': get_netcdf_dim')
endif

iRet = nf90_inq_dimid(ncId, trim(dimName), dimId)
if (iret /= nf90_noerr) then
   write(*,'("Problem getting the dimension ID ", A)') &
        '"' // trim(dimName) // '" in file: ' // trim(file)
   if(fatalErr_local) call hydro_stop(trim(callingRoutine) // ': get_netcdf_dim')
endif

iRet = nf90_inquire_dimension(ncId, dimId, len= get_netcdf_dim)
if (iret /= nf90_noerr) then
   write(*,'("Problem getting the dimension length of ", A)') &
        '"' // trim(dimName) // '" in file: ' // trim(file)
   if(fatalErr_local) call hydro_stop(trim(callingRoutine) // ': get_netcdf_dim')
endif

iRet = nf90_close(ncId)
if (iret /= nf90_noerr) then
   write(*,'("Problem closing file: ", A)') trim(file)
   if(fatalErr_local) call hydro_stop(trim(callingRoutine) // ': get_netcdf_dim')
endif
end function get_netcdf_dim


! read the GWBUCET Parm for NHDPlus
subroutine readBucket_nhd(infile, numbasns, gw_buck_coeff, gw_buck_exp, &
                z_max, LINKID, nhdBuckMask)
    implicit none
    integer :: numbasns
    integer, dimension(numbasns) :: LINKID
    real, dimension(numbasns) :: gw_buck_coeff, gw_buck_exp, z_max
    integer, dimension(numbasns) :: nhdBuckMask
    character(len=*) :: infile
!   define temp array
    integer :: i,j,k, gnid, ncid, varid, ierr, dimid, iret
    integer, allocatable, dimension(:) :: tmpLinkid
    real, allocatable, dimension(:) :: tmpCoeff, tmpExp, tmpz_max

!   get gnid
#ifdef MPP_LAND
    if(my_id .eq. io_id ) then
#endif
       iret = nf_open(trim(infile), NF_NOWRITE, ncid)
#ifdef MPP_LAND
       if(iret .ne. 0) then
           call hydro_stop("Failed to open GWBUCKET Parameter file.")
       endif
       iret = nf_inq_dimid(ncid, "BasinDim", dimid)
       if (iret /= 0) then
               !print*, "nf_inq_dimid:  BasinDim"
               call hydro_stop("Failed read GBUCKETPARM - nf_inq_dimid:  BasinDim")
       endif
       iret = nf_inq_dimlen(ncid, dimid, gnid)
    endif
    call mpp_land_bcast_int1(gnid)
#endif
    allocate(tmpLinkid(gnid))
    allocate(tmpCoeff(gnid))
    allocate(tmpExp(gnid))
    allocate(tmpz_max(gnid))
#ifdef MPP_LAND
    if(my_id .eq. io_id ) then
#endif
!      read the file data.
          iret = nf_inq_varid(ncid,"Coeff",  varid)
          if(iret /= 0) then
               print * , "could not find Coeff from ", infile
               call hydro_stop("Failed to read BUCKETPARM")
          endif
          iret = nf_get_var_real(ncid, varid, tmpCoeff)

          iret = nf_inq_varid(ncid,"Expon",  varid)
          if(iret /= 0) then
               print * , "could not find Expon from ", infile
               call hydro_stop("Failed to read BUCKETPARM")
          endif
          iret = nf_get_var_real(ncid, varid, tmpExp)

          iret = nf_inq_varid(ncid,"Zmax",  varid)
          if(iret /= 0) then
               print * , "could not find Zmax from ", infile
               call hydro_stop("Failed to read BUCKETPARM")
          endif
          iret = nf_get_var_real(ncid, varid, tmpz_max)

          iret = nf_inq_varid(ncid, "ComID",  varid)
          if(iret /= 0) then
               print * , "could not find ComID from ", infile
               call hydro_stop("Failed to read BUCKETPARM")
          endif
          iret = nf_get_var_int(ncid, varid, tmpLinkid)
#ifdef MPP_LAND
    endif
       if(gnid .gt. 0) then
          call mpp_land_bcast_real_1d(tmpCoeff)
          call mpp_land_bcast_real_1d(tmpExp)
          call mpp_land_bcast_real_1d(tmpz_max)
          call mpp_land_bcast_int(gnid ,tmpLinkid)
       endif
#endif
   
    nhdBuckMask = -999
    do k = 1, numbasns
        do i = 1, gnid
            if(LINKID(k) .eq. tmpLinkid(i)) then
               gw_buck_coeff(k) = tmpCoeff(i)
               gw_buck_exp(k) = tmpExp(i)
               z_max(k) = tmpz_max(i)
               nhdBuckMask(k) = 1
               goto 301 
            endif
        end do
301     continue
    end do 

    if(allocated(tmpCoeff)) deallocate(tmpCoeff)
    if(allocated(tmpExp)) deallocate(tmpExp)
    if(allocated(tmpz_max)) deallocate(tmpz_max)
    if(allocated(tmpLinkid)) deallocate(tmpLinkid)
end subroutine readBucket_nhd

!-- output the channel routine for fast output.
!   subroutine mpp_output_chrt2(gnlinks,gnlinksl,map_l2g,igrid,                  &
!        split_output_count, NLINKS, ORDER,                                     &
!        startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt_ch,              &
!        K,STRMFRXSTPTS,order_to_write,NLINKSL,channel_option, gages, gageMiss, &
!        lsmDt                                                                  &
!        )

#ifdef MPP_LAND
   subroutine mpp_output_chrt2(gnlinks,gnlinksl,map_l2g,igrid,                  &
        split_output_count, NLINKS, ORDER,                                     &
        startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt_ch,              &
        K,NLINKSL,channel_option,linkid  &
#ifdef WRF_HYDRO_NUDGING
        , nudge                                     &
#endif
        , QLateral, iocflag ,velocity  &
        , accLndRunOff, accQLateral, accStrmvolrt, accBucket, &
        UDMP_OPT         &
	)

       USE module_mpp_land

       implicit none

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K,NLINKSL
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(:),               intent(in) :: chlon,chlat
     real, dimension(:),                  intent(in) :: hlink,zelev

     integer, dimension(:),               intent(in) :: ORDER, linkid

     real,                                     intent(in) :: dtrt_ch
     real, dimension(:,:),                intent(in) :: qlink
#ifdef WRF_HYDRO_NUDGING
     real, dimension(:),                  intent(in) :: nudge
#endif
     real, dimension(:), intent(in) :: QLateral, velocity
     integer, intent(in) :: iocflag
     real, dimension(:), intent(in) :: accLndRunOff, accQLateral, accStrmvolrt, accBucket
     integer, intent(in) :: UDMP_OPT

     integer :: channel_option

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

      integer  :: gnlinks, map_l2g(nlinks),  gnlinksl
      real, allocatable,dimension(:) :: g_chlon,g_chlat, g_hlink,g_zelev
#ifdef WRF_HYDRO_NUDGING
      real, allocatable,dimension(:) :: g_nudge
#endif
      integer, allocatable,dimension(:) :: g_order, g_linkid
      real,allocatable,dimension(:,:) :: g_qlink
      integer  :: gsize
      real, allocatable, dimension(:) :: g_accLndRunOff, g_accQLateral, g_accStrmvolrt, g_accBucket
      real, allocatable, dimension(:) :: g_QLateral, g_velocity

        gsize = gNLINKS
        if(gnlinksl .gt. gsize) gsize = gnlinksl
     if(my_id .eq. io_id ) then
        allocate(g_chlon(gsize  ))
        allocate(g_chlat(gsize  ))
        allocate(g_hlink(gsize  ))
        allocate(g_zelev(gsize  ))
        allocate(g_qlink(gsize  ,2))
#ifdef WRF_HYDRO_NUDGING
        allocate(g_nudge(gsize))
#endif
        allocate(g_order(gsize  ))
        allocate(g_linkid(gsize  ))

        allocate(g_accLndRunOff(gsize  ))
        allocate(g_accQLateral(gsize  ))
        allocate(g_accStrmvolrt(gsize  ))
        allocate(g_accBucket(gsize  ))

        allocate(g_QLateral(gsize  ))
        allocate(g_velocity(gsize  ))

     else

       allocate(g_accLndRunOff(1))
       allocate(g_accQLateral(1))
       allocate(g_accStrmvolrt(1))
       allocate(g_accBucket(1))
       allocate(g_QLateral(1))
       allocate(g_velocity(1))

        allocate(g_chlon(1))
        allocate(g_chlat(1))
        allocate(g_hlink(1))
        allocate(g_zelev(1))
        allocate(g_qlink(1,2))
#ifdef WRF_HYDRO_NUDGING
        allocate(g_nudge(1))
#endif
        allocate(g_order(1))
        allocate(g_linkid(1))
     endif

     call mpp_land_sync()
     if(channel_option .eq. 1 .or. channel_option .eq. 2) then
        g_qlink = 0
        call ReachLS_write_io(qlink(:,1), g_qlink(:,1))
        call ReachLS_write_io(qlink(:,2), g_qlink(:,2))
#ifdef WRF_HYDRO_NUDGING
        g_nudge=0
        call ReachLS_write_io(nudge,g_nudge)
#endif
        call ReachLS_write_io(order, g_order)
        call ReachLS_write_io(linkid, g_linkid)
        call ReachLS_write_io(chlon, g_chlon)
        call ReachLS_write_io(chlat, g_chlat)
        call ReachLS_write_io(zelev, g_zelev)

        call ReachLS_write_io(accLndRunOff, g_accLndRunOff)
        call ReachLS_write_io(accQLateral, g_accQLateral)
        call ReachLS_write_io(accStrmvolrt, g_accStrmvolrt)
        call ReachLS_write_io(accBucket, g_accBucket)

        call ReachLS_write_io(QLateral, g_QLateral)
        call ReachLS_write_io(velocity, g_velocity)
        call ReachLS_write_io(hlink,g_hlink)

     else
        call write_chanel_real(qlink(:,1),map_l2g,gnlinks,nlinks,g_qlink(:,1))
        call write_chanel_real(qlink(:,2),map_l2g,gnlinks,nlinks,g_qlink(:,2))
        call write_chanel_int(order,map_l2g,gnlinks,nlinks,g_order)
        call write_chanel_int(linkid,map_l2g,gnlinks,nlinks,g_linkid)
        call write_chanel_real(chlon,map_l2g,gnlinks,nlinks,g_chlon)
        call write_chanel_real(chlat,map_l2g,gnlinks,nlinks,g_chlat)
        call write_chanel_real(zelev,map_l2g,gnlinks,nlinks,g_zelev)
        call write_chanel_real(hlink,map_l2g,gnlinks,nlinks,g_hlink)
     endif


     if(my_id .eq. IO_id) then
       call output_chrt2(igrid, split_output_count, GNLINKS, g_ORDER,                &
          startdate, date, g_chlon, g_chlat, g_hlink,g_zelev,g_qlink,dtrt_ch,K,     &
          gNLINKSL,channel_option, g_linkid  &
#ifdef WRF_HYDRO_NUDGING
          , g_nudge                                     &
#endif
          , g_QLateral, iocflag,g_velocity   &
          , g_accLndRunOff, g_accQLateral, g_accStrmvolrt, g_accBucket, &
          UDMP_OPT    &
	  )
     end if
     call mpp_land_sync()
    if(allocated(g_order)) deallocate(g_order)
    if(allocated(g_chlon)) deallocate(g_chlon)
    if(allocated(g_chlat)) deallocate(g_chlat)
    if(allocated(g_hlink)) deallocate(g_hlink)
    if(allocated(g_zelev)) deallocate(g_zelev)
    if(allocated(g_qlink)) deallocate(g_qlink)
    if(allocated(g_linkid)) deallocate(g_linkid)
#ifdef WRF_HYDRO_NUDGING
    if(allocated(g_nudge)) deallocate(g_nudge)
#endif
    if(allocated(g_QLateral)) deallocate(g_QLateral)
    if(allocated(g_velocity)) deallocate(g_velocity)

end subroutine mpp_output_chrt2

#endif


!subroutine output_chrt2 
!For realtime output only when CHRTOUT_GRID = 2.
!   subroutine output_chrt2(igrid, split_output_count, NLINKS, ORDER,             &
!        startdate, date, chlon, chlat, hlink, zelev, qlink, dtrt_ch, K,         &
!        STRMFRXSTPTS, order_to_write, NLINKSL, channel_option, gages, gageMiss, & 
!        lsmDt                                                                   &
!        )
   subroutine output_chrt2(igrid, split_output_count, NLINKS, ORDER,             &
        startdate, date, chlon, chlat, hlink, zelev, qlink, dtrt_ch, K,         &
        NLINKSL, channel_option ,linkid &
#ifdef WRF_HYDRO_NUDGING
        , nudge                                     &
#endif
        , QLateral, iocflag, velocity   &
        , accLndRunOff, accQLateral, accStrmvolrt, accBucket, &
        UDMP_OPT    &
        )
     
     implicit none
#include <netcdf.inc>
!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K,channel_option
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS, NLINKSL
     real, dimension(:),                  intent(in) :: chlon,chlat
     real, dimension(:),                  intent(in) :: hlink,zelev
     integer, dimension(:),               intent(in) :: ORDER

     real,                                     intent(in) :: dtrt_ch
     real, dimension(:,:),                intent(in) :: qlink
#ifdef WRF_HYDRO_NUDGING
     real, dimension(:),                  intent(in) :: nudge
#endif
     real, dimension(:), intent(in) :: QLateral, velocity
     integer, intent(in) :: iocflag
     real, dimension(nlinks), intent(in) :: accLndRunOff, accQLateral, accStrmvolrt, accBucket    
     integer, intent(in)  :: UDMP_OPT

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date



     integer, allocatable, DIMENSION(:)         :: linkid    

     integer, allocatable, DIMENSION(:)         :: rec_num_of_station
     integer, allocatable, DIMENSION(:)         :: rec_num_of_stationO

     integer, allocatable, DIMENSION(:)         :: lOrder !- local stream order

     integer, save  :: output_count
     integer, save  :: ncid

     integer :: stationdim, dimdata, varid, charid, n
     integer :: timedim

     integer :: iret,i !-- order_to_write is the lowest stream order to output
     integer :: start_posO, prev_posO, nlk

     integer :: previous_pos  !-- used for the station model
     character(len=256) :: output_flnm
     character(len=34)  :: sec_since_date
     integer :: seconds_since,nstations,cnt,ObsStation
     character(len=32)  :: convention
     character(len=11),allocatable, DIMENSION(:)  :: stname

     character(len=34) :: sec_valid_date

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
     PARAMETER (OTXLEN = 15) ! length of example string
     INTEGER  OTIMEID        ! record dimension id
     INTEGER  OTXID          ! variable ID
     INTEGER  OTXDIMS(OTDIMS) ! variable shape
     INTEGER  OTSTART(OTDIMS), OTCOUNT(OTDIMS)
     character(len=19)  :: date19, date19start


     seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))


     if(channel_option .ne. 3) then
        nstations = NLINKSL
     else
        nstations = NLINKS
     endif

       if(split_output_count .ne. 1 ) then
            write(6,*) "WARNING: split_output_count need to be 1 for this output option."
       endif
!-- have moved sec_since_date from above here..
        sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

        date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

        seconds_since = int(nlst_rt(1)%dt*(rt_domain(1)%out_counts-1))
        sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                      //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

        write(output_flnm, '(A12,".CHRTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid

#ifdef HYDRO_D
        print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif
        if (iret /= 0) then
           print*,  "Problem nf_create points"
           call hydro_stop("In output_chrt2() - Problem nf_create points.")
        endif

       iret = nf_def_dim(ncid, "station", nstations, stationdim)
       iret = nf_def_dim(ncid, "time", 1, timedim)

#ifndef HYDRO_REALTIME
      !- station location definition all,  lat
        iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station latitude')
        iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')

      !- station location definition,  long
        iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',17,'Station longitude')
        iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')

!     !-- elevation is ZELEV
        iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station altitude')
        iret = nf_put_att_text(ncid,varid,'units',6,'meters')

!-- parent index
!        iret = nf_def_var(ncid,"parent_index",NF_INT,1,(/stationdim/), varid)
!        iret = nf_put_att_text(ncid,varid,'long_name',36,'index of the station for this record')


     !-- prevChild
!        iret = nf_def_var(ncid,"prevChild",NF_INT,1,(/stationdim/), varid)
!        iret = nf_put_att_text(ncid,varid,'long_name',57,'record number of the previous record for the same station')
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

     !-- lastChild
!        iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
!        iret = nf_put_att_text(ncid,varid,'long_name',30,'latest report for this station')
!        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)
#endif

       if(UDMP_OPT .eq. 1) then
          iret = nf_def_var(ncid, "accLndRunOff", NF_FLOAT, 1, (/stationdim/), varid)
          iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
          iret = nf_put_att_text(ncid,varid,'long_name',28,'ACCUMULATED runoff from land')

          iret = nf_def_var(ncid, "accQLateral", NF_FLOAT, 1, (/stationdim/), varid)
          iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
          iret = nf_put_att_text(ncid,varid,'long_name',24,'Total ACCUMULATED runoff')

          iret = nf_def_var(ncid, "accStrmvolrt", NF_FLOAT, 1, (/stationdim/), varid)
          iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
          iret = nf_put_att_text(ncid,varid,'long_name',39,'ACCUMULATED runoff from terrain routing')

          iret = nf_def_var(ncid, "accBucket", NF_FLOAT, 1, (/stationdim/), varid)
          iret = nf_put_att_text(ncid,varid,'units',8,'meter^3?')
          iret = nf_put_att_text(ncid,varid,'long_name',32,'ACCUMULATED runoff from gw bucket')
       endif

        iret = nf_def_var(ncid,"time",NF_INT, 1, (/timedim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)
        iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')

        !- flow definition, var
        iret = nf_def_var(ncid, "streamflow", NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',10,'River Flow')

#ifdef WRF_HYDRO_NUDGING
        !- nudge definition
        iret = nf_def_var(ncid, "nudge", NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',32,'Amount of stream flow alteration')
#endif


!     !- head definition, var
      if(channel_option .eq. 3) then
        iret = nf_def_var(ncid, "head", NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid,varid,'long_name',11,'River Stage')
      endif
!#ifdef HYDRO_REALTIME
!      if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) ) then
!	iret = nf_def_var(ncid, "head", NF_FLOAT, 1, (/stationdim/), varid)
!        iret = nf_put_att_text(ncid,varid,'units',5,'meter')
!        iret = nf_put_att_text(ncid,varid,'long_name',11,'River Stage')
!      endif
!#endif


	!-- NEW lateral inflow definition, var
	if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) ) then
	        iret = nf_def_var(ncid, "q_lateral", NF_FLOAT, 1, (/stationdim/), varid)
       		iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
                iret = nf_put_att_text(ncid,varid,'long_name',25,'Runoff into channel reach')
	endif

        !-- NEW velocity definition, var
        if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) .and. (iocflag .ne. 4) ) then
        	iret = nf_def_var(ncid, "velocity", NF_FLOAT, 1, (/stationdim/), varid)
        	iret = nf_put_att_text(ncid,varid,'units',9,'meter/sec')
        	iret = nf_put_att_text(ncid,varid,'long_name',14,'River Velocity')
	endif

#ifndef HYDRO_REALTIME
!     !- order definition, var
        iret = nf_def_var(ncid, "order", NF_INT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',21,'Strahler Stream Order')
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)
#endif

     !-- station  id
     ! define character-position dimension for strings of max length 11
        iret = nf_def_var(ncid, "station_id", NF_INT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')

         convention(1:32) = "Unidata Observation Dataset v1.0"
         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
#ifndef HYDRO_REALTIME
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
#endif
         iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
         iret = nf_put_att_text(ncid, NF_GLOBAL, "station_dimension",7, "station")
         iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid, NF_GLOBAL, "stream_order_output",NF_INT,1,1)


         iret = nf_enddef(ncid)

         iret = nf_inq_varid(ncid,"time", varid)
         iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)
#ifndef HYDRO_REALTIME        
        !-- write latitudes
         iret = nf_inq_varid(ncid,"latitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chlat)

        !-- write longitudes
         iret = nf_inq_varid(ncid,"longitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chlon)

        !-- write elevations
         iret = nf_inq_varid(ncid,"altitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), zelev)

        !-- write order
         iret = nf_inq_varid(ncid,"order", varid)
         iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), ORDER)
#endif

        !-- write stream flow
         iret = nf_inq_varid(ncid,"streamflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), qlink(:,1))

#ifdef WRF_HYDRO_NUDGING
        !-- write nudge
         iret = nf_inq_varid(ncid,"nudge", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), nudge)
#endif

	!-- write head
     	if(channel_option .eq. 3) then
           iret = nf_inq_varid(ncid,"head", varid)
           iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), hlink)
	endif
!#ifdef HYDRO_REALTIME
!	if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) ) then
!	      ! dummy value for now
!              iret = nf_inq_varid(ncid,"head", varid)
!              iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chlon*0.-9999.)
!        endif
!#endif

        !-- write lateral inflow
	if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) ) then
	        iret = nf_inq_varid(ncid,"q_lateral", varid)
       		iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), QLateral)
        endif

        !-- writelvelocity (dummy value for now)
	if ( (channel_option .ne. 3) .and. (iocflag .ge. 0) .and. (iocflag .ne. 4) ) then
        	iret = nf_inq_varid(ncid,"velocity", varid)
         	iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), velocity)
	endif

         if(UDMP_OPT .eq. 1) then

            iret = nf_inq_varid(ncid,"accLndRunOff", varid)
            iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), accLndRunOff)

            iret = nf_inq_varid(ncid,"accQLateral", varid)
            iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), accQLateral)

            iret = nf_inq_varid(ncid,"accStrmvolrt", varid)
            iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), accStrmvolrt)

            iret = nf_inq_varid(ncid,"accBucket", varid)
            iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), accBucket)

         endif


	!-- write id
        iret = nf_inq_varid(ncid,"station_id", varid)
        iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), linkid)




      iret = nf_redef(ncid)
      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(date)) = date
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))


      iret = nf_enddef(ncid)
      iret = nf_sync(ncid)


        iret = nf_close(ncid)

#ifdef HYDRO_D
     print *, "Exited Subroutine output_chrt"
#endif


end subroutine output_chrt2


   subroutine output_GW_Diag(did)
       implicit none
       integer :: i , did, gnbasns

#ifdef MPP_LAND
       real, allocatable, dimension(:) :: g_qin_gwsubbas, g_qout_gwsubbas, g_z_gwsubbas
       integer, allocatable, dimension(:) :: g_basnsInd
       if(my_id .eq. io_id) then
          if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
               allocate(g_qin_gwsubbas(rt_domain(did)%gnumbasns))
               allocate(g_qout_gwsubbas(rt_domain(did)%gnumbasns))
               allocate(g_z_gwsubbas(rt_domain(did)%gnumbasns))
               allocate(g_basnsInd(rt_domain(did)%gnumbasns))
               gnbasns = rt_domain(did)%gnumbasns
          else
               allocate(g_qin_gwsubbas(rt_domain(did)%gnlinksl))
               allocate(g_qout_gwsubbas(rt_domain(did)%gnlinksl))
               allocate(g_z_gwsubbas(rt_domain(did)%gnlinksl))
               allocate(g_basnsInd(rt_domain(did)%gnlinksl))
               gnbasns = rt_domain(did)%gnlinksl 
          endif
       endif 
     
       if(nlst_rt(did)%channel_option .ne. 3) then
          call ReachLS_write_io(rt_domain(did)%qin_gwsubbas,g_qin_gwsubbas)
          call ReachLS_write_io(rt_domain(did)%qout_gwsubbas,g_qout_gwsubbas)
          call ReachLS_write_io(rt_domain(did)%z_gwsubbas,g_z_gwsubbas)
          call ReachLS_write_io(rt_domain(did)%linkid,g_basnsInd)
       else
          call gw_write_io_real(rt_domain(did)%numbasns,rt_domain(did)%qin_gwsubbas,  &
                 rt_domain(did)%basnsInd,g_qin_gwsubbas)
          call gw_write_io_real(rt_domain(did)%numbasns,rt_domain(did)%qout_gwsubbas,  & 
                 rt_domain(did)%basnsInd,g_qout_gwsubbas)
          call gw_write_io_real(rt_domain(did)%numbasns,rt_domain(did)%z_gwsubbas,  & 
                 rt_domain(did)%basnsInd,g_z_gwsubbas)
          call gw_write_io_int(rt_domain(did)%numbasns,rt_domain(did)%basnsInd,  &
                 rt_domain(did)%basnsInd,g_basnsInd)
       endif
       if(my_id .eq. io_id) then
!          open (unit=51,file='GW_inflow.txt',form='formatted',&
!                status='unknown',position='append')
!          open (unit=52,file='GW_outflow.txt',form='formatted',&
!                status='unknown',position='append')
!          open (unit=53,file='GW_zlev.txt',form='formatted',&
!                status='unknown',position='append')
!          do i=1,RT_DOMAIN(did)%gnumbasns
!             write (51,951) i,nlst_rt(did)%olddate,g_qin_gwsubbas(i)
951        FORMAT(I3,1X,A19,1X,F11.3)
!            write (52,951) i,nlst_rt(did)%olddate,g_qout_gwsubbas(i)
!            write (53,951) i,nlst_rt(did)%olddate,g_z_gwsubbas(i)
!         end do  
!         close(51)
!         close(52)
!         close(53)

          call   output_gw_netcdf( nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, gnbasns, &
                  trim(nlst_rt(did)%sincedate), trim(nlst_rt(did)%olddate), &
                  g_basnsInd,g_qin_gwsubbas, g_qout_gwsubbas, g_z_gwsubbas )
          deallocate(g_qin_gwsubbas, g_qout_gwsubbas, g_z_gwsubbas, g_basnsInd)
         
       endif
# else
!       open (unit=51,file='GW_inflow.txt',form='formatted',&
!             status='unknown',position='append')
!       open (unit=52,file='GW_outflow.txt',form='formatted',&
!             status='unknown',position='append')
!       open (unit=53,file='GW_zlev.txt',form='formatted',&
!             status='unknown',position='append')
!       do i=1,RT_DOMAIN(did)%numbasns
!          write (51,951) i,nlst_rt(did)%olddate,rt_domain(did)%qin_gwsubbas(i)
951        FORMAT(I3,1X,A19,1X,F11.3)
!          write (52,951) i,nlst_rt(did)%olddate,rt_domain(did)%qout_gwsubbas(i)
!          write (53,951) i,nlst_rt(did)%olddate,rt_domain(did)%z_gwsubbas(i)
!       end do  
!       close(51)
!       close(52)
!       close(53)
        if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
          call   output_gw_netcdf( nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, RT_DOMAIN(did)%numbasns, &
                  trim(nlst_rt(did)%sincedate), trim(nlst_rt(did)%olddate), &
                  rt_domain(did)%basnsInd,rt_domain(did)%qin_gwsubbas, &
                  rt_domain(did)%qout_gwsubbas, rt_domain(did)%z_gwsubbas  )
        else
          call   output_gw_netcdf( nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, RT_DOMAIN(did)%nlinksl, &
                  trim(nlst_rt(did)%sincedate), trim(nlst_rt(did)%olddate), &
                  rt_domain(did)%linkid,rt_domain(did)%qin_gwsubbas, &
                  rt_domain(did)%qout_gwsubbas, rt_domain(did)%z_gwsubbas  )
        endif
#endif
    end subroutine output_GW_Diag


!----------------------------------- gw netcdf output

   subroutine output_gw_netcdf(igrid, split_output_count, nbasns, &
        startdate, date, &
        gw_id_var, gw_in_var, gw_out_var, gw_z_var)

     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: nbasns
     real, dimension(:),                  intent(in) :: gw_in_var, gw_out_var, gw_z_var
     integer, dimension(:),               intent(in) :: gw_id_var

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date


     integer, save  :: output_count
     integer, save :: ncid

     integer :: basindim, varid,  n, nstations
     integer :: iret,i    !-- 
     character(len=256) :: output_flnm
     character(len=19)  :: date19, date19start
     character(len=32)  :: convention
     integer :: timedim
     integer :: seconds_since
     character(len=34)  :: sec_since_date
     character(len=34)  :: sec_valid_date

     if(split_output_count .ne. 1 ) then
            write(6,*) "WARNING: split_output_count need to be 1 for this output option."
     endif

     sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

     date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

     seconds_since = int(nlst_rt(1)%out_dt*60*(rt_domain(1)%out_counts-1))
     
     sec_valid_date = 'seconds since '//nlst_rt(1)%startdate(1:4)//'-'//nlst_rt(1)%startdate(6:7)//'-'//nlst_rt(1)%startdate(9:10) &
                      //' '//nlst_rt(1)%startdate(12:13)//':'//nlst_rt(1)%startdate(15:16)//' UTC'

     write(output_flnm, '(A12,".GWOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid

#ifdef HYDRO_D
      print*, 'output_flnm = "'//trim(output_flnm)//'"'
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       iret = nf_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

      if (iret /= 0) then
          print*, "Problem nf_create" 
          call hydro_stop("output_gw_netcdf") 
      endif 

!!! Define dimensions

        nstations =nbasns

      iret = nf_def_dim(ncid, "basin", nstations, basindim)

      iret = nf_def_dim(ncid, "time", 1, timedim)

!!! Define variables


      !- gw basin ID
      iret = nf_def_var(ncid,"gwbas_id",NF_INT, 1, (/basindim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',11,'GW basin ID')

      !- gw inflow
      iret = nf_def_var(ncid, "gw_inflow", NF_FLOAT, 1, (/basindim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

      !- gw outflow
      iret = nf_def_var(ncid, "gw_outflow", NF_FLOAT, 1, (/basindim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

      !- depth in gw bucket
      iret = nf_def_var(ncid, "gw_zlev", NF_FLOAT, 1, (/basindim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',2,'mm')

      ! Time variable
      iret = nf_def_var(ncid, "time", NF_INT, 1, (/timeDim/), varid)
      iret = nf_put_att_text(ncid,varid,'units',34,sec_valid_date)
      iret = nf_put_att_text(ncid,varid,'long_name',17,'valid output time')

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate

      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_initialization_time", 19, trim(nlst_rt(1)%startdate))
      iret = nf_put_att_text(ncid, NF_GLOBAL, "model_output_valid_time", 19, trim(nlst_rt(1)%olddate))
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)

      iret = nf_enddef(ncid)

!!! Input variables

        !-- write lake id
        iret = nf_inq_varid(ncid,"gwbas_id", varid)
        iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), gw_id_var)

        !-- write gw inflow
        iret = nf_inq_varid(ncid,"gw_inflow", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), gw_in_var  )

        !-- write elevation  of inflow
        iret = nf_inq_varid(ncid,"gw_outflow", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), gw_out_var  )

        !-- write elevation  of inflow
        iret = nf_inq_varid(ncid,"gw_zlev", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), gw_z_var  )

        !-- write time variable
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/1/), (/1/), seconds_since)

        iret = nf_close(ncid)

    end subroutine output_gw_netcdf

!------------------------------- end gw netcdf output


    subroutine read_NSIMLAKES(NLAKES,route_lake_f)
        integer                     :: NLAKES
        CHARACTER(len=256)          :: route_lake_f

        character(len=256)          :: route_lake_f_r
        integer                     :: lenRouteLakeFR, iRet, ncid, dimId
        logical                     :: routeLakeNetcdf

      !! is RouteLake file netcdf (*.nc) or  from the LAKEPARM.TBL ascii
#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
      route_lake_f_r = adjustr(route_lake_f)
      lenRouteLakeFR = len(route_Lake_f_r)
      routeLakeNetcdf = route_lake_f_r( (lenRouteLakeFR-2):lenRouteLakeFR) .eq. '.nc'


      write(6,'("getting NLAKES from: ''", A, "''")') trim(route_lake_f)
      write(6,*) "routeLakeNetcdf TF Name Len",routeLakeNetcdf, route_lake_f,lenRouteLakeFR
      call flush(6)

       if(routeLakeNetcdf) then
          write(6,'("getting NLAKES from: ''", A, "''")') trim(route_lake_f)
          NLAKES = get_netcdf_dim(trim(route_lake_f), 'nlakes',  &
                                   'read_NSIMLAKES', fatalErr=.true.)
        else
!yw for IOC reach based routing, if netcdf lake file is not set from the hydro.namelist, 
!    we will assume that no lake will be assimulated. 
          write(6,*) "No lake nectdf file defined. NLAKES is set to be zero."
          NLAKES = 0
      endif 
#ifdef MPP_LAND
    endif ! end if block of my_id .eq. io_id
         call mpp_land_bcast_int1(NLAKES)
#endif

    end subroutine read_NSIMLAKES

! sequential code: not used.!!!!!!
    subroutine nhdLakeMap(NLAKES, NLINKSL, TYPEL, LAKELINKID, LAKEIDX, gTO_NODE,LINKID, LAKEIDM, LAKEIDA)
        !--- get the lake configuration here.
        implicit none
        integer, dimension(:),   intent(inout) :: TYPEL, LAKELINKID, LAKEIDX
        integer, dimension(:),   intent(inout) :: gTO_NODE
        integer, dimension(:),   intent(inout) :: LINKID, LAKEIDM, LAKEIDA
        integer, intent(in) :: NLAKES, NLINKSL 
        integer, dimension(NLINKSL) :: OUTLAKEID
        integer :: i,j,k, kk
      
        TYPEL = -999

!! find the links that flow into lakes (e.g. TYPEL = 3), and update the TO_NODE, so that links flow into the lake reach
#ifdef MPP_LAND
     call nhdLakeMap_scan(NLAKES, NLINKSL, LAKELINKID, gTO_NODE,LINKID, LAKEIDM, LAKEIDA,NLINKSL)
#endif

        OUTLAKEID = gTO_NODE
        DO i = 1, NLAKES
          DO j = 1, NLINKSL
            DO k = 1, NLINKSL

              if( (gTO_NODE(j) .eq. LINKID(k) ) .and. &
                  (LAKEIDA(k) .lt. 0 .and. LAKEIDA(j) .eq. LAKEIDM(i))) then
                  TYPEL(j) = 1  !this is the link flowing out of the lake
                  OUTLAKEID(j) = LAKEIDA(j) ! LINKID(j)
                  LAKELINKID(i) = j
!                    write(61,*) gTO_NODE(j),LAKEIDA(j),LAKEIDA(k),LAKELINKID(i) , j
!                    call flush(61)
              elseif( (gTO_NODE(j) .eq. LINKID(k)) .and. &
                  (LAKEIDA(j) .lt. 0 .and. LAKEIDA(k) .gt. 0) .and. &
                  (LAKEIDA(k) .eq. LAKEIDM(i)) ) then
                  TYPEL(j) = 3 !type_3 inflow link to lake
                  OUTLAKEID(j) = LAKEIDM(i)
              elseif (LAKEIDA(j) .eq. LAKEIDM(i) .and. .not. TYPEL(j) .eq. 1) then
                  TYPEL(j) = 2 ! internal lake linkd
              endif
            END DO
          END DO
       END DO

       DO i = 1, NLAKES
            if(LAKELINKID(i) .gt. 0) then
                LAKEIDX(LAKELINKID(i)) = i
            endif
       ENDDO

 ! assign the the inflow nodes to the lank with a new TO_NODE id, which is the outflow link
       DO i = 1, NLINKSL
        DO j = 1, NLINKSL
            if(TYPEL(i) .eq. 3 .and. TYPEL(j) .eq. 1 .and. (OUTLAKEID(j) .eq. OUTLAKEID(i))) then
              gTO_NODE(i) = LINKID(j)  !   OUTLAKEID(i)
            endif
        ENDDO
       ENDDO

!     do k = 1, NLINKSL
!         write(60+my_id,*) "k, typel, lakeidx", k, typel(k), lakeidx(k) 
!         call flush(60+my_id)
!     end do

!     DO i = 1, NLINKSL
!        write(61,*) i,LAKEIDX(i), TYPEL(i)
!     end do
!     DO i = 1, NLAKES 
!        write(62,*) i,LAKELINKID(i)
!        write(63,*) i,LAKEIDM(i)
!     end do
!     close(61)
!     close(62)
!     close(63)
!     call hydro_finish()

!   write(60,*) TYPEL
!   write(63,*) LAKELINKID, LAKEIDX
!   write(64,*) gTO_NODE
!   write(61,*) LINKID
!   write(62,*) LAKEIDM, LAKEIDA
!   close(60)
!   close(61)
!   close(62)
!   close(63)
!   close(64)
!   call hydro_finish()


    end subroutine nhdLakeMap

#ifdef MPP_LAND
    subroutine nhdLakeMap_mpp(NLAKES, NLINKSL, TYPEL, LAKELINKID, LAKEIDX, TO_NODE,LINKID, LAKEIDM, LAKEIDA,GNLINKSL)
        !--- get the lake configuration here.
        implicit none
        integer, dimension(:),   intent(out) :: TYPEL, LAKELINKID, LAKEIDX
        integer, dimension(:),   intent(inout) :: TO_NODE
        integer, dimension(:),   intent(in) :: LINKID, LAKEIDA
        integer, dimension(:),   intent(inout) :: LAKEIDM
        integer, intent(in) :: NLAKES, NLINKSL ,GNLINKSL
        integer, dimension(NLINKSL) :: OUTLAKEID
        integer :: i,size ,j,k, kk, num, maxNum, m, mm
        integer, allocatable, dimension(:) :: gLINKID, tmpTYPEL, tmpLINKID, ind,  &
                    tmplakeida, tmpoutlakeid, tmpTO_NODE, gLAKEIDA, gLAKEIDX
        integer, allocatable, dimension(:,:) :: gtonodeout
    
        integer,allocatable, dimension(:) ::  gto, tmpLAKELINKID, gTYPEL, gOUTLAKEID

      integer tmpBuf(GNLINKSL)

      allocate (gto(GNLINKSL))

      if(my_id .eq. io_id) then
         allocate (tmpLAKELINKID(nlakes) )
      else
         allocate (tmpLAKELINKID(1))
      endif


!     prescan the data and remove the LAKEIDM which point to two links.
#ifdef MPP_LAND
     call nhdLakeMap_scan(NLAKES, NLINKSL, LAKELINKID, TO_NODE,LINKID, LAKEIDM, LAKEIDA,GNLINKSL)
#endif

   
      call gBcastValue(TO_NODE,gto)   
      maxNum = 0
      kk = 0
      do m = 1, NLINKSL
          num = 0
          do k = 1, gnlinksl
             if(gto(k) .eq. LINKID(m) ) then
                 kk = kk +1
                 num = num + 1
             endif
          end do
          if(num .gt. maxNum) maxNum = num
      end do

      allocate(ind(kk))
      allocate(gToNodeOut(NLINKSL,maxNum+1))
      gToNodeOut = -99
      allocate(tmpTYPEL(kk))
      allocate(tmpLINKID(kk))
      allocate(tmpLAKEIDA(kk))
      allocate(tmpOUTLAKEID(kk))
      allocate(tmpTO_NODE(kk))

      if(kk .gt. 0) then
         tmpOUTLAKEID = -999
         tmpTYPEL = -999
         tmpTO_NODE = -999
      endif
      if(NLINKSL .gt. 0) then
         OUTLAKEID = -999
         TYPEL = -999
      endif

      kk = 0
      do m = 1, NLINKSL
         num = 1
         do k = 1, gnlinksl
             if(gto(k) .eq. LINKID(m) ) then
                 kk = kk +1
                 ind(kk) = k
                 tmpTO_NODE(kk) = gto(k)
                 gToNodeOut(m,num+1) = kk
                 gToNodeOut(m,1) = num
                 num = num + 1
             endif
          end do
      end do
      size = kk
      if(allocated(gto)) deallocate (gto)
    
      allocate(gLINKID(gnlinksl))
      call gBcastValue(LINKID,gLINKID)   
      do i = 1, size
            k = ind(i)
            tmpLINKID(i) = gLINKID(k)
      enddo

      allocate(gLAKEIDA(gnlinksl))
      call gBcastValue(LAKEIDA(1:NLINKSL),gLAKEIDA(1:gnlinksl) )   
      do i = 1, size
            k = ind(i)
            tmpLAKEIDA(i) = gLAKEIDA(k)
      enddo
      if(allocated(gLAKEIDA)) deallocate(gLAKEIDA)

!yw LAKELINKID = 0
      tmpLAKELINKID = LAKELINKID
      tmpOUTLAKEID  = tmpTO_NODE
      OUTLAKEID(1:NLINKSL)  = TO_NODE(1:NLINKSL)

 !! find the links that flow into lakes (e.g. TYPEL = 3), and update the TO_NODE, so that links flow into the lake reach
        DO i = 1, NLAKES
          DO k = 1, NLINKSL
             do m = 1, gToNodeOut(k,1)
                 j = gToNodeOut(k,m+1)
                 if( (tmpTO_NODE(j) .eq. LINKID(k) ) .and. &
                     (LAKEIDA(k) .lt. 0 .and. tmpLAKEIDA(j) .eq. LAKEIDM(i))) then
                     tmpTYPEL(j) = 1  !this is the link flowing out of the lake
                     tmpOUTLAKEID(j) = tmpLAKEIDA(j) !tmpLINKID(j) ! Wei Check
                     LAKELINKID(i) = ind(j)
!                    write(61,*) tmpTO_NODE(j),tmpLAKEIDA(j),LAKEIDA(k),LAKELINKID(i) 
!                    call flush(61)
                 elseif( (tmpTO_NODE(j) .eq. LINKID(k)) .and. &
                     (tmpLAKEIDA(j) .lt. 0 .and. LAKEIDA(k) .gt. 0) .and. &
                     (LAKEIDA(k) .eq. LAKEIDM(i)) ) then
                     tmpTYPEL(j) = 3 !type_3 inflow link to lake
                     tmpOUTLAKEID(j) = LAKEIDM(i) !Wei Check
!                    write(62,*) tmpTO_NODE(j),tmpOUTLAKEID(j),LAKEIDM(i) 
!                    call flush(62)
                 elseif (tmpLAKEIDA(j) .eq. LAKEIDM(i) .and. tmpTYPEL(j) .ne. 1) then
                     tmpTYPEL(j) = 2 ! internal lake linkd
                     !! print the following to get the list of links which are ignored bc they are internal to lakes.
                     !print*,'Ndg: tmpLAKEIDA(j):', tmpLAKEIDA(j)
                 endif
            END DO
          END DO
       END DO

!yw       call sum_int1d(LAKELINKID, NLAKES) 
       call updateLake_seqInt(LAKELINKID,nlakes,tmpLAKELINKID)

       if(allocated(tmplakelinkid))  deallocate(tmpLAKELINKID)

       if(gNLINKSL .gt. 0) then
          if(my_id .eq. 0) then
              allocate(gLAKEIDX(gNLINKSL))
              gLAKEIDX = -999
              DO i = 1, NLAKES
                   if(LAKELINKID(i) .gt. 0) then
                      gLAKEIDX(LAKELINKID(i)) = i
                   endif
              ENDDO
          else
              allocate(gLAKEIDX(1))
          endif
          call ReachLS_decomp(gLAKEIDX, LAKEIDX)
          if(allocated(gLAKEIDX)) deallocate(gLAKEIDX)
       endif

!     do k = 1, size   
!         write(70+my_id,*) "k, ind(k), typel, lakeidx", k, ind(k),tmpTYPEL(k), lakeidx(ind(k)) 
!         call flush(70+my_id)
!     end do
       
       call TONODE2RSL(ind,tmpTYPEL,size,gNLINKSL,NLINKSL,TYPEL(1:NLINKSL), -999 )
       call TONODE2RSL(ind,tmpOUTLAKEID,size,gNLINKSL,NLINKSL,OUTLAKEID(1:NLINKSL), -999 )


 ! assign the the inflow nodes to the lank with a new TO_NODE id, which is the outflow link
!yw       DO i = 1, NLINKSL
!yw 105
!     DO k = 1, NLINKSL
!       do m = 1, gToNodeOut(k,1)
!                i = gToNodeOut(k,m+1)
!          DO j = 1, NLINKSL
!             if (tmpTYPEL(i) .eq. 3 .and. TYPEL(j) .eq. 1 .and. (OUTLAKEID(j) .eq. tmpOUTLAKEID(i)) &
!                  .and. tmpOUTLAKEID(i) .ne. -999) then
!                    !yw tmpTO_NODE(i) = tmpOUTLAKEID(i)  !Wei Check
!                    tmpTO_NODE(i) = LINKID(j)  !Wei Check
!             endif 
!          END DO
!        END DO
!     END DO 
!     call TONODE2RSL(ind,tmpTO_NODE,size,gNLINKSL,NLINKSL,TO_NODE(1:NLINKSL), -999 )

 ! assign the the inflow nodes to the lank with a new TO_NODE id, which is the outflow link
      allocate(gTYPEL(gNLINKSL))
      allocate(gOUTLAKEID(gNLINKSL))
      call gBcastValue(TYPEL,gTYPEL)   
      call gBcastValue(OUTLAKEID,gOUTLAKEID)   
       DO i = 1, NLINKSL
        DO j = 1, gNLINKSL
            if(TYPEL(i) .eq. 3 .and. gTYPEL(j) .eq. 1 .and. (gOUTLAKEID(j) .eq. OUTLAKEID(i))) then
              TO_NODE(i) = gLINKID(j)  !   OUTLAKEID(i)
            endif
        ENDDO
       ENDDO
      if(allocated(gLINKID)) deallocate(gLINKID)
      if(allocated(gTYPEL)) deallocate(gTYPEL)
      if(allocated(gOUTLAKEID)) deallocate(gOUTLAKEID)
      if(allocated(tmpTYPEL)) deallocate(tmpTYPEL)
      if(allocated(tmpLINKID)) deallocate(tmpLINKID)
      if(allocated(tmpTO_NODE)) deallocate(tmpTO_NODE)
      if(allocated(tmpLAKEIDA)) deallocate(tmpLAKEIDA)
      if(allocated(tmpOUTLAKEID)) deallocate(tmpOUTLAKEID)


!     do k = 1, NLINKSL
!         write(60+my_id,*) "k, typel, lakeidx", k, typel(k), lakeidx(k) 
!         call flush(60+my_id)
!     end do
      

!     call ReachLS_write_io(TO_NODE(1:NLINKSL), tmpBuf(1:gNLINKSL) )
!     if(my_id .eq. io_id ) then
!       write(70,*) tmpBuf(1:gNLINKSL)
!       call flush(70)
!     endif
!     call ReachLS_write_io(TYPEL(1:NLINKSL), tmpBuf(1:gNLINKSL) )
!     if(my_id .eq. io_id ) then
!       write(71,*) tmpBuf
!       call flush(71)
!     endif
!     call ReachLS_write_io(LAKEIDX(1:NLINKSL), tmpBuf(1:gNLINKSL))
!     if(my_id .eq. io_id ) then
!       write(72,*) tmpBuf
!       call flush(72)
!       close(72)
!     endif
!     call ReachLS_write_io(OUTLAKEID(1:NLINKSL), tmpBuf(1:gNLINKSL))
!     if(my_id .eq. io_id ) then
!       write(73,*) tmpBuf
!       call flush(73)
!     endif
!     call hydro_finish()

!     DO i = 1, NLINKSL
!        write(61,*) i,LAKEIDX(i), TYPEL(i)
!     end do
!     DO i = 1, NLAKES 
!        write(63,*) i,LAKEIDM(i)
!        write(62,*) i,LAKELINKID(i)
!     end do
!     close(61)
!     close(62)
!     close(63)

!   write(60,*) TYPEL
!   write(63,*) LAKELINKID, LAKEIDX
!   write(64,*) TO_NODE
!   write(61,*) LINKID
!   write(62,*) LAKEIDM, LAKEIDA
!   close(60)
!   close(61)
!   close(62)
!   close(63)
!   close(64)
!   call hydro_finish()

    end subroutine nhdLakeMap_mpp

    subroutine nhdLakeMap_scan(NLAKES, NLINKSL, LAKELINKID, TO_NODE,LINKID, LAKEIDM, LAKEIDA,GNLINKSL)
        !--- get the lake configuration here.
        implicit none
        integer, dimension(NLAKES) :: LAKELINKID
        integer, dimension(:),   intent(in) :: TO_NODE
        integer, dimension(:),   intent(in) :: LINKID, LAKEIDA
        integer, dimension(:),   intent(inout) :: LAKEIDM
        integer, intent(in) :: NLAKES, NLINKSL ,GNLINKSL
        integer :: i,size ,j,k, kk, num, maxNum, m, mm
        integer, allocatable, dimension(:) :: ind,  &
                    tmplakeida, tmpoutlakeid, tmpTO_NODE, gLAKEIDA
        integer, allocatable, dimension(:,:) :: gtonodeout
    
        integer,allocatable, dimension(:) ::  gto , tmpLAKELINKID, gtoLakeId_g, gtoLakeId

!       integer tmpBuf(GNLINKSL)
        integer, dimension(nlakes) :: lakemask
        integer ii

      allocate (gto(GNLINKSL))
      allocate (gtoLakeId_g(GNLINKSL))
      allocate (gtoLakeId(NLINKSL))
      if(my_id .eq. io_id) then
         allocate(tmpLAKELINKID(nlakes))
      else
         allocate(tmpLAKELINKID(1))
      endif

      gtoLakeId_g=-999
 
      call gBcastValue(TO_NODE,gto)   
      maxNum = 0
      kk = 0
      do m = 1, NLINKSL
          num = 0
          do k = 1, gnlinksl
             if(gto(k) .eq. LINKID(m) ) then
                 gtoLakeId_g(k) = lakeida(m)
                 kk = kk +1
                 num = num + 1
             endif
          end do
          if(num .gt. maxNum) maxNum = num
      end do

      allocate(ind(kk))
      allocate(gToNodeOut(NLINKSL,maxNum+1))
      gToNodeOut = -99
      allocate(tmpLAKEIDA(kk))
      allocate(tmpTO_NODE(kk))


      kk = 0
      do m = 1, NLINKSL
         num = 1
         do k = 1, gnlinksl
             if(gto(k) .eq. LINKID(m) ) then
                 kk = kk +1
                 ind(kk) = k
                 tmpTO_NODE(kk) = gto(k)
                 gToNodeOut(m,num+1) = kk
                 gToNodeOut(m,1) = num
                 num = num + 1
             endif
          end do
      end do
      size = kk
      if(allocated(gto)) deallocate (gto)
    

      allocate(gLAKEIDA(gnlinksl))
      call gBcastValue(LAKEIDA(1:NLINKSL),gLAKEIDA(1:gnlinksl) )   
      do i = 1, size
            k = ind(i)
            tmpLAKEIDA(i) = gLAKEIDA(k)
      enddo
      if(allocated(gLAKEIDA)) deallocate(gLAKEIDA)

        tmpLAKELINKID = LAKELINKID
!       LAKELINKID = 0
        DO i = 1, NLAKES
          DO k = 1, NLINKSL
             do m = 1, gToNodeOut(k,1)
                 j = gToNodeOut(k,m+1)
                 if( (tmpTO_NODE(j) .eq. LINKID(k) ) .and. &
                     (LAKEIDA(k) .lt. 0 .and. tmpLAKEIDA(j) .eq. LAKEIDM(i))) then
                     if(LAKELINKID(i) .gt. 0) then
                         LAKELINKID(i) = -999
#ifdef HYDRO_D
                         write(6,*) "remove the lake  LAKEIDM(i) ", i, LAKEIDM(i)
                         call flush(6)
#endif
                     endif
                     if(LAKELINKID(i) .eq. 0) LAKELINKID(i) = ind(j)      
                 endif
            END DO
          END DO
       END DO
!yw        call match1dLake(LAKELINKID, NLAKES, -999) 

!yw double check
      call combine_int1d(gtoLakeId_g,gnlinksl, -999) 
      call ReachLS_decomp(gtoLakeId_g,gtoLakeId)

       lakemask = 0
       DO k = 1, NLINKSL
          if(LAKEIDA(k) .gt. 0) then
             DO i = 1, NLAKES
                if(gtoLakeId(k) .eq. LAKEIDM(i) )  then
                    goto 992
                endif
             enddo
             DO i = 1, NLAKES
                if(LAKEIDA(k) .eq. LAKEIDM(i) )  then
                     lakemask(i) = lakemask(i) + 1
                      goto 992
                endif
             enddo
992          continue
          endif
       enddo

       if(allocated(gtoLakeId_g)) deallocate(gtoLakeId_g)
       if(allocated(gtoLakeId)) deallocate(gtoLakeId)
       call sum_int1d(lakemask, NLAKES) 

       do i = 1, nlakes
           if(lakemask(i) .ne. 1) then
               LAKELINKID(i) = -999
#ifdef HYDRO_D
               if(my_id .eq. IO_id) then
                  write(6,*) "double check remove the lake : ",LAKEIDM(i)
                  call flush(6)
               endif
#endif
           endif
       enddo


!end double check


       call updateLake_seqInt(LAKELINKID,nlakes,tmpLAKELINKID)

!      if(my_id .eq. 0) then
!          write(65,*) "check LAKEIDM   *****,"
!          write(65,*) LAKEIDM
!          call flush(6)
!      endif

       do k = 1, NLAKES
           if(LAKELINKID(k) .eq. -999) LAKEIDM(k) = -999
       end do

!      if(my_id .eq. 0) then
!          write(65,*) "check LAKEIDM   *****,"
!          write(65,*) LAKEIDM
!          call flush(6)
!      endif

       close(65)
      if(allocated(tmpTO_NODE)) deallocate(tmpTO_NODE)
      if(allocated(tmpLAKEIDA)) deallocate(tmpLAKEIDA)
      if(allocated(tmplakelinkid)) deallocate(tmplakelinkid)

    end subroutine nhdLakeMap_scan
#endif
  
!ADCHANGE: New output lake types routine
    subroutine output_lake_types( inNLINKS, inLINKID, inTYPEL )

#ifdef MPP_LAND
    use module_mpp_land
#endif

    implicit none
#include <netcdf.inc>

    integer, dimension(:),  intent(in) :: inLINKID, inTYPEL
    integer, intent(in) :: inNLINKS

    integer            :: iret
    integer            :: ncid, varid
    integer, parameter :: did=1
    integer            :: linkdim
    character(len=256), parameter :: output_flnm = "LAKE_TYPES.nc"

    integer, allocatable, dimension(:) :: linkId, typeL

#ifdef MPP_LAND

    if(my_id .eq. io_id) then
       allocate( linkId(inNLINKS)  )
       allocate( typeL(inNLINKS)   )
    else
       allocate(linkId(1), typeL(1))
    end if

    call mpp_land_sync()
    call ReachLS_write_io(inLINKID, linkId)
    call ReachLS_write_io(inTYPEL, typeL)

#else

    allocate( linkId(inNLINKS) )
    allocate( typeL(inNLINKS)  )

    linkId    = inLINKID
    typeL     = inTYPEL

#endif

#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif

       ! Create the channel connectivity file
#ifdef HYDRO_D
       print*,'Lakes: output_flnm = "'//trim(output_flnm)//'"'
       flush(6)
#endif

#ifdef WRFIO_NCD_NO_LARGE_FILE_SUPPORT
       write(6,*) "using normal netcdf file for LAKE TYPES"
       iret = nf_create(trim(output_flnm), NF_CLOBBER, ncid)
#else
       write(6,*) "using large netcdf file for LAKE TYPES"
       iret = nf_create(trim(output_flnm), ior(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#endif

       if (iret /= 0) then
          print*,"Lakes: Problem nf_create"
          call hydro_stop("output_lake_types")
       endif

       iret = nf_def_dim(ncid, "link", inNLINKS, linkdim)

       !-- link  id
       iret = nf_def_var(ncid, "LINKID", NF_INT, 1, (/linkdim/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',10,'Link ID')

       !- lake reach type, var
       iret = nf_def_var(ncid, "TYPEL", NF_INT, 1, (/linkdim/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',15,'Lake reach type')

       iret = nf_enddef(ncid)

       !-- write id
       iret = nf_inq_varid(ncid,"LINKID", varid)
       iret = nf_put_vara_int(ncid, varid, (/1/), (/inNLINKS/), linkId)

       !-- write type
       iret = nf_inq_varid(ncid,"TYPEL", varid)
       iret = nf_put_vara_int(ncid, varid, (/1/), (/inNLINKS/), typeL)

       iret = nf_close(ncid)

#ifdef MPP_LAND
    endif
#endif
    if(allocated(linkId)) deallocate(linkId)
    if(allocated(typeL)) deallocate(typeL)

#ifdef MPP_LAND
    if(my_id .eq. io_id) then
#endif
#ifdef HYDRO_D
    write(6,*) "end of output_lake_types"
    flush(6)
#endif
#ifdef MPP_LAND
    endif
#endif

end subroutine output_lake_types


end module module_HYDRO_io
