Module module_namelist

#ifdef MPP_LAND
          USE module_mpp_land
#endif

    IMPLICIT NONE
    INTEGER, PARAMETER :: max_domain=5

#include "namelist.inc"
    TYPE(namelist_rt_field) , dimension(max_domain) :: nlst_rt
    save nlst_rt 

CONTAINS 

    subroutine read_rt_nlst(nlst)     
          implicit none

          TYPE(namelist_rt_field) nlst

          integer ierr
          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                    GWBASESWCRT, GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                    sys_cpl, rst_typ
          real:: DTRT,dxrt
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""
       integer :: SOLVEG_INITSWC
       real out_dt, rst_dt
       character(len=256)  :: RESTART_FILE = ""
       logical            :: history_output
       integer            :: split_output_count, order_to_write
       integer :: igrid
       character(len=256) :: geo_static_flnm = ""
       integer  :: DEEPGWSPIN

       integer :: HIRES_OUT
       integer :: i

!!! add the following two dummy variables 
       integer  :: NSOIL
       real :: ZSOIL8(8)

       namelist /HYDRO_nlist/ NSOIL, ZSOIL8,&
            RESTART_FILE,HISTORY_OUTPUT,SPLIT_OUTPUT_COUNT,IGRID,&
            geo_static_flnm, &
            out_dt, rst_dt, &
            HIRES_OUT, &
            DEEPGWSPIN, SOLVEG_INITSWC, &
            RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt,dxrt,&
                    GWBASESWCRT,route_topo_f,route_chan_f,route_link_f,route_lake_f, &
                    route_direction_f,route_order_f,gwbasmskfil, geo_finegrid_flnm,&
                    gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl, &
                    order_to_write , rst_typ
#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
          open(30, file="hydro.namelist", form="FORMATTED")
          read(30, HYDRO_nlist, iostat=ierr)
          close(30)
#ifdef MPP_LAND
       endif
#endif


#ifdef MPP_LAND
!  call mpp_land_bcast_real1(DT)
  call mpp_land_bcast_int1(SPLIT_OUTPUT_COUNT)
  call mpp_land_bcast_int1(IGRID)
  call mpp_land_bcast_real1(out_dt)
  call mpp_land_bcast_real1(rst_dt)
  call mpp_land_bcast_int1(HIRES_OUT)
  call mpp_land_bcast_int1(DEEPGWSPIN)
  call mpp_land_bcast_int1(SOLVEG_INITSWC)
#endif


#ifdef MPP_LAND
      call mpp_land_bcast_int1(nlst%NSOIL)
      do i = 1, nlst%NSOIL
        call mpp_land_bcast_real1(nlst%ZSOIL8(i))
      end do
#ifdef HYDRO_D
      write(6,*) "nlst%NSOIL = ", nlst%NSOIL
      write(6,*) "nlst%ZSOIL8 = ",nlst%ZSOIL8
#endif
#endif

!  nlst%DT = DT
  nlst%RESTART_FILE = RESTART_FILE
  nlst%HISTORY_OUTPUT = HISTORY_OUTPUT
  nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT
  nlst%IGRID = IGRID
  nlst%geo_static_flnm = geo_static_flnm
  nlst%out_dt = out_dt
  nlst%rst_dt = rst_dt
  nlst%HIRES_OUT = HIRES_OUT
  nlst%DEEPGWSPIN = DEEPGWSPIN
  nlst%SOLVEG_INITSWC = SOLVEG_INITSWC

  write(nlst%hgrid,'(I1)') igrid


  if(RESTART_FILE .eq. "") rst_typ = 0

#ifdef MPP_LAND
  !bcast namelist variable.
  call mpp_land_bcast_int1(rt_option)
  call mpp_land_bcast_int1(CHANRTSWCRT)
  call mpp_land_bcast_int1(channel_option)
  call mpp_land_bcast_int1(SUBRTSWCRT)
  call mpp_land_bcast_int1(OVRTSWCRT)
  call mpp_land_bcast_int1(AGGFACTRT)
  call mpp_land_bcast_real1(DTRT)
  call mpp_land_bcast_real1(DXRT)
  call mpp_land_bcast_int1(GWBASESWCRT)
  call mpp_land_bcast_int1(GW_RESTART)
  call mpp_land_bcast_int1(RSTRT_SWC  )
  call mpp_land_bcast_int1(TERADJ_SOLAR)
  call mpp_land_bcast_int1(sys_cpl)
  call mpp_land_bcast_int1(rst_typ)
  call mpp_land_bcast_int1(order_to_write)
#endif
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART 
    nlst%RSTRT_SWC   = RSTRT_SWC  
    nlst%channel_option = channel_option
    nlst%DTRT   = DTRT
    nlst%DTCT   = DTRT
    nlst%SUBRTSWCRT = SUBRTSWCRT
    nlst%OVRTSWCRT = OVRTSWCRT
    nlst%dxrt0 = dxrt
    nlst%AGGFACTRT = AGGFACTRT
    nlst%GWBASESWCRT = GWBASESWCRT
    nlst%TERADJ_SOLAR = TERADJ_SOLAR
    nlst%sys_cpl = sys_cpl
    nlst%rst_typ = rst_typ
    nlst%order_to_write = order_to_write
! files
    nlst%route_topo_f   =  route_topo_f
    nlst%route_chan_f = route_chan_f 
    nlst%route_link_f = route_link_f
    nlst%route_lake_f =route_lake_f
    nlst%route_direction_f =  route_direction_f
    nlst%route_order_f =  route_order_f
    nlst%gwbasmskfil =  gwbasmskfil
    nlst%gwstrmfil =  gwstrmfil
    nlst%geo_finegrid_flnm =  geo_finegrid_flnm

#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
#endif
#ifdef HYDRO_D
     write(6,*) "output of the namelist file "

    write(6,*) " nlst%RT_OPTION ", RT_OPTION
    write(6,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
    write(6,*) " nlst%GW_RESTART  ", GW_RESTART 
    write(6,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
    write(6,*) " nlst%channel_option ", channel_option
    write(6,*) " nlst%DTRT   ", DTRT
    write(6,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
    write(6,*) " nlst%OVRTSWCRT ", OVRTSWCRT
    write(6,*) " nlst%dxrt0 ", dxrt
    write(6,*) " nlst%AGGFACTRT ", AGGFACTRT
    write(6,*) " nlst%GWBASESWCRT ", GWBASESWCRT
    write(6,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
    write(6,*) " nlst%sys_cpl ", sys_cpl
    write(6,*) " nlst%rst_typ ", rst_typ
    write(6,*) " nlst%order_to_write ", order_to_write
    write(6,*) " nlst%route_topo_f   ",  route_topo_f
    write(6,*) " nlst%route_chan_f ", route_chan_f 
    write(6,*) " nlst%route_link_f ", route_link_f
    write(6,*) " nlst%route_lake_f ",route_lake_f
    write(6,*) " nlst%route_direction_f ",  route_direction_f
    write(6,*) " nlst%route_order_f ",  route_order_f
    write(6,*) " nlst%gwbasmskfil ",  gwbasmskfil
    write(6,*) " nlst%gwstrmfil ",  gwstrmfil
    write(6,*) " nlst%geo_finegrid_flnm ",  geo_finegrid_flnm
#endif
#ifdef MPP_LAND
  endif
#endif

#ifdef MPP_LAND
  !bcast other  variable.
      call mpp_land_bcast_real1(nlst%dt)
#endif
      return
    end subroutine read_rt_nlst


end module module_namelist
