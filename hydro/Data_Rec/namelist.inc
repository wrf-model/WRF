   TYPE namelist_rt_field  
      
       integer :: nsoil, SOLVEG_INITSWC
       real,allocatable,dimension(:) :: ZSOIL8
       real out_dt, rst_dt, dt
       integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
       character(len=256)  :: restart_file = ""
       logical            :: history_output
       integer            :: split_output_count
       integer :: igrid
       character(len=256) :: geo_static_flnm = ""
       integer  :: DEEPGWSPIN
       integer :: HIRES_OUT, order_to_write, rst_typ
       
!      additional character
       character :: hgrid
       character(len=19) :: olddate="123456"
       character(len=19) :: startdate="123456"



          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                  SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                  GWBASESWCRT, GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                  sys_cpl
          real:: DTRT,dxrt0
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""

   END TYPE namelist_rt_field 

