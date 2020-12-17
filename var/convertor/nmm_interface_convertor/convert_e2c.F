      program convert_etoc
!--------------------Documentation Block----------------------------
!
! Author : Sujata Pattanayak, CAS, IIT Delhi, India

! Date : 20th May, 2008 ; Modified : 15 August 2008

! This is the main program for the grid conversion purpose 
! which is essential to initialize WRF-NMM with WRF-Var.
! This program reads an "wrfinput_d01" file from WRF-NMM (E-grid)
! and write a new NetCDF file "wrfoutput_d01" in C-grid.

! This program calls several subroutines.
! Subroutines Included :
!  1) get_dims_cdf		: Used to get the dimension of any variable. 
!  2) get_var_0d_real_cdf	: Used to get single valued variable.
!  3) put_var_0d_real_cdf	: Used to put single valued variable.
!  4) get_var_1d_real_cdf	: Used to get one dimensional variable.
!  5) put_var_1d_real_cdf	: Used to put one dimensional variable.
!  6) get_var_2d_real_cdf	: Used to get two dimensional variable.
!  7) put_var_2d_real_cdf	: Used to put two dimensional variable.
!  8) get_var_3d_real_cdf	: Used to get three dimensional variable.
!  9) put_var_3d_real_cdf	: Used to put three dimensional variable.
! 10) get_var_2d_int_cdf	: Used to get two dimensional integer variable.
! 11) put_var_2d_int_cdf	: Used to put twodimensional integer variable.
! 12) fill_nmm_grid2		: Used to generate two dimensional A-grid structure.
! 13) fill_nmm_grid3		: Used to generate three dimensional A-grid structure.
! 14) fill_arw_ugrid		: Used to generate 3D staggered west-east points (U component).
! 15) fill_arw_vgrid		: Used to generate 3D staggered south-north points (V component).
! 16) fill_arw_xllu_grid	: Used to generate 2D staggered west-east point.
! 17) fill_arw_xllv_grid	: Used to generate 2D staggered south-north point.

!-------------------------------------------------------------------

      implicit none

      include 'netcdf.inc'

      integer cdfid, rcode
      integer uid, vid, wid, tid, qvaporid, qcloudid, qrainid, qiceid, smoisid, tslbid
      integer u10id, v10id, t2id, q2id, psfcid, muid, mapfac_mxid, mapfac_uxid, mapfac_vxid
      integer znuid, znwid, ptopid, cf1id, cf2id, timesid, mu0id
      integer hgtid, tskid, sstid, co_fid, tmnid, xlatid, xlongid, xlat_uid, xlong_uid, xlat_vid, xlong_vid
      integer snowcid, lu_indexid, landmaskid, xlandid, seaiceid, ivgtypid, isltypid, vegfraid, snowhid
      integer latid, lonid, timid, levid, lev_stagid, dateid, lon_stagid, lat_stagid, soil_layerid
      character (len=100):: wrfinput_d01, &
                            wrfoutput_d01
      character(len=512) :: input_file, &
			    output_file
      integer, parameter :: max_variables = 25
      real, parameter    :: dellat = 0.1725
      character(len=20) :: var(max_variables)
      logical  :: debug
      integer :: ids, ide, jds, jde, kds, kde
      integer :: ndims
      integer :: value
      integer :: id_data, istart, iend
      integer ::  i1, i2, i3, time
      integer :: i,j,k
      integer :: nx, ny, nz, nnx, nny, nnz, nnux, nnuy, nnvx, nnvy, nnmx, nnmy, nnnx, nnny, nnnz
      real :: data, ptop
      real :: PI_2, D2R, R2D
      real, parameter :: ERAD=6371200.
      real, parameter :: dx=0.1725, dy=0.1725
      real, parameter    :: PDTOP= 38424.47
      real, parameter    :: PI     = 3.1415926
      integer, parameter :: TDIMS =2
      integer TSTART(TDIMS), TCOUNT(TDIMS)
      real :: dx_meter, dy_meter
      integer, dimension (4) :: dims
      real, allocatable, dimension(:,:,:) :: u_e, v_e, w_e, t_e, q_e
      real, allocatable, dimension(:,:,:) :: u_a, v_a, w_a, t_a, q_a
      real, allocatable, dimension(:,:,:) :: u_c, v_c
      real, allocatable, dimension(:,:,:) :: smc_e, stc_e
      real, allocatable, dimension(:,:,:) :: smc_a, stc_a
      real, allocatable, dimension(:,:) :: pd_e, glat_e, glon_e, fis_e
      real, allocatable, dimension(:,:) :: pd_a, glat_a, glon_a, fis_a, mu_a
      real, allocatable, dimension(:,:) :: clat_e, mapfac_ux_e, mapfac_vx_e, mapfac_mx_e
      real, allocatable, dimension(:,:) :: clat_a, mapfac_ux_a, mapfac_vx_a, mapfac_mx_a
      real, allocatable, dimension(:,:) :: mapfac_ux_c, mapfac_vx_c
      real, allocatable, dimension(:,:) :: xlat_u_c, xlong_u_c, xlat_v_c, xlong_v_c
      real, allocatable, dimension(:,:) :: u10_e, v10_e, t2_e, qs_e, dx_nmm_e, tsk_e, sst_e, co_f_e
      real, allocatable, dimension(:,:) :: u10_a, v10_a, t2_a, qs_a, dx_nmm_a, tsk_a, sst_a, co_f_a
      real, allocatable, dimension(:,:) :: soiltb_e, acsnow_e, lu_index_e, landmask_e, seaice_e, ivgtyp_e
      real, allocatable, dimension(:,:) :: soiltb_a, acsnow_a, lu_index_a, landmask_a, seaice_a, ivgtyp_a
      real, allocatable, dimension(:,:) :: isltyp_e, vegfra_e, snowh_e
      real, allocatable, dimension(:,:) :: isltyp_a, vegfra_a, snowh_a
      real, allocatable, dimension(:) :: eta1, eta2
      integer :: datestrlen, ew, sn, bt, bt_s, ew_s, sn_s, soil_layer
      integer :: diff_opt, km_opt, damp_opt, mp_physics, ra_lw_physics, ra_sw_physics, sst_update, map_proj
      integer :: sf_sfclay_physics, sf_surface_physics, bl_pbl_physics, cu_physics, surface_input_source
      real    :: khdif, kvdif, dt, cen_lat, cen_lon, truelat1, truelat2, mode_cen_lat, stand_lon
      character(len=30)   :: FCST_TIME, START_DATE, SIM_START_DATE

      call getarg( 1, FCST_TIME )
      call getarg( 2, START_DATE)
      call getarg( 3, SIM_START_DATE)
 
      TSTART(1) =1
      TSTART(2) =1
      TCOUNT(1) =19
      TCOUNT(2) =1

!-----------------------------------------------------------------
!Define dimension of the "OUTPUT" NetCDF file and specify the
!values for  the global attributes.
!-----------------------------------------------------------------
      time       = 1
      datestrlen = 19
      ew         = 697
      sn         = 429
      bt         =  50
      bt_s       =  51
      ew_s       = 698
      sn_s       = 430
      soil_layer = 5

      diff_opt			=1
      km_opt 			=1
      damp_opt 			=1
      mp_physics 		=5
      ra_lw_physics 		=99
      ra_sw_physics		=99
      sf_sfclay_physics 	=2
      sf_surface_physics 	=2
      bl_pbl_physics 		=2
      cu_physics 		=2
      surface_input_source	=1
      sst_update 		=0
      map_proj			=6
      khdif 			=0.
      kvdif 			=0.
      dt			=60.
      cen_lat 			=21.5
      cen_lon 			=78.
      truelat1 			=1.e+20
      truelat2 			=1.e+20
      mode_cen_lat 		=21.5
      stand_lon			=-78.

      input_file = 'wrfinput_d01'
      output_file = 'wrfoutput_d01'
!-----------------------------------------------------------------
      PI_2  = ACOS(0.)
      D2R = PI_2/90.
      R2D = 1./D2R
      dx_meter = D2R*ERAD*dx
      dy_meter = D2R*ERAD*dy
     
!-----------------------------------------------------------------
!Creating a New NetCDF file and define the dimensions and attributes
!-----------------------------------------------------------------
      cdfid  = nccre(output_file, NCCLOB, rcode)

!------Dimensions--------

      timid      = ncddef(cdfid, 'Time', time, rcode)
      dateid     = ncddef(cdfid, 'DateStrLen1', datestrlen, rcode)
      lonid      = ncddef(cdfid, 'west_east', ew, rcode)
      latid      = ncddef(cdfid, 'south_north', sn, rcode)
      levid      = ncddef(cdfid, 'bottom_top', bt, rcode)
      lev_stagid = ncddef(cdfid, 'bottom_top_stag', bt_s, rcode)
      lon_stagid = ncddef(cdfid, 'west_east_stag', ew_s, rcode)
      lat_stagid = ncddef(cdfid, 'south_north_stag', sn_s, rcode)
      soil_layerid = ncddef(cdfid, 'soil_layers_stag', soil_layer, rcode)

!------define additional variable----------------------------------

      timesid= ncvdef(cdfid, 'Times', ncchar, 2,(/ dateid, timid /),rcode)
      uid= ncvdef(cdfid, 'U', ncfloat, 4,(/ lon_stagid, latid, levid, timid /),rcode)
      vid= ncvdef(cdfid, 'V', ncfloat, 4,(/ lonid, lat_stagid, levid, timid /),rcode)
      wid= ncvdef(cdfid, 'W', ncfloat, 4,(/ lonid, latid, lev_stagid, timid /),rcode)
      tid= ncvdef(cdfid, 'T', ncfloat, 4,(/ lonid, latid, levid, timid /),rcode)
      qvaporid= ncvdef(cdfid, 'QVAPOR', ncfloat, 4,(/ lonid, latid, levid, timid /),rcode)
      qcloudid= ncvdef(cdfid, 'QCLOUD', ncfloat, 4,(/ lonid, latid, levid, timid /),rcode)
      qrainid= ncvdef(cdfid, 'QRAIN', ncfloat, 4,(/ lonid, latid, levid, timid /),rcode)
      qiceid= ncvdef(cdfid, 'QICE', ncfloat, 4,(/ lonid, latid, levid, timid /),rcode)
      u10id= ncvdef(cdfid, 'U10', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      v10id= ncvdef(cdfid, 'V10', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      t2id= ncvdef(cdfid, 'T2', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      q2id= ncvdef(cdfid, 'Q2', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      psfcid= ncvdef(cdfid, 'PSFC', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      muid= ncvdef(cdfid, 'MU', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      znuid= ncvdef(cdfid, 'ZNU', ncfloat, 2,(/ levid, timid /),rcode)
      znwid= ncvdef(cdfid, 'ZNW', ncfloat, 2,(/ lev_stagid, timid /),rcode)
      ptopid= ncvdef(cdfid, 'P_TOP', ncfloat, 1,(/ timid /),rcode)
      mapfac_uxid= ncvdef(cdfid, 'MAPFAC_UX', ncfloat, 3,(/ lon_stagid, latid, timid /),rcode)
      mapfac_vxid= ncvdef(cdfid, 'MAPFAC_VX', ncfloat, 3,(/ lonid, lat_stagid, timid /),rcode)
      mapfac_mxid= ncvdef(cdfid, 'MAPFAC_MX', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      mu0id= ncvdef(cdfid, 'MU0', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      cf1id= ncvdef(cdfid, 'CF1', ncfloat, 1,(/ timid /),rcode)
      cf2id= ncvdef(cdfid, 'CF2', ncfloat, 1,(/ timid /),rcode)
      hgtid= ncvdef(cdfid, 'HGT', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      tskid= ncvdef(cdfid, 'TSK', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      sstid= ncvdef(cdfid, 'SST', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      co_fid= ncvdef(cdfid, 'F', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      tmnid= ncvdef(cdfid, 'TMN', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      xlatid= ncvdef(cdfid, 'XLAT', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      xlongid= ncvdef(cdfid, 'XLONG', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      xlat_uid= ncvdef(cdfid, 'XLAT_U', ncfloat, 3,(/ lon_stagid, latid, timid /),rcode)
      xlong_uid= ncvdef(cdfid, 'XLONG_U', ncfloat, 3,(/ lon_stagid, latid, timid /),rcode)
      xlat_vid= ncvdef(cdfid, 'XLAT_V', ncfloat, 3,(/ lonid, lat_stagid, timid /),rcode)
      xlong_vid= ncvdef(cdfid, 'XLONG_V', ncfloat, 3,(/ lonid, lat_stagid, timid /),rcode)
      snowcid= ncvdef(cdfid, 'SNOWC', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      lu_indexid= ncvdef(cdfid, 'LU_INDEX', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      landmaskid= ncvdef(cdfid, 'LANDMASK', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      xlandid= ncvdef(cdfid, 'XLAND', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      smoisid= ncvdef(cdfid, 'SMOIS', ncfloat, 4,(/ lonid, latid, soil_layerid, timid /),rcode)
      tslbid= ncvdef(cdfid, 'TSLB', ncfloat, 4,(/ lonid, latid, soil_layerid, timid /),rcode)
      seaiceid= ncvdef(cdfid, 'SEAICE', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      ivgtypid= ncvdef(cdfid, 'IVGTYP', nclong, 3,(/ lonid, latid, timid /),rcode)
      isltypid= ncvdef(cdfid, 'ISLTYP', nclong, 3,(/ lonid, latid, timid /),rcode)
      vegfraid= ncvdef(cdfid, 'VEGFRA', ncfloat, 3,(/ lonid, latid, timid /),rcode)
      snowhid= ncvdef(cdfid, 'SNOWH', ncfloat, 3,(/ lonid, latid, timid /),rcode)

!------Attributes for U -----------------------------------------
      call ncapt(cdfid,uid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,uid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,uid,'description',NCCHAR,16,'x-wind component',rcode)
      call ncaptc(cdfid,uid,'units',NCCHAR,5,'m s-1',rcode)
      call ncaptc(cdfid,uid,'stagger',NCCHAR,1,'X',rcode)
      call ncaptc(cdfid,uid,'coordinates',NCCHAR,14,'XLONG_U XLAT_U',rcode)
!------Attributes for V -----------------------------------------
      call ncapt(cdfid,vid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,vid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,vid,'description',NCCHAR,16,'y-wind component',rcode)
      call ncaptc(cdfid,vid,'units',NCCHAR,5,'m s-1',rcode)
      call ncaptc(cdfid,vid,'stagger',NCCHAR,1,'Y',rcode)
      call ncaptc(cdfid,vid,'coordinates',NCCHAR,14,'XLONG_V XLAT_V',rcode)
!------Attributes for W -----------------------------------------
      call ncapt(cdfid,wid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,wid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,wid,'description',NCCHAR,16,'z-wind component',rcode)
      call ncaptc(cdfid,wid,'units',NCCHAR,5,'m s-1',rcode)
      call ncaptc(cdfid,wid,'stagger',NCCHAR,1,'Z',rcode)
      call ncaptc(cdfid,wid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for T -----------------------------------------
      call ncapt(cdfid,tid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,tid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,tid,'description',NCCHAR,20,'Sensible temperature',rcode)
      call ncaptc(cdfid,tid,'units',NCCHAR,1,'K',rcode)
      call ncaptc(cdfid,tid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,tid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for QVAPOR-------------------------------------
      call ncapt(cdfid,qvaporid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,qvaporid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,qvaporid,'description',NCCHAR,20,'Specific humidity',rcode)
      call ncaptc(cdfid,qvaporid,'units',NCCHAR,7,'kg kg-1',rcode)
      call ncaptc(cdfid,qvaporid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,qvaporid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for QCLOUD-------------------------------------
      call ncapt(cdfid,qcloudid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,qcloudid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,qcloudid,'description',NCCHAR,24,'Cloud water mixing ratio',rcode)
      call ncaptc(cdfid,qcloudid,'units',NCCHAR,7,'kg kg-1',rcode)
      call ncaptc(cdfid,qcloudid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,qcloudid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for QRAIN--------------------------------------
      call ncapt(cdfid,qrainid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,qrainid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,qrainid,'description',NCCHAR,23,'Rain water mixing ratio',rcode)
      call ncaptc(cdfid,qrainid,'units',NCCHAR,7,'kg kg-1',rcode)
      call ncaptc(cdfid,qrainid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,qrainid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for QICE--------------------------------------
      call ncapt(cdfid,qiceid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,qiceid,'MemoryOrder',NCCHAR, 3,'XYZ' , rcode)
      call ncaptc(cdfid,qiceid,'description',NCCHAR,16,'Ice mixing ratio',rcode)
      call ncaptc(cdfid,qiceid,'units',NCCHAR,7,'kg kg-1',rcode)
      call ncaptc(cdfid,qiceid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,qiceid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for U10----------------------------------------
      call ncapt(cdfid,u10id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,u10id,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,u10id,'description',NCCHAR,9,'U at 10 M',rcode)
      call ncaptc(cdfid,u10id,'units',NCCHAR,5,'m s-1',rcode)
      call ncaptc(cdfid,u10id,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,u10id,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for V10----------------------------------------
      call ncapt(cdfid,v10id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,v10id,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,v10id,'description',NCCHAR,9,'V at 10 M',rcode)
      call ncaptc(cdfid,v10id,'units',NCCHAR,5,'m s-1',rcode)
      call ncaptc(cdfid,v10id,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,v10id,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for T2-----------------------------------------
      call ncapt(cdfid,t2id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,t2id,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,t2id,'description',NCCHAR,11,'TEMP at 2 M',rcode)
      call ncaptc(cdfid,t2id,'units',NCCHAR,1,'K',rcode)
      call ncaptc(cdfid,t2id,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,t2id,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for Q2-----------------------------------------
      call ncapt(cdfid,q2id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,q2id,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,q2id,'description',NCCHAR,8,'Q at 2 M',rcode)
      call ncaptc(cdfid,q2id,'units',NCCHAR,7,'kg kg-1',rcode)
      call ncaptc(cdfid,q2id,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,q2id,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for PSFC---------------------------------------
      call ncapt(cdfid,psfcid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,psfcid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,psfcid,'description',NCCHAR,20,'Mass in sigma domain',rcode)
      call ncaptc(cdfid,psfcid,'units',NCCHAR,2,'Pa',rcode)
      call ncaptc(cdfid,psfcid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,psfcid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for MU-----------------------------------------
      call ncapt(cdfid,muid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,muid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,muid,'description',NCCHAR,23,'Mass in pressure domain',rcode)
      call ncaptc(cdfid,muid,'units',NCCHAR,2,'Pa',rcode)
      call ncaptc(cdfid,muid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,muid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for ZNU----------------------------------------
      call ncapt(cdfid,znuid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,znuid,'MemoryOrder',NCCHAR, 1,'Z' , rcode)
      call ncaptc(cdfid,znuid,'description',NCCHAR,21,'Sigma in sigma domain',rcode)
      call ncaptc(cdfid,znuid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,znuid,'stagger',NCCHAR,0,'',rcode)
!------Attributes for ZNW----------------------------------------
      call ncapt(cdfid,znwid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,znwid,'MemoryOrder',NCCHAR, 1,'Z' , rcode)
      call ncaptc(cdfid,znwid,'description',NCCHAR,24,'Sigma in pressure domain',rcode)
      call ncaptc(cdfid,znwid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,znwid,'stagger',NCCHAR,0,'',rcode)
!------Attributes for P_TOP--------------------------------------
      call ncapt(cdfid,ptopid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,ptopid,'MemoryOrder',NCCHAR, 1,'0' , rcode)
      call ncaptc(cdfid,ptopid,'description',NCCHAR,25,'pressure at top of domain',rcode)
      call ncaptc(cdfid,ptopid,'units',NCCHAR,2,'Pa',rcode)
      call ncaptc(cdfid,ptopid,'stagger',NCCHAR,0,'',rcode)
!------Attributes for MAPFAC_MX----------------------------------
      call ncapt(cdfid,mapfac_mxid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,mapfac_mxid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,mapfac_mxid,'description',NCCHAR,42,'Map scale factor on mass grid, x direction',rcode)
      call ncaptc(cdfid,mapfac_mxid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,mapfac_mxid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,mapfac_mxid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for MAPFAC_UX----------------------------------
      call ncapt(cdfid,mapfac_uxid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,mapfac_uxid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,mapfac_uxid,'description',NCCHAR,39,'Map scale factor on u-grid, x direction',rcode)
      call ncaptc(cdfid,mapfac_uxid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,mapfac_uxid,'stagger',NCCHAR,1,'X',rcode)
      call ncaptc(cdfid,mapfac_uxid,'coordinates',NCCHAR,14,'XLONG_U XLAT_U',rcode)
!------Attributes for MAPFAC_VX----------------------------------
      call ncapt(cdfid,mapfac_vxid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,mapfac_vxid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,mapfac_vxid,'description',NCCHAR,39,'Map scale factor on v-grid, x direction',rcode)
      call ncaptc(cdfid,mapfac_vxid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,mapfac_vxid,'stagger',NCCHAR,1,'Y',rcode)
      call ncaptc(cdfid,mapfac_vxid,'coordinates',NCCHAR,14,'XLONG_V XLAT_V',rcode)
!------Attributes for MU0----------------------------------------
      call ncapt(cdfid,mu0id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,mu0id,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,mu0id,'description',NCCHAR,18,'East-west distance',rcode)
      call ncaptc(cdfid,mu0id,'units',NCCHAR,1,'m',rcode)
      call ncaptc(cdfid,mu0id,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,mu0id,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for CF1----------------------------------------
      call ncapt(cdfid,cf1id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,cf1id,'MemoryOrder',NCCHAR, 1,'0' , rcode)
      call ncaptc(cdfid,cf1id,'description',NCCHAR,18,'East-west distance',rcode)
      call ncaptc(cdfid,cf1id,'units',NCCHAR,6,'degree',rcode)
      call ncaptc(cdfid,cf1id,'stagger',NCCHAR,0,'',rcode)
!------Attributes for CF2----------------------------------------
      call ncapt(cdfid,cf2id,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,cf2id,'MemoryOrder',NCCHAR, 1,'0' , rcode)
      call ncaptc(cdfid,cf2id,'description',NCCHAR,20,'North-south distance',rcode)
      call ncaptc(cdfid,cf2id,'units',NCCHAR,6,'degree',rcode)
      call ncaptc(cdfid,cf2id,'stagger',NCCHAR,0,'',rcode)
!------Attributes for HGT----------------------------------------
      call ncapt(cdfid,hgtid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,hgtid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,hgtid,'description',NCCHAR,14,'Terrain height',rcode)
      call ncaptc(cdfid,hgtid,'units',NCCHAR,1,'m',rcode)
      call ncaptc(cdfid,hgtid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,hgtid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for TSK----------------------------------------
      call ncapt(cdfid,tskid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,tskid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,tskid,'description',NCCHAR,15,'Skin temprature',rcode)
      call ncaptc(cdfid,tskid,'units',NCCHAR,1,'K',rcode)
      call ncaptc(cdfid,tskid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,tskid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for SST----------------------------------------
      call ncapt(cdfid,sstid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,sstid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,sstid,'description',NCCHAR,15,'Skin temprature',rcode)
      call ncaptc(cdfid,sstid,'units',NCCHAR,1,'K',rcode)
      call ncaptc(cdfid,sstid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,sstid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for F------------------------------------------
      call ncapt(cdfid,co_fid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,co_fid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,co_fid,'description',NCCHAR,27,'Coriolis sine latitute term',rcode)
      call ncaptc(cdfid,co_fid,'units',NCCHAR,3,'s-1',rcode)
      call ncaptc(cdfid,co_fid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,co_fid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for TMN----------------------------------------
      call ncapt(cdfid,tmnid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,tmnid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,tmnid,'description',NCCHAR,15,'Skin temprature',rcode)
      call ncaptc(cdfid,tmnid,'units',NCCHAR,1,'K',rcode)
      call ncaptc(cdfid,tmnid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,tmnid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for XLAT---------------------------------------
      call ncapt(cdfid,xlatid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlatid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlatid,'description',NCCHAR,27,'LATITUDE, SOUTH IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlatid,'units',NCCHAR,12,'degree_north',rcode)
      call ncaptc(cdfid,xlatid,'stagger',NCCHAR,0,'',rcode)
!------Attributes for XLONG--------------------------------------
      call ncapt(cdfid,xlongid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlongid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlongid,'description',NCCHAR,26,'LATITUDE, WEST IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlongid,'units',NCCHAR,12,'degree_east',rcode)
      call ncaptc(cdfid,xlongid,'stagger',NCCHAR,0,'',rcode)
!------Attributes for XLAT_U-------------------------------------
      call ncapt(cdfid,xlat_uid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlat_uid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlat_uid,'description',NCCHAR,27,'LATITUDE, SOUTH IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlat_uid,'units',NCCHAR,12,'degree_north',rcode)
      call ncaptc(cdfid,xlat_uid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlat_uid,'coordinates',NCCHAR,14,'XLONG_U XLAT_U',rcode)
!------Attributes for XLONG_U------------------------------------
      call ncapt(cdfid,xlong_uid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlong_uid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlong_uid,'description',NCCHAR,26,'LATITUDE, WEST IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlong_uid,'units',NCCHAR,12,'degree_east',rcode)
      call ncaptc(cdfid,xlong_uid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlong_uid,'coordinates',NCCHAR,14,'XLONG_U XLAT_U',rcode)
!------Attributes for XLAT_V-------------------------------------
      call ncapt(cdfid,xlat_vid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlat_vid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlat_vid,'description',NCCHAR,27,'LATITUDE, SOUTH IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlat_vid,'units',NCCHAR,12,'degree_north',rcode)
      call ncaptc(cdfid,xlat_vid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlat_vid,'coordinates',NCCHAR,14,'XLONG_V XLAT_V',rcode)
!------Attributes for XLONG_V------------------------------------
      call ncapt(cdfid,xlong_vid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlong_vid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlong_vid,'description',NCCHAR,26,'LATITUDE, WEST IS NEGATIVE',rcode)
      call ncaptc(cdfid,xlong_vid,'units',NCCHAR,12,'degree_east',rcode)
      call ncaptc(cdfid,xlong_vid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlong_vid,'coordinates',NCCHAR,14,'XLONG_V XLAT_V',rcode)
!------Attributes for SNOWC--------------------------------------
      call ncapt(cdfid,snowcid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,snowcid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,snowcid,'description',NCCHAR,48,'FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)',rcode)
      call ncaptc(cdfid,snowcid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,snowcid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,snowcid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for LU_INDEX-----------------------------------
      call ncapt(cdfid,lu_indexid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,lu_indexid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,lu_indexid,'description',NCCHAR,17,'LAND USE CATAGORY',rcode)
      call ncaptc(cdfid,lu_indexid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,lu_indexid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,lu_indexid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for LANDMASK-----------------------------------
      call ncapt(cdfid,landmaskid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,landmaskid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,landmaskid,'description',NCCHAR,35,'LAND MASK (1 FOR LAND, 0 FOR WATER)',rcode)
      call ncaptc(cdfid,landmaskid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,landmaskid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,landmaskid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for XLAND--------------------------------------
      call ncapt(cdfid,xlandid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,xlandid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,xlandid,'description',NCCHAR,35,'LAND MASK (1 FOR LAND, 2 FOR WATER)',rcode)
      call ncaptc(cdfid,xlandid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlandid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,xlandid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for SMOIS--------------------------------------
      call ncapt(cdfid,smoisid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,smoisid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,smoisid,'description',NCCHAR,13,'Soil moisture',rcode)
      call ncaptc(cdfid,smoisid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,smoisid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,smoisid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for TSLB---------------------------------------
      call ncapt(cdfid,tslbid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,tslbid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,tslbid,'description',NCCHAR,16,'Soil temperature',rcode)
      call ncaptc(cdfid,tslbid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,tslbid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,tslbid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for SEAICE-------------------------------------
      call ncapt(cdfid,seaiceid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,seaiceid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,seaiceid,'description',NCCHAR,12,'SEA ICE FLAG',rcode)
      call ncaptc(cdfid,seaiceid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,seaiceid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,seaiceid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for IVGTYP-------------------------------------
      call ncapt(cdfid,ivgtypid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,ivgtypid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,ivgtypid,'description',NCCHAR,19,'VEGETATION CATAGORY',rcode)
      call ncaptc(cdfid,ivgtypid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,ivgtypid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,ivgtypid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for ISLTYP-------------------------------------
      call ncapt(cdfid,isltypid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,isltypid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,isltypid,'description',NCCHAR,22,'DOMINANT SOIL CATAGORY',rcode)
      call ncaptc(cdfid,isltypid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,isltypid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,isltypid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for VEGFRA-------------------------------------
      call ncapt(cdfid,vegfraid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,vegfraid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,vegfraid,'description',NCCHAR,19,'VEGETATION FRACTION',rcode)
      call ncaptc(cdfid,vegfraid,'units',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,vegfraid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,vegfraid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)
!------Attributes for SNOWH--------------------------------------
      call ncapt(cdfid,snowhid,'FieldType',NCLONG, 1,104, rcode)
      call ncaptc(cdfid,snowhid,'MemoryOrder',NCCHAR, 2,'XY' , rcode)
      call ncaptc(cdfid,snowhid,'description',NCCHAR,11,'SNOW HEIGHT',rcode)
      call ncaptc(cdfid,snowhid,'units',NCCHAR,1,'m',rcode)
      call ncaptc(cdfid,snowhid,'stagger',NCCHAR,0,'',rcode)
      call ncaptc(cdfid,snowhid,'coordinates',NCCHAR,10,'XLONG XLAT',rcode)

!-------Putting new global attributes-----------------------------

      call ncaptc(cdfid, ncglobal, 'TITLE', NCCHAR, 30, 'OUTPUT FROM CONVERSION PROGRAM', rcode)
      call ncaptc(cdfid, ncglobal, 'START_DATE', NCCHAR, 19, START_DATE, rcode)
      call ncaptc(cdfid, ncglobal, 'SIMULATION_START_DATE', NCCHAR, 19, SIM_START_DATE, rcode)
      call ncapt(cdfid, ncglobal, 'WEST-EAST_GRID_DIMENSION', NCLONG, 1,ew_s, rcode)
      call ncapt(cdfid, ncglobal, 'SOUTH-NORTH_GRID_DIMENSION', NCLONG, 1,sn_s, rcode)
      call ncapt(cdfid, ncglobal, 'BOTTOM-TOP_GRID_DIMENSION', NCLONG, 1,bt_s, rcode)
      call ncapt(cdfid, ncglobal, 'DX', NCFLOAT, 1,dx_meter, rcode)
      call ncapt(cdfid, ncglobal, 'DY', NCFLOAT, 1,dy_meter, rcode)
      call ncaptc(cdfid, ncglobal, 'GRIDTYPE', NCCHAR, 1, 'C', rcode)
      call ncapt(cdfid, ncglobal, 'DIFF_OPT', NCLONG, 1,diff_opt, rcode)
      call ncapt(cdfid, ncglobal, 'KM_OPT', NCLONG, 1,km_opt, rcode)
      call ncapt(cdfid, ncglobal, 'DAMP_OPT', NCLONG, 1,damp_opt, rcode)
      call ncapt(cdfid, ncglobal, 'KHDIF', NCFLOAT, 1,khdif, rcode)
      call ncapt(cdfid, ncglobal, 'KVDIF', NCFLOAT, 1,kvdif, rcode)
      call ncapt(cdfid, ncglobal, 'MP_PHYSICS', NCLONG, 1,mp_physics, rcode)
      call ncapt(cdfid, ncglobal, 'RA_LW_PHYSICS', NCLONG, 1,ra_lw_physics, rcode)
      call ncapt(cdfid, ncglobal, 'RA_SW_PHYSICS', NCLONG, 1,ra_sw_physics, rcode)
      call ncapt(cdfid, ncglobal, 'SF_SFCLAY_PHYSICS', NCLONG, 1,sf_sfclay_physics, rcode)
      call ncapt(cdfid, ncglobal, 'SF_SURFACE_PHYSICS', NCLONG, 1,sf_surface_physics, rcode)
      call ncapt(cdfid, ncglobal, 'BL_PBL_PHYSICS', NCLONG, 1,bl_pbl_physics, rcode)
      call ncapt(cdfid, ncglobal, 'CU_PHYSICS', NCLONG, 1,cu_physics, rcode)
      call ncapt(cdfid, ncglobal, 'SURFACE_INPUT_SOURCE', NCLONG, 1,surface_input_source, rcode)
      call ncapt(cdfid, ncglobal, 'SST_UPDATE', NCLONG, 1,sst_update, rcode)
      call ncapt(cdfid, ncglobal, 'WEST-EAST_PATCH_START_UNSTAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'WEST-EAST_PATCH_END_UNSTAG', NCLONG, 1,ew, rcode)
      call ncapt(cdfid, ncglobal, 'WEST-EAST_PATCH_START_STAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'WEST-EAST_PATCH_END_STAG', NCLONG, 1,ew_s, rcode)
      call ncapt(cdfid, ncglobal, 'SOUTH-NORTH_PATCH_START_UNSTAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'SOUTH-NORTH_PATCH_END_UNSTAG', NCLONG, 1,sn, rcode)
      call ncapt(cdfid, ncglobal, 'SOUTH-NORTH_PATCH_START_STAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'SOUTH-NORTH_PATCH_END_STAG', NCLONG, 1,sn_s, rcode)
      call ncapt(cdfid, ncglobal, 'BOTTOM_TOP_PATCH_START_UNSTAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'BOTTOM_TOP_PATCH_END_UNSTAG', NCLONG, 1,bt, rcode)
      call ncapt(cdfid, ncglobal, 'BOTTOM_TOP_PATCH_START_STAG', NCLONG, 1,1, rcode)
      call ncapt(cdfid, ncglobal, 'BOTTOM_TOP_PATCH_END_STAG', NCLONG, 1,bt_s, rcode)
      call ncapt(cdfid, ncglobal, 'DT', NCFLOAT, 1,dt, rcode)
      call ncapt(cdfid, ncglobal, 'CEN_LAT', NCFLOAT, 1,cen_lat, rcode)
      call ncapt(cdfid, ncglobal, 'CEN_LON', NCFLOAT, 1,cen_lon, rcode)
      call ncapt(cdfid, ncglobal, 'TRUELAT1', NCFLOAT, 1,truelat1, rcode)
      call ncapt(cdfid, ncglobal, 'TRUELAT2', NCFLOAT, 1,truelat2, rcode)
      call ncapt(cdfid, ncglobal, 'MODE_CEN_LAT', NCFLOAT, 1,mode_cen_lat, rcode)
      call ncapt(cdfid, ncglobal, 'STAND_LON', NCFLOAT, 1,stand_lon, rcode)
      call ncapt(cdfid, ncglobal, 'MAP_PROJ', NCLONG, 1,map_proj, rcode)

!------Close the new file------------------------------------------
      call ncendf(cdfid, rcode)
      call ncvptc(cdfid, timesid, TSTART, TCOUNT,FCST_TIME,30, rcode)
      call ncclos(cdfid, rcode)
!---------------------------------------------------------------------
      call get_dims_cdf(input_file,'U', dims, ndims, debug)
      call get_dims_cdf(input_file,'V', dims, ndims, debug)
      call get_dims_cdf(input_file,'T', dims, ndims, debug)
      call get_dims_cdf(input_file,'Q', dims, ndims, debug)

        allocate(u_e(dims(1), dims(2), dims(3)))
        allocate(v_e(dims(1), dims(2), dims(3)))
        allocate(t_e(dims(1), dims(2), dims(3)))
        allocate(q_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'U',u_e, &
                             dims(1), dims(2), dims(3),1, debug )
      call get_var_3d_real_cdf(input_file, 'V',v_e, &
                             dims(1), dims(2), dims(3),1, debug )
      call get_var_3d_real_cdf(input_file, 'T',t_e, &
                             dims(1), dims(2), dims(3),1, debug )
      call get_var_3d_real_cdf(input_file, 'Q',q_e, &
                             dims(1), dims(2), dims(3),1, debug )
        nx = dims(1)
        ny = dims(2)
        nz = dims(3)
        nnx = 2*nx-1 ; nny = ny ; nnz = nz
        allocate(u_a(nnx,nny,nnz))
        allocate(v_a(nnx,nny,nnz))
        allocate(t_a(nnx,nny,nnz))
        allocate(q_a(nnx,nny,nnz))
        call fill_nmm_grid3(u_e,nx,ny,nz,u_a,2)
        nnux = nnx+1 ; nnuy = nny ; nnnz = nnz
        allocate(u_c(nnux,nnuy,nnnz))
        call fill_arw_ugrid(u_a,nnx,nny,nnz,u_c)
        call fill_nmm_grid3(v_e,nx,ny,nz,v_a,2)
        nnvx = nnx ; nnvy = nny+1 ; nnnz = nnz
        allocate(v_c(nnvx,nnvy,nnnz))
        call fill_arw_vgrid(v_a,nnx,nny,nnz,v_c)
      call put_var_3d_real_cdf(output_file, 'U', u_c, &
                             nnux, nnuy, nnnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'V', v_c, &
                             nnvx, nnvy, nnnz, 1, debug )
        call fill_nmm_grid3(t_e,nx,ny,nz,t_a,1)
        call fill_nmm_grid3(q_e,nx,ny,nz,q_a,1)
      call put_var_3d_real_cdf(output_file, 'T', t_a, &
                             nnx, nny, nnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'QVAPOR', q_a, &
                             nnx, nny, nnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'QCLOUD', q_a, &
                             nnx, nny, nnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'QRAIN', q_a, &
                             nnx, nny, nnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'QICE', q_a, &
                             nnx, nny, nnz, 1, debug )
        deallocate (u_e)    
        deallocate (v_e)    
        deallocate (t_e)    
        deallocate (q_e)    
        deallocate (u_a)    
        deallocate (v_a)    
        deallocate (t_a)    
        deallocate (q_a)    
        deallocate (u_c)    
        deallocate (v_c)    
!------------------------------------------------------------------
!------------Two dimensional variables------------------------
!------------------------------------------------------------------
      call get_dims_cdf(input_file,'U10', dims, ndims, debug)
      call get_dims_cdf(input_file,'V10', dims, ndims, debug)
      call get_dims_cdf(input_file,'T2', dims, ndims, debug)
      call get_dims_cdf(input_file,'QS', dims, ndims, debug)
      call get_dims_cdf(input_file,'PD', dims, ndims, debug)
      call get_dims_cdf(input_file,'DX_NMM', dims, ndims, debug)
      call get_dims_cdf(input_file,'FIS', dims, ndims, debug)
      call get_dims_cdf(input_file,'TSK', dims, ndims, debug)
      call get_dims_cdf(input_file,'SST', dims, ndims, debug)
      call get_dims_cdf(input_file,'F', dims, ndims, debug)
      call get_dims_cdf(input_file,'SOILTB', dims, ndims, debug)
      call get_dims_cdf(input_file,'GLAT', dims, ndims, debug)
      call get_dims_cdf(input_file,'GLON', dims, ndims, debug)
      call get_dims_cdf(input_file,'ACSNOW', dims, ndims, debug)
      call get_dims_cdf(input_file,'LU_INDEX', dims, ndims, debug)
      call get_dims_cdf(input_file,'LANDMASK', dims, ndims, debug)
      call get_dims_cdf(input_file,'SEAICE', dims, ndims, debug)
      call get_dims_cdf(input_file,'IVGTYP', dims, ndims, debug)
      call get_dims_cdf(input_file,'ISLTYP', dims, ndims, debug)
      call get_dims_cdf(input_file,'VEGFRA', dims, ndims, debug)
      call get_dims_cdf(input_file,'SNOWH', dims, ndims, debug)
        allocate(u10_e(dims(1), dims(2)))
        allocate(v10_e(dims(1), dims(2)))
        allocate(t2_e(dims(1), dims(2)))
        allocate(qs_e(dims(1), dims(2)))
        allocate(pd_e(dims(1), dims(2)))
        allocate(dx_nmm_e(dims(1), dims(2)))
        allocate(fis_e(dims(1), dims(2)))
        allocate(tsk_e(dims(1), dims(2)))
        allocate(sst_e(dims(1), dims(2)))
        allocate(co_f_e(dims(1), dims(2)))
        allocate(soiltb_e(dims(1), dims(2)))
        allocate(glat_e(dims(1), dims(2)))
        allocate(glon_e(dims(1), dims(2)))
        allocate(acsnow_e(dims(1), dims(2)))
        allocate(lu_index_e(dims(1), dims(2)))
        allocate(landmask_e(dims(1), dims(2)))
        allocate(seaice_e(dims(1), dims(2)))
        allocate(ivgtyp_e(dims(1), dims(2)))
        allocate(isltyp_e(dims(1), dims(2)))
        allocate(vegfra_e(dims(1), dims(2)))
        allocate(snowh_e(dims(1), dims(2)))
        allocate(clat_e(dims(1), dims(2)))
        allocate(mapfac_mx_e(dims(1), dims(2)))
      call get_var_2d_real_cdf(input_file, 'U10',u10_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'V10',v10_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'T2',t2_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'QS',qs_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'PD',pd_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'DX_NMM',dx_nmm_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'FIS',fis_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'TSK',tsk_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'SST',sst_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'F',co_f_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'SOILTB',soiltb_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'GLAT',glat_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'GLON',glon_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'ACSNOW',acsnow_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'LU_INDEX',lu_index_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'LANDMASK',landmask_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'SEAICE',seaice_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_int_cdf(input_file, 'IVGTYP',ivgtyp_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_int_cdf(input_file, 'ISLTYP',isltyp_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'VEGFRA',vegfra_e, &
                             dims(1), dims(2),1, debug )
      call get_var_2d_real_cdf(input_file, 'SNOWH',snowh_e, &
                             dims(1), dims(2),1, debug )
        nx = dims(1)
        ny = dims(2)
        nnx = 2*nx-1 ; nny = ny
        nnmx = nx; nnmy = (ny+1)/2
        nnux = nnx+1; nnuy = nny
        nnvx = nnx; nnvy = nny+1
       do j = nnmy, nnmy
          do i = 1, nnmx
             clat_e(i,j) = 0.
             mapfac_mx_e(i,j) = 1.0/(COS(clat_e(i,j)*(3.1415926/180.0)))
          end do
       end do
       do j = nnmy+1, ny
          do i = 1, nnmx
             clat_e(i,j) = clat_e(i,j-1) + dellat
             mapfac_mx_e(i,j) = 1.0/(COS(clat_e(i,j)*(3.1415926/180.0)))
          end do
       end do
       do j = nnmy-1, 1, -1
          do i = 1, nnmx
             clat_e(i,j) = clat_e(i,j+1) - dellat
             mapfac_mx_e(i,j) = 1.0/(COS(clat_e(i,j)*(3.1415926/180.0)))
          end do
       end do
        allocate(u10_a(nnx,nny))
        allocate(v10_a(nnx,nny))
        allocate(t2_a(nnx,nny))
        allocate(qs_a(nnx,nny))
        allocate(pd_a(nnx,nny))         
        allocate(mu_a(nnx,nny))         
        allocate(dx_nmm_a(nnx,nny))         
        allocate(fis_a(nnx,nny))         
        allocate(tsk_a(nnx,nny))         
        allocate(sst_a(nnx,nny))         
        allocate(co_f_a(nnx,nny))         
        allocate(soiltb_a(nnx,nny))         
        allocate(glat_a(nnx,nny))         
        allocate(glon_a(nnx,nny))          
        allocate(acsnow_a(nnx,nny))          
        allocate(lu_index_a(nnx,nny))
        allocate(landmask_a(nnx,nny))
        allocate(seaice_a(nnx,nny))
        allocate(ivgtyp_a(nnx,nny))
        allocate(isltyp_a(nnx,nny))
        allocate(vegfra_a(nnx,nny))
        allocate(snowh_a(nnx,nny))
        allocate(mapfac_mx_a(nnx,nny))

        call fill_nmm_grid2(u10_e,nx,ny,u10_a,2)
        call fill_nmm_grid2(v10_e,nx,ny,v10_a,2)
        call fill_nmm_grid2(t2_e,nx,ny,t2_a,1)
        call fill_nmm_grid2(qs_e,nx,ny,qs_a,1)
        call fill_nmm_grid2(pd_e,nx,ny,pd_a,1)
        call fill_nmm_grid2(dx_nmm_e,nx,ny,dx_nmm_a,1)
        call fill_nmm_grid2(fis_e,nx,ny,fis_a,1)
        call fill_nmm_grid2(tsk_e,nx,ny,tsk_a,1)
        call fill_nmm_grid2(sst_e,nx,ny,sst_a,1)
        call fill_nmm_grid2(co_f_e,nx,ny,co_f_a,1)
        call fill_nmm_grid2(soiltb_e,nx,ny,soiltb_a,1)
        call fill_nmm_grid2(glat_e,nx,ny,glat_a,1)
        call fill_nmm_grid2(glon_e,nx,ny,glon_a,1)
        call fill_nmm_grid2(acsnow_e,nx,ny,acsnow_a,1)
        call fill_nmm_grid2(lu_index_e,nx,ny,lu_index_a,1)
        call fill_nmm_grid2(landmask_e,nx,ny,landmask_a,1)
        call fill_nmm_grid2(seaice_e,nx,ny,seaice_a,1)
        call fill_nmm_grid2(ivgtyp_e,nx,ny,ivgtyp_a,1)
        call fill_nmm_grid2(isltyp_e,nx,ny,ivgtyp_a,1)
        call fill_nmm_grid2(vegfra_e,nx,ny,vegfra_a,1)
        call fill_nmm_grid2(snowh_e,nx,ny,snowh_a,1)
        call fill_nmm_grid2(mapfac_mx_e,nx,ny,mapfac_mx_a,1)

        allocate(mapfac_ux_c(nnux,nnuy))
        call fill_arw_xllu_grid(mapfac_mx_a,nnx,nny,mapfac_ux_c)
        allocate(mapfac_vx_c(nnvx,nnvy))
        call fill_arw_xllv_grid(mapfac_mx_a,nnx,nny,mapfac_vx_c)
       do j = 1, nny
          do i = 1, nnx
             mu_a(i,j) = PDTOP
             fis_a(i,j) = fis_a(i,j)/9.8
             glat_a(i,j) = glat_a(i,j)*R2D
             glon_a(i,j) = glon_a(i,j)*R2D
          end do
       end do
        allocate(xlat_u_c(nnux,nnuy))
        allocate(xlong_u_c(nnux,nnuy))
        call fill_arw_xllu_grid(glat_a,nnx,nny,xlat_u_c)
        call fill_arw_xllu_grid(glon_a,nnx,nny,xlong_u_c)
        allocate(xlat_v_c(nnvx,nnvy))
        allocate(xlong_v_c(nnvx,nnvy))
        call fill_arw_xllv_grid(glat_a,nnx,nny,xlat_v_c)
        call fill_arw_xllv_grid(glon_a,nnx,nny,xlong_v_c)

      call put_var_2d_real_cdf(output_file, 'U10', u10_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'V10', v10_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'T2', t2_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'Q2', qs_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'PSFC', pd_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'MU', mu_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'MU0', dx_nmm_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'HGT', fis_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'TSK', tsk_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'SST', sst_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'F', co_f_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'TMN', soiltb_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLAT', glat_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLONG', glon_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'SNOWC', acsnow_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'LU_INDEX', lu_index_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'LANDMASK', landmask_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'SEAICE', seaice_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_int_cdf(output_file, 'IVGTYP', ivgtyp_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_int_cdf(output_file, 'ISLTYP', ivgtyp_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'VEGFRA', vegfra_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'SNOWH', snowh_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'MAPFAC_MX', mapfac_mx_a, &
                             nnx, nny, 1, debug )
      call put_var_2d_real_cdf(output_file, 'MAPFAC_UX', mapfac_ux_c, &
                             nnux, nnuy, 1, debug )
      call put_var_2d_real_cdf(output_file, 'MAPFAC_VX', mapfac_vx_c, &
                             nnvx, nnvy, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLAT_U', xlat_u_c, &
                             nnux, nnuy, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLONG_U', xlong_u_c, &
                             nnux, nnuy, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLAT_V', xlat_v_c, &
                             nnvx, nnvy, 1, debug )
      call put_var_2d_real_cdf(output_file, 'XLONG_V', xlong_v_c, &
                             nnvx, nnvy, 1, debug )
!------------------------------------------------------------------------
!----------Variables for which the levels are the SOIL levels-----
!------------------------------------------------------------------------
      call get_dims_cdf(input_file,'SMC', dims, ndims, debug)
      call get_dims_cdf(input_file,'STC', dims, ndims, debug)
        allocate(smc_e(dims(1), dims(2), dims(3)))
        allocate(stc_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'SMC',smc_e, &
                             dims(1), dims(2), dims(3), 1, debug )
      call get_var_3d_real_cdf(input_file, 'STC',stc_e, &
                             dims(1), dims(2), dims(3), 1, debug )
        nx = dims(1)
        ny = dims(2)
        nz = dims(3)
        nnx = 2*nx-1 ; nny = ny ; nnz = nz
        allocate(smc_a(nnx,nny,nnz))
        allocate(stc_a(nnx,nny,nnz))
        call fill_nmm_grid3(smc_e,nx,ny,nz,smc_a,1)
        call fill_nmm_grid3(stc_e,nx,ny,nz,stc_a,1)
      call put_var_3d_real_cdf(output_file, 'SMOIS', smc_a, &
                             nnx, nny, nnz, 1, debug )
      call put_var_3d_real_cdf(output_file, 'TSLB', stc_a, &
                             nnx, nny, nnz, 1, debug )

!----------------------1D Variables-----------------------------
      call get_dims_cdf(input_file,'ETA1', dims, ndims, debug)
        allocate(eta1(dims(1)))
      call get_var_1d_real_cdf(input_file, 'ETA1',eta1, &
                             dims(1),1, debug )
       nx = dims(1)
       nnx= nx
      call put_var_1d_real_cdf(output_file, 'ZNW', eta1, &
                             nnx, 1, debug )
      call get_dims_cdf(input_file,'ETA2', dims, ndims, debug)
        allocate(eta2(dims(1)))
      call get_var_1d_real_cdf(input_file, 'ETA2',eta2, &
                             dims(1),1, debug )
       nx = dims(1)-1
       nnx= nx
      call put_var_1d_real_cdf(output_file, 'ZNU', eta2, &
                             nnx, 1, debug )
!----------------Single Value------------------------------------
      call get_dims_cdf(input_file,'PT', dims, ndims, debug)
      call get_var_0d_real_cdf(input_file, 'PT',ptop, &
                              1, debug )
      call put_var_0d_real_cdf(output_file,'P_TOP',ptop, &
                              1, debug )
      call put_var_0d_real_cdf(output_file,'CF1',dx, &
                              1, debug )
      call put_var_0d_real_cdf(output_file,'CF2',dy, &
                              1, debug )

     stop
    end program convert_etoc
!------------------------------------------------------------
  subroutine get_dims_cdf( file, var, dims, ndims, debug )

  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: var
  logical, intent(in ) :: debug
  integer, intent(out), dimension(4) :: dims
  integer, intent(out) :: ndims

  integer cdfid, rcode, id_time
  character (len=80) :: varnam, time1
  integer :: natts, istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ', trim(file)
  else
    write(6,*) ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_time = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for ',var,' ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
    if(debug) write(6,*) ' dimension ',i,dims(i)
  enddo

  call ncclos(cdfid,rcode)

  end subroutine get_dims_cdf
!------------------------------------------------------------
  subroutine get_var_0d_real_cdf( file, var, data, &
                                  time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real,  intent(out) :: data
  real(kind=8)  :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) then
      write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
      write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    endif
  enddo

!  check the dimensions

   if( (time > idims(1))     )  then

     write(6,*) ' error in single value read, dimension problem '
     write(6,*) time, idims(1)
     write(6,*) ' error stop '
     stop

   end if

!  get the data

    istart(1) = time
    iend(1) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_0d_real_cdf

!--------------------------------------------------------------------
  subroutine put_var_0d_real_cdf( file, var, data, &
                                  time, debug )

    implicit none

    include 'netcdf.inc'

    integer, intent(in)  ::  time
    character (len=80), intent(in) :: file
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, intent(in) :: data
    real(kind=8) :: tmp

    integer :: cdfid, rcode, id_data
    character (len=80) :: varnam, time1
    integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
    integer :: i, ivtype

    cdfid = ncopn(file, NCWRITE, rcode )

    if( rcode == 0) then
      if(debug) write(6,*) ' open netcdf file ', trim(file)
    else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
    end if

    id_data = ncvid( cdfid, var, rcode )

    rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
    if(debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
    endif
    do i=1,ndims
      rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
      if(debug) write(6,*) ' dimension ',i,idims(i)
    enddo

!---check the dimensions

    if( (time > idims(1))     )  then

       write(6,*) ' error in single value read, dimension problem '
       write(6,*) time, idims(1)
       write(6,*) ' error stop '
       stop
     end if

!----get the data

     istart(1) = time
     iend(1) = 1

     if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
        call ncvpt( cdfid,id_data,istart,iend,data,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else
        write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
        stop
     endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_0d_real_cdf
!------------------------------------------------------------
  subroutine get_var_1d_real_cdf( file, var, data, &
                                  i1, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1), intent(out) :: data
  real(kind=8), dimension(i1) :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_1d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) then
      write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
      write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    endif
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (time > idims(2))     )  then

     write(6,*) ' error in 1d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) time, idims(2)
     write(6,*) ' error stop '
     stop

   end if

!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = time
    iend(2) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data(1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_1d_real_cdf

!--------------------------------------------------------------------
  subroutine put_var_1d_real_cdf( file, var, data, &
                                  i1, time, debug )

    implicit none

    include 'netcdf.inc'

    integer, intent(in)  ::  i1, time
    character (len=80), intent(in) :: file
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1), intent(in) :: data
    real(kind=8), dimension(i1) :: tmp

    integer :: cdfid, rcode, id_data
    character (len=80) :: varnam, time1
    integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
    integer :: i, ivtype

    cdfid = ncopn(file, NCWRITE, rcode )

    if( rcode == 0) then
      if(debug) write(6,*) ' open netcdf file ', trim(file)
    else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
    end if

    id_data = ncvid( cdfid, var, rcode )

    rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
    if(debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
    endif
    do i=1,ndims
      rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
      if(debug) write(6,*) ' dimension ',i,idims(i)
    enddo

!---check the dimensions

    if((i1 /= idims(1)) .or.  &
       (time > idims(2))     )  then

       write(6,*) ' error in 1d_var_real read, dimension problem '
       write(6,*) i1, idims(1)
       write(6,*) time, idims(2)
       write(6,*) ' error stop '
       stop
     end if
!----get the data

     istart(1) = 1
     iend(1) = i1
     istart(2) = time
     iend(2) = 1

     if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
        call ncvpt( cdfid,id_data,istart,iend,data,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else
        write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
        stop
     endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_1d_real_cdf

!------------------------------------------------------------
  subroutine get_var_2d_real_cdf( file, var, data, &
                                  i1, i2, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2), intent(out) :: data
  real(kind=8), dimension(i1,i2) :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) then
      write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
      write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    endif
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

     write(6,*) ' error in 2d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) time, idims(3)
     write(6,*) ' error stop '
     stop

   end if

!  get the data
 
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data(1,1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_2d_real_cdf

!--------------------------------------------------------------------
  subroutine put_var_2d_real_cdf( file, var, data, &
                                  i1, i2, time, debug )

    implicit none

    include 'netcdf.inc'

    integer, intent(in)  ::  i1, i2, time
    character (len=80), intent(in) :: file
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1,i2), intent(in) :: data
    real(kind=8), dimension(i1,i2) :: tmp

    integer :: cdfid, rcode, id_data
    character (len=80) :: varnam, time1
    integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
    integer :: i, ivtype

    cdfid = ncopn(file, NCWRITE, rcode )

    if( rcode == 0) then
      if(debug) write(6,*) ' open netcdf file ', trim(file)
    else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
    end if

    id_data = ncvid( cdfid, var, rcode )

    rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
    if(debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
    endif
    do i=1,ndims
      rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
      if(debug) write(6,*) ' dimension ',i,idims(i)
    enddo

!---check the dimensions

    if((i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

       write(6,*) ' error in 2d_var_real read, dimension problem '
       write(6,*) i1, idims(1)
       write(6,*) i2, idims(2)
       write(6,*) time, idims(3)
       write(6,*) ' error stop '
       stop
     end if

!----get the data
 
     istart(1) = 1
     iend(1) = i1
     istart(2) = 1
     iend(2) = i2
     istart(3) = time
     iend(3) = 1

     if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
        call ncvpt( cdfid,id_data,istart,iend,data,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else
        write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
        stop
     endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_2d_real_cdf

!--------------------------------------------------------------------

  subroutine get_var_3d_real_cdf( file, var, data, &
                                  i1, i2, i3, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, i3, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2,i3), intent(out) :: data
  real(kind=8), dimension(i1,i2,i3) :: tmp

  character (len=80) :: varnam, time1

  integer :: cdfid, rcode, id_data
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(6,*) ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )
  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_3d_real_cdf: dims for ',var,' ',ndims
    write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
    write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    write(unit=*, fmt='(a,i6)') ' kind(data)=', kind(data)
  endif

  print*,'get var',var,' ',ndims
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
  enddo
    print*, ' rcode ', rcode
    print*, ' i1, i2, i3 ', i1, i2, i3

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (i3 /= idims(3)) .or.  &
       (time > idims(4))     )  then

     write(6,*) ' error in 3d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) i3, idims(3)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if
!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data(1,1,1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_3d_real_cdf

!--------------------------------------------------------------------
  subroutine put_var_3d_real_cdf( file, var, data, &
                                  i1, i2, i3, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, i3, time
  character (len=80), intent(in) :: file

  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2,i3), intent(in) :: data
  real(kind=8), dimension(i1,i2,i3) :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCWRITE, rcode )
 print*,__FILE__,__LINE__
  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )
 print*,__FILE__,__LINE__,'id_date= ',id_data

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' put_var_3d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(6,*) ' dimension ',i,idims(i)
  enddo

!  check the dimensions
   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
!       (i3 /= idims(3)) .or.  &
       (time > idims(4))     )  then

     write(6,*) ' error in 3d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) i3, idims(3)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if

!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvpt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       tmp = data
       call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       tmp = data
       call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_3d_real_cdf

!--------------------------------------------------------------------
  subroutine get_var_2d_int_cdf(file, var, data, i1, i2, time, debug)
  
  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  integer, dimension(i1,i2), intent(out) :: data

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) then
      write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
      write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    endif
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

     write(6,*) ' error in 2d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if

!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1
    call ncvgt( cdfid,id_data,istart,iend,data,rcode)

    if(debug) then
       write(unit=*, fmt='(a, i8)') ' Sample data=', data(1,1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_2d_int_cdf

!--------------------------------------------------------------------
  subroutine put_var_2d_int_cdf( file, var, data, &
                                  i1, i2, time, debug )

    implicit none

    include 'netcdf.inc'

    integer, intent(in)  ::  i1, i2, time
    character (len=80), intent(in) :: file
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1,i2), intent(in) :: data
    real(kind=8), dimension(i1,i2) :: tmp

    integer :: cdfid, rcode, id_data
    character (len=80) :: varnam, time1
    integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
    integer :: i, ivtype

    cdfid = ncopn(file, NCWRITE, rcode )

    if( rcode == 0) then
      if(debug) write(6,*) ' open netcdf file ', trim(file)
    else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
    end if

    id_data = ncvid( cdfid, var, rcode )

    rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
    if(debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
    endif
    do i=1,ndims
      rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
      if(debug) write(6,*) ' dimension ',i,idims(i)
    enddo

!---check the dimensions

    if((i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

       write(6,*) ' error in 2d_var_real read, dimension problem '
       write(6,*) i1, idims(1)
       write(6,*) i2, idims(2)
       write(6,*) time, idims(3)
       write(6,*) ' error stop '
       stop
     end if

!----get the data
 
     istart(1) = 1
     iend(1) = i1
     istart(2) = 1
     iend(2) = i2
     istart(3) = time
     iend(3) = 1
     call ncvpt( cdfid,id_data,istart,iend,data,rcode)

    if(debug) then
       write(unit=*, fmt='(a, i8)') ' Sample data=', data(1,1)
    endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_2d_int_cdf
!-------------------------------------------------------------------------------

subroutine fill_nmm_grid2(gin,nx,ny,gout,igtype)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: quarter,half,zero
  use gridmod, only: iglobal,itotsub,ltosj,ltosi,ltosj_s,ltosi_s

  implicit none
  integer(i_kind) nx,ny,igtype
!  real(r_single) gin(nx,ny),gout(2*nx-1,ny)
  real       :: gin(nx,ny),gout(2*nx-1,ny)

  integer(i_kind) i,im,ip,j,jm,jp
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
  do j=1,ny
     do i=1,2*nx-1
        gout(i,j)=fill
     end do
  end do

! First transfer all staggered points to appropriate
! points on filled output grid
  if(igtype.eq.1) then
     do j=1,ny,2
        do i=1,nx
           gout(2*i-1,j)=gin(i,j)
        end do

     end do
     do j=2,ny,2
        do i=1,nx-1
           gout(2*i,j)=gin(i,j)
        end do
     end do
 else
     do j=1,ny,2
        do i=1,nx-1
           gout(2*i,j)=gin(i,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx
           gout(2*i-1,j)=gin(i,j)
        end do
     end do
  end if

!  Now fill in holes

! Top and bottom rows:
  do j=1,ny,ny-1
     do i=1,2*nx-1
        if(gout(i,j).gt.test) then
           ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
           im=i-1 ; if(im.lt.1) im=i+1
           gout(i,j)=half*(gout(im,j)+gout(ip,j))
        end if
     end do
  end do
! Left and right rows:
  do j=1,ny
     jp=j+1 ; if(jp.gt.ny) jp=j-1
     jm=j-1 ; if(jm.lt.1) jm=j+1
     do i=1,2*nx-1,2*nx-2
        if(gout(i,j).gt.test) gout(i,j)=half*(gout(i,jm)+gout(i,jp))
     end do
  end do

! Interior points
  do j=1,ny
     jp=j+1 ; if(jp.gt.ny) jp=j-1
     jm=j-1 ; if(jm.lt.1) jm=j+1
     do i=1,2*nx-1
        if(gout(i,j).gt.test) then
           ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
           im=i-1 ; if(im.lt.1) im=i+1
           gout(i,j)=quarter*(gout(ip,j)+gout(im,j)+gout(i,jp)+gout(i,jm))
        end if
     end do
  end do

!  do j=1,ny
!  print*,j,' gin max/min for lat ',j,maxval(gin(1:nx,j)),minval(gin(1:nx,j))
!  print*,j,' gout max/min for lat ',j,maxval(gout(1:2*nx-1,j)),minval(gout(1:2*nx-1,j))
!  end do

!  do j=1,ny
!     do i=1,nx
!        print*, i, j, 'gin at (i,j) is ', gin(i,j)
!        print*, i, j, 'gout at (i,j) is ', gout(i,j)
!     end do
!  print*,j,' gin max/min for lat ',j,maxval(gin(1:nx,j)),minval(gin(1:nx,j))
!  print*,j,' gout max/min for lat ',j,maxval(gout(1:2*nx-1,j)),minval(gout(1:2*nx-1,j))
!  end do
  
return
end subroutine fill_nmm_grid2
!--------------------------------------------------------------------

subroutine fill_nmm_grid3(gin,nx,ny,nz,gout,igtype)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: quarter,half,zero
  use gridmod, only: iglobal,itotsub,ltosj,ltosi,ltosj_s,ltosi_s

  implicit none
  integer(i_kind) nx,ny,nz,igtype
!  real(r_single) gin(nx,ny),gout(2*nx-1,ny)
  real       :: gin(nx,ny,nz),gout(2*nx-1,ny,nz)

  integer(i_kind) i,im,ip,j,jm,jp,k
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
  do k=1,nz
     do j=1,ny
        do i=1,2*nx-1
           gout(i,j,k)=fill
        end do
     end do
  end do

! First transfer all staggered points to appropriate
! points on filled output grid
  if(igtype.eq.1) then
     do k=1,nz
        do j=1,ny,2
           do i=1,nx
              gout(2*i-1,j,k)=gin(i,j,k)
           end do

        end do
        do j=2,ny,2
           do i=1,nx-1
              gout(2*i,j,k)=gin(i,j,k)
           end do
        end do
     end do
 else
     do k=1,nz
        do j=1,ny,2
           do i=1,nx-1
              gout(2*i,j,k)=gin(i,j,k)
           end do
        end do
        do j=2,ny,2
           do i=1,nx
              gout(2*i-1,j,k)=gin(i,j,k)
           end do
        end do
     end do
  end if

!  Now fill in holes

! Top and bottom rows:
  do k=1,nz
     do j=1,ny,ny-1
        do i=1,2*nx-1
           if(gout(i,j,k).gt.test) then
              ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
              im=i-1 ; if(im.lt.1) im=i+1
              gout(i,j,k)=half*(gout(im,j,k)+gout(ip,j,k))
           end if
        end do
     end do
  end do
! Left and right rows:
  do k=1,nz
     do j=1,ny
        jp=j+1 ; if(jp.gt.ny) jp=j-1
        jm=j-1 ; if(jm.lt.1) jm=j+1
        do i=1,2*nx-1,2*nx-2
           if(gout(i,j,k).gt.test) gout(i,j,k)=half*(gout(i,jm,k)+gout(i,jp,k))
        end do
     end do
  end do

! Interior points
  do k=1,nz
     do j=1,ny
        jp=j+1 ; if(jp.gt.ny) jp=j-1
        jm=j-1 ; if(jm.lt.1) jm=j+1
        do i=1,2*nx-1
           if(gout(i,j,k).gt.test) then
              ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
              im=i-1 ; if(im.lt.1) im=i+1
              gout(i,j,k)=quarter*(gout(ip,j,k)+gout(im,j,k)+gout(i,jp,k)+gout(i,jm,k))
           end if
        end do
     end do
  end do

!  do j=1,ny
!  print*,j,' gin max/min for lat ',j,maxval(gin(1:nx,j)),minval(gin(1:nx,j))
!  print*,j,' gout max/min for lat ',j,maxval(gout(1:2*nx-1,j)),minval(gout(1:2*nx-1,j))
!  end do
 
return
end subroutine fill_nmm_grid3
!--------------------------------------------------------------------------------

  subroutine fill_arw_ugrid(gin,nx,ny,nz,gout)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: half

  implicit none
  integer(i_kind) nx,ny,nz
  real       :: gin(nx,ny,nz),gout(nx+1,ny,nz)

  integer(i_kind) i,im,ip,j,jm,jp,k
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
  do k=1,nz
     do j=1,ny
        do i=1,nx+1
           gout(i,j,k)=fill
        end do
     end do
  end do

! First transfer all staggered points to appropriate
! points on filled output grid
     do k=1,nz
        do j=1,ny
           do i=1,nx
              gout(i+1,j,k)=gin(i,j,k)
           end do
        end do
     end do

!  Now fill in holes
! Left and right column:
  do k=1,nz
     do j=1,ny
        do i=1,nx+1,nx
           if((gout(i,j,k).gt.test) .and. (i .eq. 1)) then
              ip=i+1
!           if(gout(i,j,k).gt.test) gout(i,j,k)=gout(ip,j,k)
            gout(i,j,k)=gout(ip,j,k)
           else if((gout(i,j,k).gt.test) .and. (i .eq. nx+1)) then
              im=i-1
            gout(i,j,k)=gout(im,j,k)
           end if
        end do
     end do
  end do

! Interior U-points:
  do k=1,nz
     do j=1,ny
!        do i=2,nx
        do i=1,nx-1
           if(gout(i,j,k).lt.test) then
!              ip=i   ; if(ip.gt.nx) ip=i-1
              ip=i+1 ; if(ip.gt.nx) ip=i-1
!              im=i-1 ; if(im.lt.1) im=i+1
              im=i   ; if(im.lt.1) im=i+1
!              gout(i,j,k)=half*(gout(im,j,k)+gout(ip,j,k))
              gout(i+1,j,k)=half*(gout(im,j,k)+gout(ip,j,k))
!              gout(i+1,j,k)=half*(gout(i-1,j,k)+gout(i+1,j,k))
           end if
        end do
     end do
  end do

return
end subroutine fill_arw_ugrid
!--------------------------------------------------------------------------------

  subroutine fill_arw_vgrid(gin,nx,ny,nz,gout)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: half

  implicit none
  integer(i_kind) nx,ny,nz
  real       :: gin(nx,ny,nz),gout(nx,ny+1,nz)

  integer(i_kind) i,im,ip,j,jm,jp,k
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
  do k=1,nz
     do j=1,ny+1
        do i=1,nx
           gout(i,j,k)=fill
        end do
     end do
  end do

! First transfer all staggered points to appropriate
! points on filled output grid
     do k=1,nz
        do j=1,ny
           do i=1,nx
              gout(i,j+1,k)=gin(i,j,k)
           end do
        end do
     end do

!  Now fill in holes
! Bottom and top row:
  do k=1,nz
     do j=1,ny+1,ny
        do i=1,nx
           if((gout(i,j,k).gt.test) .and. (j .eq. 1)) then
              jp=j+1
!           if(gout(i,j,k).gt.test) gout(i,j,k)=gout(ip,j,k)
            gout(i,j,k)=gout(i,jp,k)
           else if((gout(i,j,k).gt.test) .and. (j .eq. ny+1)) then
              jm=j-1
            gout(i,j,k)=gout(i,jm,k)
           end if
        end do
     end do
  end do

! Interior V-points:
  do k=1,nz
     do j=1,ny
        do i=1,nx
           if(gout(i,j,k).lt.test) then
!              jp=j   ; if(ip.gt.nx) ip=i-1
              jp=j+1 ; if(ip.gt.nx) ip=i-1
!              jm=j-1 ; if(im.lt.1) im=i+1
              jm=j   ; if(im.lt.1) im=i+1
              gout(i,j+1,k)=half*(gout(i,jm,k)+gout(i,jp,k))
           end if
        end do
     end do
  end do

return
end subroutine fill_arw_vgrid
!--------------------------------------------------------------------------------

  subroutine fill_arw_xllu_grid(gin,nx,ny,gout)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: half

  implicit none
  integer(i_kind) nx,ny
  real       :: gin(nx,ny),gout(nx+1,ny)

  integer(i_kind) i,im,ip,j,jm,jp
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
     do j=1,ny
        do i=1,nx+1
           gout(i,j)=fill
        end do
     end do

! First transfer all staggered points to appropriate
! points on filled output grid
        do j=1,ny
           do i=1,nx
              gout(i+1,j)=gin(i,j)
           end do
        end do

!  Now fill in holes
! Left and right column:
     do j=1,ny
        do i=1,nx+1,nx
           if((gout(i,j).gt.test) .and. (i .eq. 1)) then
              ip=i+1 
            gout(i,j)=gout(ip,j)
           else if((gout(i,j).gt.test) .and. (i .eq. nx+1)) then
              im=i-1 
            gout(i,j)=gout(im,j)
           end if
        end do
     end do

! Interior U-points:
     do j=1,ny
        do i=1,nx-1
           if(gout(i,j).lt.test) then
              ip=i+1 ; if(ip.gt.nx) ip=i-1
              im=i   ; if(im.lt.1) im=i+1
              gout(i+1,j)=half*(gout(im,j)+gout(ip,j))
           end if
        end do
     end do

return
end subroutine fill_arw_xllu_grid
!--------------------------------------------------------------------------------

  subroutine fill_arw_xllv_grid(gin,nx,ny,gout)
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: half

  implicit none
  integer(i_kind) nx,ny
  real       :: gin(nx,ny),gout(nx,ny+1)

  integer(i_kind) i,im,ip,j,jm,jp
  real(r_single) fill,test
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
     do j=1,ny+1
        do i=1,nx
           gout(i,j)=fill
        end do
     end do

! First transfer all staggered points to appropriate
! points on filled output grid
        do j=1,ny
           do i=1,nx
              gout(i,j+1)=gin(i,j)
           end do
        end do

!  Now fill in holes
! Bottom and top row:
     do j=1,ny+1,ny
        do i=1,nx
           if((gout(i,j).gt.test) .and. (j .eq. 1)) then
              jp=j+1 
            gout(i,j)=gout(i,jp)
           else if((gout(i,j).gt.test) .and. (j .eq. ny+1)) then
              jm=j-1 
            gout(i,j)=gout(i,jm)
           end if
        end do
     end do

! Interior V-points:
     do j=1,ny
        do i=1,nx
           if(gout(i,j).lt.test) then
              jp=j+1 ; if(ip.gt.nx) ip=i-1
              jm=j   ; if(im.lt.1) im=i+1
              gout(i,j+1)=half*(gout(i,jm)+gout(i,jp))
           end if
        end do
     end do

return
end subroutine fill_arw_xllv_grid
