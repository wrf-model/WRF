#! /bin/csh -f

#echo " "
#echo "*************************************************"
#echo "You should do the following manually before running the CCSM build script:"
#echo "  setenv NETCDF /usr/local/pkg/netcdf/netcdf-3.6.1.pgi (for midnight)"
#echo "  cd models/atm/wrf"
#echo "  ./clean -a"
#echo "  ./configure"
#echo "  choose 4 (for midnight)"
#echo "  choose 1"
#echo "*************************************************"
#echo " "

# prepare namelist and other parameters
cd $RUNDIR

# boundary datasets for wr50a grid
# *** WARNING ***
# *** WARNING *** When you change this, make sure you delete all wrf input data
# *** WARNING *** from your run directory to make sure the prior data is updated.
# *** WARNING *** This should include an input, boundary, and nudging file.
# *** WARNING *** Filenames could be in the form wrf* OR $CASE* (for input).
# *** WARNING ***
#set boundary_dir = "boundary"
set boundary_dir = "boundary_ERA"
#set boundary_dir = "boundary_CFSR"
echo " - WRF using ${boundary_dir} input data"

# Set link_data to 1 for linking instead of copying input data
# For lustre file systems DO NOT LINK DATA (keep set link_data = 0).
set link_data = 1
if ($ATM_GRID =~ wr50a) then
  set link_data = 0
endif
if ($ATM_GRID =~ wr10a) then
  set link_data = 0
endif

# Calculate start date and if this run is a restart.
# Calculate start date and if this run is a restart.
if (`echo ${CONTINUE_RUN} | gawk '{print toupper($0)}'` == 'TRUE') then
   set rest_flag = '.true.'
   set date = `cat $RUNDIR/rpointer.drv | sed "s/\.nc//; s/^.*\.r\.//;"`
   set rst_inname_prefix = ${CASE}
else if (`echo ${RUN_TYPE} | gawk '{print tolower($0)}'` == 'branch') then
   set rest_flag = '.true.'
   set date = ${RUN_REFDATE}
   set rst_inname_prefix = ${RUN_REFCASE}
else if (`echo ${RUN_TYPE} | gawk '{print tolower($0)}'` == 'hybrid') then
   echo "ERROR: Hybrid runs are not possible for WRF in RASM"
   exit 1
else
   set rest_flag = '.false.'
   set date = ${RUN_STARTDATE}
   set rst_inname_prefix = ${CASE}
endif
set st_year = `echo $date | cut -d "-" -f 1`
set st_mon  = `echo $date | cut -d "-" -f 2`
set st_day  = `echo $date | cut -d "-" -f 3`

set stn_year = `echo $st_year | sed 's/^0*//g'`
set stn_mon  = `echo $st_mon  | sed 's/^0*//g'`
set stn_day  = `echo $st_day  | sed 's/^0*//g'`

# Set annual_data to 1 (true).  Only use monthly data for 1979.
set annual_data = 1

if (${st_year} == '1979') then
  set annual_data = 0
endif

if ($STOP_OPTION == 'nmonths' && $STOP_N > 1) then
  set annual_data = 1
endif
if ($STOP_OPTION == 'nmonth' && $STOP_N > 1) then
  set annual_data = 1
endif
# only annual data on copper
if ($MACH == 'copper') then
  set annual_data = 1
endif
if ($annual_data) then
  echo " - WRF using annual boundary data"
else
  echo " - WRF using monthly boundary data"
endif

# Determine filedate
set initdate = ${st_year}${st_mon}01
set restdate = ${st_year}-${st_mon}-${st_day}_00\:00\:00
if (${annual_data}) then
  set filedate = ${st_year}
else
  set filedate = ${st_year}${st_mon}01
endif

@ y4   = ($stn_year / 4) * 4
@ y100 = ($stn_year / 100) * 100
@ y400 = ($stn_year / 400) * 400

set leap = 0
if ($stn_year == $y4  ) set leap = 1
if ($stn_year == $y100) set leap = 0
if ($stn_year == $y400) set leap = 1

#echo $y4 $y100 $y400 $leap

if ($CALENDAR == 'GREGORIAN' && $leap == 1) then
  set days_in_month = ('31' '29' '31' '30' '31' '30' '31' '31' '30' '31' '30' '31')
  set months   = ('01' '02' '03' '04' '05' '06' '07' '08' '09' '10' '11' '12')
else
  set days_in_month = ('31' '28' '31' '30' '31' '30' '31' '31' '30' '31' '30' '31')
  set months   = ('01' '02' '03' '04' '05' '06' '07' '08' '09' '10' '11' '12')
  set leap_ext = ""
  if (${leap} == 1) then
    if (${annual_data} == 0 && ${st_mon} == 02) then
      echo " - WRF is using a taylored leap month file for this simulation"
      set leap_ext = "_noleap"
    endif
    if (${annual_data} == 1) then
      echo " - WRF is using a taylored leap year file for this simulation"
      set leap_ext = "_noleap"
    endif
  endif
endif

# Set bdy and fdda filename
set bdyfile  = "wrfbdy${leap_ext}_d01_${filedate}"
set fddafile = "wrffdda${leap_ext}_d01_${filedate}"

# Remove existing boundary and nudging files from the directory before copying/linking anew.
# But keep the current ones around if they already exist to save the copy cost.

rm -f hold_bdyfile          >&! /dev/null
mv ${bdyfile} hold_bdyfile  >&! /dev/null
rm ${RUNDIR}/wrfbdy_*       >&! /dev/null
mv hold_bdyfile ${bdyfile}  >&! /dev/null

rm -f hold_fddafile           >&! /dev/null
mv ${fddafile} hold_fddafile  >&! /dev/null
rm ${RUNDIR}/wrffdda_*        >&! /dev/null
mv hold_fddafile ${fddafile}  >&! /dev/null

# Calculate the number of days we should run by setting run_days in WRF namelist.
# Note: run_days is the number of days to run after WRF start_year, start_month, and start_day
# It is NOT the number of days to run after CCSM run_startdate.  So, if we are running one
# month jobs, run_days will never be more than the number of days in the month.

if ($STOP_OPTION == 'ndays' || $STOP_OPTION == 'nday') then
    set run_days = $STOP_N
    @ wrf_end_date = $run_days + $stn_day - 1
    if (${annual_data} == 0) then
      # This will be a problem if the number of run days takes us into the next month
      # because our WRF lateral boundary conditions are on a month-by-month basis.
      if ($wrf_end_date > $days_in_month[$stn_mon]) then
        echo "You are asking to run RACM past available WRF lateral boundary conditions."
        echo "Set stop_option in env_run.xml to nmonths rather than ndays to accomodate this."
        exit -9
      endif
    endif
endif

if ($STOP_OPTION == 'nmonths' || $STOP_OPTION == 'nmonth') then
    if (${annual_data}) then                                                                   
       @ run_days = 1 + $days_in_month[$stn_mon] - $stn_day                                    
       set cnt = 2
       while ($cnt <= $STOP_N)                                                                
          @ mon12 = $stn_mon + $cnt - 1                                                        
          if ($mon12 > '12') then                                                              
              echo "WRF lateral bounday conditions exist on an annual basis. Please don't exceed the annual boundary"
              echo "start_month = $st_mon and stop_n = $STOP_N"
              exit -9
          endif
          @ run_days = $run_days + $days_in_month[$mon12]
          @ cnt++
       end
    else
       if ($STOP_N != '1') then
           echo "WRF lateral bounday conditions exist on a month-by-month basis. Please run one month at a time."
           exit -9
       endif
       @ run_days = 1 + $days_in_month[$stn_mon] - $stn_day
    endif
endif

if ($STOP_OPTION != 'ndays' && $STOP_OPTION != 'nmonths' && $STOP_OPTION != 'nday' && $STOP_OPTION != 'nmonth') then
    echo "Please set stop_option in env_run.xml to either ndays or nmonths."
    exit -9
endif

echo " - Running $STOP_OPTION $STOP_N or $run_days days in WRF"

#echo "wrf initdate ${initdate}"
#echo "wrf restdate ${restdate}"
#echo "wrf filedate ${filedate}"
#echo "wrf bdyfile ${bdyfile}"
#echo "wrf fddafile ${fddafile}"
#echo "wrf run_days ${run_days}"

# Turn on or off spectral nudging here.
# 0 = no spectral nudging; 1 = turn on spectral nudging
set spectral_nudging = 0
if ($ATM_GRID =~ us20) then
  set spectral_nudging = 1
endif
if ($ATM_GRID =~ wr50a) then
  set spectral_nudging = 1
endif
if ($ATM_GRID =~ wr10a) then
  set spectral_nudging = 1
endif

if ($spectral_nudging) then
    set nudging_switch = 2
else
    set nudging_switch = 0
endif

#--------------------------------------------------------------------------
# ATM_GRID is wr50a
#--------------------------------------------------------------------------

if ($ATM_GRID =~ wr50a) then
if ($link_data) then
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/CAM_ABS_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/CAM_AEROPT_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone_lat.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone_plev.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/GENPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/LANDUSE.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/SOILPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/URBPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/VEGPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr49t67 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr49t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr67t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/co2_trans .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/gribmap.txt .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/grib2map.tbl .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/wrf_hist_var_d01.txt .

  rm ${bdyfile}
  ln -sf   $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/${bdyfile} ${bdyfile}

  if (`echo ${CONTINUE_RUN} | gawk '{print toupper($0)}'` == 'FALSE') then
   if (`echo ${RUN_TYPE} | gawk '{print tolower($0)}'` == 'branch') then
     if ( -e ./${RUN_REFCASE}.wrf.r01.${restdate} ) then
      echo " - Restarting WRF with case.std.wrf.r01.${restdate}"
     else
      echo " - Check case.std.wrf.r01.${restdate} is linked in the run directory"
     endif
   else
     rm  ./${RUN_REFCASE}.wrf.i01.${restdate}
     ln -sf $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/wrfinput_d01_${initdate}  ./${RUN_REFCASE}.wrf.i01.${restdate}
   endif
  endif

  if ($spectral_nudging) then
    rm ${fddafile}
    ln -sf $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/${fddafile} ${fddafile}
  endif
else
  cp   $DIN_LOC_ROOT/atm/wrf/data/CAM_ABS_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/CAM_AEROPT_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone_lat.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone_plev.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/GENPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/LANDUSE.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/SOILPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/URBPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/VEGPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr49t67 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr49t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr67t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/co2_trans .
  cp   $DIN_LOC_ROOT/atm/wrf/data/gribmap.txt .
  cp   $DIN_LOC_ROOT/atm/wrf/data/grib2map.tbl .
  cp   $DIN_LOC_ROOT/atm/wrf/data/wrf_hist_var_d01.txt .

  if (! -e ${bdyfile}) then
    cp   $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/${bdyfile} ${bdyfile}
  endif

  if (`echo ${CONTINUE_RUN} | gawk '{print toupper($0)}'` == 'FALSE') then
   if (`echo ${RUN_TYPE} | gawk '{print tolower($0)}'` == 'branch') then
     if ( -e ./${RUN_REFCASE}.wrf.r01.${restdate} ) then
      echo " - Restarting WRF with ${RUN_REFCASE}.wrf.r01.${restdate}"
     else
      echo " - Check ${RUN_REFCASE}.wrf.r01.${restdate} is in the run directory"
     endif
   else
      cp -f $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/wrfinput_d01_${initdate}  ./${CASE}.wrf.i01.${restdate}
   endif
  endif
  if ($spectral_nudging) then
    if (! -e ${fddafile}) then
      cp $DIN_LOC_ROOT/atm/wrf/${boundary_dir}/${fddafile} ${fddafile}
    endif
  endif
  chmod u+w *
endif

### START RASM AVERAGING CONFIG  ###########################################
### only need to modify/update "avg_freq" (options sec,min,h,d,mon,none) ###
### and "avg_interval" variables. (Note: all other configurations        ###
### related to WRF average output are automated.)                        ###

set avg_freq = "d"
set avg_interval = 1

### START RASM DIURNAL CONFIG ##############################################
### only need to modify/update "diurnal_mode" (options on or off)        ###

set diurnal_mode = "on"

### END RASM STATS CONFIG ##################################################

@ mn_interval = 1
if ($avg_freq == "sec") then
    set freq_out = 1
else if ($avg_freq == "min") then 
    set freq_out = 2
else if ($avg_freq == "h") then
    set freq_out = 3
else if ($avg_freq == "d") then
    set freq_out = 4
else if ($avg_freq == "mon") then
    set freq_out = 5
else
    set freq_out = 0
endif

set avg_namelist_line1 = "mean_freq               = $freq_out"
set avg_namelist_line2 = ""
set avg_namelist_line3 = ""
set avg_namelist_line4 = ""
set avg_namelist_line5 = ""
if ($freq_out) then
    set avg_namelist_line1 = "mean_freq               = $freq_out"
    set avg_namelist_line2 = 'auxhist5_outname        = "'${CASE}'.wrf.ha.<date>.nc"' 
    set avg_namelist_line3 = "io_form_auxhist5        = 2"
    set avg_namelist_line4 = "mean_interval           = $avg_interval"
    set avg_namelist_line5 = "frames_per_auxhist5     = 1"
endif

if ($diurnal_mode == "on") then
    set diurnal_namelist_line1 = "diurnal_cycle           = 1"
    set diurnal_namelist_line2 = 'auxhist6_outname        = "'${CASE}'.wrf.hda.<date>.nc"' 
    set diurnal_namelist_line3 = "io_form_auxhist6        = 2"
    set diurnal_namelist_line4 = "frames_per_auxhist6     = 1"
else
    set diurnal_namelist_line1 = "diurnal_cycle           = 0"
    set diurnal_namelist_line2 = ""
    set diurnal_namelist_line3 = ""
    set diurnal_namelist_line4 = ""
endif

# rst_inname                      = “${CASE}.wrf.r<domain>.<date>” 
# input_inname                        = “${CASE}.wrf.i<domain>.<date>” 
# input_outname                       = “${CASE}.wrf.i<domain>.<date>” 
# history_outname                     = “${CASE}.wrf.h<domain>.<date>” 
# rst_outname                         = “${CASE}.wrf.r<domain>.<date>” 

cat >! namelist.input <<EOF
 &time_control
 start_year                          = ${st_year}
 start_month                         = ${st_mon}
 start_day                           = ${st_day}
 start_hour                          = 00
 start_minute                        = 00
 start_second                        = 00
 run_days                            = ${run_days}
 interval_seconds                    = 21600
 input_from_file                     = .true.
 history_interval                    = 0
 frames_per_outfile                  = 1
 restart                             = ${rest_flag}
 restart_interval                    = 0
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 rst_inname                          = "${rst_inname_prefix}.wrf.r<domain>.<date>"
 bdy_inname                          = "${bdyfile}"
 ${avg_namelist_line1} 
 ${avg_namelist_line2} 
 ${avg_namelist_line3} 
 ${avg_namelist_line4} 
 ${avg_namelist_line5} 
 ${diurnal_namelist_line1} 
 ${diurnal_namelist_line2} 
 ${diurnal_namelist_line3} 
 ${diurnal_namelist_line4} 
 output_diagnostics                  = 0
 auxhist3_outname                    = "wrf_clim_d<domain>_<date>.nc",
 auxhist3_interval                   = 0,
 frames_per_auxhist3                 = 1,
 io_form_auxhist3                    = 2,
 iofields_filename                   = "wrf_hist_var_d01.txt",
 ignore_iofields_warning             = .true.,
 auxhist11_outname                   = "${CASE}.wrf.hem.<date>.nc",
 auxhist11_interval                  = 360,
 io_form_auxhist11                   = 2,
 frames_per_auxhist11                = 1,
 auxhist12_outname                   = "${CASE}.wrf.hsm.<date>.nc",
 auxhist12_interval                  = 0,
 io_form_auxhist12                   = 2,
 frames_per_auxhist12                = 1,
 auxhist13_outname                   = "${CASE}.wrf.hfl.<date>.nc",
 auxhist13_interval                  = 0,
 io_form_auxhist13                   = 2,
 frames_per_auxhist13                = 1,
 auxhist14_outname                   = "${CASE}.wrf.hec.<date>.nc",
 auxhist14_interval                  = 0,
 io_form_auxhist14                   = 2,
 frames_per_auxhist14                = 1,
 auxhist23_outname                   = "${CASE}.wrf.hpm.<date>.nc",
 auxhist23_interval                  = 0,
 frames_per_auxhist23                = 1,
 io_form_auxhist23                   = 2,

 /

 &diags
 p_lev_diags                         = 0
 num_press_levels                    = 8
 press_levels                        = 92500, 85000, 77500, 70000,
                                       60000, 50000, 30000, 20000
 use_tot_or_hyd_p                    = 1
 p_lev_missing                       = -999.
 /
 
 &domains
 time_step                           = 150,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,
 e_we                                = 276,
 s_sn                                = 1,
 e_sn                                = 206,
 e_vert                              = 40,
 eta_levels                          =  1.00000, 0.99667, 0.99268, 0.98738, 0.98077,
                                        0.97223, 0.96179, 0.94884, 0.93346, 0.91447,
                                        0.89203, 0.86633, 0.83640, 0.80141, 0.76188,
                                        0.71838, 0.67156, 0.62208, 0.57068, 0.51720,
                                        0.46214, 0.40769, 0.35800, 0.31274, 0.27161,
                                        0.23431, 0.20056, 0.17106, 0.14553, 0.12291,
                                        0.10287, 0.08512, 0.06940, 0.05547, 0.04313,
                                        0.03220, 0.02251, 0.01394, 0.00634, 0.00000,
 p_top_requested                     = 5000,
 num_metgrid_levels                  = 30,
 num_metgrid_soil_levels             = 4,
 dx                                  = 50000,
 dy                                  = 50000,
 grid_id                             = 1,
 parent_id                           = 0,
 i_parent_start                      = 1,
 j_parent_start                      = 1,
 parent_grid_ratio                   = 1,
 parent_time_step_ratio              = 1,
 feedback                            = 1,
 nproc_x                             = -1,
 nproc_y                             = -1,
 max_ts_locs                         = 18,
 max_ts_level                        = 39,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 10,
 co2tf                               = 1,
 ra_lw_physics                       = 4,
 ra_sw_physics                       = 4,
 radt                                = 20,
 sf_sfclay_physics                   = 1,
 bl_pbl_physics                      = 5,
 ysu_topdown_pblmix                  = 0,
 sf_surface_physics                  = 2,
 bldt                                = 0,
 cu_physics                          = 1,
 cudt                                = 5,
 cu_rad_feedback                     = .true.,
 icloud                              = 1,
 isfflx                              = 1,
 ifsnow                              = 0,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 usemonalb                           = .false.
 fractional_seaice                   = 1,
 rad_micro_cpl                       = 1,
 sf_urban_physics                    = 0,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 slope_rad                           = 0,
 topo_shading                        = 0,
 cam_abs_freq_s                      = 21600
 cam_abs_dim2                        = 40
 cam_abs_dim1                        = 4
 paerlev                             = 29
 levsiz                              = 59
 /

 &fdda
 grid_fdda                           = ${nudging_switch}
 gfdda_inname                        = "${fddafile}"
 gfdda_interval_m                    = 360,
 fgdt                                = 0,
 gfdda_end_h                         = 9999999,
 if_no_pbl_nudging_uv                = 0,
 if_no_pbl_nudging_t                 = 0,
 if_no_pbl_nudging_ph                = 0,
 if_zfac_uv                          = 1,
 if_zfac_t                           = 1,
 if_zfac_ph                          = 1,
 k_zfac_uv                           = 20,
 k_zfac_t                            = 20,
 k_zfac_ph                           = 50,
 guv                                 = 0.0003,
 gt                                  = 0.0003,
 gph                                 = 0.0000,
 dk_zfac_uv                          = 10
 dk_zfac_t                           = 10
 dk_zfac_ph                          = 10
 xwavenum                            = 4,
 ywavenum                            = 3,
 /

 &dynamics
 rk_ord                              = 3,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 w_damping                           = 1,
 base_temp                           = 268.
 damp_opt                            = 0,
 zdamp                               = 5000.,
 dampcoef                            = 0.01,
 khdif                               = 0,
 kvdif                               = 0,
 smdiv                               = 0.1,
 emdiv                               = 0.01,
 epssm                               = 0.1,
 non_hydrostatic                     = .true.,
 h_mom_adv_order                     = 5,
 v_mom_adv_order                     = 3,
 h_sca_adv_order                     = 5,
 v_sca_adv_order                     = 3,
 time_step_sound                     = 4,
 moist_adv_opt                       = 1,
 scalar_adv_opt                      = 1,
 iso_temp                            = 0,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true.,
 nested                              = .false.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF
endif
