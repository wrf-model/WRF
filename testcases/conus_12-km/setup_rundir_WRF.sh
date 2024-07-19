#! /bin/bash
set -e

#edit these lines ----------------------------------------

casename="codee_test01"  #identifier for the new simulation

#email address to receive notifications from the slurm system
myemail="Koichi.Sakaguchi@pnnl.gov" 

#directory where we run wrf.exe and writes output
rundir="/pscratch/sd/k/ksa/simulation/WRF/WRFSIG/${casename}" 

# -------------------
scriptdir=$(pwd)

mkdir -p ${rundir}

cd ${rundir}
pwd

#get input files from NERSC's WRFSIG CFS space
echo "downloading input file archive"

cp -r /global/cfs/cdirs/m4232pub/data/testcase_input/conus12km/v4.5.2/* .
#input files for SBM runs, prepared by Chayanon Wichitrnithed
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/namelist.input      namelist.input.sbm
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/namelist.input.cold namelist.input.sbm.cold
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/CESM_RCP4.5_Current_Aerosol_Data.dat \
      /global/cfs/cdirs/m4232/intern_2024/input_sbm/CESM_RCP4.5_Future_Aerosol_Data.dat  .
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/wrfrst_d01_2008-07-15_00_00_00 wrfrst_d01_2008-07-15_00_00_00.sbm
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/wrfbdy_d01                     wrfbdy_d01.sbm
cp    /global/cfs/cdirs/m4232/intern_2024/input_sbm/wrfinput_d01                   wrfinput_d01.sbm
cp -r /global/cfs/cdirs/m4232/intern_2024/input_sbm/{SBM_input_33,SBM_input_43} \
      /global/cfs/cdirs/m4232/intern_2024/input_sbm/scattering_tables_2layer_high_quad_1dT_1%fw_110 .

echo "finished copying the input files"


rm -rf ${rundir}/namelist.input #remove the default namelist.input file
#copy WRF namelist
cp ${scriptdir}/namelist_v4.5.2_conus12km_restart.input ${rundir}/namelist.input

#copy sbatch script
cp ${scriptdir}/sub_wrf_pm_testcase.sh ${rundir}/sub_testcase.sh
cp ${scriptdir}/updown.sh              ${rundir}/updown.sh

#edit the email address and job name in the sbatch script
sed -i "s/elvis@nersc.gov/${myemail}/" sub_testcase.sh
sed -i "s/elvis_test01/${casename}/" sub_testcase.sh

sbatch sub_testcase.sh
