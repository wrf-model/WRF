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

echo "finished copying the input files"


rm -rf ${rundir}/namelist.input #remove the default namelist.input file
#copy WRF namelist
cp ${scriptdir}/namelist_v4.5.2_conus12km_restart.input ${rundir}/namelist.input

#copy sbatch script
cp ${scriptdir}/sub_wrf_pm_testcase.sh ${rundir}/sub_testcase.sh

#edit the email address and job name in the sbatch script
sed -i "s/elvis@nersc.gov/${myemail}/" sub_testcase.sh
sed -i "s/elvis_test01/${casename}/" sub_testcase.sh

sbatch sub_testcase.sh