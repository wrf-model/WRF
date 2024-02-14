#! /bin/bash
casename="codee_test01"  #identifier for the new simulation
myemail="Koichi.Sakaguchi@pnnl.gov"

rundir="/pscratch/sd/k/ksa/simulation/WRF/WRFSIG/${casename}"

mkdir -p ${rundir}

#copy WRF namelist
cp namelist_v4.5.2_conus12km_restart.input ${rundir}/namelist.input

#copy sbatch script
cp sub_wrf_pm_testcase.sh ${rundir}/sub_wrf_pm_testcase.sh

cd ${rundir}
#edit the email address in the sbatch script
sed -i "s/elvis@nersc.gov/${mymail}/" sub_wrf_pm_testcase.sh

#get input files from NERSC's WRFSIG CFS space
wget https://portal.nersc.gov/cfs/m4232pub/testcase_input/testcase_input_conus12km_v4.5.2.tar.gz

tar -zxf testcase_input_conus12km_v4.5.2.tar.gz


echo " ----- namelist.input ----- "
more namelist.input

echo " ----- sbatch script ----- "
more sub_wrf_pm_testcase.sh

echo sbatch sub_wrf_pm_testcase.sh