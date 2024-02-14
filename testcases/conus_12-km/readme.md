# CONUS12km benchmark case

## Source

Original files were downloaded from NCAR MMM website:

https://www2.mmm.ucar.edu/wrf/users/benchmark/benchdata_v422.html

## Files

The following three files + tar.gz archive file provide everything needed to run the test case.

Only thing a user needs to do is to run the shell script `setup_rundir_WRF.sh`

- `namelist_v4.5.2_conus12km_restart.input`: wrf namelist for the test case
- `sub_wrf_pm_testcase.sh` : an example batch script to run a test case on Perlmutter
- `setup_rundir_WRF.sh` : createa a directory to run the test simulation, copy input files, and submit a job after editing (sed) a copy of `sub_wrf_pm_testcase.sh` batch script. 
- Initial and boundary conditions, and look-up tables for various model modules/kernels are archived and 
stored in a publicly available space under WRF-SIG repository. The `setup_rundir_WRF.sh` script downloads this file
by wget command.

### setup_rundir_WRF.sh

Edit the following three lines and then execute.
```
casename="codee_test01"  #identifier for the new simulation

#email address to receive notifications from the slurm system
myemail="Koichi.Sakaguchi@pnnl.gov" 

#directory where we run wrf.exe and writes output
rundir="/pscratch/sd/k/ksa/simulation/WRF/WRFSIG/${casename}" 

```

## Test Case

The original CONUS12km case runs only for one hour to simulate a strong winter storm case.

For more robust statistics and higher sensitivity to source code changes, the simulation period is extended
to 5 days (can be as long as 10 days) and to the summer season (July) in 2008; this time period is chosen
just for the convenience of K. Sakaguchi, who uses this period for his own research). 
In the summer the influence of lateral boundary forcing 
(e.g., winds coming in from the west) is weaker than in the winter, therefore processes originated 
locally (within the model domain) have more impact. 
So changes in the model executable would appear sooner and stronger.

The 5-day simulation typically ends within 15 minutes using 194 MPI tasks, 4 threads in each task, 
on 3 CPU nodes of Perlmutter. 
