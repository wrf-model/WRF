

                  Summary of ESMF-Coupled WRF Experiment

                              Tom Henderson
                             John Michalakes
                                NCAR/MMM
                             12 October 2006

INTRODUCTION

This document describes ESMF-enablement and coupling of WRF through ESMF
to another component that simulates coupling to an ocean model. As a
prelude to availability of an ESMF-enabled implementation of the HYCOM
model, WRF has been coupled through ESMF to a very simple "data-ocean"
component named "SST" via a very simple coupler component named "CPL".
The demonstration, conducted on kraken.navo.hpc.mil, a DoD HPC system
at the NAVO MSRC, was supported by AFWA under UCAR contract FA4600-05-P-0162.  
The demonstration was later repeated on NCAR's bluesky machine (IBM p690).  A 
description of the bluesky experiment follows.  



EXPERIMENT DETAILS

  The "WRF+CPL+SST" experiment was conducted on bluesky on 12 October 2006 at 
which time the following software was installed:  
  xlfrte   8.1.1.6
  bos.mp   5.1.0.58
  xlC.rte   6.0.0.0
  ESMF 2.2.0rp1
WRF source code was installed in
/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/.  

(Note that this experiment was also attempted using an installation of 
ESMF 3.0.0.  This experiment failed due to exhaustion of compiler resources 
building real.exe.  The ESMF core team has been notified of this problem and 
is working with IBM to resolve it.)  

  Additional documentation that describes the SST component and provides 
instructions for building WRF with ESMF can be found in 
external/io_esmf/README.io_esmf.  Briefly, the 
SST component simply reads SST data stored in a file (sstin_d01_000000), sends 
it to WRF via CPL, receives SST data back from WRF via CPL, and verifies that 
data received matches data sent.  Since ESMF coupling is implemented within 
WRF via the WRF I/O and coupling API (WRF IOAPI), the experiment can be 
repeated reading the SST data directly into WRF as a netCDF file simply by 
changing the appropriate I/O format in the WRF "namelist.input" file.  This 
feature is illustrated below.  

  The following "recipe" can be used to reproduce the experiment, if desired.  
All commands are csh.  


1)  Set environment variables to build WRF with ESMF using 32-bit addressing.  
The $ESMFLIB and $ESMFINC environment variables are used to tell the WRF build 
automation where to find an installation of ESMF 2.2.0rp1.  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> setenv OBJECT_MODE 32
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> setenv ESMFLIB /home/bluesky/hender/esmf/lib/libO/AIX.default.32.default
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> setenv ESMFINC /home/bluesky/hender/esmf/mod/modO/AIX.default.32.default



2)  Set up WRF Registry for WRF+CPL+SST case:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/Registry >> mv -f Registry.EM Registry.EM_ORIG
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/Registry >> cp -f Registry.EM_SST Registry.EM



3)  Build WRF from scratch with RSL for use with ESMF:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> echo 4 | configure
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> compile em_real >&! compile.em_real.4.out

  Verify that executables exist:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2 >> ls -al main/*exe*
-rwxr-xr-x   1 hender   ncar       12554240 Oct 12 15:32 main/ndown.exe
-rwxr-xr-x   1 hender   ncar       11220419 Oct 12 15:33 main/nup.exe
-rwxr-xr-x   1 hender   ncar       11595285 Oct 12 15:32 main/real.exe
-rwxr-xr-x   1 hender   ncar       18018321 Oct 12 15:30 main/wrf.exe
-rwxr-xr-x   1 hender   ncar       18486948 Oct 12 15:31 main/wrf_SST_ESMF.exe

  Note that "wrf.exe" is the usual stand-alone WRF executable.  
"wrf_SST_ESMF.exe" is the WRF+CPL+SST coupled application.  



4)  Go to run directory, unpack the required SST data, namelists and run 
scripts from WRF_CPL_SST.tar.gz, and verify that all required files are
present:   

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> gunzip WRF_CPL_SST.tar.gz
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> tar xvf WRF_CPL_SST.tar
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> ls -l sstin_d01_000000 namelist.input.jan00.* *.csh
-rwxr-xr-x   1 hender   ncar   6008 Oct 12 12:49 namelist.input.jan00.ESMFSST
-rwxr-xr-x   1 hender   ncar   6008 Oct 12 12:49 namelist.input.jan00.NETCDFSST
-rw-r--r--   1 hender   ncar   1271 Oct 12 12:49 real.csh
-rw-r--r--   1 hender   ncar 458064 Oct 12 11:35 sstin_d01_000000
-rw-r--r--   1 hender   ncar   1070 Oct 12 12:49 test4_0.csh
-rw-r--r--   1 hender   ncar   1150 Oct 12 12:49 test4_0_ESMFSST.csh
-rw-r--r--   1 hender   ncar   1186 Oct 12 12:49 test4_0_NETCDFSST_wrfexe.csh

  "sstin_d01_000000" contains time-varying SST input data.  

  "test4_0_ESMFSST.csh" is a LoadLeveler batch submission script that runs the 
WRF+CPL+SST coupled system.  It copies "namelist.input.jan00.ESMFSST" to 
"namelist.input" and runs "wrf_SST_ESMF.exe" on four CPUs.  

  "test4_0_NETCDFSST.csh" is a LoadLeveler batch submission script that runs 
WRF by itself, reading the SST data directly into WRF as a netCDF file.  It 
copies "namelist.input.jan00.NETCDFSST" to "namelist.input" and runs "wrf.exe" 
on four CPUs.  

  Note that namelists namelist.input.jan00.ESMFSST and 
namelist.input.jan00.NETCDFSST differ only in the I/O format used for the 
I/O streams used to "read" and "write" SST data.  Vanilla netCDF I/O is used 
when "io_form" == 2.  ESMFStates are used when "io_form" == 7.  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> diff namelist.input.jan00.ESMFSST namelist.input.jan00.NETCDFSST
32c32
<  io_form_auxinput5                   = 7,
---
>  io_form_auxinput5                   = 2,
37c37
<  io_form_auxhist5                    = 7,
---
>  io_form_auxhist5                    = 2,

  (Also note that a script is provided to run WRF without SST forcing for 
comparison purposes.  "test4_0.csh" is a LoadLeveler batch submission script 
that runs WRF by itself without SST forcing.  It copies "namelist.input.jan00" 
to "namelist.input" and runs "wrf.exe" on four CPUs.)  



5)  Run the wrf real program using the "jan00" data set.  Script "real.csh" 
can be modified to do this if desired.  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> llsubmit real.csh

  Verify that real.exe produced the usual WRF input and boundary data files 
"wrfbdy_d01" and "wrfinput_d01".  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real
>> ls -al wrfbdy_d01 wrfinput_d01
-rw-r--r--   1 hender   ncar        9355792 Oct 12 12:58 wrfbdy_d01
-rw-r--r--   1 hender   ncar        6005368 Oct 12 12:58 wrfinput_d01

Move all other files produced by the real.exe run into a new directory:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mkdir real.out
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mv PET?.ESMF* namelist.input rsl.*.* real.*.err real.*.out real.out



6)  Run the WRF+CPL+SST test case:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> llsubmit test4_0_ESMFSST.csh



7)  Verify that run completed successfully:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> tail -1 rsl.out.0000
 d01 2000-01-25_00:00:00 wrf: SUCCESS COMPLETE WRF



8)  Move all files produced by the WRF+CPL+SST run into a new directory:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mkdir test4_0_ESMFSST.out
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mv PET?.ESMF* namelist.input rsl.*.* test4_0_ESMFSST.*.* wrfout* test4_0_ESMFSST.out



9)  Run the WRF stand-alone test case:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> llsubmit test4_0_NETCDFSST_wrfexe.csh



10)  Verify that run completed successfully:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> tail -1 rsl.out.0000
 d01 2000-01-25_00:00:00 wrf: SUCCESS COMPLETE WRF



11)  Move all files produced by the WRF stand-alone run into a new directory:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mkdir test4_0_NETCDFSST.out
bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> mv PET?.ESMF* namelist.input rsl.*.* test4_0_NETCDFSST.*.* wrfout* sstout_d01_000000 test4_0_NETCDFSST.out



12)  Verify that both tests produced bitwise-identical history output:  

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> ls -l test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00
-rw-r--r--   1 hender   ncar       32614704 Oct 12 15:48 test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00
-rw-r--r--   1 hender   ncar       32614704 Oct 12 15:52 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00

bluesky:/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/test/em_real >> cmp -l test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00 | wc
       0       0       0

