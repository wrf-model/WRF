

                  Summary of ESMF-Coupled WRF Experiment

                              Tom Henderson
                             John Michalakes
                                NCAR/MMM
                             27 February 2007

INTRODUCTION

This document describes ESMF-enablement and coupling of WRF through ESMF
to another component that simulates coupling to an ocean model. As a
prelude to availability of an ESMF-enabled implementation of the HYCOM
model, WRF has been coupled through ESMF to a very simple "data-ocean"
component named "SST" via a very simple coupler component named "CPL".
The demonstration, conducted on kraken.navo.hpc.mil, a DoD HPC system
at the NAVO MSRC, was supported by AFWA under UCAR contract FA4600-05-P-0162.  
The demonstration was later repeated on NCAR's bluesky machine (IBM p690) and 
again on NCAR's bluevista machine (IBM p575).  A description of the bluesky 
and bluevista experiments follows.  



EVENT LOOP

Please read the section entitled "NOTES ABOUT THE EVENT LOOP FOR WRF+CPL+SST" 
in file external/io_esmf/README.io_esmf for details about order of operations
in the current event loop (time-stepping loop).  



EXPERIMENT DETAILS

  The "WRF+CPL+SST" experiment was conducted on bluesky on 12 October 2006 at 
which time the following software was installed:  
  xlfrte   8.1.1.6
  bos.mp   5.1.0.58
  xlC.rte   6.0.0.0
  ESMF 2.2.0rp1
WRF source code was installed in
WRFDIR=/ptmp/hender/ESMF2.2.0rp1/WRFV2_20061005_1123_WORK/WRFV2/.  
(Referred to below as "$WRFDIR".)  

  The "WRF+CPL+SST" experiment was repeated on bluevista on 27 February 2007 at 
which time the following software was installed:  
  xlfrte   10.1.0.3
  bos.mp   5.3.0.42
  xlC.rte  8.0.0.5
  ESMF     2.2.2r
WRF source code was installed in
WRFDIR=/ptmp/hender/WRF-LIS/WRFV2_20070214_0906/WRFV2/.

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


1)  On bluesky, set environment variables to build WRF with ESMF using 32-bit 
addressing.  On bluevista, use the default 64-bit addressing.  
Set the $ESMFLIB and $ESMFINC environment variables are to tell the WRF build 
automation where to find an installation of ESMF.  

BLUESKY:  
OBJECT_MODE=32
ESMFLIB=/home/bluesky/hender/esmf/lib/libO/AIX.default.32.default
ESMFINC=/home/bluesky/hender/esmf/mod/modO/AIX.default.32.default

BLUEVISTA:  
OBJECT_MODE=64
ESMFLIB=/home/bluevista/hender/esmf/esmf_2_2_2r/lib/libO/AIX.default.64.mpi.default
ESMFINC=/home/bluevista/hender/esmf/esmf_2_2_2r/mod/modO/AIX.default.64.mpi.default



2)  Set up WRF Registry for WRF+CPL+SST case:  

$WRFDIR/Registry >> mv -f Registry.EM Registry.EM_ORIG
$WRFDIR/Registry >> cp -f Registry.EM_SST Registry.EM



3)  Build WRF from scratch with RSL_LITE for use with ESMF:  

$WRFDIR >> echo 5 | configure
$WRFDIR >> compile em_real >&! compile.em_real.5.out

  Verify that executables exist:  

$WRFDIR >> ls -1 main/*exe
main/ndown.exe
main/nup.exe
main/real.exe
main/wrf.exe
main/wrf_SST_ESMF.exe

  Note that "wrf.exe" is the usual stand-alone WRF executable.  
"wrf_SST_ESMF.exe" is the WRF+CPL+SST coupled application.  



4)  Go to run directory, unpack the required SST data, namelists and run 
scripts from WRF_CPL_SST.tar.gz, and verify that all required files are
present:   

$WRFDIR/test/em_esmf_exp >> gunzip WRF_CPL_SST.tar.gz
$WRFDIR/test/em_esmf_exp >> tar xvf WRF_CPL_SST.tar
$WRFDIR/test/em_esmf_exp >> ls -l sstin_d01_000000 namelist.input.jan00.* *.csh
-rw-r--r--   6368 Feb 27 13:16 namelist.input.jan00.ESMFSST
-rw-r--r--   6368 Feb 27 13:16 namelist.input.jan00.NETCDFSST
-rw-r--r--   1286 Feb 27 14:51 real.csh
-rwxr-xr-x    948 Feb 27 14:51 real.lsf.csh
-rw-r--r-- 458064 Oct 12 11:58 sstin_d01_000000
-rw-r--r--   1074 Feb 27 14:51 test4_0.csh
-rwxr-xr-x    732 Feb 27 14:51 test4_0.lsf.csh
-rw-r--r--   1162 Feb 27 14:51 test4_0_ESMFSST.csh
-rw-r--r--    824 Feb 27 14:51 test4_0_ESMFSST.lsf.csh
-rw-r--r--   1190 Feb 27 14:52 test4_0_NETCDFSST_wrfexe.csh
-rw-r--r--    824 Feb 27 14:52 test4_0_NETCDFSST_wrfexe.lsf.csh

  "sstin_d01_000000" contains time-varying SST input data.  
NOTE:  sstin_d01_000000 also contains LANDMASK fields soley for validation 
purposes.  Only the first time-level of LANDMASK is significant.  For
historical reasons, the other time-levels may differ -- they are ignored 
(and should eventually be removed).  

  "test4_0_ESMFSST.csh" is a LoadLeveler batch submission script that runs the 
WRF+CPL+SST coupled system.  It copies "namelist.input.jan00.ESMFSST" to 
"namelist.input" and runs "wrf_SST_ESMF.exe" on four CPUs.
"test4_0_ESMFSST.lsf.csh" does the same thing for LSF.  

  "test4_0_NETCDFSST.csh" is a LoadLeveler batch submission script that runs 
WRF by itself, reading the SST data directly into WRF as a netCDF file.  It 
copies "namelist.input.jan00.NETCDFSST" to "namelist.input" and runs "wrf.exe" 
on four CPUs.  
"test4_0_NETCDFSST.lsf.csh" does the same thing for LSF.  

  Note that namelists namelist.input.jan00.ESMFSST and 
namelist.input.jan00.NETCDFSST differ only in the I/O format used for the 
I/O streams used to "read" and "write" SST data.  Vanilla netCDF I/O is used 
when "io_form" == 2.  ESMFStates are used when "io_form" == 7.  

$WRFDIR/test/em_esmf_exp >> diff namelist.input.jan00.ESMFSST namelist.input.jan00.NETCDFSST
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
to "namelist.input" and runs "wrf.exe" on four CPUs.  "test4_0.lsf.csh" does 
the same thing for LSF.)  



5)  Run the wrf real program using the "jan00" data set.  Script "real.csh" 
can be modified to do this if desired (or real.lsf.csh for LSF).  

BLUESKY (LoadLeveler):  
$WRFDIR/test/em_esmf_exp >> llsubmit real.csh
BLUEVISTA (LSF):  
$WRFDIR/test/em_esmf_exp >> bsub < real.lsf.csh

  Verify that real.exe produced the usual WRF input and boundary data files 
"wrfbdy_d01" and "wrfinput_d01".  

$WRFDIR/test/em_esmf_exp >> ls -al wrfbdy_d01 wrfinput_d01
-rw-r--r--   9355944 Feb 27 12:58 wrfbdy_d01
-rw-r--r--   6076408 Feb 27 12:58 wrfinput_d01

Move all other files produced by the real.exe run into a new directory:  

$WRFDIR/test/em_esmf_exp >> mkdir real.out
$WRFDIR/test/em_esmf_exp >> mv PET?.ESMF* namelist.input rsl.*.* real.*.err real.*.out real.out



6)  Run the WRF+CPL+SST test case:  

BLUESKY (LoadLeveler):  
$WRFDIR/test/em_esmf_exp >> llsubmit test4_0_ESMFSST.csh
BLUEVISTA (LSF):  
$WRFDIR/test/em_esmf_exp >> bsub < test4_0_ESMFSST.lsf.csh



7)  Verify that run completed successfully:  

$WRFDIR/test/em_esmf_exp >> tail -1 rsl.out.0000
 d01 2000-01-25_00:00:00 wrf: SUCCESS COMPLETE WRF



8)  Move all files produced by the WRF+CPL+SST run into a new directory:  

$WRFDIR/test/em_esmf_exp >> mkdir test4_0_ESMFSST.out
$WRFDIR/test/em_esmf_exp >> mv PET?.ESMF* namelist.input rsl.*.* test4_0_ESMFSST.*.* wrfout* test4_0_ESMFSST.out



9)  Run the WRF stand-alone test case:  

BLUESKY (LoadLeveler):  
$WRFDIR/test/em_esmf_exp >> llsubmit test4_0_NETCDFSST_wrfexe.csh
BLUEVISTA (LSF):  
$WRFDIR/test/em_esmf_exp >> bsub < test4_0_NETCDFSST_wrfexe.lsf.csh



10)  Verify that run completed successfully:  

$WRFDIR/test/em_esmf_exp >> tail -1 rsl.out.0000
 d01 2000-01-25_00:00:00 wrf: SUCCESS COMPLETE WRF



11)  Move all files produced by the WRF stand-alone run into a new directory:  

$WRFDIR/test/em_esmf_exp >> mkdir test4_0_NETCDFSST.out
$WRFDIR/test/em_esmf_exp >> mv PET?.ESMF* namelist.input rsl.*.* test4_0_NETCDFSST.*.* wrfout* sstout_d01_000000 test4_0_NETCDFSST.out



12)  Verify that both tests produced bitwise-identical history output:  

$WRFDIR/test/em_esmf_exp >> ls -l test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00
-rw-r--r--   1 hender   ncar       32614704 Oct 12 15:48 test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00
-rw-r--r--   1 hender   ncar       32614704 Oct 12 15:52 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00

$WRFDIR/test/em_esmf_exp >> cmp -l test4_0_ESMFSST.out/wrfout_d01_2000-01-24_12:00:00 test4_0_NETCDFSST.out/wrfout_d01_2000-01-24_12:00:00 | wc
       0       0       0

