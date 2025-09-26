CRYOWRF v1.0 uses WRF (v4.2.1) as the base atmospheric model. 

There are significant edits to WRF's routines to create CRYOWRF and are listed below with a one-line description of the edit. 

NOTE: the mods/edits are categorized as 'technical' or 'physics' or 'input/output' with a value in [ ] indicating whether it is a mod or an entirely new file

TECHNICAL
---------------------

1. arch/configure.defaults                [mod] : modifications to link snowpack and the coupler to WRF during compilation. Mods only for INTEL/GCC compilers
2. arch/postamble                         [mod] : adding the coupler to the include paths for WRF compilation.

3. Registry/Registry.EM_COMMON            [mod] : additional variables added for output along with declaring a new LSM scheme (SNOWPACK) 
4. Registry/registry.em_shared_collection [mod] : invoking snowpack's registry file ( registry.snowpack )
5. Registry/registry.snowpack             [new] : a new registry file that declares all new variables introduced by CRYOWRF
6. Registry/registry.solar_fields         [mod] : minor mod to add cloud optical thickness to restart files
   
PHYSICS
---------------------

7.  phys/module_sf_snowpacklsm.F           [new] : the main addition in CRYOWRF that implements SNOWPACK as LSM in WRF. calls to the API in the coupler are made here.
8.  phys/module_surface_driver.F           [mod] : mostly new variables for SNOWPACK that ultimately is passed to the runtime subroutine of SNOWPACK
9.  phys/module_diag_solar.F               [mod] : bug fix only 
10. phys/module_microphysics_driver.F      [mod] : the blowing snow scheme is implemented here.
11. phys/module_mp_morr_two_moment.F       [mod] : modified morrison scheme as described by Vignon et al (2021)[ https://doi.org/https://doi.org/10.1029/2020JD033490 ]
12. phys/module_mp_thompson.F              [mod] : impact of blowing snow on radiation : including the effect of blowing snow particles on optical depth [NOT ACTIVE AT THE MOMENT]
13. phys/module_physics_init.F             [mod] : initialization of snowpack objects through call to the init subroutine in module_sf_snowpacklsm.F is made here.
14. phys/module_sf_noahmpdrv.F             [mod] : minor change to increase the maximum depth of snow 
15. phys/module_sf_noahmp_glacier.F        [mod] : minor change to increase the maximum depth of snow 
16. phys/module_sf_noahmplsm.F             [mod] : minor change to increase the maximum depth of snow 
17. phys/module_sf_noah_seaice.F           [mod] : minor change to increase the maximum depth of snow 
18. phys/module_surface_driver.F           [mod] : minor change to increase the maximum depth of snow 

INPUT/OUTPUT
---------------------

19. share/input_wrf.F                      [mod] : edit to make and enforce snowpack as LSM is compatible with Noah / NoahMP for WRF preprocessing steps ,i.e. real.exe 
20. share/interp_fcn.F                     [mod] : implemented a special interpolation for snowpack pixels - useful for initialization of nests
21. share/mediation_integrate.F            [mod] : bugfix for diagnostics calls for wrfmean
22. share/module_check_a_mundo.F           [mod] : edit to make and enforce snowpack as LSM is compatible with Noah / NoahMP for WRF preprocessing steps ,i.e. real.exe 
23. share/module_soil_pre.F                [mod] : edit to make and enforce snowpack as LSM is compatible with Noah / NoahMP for WRF preprocessing steps ,i.e. real.exe 
24. share/output_wrf.F                     [mod] : bugfix 
25. share/wrf_timeseries.F                 [mod] : added new variables, from snowpack and blowing snow model for timeseries output 
26. share/wrf_tsin.F                       [mod] : minor edit for changes to filename format 

 
