CRYOWRF v1.0 uses WPS (v4.2) for part of the preprocessing of the forcing data. 

There are mostly minor fixes to WPS's routines and addition of new files to make the preprocessing more useful for CRYOWRF. 

Note: the edits are categorized as 'new' or 'mod' to indicate whether it is a new file or modification of an existing file respectively

1.  ./arch/configure.defaults           [mod] : bugfix to link to openmp-compiled WRF 
3.  ./geogrid/GEOGRID.TBL.ALPS_COSMO    [new] : new table to link to special 1s topography for the Swiss Alps
4.  ./geogrid/GEOGRID.TBL.ANTARCTICA    [new] : new table to linke to REMA and Bedmap2 topography for Antarctica
5.  ./geogrid/src/process_tile_module.F [mod] : edit related to smooth_module.F 
6.  ./geogrid/src/smooth_module.F       [mod] : only localized smoothing only for pixels with slopes greater than 45 degrees.  
8.  ./metgrid: METGRID.TBL.ALPS_COSMO
9.  ./metgrid: METGRID.TBL.ANTARCTICA
11. ./CRYOWRF/WPS-4.2: setup_wps.sh     [new] : a simple script to set up WPS for either Antarctica or Alps
12. ./ungrib/src/rd_grib1.F             [mod] : to automatically check if the incoming grib data is for the Alps
13. ./ungrib/Variable_Tables: Vtable.ALPS_COSMO [new] : Vtable for COSMO grib files
14. ./ungrib/Variable_Tables: Vtable.ANTARCTICA [new] : Vtable for ERA-5 pressure level data / surface level data

