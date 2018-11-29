#!/usr/bin/perl

@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
	 "30","31");
	 
@hrs = ("00","03","06","09","12","15","18","21");

@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

$yyyy = "2010";
$mm = "01";
$dd = "01";
$hh = "00";

$data_dir = "/d1/barlage/data/NARR/raw";
$results_dir = "/d1/barlage/data/NARR/extracted";

$filesfc = "${data_dir}/merged_AWIP32.$yyyy$mm$dd$hh.RS.sfc";
$fileflx = "${data_dir}/merged_AWIP32.$yyyy$mm$dd$hh.RS.flx";

print("$filesfc\n");
print("$fileflx\n");

system ("wgrib -s $fileflx | grep ':TSOIL:0-10'    | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_TSOIL:0-10.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':TSOIL:10-40'   | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_TSOIL:10-40.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':TSOIL:40-100'  | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_TSOIL:40-100.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':TSOIL:100-200' | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_TSOIL:100-200.$yyyy$mm$dd$hh.grb");

system ("wgrib -s $fileflx | grep ':SOILW:0-10'    | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_SOILW:0-10.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':SOILW:10-40'   | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_SOILW:10-40.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':SOILW:40-100'  | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_SOILW:40-100.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $fileflx | grep ':SOILW:100-200' | wgrib -i -grib $fileflx -o ${results_dir}/INIT/NARR_SOILW:100-200.$yyyy$mm$dd$hh.grb");

system ("wgrib -s $filesfc | grep ':TMP:sfc'       | wgrib -i -grib $filesfc -o ${results_dir}/INIT/NARR_TMP:sfc.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $filesfc | grep ':CNWAT:sfc'     | wgrib -i -grib $filesfc -o ${results_dir}/INIT/NARR_CNWAT.$yyyy$mm$dd$hh.grb");
system ("wgrib -s $filesfc | grep ':WEASD:sfc'     | wgrib -i -grib $filesfc -o ${results_dir}/INIT/NARR_WEASD.$yyyy$mm$dd$hh.grb");


