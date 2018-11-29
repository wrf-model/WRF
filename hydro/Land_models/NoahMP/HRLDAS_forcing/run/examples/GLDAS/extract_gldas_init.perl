#!/usr/bin/perl

	 
$date = "2010010100";   # Manually set the date

$data_dir = "/d1/barlage/data/GLDAS/raw";
$results_dir = "/d1/barlage/data/GLDAS/extracted/INIT";

$file = "$data_dir/GLDAS_NOAH025SUBP_3H.A2010001.0000.001.2010048211443.grb";

@vars = ("SWE","Canopint","AvgSurfT","SoilM","TSoil"); 

for $var (@vars)
 {

  if($var eq "SoilM" || $var eq "TSoil") 
   {
     system ("wgrib -s $file | grep ':${var}:0-1' | wgrib -i -grib $file -o $results_dir/GLDAS_${var}_100-200.$date.grb ");
     system ("wgrib -s $file | grep ':${var}:0-2' | wgrib -i -grib $file -o $results_dir/GLDAS_${var}_040-100.$date.grb ");
     system ("wgrib -s $file | grep ':${var}:0-3' | wgrib -i -grib $file -o $results_dir/GLDAS_${var}_010-040.$date.grb ");
     system ("wgrib -s $file | grep ':${var}:0-4' | wgrib -i -grib $file -o $results_dir/GLDAS_${var}_000-010.$date.grb ");
   } else
   {
     system ("wgrib -s $file | grep ':$var:' | wgrib -i -grib $file -o $results_dir/GLDAS_$var.$date.grb ");
   }

 }
