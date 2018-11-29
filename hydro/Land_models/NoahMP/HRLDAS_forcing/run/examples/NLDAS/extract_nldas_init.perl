#!/usr/bin/perl

	 
$date = "20100101";   # Manually set the date
$hh = "00";           # Manually set the hour

@vars = ("WEASD","CNWAT","AVSFT","SOILM","TSOIL"); 

$data_dir = "/d1/barlage/data/NLDAS/raw";
$results_dir = "/d1/barlage/data/NLDAS/extracted/INIT";

for $var (@vars)
 {

  $file = "$data_dir/NLDAS_NOAH0125_H.A$date.${hh}00.002.grb";

  if($var eq "SOILM" || $var eq "TSOIL") 
   {
     system ("wgrib -s $file | grep ':$var:0-10 ' | wgrib -i -grib $file -o $results_dir/NLDAS_${var}_000-010.$date$hh.grb ");
     system ("wgrib -s $file | grep ':$var:10-40' | wgrib -i -grib $file -o $results_dir/NLDAS_${var}_010-040.$date$hh.grb ");
     system ("wgrib -s $file | grep ':$var:40-100' | wgrib -i -grib $file -o $results_dir/NLDAS_${var}_040-100.$date$hh.grb ");
     system ("wgrib -s $file | grep ':$var:100-200' | wgrib -i -grib $file -o $results_dir/NLDAS_${var}_100-200.$date$hh.grb ");
   } else
   {
     system ("wgrib -s $file | grep ':$var:' | wgrib -i -grib $file -o $results_dir/NLDAS_$var.$date$hh.grb ");
   }

 }
