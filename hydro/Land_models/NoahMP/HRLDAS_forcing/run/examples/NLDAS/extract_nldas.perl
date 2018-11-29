#!/usr/bin/perl

@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
	 "30","31");

@yrs = ("10");

$day_start = 1;
$day_end   = 3;

$cc = "20";   # Manually set the century

@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

@vars = ("DLWRF","DSWRF","APCP","PRES","TMP","SPFH","UGRD","VGRD"); 

$data_dir = "/d1/barlage/data/NLDAS/raw";
$results_dir = "/d1/barlage/data/NLDAS/extracted";

for $var (@vars)
 {
for $yy (@yrs)
 {

# This is clumsy, but take care of leap years
if($yy == "92" || $yy == "96") {$day_end = 366}
if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {$day_end = 366}

@modays = @noleap_days;
if($yy == "92" || $yy == "96") {@modays = @leap_days}
if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {@modays = @leap_days}

# This will be the jday time loop

for($julday=$day_start;$julday<=$day_end;$julday++)

 { 
 
 # This little section finds the text month and day
 
 for($mo=1;$mo<=12;$mo++)
  {
    if($julday>$modays[$mo-1] && $julday<=$modays[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $modays[$mo-1];
     }
  }

 for($hr=0;$hr<=23;$hr++)
 {

   $file = "$data_dir/NLDAS_FORA0125_H.A$cc$yy$nums[$mon]$nums[$day].$nums[$hr]00.002.grb";

   system ("wgrib -s $file | grep ':$var:' | wgrib -i -grib $file -o $results_dir/$var/NLDAS_$var.$cc$yy$nums[$mon]$nums[$day]$nums[$hr].grb ");

 }
 }
 } # End of outer time loop
 }
