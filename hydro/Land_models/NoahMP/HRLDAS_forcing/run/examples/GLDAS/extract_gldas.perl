#!/usr/bin/perl

@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
	 "30","31");
	 
@yrs = ("10");

$day_start = 1;
$day_end   = 3;

@hrs = ("00","03","06","09","12","15","18","21");

$cc = "20";   # Manually set the century

@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

@vars = ("Rainf","Snowf","Wind","Tair","Qair","Psurf","SWdown","LWdown"); 

$data_dir = "/d1/barlage/data/GLDAS/raw";
$results_dir = "/d1/barlage/data/GLDAS/extracted";

for $var (@vars)
 {
for $yy (@yrs)
 {

# This will be the jday time loop

for($julday=$day_start;$julday<=$day_end;$julday++)

 { 
 
 # This little section finds the text month and day
 
 @modays = @noleap_days;
 if($yy == "92" || $yy == "96") {@modays = @leap_days}
 if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {@modays = @leap_days}

 for($mo=1;$mo<=12;$mo++)
  {
    if($julday>$modays[$mo-1] && $julday<=$modays[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $modays[$mo-1];
     }
  }

  $jday3 = "$julday";
  if($julday<100) {$jday3 = "0$julday"};
  if($julday<10) {$jday3 = "00$julday"};
  
  @infiles = ( `ls $data_dir/GLDAS_NOAH025SUBP_3H.A$cc$yy$jday3*` );
  chop(@infiles);

for($hr=1;$hr<=8;$hr++)
 {

  $file = $infiles[$hr-1];

  system ("wgrib -s $file | grep ':$var:' | wgrib -i -grib $file -o $results_dir/$var/GLDAS_$var.20$yy$nums[$mon]$nums[$day]$hrs[$hr-1].grb ");
 }
 }
 } # End of outer time loop
 }
