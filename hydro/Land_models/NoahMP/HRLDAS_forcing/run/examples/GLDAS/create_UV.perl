#!/usr/bin/perl


$filename = "create_UV";
system ("pgf90 -o $filename $filename.f90 -L/home/barlage/programs/w3lib-1.6 -lw3");

@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
	 "30","31");
	 
@yrs = ("10");

$day_start = 1;
$day_end = 3;

@hrs = ("00","03","06","09","12","15","18","21");

@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

$data_dir = "/d1/barlage/data/GLDAS/extracted";
$results_dir = "/d1/barlage/data/GLDAS/extracted";

for $yy (@yrs)
 {

# This will be the jday time loop

for($julday=$day_start;$julday<=$day_end;$julday++)

 { 
 
 # This little section finds the text month and day
 
 @modays = @noleap_days;
 if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {@modays = @leap_days}

 for($mo=1;$mo<=12;$mo++)
  {
    if($julday>$modays[$mo-1] && $julday<=$modays[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $modays[$mo-1];
     }
  }

for $hr (@hrs)
 {

  $file_in  =    "$data_dir/Wind/GLDAS_Wind.20$yy$nums[$mon]$nums[$day]$hr.grb";
  $file1_out =  " $results_dir/U/GLDAS_U.20$yy$nums[$mon]$nums[$day]$hr.grb";
  $file2_out =  " $results_dir/V/GLDAS_V.20$yy$nums[$mon]$nums[$day]$hr.grb";

  print ("$file_in \n");
  system ("$filename $file_in $file1_out $file2_out");
 }
 }
 } # End of outer time loop

system("rm $filename");
