#!/usr/bin/perl


$filename = "fill_DSWRF";

$w3lib = '/home/barlage/programs/w3lib-1.6 -lw3';

system ("pgf90 -o $filename $filename.f90 -L$w3lib");

@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
         "30","31","32","33","34","35","36","37","38","39",
	 "40","41","42","43","44","45","46","47","48","49",
	 "50","51","52","53","54","55","56","57","58","59",
	 "60","61","62","63","64","65","66","67","68","69",
	 "70","71","72","73","74","75","76","77","78","79",
	 "80","81","82","83","84","85","86","87","88","89",
	 "90","91","92","93","94","95","96","97","98","99");
	 
@leap_days    = (0,31,29,31,30,31,30,31,31,30,31,30,31);
@nonleap_days = (0,31,28,31,30,31,30,31,31,30,31,30,31);

$cc = "20";  # Manually set century
@yrs = ("10");
$start_month = 1;
$end_month = 1;

$data_dir = "/d1/barlage/data/NARR/extracted";

for $yr (@yrs)
 {
for($mo=$start_month; $mo<=$end_month; $mo++)
 {
  @days = @nonleap_days;
  if($yr == "80" || $yr == "84" || $yr == "88" || $yr == "92" || $yr == "96") {@days = @leap_days}
  if($yr == "00" || $yr == "04" || $yr == "08" || $yr == "12") {@days = @leap_days}
  $jdaymo = 0;
  for($jmo=0; $jmo<=$mo-1; $jmo++) {$jdaymo = $jdaymo + $days[$jmo]}

#for($dy=1; $dy<=$days[$mo]; $dy++)
for($dy=1; $dy<=3; $dy++)
 {
 
  $jday = $jdaymo + $dy;

  print ("$yr $mo $dy $jday\n");
 
  $dy_now = $nums[$dy];
  $mo_now = $nums[$mo];
  $yr_now = $nums[$yr];

for($hr=0; $hr<=21; $hr=$hr+3)
 {

 # NARR data are 3hr averages valid from file time to file time + 3hrs

  open(FILE, "> fill_DSWRF.input") || die print "Can't open: \n";

  $file_in = "$data_dir/DSWRF/NARR_DSWRF.$cc$yr_now$mo_now$dy_now$nums[$hr].grb";
  print FILE "$file_in \n";

   $hr_out = $hr;
   $file_out = "$data_dir/DSWRF24/NARR_DSWRF.$cc$yr_now$mo_now$dy_now$nums[$hr_out].grb";
   print FILE "$file_out \n";
   print FILE "$yr $mo $dy $hr_out $jday $hr_out\n";

   $hr_out = $hr + 1;
   $file_out = "$data_dir/DSWRF24/NARR_DSWRF.$cc$yr_now$mo_now$dy_now$nums[$hr_out].grb";
   print FILE "$file_out \n";
   print FILE "$yr $mo $dy $hr_out $jday $hr_out\n";

   $hr_out = $hr + 2;
   $file_out = "$data_dir/DSWRF24/NARR_DSWRF.$cc$yr_now$mo_now$dy_now$nums[$hr_out].grb";
   print FILE "$file_out \n";
   print FILE "$yr $mo $dy $hr_out $jday $hr_out\n";

 # end of file naming mess

  close(FILE);

  system ("./$filename");

 }
 }

 }
 }
system("rm $filename");
system("rm $filename.input");
