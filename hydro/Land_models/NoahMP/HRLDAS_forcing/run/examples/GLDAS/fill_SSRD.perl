#!/usr/bin/perl


$filename = "fill_SSRD";

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
	 
@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

$cc = "20";  # Manually set century

@yys = ("10");

$day_start = 1;
$day_end = 3;

$data_dir = "/d1/barlage/data/GLDAS/extracted";

for $yy (@yys)
 {

# This will be the jday time loop

for($jday=$day_start;$jday<=$day_end;$jday++)

 { 
 
 # This little section finds the text month and day
 
 @modays = @noleap_days;
 if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {@modays = @leap_days}

 for($mo=1;$mo<=12;$mo++)
  {
    if($jday>$modays[$mo-1] && $jday<=$modays[$mo]) 
     {
       $mm = $mo;
       $dd = $jday - $modays[$mo-1];
     }
  } 

  print ("$yy $mm $dd $jday\n");
 
  $dd_now = $nums[$dd];
  $mm_now = $nums[$mm];
  $yy_now = $nums[$yy];

for($hh=0; $hh<=21; $hh=$hh+3)
 {

 # GLDAS1 data are 3hr instant NOT averages for the previous three hours

  open(FILE, "> fill_SSRD.input") || die print "Can't open: \n";

 # find the bracket days for interpolation
 #   - will need editing if crossing century

 $file_in = "$data_dir/SWdown/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh].grb";
   print FILE "$file_in \n";

 if($hh != 21) {
   $file_in = "$data_dir/SWdown/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh+3].grb";
     print FILE "$file_in \n";
  }else{
   # find the next day since averages are ending at filename time
   #   - will need editing if crossing century
 
   $jdayp1 = $jday + 1;
   if($dd != $days[$mm]) {
     $ddp1 = $dd + 1;
     $mmp1 = $mm;
     $yyp1 = $yy;
    }else{
     $ddp1 = 1;
     $mmp1 = $mm + 1;
     $yyp1 = $yy;
     if($mmp1 > 12) {
       $mmp1 = 1;
       $yyp1 = $yy + 1;
       $jdayp1 = 1;
     }
    }

   $file_in = "$data_dir/SWdown/GLDAS_SWdown.$cc$nums[$yyp1]$nums[$mmp1]$nums[$ddp1]00.grb";
     print FILE "$file_in \n";

  }
 
  $hh_out = $hh;
  $file_out = "$data_dir/SWdown24/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh_out].grb";
    print FILE "$file_out \n";
    print FILE "$yy $mm $dd $hh_out $jday $hh_out\n";
  $hh_out = $hh + 1;
  $file_out = "$data_dir/SWdown24/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh_out].grb";
    print FILE "$file_out \n";
    print FILE "$yy $mm $dd $hh_out $jday $hh_out\n";
  $hh_out = $hh + 2;
  $file_out = "$data_dir/SWdown24/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh_out].grb";
    print FILE "$file_out \n";
    print FILE "$yy $mm $dd $hh_out $jday $hh_out\n";

  if($hh != 21) {
    $hh_out = $hh + 3;
    $file_out = "$data_dir/SWdown24/GLDAS_SWdown.$cc$yy_now$mm_now$dd_now$nums[$hh_out].grb";
      print FILE "$file_out \n";
      print FILE "$yy $mm $dd $hh_out $jday $hh_out\n";
   }else{
 
    $file_out = "$data_dir/SWdown24/GLDAS_SWdown.$cc$nums[$yyp1]$nums[$mmp1]$nums[$ddp1]00.grb";
      print FILE "$file_out \n";
      print FILE "$yyp1 $mmp1 $ddp1 0 $jdayp1 0\n";
  }

 # end of file naming mess

  close(FILE);

  system ("$filename");

 }
 }
 }
system("rm $filename");
system("rm $filename.input");
