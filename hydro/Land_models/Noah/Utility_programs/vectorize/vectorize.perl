#!/usr/bin/perl

# Set the number of days you want to run the system
$day_start = 32;  # 305=Nov 1
$day_end = 273;    # 181=Jun 30; 212=Jul 31; 243=Aug 31
                   
$year = 2005;

# Utilities
@nums = ("00","01","02","03","04","05","06","07","08","09","10",
         "11","12","13","14","15","16","17","18","19","20",
         "21","22","23","24","25","26","27","28","29","30",
	 "31","32","33","34","35","36","37","38","39","40",
	 "41","42","43","44","45","46","47","48","49","50",
	 "51","52","53","54","55","56","57","58","59","60",
	 "61","62","63","64","65","66","67","68","69","70",
	 "71","72","73","74","75","76","77","78","79","80",
	 "81","82","83","84","85","86","87","88","89","90",
	 "91","92","93","94","95","96","97","98","99");
@days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days = (0,31,60,91,121,152,182,213,244,274,305,335,366);

# This will be the outer time loop

for($julday=$day_start;$julday<=$day_end;$julday=$julday+1)

 { 
 
 # This little section finds the text month and day in format of date and netcdf file title
 
 for($mo=1;$mo<=12;$mo++)
  {
  if($year == 2004 || $year == 2008) 
   {
    if($julday>$leap_days[$mo-1] && $julday<=$leap_days[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $leap_days[$mo-1];
     }
   }else{
    if($julday>$days[$mo-1] && $julday<=$days[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $days[$mo-1];
     }
    }
  }

###########################################
# Vectorize LDASIN files
########################################### 

  print("Starting vectorize for $year$nums[$mon]$nums[$day]$nums[$hr]\n");

  for($hr=0;$hr<=23;$hr++)
   {
    if($hr == 0) 
     {
      system("vectorize_ldasin_00Z $year$nums[$mon]$nums[$day]$nums[$hr].LDASIN_DOMAIN1");
     }else{
      system("vectorize_ldasin_non00Z $year$nums[$mon]$nums[$day]$nums[$hr].LDASIN_DOMAIN1");
     }
   }

 } # End of outer time loop
