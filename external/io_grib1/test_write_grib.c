#include <stdio.h>
#include <stdlib.h>
#include "gribmap.h"
main()
{
  float level;
  int projection;
  int xdim;
  int ydim;
  float center_lat, center_lon;
  int proj_central_lon;
  float dx, dy;
  int south;
  float latin1, latin2;
  float *data;
  int filefd;
  int error;
  char datestr[200];
  int i,j;
  float fcst_secs;
  float accum_period;
  int leveltype;
  float level2;  
  Grib_Table_Info grib_table_info;


  level = .995;
  projection = 0;
  xdim = 422;
  ydim = 271;
  center_lat = 0.0;
  center_lon = 0.0;
  proj_central_lon = 0.0;
  dx = .018;
  dy = .018;
  south = 0;
  latin1 = 0.0;
  latin2 = 0.0;
  data = (float *)calloc(xdim*ydim,sizeof(float));
  fcst_secs = 180.0;
  accum_period = 180.0;
  leveltype = 105;
  level2 = 0.0;

  read_gribmap_("gribmap.txt",&grib_table_info);

  open_file_("test.grb",&filefd,&error,8);
  strcpy(datestr,"   0-01-01_00:00:00");
  for (i=0; i< 1000; i++) {
    for (j=0; j<xdim*ydim; j++) {
      /*
      data[j] = rand()/RAND_MAX;
      */
      data[j] = j/(float)ydim;
    }
    fprintf(stderr,"i: %d !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",i);
    write_grib_("U",&level,&level2,&leveltype,
		datestr,&fcst_secs,&accum_period,
		&projection,&xdim,&ydim,&center_lat,
		&center_lon,&proj_central_lon,&dx,&dy,&south,&latin1,
		&latin2,data,&grib_table_info,&filefd,1);
  }
}

