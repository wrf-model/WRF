#include <stdio.h>
#include <stdlib.h>
#include "grib1_routines.h"

#define LATLON 0
#define LAMBERT 1
#define POLAR_STEREO 2
#define MERCATOR 3

main()
{
  float level;
  int projection;
  int xdim;
  int ydim;
  int grid_id;
  float center_lat, center_lon;
  float proj_central_lon;
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
  Grib1_Table_Info grib_table_info;
  int ret;
  int status;
  Grid_Info gridinfo;

  level = .995;
  projection = POLAR_STEREO;
  xdim = 422;
  ydim = 271;
  center_lat = 0.0;
  center_lon = 0.0;
  proj_central_lon = 0.0;
  dx = 12.0;
  dy = 12.0;
  south = 0;
  latin1 = 30.0;
  latin2 = 60.0;
  data = (float *)calloc(xdim*ydim,sizeof(float));
  fcst_secs = 180;
  accum_period = 180;
  leveltype = 119;
  level2 = 0.0;
  grid_id = 255;

  read_gribmap_("gribmap.txt",&grib_table_info,&ret);

  open_file_("test2.grb","w",&filefd,&error,9,1);
  strcpy(datestr,"2005-01-01_00:00:00");
  for (i=0; i< 1; i++) {
    for (j=0; j<xdim*ydim; j++) {
      /*
      data[j] = rand()/RAND_MAX;
      */
      data[j] = j/(float)ydim;
    }
    fprintf(stderr,"Writing grib record %d\n",i);
    LOAD_GRID_INFO("TSK", datestr, &leveltype, &level, 
		   &level2, &fcst_secs, &accum_period, &grid_id, &projection, 
		   &xdim, &ydim, &center_lat, &center_lon, &dx, &dy, 
		   &proj_central_lon, &south, &latin1, &latin2,
		   &grib_table_info, &gridinfo);
      
    WRITE_GRIB(&gridinfo, &filefd, data);
    /*
    status = write_grib_("ABC",&level,&level2,&leveltype,
		datestr,&fcst_secs,&accum_period,
		&projection,&grid_id,&xdim,&ydim,&center_lat,
		&center_lon,&proj_central_lon,&dx,&dy,&south,&latin1,
		&latin2,data,&grib_table_info,&filefd,8);
    */
    fprintf(stderr,"status: %d\n",status);

  }

  free_gribmap_(&grib_table_info);
}

