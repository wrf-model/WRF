#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "pdstool.h"
#include "gdstool.h"
#include "gridnav.h"
#include "gribmap.h"

#define LATLON 0
#define LAMBERT 1
#define POLAR_STEREO 2
#define MERCATOR 3

#define LINESIZE 300

#define SECS_IN_SEC 1
#define SECS_IN_MIN 60
#define MINS_IN_HOUR 60
#define MINS_IN_5MINS 5
#define HOURS_IN_DAY 24

#define MAX_FCST 65535
#define MAX_FCST_SECS MAX_FCST*SECS_IN_SEC
#define MAX_FCST_MINS MAX_FCST*SECS_IN_MIN
#define MAX_FCST_5MINS MAX_FCST*MINS_IN_5MINS*SECS_IN_MIN
#define MAX_FCST_HOURS MAX_FCST*MINS_IN_HOUR*SECS_IN_MIN
#define MAX_FCST_DAYS MAX_FCST*HOURS_IN_DAY*MINS_IN_HOUR*SECS_IN_MIN

#define MAX1B_FCST 256
#define MAX1B_FCST_SECS MAX1B_FCST*SECS_IN_SEC
#define MAX1B_FCST_MINS MAX1B_FCST*SECS_IN_MIN
#define MAX1B_FCST_5MINS MAX1B_FCST*MINS_IN_5MINS*SECS_IN_MIN
#define MAX1B_FCST_HOURS MAX1B_FCST*MINS_IN_HOUR*SECS_IN_MIN
#define MAX1B_FCST_DAYS MAX1B_FCST*HOURS_IN_DAY*MINS_IN_HOUR*SECS_IN_MIN

#define PI 3.1415

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define WRITE_GRIB write_grib
#      define GET_GRIB_PARAM get_grib_param
# else
#   ifdef F2CSTYLE
#      define WRITE_GRIB  write_grib__
#      define GET_GRIB_PARAM get_grib_param__
#   else
#      define WRITE_GRIB  write_grib_
#      define GET_GRIB_PARAM get_grib_param_
#   endif
# endif
#endif

int get_byte(int fcst_secs, int bytenum);

/****************************************************************************
 *
 * This C function takes in data and calls routines to write out a grib record.
 *
 ****************************************************************************/

int WRITE_GRIB(char *varname, float *level1, float *level2, int *vert_unit,
		char *datestr, 
		int *fcst_secs, int *accum_period, int *projection, int *xdim, 
		int *ydim, float *center_lat, float *center_lon, 
		float *proj_central_lon, float *dx, float *dy, int *south, 
		float *latin1, float *latin2, float *data, 
		Grib_Table_Info *grib_table_info, int *filefd, int strlen)
{
  int param;
  int gribidx;
  int year, month, day, hour, minute;
  float second;
  unsigned char *pds;
  unsigned char *gds;
  int fcst_unit;
  int time_range;
  int P1, P2;
  int fcst_unit_ext_1, fcst_unit_ext_2;
  int P1_ext, P2_ext;
  int time_range_ext;
  int table, center, subcenter, grid;
  char line[LINESIZE];
  GridNav gridnav;
  float x_center, y_center;
  int status;
  float first_lat, first_lon, last_lat, last_lon;
  float dis_x, dis_y;
  int dec_sc_factor;
  int grid_projection;
  int table_index;
  float *data_orig;
  char varname2[1000];

  strncpy(varname2,varname,strlen);
  varname2[strlen]='\0';
  trim(varname2);

  data_orig = (float *)malloc((*xdim)*(*ydim)*sizeof(float));
  memcpy(data_orig,data,(*xdim)*(*ydim)*sizeof(float));

  /* Get coords of center of grid */
  x_center = (*xdim + 1)/2.;
  y_center = (*ydim + 1)/2.;

  if ((*projection == LATLON) || (*projection == MERCATOR)) 
    {
      dis_x = *dx/(110.56*cos(*latin1 * PI/180.));
      dis_y = *dy/110.56;
    } 
  else if ((*projection == LAMBERT) || (*projection == POLAR_STEREO))
    {
      dis_x = *dx;
      dis_y = *dy;
    }

  /* Set the grid projection in the gridnav units */
  switch (*projection) 
    {
    case LATLON:
      grid_projection = GRID_LATLON;
      break;
    case MERCATOR:
      grid_projection = GRID_MERCATOR;
      break;
    case LAMBERT:
      grid_projection = GRID_LAMCON;
      break;
    case POLAR_STEREO:
      grid_projection = GRID_POLSTR;
      break;
    default:
      fprintf(stderr,"Error, invalid projection: %d\n",*projection);
      return -1;
    }

  /* Read the grib parameter table */
  GET_GRIB_PARAM (grib_table_info, varname2, &table_index);
  if (table_index < 0) 
    {
      fprintf(stdout,\
	      "Skipping %s, Could not find parameter for %s in gribmap.txt\n",\
	      varname2,varname2);
      return ;
    }
  center = grib_table_info->center;
  subcenter = grib_table_info->subcenter;
  table = grib_table_info->parmtbl;
  gribidx = grib_table_info->gribidx[table_index];
  dec_sc_factor = grib_table_info->dec_sc_factor[table_index];
  grid = 255;

  param = gribidx;

  sscanf(datestr,"%d-%d-%d_%d:%d:%f",&year,&month,&day,&hour,&minute,&second);

  /*
   * We will code up the forecast times using standard NCEP grib conventions
   *  if possible.  However, when the forecast time is > 65535 seconds (~18 hrs)
   *  and the forecast time is not divisible by 60 s, it is not possible
   *  to use the standard method (without truncation) for encoding the
   *  forecast time.  In those cases, octet PDS21 is set to 255 (missing), and
   *  octets PDS41-PDS51 are used for encoding the time as follows:
   *
   *  PDS21    = 255
   *  PDS41    = 254 forecast time unit for PDS42-45 (254 is seconds)
   *  PDS42-45 = start of period in seconds
   *  PDS46    = 254 forecast time unit for PDS47-51 (254 is seconds)
   *  PDS47-50 = duration of period in seconds
   *  PDS51    = 203 time-range indicator (203 is for duration)
   */


  /* 
   * Added ability to output a "5-minute" forecast time unit for the
   *   sake of WxProducer.  This allows WxPro to ingest data beyond
   *   18 hours, and accumulation data beyond 255 units.
   */

  if (*accum_period == 0) 
    {
      if (*fcst_secs < MAX_FCST_SECS)
	{
	  time_range = 10;
	  fcst_unit = 254;
	  P1 = get_byte(*fcst_secs,2);
	  P2 = get_byte(*fcst_secs,1);
	}
      else if (((*fcst_secs % SECS_IN_MIN) == 0) &&
	       (*fcst_secs < MAX_FCST_MINS))
	{
	  time_range = 10;
	  fcst_unit = 0;
	  P1 = get_byte(*fcst_secs/SECS_IN_MIN,2);
	  P2 = get_byte(*fcst_secs/SECS_IN_MIN,1);
	}
      else if (((*fcst_secs % SECS_IN_MIN*MINS_IN_HOUR) == 0) && 
	       (*fcst_secs < MAX_FCST_HOURS))
	{
	  time_range = 10;
	  fcst_unit = 1;
	  P1 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR),2);
	  P2 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR),1);      
	}
      /* 
       * MAX_FCST_DAYS is causing an integer overflow, so, we'll just skip 
       *   the check here.  It's very unlikely that someone would exceed this
       *   anyway (5.6 million days!)
       */
      /*
      else if (((*fcst_secs % SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY) == 0) &&
	       (*fcst_secs < MAX_FCST_DAYS))
      */
      else if (((*fcst_secs % SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY) == 0))
	{
	  time_range = 10;
	  fcst_unit = 2;
	  P1 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY),2);
	  P2 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY),1);
	}
      else if (((*fcst_secs % SECS_IN_MIN*MINS_IN_5MINS) == 0)
	       && (*fcst_secs < MAX_FCST_5MINS))
	{
	  time_range = 10;
	  fcst_unit = 50;
	  P1 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS),2);
	  P2 = get_byte(*fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS),1);
	}
      else 
	{
	  time_range = 255;
	  fcst_unit = 0;
	  P1 = 0;
	  P2 = 0;
	  
	  fcst_unit_ext_1 = 254;
	  fcst_unit_ext_2 = 254;
	  P1_ext = *fcst_secs;
	  P2_ext = 0;
	  time_range_ext = 0;
	}
    }
  else  /* Accumulation period is not 0 */
    {
      if ((*fcst_secs < MAX1B_FCST_HOURS) &&
	  (*fcst_secs%(SECS_IN_MIN*MINS_IN_HOUR) == 0) && 
	  (*accum_period%(SECS_IN_MIN*MINS_IN_HOUR) == 0))
	{
	  time_range = 4;
	  fcst_unit = 1;
	  P1 = (*fcst_secs-*accum_period)/(SECS_IN_MIN*MINS_IN_HOUR);
	  P2 = *fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR);
	}
      else if ((*fcst_secs < MAX1B_FCST_MINS) &&
	       ((*fcst_secs-*accum_period)%SECS_IN_MIN == 0) && 
	       (*fcst_secs%SECS_IN_MIN == 0))
	{
	  time_range = 4;
	  fcst_unit = 0;
	  P1 = (*fcst_secs-*accum_period)/SECS_IN_MIN;
	  P2 = *fcst_secs/SECS_IN_MIN;
	}
      else if (*fcst_secs < MAX1B_FCST_SECS)
	{
	  time_range = 4;
	  fcst_unit = 254;
	  P1 = *fcst_secs-*accum_period;
	  P2 = *fcst_secs;
	}
      else if ((*fcst_secs < MAX1B_FCST_5MINS) && 
	       (*fcst_secs%(SECS_IN_MIN*MINS_IN_5MINS) == 0) &&
	       (*accum_period%(SECS_IN_MIN*MINS_IN_5MINS) == 0)) 
	{
	  time_range = 4;
	  fcst_unit = 50;
	  P1 = (*fcst_secs-*accum_period)/(SECS_IN_MIN*MINS_IN_5MINS);
	  P2 = *fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS);
	}
      else
	{
	  time_range = 255;
	  fcst_unit = 0;
	  P1 = 0;
	  P2 = 0;
	  
	  fcst_unit_ext_1 = 254;
	  fcst_unit_ext_2 = 254;
	  P1_ext = *fcst_secs - *accum_period;
	  P2_ext = *accum_period;
	  time_range_ext = 203;    /* Duration */
	}
    }

  /* Setup the pds section */
  pds = pdstool(New_ext_PDS(53), P_param_table(table), 
		P_Center(center,subcenter), P_process(220),
		P_grid(grid), P_has_gds(1), P_has_bms(0), P_param(param), 
	        P_year(year), P_month(month), P_day(day), 
		P_hour(hour), P_minute(minute), P_fcst_unit(fcst_unit), 
		P_p1(P1), P_p2(P2), P_time_range(time_range), 
		P_dec_scale(dec_sc_factor), 
		P_ext_fcst_unit_1(fcst_unit_ext_1),
		P_ext_P1(P1_ext),
		P_ext_fcst_unit_2(fcst_unit_ext_2),
		P_ext_P2(P2_ext),
		P_ext_time_range(time_range_ext),
		P_end);


  /* Now, set the level information */
  if (*vert_unit == 119) 
    {
      pdstool(pds,P_eta(*level1),P_end);
    }
  else if (*vert_unit == 112)
    {
      pdstool(pds,P_hgt_layer_below_gnd(*level1,*level2),P_end);
    }
  else if (*vert_unit == 105)
    {
      pdstool(pds,P_hgt_gnd(*level1),P_end);
    }
  else if (*vert_unit == 200)
    {
      pdstool(pds,P_atmos_clm,P_end);
    }
  else 
    {
      fprintf(stderr,"invalid vertical unit: %d\n",*vert_unit);
      return -1;
    }

  /* Initialize grid structure */
  status = GRID_init(*center_lat, *proj_central_lon, grid_projection, 
		     *latin1, *latin2, *xdim, *ydim, dis_x, dis_y, 
		     *center_lat, *center_lon, x_center, y_center, 
		     &gridnav);
  if (!status) 
    {
      fprintf(stderr,"write_grib: error from GRID_init\n");
    }

  /* get lat/lon of lower left corner */
  status = GRID_to_latlon(&gridnav, 1, 1, &first_lat, &first_lon);
  if (!status) 
    {
      fprintf(stderr,"write_grib: error from GRID_to_latlon for first lat/lon\n");
    }

  /* get lat/lon of upper right corner */
  status = GRID_to_latlon(&gridnav, *xdim, *ydim, &last_lat, &last_lon);
  if (!status) 
    {
      fprintf(stderr,"write_grib: error from GRID_to_latlon for last lat/lon\n");
    }

  /* Setup GDS section */
  switch (*projection) 
    {
    case LATLON:
      gds = new_LatLon_GDS(pds,*xdim,*ydim,first_lon,first_lat,last_lon,last_lat,dis_x,dis_y);
      break;
    case LAMBERT:
      gds = new_lambert_GDS(pds,*xdim,*ydim,first_lon,first_lat,
			    *proj_central_lon,dis_x,dis_y,*south,
			    *latin1,*latin2,1);
      break;
    case POLAR_STEREO:
      gds = new_polar_GDS(pds,*xdim,*ydim,first_lon,first_lat,
			  *proj_central_lon,dis_x,dis_y,*south,1);
      break;
    case MERCATOR:
      gds = new_Mercator_GDS(pds,*xdim,*ydim,first_lon,first_lat,last_lon,
			     last_lat,*latin1,1,dis_x,dis_y);
      break;
    default:
      fprintf(stderr,"write_grib: Unsupported projection: %d\n",*projection);
  }

  /* write out grib record */
  wrt_grib_rec(pds, gds, data, (*xdim)*(*ydim), *filefd);

  memcpy(data,data_orig,(*xdim)*(*ydim)*sizeof(float));
  free(data_orig);
  free(gds);
  free(pds);

  return 0;
}

int get_byte(int fcst_secs, int bytenum)
{
  int out;
  out = ((fcst_secs >> (bytenum-1)*8) & ~(~0 <<8));
  return out;
}

/* trim: remove trailing blanks, tabs, newlines */
int trim(char s[])
{
  int n;

  for (n = strlen(s)-1; n >= 0; n--)
    if (s[n] != ' ' && s[n] != '\t' && s[n] != '\n')
      break;
  s[n+1] = '\0';
  return n;
}
