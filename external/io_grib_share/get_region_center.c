#include "get_region_center.h"
#include "gridnav.h"
#include <stdio.h>
#include <stdlib.h>

#include "wrf_projection.h"

int get_gridnav_projection(int wrf_projection);

/****************************************************************************
 *
 * This function calculates the center lat and lon of a region within a larger
 *   domain.  It is useful for calculating the center of the boundary regions
 *   in a domain.
 *
 ****************************************************************************/


int GET_REGION_CENTER(char *MemoryOrderIn, int *projection, 
		      float *domain_center_lat, 
		      float *domain_center_lon, int *full_xsize, 
		      int *full_ysize, float *dx, float *dy, 
		      float *proj_central_lon, 
		      int *proj_center_flag, float *truelat1, 
		      float *truelat2, int *region_xsize, int *region_ysize, 
		      float *region_center_lat, float *region_center_lon, 
		      int strlen1)
{

  char *MemoryOrder;
  int grid_projection;
  float full_xcenter, full_ycenter;
  float region_xcenter, region_ycenter;
  int status;
  int orig;
  int x_pos, y_pos;
  GridNav gridnav;

  MemoryOrder = (char *)malloc((strlen1+1)*sizeof(char));
  memcpy(MemoryOrder,MemoryOrderIn,strlen1);
  MemoryOrder[strlen1] = '\0';

  grid_projection = get_gridnav_projection(*projection);

  full_xcenter = (*full_xsize - 1) / 2.;
  full_ycenter = (*full_ysize - 1) / 2.;
  region_xcenter = (*region_xsize - 1) / 2.;
  region_ycenter = (*region_ysize - 1) / 2.;

  orig = 0;

  if (strncmp(MemoryOrder,"XS", 2) == 0) 
    {
      x_pos = region_xcenter;
      y_pos = full_ycenter;
    }
  else if (strncmp(MemoryOrder,"XE", 2) == 0)
    {
      x_pos = (*full_xsize - 1) - region_xcenter;
      y_pos = full_ycenter;
    }
  else if (strncmp(MemoryOrder,"YS", 2) == 0) 
    {
      x_pos = full_xcenter;
      y_pos = region_ycenter;
    }
  else if (strncmp(MemoryOrder,"YE", 2) == 0) 
    {
      x_pos = full_xcenter;
      y_pos = (*full_ysize - 1) - region_ycenter;
    }
  else
    {
      orig = 1;
    }

  if (orig == 1)
    {
      *region_center_lat = *domain_center_lat;
      *region_center_lon = *domain_center_lon;
      status = 0;
    } 
  else
    {
      /* Initialize grid structure */
      /*
      status = GRID_init(grid_info->center_lat, grid_info->central_lon, 
			 grid_projection,
			 grid_info->latin1, grid_info->latin2, 
			 grid_info->xpoints, grid_info->ypoints, 
			 grid_info->Di, grid_info->Dj,
			 grid_info->center_lat, grid_info->center_lon, 
			 x_center, y_center,
			 &gridnav);
      */
      status = GRID_init(*domain_center_lat, *proj_central_lon, 
			 grid_projection,
			 *truelat1, *truelat2, 
			 *full_xsize, *full_ysize, *dx, *dy,
			 *domain_center_lat, *domain_center_lon, 
			 full_xcenter, full_ycenter,
			 &gridnav);
      if (!status)
	{
	  fprintf(stderr,"get_region_center: error from GRID_init\n");
	}

      /* get lat/lon of center of region */
      status = GRID_to_latlon(&gridnav, x_pos, y_pos, region_center_lat, 
			      region_center_lon);
      if (!status)
	{
	  fprintf(stderr,
		  "get_region_cneter: error from GRID_to_latlon for first lat/lon\n");
	}
      
    }

  free(MemoryOrder);
  return status;

}
/******************************************************************************
 * translates the grid projection identifier from the WRF id to the grib id.
 *****************************************************************************/

int get_gridnav_projection(int wrf_projection)
{
  int gridnav_projection;

  /* Set the grid projection in the gridnav units */
  switch (wrf_projection) 
    {
    case WRF_LATLON:
    case WRF_CASSINI:
      gridnav_projection = GRID_LATLON;
      break;
    case WRF_MERCATOR:
      gridnav_projection = GRID_MERCATOR;
      break;
    case WRF_LAMBERT:
      gridnav_projection = GRID_LAMCON;
      break;
    case WRF_POLAR_STEREO:
      gridnav_projection = GRID_POLSTR;
      break;
    default:
      fprintf(stderr,"Error, invalid projection: %d\n",wrf_projection);
      gridnav_projection = -1;
    }
  
  return gridnav_projection;
}

int GET_LL_LATLON(float *central_lat, float *central_lon, int *projection,
		  float *latin1, float *latin2, int *nx, int *ny, 
		  float *dx, float *dy, float *center_lat, float *center_lon,
		  float *LLLa, float *LLLo, float *URLa, float *URLo, int *ierr)
{

  int grid_projection;
  float x_center;
  float y_center;
  GridNav gridnav;
  int status;

  grid_projection = get_gridnav_projection(*projection);

  /* Get coords of center of grid */
  x_center = (*nx + 1)/2.;
  y_center = (*ny + 1)/2.;

 /* Initialize grid structure */
  status = GRID_init(*central_lat, *central_lon, grid_projection,
		     *latin1, *latin2, *nx, *ny, *dx, *dy,
		     *center_lat, *center_lon, x_center, y_center,
		     &gridnav);
  if (!status)
    {
      fprintf(stderr,"write_grib: error from GRID_init\n");
      *ierr = 1;
      return(0);
    }

  /* get lat/lon of lower left corner */
  status = GRID_to_latlon(&gridnav, 1, 1, LLLa, LLLo);
  if (!status)
    {
      fprintf(stderr,
	      "write_grib: error from GRID_to_latlon for first lat/lon\n");
      *ierr = 1;
      return(0);
    }

  /* get lat/lon of upper right corner */
  status = GRID_to_latlon(&gridnav, *nx, *ny, URLa, URLo);
  if (!status)
    {
      fprintf(stderr,
	      "write_grib: error from GRID_to_latlon for first lat/lon\n");
      *ierr = 1;
      return(0);
    }

  *ierr = 0;
  return(0);

}
