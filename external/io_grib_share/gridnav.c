#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gridnav.h"

#define MISSING -999
#define PI 3.1415927
#define RAD_TO_DEG 57.29577951
/* #define EARTH_RAD 6371.200 */
#define EARTH_RAD 6370.000

int fill_proj_parms(GridNav *gridnav);

/*****************************************************************************
 * Fills up the gridnav structure using arguments input to the function
 *
 * input:
 *   central_lat - the central latitude of the projection, in degrees
 *   central_lon - the central longitude of the projection, in degrees
 *   projection  - the projection number (defined by #define's in gridnav.h)
 *   truelat1    - the first "true" latitude.  Only has an effect with 
 *                 polar stereographic and lambert projections
 *   truelat2    - the second true latitude.  Only has an effect with lambert
 *                 projection
 *   num_columns - number of columns in grid
 *   num_rows    - number of rows in grid
 *   dx,dy       - east-west (dx) and north-south (dy) distance between grid 
 *                 points, in degrees for latlon (i.e., cylindrical 
 *                 equidistant) projection, in km for all other projections 
 *                 (including mercator).
 *   lat_origin  - latitude of grid point defined in origin_column, origin_row
 *   lon_origin  - longitude of grid point defined in origin_column, origin_row
 *   origin_column - column for lat_origin, long_origin pair
 *   origin_row  - row for lat_origin, long_origin pair
 *  
 * output:
 *   gridnav - filled gridnav structure
 *
 *****************************************************************************/

int GRID_init(float central_lat, float central_lon, int projection, 
	      float truelat1, float truelat2, int num_columns, 
	      int num_rows, float dx, float dy, float lat_origin, 
	      float lon_origin, float origin_column, float origin_row, 
	      GridNav *gridnav)
{

  int status = 1;

  gridnav->proj.central_lon = central_lon;
  gridnav->proj.central_lat = central_lat;
  gridnav->proj.map_proj = projection;
  gridnav->proj.truelat1 = truelat1;
  gridnav->proj.truelat2 = truelat2;

  gridnav->grid.num_columns = num_columns;
  gridnav->grid.num_rows = num_rows;
  gridnav->grid.dx = dx;
  gridnav->grid.dy = dy;
  gridnav->grid.lat_origin = lat_origin;
  gridnav->grid.lon_origin = lon_origin;
  gridnav->grid.origin_column = origin_column;
  gridnav->grid.origin_row = origin_row;

  fill_proj_parms(gridnav);

  return status;
}

/*****************************************************************************
 * Outputs the latitude and longitude of a grid point.
 *
 * input:
 *   *gridnav - a pointer to a filled gridnav structure.  The gridnav 
 *              structure should have been filled using one of the init 
 *              functions.
 *   column   - the column in the grid to retrieve.
 *   row      - the row in the grid to retrieve.
 * 
 * output:
 *   *lat - pointer to output latitude
 *   *lon - pointer to output longitude
 *
 *****************************************************************************/

int GRID_to_latlon(GridNav *gridnav, float column, float row, float *lat, 
		   float *lon)
{
    /* 
     * Calculate the latitude and longitude for an input grid point location 
     */
    double X, Y, R;
    int status = 1;
    double eps = 0.00001;

    switch (gridnav->proj.map_proj)
	{
	case GRID_MERCATOR:
	    *lat = 2 * RAD_TO_DEG *
		atan(exp((gridnav->proj_transform.parm2 + gridnav->grid.dx *
			  (row - gridnav->grid.origin_row)) /
			 gridnav->proj_transform.parm1 )) - 90;
	    *lon = gridnav->grid.lon_origin + RAD_TO_DEG * gridnav->grid.dx *
		(column - gridnav->grid.origin_column) /
		gridnav->proj_transform.parm1;
	    break;

	case GRID_LAMCON:
	    X = (column - gridnav->grid.origin_column) * gridnav->grid.dx +
		gridnav->proj_transform.parm6;
	    Y = (row - gridnav->grid.origin_row) * gridnav->grid.dy +
		gridnav->proj_transform.parm3 + gridnav->proj_transform.parm7;
	    R = sqrt(X*X + Y*Y);
	    *lat = gridnav->proj_transform.parm5 * 90 - 
		2 * RAD_TO_DEG * atan(gridnav->proj_transform.parm4 * 
				      pow(R, 1 / 
					  gridnav->proj_transform.parm2));
	    *lon = gridnav->proj.central_lon + 
		RAD_TO_DEG / gridnav->proj_transform.parm2 *
		atan(X / (gridnav->proj_transform.parm5 * -Y));
	    while (*lon > 180) *lon -= 360;
	    while (*lon <= -180) *lon += 360;
	    break;

	case GRID_POLSTR:
	    X = (column - gridnav->grid.origin_column) * 
		gridnav->proj_transform.parm3 + 
		gridnav->proj_transform.parm1;
	    Y = (row - gridnav->grid.origin_row) * 
		gridnav->proj_transform.parm3 +
		gridnav->proj_transform.parm2 + eps;
	    *lon = gridnav->proj_transform.parm5 * -1 * 
		atan(X / Y) * RAD_TO_DEG + gridnav->proj.central_lon;
            *lat = (gridnav->proj_transform.parm5 * 90) - 2 * RAD_TO_DEG *
                atan(X / (2 * EARTH_RAD *
                          sin((*lon - gridnav->proj.central_lon ) /
                              RAD_TO_DEG) + eps) );
	    while (*lon > 180) *lon -= 360;
	    while (*lon <= -180) *lon += 360;
	    break;
      
	case GRID_LATLON:
	    *lat = (row - gridnav->grid.origin_row)*gridnav->grid.dy +
		gridnav->grid.lat_origin;
	    *lon = (column - gridnav->grid.origin_column)*gridnav->grid.dx +
		gridnav->grid.lon_origin;
	    break;

	default:
	    /* 
	     *  Unsupported map projection: set lat-lon to no-data values. 
	     */ 
	    fprintf(stderr,"GRID_to_latlon: Unsupport map projection type %d\n", 
		    gridnav->proj.map_proj); 
	    *lon = -9999; 
	    *lat = -9999; 
	    status = 0; 
	    break; 

	} /* end of switch on map projection type */


    return status;
}


/*****************************************************************************
 * Outputs grid point indices, given lat/lon location.
 *
 * input:
 *   *gridnav - a pointer to a filled gridnav structure.  The gridnav 
 *              structure should have been filled using one of the init 
 *              functions.
 *   lat      - latitude for location
 *   lon      - longitude for location
 * output:
 *   *column  - pointer to value for column
 *   *row     - pointer to value for row
 *
 *****************************************************************************/

int GRID_from_latlon(GridNav *gridnav, float lat, float lon, float *column, 
		     float *row)
{
    double X, Y, Rs;
    double lat_rad;
    int status = 1;

    switch (gridnav->proj.map_proj) 
	{
	case GRID_MERCATOR:
	    X = gridnav->proj_transform.parm1 *
		((lon - gridnav->grid.lon_origin) / RAD_TO_DEG);
	    *column = gridnav->grid.origin_column + X / gridnav->grid.dx;
	    lat_rad = lat / RAD_TO_DEG;
	    Y = gridnav->proj_transform.parm1 * log((1 + sin(lat_rad)) / 
						    cos(lat_rad));
	    *row = gridnav->grid.origin_row + 
		((Y-gridnav->proj_transform.parm2) / gridnav->grid.dy);;
	    break;

	case GRID_LAMCON:
	    Rs = (EARTH_RAD / gridnav->proj_transform.parm2) *
		sin(gridnav->proj_transform.parm1) *
		pow(tan((gridnav->proj_transform.parm5 * 90 - lat) / 
			(2 * RAD_TO_DEG))/
		    tan(gridnav->proj_transform.parm1 / 2),
		    gridnav->proj_transform.parm2);
	    *row = gridnav->grid.origin_row - 
		(1 / gridnav->grid.dy) * 
		(gridnav->proj_transform.parm3 + 
		 Rs * cos(gridnav->proj_transform.parm2*
			  (lon - gridnav->proj.central_lon) / RAD_TO_DEG)) -
		gridnav->proj_transform.parm7 / gridnav->grid.dy;
	    *column = gridnav->grid.origin_column + 
		( gridnav->proj_transform.parm5*
		((Rs / gridnav->grid.dx)*
		 sin(gridnav->proj_transform.parm2 * 
		     (lon - gridnav->proj.central_lon) / RAD_TO_DEG) - 
		  gridnav->proj_transform.parm6 / gridnav->grid.dx));
	    break;

	case GRID_POLSTR:	    
	    Rs = 2 * EARTH_RAD * tan ( (45 - fabs(lat)/2) / RAD_TO_DEG );
	    Y = gridnav->proj_transform.parm5 * -1 * Rs * 
		cos ((lon - gridnav->proj.central_lon) / RAD_TO_DEG);
	    X = Rs * sin ((lon - gridnav->proj.central_lon) / RAD_TO_DEG);

	    *row = (Y - gridnav->proj_transform.parm2) / 
		gridnav->proj_transform.parm3 
		+ gridnav->grid.origin_row;
	    *column = (X - gridnav->proj_transform.parm1) / 
		gridnav->proj_transform.parm3 
		+ gridnav->grid.origin_column;
	    break;
    
	case GRID_LATLON:
	    *row = ((lat - gridnav->grid.lat_origin) / gridnav->grid.dy) +
		gridnav->grid.origin_row;
	    /* If lon is negative, make it positive */
	    while (lon < 0) lon += 360.;
	    *column = ((lon - gridnav->grid.lon_origin) / gridnav->grid.dx) +
		gridnav->grid.origin_column;
	    break;

	default:
	    fprintf(stderr,"GRID_from_latlon: Unsupported map projection type %d\n",
		    gridnav->proj.map_proj);
	    *column = -9999;
	    *row = -9999;
	    status = 0;
	    break;

	} /* End of switch on map projection type */

    return status;
}

int fill_proj_parms(GridNav *gridnav)
{
    double orig_lat_rad;
    double R_orig;
    double r_not;
    double r_truelat1;
    int hemifactor;
  
    switch (gridnav->proj.map_proj) 
	{
	case GRID_MERCATOR:
	    gridnav->proj_transform.parm1 = 
		EARTH_RAD * cos(gridnav->proj.truelat1 / RAD_TO_DEG);
	    orig_lat_rad = (gridnav->grid.lat_origin) / RAD_TO_DEG;
	    gridnav->proj_transform.parm2 = 
		gridnav->proj_transform.parm1 *
		log((1. + sin(orig_lat_rad)) / cos(orig_lat_rad));
	    gridnav->proj_transform.parm3 = MISSING;
	    gridnav->proj_transform.parm4 = MISSING;
	    gridnav->proj_transform.parm5 = MISSING;
	    break;
	case GRID_LAMCON:
	    if (gridnav->proj.truelat1 >= 0) 
		{
		    hemifactor = 1;
		} 
	    else 
		{
		    hemifactor = -1;
		}
	    /* This is Psi1 in MM5 speak */
	    gridnav->proj_transform.parm1 = 
		hemifactor*(PI/2 - fabs(gridnav->proj.truelat1) / RAD_TO_DEG);
	    /* This is Kappa in MM5 speak */
            if (fabs(gridnav->proj.truelat1 - gridnav->proj.truelat2) < .01) 
		{
		    gridnav->proj_transform.parm2 = 
                        fabs(sin(gridnav->proj.truelat1 / RAD_TO_DEG));
		}
	    else 
		{
		    gridnav->proj_transform.parm2 = 
			(log10(cos(gridnav->proj.truelat1 / RAD_TO_DEG)) - 
			 log10(cos(gridnav->proj.truelat2 / RAD_TO_DEG))) /
			(log10(tan((45 - fabs(gridnav->proj.truelat1) / 2) / RAD_TO_DEG)) - 
			 log10(tan((45 - fabs(gridnav->proj.truelat2) / 2) / RAD_TO_DEG)));
		}
	    /* This is Yc in MM5 speak */
	    gridnav->proj_transform.parm3 = 
		-EARTH_RAD/gridnav->proj_transform.parm2 * 
		sin(gridnav->proj_transform.parm1) *
		pow(
		    (tan((hemifactor * 90 - gridnav->grid.lat_origin) / 
			 (RAD_TO_DEG * 2)) /
		     tan(gridnav->proj_transform.parm1 / 2)),
		    gridnav->proj_transform.parm2);
	    gridnav->proj_transform.parm4 = 
		tan(gridnav->proj_transform.parm1 / 2)*
		pow(hemifactor * (gridnav->proj_transform.parm2)/
		    (EARTH_RAD * sin(gridnav->proj_transform.parm1)),
		    1 / gridnav->proj_transform.parm2);
	    gridnav->proj_transform.parm5 = hemifactor;
	    R_orig = (EARTH_RAD / gridnav->proj_transform.parm2) *
		sin(gridnav->proj_transform.parm1) *
		pow(tan((gridnav->proj_transform.parm5 * 90 - 
			 gridnav->grid.lat_origin) / 
			(2 * RAD_TO_DEG))/
		    tan(gridnav->proj_transform.parm1 / 2),
		    gridnav->proj_transform.parm2);
	    /* X origin */
	    gridnav->proj_transform.parm6 = 
		R_orig * sin(gridnav->proj_transform.parm2 * 
			     (gridnav->grid.lon_origin - 
			      gridnav->proj.central_lon) / RAD_TO_DEG);

	    /* Y origin */
	    gridnav->proj_transform.parm7 = -(gridnav->proj_transform.parm3 + 
		R_orig * cos(gridnav->proj_transform.parm2 * 
			     (gridnav->grid.lon_origin - 
			      gridnav->proj.central_lon) / RAD_TO_DEG));
	    break;
	case GRID_POLSTR:
            if (gridnav->proj.central_lat > 0) 
                {
                    hemifactor = 1;
                } 
            else 
                {
                    hemifactor = -1;
                }

	    /* Calculate X for origin */
	    r_not = 2 * EARTH_RAD * 
		tan((45 - fabs(gridnav->grid.lat_origin) / 2) / RAD_TO_DEG);
	    gridnav->proj_transform.parm1 =  
		r_not * 
		sin ( (gridnav->grid.lon_origin - gridnav->proj.central_lon) /
		      RAD_TO_DEG);
	    /* Calculate Y for origin */
	    gridnav->proj_transform.parm2 = 
		hemifactor * -1 * r_not * 
		cos ( (gridnav->grid.lon_origin - gridnav->proj.central_lon) / 
		      RAD_TO_DEG);
	    /* Calculate grid spacing at pole */
	    r_truelat1 = 2 * EARTH_RAD * 
		tan((45 - fabs(gridnav->proj.truelat1) / 2) / RAD_TO_DEG);
	    gridnav->proj_transform.parm3 = 
		gridnav->grid.dx * r_truelat1 / 
		( EARTH_RAD * cos (gridnav->proj.truelat1 / RAD_TO_DEG));
            gridnav->proj_transform.parm4 = MISSING;
            gridnav->proj_transform.parm5 = hemifactor;
            break;
	case GRID_LATLON:
	  gridnav->proj_transform.parm1 = MISSING;
	  gridnav->proj_transform.parm2 = MISSING;
	  gridnav->proj_transform.parm3 = MISSING;
	  gridnav->proj_transform.parm4 = MISSING;
	  gridnav->proj_transform.parm5 = MISSING;
	  break;

	default:
	    fprintf(stderr,"GRID_init_mm5data: Invalid Projection\n");
	    return 0;
	}
    return 1;
}

/*****************************************************************************
 * Rotates u and v components of wind, from being relative to earth, to 
 *  relative to grid.
 *
 * input:
 *   *gridnav - a pointer to a filled gridnav structure.  The gridnav 
 *              structure should have been filled using one of the init 
 *              functions.
 *   lon      - longitude for location
 *   u_earth  - the u component of the wind in earth (north-relative) 
 *              coordinates.
 *   v_earth  - the v component of the wind in earth (north-relative) 
 *              coordinates.
 * output:
 *   *u_grid  - pointer to value for u in grid coordinates
 *   *v_grid  - pointer to value for v in grid coordinates
 *
 *****************************************************************************/

int GRID_rotate_from_earth_coords(GridNav *gridnav, float lon, float u_earth, 
				  float v_earth, float *u_grid, float *v_grid)
{
    float speed, dir;
    float dir_grid;

    /* Calculate Speed and Direction from u,v */
    switch (gridnav->proj.map_proj) 
	{
	case GRID_MERCATOR:
	    *u_grid = u_earth;
	    *v_grid = v_earth;
	    break;
	case GRID_POLSTR: case GRID_LAMCON:
	    speed = sqrt(u_earth * u_earth + v_earth * v_earth);
	    dir = RAD_TO_DEG * atan2(-u_earth,-v_earth);
	    while (dir >= 360) dir -= 360;
	    while (dir < 0) dir += 360;
	    
	    dir_grid = dir - (lon - gridnav->proj.central_lon) * 
		gridnav->proj_transform.parm2;
	    while (dir_grid >= 360) dir_grid -= 360;
	    while (dir_grid < 0) dir_grid += 360;
	    
	    *u_grid = -1. * speed * sin(dir_grid / RAD_TO_DEG);
	    *v_grid = -1. * speed * cos(dir_grid / RAD_TO_DEG);
	    break;
	default:
	    fprintf(stderr,
		    "GRID_rotate_from_earth_coords: Invalid Projection\n");
	    return 0;
	}  /* End of switch projection */

    return 1;
}

/*****************************************************************************
 * Rotates u and v components of wind, from being relative to earth, to 
 *  relative to grid.
 *
 * input:
 *   *gridnav - a pointer to a filled gridnav structure.  The gridnav 
 *              structure should have been filled using one of the init 
 *              functions.
 *   lon      - longitude for location
 *   u_grid   - the u component of the wind in grid coordinates 
 *   v_grid   - the v component of the wind in grid coordinates
 *
 * output:
 *   *u_earth - pointer to value for u in earth-relative coordinates
 *   *v_grid  - pointer to value for v in earth-relative coordinates
 *
 *****************************************************************************/

int GRID_rotate_to_earth_coords(GridNav *gridnav, float lon, float u_grid, 
				float v_grid, float *u_earth, float *v_earth)
{
    float speed, dir_grid;
    float dir_earth;
    
    /* Calculate Speed and Direction from u,v */
    switch (gridnav->proj.map_proj) 
	{
	case GRID_MERCATOR:
	    *u_earth = u_grid;
	    *v_earth = v_grid;
	    break;
	case GRID_POLSTR: case GRID_LAMCON:
	    speed = sqrt(u_grid * u_grid + v_grid * v_grid);
	    dir_grid = RAD_TO_DEG * atan2(-u_grid, -v_grid);
	    while (dir_grid >= 360) dir_grid -= 360;
	    while (dir_grid < 0) dir_grid += 360;
	    
	    dir_earth = dir_grid + (lon - gridnav->proj.central_lon) *
		gridnav->proj_transform.parm2;
	    
	    while (dir_earth >= 360) dir_earth -= 360;
	    while (dir_earth < 0) dir_earth += 360;
	    
	    *u_earth = -1. * speed * sin(dir_earth / RAD_TO_DEG);
	    *v_earth = -1. * speed * cos(dir_earth / RAD_TO_DEG);
	    break;
	default:
	    fprintf(stderr,
		    "GRID_rotate_to_earth_coords: Invalid Projection\n");
	    return 0;
	}  /* End of switch projection */
    return 1;
}
