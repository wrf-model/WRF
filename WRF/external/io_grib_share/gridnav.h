/*****************************************************************************
 * Todd Hutchinson
 * WSI Corporation
 * Billerica, MA
 *****************************************************************************/

/* This header contains the public interface to the GridNav API. */

typedef struct {
  float central_lat;
  float central_lon;
  int map_proj; 
  float truelat1;
  float truelat2;
} Projection;

typedef struct {
  int num_columns;
  int num_rows;
  float dx;
  float dy;
  float lat_origin;
  float lon_origin;
  float origin_column;
  float origin_row;
} GridStruct;

typedef struct {
  double parm1;
  double parm2;
  double parm3;
  double parm4;
  double parm5;
  double parm6;
  double parm7;
} ProjTransform;

typedef struct {
  Projection proj;
  GridStruct grid;
  ProjTransform proj_transform;
} GridNav;


/* Public Interface */

int GRID_init(float center_lat, float center_lon, int projection, 
	      float truelat1, float truelat2, int num_columns, 
	      int num_rows, float dx, float dy, float lat_origin, 
	      float lon_origin, float origin_column, float origin_row, 
	      GridNav *gridnav);
int GRID_to_latlon(GridNav *gridnav, float column, float row, float *lat, 
		   float *lon);
int GRID_from_latlon(GridNav *gridnav, float lat, float lon, float *column, 
		     float *row);
int GRID_rotate_from_earth_coords(GridNav *gridnav, float lon, float u_earth, 
				  float v_earth, float *u_grid, float *v_grid);
int GRID_rotate_to_earth_coords(GridNav *gridnav, float lon, float u_grid, 
				float v_grid, float *u_earth, float *v_earth);

#define GRID_LATLON   0
#define GRID_MERCATOR 1
#define GRID_LAMCON   3
#define GRID_POLSTR   5
