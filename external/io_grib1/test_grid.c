#include <stdio.h>
#include <stdlib.h>

#include "gridnav.h"
main()
{
  GridNav gridnav;
  float first_lat, first_lon;
  int status;

  status = GRID_init(41.0, -105.0, GRID_LAMCON, 
		     30.0, 60.0, 95, 73, 36.0, 36.0, 
		     41.0, -105.0, 48, 37, 
		     &gridnav);
  status = GRID_to_latlon(&gridnav, 1, 73, &first_lat, &first_lon);
  fprintf(stderr,"first_lat: %f  first_lon: %f\n",first_lat,first_lon);

}
