#include <stdio.h>
#include <stdlib.h>
#include "grib.h"
#include "read_grib.h"

main(argc,argv)
int argc;
char *argv[];
{
  char enddate[15];
  advance_time_str("200509081200",-243,enddate);
  fprintf(stderr,"advanced to %s\n",enddate);
}
