#include <stdio.h>
#include <stdlib.h>
#include "grib.h"
#include "read_grib.h"

main(argc,argv)
int argc;
char *argv[];
{

  GribParameters gribmap;
  GribTableEntry entry;

  char infile[] = "/home/tahutchinson/projects/venture/config/grib/gribmap.txt";
  int status;

  /*
   * This function indexes the grib file and fills the gribinfo
   *  structure.  It needs to be called before any other
   *  rg_ functions.
   */
  status = rg_setup_gribmap(&gribmap,infile);

  status = rg_gribmap_parameter(&gribmap,"UGRD",-1,&entry);
  fprintf(stderr,"%d %d %d %d %s %s\n",
	  entry.center, entry.subcenter, entry.table, entry.parmid, 
	  entry.name, entry.comment);

  status = rg_gribmap_parameter(&gribmap,"BouPTYPE",204,&entry);
  fprintf(stderr,"%d %d %d %d %s %s\n",
	  entry.center, entry.subcenter, entry.table, entry.parmid, 
	  entry.name, entry.comment);

  rg_free_gribmap_elements(&gribmap);

}
