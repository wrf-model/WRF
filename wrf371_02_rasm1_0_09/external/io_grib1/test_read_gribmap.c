#include <stdio.h>
#include <stdlib.h>

#include "gribmap.h"
main()
{
  int center, subcenter, parmtbl, parm_id, dec_sc_factor;
  int ret;
  int idx;
  Grib_Table_Info grib_table_info;

  /*  get_grib_param_("gribmap.txt","TMN",&center,&subcenter,&parmtbl,&parm_id,&dec_sc_factor);
   */
  read_gribmap_("gribmap.txt",&grib_table_info,&ret);

  fprintf(stdout,"%d:%d:%d:%d\n",grib_table_info.center,
	  grib_table_info.subcenter,grib_table_info.parmtbl,
	  grib_table_info.gribid);

  get_grib_param_(&grib_table_info,"TMN",&idx);
  fprintf(stdout,"idx: %d\n",idx);

}
