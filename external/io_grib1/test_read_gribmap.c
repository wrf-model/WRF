#include <stdio.h>
main()
{
  int center, subcenter, parmtbl, gribidx, dec_sc_factor;

  get_grib_param("gribmap.txt","TMN",&center,&subcenter,&parmtbl,&gribidx,&dec_sc_factor);

  fprintf(stdout,"%d:%d:%d\n",center,subcenter,parmtbl);
  fprintf(stdout,"%d:%d\n",gribidx,dec_sc_factor);
}
