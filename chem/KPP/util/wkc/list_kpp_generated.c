#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"



void
list_kpp_generated( )
{
  knode_t * p1, * p2, * pm1;
  FILE *output;
  char *output_filename = "kpp_sources.txt";
  char kname[NAMELEN];

  output = fopen( output_filename, "W" );

  for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next )
  {
    p2 = p1->assoc_wrf_pack;
    if ( p2 )
    {
      strcpy( kname, p1->name );
      fprintf( output, "module_kpp_%s_Integr.o \\\n",        kname );
      fprintf( output, "module_kpp_%s_Precision.o \\\n",     kname );
      fprintf( output, "module_kpp_%s_Parameters.o \\\n",    kname );
      fprintf( output, "module_kpp_%s_Jacobian.o \\\n",      kname );
      fprintf( output, "module_kpp_%s_JacobianSP.o \\\n",    kname );
      fprintf( output, "module_kpp_%s_Update_Rconst.o \\\n", kname );
      fprintf( output, "module_kpp_%s_interface.o \\\n",     kname );
    }
  }

  fclose( output );
}
