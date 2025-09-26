#include <stdio.h>
#include <stdlib.h>

int32_t copyfile ( char *ifile, char *ofile)
{
   char buff[BUFSIZ];
   size_t n;
   FILE *source, *target;

   source = fopen(ifile, "r");

   if( source == NULL )
     { return -1; }

   target = fopen(ofile, "wb+");

   if( target == NULL ) {
      fclose(source);
      return -1;
   }

  while( (n=fread(buff,1,BUFSIZ, source)) != 0)  {
    fwrite(buff,1,n,target);
   }

   fclose(source);
   fclose(target);
   return 0;
}
