#include <stdio.h>

main()
{
  int i;
  long longint = -10342;
  char charstring[sizeof(long)];
  char charstring_orig[sizeof(long)];

  memcpy(charstring_orig,(void *)&longint,8);
  set_bytes(longint,3,charstring);
  for (i=0; i < sizeof(long); i++) 
    {
      fprintf(stderr,"new:  %d: %d\n",i,charstring[i]);
      /*      fprintf(stderr,"orig: %d: %d\n",i,charstring_orig[i]); */
    }
}

