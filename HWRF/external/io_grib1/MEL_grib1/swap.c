#include <stdio.h>
#include <stdlib.h>
void *swap_byte4(long *theInt)
{
  char tmp[4];
  int i;

  for (i=0; i<sizeof(long); i++)  
    tmp[i] = *((char*)theInt+(sizeof(long)-1-i));

  *theInt=*((long *)tmp);

}

void *swap_byte2(short *theInt)
{
  char tmp[4];
  int i;

  for (i=0; i<sizeof(short); i++)  
    tmp[i] = *((char*)theInt+(sizeof(short)-1-i));

  *theInt=*((short *)tmp);
}
