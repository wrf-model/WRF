#include <stdio.h>
#include "isdb.h"

void set_bytes(long in, int numbytes, char *out)
{
  int i;
  long tmp;

  tmp = abs(in);

  for (i=0; i < numbytes; i++)
    {
      out[numbytes-1-i] = (tmp << (sizeof(long) - (i+1))*BYTE_BIT_CNT) 
				 >> (sizeof(long) - 1)*BYTE_BIT_CNT;
    }
  if ( in < 0 ) out[0] |= 0x0080;
}


void set_bytes_u(unsigned long in, int numbytes, unsigned char *out)
{
  int i;

  for (i=0; i < numbytes; i++)
    {
      out[numbytes-1-i] = (in << (sizeof(unsigned long) - (i+1))*BYTE_BIT_CNT) 
				 >> (sizeof(unsigned long) - 1)*BYTE_BIT_CNT;
    }
}
