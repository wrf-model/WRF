#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if !(defined(MACOS) || defined(IRIX))
#include <malloc.h>
#endif

#ifdef NOUNDERSCORE
void da_memory(
#else
#ifdef F2CSTYLE
void da_memory__(
#else
void da_memory_(
#endif
#endif

  memory_used)

  int *memory_used;
{

#if !(defined(vpp) || defined(vpp2) || defined(SUN) || defined(crayx1) || defined(MACOS)) || defined(IRIX)
struct mallinfo result;

result=mallinfo();
/* return memory in kbytes, both for smaller numbers, and to avoid going outside integer*4 range */
*memory_used=result.uordblks/1024;
#else
  *memory_used=0;
#endif
}
