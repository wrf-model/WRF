#ifndef MAX_HISTORY
# include <stdint.h>
# define MAX_HISTORY (UINT8_C(12))
# if (MAX_HISTORY > 120)
#  warning If changing MAX_HISTORY to be above 120, check uses, loop variables,
#  warning and destination string buffers to ensure the types used are wide
#  warning enough.  Enabling compiler warnings for format strings should help.
# endif
#endif
#ifndef IWORDSIZE
# define IWORDSIZE 4
#endif
#define HISTORY_STREAM         0
#define INPUT_STREAM           ((HISTORY_STREAM)+(MAX_HISTORY))
#if 0
  max streams is MAX_HISTORY plus equal number of input streams plus 1 restart + 1 boundary
#endif
#define MAX_STREAMS  (2*(MAX_HISTORY)+2)
#define BOUNDARY_STREAM (2*(MAX_HISTORY)+1)
#define RESTART_STREAM (2*(MAX_HISTORY)+2)
#define IO_MASK_SIZE ((MAX_STREAMS)/(IWORDSIZE*8)+1)

