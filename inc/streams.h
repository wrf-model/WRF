#ifndef MAX_HISTORY
# define MAX_HISTORY 12
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

