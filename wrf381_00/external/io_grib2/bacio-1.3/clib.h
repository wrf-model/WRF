/*     Include file to define variables for Fortran to C interface(s) */
/*     Robert Grumbine 16 March 1998                  */
/*     NOSEEK added 25 March 1998                  */
/*     CRAY compatibility added 20 April 1998      */

/* The following line should be either undef or define VERBOSE */
/* The latter gives noisy debugging output, while the former */
/*   relies solely on the return codes */
#undef  VERBOSE

/* Declare the system type, supported options are: */
/* LINUX, SGI, HP, CRAY90, IBM4, IBM8, LINUXF90 */
#define IBM4


#include <stdlib.h>

/* Do not change things below here yourself */

/*     IO-related (bacio.c, banio.c) */
#define BAOPEN_RONLY              1
#define BAOPEN_WONLY              2
#define BAOPEN_RW                 4
#define BACLOSE                   8
#define BAREAD                   16
#define BAWRITE                  32
#define NOSEEK                   64
#define BAOPEN_WONLY_TRUNC      128
#define BAOPEN_WONLY_APPEND     256

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define BACIO bacio
# else
#   ifdef F2CSTYLE
#      define BACIO bacio__
#      define BANIO banio__
#   else
#      define BACIO bacio_
#      define BANIO banio_
#   endif
# endif
#endif
