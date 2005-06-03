#ifndef REGISTRY_H
#define NAMELEN 8092
#define MAXDIMS 21
#define MAX_DYNCORES 50   /* ha ha, just kidding */
#define MAX_ARGLINE 128
#define MAX_TYPEDEFS 50   /* typedef history -ajb */

/* defines of system commands */
#define UNIQSORT "/bin/sort -u"
#define CATCOMM  "/bin/cat"
#define RMCOMM   "/bin/rm"
#define MVCOMM   "/bin/mv"

#define DRIVER_LAYER     100
#define MEDIATION_LAYER  200

enum coord_axis      { COORD_X , COORD_Y , COORD_Z , COORD_C } ;
enum len_defined_how { DOMAIN_STANDARD , NAMELIST , CONSTANT } ;
enum type_type       { SIMPLE , DERIVED } ;
enum proc_orient     { ALL_Z_ON_PROC , ALL_X_ON_PROC , ALL_Y_ON_PROC } ;

/* node_kind  mask settings */
#define FIELD      1
#define I1         2
#define RCONFIG    4
#define FOURD      8
#define MEMBER    16
#define TYPE      32
#define DIM       64
#define PACKAGE  128
#define HALO     256
#define PERIOD   512
#define XPOSE   1024

/* I/O mask settings */
#define HISTORY          1
#define AUXHIST1         2
#define AUXHIST2         4
#define AUXHIST3         8
#define AUXHIST4        16
#define AUXHIST5        32
#define INPUT           64
#define AUXINPUT1      128
#define AUXINPUT2      256
#define AUXINPUT3      512
#define AUXINPUT4     1024
#define AUXINPUT5     2048
#define RESTART       4096
#define BOUNDARY      8192
#define INTERP_DOWN   16384
#define FORCE_DOWN (16384*2)
#define INTERP_UP  (16384*4)
#define SMOOTH_UP  (16384*8)
#define METADATA   (16384*16)

#define REGISTRY_H
#endif

