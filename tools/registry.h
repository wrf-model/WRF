#ifndef REGISTRY_H
#define NAMELEN 512
#define NAMELEN_LONG 8192
#define MAXDIMS 21
#define MAX_DYNCORES 50   /* ha ha, just kidding */
/* #define MAX_ARGLINE 175    WRF uses 128 by default, but the nested chem version hit the continuation line limit for efc so it had to be increased, wig 14-Oct-2004 */
#define MAX_ARGLINE 128   /* welp, 175 means lines longer than 130 chars, which is a Fortran no no */
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
#define SWAP    1024
#define CYCLE   2048
#define XPOSE   4096
#define FOURD1  8192
#define BDYONLY 16384

/* I/O mask settings                     bit  */
#define HISTORY       0x00000002      /*    1 */
#define AUXHIST1      0x00000004      /*    2 */
#define AUXHIST2      0x00000008      /*    3 */
#define AUXHIST3      0x00000010      /*    4 */
#define AUXHIST4      0x00000020      /*    5 */
#define AUXHIST5      0x00000040      /*    6 */
#define AUXHIST6      0x00000080      /*    7 */
#define AUXHIST7      0x00000100      /*    8 */
#define AUXHIST8      0x00000200      /*    9 */
#define AUXHIST9      0x00000400      /*    0 */
#define AUXHIST10     0x00000800      /*   11 */
#define AUXHIST11     0x00001000      /*   12 */
#define INPUT         0x00002000      /*   13 */
#define AUXINPUT1     0x00004000      /*   14 */
#define AUXINPUT2     0x00008000      /*   15 */
#define AUXINPUT3     0x00010000      /*   16 */
#define AUXINPUT4     0x00020000      /*   17 */
#define AUXINPUT5     0x00040000      /*   18 */
#define AUXINPUT6     0x00080000      /*   19 */
#define AUXINPUT7     0x00100000      /*   10 */
#define AUXINPUT8     0x00200000      /*   21 */
#define AUXINPUT9     0x00400000      /*   22 */
#define AUXINPUT10    0x00800000      /*   23 */
#define AUXINPUT11    0x01000000      /*   24 */
#define RESTART       0x02000000      /*   25 */
#define BOUNDARY      0x04000000      /*   26 */
#define INTERP_DOWN   0x08000000      /*   27 */
#define FORCE_DOWN    0x10000000      /*   28 */
#define INTERP_UP     0x20000000      /*   29 */
#define SMOOTH_UP     0x40000000      /*   20 */
#define METADATA      0x80000000      /*   31 */


#define REGISTRY_H
#endif

