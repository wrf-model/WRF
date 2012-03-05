#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
** Define a global variable for sharing of file pointers across different
** subprograms within the BUFRLIB software.
*/
#ifdef BUFRLIB_GLOBAL
    FILE *pbf[2];  /* each element will automatically initialize to NULL */
#else
    extern FILE *pbf[2];
#endif

/*
** On certain operating systems, the FORTRAN compiler appends an underscore
** to subprogram names in its object namespace.  Therefore, on such systems,
** a matching underscore must be appended to any C language references to the
** same subprogram names so that the linker can correctly resolve such
** references across the C <-> FORTRAN interface at link time.
*/
#ifndef CRAY
# ifdef NOUNDERSCORE
#  define bort	     bort
#  define bort_exit  bort_exit
#  define cadn30     cadn30
#  define ccbfl	     ccbfl
#  define cobfl	     cobfl
#  define crbmg	     crbmg
#  define cwbmg	     cwbmg
#  define gets1loc   gets1loc
#  define ichkstr    ichkstr
#  define ifxy	     ifxy
#  define ipkm	     ipkm
#  define istdesc    istdesc
#  define iupbs01    iupbs01
#  define iupm	     iupm
#  define nemtbb     nemtbb
#  define numtbd     numtbd
#  define rbytes     rbytes
#  define restd	     restd
#  define uptdd	     uptdd
#  define wrdesc     wrdesc
#  define wrdlen     wrdlen
# else
#  ifdef F2CSTYLE
#  define bort	     bort_
#  define bort_exit  bort_exit__
#  define cadn30     cadn30_
#  define ccbfl	     ccbfl_
#  define cobfl	     cobfl_
#  define crbmg	     crbmg_
#  define cwbmg	     cwbmg_
#  define gets1loc   gets1loc_
#  define ichkstr    ichkstr_
#  define ifxy	     ifxy_
#  define ipkm	     ipkm_
#  define istdesc    istdesc_
#  define iupbs01    iupbs01_
#  define iupm	     iupm_
#  define nemtbb     nemtbb_
#  define numtbd     numtbd_
#  define rbytes     rbytes_
#  define restd	     restd_
#  define uptdd	     uptdd_
#  define wrdesc     wrdesc_
#  define wrdlen     wrdlen_
#  else
#  define bort	     bort_
#  define bort_exit  bort_exit_
#  define cadn30     cadn30_
#  define ccbfl	     ccbfl_
#  define cobfl	     cobfl_
#  define crbmg	     crbmg_
#  define cwbmg	     cwbmg_
#  define gets1loc   gets1loc_
#  define ichkstr    ichkstr_
#  define ifxy	     ifxy_
#  define ipkm	     ipkm_
#  define istdesc    istdesc_
#  define iupbs01    iupbs01_
#  define iupm	     iupm_
#  define nemtbb     nemtbb_
#  define numtbd     numtbd_
#  define rbytes     rbytes_
#  define restd	     restd_
#  define uptdd	     uptdd_
#  define wrdesc     wrdesc_
#  define wrdlen     wrdlen_
#  endif
# endif
#endif

/*
** In order to ensure that the C <-> FORTRAN interface works properly (and
** portably!), the default size of an "INTEGER" declared in FORTRAN must be 
** identical to that of an "int" declared in C.  If this is not the case (e.g.
** some FORTRAN compilers, most notably AIX via the -qintsize= option, allow the
** sizes of INTEGERs to be definitively prescribed outside of the source code
** itself!), then the following conditional directive (or a variant of it) can
** be used to ensure that the size of an "int" in C remains identical to that
** of an "INTEGER" in FORTRAN.
*/ 
#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif

/*
** Declare prototypes for ANSI C compatibility.
*/
void bort( char *, f77int );
void bort_exit( void );
void cadn30( f77int *, char *, f77int ); 
void ccbfl( void );
void cobfl( char *, char * );
void crbmg( char *, f77int *, f77int *, f77int * );
void cwbmg( char *, f77int *, f77int * );
void gets1loc( char *, f77int *, f77int *, f77int *, f77int *, f77int );
f77int ichkstr ( char *, char *, f77int *, f77int, f77int );
f77int ifxy( char *, f77int );
void ipkm( char *, f77int *, f77int *, f77int );
f77int istdesc( f77int * );
f77int iupbs01 ( f77int *, char *, f77int );
f77int iupm ( char *, f77int *, f77int );
void nemtbb( f77int *, f77int *, char *, f77int *, f77int *, f77int *, f77int );
void numtbd( f77int *, f77int *, char *, char *, f77int *, f77int, f77int );
f77int rbytes( char *, f77int *, f77int, f77int );
void restd( f77int *, f77int *, f77int *, f77int * );
void uptdd( f77int *, f77int *, f77int *, f77int * );
void wrdesc( f77int, f77int *, f77int * );
void wrdlen( void );

/*
** Define the maximum number of Section 3 FXY descriptors that can be written
** into a BUFR message by the BUFRLIB software.  Note that this parameter must
** also be identically defined within "bufrlib.prm" for use by several FORTRAN
** routines.
*/
#define MAXNC	300
