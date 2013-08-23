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
#ifdef UNDERSCORE
#define bort	   bort_
#define bort_exit  bort_exit_
#define cadn30	   cadn30_
#define ccbfl	   ccbfl_
#define cmpia	   cmpia_
#define cobfl	   cobfl_
#define crbmg	   crbmg_
#define cwbmg	   cwbmg_
#define elemdx	   elemdx_
#define gets1loc   gets1loc_
#define ichkstr	   ichkstr_
#define icvidx	   icvidx_
#define ifxy	   ifxy_
#define igetntbi   igetntbi_
#define igettdi	   igettdi_
#define ipkm	   ipkm_
#define istdesc	   istdesc_
#define iupbs01	   iupbs01_
#define iupm	   iupm_
#define mstabs	   mstabs_
#define nemtab	   nemtab_
#define nemtbb	   nemtbb_
#define nummtb	   nummtb_
#define numtbd	   numtbd_
#define pktdd	   pktdd_
#define rbytes	   rbytes_
#define restd	   restd_
#define stntbi	   stntbi_
#define strnum	   strnum_
#define stseq	   stseq_
#define uptdd	   uptdd_
#define wrdesc	   wrdesc_
#define wrdlen	   wrdlen_
#define openrb     openrb_
#define openwb     openwb_
#define openab     openab_
#define backbufr   backbufr_
#define cewind     cewind_
#define closfb     closfb_
#define crdbufr    crdbufr_
#define cwrbufr    cwrbufr_
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
int cmpia( const f77int *, const f77int * );
void cobfl( char *, char * );
void crbmg( char *, f77int *, f77int *, f77int * );
void cwbmg( char *, f77int *, f77int * );
void elemdx( char *, f77int *, f77int );
void gets1loc( char *, f77int *, f77int *, f77int *, f77int *, f77int );
f77int ichkstr ( char *, char *, f77int *, f77int, f77int );
f77int ifxy( char *, f77int );
f77int igetntbi( f77int *, char *, f77int );
f77int igettdi( f77int * );
void ipkm( char *, f77int *, f77int *, f77int );
f77int istdesc( f77int * );
f77int iupbs01 ( f77int *, char *, f77int );
f77int iupm ( char *, f77int *, f77int );
void nemtab( f77int *, char *, f77int *, char *, f77int *, f77int, f77int );
void nemtbb( f77int *, f77int *, char *, f77int *, f77int *, f77int *, f77int );
void nummtb( f77int *, char *, f77int * );
void numtbd( f77int *, f77int *, char *, char *, f77int *, f77int, f77int );
void pktdd( f77int *, f77int *, f77int *, f77int * );
f77int rbytes( char *, f77int *, f77int, f77int );
void restd( f77int *, f77int *, f77int *, f77int * );
void strnum( char *, f77int *, f77int );
void stseq( f77int *, f77int *, f77int *, char *, char *, f77int *, f77int * );
void uptdd( f77int *, f77int *, f77int *, f77int * );
void wrdesc( f77int, f77int *, f77int * );
void wrdlen( void );

/*
** The following parameters must also be identically defined within
** "bufrlib.PRM" for use by several FORTRAN routines.  See "bufrlib.PRM"
** for a description of these parameters.
*/
#define MAXNC	300
#define MXMTBB	4000
#define MXMTBD	1000
#define MAXCD	250
#define MXNAF	3
#define NFILES	32

/*
** Enable access to FORTRAN COMMON block /MSTABS/ from within C.
*/
#ifdef COMMON_MSTABS
    extern struct {
	f77int nmtb; f77int ibfxyn[MXMTBB];     char cbscl[MXMTBB][4];
		     char   cbsref[MXMTBB][12]; char cbbw[MXMTBB][4];
		     char   cbunit[MXMTBB][14]; char cbmnem[MXMTBB][8];
		     char   cbelem[MXMTBB][120];
	f77int nmtd; f77int idfxyn[MXMTBD];     char   cdseq[MXMTBD][120];
		     char   cdmnem[MXMTBD][8];  f77int ndelem[MXMTBD];
		     f77int idefxy[MXMTBD*MAXCD];
		     char   cdelem[MXMTBD*MAXCD][120];
    } mstabs;
#endif
