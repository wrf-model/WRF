/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/


#ifndef RSL_H
#define RSL_H

/**************************************************************/
/* Any changes to this file should be followed by make clean  */
/* before remaking the RSL library                            */
/**************************************************************/

/**************************************************************/
/* not likely that you will need to change things below here */

/* this definition used in cd.c */
#define HARD_CODED_BOUNDARY_WIDTH_FIX_ME_PLEASE 5

#define RSL_MAXDOMAINS  MAXDOM_MAKE	/* FORTRAN */

#define RSL_MAXPROC     MAXPROC_MAKE	/* FORTRAN */

/* increased from 5 to 7 10/4/96 to accomodate 168 pt stencil */
/* decreased to 3 (2 + 1 for garbage row) to save memory -- only estore
   in sound needs 168 pt stencil and we'll handle that as a special case */
#define RSL_DEFAULT_PADAREA 4           /* FORTRAN */

#define RSL_MLOW        1		/* FORTRAN */
#define RSL_MHIGH       2               /* FORTRAN */
#define RSL_NLOW        3               /* FORTRAN */
#define RSL_NHIGH       4               /* FORTRAN */
#define RSL_DBDY	5		/* FORTRAN */
#define RSL_CLOSEST     6		/* FORTRAN */

#define DOT_BDY_INFO_LEN 10		/* FORTRAN */

#define RSL_MLOW_X      7		/* FORTRAN */
#define RSL_MHIGH_X     8               /* FORTRAN */
#define RSL_NLOW_X      9               /* FORTRAN */
#define RSL_NHIGH_X    10               /* FORTRAN */
#define RSL_DBDY_X     11		/* FORTRAN */
#define RSL_CLOSEST_X  12		/* FORTRAN */

#define CROSS_BDY_INFO_LEN 12 		/* FORTRAN */

#define RSL_00	        7		/* FORTRAN */
#define RSL_M0	        8		/* FORTRAN */
#define RSL_0N	        9		/* FORTRAN */
#define RSL_MN	       10		/* FORTRAN */

#define MLOW (RSL_MLOW-1)
#define MHIGH (RSL_MHIGH-1)
#define NLOW (RSL_NLOW-1)
#define NHIGH (RSL_NHIGH-1)

#define MAX_RUNPAD  6
#define MAX_KINDPAD 8
/* break ties in boundary classification */
#define M_WINS      0                   /* FORTRAN */
#define N_WINS      1                   /* FORTRAN */
#define DIAG_WINS  2                   /* FORTRAN */

#ifdef SHOW_RSL_CONFIGURATION
/* SHOW_RSL_CONFIGURATION is only every defined externally.  This
   next bit is never actually used.  Rather it is used by the top
   level makefile to show the configuration of the model when the
   user types make showconfig.  */
show config:  ---if the library were completely remade---
show config:"RSL_IMAX" would be set to:            RSL_IMAX
show config:"RSL_JMAX" would be set to:            RSL_JMAX
show config:"RSL_MAXDOMAINS" would be set to:      RSL_MAXDOMAINS
show config:"RSL_MAXPROC" would be set to:         RSL_MAXPROC
show config:  ---See also the makefile---
#endif


/* this should become runtime setaable from the fortran someday */

#define RSL_TRUE  1			/* FORTRAN */
#define RSL_FALSE 0			/* FORTRAN */
#define RSL_INVALID -1			/* FORTRAN */
#define RSL_VALID 1			/* FORTRAN */

#define RSL_IRAX 3			/* FORTRAN */
#define RSL_MAXCHILDREN (RSL_IRAX*RSL_IRAX)
#define RSL_MAXKIDS RSL_MAXCHILDREN
#define RSL_MAXDESCRIPTORS  2048
/* settings below will allow for 20000 fields */
#define MAX_BASE_TABLE_ENTRIES 20000

#if (ALLOW_RSL_168PT == 1)
#  define RSL_MAXSTEN    168              /* MAX NUMBER OF STENCIL PTS */
#else
#  define RSL_MAXSTEN	 24		/* MAX NUMBER OF STENCIL PTS */
#endif


#define RSL_REAL		0	/* FORTRAN */
#define RSL_DOUBLE		1	/* FORTRAN */
#define RSL_COMPLEX		2	/* FORTRAN */
#define RSL_INTEGER		3	/* FORTRAN */
#define RSL_CHARACTER		4	/* FORTRAN */

#define RSL_REAL_F90		100	/* FORTRAN */
#define RSL_DOUBLE_F90		101	/* FORTRAN */
#define RSL_COMPLEX_F90		102	/* FORTRAN */
#define RSL_INTEGER_F90		103	/* FORTRAN */
#define RSL_CHARACTER_F90	104	/* FORTRAN */

/* traversal orders */
/* A = Ascending, D = Descending, applied to dim in order left to right */
#define MINMAJ_AA                0       /* FORTRAN */
#define MINMAJ_AD                1       /* FORTRAN */
#define MINMAJ_DA                2       /* FORTRAN */
#define MINMAJ_DD                3       /* FORTRAN */
#define MAJMIN_AA             1000       /* FORTRAN */
#define MAJMIN_AD             1001       /* FORTRAN */
#define MAJMIN_DA             1002       /* FORTRAN */
#define MAJMIN_DD             1003       /* FORTRAN */
#define MINMAJ           MINMAJ_AA       /* FORTRAN */
#define MAJMIN           MAJMIN_AA       /* FORTRAN */

#define MNMJ(X)     ( ( (X) / 1000 ) == 0 )
#define D1(X)       ( 2 & (X) )
#define D2(X)       ( 1 & (X) )
#define A1(X)       ( ! A1 )
#define A2(X)       ( ! A2 )

/* nested domain shapes */
#define RSL_REGULAR_NEST  55		/* FORTRAN */
#define RSL_RAGGED_NEST  56		/* FORTRAN */

/* packing strategies */
#define MINNS_MAJEW_2D   0
#define MINEW_MAJNS_2D   1
#define MINNS_MAJEW_K_3D 2
#define MINEW_MAJNS_K_3D 3
#define K_MIDNS_MAJEW_3D 4
#define MINNS_K_MAJEW_3D 5

/* io strategies */

#define IO_REPL  99		/* FORTRAN */

#define IO2D_IJ  0		/* FORTRAN */
#define IO2D_JI  1		/* FORTRAN */
#define IO3D_IJK 2		/* FORTRAN */
#define IO3D_JIK 3		/* FORTRAN */
#define IO2D     4		/* FORTRAN */
#define IO3D     5		/* FORTRAN */
#define IO3D_KIJ 6              /* FORTRAN */
#define IO3D_IKJ 7              /* FORTRAN */

#define IO2D_IJ_RAW  10		/* FORTRAN */
#define IO2D_JI_RAW  11		/* FORTRAN */
#define IO3D_IJK_RAW 12		/* FORTRAN */
#define IO3D_JIK_RAW 13		/* FORTRAN */

#define IO2D_IJ_PORTAL  20	/* FORTRAN */
#define IO2D_JI_PORTAL  21	/* FORTRAN */
#define IO3D_IJK_PORTAL 22	/* FORTRAN */
#define IO3D_JIK_PORTAL 23	/* FORTRAN */

#define IO2D_IJ_INTERNAL  24	/* FORTRAN */
#define IO2D_JI_INTERNAL  25	/* FORTRAN */
#define IO3D_IJK_INTERNAL 26	/* FORTRAN */
#define IO3D_JIK_INTERNAL 27	/* FORTRAN */
#define IO3D_KIJ_INTERNAL 28	/* FORTRAN */
#define IO3D_IKJ_INTERNAL 29	/* FORTRAN */

#define IO2D_IJ_88  30	/* FORTRAN */
#define IO2D_JI_88  31	/* FORTRAN */
#define IO3D_IJK_88 32	/* FORTRAN */
#define IO3D_JIK_88 33	/* FORTRAN */

#define RSL_MAXDIM 3

/* type declarations */

typedef int * int_p ;
#if !(defined(SUNDEBUG) || defined(crayx1))
typedef short   rsl_processor_t ;
typedef short   rsl_index_t ;
typedef short   rsl_dimlen_t ;
#else
typedef int   rsl_processor_t ;
typedef int   rsl_index_t ;
typedef int   rsl_dimlen_t ;
#endif
typedef long    rsl_point_id_t ;

#ifdef crayx1
typedef int rsl_tag_t ;
#else
typedef unsigned char rsl_tag_t ;
#endif

typedef struct rsl_list {
  struct rsl_list * next ;
  void * data ;	                /* pointer to some node */
#ifdef crayx1
  int info1 ;                   /* blank info field */
  int info2 ;                   /* blank info field */
#else
  short info1 ;                 /* blank info field */
  short info2 ;                 /* blank info field */
#endif
} rsl_list_t ;


typedef struct rsl_runrec {	/* added 1/9/95 for rsl_compute_islab */
  int i, j, ig, jg, runlength ;
} rsl_runrec_t ;

typedef struct bcast_point_desc {
  rsl_point_id_t nest_id ;	/* pt in nest */
  rsl_point_id_t parent_id ;	/* cd pt */
  unsigned char cn, cm ;	/* indices of point, with respect to cd pt */
} bcast_point_desc_t ;

typedef bcast_point_desc_t merge_point_desc_t ;

typedef struct rsl_fldspec {
  struct rsl_fldspec    * next ;
  void                  * base ;
  rsl_tag_t             ndim ;
  rsl_tag_t             elemsz ;
  rsl_tag_t             memsize ;
  rsl_tag_t             type ;
  int                   f90_table_index ;
  rsl_tag_t             strategy ;
  rsl_tag_t             decomp[ RSL_MAXDIM ] ;
  rsl_index_t           gdex[ RSL_MAXDIM ] ;
  rsl_dimlen_t          glen[ RSL_MAXDIM ] ;
  rsl_dimlen_t          llen[ RSL_MAXDIM ] ;
  rsl_dimlen_t          stag[ RSL_MAXDIM ] ;  /* 0 = not staggered; 1 = staggered */
} rsl_fldspec_t ;

typedef struct message_desc {
  rsl_tag_t tag;
    /* should be MESSAGE_DESC or BLANK_MESSAGE_DESC */
  int mh ;              /* handle */
  int nflds ;           /* number of field specs in message */
  int nbytes ;          /* for blank messages */
  rsl_fldspec_t *fldspecs ;
} message_desc_t ;

typedef struct rsl_plist_elem {
  struct rsl_plist_elem *next ;
  rsl_point_id_t id ;
  rsl_processor_t P ;
} rsl_plist_elem_t ;

typedef struct rsl_child_info {
  rsl_point_id_t  child[RSL_MAXCHILDREN] ;
  rsl_processor_t P[RSL_MAXCHILDREN] ;
} rsl_child_info_t ;

typedef struct rsl_point {
  rsl_processor_t   P ;		/* physical processor number */
  int               valid ;	/* a valid point? */
  rsl_tag_t         trimmed ;	/* was this point trimmed */
  rsl_tag_t         cross ;     /* member of the cross grid */
  rsl_tag_t	    info_1 ;	/* misc tag info */
  rsl_tag_t	    info_2 ;	/* misc tag info */
  rsl_point_id_t    id ;
  rsl_point_id_t    mother_id ;
  rsl_processor_t   mother_P ;
  rsl_tag_t         which_kid_am_i_m ;
  rsl_tag_t         which_kid_am_i_n ;

/* on whole grid */

 			/* counter-clockwise and clockwise below refer
			   to the direction of the traversal in fill_boundary
			   that determined the boundary association... 
			   we store both to allow for several resolution
			   strategies on corners.  */

/* dot grid */
  rsl_index_t       bdy_cclockwise ;	/* direction to boundary (counter) */
  rsl_index_t       bdy_clockwise ;	/* direction to boundary (clockwise) */
  rsl_index_t       dbdy ;		/* distance to boundary */

			/* these next four fields are computed differently,
			   in that they do not take into account corners.
			   They simply measure the distance from the point
			   to the boundary in question. */
  rsl_index_t       dist_mlow ;		/* distance to southern bdy */
  rsl_index_t       dist_mhigh ;	/*     "    to northern bdy */
  rsl_index_t       dist_nlow ;         /*     "    to western bdy  */
  rsl_index_t       dist_nhigh ;        /*     "    to eastern bdy  */

/* on cross grid */
/* cross grid */
  rsl_index_t       bdy_x_cclockwise ;	/* direction to boundary (counter) */
  rsl_index_t       bdy_x_clockwise ;	/* direction to boundary (clockwise) */
  rsl_index_t       dbdy_x ;		/* distance to boundary */

  rsl_index_t       dist_mlow_x ;	/* distance to southern bdy */
  rsl_index_t       dist_mhigh_x ;	/*     "    to northern bdy */
  rsl_index_t       dist_nlow_x ;       /*     "    to western bdy  */
  rsl_index_t       dist_nhigh_x ;      /*     "    to eastern bdy  */

  rsl_child_info_t *children_p ;
} rsl_point_t ;

typedef struct rsl_hemi_rec {
  int oig , ojg ;
  int nbytes ;
  int curs ;
  char * data ;
  struct rsl_hemi_rec * next ;
} rsl_hemi_rec_t ;

typedef struct rsl_domain_info {
  int valid ;
  int decomposed ;

  /* parent domain descriptor */
  int parent ;
  int nestshape ;	/* RSL_REGULAR_NEST, RSL_RAGGED_NEST */

  /* information about parent, children */
  int parent_bcast_compiled ;
  int parent_merge_compiled ;
  int child_bcast_compiled[RSL_MAXDOMAINS] ;
  int child_merge_compiled[RSL_MAXDOMAINS] ;

  /* pointer to MN domain data structure (array of points) */
  rsl_point_t     *domain ;

  /* pointer to MZ domain data structure (array of points) (20010222) */
  rsl_point_t     *domain_mz ;

  /* pointer to NZ domain data structure (array of points) (20010222) */
  rsl_point_t     *domain_nz ;

/* MN decomp */
  /* dimensions of the global domain data structure */
  rsl_dimlen_t    len_m ;
  rsl_dimlen_t    len_n ;
  rsl_dimlen_t    eff_m ;
  rsl_dimlen_t    eff_n ;
  rsl_dimlen_t    loc_m ;
  rsl_dimlen_t    loc_n ;
  /* dimensions of the global domain data structure (20010222) */
  rsl_dimlen_t    len_z ;
  rsl_dimlen_t    eff_z ;
  rsl_dimlen_t    loc_z ;
/* MZ decomp (20010223) */
  /* dimensions of the global domain data structure */
  rsl_dimlen_t    len_mz_m ;
  rsl_dimlen_t    len_mz_n ;
  rsl_dimlen_t    eff_mz_m ;
  rsl_dimlen_t    eff_mz_n ;
  rsl_dimlen_t    loc_mz_m ;
  rsl_dimlen_t    loc_mz_n ;
  rsl_dimlen_t    len_mz_z ;
  rsl_dimlen_t    eff_mz_z ;
  rsl_dimlen_t    loc_mz_z ;
/* NZ decomp  (20010223 */
  /* dimensions of the global domain data structure */
  rsl_dimlen_t    len_nz_m ;
  rsl_dimlen_t    len_nz_n ;
  rsl_dimlen_t    eff_nz_m ;
  rsl_dimlen_t    eff_nz_n ;
  rsl_dimlen_t    loc_nz_m ;
  rsl_dimlen_t    loc_nz_n ;
  rsl_dimlen_t    len_nz_z ;
  rsl_dimlen_t    eff_nz_z ;
  rsl_dimlen_t    loc_nz_z ;

  /* list of stencils used on this domain  (just keep handles) */
  int stenlist[RSL_MAXDESCRIPTORS] ;
  int stencurs ;
  int periodlist[RSL_MAXDESCRIPTORS] ;
  int periodcurs ;
  int xposelist[RSL_MAXDESCRIPTORS] ;
  int xposecurs ;

  message_desc_t *old_state_vect, *new_state_vect ;

  /* coordinates of the domain in its parent (sort of) */
  rsl_index_t     coord_m ;
  rsl_index_t     coord_n ;

  /* nesting ratio in parent domain */
  int irax_m ;
  int irax_n ;

  /* trimming constants -- these are used to tell rsl how much shorter
     in a dimension the effective range of a nest is than the declared
     range -- it might be declared longer because it has to be a 
     multiple of irax_[mn] */
  int trim_m ;
  int trim_n ;

  rsl_index_t  nest_level ;     /* 0 is mother domain */
  rsl_list_t   *pts ;	 	/* list of points on this processor */
  rsl_list_t   *ghost_pts ;	/* list of points communicating with us */
  rsl_list_t   *iruns ;		/* list of runs in idimension 1/9/95 */
  rsl_index_t  ilocaloffset ;	/* ns offset global to local in domain ds */
  rsl_index_t  jlocaloffset ;   /* ew offset global to local in domain ds */

  rsl_index_t  ilocaloffset_mz ;
  rsl_index_t  jlocaloffset_mz ;
  rsl_index_t  klocaloffset_mz ;

  rsl_index_t  ilocaloffset_nz ;
  rsl_index_t  jlocaloffset_nz ;
  rsl_index_t  klocaloffset_nz ;

  rsl_index_t  old_ilocaloffset ;
  rsl_index_t  old_jlocaloffset ;
  int          maskid  ;	/* maximum stencil specified for this domain */

/* added for RSL_COMPUTE -- in comp_world.c */
/* these are worked out in rsl_new_decomp.c */
  int    idif, jdif ;
  /* these are for iterating with j-major */
  int    nrun[MAX_KINDPAD+1];
  int    *(js[MAX_KINDPAD+1]) ;
  int    *(is[MAX_KINDPAD+1]), *(ie[MAX_KINDPAD+1]), *(jg2n[MAX_KINDPAD+1]) ;
  /* these are for iterating with i-major */
  int    nruni[MAX_KINDPAD+1];
  int    *(is2[MAX_KINDPAD+1]) ;
  int    *(js2[MAX_KINDPAD+1]), *(je2[MAX_KINDPAD+1]), *(ig2n[MAX_KINDPAD+1]) ;

  rsl_processor_t bcast_recv_Pnpts[RSL_MAXPROC] ;
  rsl_processor_t bcast_recv_Plist[RSL_MAXPROC] ;
  rsl_processor_t bcast_recv_Ptags[RSL_MAXPROC] ;
  int Nbcast_recv_Plist ;
  rsl_processor_t bcast_send_Pnpts[RSL_MAXPROC] ;
  rsl_processor_t bcast_send_Plist[RSL_MAXPROC] ;
  int Nbcast_send_Plist ;

  rsl_list_t *bcast_Xlist ;


  rsl_processor_t merge_recv_Pnpts[RSL_MAXPROC] ;
  rsl_processor_t merge_recv_Plist[RSL_MAXPROC] ;
  rsl_processor_t merge_recv_Ptags[RSL_MAXPROC] ;
  int Nmerge_recv_Plist ;
  rsl_processor_t merge_send_Pnpts[RSL_MAXPROC] ;
  rsl_processor_t merge_send_Plist[RSL_MAXPROC] ;
  int Nmerge_send_Plist ;

  rsl_list_t *merge_Xlist ;


  int other_hemi_proclist_built ;
  rsl_hemi_rec_t * other_hemi_procbufs[RSL_MAXPROC] ;
  int hemi_sendPlist[RSL_MAXPROC] ;
  int hemi_recvPlist[RSL_MAXPROC] ;
  int hemi_recv_tags[RSL_MAXPROC] ;

  int is_write, is_read, ie_write, ie_read ;
  int js_write, js_read, je_write, je_read ;

} rsl_domain_info_t ;


/* March 1998 -- structure for new packing strategy */
#ifdef crayx1
struct packrec_struct {
    void * base ;
    int offset ;
    int n ;             /* number of bytes in an element */
    int nelems ;        /* number of elements */
    int stride ;        /* number of bytes between each element */
    int f90_table_index ;
    int endstop ;
    int valid ;
    int curs ;          /* position of data in pack buf */
} ;
typedef struct packrec_struct packrec_t ;

#else
struct packrec_struct {
  void * base ;
  int offset ;
  int n ;		/* number of bytes in an element */
  int nelems ;		/* number of elements */
  int stride ;		/* number of bytes between each element */
  int f90_table_index ;
  int endstop ;
  int valid ;
} ;
typedef struct packrec_struct packrec_t ;
#endif

typedef struct rsl_procrec {
  struct rsl_procrec * next ;
  rsl_processor_t P ;
  int sendsize ;	/* size of send buf needed for this processor */
  int recvsize ;	/* size of recv buf needed for this processor */
  int npts ;
  int recv_npts ;
  int nsends ;		/* diagnostic -- keeps running total number of sends */
  int nrecvs ;		/* diagnostic -- keeps running total number of sends */
  rsl_list_t *point_list ;
/* Mar 1998 */
  rsl_list_t *recv_point_list ;
  packrec_t * pack_table ;
  int pack_table_size ;
  int pack_table_nbytes ;
  packrec_t * unpack_table ;
  int unpack_table_size ;
  int unpack_table_nbytes ;
} rsl_procrec_t ;

typedef struct rsl_ptrec {
  struct rsl_ptrec * next ;
  rsl_point_t *pt ;
  rsl_index_t ig, jg ;
  rsl_list_t  *send_messages ;
  rsl_list_t  *recv_messages ;
#ifdef crayx1
  int nsendmsgs ;
  int nrecvmsgs ;
#else
  short nsendmsgs ;
  short nrecvmsgs ;
#endif
} rsl_ptrec_t ;

typedef struct rsl_point_hdr {
  rsl_index_t d, ig, jg, dummy ;   /* dummy maintains alignment */
  int nmsgs ;
} rsl_point_hdr_t ;

typedef struct rsl_message_hdr {  /* packet hdr for message */
  int sp ;
} rsl_message_hdr_t ;

typedef struct rsl_fld_hdr {	/* packet hdr for fld */
  void * base ;                 /* base address; byte pointer arithmetic */
#ifdef crayx1
  int len ;                     /* number of bytes to follow */
#else
  short len ;                   /* number of bytes to follow */
#endif
} rsl_fld_hdr_t ;

/* global data */

#ifdef DEFINE_GLOBAL
#   define EXTERN
#else
#   define EXTERN extern
#endif

EXTERN rsl_domain_info_t  domain_info[RSL_MAXDOMAINS] ;
/* message descriptors */
EXTERN void * mh_descriptors[RSL_MAXDESCRIPTORS] ;
/* stencil descriptors */
EXTERN void * sh_descriptors[RSL_MAXDESCRIPTORS] ;
/* xpose descriptors */
EXTERN void * xp_descriptors[RSL_MAXDESCRIPTORS] ;
/* period descriptors */
EXTERN void * pr_descriptors[RSL_MAXDESCRIPTORS] ;
EXTERN int rsl_ndomains  ; /* number of active domains */
EXTERN int rsl_nproc     ; /* number of compute processors */
EXTERN int rsl_nproc_all ; /* total # of processors (compute and monitor) */
EXTERN int rsl_nproc_n ; /* number of processors decomposing maj dimension */
EXTERN int rsl_nproc_m ; /* number of processors decomposing min dimension */
EXTERN int rsl_myproc ;	/* my physical processor id */
EXTERN int rsl_idum ;	/* dummy integer variable */
EXTERN int rsl_padarea ;   /* pad area */
EXTERN int io_seq_compute ;
EXTERN int io_seq_monitor ;
EXTERN char mess[1024] ;      /* misc. message buffer for errors and warns */
EXTERN char * rsl_noprobe ;  /* set from environment */

EXTERN int rsl_debug_flg ;	/* set by rsl_debug */
EXTERN int old_offsets ;	/* used in rsl_new_decomp.c */

EXTERN int regular_decomp ;
EXTERN int sw_allow_dynpad ;

/* rsl macros */

#define RSL_FATAL(N)     rsl_fatal(N)
#define RSL_TEST_ERR(T,M) {if(T){fprintf(stderr,"%d rsl error (\"%s\":%d) %s\n",rsl_myproc,__FILE__,__LINE__,M);RSL_FATAL(5);}}
#define RSL_TEST_WRN(T,M) {if(T){fprintf(stderr,"%d rsl warning (\"%s\":%d) %s\n",rsl_myproc,__FILE__,__LINE__,M);}}

#if 1
#define RSL_MALLOC(T,N)  (T *)rsl_malloc(__FILE__,__LINE__,(sizeof(T))*(N))
#define RSL_FREE(P)      rsl_free(P)
#else
/* Bob Olson's stuff */
#define RSL_MALLOC(T,N)  (T *)nexus_debug_malloc((sizeof(T))*(N),__FILE__,__LINE__)
#define RSL_FREE(P)      nexus_debug_free(P,__FILE__,__LINE__)
#endif

#define MONITOR_ONLY(S)  { int x ; \
    RSL_C_IAMMONITOR( &x ) ; \
    if ( x==0 )                 \
      sprintf(mess,"%s: callable only by monitor",S) ; \
      RSL_TEST_ERR( x == 0,mess) ; }

/*
   This uses the low 26 bits of a 32 bit message type to encode a message
   type, a FROM processor designation and a TO processor designation.  Note
   the limits of this encoding:

        o  10 bits available for processor id (limit is 1024 processors)
        o   6 bits available for message tag - 1, because we are not
            allowing 0.  (limit is thus 63 possible tags)

   This is a potential machine or message passing interface dependency.
   Note, though, that under the proposed MPI standard, this would not
   be a problem because there are no FORCED type ranges to avoid and
   all .

rev: 95/02/15 -- jm.  Modified to drop the TO field (never used) and
   shorten the TAG field by 1 bit so that the total number of bits
   required is 15 (compatibility with minimum MPI standard and, in
   particular for the Fujitsu AP1000 at ANU).  Note the new limits
   from this encoding:

        o  10 bits available for processor id (limit is 1024 processors)
           (FROM only is encoded now)
        o   5 bits available for message tag - 1, because we are not
            allowing 0.  (limit is now 31 possible tags).
*/

#if 0
# define MTYPE_FROMTO(Y,F,T) ((((Y)&0x3f)<<20)|(((F)&0x3ff)<<10)|(((T)&0x3ff)))
# define MTYPE_TAG(X)   (((X)>>20)&0x3f)
# define MTYPE_FROM(X)  (((X)>>10)&0x3ff)
# define MTYPE_TO(X)    ((X)&0x3ff)
#else
# if 1
/* T is ignored and not encoded in the message tag, Y has been shorted 1 bit */
# define MTYPE_FROMTO(Y,F,T) (((((Y)&0x1f)<<10)|(((F)&0x3ff))) | 0 )
# define MTYPE_TAG(X)   (((X)>>10)&0x1f)
# define MTYPE_FROM(X)  ((X)&0x3ff)
# else
/* update May 2002, increase upper bound on processors to 4096 (12 bits)*/
# define MTYPE_FROMTO(Y,F,T) (((((Y)&0x1f)<<12)|(((F)&0xfff))) | 0 )
# define MTYPE_TAG(X)   (((X)>>12)&0x1f)
# define MTYPE_FROM(X)  ((X)&0xfff)
# endif
#endif


#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
#define INDEX_3(A,B,NB,C,NC)  INDEX_2( (A), INDEX_2( (B), (C), (NC) ), (NB)*(NC) )

#if 0
/* new encoding for these -- 
    7 bits -- domain id		maximum number is 127
   12 bits -- ig index		maximum number is 4095
   12 bits -- jg index		maximum number is 4095
  ----
   31 bits
*/
#define POINTID(D,J,I)   ((((((D)&0x7f))<<24)|(((J)&0xfff)<<12)|((I)&0xfff))|0L)
#define ID_DOMAIN(M)     ((((M)>>24)&0x7f)|0L)
#define ID_JDEX(M)       ((((M)>>12)&0xfff)|0L)
#define ID_IDEX(M)       (((M)      &0xfff)|0L)
#else
/* make ig and jg work as signed quantities */
/* new encoding for these -- 
    7 bits -- domain id		maximum number is 127
   12 bits -- ig index		range is -2048 to 2047
   12 bits -- jg index		range is is -2048 to 2047
  ----
   31 bits
*/
#define POINTID(D,J,I)   ((((((D)&0x7f))<<24)|(((J)&0xfff)<<12)|((I)&0xfff))|0L)
#define ID_DOMAIN(M)     ((((M)>>24)&0x7f)|0L)
#define ID_JDEX(M)       (((((M)>>12)&0xfff)&0x800)?((((M)>>12)&0xfff)|(-4096L)):((((M)>>12)&0xfff)|0L))
#define ID_IDEX(M)       (((M)&0x800)?(((M)      &0xfff)|(-4096L)):(((M)      &0xfff)|0L))
#endif

#define IS_INVALID(M) ( (M) == RSL_INVALID )

/* forward declarations */

rsl_point_id_t pointid() ;
rsl_index_t id_domain(), id_jdex(), id_idex() ;

/* RSL PT DESCRIPTORS */

#define RSL_4PT     4		/* FORTRAN */
#define RSL_8PT     8		/* FORTRAN */
#define RSL_12PT   12		/* FORTRAN */
#define RSL_24PT   24		/* FORTRAN */
#define RSL_48PT   48		/* FORTRAN */
#define RSL_80PT   80		/* FORTRAN */
#define RSL_120PT  120		/* FORTRAN */
#if (ALLOW_RSL_168PT == 1)
/* new -- added for MM5's upper radiative boundary stencil 9/26/96 */
#define RSL_168PT  168          /* FORTRAN */
#endif

#define RSL_NORTHSOUTH       1	/* FORTRAN */
#define RSL_EASTWEST         2	/* FORTRAN */
#define RSL_NORTHSOUTH_STAG  11	/* FORTRAN */
#define RSL_EASTWEST_STAG    12	/* FORTRAN */
#define RSL_NOTDECOMPOSED    3	/* FORTRAN */

#define RSL_M                1	/* FORTRAN */
#define RSL_N                2	/* FORTRAN */
#define RSL_M_STAG          11	/* FORTRAN */
#define RSL_N_STAG          12	/* FORTRAN */

/* message tags */

#define MSG_NEWDECOMPOSITION 100
#define MSG_MONITOR_REQUEST  101
#define MSG_STENCOM          		1
#define MSG_READ_RESPONSE    		2
#define MSG_WRITE_RESPONSE    		3
#define MSG_WRITE_COMPUTE_RESPONSE    	4
#define MSG_SPECIAL1_RESPONSE           5
#define MSG_SPECIAL2_RESPONSE           6
#define MSG_FROM_PARENT      		7
#define MSG_BCAST_SETUP      		8
#define MSG_MERGE_SETUP      		9
#define MSG_TO_PARENT        	       10
#define MSG_MON_BCAST		       11
#define MSG_REDISTCOM    	       12
#define MSG_PERCOM                     13
#define MSG_XPOSECOM                     14

#define MSG_IO_FORTRAN                  1
#define MSG_IO_SOCKET                   2

/* values for request mode2 when writing to sockets
   Raw mode just writes a byte stream,

   Fortran mode puts control words at beginning and end of each write
   with byte count in control word. 

   Portal mode puts a portal style dimension descriptor at the
   head of each new write.
*/

#define MSG_MODE2_RAW                         1
#define MSG_MODE2_FORTRAN                     2
#define MSG_MODE2_PORTAL                      3
#define MSG_MODE2_88                          4

/* monitor request types */
#define RSL_READ_REQUEST      100
#define RSL_READ_RESPONSE     101
#define RSL_READ_SPECIAL1     102
#define RSL_READ_SPECIAL2     103
#define RSL_READ_SPECIAL3     104
#define RSL_SHUTDOWN_REQUEST  105
#define RSL_WRITE_REQUEST     106
#define RSL_WRITE_RESPONSE    107

/* descriptor tags */

#define MESSAGE_DESC 0
#define STENCIL_DESC 1
#define BLANK_MESSAGE_DESC 2
#define PERIOD_DESC 3
#define XPOSE_DESC 4

/* xpose switches */
#define XPOSE_MN_MZ 0
#define XPOSE_MZ_MN 0

#define XPOSE_MZ_NZ 1
#define XPOSE_NZ_MZ 1

#define XPOSE_MN_NZ 2
#define XPOSE_NZ_MN 2

/* defines for MPI2 compat */

#ifndef MPI2_SUPPORT
typedef int MPI_Fint;
#  define MPI_Comm_c2f(comm) (MPI_Fint)(comm)
#  define MPI_Comm_f2c(comm) (MPI_Comm)(comm)
#endif

/* other includes */

#include "rsl_comm.h"
#include "compat.h"
#include "buf_for_proc.h"
#include "stencil_def.h"	/* stencil_def.h must follow message_def.h */
#include "xpose_def.h"	/* xpose_def.h must follow message_def.h */
#include "period_def.h"	/* period_def.h must follow message_def.h */
#include "rsl_io.h"

#if (( defined(vpp) || defined(vpp2) ) && ! defined(sx))
#define bcopy(a,b,c) vbcopy_C(a,b,c)
#endif

void * rsl_malloc(), * malloc()  ;
void * get_base_for_index() ;



#endif   /* nothing after this line */


