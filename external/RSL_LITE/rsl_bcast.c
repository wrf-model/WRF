/* #define LEARN_BCAST */
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

#define MOD_9707

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include "rsl_lite.h"

char mess[4096] ;

typedef struct bcast_point_desc {
  int ig ;
  int jg ;
} bcast_point_desc_t ;


static destroy_par_info ( p )
  char * p ;
{
  if ( p != NULL ) RSL_FREE( p ) ;
}

static rsl_list_t *Xlist, *Xp, *Xprev ;
static rsl_list_t *stage ;
static int stage_len = 0 ;              /* 96/3/15 */

static int  Sendbufsize ;
static int  Sendbufcurs ;
static char *Sendbuf ;
static int  Sdisplacements[RSL_MAXPROC] ;
static int  Ssizes[RSL_MAXPROC] ;

static int  Recsizeindex ;

static int  Rbufsize ;
static int  Rbufcurs ;
static int  Rpointcurs ;
static char *Recvbuf ;
static int  Rdisplacements[RSL_MAXPROC] ;
static int  Rsizes[RSL_MAXPROC] ;
static int  Rreclen ;

static int s_d ;
static int s_nst ;
static int s_msize ;
static int s_idim ;
static int s_jdim ;
static int s_idim_nst ;
static int s_jdim_nst ;
static int s_irax_n ;
static int s_irax_m ;
static int s_ntasks_x ;
static int s_ntasks_y ;
static rsl_list_t **Plist ;
static int Psize[RSL_MAXPROC] ;
static char *s_parent_msgs ;
static int s_parent_msgs_curs ;
static int s_remaining ;  /* number of bytes left in a parent message before
                           the next point descriptor */

/* add a field to a message outgoing for the specified child domain cell */
/* relies on rsl_ready_bcast having been called already */
/* sends are specified in terms of coarse domain */

static int s_i, s_j, s_ig, s_jg, s_cm, s_cn,
           s_nig, s_njg ;

static int Pcurs ;
static rsl_list_t *Pptr ; 

#ifdef LEARN_BCAST
static int s_putmsg = 0 ;
#endif

/* parent->nest */
RSL_LITE_TO_CHILD_INFO ( msize_p,
                         cips_p, cipe_p, cjps_p, cjpe_p, /* patch dims of CD */
                         nids_p, nide_p, njds_p, njde_p, /* domain dims of ND */
                         ntasks_x_p , ntasks_y_p ,       /* proc counts in x and y */
                         icoord_p, jcoord_p,
                         idim_cd_p, jdim_cd_p,
                         ig_p, jg_p,
                         retval_p )
  int_p
     cips_p, cipe_p, cjps_p, cjpe_p   /* (i) c.d. patch dims */
    ,nids_p, nide_p, njds_p, njde_p   /* (i) n.n. global dims */
    ,ntasks_x_p , ntasks_y_p          /* proc counts in x and y */
    ,icoord_p       /* i coordinate of nest in cd */
    ,jcoord_p       /* j coordinate of nest in cd */
    ,idim_cd_p      /* i width of nest in cd */
    ,jdim_cd_p      /* j width of nest in cd */
    ,msize_p        /* (I) Message size in bytes. */
    ,ig_p           /* (O) Global N index of parent domain point. */
    ,jg_p           /* (O) Global N index of parent domain point. */
    ,retval_p ;     /* (O) =1 if a valid point returned; =0 (zero) otherwise. */
{
  int P, Px, Py ;
  rsl_list_t *q ;
  int *r ;
  int i, j ;

  if ( Plist == NULL ) {
    s_ntasks_x = *ntasks_x_p ;
    s_ntasks_y = *ntasks_y_p ;
    /* construct Plist */
    Sendbufsize = 0 ;
    Plist = RSL_MALLOC( rsl_list_t * , s_ntasks_x * s_ntasks_y ) ;  /* big enough for nest points */
    for ( j = 0 ; j < s_ntasks_x * s_ntasks_y ; j++ ) {
       Plist[j] = NULL ;
       Sdisplacements[j] = 0 ;
       Ssizes[j] = 0 ;
    }
    for ( j = *cjps_p ; j <= *cjpe_p ; j++ )
    {
      for ( i = *cips_p ; i <= *cipe_p ; i++ )
      {
	if ( ( *jcoord_p <= j && j <= *jcoord_p+*jdim_cd_p-1 ) && ( *icoord_p <= i && i <= *icoord_p+*idim_cd_p-1 ) ) {
	   TASK_FOR_POINT ( &i, &j, nids_p, nide_p, njds_p, njde_p, &s_ntasks_x, &s_ntasks_y, &Px, &Py, &P ) ;
#if 0
fprintf(stderr,"%d %d is on P %d (nids_p, nide_p, njds_p, njde_p %d %d %d %d)\n",i,j,P,*nids_p,*nide_p,*njds_p,*njde_p) ;
#endif
	   q = RSL_MALLOC( rsl_list_t , 1 ) ;
	   q->info1 = i ;
	   q->info2 = j ;
	   q->next = Plist[P] ;
	   Plist[P] = q ;
	   Sendbufsize += *msize_p + 3 * sizeof( int ) ;  /* point data plus 3 ints for i, j, and size */
        }
      }
    }
    Sendbuf = RSL_MALLOC( char , Sendbufsize ) ;
    Sendbufcurs = 0 ;
    Recsizeindex = -1 ;
    Pcurs = -1 ;
    Pptr = NULL ;
  }
  if ( Pptr != NULL ) {
    Pptr = Pptr->next ;
  } 

  if ( Recsizeindex >= 0 ) {
          r = (int *) &(Sendbuf[Recsizeindex]) ;
          *r = Sendbufcurs - Recsizeindex + 2 * sizeof(int) ;
          Ssizes[Pcurs] += *r ;
#if 0
fprintf(stderr,"B Sendbufcurs %d Recsizeindex %d Pcurs %d\n",Sendbufcurs,Recsizeindex,Pcurs);
fprintf(stderr,"B Ssizes[%d] %d\n",Pcurs,Ssizes[Pcurs]  ) ;
fprintf(stderr,"B storing size for %d %d (%d %d) %d\n",*ig_p,*jg_p,*(r-2),*(r-1),*r ) ;
#endif
  }

  while ( Pptr == NULL ) {
      Pcurs++ ;
      while ( Plist[Pcurs] == NULL && Pcurs < s_ntasks_x * s_ntasks_y ) Pcurs++ ;
      if ( Pcurs < s_ntasks_x * s_ntasks_y ) {
        Sdisplacements[Pcurs] = Sendbufcurs ;
        Ssizes[Pcurs] = 0 ;
        Pptr = Plist[Pcurs] ;
      } else {
        *retval_p = 0 ;
#if 0
fprintf(stderr,"to child info returns 0 \n" );
#endif
        return ;  /* done */
      }
  }

#if 0
  if ( Recsizeindex >= 0 ) {
    r = (int *) &(Sendbuf[Recsizeindex]) ;
    *r = Sendbufcurs - Recsizeindex + 2 * sizeof(int) ;
    Ssizes[Pcurs] += *r ;
fprintf(stderr,"A Sendbufcurs %d Recsizeindex %d Pcurs %d\n",Sendbufcurs,Recsizeindex,Pcurs);
fprintf(stderr,"A Ssizes[Pcurs] %d\n",Ssizes[Pcurs]  ) ;
fprintf(stderr,"A storing size for %d %d (%d %d) %d\n",*ig_p,*jg_p,*(r-2),*(r-1),*r ) ;
  }
#endif

  *ig_p = Pptr->info1 ;
  *jg_p = Pptr->info2 ;

#if 0
fprintf(stderr,"to child info %d %d %d\n",*ig_p,*jg_p,Sendbufcurs) ;
#endif
  r = (int *) &(Sendbuf[Sendbufcurs]) ;
  *r++ = Pptr->info1 ; Sendbufcurs += sizeof(int) ;  /* ig to buffer */
  *r++ = Pptr->info2 ; Sendbufcurs += sizeof(int) ;  /* jg to buffer */
  Recsizeindex = Sendbufcurs ;
  *r++ =           0 ; Sendbufcurs += sizeof(int) ;  /* store start for size */
  *retval_p = 1 ;
#if 0
fprintf(stderr,"to child info returns 1 \n" );
#endif

  return ;
}

/********************************************/

/* nest->parent */
RSL_LITE_TO_PARENT_INFO ( msize_p,
                          nips_p, nipe_p, njps_p, njpe_p, /* patch dims of ND */
                          cids_p, cide_p, cjds_p, cjde_p, /* domain dims of CD */
                          ntasks_x_p , ntasks_y_p ,       /* proc counts in x and y */
                          icoord_p, jcoord_p,
                          idim_cd_p, jdim_cd_p,
                          ig_p, jg_p,
                          retval_p )
  int_p
     nips_p, nipe_p, njps_p, njpe_p   /* (i) n.d. patch dims */
    ,cids_p, cide_p, cjds_p, cjde_p   /* (i) n.n. global dims */
    ,ntasks_x_p , ntasks_y_p          /* proc counts in x and y */
    ,icoord_p       /* i coordinate of nest in cd */
    ,jcoord_p       /* j coordinate of nest in cd */
    ,idim_cd_p      /* i width of nest in cd */
    ,jdim_cd_p      /* j width of nest in cd */
    ,msize_p        /* (I) Message size in bytes. */
    ,ig_p           /* (O) Global N index of parent domain point. */
    ,jg_p           /* (O) Global N index of parent domain point. */
    ,retval_p ;     /* (O) =1 if a valid point returned; =0 (zero) otherwise. */
{
  int P, Px, Py ;
  rsl_list_t *q ;
  int *r ;
  int i, j ;

  if ( Plist == NULL ) {
    s_ntasks_x = *ntasks_x_p ;
    s_ntasks_y = *ntasks_y_p ;
    /* construct Plist */
    Sendbufsize = 0 ;
    Plist = RSL_MALLOC( rsl_list_t * , s_ntasks_x * s_ntasks_y ) ;
    for ( j = 0 ; j < s_ntasks_x * s_ntasks_y ; j++ ) {
       Plist[j] = NULL ;
       Sdisplacements[j] = 0 ;
       Ssizes[j] = 0 ;
    }
    for ( j = *njps_p ; j <= *njpe_p ; j++ )
    {
      for ( i = *nips_p ; i <= *nipe_p ; i++ )
      {
	if ( ( *jcoord_p <= j && j <= *jcoord_p+*jdim_cd_p-1 ) && ( *icoord_p <= i && i <= *icoord_p+*idim_cd_p-1 ) ) {
	  TASK_FOR_POINT ( &i, &j, cids_p, cide_p, cjds_p, cjde_p, &s_ntasks_x, &s_ntasks_y, &Px, &Py, &P ) ;
	  q = RSL_MALLOC( rsl_list_t , 1 ) ;
	  q->info1 = i ;
	  q->info2 = j ;
	  q->next = Plist[P] ;
	  Plist[P] = q ;
	  Sendbufsize += *msize_p + 3 * sizeof( int ) ;  /* point data plus 3 ints for i, j, and size */
        }
      }
    }
    Sendbuf = RSL_MALLOC( char , Sendbufsize ) ;
    Sendbufcurs = 0 ;
    Recsizeindex = -1 ;
    Pcurs = -1 ;
    Pptr = NULL ;
  }
  if ( Pptr != NULL ) {
    Pptr = Pptr->next ;
  } 

  if ( Recsizeindex >= 0 ) {
          r = (int *) &(Sendbuf[Recsizeindex]) ;
          *r = Sendbufcurs - Recsizeindex + 2 * sizeof(int) ;
          Ssizes[Pcurs] += *r ;
  }

  while ( Pptr == NULL ) {
      Pcurs++ ;
      while ( Plist[Pcurs] == NULL && Pcurs < s_ntasks_x * s_ntasks_y ) Pcurs++ ;
      if ( Pcurs < s_ntasks_x * s_ntasks_y ) {
        Sdisplacements[Pcurs] = Sendbufcurs ;
        Ssizes[Pcurs] = 0 ;
        Pptr = Plist[Pcurs] ;
      } else {
        *retval_p = 0 ;
        return ;  /* done */
      }
  }

  *ig_p = Pptr->info1 ;
  *jg_p = Pptr->info2 ;

  r = (int *) &(Sendbuf[Sendbufcurs]) ;
  *r++ = Pptr->info1 ; Sendbufcurs += sizeof(int) ;  /* ig to buffer */
  *r++ = Pptr->info2 ; Sendbufcurs += sizeof(int) ;  /* jg to buffer */
  Recsizeindex = Sendbufcurs ;
  *r++ =           0 ; Sendbufcurs += sizeof(int) ;  /* store start for size */
  *retval_p = 1 ;

  return ;
}


/********************************************/

/*@
  RSL_TO_CHILD_MSG -- Pack force data into a message for a nest point.

@*/

/* parent->nest */
RSL_LITE_TO_CHILD_MSG ( nbuf_p, buf )
  int_p
    nbuf_p ;     /* (I) Number of bytes to be packed. */
  char *
    buf ;        /* (I) Buffer containing the data to be packed. */
{
   rsl_lite_to_peerpoint_msg ( nbuf_p, buf ) ;
}

/* nest->parent */
RSL_LITE_TO_PARENT_MSG ( nbuf_p, buf )
  int_p
    nbuf_p ;     /* (I) Number of bytes to be packed. */
  char *
    buf ;        /* (I) Buffer containing the data to be packed. */
{
   rsl_lite_to_peerpoint_msg ( nbuf_p, buf ) ;
}

/* common code */
rsl_lite_to_peerpoint_msg ( nbuf_p, buf )
  int_p
    nbuf_p ;     /* (I) Number of bytes to be packed. */
  char *
    buf ;        /* (I) Buffer containing the data to be packed. */
{
  int kiddex ;
  int nbuf ;
  int P, Px, Py ;
  int *p, *q ;
  int i ;

  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;

  nbuf = *nbuf_p ;

  if ( Sendbufcurs + nbuf >= Sendbufsize ) {
    sprintf(mess,"RSL_LITE_TO_CHILD_MSG: Sendbufcurs + nbuf (%d) would exceed Sendbufsize (%d)\n",
           Sendbufcurs + nbuf , Sendbufsize ) ;
    RSL_TEST_ERR(1,mess) ;
  }

  for ( p = (int *)buf, q = (int *) &(Sendbuf[Sendbufcurs]), i = 0 ; i < nbuf ; i += sizeof(int) )
  {
    *q++ = *p++ ;
  }
  Sendbufcurs += nbuf ;

}

/********************************************/

/* parent->nest */
RSL_LITE_BCAST_MSGS ( mytask_p, ntasks_p, comm0 )
  int_p mytask_p, ntasks_p, comm0 ;
{
  rsl_lite_allgather_msgs ( mytask_p, ntasks_p, comm0 ) ;
}

/* nest->parent */
RSL_LITE_MERGE_MSGS ( mytask_p, ntasks_p, comm0 )
  int_p mytask_p, ntasks_p, comm0 ;
{
  rsl_lite_allgather_msgs ( mytask_p, ntasks_p, comm0 ) ;
}

/* common code */
rsl_lite_allgather_msgs ( mytask_p, ntasks_p, comm0 )
  int_p mytask_p, ntasks_p, comm0 ;
{
  int P ;
  char *work ;
  int * r ;
  bcast_point_desc_t pdesc ;
  int curs ;
  int msglen, mdest, mtag ;
  int ntasks, mytask ;
  int ii, i, j ;
  int ig, jg ;
  int *Psize_all ;
  int *sp, *bp ;

  ntasks = *ntasks_p ;
  mytask = *mytask_p ;

  RSL_TEST_ERR( Plist == NULL,
    "RSL_BCAST_MSGS: rsl_to_child_info not called first" ) ;

  RSL_TEST_ERR( ntasks == RSL_MAXPROC ,
    "RSL_BCAST_MSGS: raise the compile time value of MAXPROC" ) ;
  
  Psize_all = RSL_MALLOC( int, ntasks * ntasks ) ;

  MPI_Allgather( Ssizes, ntasks, MPI_INT , Psize_all, ntasks, MPI_INT, *comm0 ) ;

  for ( j = 0 ; j < ntasks ; j++ ) 
    Rsizes[j] = 0 ;

  for ( j = 0 ; j < ntasks ; j++ ) 
  {
    Rsizes[j] += Psize_all[ INDEX_2( j , mytask , ntasks ) ] ;
  }

  for ( Rbufsize = 0, P = 0, Rdisplacements[0] ; P < ntasks ; P++ )
  {
    Rdisplacements[P+1] = Rsizes[P] + Rdisplacements[P] ;
    Rbufsize += Rsizes[P] ;
  }

  /* this will be freed later */
  Recvbuf = RSL_MALLOC( char , Rbufsize + 3 * sizeof(int) ) ; /* for sentinal record */
  Rbufcurs = 0 ;
  Rreclen = 0 ;

  MPI_Alltoallv ( Sendbuf, Ssizes, Sdisplacements, MPI_BYTE , 
                  Recvbuf, Rsizes, Rdisplacements, MPI_BYTE ,  *comm0 ) ;

/* add sentinel to the end of Recvbuf */

  r = (int *)&(Recvbuf[Rbufsize + 2 * sizeof(int)]) ;
  *r = RSL_INVALID ;

  RSL_FREE( Sendbuf ) ;
  RSL_FREE( Psize_all ) ;

  for ( j = 0 ; j < *ntasks_p ; j++ )  {
    destroy_list ( &(Plist[j]), NULL ) ;
  }
  RSL_FREE( Plist ) ;
  Plist = NULL ;

}

/********************************************/

/* parent->nest */
RSL_LITE_FROM_PARENT_INFO ( ig_p, jg_p, retval_p )
  int_p
    ig_p        /* (O) Global index in M dimension of nest. */
   ,jg_p        /* (O) Global index in N dimension of nest. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  rsl_lite_from_peerpoint_info ( ig_p, jg_p, retval_p ) ;
}

/* nest->parent */
RSL_LITE_FROM_CHILD_INFO ( ig_p, jg_p, retval_p )
  int_p
    ig_p        /* (O) Global index in M dimension of nest. */
   ,jg_p        /* (O) Global index in N dimension of nest. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  rsl_lite_from_peerpoint_info ( ig_p, jg_p, retval_p ) ;
}

/* common code */
rsl_lite_from_peerpoint_info ( ig_p, jg_p, retval_p )
  int_p
    ig_p        /* (O) Global index in M dimension of nest. */
   ,jg_p        /* (O) Global index in N dimension of nest. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  int ii ;

  Rbufcurs = Rbufcurs + Rreclen ;
  Rpointcurs = 0 ;
  *ig_p    = *(int *)&( Recvbuf[Rbufcurs + Rpointcurs ] ) ; Rpointcurs += sizeof(int) ;
  *jg_p    = *(int *)&( Recvbuf[Rbufcurs + Rpointcurs ] ) ; Rpointcurs += sizeof(int) ;
/* read sentinel */
  Rreclen  = *(int *)&( Recvbuf[Rbufcurs + Rpointcurs ] ) ; Rpointcurs += sizeof(int) ;
  *retval_p = 1 ;
  if ( Rreclen == RSL_INVALID ) {
    *retval_p = 0 ;
    RSL_FREE( Recvbuf ) ;
  }
     
#if 0
fprintf(stderr,"FROM  INFO: %d %d %d %d %d\n",*ig_p,*jg_p,Rreclen, Rbufcurs + Rpointcurs, *retval_p) ;
#endif
  return ;
}

/********************************************/

/* parent->nest */
RSL_LITE_FROM_PARENT_MSG ( len_p, buf )
  int_p
    len_p ;          /* (I) Number of bytes to unpack. */
  int *
    buf ;            /* (O) Destination buffer. */
{
  rsl_lite_from_peerpoint_msg ( len_p, buf ) ;
}

/* nest->parent */
RSL_LITE_FROM_CHILD_MSG ( len_p, buf )
  int_p
    len_p ;          /* (I) Number of bytes to unpack. */
  int *
    buf ;            /* (O) Destination buffer. */
{
  rsl_lite_from_peerpoint_msg ( len_p, buf ) ;
}

/* common code */
rsl_lite_from_peerpoint_msg ( len_p, buf )
  int_p
    len_p ;          /* (I) Number of bytes to unpack. */
  int *
    buf ;            /* (O) Destination buffer. */
{
  int *p, *q ;
  int i ;
  for ( p = (int *)&(Recvbuf[Rbufcurs+Rpointcurs]), q = buf , i = 0 ; i < *len_p ; i += sizeof(int) ) 
  {
    *q++ = *p++ ;
  }
  Rpointcurs += *len_p ;
}

/********************************************/

destroy_list( list, dfcn )
  rsl_list_t ** list ;          /* pointer to pointer to list */
  int (*dfcn)() ;               /* pointer to function for destroying
                                   the data field of the list */
{
  rsl_list_t *p, *trash ;
  if ( list == NULL ) return(0) ;
  if ( *list == NULL ) return(0) ;
  for ( p = *list ; p != NULL ; )
  {
    if ( dfcn != NULL ) (*dfcn)( p->data ) ;
    trash = p ;
    p = p->next ;
    RSL_FREE( trash ) ;
  }
  *list = NULL ;
  return(0) ;
}

/********************************************/


#if 0
RSL_MOVE_NEST ( d_p, n_p,  mdisp_p, ndisp_p )
  int_p d_p, n_p, mdisp_p, ndisp_p ;
{
  int parent, intermed, nest, mdisp, ndisp ;
  rsl_domain_info_t *dinfo, *ninfo ;
  int i, j, cm, cn, irax_m, irax_n, nid ;
  int mother_id ;
  rsl_child_info_t ** children_p ;

  parent = *d_p    ; nest = *n_p      ;
  mdisp = *mdisp_p ; ndisp = *ndisp_p ; 

  RSL_TEST_ERR( parent < 0 || parent > RSL_MAXDOMAINS, "rsl_move_nest: bad parent domain descriptor" ) ;
  RSL_TEST_ERR( nest < 0   || nest > RSL_MAXDOMAINS,   "rsl_move_nest: bad nested domain descriptor" ) ;
  dinfo = &( domain_info[parent]) ;
  ninfo = &( domain_info[nest]) ;
  irax_m = ninfo->irax_m ;
  irax_n = ninfo->irax_n ;

  if ( dinfo->child_bcast_compiled[s_nst] != 1 ||
       ninfo->parent_bcast_compiled != 1 )
  {
    rsl_comp_bcast( d_p, n_p ) ;
  }
  dinfo->child_bcast_compiled[nest] = 0 ;  /* invalidate broadcast */
  ninfo->parent_bcast_compiled      = 0 ;  /* invalidate broadcast */


  if ( dinfo->child_merge_compiled[s_nst] != 1 ||
       ninfo->parent_merge_compiled != 1 )
  {
    rsl_comp_merge( d_p, n_p ) ;
  }
  dinfo->child_merge_compiled[nest] = 0 ;  /* invalidate merge */
  ninfo->parent_merge_compiled      = 0 ;  /* invalidate merge */

  children_p = RSL_MALLOC( rsl_child_info_t *, dinfo->len_n * dinfo->len_m ) ;

  for ( j = 0 ; j < dinfo->len_n ; j++ )
    for ( i = 0 ; i < dinfo->len_m ; i++ )
         children_p[ INDEX_2( j, i, dinfo->len_m ) ] = NULL ;

  for ( j = 0 ; j < dinfo->len_n ; j++ )
    for ( i = 0 ; i < dinfo->len_m ; i++ )
       if (    i - mdisp >= 0 && i - mdisp < dinfo->len_m 
            && j - ndisp >= 0 && j - ndisp < dinfo->len_n ) {
         children_p[ INDEX_2( j, i, dinfo->len_m ) ] = dinfo->domain[ INDEX_2( j - ndisp , i - mdisp, dinfo->len_m ) ].children_p ;
       }

  for ( j = 0 ; j < dinfo->len_n ; j++ )
    for ( i = 0 ; i < dinfo->len_m ; i++ )
       dinfo->domain[ INDEX_2( j , i , dinfo->len_m ) ].children_p = children_p[ INDEX_2( j, i, dinfo->len_m ) ] ;

  RSL_FREE( children_p ) ;

  for ( j = 0 ; j < dinfo->len_n ; j++ )
    for ( i = 0 ; i < dinfo->len_m ; i++ )
      for ( cn = 0 ; cn < irax_n ; cn++ )
        for ( cm = 0 ; cm < irax_m ; cm++ )
          if ( dinfo->domain[ INDEX_2( j, i, dinfo->len_m ) ].children_p != NULL )
          {
            dinfo->domain[ INDEX_2( j, i, dinfo->len_m ) ].children_p->child[INDEX_2(cn,cm,irax_m)] = RSL_INVALID ;
          }

  for ( j = 0 ; j < ninfo->len_n ; j++ )
  {
    for ( i = 0 ; i < ninfo->len_m ; i++ )
    {
      nid = POINTID( nest, j, i ) ;
      mother_id = ninfo->domain[ INDEX_2( j, i, ninfo->len_m ) ].mother_id ;
      mother_id = POINTID(parent, (ID_JDEX( mother_id )) + ndisp, (ID_IDEX( mother_id )) + mdisp ) ;
      ninfo->domain[ INDEX_2( j, i, ninfo->len_m ) ].mother_id = mother_id ;
      ninfo->domain[ INDEX_2( j, i, ninfo->len_m ) ].mother_P = 
                       dinfo->domain[ INDEX_2( ID_JDEX( mother_id ), ID_IDEX( mother_id ), dinfo->len_m ) ].P ;
      cm = ninfo->domain[ INDEX_2( j, i, ninfo->len_m ) ].which_kid_am_i_m ;
      cn = ninfo->domain[ INDEX_2( j, i, ninfo->len_m ) ].which_kid_am_i_n ;
      if ( dinfo->domain[ INDEX_2( ID_JDEX( mother_id ), ID_IDEX( mother_id ), dinfo->len_m ) ].children_p != NULL ) {
        dinfo->domain[ INDEX_2( ID_JDEX( mother_id ), ID_IDEX( mother_id ), dinfo->len_m ) ].children_p->child[INDEX_2(cn,cm,irax_m)] = nid ;
      }
    }
  }
  ninfo->coord_m += mdisp ;
  ninfo->coord_n += ndisp ;
}


#ifdef NOUNDERSCORE
cwrap_fbcast ( pd, nd, msize, mf, pf, upf )
#else
# ifdef T3D
CWRAP_FBCAST ( pd, nd, msize, mf, pf, upf )
# else
#   ifdef F2CSTYLE
cwrap_fbcast__( pd, nd, msize, mf, pf, upf )
#   else
cwrap_fbcast_( pd, nd, msize, mf, pf, upf )
#   endif
# endif
#endif
  int *pd, *nd, *msize ;
  void (*mf)(), (*pf)(), (*upf)() ;
{
  char * buf ;
  buf = RSL_MALLOC( char, *msize ) ;
#ifdef NOUNDERSCORE
  rsl_f_bcast_chld ( pd, nd, msize, buf, mf, pf, upf ) ;
#else
# ifdef T3D
  RSL_F_BCAST_CHLD ( pd, nd, msize, buf, mf, pf, upf ) ;
# else
#   ifdef F2CSTYLE
  rsl_f_bcast_chld__( pd, nd, msize, buf, mf, pf, upf ) ;
#   else
  rsl_f_bcast_chld_( pd, nd, msize, buf, mf, pf, upf ) ;
#   endif
# endif
#endif
  RSL_FREE( buf ) ;
}

#ifdef NOUNDERSCORE
cwrap_fmerge ( pd, nd, msize, mf, pf, upf )
#else
# ifdef T3D
CWRAP_FMERGE ( pd, nd, msize, mf, pf, upf )
# else
#   ifdef F2CSTYLE
cwrap_fmerge__( pd, nd, msize, mf, pf, upf )
#   else
cwrap_fmerge_( pd, nd, msize, mf, pf, upf )
#   endif
# endif
#endif
  int *pd, *nd, *msize ;
  void (*mf)(), (*pf)(), (*upf)() ;
{
  char * buf ;
  buf = RSL_MALLOC( char, *msize ) ;
#ifdef NOUNDERSCORE
  rsl_f_merge_chld ( pd, nd, msize, buf, mf, pf, upf ) ;
#else
# ifdef T3D
  RSL_F_MERGE_CHLD ( pd, nd, msize, buf, mf, pf, upf ) ;
# else
#   ifdef F2CSTYLE
  rsl_f_merge_chld__( pd, nd, msize, buf, mf, pf, upf ) ;
#   else
  rsl_f_merge_chld_( pd, nd, msize, buf, mf, pf, upf ) ;
#   endif
# endif
#endif
  RSL_FREE( buf ) ;
}

vbcopy_C(a,b,c)
  char *a, *b ; int c ;
{
#if (( defined(vpp) || defined(vpp2) ) && ! defined(sx))
  int l, lb ;
  l = ((c)/sizeof(int)) ;
  lb = l*sizeof(int) ;
  vicopy_(a,b,&l) ;
  l = c-lb ;
  vbcopy_(a+lb,b+lb,&l) ;
#endif
}


#endif

