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

#define MAX(a,b) (((a)>(b))?a:b)

#define MOD_9707

#ifndef MS_SUA
# include <stdio.h>
#endif
#include <stdlib.h>
#ifndef STUBMPI
#  include "mpi.h"
#endif
#include "rsl_lite.h"

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
static int  Rdisplacements[RSL_MAXPROC+1] ;
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
static int s_ntasks_nest_x ;
static int s_ntasks_nest_y ;
static int s_ntasks_par_x ;
static int s_ntasks_par_y ;
static rsl_list_t **Plist ;
static int Plist_length = 0 ;
static int Psize[RSL_MAXPROC] ;
static char *s_parent_msgs ;
static int s_parent_msgs_curs ;
static int s_remaining ;  /* number of bytes left in a parent message before
                           the next point descriptor */
static int alltasks, offset ;

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

// NOTES for PARALLELNESTING
// This routine is building a list of destination processes to send to on a communicator that .
// It needs the minor number of tasks on the nest's MPI mesh (just pass that in)
// Otherwise it doesn't need a communicator

RSL_LITE_NESTING_RESET (
                       )
{
  int j ;

  for ( j = 0 ; j < RSL_MAXPROC ; j++ ) {
    Ssizes[j] = 0 ;
    Sdisplacements[j] = 0 ;
    Rsizes[j] = 0 ;
    Rdisplacements[j] = 0 ;
  }
  Rdisplacements[RSL_MAXPROC] = 0 ;
  if ( Plist != NULL ) {
    for ( j = 0 ; j < Plist_length ; j++ ) {
      destroy_list ( &(Plist[j]), NULL ) ;
    }
    RSL_FREE( Plist ) ;
    Plist = NULL ;
  }
}

/* parent->nest */
void RSL_LITE_TO_CHILD_INFO ( msize_p,                        /* number of tasks in minor dim of nest's mesh */
                         cips_p, cipe_p, cjps_p, cjpe_p, /* patch dims of SOURCE DOMAIN */
                         iids_p, iide_p, ijds_p, ijde_p, /* domain dims of INTERMEDIATE DOMAIN */
                         nids_p, nide_p, njds_p, njde_p, /* domain dims of CHILD DOMAIN */
                         pgr_p,  shw_p ,                 /* nest ratio and stencil half width */
                         offset_p,                         /* first task of the nest in me_and_mom communicator */
                         ntasks_par_x_p , ntasks_par_y_p ,       /* proc counts in x and y */
                         ntasks_nest_x_p , ntasks_nest_y_p ,       /* proc counts in x and y */
                         min_subdomain ,                 /* minimum width allowed for a subdomain in a dim ON PARENT */
                         icoord_p, jcoord_p,
                         idim_cd_p, jdim_cd_p,
                         ig_p, jg_p,
                         retval_p )
  
  int_p
     cips_p, cipe_p, cjps_p, cjpe_p   /* (i) c.d. patch dims */
    ,iids_p, iide_p, ijds_p, ijde_p   /* (i) n.n. global dims -- in WRF this will be intermediate domain */
    ,nids_p, nide_p, njds_p, njde_p   /* (i) n.n. global dims */
    ,pgr_p                            /* nesting ratio */
    ,offset_p                         /* first task of the nest in me_and_mom communicator */
    ,ntasks_nest_x_p , ntasks_nest_y_p          /* proc counts in x and y */
    ,ntasks_par_x_p , ntasks_par_y_p          /* proc counts in x and y */
    ,min_subdomain
    ,icoord_p       /* i coordinate of nest in cd */
    ,jcoord_p       /* j coordinate of nest in cd */
    ,shw_p          /* stencil half width */
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
  int i, j, ni, nj ;
  int coords[2] ;
  int ierr ;

  if ( Plist == NULL ) {
    s_ntasks_par_x = *ntasks_par_x_p ;
    s_ntasks_par_y = *ntasks_par_y_p ;
    s_ntasks_nest_x = *ntasks_nest_x_p ;
    s_ntasks_nest_y = *ntasks_nest_y_p ;
    offset = *offset_p ;
    alltasks = MAX( s_ntasks_nest_x*s_ntasks_nest_y + offset, s_ntasks_par_x*s_ntasks_par_y ) ;

#if 0
fprintf(stderr,"s_ntasks_par_x %d\n",s_ntasks_par_x)  ;
fprintf(stderr,"s_ntasks_par_y %d\n",s_ntasks_par_y)  ;
fprintf(stderr,"s_ntasks_nest_x %d\n",s_ntasks_nest_x)  ;
fprintf(stderr,"s_ntasks_nest_y %d\n",s_ntasks_nest_y)  ;
fprintf(stderr,"%s %d offset %d\n",__FILE__,__LINE__,offset)  ;
fprintf(stderr,"%s %d alltasks %d\n",__FILE__,__LINE__,alltasks)  ;
fprintf(stderr,"%s %d a %d b %d\n",__FILE__,__LINE__,s_ntasks_nest_x*s_ntasks_nest_y+offset,s_ntasks_par_x*s_ntasks_par_y)  ;
#endif

    /* construct Plist */
    Sendbufsize = 0 ;
    Plist = RSL_MALLOC( rsl_list_t * , alltasks ) ;  /* big enough for nest points */
    Plist_length = alltasks ;
   /* big enough for the mom and me communicator, which includes tasks for the parent and the nest */
    for ( j = 0 ; j < alltasks ; j++ ) {
       Plist[j] = NULL ;
       Sdisplacements[j] = 0 ;
       Ssizes[j] = 0 ;
    }
    ierr = 0 ;
    for ( j = *cjps_p ; j <= *cjpe_p ; j++ )
    {
      for ( i = *cips_p ; i <= *cipe_p ; i++ )
      {
	if ( ( *jcoord_p <= j && j <= *jcoord_p+*jdim_cd_p-1 ) && ( *icoord_p <= i && i <= *icoord_p+*idim_cd_p-1 ) ) {
           ni = ( i - (*icoord_p + *shw_p) ) * *pgr_p + 1 + 1 ; /* add 1 to give center point */
           nj = ( j - (*jcoord_p + *shw_p) ) * *pgr_p + 1 + 1 ;

#ifndef STUBMPI
	   TASK_FOR_POINT ( &ni, &nj, nids_p, nide_p, njds_p, njde_p, &s_ntasks_nest_x, &s_ntasks_nest_y, &Px, &Py, 
                            min_subdomain, min_subdomain, &ierr ) ;
           P = Px + Py * *ntasks_nest_x_p + offset ; 
//           coords[1] = Px ; coords[0] = Py ;
//           MPI_Cart_rank( *comm, coords, &P ) ;
// PARALLELNESTING
// adjust P so that is the rank in the intercomm_to_kid communicator for this parent/nest pair
//fprintf(stderr,"after tfp ni %d nj %d Px %d Py %d P %d ntx %d nty %d\n",ni,nj,Px,Py,P,*ntasks_nest_x_p,*ntasks_nest_y_p) ;

#else
           P = 0 ;
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
    if ( ierr != 0 ) {
      fprintf(stderr,"rsl_to_child_info: ") ;
      TASK_FOR_POINT_MESSAGE () ;
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
      while ( Pcurs < alltasks && Plist[Pcurs] == NULL  ) Pcurs++ ;
      if ( Pcurs < alltasks ) {
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

/* nest->parent */
void RSL_LITE_TO_PARENT_INFO ( msize_p, 
                          nips_p, nipe_p, njps_p, njpe_p, /* patch dims of SOURCE DOMAIN (CHILD) */
                          cids_p, cide_p, cjds_p, cjde_p, /* domain dims of TARGET DOMAIN (PARENT) */
                          offset_p, 
                          ntasks_par_x_p , ntasks_par_y_p ,       /* proc counts in x and y */
                          ntasks_nest_x_p , ntasks_nest_y_p ,       /* proc counts in x and y */
                          min_subdomain ,
                          icoord_p, jcoord_p,
                          idim_cd_p, jdim_cd_p,
                          ig_p, jg_p,
                          retval_p )
  int_p
     nips_p, nipe_p, njps_p, njpe_p   /* (i) n.d. patch dims */
    ,cids_p, cide_p, cjds_p, cjde_p   /* (i) n.n. global dims */
    ,offset_p
    ,ntasks_nest_x_p , ntasks_nest_y_p          /* proc counts in x and y */
    ,ntasks_par_x_p , ntasks_par_y_p          /* proc counts in x and y */
    ,min_subdomain
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
  int coords[2] ;
  int ierr ;

  if ( Plist == NULL ) {
    s_ntasks_nest_x = *ntasks_nest_x_p ;
    s_ntasks_nest_y = *ntasks_nest_y_p ;
    s_ntasks_par_x = *ntasks_par_x_p ;
    s_ntasks_par_y = *ntasks_par_y_p ;
    offset = *offset_p ;
    alltasks = MAX( s_ntasks_nest_x*s_ntasks_nest_y + offset, s_ntasks_par_x*s_ntasks_par_y ) ;

    /* construct Plist */
    Sendbufsize = 0 ;
    Plist = RSL_MALLOC( rsl_list_t * , alltasks ) ;
    Plist_length = alltasks ;
    for ( j = 0 ; j < alltasks ; j++ ) {
       Plist[j] = NULL ;
       Sdisplacements[j] = 0 ;
       Ssizes[j] = 0 ;
    }
    ierr = 0 ;
    for ( j = *njps_p ; j <= *njpe_p ; j++ )
    {
      for ( i = *nips_p ; i <= *nipe_p ; i++ )
      {
	if ( ( *jcoord_p <= j && j <= *jcoord_p+*jdim_cd_p-1 ) && ( *icoord_p <= i && i <= *icoord_p+*idim_cd_p-1 ) ) {
#ifndef STUBMPI
	  TASK_FOR_POINT ( &i, &j, cids_p, cide_p, cjds_p, cjde_p, &s_ntasks_par_x, &s_ntasks_par_y, &Px, &Py, 
                           min_subdomain, min_subdomain, &ierr ) ;
          P = Px + Py * *ntasks_par_x_p ;  // we are computing parent task numbers, so no offset
#else
          P = 0 ;
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
    if ( ierr != 0 ) {
      fprintf(stderr,"rsl_to_parent_info: ") ;
      TASK_FOR_POINT_MESSAGE () ;
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
      while ( Pcurs < alltasks && Plist[Pcurs] == NULL ) Pcurs++ ;
      if ( Pcurs < alltasks ) {
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
  int nbuf ;
  int *p, *q ;
  char *c, *d ;
  int i ;
  char mess[4096] ;

  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;

  nbuf = *nbuf_p ;

  if ( Sendbufcurs + nbuf >= Sendbufsize ) {
    sprintf(mess,"rsl_lite_to_peerpoint_msg: Sendbufcurs + nbuf (%d) would exceed Sendbufsize (%d)\n",
           Sendbufcurs + nbuf , Sendbufsize ) ;
    RSL_TEST_ERR(1,mess) ;
  }

  if ( nbuf % sizeof(int) == 0 ) {
    for ( p = (int *)buf, q = (int *) &(Sendbuf[Sendbufcurs]), i = 0 ; i < nbuf ; i += sizeof(int) )
    {
      *q++ = *p++ ;
    }
  }
  else
  {
    for ( c = buf, d = &(Sendbuf[Sendbufcurs]), i = 0 ; i < nbuf ; i++ )
    {
      *d++ = *c++ ;
    }
  }

  Sendbufcurs += nbuf ;

}

/********************************************/

// PARALLELNESTING NOTES
//  what communicator should be passed and what are mytask and ntasks?
//  I think it should be the mom_and_me communicator and the mytask and ntasks from
//  that communicator
//
//  nest if it's parent->nest and the parent if it's nest->parent (we'll see)

/* parent->nest */
RSL_LITE_BCAST_MSGS ( mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, Fcomm )
  int_p mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, Fcomm ;  /* offset is the id of the first task in the nest set */
{
#ifndef STUBMPI
  MPI_Comm comm ;

  comm = MPI_Comm_f2c( *Fcomm ) ;
#else
  int comm ;
#endif
  rsl_lite_allgather_msgs ( mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, comm, 0 ) ;
}

/* nest->parent */
RSL_LITE_MERGE_MSGS ( mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, Fcomm )
  int_p mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, Fcomm ;  /* offset is the id of the first task in the nest set */
{
#ifndef STUBMPI
  MPI_Comm comm ;

  comm = MPI_Comm_f2c( *Fcomm ) ;
#else
  int comm ;
#endif
  rsl_lite_allgather_msgs ( mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, comm, 1 ) ;
}

/* common code */
rsl_lite_allgather_msgs ( mytask_p, ntasks_par_p, ntasks_nest_p, offset_p, comm, dir )
  int_p mytask_p, ntasks_par_p, ntasks_nest_p, offset_p ;
  int dir ;  /* 0 = parent to nest, otherwist nest to parent */
#ifndef STUBMPI
  MPI_Comm comm ;
#else
  int comm ;
#endif
{
  int P ;
  char *work ;
  int * r ;
  bcast_point_desc_t pdesc ;
  int curs ;
  int msglen, mdest, mtag ;
  int ntasks_par, ntasks_nest, ntasks, mytask ;
  int  mytask_on_comm ;
  int ii, i, j ;
  int ig, jg ;
  int *sp, *bp ;
  int rc ;

#ifndef STUBMPI
  ntasks_par = *ntasks_par_p ;
  ntasks_nest = *ntasks_nest_p ;
  mytask = *mytask_p ;
  MPI_Comm_rank( comm, &mytask_on_comm ) ;
#else
  ntasks = 1 ;
  mytask = 0 ;
  mytask_on_comm = 0 ;
#endif

  if ( ( mytask_on_comm <  ntasks_par  && dir == 0 )   /* parent in parent->child */
    || ( mytask_on_comm >= *offset_p && 
         mytask_on_comm < *offset_p + ntasks_nest && dir == 1 )) { /* child  in child->parent */
    RSL_TEST_ERR( Plist == NULL,
      "rsl_lite_allgather_msgs: rsl_to_child_info or rsl_to_parent_info not called first" ) ;
  }

#ifndef STUBMPI
  ntasks = MAX(ntasks_par,ntasks_nest+*offset_p) ;
#endif

  RSL_TEST_ERR( ntasks >= RSL_MAXPROC ,
    "rsl_lite_allgather_msgs: raise the compile time value of MAXPROC" ) ;
  
#ifndef STUBMPI
  MPI_Alltoall(Ssizes,1,MPI_INT, Rsizes,1,MPI_INT,comm);
#else
  Rsizes[0] = Ssizes[0];
#endif

  for ( Rbufsize = 0, P = 0, Rdisplacements[0] = 0 ; P < ntasks ; P++ )
  {
    Rdisplacements[P+1] = Rsizes[P] + Rdisplacements[P] ;

    Rbufsize += Rsizes[P] ;
  }

  /* this will be freed later */

  Recvbuf = RSL_MALLOC( char , Rbufsize + 3 * sizeof(int) ) ; /* for sentinal record */
  Rbufcurs = 0 ;
  Rreclen = 0 ;

#ifndef STUBMPI
  rc = MPI_Alltoallv ( Sendbuf, Ssizes, Sdisplacements, MPI_BYTE , 
                       Recvbuf, Rsizes, Rdisplacements, MPI_BYTE ,  comm ) ;
#else
  work = Sendbuf ;
  Sendbuf = Recvbuf ;
  Recvbuf = work ;
#endif

/* add sentinel to the end of Recvbuf */

  r = (int *)&(Recvbuf[Rbufsize + 2 * sizeof(int)]) ;
  *r = RSL_INVALID ;

  if ( Sendbuf != NULL ) RSL_FREE( Sendbuf ) ; 
  if ( Plist != NULL ) {
    for ( j = 0 ; j < Plist_length ; j++ ) { 
      destroy_list ( &(Plist[j]), NULL ) ;
    }
    RSL_FREE( Plist ) ;
    Plist = NULL ;
    Plist_length = 0 ;
  }

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
  char *c, *d ;
  int i ;

  if ( *len_p % sizeof(int) == 0 ) {
    for ( p = (int *)&(Recvbuf[Rbufcurs+Rpointcurs]), q = buf , i = 0 ; i < *len_p ; i += sizeof(int) ) 
    {
      *q++ = *p++ ;
    }
  } else {
    for ( c = &(Recvbuf[Rbufcurs+Rpointcurs]), d = (char *) buf , i = 0 ; i < *len_p ; i++ )
    {
      *d++ = *c++ ;
    }
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
