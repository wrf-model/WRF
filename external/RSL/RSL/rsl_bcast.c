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
#include "rsl.h"

typedef struct stage_point {
  char * p ;            /* pointer to buffer for point */
  int curs ;            /* cursor into point buffer */
#if (defined(vpp) || defined(vpp2))
  int blankcurs ;       /* curser into blank stage array */
#endif
  int P ;               /* processor ID for point */
  int kid_id ;
  int parent_id ;
  int cm ;
  int cn ;
  struct stage_point *next ;    /* list pointer */
} stage_point_t ;

typedef struct par_info {
  rsl_index_t ig, jg, cn, cm ;
  int kidid ;
} par_info_t ;

static destroy_par_info ( p )
  char * p ;
{
  if ( p != NULL ) RSL_FREE( p ) ;
}

static rsl_list_t *Xlist, *Xp, *Xprev ;
static stage_point_t *stage ;
static int stage_len = 0 ;              /* 96/3/15 */

static int s_d ;
static int s_nst ;
static int s_msize ;
static int s_nlen ;
static int s_mlen ;
static int s_nlen_nst ;
static int s_mlen_nst ;
static int s_irax_n ;
static int s_irax_m ;
static stage_point_t *Plist[RSL_MAXPROC] ;
static int Psize[RSL_MAXPROC] ;
static rsl_domain_info_t *s_dinfo, *s_ninfo ;
static rsl_point_t *s_ddomain, *s_ndomain ;
static char *s_parent_msgs ;
static int s_parent_msgs_curs ;
static int s_remaining ;  /* number of bytes left in a parent message before
                           the next point descriptor */

/* add a field to a message outgoing for the specified child domain cell */
/* relies on rsl_ready_bcast having been called already */
/* sends are specified in terms of coarse domain */

static int s_i, s_j, s_ig, s_jg, s_cm, s_cn,
           s_nig, s_njg ;

#ifdef LEARN_BCAST
static int s_putmsg = 0 ;
#endif

#if (defined(vpp) || defined(vpp2))
static char *blankstage = NULL ;
static int blankstagesize = 0 ;
static int blankstagecurs = 0 ;
#endif

/*@
  RSL_TO_CHILD_INFO -- Get the next cell in a packing sequence for forcing.

  Notes:
  RSL_TO_CHILD_INFO is used in a packing loop to build messages for
  forcing a nested (Arg2) domain with data from a parent (Arg1), in
  preparation for a call to RSL_BCAST_MSGS.  For an overview of the
  mechanism and a detailed example, see RSL_BCAST_MSGS.

  The Arg3 argument gives the size of the parent to child cell messages
  in bytes.  This may be larger than needed, but never smaller;
  otherwise the program will abort (to avoid overwriting memory).

  Each call to RSL_TO_CHILD_INFO gives the coordinates of a nested
  point to be sent forcing data (Arg10, Arg11), the local indices
  (Arg4, Arg5) and global indices (Arg6, Arg7) of the associated parent
  cell, and the indices of the child cell in the set of nest cells
  associated with the parent (Arg8, Arg9).  These
  specify which of the nest cells associated with the parent
  is being referred to.  There are IRAX_M by IRAX_N nest points
  associated with each parent, where IRAX_M is the nesting ratio in the
  M dimension and IRAX_N is the nesting ratio in the N dimension (See
  the descriptions for the RSL nest spawning routines RSL_SPAWN...).

  RSL_TO_CHILD_INFO will return a new set of coordinates for every nest
  point associated with a locally stored parent domain point.  In other
  words, if the processor has 15 points from the parent domain in its
  local memory and the nesting ratios are 3 by 3, then 135 (15*9)
  successive calls to RSL_TO_CHILD_INFO will return valid coordinates.
  Each time the routine returns with a valid point, the value of Arg12
  will be 1 and RSL will be in a state that is ready to accept data for
  the point.  The message is constructed using the routine
  RSL_TO_CHILD_MSG.  The 136th call will return a value of 0 (zero) in
  Arg12, indicating there are no more points.

  It isn't necessary that anything be done with the coordinates that
  are returned.  However, once called, RSL_TO_CHILD_INFO must be called
  as many times as it takes to exhaust the number of points; otherwise
  the underlying RSL mechanism will not be left in the proper state at
  the conclusion of the broadcast.

  See also:
  RSL_BCAST_MSGS, RSL_TO_CHILD_MSG, RSL_FROM_PARENT_INFO
@*/

RSL_TO_CHILD_INFO ( d_p, n_p, msize_p,
                    i_p, j_p,
                    ig_p, jg_p, cm_p, cn_p,
                    nig_p, njg_p, retval_p )
  int_p
     d_p            /* (I) RSL domain descriptor of parent. */
    ,n_p            /* (I) RSL domain descriptor of nest. */
    ,msize_p        /* (I) Message size in bytes. */
    ,i_p            /* (O) Local M index of parent domain point. */
    ,j_p            /* (O) Local N index of parent domain point. */
    ,ig_p           /* (O) Global N index of parent domain point. */
    ,jg_p           /* (O) Global N index of parent domain point. */
    ,cm_p           /* (O) M index of child cell beneath parent cell (see discussion). */
    ,cn_p           /* (O) N index of child cell beneath parent cell (see discussion). */
    ,nig_p          /* (O) Global M index of nest domain point. */
    ,njg_p          /* (O) Global N index of nest domain point. */
    ,retval_p ;     /* (O) =1 if a valid point returned; =0 (zero) otherwise. */
{
  rsl_child_info_t *kid ;
  rsl_list_t *lp ;
#ifdef LEARN_BCAST
  int removed ;
#endif
  par_info_t *dp ;
  int P ;

  if ( stage == NULL )
  {
    rsl_ready_bcast( d_p, n_p, msize_p ) ;
  }

  if (( lp = Xlist) == NULL ) 
  {
    *retval_p = -1 ;
    Xlist = s_ninfo->bcast_Xlist ;
#ifdef LEARN_BCAST
    Xprev = NULL ;
    Xp = NULL ;
    s_putmsg = 0 ;
#endif
    return ;
  }

#ifdef LEARN_BCAST
  removed = 0 ;
  if ( s_putmsg == 0 )
  {
     /* the node  previous to the one pointed to by Xlist did not
        add any messages.  Let's get rid of it. */
     if ( Xprev != NULL  ) { Xprev->next = Xlist ; Xp = Xlist ; removed = 1 ; }
  }
  if ( Xlist != s_ninfo->bcast_Xlist && ! removed ) Xprev = Xp ;
  Xp = Xlist ;
#endif

  Xlist = Xlist->next ;
  dp = (par_info_t *)(lp->data) ;
  s_ig = dp->ig ;
  s_jg = dp->jg ;
  s_i = s_ig + s_dinfo->idif ;
  s_j = s_jg + s_dinfo->jdif ;
  s_cm = dp->cm ;
  s_cn = dp->cn ;
  s_nig = ID_IDEX( dp->kidid ) ;
  s_njg = ID_JDEX( dp->kidid ) ;

  *ig_p = s_ig + 1 ;
  *jg_p = s_jg + 1;
  *i_p = s_i + 1 ;
  *j_p = s_j + 1;
  *cm_p = s_cm + 1;
  *cn_p = s_cn + 1;
  *nig_p = s_nig + 1;
  *njg_p = s_njg + 1;

#ifdef LEARN_BCAST
  s_putmsg = 0 ;
#endif

  *retval_p = 1 ;
  return ;
}

/*@
  RSL_TO_CHILD_MSG -- Pack force data into a message for a nest point.

  Notes:
  RSL_TO_CHILD_MSG is used in a loop to pack messages for forcing
  a nested domain with data from a parent in preparation for
  a call to RSL_BCAST_MSGS.  For an overview of the mechanism and a detailed
  example, see RSL_BCAST_MSGS.

  Before calling RSL_TO_CHILD_MSG, RSL must have been put into the correct
  state; that is, ready to accept data that will be sent to a particular
  point in the nest.  This is done by first calling RSL_TO_CHILD_INFO.
  RSL_TO_CHILD_MSG may be called as many times as necessary to pack
  data into the message (or not at all, if there is no data for the point
  returned by RSL_TO_CHILD_INFO).  Each call to RSL_TO_CHILD_MSG copies
  Arg1 bytes from the bufffer specified by Arg2 into the message, which
  is allocated by RSL and never manipulated directly by
  the user program.  The amount of data that can be packed is limited
  to the message size that
  was specified in the first call to RSL_TO_CHILD_INFO.

  See also:
  RSL_BCAST_MSGS, RSL_TO_CHILD_INFO

@*/

RSL_TO_CHILD_MSG ( nbuf_p, buf )
  int_p
    nbuf_p ;     /* (I) Number of bytes to be packed. */
  char *
    buf ;        /* (I) Buffer containing the data to be packed. */
{
  int kiddex ;
  int nbuf ;
  int P ;

  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;

  nbuf = *nbuf_p ;

#if (defined(vpp)||defined(vpp2))
#define BLANKBUMP (4*1024)
#endif

#ifdef LEARN_BCAST
  s_putmsg = 1 ;
#endif

  kiddex = INDEX_2(s_njg,s_nig,s_mlen_nst) ;
  P = s_ndomain[ kiddex ].P ;
#if ! (defined(vpp)||defined(vpp2))
  if ( stage[ kiddex ].p == NULL )
  {
    stage[ kiddex ].p = RSL_MALLOC( char, s_msize ) ;
    stage[ kiddex ].curs = 0 ;
    stage[ kiddex ].P = P ;
    stage[ kiddex ].next = Plist[P] ;
    Plist[P] =  &(stage[ kiddex ]) ;
  }
#else
  if ( stage[ kiddex ].p == NULL )
  {
    if (  blankstagecurs + s_msize >= blankstagesize )
    {
       char * newblank ;
       int i ;
       newblank = RSL_MALLOC( char, blankstagesize + BLANKBUMP*s_msize ) ;
       bcopy(blankstage,newblank,blankstagesize) ;
       /* reset the stage p pointers into the new blank stage buffer */
       for ( i = 0 ; i < s_mlen_nst * s_nlen_nst ; i++)
       {
         if ( stage[i].p != NULL )
         {
           stage[i].p = &(newblank[stage[i].blankcurs]) ;
         }
       }
       blankstagesize += BLANKBUMP*s_msize ;
       if ( blankstage != NULL ) RSL_FREE( blankstage ) ;
       blankstage = newblank ;
    }
    stage[ kiddex ].p = (char *) &( blankstage[blankstagecurs] ) ;
    stage[ kiddex ].blankcurs = blankstagecurs ;
    blankstagecurs += s_msize ;
    stage[ kiddex ].curs = 0 ;
    stage[ kiddex ].P = P ;
    stage[ kiddex ].next = Plist[P] ;
    Plist[P] =  &(stage[ kiddex ]) ;
  }
#endif
  if ( stage[ kiddex ].curs + nbuf > s_msize )
  {
    sprintf(mess,
    "RSL_TO_CHILD_MSG: would overflow buffer (%d+%d>%d)\n",
        stage[ kiddex ].curs, nbuf, s_msize ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }

  /* add point to head of list of points for processor P */
  stage[kiddex].kid_id = POINTID(s_nst,s_njg,s_nig) ;
  stage[kiddex].parent_id = POINTID( s_d, s_jg, s_ig ) ;
  stage[kiddex].cm = s_cm ;
  stage[kiddex].cn = s_cn ;
  Psize[P] += s_msize + sizeof( bcast_point_desc_t ) ;

  /* pack the buffer associated with stage[kiddex] */
  bcopy( buf, &(stage[ kiddex ].p[ stage[ kiddex ].curs ]), nbuf ) ;
  stage[ kiddex ].curs += nbuf ;

}

/*@
  RSL_BCAST_MSGS -- Convey forcing data from parent to nest points.

  Notes:
  RSL_BCAST_MSGS is called once forcing data
  from points in a parent domain have been packed into messages
  destined for associated points on the nest.  The routine has
  no arguments; rather, RSL must be in a ready state; this occurs
  once RSL_TO_CHILD_INFO has returned a value of 0 (zero)
  in its last argument on all processors.  The forcing data is
  conveyed along logical communication channels that were set up
  between nest points and associated points in the parent domain
  when the nest was spawned.  Interprocessor communication
  is generated for messages between points on different processors;
  otherwise, the transfer
  is done within the processor's local memory.

  The inverse operation to RSL_BCAST_MSGS is RSL_MERGE_MSGS, which
  is used for conveying feedback data from nest to parent.

  Note while reading the following example that RSL decomposes all
  domains independently and over all processors, so that every
  processor will have cells from the parent and from the nest.
  Thus, all processors perform both the packing of data from
  the parent and the unpacking of data onto the nest.  Other
  RSL routines appearing in the example are described elsewhere
  in these documents.

  Example:

$ C
$ C Packing data from the parent.
$ C
$   NLEV = (the number of vertical levels)
$   MSIZE = 3 * NLEV + 1 * WORDSIZE
$ C
$ C First call to rsl_to_child_info
$ C
$   CALL RSL_TO_CHILD_INFO( PID, NID,      ! parent, nest domain descriptors
$                           MSIZE,         ! size of message to a point
$                           I,J,PIG,PJG,   ! local and global parent cell coords
$                           CM,CN,         ! index of nest cell in parent cell
$                           NIG,NJG,       ! global nest cell coords
$                           IRETVAL )      ! return value
$   DO WHILE ( IRETVAL .EQ. 1 )
$     IF ( NIG .EQ. 1 .OR. NIG .EQ. M .OR. ! force only cells on nest bdy
$          NJG .EQ. 1 .OR. NJG .EQ. N     ) THEN
$       DO K = 1, NLEV
$         CALL RSL_TO_CHILD_MSG( WORDSIZE, FINTERP( CN, CM, U, I, J ) )
$         CALL RSL_TO_CHILD_MSG( WORDSIZE, FINTERP( CN, CM, V, I, J ) )
$         CALL RSL_TO_CHILD_MSG( WORDSIZE, FINTERP( CN, CM, T, I, J ) )
$       ENDDO
$       CALL RSL_TO_CHILD_MSG( WORDSIZE, FINTERP( CN, CM, PS, I, J ) )
$     ENDIF
$ C
$ C Subsequent calls to rsl_to_child_info
$ C
$     CALL RSL_TO_CHILD_INFO( PID, NID, MSIZE, I,J,PIG,PJG, CM,CN,
$                             NIG,NJG, IRETVAL )
$   END DO
$ C
$ C Broadcast the data.
$ C
$   CALL RSL_BCAST_MSGS
$ C
$ C Unpack the data onto the nest.
$ C
$ C
$ C First call to rsl_from_parent_info
$ C
$   CALL RSL_FROM_PARENT_INFO( I, J,       ! local nest cell coords
$                              NIG, NJG,   ! global nest cell coords
$                              CM, CN,     ! index of nest cell in parent cell
$                              PIG, PJG,   ! global parent cell coords
$                              IRETVAL )   ! return value
$   DO WHILE ( IRETVAL .EQ. 1 )
$     IF ( NIG .EQ. 1 .OR. NIG .EQ. M .OR.      ! force only cells on nest bdy
$          NJG .EQ. 1 .OR. NJG .EQ. N     ) THEN
$       DO K = 1, NLEV
$         CALL RSL_FROM_PARENT_MSG( WORDSIZE, U(I,J,K) )
$         CALL RSL_FROM_PARENT_MSG( WORDSIZE, V(I,J,K) )
$         CALL RSL_FROM_PARENT_MSG( WORDSIZE, T(I,J,K) )
$       ENDDO
$       CALL RSL_FROM_PARENT_MSG( WORDSIZE, PS(I,J) )
$     END IF
$ C
$ C Subsequent calls to rsl_from_parent_info
$ C
$     CALL RSL_FROM_PARENT_INFO( I, J, NIG, NJG, CM, CN, PIG, PJG, IRETVAL )
$   END DO

BREAKTHEEXAMPLECODE

  In this example, three 3-dimensional fields, U, V, and T, and one
  2-dimensional field, PS, from the parent domain are interpolated and
  sent as forcing data to the boundary of the nest.  Finterp is an
  interpolation function that computes the value for the nest based
  on the coordinates in the parent and on which nest cell the value
  is going to.  WORDSIZE, PIG, and PJG are integers; otherwise implicit typing
  holds.

                          
  See also:
  RSL_TO_CHILD_INFO, RSL_FROM_PARENT_INFO, RSL_MERGE_MSGS

@*/


RSL_BCAST_MSGS ()
{
  int P ;
  char *work ;
  bcast_point_desc_t pdesc ;
  stage_point_t *pt ;
  int curs ;
  int msglen, mdest, mtag ;
  int ii ;
  int ig, jg ;

  RSL_TEST_ERR( stage == NULL,
    "RSL_BCAST_MSGS: rsl_to_child_info not called first" ) ;

#if 0
 fprintf(stderr,"RSL_BCAST DEBUG s_msize %d\n",s_msize) ;
#endif

  for ( ii = 0 ; ii < s_ninfo->Nbcast_send_Plist ; ii++ )
  {
    P = s_ninfo->bcast_send_Plist[ii] ;
    msglen = s_ninfo->bcast_send_Pnpts[ii]*( sizeof(pdesc) + s_msize ) 
             + sizeof(pdesc) ; /*  end of message marker */
    curs = 0 ;
    work = buffer_for_proc( P, msglen, RSL_SENDBUF ) ;
    /* NOTE ASSUMPTION that the number of points in Plist will
       be less or equal to ninfo->bcast_send_Pnpts[ii].  If it isn't,
       we've got trouble.  */
#if 0
    /* debugging -- check the length of the list and compare
       it with the number of points we *think* we have. */
    { int npts_have, npts_thinkhave ;
      npts_thinkhave = s_ninfo->bcast_send_Pnpts[ii] ;
      for ( pt = Plist[P], npts_have = 0 ; pt != NULL ; pt = pt->next )
      {
        npts_have++ ;
      }
      if ( npts_thinkhave < npts_have )
      {
        sprintf(mess,"For P=%d Think have (%d) < have (%d)\n",P,npts_thinkhave,npts_have) ;
        RSL_TEST_ERR(1,mess) ;
      }
    }
#endif
    for ( pt = Plist[P] ; pt != NULL ; pt = pt->next )
    {
      if ( curs+sizeof(bcast_point_desc_t)+s_msize>msglen )
      {
        sprintf(mess,"would overwrite in bcast messages: %d > %d",
                curs+sizeof(bcast_point_desc_t)+s_msize, msglen) ;
        RSL_TEST_ERR(1,mess) ;
      }
#if 0
      ig = ID_IDEX( pt->parent_id ) ;
      jg = ID_JDEX( pt->parent_id ) ;
      if ( rsl_c_comp2phys_proc(s_dinfo->domain[INDEX_2(jg,ig,s_mlen)].P) != rsl_myproc )
      {
        sprintf(mess,"Point %d %d doesn't belong to me (%d) but rather to %d\n",ig,jg,rsl_myproc,
                      rsl_c_comp2phys_proc(s_dinfo->domain[INDEX_2(jg,ig,s_mlen)].P)) ;
        RSL_TEST_ERR(1,mess) ;
      }
#endif
      pdesc.nest_id = pt->kid_id ;
      pdesc.parent_id = pt->parent_id ;
      pdesc.cm = pt->cm ;
      pdesc.cn = pt->cn ;
      bcopy( &pdesc, &work[curs], sizeof( bcast_point_desc_t )) ;
      curs += sizeof( bcast_point_desc_t ) ;
      bcopy( pt->p, &work[curs], s_msize ) ;

#if 0
{
  int v ;
  float f ;
  fprintf(stderr,"RSL_BCAST DEBUG : ") ;
  for ( v = 0 ; v < s_msize/4 ; v++ )
  {
    bcopy( &work[v*4], &f, 4 ) ;
    fprintf(stderr," %f ",f ) ;
  }
}
#endif

      curs += s_msize ;
    }
    RSL_TEST_ERR(curs+sizeof(bcast_point_desc_t)>msglen,
      "Internal error: (end marker) would overwrite in bcast messages.") ;
    /* add end marker */
    pdesc.nest_id = RSL_INVALID ;
    pdesc.parent_id = RSL_INVALID ;
    pdesc.cm = RSL_INVALID ;
    pdesc.cn = RSL_INVALID ;
    bcopy( &pdesc, &work[curs], sizeof( bcast_point_desc_t )) ;
    curs += sizeof( bcast_point_desc_t ) ;
    /* note that it is all right for mlen to be less than msglen */
    if ( rsl_c_comp2phys_proc(P) != rsl_myproc )
    {
      mdest = rsl_c_comp2phys_proc(P) ;
      mtag = MTYPE_FROMTO( MSG_FROM_PARENT, rsl_myproc, mdest ) ;
      msglen = curs ;
      RSL_SEND( work, msglen, mtag, mdest ) ;
    }
    /* othersize, leave in the send buffer and we'll pick it
       up later */
  }

  /* reset this for the next phase, in RSL_MESSAGE_FROM_PARENT */
  s_parent_msgs = NULL ;
}

/* Return a point from some parent processor each time called.
   If no more points, from a processor, got to the next one.
   If no more points at all, retval is returned as RSL_INVALID */


/*@
  RSL_FROM_PARENT_INFO -- Get the next cell in a unpacking sequence for forcing.

  Notes:
  RSL_FROM_PARENT_INFO is used in a loop to unpack messages
  containing forcing data for a nested domain.  The messages have arrived on the
  local processor as a result of a previous call to RSL_BCAST_MSGS.
  The domain descriptors do not need to be specified; they are part of
  the state of RSL as a result of the calls to RSL_TO_CHILD_INFO that
  have come before.  For a detailed example, see RSL_BCAST_MSGS.

  Each call to RSL_FROM_PARENT_INFO gives the local indices a
  nested point receiving forcing data (Arg1, Arg2), the global indices
  of the nested point (Arg3, Arg4), and the global indices of the
  parent domain point providing the forcing data (Arg7, Arg8).  The
  indices of the child cell in the set of nest cells associated with
  the parent are returned through arguments Arg5 and Arg6.  These
  specify which of the nest cells associated with the parent
  is being referred to.  There are IRAX_M by IRAX_N nest points
  associated with each parent where IRAX_M is the nesting ratio in the
  M dimension and IRAX_N is the nesting ratio in the N dimension (See
  the descriptions for the RSL nest spawning routines RSL_SPAWN...).

  RSL_FROM_PARENT_INFO will return a new set of coordinates for every
  nest point stored in local processor memory.  For each point,
  RSL_FROM_PARENT_INFO returns a value of 1 through Arg9.  RSL is
  left in a state ready to yield
  data for the nest point; the data
  is unpacked from the message by calling RSL_FROM_PARENT_MSG.  Once
  all local nest points have been traversed, RSL_FROM_PARENT_INFO
  returns a value of 0 (zero) in Arg9.

  It isn't necessary that anything be unpacked with the coordinates
  that are returned.  However, once called, RSL_FROM_PARENT_INFO must
  be called as many times as it takes to exhaust the number of points;
  otherwise the underlying RSL mechanism will not be left in the proper
  state at the conclusion of the broadcast.

  See also:
  RSL_BCAST_MSGS, RSL_TO_CHILD_INFO, RSL_FROM_PARENT_MSG
@*/

RSL_FROM_PARENT_INFO ( i_p, j_p, ig_p, jg_p, cm_p, cn_p,
                       pig_p, pjg_p, retval_p )
  int_p
    i_p         /* (O) Local index in M dimension of nest. */
   ,j_p         /* (O) Local index in N dimension of nest. */
   ,ig_p        /* (O) Global index in M dimension of nest. */
   ,jg_p        /* (O) Global index in N dimension of nest. */
   ,cm_p        /* (O) M index of child cell beneath parent cell. */
   ,cn_p        /* (O) N index of child cell beneath parent cell. */
   ,pig_p       /* (O) Global index in M dimension of parent. */
   ,pjg_p       /* (O) Global index in N dimension of parent. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  int ii ;
  bcast_point_desc_t pdesc ;

  if ( s_remaining > 0 )
  {
    s_parent_msgs_curs += s_remaining ;
    s_remaining = 0 ;
  }
  get_a_new_bcast_point( retval_p ) ;
  if ( *retval_p != 1 )
  {
    cleanup_after_bcast() ;
    return ;
  }
  s_remaining = s_msize + sizeof(bcast_point_desc_t) ;

  /* at this point we have a non-null message buffer */
  /* read the descriptor */
  bcopy( &(s_parent_msgs[s_parent_msgs_curs]),
         &pdesc,
         sizeof(bcast_point_desc_t)) ;
  s_parent_msgs_curs += sizeof(bcast_point_desc_t) ;
  s_remaining -= sizeof(bcast_point_desc_t) ;

  /* get_a_new_bcast_point should not be returning these */
  RSL_TEST_ERR( pdesc.nest_id == RSL_INVALID, "Internal error.") ;

  *ig_p = ID_IDEX(pdesc.nest_id)+1 ;
  *jg_p = ID_JDEX(pdesc.nest_id)+1 ;
  *i_p = *ig_p + s_ninfo->idif ;
  *j_p = *jg_p + s_ninfo->jdif ;
  *pig_p = ID_IDEX(pdesc.parent_id)+1 ;
  *pjg_p = ID_JDEX(pdesc.parent_id)+1 ;
  *cm_p = pdesc.cm+1 ;
  *cn_p = pdesc.cn+1 ;

  *retval_p = 1 ;
  return ;
}

/*@
  RSL_FROM_PARENT_MSG -- Unpack feedback data into a nest point.

  Notes:
  RSL_FROM_PARENT_MSG is used in a loop to unpack messages containing
  forcing data from a parent domain.
  For an overview of the mechanism and a detailed
  example, see RSL_BCAST_MSGS.

  Before calling RSL_FROM_PARENT_MSG, RSL must have been put into the correct
  state; that is, ready to accept data that will be sent to a particular
  point in the nest.  This is done by first calling RSL_FROM_PARENT_INFO.
  RSL_FROM_PARENT_MSG may then be called as many times as necessary to unpack
  data from the message (or not at all, if there is no data for the point).
  Each call to RSL_FROM_PARENT_MSG copies
  Arg1 bytes from message into the bufffer specified by Arg2. Note that
  the message
  is allocated and handled entirely within RSL and never manipulated directly by
  the user program.  The amount of data that can be packed is limited
  to the message size that
  was specified in the first call to RSL_TO_CHILD_INFO.

  See also:
  RSL_BCAST_MSGS, RSL_FROM_PARENT_INFO, RSL_TO_CHILD_INFO

@*/
RSL_FROM_PARENT_MSG ( len_p, buf )
  int_p
    len_p ;          /* (I) Number of bytes to unpack. */
  char *
    buf ;            /* (O) Destination buffer. */
{
  if ( *len_p <= 0 ) return ;
  if ( *len_p > s_remaining ) 
  {
    sprintf(mess,
"RSL_FROM_PARENT_MSG:\n   Requested number of bytes (%d) exceeds %d, the number remaining for this point.\n", *len_p, s_remaining) ;
    RSL_TEST_WRN(1,mess) ;
  }
  bcopy( &(s_parent_msgs[s_parent_msgs_curs]),
         buf,
         *len_p ) ;

#if 0
{
float f ;
bcopy(buf,&f,4) ;
fprintf(stderr,"RSL_FROM_PARENT_MSG debug: curs: %d, val %f\n",
        s_parent_msgs_curs, f ) ;
}
#endif

  s_parent_msgs_curs += *len_p ;
  s_remaining -= *len_p ;
}

get_a_new_bcast_point( retval_p )
  int_p retval_p ;
{
  int result, mtag, ii ;
  bcast_point_desc_t pdesc ;

  do {
    if ( s_parent_msgs != NULL )
    {
#if 0
      pdesc = *((bcast_point_desc_t *)(&(s_parent_msgs[s_parent_msgs_curs]))) ;
#else
/* djs 1/98 */
      bcopy( &s_parent_msgs[s_parent_msgs_curs]
           , &pdesc
           , sizeof( bcast_point_desc_t )
           ) ;
#endif
      if ( pdesc.nest_id != RSL_INVALID )
      {
        *retval_p = 1 ;
        return ;                /* 2.a. */
      }
    }
    /* are there outstanding messages? */
    for ( ii = 0 ; ii < s_ninfo->Nbcast_recv_Plist ; ii++ )
    {
      if ( s_ninfo->bcast_recv_Ptags[ii] != RSL_INVALID )
      {
        break ;
      }
    }
    if ((ii == s_ninfo->Nbcast_recv_Plist)||(s_ninfo->Nbcast_recv_Plist <= 0))
    {
      *retval_p = -1 ;
      s_parent_msgs = NULL ;
      return ;                  /* 2.b.i */
    }
    /* scan till we get a message */
    ii = 0 ;
    result = 1 ;
    while (1)
    {
      if ( s_ninfo->bcast_recv_Ptags[ii] != RSL_INVALID )
      {
        if ( rsl_c_comp2phys_proc(s_ninfo->bcast_recv_Plist[ii]) != rsl_myproc )
        {
          mtag = s_ninfo->bcast_recv_Ptags[ii] ;
          if ( rsl_noprobe == NULL )
            RSL_PROBE( mtag, &result ) ;
          /* else, result will always be 1 */
          if ( result )
          {
#ifdef PGON
            if ( rsl_noprobe != NULL ) RSL_RECVEND ( mtag ) ;
#else
            RSL_RECVEND ( mtag ) ;
#endif
            s_ninfo->bcast_recv_Ptags[ii] = RSL_INVALID ;
            s_parent_msgs =
               buffer_for_proc( s_ninfo->bcast_recv_Plist[ii], 0, RSL_RECVBUF )
;
            break ;
          }
        }
        else
        {
          /* code to handle data from myself, which will be in my send buffer */
          s_ninfo->bcast_recv_Ptags[ii] = RSL_INVALID ;
          s_parent_msgs =
             buffer_for_proc( s_ninfo->bcast_recv_Plist[ii], 0, RSL_SENDBUF ) ;
                                                          /*    ^^^^^^^^^^^  */
                                                          /* because data is */
                                                          /* from myself     */
          break ;
        }
      }
      if ( ++ii >= s_ninfo->Nbcast_recv_Plist ) ii = 0 ;
    }
    s_parent_msgs_curs = 0 ;
#if 0
    pdesc = *((bcast_point_desc_t *)(&(s_parent_msgs[s_parent_msgs_curs]))) ;
#else
/* djs 1/98 */
    bcopy( &s_parent_msgs[s_parent_msgs_curs]
         , &pdesc
         , sizeof(bcast_point_desc_t)
         ) ;
#endif
  } while ( pdesc.nest_id == RSL_INVALID ) ;
  *retval_p = 1 ;
}

post_receives_from_parent()
{
  int ii, msglen, P, mtag, mfrom ;
  char * work ;

  for ( ii = 0 ; ii < s_ninfo->Nbcast_recv_Plist ; ii++ )
  {
    P = s_ninfo->bcast_recv_Plist[ii] ;
    mfrom = rsl_c_comp2phys_proc(P) ;
    if ( mfrom != rsl_myproc )
    {
      msglen = s_ninfo->bcast_recv_Pnpts[ii]*(sizeof(bcast_point_desc_t)+s_msize)
               + sizeof(bcast_point_desc_t) ;    /* end marker */
      work = buffer_for_proc(P, msglen, RSL_RECVBUF) ;
      mtag = MTYPE_FROMTO( MSG_FROM_PARENT, mfrom, rsl_myproc ) ;
      RSL_RECVBEGIN( work, msglen, mtag ) ;
      s_ninfo->bcast_recv_Ptags[ii] = mtag ; /* store tag */
    }
    else
    {
      /* set the tag so we know to unpack the send buffer 
         for data from ourself */
      mtag = MTYPE_FROMTO( MSG_FROM_PARENT, mfrom, rsl_myproc ) ;
      s_ninfo->bcast_recv_Ptags[ii] = mtag ;
    }
  }
}

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

rsl_ready_bcast( d_p, n_p, msize_p )
  int_p d_p, n_p, msize_p ;
{
  int i ;
  par_info_t *dp ;
  rsl_list_t *lp ;
  rsl_child_info_t * kid ;
  rsl_point_t *pt, *pt2 ;
  int kidid ;
  int ig, jg, kig, kjg, cn, cm ;
  int P ;

  s_msize = *msize_p ;
  s_d = *d_p ;
  s_nst = *n_p ;
  RSL_TEST_ERR( stage != NULL,
    "rsl_ready_bcast: called again before RSL_BCAST_MSGS of previous call.") ;
  RSL_TEST_ERR( s_d < 0 || s_d > RSL_MAXDOMAINS,
    "rsl_ready_bcast: bad parent domain descriptor" ) ;
  RSL_TEST_ERR( s_nst < 0 || s_nst > RSL_MAXDOMAINS,
    "rsl_ready_bcast: bad nested domain descriptor" ) ;
  RSL_TEST_ERR( s_d == s_nst,
    "rsl_ready_bcast: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[s_nst].parent != s_d ,
    "rsl_ready_bcast: the nest is not a child of the parent" ) ;
 
  s_dinfo = &( domain_info[s_d]) ;
  s_ninfo = &( domain_info[s_nst]) ;
  RSL_TEST_ERR( s_dinfo->valid != RSL_VALID,
    "rsl_ready_bcast: invalid parent domain" ) ;
  RSL_TEST_ERR( s_ninfo->valid != RSL_VALID,
    "rsl_ready_bcast: invalid nested domain" ) ;
  s_ddomain = s_dinfo->domain ;
  s_ndomain = s_ninfo->domain ;

  s_mlen = s_dinfo->len_m ;
  s_nlen = s_dinfo->len_n ;
  s_mlen_nst = s_ninfo->len_m ;
  s_nlen_nst = s_ninfo->len_n ;
  s_irax_n = s_ninfo->irax_n ;
  s_irax_m = s_ninfo->irax_m ;


  if ( s_dinfo->child_bcast_compiled[s_nst] != 1 ||
       s_ninfo->parent_bcast_compiled != 1 )
  {
    rsl_comp_bcast( d_p, n_p ) ;
    if ( s_ninfo->bcast_Xlist != NULL )
    {
      destroy_list( &(s_ninfo->bcast_Xlist), destroy_par_info ) ;
    }
    s_ninfo->bcast_Xlist = NULL ;
  }

  post_receives_from_parent() ;

  stage = RSL_MALLOC( stage_point_t , s_mlen_nst * s_nlen_nst ) ;
  stage_len = s_mlen_nst * s_nlen_nst ;         /* 96/3/15 */
  for ( i = 0 ; i < stage_len ; i++ )
  {
    stage[i].p = NULL ;
  }

#if (defined(vpp)||defined(vpp2))
  blankstagecurs = 0 ;
#endif

  /* construct the list of nested points under local parent points */

  if ( s_ninfo->bcast_Xlist == NULL )
  {
    /* traverse backwards so that Xlist can be constructed easily frontwards */
    for ( jg = s_nlen-1 ; jg >=0 ; jg-- )
    {
      for ( ig = s_mlen-1 ; ig >= 0 ; ig-- )
      {
        pt = &(s_ddomain[INDEX_2(jg,ig,s_mlen)]) ;
        if ( pt->valid == RSL_VALID &&
             rsl_c_comp2phys_proc(pt->P) == rsl_myproc )
        {
          if ((kid=pt->children_p) != NULL )
          {
            for ( cn = s_irax_n-1 ; cn >= 0 ; cn-- )
            {
              for ( cm = s_irax_m-1 ; cm >= 0 ; cm-- )
              {
                kidid = kid->child[INDEX_2(cn,cm,s_irax_m)] ;
                kig = ID_IDEX( kidid ) ;
                kjg = ID_JDEX( kidid ) ;
                pt2 = &(s_ndomain[INDEX_2(kjg,kig,s_mlen_nst)]) ;
                if ( pt2->valid == RSL_VALID  &&
                     ID_DOMAIN(kidid)==s_nst)
                {
                  dp = RSL_MALLOC( par_info_t, 1 ) ;
                  dp->ig = ig ;
                  dp->jg = jg ;
                  dp->cn = cn ;
                  dp->cm = cm ;
                  dp->kidid = kidid ;
                  lp = RSL_MALLOC( rsl_list_t, 1 ) ;
                  lp->data = dp ;
                  lp->next = s_ninfo->bcast_Xlist ;
                  s_ninfo->bcast_Xlist = lp ;
                }
              }
            }
          }
        }
      }
    }
  }
  Xlist = s_ninfo->bcast_Xlist ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    Plist[P] = NULL ;
  }

  return ;
}

/* now used internally only */
rsl_comp_bcast( d_p, n_p )
  int_p d_p, 
        n_p ;
{
  int d, nst, mlen, nlen, mlen_nst, nlen_nst ;
  rsl_domain_info_t *dinfo, *ninfo ;
  rsl_point_t *ddomain, *ndomain, *pt ;
  rsl_child_info_t *kids ;
  rsl_processor_t P ;
  int i, j, jg, ig, jgn, ign, cn, cm, cnt, p ;
  int irax_n, irax_m ;

  d = *d_p ;
  nst = *n_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, 
        "rsl_comp_bcast: bad parent domain descriptor") ;
  RSL_TEST_ERR( nst < 0 || nst >= RSL_MAXDOMAINS, 
        "rsl_comp_bcast: bad nested domain descriptor") ;
  RSL_TEST_ERR( d == nst, 
        "rsl_comp_bcast: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
        "rsl_comp_bcast: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
        "rsl_comp_bcast: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
        "rsl_comp_bcast: invalid nested domain" ) ;

  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  mlen_nst = ninfo->len_m ;
  nlen_nst = ninfo->len_n ;
  ddomain = dinfo->domain ;
  ndomain = ninfo->domain ;
  irax_n = ninfo->irax_n ;
  irax_m = ninfo->irax_m ;

  destroy_bcast_compilation( d_p, n_p ) ;

  if ( dinfo->decomposed != 1 )
  {
     fprintf(stderr,"Calling default decomposition for parent %d\n",*d_p);
     default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  if ( ninfo->decomposed != 1 )
  {
     fprintf(stderr,"Calling default decomposition for nest %d\n",*n_p);
     default_decomposition( n_p,
                           &(domain_info[*n_p].loc_m),
                           &(domain_info[*n_p].loc_n) ) ;
  }

  /* begin by computing the receive list */
  for ( i = 0 ; i < rsl_nproc_all ; i++ ) 
    ninfo->bcast_recv_Pnpts[i] = 0 ;

  for ( jgn = 0 ; jgn < nlen_nst ; jgn++ )
  {
    for ( ign = 0 ; ign < mlen_nst ; ign++ )
    {
      pt = &(ndomain[INDEX_2(jgn,ign,mlen_nst)]) ;
      if ( pt->valid == RSL_VALID  &&
           rsl_c_comp2phys_proc(pt->P) == rsl_myproc )
      {
        (ninfo->bcast_recv_Pnpts[pt->mother_P])++ ; /* count this point as 
                                                       coming from the parent
                                                       processor */
      }
    }
  }
  /* compress and copy the plist */
  ninfo->Nbcast_recv_Plist = 0 ;
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    if ( ninfo->bcast_recv_Pnpts[P] > 0 )
    {
      ninfo->bcast_recv_Pnpts[ninfo->Nbcast_recv_Plist] =
                                        ninfo->bcast_recv_Pnpts[P] ;
      ninfo->bcast_recv_Plist[ninfo->Nbcast_recv_Plist] = P ;
      (ninfo->Nbcast_recv_Plist)++ ;
    }
  }

  /* now compute the send list */
  for ( i = 0 ; i < rsl_nproc_all ; i++ ) 
    ninfo->bcast_send_Pnpts[i] = 0 ;

  for ( jgn = 0 ; jgn < nlen_nst ; jgn++ )
  {
    for ( ign = 0 ; ign < mlen_nst ; ign++ )
    {
      pt = &(ndomain[INDEX_2(jgn,ign,mlen_nst)]) ;
      if ( pt->valid == RSL_VALID  &&
           rsl_c_comp2phys_proc(pt->mother_P) == rsl_myproc )
      {
         if ( pt->valid == RSL_VALID )
          ninfo->bcast_send_Pnpts[pt->P]++ ; /* count this point being
                                                sent from me to P */
      }
    }
  }

  /* compress and copy the plist */
  ninfo->Nbcast_send_Plist = 0 ;
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    if ( ninfo->bcast_send_Pnpts[P] > 0 )
    {
      ninfo->bcast_send_Pnpts[ninfo->Nbcast_send_Plist] =
                                        ninfo->bcast_send_Pnpts[P] ;
      ninfo->bcast_send_Plist[ninfo->Nbcast_send_Plist] = P ;

      (ninfo->Nbcast_send_Plist)++ ;
    }
  }

  dinfo->child_bcast_compiled[nst] = 1 ;
  ninfo->parent_bcast_compiled = 1 ;

  return ;
}

cleanup_after_bcast()
{
  int i ;
  if ( stage != NULL )
  {
    for ( i = 0 ; i < stage_len ; i++ )
    {
#if ! (defined(vpp)||defined(vpp2))
      if ( stage[i].p != NULL ) RSL_FREE( stage[i].p ) ;  /* 96/3/15 */
#else
      stage[i].p = NULL ;
#endif
    }
    RSL_FREE( stage ) ;
  }
#if (defined(vpp)||defined(vpp2))
  blankstagecurs = 0 ;
#endif
  stage = NULL ;
  s_msize = RSL_INVALID ;
  s_dinfo = NULL ;
  s_ninfo = NULL ;
  s_ddomain = NULL ;
  s_ndomain = NULL ;
  s_parent_msgs = NULL ;
  s_parent_msgs_curs = RSL_INVALID ;
}

destroy_bcast_compilation( d_p, n_p )
  int_p d_p, n_p ;
{
  int d, nst, P ;
  rsl_domain_info_t *dinfo, *ninfo ;
  rsl_point_t *ddomain, *ndomain, *pt ;

  d = *d_p ;
  nst = *n_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
        "rsl_comp_bcast: bad parent domain descriptor") ;
  RSL_TEST_ERR( nst < 0 || nst >= RSL_MAXDOMAINS,
        "rsl_comp_bcast: bad nested domain descriptor") ;
  RSL_TEST_ERR( d == nst,
        "rsl_comp_bcast: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
        "rsl_comp_bcast: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
        "rsl_comp_bcast: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
        "rsl_comp_bcast: invalid nested domain" ) ;
  
  ninfo->parent_bcast_compiled = 0 ;
  dinfo->child_bcast_compiled[nst] = 0 ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    ninfo->bcast_recv_Pnpts[P] = 0 ;
    ninfo->bcast_recv_Plist[P] = RSL_INVALID ;
    ninfo->bcast_recv_Ptags[P] = RSL_INVALID ;
    ninfo->Nbcast_recv_Plist = 0 ;
  }
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

RSL_RESET_STAGING ()
{
#if (defined(vpp) || defined(vpp2))
if ( blankstage != NULL ) RSL_FREE( blankstage ) ;
blankstage = NULL ;
blankstagesize = 0 ;
blankstagecurs = 0 ;
#endif
}


