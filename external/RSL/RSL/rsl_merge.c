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

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

typedef struct stage_point {
  char * p ;            /* pointer to buffer for point */
  int curs ;            /* cursor into point buffer */
  int P ;               /* processor ID for point */
  int kid_id ;
  int parent_id ;
  int cm ;
  int cn ;
  struct stage_point *next ;    /* list pointer */
} stage_point_t ;

typedef struct nest_info {
  rsl_index_t ig, jg, cn, cm ;
  int mother_id ;
} nest_info_t ;

static destroy_nest_info ( p )
  char * p ;
{
  if ( p != NULL ) RSL_FREE( p ) ;
}

static rsl_list_t *Xlist = NULL ;
static stage_point_t *stage = NULL ;
static int stage_len = 0 ;

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
static rsl_domain_info_t *s_dinfo = NULL , *s_ninfo  = NULL ;
static rsl_point_t *s_ddomain = NULL , *s_ndomain  = NULL ;
static char *s_child_msgs = NULL ;
static int s_child_msgs_curs = 0 ;
static int s_remaining = 0 ;  /* number of bytes left in a child message before
                           the next point descriptor */

/* add a field to a message outgoing for the specified child domain cell */
/* relies on rsl_ready_merge having been called already */
/* sends are specified in terms of coarse domain */

static int s_i, s_j, s_ig, s_jg, s_cm, s_cn,
           s_pig, s_pjg ;

/*@
  RSL_TO_PARENT_INFO -- Get the next cell in a packing sequence for feedback.

  Notes:
  RSL_TO_PARENT_INFO is used in a packing loop to build feedback
  messages from a nested (Arg2) domain with data from a parent (Arg1),
  in preparation for a call to RSL_MERGE_MSGS.  For an overview of the
  mechanism and a detailed example, see RSL_MERGE_MSGS.

  The Arg3 argument gives the size of the child to parent cell messages
  in bytes.  This may be larger than needed, but never smaller;
  otherwise the program will abort (to avoid overwriting memory).

  Each call to RSL_TO_PARENT_INFO gives the local (Arg4, Arg5) and
  global (Arg6, Arg7) indices of a nested point that will send feedback
  data, the global indices (Arg10, Arg11) of the associated parent
  cell, and the indices of the nest cell in the set of nest cells
  associated with the parent (Arg8, Arg9).  These last two are needed
  to differentiate which of the nest cells associated with the parent
  is being referred to.  There are IRAX_M by IRAX_N nest points
  associated with each parent, where IRAX_M is the nesting ratio in the
  M dimension and IRAX_N is the nesting ratio in the N dimension (See
  the descriptions for the RSL nest spawning routines RSL_SPAWN...).

  RSL_TO_PARENT_INFO will return a new set of coordinates for every
  nest point stored locally on the processor.  Each time the routine
  returns with a valid point, the value of Arg12 will be 1 and RSL will
  be in a state that is ready to accept data for the point.  The
  message is constructed using the routine RSL_TO_PARENT_MSG.  The last
  call will return a value of 0 (zero) in Arg12 indicating there are no
  more nest points.

  It isn't necessary that anything be done with the coordinates that
  are returned.  However, once called, RSL_TO_PARENT_INFO must be
  called as many times as it takes to exhaust the number of points;
  otherwise the underlying RSL mechanism will not be left in the proper
  state at the conclusion of the broadcast.

  See also:
  RSL_MERGE_MSGS, RSL_TO_PARENT_MSG, RSL_FROM_CHILD_INFO
@*/

RSL_TO_PARENT_INFO ( d_p, n_p, msize_p,
                     i_p, j_p,
                     ig_p, jg_p, cm_p, cn_p,
                     pig_p, pjg_p, retval_p )
  int_p
    d_p         /* (I) RSL domain descriptor of parent. */
   ,n_p         /* (I) RSL domain descriptor of nest. */
   ,msize_p     /* (I) Message size in bytes. */
   ,i_p         /* (O) Local M index of nest domain point. */
   ,j_p         /* (O) Local N index of nest domain point. */
   ,ig_p        /* (O) Global M index of nest domain point. */
   ,jg_p        /* (O) Global N index of nest domain point. */
   ,cm_p        /* (O) M index of child cell beneath parent cell. */
   ,cn_p        /* (O) N index of child cell beneath parent cell. */
   ,pig_p       /* (O) Global N index of parent domain point. */
   ,pjg_p       /* (O) Global N index of parent domain point. */
   ,retval_p ;  /* (O) =1 if a valid point returned; =0 (zero) otherwise. */
{
  rsl_child_info_t *kid ;
  rsl_list_t *lp ;
  nest_info_t *dp ;
  int P ;

  if ( stage == NULL )
  {
    rsl_ready_merge( d_p, n_p, msize_p ) ;
  }

  if (( lp = Xlist) == NULL ) 
  {
    *retval_p = -1 ;
    return ;
  }
  Xlist = Xlist->next ;
  dp = (nest_info_t *)(lp->data) ;

  s_ig = dp->ig ;
  s_jg = dp->jg ;
  s_i = s_ig + s_ninfo->idif ;
  s_j = s_jg + s_ninfo->jdif ;
  s_cm = dp->cm ;
  s_cn = dp->cn ;
  s_pig = ID_IDEX( dp->mother_id ) ;
  s_pjg = ID_JDEX( dp->mother_id ) ;

  *ig_p = s_ig + 1 ;
  *jg_p = s_jg + 1;
  *i_p = s_i + 1 ;
  *j_p = s_j + 1;
  *cm_p = s_cm + 1;
  *cn_p = s_cn + 1;
  *pig_p = s_pig + 1;
  *pjg_p = s_pjg + 1;

/*
this should only be freed when the list is recalculated 960605
  RSL_FREE(lp) ;
  RSL_FREE(dp) ;
*/

  *retval_p = 1 ;
  return ;
}

/*@
  RSL_TO_PARENT_MSG -- Pack feedback data into a message for a parent point.

  Notes:
  RSL_TO_PARENT_MSG is used in a loop to pack messages for feeding back
  data from
  a nested domain to a parent in preparation for
  a call to RSL_MERGE_MSGS.  For an overview of the mechanism and a detailed
  example, see RSL_MERGE_MSGS.

  Before calling RSL_TO_PARENT_MSG, RSL must have been put into the
  correct state; that is, ready to accept data that will be sent to a
  particular parent domain point.  This is done by first calling
  RSL_TO_PARENT_INFO.  RSL_TO_PARENT_MSG may be called as many times as
  necessary to pack data into the message (or not at all, if there is
  no data for the point returned by RSL_TO_PARENT_INFO).  Each call to
  RSL_TO_PARENT_MSG copies Arg1 bytes from the bufffer specified by
  Arg2 into the message, which is allocated by RSL and never
  manipulated directly by the user program.  The amount of data that
  can be packed is limited to the message size that was specified in
  the first call to RSL_TO_PARENT_INFO.

  See also:
  RSL_MERGE_MSGS, RSL_TO_PARENT_INFO

@*/

RSL_TO_PARENT_MSG ( nbuf_p, buf )
  int_p
    nbuf_p ;            /* (I) Number of bytes to be packed. */
  char *
    buf ;               /* (I) Buffer containing the data to be packed. */
{
  int kiddex ;
  int nbuf ;
  int P ;

  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;
  nbuf = *nbuf_p ;
  kiddex = INDEX_2(s_jg,s_ig,s_mlen_nst) ;
  P = s_ndomain[ kiddex ].mother_P ;
  if ( stage[ kiddex ].p == NULL )
  {
    stage[ kiddex ].p = RSL_MALLOC( char, s_msize ) ;
    stage[ kiddex ].curs = 0 ;
    stage[ kiddex ].P = P ;
    stage[kiddex].next = Plist[P] ;
    Plist[P] =  &(stage[ kiddex ]) ;
  }
  if ( stage[ kiddex ].curs + nbuf > s_msize ) 
  {
    sprintf(mess,
    "RSL_TO_PARENT_MSG: would overflow buffer (%d+%d>%d)\n",
        stage[ kiddex ].curs, nbuf, s_msize ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }

  /* add point to head of list of points for processor P */
  stage[kiddex].kid_id = POINTID(s_nst,s_jg,s_ig) ;
  stage[kiddex].parent_id = POINTID( s_d, s_pjg, s_pig ) ;
  stage[kiddex].cm = s_cm ;
  stage[kiddex].cn = s_cn ;
  Psize[P] += s_msize + sizeof( merge_point_desc_t ) ;

  /* pack the buffer associated with stage[kiddex] */
#ifdef crayx1
  if( nbuf == sizeof(float) ) {
    float *bufin  = (float *) buf;
    float *bufout = (float *)&(stage[ kiddex ].p [ stage[ kiddex].curs]);
    bufout[0] = bufin[0];
  }
  else {
    bcopy( buf, &(stage[ kiddex ].p[ stage[ kiddex ].curs ]), nbuf ) ;
  }
  stage[ kiddex ].curs += nbuf ;
#else
  bcopy( buf, &(stage[ kiddex ].p[ stage[ kiddex ].curs ]), nbuf ) ;
  stage[ kiddex ].curs += nbuf ;
#endif
}

#ifdef crayx1
RSL_TO_PARENT_MSGX ( n_vals_p, s_vals_p, stride_p, buf )
  int_p
    n_vals_p ;            /* (I) Number of values to be packed. */
  int_p
    s_vals_p ;            /* (I) Size of values to be packed. */
  int_p
    stride_p ;            /* (I) Number of values for stride. */
  char *
    buf ;                 /* (I) Buffer containing the data to be packed. */
{
  int n_vals, s_vals, stride;
  int kiddex, nbuf, P;
                                                                                                    
  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;
  n_vals = *n_vals_p;
  s_vals = *s_vals_p;
  stride = *stride_p;
  nbuf   = n_vals * s_vals ;   /* Number of bytes to be packed */
  kiddex = INDEX_2(s_jg,s_ig,s_mlen_nst) ;
  P = s_ndomain[ kiddex ].mother_P ;
  if ( stage[ kiddex ].p == NULL )
  {
    stage[ kiddex ].p = RSL_MALLOC( char, s_msize ) ;
    stage[ kiddex ].curs = 0 ;
    stage[ kiddex ].P = P ;
    stage[kiddex].next = Plist[P] ;
    Plist[P] =  &(stage[ kiddex ]) ;
  }
  if ( stage[ kiddex ].curs + nbuf > s_msize )
  {
    sprintf(mess,
    "RSL_TO_PARENT_MSGX: would overflow buffer (%d+%d>%d)\n",
        stage[ kiddex ].curs, nbuf, s_msize ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  /* add point to head of list of points for processor P */
  stage[kiddex].kid_id = POINTID(s_nst,s_jg,s_ig) ;
  stage[kiddex].parent_id = POINTID( s_d, s_pjg, s_pig ) ;
  stage[kiddex].cm = s_cm ;
  stage[kiddex].cn = s_cn ;
  Psize[P] += s_msize + sizeof( merge_point_desc_t ) ;
   
  /* pack the buffer associated with stage[kiddex] */
  if ( s_vals == sizeof(float) ) {
    int k, ki;
    float *bufin = (float *)buf;
    float *bufout = (float *)&(stage[ kiddex ].p[ stage[ kiddex ].curs ]);
#pragma _CRI ivdep
    for ( k = 0, ki = 0; k < n_vals; k++, ki += stride ) {
          bufout[k] = bufin[ki];
    }
    stage[ kiddex ].curs += nbuf ;
  }
  else {
    sprintf(mess,
    "RSL_TO_PARENT_MSGX: Element size %d not supported for stride\n", s_vals);
    RSL_TEST_ERR( 1, mess ) ;
  }
}
#endif

/*@
  RSL_MERGE_MSGS -- Convey feedback data from nest to parent points.

  Notes:
  RSL_MERGE_MSGS is called once feedback data
  to points in a parent domain have been packed into messages
  from the associated points on the nest.  The routine has
  no arguments; rather, RSL must be in a ready state; this occurs
  once RSL_TO_PARENT_INFO has returned a value of 0 (zero)
  in its last argument on all processors.  The feedback data is
  conveyed along logical communication channels that were set up
  between nest points and associated points in the parent domain
  when the nest was spawned.  Interprocessor communication
  is generated for messages between
  points on different processors; otherwise, the transfer
  is done within the processor's local memory.

  The inverse operation to RSL_MERGE_MSGS is RSL_BCAST_MSGS, which
  is used for conveying forcing data from parent to nest.

  Note while reading the following example that RSL decomposes all
  domains independently and over all processors, so that every
  processor will have cells from the parent and from the nest.
  Thus, all processors perform both the packing of data from
  the nest and the unpacking of data onto the parent.  Other
  RSL routines appearing in the example are described elsewhere
  in these documents.

  Example:

$ C
$ C Packing data from the nest.
$ C
$   NLEV = (the number of vertical levels)
$   MSIZE = 3 * NLEV + 1 * WORDSIZE
$ C
$ C First call to rsl_to_parent_info
$ C
$   CALL RSL_TO_PARENT_INFO( PID, NID,     ! parent, nest domain descriptors
$                            MSIZE,        ! size of message to a point
$                            I,J,IG,JG,    ! local and global nest cell coords
$                            CM,CN,        ! index of nest cell in parent cell
$                            PIG,PJG,      ! global parent cell coords
$                            IRETVAL )     ! return value
$   DO WHILE ( IRETVAL .EQ. 1 )
$ C   Dot point variables feedback from the sw nest cell under each parent.
$     IF ( CM .EQ. 1 .AND. CN .EQ. 1 ) THEN
$       DO K = 1, NLEV
$         CALL RSL_TO_PARENT_MSG( WORDSIZE, U(I,J,K) )
$         CALL RSL_TO_PARENT_MSG( WORDSIZE, V(I,J,K) )
$       ENDDO
$     END IF
$ C   Cross point variables feedback from the center nest cell.
$     IF ( CM .EQ. 2 .AND. CN .EQ. 2 ) THEN
$       DO K = 1, NLEV
$         CALL RSL_TO_PARENT_MSG( WORDSIZE, T(I,J,K) )
$       ENDDO
$       CALL RSL_TO_PARENT_MSG( WORDSIZE, PS(I,J) )
$     ENDIF
$ C
$ C Subsequent calls to rsl_to_parent_info
$ C
$     CALL RSL_TO_PARENT_INFO( PID, NID, MSIZE, I,J,IG,JG, CM,CN,
$                              PIG,PJG, IRETVAL )
$   END DO
$ C
$ C Merge the data.
$ C
$   CALL RSL_MERGE__MSGS
$ C
$ C Unpack the data onto the parent.
$ C
$ C
$ C First call to rsl_from_child_info
$ C
$   CALL RSL_FROM_CHILD_INFO( I, J,       ! local parent cell coords
$                             IG, JG,     ! global parent cell coords
$                             CM, CN,     ! index of nest cell in parent cell
$                             NIG, NJG,   ! global nest cell coords
$                             IRETVAL )   ! return value
$   DO WHILE ( IRETVAL .EQ. 1 )
$     IF ( CM .EQ. 1 .AND. CN .EQ. 1 ) THEN
$       DO K = 1, NLEV
$         CALL RSL_FROM_CHILD_MSG( WORDSIZE, U(I,J,K) )
$         CALL RSL_FROM_CHILD_MSG( WORDSIZE, V(I,J,K) )
$       ENDDO
$     END IF
$     IF ( CM .EQ. 2 .AND. CN .EQ. 2 ) THEN
$       DO K = 1, NLEV
$         CALL RSL_FROM_CHILD_MSG( WORDSIZE, T(I,J,K) )
$       ENDDO
$       CALL RSL_FROM_CHILD_MSG( WORDSIZE, PS(I,J) )
$     END IF
$ C
$ C Subsequent calls to rsl_from_child_info
$ C
$     CALL RSL_FROM_CHILD_INFO( I, J, IG, JG, CM, CN, NIG, NJG, IRETVAL )
$   END DO

BREAKTHEEXAMPLECODE

  In this example, three 3-dimensional fields, U, V, and T, and one
  2-dimensional field, PS, from the parent domain are fed back from the
  entire nest (as opposed to the forcing example for RSL_BCAST_MSGS in
  which only the nest boundaries received data).  Assuming an Arakawa-B
  grid, selecting the center nest cell associated with a parent cell
  for cross-point data (T, PS) and the south-west nest cell for
  dot-point data (U, V) accounts for staggering of the dot and cross
  grids.
  WORDSIZE, PIG, and PJG are integers; otherwise implicit typing
  holds.

                          
  See also:
  RSL_TO_CHILD_INFO, RSL_FROM_PARENT_INFO, RSL_BCAST_MSGS

@*/

RSL_MERGE_MSGS ()
{
  int P ;
  char *work ;
  merge_point_desc_t pdesc ;
  stage_point_t *pt ;
  int curs ;
  int msglen, mdest, mtag ;
  int ii ;

  RSL_TEST_ERR( stage == NULL,
    "RSL_MERGE_MESSAGES: RSL_MESSAGE_TO_CHILD not called first" ) ;


  for ( ii = 0 ; ii < s_ninfo->Nmerge_send_Plist ; ii++ )
  {
    P = s_ninfo->merge_send_Plist[ii] ;
    msglen = s_ninfo->merge_send_Pnpts[ii]*( sizeof(pdesc) + s_msize ) 
             + sizeof(pdesc) ; /*  end of message marker */
    curs = 0 ;
    work = buffer_for_proc( P, msglen, RSL_SENDBUF ) ;
    /* NOTE ASSUMPTION that the number of points in Plist will
       be less or equal to ninfo->merge_send_Pnpts[ii].  If it isn't,
       we've got trouble.  */
    for ( pt = Plist[P] ; pt != NULL ; pt = pt->next )
    {
      RSL_TEST_ERR(curs+sizeof(merge_point_desc_t)+s_msize>msglen,
        "Internal error: would overwrite in merge messages.") ;
      pdesc.nest_id = pt->kid_id ;
      pdesc.parent_id = pt->parent_id ;
      pdesc.cm = pt->cm ;
      pdesc.cn = pt->cn ;
      bcopy( &pdesc, &work[curs], sizeof( merge_point_desc_t )) ;
      curs += sizeof( merge_point_desc_t ) ;
      bcopy( pt->p, &work[curs], s_msize ) ;
      curs += s_msize ;
    }
    RSL_TEST_ERR(curs+sizeof(merge_point_desc_t)>msglen,
      "Internal error: (end marker) would overwrite in merge messages.") ;
    /* add end marker */
    pdesc.nest_id = RSL_INVALID ;
    pdesc.parent_id = RSL_INVALID ;
    pdesc.cm = RSL_INVALID ;
    pdesc.cn = RSL_INVALID ;
    bcopy( &pdesc, &work[curs], sizeof( merge_point_desc_t )) ;
    curs += sizeof( merge_point_desc_t ) ;
    /* note that it is all right for mlen to be less than msglen */
    if ( rsl_c_comp2phys_proc(P) != rsl_myproc )
    {
      mdest = rsl_c_comp2phys_proc(P) ;
      mtag = MTYPE_FROMTO( MSG_TO_PARENT, rsl_myproc, mdest ) ;
      msglen = curs ;
      RSL_SEND( work, msglen, mtag, mdest ) ;
    }
    /* otherwise, leave in the send buffer and we'll pick it
       up later */
  }

  /* reset this for the next phase, in */
  s_child_msgs = NULL ;
  s_remaining = 0 ;
}

/* Return a point from some child processor each time called.
   If no more points, from a processor, go to the next one.
   If no more points at all, retval is returned as RSL_INVALID */

/*@
  RSL_FROM_CHILD_INFO -- Get the next cell in a unpacking sequence for feedback.

  Notes:
  RSL_FROM_CHILD_INFO is used in a loop to unpack messages containing
  feedback data from a nested domain.  The messages have arrived on the
  local processor as a result of a previous call to RSL_MERGE_MSGS.
  The domain descriptors do not need to be specified; they are part of
  the state of RSL as a result of the calls to RSL_TO_CHILD_INFO that
  have come before.  For a detailed example, see RSL_MERGE_MSGS.

  Each call to RSL_FROM_CHILD_INFO gives the local indices (Arg1, Arg2)
  and global indices (Arg3, Arg4) of a
  parent domain point receiving feedback data from a nest point
  whose the global indices are (Arg7, Arg8).
  The indices of the child cell in the set of nest cells associated with
  the parent are returned through arguments Arg5 and Arg6.  These
  specify which of the nest cells associated with the parent
  is being referred to.  There are IRAX_M by IRAX_N nest points
  associated with each parent where IRAX_M is the nesting ratio in the
  M dimension and IRAX_N is the nesting ratio in the N dimension (See
  the descriptions for the RSL nest spawning routines RSL_SPAWN...).

  RSL_FROM_CHILD_INFO will return a new set of coordinates for every
  parent domain point stored in local processor memory.  For each
  point, RSL_FROM_CHILD_INFO returns a value of 1 through
  Arg9.   RSL is left in a state ready to yield
  data from the nest point; the data
  is unpacked from the message by calling RSL_FROM_CHILD_MSG.  Once
  all local nest points have been traversed, RSL_FROM_CHILD_INFO returns
  a value of 0 (zero) in Arg9.

  It isn't necessary that anything be unpacked with the coordinates
  that are returned.  However, once called, RSL_FROM_CHILD_INFO must
  be called as many times as it takes to exhaust the number of points;
  otherwise the underlying RSL mechanism will not be left in the proper
  state at the conclusion of the broadcast.

  See also:
  RSL_MERGE_MSGS, RSL_TO_PARENT_INFO, RSL_FROM_CHILD_MSG
@*/

RSL_FROM_CHILD_INFO ( i_p, j_p, ig_p, jg_p, cm_p, cn_p,
                       nig_p, njg_p, retval_p )
  int_p
    i_p         /* (O) Local index in M dimension of parent. */
   ,j_p         /* (O) Local index in N dimension of parent. */
   ,ig_p        /* (O) Global index in M dimension of parent. */
   ,jg_p        /* (O) Global index in N dimension of parent. */
   ,cm_p        /* (O) M index of child cell beneath parent cell. */
   ,cn_p        /* (O) N index of child cell beneath parent cell. */
   ,nig_p       /* (O) Global index in M dimension of nest. */
   ,njg_p       /* (O) Global index in N dimension of nest. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  int ii ;
  merge_point_desc_t pdesc ;

  if ( s_remaining > 0 ) 
  {
    s_child_msgs_curs += s_remaining ;
    s_remaining = 0 ;
  }
  get_a_new_merge_point( retval_p ) ;
  if ( *retval_p != 1 )
  {
    cleanup_after_merge() ;
    return ; 
  }
  s_remaining = s_msize + sizeof(merge_point_desc_t) ;

  /* at this point we have a non-null message buffer */
  /* read the descriptor */
  bcopy( &(s_child_msgs[s_child_msgs_curs]),
         &pdesc,
         sizeof(merge_point_desc_t)) ;
  s_child_msgs_curs += sizeof(merge_point_desc_t) ;
  s_remaining -= sizeof(merge_point_desc_t) ;

  /* get_a_new_merge_point should not be returning these */
  RSL_TEST_ERR( pdesc.nest_id == RSL_INVALID, "Internal error.") ;

  *ig_p = ID_IDEX(pdesc.parent_id)+1 ;
  *jg_p = ID_JDEX(pdesc.parent_id)+1 ;
  *i_p = *ig_p + s_dinfo->idif ;
  *j_p = *jg_p + s_dinfo->jdif ;
  *nig_p = ID_IDEX(pdesc.nest_id)+1 ;
  *njg_p = ID_JDEX(pdesc.nest_id)+1 ;
  *cm_p = pdesc.cm+1 ;
  *cn_p = pdesc.cn+1 ;

  *retval_p = 1 ;
  return ;
}

/*@
  RSL_FROM_CHILD_MSG -- Unpack feedback data into a parent point.

  Notes:
  RSL_FROM_CHILD_MSG is used in a loop to unpack messages containing
  feedback data from a nested domain.  For an overview of the mechanism
  and a detailed example, see RSL_MERGE_MSGS.

  Before calling RSL_FROM_CHILD_MSG, RSL must have been put into the
  correct state; that is, ready to accept data that will be sent to a
  particular point in the nest.  This is done by first calling
  RSL_FROM_CHILD_INFO.  RSL_FROM_CHILD_MSG may then be called as many
  times as necessary to unpack data from the message (or not at all, if
  there is no data for the point).  Each call to RSL_FROM_CHILD_MSG
  copies Arg1 bytes from message into the bufffer specified by Arg2.
  Note that the message is allocated and handled entirely within RSL
  and never manipulated directly by the user program.  The amount of
  data that can be packed is limited to the message size that was
  specified in the first call to RSL_TO_PARENT_INFO.

  See also:
  RSL_BCAST_MSGS, RSL_FROM_CHILD_INFO, RSL_TO_PARENT_INFO

@*/

RSL_FROM_CHILD_MSG ( len_p, buf )
  int_p
    len_p ;             /* (I) Number of bytes to unpack. */
  char *
    buf ;               /* (O) Destination buffer. */
{
  if ( *len_p <= 0 ) return ;
  if ( *len_p > s_remaining ) 
  {
    sprintf(mess,
"RSL_FROM_CHILD_MSG:\n   Requested number of bytes (%d) exceeds %d, the number remaining for this point.\n", *len_p, s_remaining) ;
    RSL_TEST_WRN(1,mess) ;
  }
#ifdef crayx1
  if( (*len_p) == sizeof(float) ) {
    float *bufout = (float *)buf;
    float *bufin  = (float *)&(s_child_msgs[s_child_msgs_curs]);
    bufout[0]   = bufin[0];
  }
  else {
    bcopy( &(s_child_msgs[s_child_msgs_curs]),
         buf,
         *len_p ) ;
  }
  s_child_msgs_curs += *len_p ;
  s_remaining -= *len_p ;
#else
  bcopy( &(s_child_msgs[s_child_msgs_curs]),
         buf,
         *len_p ) ;
  s_child_msgs_curs += *len_p ;
  s_remaining -= *len_p ;
#endif
}

#ifdef crayx1
RSL_FROM_CHILD_MSGX ( n_vals_p, s_vals_p, stride_p, buf )
  int_p
    n_vals_p ;            /* (I) Number of values to be packed. */
  int_p
    s_vals_p ;            /* (I) Size of values to be packed. */
  int_p
    stride_p ;            /* (I) Number of values for stride. */
  char *
    buf ;                 /* (O) Buffer containing the unpacked data. */
{
  int n_vals, s_vals, stride, len;
  n_vals = *n_vals_p;
  s_vals = *s_vals_p;
  stride = *stride_p;
  len    = n_vals * s_vals;  /* Number of bytes to unpack */
  if ( len <= 0 ) return ;
  if ( len > s_remaining )
  {
    sprintf(mess,
"RSL_FROM_CHILD_MSGX:\n   Requested number of bytes (%d) exceeds %d, the number remaining for this point.\n", len, s_remaining) ;
    RSL_TEST_WRN(1,mess) ;
  }
  if ( (s_vals) == sizeof (float) ) {
    int k, ki;
    float *bufout = (float *)buf;
    float *bufin  = (float *)&(s_child_msgs[s_child_msgs_curs]);
                                                                 
#pragma _CRI ivdep
    for ( k = 0, ki = 0; k < n_vals; k++, ki += stride ) {
          bufout[ki] = bufin[k];
    }
    s_child_msgs_curs += len ;
    s_remaining -= len ;
  }
  else {
    sprintf(mess, "RSL_FROM_CHILD_MSGX: Element size %d not supported for stride\n", s_vals);
    RSL_TEST_WRN(1,mess) ;
  }
}
#endif

/* This is called by RSL_FROM_CHILD_INFO on a parent domain each time we
   need a received point from a nest.

   When this is called, one of two states may obtain.

   1. The first time this is called for a given merge operation, the global
   pointer s_child_msgs will be equal to NULL.

   2. Subsequent times this is called, the s_child_msgs pointer will be
   non-null and point to a buffer that was returned by a previous call
   to this routine.  The integer cursor s_child_msgs_curs will always
   be the index of the starting byte of a point descriptor of type
   merge_point_desc_t. 
   
     a. If the descriptor is a valid point, we return
     without doing anything.

     b. If the descriptor is a special invalid descriptor, this marks
     the end of a set of messages from a processor.  Get the next 
     buffer and assign it to s_child_msgs, and set the cursors appropriately.

        i. If there are no more messages, the s_child_msgs is set to
           null and *retval_p is set to -1, indicating that we are 
           finished with this set of messages for the merge operation.


*/
get_a_new_merge_point( retval_p )
  int_p retval_p ;
{
  int result, mtag, ii ;
  merge_point_desc_t pdesc ;

  do {
    if ( s_child_msgs != NULL )
    {
#if 0
      pdesc = *((merge_point_desc_t *)(&(s_child_msgs[s_child_msgs_curs]))) ;
#else
/* djs 1/98 */
      bcopy( &s_child_msgs[s_child_msgs_curs]
           , &pdesc
           , sizeof(merge_point_desc_t)
           ) ;
#endif
      if ( pdesc.nest_id != RSL_INVALID )
      {
        *retval_p = 1 ;
        return ;                /* 2.a. */
      }
    }
    /* are there outstanding messages? */
    for ( ii = 0 ; ii < s_ninfo->Nmerge_recv_Plist ; ii++ )
    {
      if ( s_ninfo->merge_recv_Ptags[ii] != RSL_INVALID )
      {
        break ;
      }
    }
    if ((ii == s_ninfo->Nmerge_recv_Plist)||(s_ninfo->Nmerge_recv_Plist <= 0))
    {
      *retval_p = -1 ;
      s_child_msgs = NULL ;
      return ;                  /* 2.b.i */
    }
    /* scan till we get a message */
    ii = 0 ;
    result = 1 ;
    while (1)
    {
      if ( s_ninfo->merge_recv_Ptags[ii] != RSL_INVALID )
      {
        if ( rsl_c_comp2phys_proc(s_ninfo->merge_recv_Plist[ii]) != rsl_myproc )
        {
          mtag = s_ninfo->merge_recv_Ptags[ii] ;
          if ( rsl_noprobe == NULL )
          {
            RSL_PROBE( mtag, &result ) ;
          }
          /* else, result will always be 1 */
          if ( result )
          {
#ifdef PGON
            if ( rsl_noprobe != NULL ) RSL_RECVEND ( mtag ) ;
#else
            RSL_RECVEND ( mtag ) ;
#endif
            s_ninfo->merge_recv_Ptags[ii] = RSL_INVALID ;
            s_child_msgs =
               buffer_for_proc( s_ninfo->merge_recv_Plist[ii], 0, RSL_RECVBUF ) ;
            break ;
          }
        }
        else
        {
          /* code to handle data from myself, which will be in my send buffer */
          s_ninfo->merge_recv_Ptags[ii] = RSL_INVALID ;
          s_child_msgs =
             buffer_for_proc( s_ninfo->merge_recv_Plist[ii], 0, RSL_SENDBUF ) ;
                                                          /*    ^^^^^^^^^^^  */
                                                          /* because data is */
                                                          /* from myself     */
          break ;
        }
      }
      if ( ++ii >= s_ninfo->Nmerge_recv_Plist ) ii = 0 ;
    }
    s_child_msgs_curs = 0 ;
#if 0
    pdesc = *((merge_point_desc_t *)(&(s_child_msgs[s_child_msgs_curs]))) ;
#else
/* djs 1/98 */
    bcopy ( &s_child_msgs[s_child_msgs_curs]
          , &pdesc
          , sizeof(merge_point_desc_t)
          ) ;
#endif
  } while ( pdesc.nest_id == RSL_INVALID ) ;
  *retval_p = 1 ;
}


post_receives_from_child()
{
  int ii, msglen, P, mtag, mfrom ;
  char * work ;

  for ( ii = 0 ; ii < s_ninfo->Nmerge_recv_Plist ; ii++ )
  {
    P = s_ninfo->merge_recv_Plist[ii] ;
    mfrom = rsl_c_comp2phys_proc(P) ;
    if ( mfrom != rsl_myproc )
    {
      msglen = s_ninfo->merge_recv_Pnpts[ii]*(sizeof(merge_point_desc_t)+s_msize)
               + sizeof(merge_point_desc_t) ;    /* end marker */
      work = buffer_for_proc(P, msglen, RSL_RECVBUF) ;
      mtag = MTYPE_FROMTO( MSG_TO_PARENT, mfrom, rsl_myproc ) ;
      RSL_RECVBEGIN( work, msglen, mtag ) ;
      s_ninfo->merge_recv_Ptags[ii] = mtag ; /* store tag */
    }
    else
    {
      /* next statement is just diagnostic */
      msglen = s_ninfo->merge_recv_Pnpts[ii]*(sizeof(merge_point_desc_t)+s_msize)
               + sizeof(merge_point_desc_t) ;    /* end marker */
      /* set the tag so we know to unpack the send buffer 
         for data from ourself */
      mtag = MTYPE_FROMTO( MSG_TO_PARENT, mfrom, rsl_myproc ) ;
      s_ninfo->merge_recv_Ptags[ii] = mtag ;
    }
  }
}

rsl_ready_merge( d_p, n_p, msize_p )
  int_p d_p, n_p, msize_p ;
{
  int i ;
  nest_info_t *dp ;
  rsl_list_t *lp ;
  rsl_child_info_t * kid ;
  rsl_point_t *pt ;
  int kidid ;
  int ig, jg, cn, cm ;
  int P ;

  s_msize = *msize_p ;
  s_d = *d_p ;
  s_nst = *n_p ;
  RSL_TEST_ERR( stage != NULL,
    "rsl_ready_merge: called again before RSL_MERGE_MSGS of previous call.") ;
  RSL_TEST_ERR( s_d < 0 || s_d > RSL_MAXDOMAINS,
    "rsl_ready_merge: bad parent domain descriptor" ) ;
  RSL_TEST_ERR( s_nst < 0 || s_nst > RSL_MAXDOMAINS,
    "rsl_ready_merge: bad nested domain descriptor" ) ;
  RSL_TEST_ERR( s_d == s_nst,
    "rsl_ready_merge: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[s_nst].parent != s_d ,
    "rsl_ready_merge: the nest is not a child of the parent" ) ;
 
  s_dinfo = &( domain_info[s_d]) ;
  s_ninfo = &( domain_info[s_nst]) ;
  RSL_TEST_ERR( s_dinfo->valid != RSL_VALID,
    "rsl_ready_merge: invalid parent domain" ) ;
  RSL_TEST_ERR( s_ninfo->valid != RSL_VALID,
    "rsl_ready_merge: invalid nested domain" ) ;
  s_ddomain = s_dinfo->domain ;
  s_ndomain = s_ninfo->domain ;

  s_mlen = s_dinfo->len_m ;
  s_nlen = s_dinfo->len_n ;
  s_mlen_nst = s_ninfo->len_m ;
  s_nlen_nst = s_ninfo->len_n ;
  s_irax_n = s_ninfo->irax_n ;
  s_irax_m = s_ninfo->irax_m ;

  if ( s_dinfo->child_merge_compiled[s_nst] != 1 ||
       s_ninfo->parent_merge_compiled != 1 )
  {
    rsl_comp_merge( d_p, n_p ) ;
    if ( s_ninfo->merge_Xlist != NULL )
    {
      destroy_list( &(s_ninfo->merge_Xlist), destroy_nest_info ) ;
    }
    s_ninfo->merge_Xlist = NULL ;
  }

  post_receives_from_child() ;

  /* stage will be NULL here because rsl_ready_merge is only called
     if stage is NULL (that's tested near the top of this routine).
     cleanup_after_merge deallocates all this malloc'd storage */

                                                             /* v-paranoid */
  stage = RSL_MALLOC( stage_point_t , s_mlen_nst * s_nlen_nst * 2 ) ;
                                                             /* ^-paranoid */
  stage_len = s_mlen_nst * s_nlen_nst ;  /* 96/3/15 */
  for ( i = 0 ; i < stage_len ; i++ )
  {
    stage[i].p = NULL ;
  }

  /* construct the list of local nested points that go to parent points */
#if 0
  if ( Xlist != NULL ) destroy_list( &(Xlist), destroy_nest_info ) ;
  Xlist = NULL ;
#endif
  if ( s_ninfo->merge_Xlist == NULL )
  {

  /* traverse backwards so that Xlist can be constructed easily frontwards */
    for ( jg = s_nlen_nst-1 ; jg >=0 ; jg-- )
    {
      for ( ig = s_mlen_nst-1 ; ig >= 0 ; ig-- )
      {
        pt = &(s_ndomain[INDEX_2(jg,ig,s_mlen_nst)]) ;
        if ( pt->valid == RSL_VALID &&
             rsl_c_comp2phys_proc(pt->P) == rsl_myproc )
        {
          dp = RSL_MALLOC( nest_info_t, 1 ) ;
          dp->ig = ig ;
          dp->jg = jg ;
          dp->cn = pt->which_kid_am_i_n ;
          dp->cm = pt->which_kid_am_i_m ;
          dp->mother_id = pt->mother_id ;
          lp = RSL_MALLOC( rsl_list_t, 1 ) ;
          lp->data = dp ;
          lp->next = s_ninfo->merge_Xlist ;
          s_ninfo->merge_Xlist = lp ;
        }
      }
    }
  }
  Xlist = s_ninfo->merge_Xlist ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    Plist[P] = NULL ;
  }
}



/* now used internally only */
rsl_comp_merge( d_p, n_p )
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
        "rsl_comp_merge: bad parent domain descriptor") ;
  RSL_TEST_ERR( nst < 0 || nst >= RSL_MAXDOMAINS, 
        "rsl_comp_merge: bad nested domain descriptor") ;
  RSL_TEST_ERR( d == nst, 
        "rsl_comp_merge: domain cannot merge to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
        "rsl_comp_merge: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
        "rsl_comp_merge: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
        "rsl_comp_merge: invalid nested domain" ) ;

  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  mlen_nst = ninfo->len_m ;
  nlen_nst = ninfo->len_n ;
  ddomain = dinfo->domain ;
  ndomain = ninfo->domain ;
  irax_n = ninfo->irax_n ;
  irax_m = ninfo->irax_m ;

  destroy_merge_compilation( d_p, n_p ) ;

  if ( dinfo->decomposed != 1 )
     default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  if ( ninfo->decomposed != 1 )
     default_decomposition( n_p,
                           &(domain_info[*n_p].loc_m),
                           &(domain_info[*n_p].loc_n) ) ;

  /* begin by computing the receive list */
  for ( i = 0 ; i < rsl_nproc_all ; i++ ) 
    ninfo->merge_recv_Pnpts[i] = 0 ;

  for ( jgn = 0 ; jgn < nlen_nst ; jgn++ )
  {
    for ( ign = 0 ; ign < mlen_nst ; ign++ )
    {
      pt = &(ndomain[INDEX_2(jgn,ign,mlen_nst)]) ;
      if ( pt->valid == RSL_VALID &&
           rsl_c_comp2phys_proc(pt->mother_P) == rsl_myproc )
      {
        if ( pt->valid == RSL_VALID )
          (ninfo->merge_recv_Pnpts[pt->P])++ ; /* count this point as 
                                                  coming from the nest
                                                  processor */
      }
    }
  }
  /* compress and copy the plist */
  ninfo->Nmerge_recv_Plist = 0 ;
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    if ( ninfo->merge_recv_Pnpts[P] > 0 )
    {
      ninfo->merge_recv_Pnpts[ninfo->Nmerge_recv_Plist] =
                                        ninfo->merge_recv_Pnpts[P] ;
      ninfo->merge_recv_Plist[ninfo->Nmerge_recv_Plist] = P ;
      (ninfo->Nmerge_recv_Plist)++ ;
    }
  }

  /* now compute the send list */
  for ( i = 0 ; i < rsl_nproc_all ; i++ ) 
    ninfo->merge_send_Pnpts[i] = 0 ;

  for ( jgn = 0 ; jgn < nlen_nst ; jgn++ )
  {
    for ( ign = 0 ; ign < mlen_nst ; ign++ )
    {
      pt = &(ndomain[INDEX_2(jgn,ign,mlen_nst)]) ;
      if ( pt->valid == RSL_VALID && 
           rsl_c_comp2phys_proc(pt->P) == rsl_myproc )
      {
        ninfo->merge_send_Pnpts[pt->mother_P]++ ;
      }
    }
  }
  /* compress and copy the plist */
  ninfo->Nmerge_send_Plist = 0 ;
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    if ( ninfo->merge_send_Pnpts[P] > 0 )
    {
      ninfo->merge_send_Pnpts[ninfo->Nmerge_send_Plist] =
                                        ninfo->merge_send_Pnpts[P] ;
      ninfo->merge_send_Plist[ninfo->Nmerge_send_Plist] = P ;
      (ninfo->Nmerge_send_Plist)++ ;
    }
  }

  dinfo->child_merge_compiled[nst] = 1 ;
  ninfo->parent_merge_compiled = 1 ;
}

cleanup_after_merge()
{
  int i ;
  if ( stage != NULL )
  {
    for ( i = 0 ; i < stage_len ; i++ )
    {
      if ( stage[i].p != NULL ) RSL_FREE( stage[i].p ) ;  /* 96/3/15 */
    }
    RSL_FREE( stage ) ;
  }
  stage = NULL ;
  s_msize = RSL_INVALID ;
  s_dinfo = NULL ;
  s_ninfo = NULL ;
  s_ddomain = NULL ;
  s_ndomain = NULL ;
  s_child_msgs = NULL ;
  s_child_msgs_curs = RSL_INVALID ;
}

destroy_merge_compilation( d_p, n_p )
  int_p d_p, n_p ;
{
  int d, nst, P ;
  rsl_domain_info_t *dinfo, *ninfo ;
  rsl_point_t *ddomain, *ndomain, *pt ;

  d = *d_p ;
  nst = *n_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
        "rsl_comp_merge: bad parent domain descriptor") ;
  RSL_TEST_ERR( nst < 0 || nst >= RSL_MAXDOMAINS,
        "rsl_comp_merge: bad nested domain descriptor") ;
  RSL_TEST_ERR( d == nst,
        "rsl_comp_merge: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
        "rsl_comp_merge: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
        "rsl_comp_merge: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
        "rsl_comp_merge: invalid nested domain" ) ;
  
  ninfo->parent_merge_compiled = 0 ;
  dinfo->child_merge_compiled[nst] = 0 ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    ninfo->merge_recv_Pnpts[P] = 0 ;
    ninfo->merge_recv_Plist[P] = RSL_INVALID ;
    ninfo->merge_recv_Ptags[P] = RSL_INVALID ;
    ninfo->Nmerge_recv_Plist = 0 ;
  }
}


