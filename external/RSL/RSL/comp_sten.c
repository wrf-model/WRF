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

/*
   Here's a drawing of the graph this routine is trying to construct:

      (stencil_desc)
        |      |
        |      |
        |    array of message_descs corresponding to stencil pts
        |      msg1 msg2 msg3 msg4 ...
        |       0    1    2    3     
        |                       \
   array of processor lists      \ <---pointers back to message structures
   on list for each domain        \        |
     |                            |        |
  d                               ^      \ v       +-------------------------+
  o (0) -->  procrec  -->         |       \        |there is one node for    |
  m            |                  |        \       |each physical processor  |
  a (1)        |                  |         \      |this processor will need |
  i            |                  ^          \     |to communicate with for  |
  n  .         |                  |           \    |this stencil.            |
     .         v                  |            ^   +-------------------------+
     .      (list_t)              |            |
               |  \               ^            ^     +-------------------------+
               | ( ptrec ) --> (list_t) --> (list_t) |1 for each msg associated|
               |     |                               |with the point           |
               |      \                              +-------------------------+
               v       \+-------------------------------------------------+
            (list_t)    |points to the entry for the local point in domain|
               |        |data structure.                                  |
               |        +-------------------------------------------------+
               .
               .
               .

**************
  Algorithm:

    P is a remote processor, M is me

 1  To work out sends to P from M:
 1.1   for each ghost point GP from P
 1.1.1   for each point p on GP's stencil
 1.1.1.1    if p is on M
 1.1.1.1.1     add p to list of points going to P (if not already on)
 1.1.1.1.2     add message from p to p's entry in the aforementioned list 

 2  To work out receives from P to M:
 2.1   for each ghost point GP from P
 2.1.1   for each point p on GP's stencil
 2.1.1.1    if p is on M
 2.1.1.1.1     add GP to list of points being sent from P (if not already on)
 2.1.1.1.2     add message from GP to GP's entry in the aforementioned list 

    Combined algorithm;
          
 1  To work out receives from P to M:
 1.1   for each ghost point GP from P
 1.1.1   for each point p on GP's stencil
 1.1.1.1    if p is on M
 1.1.1.1.1     add p to list of points going to P (if not already on)
 1.1.1.1.2     add message from p to p's entry in the aforementioned list 
 2.1.1.1.1     add GP to list of points being sent from P (if not already on)
 2.1.1.1.2     add message from GP to GP's entry in the aforementioned list 

*/

/* used by compile_stencil, below */
static stencil_desc_t *sd ;            /* set in compile_stencil */
static rsl_procrec_t *procrec ;        /* set in compile_stencil */
static int send_accum ;
static int recv_accum ;

#if 0
/* this is a linked list that is used for the receives from the remote
   processor (the GP list in the above algorithmic descriptions).  We are 
   only counting bytes for these messages (to allocated the correctly sized 
   buffers) so this is just a temporary data structure that is cleaned up 
   on each call to rsl_compile_stencil */
static rsl_list_t *recv_point_list = NULL ;   /* 940308 */
#endif

dstry_ptrec_list( recv_ptrec )
  rsl_ptrec_t *recv_ptrec ;
{
  destroy_list( &(recv_ptrec->recv_messages), NULL ) ;
}

rsl_processor_t idx_ ;

/* 1.1.1 (continued) */
/* this routine is called for each point on the ghost point's stencil */
check_local_pts( d, m, n, hm, hn, pt, ipt )
  rsl_index_t d ;               /* domain index */
  rsl_index_t m, n ;            /* this point */
  rsl_index_t hm, hn ;          /* home point (whose stencil I'm on)  */
  rsl_index_t pt ;              /* point in stencil */
  rsl_index_t ipt ;             /* inverse point in stencil */
{
  int mlen ;			/* length of minor domain dimension */
  rsl_fldspec_t *fp, *fpm, *prev, *new ;
  int message, found ;
  rsl_processor_t  P ;
  rsl_point_id_t id ;
  rsl_ptrec_t *ptrec, *recv_ptrec ;
  int recv_npts ;       /* dummy */
  rsl_list_t *lp ;
  message_desc_t *msg ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *domain ;

  dinfo = &(domain_info[d]) ;
  domain = dinfo->domain ;
  mlen = dinfo->len_m ;

  /* P is proc of ghost point */
  P = domain[INDEX_2(hn,hm,mlen)].P ;
/* 1.1.1.1 */
  if ( rsl_c_comp2phys_proc (domain[INDEX_2(n,m,mlen)].P) == rsl_myproc )
  {
/* 1.1.1.1.1 */
    id = POINTID(d,n,m) ;
    found = 0 ;
    for ( lp = procrec->point_list ; lp != NULL ; lp = lp->next )
    {
      ptrec = (rsl_ptrec_t *)lp->data ;
      if ( ptrec->pt->id == id )
      {
        found = 1 ;
        break ;
      }
    }
    if ( !found )       /* add it */
    {
      lp    = RSL_MALLOC( rsl_list_t, 1 ) ;
      ptrec = RSL_MALLOC( rsl_ptrec_t, 1 ) ;
      ptrec->pt = &(domain[INDEX_2(n,m,mlen)]) ;
      ptrec->ig = m ;
      ptrec->jg = n ;
      ptrec->nsendmsgs = 0 ;
      ptrec->nrecvmsgs = 0 ;
      ptrec->send_messages = NULL ;
      ptrec->recv_messages = NULL ;
      lp->data = ptrec ;
      lp->next = procrec->point_list ;
      procrec->point_list = lp ;
      procrec->npts++ ;
      send_accum += sizeof( rsl_point_hdr_t ) ;
    }

/* 2.1.1.1.1 */
    /* add the ghost point to the list of points from which we
       will receive messages */
    id = POINTID(d,hn,hm) ;
    found = 0 ;
    for ( lp = procrec->recv_point_list ; lp != NULL ; lp = lp->next )
    {
      recv_ptrec = (rsl_ptrec_t *)lp->data ;
      if ( recv_ptrec->pt->id == id )
      {
        found = 1 ;
        break ;
      }
    }
    if ( !found )       /* add it */
    {
      lp    = RSL_MALLOC( rsl_list_t, 1 ) ;
      recv_ptrec = RSL_MALLOC( rsl_ptrec_t, 1 ) ;
      recv_ptrec->pt = &(domain[INDEX_2(hn,hm,mlen)]) ;
      recv_ptrec->ig = hm ;
      recv_ptrec->jg = hn ;
      recv_ptrec->nsendmsgs = 0 ;
      recv_ptrec->nrecvmsgs = 0 ;
      recv_ptrec->send_messages = NULL ;
      recv_ptrec->recv_messages = NULL ;
      lp->data = recv_ptrec ;
      lp->next = procrec->recv_point_list ;
      procrec->recv_point_list = lp ;
      procrec->recv_npts++ ;
      recv_accum += sizeof( rsl_point_hdr_t ) ;
    }


/* 1.1.1.1.2 */
    /* at this point ptrec points to a ptrec (for the local point) in the 
       list for the non-local processor. */
    msg = sd->msgs[d][ pt-1 ] ;
    if ( msg != NULL )
    {
      /* iterate through message list for ptrec and add the message 
         to be sent to the ghost point from this local point to the list
         for the local point if it isn't there */
      for ( lp = ptrec->send_messages, found = 0 ; lp != NULL ; lp = lp->next )
      {
        if ( msg == ( message_desc_t * )lp->data )
        {
          found = 1 ;
          break ;
        }
      }
      if ( !found )   /* add it */
      {
        lp = RSL_MALLOC( rsl_list_t, 1 ) ;
        lp->data = msg ;
        lp->next = ptrec->send_messages ;
	lp->info1 = pt ;			/* index of stencil point */
        ptrec->send_messages = lp ;
        send_accum += message_size( msg ) ;
        send_accum += sizeof(int) ;		/* for send of stencil point index */
        ptrec->nsendmsgs++ ;
      }
    }

/* 2.1.1.1.2 */
    /* repeat for the receives, but note, that for the receives we
       are only interested in the size of the messages. Also, we're
       interested in the ghost-point, not the point.*/
    msg = sd->msgs[d][ ipt-1 ] ;     /* ipt instead of pt used for index */
    if ( msg != NULL )
    {
      /* iterate through message list for recv_ptrec and if message to be
         received is not there add it. */
      for ( lp = recv_ptrec->recv_messages, found = 0 ;
                                      lp != NULL ; lp = lp->next )
      {
        if ( msg == ( message_desc_t * )lp->data )
        {
          found = 1 ;
          break ;
        }
      }
      if ( !found )
      {
        lp = RSL_MALLOC( rsl_list_t, 1 ) ;
        lp->data = msg ;
        lp->next = recv_ptrec->recv_messages ;
	lp->info2 = ipt ;		/* inverse stencil point */
        recv_ptrec->recv_messages = lp ;
        recv_accum += message_size( msg ) ;
        recv_accum += sizeof(int) ;		/* for send of stencil point index */
        recv_ptrec->nrecvmsgs++ ;
        ptrec->nrecvmsgs = recv_ptrec->nrecvmsgs ;   /* intentional (ptrec) */
      }
    }
  }
}

check_sten ( s_p )
  int_p s_p ;
{
  int s ;

  s = *s_p ;
  RSL_TEST_ERR( s <= 0 || s > RSL_MAXDESCRIPTORS,
	"rsl_compile_stencil: bad stencil descriptor" ) ;
  RSL_TEST_ERR((sd = (stencil_desc_t *)sh_descriptors[s]) == NULL,
	"compile_descriptor: null stencil descriptor" ) ;
fprintf(stderr,"DEBUG CHECK_STEN: s %d, sd->tag %d\n", s, sd->tag ) ;
  RSL_TEST_ERR( sd->tag != STENCIL_DESC,
        "compile_descriptor: bad stencil descriptor" ) ;
}

/* this is now used internally only -- this will be called automatically
   whenever a stencil exchange is attempted on a stencil that has not
   yet been compiled */
rsl_compile_stencil( d_p, s_p )
  int_p d_p, s_p ;
{
  int d, s ;
  int i, j, k ;
  int len_plist ;
  int (*ptfcn)() ;
  rsl_list_t *lp, *lp2, *destr, *destr2, *ghost_points ;
  rsl_domain_info_t *dp ;
  rsl_point_t  *pt ;
  rsl_dimlen_t mlen, nlen ;
  int m, n ;
  rsl_processor_t P, Plist[RSL_MAXPROC] ;
  int check_local_pts() ;

  d = *d_p ;
  s = *s_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "rsl_compile_stencil: bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID, 
	"rsl_compile_stencil: descriptor for invalid domain" ) ;

  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;

  /* sd is static so that check_local_pts can get at it */
  RSL_TEST_ERR( s <= 0 || s > RSL_MAXDESCRIPTORS,
	"rsl_compile_stencil: bad stencil descriptor" ) ;
  RSL_TEST_ERR((sd = (stencil_desc_t *)sh_descriptors[s]) == NULL,
	"compile_descriptor: null stencil descriptor" ) ;
  RSL_TEST_ERR( sd->tag != STENCIL_DESC,
        "compile_descriptor: bad stencil descriptor" ) ;
  RSL_TEST_ERR( sd->compiled[d] != 0,
        "compile_stencil: stencil has already been compiled for this domain") ;

  sd->compiled[d] = 1 ;
  ptfcn = sd->f[d].ptfcn ;
  dp = &(domain_info[d]) ;

  if ( dp->decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  /* get a list of the processors that have ghost points and store in 
     Plist; len_plist is the number of processors stored */
  for ( i = 0 ; i < RSL_MAXPROC ; i++ )
    Plist[i] = 0 ;
  for ( lp = dp->ghost_pts ; lp != NULL ; lp = lp->next ) 
  {
    idx_ = ((rsl_point_t *)lp->data)->P ;
    if ( idx_ < 0 || idx_ >= RSL_MAXPROC )
    {
      sprintf(mess,"domain %d: idx_ = %d\n",d, idx_ );
      RSL_TEST_WRN(1,mess) ;
    }
    Plist[ idx_ ] = 1 ;
  }
  for ( len_plist = 0, i = 0 ; i < RSL_MAXPROC ; i++ )
    if ( Plist[i] == 1 ) Plist[ len_plist++ ] = i ;

  for ( k = 0 ; k < len_plist ; k++ )
  {
    P = Plist[k] ; 

    procrec = RSL_MALLOC( rsl_procrec_t, 1 ) ;
    procrec->P = P ;
    procrec->next = sd->procs[d] ;
    sd->procs[d] = procrec ;

#if 0
    destroy_list( &recv_point_list, dstry_ptrec_list ) ;
    recv_point_list = NULL ;
#endif

/* 1.1 */
    /* for every ghost point from P, mark any local point that lies
       on its stencil (using the point function associated with the
       stencil.   */
    procrec->npts = 0 ;
    procrec->recv_npts = 0 ;
    send_accum = 0 ;
    recv_accum = 0 ;
    for ( n = 0 ; n < nlen ; n++ )
      for ( m = 0 ; m < mlen ; m++ )
        if ( dp->domain[ INDEX_2( n, m, mlen ) ].P == P )
        {
/* 1.1.1 */
           (*ptfcn)( d, m, mlen, n, nlen, check_local_pts ) ;
        }

    procrec->nsends = 0 ;
    procrec->nrecvs = 0 ;
    procrec->sendsize = send_accum + sizeof(int) ; /* extra word for count */
    procrec->recvsize = recv_accum + sizeof(int) ; /* extra word for count */
  }
#define NEW
#ifdef NEW
  {
    int ig, jg, i, j ;
    rsl_list_t *lp1 ;
    void * base ;
    int elemsz, t0, t1, pack_table_size ;
    stencil_desc_t * sten ;
    rsl_ptrec_t * ptrec ;
    message_desc_t * msg ;
    rsl_fldspec_t * fld ;

    sten = (stencil_desc_t *) sh_descriptors[ s ] ;
    for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
    {
      init_process_refs() ;
      for ( lp = procrec->point_list ; lp != NULL ; lp = lp->next )
      {
        ptrec = (rsl_ptrec_t *) lp->data ;
        ig = ptrec->ig ;
        jg = ptrec->jg ;
        i = ig - domain_info[d].ilocaloffset ;
        j = jg - domain_info[d].jlocaloffset ;

        for ( lp1 = ptrec->send_messages ; lp1 != NULL ; lp1 = lp1->next )
        {
	  msg = (message_desc_t *) lp1->data ;
	  for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
	  {
            if ( fld->type >= 100 ) sten->has_f90_fields = 1 ;
	    base = fld->base ;
	    elemsz = fld->elemsz ;
	    switch (fld->strategy)
	    {
	    case MINNS_MAJEW_2D :		/* <MM> eg: psa(i,j) */
	      t0 = fld->llen[0] ;
	      store_process_refs( base, fld->f90_table_index, (i+j*t0)*elemsz, elemsz, 1, elemsz) ;
	      break ;
	    case MINEW_MAJNS_2D :		/* xxx(j,i) */
	      t0 = fld->llen[0] ;
	      store_process_refs( base, fld->f90_table_index, (j+i*t0)*elemsz, elemsz, 1, elemsz) ;
	      break ;
	    case MINNS_MAJEW_K_3D :		/* <MM> eg: ua(i,j,k) */
	      t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
	      store_process_refs( base, fld->f90_table_index, (i+j*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
	      break ;
	    case MINEW_MAJNS_K_3D :		/* <MM> eg: u(j,i,k) */
	      t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
	      store_process_refs( base, fld->f90_table_index, (j+i*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
	      break ;
	    case K_MIDNS_MAJEW_3D :		/* <MM> eg: u(k,i,j) */
	      t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
	      store_process_refs( base, fld->f90_table_index, (i*t0+j*t1)*elemsz, elemsz * t0, 1, elemsz) ;
	      break ;
	    case MINNS_K_MAJEW_3D :		/* <MM> eg: u(i,k,j) */
	      t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
	      store_process_refs( base, fld->f90_table_index, (i+j*t1)*elemsz, elemsz, fld->llen[1], t0*elemsz) ;
	      break ;
	    default:
	      RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
	      break ;
	    }
	  }
        }
      }
      process_refs( &(procrec->pack_table),
                    &(procrec->pack_table_size),
                    &(procrec->pack_table_nbytes), 1 ) ;
#if 0
      fprintf(stderr,"pack P = %3d:\n",procrec->P ) ;
      show_pack_table( procrec->pack_table,
                       procrec->pack_table_size,
                       procrec->pack_table_nbytes ) ;
#endif
    }
  }
  {
    int ig, jg, i, j ;
    rsl_list_t *lp1 ;
    void * base ;
    int elemsz, t0, t1, pack_table_size ;
    stencil_desc_t * sten ;
    rsl_ptrec_t * ptrec ;
    message_desc_t * msg ;
    rsl_fldspec_t * fld ;
    
    sten = (stencil_desc_t *) sh_descriptors[ s ] ;
    for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
    {
      init_process_refs() ;
      for ( lp = procrec->recv_point_list ; lp != NULL ; lp = lp->next )
      {
        ptrec = (rsl_ptrec_t *) lp->data ;
        ig = ptrec->ig ;
        jg = ptrec->jg ;
        i = ig - domain_info[d].ilocaloffset ;
        j = jg - domain_info[d].jlocaloffset ;

        for ( lp1 = ptrec->recv_messages ; lp1 != NULL ; lp1 = lp1->next )
        {
	  msg = (message_desc_t *) lp1->data ;
	  for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
	  {
            if ( fld->type >= 100 ) sten->has_f90_fields = 1 ;
	    base = fld->base ;
	    elemsz = fld->elemsz ;
            switch (fld->strategy)
            {
            case MINNS_MAJEW_2D :               /* <MM> eg: psa(i,j) */
              t0 = fld->llen[0] ;
              store_process_refs( base, fld->f90_table_index, (i+j*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;
            case MINEW_MAJNS_2D :               /* xxx(j,i) */
              t0 = fld->llen[0] ;
              store_process_refs( base, fld->f90_table_index, (j+i*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;
            case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_process_refs( base, fld->f90_table_index, (i+j*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
              break ;
            case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_process_refs( base, fld->f90_table_index, (j+i*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
              break ;
            case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_process_refs( base, fld->f90_table_index, (i*t0+j*t1)*elemsz, elemsz * t0, 1, elemsz) ;
              break ;
            case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_process_refs( base, fld->f90_table_index, (i+j*t1)*elemsz, elemsz, fld->llen[1], t0*elemsz) ;
              break ;
            default:
              RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
              break ;
            }
	  }
        }
      }
      process_refs( &(procrec->unpack_table),
                    &(procrec->unpack_table_size),
                    &(procrec->unpack_table_nbytes), 1 ) ;
#if 0
      fprintf(stderr,"upack P = %3d:\n",procrec->P ) ;
      show_pack_table( procrec->unpack_table,
                       procrec->unpack_table_size,
                       procrec->unpack_table_nbytes ) ;
#endif
    }
  }
#endif
}

show_pack_table( pack_table, pack_table_size, pack_table_nbytes )
  packrec_t pack_table[] ;
  int pack_table_size ;
  int pack_table_nbytes ;
{
  int i,ii,jj ;
  for ( i = 0 ; i < pack_table_size ; i++ )
  {
    fprintf(stderr,
"  base %08x %12d offset %10d f90 index %d n %3d nelem %5d stride %5d valid %2d\n",
	      pack_table[i].base,
	      pack_table[i].base,
	      pack_table[i].offset,
	      pack_table[i].f90_table_index,
	      pack_table[i].n,
	      pack_table[i].nelems,
	      pack_table[i].stride,
	      pack_table[i].valid ) ;
#if 0
    for ( jj = 0 ; jj < pack_table[i].nelems ; jj++ )
    for ( ii = 0 ; ii < pack_table[i].n ; ii += 4 )
    {
      fprintf(stderr,"** elem %d, n %d, %16lx, %f\n",jj,ii,
		(float *)( (char *)
		   pack_table[i].base + 
		   pack_table[i].offset +
		   jj * pack_table[i].stride +
		   ii ),
		*((float *)( (char *)
		   pack_table[i].base + 
		   pack_table[i].offset +
		   jj * pack_table[i].stride +
		   ii ))
		   ) ;
    }
#endif

  }
  fprintf(stderr," table nbytes=%d\n", pack_table_nbytes ) ;
}


/*@
  SHOW_STEN_DIAGS - Show run time information about stencil performance.

  Input parameter:
. d - domain descriptor
. s - stencil descriptor

  Synopsis:
  subroutine SHOW_STEN_DIAGS ( d, s )
  integer d
  integer s

  Notes:
  Information is sent to a file sten_diags_<pid> for each
  processor.

  See also:
  RSL_CREATE_STENCIL, RSL_DESCRIBE_STENCIL, RSL_EXCH_STENCIL

@*/

static int show_sten_diags_first = 1 ;

SHOW_STEN_DIAGS ( d_p, s_p )
  int_p d_p, s_p ;
{
  int d, s, P, nsends, nbytes ;
  stencil_desc_t *sp ;
  rsl_procrec_t *procrec ;
  rsl_ptrec_t *ptrec ;
  FILE *fp ;
  char fname[80], *code  ;
  int smsgs, rmsgs ;
  rsl_list_t *lp ;

  s = *s_p ;
  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, 
                            "show_sten_diags: bad domain descriptor") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
		"show_sten_diags: invalid domain descriptor" ) ;
  sp = (stencil_desc_t *)sh_descriptors[s] ;
  if ( sp == NULL ) return ;
  if ( show_sten_diags_first )
  {
    code = "w" ;
    show_sten_diags_first = 0 ;
  }
  else
  {
    code = "a" ;
  }
  sprintf(fname,"sten_diags_%04d",rsl_myproc) ;
  if (( fp = fopen ( fname, code )) == NULL )
  {
     perror(fname) ;
     exit(2) ;
  }
  fprintf(fp,"Diagnostics for stencil %3d, domain %3d\n",s,d) ;

  for ( procrec = sp->procs[d] ; procrec != NULL ; procrec = procrec->next )
  {
    smsgs = 0 ; rmsgs = 0 ;
    for ( lp = procrec->point_list ; lp != NULL ; lp = lp->next )
    {
      ptrec = (rsl_ptrec_t *) lp->data ;
      smsgs += ptrec->nsendmsgs ;
      rmsgs += ptrec->nrecvmsgs ;
    }
    fprintf(fp,"   to %4d : %5d of %10d bytes (%10d tot), pts %4d, msgs %4d\n",
            procrec->P,
            procrec->nsends,
            procrec->sendsize,
            procrec->nsends*procrec->sendsize,
            procrec->npts,
            smsgs ) ;
    fprintf(fp," from %4d : %5d of %10d bytes (%10d tot), pts %4d, msgs %4d\n",
            procrec->P,
            procrec->nrecvs,
            procrec->recvsize,
            procrec->nrecvs*procrec->recvsize,
            procrec->recv_npts,
            rmsgs ) ;
  }
  fclose(fp) ;
}

static FILE * fp = NULL ;
static int show_first = 1 ;
SHOW_MESSAGE ( mh_p )
  int_p mh_p ;
{
  int mh ;
  message_desc_t *msg ;
  rsl_fldspec_t  *fld ; 
  int dim ;
  char * code ;
  char fname[80] ;

  mh = *mh_p ;
  if ( show_first )
  {
    code = "w" ;
    show_first = 0 ;
    sprintf(fname,"show_def_%04d",rsl_myproc) ;
    if (( fp = fopen ( fname, code )) == NULL )
    {
       perror(fname) ;
       exit(2) ;
    }
  }
  if ( mh == RSL_INVALID )
  {
    fprintf(fp,"MESSAGE HANDLE: RSL_INVALID\n" ) ;
  }
  else
  {
    msg = (message_desc_t *)mh_descriptors[mh] ;
    show_message_desc( msg ) ;
  }
}

show_message_desc( msg )
  message_desc_t * msg ;
{
  rsl_fldspec_t  *fld ; 
  int dim ;
  int mh ;

  if ( msg == NULL ) return ;
  fprintf(fp,"MESSAGE HANDLE: %d\n",msg->mh ) ;
  fprintf(fp,"           tag: %d\n",msg->tag ) ;
  fprintf(fp,"         nflds: %d\n",msg->nflds ) ;
  for ( fld = msg->fldspecs ; fld != NULL ; fld=fld->next )
  {
    fprintf(fp,"  FLD:\n") ;
    fprintf(fp,"          base: %x\n",fld->base ) ;
    fprintf(fp,"          ndim: %d\n",fld->ndim ) ;
    fprintf(fp,"        elemsz: %d\n",fld->elemsz ) ;
    for ( dim = 0 ; dim < fld->ndim && dim < RSL_MAXDIM ; dim++ )
    {
      fprintf(fp,"       decomp[%3d]: %d\n",dim,fld->decomp[dim] ) ;
      fprintf(fp,"         gdex[%3d]: %d\n",dim,fld->gdex[dim] ) ;
      fprintf(fp,"         glen[%3d]: %d\n",dim,fld->glen[dim] ) ;
      fprintf(fp,"         llen[%3d]: %d\n",dim,fld->llen[dim] ) ;
    }
  }
}

/*@
  SHOW_STENCIL - Show information about the stencil structure

  Input parameter:
. s - domain descriptor

  Synopsis:
  subroutine SHOW_STENCIL ( s )
  integer s		

  Notes:
  Information is sent to a file show_def_<pid> for each
  processor.

  See also:
  RSL_CREATE_STENCIL, RSL_DESCRIBE_STENCIL

@*/

SHOW_STENCIL ( d_p, sh_p )
  int_p d_p ;
  int_p sh_p ;
{
  int sh, d ;
  int spt ;
  stencil_desc_t * sten ; 
  char * code ;
  char fname[80] ;

  d  = *d_p ;
  sh = *sh_p ;
  if ( show_first )
  {
    code = "w" ;
    show_first = 0 ;
    sprintf(fname,"show_def_%04d",rsl_myproc) ;
    if (( fp = fopen ( fname, code )) == NULL )
    {
       perror(fname) ;
       exit(2) ;
    }
  }
  sten = (stencil_desc_t *)sh_descriptors[sh] ;
  if ( sten == NULL ) return ;
  fprintf(fp,"STENCIL HANDLE: %d\n",sh ) ;
  fprintf(fp,"           tag: %d\n",sten->tag ) ;
  fprintf(fp,"          npts: %d\n",sten->npts[d] ) ;
  fprintf(fp,"        maskid: %d\n",sten->maskid[d] ) ;
  
  for ( spt = 0 ; spt < sten->npts[d] && spt < RSL_MAXSTEN+1 ; spt++ )
  {
    fprintf(fp,"    stencil pt: %d\n",spt ) ;
    show_message_desc( sten->msgs[d][spt] ) ;
  }

  /* code to show processor lists not here yet */

}

