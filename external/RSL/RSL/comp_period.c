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

/* The mechanism here is similar to and patterned after that used by the
   stencil mechanism: see comp_sten.c */


/* used by compile_period, below */
static period_desc_t *sd ;            /* set in compile_period */
static rsl_procrec_t *procrec ;       /* set in compile_period */
static int send_accum ;
static int recv_accum ;

static check_local_pts_period( d, m, n, hm, hn, min_gh, maj_gh, fldspec )
  rsl_index_t d ;               /* domain index */
  rsl_index_t m, n ;            /* this point */
  rsl_index_t hm, hn ;          /* home point (whose period I'm on)  */
  rsl_index_t min_gh ;        /* direction and amount to make sure minor ghost region is updated */
  rsl_index_t maj_gh ;        /* direction and amount to make sure major ghost region is updated */
  rsl_fldspec_t *fldspec ;
{
  int mlen ;                    /* length of minor domain dimension */
  rsl_fldspec_t *fp, *fpm, *prev, *new ;
  int message, found ;
  rsl_processor_t  P , Pthis , Pmin_gh , Pmaj_gh ;
  rsl_point_id_t id ;
  rsl_ptrec_t *ptrec, *recv_ptrec ;
  int recv_npts ;       /* dummy */
  rsl_list_t *lp ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *domain ;
  message_desc_t * msg ;
  int mfldlen, nfldlen ;
  rsl_fldspec_t *fld ;

  dinfo = &(domain_info[d]) ;
  domain = dinfo->domain ;
  mlen = dinfo->len_m ;

  switch ( fldspec->strategy )
  {
    case MINNS_MAJEW_2D :
      mfldlen = fldspec->glen[0] ; nfldlen = fldspec->glen[1] ; break ;
    case MINEW_MAJNS_2D :
      mfldlen = fldspec->glen[1] ; nfldlen = fldspec->glen[0] ; break ;
    case MINNS_MAJEW_K_3D :
      mfldlen = fldspec->glen[0] ; nfldlen = fldspec->glen[1] ; break ;
    case MINEW_MAJNS_K_3D :
      mfldlen = fldspec->glen[1] ; nfldlen = fldspec->glen[0] ; break ;
    case K_MIDNS_MAJEW_3D :
      mfldlen = fldspec->glen[1] ; nfldlen = fldspec->glen[2] ; break ;
    case MINNS_K_MAJEW_3D :
      mfldlen = fldspec->glen[0] ; nfldlen = fldspec->glen[2] ; break ;
    default :
      RSL_TEST_ERR(1,"unsupported strategy") ;
  }

/* P is the processor on which sits the off-domain point being filled in */
  P       = domain[INDEX_2( (hn<0)?0:((hn>nfldlen-1)?nfldlen-1:hn) , (hm<0)?0:((hm>mfldlen-1)?mfldlen-1:hm),mlen )  ].P ;
  Pmin_gh = domain[INDEX_2( (hn<0)?0:((hn>nfldlen-1)?nfldlen-1:hn) , (hm+min_gh<0)?0:((hm+min_gh>mfldlen-1)?mfldlen-1:hm+min_gh),mlen )  ].P ;
  Pmaj_gh = domain[INDEX_2( (hn+maj_gh<0)?0:((hn+maj_gh>nfldlen-1)?nfldlen-1:hn+maj_gh) , (hm<0)?0:((hm>mfldlen-1)?mfldlen-1:hm       ),mlen )  ].P ;

/* Pthis is the processor on which sits the on-domain point being replicated */
  Pthis = RSL_INVALID ;
  if ( n >= 0 && n < dinfo->len_n && m >= 0 && m < dinfo->len_m )
     Pthis = domain[INDEX_2(n,m,mlen)].P ;

/* SENDS -- if the point to be replicated sits on my processsor, and the off-domain point being filled
   in sits on the "other" processor, record a send that includes the coordinates of the point being
   replicated for the packing mechanism.  */

#if 1
  if (   rsl_c_comp2phys_proc ( Pthis ) == rsl_myproc  && 
       ( P == procrec->P || Pmin_gh == procrec->P || Pmaj_gh == procrec->P ) &&
         rsl_c_comp2phys_proc ( procrec->P ) != rsl_myproc )   /* if the other processor is me don't bother */
#else
  if ( rsl_c_comp2phys_proc ( Pthis ) == rsl_myproc  &&  P == procrec->P )
#endif
  {
#if 0
fprintf(stderr,"send: %d %d P = %d , Pthis = %d , procrec->P %d , m %d , n %d , hm %d , hn %d , min_gh %d , maj_gh %d \n", mfldlen, nfldlen,  P, Pthis, procrec->P, m,n,hm,hn,min_gh,maj_gh ) ;
#endif
    found = 0 ;
/* always create a new record; searching and trying to reuse records will throw
   off the order of packing when multiple fields with different dimensions are
   involved. */
    if ( !found )       /* add it */
    {
      lp    = RSL_MALLOC( rsl_list_t, 1 ) ;
      ptrec = RSL_MALLOC( rsl_ptrec_t, 1 ) ;
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
/* 1.1.1.1.2 */
    /* at this point ptrec points to a ptrec (for the local point) in the 
       list for the non-local processor. */
    if ( ptrec->send_messages == NULL )
    {
      msg = RSL_MALLOC( message_desc_t , 1 ) ;
      lp = RSL_MALLOC( rsl_list_t, 1 ) ;
      lp->data = msg ;
      lp->next = ptrec->send_messages ;
      ptrec->send_messages = lp ;
    }
    lp = ptrec->send_messages ;
    msg = lp->data ;
    fld = RSL_MALLOC( rsl_fldspec_t , 1 ) ;
    *fld = *fldspec ;
    fld->next = msg->fldspecs ;
    msg->fldspecs = fld ;
    send_accum += fldsize( fld ) ;
    send_accum += sizeof(int) ;         /* for send of period point index */
    ptrec->nsendmsgs = 1 ;
  }

/* RECEIVES: if the off-domain point to be filled in is on my processor, and the processor
   this with the on-domain point is the "other" processor, generate a receive, recording the
   coordinates of the off-domain point for the upacking mechanism.  */

#if 1
  if ( ( rsl_c_comp2phys_proc ( P ) == rsl_myproc || rsl_c_comp2phys_proc ( Pmaj_gh ) == rsl_myproc || rsl_c_comp2phys_proc ( Pmin_gh ) == rsl_myproc ) && 
         Pthis == procrec->P && 
         rsl_c_comp2phys_proc ( Pthis ) != rsl_myproc )   /* if the other processor is me don't bother */
#else
  if ( rsl_c_comp2phys_proc ( P ) == rsl_myproc && Pthis == procrec->P )
#endif
  {

#if 0
fprintf(stderr,"recv: %d %d P = %d , Pthis = %d , procrec->P %d , m %d , n %d , hm %d , hn %d , min_gh %d , maj_gh %d\n", mfldlen, nfldlen, P, Pthis, procrec->P, m,n,hm,hn,min_gh,maj_gh ) ;
#endif

/* 2.1.1.1.1 */
    /* add the ghost point to the list of points from which we
       will receive messages */
    found = 0 ;
    if ( !found )       /* add it */
    {
      lp    = RSL_MALLOC( rsl_list_t, 1 ) ;
      recv_ptrec = RSL_MALLOC( rsl_ptrec_t, 1 ) ;
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

    if ( recv_ptrec->recv_messages == NULL )
    {
      msg = RSL_MALLOC( message_desc_t , 1 ) ;
      lp = RSL_MALLOC( rsl_list_t, 1 ) ;
      lp->data = msg ;
      lp->next = recv_ptrec->recv_messages ;
      recv_ptrec->recv_messages = lp ;
    }
    lp = recv_ptrec->recv_messages ;
    msg = lp->data ;
    fld = RSL_MALLOC( rsl_fldspec_t , 1 ) ;
    *fld = *fldspec ;
    fld->next = msg->fldspecs ;
    msg->fldspecs = fld ;
    recv_accum += fldsize( fld ) ;
    recv_accum += sizeof(int) ;
    recv_ptrec->nrecvmsgs = 1 ;

  }
}

/* this is used internally only -- this will be called automatically
   whenever a period exchange is attempted on a period that has not
   yet been compiled */
rsl_compile_period( d_p, s_p )
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
  rsl_fldspec_t * fld ;
  message_desc_t *msg ;
  int m, n, dir ;
  rsl_processor_t P, Plist[RSL_MAXPROC] ;
  int check_local_pts_period() ;

  d = *d_p ;
  s = *s_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "rsl_compile_period: bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID, 
        "rsl_compile_period: descriptor for invalid domain" ) ;

  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;

  /* sd is static so that check_local_pts can get at it */
  RSL_TEST_ERR( s <= 0 || s > RSL_MAXDESCRIPTORS,
        "rsl_compile_period: bad period descriptor" ) ;
  RSL_TEST_ERR((sd = (period_desc_t *)pr_descriptors[s]) == NULL,
        "compile_descriptor: null period descriptor" ) ;
  RSL_TEST_ERR( sd->tag != PERIOD_DESC,
        "compile_descriptor: bad period descriptor" ) ;
  RSL_TEST_ERR( sd->compiled[d] != 0,
        "compile_period: period has already been compiled for this domain") ;

  sd->compiled[d] = 1 ;
  dp = &(domain_info[d]) ;

  if ( dp->decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

for ( dir = 0 ; dir < 2 ; dir++ )
{
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    procrec = RSL_MALLOC( rsl_procrec_t, 1 ) ;
    procrec->P = P ;
    procrec->npts = 0 ;
    procrec->recv_npts = 0 ;

/* 1.1 */
    /* for every ghost point from P, mark any local point that lies
       on its period (using the point function associated with the
       period.   */
    
    send_accum = 0 ;
    recv_accum = 0 ;

        for ( fld = sd->msgs[d]->fldspecs ; fld != NULL ; fld = fld->next )
    for ( n = 0 ; n < nlen ; n++ )
      for ( m = 0 ; m < mlen ; m++ )
/* 1.1.1 */
         {
           switch ( fld->strategy )
           {
           case MINNS_MAJEW_2D :
             rsl_period_pt( dir, d, m, fld->glen[0], fld->stag[0], n, fld->glen[1], fld->stag[1], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           case MINEW_MAJNS_2D :
             rsl_period_pt( dir, d, m, fld->glen[1], fld->stag[1], n, fld->glen[0], fld->stag[0], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           case MINNS_MAJEW_K_3D :
             rsl_period_pt( dir, d, m, fld->glen[0], fld->stag[0], n, fld->glen[1], fld->stag[1], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           case MINEW_MAJNS_K_3D :
             rsl_period_pt( dir, d, m, fld->glen[1], fld->stag[1], n, fld->glen[0], fld->stag[0], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           case K_MIDNS_MAJEW_3D :
             rsl_period_pt( dir, d, m, fld->glen[1], fld->stag[1], n, fld->glen[2], fld->stag[2], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           case MINNS_K_MAJEW_3D :
             rsl_period_pt( dir, d, m, fld->glen[0], fld->stag[0], n, fld->glen[2], fld->stag[2], fld, sd->bdyw[d], check_local_pts_period ) ; break ;
           default :
              RSL_TEST_ERR(1,"unsupported strategy") ;
           }
         }

    procrec->nsends = 0 ;
    procrec->nrecvs = 0 ;
    procrec->sendsize = send_accum + sizeof(int) ; /* extra word for count */
    procrec->recvsize = recv_accum + sizeof(int) ; /* extra word for count */

    if ( send_accum != 0 || recv_accum != 0 )
    {
      procrec->next = sd->procs[dir][d] ;
      sd->procs[dir][d] = procrec ;
    }
    else
    {
      RSL_FREE(procrec) ;
    }
  }
  {
    int ig, jg, i, j ;
    rsl_list_t *lp1 ;
    void * base ;
    int elemsz, t0, t1, pack_table_size ;
    period_desc_t * per ;
    rsl_ptrec_t * ptrec ;
    message_desc_t * msg ;
    rsl_fldspec_t * fld ;
    
    per = (period_desc_t *) pr_descriptors[ s ] ;
    for ( procrec = per->procs[dir][d] ; procrec != NULL ; procrec = procrec->next )
    {
      init_period_refs() ;
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
            if ( fld->type >= 100 ) sd->has_f90_fields = 1 ;
            base = fld->base ;
#if 0
fprintf(stderr,"pack  P=%d  i j ig jg    %3d %3d %3d %3d, base %08x\n",procrec->P,i,j,ig,jg,base) ;
#endif
            elemsz = fld->elemsz ;
            switch (fld->strategy)
            {
            case MINNS_MAJEW_2D :               /* <MM> eg: psa(i,j) */
              t0 = fld->llen[0] ;
              store_period_refs( base, fld->f90_table_index , (i+j*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;
            case MINEW_MAJNS_2D :               /* xxx(j,i) */
              t0 = fld->llen[0] ;
              store_period_refs( base, fld->f90_table_index , (j+i*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;



            case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , 
                                         (i+j*t0)*elemsz,     /* offset */
                                         elemsz,              /* n      */
                                         fld->llen[2],        /* nelems */
                                         t1*elemsz) ;         /* stride */
              break ;

            case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index ,
                                         (j+i*t0)*elemsz,     /* offset */
                                         elemsz,              /* n      */
                                         fld->llen[2],        /* nelems */
                                         t1*elemsz) ;         /* stride */
              break ;

            case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                                                             /* offset               n      nelems   stride */
                                                             /*   |                  |         |      |     */
                                                             /*   v                  v         v      v     */
              store_period_refs( base, fld->f90_table_index , (i*t0+j*t1)*elemsz, elemsz * t0, 1, elemsz) ;
              break ;

            case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , (i+j*t1)*elemsz, elemsz, fld->llen[1], t0*elemsz) ;
              break ;
            default:
              RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
              break ;
            }
          }
        }
      }
      period_refs( &(procrec->pack_table),
                    &(procrec->pack_table_size),
                    &(procrec->pack_table_nbytes) , 0 ) ;
    }
  }
#if 0
fprintf(stderr,"-=-=-=-=-=-=-\n") ;
#endif
  {
    int ig, jg, i, j ;
    rsl_list_t *lp1 ;
    void * base ;
    int elemsz, t0, t1, pack_table_size ;
    period_desc_t * per ;
    rsl_ptrec_t * ptrec ;
    message_desc_t * msg ;
    rsl_fldspec_t * fld ;
    
    per = (period_desc_t *) pr_descriptors[ s ] ;
    for ( procrec = per->procs[dir][d] ; procrec != NULL ; procrec = procrec->next )
    {
      init_period_refs() ;
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
            if ( fld->type >= 100 ) sd->has_f90_fields = 1 ;
            base = fld->base ;
#if 0
fprintf(stderr,"unpack  P = %d  i j ig jg    %3d %3d %3d %3d, base %08x\n",procrec->P,i,j,ig,jg, base) ;
#endif
            elemsz = fld->elemsz ;
            switch (fld->strategy)
            {
            case MINNS_MAJEW_2D :               /* <MM> eg: psa(i,j) */
              t0 = fld->llen[0] ;
              store_period_refs( base, fld->f90_table_index , (i+j*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;
            case MINEW_MAJNS_2D :               /* xxx(j,i) */
              t0 = fld->llen[0] ;
              store_period_refs( base, fld->f90_table_index , (j+i*t0)*elemsz, elemsz, 1, elemsz) ;
              break ;
            case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , (i+j*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
              break ;
            case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , (j+i*t0)*elemsz, elemsz,
                                         fld->llen[2],
                                         t1*elemsz) ;
              break ;
            case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , (i*t0+j*t1)*elemsz, elemsz * t0, 1, elemsz) ;
              break ;
            case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
              t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
              store_period_refs( base, fld->f90_table_index , (i+j*t1)*elemsz, elemsz, fld->llen[1], t0*elemsz) ;
              break ;
            default:
              RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
              break ;
            }
          }
        }
      }
      period_refs( &(procrec->unpack_table),
                    &(procrec->unpack_table_size),
                    &(procrec->unpack_table_nbytes) , 0 ) ;
    }
  }
}
}

