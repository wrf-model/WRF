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

RSL_ORDER ( d_p, dir_p ) 
  rsl_index_t *d_p  ; /* domain */
  int_p       dir_p ; /* sort direction */
{
  int d, dir, up ;
  int retval ;
  rsl_list_t *lp ;

  RSL_TEST_ERR( d_p == NULL, "rsl_order: null domain ptr" ) ;
  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_order: bad domain descriptor") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
    "rsl_order: invalid domain") ;
  RSL_TEST_WRN(1,"RSL_ORDER: obsolete") ;

  if ( dir_p != NULL )
    dir = *dir_p ;
  else
    dir = MINMAJ_AA ;

  lp = domain_info[d].pts ;

  if ( MNMJ( dir ))  /* MNMJ, D1, and D2 macros defined in rsl.h */
  {
    up = D1(dir)?-1:1 ;
    bubble( lp, up ) ; /* sort by inner key */
    up = D2(dir)?-2:2 ;
    bubble( lp, up ) ; /* sort by outer key */
  }
  else
  {
    up = D1(dir)?-2:2 ;
    bubble( lp, up ) ; /* sort by inner key */
    up = D2(dir)?-1:1 ;
    bubble( lp, up ) ; /* sort by outer key */
  }
}

lenlist( a )
  rsl_list_t *a ;
{
  rsl_list_t *p ;
  int i ;
  for ( p = a, i = 0 ; p != NULL ; p = p->next ) i++  ;
  return (i) ;
}

showlist( l, s )
  rsl_list_t * l ;
  char * s  ;
{
  rsl_list_t * lp ;
  rsl_point_t *p ;
  int i, j ;

  for ( lp = l ; lp != NULL ; lp = lp->next ) 
  {
    p = (rsl_point_t *) lp->data ;
    i = ID_IDEX( p->id ) ;
    j = ID_JDEX( p->id ) ;
    fprintf(stderr,"%s > %d %d\n",s,i,j) ;
  }
}

rsl_sort( list, compare, up )
  rsl_list_t **list ;
  int (*compare)() ;
  int up ;
{
#if 0
  fprintf(stderr,"rsl_sort: lenlist(*list) = %d  %x\n", lenlist(*list), *list ) ; 
#endif
  rsl_sort1( list, compare, up ) ;
}

rsl_sort1( list, compare, up )
  rsl_list_t **list ;
  int (*compare)() ;
  int up ;
{
  int n, np, nq ;
  rsl_list_t *p, *q, *qnext ;

  p = *list ; q = NULL ;

  /* count items and q at list midpoint */
  for ( n = 0, nq = 0 ; p != NULL ; p = p->next )
  {
    if ( n % 2 == 1 ) {
      if ( q == NULL ) { q = *list ; }
      else             { q = q->next ; }
      nq++ ;
    }
    n++ ;
  }
  if ( n <= 1 ) return ;
  np = n - nq ;  /* nq is length of first half; np is length of second half */

  qnext = q->next ;
  q->next = NULL ;
  /* first half */
  if ( nq > 1 )
  {
    rsl_sort1( list, compare, up ) ;
  }
  /* second half */
  if ( np > 1 )
  {
    rsl_sort1( &qnext, compare, up ) ;
  }
  if ( n > 0 )
  {
    rsl_quicksort_merge ( list, *list, qnext, compare, up, n ) ;
  }
}

rsl_quicksort_merge( retlist, a , b, compare, up, n )
  rsl_list_t **retlist, *a , *b  ;
  int (*compare)() ;
  int up ;
  int n ;
{
  rsl_list_t *newlist, *t, *p, *q ;
  int i ;

  p = b ; q = a ;
  if ( q != NULL && p != NULL )
  {
    if ( (*compare)(q->data,p->data,up) )
    {
      newlist = p ;
      p = p->next ;
    }
    else
    {
      newlist = q ;
      q = q->next ;
    }
  }
  else if ( p != NULL )
  {
    newlist = p ; p = p->next ;
  }
  else if ( q != NULL )
  {
    newlist = q ; q = q->next ;
  }

  t = newlist ;
  for ( i = 1 ; i < n ; i++ )
  {
    if ( q != NULL && p != NULL )
    {
      if( (*compare)(q->data,p->data,up) )
      {
        t->next = p ;
        p = p->next ;
      }
      else
      {
        t->next = q ;
        q = q->next ;
      }
    }
    else if ( q != NULL )
    {
      t->next = q ;
      q = q->next ;
    }
    else if ( p != NULL )
    {
      t->next = p ;
      p = p->next ;
    }
    t = t->next ;
  }
  t->next = NULL ;
  *retlist = newlist ;
}



/* OBSOLETE */
bubble( list, up )
  rsl_list_t *list ;
  int up ;
{
  int i, desc, p, c ;
  rsl_list_t *lp, *prev ;
  struct srtrec {
    rsl_point_t *pt ;
    int         k1 ;
    int         k2 ;
  } *lst, *lp2 ;
  int swap, pass, listlen, kk ;
  void * data ;
  rsl_point_id_t id ;
  rsl_point_t *pt ;

  desc = 0 ;
  if ( up < 0 )
  {
    up = -up ;
    desc = 1 ;
  }
  listlen = 0 ;
  for ( lp = list ; lp != NULL ; lp = lp->next ) listlen++ ;
  lst = RSL_MALLOC( struct srtrec, listlen ) ;
  for ( lp = list, lp2 = lst  ; lp != NULL ; lp = lp->next, lp2++ )
  {
    lp2->pt = (rsl_point_t *) lp->data ;
    id = lp2->pt->id ;
    lp2->k1 = ID_IDEX( id ) ;
    lp2->k2 = ID_JDEX( id ) ;
  }

  pass = 0 ;
  swap = 1 ;
  while ( swap )
  {
    swap = 0 ;
    lp2 = lst ;
    switch ( up )
    {
    case 1:
      for ( i = 0 ; i < listlen-1 ; i++ )
      {
	p = lp2->k1 ;
	c = (lp2+1)->k1 ;
	if ((desc && ( p < c )) || (!desc && ( p > c )))
        {
            pt = lp2->pt ;
            lp2->pt = (lp2+1)->pt ;
            (lp2+1)->pt = pt ;
            kk = lp2->k1 ;
            lp2->k1 = (lp2+1)->k1 ;
            (lp2+1)->k1 = kk ;
            swap = 1 ;
        }
        lp2++ ;
      }
      break ;
    case 2:
      for ( i = 0 ; i < listlen-1 ; i++ )
      {
	p = lp2->k2 ;
	c = (lp2+1)->k2 ;
	if ((desc && ( p < c )) || (!desc && ( p > c )))
        {
            pt = lp2->pt ;
            lp2->pt = (lp2+1)->pt ;
            (lp2+1)->pt = pt ;
            kk = lp2->k2 ;
            lp2->k2 = (lp2+1)->k2 ;
            (lp2+1)->k2 = kk ;
            swap = 1 ;
        }
        lp2++ ;
      }
      break ;
    default:
      RSL_TEST_ERR(1,"sort: no such key") ;
      break ;
    }

    pass++ ;
#if 0
    if ( pass%100 == 0 )
    {
      fprintf(stderr,"pass %d, listlen %d\n",pass,listlen) ;
    }
#endif
    if ( pass > (listlen + 110))
    {
      fprintf(stderr,"pass = %d, listlen=%d\n",pass,listlen) ;
      RSL_TEST_ERR(1,"something's wrong in sort in rsl order") ;
    }
  }

  /* restored sorted values to linked list */
  for ( lp = list, lp2 = lst  ; lp != NULL ; lp = lp->next, lp2++ )
  {
    lp->data = lp2->pt ;
  }

  RSL_FREE(lst) ;
}
