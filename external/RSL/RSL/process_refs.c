#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

static rsl_list_t * list_head = NULL ;

static int destroy_packrec( p ) packrec_t * p ; { free( p ) ; return(0) ;}

init_process_refs()
{
  int destroy_packrec() ;
  rsl_list_t * lp, *lpnext ; 

  for ( lp = list_head ; lp ; lp = lp->next )
  {
    destroy_list( &(lp->data), destroy_packrec ) ;
  }
  destroy_list( &list_head, NULL ) ;
}
/*
   The data structure being built by this routine:
      (n is next pointer, d is data pointer)
   The primary list (downwards) is a list of lists.
   Each of the secondary lists (leftwards) is a list of all the
   pack or unpack records with the same base.  This routine
   checks to ensure that recs that are complete duplicates
   (which can happen) are not added.

   list_t -d-> list_t -n-> list_t -n-> list_t -n-> ...
    |           \            \           \
    n            d            d           d
    |             \            \            \
    |           base,off,n    base,off,n   base,off,n    ...
    v
   list_t -d-> list_t -n-> list_t -n-> list_t -n-> ...
    |           \            \           \
    n            d            d           d
    |             \            \            \
    |           base,off,n    base,off,n   base,off,n    ...
    v
   list_t -d-> list_t -n-> list_t -n-> list_t -n-> ...
    |           \            \           \
    n            d            d           d
    |             \            \            \
    |           base,off,n    base,off,n   base,off,n    ...
    v
    .
    .
    .
*/

store_process_refs( base, f90_table_index , offset, n, nelems, stride )
  void * base ;
  int f90_table_index ;
  int offset ;
  int n ;
  int nelems ;
  int stride ;
{
  rsl_list_t * lp, *lp1, *lp2, *lp3, *lp4 ;
  rsl_list_t * x ;
  packrec_t * newrec, *arec, *nextrec ;
  int found, found1 ;

#if 0
fprintf(stderr,"debug store_process_refs 1 base %08x ",base) ;
fprintf(stderr," f90_table_index %3d ",f90_table_index) ;
fprintf(stderr," offset %10d ",offset) ;
fprintf(stderr," n %5d",n) ;
fprintf(stderr," nelems %5d",nelems) ;
fprintf(stderr," stride %5d\n",stride) ;
#endif

  newrec = RSL_MALLOC( packrec_t, 1 ) ;
  newrec->endstop = 0 ;
  if ( stride < 0 ) { newrec->endstop = 1 ; stride = -stride ; }
  newrec->base = base ;
  newrec->f90_table_index = f90_table_index ;
  newrec->offset = offset ;
  newrec->n = n ;
  newrec->nelems = nelems ;
  newrec->stride = stride ;
  newrec->valid = 1 ;

  /* traverse the primary list and see if there's a secondary
     list already for this base address.  If there is not, add
     it with newrec as the first entry in the new secondar list. 
     If there is alread a secondary list, traverse it and make sure
     there's not already an entry for newrec.  If there isn't, add
     an entry for newrec to the end of the secondary list. */
  found = 0 ;
  for ( lp = list_head ; lp ; lp = lp->next )
  {
    if ( lp != NULL )
      if ((lp1 = (rsl_list_t *)lp->data) != NULL )
         if ((arec = (packrec_t *) lp1->data) != NULL )
           if ( arec->base == base )
           {
             found = 1 ;
             break ;
           }
  }
  if ( !found )
  {
    x = RSL_MALLOC( rsl_list_t, 1 ) ;
    x->next = list_head ;
    list_head = x ;
    x->data = RSL_MALLOC( rsl_list_t, 1 ) ;
    ((rsl_list_t *) x->data)->data = newrec ;
    ((rsl_list_t *) x->data)->next = NULL ;
  }
  else
  {
    /* includes an insertion sort */
    found1 = 0 ;
    for ( lp2 = lp1 ; lp2 != NULL ; lp2 = lp2->next )
    {
      lp3 = lp2 ;  /* store previous lp2 */
      arec = (packrec_t *) lp2->data ;

      if (lp2 == lp1)
      {
	if ( newrec->offset < arec->offset )
	{ found1 = 0 ; break ; }
      }
      if (newrec->offset == arec->offset)
      {
        if (arec->n >= newrec->n)
          { found1 = 1 ; break ; }
        else
          { arec->n = newrec->n ; found1 = 1 ; break ; }
      }
      else if (lp2->next != NULL)
      {
	nextrec = lp2->next->data ;
	if ( newrec->offset > arec->offset &&
	     newrec->offset < nextrec->offset )
        { found1 = 2 ; break ; }
      }
      else if (newrec->offset > arec->offset)
      {
        { found1 = 2 ; break ; }
      }
    }
    if ( found1 == 0 )      /* not found; add to beginning of list */
    {
      lp4 = RSL_MALLOC( rsl_list_t, 1 ) ;
      lp4->next = (rsl_list_t *) lp->data ;
      lp4->data = newrec ;
      lp->data = lp4 ;
    }
    if ( found1 == 2 )      /* insert after this element */
    {
      lp4 = RSL_MALLOC( rsl_list_t, 1 ) ;
      lp4->data = newrec ;
      lp4->next = lp2->next ;
      lp2->next = lp4 ;
    }
  }
}

static int compare_primary( lp1, lp2, dummy )
  rsl_list_t *lp1, *lp2 ;
  int dummy ;
{
  rsl_list_t *a, *b ;
  packrec_t *x, *y ;
  if ( lp1 != NULL && lp2 != NULL )
  {
    if ((x=(packrec_t*)lp1->data) != NULL && (y=(packrec_t*)lp2->data) != NULL )
    {
      if (x->base > y->base)
      {
        return(1) ;
      }
    }
    else
      RSL_TEST_ERR(1,"compare_primary 2") ;
  }
  else
    RSL_TEST_ERR(1, "compare_primary 1" ) ;
  return(0) ;
}

static int compare_secondary( a, b, dummy )
  packrec_t *a, *b ;
  int dummy ;
{
  if ( a != NULL && b != NULL )
    if (a->offset > b->offset) 
      return(1) ;
  return(0) ;
}

static int collapsetable( lst )
  rsl_list_t ** lst ;
{
  rsl_list_t * lp, * lp2, * prevlp ;
  packrec_t *x, *y ;

  if ( lst == NULL ) return(0)  ;

lp = *lst ; x = lp->data ; if ( ! (x->valid ) ) RSL_TEST_ERR(1,"internal error: first entry invalid\n") ;

  for ( lp = *lst ; lp != NULL ; lp = lp->next )
  {
    if (( x = lp->data ) != NULL ) ; if ( x->valid )       /* 2 statements */
    {
      for ( lp2 = lp->next ; lp2 != NULL ; lp2 = lp2->next )
      {
        if (( y = lp2->data ) != NULL ) ; if ( y->valid )  /* 2 statements */
        {
          if ((x->stride == y->stride) &&
              (x->nelems == y->nelems) &&
	      ((x->offset + x->n ) == y->offset) && ! x->endstop )
          {
	    {
              y->valid = 0 ;
              x->n += y->n ;
	    }
	  }
          else
	  {
	    break ;   /* out of inner loop */
	  }
        }
      }
    }
  }
/* new bit... collapse sequences of entries with the same base and stride */
  {
    int xn, bigstride, firsty ;
    for ( lp = *lst ; lp != NULL ; lp = lp->next )
    {
      if (( x = lp->data ) != NULL ) ; if ( x->valid )       /* 2 statements */
      {
        if ( x->nelems != 1 ) continue ;
        xn = x->n ;
	firsty = 1 ;
        for ( lp2 = lp->next ; lp2 != NULL ; lp2 = lp2->next )
        {
          if (( y = lp2->data ) != NULL ) ; if ( y->valid )  /* 2 statements */
          {
	    if ( y->base != x->base ) break ;
	    if ( y->nelems != 1 ) break ;
	    if ( y->n != xn ) break ;
	    if ( firsty == 1 ) /* first y */
	    {
	      firsty = 0 ;
	      bigstride = y->offset - x->offset ;
	    }
	    if ( bigstride <= x->n ) break ;
	    if ( y->offset - x->offset == bigstride )
	    {
	      y->valid = 0 ;
	      x->nelems++ ;
	      x->stride = bigstride ;
	    }
          }
        }
      }
    }
  }
  /* now eliminate the invalidated entries */
  for ( prevlp = *lst, lp = *lst ; lp != NULL ; )
  {
    if (( x=lp->data ) != NULL ) ; if ( ! x->valid )   /* 2 statements */
    {
      RSL_TEST_ERR( lp == *lst , " internal error -- shouldn't happen " ) ;
      prevlp->next = lp->next ;
      lp->next = NULL ;
      destroy_list( &lp, destroy_packrec ) ;  /* destroys just one rec */
      lp2 = prevlp ;
      lp = prevlp->next ;
    }
    else
    {
      lp2 = lp ;
      lp = lp->next ;
    }
    prevlp = lp2 ;
  }
}

process_refs( pack_table, pack_table_size, pack_table_nbytes, collapse )
  packrec_t ** pack_table ;
  int * pack_table_size, *pack_table_nbytes, collapse ;
{
  /* First sort the primary list, then sort each of the secondary lists
     in the data structure built by
     store_process_refs.   Finally, go through and collapse them. */

  int compare_primary(), compare_secondary() ;
  rsl_list_t * lp, *lp1, *lp2, *lp3 ;
  packrec_t *x, *y ;
  int dummy ;
  int i, nbytes ;

#if 0
fprintf(stderr,"before sort\n") ;
for ( i = 0, lp = list_head ; lp ; lp = lp->next )
{
lp2 = lp->data ;
fprintf(stderr,"%d %08x\n", i,  ((packrec_t *)(lp2->data))->base) ;
i++ ;
}
#endif

  dummy = 0 ;
  rsl_sort( &list_head, compare_primary, dummy ) ;

  /* figure the number of entries */
  for ( i = 0, lp = list_head ; lp ; lp = lp->next )
    for ( lp1 = (rsl_list_t *)lp->data ; lp1 ; lp1 = lp1->next )
      i++ ;

  for ( lp = list_head ; lp ; lp = lp->next )
  {
    rsl_sort( &(lp->data), compare_secondary, 99 ) ;
#if 1
    if ( collapse ) collapsetable( &(lp->data) ) ;
#endif
  }

#if 0
  for ( i = 0, lp = list_head ; lp ; lp = lp->next )
  {
#if 0
lp1 = (rsl_list_t *)lp->data ; x = (packrec_t *)lp1->data ;
fprintf(stderr,"Entries for base %08x\n", x->base ) ;
#endif
    for ( lp1 = (rsl_list_t *)lp->data ; lp1 ; lp1 = lp1->next )
    {
#if 0
x = (packrec_t *)lp1->data ;
fprintf(stderr,"     offset %10d  %d  %4d\n", x->offset, x->f90_table_index,x->n ) ;
#endif
      i++ ;
    }
  }
#endif


  /* figure the number of remaining entries */
  for ( i = 0, lp = list_head ; lp ; lp = lp->next )
    for ( lp1 = (rsl_list_t *)lp->data ; lp1 ; lp1 = lp1->next )
      i++ ;

  *pack_table_size = i ;

#if 0
fprintf(stderr,"debug 2 pack_table_size = %d\n",*pack_table_size) ;
#endif

  /* now allocate and populate the table */
  *pack_table = RSL_MALLOC( packrec_t, *pack_table_size ) ;
  for ( i = 0, nbytes = 0, lp = list_head ; lp ; lp = lp->next )
    for ( lp1 = (rsl_list_t *)lp->data ; lp1 ; lp1 = lp1->next )
    {
      x = (packrec_t *)lp1->data ;
      nbytes += x->n * x->nelems ;
      bcopy(lp1->data,&((*pack_table)[i]),sizeof(packrec_t)) ;
      i++ ;
    }

  *pack_table_nbytes = nbytes ;
  
  return ;
}
 
