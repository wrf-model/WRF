#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

static rsl_list_t * list_head = NULL ;

static int destroy_packrec( p ) packrec_t * p ; { free( p ) ; return(0) ; }

static int compare_period_sort( a, b, dummy )
  packrec_t *a, *b ;
  int dummy ;
{
  if ( a != NULL && b != NULL )
#if 0
    if ((unsigned long)a->offset+(unsigned long)a->base > (unsigned long)b->offset+(unsigned long)b->base)
#else
    if ((unsigned long)a->base > (unsigned long)b->base)
#endif
      return(1) ;
  return(0) ;
}

init_period_refs()
{
  rsl_list_t * lp, *lpnext ; 
  destroy_list( &list_head, destroy_packrec ) ;
  list_head = NULL ;
}

store_period_refs( base, f90_table_index , offset, n, nelems, stride )
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

  newrec = RSL_MALLOC( packrec_t, 1 ) ;
  newrec->base = base ;
  newrec->f90_table_index = f90_table_index ;
  newrec->offset = offset ;
  newrec->n = n ;
  newrec->nelems = nelems ;
  newrec->stride = stride ;
  newrec->valid = 1 ;

  lp1 = NULL ;
  for ( lp = list_head ; lp != NULL ; lp = lp->next )
  {
    lp1 = lp ;
  }
  if ( lp1 == NULL )
  {
    lp1 = RSL_MALLOC( rsl_list_t, 1 ) ;
    lp1->next = NULL ;
    list_head = lp1 ;
  }
  else
  {
    lp1->next = RSL_MALLOC( rsl_list_t, 1 ) ;
    lp1 = lp1->next ;
    lp1->next = NULL ;
  }
  lp1->data = newrec ;
}

#include <sys/time.h>
RSL_INTERNAL_MILLICLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000 * isec + usec / 1000 ;
    return(msecs) ;
}
RSL_INTERNAL_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}

period_refs( pack_table, pack_table_size, pack_table_nbytes, collapse )
  packrec_t ** pack_table ;
  int * pack_table_size, *pack_table_nbytes, collapse ;
{
  /* First sort the primary list, then sort each of the secondary lists
     in the data structure built by
     store_process_refs.   Finally, go through and collapse them. */

  rsl_list_t * lp, *lp1, *lp2, *lp3 ;
  packrec_t *x, *y ;
  int i, nbytes ;
  int compare_period_sort() ;

  /* figure the number of entries */
  for ( i = 0, lp = list_head ; lp ; lp = lp->next )
      i++ ;

  *pack_table_size = i ;

  /* now allocate and populate the table */
  *pack_table = RSL_MALLOC( packrec_t, *pack_table_size ) ;
  for ( i = 0, nbytes = 0, lp = list_head ; lp ; lp = lp->next )
    {
      x = (packrec_t *)lp->data ;
      nbytes += x->n * x->nelems ;
      bcopy(lp->data,&((*pack_table)[i]),sizeof(packrec_t)) ;
      i++ ;
    }

  *pack_table_nbytes = nbytes ;
  
  return ;
}
 
