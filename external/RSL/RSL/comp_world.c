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

/*@
  RSL_GET_RUN_INFO - get RSL information about size and shape local allocation.

  Notes:
  This routine is used to initialize loops over each processor's
  local partition of the decomposed domain.  The routine can be used
  directly, or RSL-provided macros may be used in the code that expand
  to the proper calls.  Both M4 and CPP macros are provided in the 
  RSL distribution (see LoopMacros).
  The macro approach is recommended
  for readability, simplicity,
  and also to insulate the code from future potential updates in RSL.
  Of the two sets, the M4 macros are recommended since they
  are more flexible.

  RSL returns through the arguments Arg6, Arg7, and Arg8 information
  for iterating over the local processor partition N-major, M-minor.
  Argument Arg4 is the number of iterations to cover the part of the
  domain allocated to the processor.  Arg6 contains the
  J-indices (Arg4 of them) of each I-strip local to the processor.
  The first index into the local partition is stored at stored at Arg6(3).
  The locations Arg6(1) and Arg6(2) are placeholders for indices
  if one wishes to include one or two of the pad or ghost cells in
  the iteration (see RSL_GET_INFOP).
  Arg7 contains the starting and ending indices of each I-strip.  Likewise,
  the first actual index in Arg7 is at Arg7(3).
  The arguments Arg9, Arg10, and
  Arg11 have the same sense as Arg6, Arg7, and Arg8, except that they
  provide information for 
  iteration in M-major, N-minor order.  Argument Arg5 is
  the number of major iterations over M.
  The arrays Arg6, Arg7, Arg8, Arg9, Arg10, and Arg11 store local (memory)
  indices.  The logical (global) indices can be obtained by subtracting
  Arg12 (for M-dimension indices) or Arg13 (for N-dimension indices).
  
  Unlike programs that are implemented in single address
  spaces, the identical
  relationship between logical and memory
  indices does not hold for data-domain decomposition over distributed
  memories.  That is, the memory index (the subscripts into a model
  array) may not be used for testing proximity of the point to a
  boundary in the logical domain.  Further, the relationship between
  logical and memory indices differs on each processor.  The Arg12 and
  Arg13 arguments are the differences between the local and global
  indices in the M and N dimensions, respectively, and can be used by
  the program for converting between logical and memory indices.  For
  example, the local index I in the M dimension is equal to Arg12 plus
  the global index IG.

  All of these arrays -- Arg6, Arg7, Arg8, Arg9, Arg10, and Arg11 
  are integers and must have been allocated by the user with size
  large enough to fit the largest possible run through a dimension.
  The argument Arg2 is the length of the arrays.
  The integer Arg1 is an RSL domain descriptor.

  The integer Arg3
  is the nest level of the domain (mother domain is at nest-level 1),
  which is not necessary for iteration over the domain but which 
  is information that RSL has available and that is useful to have
  at the beginning of a module.

  Example:

$  -- original code --

$  subroutine F( ... )
$  ...
$  do j = 3, jl-2
$    do i = 3, il-2
$      a(i,j) = b(i,j) + c(i,j)
$    enddo
$  enddo

$  -- example using M4 macros in LoopMacros.m4 --

$  subroutine F( ... )
$  RSL_RUN_DECL
$  ...
$  RSL_INIT_RUNVARS(d)              ! d is an RSL domain descriptor
$  RSL_DO_N(j,3,jl-2)
$    RSL_DO_M(i,3,il-2)
$      a(i,j) = b(i,j) + c(i,j)
$    RSL_ENDDO
$  RSL_ENDDO

$  -- example using CPP macros in LoopMacros.inc --

$ #include "LoopMacros.cpp"
$  subroutine F( ... )
$  RSL_DECLARE_RUN_VARS 
$  ...
$  RSL_INIT_RUNS(d)                 ! d is an RSL domain descriptor
$  RSL_MAJOR_BOUND(j,3,jl-2)
$    RSL_MINOR_BOUND(j,3,jl-2)
$      a(i,j) = b(i,j) + c(i,j)
$    RSL_END_MINOR_LOOPB
$  RSL_END_MAJOR_LOOPB

$  -- example with macros expanded --

$  subroutine F( ... )
$  integer   ig,jg,nruni,nrunj,js,is,ie,is2,js2,je2,idif,jdif,nr
$  dimension js(512)  ,is(512)  ,ie(512)    ! for N-major iteration
$  dimension is2(512) ,js2(512) ,je2(512)   ! for M-major iteration
$
$  call rsl_get_run_info( d,     512,   nl,   nrunj,   nruni, 
$ +                       js,    is,    ie,
$ +                       js2,   is2,   ie2,
$ +                       idif,  jdif                         )

$  do nr = 3, nrun+2
$    j=js(nr)
$    jg=j-jdif
$    if ( jg .ge. 3 .and. jg .le. maxj-2 ) then
$      do i=is(nr),ie1(nr)
$        ig=i-idif
$        if ( ig .ge. 3 .and. ig .le. maxi-2 ) then
$          a(i,j) = b(i,j) + c(i,j)
$        endif
$      enddo
$    endif
$  enddo

BREAKTHEEXAMPLECODE

  See also:
  RSL_GET_RUN_INFOP, LoopMacros.m4

@*/

RSL_GET_RUN_INFO ( d_p, maxrun_p, nl_p, nrunj_p, nruni_p, js, is, ie, is2, js2, je2, idif_p, jdif_p )
  int_p 
    d_p          /* (I) RSL domain descriptor (input) */
   ,maxrun_p     /* (I) Number of elements in array arguments to this routine */
   ,nl_p         /* (O) Nest level of the domain */
   ,nrunj_p      /* (O) Number of runs through domain in j-major traversal */
   ,nruni_p ;    /* (O) Number of runs through domain in i-minor traversal */
  int
    js[]         /* (O) Local J-index of each run in j-major traversal */
   ,is[]         /* (O) Starting local I-index of each run in j-major traversal */
   ,ie[]         /* (O) Ending local I-index of each run in j-major traversal */
   ,is2[]        /* (O) Local I-index of each run in i-major traversal */
   ,js2[]        /* (O) Starting local J-index of each run in i-major traversal */
   ,je2[] ;      /* (O) Ending local J-index of each run in i-major traversal */
  int_p
    idif_p       /* (O) Difference between local and global I indices (i-ig). */
   ,jdif_p  ;    /* (O) Difference between local and global J indices (j-jg). */
{
  int x ;
  int *dummy ;
  x = 0 ;
  dummy = NULL ;
  RSL_GET_RUN_INFOP ( d_p, &x, maxrun_p, nl_p, nrunj_p, nruni_p,
                      js, is, ie, is2, js2, je2, idif_p, jdif_p,
                      dummy, dummy ) ;
}

/* additional P argument is the width of pad to allow for */
/*@
  RSL_GET_RUN_INFOP - get RSL information about size and shape local allocation.

  Notes:
  This routine is similar to RSL_GET_RUN_INFO except that it allows for
  execution on the extended array pads of the local processor
  subdomains.  This can be useful for trading off computation for
  communication in the code and can simplify the implementation
  by allowing fewer modifications for distributed memory
  parallelism.  The argument Arg2 may be set to
  for one of 3 modes of iteration over the local subdomain ---
  Arg2 = 2 gives iteration over the local subdomain and the set of
  ghost points that are two-away from points in the local subdomain,
  Arg2 = 1 gives iteration over the local subdomain and the set of
  ghost points that are one-away from points in the local subdomain, and
  Arg2 = 0 gives iteration over just the local subdomain (no ghost points).
  that are immediately adjacent to the local processor subdomain, or
  the set that is within 2 cells of the local processor subdomain.  The
  information for controlling iteration over the region is returned in
  the arguments Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, and Arg12.

  The M4 RSL loop macros automatically initialize 3 separate sets of
  these data structures for the 3 available modes of iteration.

  See also:
  RSL_GET_RUN_INFO, LoopMacros.m4
@*/
RSL_GET_RUN_INFOP ( d_p, p_p, maxrun_p, nl_p, nrunj_p, nruni_p,
                    js, is, ie, is2, js2, je2, idif_p, jdif_p,
                    jg2n, ig2n )
  int_p
    d_p          /* (I) RSL domain descriptor (input) */
   ,p_p          /* (I) How many extra pad cells to include (0, 1, or 2) */
   ,maxrun_p     /* (I) Number of elements in array arguments to this routine */
   ,nl_p         /* (O) Nest level of the domain */
   ,nrunj_p      /* (O) Number of runs through domain in j-major traversal */
   ,nruni_p      /* (O) Number of runs through domain in i-minor traversal */
   ,js           /* (OA) Local J-index of each run in j-major traversal */
   ,is           /* (OA) Starting local I-index of each run in j-major traversal */
   ,ie           /* (OA) Ending local I-index of each run in j-major traversal */
   ,is2          /* (OA) Local I-index of each run in i-major traversal */
   ,js2          /* (OA) Starting local J-index of each run in i-major traversal */
   ,je2          /* (OA) Ending local J-index of each run in i-major traversal */
   ,idif_p       /* (O) Difference between local and global I indices (i-ig). */
   ,jdif_p       /* (O) Difference between local and global J indices (j-jg). */
   ,jg2n         /* (OA) Number of run for a global J-index in j-major traversal. */
   ,ig2n         /* (OA) Number of run for a global I-index in i-major traversal. */
   ;
{
  int d, i, p ;
  d = *d_p ;
  p = *p_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_get_run_info: bad domain") ;
  RSL_TEST_ERR( p < 0, "Negative pad arg for RSL_GET_RUN_INFOP") ;
  if ( p > MAX_KINDPAD )
  {
    sprintf(mess,"RSL_GET_RUN_INFOP: pad arg (%d) larger than %d",p,MAX_KINDPAD) ;
    RSL_TEST_ERR( 1,  mess ) ;
  }
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

  *nl_p = domain_info[d].nest_level ;
  *idif_p = domain_info[d].idif ;
  *jdif_p = domain_info[d].jdif ;

  *nrunj_p = domain_info[d].nrun[p] ;
  if ( *maxrun_p < *nrunj_p )
  {
    sprintf(mess,
     "rsl_get_run_info: would overwrite user arrays: maxrun (%d) < nruni (%d)",
         *maxrun_p, *nrunj_p ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }

/*****************/
  if ( p <= MAX_RUNPAD )
  {
  for ( i = 0 ; i < MAX_RUNPAD-p ; i++ )
  {
    js[i] = 0 ;
    is[i] = 0 ;
    ie[i] = -1 ;
  }
  for ( i = 0 ; i < *nrunj_p ; i++ )
  {
    js[i+(MAX_RUNPAD-p)] = domain_info[d].js[p][i] ;
    is[i+(MAX_RUNPAD-p)] = domain_info[d].is[p][i] ;
    ie[i+(MAX_RUNPAD-p)] = domain_info[d].ie[p][i] ;
  }
  if ( jg2n != NULL )
  {
    for ( i = 0 ; i < domain_info[d].len_n ; i++ )
    {
      jg2n[i] = domain_info[d].jg2n[p][i] + MAX_RUNPAD-p;
    }
  }

  *nruni_p = domain_info[d].nruni[p] ;
  if ( *maxrun_p < *nruni_p )
  {
    sprintf(mess,
     "rsl_get_run_info: would overwrite user arrays: maxrun (%d) < nruni (%d)",
         *maxrun_p, *nruni_p ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  for ( i = 0 ; i < MAX_RUNPAD-p ; i++ )
  {
    is2[i] = 0 ;
    js2[i] = 0 ;
    je2[i] = -1 ;
  }
  for ( i = 0 ; i < *nruni_p ; i++ )
  {
    is2[i+(MAX_RUNPAD-p)] = domain_info[d].is2[p][i] ;
    js2[i+(MAX_RUNPAD-p)] = domain_info[d].js2[p][i] ;
    je2[i+(MAX_RUNPAD-p)] = domain_info[d].je2[p][i] ;
  }
  if ( ig2n != NULL )
  {
    for ( i = 0 ; i < domain_info[d].len_n ; i++ )
    {
      ig2n[i] = domain_info[d].ig2n[p][i] + MAX_RUNPAD-p;
    }
  }
  }
/*****************/
/*****************/
  else if ( p > 3 && p <= 4)
  {
  int p1 ;
  p1 = 0 ;
  for ( i = 0 ; i < MAX_RUNPAD-p1 ; i++ )
  {
    js[i] = 0 ;
    is[i] = 0 ;
    ie[i] = -1 ;
  }
  for ( i = 0 ; i < *nrunj_p ; i++ )
  {
    js[i+(MAX_RUNPAD-0)] = domain_info[d].js[p][i] ;
    is[i+(MAX_RUNPAD-0)] = domain_info[d].is[p][i] ;
    ie[i+(MAX_RUNPAD-0)] = domain_info[d].ie[p][i] ;
  }
  if ( jg2n != NULL )
  {
    for ( i = 0 ; i < domain_info[d].len_n ; i++ )
    {
      jg2n[i] = domain_info[d].jg2n[p][i] + MAX_RUNPAD-0;
    }
  }

  *nruni_p = domain_info[d].nruni[p] ;
  if ( *maxrun_p < *nruni_p )
  {
    sprintf(mess,
     "rsl_get_run_info: would overwrite user arrays: maxrun (%d) < nruni (%d)",
         *maxrun_p, *nruni_p ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  for ( i = 0 ; i < MAX_RUNPAD-0 ; i++ )
  {
    is2[i] = 0 ;
    js2[i] = 0 ;
    je2[i] = -1 ;
  }
  for ( i = 0 ; i < *nruni_p ; i++ )
  {
    is2[i+(MAX_RUNPAD-0)] = domain_info[d].is2[p][i] ;
    js2[i+(MAX_RUNPAD-0)] = domain_info[d].js2[p][i] ;
    je2[i+(MAX_RUNPAD-0)] = domain_info[d].je2[p][i] ;
  }
  if ( ig2n != NULL )
  {
    for ( i = 0 ; i < domain_info[d].len_n ; i++ )
    {
      ig2n[i] = domain_info[d].ig2n[p][i] + MAX_RUNPAD-0;
    }
  }
  }
/*****************/
}

RSL_REG_RUN_INFOP ( d_p, p_p, maxrun_p, nl_p,
		  is, ie,
		  js, je,
		  idif_p, jdif_p )
  int_p
    d_p          /* (I) RSL domain descriptor (input) */
   ,p_p          /* (I) How many extra pad cells to include (0, 1, or 2) */
   ,maxrun_p     /* (I) Number of elements in array arguments to this routine */
   ,nl_p         /* (O) Nest level of the domain */
   ,is
   ,ie
   ,js
   ,je
   ,idif_p
   ,jdif_p ;
{
  int d, i, j, p, cnt ;
  d = *d_p ;
  p = *p_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_get_run_info: bad domain") ;
  RSL_TEST_ERR( p < 0, "Negative pad arg for RSL_GET_RUN_INFOP") ;
  if ( p > MAX_KINDPAD )
  {
    sprintf(mess,"RSL_GET_RUN_INFOP: pad arg (%d) larger than %d",p,MAX_KINDPAD)
 ;
    RSL_TEST_ERR( 1,  mess ) ;
  }
  RSL_TEST_ERR( ((! sw_allow_dynpad) && p > 4),
   "Invalid to call RSL_REG_RUN_INFOP with p > 4 if RSL_ALLOW_DYNPAD has not been called.\n") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  RSL_TEST_ERR( domain_info[*d_p].len_n > *maxrun_p,
    "domain_info[*d_p].len_n > *maxrun_p") ;
  RSL_TEST_ERR( domain_info[*d_p].len_m > *maxrun_p,
    "domain_info[*d_p].len_m > *maxrun_p") ;

  *nl_p = domain_info[d].nest_level ;
  *idif_p = domain_info[d].idif ;
  *jdif_p = domain_info[d].jdif ;

#define WHICH_RUN 0
/* in following code, note assumptions on order of traversal,
   contiguity of points, and rectangularity of partitions */
  /** js, je **/
  for ( j=0, cnt=0 ; j < domain_info[*d_p].len_n ; j++ )
  {
    if ( j+1 < domain_info[d].js2[p][WHICH_RUN]-*jdif_p )
    {
      js[j]=domain_info[d].js2[p][WHICH_RUN] ;
      je[j]=-9999999 ;
    }
    else if ( j+1 > domain_info[d].je2[p][WHICH_RUN]-*jdif_p )
    {
      js[j]=9999999 ;
      je[j]=domain_info[d].je2[p][WHICH_RUN] ;
    }
    else
    {
      js[j]=domain_info[d].js2[p][WHICH_RUN] + cnt ;
      je[j]=domain_info[d].js2[p][WHICH_RUN] + cnt ;  /* yes -> js2 */
      cnt++ ;
    }
  }
  /** is, ie **/
  for ( i=0, cnt=0 ; i < domain_info[*d_p].len_m ; i++ )
  {
    if ( i+1 < domain_info[d].is[p][WHICH_RUN]-*idif_p )
    {
      is[i]=domain_info[d].is[p][WHICH_RUN] ;
      ie[i]=-9999999 ;
    }
    else if ( i+1 > domain_info[d].ie[p][WHICH_RUN]-*idif_p )
    {
      is[i]=9999999 ;
      ie[i]=domain_info[d].ie[p][WHICH_RUN] ;
    }
    else
    {
      is[i]=domain_info[d].is[p][WHICH_RUN] + cnt ;
      ie[i]=domain_info[d].is[p][WHICH_RUN] + cnt ;  /* yes -> is */
      cnt++ ;
    }
  }
}

RSL_DYNPAD_7    ( d_p, maxrun_p, nl_p,
		  is, ie,
		  js, je,
		  idif_p, jdif_p )
  int_p
    d_p          /* (I) RSL domain descriptor (input) */
   ,maxrun_p     /* (I) Number of elements in array arguments to this routine */
   ,nl_p         /* (O) Nest level of the domain */
   ,is          /* 2d arrays -- first index is array elements, second is runpad from 0..6 */
   ,ie
   ,js
   ,je
   ,idif_p
   ,jdif_p ;
{
  int d, i, j, p, cnt ;
  d = *d_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_get_run_info: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  RSL_TEST_ERR( ! sw_allow_dynpad, "RSL_DYNPAD_7 cannot be used unless RSL_ALLOW_DYNPAD has been called") ; 
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  RSL_TEST_ERR( domain_info[*d_p].len_n > *maxrun_p,
    "domain_info[*d_p].len_n > *maxrun_p") ;
  RSL_TEST_ERR( domain_info[*d_p].len_m > *maxrun_p,
    "domain_info[*d_p].len_m > *maxrun_p") ;

  *nl_p = domain_info[d].nest_level ;
  *idif_p = domain_info[d].idif ;
  *jdif_p = domain_info[d].jdif ;

  for ( p = 0 ; p <= 6 ; p++ )
  {

#define WHICH_RUN 0
/* in following code, note assumptions on order of traversal,
   contiguity of points, and rectangularity of partitions */
  /** js, je **/
  for ( j=0, cnt=0 ; j < domain_info[*d_p].len_n ; j++ )
  {
    if ( j+1 < domain_info[d].js2[p][WHICH_RUN]-*jdif_p )
    {
      js[j+*maxrun_p*p]=domain_info[d].js2[p][WHICH_RUN] ;
      je[j+*maxrun_p*p]=-9999999 ;
    }
    else if ( j+1 > domain_info[d].je2[p][WHICH_RUN]-*jdif_p )
    {
      js[j+*maxrun_p*p]=9999999 ;
      je[j+*maxrun_p*p]=domain_info[d].je2[p][WHICH_RUN] ;
    }
    else
    {
      js[j+*maxrun_p*p]=domain_info[d].js2[p][WHICH_RUN] + cnt ;
      je[j+*maxrun_p*p]=domain_info[d].js2[p][WHICH_RUN] + cnt ;  /* yes -> js2 */
      cnt++ ;
    }
  }
  /** is, ie **/
  for ( i=0, cnt=0 ; i < domain_info[*d_p].len_m ; i++ )
  {
    if ( i+1 < domain_info[d].is[p][WHICH_RUN]-*idif_p )
    {
      is[i+*maxrun_p*p]=domain_info[d].is[p][WHICH_RUN] ;
      ie[i+*maxrun_p*p]=-9999999 ;
    }
    else if ( i+1 > domain_info[d].ie[p][WHICH_RUN]-*idif_p )
    {
      is[i+*maxrun_p*p]=9999999 ;
      ie[i+*maxrun_p*p]=domain_info[d].ie[p][WHICH_RUN] ;
    }
    else
    {
      is[i+*maxrun_p*p]=domain_info[d].is[p][WHICH_RUN] + cnt ;
      ie[i+*maxrun_p*p]=domain_info[d].is[p][WHICH_RUN] + cnt ;  /* yes -> is */
      cnt++ ;
    }
  }
  }
}

RSL_REG_PATCHINFO_MN ( d_p ,
                       sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3   )
  int_p d_p ;
  int_p sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3 ;
{
  int d, i, j, k ;
  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "rsl_get_run_info: bad domain" ) ;
  *sp1 = -1 ; *ep1 = -1 ;
  *sp2 = -1 ; *ep2 = -1 ;
  for ( j = 0 ; j < domain_info[d].len_n ; j++ )
  {
    for ( i = 0 ; i < domain_info[d].len_m ; i++ )
    {
      if ( rsl_c_comp2phys_proc(domain_info[d].domain[INDEX_2(j,i,domain_info[d].len_m)].P)==rsl_myproc)
      {
	if ( *sp1 < 0 ) *sp1 = i + 1 ;
	*ep1 = i + 1 ;
	if ( *sp2 < 0 ) *sp2 = j + 1 ;
	*ep2 = j + 1 ;
      }
    }
  }
  *sp3 = 1 ;
  *ep3 = domain_info[d].len_z ;
}

RSL_REG_PATCHINFO_MZ ( d_p ,
                       sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3   )
  int_p d_p ;
  int_p sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3 ;
{
  int d, i, j, k ;
  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "rsl_get_run_info: bad domain" ) ;
  *sp1 = -1 ; *ep1 = -1 ;
  *sp3 = -1 ; *ep3 = -1 ;
  for ( k = 0 ; k < domain_info[d].len_z ; k++ )
  {
    for ( i = 0 ; i < domain_info[d].len_m ; i++ )
    {
      if ( rsl_c_comp2phys_proc(domain_info[d].domain_mz[INDEX_2(k,i,domain_info[d].len_m)].P)==rsl_myproc)
      {
        if ( *sp1 < 0 ) *sp1 = i + 1 ;
        *ep1 = i + 1 ;
        if ( *sp3 < 0 ) *sp3 = k + 1 ;
        *ep3 = k + 1 ;
      }
    }
  }
  *sp2 = 1 ;
  *ep2 = domain_info[d].len_n ;
}

RSL_REG_PATCHINFO_NZ ( d_p ,
                       sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3   )
  int_p d_p ;
  int_p sp1  , ep1  , sp2  , ep2 ,  sp3  , ep3 ;
{
  int d, i, j, k ;
  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "rsl_get_run_info: bad domain" ) ;
  *sp2 = -1 ; *ep2 = -1 ;
  *sp3 = -1 ; *ep3 = -1 ;
  for ( k = 0 ; k < domain_info[d].len_z ; k++ )
  {
    for ( j = 0 ; j < domain_info[d].len_n ; j++ )
    {
      if ( rsl_c_comp2phys_proc(domain_info[d].domain_nz[INDEX_2(k,j,domain_info[d].len_n)].P)==rsl_myproc)
      {
        if ( *sp2 < 0 ) *sp2 = j + 1 ;
        *ep2 = j + 1 ;
        if ( *sp3 < 0 ) *sp3 = k + 1 ;
        *ep3 = k + 1 ;
      }
    }
  }
  *sp1 = 1 ;
  *ep1 = domain_info[d].len_m ;
}

RSL_GET_GLEN ( d_p , mlen_p , nlen_p , zlen_p )
  int_p d_p, mlen_p, nlen_p, zlen_p ;
{
  rsl_domain_info_t *dinfo ;
  int d ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_get_get_glen: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_get_glen: invalid domain") ;

  dinfo = &(domain_info[d]) ;
  *mlen_p = dinfo->len_m ;
  *nlen_p = dinfo->len_n ;
  *zlen_p = dinfo->len_z ;
  return(0) ;
}
