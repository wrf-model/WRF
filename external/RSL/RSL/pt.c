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

/* apply the function f to each of the points of the stencil in order:

                       1
                    2 00  3
                       4

NOTE: as written, this routine assumes that the ns index is minor as
in the NCAR MM.

*/

rsl_4pt( d, min, minlen, maj, majlen, f  )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
if ( maj >= 0 && maj < majlen )
{
  /* n1 */ if ( min+1 < minlen ) (*f)( d, min+1, maj, min, maj, 1, 4 ) ;
  /* s1 */ if ( min-1 >= 0 )      (*f)( d, min-1, maj, min, maj, 4, 1 ) ;
}
if ( min >= 0 && min < minlen )
{
  /* w1 */ if ( maj-1 >= 0 )      (*f)( d, min, maj-1, min, maj, 2, 3 ) ;
  /* e1 */ if ( maj+1 < majlen ) (*f)( d, min, maj+1, min, maj, 3, 2 ) ;
}
/* 00 */ if ( maj >= 0 && maj < majlen &&
              min >= 0 && min < minlen ) (*f)( d, min, maj, min, maj, 0, 0 ) ;
}

/* Comment about periodic boundaries and staggered fields. JM 20031223 

This is a bit of a stretch from the point of view of intuition, but if
you have the following grid that is 5 dot points and 4 cross points
across:

        0     1     2     3     4     5
        .  x  .  x  .  x  .  x  .  x  .
           0     1     2     3     4 

then the correct way to update a 1-deep periodic boundary is:

(not 5!)                                  (not zero!)
  4     0     1     2     3     4     5     1
  .  x  .  x  .  x  .  x  .  x  .  x  .  x  .
     4     0     1     2     3     4     0

It may help to think about this as the 0 and 5 dot points being
copies of each other. */

/* periodic version ; Feb 99 */
rsl_period_pt( dir, d, min, minlen, minstag, maj, majlen, majstag, fld, bdyw, f  )
  int dir ;    /* direction: 0 is M, 1 is N */
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  int minstag ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int majstag ;
  int bdyw ;
  rsl_fldspec_t * fld ;
  int (*f)() ;
{
  if ( bdyw >= 1 )
  {
    if      ( dir == 1 )   /* RSL_N */
    {
      if ( majstag )
      {
        /* Dot point at YS (index 0) replicated to last dot point in domain before XE boundary, (majlen-1) */
        if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min-1, maj, min-1,    majlen-0, -1, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,    majlen-0,  0, 0, fld ) ;
                                                                    (*f)( d, min+1, maj, min+1,    majlen-0,  1, 0, fld ) ; }
        /* Dot point at YS+1 (index 1) replicated to first dot point in YE boundary, (majlen) */
        if ( maj == 1        && min >=0 && min < minlen+minstag ) { (*f)( d, min-1, maj, min-1,    majlen+1, -1, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,    majlen+1,  0, 0, fld ) ;
                                                                    (*f)( d, min+1, maj, min+1,    majlen+1,  1, 0, fld ) ; }
        /* YE edge of domain goes to YS edge - 1 */
        if ( maj == majlen-1 && min >=0 && min < minlen+minstag ) { (*f)( d, min-1, maj, min-1,    -1,     -1, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,    -1,      0, 0, fld ) ;
                                                                    (*f)( d, min+1, maj, min+1,    -1,      1, 0, fld ) ; }

      }
      else
      {
        /* YS edge of domain goes to YE edge + 1 */
        if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min-1, maj, min-1,    majlen, -1, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,    majlen,  0, 0, fld ) ;
                                                                    (*f)( d, min+1, maj, min+1,    majlen,  1, 0, fld ) ; }

        /* YE edge of domain goes to YS edge - 1 */
        if ( maj == majlen-1 && min >=0 && min < minlen )         { (*f)( d, min-1, maj, min-1,    -1,     -1, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,    -1,      0, 0, fld ) ;
                                                                    (*f)( d, min+1, maj, min+1,    -1,      1, 0, fld ) ; }
      }
    }
    else if ( dir == 0 )   /* RSL_M */
    {
      if ( minstag )
      {
        /* Dot point at XS (index 0) replicated to last dot point in domain before XE boundary, (minlen-1) */
        if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-1, minlen-0, maj-1,    0, -1, fld ) ;
                                                                    (*f)( d, min, maj  , minlen-0, maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+1, minlen-0, maj+1,    0,  1, fld ) ; }
        if ( min == 1        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-1, minlen+1, maj-1,    0, -1, fld ) ;
                                                                    (*f)( d, min, maj  , minlen+1, maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+1, minlen+1, maj+1,    0,  1, fld ) ; }
        /* XE edge of domain goes to XS edge - 1 */
        if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-1, -1,       maj-1,    0, -1, fld ) ;
                                                                    (*f)( d, min, maj  , -1,       maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+1, -1,       maj+1,    0,  1, fld ) ; }
      }
      else
      {
        /* XS edge of domain goes to XE edge + 1 */
        if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-1, minlen, maj-1,    0, -1, fld ) ;
                                                                    (*f)( d, min, maj  , minlen, maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+1, minlen, maj+1,    0,  1, fld ) ; }

        /* XE edge of domain goes to XS edge - 1 */
        if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-1, -1,     maj-1,    0, -1, fld ) ;
                                                                    (*f)( d, min, maj  , -1,     maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+1, -1,     maj+1,    0,  1, fld ) ; }
      }
    }

/* sw corner to ne corner */
    if ( min == 0+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag, majlen+majstag,   0,  0, fld ) ; }
/* nw corner to se corner */
    if ( min == 0+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag, -1            ,   0,  0, fld ) ; }
/* se corner to nw corner */
    if ( min == minlen-1-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag,   0,  0, fld ) ; }
/* ne corner to sw corner */
    if ( min == minlen-1-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -1            , -1            ,   0,  0, fld ) ; }

  }
#if 1
  if ( bdyw >= 2 )
  {
    if      ( dir == 1 ) /* RSL_N */
    {
      if ( majstag )
      {
/**/    if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  majlen-0, -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    majlen-0,  0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  majlen-0,  2, 0, fld ) ; }
        if ( maj == 1        && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  majlen+1, -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    majlen+1,  0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  majlen+1,  2, 0, fld ) ; }
        if ( maj == 2        && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  majlen+2, -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    majlen+2,  0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  majlen+2,  2, 0, fld ) ; }
        if ( maj == majlen-2 && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  -2,       -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    -2,        0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  -2,        2, 0, fld ) ; }
/**/    if ( maj == majlen-1 && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  -1,       -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    -1,        0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  -1,        2, 0, fld ) ; }
      }
      else
      {
/**/    if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  majlen,   -2, 0, fld ) ;
                                                                    (*f)( d, min  , maj, min  ,  majlen,    0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  majlen,    2, 0, fld ) ; }
        if ( maj == 1        && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  majlen+1, -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    majlen+1,  0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  majlen+1,  2, 0, fld ) ; }
        if ( maj == majlen-2 && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  -2,       -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    -2,        0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  -2,        2, 0, fld ) ; }
/**/    if ( maj == majlen-1 && min >=0 && min < minlen+minstag ) { (*f)( d, min-2, maj, min-2,  -1,       -2, 0, fld ) ;
                                                                    (*f)( d, min,   maj, min,    -1,        0, 0, fld ) ;
                                                                    (*f)( d, min+2, maj, min+2,  -1,        2, 0, fld ) ; }
      }
    }
    else if ( dir == 0 ) /* RSL_M */
    {
      if ( minstag )
      {
/**/    if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, minlen-0, maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   minlen-0, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, minlen-0, maj+2,    0,  2, fld ) ; }
/**/    if ( min == 1        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, minlen+1, maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   minlen+1, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, minlen+1, maj+2,    0,  2, fld ) ; }
        if ( min == 2        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, minlen+2, maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   minlen+2, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, minlen+2, maj+2,    0,  2, fld ) ; }
        if ( min == minlen-2 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, -2,       maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   -2,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, -2,       maj+2,    0,  2, fld ) ; }
/**/    if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, -1,       maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   -1,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, -1,       maj+2,    0,  2, fld ) ; }
      }
      else
      {
/**/    if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, minlen,   maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj  , minlen,   maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, minlen,   maj+2,    0,  2, fld ) ; }
        if ( min == 1        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, minlen+1, maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj  , minlen+1, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, minlen+1, maj+2,    0,  2, fld ) ; }
        if ( min == minlen-2 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, -2,       maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj,   -2,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, -2,       maj+2,    0,  2, fld ) ; }
/**/    if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj-2, -1,       maj-2,    0, -2, fld ) ;
                                                                    (*f)( d, min, maj  , -1,       maj  ,    0,  0, fld ) ;
                                                                    (*f)( d, min, maj+2, -1,       maj+2,    0,  2, fld ) ; }
      }
    }

/* sw corner to ne corner */
/*  if ( min == 0+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag  , majlen+majstag  ,   0,  0, fld ) ; } */
    if ( min == 1+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag+1, majlen+majstag  ,   0,  0, fld ) ; }
    if ( min == 0+minstag        && maj == 1+majstag        ) { (*f)( d, min, maj  , minlen+minstag  , majlen+majstag+1,   0,  0, fld ) ; }
    if ( min == 1+minstag        && maj == 1+majstag        ) { (*f)( d, min, maj  , minlen+minstag+1, majlen+majstag+1,   0,  0, fld ) ; }
/* nw corner to se corner */
/*  if ( min == 0+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag  , -1            ,   0,  0, fld ) ; } */
    if ( min == 0+minstag        && maj == majlen-2-majstag ) { (*f)( d, min, maj  , minlen+minstag  , -2            ,   0,  0, fld ) ; }
    if ( min == 1+minstag        && maj == majlen-2-majstag ) { (*f)( d, min, maj  , minlen+minstag+1, -2            ,   0,  0, fld ) ; }
    if ( min == 1+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag+1, -1            ,   0,  0, fld ) ; }
/* se corner to nw corner */
/*  if ( min == minlen-1-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag  ,   0,  0, fld ) ; } */
    if ( min == minlen-2-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -2            , majlen+majstag  ,   0,  0, fld ) ; }
    if ( min == minlen-2-minstag && maj == 1+majstag        ) { (*f)( d, min, maj  , -2            , majlen+majstag+1,   0,  0, fld ) ; }
    if ( min == minlen-1-minstag && maj == 1+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag+1,   0,  0, fld ) ; }
/* ne corner to sw corner */
/*  if ( min == minlen-1-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -1            , -1            ,   0,  0, fld ) ; } */
    if ( min == minlen-1-minstag && maj ==majlen-2-majstag  ) { (*f)( d, min, maj  , -1            , -2            ,   0,  0, fld ) ; }
    if ( min == minlen-2-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -2            , -1            ,   0,  0, fld ) ; }
    if ( min == minlen-2-minstag && maj ==majlen-2-majstag  ) { (*f)( d, min, maj  , -2            , -2            ,   0,  0, fld ) ; }

  }

#endif

  if ( bdyw >= 3 )
  {
    if      ( dir == 1 ) /* RSL_N */
    {
      if ( majstag )
      {
        if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen-0, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen-0,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen-0,  3, 0, fld ) ; }
        if ( maj == 1        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+1, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+1,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+1,  3, 0, fld ) ; }
        if ( maj == 2        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+2, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+2,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+2,  3, 0, fld ) ; }
        if ( maj == 3        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+3, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+3,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+3,  3, 0, fld ) ; }
        if ( maj == majlen-3 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -3,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -3,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -3,        3, 0, fld ) ; }
        if ( maj == majlen-2 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -2,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -2,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -2,        3, 0, fld ) ; }
        if ( maj == majlen-1 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -1,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -1,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -1,        3, 0, fld ) ; }

      }
      else
      {
        if ( maj == 0        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+0, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+0,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+0,  3, 0, fld ) ; }
        if ( maj == 1        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+1, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+1,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+1,  3, 0, fld ) ; }
        if ( maj == 2        && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      majlen+2, -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+2,  0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      majlen+2,  3, 0, fld ) ; }
        if ( maj == majlen-3 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -3,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -3,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -3,        3, 0, fld ) ; }
        if ( maj == majlen-2 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -2,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -2,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -2,        3, 0, fld ) ; }
        if ( maj == majlen-1 && min >=0 && min < minlen+minstag ) { (*f)( d, min, maj, min,      -1,       -3, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -1,        0, 0, fld ) ;
                                                                    (*f)( d, min, maj, min,      -1,        3, 0, fld ) ; }
      }
    }
    else if ( dir == 0 ) /* RSL_M */
    {
      if ( minstag )
      {
        if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen-0, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen-0, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen-0, maj,      0,  3, fld ) ; }
        if ( min == 1        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+1, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+1, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+1, maj,      0,  3, fld ) ; }
        if ( min == 2        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+2, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+2, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+2, maj,      0,  3, fld ) ; }
        if ( min == 3        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+3, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+3, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+3, maj,      0,  3, fld ) ; }
        if ( min == minlen-3 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -3,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -3,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -3,       maj,      0,  3, fld ) ; }
        if ( min == minlen-2 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -2,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -2,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -2,       maj,      0,  3, fld ) ; }
        if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -1,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -1,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -1,       maj,      0,  3, fld ) ; }
      }
      else
      {
        if ( min == 0        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+0, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+0, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+0, maj,      0,  3, fld ) ; }
        if ( min == 1        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+1, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+1, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+1, maj,      0,  3, fld ) ; }
        if ( min == 2        && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, minlen+2, maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, minlen+2, maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, minlen+2, maj,      0,  3, fld ) ; }
        if ( min == minlen-3 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -3,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -3,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -3,       maj,      0,  3, fld ) ; }
        if ( min == minlen-2 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -2,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -2,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -2,       maj,      0,  3, fld ) ; }
        if ( min == minlen-1 && maj >=0 && maj < majlen+majstag ) { (*f)( d, min, maj, -1,       maj,      0, -3, fld ) ;
                                                                    (*f)( d, min, maj, -1,       maj,      0,  0, fld ) ;
                                                                    (*f)( d, min, maj, -1,       maj,      0,  3, fld ) ; }
      }
    }
  }
/* sw corner to ne corner */
/*  if ( min == 0+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag  , majlen+majstag  ,   0,  0, fld ) ; } */
/*  if ( min == 1+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag+1, majlen+majstag  ,   0,  0, fld ) ; } */
/*  if ( min == 0+minstag        && maj == 1+majstag        ) { (*f)( d, min, maj  , minlen+minstag  , majlen+majstag+1,   0,  0, fld ) ; } */
/*  if ( min == 1+minstag        && maj == 1+majstag        ) { (*f)( d, min, maj  , minlen+minstag+1, majlen+majstag+1,   0,  0, fld ) ; } */
    if ( min == 2+minstag        && maj == 0+majstag        ) { (*f)( d, min, maj  , minlen+minstag+2, majlen+majstag  ,   0,  0, fld ) ; }
    if ( min == 2+minstag        && maj == 1+majstag        ) { (*f)( d, min, maj  , minlen+minstag+2, majlen+majstag+1,   0,  0, fld ) ; }
    if ( min == 2+minstag        && maj == 2+majstag        ) { (*f)( d, min, maj  , minlen+minstag+2, majlen+majstag+2,   0,  0, fld ) ; }
    if ( min == 1+minstag        && maj == 2+majstag        ) { (*f)( d, min, maj  , minlen+minstag+1, majlen+majstag+2,   0,  0, fld ) ; }
    if ( min == 0+minstag        && maj == 2+majstag        ) { (*f)( d, min, maj  , minlen+minstag  , majlen+majstag+2,   0,  0, fld ) ; }
/* nw corner to se corner */
/*  if ( min == 0+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag  , -1            ,   0,  0, fld ) ; } */
/*  if ( min == 0+minstag        && maj == majlen-2-majstag ) { (*f)( d, min, maj  , minlen+minstag  , -2            ,   0,  0, fld ) ; } */
/*  if ( min == 1+minstag        && maj == majlen-2-majstag ) { (*f)( d, min, maj  , minlen+minstag+1, -2            ,   0,  0, fld ) ; } */
/*  if ( min == 1+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag+1, -1            ,   0,  0, fld ) ; } */
    if ( min == 2+minstag        && maj == majlen-1-majstag ) { (*f)( d, min, maj  , minlen+minstag+2, -1            ,   0,  0, fld ) ; }
    if ( min == 2+minstag        && maj == majlen-2-majstag ) { (*f)( d, min, maj  , minlen+minstag+2, -2            ,   0,  0, fld ) ; }
    if ( min == 2+minstag        && maj == majlen-3-majstag ) { (*f)( d, min, maj  , minlen+minstag+2, -3            ,   0,  0, fld ) ; }
    if ( min == 1+minstag        && maj == majlen-3-majstag ) { (*f)( d, min, maj  , minlen+minstag+1, -3            ,   0,  0, fld ) ; }
    if ( min == 0+minstag        && maj == majlen-3-majstag ) { (*f)( d, min, maj  , minlen+minstag  , -3            ,   0,  0, fld ) ; }
/* se corner to nw corner */
/*  if ( min == minlen-1-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag  ,   0,  0, fld ) ; } */
/*  if ( min == minlen-2-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -2            , majlen+majstag  ,   0,  0, fld ) ; } */
/*  if ( min == minlen-2-minstag && maj == 1+majstag        ) { (*f)( d, min, maj  , -2            , majlen+majstag+1,   0,  0, fld ) ; } */
/*  if ( min == minlen-1-minstag && maj == 1+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag+1,   0,  0, fld ) ; } */
    if ( min == minlen-3-minstag && maj == 0+majstag        ) { (*f)( d, min, maj  , -3            , majlen+majstag+0,   0,  0, fld ) ; }
    if ( min == minlen-3-minstag && maj == 1+majstag        ) { (*f)( d, min, maj  , -3            , majlen+majstag+1,   0,  0, fld ) ; }
    if ( min == minlen-3-minstag && maj == 2+majstag        ) { (*f)( d, min, maj  , -3            , majlen+majstag+2,   0,  0, fld ) ; }
    if ( min == minlen-2-minstag && maj == 2+majstag        ) { (*f)( d, min, maj  , -2            , majlen+majstag+2,   0,  0, fld ) ; }
    if ( min == minlen-1-minstag && maj == 2+majstag        ) { (*f)( d, min, maj  , -1            , majlen+majstag+2,   0,  0, fld ) ; }
/* ne corner to sw corner */
/*  if ( min == minlen-1-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -1            , -1            ,   0,  0, fld ) ; } */
/*  if ( min == minlen-1-minstag && maj ==majlen-2-majstag  ) { (*f)( d, min, maj  , -1            , -2            ,   0,  0, fld ) ; } */
/*  if ( min == minlen-2-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -2            , -1            ,   0,  0, fld ) ; } */
/*  if ( min == minlen-2-minstag && maj ==majlen-2-majstag  ) { (*f)( d, min, maj  , -2            , -2            ,   0,  0, fld ) ; } */
    if ( min == minlen-3-minstag && maj ==majlen-1-majstag  ) { (*f)( d, min, maj  , -3            , -1            ,   0,  0, fld ) ; }
    if ( min == minlen-3-minstag && maj ==majlen-2-majstag  ) { (*f)( d, min, maj  , -3            , -2            ,   0,  0, fld ) ; }
    if ( min == minlen-3-minstag && maj ==majlen-3-majstag  ) { (*f)( d, min, maj  , -3            , -3            ,   0,  0, fld ) ; }
    if ( min == minlen-2-minstag && maj ==majlen-3-majstag  ) { (*f)( d, min, maj  , -2            , -3            ,   0,  0, fld ) ; }
    if ( min == minlen-1-minstag && maj ==majlen-3-majstag  ) { (*f)( d, min, maj  , -1            , -3            ,   0,  0, fld ) ; }

}


/* apply the function f to each of the points of the stencil in order:

                    1  2  3
                    4 00  5   
                    6  7  8

NOTE: as written, this routine assumes that the ns index is minor as
in the NCAR MM.

*/

rsl_8pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
if ( maj >= 0 && maj < majlen )
{
  /* n1 */ if ( min+1 < minlen ) (*f)( d, min+1, maj, min, maj, 2, 7 ) ;
  /* s1 */ if ( min-1 >= 0 )      (*f)( d, min-1, maj, min, maj, 7, 2 ) ;
}
if ( min >= 0 && min < minlen )
{
  /* w1 */ if ( maj-1 >= 0 )      (*f)( d, min, maj-1, min, maj, 4, 5 ) ;
  /* e1 */ if ( maj+1 < majlen ) (*f)( d, min, maj+1, min, maj, 5, 4 ) ;
}
/* nw */ if ( maj-1 >= 0 &&
              min+1 < minlen ) (*f)( d, min+1, maj-1, min, maj, 1, 8  ) ;
/* ne */ if ( maj+1 < majlen &&
              min+1 < minlen ) (*f)( d, min+1, maj+1, min, maj, 3, 6 ) ;
/* sw */ if ( maj-1 >= 0 &&
              min-1 >= 0 )      (*f)( d, min-1, maj-1, min, maj, 6, 3 ) ;
/* se */ if ( maj+1 < majlen &&
              min-1 >= 0 )      (*f)( d, min-1, maj+1, min, maj, 8, 1 ) ;
/* 00 */ if ( maj >= 0 && maj < majlen &&
              min >= 0 && min < minlen ) (*f)( d, min, maj, min, maj, 0, 0 ) ;
}

/* apply the function f to each of the points of the stencil in order:

                       1
                    2  3  4
                 5  6 00  7  8
                    9 10 11
                      12

NOTE: as written, this routine assumes that the ns index is minor as
in the NCAR MM.

*/

rsl_12pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
if ( maj >= 0 && maj < majlen )
{
  /* n2 */ if ( min+2 < minlen ) (*f)( d, min+2, maj, min, maj, 1, 12 ) ;
  /* n1 */ if ( min+1 < minlen ) (*f)( d, min+1, maj, min, maj, 3, 10 ) ;
  /* s1 */ if ( min-1 >= 0 )      (*f)( d, min-1, maj, min, maj, 10, 3 ) ;
  /* s2 */ if ( min-2 >= 0 )      (*f)( d, min-2, maj, min, maj, 12, 1 ) ;
}
if ( min >= 0 && min < minlen )
{
  /* w2 */ if ( maj-2 >= 0 )      (*f)( d, min, maj-2, min, maj, 5, 8 ) ;
  /* w1 */ if ( maj-1 >= 0 )      (*f)( d, min, maj-1, min, maj, 6, 7 ) ;
  /* e1 */ if ( maj+1 < majlen ) (*f)( d, min, maj+1, min, maj, 7, 6 ) ;
  /* e2 */ if ( maj+2 < majlen ) (*f)( d, min, maj+2, min, maj, 8, 5 ) ;
}
/* nw */ if ( maj-1 >= 0 &&
              min+1 < minlen ) (*f)( d, min+1, maj-1, min, maj, 2, 11 ) ;
/* ne */ if ( maj+1 < majlen && 
              min+1 < minlen ) (*f)( d, min+1, maj+1, min, maj,  4, 9 ) ;
/* sw */ if ( maj-1 >= 0 && 
              min-1 >= 0 )      (*f)( d, min-1, maj-1, min, maj, 9, 4 ) ;
/* se */ if ( maj+1 < majlen && 
              min-1 >= 0 )      (*f)( d, min-1, maj+1, min, maj, 11, 2 ) ;
/* 00 */ if ( maj >= 0 && maj < majlen && 
              min >= 0 && min < minlen ) (*f)( d, min, maj, min, maj, 0, 0 ) ;
}

/* apply the function f to each of the points of the stencil in order:

                 1  2  3  4  5
                 6  7  8  9 10
                11 12 00 13 14
                15 16 17 18 19
                20 21 22 23 24

NOTE: as written, this routine assumes that the ns index is minor as
in the NCAR MM.

*/
static char pts[] = { 20, 21, 22, 23, 24,
                      15, 16, 17, 18, 19,
                      11, 12,  0, 13, 14,
                       6,  7,  8,  9, 10,
                       1,  2,  3,  4,  5, } ;

static char ipts[] = { 5,  4,  3,  2,  1,
                      10,  9,  8,  7,  6,
                      14, 13,  0, 12, 11,
                      19, 18, 17, 16, 15,
                      24, 23, 22, 21, 20, } ;

rsl_24pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
  rsl_index_t i, j, k ;

  k = 0 ;
  for ( i = -2 ; i <= 2 ; i++ )
  {
    for ( j = -2 ; j <= 2 ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {
        (*f)(d,min+i,maj+j,min,maj,pts[k],ipts[k]) ;
      }
      k++ ;
    }
  }
}

#if ( ALLOW_RSL_168PT == 1 )
/* apply the function f to each of the points of the stencil in order:

  1   2   3   4   5   6   7   8   9  10  11  12  13
 14  15  16  17  18  19  20  21  22  23  23  25  26
 27  28  29  30  31  32  33  34  35  36  37  38  39
 40  41  42  43  44  45  46  47  48  49  50  51  52
 53  54  55  56  57  58  59  60  61  62  63  64  65
 66  67  68  69  70  71  72  73  74  75  76  77  78
 79  80  81  82  83  84  00  85  86  87  88  89  90
 91  92  93  94  95  96  97  98  99 100 101 102 103
104 105 106 107 108 109 110 111 112 113 114 115 116
117 118 119 120 121 122 123 124 125 126 127 128 129
130 131 132 133 134 135 136 137 138 139 140 141 142
143 144 145 146 147 148 149 150 151 152 153 154 155
156 157 158 159 160 161 162 163 164 165 166 167 168

NOTE: as written, this routine assumes that the ns index is minor as
in the NCAR MM.

Here is a shell script for converting the above table into 
the form that is assigned to ipts168 below (it reverses
the columns).   Commas get added separately.

#
/bin/cp xxx yyy
echo phase 1
set i=0
while ($i < 13)
  echo $i
  cut -c1-4 yyy > /tmp/$i.temp
  cut -c5-  yyy > foo
  /bin/mv foo yyy
  @ i += 1
end
echo phase 1
/bin/cp /tmp/0.temp yyy
set i=1
while ($i < 13)
  echo $i
  paste /tmp/$i.temp yyy | sed 's/    //' > foo
  /bin/mv foo yyy
  @ i += 1
end

*/


static rsl_index_t pts48[] = {
    42, 43, 44, 45, 46, 47, 48,
    35, 36, 37, 38, 39, 40, 41,
    28, 29, 30, 31, 32, 33, 34,
    22, 23, 24, 00, 25, 26, 27,
    15, 16, 17, 18, 19, 20, 21,
     8,  9, 10, 11, 12, 13, 14,
     1,  2,  3,  4,  5,  6,  7
    } ;

static rsl_index_t ipts48[] = {
     7,  6,  5,  4,  3,  2,  1,
    14, 13, 12, 11, 10,  9,  8,
    21, 20, 19, 18, 17, 16, 15,
    27, 26, 25, 00, 24, 23, 22,
    34, 33, 32, 31, 30, 29, 28,
    41, 40, 39, 38, 37, 36, 35,
    48, 47, 46, 45, 44, 43, 42
    } ;


rsl_48pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
  rsl_index_t i, j, k ;

  k = 0 ;
  for ( i = -3 ; i <= 3 ; i++ )
  {
    for ( j = -3 ; j <= 3 ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {
        (*f)(d,min+i,maj+j,min,maj,pts48[k],ipts48[k]) ;
      }
      k++ ;
    }
  }
}

static rsl_index_t pts80[] = {
    72, 73, 74, 75, 76, 77, 78, 79, 80,
    63, 64, 65, 66, 67, 68, 69, 70, 71,
    54, 55, 56, 57, 58, 59, 60, 61, 62,
    45, 46, 47, 48, 49, 50, 51, 52, 53,
    37, 38, 39, 40, 00, 41, 42, 43, 44,
    28, 29, 30, 31, 32, 33, 34, 35, 36,
    19, 20, 21, 22, 23, 24, 25, 26, 27,
    10, 11, 12, 13, 14, 15, 16, 17, 18,
     1,  2,  3,  4,  5,  6,  7,  8,  9,
    } ;

static rsl_index_t ipts80[] = {
     9,  8,  7,  6,  5,  4,  3,  2,  1,
    18, 17, 16, 15, 14, 13, 12, 11, 10,
    27, 26, 25, 24, 23, 22, 21, 20, 19,
    36, 35, 34, 33, 32, 31, 30, 29, 28,
    44, 43, 42, 41, 00, 40, 39, 38, 37,
    53, 52, 51, 50, 49, 48, 47, 46, 45,
    62, 61, 60, 59, 58, 57, 56, 55, 54,
    71, 70, 69, 68, 67, 66, 65, 64, 63,
    80, 79, 78, 77, 76, 75, 74, 73, 72
    } ;


rsl_80pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
  rsl_index_t i, j, k ;

  k = 0 ;
  for ( i = -4 ; i <= 4 ; i++ )
  {
    for ( j = -4 ; j <= 4 ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {
        (*f)(d,min+i,maj+j,min,maj,pts80[k],ipts80[k]) ;
      }
      k++ ;
    }
  }
}

static rsl_index_t pts120[] = {
   110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
    99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
    88,  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,
    77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,
    66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,
    56,  57,  58,  59,  60,  00,  61,  62,  63,  64,  65,
    45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,
    34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
    23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,
    12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,
     1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,
    } ;

static rsl_index_t ipts120[] = {
    11,  10,   9,   8,   7,   6,   5,   4,   3,   2,   1,
    22,  21,  20,  19,  18,  17,  16,  15,  14,  13,  12,
    33,  32,  31,  30,  29,  28,  27,  26,  25,  24,  23,
    44,  43,  42,  41,  40,  39,  38,  37,  36,  35,  34,
    55,  54,  53,  52,  51,  50,  49,  48,  47,  46,  45,
    65,  64,  63,  62,  61,  00,  60,  59,  58,  57,  56,
    76,  75,  74,  73,  72,  71,  70,  69,  68,  67,  66,
    87,  86,  85,  84,  83,  82,  81,  80,  79,  78,  77,
    98,  97,  96,  95,  94,  93,  92,  91,  90,  89,  88,
   109, 108, 107, 106, 105, 104, 103, 102, 101, 100,  99,
   120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110
    } ;


rsl_120pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
  rsl_index_t i, j, k ;

  k = 0 ;
  for ( i = -5 ; i <= 5 ; i++ )
  {
    for ( j = -5 ; j <= 5 ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {
        (*f)(d,min+i,maj+j,min,maj,pts120[k],ipts120[k]) ;
      }
      k++ ;
    }
  }
}

static rsl_index_t pts168[] = {
   156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168,
   143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155,
   130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
   117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
   104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
    91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103,
    79,  80,  81,  82,  83,  84,  00,  85,  86,  87,  88,  89,  90,
    66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,
    53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,  64,  65,
    40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,
    27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
    14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  23,  25,  26,
     1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13
    } ;

static rsl_index_t ipts168[] = {
    13,  12,  11,  10,   9,   8,   7,   6,   5,   4,   3,   2,   1, 
    26,  25,  23,  23,  22,  21,  20,  19,  18,  17,  16,  15,  14, 
    39,  38,  37,  36,  35,  34,  33,  32,  31,  30,  29,  28,  27, 
    52,  51,  50,  49,  48,  47,  46,  45,  44,  43,  42,  41,  40, 
    65,  64,  63,  62,  61,  60,  59,  58,  57,  56,  55,  54,  53, 
    78,  77,  76,  75,  74,  73,  72,  71,  70,  69,  68,  67,  66, 
    90,  89,  88,  87,  86,  85,  00,  84,  83,  82,  81,  80,  79, 
   103, 102, 101, 100,  99,  98,  97,  96,  95,  94,  93,  92,  91, 
   116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 
   129, 128, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 
   142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 
   155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 
   168, 167, 166, 165, 164, 163, 162, 161, 160, 159, 158, 157, 156
    } ;


rsl_168pt( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
  rsl_index_t i, j, k ;

  k = 0 ;
  for ( i = -6 ; i <= 6 ; i++ )
  {
    for ( j = -6 ; j <= 6 ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {
        (*f)(d,min+i,maj+j,min,maj,pts168[k],ipts168[k]) ;
      }
      k++ ;
    }
  }
}

/* March 1997 */

rsl_2ptm( d, min, minlen, maj, majlen, f  )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
if ( maj >= 0 && maj < majlen )
{
  /* n1 */ if ( min+1 < minlen ) (*f)( d, min+1, maj, min, maj, 1, 4 ) ;
  /* s1 */ if ( min-1 >= 0 )      (*f)( d, min-1, maj, min, maj, 4, 1 ) ;
}
/* 00 */ if ( maj >= 0 && maj < majlen &&
              min >= 0 && min < minlen ) (*f)( d, min, maj, min, maj, 0, 0 ) ;
}

rsl_4ptm( d, min, minlen, maj, majlen, f )
  rsl_index_t d ;
  rsl_index_t min ;
  rsl_dimlen_t minlen ;
  rsl_index_t maj ;
  rsl_dimlen_t majlen ;
  int (*f)() ;
{
if ( maj >= 0 && maj < majlen )
{
  /* n2 */ if ( min+2 < minlen ) (*f)( d, min+2, maj, min, maj, 1, 12 ) ;
  /* n1 */ if ( min+1 < minlen ) (*f)( d, min+1, maj, min, maj, 3, 10 ) ;
  /* s1 */ if ( min-1 >= 0 )      (*f)( d, min-1, maj, min, maj, 10, 3 ) ;
  /* s2 */ if ( min-2 >= 0 )      (*f)( d, min-2, maj, min, maj, 12, 1 ) ;
}
/* 00 */ if ( maj >= 0 && maj < majlen && 
              min >= 0 && min < minlen ) (*f)( d, min, maj, min, maj, 0, 0 ) ;
}


#endif
