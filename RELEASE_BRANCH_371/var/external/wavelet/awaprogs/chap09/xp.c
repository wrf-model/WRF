/*
 *
 * Transpose multidimensional arrays.
 *
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 */

#include <assert.h>
#include "common.h"	// Common definitions and macros.
#include "real.h"
#include "xp.h"


/***************************************************************************
 * xpi2()
 *
 * [XP]Transpose [I]n-place a [2]-dimensional array of arbitrary objects,
 * presented as a one-dimensional array.
 *
 *  Calling sequence and basic algorithm:
 *
 *   xpi2( DATA, X, Y, SIZE ):
 *      For N = 1 to X*Y-2
 *        If DATA[N] has not been permuted then
 *           Let TEMP = DATA[N]
 *           Let TARGET = N
 *           Let SOURCE = (N*Y) % (X*Y-1)
 *           While SOURCE > N
 *              Let DATA[TARGET] = DATA[SOURCE]
 *              Let TARGET = SOURCE
 *              Let SOURCE = (SOURCE*Y) % (X*Y-1)
 *           Let DATA[TARGET] = TEMP
 *
 *  Inputs:
 *	(void *)data	This is both the input and output; it must contain
 * 			  at least `x*y' elements.
 *
 *	(int)x		These positive integers give the number of rows
 *	(int)y		  and columns in the array `data[]'
 *
 *	(size_t)size	This is the size in bytes of `data[]' elements.
 *
 *
 * Outputs:
 *	(void *)data	This array is transposed in place by reference.
 *
 * External functions called:
 *	assert(), malloc(), free(), memcpy()
 *
 * Assumptions:
 *	1. x>0
 *	2. y>0
 *	3. data != NULL
 *	4. size>0
 */
extern void
  xpi2(
       void *data,		/* Array to be transposed in place.     */
       int   x,			/* Number of rows in `data[]'.          */
       int   y,			/* Number of columns in `data[]'.       */
       size_t size)		/* Number of bytes per `data[]' member. */
{
  void *holder, *target, *source;
  
  assert(data);
  assert(x>0);
  assert(y>0);
  assert(size>0);

  /* Allocate one temporary storage element: */
  holder = malloc(size);  assert(holder);

  /* Use a simple algorithm for SQUARE ARRAYS: */
  if(x==y)
    {
      int i, j;

      for(i=0; i<x; i++)
	for( j=i+1; j<x; j++)
	  {
	    target = data + (i*x+j)*size;
	    source = data + (j*x+i)*size;
	    memcpy( holder, target, size );
	    memcpy( target, source, size );
	    memcpy( source, holder, size );
	  }
    }				/* -- end SQUARE -- */


  /* We need some number theory for NON-SQUARE ARRAYS: */
  else
    {
      int mod, earlier;
      int n, fn, ifn;

#define AWA_XPI2_SOURCE(n)	( ((n)*y) % mod )
#define AWA_XPI2_TARGET(n)	( ((n)*x) % mod )

      /* `... <-- data[n*x % mod] <-- data[n] <-- data[n*y % mod] <-- ...' */
      mod = x*y-1;

      /* Use the offset `n' into the array `data[]' as the permuted index: */
      for(n=1; n<mod; n++)	/*  n=0 and n=mod=x*y-1 are fixed points. */
	{
	  /* Test whether `n' is fixed by the transposition: */
	  fn  = AWA_XPI2_SOURCE(n);
	  if(fn == n) continue;

	  /* Test whether `n' belonged to a previous cycle: */
	  ifn = AWA_XPI2_TARGET(n);
	  earlier = FALSE;
	  while( fn != n )
	    {
	      if( fn<n || ifn<n )
		{
		  earlier = TRUE;
		  break;
		}
	      fn  = AWA_XPI2_SOURCE(fn);
	      ifn = AWA_XPI2_TARGET(ifn);	  
	    }
	  if( !earlier )
	    {
	      /* Permute this new non-singleton cycle with least index `n': */
	      fn = AWA_XPI2_SOURCE(n);
	      target = data + n*size;
	      memcpy( holder, target, size );
	      while( fn > n )
		{
		  source = data + fn*size;
		  memcpy( target, source, size );
		  target = source;
		  fn = AWA_XPI2_SOURCE(fn);
		}
	      memcpy( target, holder, size );
	    }
	}
    }				/*  -- end NON-SQUARE -- */
  free(holder);
  return;
}

/***************************************************************************
 * xpd2()
 *
 * [XP]Transpose and copy a [D]isjoint [2]-dimensional array of arbitrary
 * data objects to another such array.
 *
 *  Calling sequence and basic algorithm:
 *
 *   xpd2( OUT, IN, X, Y, SIZE ):
 *      For I = 0 to X-1
 *         For J = 0 to Y-1
 *            Let OUT[J*X + I] = IN[I*Y + J]
 *
 *  Inputs:
 *	(void *)out		This output array must contain
 * 				  at least `x*y' elements;
 *				  it will be overwritten.
 *
 *	(const void *)in	This input array it must contain
 * 				  at least `x*y' elements;
 *				  it will not be changed.
 *
 *	(int)x		These positive integers give the number of rows
 *	(int)y		  and columns in the array `in[]'
 *
 *	(size_t)size	This is the size in bytes of `data[]' elements.
 *
 * Outputs:
 *	(void *)out	This array is overwritten, even before all
 *			  of `in[]' has been read.
 *
 * External functions called:
 *	memcpy(), assert()
 *
 * Assumptions:
 *	1.  `in[]' and `out[]' are disjoint and non-NULL.
 *	2.  `x' and `y' are positive integers.
 */
extern void
  xpd2(
       void *out,		/* Output: disjoint transposed array   */
       const void *in,		/* Input array, to be transposed       */
       int   x,			/* Number of rows in `in[]'            */
       int   y,			/* Number of columms in `in[]'         */
       size_t size)		/* Number of bytes per `data[]' member */
{
  int i, j;

  assert(in);
  assert(out);
  assert(in != out);
  assert(x>0);
  assert(y>0);
  assert(size>0);

  for( i=0; i<x*size; i+=size )
    for( j=0; j<y*size; j+=size )
      memcpy( out+j*x+i, in+i*y+j, size );
  return;
}

/***************************************************************************
 * cycled()
 *
 * [CYCLE] an array to a [D]isjoint array, putting the last element of
 * the input array at the front of the output array and shifting all other
 * elements back to a position one index higher.
 *
 * If the array length parameter is negative, perform the inverse.
 *
 *  d>0:
 *  in[0];  in[1]; ...;  in[d-2];  in[d-1]
 *   |       |            |         |
 *   V       V            V         V
 * out[1]  out[2]       out[d-1]   out[0]
 *
 *  d<0:
 *  in[0];  in[1]; ...;  in[d-2];  in[d-1]
 *   |       |            |         |
 *   V       V            V         V
 * out[d-1] out[0]      out[d-3]  out[d-2]
 *
 *  Calling sequence and basic algorithm:
 *
 *   cycled( OUT, IN, D, SIZE ):
 *      If D>0 then
 *         For N = 0 to D-2
 *            memcpy( OUT+(N+1)*SIZE, IN+N*SIZE, SIZE )
 *         memcpy( OUT, IN+(D-1)*SIZE, SIZE )
 *      Else
 *         Let D = -D
 *         For N = 1 to D-1
 *            memcpy( OUT+(N-1)*SIZE, IN+N*SIZE, SIZE )
 *         memcpy( OUT+(D-1)*SIZE, IN, SIZE )
 *
 *
 *  Inputs:
 *	(void *)out	This array receives the output.
 *
 *	(const void *)in	This is the input array.
 *
 *	(int)d		This is the number of elements in `in[]' and `out[]'.
 *
 *	(size_t)size	This is the number of bytes in each element
 *
 * Outputs:
 *	(void *)out	This array receives a cyclically permuted copy
 *			  of `in[]' by reference.
 *
 * External functions called:
 *	memcpy(), assert()
 *
 * Assumptions:
 *	1. d != 0
 *	2. in != out
 *	3. size>0
 */
extern void
  cycled(			/* Cyclic permutation to disjoint arrays. */
	 void  *out,		/* Array to receive the cyclic permutation. */
	 const void  *in,	/* Input array. */
	 int    d,		/* Length of `in[],out[]'; invert if `d<0'. */
	 size_t size)		/* Bytes per `in[]' or `out[]' member.   */
{
  int n;
  
  assert(d!=0);
  assert(size>0);
  assert(in != out);

  if(d>0)			/* Cycle in[n] --> out[n+1 mod d]: */
    {
      memcpy(out, in+(d-1)*size, size);
      out += size;
      for(n=0; n<d-1; ++n, in+=size, out+=size )
	memcpy( out, in, size);
    }

  else				/* d<0: Cycle item[n] <-- item[n+1] */
    {
      d = -d;
      memcpy(out+(d-1)*size, in, size);
      in += size;
      for(n=0; n<d-1; ++n, in+=size, out+=size )
	memcpy( out, in, size);
    }

  return;
}

/***************************************************************************
 * cyclei()
 *
 * [CYCLE] an array [I]n place, transposing the last element to the front
 * and shifting all other elements back one index higher.
 *
 * If the array length parameter is negative, perform the inverse.
 *
 *  d>0:
 *  item[0] --> item[1] --> ... --> item[d-1] --> item[0]
 *
 *  d<0:
 *  item[0] <-- item[1] <-- ... <-- item[d-1] <-- item[0]
 *
 *  Calling sequence and basic algorithm:
 *
 *   cyclei( ITEM, D, SIZE ):
 *      If D>0 then
 *         ITEM += (D-1)*SIZE
 *         memcpy(TEMP, ITEM, SIZE) 
 *         For N = 0 to D-2
 *            memcpy( ITEM-N*SIZE, ITEM-(N+1)*SIZE, SIZE )
 *         memcpy( ITEM-(D-1)*SIZE, TEMP, SIZE )
 *      Else
 *         Let D = -D
 *         memcpy(TEMP, ITEM, SIZE) 
 *         For N = 0 to D-2
 *            memcpy( ITEM+N*SIZE, ITEM+(N+1)*SIZE, SIZE )
 *         memcpy( ITEM+(D-1)*SIZE, TEMP, SIZE )
 *
 *
 *  Inputs:
 *	(void *)item	This is both the input and output.
 *
 *	(int)d		This is the number of elements in `item[]'.
 *
 *	(size_t)size	This is the number of bytes in each `item[]' element
 *
 * Outputs:
 *	(void *)item	This array is cyclically permuted in place
 *			  by reference.
 *
 * External functions called:
 *	assert(), malloc(), free(), memcpy()
 *
 * Assumptions:
 *	1. d != 0
 *	2. item != NULL
 *	3. size>0
 */
extern void
  cyclei(			/* `item[0]-->item[1]> >item[d-1]-->item[0]' */
	 void  *item,		/* Array to be cyclically permuted in place. */
	 int    d,		/* Length of `item[]'; inverse if `d<0'. */
	 size_t size)		/* Bytes per `item[]' member.   */
{
  void *temp;
  int n;
  
  assert(d!=0);
  assert(size>0);
  assert(item);

  temp = malloc(size);

  if(d>0)			/* Cycle item[n] --> item[n+1]: */
    {
      item += (d-1)*size;
      memcpy(temp, item, size);
      for(n=0; n<d-1; ++n, item -= size ) memcpy( item, item-size, size);
      memcpy( item, temp, size );
    }

  else				/* d<0: Cycle item[n] <-- item[n+1] */
    {
      d = -d;
      memcpy(temp, item, size);
      for(n=0; n<d-1; ++n, item += size ) memcpy( item, item+size, size);
      memcpy( item, temp, size );
    }

  free(temp);
  return;
}

/***************************************************************************
 * xpid()
 *
 * Cyclically [XP]transpose [I]n-place a [D]-dimensional array of arbitrary
 * data types, presented as a  one-dimensional array.  The basic formula is:
 *
 *  1 < d < 32:
 *  ++++++++++
 *  in[i0][i1]...[i_{d-2}][i_{d-1}] ---> out[i_{d-1}][i0][i1]...[i_{d-2}]
 *
 *  (len[0],len[1],...,len[d-1]) ---> (len[d-1],len[0],len[1],...,len[d-2])
 *
 *  If the dimension index `d' is negative, cycle in the opposite direction:
 *
 *  -32 < d < -1
 *  ------------
 *  in[i0][i1][i2]...[i_{|d|-1}] ---> out[i1][i2]...[i_{|d|-1}][i0]
 *
 *  (len[0],len[1],...,len[|d|-1]) ---> (len[1],len[2],...,len[|d|-1],len[0])
 *
 * In either case, applying `xpid()' a total of `|d|' times is the identity
 * operation.  The array `len[]' is cyclically permuted in place, after the
 * `data[]' array is transposed, so that the indexing algorithm is correct
 * for the new arrangement.  
 *
 * The simpler function `xpi2()' should be used for 2-dimensional arrays,
 * since it is faster.
 *
 *  Calling sequence and basic algorithm:
 *
 *   xpid( DATA, LEN, DIM, SIZE )
 *	Let D = absval( DIM )
 *      Let VOLUME = LEN[0]
 *      For K = 1 to D-1
 *         VOLUME *= LEN[K]
 *	If DIM>0 then
 *	  Let MULT = LEN[D-1]
 *      Else
 *	  Let MULT = LEN[1]
 *      For N = 1 to VOLUME-2
 *         If DATA[N] has not been permuted then
 *            Let TEMP = DATA[N]
 *            Let TARGET = N
 *            Let SOURCE = ( N*MULT ) % (VOLUME-1)
 *            While SOURCE > N
 *               Let DATA[TARGET] = DATA[SOURCE]
 *               Let TARGET = SOURCE
 *               Let SOURCE = ( SOURCE*MULT ) % (VOLUME-1)
 *            Let DATA[TARGET] = TEMP
 *      cyclei( LEN, DIM, sizeof(INT))
 *
 *  Inputs:
 *	(void *)data	This is both the input and output; it must contain
 * 			  at least `len[0]*...*len[d-1]' elements.
 *
 *	(int *)len	This is an array containing the dimensions of 
 *			  `data[]', with the most rapidly changing
 *			  dimension last.
 *
 *	(int)dim	This is dimensionality of `data[]', or the number 
 *			  of elements in `len[]'.  If negative, use its
 *			  absolute value and cyclically permute in the
 *			  opposite direction.
 *
 *	(size_t)size	This is the number of bytes in each `data[]' element
 *
 * Outputs:
 *	(void *)data	This array is transposed in place by reference.
 *
 *	(int *)len	This array is cyclically permuted in place
 *			  by reference.
 *
 * External functions called:
 *	malloc(), free(), memcpy(), cyclei()
 *
 * Assumptions:
 *	1. 1 < |dim| < 32
 *	2. data != NULL
 *	3. len != NULL
 *	4. size>0
 */
extern void
  xpid(
       void  *data,	/* 1-D array, to be transposed in place as d-D   */
       int   *len,	/* Dimensions of `data[]'; last changest fastest */
       int    dim,	/* Dimensionality of `data[]'; invert if negative. */
       size_t size)	/* Number of bytes in each `data[]' member.      */
{
  void *holder, *source, *target;
  int  volume, d, k, n, fn, ifn, mod, earlier, mult, imult;

#define AWA_XPID_SOURCE(n)	( ((n)*mult) % mod )
#define AWA_XPID_TARGET(n)	( ((n)*imult) % mod )

  d = absval(dim);

  assert( d>1 );
  assert( d<32 );
  assert(data);
  assert(len);
  assert(size>0);

  /* Allocate space for exchanges: */
  holder = malloc(size); assert(holder);

  /* Compute the total number of elements in the array. This is the
   * product of the lengths along the axes `0,1,2,...,d-1':
   */
  volume = len[0];
  for(k=1; k<d; k++) volume *= len[k];

  /* Choose the multipliers used to permute, based on the sign of `dim': */
  mult = dim>0 ? len[d-1] : volume/len[0];
  imult = volume/mult;
  mod = volume - 1;

  /* TRANSPOSE:
   * Use the offset `n' into the array `data[]' as the permuted index:
   */
  for( n=1; n<mod; n++ )
    {
      /* Test whether `n' is fixed by the transposition: */
      fn  = AWA_XPID_SOURCE(n);
      if(fn == n) continue;
      
      /* Search the cycle <n> for an earlier offset: */
      ifn = AWA_XPID_TARGET(n);
      earlier = FALSE;
      while( fn != n )
	{
	  if( fn<n || ifn<n )
	    {
	      earlier = TRUE;
	      break;
	    }
	  fn  = AWA_XPID_SOURCE(fn);
	  ifn = AWA_XPID_TARGET(ifn);
	}

      /* Transpose if `n' is the first offset of a nontrivial cycle: */
      if( !earlier )
	{
	  /* Permute this new non-singleton cycle with least index `n': */
	  fn = AWA_XPID_SOURCE(n);
	  target = data + n*size;
	  memcpy( holder, target, size );
	  while( fn > n )
	    {
	      source = data + fn*size;
	      memcpy( target, source, size );
	      target = source;
	      fn = AWA_XPID_SOURCE(fn);
	    }
	  memcpy( target, holder, size );
	}
    }

  /* CYCLE:
   * (len[0],...,len[d-1]) --> (len[d-1],len[0],len[1],...,len[d-2]) if `dim>0',
   * (len[0],...,len[d-1]) --> (len[1],len[2],...,len[d-1],len[0]) if `dim<0':
   */
  cyclei( len, dim, sizeof(int) );

  free(holder);
  return;
}
