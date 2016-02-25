/*
 * 
 * Transposition functions, in place and with copying.
 * 
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 * 
 */

#ifndef XP_HDR_ALREADY_INCLUDED
# define XP_HDR_ALREADY_INCLUDED

#include <stdlib.h>
#include "real.h"
extern void
  xpi2(
       void *data,		/* Array to be transposed in place.     */
       int   x,			/* Number of rows in `data[]'.          */
       int   y,			/* Number of columns in `data[]'.       */
       size_t size);		/* Number of bytes per `data[]' member. */

extern void
  xpd2(
       void *out,		/* Output: disjoint transposed array   */
       const void *in,		/* Input array, to be transposed       */
       int   x,			/* Number of rows in `in[]'            */
       int   y,			/* Number of columms in `in[]'         */
       size_t size);		/* Number of bytes per `data[]' member */

extern void
  cycled(			/* Cyclic permutation to disjoint arrays. */
	 void  *out,		/* Array to receive the cyclic permutation. */
	 const void  *in,	/* Input array. */
	 int    d,		/* Length of `in[],out[]'; invert if `d<0'. */
	 size_t size);		/* Bytes per `in[]' or `out[]' member.   */

extern void
  cyclei(			/* `item[0]-->item[1]> >item[d-1]-->item[0]' */
	 void  *item,		/* Array to be cyclically permuted in place. */
	 int    d,		/* Length of `item[]'; inverse if `d<0'. */
	 size_t size);		/* Bytes per `item[]' member.   */

extern void
  xpid(
       void  *data,	/* 1-D array, to be transposed in place as d-D   */
       int   *len,	/* Dimensions of `data[]'; last changest fastest */
       int    dim,	/* Dimensionality of `data[]'; invert if negative. */
       size_t size);	/* Number of bytes in each `data[]' member.      */

#endif /* XP_HDR_ALREADY_INCLUDED */
