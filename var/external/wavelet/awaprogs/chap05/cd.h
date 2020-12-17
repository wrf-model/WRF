/*
 * Declare convolution-decimation functions.
 * 
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 * 
 */

#ifndef CD_HDR_ALREADY_INCLUDED
# define CD_HDR_ALREADY_INCLUDED

#include "real.h"
#include "qf.h"

/* Calculate interval endpoints after aperiodic convolution-decimation:  */
#define cdaleast( I, F )	ICH( (I)->least + (F)->alpha )
#define cdafinal( I, F )	IFH( (I)->final + (F)->omega )

/* Interval endpoints after adjoint aperiodic convolution-decimation:  */
#define acdaleast( I, F )	( 2*(I)->least - (F)->omega )
#define acdafinal( I, F )	( 2*(I)->final - (F)->alpha )

#define cdpo cdpo2   /* Arbitrary default; cdpo1 is OK too. */
#define cdpe cdpe2   /* Arbitrary default; cdpe1 is OK too. */

extern void
  cdai(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F ); /* QF data structure             */

extern void
  cdao(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F ); /* QF data structure             */

extern void
  cdae(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F ); /* QF data structure             */

extern void
  acdai(
	real *out,      /* Preallocated output array      */
	int  step,      /* Increment for `out[]' values   */
	const real *in, /* Preallocated input array       */
	int c,          /* First valid `in[]' offset      */
	int d,          /* Last valid `in[]' offset       */
	const pqf *F ); /* QF data structure              */

extern void
  acdao(
	real *out,      /* Preallocated output array       */
	int  step,      /* Increment for `out[]' values    */
	const real *in, /* Preallocated input array        */
	int c,          /* First valid `in[]' offset       */
	int d,          /* Last valid `in[]' offset        */
	const pqf *F ); /* QF data structure               */

extern void
  acdae(
	real *out,      /* Preallocated output array       */
	int  step,      /* Increment for `out[]' values    */
	const real *in, /* Preallocated input array        */
	int c,          /* First valid `in[]' offset       */
	int d,          /* Last valid `in[]' offset        */
	const pqf *F ); /* QF data structure               */

extern void
  cdmo(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F ); /* Periodized QF data structure      */

extern void
  cdmi(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F ); /* Periodized QF data structure      */

extern void
  cdpi(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F ); /* Periodized QF data structure      */

extern void
  cdpo1(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F ); /* Periodized QF data structure      */

extern void
  cdpo2(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F ); /* Periodized QF data structure      */

extern void
  cdpe1(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F ); /* Periodized QF data structure      */

extern void
  cdpe2(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F ); /* Periodized QF data structure      */

extern void
  acdpi(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int q2,         /* Number of elements of `in[]'      */
	const pqf *F ); /* Periodized QF data structure      */

extern void
  acdpo(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int   q2,       /* Number of elements of `in[]'      */
	const pqf *F ); /* Periodized qf data structure      */

extern void
  acdpe(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int   q2,       /* Number of elements of `in[]'      */
	const pqf *F ); /* Periodized QF data structure      */

#endif    /* CD_HDR_ALREADY_INCLUDED  */
