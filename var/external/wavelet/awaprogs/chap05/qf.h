/*
 * Declare some utilities needed to create quadrature filters.
 * 
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 * 
 */

#ifndef QF_HDR_ALREADY_INCLUDED
#define QF_HDR_ALREADY_INCLUDED

#include "real.h"

#define IFH(x)               ((x)&1?((x)-1)/2:(x)/2)
#define ICH(x)               ((x)&1?((x)+1)/2:(x)/2)

#define		LOW_PASS_QF		(0x0)
#define		HIGH_PASS_QF		(0x1)

/* This macro gives the starting offset of the `2m'-preperiodized
 * filter sequence in the `->fp' element of a `pqf' struct:
 */
#define PQFL(m)             (2*(m)*(m+1))
#define PQFO(m)             (2*(m)*(m))


/* DATA STRUCTURES: these are passed to convolution/decimation routines. */
typedef struct {
  int   alpha;          /* Least valid `f[]' index.        */
  int   omega;          /* Greatest valid `f[]' index.     */
  real  center;		/* Center of energy of the filter. */
  real  deviation;	/* Deviation from linear phase.    */
  const real *f;        /* Filter coefficient array.       */
  real *fp;             /* Periodized coefficient array.   */
}
pqf;         /* Periodized conjugate quadrature filter. */

/* UTILITIES: */
void
qfcirc(
	real *fp,         /* Preallocated output array. */
	const real *f,    /* Filter coefficients.       */
	int alpha,	  /* First valid `in[]' index. */
	int omega);	  /* Last valid `in[]' index.  */

pqf *
mkpqf(
       const real *coefs,	/* Original filter: `coefs[0]...'.   */
       int alpha,		/* Least valid index of `?->f[]'.    */
       int omega,		/* Greatest valid index of `?->f[]'. */
       int flags);		/* Reserved for future use. */

pqf *
qf(				/* Coefficient struct or NULL pointer. */
    const char *name,		/* String "B", "C", "D", or "V". */
    int         range,		/* Length of coefficient array. */
    int         kind);		/* LOW_PASS_QF or HIGH_PASS_QF. */

real
coe(				/* Center of energy. */
    const real *in,		/* Sequence of coefficients. */
    int alpha,			/* First valid `in[]' index. */
    int omega);			/* Last valid `in[]' index.  */

real
lphdev(				/* Center of energy. */
       const real *in,		/* Sequence of coefficients. */
       int alpha,		/* First valid `in[]' index. */
       int omega);		/* Last valid `in[]' index.  */

#endif /* QF_HDR_ALREADY_INCLUDED */

