/*
 *  These functions calculate the values for quadrature mirror filter
 *  coefficient arrays.
 *
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 * 
 */

#include <assert.h>
#include <stdlib.h>
#include "real.h"
#include "common.h"
#include "qf.h"		/* Data structs and function prototypes */
#include "oqfs.h"	/* Orthogonal filter coefficients.   */

/********************************************************************
 * mkpqf()
 *
 * Function `mkpqf()' returns a pointer to a `pqf' data structure
 *  containing the coefficients, length and the pre-periodized filter
 *  named by the input parameters.
 *
 * Calling sequence:
 *       mkpqf( coefs, alpha, omega, flags )
 * 
 * Inputs:
 *	(const real *)coefs     These are the filter coefficients,
 *				  assumed to be given in the array
 *				  `coefs[0],...,coefs[omega-alpha]'
 *				  are the only nonzero values.
 *
 *      (int)alpha      These are to be the first and last valid
 *      (int)omega        indices of the pqf->f struct member.
 *
 *	(int)flags	This is reserved for later uses, such as 
 *			  indicating when to generate a full QF
 *			  sequence from just one symmetric half.
 *
 * Return value:
 *	(pqf *)mkpqf  The return value is a pointer to a newly 
 *			  allocated pqf struct containing `coefs[]',
 *			  `alpha', `omega', and the preperiodized
 *			  version of  `coefs[]'.
 * Assumptions:
 *     (1) Conventional indexing: `alpha <= 0 <= omega'.
 */
extern pqf *
  mkpqf(
	const real *coefs,	/* Original filter coefficients. */
	int alpha,		/* Least valid index of `?->f[]'.    */
	int omega,		/* Greatest valid index of `?->f[]'. */
	int flags)		/* Reserved for future use. */
{
  pqf *qm;
  int M;

  assert(alpha <= 0);		/* Conventional indexing. */
  assert(0 <= omega);		/* Conventional indexing. */

  qm = (pqf *)calloc(1,sizeof(pqf)); assert(qm);
  qm->alpha = alpha;
  qm->omega = omega;
  qm->f = coefs-alpha;
  M = IFH( omega-alpha );
  qm->fp = (real *)calloc( PQFL(M), sizeof(real)); assert(qm->fp);
  qfcirc( qm->fp, qm->f, alpha, omega );

  qm->center = coe( qm->f, alpha, omega );
  qm->deviation = lphdev( qm->f, alpha, omega );
  return(qm);
}

/********************************************************************
 * qf()
 *
 * Function `qf()' returns a pointer to a `pqf' data structure
 *  containing the coefficients, name, length and kind of the filter
 *  named by the input parameters.
 *
 * Calling sequence:
 *       qf( name, range, kind )
 * 
 * Inputs:
 *	(const char *)name     This is the name of the filter, specified as
 *				 at least the first letter of "Beylkin",
 *				 "Coifman", "Vaidyanathan", or "Daubechies",
 *  				 written as a string (enclosed in " marks).
 *  				 Case is ignored, only the first letter 
 *				 matters, and "Standard" is an alias for "D".
 *
 *      (int)range      This is the length of the requested filter.  Allowed
 *				values depend on what `name' is.
 *
 *      (int)kind	If kind==LOW_PASS_QF, qf() returns the summing or
 *			  low-pass filter `pqf' structure.
 *			If kind==HIGH_PASS_QF, qf() returns the differencing
 *			  or high-pass filter `pqf' structure.
 *
 * Return value:
 *	(pqf *)qf    If a `name'd filter of the requested `range' and
 *			  `kind' is listed, then the return value is
 *			  a pointer to a newly-allocated pqf struct
 *			  specifying a  conventionally-indexed filter
 *			  to be used for convolution-decimation.
 * 			  Otherwise, the returned pointer is NULL.
 * 
 */
extern pqf *
  qf(				/* Coefficient struct or NULL pointer. */
     const char *name,		/* String "B", "C", "D", or "V". */
     int         range,		/* Length of coefficient array. */
     int         kind)		/* LOW_PASS_QF or HIGH_PASS_QF. */
{
  pqf *qm;

  /* These macros call `mkpqf()' with the right arguments: */
#define MKMKPQF(n, sd)  mkpqf( n##sd##oqf, n##sd##alpha, n##sd##omega, 0 )
#define SETSDPQF(n, k)  ( k==LOW_PASS_QF ) ? MKMKPQF(n, s) : MKMKPQF(n, d)

  qm = 0;			/* Return NULL pointer if unsuccessful. */

  /* Choose an orthogonal filter based on the first letter of
   * the filter name: `name' argument starts with  B, C, D, V
   */
  switch( name[0] ) {

  case 'b':
  case 'B':
    /* Beylkin filters.
     *
     * Here are the coefficients for Gregory BEYLKIN'S wave packets.
     */
    switch( range )
      {
      case 18: qm = SETSDPQF( b18, kind ); break;
      default: break;		/* Fall out: the length is unavailable. */
      }
    break;			/* Beylkin type. */
    
  case 'c':
  case 'C':
    /* Coiflet filters.
     *
     * Here are the coefficients for COIFLETS with respectively
     * 2, 4, 6, 8 and 10 moments vanishing for phi.  Filter Q has
     * range/3 moments vanishing.  Filter P has range/3 moments
     * vanishing with the appropriate shift.
     */
    switch( range ) 
      {
      case 6:  qm = SETSDPQF( c06, kind ); break;
      case 12: qm = SETSDPQF( c12, kind ); break;
      case 18: qm = SETSDPQF( c18, kind ); break;
      case 24: qm = SETSDPQF( c24, kind ); break;
      case 30: qm = SETSDPQF( c30, kind ); break;
      default: break;		/* Fall out: the length is unavailable. */
      }
    break;			/* Coifman type. */
    
  case 's':
  case 'S':			/* STANDARD filters, in old terminology. */
  case 'd':
  case 'D':
    /* Daubechies filters.
     *
     * Initialize quadrature mirror filters P,Q of length 'range' with 
     * smooth limits and minimal phase, as in DAUBECHIES:
     */
    switch( range )
      {
      case 2:  qm = SETSDPQF( d02, kind ); break;
      case 4:  qm = SETSDPQF( d04, kind ); break;
      case 6:  qm = SETSDPQF( d06, kind ); break;
      case 8:  qm = SETSDPQF( d08, kind ); break;
      case 10: qm = SETSDPQF( d10, kind ); break;
      case 12: qm = SETSDPQF( d12, kind ); break;
      case 14: qm = SETSDPQF( d14, kind ); break;
      case 16: qm = SETSDPQF( d16, kind ); break;
      case 18: qm = SETSDPQF( d18, kind ); break;
      case 20: qm = SETSDPQF( d20, kind ); break;
      default: break;		/* Fall out: the length is unavailable. */
      }
    break;			/* Standard or Daubechies type. */
    
  case 'v':
  case 'V':
    /* Vaidyanathan filters 
     * 
     * The following coefficients correspond to one of the filters
     * constructed by Vaidyanathan (Filter #24B from his paper
     * in IEEE Trans. ASSP Vol.36, pp 81-94 (1988). These coefficients
     * give an exact reconstruction scheme, but they don't satisfy ANY
     * moment condition (not even the normalization : the sum of the c_n
     * is close to 1, but not equal to 1). The function one obtains after
     * iterating the reconstruction procedure 9 times looks continuous,
     * but is of course not differentiable. It would be interesting to
     * see how such a filter performs. It has been optimized, for its
     * filter length, for the standard requirements that speech people
     * impose.
     */
    switch( range )
      {
      case 24:  qm = SETSDPQF( v24, kind ); break;
      default: break;		/* Fall out: the length is unavailable. */
      }
    break;			/* Vaidyanathan type. */
    
  default: break;		/* Fall out: the filter is unavailable. */
  }
  return(qm);
}

/*******************************************************************
 * coe()
 *
 * Compute the center-of-energy for a sequence.
 *
 * Basic algorithm:
 *   center =  ( Sum_k k*in[x]^2 ) / (Sum_k in[k]^2 )
 *
 *  Calling sequence:
 *	coe( in, alpha, omega )
 *
 *  Inputs:
 *	(const real *)in	The sequence is `in[alpha],...,in[omega]'
 *
 *      (int)alpha      	These are to be the first and last
 *      (int)omega		  valid indices of the array `in[]'.
 *
 *  Output:
 *	(real)coe		This is in the interval `[alpha, omega]'.
 *
 *  Assumptions:
 *     (1) Conventional indexing: `alpha <= 0 <= omega'.
 */
extern real
  coe(				/* Center of energy. */
      const real *in,		/* Sequence of coefficients. */
      int alpha,		/* First valid `in[]' index.   */
      int omega)		/* Last valid `in[]' index.    */
{
  real center, energy;
  int k;

  assert(alpha <= 0);		/* Conventional indexing. */
  assert(0 <= omega);		/* Conventional indexing. */

  center = 0;  energy = 0;
  for( k=alpha; k<=omega; k++)
    {
      center += k*in[k]*in[k];
      energy += in[k]*in[k];
    }
  if( energy>0 ) center /= energy;
  return(center);
}

/*******************************************************************
 * lphdev()
 *
 * Compute the maximum deviation from linear phase of the
 * convolution-decimation operator associated to a sequence.
 * Basic algorithm:
 *                    Sum_{x>0} (-1)^x Sum_k k*in[k-x]*in[k+x]
 *   deviation =  2 * ----------------------------------------
 *                               Sum_k in[k]^2
 *
 *  Calling sequence:
 *	lphdev( in, alpha, omega )
 *
 *  Inputs:
 *	(const real *)in  The sequence is `in[alpha],...,in[omega]'
 *
 *      (int)alpha      These are to be the first and last valid
 *      (int)omega        indices of the `pqf->f[]' struct member.
 *
 *  Output:
 *	(real)lphdev		This is the absolute value of the maximum.
 *
 * Assumptions:
 *     (1) Conventional indexing: `alpha <= 0 <= omega'.
 */
extern real
  lphdev(			/* Center of energy. */
	 const real *in,	/* Sequence of coefficients. */
	 int alpha,		/* First valid `in[]' index.   */
	 int omega)		/* Last valid `in[]' index.    */
{
  real energy, fx, deviation;
  int k, x, sgn;

  assert(alpha <= 0);		/* Conventional indexing. */
  assert(0 <= omega);		/* Conventional indexing. */

  /* First compute the sum of the squares of the sequence elements: */
  energy = 0;
  for( k=alpha; k<omega; k++)    energy += in[k]*in[k];

  /* If the sum of the squares in nonzero, compute the deviation: */
  deviation = 0;
  if( energy>0 )
    {
      sgn= -1;
      for( x=1;  x <= (omega-alpha)/2;  x++ )
	{
	  fx = 0;
	  for( k=x+alpha; k<=omega-x; k++ )
	    {
	      fx += k*in[k-x]*in[k+x];
	    }
	  deviation += sgn*fx;
	  sgn = -sgn;
	}
      deviation = absval(deviation);
      deviation /= energy;	/* Normalize. */
      deviation *= 2;		/* Final factor from the formula. */
    }
  /* If `energy==0' then `deviation' is trivially 0 */

  return(deviation);
}

/***************************************************************
 * periodize()
 *
 * Periodize an array into a shorter-length array.
 *
 * Calling sequence and basic algorithm:
 *
 * periodize(FQ, Q, F, ALPHA, OMEGA)
 *   For K = 0 to Q-1
 *      Let FQ[K] = 0
 *      Let J = (ALPHA-K)/Q
 *      While K+J*Q <= OMEGA
 *         FQ[K] += F[K+J*Q]
 *         J += 1
 *
 * Inputs:
 *   (real *)fq       Preallocated array of length `q'.
 *   (int)q           This is the period of `fq[]'.
 *   (const real *)f  This array holds the original function. 
 *   (int)alpha       These are to be the first and last valid
 *   (int)omega          indices of the array `f[]'.
 *
 * Output:
 *   (real *)fq       The array `fq[]' is assigned as follows:
 *                      fq[k] = f[k+j0*q]+...+f[k+j1*q],
 *				k = 0, 1, ..., q-1;
 *				j0 = ceil[(alpha-k)/q];
 *				j1 = floor[(omega-k)/q].
 *
 * Assumptions:
 *	(1) `omega-alpha >= q > 0',
 *	(2) `alpha <= 0 <= omega'
 */
static void
  periodize(
	    real *fq,        /* Preallocated output array. */
	    int q,           /* Length of `fq[]'.          */
	    const real *f,   /* Input array.               */
	    int alpha,       /* First valid `f[]' index.   */
	    int omega)       /* Last valid `f[]' index.    */
{
  int j, k;

  assert(q>0);			/* Nontrivial period.     */
  assert(q<=omega-alpha);	/* Periodization needed.  */
  assert(alpha <= 0);		/* Conventional indexing. */
  assert(0 <= omega);		/* Conventional indexing. */

  for(k=0; k<q; k++)
    {
      fq[k] = 0;
      j = (alpha-k)/q;		/* `j==ceil((alpha-k)/q)' */
      while( (k+j*q) <= omega )
	{
	  fq[k] += f[k+j*q];
	  j++;
	}
    }
  return;
}

/***************************************************************
 *  qfcirc()
 *
 * This function computes the periodized filter coefficients
 * associated to the input array of quadrature mirror filter
 * coefficients, for lengths 2, 4, 6,...  It then fills an array
 * with these periodized coefficients, duplicating some of them
 * to facilitate circular convolution.  If the input array is
 *          f[alpha], f[alpha+1], ..., f[omega],
 * and M is the largest integer satisfying `2M<=omega-alpha',
 * then the output array is the concatenation of:
 *
 * Start     Contents                                         End
 * -----  -------------------------------------------------- -----
 *   0            f2[0],f2[1],f2[0],f2[1];                     3
 *   4    f4[0],f4[1],f4[2],f4[3],f4[0],f4[1],f4[2],f4[3];     11
 *  12    f6[0],f6[1],f6[2],...,f6[5],f6[0],f6[1],...,f6[5];   23
 *  24    f8[0],f8[1],f8[2],...,f8[7],f8[0],f8[1],...,f8[7];   39
 *   .          ...         ...                   ...      ;    .
 * S(m-1) f{2m}[0],...,f{2m}[2m-1],f{2m}[0],...,f{2m}[2m-1]; S(m)-1
 *   .          ...         ...             ...            ;    .
 * S(M-1) f{2M}[0],...,f{2M}[2M-1],f{2M}[0],...,f{2M}[2M-1]; S(M)-1
 *
 * where S(m)= 4+8+16+...+ 4(m-1) = 2m(m-1) gives the
 * starting index of the m-th segment.  
 *
 * The "midpoint" or "origin" of the m-th periodic subarray is: 
 *		PQFO(m) = S(m-1)+2m = 2m*m.
 *
 * The total length of concatenated periodic subarrays `1,...,M' is
 *		PQFL(M) = S(M-1)+4M = 2M*(M+1).
 *
 *  Calling sequence and basic algorithm:
 *
 * qfcirc(FP, F, ALPHA, OMEGA)
 *   Let M = IFH(omega-alpha)
 *   For N = 1 to M
 *      Let Q = 2*N
 *      Let FQ = FP + PQFO(N)
 *      periodize( FQ, Q, F, ALPHA, OMEGA )
 *      For K = 0 to Q-1
 *         Let FQ[K-Q] = FQ[K]
 *
 * Inputs:
 *      (real *)fp	This must point to a preallocated array
 *			  with at least `PQFL(M)' memory 
 *			  locations.  This function call fills
 *			  those locations, beginning with 0.
 *
 *	(const real *)f	This is an array of quadrature mirror
 *			  filter coefficients.
 *
 *      (int)alpha      These are to be the first and last valid
 *      (int)omega        indices of the array `f[]'.
 *
 * Output:
 *      (real *)fp	This array is filled with periodized,
 *			  partially duplicated arrays with 
 *			  lengths 4, 8, ..., 4M.
 *
 * Assumptions:
 *     (1) Conventional indexing: `alpha <= 0 <= omega'.
 */
extern void
  qfcirc(
	 real *fp,	   /* Preallocated output array  */
	 const real *f,	   /* Filter coefficients        */
	 int alpha,	   /* First valid `f[]' index    */
	 int omega)	   /* Last valid `f[]' index     */
{
  int k, m, M, q;
  real *fq;

  assert(alpha <= 0);		/* Conventional indexing */
  assert(0 <= omega);		/* Conventional indexing */

  M = IFH(omega-alpha);		/* Max with `2*M <= omega-alpha' */

  for( m=1; m<=M; m++ )
    {
      q  = 2*m;
      fq = fp+PQFO(m);
      periodize( fq, q, f, alpha, omega);
      for( k=0; k<q; k++ )
	fq[k-q] = fq[k];
    }
  return;
}


