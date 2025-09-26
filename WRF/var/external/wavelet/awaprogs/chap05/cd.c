/*
 * Convolution-decimation functions and their adjoints.
 *
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 * 
 */

#include <assert.h>
#include "real.h"
#include "cd.h"
#include "common.h"		/* for `min()' and `max()'. */

/***************************************************************
 * cdai(): 
 *
 * [C]onvolution and [D]ecimation by 2;
 *   [A]periodic, sequential [I]nput, superposition. 
 *
 *  Basic algorithm:
 *
 *   For J = A to B
 *    Let BEGIN = ICH(J+F.ALPHA)
 *    Let END   = IFH(J+F.OMEGA)
 *    For I = BEGIN to END
 *       OUT[I*STEP] += F.F[2*I-J]*IN[J]
 *
 *  Calling sequence:
 *     cdai(out, step, in, a, b, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*ICH(a+F->alpha)]' through
 *                    `out[step*IFH(b+F->omega)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[a],...,in[b]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + step*ICH(a + F->alpha)'.
 */
extern void
  cdai(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F )  /* QF data structure             */
{
  int i, j;
  real *outp;
  const real *filt;
  
  for( j=a; j<=b; j++ )
    {
      i=ICH(j+F->alpha);
      filt = F->f +(2*i-j);
      outp = out + i*step;
      while( i<= IFH(j+F->omega) )
	{
	  *outp += (*filt) * in[j];
	  outp += step;   filt += 2;   ++i;
	}
    }
  return;
}

/**************************************************************
 * cdao(): 
 *
 * [C]onvolution and [D]ecimation by 2;
 *   [A]periodic, sequential [O]utput, superposition. 
 *
 *  Basic algorithm:
 *
 *   Let APRIME = ICH(A+F.ALPHA)
 *   Let BPRIME = IFH(B+F.OMEGA)
 *   For I = APRIME to BPRIME
 *      Let BEGIN = max( F.ALPHA, 2*I-B )
 *      Let END   = min( F.OMEGA, 2*I-A )
 *      For J = BEGIN to END
 *         OUT[I*STEP] += F.F[J]*IN[2*I-J]
 *
 *  Calling sequence:
 *     cdao(out, step, in, a, b, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*ICH(a+F->alpha)]' through
 *                    `out[step*IFH(b+F->omega)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[a],...,in[b]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + step*ICH(a+F->alpha)'.
 */
extern void
  cdao(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F )  /* QF data structure             */
{
  int  i, j, end;
  const real *inp;
  
  i = ICH(a+F->alpha);
  out += i*step;
  while( i <= IFH(b+F->omega) )
    {
      j = 2*i-b;
      if(j<F->alpha) 
	{
	  j=F->alpha;		/* j = max(F->alpha,2*i-b) */
	  inp = in + 2*i-j;	/* *inp = in[2*i-j] */
	}
      else
	{
	  inp = in + b;		/* *inp = in[2*i-j] */
	}
      end = 2*i-a;
      end =  min( F->omega, end );
      while( j <= end  )
	*out += F->f[j++]*(*inp--);
      ++i;
      out += step;
    }
  return;
}

/**************************************************************
 * cdae(): 
 *
 * [C]onvolution and [D]ecimation by 2;
 *   [A]periodic, set output using [E]quals. 
 *
 *  Basic algorithm:
 *
 *   Let APRIME = ICH(A+F.ALPHA)
 *   Let BPRIME = IFH(B+F.OMEGA)
 *   For I = APRIME to BPRIME
 *      Let OUT[I*STEP] = 0
 *      Let BEGIN = max( F.ALPHA, 2*I-B )
 *      Let END   = min( F.OMEGA, 2*I-A )
 *      For J = BEGIN to END
 *         OUT[I*STEP] += F.F[J]*IN[2*I-J]
 *
 *  Calling sequence:
 *     cdae(out, step, in, a, b, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*ICH(a+F->alpha)]' through
 *                    `out[step*IFH(b+F->omega)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[a],...,in[b]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are assigned into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + step*ICH(a+F->alpha)'.
 */
extern void
  cdae(
       real *out,      /* Preallocated output array     */
       int  step,      /* Increment for `out[]'         */
       const real *in, /* Preallocated input array      */
       int a,          /* First valid `in[]' offset     */
       int b,          /* Last valid `in[]' offset      */
       const pqf *F )  /* QF data structure             */
{
  int  i, j, end;
  const real *inp;
  
  i = ICH(a+F->alpha);
  out += i*step;
  while( i <= IFH(b+F->omega) )
    {
      *out = 0;
      j = 2*i-b;
      if(j<F->alpha) 
	{
	  j=F->alpha;		/* j = max(F->alpha,2*i-b) */
	  inp = in + 2*i-j;	/* *inp = in[2*i-j] */
	}
      else
	{
	  inp = in + b;		/* *inp = in[2*i-j] */
	}
      end = 2*i-a;
      end =  min( F->omega, end );
      while( j <= end  )
	*out += F->f[j++]*(*inp--);
      ++i;
      out += step;
    }
  return;
}

/*************************************************************
 * acdai():
 * 
 *     [A]djoint [C]onvolution and [D]ecimation:
 *      [A]periodic, sequential [I]nput, superposition.
 * 
 *  Basic algorithm:
 *
 *   For I = C to D
 *      Let BEGIN = 2*I-F.OMEGA
 *      Let END   = 2*I-F.ALPHA
 *      For J = BEGIN to END
 *         OUT[J*STEP] += F.F[2*I-J]*IN[I]
 *
 * We change variables to save operations:
 *
 *   For I = C to D
 *      For J = F.ALPHA to F.OMEGA
 *         OUT[(2*I-J)*STEP] += F.F[J]*IN[I]
 *
 *  Calling sequence:
 *     acdai(out, step, in, c, d, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*(2*c-F->omega)]' through 
 *                    `out[step*(2*d-F->alpha)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[c],...,in[d]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + (2*c-F->omega)*step'.
 */
extern void
  acdai(
	real *out,      /* Preallocated output array      */
	int  step,      /* Increment for `out[]' values   */
	const real *in, /* Preallocated input array       */
	int c,          /* First valid `in[]' offset      */
	int d,          /* Last valid `in[]' offset       */
	const pqf *F )  /* QF data structure              */
{
  int i, j;
  real *outp;
  
  for( i=c; i <= d ; i++ )
    {
      j = F->alpha;
      outp = out + (2*i-j)*step;
      while( j <= F->omega )
	{
	  *outp +=  F->f[j++] * in[i];
	  outp -= step;
	}
    }
  return;
}

/***************************************************************
 * acdao():
 * 
 *     [A]djoint [C]onvolution and [D]ecimation:
 *      [A]periodic, sequential [O]utput, superposition.
 * 
 *  Basic algorithm:
 *
 *   Let CPRIME = 2*C-F.OMEGA
 *   Let DPRIME = 2*D-F.ALPHA
 *   For J = CPRIME to DPRIME
 *      Let BEGIN = max( ICH(J+F.ALPHA ), C )
 *      Let END   = min( IFH(J+F.OMEGA ),  D )
 *      For I = BEGIN to END
 *         OUT[J*STEP] += F.F[2*I-J]*IN[I]
 *
 *  Calling sequence:
 *     acdao(out, step, in, c, d, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*(2*c-F->omega)]' through 
 *                    `out[step*(2*d-F->alpha)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[c],...,in[d]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + (2*c - F->omega)*step'.
 */
extern void
  acdao(
	real *out,      /* Preallocated output array       */
	int  step,      /* Increment for `out[]' values    */
	const real *in, /* Preallocated input array        */
	int c,          /* First valid `in[]' offset       */
	int d,          /* Last valid `in[]' offset        */
	const pqf *F )  /* QF data structure               */
{
  int  end, i, j;
  const real *filt;
  
  j = 2*c - F->omega;
  out += j*step;
  
  while( j <= 2*d-F->alpha )
    {
      i = ICH( j + F->alpha );
      i = max( i, c ); 
      end   = IFH( j + F->omega );
      end   = min( end, d );
      filt = F->f+2*i-j;
      while( i<=end )
	{
	  *out += (*filt)*in[i++];
	  filt += 2;
	}
      out += step;
      ++j;
    }
  return;
}

/***************************************************************
 * acdae():
 * 
 *     [A]djoint [C]onvolution and [D]ecimation:
 *      [A]periodic, set output using [E]quals.
 * 
 *  Basic algorithm:
 *
 *   Let CPRIME = 2*C-F.OMEGA
 *   Let DPRIME = 2*D-F.ALPHA
 *   For J = CPRIME to DPRIME
 *      Let OUT[J*STEP] = 0
 *      Let BEGIN = max( ICH(J+F.ALPHA ), C )
 *      Let END   = min( IFH(J+F.OMEGA ),  D )
 *      For I = BEGIN to END
 *         OUT[J*STEP] += F.F[2*I-J]*IN[I]
 *
 *  Calling sequence:
 *     acdae(out, step, in, c, d, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that `out[step*(2*c-F->omega)]' through 
 *                    `out[step*(2*d-F->alpha)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[c],...,in[d]' are all defined. 
 *    (int)a,b      These are the first and last valid indices
 *                    for array `in[]'.
 *    (const pqf *)F     This specifies the QF to use.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at
 *                    `out + (2*c - F->omega)*step'.
 */
extern void
  acdae(
	real *out,      /* Preallocated output array       */
	int  step,      /* Increment for `out[]' values    */
	const real *in, /* Preallocated input array        */
	int c,          /* First valid `in[]' offset       */
	int d,          /* Last valid `in[]' offset        */
	const pqf *F )  /* QF data structure               */
{
  int  end, i, j;
  const real *filt;
  
  j = 2*c - F->omega;
  out += j*step;
  
  while( j <= 2*d-F->alpha )
    {
      *out = 0;
      i = ICH( j + F->alpha );
      i = max( i, c ); 
      end   = IFH( j + F->omega );
      end   = min( end, d );
      filt = F->f+2*i-j;
      while( i<=end )
	{
	  *out += (*filt)*in[i++];
	  filt += 2;
	}
      out += step;
      ++j;
    }
  return;
}

/*******************************************************
 * cdmi():
 *
 *   [C]onvolution and [D]ecimation by 2, using the
 *      [M]od operator, sequential [I]nput, superposition. 
 *
 *  Basic algorithm:
 *
 *   Let Q2  = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For J = 0 to Q-1
 *         Let JA2 = ICH(J+F.ALPHA)
 *         Let JO2 = IFH(J+F.OMEGA)
 *         For I = 0 to JO2-Q2
 *            OUT[I*STEP] += FILTER[Q+2*I-J]*IN[J]
 *         For I = max(0,JA2) to min(Q2-1,JO2)
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *         For I = JA2+Q2 to Q2-1
 *            OUT[I*STEP] += FILTER[2*I-J-Q]*IN[J]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For J = 0 to Q-1
 *         For I = 0 to Q2-1
 *            OUT[I*STEP] += FILTER[(Q+2*I-J)%Q]*IN[J]
 * 
 *  Calling sequence:
 *     cdmi(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F    This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdmi(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F )  /* Periodized QF data structure      */
{
  int i, j, q2;
  const real *filt;
  real *outp;

  assert( (q&1)==0 );		/* Assume `q' is even.  */
  
  q2 = q/2;                     /* Number of outputs.   */
  if( q > F->omega-F->alpha )	/* Long input case.     */
    {
      int  ja2, jo2;

      for( j=0; j<q; j++ )
	{
	  ja2 = ICH(j+F->alpha);
	  jo2 = IFH(j+F->omega);

	  i=0;    outp = out;    filt = F->f + q-j;
	  while( i <= jo2-q2 )
	    {
	      *outp += (*filt) * (*in);
	      ++i;  outp += step; filt += 2;
	    }

	  i = max(0, ja2);   outp = out+i*step;  filt = F->f + 2*i-j;
	  while( i <= min(q2-1,jo2) )
	    {
	      *outp += (*filt) * (*in);
	      ++i; outp += step; filt += 2;
	    }

	  i = ja2+q2;   outp = out+i*step;   filt = F->f + 2*i-j-q;
	  while( i < q2 )
	    {
	      *outp +=(*filt) * (*in);
	      ++i; outp += step; filt += 2;
	    }

	  ++in;
	}
    }
  else                         /* Short input case.     */
    {
      filt = F->fp + PQFO(q2);
      for( j=0; j<q; j++ )
	{
	  outp = out;
	  for( i=0; i<q2; i++ )
	    {
	      *outp += filt[(q+2*i-j)%q]*(*in);
	      outp += step;
	    }
	  ++in;
	}
    }
  return;
}

/************************************************************
 * cdmo():
 *
 *    [C]onvolution and [D]ecimation by 2, using the
 *       [M]od operator,  sequential [O]utput, superposition. 
 *
 *  Basic algorithm:
 *
 *   Let Q2 = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *         For J = F.ALPHA to F.OMEGA
 *            OUT[I*STEP] += FILTER[J]*IN[(Q+2*I-J)%Q]
 *   Else
 *      Let FILTER = F.FP + PQFO[Q2]
 *      For I = 0 to Q2-1
 *         For J = 0 to Q-1
 *            OUT[I*STEP] += FILTER[J]*IN[(Q+2*I-J)%Q]
 *
 *  Calling sequence:
 *     cdmo(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F    This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdmo(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F )  /* Periodized QF data structure      */
{
  int i, j, k, q2;

  assert( (q&1)==0 );	         /* Test that `q' is even.  */
  
  q2 = q/2;
  if( q > F->omega - F->alpha )    /* Long input case.    */
    {
      for( i=0; i<q2; i++)
	{
	  j = F->alpha;
	  k = q+2*i-j;
	  while( j <= F->omega)
	    *out += F->f[j++]*in[(k--)%q];
	  out += step;
	}
      }
  else                             /* Short input case.   */
    {
      for(i=0; i<q2; i++)
	{
	  j = PQFO(q2);	/* q-periodized coefs. */
	  k = q+2*i;
	  while( k > 2*i)
	    *out += F->fp[j++]*in[(k--)%q];
	  out += step;
	}
    }
  return;
}

/************************************************************
 * cdpi():
 *
 *    [C]onvolution and [D]ecimation by 2:
 *       [P]eriodic, sequential [I]nput, superposition. 
 *
 *  Basic algorithm:
 *
 *   Let Q2  = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For J = 0 to Q-1
 *         Let JA2 = ICH(J+F.ALPHA)
 *         Let JO2 = IFH(J+F.OMEGA)
 *         For I = 0 to JO2-Q2
 *            OUT[I*STEP] += FILTER[Q+2*I-J]*IN[J]
 *         For I = max(0,JA2) to min(Q2-1,JO2)
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *         For I = JA2+Q2 to Q2-1
 *            OUT[I*STEP] += FILTER[2*I-J-Q]*IN[J]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For J = 0 to Q-1
 *         For I = 0 to Q2-1
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *
 *  Calling sequence:
 *     cdpi(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F    This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdpi(
       real *out,      /* Preallocated array, length `q/2'  */
       int step,       /* Increment between `out[]' values  */
       const real *in, /* Preallocated array, length `q'    */
       int   q,        /* Number of inputs---must be even   */
       const pqf *F )  /* Periodized QF data structure      */
{
  int i, j, q2;
  const real *filt;
  real *outp;
  
  assert( (q&1)==0 );	       /* Test that `q' is even.   */
  
  q2 = q/2;                    /* Number of outputs.     */
  if( q > F->omega-F->alpha )      /* Long input case. */
    {
      int  ja2, jo2;

      for( j=0; j<q; j++ )
	{
	  ja2 = ICH(j+F->alpha);
	  jo2 = IFH(j+F->omega);

	  i=0;    outp = out;    filt = F->f + q-j;
	  while( i<=jo2-q2 )
	    {
	      *outp += (*filt) * (*in);
	      ++i;  outp += step; filt += 2;
	    }

	  i = max(0, ja2); outp = out+i*step; filt = F->f + 2*i-j;
	  while(  i<=min(q2-1,jo2) )
	    {
	      *outp += (*filt) * (*in);
	      ++i; outp += step; filt += 2;
	    }

	  i = ja2+q2; outp = out+i*step; filt = F->f + 2*i-j-q;
	  while( i<q2 )
	    {
	      *outp +=(*filt) * (*in);
	      ++i; outp += step; filt += 2;
	    }

	  ++in;
	}
    }
  else                         /* Short input case.     */
    {
      for( j=0; j<q; j++ )
	{
	  filt = F->fp + PQFO(q2) - j;
	  outp = out;
	  for( i=0; i<q2; i++ )
	    {
	      *outp += (*filt) * (*in);
	      filt += 2;   outp += step;
	    }
	  ++in;
	}
    }
  return;
}

/*********************************************************
 * cdpo1():
 *
 *     [C]onvolution and [D]ecimation by 2:
 *         [P]eriodic, sequential [O]utput, superposition (1).
 *
 *   Basic algorithm:
 *
 *   Let Q2  = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *         Let A2I = 2*I-F.ALPHA
 *         Let O2I = 2*I-F.OMEGA
 *         For J = 0 to A2I-Q
 *            OUT[I*STEP] += FILTER[2*I-J-Q]*IN[J]
 *         For J = max(0,O2I) to min(Q-1,A2I)
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *         For J = O2I+Q to Q-1
 *            OUT[I*STEP] += FILTER[Q+2*I-J]*IN[J]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For I = 0 to Q2-1
 *         For J = 0 to Q-1
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *
 *
 *  Calling sequence:
 *     cdpo1(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in   This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdpo1(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F )  /* Periodized QF data structure      */
{
  int ii, j;
  const real *filt;
  int q2;
  
  assert( (q&1)==0 );	       /* Test that `q' is even.   */

  q2 = q/2;
  if( q > F->omega-F->alpha )  /* Long input case.       */
    {
      int a2i, o2i;
      
      for( ii=0; ii<q; ii+=2 )
	{
	  a2i = ii - F->alpha;
	  o2i = ii - F->omega;

	  filt = F->f + ii-q;
	  for( j=0; j<=a2i-q; j++ )
	    *out += (*filt--)* in[j];

          j = max(0, o2i);  filt = F->f + ii-j;
	  for( ; j<=min(q-1,a2i); j++ )
	    *out += (*filt--)* in[j];

	  j = o2i + q;	    filt = F->f + q+ii-j;
	  for( ; j<q; j++ )
	    *out += (*filt--) * in[j];

	  out += step;
	}
    }
  else                         /* Short input case.     */
    {
      for( ii=0; ii<q; ii+=2 )
	{
	  filt = F->fp + PQFO(q2) + ii;
	  for( j=0; j<q; j++ )
	    *out += (*filt--) * in[j];
	  out += step;
	}
    }
  return;
}

/*********************************************************
 * cdpo2():
 *
 *     [C]onvolution and [D]ecimation by 2:
 *         [P]eriodic, sequential [O]utput, superposition (2).
 *
 *   Basic algorithm:
 *
 *   Let Q2 = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *         Let END   = 2*I-Q
 *         For J = F.ALPHA to END
 *            OUT[I*STEP] += FILTER[J]*IN[2*I-J-Q]
 *         Let BEGIN = max(F.ALPHA, END+1)
 *         Let END   = min(F.OMEGA, 2*I)
 *         For J = BEGIN to END
 *            OUT[I*STEP] += FILTER[J]*IN[2*I-J]
 *         Let BEGIN = 2*I+1
 *         For J = BEGIN to F.OMEGA
 *            OUT[I*STEP] += FILTER[J]*IN[Q+2*I-J]
 *   Else
 *      Let FILTER = F.FP + PQFO(Q2)
 *      For I = 0 to Q2-1
 *         For J = 0 to Q-1
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *
 *  Calling sequence:
 *     cdpo2(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in   This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdpo2(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F )  /* Periodized QF data structure      */
{
  int ii, j;
  const real *ptr;
  int q2;
  
  assert( (q&1)==0 );	       /* Test that `q' is even.   */

  q2 = q/2;
  if( q > F->omega-F->alpha )  /* Long input case.       */
    {
      int  end;
      
      for( ii=0; ii<q; ii+=2 )
	{
	  end = ii-q;
	  ptr = in + end;
	  for( j=F->alpha; j<=end; j++ )
	    *out += F->f[j]* (*ptr--);

	  j   = max(F->alpha, j);
	  end = min(F->omega, ii);
	  ptr = in + ii - j;
	  for( ; j<=end; j++ )
	    *out += F->f[j]* (*ptr--);

	  j   = ii + 1;
	  ptr = in + q - 1;
	  for( ; j<=F->omega; j++ )
	    *out += F->f[j] * (*ptr--);

	  out += step;
	}
    }
  else                         /* Short input case.     */
    {
      for( ii=0; ii<q; ii+=2 )
	{
	  ptr = F->fp + PQFO(q2) + ii;
	  for( j=0; j<q; j++ )
	    *out += (*ptr--) * in[j];
	  out += step;
	}
    }
  return;
}

/*********************************************************
 * cdpe1():
 *
 *     [C]onvolution and [D]ecimation by 2:
 *         [P]eriodic, set output using [E]quals (1).
 *
 *   Basic algorithm:
 *
 *   Let Q2  = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *      Let OUT[I*STEP] = 0
 *         Let A2I = 2*I-F.ALPHA
 *         Let O2I = 2*I-F.OMEGA
 *         For J = 0 to A2I-Q
 *            OUT[I*STEP] += FILTER[2*I-J-Q]*IN[J]
 *         For J = max(0,O2I) to min(Q-1,A2I)
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *         For J = O2I+Q to Q-1
 *            OUT[I*STEP] += FILTER[Q+2*I-J]*IN[J]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For I = 0 to Q2-1
 *         Let OUT[I*STEP] = 0
 *         For J = 0 to Q-1
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *
 *
 *  Calling sequence:
 *     cdpe1(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in   This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are assigned into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdpe1(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F )  /* Periodized QF data structure      */
{
  int ii, j;
  const real *filt;
  int q2;
  
  assert( (q&1)==0 );	       /* Test that `q' is even.   */

  q2 = q/2;
  if( q > F->omega-F->alpha )  /* Long input case.       */
    {
      int a2i, o2i;
      
      for( ii=0; ii<q; ii+=2 )
	{
	  *out = 0;
	  a2i = ii - F->alpha;
	  o2i = ii - F->omega;

	  filt = F->f + ii-q;
	  for( j=0; j<=a2i-q; j++ )
	    *out += (*filt--)* in[j];

          j = max(0, o2i);  filt = F->f + ii-j;
	  for( ; j<=min(q-1,a2i); j++ )
	    *out += (*filt--)* in[j];

	  j = o2i + q;	    filt = F->f + q+ii-j;
	  for( ; j<q; j++ )
	    *out += (*filt--) * in[j];

	  out += step;
	}
    }
  else                         /* Short input case.     */
    {
      for( ii=0; ii<q; ii+=2 )
	{
	  *out = 0;
	  filt = F->fp + PQFO(q2) + ii;
	  for( j=0; j<q; j++ )
	    *out += (*filt--) * in[j];
	  out += step;
	}
    }
  return;
}

/*********************************************************
 * cdpe2():
 *
 *     [C]onvolution and [D]ecimation by 2:
 *         [P]eriodic, output set by [E]qual sign (2).
 *
 *   Basic algorithm:
 *
 *   Let Q2 = Q/2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *         Let OUT[I*STEP] = 0
 *         Let END   = 2*I-Q
 *         For J = F.ALPHA to END
 *            OUT[I*STEP] += FILTER[J]*IN[2*I-J-Q]
 *         Let BEGIN = max(F.ALPHA, END+1)
 *         Let END   = min(F.OMEGA, 2*I)
 *         For J = BEGIN to END
 *            OUT[I*STEP] += FILTER[J]*IN[2*I-J]
 *         Let BEGIN = 2*I+1
 *         For J = BEGIN to F.OMEGA
 *            OUT[I*STEP] += FILTER[J]*IN[Q+2*I-J]
 *   Else
 *      Let FILTER = F.FP + PQFO(Q2)
 *      For I = 0 to Q2-1
 *         Let OUT[I*STEP] = 0
 *         For J = 0 to Q-1
 *            OUT[I*STEP] += FILTER[2*I-J]*IN[J]
 *
 *  Calling sequence:
 *     cdpe2(out, step, in, q, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q/2-1)]' are defined. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in   This input array must be preallocated
 *                    so that `in[0],...,in[q-1]' are defined. 
 *    (int)q        This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are assigned into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 *
 *  Assumptions: We assume that `q' is even.
 */
extern void
  cdpe2(
	real *out,      /* Preallocated array, length `q/2'  */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q'    */
	int   q,        /* Number of inputs---must be even   */
	const pqf *F )  /* Periodized QF data structure      */
{
  int ii, j;
  const real *ptr;
  int q2;
  
  assert( (q&1)==0 );	       /* Test that `q' is even.   */

  q2 = q/2;
  if( q > F->omega-F->alpha )  /* Long input case.       */
    {
      int  end;
      
      for( ii=0; ii<q; ii+=2 )
	{
	  *out = 0;
	  end = ii-q;
	  ptr = in + end;
	  for( j=F->alpha; j<=end; j++ )
	    *out += F->f[j]* (*ptr--);

	  j   = max(F->alpha, j);
	  end = min(F->omega, ii);
	  ptr = in + ii - j;
	  for( ; j<=end; j++ )
	    *out += F->f[j]* (*ptr--);

	  j   = ii + 1;
	  ptr = in + q - 1;
	  for( ; j<=F->omega; j++ )
	    *out += F->f[j] * (*ptr--);

	  out += step;
	}
    }
  else                         /* Short input case.     */
    {
      for( ii=0; ii<q; ii+=2 )
	{
	  *out = 0;
	  ptr = F->fp + PQFO(q2) + ii;
	  for( j=0; j<q; j++ )
	    *out += (*ptr--) * in[j];
	  out += step;
	}
    }
  return;
}

/************************************************************
 * acdpi():
 *
 *    [A]djoint [C]onvolution-[D]ecimation:
 *      [P]eriodic, sequential [I]nput, superposition.
 *
 *  Basic algorithm:
 *
 *   Let Q = 2*Q2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For I = 0 to Q2-1
 *         Let A2I = 2*I-F.ALPHA
 *         Let O2I = 2*I-F.OMEGA
 *         For J = 0 to A2I-Q
 *            OUT[J*STEP] += FILTER[2*I-J-Q]*IN[I]
 *         For J = max(0,O2I) to min(Q-1,A2I)
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *         For J = O2I+Q to Q-1
 *            OUT[J*STEP] += FILTER[Q+2*I-J]*IN[I]
 *   Else
 *      Let FILTER = F.FP + PQFO(Q2)
 *      For I = 0 to Q2-1
 *         For J = 0 to Q-1
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *
 *
 *  Calling sequence:
 *     acdpi(out, step, in, q2, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q-1)]' are defined, q=2*q2. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q2-1]' are defined. 
 *    (int)q2       This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 */
extern void
  acdpi(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int q2,         /* Number of elements of `in[]'      */
	const pqf *F )  /* Periodized QF data structure      */
{
  int ii, j, q;
  real *outp;
  const real *filt;
  
  q = 2*q2;			 /* Output array length.  */
  if( q > F->omega-F->alpha )    /* Long signal case:     */
    {
      int a2i, o2i;

      for( ii=0; ii<q; ii+=2 )
	{
	  a2i = ii - F->alpha;
	  o2i = ii - F->omega;

	  j = 0;   outp = out;   filt = F->f + ii-q;
	  while( j<=a2i-q )
	    {
	      *outp += (*filt--) * (*in);
	      outp += step; ++j;
	    }
	  
	  j = max(0, o2i);  outp = out+j*step;  filt = F->f +ii-j;
	  while( j<=min(a2i, q-1) )
	    {
	      *outp += (*filt--) * (*in);
	      outp += step; ++j;
	    }
	  
	  j = o2i+q;  outp = out+j*step;  filt = F->f + q+ii-j;
	  while( j<q )
	    {
	      *outp += (*filt--) * (*in);
	      outp += step; ++j;
	    }
	  ++in;
	}
    }
  else				/* Short signal case:    */
    {
      for( ii=0; ii<q; ii+=2 )
	{
	  outp = out;   filt = F->fp + PQFO(q2) + ii;
	  for( j=0; j<q; j++)
	    {
	      *outp += (*filt--) * (*in);
	      outp += step;
	    }
	  ++in;
	}
    }
  return;
}

/*************************************************************
 * acdpo():
 *
 *   [A]djoint [C]onvolution-[D]ecimation:
 *       [P]eriodic, sequential [O]utput, superposition.
 *
 *  Basic algorithm:
 *
 *   Let Q = 2*Q2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      For J = 0 to Q-1
 *         Let JA2 = ICH(J+F.ALPHA)
 *         Let JO2 = IFH(J+F.OMEGA)
 *         For I = 0 to JO2-Q2
 *            OUT[J*STEP] += FILTER[Q+2*I-J]*IN[I]
 *         For I = max(0,JA2) to min(Q2-1,JO2)
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *         For I = JA2+Q2 to Q2-1
 *            OUT[J*STEP] += FILTER[2*I-J-Q]*IN[I]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For J = 0 to Q-1
 *         For I = 0 to Q2-1
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *
 *  Calling sequence:
 *     acdpo(out, step, in, q2, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q-1)]' are defined, q=2*q2. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q2-1]' are defined. 
 *    (int)q2       This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are added into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 */
extern void
  acdpo(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int   q2,       /* Number of elements of `in[]'      */
	const pqf *F )  /* Periodized qf data structure      */
{
  int i, j, q;
  const real *inp;
  const real *filt;
  
  q = 2*q2;			 /* Output array length.   */
  if( q > F->omega-F->alpha )    /* Long signal case:      */
    {
      int ja2, jo2;
      
      for( j=0; j<q; j++ )
	{
	  ja2 = ICH(j+F->alpha);
	  jo2 = IFH(j+F->omega);
	  
	  i=0;  filt = F->f +q-j;  inp = in;
	  while( i<=jo2-q2 )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  
	  i=max(0, ja2);  filt = F->f + 2*i-j; inp = in+i;
	  while( i<=min(q2-1,jo2) )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  
	  i=q2+ja2;  filt = F->f+2*i-j-q;  inp = in+i;
	  while( i<q2 )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  out += step;
	}
    }
  else				/* Short signal case:    */
    {
      for( j=0; j<q; j++ )
	{
	  inp = in;  filt = F->fp + PQFO(q2) - j;
	  for( i=0; i<q2; i++)
	    {
	      *out += (*filt) * (*inp++);
	      filt += 2;
	    }
	  out += step;
	}
    }
  return;
}

/*************************************************************
 * acdpe():
 *
 *   [A]djoint [C]onvolution-[D]ecimation:
 *       [P]eriodic, assign output with [E]quals.
 *
 *  Basic algorithm:
 *
 *   Let Q = 2*Q2
 *   If Q > F.OMEGA-F.ALPHA then
 *      Let FILTER = F.F
 *      Let OUT[J*STEP] = 0
 *      For J = 0 to Q-1
 *         Let JA2 = ICH(J+F.ALPHA)
 *         Let JO2 = IFH(J+F.OMEGA)
 *         For I = 0 to JO2-Q2
 *            OUT[J*STEP] += FILTER[Q+2*I-J]*IN[I]
 *         For I = max(0,JA2) to min(Q2-1,JO2)
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *         For I = JA2+Q2 to Q2-1
 *            OUT[J*STEP] += FILTER[2*I-J-Q]*IN[I]
 *   Else
 *      Let FILTER = F.FP+PQFO(Q2)
 *      For J = 0 to Q-1
 *         Let OUT[J*STEP] = 0
 *         For I = 0 to Q2-1
 *            OUT[J*STEP] += FILTER[2*I-J]*IN[I]
 *
 *  Calling sequence:
 *     acdpe(out, step, in, q2, F)
 *
 *  Inputs:
 *    (real *)out   This output array must be preallocated so
 *                    that elements `out[0]' through
 *                    `out[step*(q-1)]' are defined, q=2*q2. 
 *    (int)step     This integer is the increment between 
 *                    `out[]' elements.
 *    (const real *)in    This input array must be preallocated
 *                    so that `in[0],...,in[q2-1]' are defined. 
 *    (int)q2       This is the period of the input array.
 *    (const pqf *)F     This specifies the periodized QF struct.
 *
 *  Outputs:
 *    (real *)out   Computed values are assigned into this array
 *                    by side effect, at elements separated
 *                    by offsets of `step' starting at `out'.
 */
extern void
  acdpe(
	real *out,      /* Preallocated array, length `2*q2' */
	int step,       /* Increment between `out[]' values  */
	const real *in, /* Preallocated array, length `q2'   */
	int   q2,       /* Number of elements of `in[]'      */
	const pqf *F )  /* Periodized QF data structure      */
{
  int i, j, q;
  const real *inp;
  const real *filt;
  
  q = 2*q2;			 /* Output array length.   */
  if( q > F->omega-F->alpha )    /* Long signal case:      */
    {
      int ja2, jo2;
      
      for( j=0; j<q; j++ )
	{
	  *out = 0;
	  ja2 = ICH(j+F->alpha);
	  jo2 = IFH(j+F->omega);
	  
	  i=0;  filt = F->f +q-j;  inp = in;
	  while( i<=jo2-q2 )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  
	  i=max(0, ja2);  filt = F->f + 2*i-j; inp = in+i;
	  while( i<=min(q2-1,jo2) )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  
	  i=q2+ja2;  filt = F->f+2*i-j-q;  inp = in+i;
	  while( i<q2 )
	    {
	      *out += (*filt) * (*inp++);
	      filt +=2;   ++i;
	    }
	  out += step;
	}
    }
  else				/* Short signal case:    */
    {
      for( j=0; j<q; j++ )
	{
	  *out = 0;
	  inp = in;  filt = F->fp + PQFO(q2) - j;
	  for( i=0; i<q2; i++)
	    {
	      *out += (*filt) * (*inp++);
	      filt += 2;
	    }
	  out += step;
	}
    }
  return;
}
