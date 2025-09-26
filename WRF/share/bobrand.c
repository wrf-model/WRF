/* Author: Sam Trahan, October 2011

   This is the implementation of a good but simple random number
   generator designed by Bob Jenkins, who placed it in the public
   domain.  His website described the algorithm and its public domain
   status on 2:23 AM EDT October 8, 2011 at this location:

       http://burtleburtle.net/bob/rand/smallprng.html

   And at that time, it said, "I wrote this PRNG. I place it in the
   public domain."  (PNRG is an acronym for "psuedo-random number
   generator" as defined elsewhere on his website.)

   I modified his code to work as an array of random number generators
   and generate four output types (float, double, int32, int64).  This
   code is tested on the Intel, IBM and GNU C compilers, and will
   successfully produce identical floating-point numbers in [0,1) on
   all three compilers.  This code is not sensitive to optimization
   since all calculations are integer calculations, and hence are
   exact.

   This algorithm, unlike the common Mersenne Twister, is not
   cryptographically secure, so don't use it to encrypt your banking
   information.  However, it does pass the entire suite of DIEHARD
   tests, so it is sufficiently random for meterological purposes.
   Its advantage over cryptographically secure algorithms is that it
   only needs 16 bytes to store its state, and is very fast, allowing
   us to have an independent random number generator for each
   gridpoint.  That avoids domain decomposition issues and allows us
   to generate random numbers in parallel across all processes,
   producing the same results regardless of which process or thread
   has which gridpoint.

   Don't change any of the constants in this file without rerunning
   the full suite of randomness tests as described on Bob's website.
   Also, don't change the floating-point conversion unless you first
   test that it correctly produces 0, never produces 1.0, is uniformly
   distributed, and produces identical results on at least the Intel,
   GNU and IBM C compilers.
*/
#include <stdint.h>

typedef uint32_t u4;
typedef uint64_t u8;

#define rot(x,k) (((x)<<(k))|((x)>>(32-(k))))

void bobranval_impl( u4 *a, u4 *b, u4 *c, u4 *d, u4 *n ) {
  u4 e,i,nd=*n;

  for(i=0;i<nd;i++) {
    e = a[i] - rot(b[i], 27);
    a[i] = b[i] ^ rot(c[i], 17);
    b[i] = c[i] + d[i];
    c[i] = d[i] + e;
    d[i] = e + a[i];
  }
}

void bob_int_hash(u4 *in, u4 *out) {
  u4 a=0xf1ea5eed;
  u4 b,c,d,e,i;
  b=c=d=*in;

  for(i=0;i<20;i++) {
    e = a - rot(b, 27);
    a = b ^ rot(c, 17);
    b = c + d;
    c = d + e;
    d = e + a;
  }

  *out=d;
}

void bobranval_r4_impl( u4 *a, u4 *b, u4 *c, u4 *d, float *result, u4 *n ) {
  /* 32-bit floating point implementation */
  u4 i,nd=*n;

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++) {
    result[i]=(d[i]&0xfffff000)*2.328305e-10f;
  }
}

void bobranval_i4_impl( u4 *a, u4 *b, u4 *c, u4 *d, u4 *result, u4 *n ) {
  /* 32-bit integer implementation */
  u4 i,nd=*n;

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++)
    result[i]=d[i];
}

void bobranval_i8_impl( u4 *a, u4 *b, u4 *c, u4 *d, u8 *result, u4 *n ) {
  /* 64-bit integer implementation */
  u4 i,nd=*n;

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++)
    result[i]=d[i];

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++)
    result[i]=(result[i]<<32) | d[i];
}

void bobranval_r8_impl( u4 *a, u4 *b, u4 *c, u4 *d, u8 *result, u4 *n ) {
  /* 64-bit floating-point implementation */
  u4 i,nd=*n;

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++)
    result[i]=d[i];

  bobranval_impl(a,b,c,d,n);
  for(i=0;i<nd;i++) {
    ((double*)result)[i] = 
       (((result[i]<<32) | d[i]) & 0xfffffffffffffc00ll)
             * 5.4210108624275218691107101938441e-20;
  }
}

void bobraninit(u4 *a, u4 *b, u4 *c, u4 *d, u4 *seeds, u4 *seed2, u4 *n) {
  u4 i,nd=*n,one=1,iter;
  for(i=0;i<nd;i++) {
    a[i] = 0xf1ea5eed;
    b[i] = c[i] = d[i] = seeds[i]^*seed2;
    for (iter=0; iter<20; ++iter) {
      bobranval_impl(a+i,b+i,c+i,d+i,&one);
    }
  }
}

/* Aliases for various fortran compilers */

void int_hash(u4*i,u4*o) { bob_int_hash(i,o); }
void int_hash_(u4*i,u4*o) { bob_int_hash(i,o); }
void int_hash__(u4*i,u4*o) { bob_int_hash(i,o); }
void INT_HASH(u4*i,u4*o) { bob_int_hash(i,o); }
void INT_HASH_(u4*i,u4*o) { bob_int_hash(i,o); }
void INT_HASH__(u4*i,u4*o) { bob_int_hash(i,o); }

void bobraninit_(u4*a,u4*b,u4*c,u4*d,u4*s,u4*s2,u4*n) { bobraninit(a,b,c,d,s,s2,n); }
void bobraninit__(u4*a,u4*b,u4*c,u4*d,u4*s,u4*s2,u4*n) { bobraninit(a,b,c,d,s,s2,n); }
void BOBRANINIT_(u4*a,u4*b,u4*c,u4*d,u4*s,u4*s2,u4*n) { bobraninit(a,b,c,d,s,s2,n); }
void BOBRANINIT__(u4*a,u4*b,u4*c,u4*d,u4*s,u4*s2,u4*n) { bobraninit(a,b,c,d,s,s2,n); }

void bobranval_r4(u4*a,u4*b,u4*c,u4*d,float*f,u4*n) { bobranval_r4_impl(a,b,c,d,f,n); }
void bobranval_r4_(u4*a,u4*b,u4*c,u4*d,float*f,u4*n) { bobranval_r4_impl(a,b,c,d,f,n); }
void bobranval_r4__(u4*a,u4*b,u4*c,u4*d,float*f,u4*n) { bobranval_r4_impl(a,b,c,d,f,n); }
void BOBRANVAL_R4_(u4*a,u4*b,u4*c,u4*d,float*f,u4*n) { bobranval_r4_impl(a,b,c,d,f,n); }
void BOBRANVAL_R4__(u4*a,u4*b,u4*c,u4*d,float*f,u4*n) { bobranval_r4_impl(a,b,c,d,f,n); }

void bobranval_i4(u4*a,u4*b,u4*c,u4*d,u4*i,u4*n) { bobranval_i4_impl(a,b,c,d,i,n); }
void bobranval_i4_(u4*a,u4*b,u4*c,u4*d,u4*i,u4*n) { bobranval_i4_impl(a,b,c,d,i,n); }
void bobranval_i4__(u4*a,u4*b,u4*c,u4*d,u4*i,u4*n) { bobranval_i4_impl(a,b,c,d,i,n); }
void BOBRANVAL_I4_(u4*a,u4*b,u4*c,u4*d,u4*i,u4*n) { bobranval_i4_impl(a,b,c,d,i,n); }
void BOBRANVAL_I4__(u4*a,u4*b,u4*c,u4*d,u4*i,u4*n) { bobranval_i4_impl(a,b,c,d,i,n); }

void bobranval_r8(u4*a,u4*b,u4*c,u4*d,u8*f,u4*n) { bobranval_r8_impl(a,b,c,d,f,n); }
void bobranval_r8_(u4*a,u4*b,u4*c,u4*d,u8*f,u4*n) { bobranval_r8_impl(a,b,c,d,f,n); }
void bobranval_r8__(u4*a,u4*b,u4*c,u4*d,u8*f,u4*n) { bobranval_r8_impl(a,b,c,d,f,n); }
void BOBRANVAL_R8_(u4*a,u4*b,u4*c,u4*d,u8*f,u4*n) { bobranval_r8_impl(a,b,c,d,f,n); }
void BOBRANVAL_R8__(u4*a,u4*b,u4*c,u4*d,u8*f,u4*n) { bobranval_r8_impl(a,b,c,d,f,n); }

void bobranval_i8(u4*a,u4*b,u4*c,u4*d,u8*i,u4*n) { bobranval_i8_impl(a,b,c,d,i,n); }
void bobranval_i8_(u4*a,u4*b,u4*c,u4*d,u8*i,u4*n) { bobranval_i8_impl(a,b,c,d,i,n); }
void bobranval_i8__(u4*a,u4*b,u4*c,u4*d,u8*i,u4*n) { bobranval_i8_impl(a,b,c,d,i,n); }
void BOBRANVAL_I8_(u4*a,u4*b,u4*c,u4*d,u8*i,u4*n) { bobranval_i8_impl(a,b,c,d,i,n); }
void BOBRANVAL_I8__(u4*a,u4*b,u4*c,u4*d,u8*i,u4*n) { bobranval_i8_impl(a,b,c,d,i,n); }
