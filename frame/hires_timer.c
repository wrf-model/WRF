/* hires_timer: this implements a cross-platform, high temporal
   resolution timer using POSIX standard C routines.  Call
   init_hires_timer to initialize and call hires_timer to get the time
   in seconds since the call to init_hires_timer as a double (fortran
   REAL(KIND=8)).

   On platforms that support clock_gettime, nanosecond resolution will
   be provided.  On platforms that don't, gettimeofday will be used,
   and microsecond resolution will be provided.

   If init_hires_timer is never called, the time returned is seconds
   since the system epoch, but with lower precision.  On unix, the
   epoch is midnight, the morning of January 1, 1970, in either UTC or
   GMT depending on your particular flavor of unix.  However, a
   double-precision floating point number cannot represent the time in
   seconds since that epoch to nanosecond resolution, so you'll end up
   with something more like 10^-5 second resolution unless you call
   init_hires_timer.
*/

#include <time.h>
#include <sys/time.h>
#include <stdlib.h>

#if defined(OLD_TIMERS)
/* User does not want to use the high-res timers.  To 
   make sure they are not used, we don't even compile
   them.  We'll make sure the object file isn't empty
   though, to avoid confusing ld:  */
void hires_timer_dummy() {
  return;
}
#else

static int initialized=0; /* =1 if start and startnano are valid */

static time_t start_ipart=0; /* integer part of starting time in seconds */
static double start_fpart=0; /* fractional part */

/* DETERMINE IF CLOCK_GETTIME IS AVAILABLE */

#define USE_HIRES 0
#if defined(_POSIX_TIMERS)
#if ( _POSIX_TIMERS > 0 )
/* According to the POSIX standard, we only get here if the system
   supports clock_gettime. */
#define USE_HIRES 1
#endif
#endif

void init_hires_timer() {
#if ( USE_HIRES == 1 )
  struct timespec when;
  if(!clock_gettime(CLOCK_REALTIME,&when)) {
    start_ipart=when.tv_sec;
    start_fpart=when.tv_nsec/1e9;
  } else { /* clock_gettime failed */
#endif
    struct timeval tv;
    if(!gettimeofday(&tv,NULL)) {
      start_ipart=tv.tv_sec;
      start_fpart=tv.tv_usec/1e6;
    } else {
      /* Should never get here; gettimeofday never fails
         unless tv is outside of the address space.  Just
         as a paranoid fallback, we check for a failure
         and use time() */
      start_ipart=time(NULL);
      start_fpart=0;
    }
#if ( USE_HIRES == 1 )
  }
#endif
  initialized=1;
}


void hires_timer(double *d) {
  struct timeval tv;
#if ( USE_HIRES == 1 )
  struct timespec when;
#endif
  if(!initialized) init_hires_timer();
#if ( USE_HIRES == 1 )
  if(!clock_gettime(CLOCK_REALTIME,&when)) {
    *d=(double)(when.tv_sec-start_ipart) + ( ((double)when.tv_nsec)/1e9 - start_fpart );
  } else { /* clock_gettime failed */
#endif
    if(!gettimeofday(&tv,NULL)) {
      *d=(double)(tv.tv_sec-start_ipart) + ( ((double)tv.tv_usec)/1e6 - start_fpart );
    } else {
      /* Should never get here; gettimeofday never fails
         unless tv is outside of the address space.  Just
         as a paranoid fallback, we check for a failure
         and use time() */
      *d=(double)(time(NULL)-start_ipart) - start_fpart;
    }
#if ( USE_HIRES == 1 )
  }
#endif
}

/* Support all common fortran name mangling schemes: */
void hires_timer_(double *d) { hires_timer(d); }
void hires_timer__(double *d) { hires_timer(d); }
void HIRES_TIMER(double *d) { hires_timer(d); }
void HIRES_TIMER_(double *d) { hires_timer(d); }
void HIRES_TIMER__(double *d) { hires_timer(d); }

void init_hires_timer_() { init_hires_timer(); }
void init_hires_timer__() { init_hires_timer(); }
void INIT_HIRES_TIMER() { init_hires_timer(); }
void INIT_HIRES_TIMER_() { init_hires_timer(); }
void INIT_HIRES_TIMER__() { init_hires_timer(); }

#endif /* ELSE for the "if(OLD_TIMERS)" */
