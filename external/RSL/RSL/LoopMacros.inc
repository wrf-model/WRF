c#define OLDR90

#ifdef OLDR90
# define MAX_RUNPAD 0
#else
# define MAX_RUNPAD 2
#endif

#ifndef RSL_MAXRUN
# define RSL_MAXRUN 700
#endif


#  define RSL_RUN_ARGS  xinest,nrunj,nruni,js,is,ie,is2,js2,je2,idif,jdif
#  define RSL_RUN_VARS RSL_RUN_ARGS

#  define RSL_INIT_RUNS(D) rsl_get_run_info(D,RSL_MAXRUN,RSL_RUN_VARS)

#  define RSL_DECLARE_RUN_ARGS integer xinest,nrunj,nruni,js(nrunj),is(nrunj),ie(nrunj),is2(nruni),js2(nruni),je2(nruni),idif,jdif,nr,ig,jg
#  define RSL_DECLARE_RUN_VARS integer xinest,nrunj,nruni,js(RSL_MAXRUN),is(RSL_MAXRUN),ie(RSL_MAXRUN),is2(RSL_MAXRUN),js2(RSL_MAXRUN),je2(RSL_MAXRUN),idif,jdif,nr,ig,jg

#  define RSL_MAJOR_BOUND(J,L,H) if ((((J)-jdif).ge.(L)).and.(((J)-jdif).le.(H))) then
#  define RSL_MINOR_BOUND(I,L,H) if ((((I)-idif).ge.(L)).and.(((I)-idif).le.(H))) then
#  define RSL_END_BOUND  endif

#  define RSL_MAJOR_LOOP(J) do nr=1+MAX_RUNPAD,nrunj+MAX_RUNPAD;J=js(nr);jg=J-jdif
#  define RSL_END_MAJOR_LOOP enddo

#  define RSL_MINOR_LOOP(I) do I=is(nr),ie(nr);ig=I-idif
#  define RSL_END_MINOR_LOOP enddo

#  define RSL_MAJOR_LOOPB(J,L,H) do nr=1+MAX_RUNPAD,nrunj+MAX_RUNPAD;J=js(nr);jg=J-jdif;RSL_MAJOR_BOUND(J,L,H)
#  define RSL_END_MAJOR_LOOPB RSL_END_BOUND ; enddo

#  define RSL_MINOR_LOOPB(I,L,H) do I=is(nr),ie(nr);ig=I-idif;RSL_MINOR_BOUND(I,L,H)
#  define RSL_END_MINOR_LOOPB RSL_END_BOUND ; enddo


