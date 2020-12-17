#if defined(__linux) && defined(NMM_FIND_LOAD_IMBALANCE)
#define _GNU_SOURCE
#include <utmpx.h>

void nmm_get_cpu(int *cpu,int *ierr) {
  *cpu=sched_getcpu();
  *ierr = (*cpu>=0);
}
#else
void nmm_get_cpu(int *cpu,int *ierr) {
  *cpu=0;
  *ierr=0;
}
#endif

void nmm_get_cpu_(int *c,int*i) { nmm_get_cpu(c,i); }
void nmm_get_cpu__(int *c,int*i) { nmm_get_cpu(c,i); }
void NMM_GET_CPU(int *c,int*i) { nmm_get_cpu(c,i); }
void NMM_GET_CPU_(int *c,int*i) { nmm_get_cpu(c,i); }
void NMM_GET_CPU__(int *c,int*i) { nmm_get_cpu(c,i); }
