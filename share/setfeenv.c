#ifndef CRAY
# ifdef NOUNDERSCORE
#      define SETFEENV setfeenv
# else
#   ifdef F2CSTYLE
#      define SETFEENV setfeenv__
#   else
#      define SETFEENV setfeenv_
#   endif
# endif
#endif

#include <fenv.h>
#include <stdio.h>

void SETFEENV()
{
  fenv_t envp;
  int stat;
#ifdef _OPENMP

  stat = fegetenv(&envp);
/*
  if (fesetenv(&envp) != 0) {
     perror("Error getting fp env");
  }
*/

#pragma omp parallel shared(envp)
{
  stat = fesetenv(&envp);
/*
  if (fesetenv(&envp) != 0) {
     perror("Error setting fp env");
  }
*/
}
#endif
}
