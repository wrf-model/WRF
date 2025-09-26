#include <stdlib.h>

#if defined _UNDERSCORE
  void errexit_ (int a)
#elif defined _DOUBLEUNDERSCORE
  void errexit__ (int a)
#else
  void errexit (int a)
#endif
{
    exit (a);
}
