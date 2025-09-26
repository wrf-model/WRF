// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include <stdio.h>
#include <string.h>

void qftest(char *nam,		// in: filter name.
             int ran) {		// in: filter length.
  int i=0;			// assume input checks OK.

  if( ! strcmp(nam,"B") ) {
     if( ran!=18 ) {
        fprintf(stderr,"qftest: for nam==B need ran==18, not %d.\n",ran);
        i=1;
     }
  } else if( ! strcmp(nam,"C") ) {
     if( ran!=6 && ran!=12 && ran!=18 && ran!=24 && ran!=30 ) {
        fprintf(stderr,"qftest: for nam==C need ran in {6,12,18,24,30}; not %d.\n",ran);
        i=1;
     }
  } else if( ! strcmp(nam,"D") ) {
     if( ran!=2 && ran!=4 && ran!=6 && ran!=8 && ran!=10 && ran!=12 && ran!=14 && ran!=16 && ran!=18 && ran!=20 ) {
        fprintf(stderr,"qftest: for nam==D need ran in {2,4,6,8,10,12,14,16,18,20}; not %d.\n",ran);
        i=1;
     }
  } else if( ! strcmp(nam,"V") ) {
     if( ran!=24 ) {
        fprintf(stderr,"qftest: for nam==V need ran==24, not %d.\n",ran);
        i=1;
     }
  } else {
     fprintf(stderr,"qftest: need <nam> to be B, C, D or V.\n");
     i=1;
  }
  if( i ) {
     fprintf(stderr,"qftest: abort().\n");
     fflush(stderr);
     abort();
  } else return;
}
