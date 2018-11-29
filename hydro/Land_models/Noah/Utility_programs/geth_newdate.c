/*
  Program Name:
  Author(s)/Contact(s):
  Abstract:
  History Log:
 
  Usage:
  Parameters: <Specify typical arguments passed>
  Input Files:
        <list file names and briefly describe the data they include>
  Output Files:
        <list file names and briefly describe the information they include>
 
  Condition codes:
        <list exit condition or error codes returned >
        If appropriate, descriptive troubleshooting instructions or
        likely causes for failures could be mentioned here with the
        appropriate error code
 
  User controllable options: <if applicable>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <sys/uio.h>

int nfeb(int year);
int geti(char *s, int p, int l);

main(int *argc, char **argv)
{
  char *h, *hh;
  char *g;
  char *k;
  int yrold, moold, dyold, hrold, miold, scold, frold, i, j, ilen;
  int yrnew, monew, dynew, hrnew, minew, scnew, frnew;
  int idt;
  int mday[12];

  int ifrc;
  int nday, nhour, nmin, nsec, nfrac;
  char Continuous, SwapFmt;

  mday[0]  = 31;
  mday[1]  = 28;
  mday[2]  = 31;
  mday[3]  = 30;
  mday[4]  = 31;
  mday[5]  = 30;
  mday[6]  = 31;
  mday[7]  = 31;
  mday[8]  = 30;
  mday[9] = 31;
  mday[10] = 30;
  mday[11] = 31;

  if (strncmp("-swap", argv[1], 5)) {
    SwapFmt = 0;
    idt = atoi(argv[2]);
    hh = argv[1];
  }
  else {
    SwapFmt = 1;
    idt = atoi(argv[3]);
    hh = argv[2];
  }

  for (i = 0; hh[i] != '\0'; ++i)
    ilen = i; 

  yrold = 0;
  moold = 0;
  dyold = 0;
  hrold = 0;
  miold = 0;
  scold = 0;
  frold = 0;

  /* Check the date string, to see if it is of the form YYYY-MM-DD_HH  *
   * or of the form YYYYMMDDHH.                                        */
  if ( hh[4] == '-' ) {
    /* We have the form YYYY-MM-DD_HH */
    Continuous = 0;
    yrold = geti(hh, 0, 4);
    moold = geti(hh, 5, 2);
    dyold = geti(hh, 8, 2);
    if (ilen>10) {
      hrold = geti(hh, 11, 2);
      if (ilen>13) {
	miold = geti(hh, 14, 2);
	if (ilen>16) {
	  scold = geti(hh, 17, 2);
	  if (ilen>19) {
	    frold = geti(hh, 20, ilen-19);
	  }
	}
      }
    }
  }
  else {
    int jadd;
    /* We have the form YYYYMMDDHH */
    Continuous = 1;
    jadd = 0;
    yrold = geti(hh, 0, 4);
    moold = geti(hh, 4, 2);
    jadd ++;
    dyold = geti(hh, 6, 2);
    jadd ++;
    if (ilen > 8) {
      hrold = geti(hh, 8, 2);
      jadd ++;
      if (ilen > 10) {
	miold = geti(hh, 10,2);
	jadd ++;
	if (ilen > 12) {
	  scold = geti(hh, 12,2);
	  jadd ++;
	  if (ilen > 13) {
	    frold = geti(hh, 14,ilen-13);
	    jadd++;
	  }
	}
      }
    }
    ilen += jadd;
  }

  mday[1] = nfeb(yrold); 

/*   Compute the number of days, hours, minutes, and seconds in idt */

  if (ilen > 19) /*idt should be in fractions of seconds*/
    {
      if (ilen == 20) ifrc = 10;
      if (ilen == 21) ifrc = 100;
      if (ilen == 22) ifrc = 1000;
      if (ilen == 23) ifrc = 10000;
      nday   = abs(idt)/(86400*ifrc);
      nhour  = div(abs(idt), 86400*ifrc).rem/(3600*ifrc);
      nmin = div(abs(idt), 3600*ifrc).rem/(60*ifrc);
      nsec = div(abs(idt), 60*ifrc).rem/ifrc;
      nfrac = div(abs(idt), ifrc).rem;
       }
  else if (ilen > 17) /* idt should be in seconds */
    {
      ifrc = 1;
      nday   = abs(idt)/86400;  /* Integer number of days in delta-time */
      nhour = div(abs(idt), 86400).rem/3600;
      nmin = div(abs(idt), 3600).rem/60;
      nsec = div(abs(idt), 60).rem;
      nfrac  = 0;
    }
  else if (ilen > 14) /*idt should be in minutes */
    {
      ifrc = 1; 
      nday   = abs(idt)/1440; /* Integer number of days in delta-time */
      nhour = div(abs(idt), 1440).rem/60;
      nmin = div(abs(idt), 60).rem; 
      nsec   = 0; 
      nfrac  = 0; 
    }
  else if (ilen > 11)  /* idt should be in hours */
    {
      ifrc = 1;
      nday   = abs(idt)/24; /* Integer number of days in delta-time */
      nhour = div(abs(idt),24).rem;
      nmin   = 0; 
      nsec   = 0;
      nfrac  = 0;
    }
  else if (ilen > 8)   /* idt should be in days */
    {
      ifrc = 1; 
      nday   = abs(idt);  /* Integer number of days in delta-time */
      nhour  = 0;
      nmin   = 0; 
      nsec   = 0; 
      nfrac  = 0; 
    }
  else
    {
      printf("Strangeness\n");
      return(-1);
    }

  if (idt == 0) {
    yrnew = yrold;
    monew = moold;
    dynew = dyold;
    hrnew = hrold;
    minew = miold; 
    scnew = scold;
    frnew = frold;
  }
  else if (idt > 0) {

    frnew = frold + nfrac;
    if (frnew >= ifrc) {
      frnew = frnew - ifrc;
      nsec = nsec + 1;}

    scnew = scold + nsec; 
    if (scnew >= 60) {
      scnew = scnew - 60; 
      nmin  = nmin + 1;}

    minew = miold + nmin; 
    if (minew >= 60) {
      minew = minew - 60;
      nhour  = nhour + 1;}

    hrnew = hrold + nhour; 
    if (hrnew >= 24) {
      hrnew = hrnew - 24; 
      nday  = nday + 1; }

    dynew = dyold;
    monew = moold;
    yrnew = yrold;
    for(i = 1; i <= nday; i++){
      dynew = dynew + 1;
      if (dynew > mday[monew-1]) {
	dynew = dynew - mday[monew-1]; 
	monew = monew + 1; 
	if (monew > 12) {
	  monew = 1; 
	  yrnew = yrnew + 1; 
	  /* If the year changes, recompute the number of days in February */
	  mday[1] = nfeb(yrnew); }
      }
    }
  }

  else if (idt < 0) {

    frnew = frold - nfrac; 
    if (frnew <  0) {
      frnew = frnew + ifrc; 
      nsec = nsec - 1; }

    scnew = scold - nsec; 
    if (scnew < 00) {
      scnew = scnew + 60;
      nmin  = nmin + 1; }

    minew = miold - nmin;
    if (minew < 00) {
      minew = minew + 60;
      nhour  = nhour + 1;}

    hrnew = hrold - nhour; 
    if (hrnew < 00) {
      hrnew = hrnew + 24; 
      nday  = nday + 1; }

    dynew = dyold;
    monew = moold;
    yrnew = yrold;
    for(i = 1; i <= nday; i++){
      dynew = dynew - 1; 
      if (dynew == 0) {
	monew = monew - 1; 
	if (monew == 0) {
	  monew = 12; 
	  yrnew = yrnew - 1; 
          /* If the year changes, recompute the number of days in February */
	  mday[1] = nfeb(yrnew);
	}
	dynew = mday[monew-1];
      }
    }
  }

  if ( (Continuous) && SwapFmt ) {
    Continuous = 0;
  }
  else if ( ( ! Continuous)  && SwapFmt ) {
    Continuous = 1;
  }

  if (ilen > 19) {
    if (Continuous) {
      printf("%4.4i%2.2i%2.2i%2.2i%2.2i%2.2i%4.4i\n",
	     yrnew, monew, dynew, hrnew, minew, scnew, frnew); 
    }
    else {
      printf("%4.4i-%2.2i-%2.2i_%2.2i:%2.2i:%2.2i.%4.4i\n",
	     yrnew, monew, dynew, hrnew, minew, scnew, frnew); 
    }
  }
  else if (ilen > 17) {
    if (Continuous) {
      printf("%4.4i%2.2i%2.2i%2.2i%2.2i%2.2i\n",
	     yrnew, monew, dynew, hrnew, minew, scnew); 
    }
    else {
      printf("%4.4i-%2.2i-%2.2i_%2.2i:%2.2i:%2.2i\n",
	     yrnew, monew, dynew, hrnew, minew, scnew); 
    }
  }
  else if (ilen > 14) {
    if (Continuous) {
      printf("%4.4i%2.2i%2.2i%2.2i%2.2i\n",
	     yrnew, monew, dynew, hrnew, minew); 
    }
    else {
      printf("%4.4i-%2.2i-%2.2i_%2.2i:%2.2i\n",
	     yrnew, monew, dynew, hrnew, minew); 
    }
  }
  else if (ilen > 11) {
    if (Continuous) {
      printf("%4.4i%2.2i%2.2i%2.2i\n",
	     yrnew, monew, dynew, hrnew); 
    }
    else {
      printf("%4.4i-%2.2i-%2.2i_%2.2i\n",
	     yrnew, monew, dynew, hrnew); 
    }
  }
  else if (ilen > 8) {
    if (Continuous) {
      printf("%4.4i%2.2i%2.2i\n",
	     yrnew, monew, dynew); 
    }
    else {
      printf("%4.4i-%2.2i-%2.2i\n",
	     yrnew, monew, dynew); 
    }
  }
  exit(0);

}

int nfeb(int year)
{
  int nfeb;

  nfeb = 28; /* By default, February has 28 days ... */
  if (div(year,4).rem == 0){
    nfeb = 29;  /* But every four years, it has 29 days ... */
    if (div(year,100).rem == 0) {
      nfeb = 28;  /* Except every 100 years, when it has 28 days ... */
      if (div(year,400).rem == 0){
	nfeb = 29;  /* Except every 400 years, when it has 29 days ... */
	if (div(year,3600).rem == 0){
	  nfeb = 28;  /* Except every 3600 years, when it has 28 days. */
	}
      }
    }
  }
  return(nfeb);
}

int geti(char *s, int p, int l) {
  int i, ival;
  ival = 0;
  for (i=0; i<l; i++) {
    ival = (ival*10) + s[p+i]-'0';
  }
  return(ival);
}
