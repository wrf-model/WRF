/*C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CREAD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2012-09-15
C
C ABSTRACT: CREAD IS A PACKAGE OF C LANGUAGE I/O ROUTINES WHICH 
C           ARE DESIGNED TO OPERATE BUFRLIB INPUT AND OUTPUT
C           FUNCTIONS IN A LESS RESTRICTIVE WAY COMPARED TO 
C           THOSE AVAILABLE IN STANDARD FORTRAN IMPLEMENTATIONS.
C           THE PACKAGE CONSISTS OF THREE FILE OPEN ROUTINES,
C           ONE FILE CLOSE ROUTINE, TWO FILE POSITIONING 
C           ROUTINES, ONE READ BUFR AND ONE WRITE BUFR ROUTINE. 
C           ARRAYS OF FILE CONNECTION DESCRIPTORS AND FILE 
C           POSITION POINTERS PROVIDE THE CONNECTION TO THE
C           BUFRLIB INTERNAL FILE STATUS INDICATORS.  THE
C           BUFRLIB FILE CONNECTION INDEX LUN, OBTAINED BY
C           CALLS TO STATUS, IS USED TO REFERENCE THE CREAD
C           DESCRIPTOR AND POINTER ARRAYS.
C
C PROGRAM HISTORY LOG:
C 2012-09-15  J. WOOLLEN -- ORIGINAL AUTHOR
C
C USAGE: CALL openrb(nfile,ufile)        - open ufile for binary reading
C        CALL openwb(nfile,ufile)        - open ufile for binary writing
C        CALL openab(nfile,ufile)        - open ufile for binary appending
C        CALL backbufr(nfile)            - backspace file nfile 1 message
C        CALL cewind(nfile)              - rewind file nfile to beginning
C        CALL closfb(nfile)              - disconnect file nfile from c
C        CALL crdbufr(nfile,bufr,maxbyt) - read next bufr message from file nfile into bufr
C        CALL cwrbufr(nfile,bufr,nwrd)   - write bufr message from bufr into file nfile
C
C   INPUT ARGUMENTS:
c      nfile  - integer bufrlib file connection index
C      ufile  - full file path/filename
c      bufr   - in crdbufr: char array to read a bufr message into 
c      maxbyt - in crdbufr: maximum number of bytes allowed to read
c      bufr   - in cwrbufr: integer array to write a bufr message from 
c      nwrd   - in cwrbufr: number of words to write for bufr message
C
C   OUTPUT ARGUMENTS:
c      crdbufr - return code from reading
c                -3 - sec0 message length > maxbyt
c                -2 - error reading bufr message
c                -1 - no more more messages in file
c                 0 - read a bufr message 
C
C REMARKS:
C    THIS ROUTINE CALLS:           IUPBS01
C
C    THIS ROUTINE IS CALLED BY: 
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

/* The following arrays are dimensioned one larger than NFILES because of the difference in array
   indexing between Fortran and C.  In each of the following C functions, the value passed in for
   nfile will a be Fortran index ranging from 1 to NFILES, so we need to allow for this same range
   of values in C, which would otherwise expect the array indices to range from 0 to NFILES-1. */
FILE *pb[NFILES+1]; fpos_t lstpos[NFILES+1]; 

void openrb   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "rb " ); }
void openwb   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "wb " ); }
void openab   (nfile,ufile) f77int *nfile; char *ufile; { pb[*nfile] = fopen( ufile , "a+b" ); }
void backbufr (nfile      ) f77int *nfile;              { fsetpos(pb[*nfile],&lstpos[*nfile]);}
void cewind   (nfile      ) f77int *nfile;              { rewind(pb[*nfile]); }
void closfb   (nfile      ) f77int *nfile;              { fclose(pb[*nfile]); }

f77int crdbufr (nfile,bufr,mxbyt)
f77int *nfile; f77int *mxbyt; char *bufr;
{  f77int  nbyt; f77int  nb; f77int wkint[2]; fpos_t nxtpos;
   fgetpos(pb[*nfile],&lstpos[*nfile]);
   nb = sizeof(*bufr); bufr[0]=bufr[1];
   while ( strncmp(bufr,"BUFR",4)!=0)
   {  memmove(bufr,&bufr[1],3);
      if(fread(bufr+3,nb,1,pb[*nfile])!=1) return -1;
   }
   fgetpos(pb[*nfile],&nxtpos); if(fread(bufr+4,nb,4,pb[*nfile])!=4) return -1;
   memcpy(wkint,bufr,8); nbyt=iupbs01(wkint,"LENM",4)-8;
   if(nbyt+8>*mxbyt)                           {fsetpos(pb[*nfile],&nxtpos);return -3;};
   if(fread(bufr+8,nb,nbyt,pb[*nfile])!=nbyt)  {fsetpos(pb[*nfile],&nxtpos);return -2;};
   if(strncmp(bufr+nbyt+4,"7777",4)!=0)        {fsetpos(pb[*nfile],&nxtpos);return -2;};
   return 0;
}

void cwrbufr (nfile,bufr,nwrd) 
f77int *nfile; f77int *nwrd; f77int  *bufr;
{  f77int  nb; nb = sizeof(*bufr);
   fwrite(bufr,nb,*nwrd,pb[*nfile]);
}
