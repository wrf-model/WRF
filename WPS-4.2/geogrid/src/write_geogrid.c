/* File: write_geogrid.c

   Sample subroutine to write an array into the geogrid binary format.

   Side effects: Upon completion, a file named 00001-<NX>.00001-<NY> is
   created, where <NX> is the argument nx and <NY> is the argument ny,
   both in i5.5 format.
  
   Notes: Depending on the compiler and compiler flags, the name of 
   the write_geogrid() routine may need to be adjusted with respect
   to the number of trailing underscores when calling from Fortran.

   Michael G. Duda, NCAR/MMM
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _UNDERSCORE
#define write_geogrid write_geogrid_
#endif
#ifdef _DOUBLEUNDERSCORE
#define write_geogrid write_geogrid__
#endif

#define BIG_ENDIAN    0
#define LITTLE_ENDIAN 1

int write_geogrid(
      float * rarray,          /* The array to be written */
      int * nx,                /* x-dimension of the array */
      int * ny,                /* y-dimension of the array */
      int * nz,                /* z-dimension of the array */
      int * isigned,           /* 0=unsigned data, 1=signed data */
      int * endian,            /* 0=big endian, 1=little endian */
      float * scalefactor,     /* value to divide array elements by before truncation to integers */
      int * wordsize )         /* number of bytes to use for each array element */
{
   int i, narray;
   int A2, B2;
   int A3, B3, C3;
   int A4, B4, C4, D4;
   unsigned int * iarray;
   unsigned char * barray;
   char fname[24];
   FILE * bfile;

   narray = (*nx) * (*ny) * (*nz);

   iarray = (unsigned int *)malloc(sizeof(int) * narray);
   barray = (unsigned char *)malloc(sizeof(unsigned char) * narray * (*wordsize));

   /* Scale real-valued array by scalefactor and convert to integers */
   for (i=0; i<narray; i++)
      iarray[i] = (unsigned int)(rarray[i] / (*scalefactor));

   /* 
      Set up byte offsets for each wordsize depending on byte order.
      A, B, C, D give the offsets of the LSB through MSB (i.e., for 
      word ABCD, A=MSB, D=LSB) in the array from the beginning of a word 
   */
   if (*endian == BIG_ENDIAN) {
      A2 = 0; B2 = 1;
      A3 = 0; B3 = 1; C3 = 2;
      A4 = 0; B4 = 1; C4 = 2; D4 = 3;
   }
   else {
      B2 = 0; A2 = 1;
      C3 = 0; B3 = 1; A3 = 2;
      D4 = 0; C4 = 1; B4 = 2; A4 = 3;
   }

   /* Place words into storage byte order */
   switch(*wordsize) {
      case 1:
         for(i=0; i<narray; i++) {
            if (iarray[i] < 0 && *isigned) iarray[i] += (1 << 8);
            barray[(*wordsize)*i] = (unsigned char)(iarray[i] & 0xff);
         }
         break;

      case 2:
         for(i=0; i<narray; i++) {
            if (iarray[i] < 0 && *isigned) iarray[i] += (1 << 16);
            barray[(*wordsize)*i+A2] = (unsigned char)((iarray[i] >> 8) & 0xff);
            barray[(*wordsize)*i+B2] = (unsigned char)( iarray[i]       & 0xff);
         }
         break;

      case 3:
         for(i=0; i<narray; i++) {
            if (iarray[i] < 0 && *isigned) iarray[i] += (1 << 24);
            barray[(*wordsize)*i+A3] = (unsigned char)((iarray[i] >> 16) & 0xff);
            barray[(*wordsize)*i+B3] = (unsigned char)((iarray[i] >> 8)  & 0xff);
            barray[(*wordsize)*i+C3] = (unsigned char)( iarray[i]        & 0xff);
         }
         break;

      case 4:
         for(i=0; i<narray; i++) {
            if (iarray[i] < 0 && *isigned) iarray[i] += (1 << 32);
            barray[(*wordsize)*i+A4] = (unsigned char)((iarray[i] >> 24) & 0xff);
            barray[(*wordsize)*i+B4] = (unsigned char)((iarray[i] >> 16) & 0xff);
            barray[(*wordsize)*i+C4] = (unsigned char)((iarray[i] >> 8)  & 0xff);
            barray[(*wordsize)*i+D4] = (unsigned char)( iarray[i]        & 0xff);
         }
         break;
   }

   sprintf(fname,"%5.5i-%5.5i.%5.5i-%5.5i",1,*nx,1,*ny);

   /* Write array to file */
   bfile = fopen(fname,"wb");
   fwrite(barray,sizeof(unsigned char),narray*(*wordsize),bfile);
   fclose(bfile);

   free(iarray);
   free(barray);

   return 0;
}
