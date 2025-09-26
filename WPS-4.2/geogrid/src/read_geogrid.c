/* File: read_geogrid.c

   Sample subroutine to read an array from the geogrid binary format.

   Notes: Depending on the compiler and compiler flags, the name of 
   the read_geogrid() routine may need to be adjusted with respect
   to the number of trailing underscores when calling from Fortran.

   Michael G. Duda, NCAR/MMM
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _UNDERSCORE
#define read_geogrid read_geogrid_
#endif
#ifdef _DOUBLEUNDERSCORE
#define read_geogrid read_geogrid__
#endif

#define BIG_ENDIAN    0
#define LITTLE_ENDIAN 1

int read_geogrid(
      char * fname,            /* The name of the file to read from */
      int * len,               /* The length of the filename */
      float * rarray,          /* The array to be filled */
      int * nx,                /* x-dimension of the array */
      int * ny,                /* y-dimension of the array */
      int * nz,                /* z-dimension of the array */
      int * isigned,           /* 0=unsigned data, 1=signed data */
      int * endian,            /* 0=big endian, 1=little endian */
      float * scalefactor,     /* value to multiply array elements by before truncation to integers */
      int * wordsize,          /* number of bytes to use for each array element */
      int * status)
{
   size_t i, cnt, narray;
   int ival;
   int A2, B2;
   int A3, B3, C3;
   int A4, B4, C4, D4;
   unsigned char * c;
   char local_fname[1024];
   FILE * bfile;

   *status = 0;

   narray = (size_t)(*nx) * (size_t)(*ny) * (size_t)(*nz);

   /* Make a null-terminated local copy of the filename */
   strncpy(local_fname,fname,*len);
   local_fname[*len]='\0';

   /* Attempt to open file for reading */
   if (!(bfile = fopen(local_fname,"rb")))
   {
      *status = 1;
      return 1;
   }

   /* Allocate memory to hold bytes from file and read data */ 
   c = (unsigned char *)malloc(sizeof(unsigned char)*(*wordsize) * narray);
   cnt = fread((void *)c, sizeof(unsigned char), narray*(size_t)(*wordsize), bfile);
 
   fclose(bfile);

   if (cnt == 0) 
   {
      *status = 1;
      return 1;
   }

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

   /* Convert words from native byte order */
   switch(*wordsize) {
      case 1:
         for(i=0; i<narray; i++)
         {
            ival = (int)(c[i]);      
            if ((*isigned) && (ival > (1 << 7))) ival -= (1 << 8);
            rarray[i] = (float)ival;
         }
         break;

      case 2:
         for(i=0; i<narray; i++)
         {
            ival = (int)((c[2*i+A2]<<8) | (c[2*i+B2]));      
            if ((*isigned) && (ival > (1 << 15))) ival -= (1 << 16);
            rarray[i] = (float)ival;
         }
         break;

      case 3:
         for(i=0; i<narray; i++)
         {
            ival = (int)((c[3*i+A3]<<16) | (c[3*i+B3]<<8) | c[3*i+C3]);      
            if ((*isigned) * (ival > (1 << 23))) ival -= (1 << 24);
            rarray[i] = (float)ival;
         }
         break;

      case 4:
         for(i=0; i<narray; i++)
         {
            ival = (int)((c[4*i+A4]<<24) | (c[4*i+B4]<<16) | (c[4*i+C4]<<8) | c[4*i+D4]);      
            if ((*isigned) && (ival > (1 << 31))) ival -= (1 << 32);
            rarray[i] = (float)ival;
         }
         break;
   }

   free(c);

   /* Scale real-valued array by scalefactor */
   if (*scalefactor != 1.0)
   {
      for (i=0; i<narray; i++)
         rarray[i] = rarray[i] * (*scalefactor);
   }

   return 0;
}
