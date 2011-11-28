/* Jan. 2005.

   A utility that converts unformatted binary real data files
   to unformatted binary double precision data files.

   Compile as 

        cc -o four2eight four2eight.c

   If you are running this on a little-endian platform
   compile with -DSWAP like so:

        cc -o four2eight -DSWAP four2eight.c

   Use as 

        four2eight < RRTM_DATA > RRTM_DATA_DBL  (for example)

   JM

*/


#include <stdio.h>

int
main()
{
   int in, cr, cr1, n ;
   int i ;
   float x, x1 ;
   double y, y1 ;

   while ( 
     fread( &in, 1, 4, stdin ) > 0 ) {
     swap4(&in, &cr) ;
     n = cr ;
     cr1 = 2*cr ;
     fprintf(stderr, "%d > %d\n",cr,cr1) ;
     swap4(&cr1,&cr) ;
     fwrite( &cr, 1, 4, stdout ) ;
     for ( i = 0 ; i < n ; i += 4 )
     {
       fread ( &x, 1, 4, stdin ) ;
       swap4(&x,&x1) ;
       y1 = x1 ;
       swap8(&y1,&y) ;
       fwrite ( &y, 1, 8, stdout ) ;
     }
     fread( &in, 1, 4, stdin ) ;
     fwrite( &cr, 1, 4, stdout ) ;
   }
   fprintf(stderr,"\n") ;
}

void
swap4( a, b )
   char a[], b[] ;
{
#ifdef SWAP
   b[0] = a[3] ;
   b[1] = a[2] ;
   b[2] = a[1] ;
   b[3] = a[0] ;
#else
   b[0] = a[0] ;
   b[1] = a[1] ;
   b[2] = a[2] ;
   b[3] = a[3] ;
#endif
}

void
swap8( a, b )
   char a[], b[] ;
{
#ifdef SWAP
   b[0] = a[7] ;
   b[1] = a[6] ;
   b[2] = a[5] ;
   b[3] = a[4] ;
   b[4] = a[3] ;
   b[5] = a[2] ;
   b[6] = a[1] ;
   b[7] = a[0] ;
#else
   b[0] = a[0] ;
   b[1] = a[1] ;
   b[2] = a[2] ;
   b[3] = a[3] ;
   b[4] = a[4] ;
   b[5] = a[5] ;
   b[6] = a[6] ;
   b[7] = a[7] ;
#endif
}

