/* 20jun97/atn:  always scaling data up whether dsf is -/+; 
*/
#include <stdio.h> 
#include <stdlib.h>
#include <string.h> 
#include <math.h>
#ifdef XT3_Catamount
#include <features.h>
#undef htonl
#define htonl(x)     swap_byte4(x)
#elif defined(_WIN32)
#include <Winsock2.h>
#else
#include <netinet/in.h>
#endif
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
#include "isdb.h"		/* WORD_BIT_CNT defn */

/*
****************************************************************
* A. FUNCTION:   pack_spatial
*      pack gridded data values into a bitstream 
*
*    INTERFACE:
*      int     pack_spatial (pt_cnt, bit_cnt, pack_null, fbuff, ppbitstream,
*                           dec_scl_fctr, BDSlength, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*     (I) long *pt_cnt;                count of points in grid 
*     (I) long *bit_cnt;               count of bits to pack value in.  
*                                      will be calculated if set to zero
*     (I) float *pack_null;            parameter value for null (huge number) 
*   (I&O) float *fbuff;                array containing grid values to pack
*                                     returned scaled up by Decimal Scale Factor
*     (O) unsigned long **ppbitstream; Null upon entry; 
*                                      returned pointing to new Storage 
*                                      holding packed bitstream;
*     (I) short dec_scl_fctr;          decimal scale factor to apply to data
*     (O) long  *BDSlength;            updated with #bytes in packed bitstream
*     (O) char  *errmsg                returned filled if error occurred
*
*    RETURN CODE: 
*      0> success, ppbitstream contains packed values
*   else> error: errmsg holds msg;
****************************************************************
*/
#if PROTOTYPE_NEEDED
int	pack_spatial ( long   		*pt_cnt,
			unsigned short	*bit_cnt,
			float		*pack_null,
			float 		*fbuff,
			unsigned long 	**ppbitstream,
			short   	dec_scl_fctr,
			long    	*BDSlength,
			char    	*errmsg)

#else
int	pack_spatial (  pt_cnt, bit_cnt, pack_null, fbuff, ppbitstream,
			dec_scl_fctr, BDSlength, errmsg)
			long   		*pt_cnt;
			unsigned short	*bit_cnt;
			float		*pack_null;
			float 		*fbuff;
			unsigned long  	**ppbitstream;
			short   	dec_scl_fctr;
			long    	*BDSlength;
			char    	*errmsg;
#endif
{
    char *func="pack_spatial";
    long ipt;			/* index over points */
    int null_flag;		/* flag indicating presence of null values */
    int bit1;			/* starting bit in current word */ 
    int empty;			/* number of empty bits in word */ 
    int diff;			/* difference of empty - bit1 */ 
    long max_value;		/* max value storable in bit_cnt bits */
    unsigned long itemp;	/* temporary unsigned integer */
    unsigned long *bstr;	/* pointer running across bitstream */
    int pack_bit_cnt;		/* count of bits to pack parameter values */ 
    int unused_bit_cnt; 	/* count of unused bits for i2 words */
    /*long byte4_cnt;		/- count of bytes using i4 words */
    long byte2_cnt;		/* count of bytes using i2 words */
    short scl_fctr;		/* scaling factor for grid values */
    double pow_scl;		/* 2 ** (-scl_fctr) */
    double pwr10toD;		/* 10 ** (D) */
    float reference;		/* reference = minimum value in grid */
    float max_grid;		/* maximum value in grid */
    float ftemp;		/* temporary float containing grid value */
    unsigned long *pBitstream;
    unsigned long grib_local_ibm();
    int wordnum;
    int zero_cnt;
    int prec_too_high = 0;
    unsigned char bdshdr[14];   /* Character array to temporarily hold bds
                                 *   header */
    int hdrwords;

    DPRINT1 ( "Entering %s....\n", func );

/*
*
* A.1       IF (no data in grid) THEN
*              PRINT message
*              RETURN Stat= -1
*           ENDIF
*/
    if (*pt_cnt <= 0) {
	DPRINT2 ("%s; invalid pt_cnt = %d\n", func,*pt_cnt);
	sprintf(errmsg, "%s; invalid pt_cnt = %d\n", func,*pt_cnt);
	return (-1);
    }

/*
*
* A.2       IF (number of bits to pack into is greater than 30) THEN
*               PRINT message
*               RETURN Stat= -1
*           ENDIF
*           SET pack_bit_cnt for local use
*/
    if ( *bit_cnt > 30 ) {
	DPRINT2 ("%s; invalid bit_cnt = %d\n", func,*bit_cnt);
	sprintf(errmsg, "%s; invalid bit_cnt = %d\n", func,*bit_cnt);
	return (-1);
    }
    pack_bit_cnt	= (int) *bit_cnt; 
    DPRINT1 ("  use Pack_bit_cnt= %d\n", pack_bit_cnt);

/*
*  		
* A.3      FOR (each data point) DO
*              SCALE all values of data array  !multiply by 10**DSF
*          ENDDO
*/
	pwr10toD=  pow ( 10., (double) dec_scl_fctr );
	for (ipt=0; ipt < *pt_cnt; ipt++)  fbuff[ipt] *= pwr10toD;

        DPRINT2 ("  Decimal Scale Fctr= %d, scale data by 10**dsf "\
	"(Fbuff *= %lf)\n", dec_scl_fctr,  pwr10toD);
/*
*
* A.4       INIT reference, max_grid, null_flag 
*/
    reference  	= 1.e30;
    max_grid  	= -1.e30;
    null_flag 	= 0;

/*
*
* A.5       FOR (each data point) DO
*              IF (value < reference) THEN
*                  SET reference to this value   !smallest value
*              ENDIF
*              IF (value > max_grid AND not a missing value ) THEN
*                  SET max_grid to this value    !largest value
*              ENDIF
*              IF (value >= missing value ) THEN
*                  SET null_flag to 1            !grid contains nulls
*              ENDIF
*           ENDDO
  Find reference (minimum) and maximum values of the grid points 
*/
    for (ipt = 0; ipt < *pt_cnt; ipt++) {
	ftemp	= *(fbuff+ipt);
	if (ftemp < reference) reference = ftemp;    /* REF is SCALED UP */
	if (ftemp > max_grid && ftemp < *pack_null) max_grid = ftemp;
	if (ftemp >= *pack_null) null_flag = 1;
    }

    DPRINT2 ("  Max before taking out Ref =%.4lf\n  Null flag=%d\n",
    max_grid, null_flag);

/*  Compute maximum range of grid (max_grid - reference) */
/*
*
* A.6       IF (max value is same as smallest value AND
*               null_flag is zero) THEN
*               CLEAR pack_bit_cnt  !constant values, no nulls
*               CLEAR max_grid      !set grid range to 0
*/
    if (((max_grid - reference) < 1.0) && null_flag == 0) {   
	pack_bit_cnt = 0;
        max_grid =  0;

/*
* A.6.a     ELSE IF (max value is same as smallest value AND
*                    null_flag is set) THEN
*               SET max_grid to 1      !const values, some nulls
*/
    } else if (((max_grid - reference) < 1.0) && null_flag == 1) {
	max_grid = 1.;

/*
* A.6.b     ELSE IF (max value <= -1.e29 AND null_flag is set) THEN
*               PRINT message
*               RETURN Stat= -1
*/
    } else if (max_grid <= -1.e29 && null_flag == 1) {
	DPRINT1 ("%s; Grid contains all NULLS\n",func);
	sprintf(errmsg, "%s; Grid contains all NULLS\n",func);
	return (-1);

/*
* A.6.c     ELSE IF (max value not equal to reference) THEN
*               SET max_grid (max_grid-reference) !non-constant values w/wo nulls
*/
    } else if (max_grid != reference) {
	max_grid -= reference;

/*
* A.6       ENDIF
*/
    }

/*
* 
* A.7       DEBUG print grid range and reference value
*/
    DPRINT2 ( "  Reference = %f\n  Max_grid (range) = %f\n",  
    reference, max_grid);

/*  Find minimum number of bits to pack data */
/*
*
* A.8.a     IF (grid range is not zero) THEN
*/
    if ( max_grid != 0 )    
    {

/*
*
* A.8.a.1      DEBUG print input bit width
*              IF (input bit_num is zero) THEN
*                 CALCULATE number of bits needed to store grid range
*                 DEBUG print calculated bit count
*              ENDIF
*/
       DPRINT1 ( "  Input bit cnt = %d\n", pack_bit_cnt );
       if ( pack_bit_cnt == 0 )
       {
          pack_bit_cnt = (int)(floor(log10((double)max_grid) / log10(2.)) +1); 
          DPRINT1 ( "  Calculated bit cnt = %d\n", pack_bit_cnt );
       }
       if ( (pack_bit_cnt < 0) || (pack_bit_cnt > 30) )
       {
          DPRINT1 ("%s: Calculated bit count OUT OF RANGE [0 - 30] !!\n", func);
          sprintf (errmsg, "%s: Calculated bit count OUT OF RANGE!! bit_cnt: %d  max: %f\n", func,pack_bit_cnt,max_grid);
          return (-1);
       }
/*
*
* A.8.a.2      CALCULATE various byte counters
*              !itemp: #bits required for header + grid
*              !Byte2_cnt: #bytes rounded up to next 2-byte bdry
*              !Byte4_cnt: #bytes rounded up to next 4-byte bdry
*              !Unused_bit_cnt: #unused bits at end using byte2_cnt
*              DEBUG print expected length and unused bits
*/
       itemp = *pt_cnt * pack_bit_cnt + 11 * BYTE_BIT_CNT;
       byte2_cnt = (long) ceil(((double) itemp / BYTE_BIT_CNT) / 2.) * 2;
       /*byte4_cnt = (long) ceil(((double) itemp / BYTE_BIT_CNT) / 4.) * 4;*/
       unused_bit_cnt = byte2_cnt * BYTE_BIT_CNT - itemp;
       DPRINT1 ( "  Calculated length = %ld bytes (Rnd2)\n", byte2_cnt);
       DPRINT1 ( "  Bitstream padding = %ld bits\n",unused_bit_cnt);

/*
*
* A.8.a.3      CALCULATE maximum storable value 
*              CALCULATE scl_fctr required to fit grid range
*                        into available bit width
*/
       max_value = (long) pow(2., (double) pack_bit_cnt) - 1;
       if (max_value < 2) max_value = 2;
       scl_fctr	= -(short) floor(log10((double) (max_value-1) / 
		  	  (double) max_grid) / log10(2.));
       pow_scl 	= pow(2., (double) -scl_fctr); 
       DPRINT1 ( "  Calculated Binary scale = %d\n",scl_fctr);
    }

/*
*
* A.8.b     ELSE       !max_grid = 0, all zero data or constant values
*              SET number of bits to pack to zero
*              SET lengths to 12 bytes
*              SET unused bits to 8 (1 byte of padding)
*              SET scl_fctr to 0
*              DEBUG print constant grid
*           ENDIF
*/
    else
    {
       pack_bit_cnt = 0;
       byte2_cnt = 12;
       /*byte4_cnt = 12;*/
       unused_bit_cnt	= 8;
       scl_fctr	= 0;
       DPRINT0 ( "  Constant grid.  Using bit cnt = 0\n");
    }

/*
*
* A.9       MALLOC space for bitstream (Rnd2_cnt)
*           IF (failed) THEN
*              PRINT error mesg
*              RETURN Stat= 999;
*           ENDIF
*/ 
    pBitstream = ( unsigned long * ) malloc ( sizeof( unsigned long ) * 
					      byte2_cnt );
    if ( !pBitstream )
    {
       DPRINT1 ("%s:  MAlloc failed pBitstream\n", func );
       sprintf(errmsg, "%s:  MAlloc failed pBitstream\n", func );
       return (999);
    }

/*
*
* A.10      SET ptr to bitstream
*           UPDATE bit_cnt for input structure
*/
    *bit_cnt = (unsigned short) pack_bit_cnt;
    DPRINT1 ("  Updated input bit cnt to %d\n", *bit_cnt);
    bstr = pBitstream;
 
/*
*
* A.11      ZERO out entire bitstream
*/
   zero_cnt = ceil(byte2_cnt / (float)sizeof(long)) * sizeof(long);
   memset ((void *)pBitstream, '\0', zero_cnt);

/*
*
* A.12      PUT packing info into first 11 bytes: 
*           NOTE: The Table 11 Flag stored in the first
*                 4 bits of Octet 4 is HARDCODED to 0000.
*                 This implies Simple packing of float
*                 grid point data with no additional flags.
*           Octet 1-3  = Byte2_cnt
*           Octet 4    = Table 11 Flag & unused_bit_cnt
*           Octet 5-6  = Scl_fctr
*           Octet 7-10 = Reference truncated to DSF precision
*           Octet 11   = Pack_bit_cnt
*           Octet 12   = Bitstream starts (bit 25 of word 3)
*/


   set_bytes_u(byte2_cnt, 3, bdshdr);

   itemp = unused_bit_cnt;
   set_bytes_u(itemp, 1, bdshdr+3);

   set_bytes_u(scl_fctr, 2, bdshdr+4);

   DPRINT1 ("  Reference (%f) ", reference);
   reference   =  floor((double) reference + .5);
   DPRINT1 ("truncated to DSF precision= %lf\n", reference);
   itemp        = grib_local_ibm(reference);
   DPRINT1 ("  Reference converted to local IBM format= 0x%x\n", itemp);

   set_bytes_u(itemp, 4, bdshdr+6);

   set_bytes_u(pack_bit_cnt, 1, bdshdr+10);

   bit1 = 25;

   memcpy(bstr,bdshdr,11);

   /* 
    * For non-internet byte order machines (i.e., linux), 
    * We reverse the order of the last byte in the bds header, since
    *   it will be reversed once again below.
    */
   hdrwords = 11/(WORD_BIT_CNT/BYTE_BIT_CNT);
   set_bytes_u(bstr[hdrwords], WORD_BIT_CNT/BYTE_BIT_CNT,
              (char *)(bstr+hdrwords) );

   bstr += hdrwords;


/*
    itemp	= unused_bit_cnt;
    *bstr	= (byte2_cnt << 8) | itemp;
    bstr++;
    *bstr	= scl_fctr;

    DPRINT1 ("  Reference (%f) ", reference);
    reference   =  floor((double) reference + .5);
    DPRINT1 ("truncated to DSF precision= %lf\n", reference);
    itemp	= grib_local_ibm(reference);
    DPRINT1 ("  Reference converted to local IBM format= 0x%x\n", itemp);

    *bstr	= (*bstr << 16) | (itemp >> 16);
    bstr++;
    *bstr	= (itemp << 16) | (pack_bit_cnt << 8);
    bit1	= 25;  	*/	 /* starting bit within current bstr word */

/*
*
* A.13      IF (grid values are not constant) THEN
*/ 
    if (pack_bit_cnt > 0) {


/*
* A.13.1       SET empty value
*/
   	empty = WORD_BIT_CNT - pack_bit_cnt + 1;

	for (ipt=0; ipt < 5; ipt++) DPRINT4 (
	    " ITEMP= (*(fbuff+ipt) - reference) * pow_scl + .5=\n"\
	    "        (%lf -%lf) * %lf + .5 = %lf\n",
	    *(fbuff+ipt), reference, pow_scl, 
	    (*(fbuff+ipt) - reference) * pow_scl + .5);
	   
/*
* A.13.2       FOR (each point in bitstream) DO
*/
	for (ipt = 0; ipt < *pt_cnt; ipt++) {

/*
* A.13.2.1         IF ( data value < pack_null) THEN
*                     SET itemp to (value - reference) * pow_scl + .5;
*                  ELSE
*                     SET itemp to max value;
*                  ENDIF
*/
	    if (*(fbuff+ipt) < *pack_null) {
	    	itemp  = (*(fbuff+ipt) - reference) * pow_scl + .5;
	    } else {
	    	itemp  = max_value; 
		DPRINT1 ("%s: Setting to max_value: Precision may be too high !!\n", func);
		sprintf (errmsg, "%s: Setting grid point to max value, precision may be too high", func);
		/*		return (-1); */
	    }

/*
* A.13.2.2         COMPUTE if data point can fit in current word
*/
	    diff	= empty - bit1;

/*
* A.13.2.3.a       IF (data point falls within word ) THEN
*                      SHIFT value to the correct bit position 
*                      COMBINE it with data in current word of bitstream
*                      CALCULATE starting bit in curr word for next time
*/
	    if (diff > 0)
            {
		*bstr 	|= itemp << diff;
		bit1 	+= pack_bit_cnt;
	    }

/*
* A.13.2.3.b       ELSE IF (data point ends at word boundary) THEN
*                      COMBINE value with data in current word of bitstream
*                      SET starting bit to 1
*                      BUMP word counter in bitstream up by 1 word
*/
	    else if (diff == 0) 
            {
	        *bstr |= itemp;
	        bit1   = 1;
	        bstr++;
	    }

/*
* A.13.2.3.c       ELSE     !point crosses word boundary
*                      STORE "diff" bits of value in current word of bitstream
*                      BUMP word counter in bitstream up by 1 word
*                      STORE remaining bits of value in next word
*                      CALCULATE starting bit in curr word for next time
*                  ENDIF    !word location check
*/
	    else  		/* pixel crosses word boundary */
            {
	        *bstr |= itemp >> -diff;
	        bstr++;
	        *bstr |= itemp << (WORD_BIT_CNT + diff);
	        bit1   = -diff + 1;
    	    }

/*
* A.13.2        ENDFOR loop over grid points
*/
       }

/*
* A.13       ENDIF (pack_bit_cnt > 0)
*/
    }

/* For little endian machines, swap the bytes in the bstr pointer */
    /*    for (wordnum = 0; */
    for (wordnum = hdrwords;
	 wordnum < ceil(byte2_cnt/(float)(WORD_BIT_CNT/BYTE_BIT_CNT)); 
	 wordnum++) {
      set_bytes_u(pBitstream[wordnum], WORD_BIT_CNT/BYTE_BIT_CNT, 
		  (char *)(pBitstream+wordnum) );
    }

/*
*
* A.14       ASSIGN bitstream block to ppbitstream pointer
*            SET BDSlength (size rnded to next 2 byte boundary)
*            RETURN Status 0  ! success
*/
    *ppbitstream = pBitstream;
    *BDSlength = (long) byte2_cnt;

    DPRINT1 ("Exiting pack_spatial, BDS len=%ld,  Status=0\n" , *BDSlength);

    if (prec_too_high) {
	fprintf(stderr,"pack_spatial: Warning: Precision for a parameter may be too high in gribmap.txt\n");
    }
    return (0);
/*
* END OF FUNCTION
*
*/
}

/*
*
**************************************************************
* B. FUNCTION  grib_local_ibm
*      convert local_float from local floating point to
*      IBM floating point stored in a 4-byte integer.
*
*    INTERFACE:
*      unsigned long grib_local_ibm (local_float)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  double local_float      float value in local format
*
*    RETURNS:
*       the actual IBM floating point value
**************************************************************
*
*/
#if PROTOTYPE_NEEDED
unsigned long grib_local_ibm (double local_float)
#else
unsigned long grib_local_ibm (local_float)
double local_float;
#endif
{
    long a, b; 
    unsigned long ibm_float;
/*
*
* B.1.a     IF (local float value is zero) THEN
*               SET the ibm float to zero too
*/
    if (local_float == 0.) { 
        ibm_float = 0;
    } else {
/*
* B.1.b     ELSE
*              CONVERT to IBM floating point
*              ! IBM floating point is stored in 4 bytes as:  
*              ! saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb
*              ! where s is sign bit, 0 -> positive, 1 -> negative
*              !       a is 7-bit characteristic
*              !       b is 24-bit fraction
*              ! s, a and b are obtained from local_float (local 32-bit float) as
*              !       s = sign(local_float)
*              !       a = ceil(log10(local_float) / log10(16.)) + 64 
*              !       b = local_float / 16**(a-64) * 2**24
* B.1.b     ENDIF
*/
        a = ceil(log10(fabs(local_float)) / log10(16.)) + 64;
	/* Added by Todd Hutchinson, 8/13/99 */
	/* This fixes a problem when local_float == 256, etc. */
	if ( fmod((log10(fabs(local_float))/log10(16.)),1.) == 0) {
	  a++;
	}
/*      Local_float == +/-1. is a special case because of log function */
        if ( (local_float == 1.) || (local_float == -1.)) a = 65;
        b = (long) (fabs(local_float) * pow(16.,(double) (70 - a)) +.5)
            & 0x00ffffff; 
        ibm_float = (((local_float > 0.) ? 0 : 1) << 31) | (a << 24) | b;
    }
/*
*
* B.2       RETURN the ibm float value
*/
    return ibm_float;
/*
*
* END OF FUNCTION
*
*/ }

/*
*
**************************************************************
* C. FUNCTION:   grib_ibm_local
*      convert local_float from IBM floating point to
*      local floating point.
*
*    INTERFACE:
*      float grib_ibm_local(ibm_float)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  double local_float      float value in local format
*
*     RETURNS:
*        the actual local floating point
**************************************************************
*/
#if PROTOTYPE_NEEDED
float grib_ibm_local( unsigned long ibm_float)
#else
float grib_ibm_local( ibm_float)
unsigned long ibm_float;
#endif
{
/*
  Convert ibm_float from IBM floating point stored in a 4-byte integer 
  to local floating point.
*
* C.1       DETERMINE local floating point
*           ! IBM floating point is stored in 4 bytes as:  
*           ! saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb
*           ! where s is sign bit, 0 -> positive, 1 -> negative
*           !       a is 7-bit characteristic
*           !       b is 24-bit fraction
*           ! local_float (local 32-bit float) is recovered from 
*           ! s, a and b as
*           ! local_float = (-1)**s * 2**(-24) * b * 16**(a-64)
*/

    long a, b;
    float local_float;

    a = (ibm_float >> 24) & 0x0000007f;
    b = ibm_float & 0x00ffffff;
    local_float = (float) b * pow(16., (double) (a - 70));
    if (ibm_float >> 31) local_float = -local_float;
/*
*
* C.2       RETURN floating point
*/
    return local_float;
/*
* END OF FUNCTION
*
*/ 
}
