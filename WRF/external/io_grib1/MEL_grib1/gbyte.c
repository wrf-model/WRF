/* gbyte.c:
  ADAPTED FROM THE ORIGINAL FORTRAN VERSION OF GBYTE BY:
 
              DR. ROBERT C. GAMMILL, CONSULTANT
              NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
              MAY 1972
 
              CHANGES FOR FORTRAN 90
              AUGUST 1990  RUSSELL E. JONES
              NATIONAL WEATHER SERVICE
              GBYTE RUN WITHOUT CHANGES ON THE FOLLOWING COMPILERS
              MICROSOFT FORTRAN 5.0 OPTIMIZING COMPILER
              SVS 32 386 FORTRAN 77 VERSION V2.8.1B
              SUN FORTRAN 1.3, 1.4
              DEC VAX FORTRAN
              SILICONGRAPHICS 3.3, 3.4 FORTRAN 77
              IBM370 VS COMPILER
              INTERGRAPH GREEN HILLS FORTRAN CLIPPER 1.8.4B
*/
#include <stdio.h>
#include <stdlib.h>

#include "dprints.h"	/* debug prints & func prototypes */
#include "gribfuncs.h"		/* prototypes */
#include "isdb.h"		/* WORD_BIT_CNT defn */

/* Added by Todd Hutchinson, 8/10/05*/
/* 
 * gbyte requires the word bit count to be 32.  In order for this to work
 *    on platforms with 8 byte longs, we must set WORD_BIT_CNT to 32 for
 *    gbyte.
 */

#ifdef WORD_BIT_CNT
#undef WORD_BIT_CNT
#endif
#define WORD_BIT_CNT 32  /* gbyte.c requires the word bit count to be 32! */

/*
*
*****************************************************************
* A.  FUNCTION:   gbyte
*       extracts data of specified length from the specified offset
*       from beginning of the given Data block.
*
*    INTERFACE:
*      void   gbyte (inchar, iout, iskip, nbits)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) char *inchar;
*          The fullword in memory from which unpacking is to
*          begin, successive fullwords will be fetched as required.
*      (O) unsigned long *iout;
*          The value read from in memory that's returned.
*    (I&O) unsigned long  *iskip;
*          a fullword integer specifying the inital offset
*          in bits of the first byte, counted from the
*          leftmost bit in Inchar.  Gets updated upon exit;
*      (I) unsigned long nbits;
*          a fullword integer specifying the number of bits
*          in each byte to be unpacked.  Legal byte widths
*          are in the range 1 - 32, bytes of width  less than 32
*          will be right justified in the low-order positions
*          of the unpacked fullwords with high-order zero fill.
*
*    RETURN CODE:  none;
*****************************************************************
*
*/

#if PROTOTYPE_NEEDED
void 	gbyte (char *inchar, unsigned long *iout, unsigned long *iskip,
		unsigned long nbits)
#else
void gbyte (inchar, iout, iskip, nbits)
	  char *inchar;         /* input */
	  unsigned long *iout;  /* output, is the value returned */
	  unsigned long *iskip; /* input, gets updated */
	  unsigned long nbits;  /* input */
#endif
{
   long masks[32];
   long	icon,index,ii,mover,movel;
   unsigned long temp, mask, inlong;


/*
* A.1      INITIALIZE mask possibilities of all bits set from LSB to
*          a particular bit position;  !bit position range: 0 to 31
*/
   masks[0] = 1; 
   masks[1] = 3;
   masks[2] = 7;
   masks[3] = 15; 
   masks[4] = 31;
   masks[5] = 63;
   masks[6] = 127;
   masks[7] = 255;
   masks[8] = 511;
   masks[9] = 1023;
   masks[10] = 2047;
   masks[11] = 4095;
   masks[12] = 8191;
   masks[13] = 16383;
   masks[14] = 32767;
   masks[15] = 65535;
   masks[16] = 131071;
   masks[17] = 262143;
   masks[18] = 524287;
   masks[19] = 1048575; 
   masks[20] = 2097151; 
   masks[21] = 4194303;
   masks[22] = 8388607; 
   masks[23] = 16777215;
   masks[24] = 33554431;
   masks[25] = 67108863;
   masks[26] = 134217727;
   masks[27] = 268435455; 
   masks[28] = 536870911; 
   masks[29] = 1073741823;
   masks[30] = 2147483647;
   masks[31] = -1;

/* NBYTE MUST BE LESS THAN OR EQUAL TO WORD_BIT_CNT

*
* A.2      IF (trying to retrieve more than numbits_perword) THEN  !here, 32
*              RETURN
*          ENDIF
*/
   icon = WORD_BIT_CNT - nbits;
   if ( icon < 0 )
   {
      return;
   }
/*
*
* A.3      SET up mask needed for specified #bits to retrieve
*/
   mask = masks[nbits-1];
/*
*
* A.4      CALCULATE Index !Byte offset from 'inchar' where retrieval begins
*/
   index = *iskip / WORD_BIT_CNT;
/*
*
* A.5      CALCULATE Bit position within byte Index where retrieval begins
*/
   ii = *iskip % WORD_BIT_CNT;

/*
*
* A.6      CALCULATE #times to Right-shift the retrieved data so it 
*          is right adjusted
*/
   mover = icon - ii;

/*
*
* A.7.a    IF (need to right-adjust the byte) THEN
*/
   if ( mover > 0 )
   {

/*
* A.7.a.1     RETRIEVE 4 continuous byte from offset Index in block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)inchar[index*4] << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* A.7.a.2     RIGHT adjust this value
*/
     *iout = inlong >> mover;
/*
* A.7.a.3     MASK out the bits wanted only    !result in *out
*/
     *iout = (*iout & mask);
   } /* If */


/*
* A.7.b    ELSE IF (byte is split across a word break) THEN
*/
   else if ( mover < 0 )
   {
/*
*             !
*             !Get the valid bits out of the FIRST WORD
*             !
* A.7.b.1     CALCULATE #times to move retrieve data left so
*             the 1st significant bit aligns with MSB of word
* A.7.b.2     CALCULATE #times to move data that's aligned 
*             with MSB so that it aligns with LSB of word
*/
      movel = -mover;
      mover = WORD_BIT_CNT - movel;   /* WORD_BIT_CNT is 32 */

/*
* A.7.b.3     RETRIEVE 4-byte word from offset Index from block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* A.7.b.4     SHIFT retrieve this data all the way left !Left portion
*/

/*
*             !
*             !Now Get the valid bits out of the SECOND WORD
*             !
* A.7.b.5     RETRIEVE the next 4-byte word from block
*/
      *iout = inlong << movel;
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4+4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+5 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+6 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+7 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/* 
* A.7.b.6     SHIFT this data all the way right   !Right portion
* A.7.b.7     OR the Left portion and Right portion together
* A.7.b.8     MASK out the #bits wanted only     !result in *iout
*/
      temp  = inlong >> mover;
      *iout = *iout|temp;
      *iout &= mask;
/*
  THE BYTE IS ALREADY RIGHT ADJUSTED.
*/
   }
   else
/*
* A.7.c    ELSE    !the byte is already adjusted, no shifts needed
*/
   {
/*
* A.7.c.1     RETRIEVE the next 4-byte word from block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* A.7.c.2     MASK out the bits wanted only    !result in *out
*/
      *iout = inlong&mask;
   }
/*
* A.7.c    ENDIF    !the byte is already adjusted
*/

/*
*
* A.8      DEBUG printing
*/
  DPRINT3 ("gbyte(skip=%d %d bits)= %lu stored as ", *iskip, nbits, *iout);
/*
*
* A.9      BUMP pointer up
*/
	*iskip += nbits;
/* 
* END OF FUNCTION
*
*
*/
}

/*
*
*****************************************************************
* B.  FUNCTION:   gbyte_quiet
*       called to extract data of specified length from
*       specified offset from a block of type char;  
*       Identical to gbyte() except it does not print out in debug mode;
*
*    INTERFACE:
*      void gbyte_quiet (inchar, iout, iskip, nbits)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) char *inchar
*          The fullword in memory from which unpacking is to
*          begin, successive fullwords will be fetched as required.
*      (O) unsigned long *iout
*          The value read from memory that's being returned.
*    (I&O) unsigned long  *iskip
*          a fullword integer specifying the inital offset
*          in bits of the first byte, counted from the
*          leftmost bit in Inchar.  Gets updated upon exit;
*      (I) unsigned long nbits
*          a fullword integer specifying the number of bits
*          in each byte to be unpacked.  Legal byte widths
*          are in the range 1 - 32, bytes of width  less than 32
*          will be right justified in the low-order positions
*          of the unpacked fullwords with high-order zero fill.
*
*    RETURN CODE:  none;
*****************************************************************
*
*/

#if PROTOTYPE_NEEDED
void gbyte_quiet (char *inchar, unsigned long *iout, unsigned long *iskip,
		unsigned long nbits)
#else
void gbyte_quiet (inchar, iout, iskip, nbits)
	  char *inchar;         /* input */
	  unsigned long *iout;  /* output, is the value returned */
	  unsigned long *iskip; /* input, gets updated */
	  unsigned long nbits;  /* input */

#endif
{
   long masks[32];
   long	icon,index,ii,mover,movel;
   unsigned long temp, mask, inlong;


/*
* B.1      INITIALIZE mask possibilities of all bits set from LSB to
*          a particular bit position;  !bit position range: 0 to 31
*/
   masks[0] = 1; 
   masks[1] = 3;
   masks[2] = 7;
   masks[3] = 15; 
   masks[4] = 31;
   masks[5] = 63;
   masks[6] = 127;
   masks[7] = 255;
   masks[8] = 511;
   masks[9] = 1023;
   masks[10] = 2047;
   masks[11] = 4095;
   masks[12] = 8191;
   masks[13] = 16383;
   masks[14] = 32767;
   masks[15] = 65535;
   masks[16] = 131071;
   masks[17] = 262143;
   masks[18] = 524287;
   masks[19] = 1048575; 
   masks[20] = 2097151; 
   masks[21] = 4194303;
   masks[22] = 8388607; 
   masks[23] = 16777215;
   masks[24] = 33554431;
   masks[25] = 67108863;
   masks[26] = 134217727;
   masks[27] = 268435455; 
   masks[28] = 536870911; 
   masks[29] = 1073741823;
   masks[30] = 2147483647;
   masks[31] = -1;

/* NBYTE MUST BE LESS THAN OR EQUAL TO WORD_BIT_CNT

*
* B.2      IF (trying to retrieve more than numbits_perword) THEN  !here, 32
*              RETURN
*          ENDIF
*/
   icon = WORD_BIT_CNT - nbits;
   if ( icon < 0 )
   {
      return;
   }
/*
*
* B.3      SET up mask needed for specified #bits to retrieve
*/
   mask = masks[nbits-1];
/*
*
* B.4      CALCULATE Index !Byte offset from 'inchar' where retrieval begins
*/
   index = *iskip / WORD_BIT_CNT;
/*
*
* B.5      CALCULATE Bit position within byte Index where retrieval begins
*/
   ii = *iskip % WORD_BIT_CNT;

/*
*
* B.6      CALCULATE #times to Right-shift the retrieved data so it 
*          is right adjusted
*/
   mover = icon - ii;

/*
*
* B.7.a    IF (need to right-adjust the byte) THEN
*/
   if ( mover > 0 )
   {

/*
* B.7.a.1     RETRIEVE 4 continuous byte from offset Index in block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)inchar[index*4] << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* B.7.a.2     RIGHT adjust this value
*/
     *iout = inlong >> mover;
/*
* B.7.a.3     MASK out the bits wanted only    !result in *out
*/
     *iout = (*iout & mask);
   } /* If */


/*
* B.7.b    ELSE IF (byte is split across a word break) THEN
*/
   else if ( mover < 0 )
   {
/*
*             !
*             !Get the valid bits out of the FIRST WORD
*             !
* B.7.b.1     CALCULATE #times to move retrieve data left so
*             the 1st significant bit aligns with MSB of word
* B.7.b.2     CALCULATE #times to move data that's aligned 
*             with MSB so that it aligns with LSB of word
*/
      movel = -mover;
      mover = WORD_BIT_CNT - movel;   /* WORD_BIT_CNT is 32 */

/*
* B.7.b.3     RETRIEVE 4-byte word from offset Index from block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* B.7.b.4     SHIFT retrieve this data all the way left !Left portion
*/

/*
*             !
*             !Now Get the valid bits out of the SECOND WORD
*             !
* B.7.b.5     RETRIEVE the next 4-byte word from block
*/
      *iout = inlong << movel;
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4+4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+5 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+6 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+7 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/* 
* B.7.b.6     SHIFT this data all the way right   !Right portion
* B.7.b.7     OR the Left portion and Right portion together
* B.7.b.8     MASK out the #bits wanted only     !result in *iout
*/
      temp  = inlong >> mover;
      *iout = *iout|temp;
      *iout &= mask;
/*
  THE BYTE IS ALREADY RIGHT ADJUSTED.
*/
   }
   else
/*
* B.7.c    ELSE    !the byte is already adjusted, no shifts needed
*/
   {
/*
* B.7.c.1     RETRIEVE the next 4-byte word from block
*/
     {
       unsigned long l0, l1, l2, l3;
       l0 = (unsigned long)(0x000000FF & inchar[index*4]) << 24;
       l1 = (unsigned long)(0x000000FF & inchar[index*4+1 ]) << 16;
       l2 = (unsigned long)(0x000000FF & inchar[index*4+2 ]) << 8;
       l3 = (unsigned long)(0x000000FF & inchar[index*4+3 ]);
       inlong = l0 + l1 + l2 + l3;
     }
/*
* B.7.c.2     MASK out the bits wanted only    !result in *out
*/
      *iout = inlong&mask;
   }
/*
* B.7.c    ENDIF    !the byte is already adjusted
*/

/*
*
* B.8      BUMP pointer up
*/
	*iskip += nbits;
/* 
* END OF FUNCTION
*
*/
}
