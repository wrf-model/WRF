/*  gribgetbms.c 		June 17, 1996 by Alice Nakajima, SAIC   */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*
************************************************************************
* A.  FUNCTION:   gribgetbms
*       decode the Bitmap Section from the provided pointer location
*       and store its info in the internal BMS structure.
*       Pre-defined Bitmap case is not supported.
*               
*    INTERFACE:
*       int   gribgetbms ( curr_ptr, bms, gds_flag, ulGrid_size, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)   char *curr_ptr;
*            pointer to location where Bitmap Section to decode is expected;
*      (O)   BMS_INPUT *bms;
*            pointer to empty BMS structure; will hold decoded BMS info;
*      (I)   int gds_flag;      
*            flag set if GDS is present
*      (I)   unsigned long ulGrid_size;  
*            size of grid as in Binary Data Section struct
*      (O)   char *errmsg
*            returned filled if error occurred;
* 
*     RETURN CODE:
*     0>  BMS info stored in BMS structure if not using pre-defined bitmap;
*     1>  error, corrupted bms; msg in errmsg;
************************************************************************
*/

#if PROTOTYPE_NEEDED
int   gribgetbms ( char *curr_ptr, BMS_INPUT *bms, int gds_flag,
		unsigned long ulGrid_size, char *errmsg)
#else
int   gribgetbms (curr_ptr, bms, gds_flag, ulGrid_size, errmsg)
		char *curr_ptr; 
		BMS_INPUT *bms; 
		int gds_flag;
		unsigned long ulGrid_size; 
		char *errmsg;
#endif
{
  char *func= "gribgetbms";
  char  *pp;
  int totbits,val, bitpos,stopbit;  /* tmp working vars */
  unsigned long SectLength;       /* message and section size */
  unsigned long ulvar;		    /* tmp var */
  unsigned long skip=0;

/*
*
* A.0      INIT Status to no error
*/
int   status=0;

     DPRINT0 ("Entering gribgetbms():\n");
/* 
*
* A.1      FUNCTION gbyte   !get bitmap length
*/
    skip=0;
    gbyte(curr_ptr,(unsigned long *)&SectLength,&skip,24); 
    DPRINT0 ("SectLength\n");
    bms->uslength= (unsigned long) SectLength;

/* 
*
* A.2      FUNCTION gbyte   !get number of unused bits
*/
    gbyte(curr_ptr,&ulvar,&skip,8); 
    DPRINT0 ("bms->usUnused_bits\n");
    bms->usUnused_bits= (unsigned short) ulvar;

/* 
*
* A.3      FUNCTION gbyte   !get bitmap id (non-zero for a pre-defined bitmap)
*/
    gbyte(curr_ptr,&ulvar,&skip,16); 
    DPRINT0 ("bms->usBMS_id\n");
    bms->usBMS_id= (unsigned short) ulvar;

/*
*
* A.4      IF (Bitmap follows)   !not a predefined bitmap
*/
    if ( bms->uslength > 6)     /* Bitmap follows */
       {

/* 
* A.4.1       CALCULATE Num of bits in bitmap
*/
         /* = (BMS length)*8 bits - 48 header bits - # of unsused bits */
         totbits=SectLength*8 - 48 - bms->usUnused_bits;

/*
* A.4.2       IF (GDS is present AND 
*                      #bits differs from Grid Size)        !Corrupted BMS
*                 RETURN 1
*             ENDIF
*/
          if (gds_flag && totbits != ulGrid_size) {
		DPRINT3( "%s: corrupted BMS, gds_flag set but "\
		"totbits %d != ulgrid_sz %d\n" 
		, func, totbits, ulGrid_size);

		sprintf(errmsg, "%s: corrupted BMS, gds_flag set but "\
		"totbits %d != ulgrid_sz %d\n" , func, totbits, ulGrid_size);
		status=  (1); /* Corrupted BMS */
	 	goto BYE;
		}

/*
* A.4.3       ASSIGN bitmap pointer to 6th byte of BMS
*/
          bms->bit_map =  curr_ptr + 6;
          pp= bms->bit_map; 
          bms->ulbits_set= 0; 
/* 
*
* A.4.4       !SUM up total number of bits set
*             FOR (Each 8-bit block of Total Bits Present in BMS)
*/
          for ( ; totbits > 0 ; totbits-=8) 
          {
/*
* A.4.4.1       IF (any of the 8 bits are set) 
*/
             if ((val=(int)*pp++) != 0) 
               {

/*
* A.4.4.1.1        IF (not within 8 bits of end of bitmap)
*                      SET stopbit to 0
*                  ELSE
*                      SET stopbit to end of bitmap
*                  ENDIF
*/
                 if (totbits > 8) stopbit=0;   /* check all 8 bits */
                 else stopbit= 7-totbits+1;    /* stop at end of bitmap */

/*
* A.4.4.1.2        SUM up number of bits set in this BMS byte
*/
                 for (bitpos= 7; bitpos >= stopbit; bitpos--)
                    if (val >> bitpos & 0x0001) bms->ulbits_set += 1;
/*
* A.4.4.1       ENDIF  ! any of 8 exists
*/
               }
/*
* A.4.4     ENDFOR   !each 8-bit loop
*/
           }
/*
* A.4      ENDIF	!Bitmap follows
*/
	}  

/*  else {
        / * Predefined Bitmap - not supported!! Could add function here 
	    to load a bitmap from local storage * /
         bms->uslength=6;	
         bms->bit_map= Load_predefined_bms (bms->usBMS_id);
       }
*/


BYE:
/*
*
* A.5     DEBUG Print
*
* A.6     RETURN Status 
*/
    DPRINT2 ("Exiting %s, Status=%d;\n", func, status);
    return (status);
/*
* END OF FUNCTION
*/ 
}
