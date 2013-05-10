#include <stdio.h>
#include <stdlib.h>

#include "dprints.h"		/* debug prints */
#include "gribfuncs.h"		/* prototypes */

/*
*
*********************************************************************
* A. FUNCTION:  apply_bitmap
*       apply the bitmap to the float array.  The input float array is
*       expanded and filled with 'fill_value' in places where data
*       points are missing.
*
*    INTERFACE:
*      int  apply_bitmap (bms, pgrib_data, fill_value, bds_head, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  BMS_INPUT *bms;
*           pointer to the internal BitMap header structure;  bit set means
*           datapoint is present, bit clear means datapoint is missing.
*     (I&O) float **pgrib_data;  
*           pointer to Data that was unpacked from BDS's bitstr;  Incoming
*           size is bms->ulbits_set or (ROW*COL - #missingpts) elements;
*      (I)  float fill_value;    
*           float value used for missing datapoints in expanded array;
*      (O)  BDS_HEAD_INPUT *bds_head; 
*           attribute 'ulGrid_size' to be updated;
*      (O)  char *errmsg;
*           Empty array that's returned filled if error occurred; 
*
*    RETURN CODE:
*      0>  Success; float **pgrib_data probably have been expanded, OR
*          Predefined bitmap used, no action taken (float array unchanged);
*      1>  NULL bitmap encountered, errmsg filled;
*      2>  Error Mallocing space for data array, errmsg filled;
*      3>  Tried to access more than available in message, errmsg filled;
*      4>  No bits set in BMS, errmsg filled;
**********************************************************************
*/ 
#if PROTOTYPE_NEEDED

int apply_bitmap ( BMS_INPUT *bms, float **pgrib_data, float fill_value,
		BDS_HEAD_INPUT *bds_head, char *errmsg)
#else

int apply_bitmap ( bms, pgrib_data, fill_value, bds_head, errmsg)
		BMS_INPUT *bms; 
		float **pgrib_data; 
		float fill_value;
		BDS_HEAD_INPUT *bds_head; 
		char *errmsg;

#endif
{
  char  *func= "apply_bitmap";
  int   i,j;			/* temp var */
  int   val;			/* temp var */
  int	buf_indx;		/* index for expanded float *buff array */
  int   gribdata_indx;		/* index for float *Grid_data array */
  int	tot_bits_set;		/* must be < expanded size */
  char  *pbms; 			/* BIT ptr beg. at BMS data array */
  float	*fbuff;		 	/* holds expanded float array */
  int	 gridsize;		/* expanded size r*c */

/*
* 
* A.0     DEBUG printing
*/
  DPRINT1 ("Enter %s()\n", func);

/*
*
* A.1     IF (using pre-defined bitmap)
*            FILL errmsg ! 'case not supported' 
*            RETURN 0  !success
*         ENDIF
*/
  if (bms->uslength == 6) /* References pre-defined bitmap */
    {
      /* Not currently supported.  User can add code inside this IF
       * to retreive the bitmap from local storage if available.
       * For now, code prints warning and leaves data array alone */
      fprintf(stdout,
		"\n%s Warning: Predefined bitmap encountered! Not supported; " \
      		"Must apply bitmap to data externally.\n", func);
      DPRINT1("Leaving %s: Predefined bitmap used, no action taken\n",func);
      return(0);
    }
           
/*
*
* A.2     IF (Bitmap pointer is NULL)
*            FILL errmsg  !null pointer
*            RETURN 1   
*         ENDIF
*/
  if (bms->bit_map==NULL) {
	DPRINT1 ("Leaving %s:  bitmap is Null, no action taken\n", func);
	return(1); 
	}

/*
*
* A.3     IF (count of bits set in BMS is Zero)
*            FILL errmsg
*            RETURN 4   !no bits set
*         ENDIF
*/
   if ((tot_bits_set=bms->ulbits_set) == 0) {
       sprintf(errmsg,"%s: No bits set in bitmap.  No data retrieved!!\n",func); 
       DPRINT1("Leaving %s: No bits set in bitmap\n",func); 
       return(4);
   }

/*
*
* A.4     CALCULATE grid_size from total number of bits in BMS;
*/
  /* = (BMS length)*8 bits - 48 header bits - # of unsused bits */
  gridsize=(bms->uslength)*8 - 48 - bms->usUnused_bits;

  DPRINT2 ("Apply Bitmap: expanding array from [%d] to [%d]; ",
  tot_bits_set, gridsize);

/* 
*
* A.5     ALLOCATE storage for expanded array 
*         IF (Malloc error) 
*            RETURN 2 
*         ENDIF
*/
  fbuff= (float *)malloc (gridsize * sizeof(float));
  if (fbuff==(float *)NULL)
	{ 
          sprintf(errmsg, "%s: Error mallocing %ld bytes\n", func,gridsize);
          DPRINT1 ("Leaving %s, malloc error\n",func);
          return(2); 
	}

/*
*
* A.6     FOR (each point expected)
*/
  pbms= bms->bit_map;	/* pts to char arry bstr of BMS */
  gribdata_indx=0;	/* index for incoming float arr */
  for (buf_indx=0; buf_indx < gridsize; ++pbms) {

/*
* A.6.1      GET next byte from Bitmap Section Data 
*/
	val= (int)*pbms & 0x0000ff ;	/* BMS bitstream */

/*
* A.6.2      LOOP, check each Bit of byte read (left to rightmost)
*/
	for (j=7; j>=0 && buf_indx < gridsize; j--) {
/*
* A.6.2.1       IF (bit is set)  !means datapoint is present
*/
	    if (val & (1<<j))
		{  
/*
* A.6.2.1.1         IF (trying to access more than #elements in Incoming Array)
*                      FILL errmsg
*                      RETURN 3    ! incoming float array unchanged
*                   ENDIF
*/
		   if (gribdata_indx == tot_bits_set) {
		     sprintf(errmsg,
                    "%s:  accessing more than %d elements in Grib_data[]\n",
			func,	tot_bits_set);
		     DPRINT1("Leaving %s, access out of range element\n",func);
		     return(3);  /* incoming Float array is unchanged */
		     }

/*
* A.6.2.1.2         !still within range
*                   STORE datapoint at correct place in expanded array
*/
		   fbuff[buf_indx++]= (*pgrib_data)[gribdata_indx++];
		}
/*
*                ELSE  ! means data point is missing
*/
	    else {
/*
* A.6.2.2           STORE Missing Value at correct place in expanded array
*/
		   fbuff[buf_indx++]= fill_value;
		}
/*
* A.6.2.1        ENDIF
*/
         }  /* bit loop */
/*
* A.6.2      ENDLOOP 
*/

    } /* for every datapt */
/*
* A.6     ENDFOR !for every datapoint
*/

/*
*
* A.7     STORE the grid size into caller's argument
*/
  bds_head->ulGrid_size= (unsigned long)gridsize;  /* store new sz */

/*
*
* A.8      FREE old float array 
*/
  free (*pgrib_data);

/*
*
* A.9      ASSIGN new expanded array to pointer
*/
  *pgrib_data= fbuff;	/* give it addr of expanded arr */
/*
*
* A.10     RETURN 0 !success
*/
  DPRINT1("Leaving %s, Stat=0", func);
  return (0); 
/*
*
* END OF FUNCTION
*
*
*/
}
