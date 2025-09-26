/* FILE:  display_gribhdr.c  		10-OCT-96 by Alice Nakajima/SAIC */
#include <stdio.h>              /* standard I/O header file     */
#include <stdlib.h>

#include "dprints.h"		/* function prototypes */
#include "gribfuncs.h"		/* prototypes */

#define COLS	10		/* # of cols to print per line */
#define HALFWAY	COLS/2		/* half of #cols */

/*
**********************************************************************
* A. FUNCTION:   display_gribhdr
*      do a byte dump for each of the defined GRIB Sections in the 
*      GRIB message currently stored in the Grib Header struct.
*
*    INTERFACE:       
*       void display_gribhdr (gribhdr)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  GRIB_HDR *gribhdr;
*           holds Grib header info to be printed to standard output;
*    
*    RETURNS:  nothing;  
**********************************************************************
*/
#if PROTOTYPE_NEEDED
void display_gribhdr ( GRIB_HDR *hdr)
#else
void display_gribhdr ( hdr)
	GRIB_HDR *hdr;
#endif
{
  char *func="dislay_gribhr";
   unsigned char *ptr, *ptr2;
   long  i, j, cnt, skip;
   char title[200];

   fprintf(stdout, "In %s: showing Grib msg in Grib Hdr:\n", func);
/*
*
* A.1      IF (the entire_msg buffer is NULL) THEN
*              PRINT error
*              RETURN
*          ENDIF
*/
   if (hdr->entire_msg == NULL) {
	fprintf(stdout,"Entire Msg Buffer is Null, cannot proceed;\n");
	goto RETURN;
	}
/*
*
* A.2      IF (sum of section lengths does not equal Total Msg length) THEN
*              PRINT warning
*          ELSE 
*              PRINT msg_length
*          ENDIF
*/
   if (hdr->msg_length != (hdr->ids_len + hdr->pds_len + hdr->gds_len 
			   + hdr->bms_len + hdr->bds_len + hdr->eds_len))
	fprintf(stdout,"\n*******************************************\n"\
	"WARNING: Msg_length=%d but SUM of sect lengths= %d "\
	"(%d+%d+%d+%d+%d+%d);\n*******************************************\n",
   	hdr->msg_length,
        hdr->ids_len+hdr->pds_len+hdr->gds_len+hdr->bms_len+
	hdr->bds_len+hdr->eds_len, 
	hdr->ids_len, hdr->pds_len, hdr->gds_len , hdr->bms_len, 
	hdr->bds_len, hdr->eds_len);
   else  
	fprintf(stdout,"Msg_length=%d;  IDS=%ld,"\
	"PDS=%ld, GDS=%ld, BMS=%ld, BDS=%ld, EDS=%ld;\n",
   	hdr->msg_length,
	hdr->ids_len, hdr->pds_len, hdr->gds_len , hdr->bms_len, 
	hdr->bds_len, hdr->eds_len);

   fprintf(stdout,"Printing each defined section, upto 100 bytes only\n");
/*
*
* A.3      PRINT Identification Defn Section if defined;
*          FUNCTION hdr_print   !dump out its content
*/
   if (hdr->ids_ptr == NULL)  fprintf(stdout,"Section 0 is Null, len=%ld;\n",
	hdr->ids_len);
   else {
	cnt= (hdr->ids_len > 100 ? 100 : hdr->ids_len);
	sprintf(title,"Section 0 Content Len=%ld (upto 100 bytes)", 
	hdr->ids_len);
	hdr_print (title, hdr->ids_ptr, cnt);
	}
	
/*
*
* A.4      PRINT Product Defn Section if defined;
*          FUNCTION hdr_print   !dump out its content
*/
   if (hdr->pds_ptr == NULL)  fprintf(stdout,"Product Data Section is Null, "\
	"len=%ld;\n", hdr->pds_len);
   else {
	cnt= (hdr->pds_len > 100 ? 100 : hdr->pds_len);
	sprintf(title,"PDS Content (offs=%ld, Len=%ld)",
	(long)(hdr->pds_ptr - hdr->entire_msg), hdr->pds_len);
	hdr_print (title, hdr->pds_ptr, cnt);
	}
	    
/*
*
* A.5      PRINT Grid Defn Section if defined;
*          FUNCTION hdr_print   !dump out its content
*/
   if (hdr->gds_ptr == NULL)  fprintf(stdout,"Grid Defn Section is Null, "\
	"len=%ld;\n", hdr->gds_len);
   else {
	cnt= (hdr->gds_len > 100 ? 100 : hdr->gds_len);
	sprintf(title,"GDS Content (offs=%ld, Len=%ld)",
	(long)(hdr->gds_ptr - hdr->entire_msg), hdr->gds_len);
	hdr_print (title, hdr->gds_ptr, cnt);
	}
	
/*
*
* A.6      PRINT Bitmap Data Section if defined;
*          FUNCTION hdr_print   !dump out its content upto 100 bytes
*/
   if (hdr->bms_ptr == NULL)  fprintf(stdout,"Bitmap Section is Null, "\
	"len=%ld;\n", hdr->bms_len);
   else {
	cnt= (hdr->bms_len > 100 ? 100 : hdr->bms_len);
	sprintf(title,"BMS Content (offs=%ld, Len=%ld)",
	(long)(hdr->bms_ptr - hdr->entire_msg), hdr->bms_len);
	hdr_print (title, hdr->bms_ptr, cnt);
	}
	
/*
*
* A.7      PRINT Binary Defn Section if defined;
*          FUNCTION hdr_print   !dump out its content upto 100 bytes
*/
   if (hdr->bds_ptr == NULL)  fprintf(stdout,"Binary Data Section is Null, "\
	"len=%ld;\n", hdr->bds_len);
   else {
	cnt= (hdr->bds_len > 100 ? 100 : hdr->bds_len);
	sprintf(title,"BDS Content (offs=%ld, Len=%ld)",
	(long)(hdr->bds_ptr - hdr->entire_msg), hdr->bds_len);
	hdr_print (title, hdr->bds_ptr, cnt);
	}
	
/*
*
* A.8      PRINT End Defn Section if defined;
*          FUNCTION hdr_print   !dump out its content
*/
   if (hdr->eds_ptr == NULL)  fprintf(stdout,"End Data Section is Null, "\
	"len=%ld;\n", hdr->eds_len);
   else {
        cnt= (hdr->eds_len > 100 ? 100 : hdr->eds_len);
	sprintf(title, "End Data Section (offs=%ld, size=%ld) ",
	(long) (hdr->eds_ptr - hdr->entire_msg), hdr->eds_len); 
	hdr_print (title, hdr->eds_ptr, cnt);
	}

RETURN:
/*
*
* A.9      RETURN to caller  !return nothing
*/
   fprintf(stdout, "%s complete;\n", func);
   return;

/*
*
* END OF FUNCTION
*
*
*/
}
