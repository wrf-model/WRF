/* FILENAME:   init_dec_struct.c 
   DATE:         05 FEB 1996
   PROGRAMMER:   STEVE LOWE, SAIC
   Revisions:
   17apr96 Alice Nakajima, SAIC: added BMS initialization
   11jun96 Nakajima: replaced with Memset
   10oct96 Nakajima: renamed from init_struct() to init_dec_struct()
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dprints.h"	/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*
*
*************************************************************************
* A. FUNCTION:  init_dec_struct
*       initializes the four internal Decoder structures 
*    
*    INTERFACE:
*       void init_dec_struct ( pds, gds, bms, bds_head)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (O)  PDS_INPUT      *pds;      internal PDS struct to be initialized
*      (O)  grid_desc_sec  *gds;      internal GDS struct to be initialized
*      (O)  BMS_INPUT      *bms;      internal BMS struct to be initialized
*      (O)  BDS_HEAD_INPUT *bds_head; internal BDS struct to be initialized
*
*    RETURN CODE:  none
*************************************************************************
*/
#if PROTOTYPE_NEEDED
void init_dec_struct (  PDS_INPUT      *pds, grid_desc_sec  *gds,
			BMS_INPUT      *bms, BDS_HEAD_INPUT *bds_head)
#else
void init_dec_struct (pds,gds,bms,bds_head)
        PDS_INPUT      *pds;
        grid_desc_sec  *gds;
        BMS_INPUT      *bms;
        BDS_HEAD_INPUT *bds_head;
#endif
{
/* 
*
* A.0       DEBUG printing
*/
  DPRINT0 ("Inside init_dec_struct()\n");

/* 
*
* A.1       INITIALIZE Product Description Section struct elements
*/
  memset ((void *)pds, '\0', sizeof(PDS_INPUT)); 

/* 
*
* A.2       INITIALIZE Grid Description Section struct elements 
*
* A.3       INITIALIZE Bitmap Map Section  header struct elements to zero
*
* A.4       INITIALIZE Binary Data Section Header Struct elements  to zero
*/
  memset ((void *)gds, '\0', sizeof(grid_desc_sec)); 
  gds->head.usData_type = 255;
  memset ((void *)bms, '\0', sizeof(BMS_INPUT));
  memset ((void *)bds_head, '\0', sizeof(BDS_HEAD_INPUT));

/* 
*
* A.5       DEBUG printing
*/
  DPRINT0("Leaving init_dec_struct(), no return code\n");
/*
* END OF FUNCTION
*/
}
