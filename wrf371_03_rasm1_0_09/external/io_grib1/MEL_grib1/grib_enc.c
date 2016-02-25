/*
 REVISIONS:
 10/15/96 A. Nakajima, SAIC:  removed 'write_grib' call; make combined lib;
 11/03/97 /ATN  -Realloc
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
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
#include "grib_lookup.h"	/* parm/model/lvl defn */
#include "gribfuncs.h"		/* prototypes */

/*
*
****************************************************************************
* A.  FUNCTION:  grib_enc
*        to encode a GRIB Edition 1 message using the three
*        input internal structures (DATA_INPUT, USER_INPUT, GEOM_IN),
*        and the Floating point data array;  
*        It's ok for Float array to be null if Grib Hdr shows that
*        it contains a predefined BDS;  that case, just exits w/ no errs;
*
*    INTERFACE:
*      int grib_enc (Data_Input, User_Input, Geom_In, pfData_Array, gh, errmsg)
*      
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  DATA_INPUT Data_Input;
*           Structure containing input field information.  
*      (I)  USER_INPUT User_Input;
*           Structure containing encoder configuration data.
*      (I)  GEOM_IN Geom_In;
*           Structure containing grid geometry description.
*    (I&O)  float *pfData_Array;
*           array of float data to be packed and stored in the Binary Data 
*           Section.  Float array may be Null if the Grib Header already
*           contains a Binary Data Section in its attribute 'entire_msg'.
*           That case is referred to as the 'Shuffle Mode' which results
*           in the encoder to only create the sections which are not already
*           in entire_msg;  
*           Note: non-null data array will be returned with the data being 
*           scaled up by the Decimal Scale Factor.
*    (I&O)  GRIB_HDR *gh;
*           Pre-malloced structure used to hold the encoded GRIB message 
*           and its info.  It contains a large array to hold the encoded
*           message, pointers to each of the Section along with their length,
*           and a flag 'shuffled' which determines how the message is encoded.
*           If 'shuffled' is zero upon entry, all 6 sections will be created 
*           and array (float *pfData_Array) must contain the float data.
*           If 'shuffled' is set upon entry, there is already one or more
*           sections in Entire_msg;  Each of these pre-included sections
*           sections will have a Non-Null pointer & a non-Zero length. 
*           The encoder will then only create the missing sections and
*           append them at the end of the existing sections in array
*           'entire_msg', hence these sections may not be in the proper 
*           order expected by GRIB.
*      (O)  char *errmsg
*           Empty array, returned filled if error occurred;
*
*    RETURN VALUE:  
*       0>  no errors;   
*           GRIB_HDR is returned with the encoded message in 'entire_msg',
*           w/ total message length in msg_length,
*           w/ pointers to each defined Grib Header Sections in
*           ids_ptr, pds_ptr, gds_ptr, bms_ptr, gds_ptr, eds_ptr,
*           and each section length in ids_len, pds_len, gds_len, bms_len,
*           bds_len, eds_len;  Note that the sections may not be in order if
*           the 'shuffled' bit is set;
*       1>  failed,  msg in errmsg;
****************************************************************************/
#if PROTOTYPE_NEEDED
int grib_enc   (DATA_INPUT Data_Input, USER_INPUT User_Input, GEOM_IN Geom_In,
        	float *pfData_Array, GRIB_HDR *gh, char *errmsg)
#else
int grib_enc   (Data_Input, User_Input, Geom_In, pfData_Array, gh, errmsg)
		DATA_INPUT Data_Input; 
		USER_INPUT User_Input; 
		GEOM_IN Geom_In;
        	float *pfData_Array; 
		GRIB_HDR *gh; 
		char *errmsg;
#endif
{
   PDS_INPUT 		*pPDS_Input= 0;		/* internal Pds struc */
   GDS_HEAD_INPUT       *pGDS_Head_Input = 0;   /* Internal Gds struc */
   void                 *pvGDS_Proj_Input = 0;	/* depends on Projection*/
   BDS_HEAD_INPUT       *pBDS_Head_Input = 0;   /* Internal Bds struc */
   char			*func= "grib_enc";
   char 		*Sevens= "7777";
   int                  gdsbms_flag= 0;		/* whether to include them */
   unsigned char	*px;			/* working ptr w/in EntireMsg */
   long			lTemp;			/* working var */
   int			n,Stat= 1;		/* default to error */

   DPRINT1 ("Entering %s...\n", func);
/*
*
* A.1          IF (ptr is null or if the Entire_msg buffer is null) THEN
*                 RETURN 1 !errmsg filled
*              ENDIF
*/
   if (!gh || !gh->entire_msg) 
	{  
	sprintf (errmsg, "%s: expecting non-null GRIB_HDR struct\n", func);
	goto BYE; 
	}

/*
*
* A.2          CREATE storage for the Internal structures;
*                  ! PDS_INPUT
*                  ! GDS_HEAD_INPUT
*                  ! GDS_Proj_Input set to MAX_INP_PROJ_SIZE defined in grib.h
*                  ! BDS_HEAD_INPUT
*              RETURN with Malloc Err in errmsg if fails;
*              INITIALIZE Internal structures
*/
   if (! (pPDS_Input= (PDS_INPUT *)malloc(sizeof(PDS_INPUT))) ||
       ! (pGDS_Head_Input =(GDS_HEAD_INPUT*)malloc(sizeof(GDS_HEAD_INPUT))) ||
       ! (pvGDS_Proj_Input= (void *) malloc (MAX_INP_PROJ_SIZE))  ||
       ! (pBDS_Head_Input =(BDS_HEAD_INPUT*)malloc(sizeof(BDS_HEAD_INPUT))))
	{
        sprintf(errmsg,"%s:  failed to make storage for Internal Structs\n",
	func);
	goto BYE; 
	}
   memset ((void *) pPDS_Input, '\0', sizeof(PDS_INPUT));
   memset ((void *) pGDS_Head_Input, '\0', sizeof (GDS_HEAD_INPUT));
   memset ((void *) pvGDS_Proj_Input,'\0', MAX_INP_PROJ_SIZE);
   memset ((void *) pBDS_Head_Input, '\0', sizeof (BDS_HEAD_INPUT));

/*
*
* A.3          IF (creating all sections) 
*              ! ** (shuffled == 0) **
*              ! user passed Float data in, and the GribHdr's
*              ! Entire_Msg array has no valid data in it;
*              ! Must 'put' all Sections 0 thru 5 into Grib Hdr in that order;
* A.3.a        THEN
*/
   if (! gh->shuffled) 
        { 
	/* Create All Sections mode:  user must send float data */
        DPRINT0 ("(SHUFFLE=0)  Create ALL sections mode\n");

/*
* A.3.a.1          RETURN if Float array is Null;  !errmsg filled
*/
	if ( !pfData_Array) 
	  {
	   sprintf(errmsg,
	  "%s: <Create-All mode>  No DataArray avail to encode\n",func);
	   goto BYE;
	   }

/*
* A.3.a.2          CLEAR out the length and section ptrs
*                  ASSIGN beginning of Entire Msg to 'px', as location to 
*                  append things to;
*/
	gh->msg_length= gh->eds_len= gh->pds_len= gh->gds_len= 0;
	gh->bms_len= gh->bds_len= gh->eds_len= 0;
	gh->eds_ptr= gh->pds_ptr= gh->gds_ptr= NULL;
	gh->bms_ptr= gh->bds_ptr= gh->eds_ptr= NULL;
	px = gh->entire_msg; /* append from here on */
/*
*
* A.3.a.3          BUILD IDS SECTION
*                  SET up pointer to IDS
*                  SET up IDS length  (8 for Edition 1)
*                  WRITE the Ident Data Section to Grib Hdr
*                  UPDATE 'px' to end of IDS !where to write next section
*/
	gh->ids_len = 8; 
	gh->msg_length += 8L;
	gh->ids_ptr = px;
	memcpy  ((void *) gh->ids_ptr, (void *)"GRIB....", 8);
	px = gh->entire_msg + gh->msg_length;
	DPRINT3 ("%s: 'putting' IDS (%ld), msg_len=%ld\n",
	func, gh->ids_len,gh->msg_length);

/*
*
* A.3.a.4          FUNCTION gribputpds   !Build PDS Section into GRIB_HDR
*                  IF failed
*                  THEN return with error !errmsg filled
*                  ELSE  bump 'px' to end of this section
*/
        if (n= gribputpds (Data_Input, User_Input, pPDS_Input, &gh, errmsg)) 
	 { 
	   upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
	else   px = gh->entire_msg + gh->msg_length;

	DPRINT3("%s: Encoding PDS (%ld), msg_len=%ld\n",
	func, gh->pds_len,gh->msg_length);

/*
*
* A.3.a.5          FUNCTION gribputgds  !Build GDS Section into GRIB_HDR
*                  IF failed
*                  THEN return with error !errmsg filled
*                  ELSE  bump 'px' to end of this section
*/
 if ((int) (User_Input.usGds_bms_id) >= 128)
      {
           if ( n = gribputgds (Geom_In, 
	    	       pGDS_Head_Input, &pvGDS_Proj_Input, &gh, errmsg) )
	     { 
	       upd_child_errmsg (func, errmsg);
	       goto BYE; 
	     }
           else   px = gh->entire_msg + gh->msg_length;

           DPRINT3 ("%s: Encoding  GDS (%ld), msg_len=%ld\n",
	    func, gh->gds_len,gh->msg_length);
      }
        else DPRINT1 ("%s: SKIPPING GDS!\n", func);

/*
*
* A.3.a.6          Force no BMS by default
*/
	gh->bms_ptr=0; gh->bms_len= 0; 
	DPRINT1 ("%s: Skipping BMS by default\n", func);

/*
*
* A.3.a.7          FUNCTION gribputbds  Build BDS Section into GRIB_HDR
*                  IF failed
*                  THEN return with error !errmsg filled
*                  ELSE  bump 'px' to end of this section
*/
        if (n= gribputbds (User_Input, Geom_In.nx*Geom_In.ny, 
			pPDS_Input->sDec_sc_fctr, 
			pfData_Array, 
			pBDS_Head_Input, &gh, errmsg)) 
	 { 
	   upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
       else   px = gh->entire_msg + gh->msg_length;

       DPRINT3 ("%s: Encoding BDS (%ld), msg_len=%ld\n",
		func, gh->bds_len,gh->msg_length);

/*
*
* A.3.a.8          IF (Entire Msg buffer isn't big enough to hold EDS)
*                  THEN
*                     FUNCTION Expand_gribhdr    !make it 4 bytes larger
*                     RETURN with Error if fails !errmsg filled
*                  ENDIF
*                  SET up pointer to EDS
*                  WRITE Grib EDS section to the end of Data !"7777"
*                  UPDATE Grib Hdr's Eds_Ptr, Eds_Len 
*/
       if (gh->msg_length > gh->abs_size 
	&& Expand_gribhdr (gh, gh->msg_length, errmsg) != 0)
	 { 
	   upd_child_errmsg (func, errmsg); 
	   goto BYE; 
	 }

       gh->eds_ptr= px;
       gh->eds_len= 4;
       gh->msg_length += gh->eds_len;
       memcpy ((void *)gh->eds_ptr, (void*)Sevens, 4);
       DPRINT3 ("%s: 'putting' EDS (%ld), msg_len=%ld\n",
       func, gh->eds_len,gh->msg_length);

  } /* END SHUFFLED == 0 SECTION */

/*
* A.3.b        ELSE 
*              ! ** (shuffled == 1) **
*              ! means that user has already put 1/more GRIB sections 
*              ! in GRIB_HDR struct's Entire_Msg;  The already included
*              ! Sections may not be in proper GRIB-format order, and have 
*              ! non-null Pointers and non-zero length;  Msg_Length also 
*              ! reflects total length of all included sections;  
*              ! -if the Float data is Null, the Bds must already be included
*              ! in the Grib Hdr;  Func will return error if the Bds pointer
*              ! is Null or the Bds Len is zero;  
*              ! -if the incoming Float data has data and the Grib hdr shows
*              ! that BMS is already defined then the func will Ignore the
*              ! float data;
*              ! otherwise, the float data will be used to create a new
*              ! Binary Data Section;
*              ! Only need to 'put' the Sections that have not already been 
*              ! included in the Grib Header;  
*/
   else {  /* Shuffle Mode:  Create Missing Sections mode */

/*
* A.3.b.1          IF (there is discrepency in section pointers and length)
*                      RETURN 1  !errmsg filled
*                  ENDIF
*/
	if (  (gh->ids_ptr && !gh->ids_len) || (gh->ids_len && !gh->ids_ptr)
	   || (gh->pds_ptr && !gh->pds_len) || (gh->pds_len && !gh->pds_ptr)
	   || (gh->gds_ptr && !gh->gds_len) || (gh->gds_len && !gh->gds_ptr)
	   || (gh->bms_ptr && !gh->bms_len) || (gh->bms_len && !gh->bms_ptr)
	   || (gh->bds_ptr && !gh->bds_len) || (gh->bds_len && !gh->bds_ptr)
	   || (gh->eds_ptr && !gh->eds_len) || (gh->eds_len && !gh->eds_ptr)
	   || (gh->entire_msg && !gh->msg_length) 
	   || (gh->msg_length && !gh->entire_msg)  )
	    {
	    sprintf (errmsg,
	    "%s:  GribHdr Length/Ptr to sections are not consistent\n", func); 
	    goto BYE;  
	    }

/*
* A.3.b.2          IF (no float array was passed in AND 
*                      Grib Hdr shows BDS is undefined) THEN
*                      RETURN 1 !errmsg filed
*                  ENDIF
*/
	if ( !pfData_Array && !gh->bds_ptr) {
	   sprintf(errmsg, 
	   "%s: <Create Missing Sect mode> No DataArray avail to encode Bds\n",
	   func);
	   goto BYE;
	   }

/*
* A.3.b.3          IF (user did send in float array AND 
*                      Grib Hdr shows BDS is already defined) THEN
*                      PRINT warning   !won't encode float array
*                  ENDIF
*/
	if ( pfData_Array && gh->bds_ptr && gh->bds_len>0) {
	   DPRINT2 ("%s: GribHdr already has a BDS (Len=%ld), " \
	   " not going to encode the Float Data\n" , func, gh->bds_len);
	 }

	DPRINT7 ("(SHUFFLE=1)  gribhdr contains msg with totlen=%ld\n" \
	        "   IDS(%d), PDS(%d), GDS(%d), BMS(%d), BDS(%d), EDS(%d)\n",
		gh->msg_length, gh->ids_len, gh->pds_len, gh->gds_len,
		gh->bms_len, gh->bds_len, gh->eds_len);

/*
* A.3.b.4          ASSIGN to local ptr 'px' the address of Msg_length bytes
*                  away from Entire Msg, as location to append things to;
*/
	 px = gh->entire_msg + gh->msg_length; /* append from here */
	  
/*
*
* A.3.b.5          IF (GribHdr has no IDS yet)
*                  THEN
*                      SET up pointer to IDS
*                      SET up IDS length  (8 for Edition 1)
*                      WRITE the Ident Data Section to Grib Hdr
*                      !use dummy message length for now
*                      UPDATE 'px' to end of IDS !where to write next section
*                  ENDIF
*/
   if ( gh->ids_ptr==NULL ) 
     {
	gh->ids_len = 8; 
	gh->msg_length += 8L;
	gh->ids_ptr = px;
	memcpy  ((void *) gh->ids_ptr, (void *)"GRIB....", 8);
	px = gh->entire_msg + gh->msg_length;
	DPRINT3 ("%s: 'putting' IDS (%ld), msg_len=%ld\n",
	func, gh->ids_len,gh->msg_length);
    }
  else DPRINT1 ("%s: skip writing IDS\n", func);

/*
*
* A.3.b.6          IF (GribHdr has no PDS yet)
*                  THEN
*                      FUNCTION gribputpds  !Build PDS Section into GRIB_HDR
*                      IF failed
*                      THEN return with error !errmsg filled
*                      ELSE  bump 'px' to end of this section
*                  ENDIF
*/
   if ( gh->pds_ptr==NULL)  
     {
        if (n= gribputpds (Data_Input, User_Input, pPDS_Input, &gh, errmsg)) 
	 { 
	   DPRINT2 ("%s:  got err=%d in Grib Put Pds()\n",func,n);
	   upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
	else   px = gh->entire_msg + gh->msg_length;

	DPRINT3("%s: 'putting' PDS (%ld), msg_len=%ld\n",
	func, gh->pds_len,gh->msg_length);
     }
  else DPRINT1("%s:  skip writing PDS\n", func);

/*
*
* A.3.b.7          IF (GribHdr has no GDS yet)
*                  THEN
*                      FUNCTION gribputgds  !Build GDS Section into GRIB_HDR
*                      IF failed
*                      THEN return with error !errmsg filled
*                      ELSE  bump 'px' to end of this section
*                  ENDIF
*/

   if ((gh->gds_ptr==NULL) && (User_Input.usGds_bms_id >= 128))
    {
       if ( n = gribputgds (Geom_In, 
	    	   pGDS_Head_Input, &pvGDS_Proj_Input, &gh, errmsg) )
	 { 
	   DPRINT2 ("%s:  got err=%d in Grib Put Gds()\n",func,n);
	   upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
       else   px = gh->entire_msg + gh->msg_length;

       DPRINT3 ("%s: 'putting' GDS (%ld), msg_len=%ld\n",
	func, gh->gds_len,gh->msg_length);
    }
  else DPRINT1 ("%s:  skip writing GDS\n", func);

/* 
*
* A.3.b.8          CHECK consistency on Gds/Bms flag
*                  IF (GDS is included) 
*                  THEN SET the GdsPresent bit
*                  ELSE CLEAR the GdsPresent bit 
*                  ENDIF
*/
  gdsbms_flag = (int)gh->pds_ptr[7] & 0x000000FF;
  DPRINT1 ("orig gds/bms flag, pds[7] = 0x%x\n", gdsbms_flag);

  if (gh->gds_ptr == NULL) {
	 gdsbms_flag &= ~(0x00000080); 	
	 DPRINT2 ("%s: GDS missing, so CLEAR 0x80;  newFLG=0x%x, \n", 
	 func, gdsbms_flag);
	}
  else { 
	 gdsbms_flag |= (0x00000080); 
	 DPRINT2 ("%s: GDS Present, so SET 0x80;  newFLG=0x%x, \n", 
	 func, gdsbms_flag);
	 /* 
 	    DONOT set grid id to 255, since it is possible for user to
	    define a new grid with id w/in range, and still include GDS;
 	  */
	}

/* 
*
* A.3.b.9          IF (BMS is there) THEN
*                      SET the BmsPresent bit
*                  ELSE
*                      CLEAR the BmsPresent bit
*                  ENDIF
*/
  if (gh->bms_ptr == NULL) {
	 gdsbms_flag &= ~(0x00000040);
	 DPRINT2 ("%s: no BMS, so CLEAR 0x40; new FLG=0x%x",func,gdsbms_flag);
	}
  else { gdsbms_flag |= (0x00000040);
	 DPRINT2("%s: BMS Present, so SET 0x40; new FLG=0x%x",func,gdsbms_flag);
	}

   gh->pds_ptr[7] = (unsigned char)gdsbms_flag;
   DPRINT1 ("; PDS_ptr[7]= %x\n",gh->pds_ptr[7]);

/*
*
* A.3.b.10         IF (GribHdr has no BDS yet) THEN 
*                      FUNCTION gribputBds  !Build BDS Section into GRIB_HDR
*                      !**NOT doing anything to Data even if BMS is included ***
*                      IF failed
*                      THEN return with error !errmsg filled
*                      ELSE  bump 'px' to end of this section
*                  ENDIF
*/
   if ( gh->bds_ptr==NULL )
    {
        if (n= gribputbds (User_Input, Geom_In.nx*Geom_In.ny, 
			pPDS_Input->sDec_sc_fctr, 
			pfData_Array, 
			pBDS_Head_Input, &gh, errmsg)) 
	 { 
	   DPRINT2 ("%s:  got err=%d in Grib Put BDS()\n",func,n);
	   upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
       else   px = gh->entire_msg + gh->msg_length;

       DPRINT3 ("%s: 'putting' BDS (%ld), msg_len=%ld\n",
		func, gh->bds_len,gh->msg_length);
    }
  else DPRINT1("%s: skip writing BDS\n", func);

/*
*
* A.3.b.11         IF (GribHdr has no EDS yet)
*                  THEN
*                    IF (Entire Msg buffer isn't big enough to hold EDS)
*                     FUNCTION Expand_gribhdr    !make it 4 bytes larger
*                     RETURN with Error if fails !errmsg filled
*                  ENDIF
*                  SET up pointer to EDS
*                  WRITE Grib EDS section to the end of Data !"7777"
*                  UPDATE Grib Hdr's Eds_Ptr, Eds_Len 
*/
   if ( gh->eds_ptr==NULL ) 
   {
       if (gh->msg_length+5L > gh->abs_size ) {
    	  DPRINT1 ("Need to expand gribhdr (%ld) to hold EDS\n", 
	  gh->abs_size);	  
          /*if (NULL == (realloc (gh->entire_msg, gh->msg_length))) */

	  if (Expand_gribhdr (gh, gh->msg_length+5L, errmsg) ) {
	     upd_child_errmsg (func, errmsg);
	     goto BYE;
	  }
	  DPRINT1("gribhdr now has abs_size of %ld\n",
	  gh->abs_size);

	}  /* size changed */

       gh->eds_ptr= px;
       gh->eds_len= 4;
       gh->msg_length += gh->eds_len;
       memcpy ((void *)gh->eds_ptr, (void*)Sevens, 4);
       DPRINT3 ("%s: 'putting' EDS (%ld), msg_len=%ld\n",
       func, gh->eds_len,gh->msg_length);
  }
  else DPRINT1 ("%s:  skip writing EDS\n", func);

/*
* A.3          ENDIF
*/
        } /* END SHUFFLED == 1 SECTION */

/*
*
* A.4          UPDATE Total Msg Length in Grib Hdr's Ident Data Sect
*/
   set_bytes(gh->msg_length, 3, gh->ids_ptr+4);
   
   /* 1 is for the Edition  1 */   
   set_bytes(1,1,gh->ids_ptr+7);

/*
*
* A.5          SET status to 0  ! no errors
*/
   Stat = 0;
   

BYE:
/*
*
* A.6          PRINT message if error occurred
*/
  if (errmsg[0]!='\0') DPRINT1("%s\n", errmsg);
/*
*
* A.7          FREE up space of local Input structures
*
* A.8          RETURN stat
*/
  /* 
   * Changed by Todd Hutchinson, TASC
   * With this original code, not all memory was being freed
   */
  /* Original
  if (! pPDS_Input)  free(pPDS_Input);
  if (! pGDS_Head_Input) free(pGDS_Head_Input);
  if (! pvGDS_Proj_Input) free(pvGDS_Proj_Input);
  if (! pBDS_Head_Input) free(pBDS_Head_Input);
  */
  /* New: */
  free(pPDS_Input);
  free(pGDS_Head_Input);
  free(pvGDS_Proj_Input);
  free(pBDS_Head_Input);

  DPRINT3 ("Leaving %s (Msglen=%ld), stat=%d\n", func, gh->msg_length,Stat);
  return Stat;

/*
*
* END OF FUNCTION
*
*
*/ 
}
