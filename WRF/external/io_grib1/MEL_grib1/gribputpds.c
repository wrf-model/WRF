/* FILENAME:   gribputpds.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#ifdef XT3_Catamount
#include <features.h>
#elif defined(_WIN32)
#include <Winsock2.h>
#else
#include <netinet/in.h>
#endif
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*
****************************************************************************
* A.  FUNCTION:  gribputpds
*       Use the information provided to create a Product Defn Section of
*       the GRIB format and store it in the GRIB_HDR structure;
*
*    INTERFACE:
*       int gribputpds (Data_Input, User_Input, pPDS_Input, ppgrib_hdr, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  DATA_INPUT Data_Input;
*           Structure containing info of this field (ids used in the Sections)
*      (I)  USER_INPUT User_Input;
*           Structure containing encoder configuration data
*      (O)  PDS_INPUT  *pPDS_Input;
*           points to an empty Structure;   to be filled with PDS info
*           retrieved from  Data_Input and User_Input.
*     (I&O) GRIB_HDR **ppgrib_hdr;
*           points to Grib Header Structure;  May already have 1 or more
*           GRIB sections in it;  Will have PDS appended to its 'entire_msg',
*           'pds_ptr', 'pds_len' and 'mesg_len' updated also.
*      (O)  char *errmsg;
*           empty array, returned filled if error occurred
* 
*    RETURN CODE:
*       0>  no errors;
*           Grib Header structure now has PDS newly appended to its
*           entire_msg buffer, its sections length, message length,
*           and section pointers are updated.
*	1>  error, errmsg filled;
*           failed to make storage for PDS_GRIB, or
*           failed to enlarge 'entire_msg' to hold new PDS block;
*      99>  error in create_inpPDS() or inp2grib_PDS(); errmsg filled;
****************************************************************************/

#if PROTOTYPE_NEEDED
int gribputpds ( DATA_INPUT 	Data_Input,
		USER_INPUT 	User_Input,
		PDS_INPUT 	*pPDS_Input,
		GRIB_HDR 	**ppgrib_hdr,
		char 		*errmsg)
#else
int gribputpds ( Data_Input, User_Input, pPDS_Input, ppgrib_hdr, errmsg)
		DATA_INPUT 	Data_Input;
		USER_INPUT 	User_Input;
		PDS_INPUT 	*pPDS_Input;
		GRIB_HDR 	**ppgrib_hdr;
		char 		*errmsg;
#endif
{
   PDS_GRIB  *pPDS_Grib=0; /* true GRIB format for pds */
   GRIB_HDR  *gh;	   /* working var */
   int       stat= 0;	   /* status */
   long	     newsize=0L;   /* size of msg after adding PDS block */
   /*void      create_inpPDS ();
   int       inp2grib_PDS ();*/
   char	     *func= "GribPutPDS";

  DPRINT1("\nEntering %s()......\n", func);
/*
*
* A.1       FUNCTION create_inpPDS   	        !void
*           !create internal struct PDS_INPUT from DATA_INPUT & USER_INPUT
*/
   create_inpPDS (Data_Input, User_Input, pPDS_Input);

/*
*
* A.2       MALLOC local struct PDS_GRIB, clear it out;
*           IF (fails) THEN
*               SET bad stat
*               RETURN
*           ELSE 
*               CLEAR out the struct
*           ENDIF
*/
  if ( !(pPDS_Grib= (PDS_GRIB *)malloc(sizeof(PDS_GRIB))) )
    {	
     sprintf(errmsg,"%s:  failed storage for PDS_GRIB\n",func);
     stat=1; goto BYE; 
    }
   else memset ((void *)pPDS_Grib, '\0', sizeof(PDS_GRIB));

/*
*
* A.3       FUNCTION inp2grib_PDS   
*           !convert internal PDS_INPUT to true Grib format PDS_GRIB
*           IF (error) THEN
*               SAVE error from func in stat
*               RETURN
*           ENDIF
*/
   if (stat = inp2grib_PDS (pPDS_Input, &pPDS_Grib, errmsg))
	 { upd_child_errmsg (func, errmsg);
	   goto BYE; 
	 }
/*
*
* A.4       CALCULATE new msg length after adding new PDS
*/
  DPRINT0("putting Pds into Grib Hdr struct\n");

  gh= *ppgrib_hdr;
  newsize= gh->msg_length + sizeof(PDS_GRIB);

/*
*
* A.5       IF gribhdr's buffer is too small AND
*               FUCTION Expand_gribhdr failed 
*           THEN
*               SET stat = 1
*               RETURN with error   !errmsg filled
*           ENDIF
*/
     if (newsize > gh->abs_size
        && Expand_gribhdr (gh, newsize, errmsg) !=0)
        {
        stat = 1;
        upd_child_errmsg (func, errmsg);
        goto BYE;
        }

/*
*
* A.6       COPY Pds and its info into Grib Header
*           !copy PDS_GRIB struct to the end of Entire_msg array;
*           !store pds pointer and length
*           !update msg length
*/
   gh->pds_ptr= gh->entire_msg + gh->msg_length;
   memcpy ((void *) gh->pds_ptr, (void *) pPDS_Grib, sizeof (PDS_GRIB));
   gh->pds_len    = sizeof(PDS_GRIB);
   gh->msg_length += gh->pds_len;

   DPRINT1 ("copied PDS_GRIB(%ld) bytes from pPDS_Grib to PDS_PTR\n",
     	sizeof(PDS_GRIB));

/*
*
* A.7       FREE up local struct PDS_GRIB
*
* A.8       RETURN to caller with stat
*/
BYE:
   if (pPDS_Grib!=NULL) free (pPDS_Grib);
   DPRINT2 ("Leaving %s(), stat=%d\n", func,stat);
   return stat;
/*
*
* END OF FUNCTION
*
*
*/
}

/*
*
*********************************************************************
* B. FUNCTION:  create_inpPDS
*        Fill the internal Product Defn Section structure with info
*        retrieved from the 2 input structures DATA_INPUT and USER_INPUT.
*
*    INTERFACE:
*        void  create_inpPDS (Data_Input, User_Input, pPDS_Input)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  DATA_INPUT Data_Input;   holds ids to be used in Sections
*      (I)  USER_INPUT User_Input;   holds encoder configuration info
*      (O)  PDS_INPUT *pPDS_Input;   pre-allocated structure to be filled;
*
*    RETURN CODE:   none;
**********************************************************************
*/
#if PROTOTYPE_NEEDED
void  create_inpPDS ( DATA_INPUT Data_Input, USER_INPUT User_Input,
			PDS_INPUT *pPDS_Input)
#else
void  create_inpPDS ( Data_Input, User_Input, pPDS_Input)
			DATA_INPUT Data_Input; 
			USER_INPUT User_Input;
			PDS_INPUT *pPDS_Input;
#endif
{
int i;

   DPRINT0 ( "Entering create_inpPDS ()\n" );
/*
*
* B.1       LOAD info from struct USER_INPUT into struct PDS_INPUT 
*/

/* assigns the values from USER_INPUT to PDS_Input */
   pPDS_Input->usEd_num = (unsigned short) 1; /* GRIB Edition num */
   pPDS_Input->usParm_tbl = User_Input.usParm_tbl; /* GRIB TblVersion num */
   pPDS_Input->usSub_tbl = User_Input.usSub_tbl; /* Local TblVersion num */
   pPDS_Input->usCenter_id = User_Input.usCenter_id; /* Originating Ctr-Tbl0*/
   pPDS_Input->usProc_id = Data_Input.usProc_id; /* Model id */
   pPDS_Input->usGrid_id = Data_Input.usGrid_id; /* Grid id num */
   pPDS_Input->usGds_bms_id = User_Input.usGds_bms_id; /* GDS/BMS flag-Tbl1 */
   pPDS_Input->usParm_id = Data_Input.usParm_id; /* Parameter& Units id -Tbl2 */
   pPDS_Input->usParm_sub = Data_Input.usParm_sub_id;/* Sub-Tblentry for Tbl2 */
   pPDS_Input->usLevel_id = Data_Input.usLevel_id; /* Type of level/layer-Tbl3*/ /* Height, pressure of level 1  and 2 */ 
   pPDS_Input->usHeight1 =(unsigned short)Data_Input.nLvl_1 ; 
   pPDS_Input->usHeight2 =(unsigned short)Data_Input.nLvl_2 ; 
   pPDS_Input->usYear =(unsigned short)( Data_Input.nYear % 100 ); /* Year */
   pPDS_Input->usMonth =(unsigned short)Data_Input.nMonth; /* Month */
   pPDS_Input->usDay =(unsigned short)Data_Input.nDay;/* Day of month */
   pPDS_Input->usHour =(unsigned short)Data_Input.nHour; /* Hour of day */
   pPDS_Input->usMinute =(unsigned short)Data_Input.nMinute; /* Minute of hour*/
   pPDS_Input->usSecond =(unsigned short)Data_Input.nSecond; /* Secs of Min */
   pPDS_Input->usFcst_unit_id = Data_Input.usFcst_id; /*ForecastTime unit-Tbl4*/
   /* Period of time (tau)- 0 for analysis */
   pPDS_Input->usP1 = Data_Input.usFcst_per1;
   /* Period of time between analyses */
   pPDS_Input->usP2 = Data_Input.usFcst_per2;
   /* Time range indicator-Tbl5 */
   pPDS_Input->usTime_range = Data_Input.usTime_range_id;
   /* Num in average */
   pPDS_Input->usTime_range_avg = Data_Input.usTime_range_avg;
   /* Num missing from average */
   pPDS_Input->usTime_range_mis = Data_Input.usTime_range_mis; 

   /* Century of reference time */
   if (Data_Input.nYear % 100 == 0) {
     pPDS_Input->usCentury = (unsigned short)( Data_Input.nYear / 100 );
     pPDS_Input->usYear += 100;
   } else {
     pPDS_Input->usCentury =(unsigned short)( Data_Input.nYear / 100 + 1);
   }

   /* Decimal scale factor */
   pPDS_Input->sDec_sc_fctr = (short) Data_Input.nDec_sc_fctr;
   /* reserved bytes */
   for ( i=0 ; i< 12 ; i++)pPDS_Input->ausZero[i] = 0; /* Reserved- Set to 0 */
   /* Oct-26 was reserved, now holds Sub-Center Id */
   pPDS_Input->usCenter_sub =  User_Input.usCenter_sub;
   /* Oct-41:  show that Grib Extensions are used */
   pPDS_Input->usExt_flag = (unsigned short)EXTENSION_FLAG; 
   /* Tracking ID for data set */
   pPDS_Input->usTrack_num = User_Input.usTrack_num; 

   /* WSI Extended PDS section: Used for extended and higher resolution time periods */
   pPDS_Input->PDS_41 = Data_Input.PDS_41;
   pPDS_Input->PDS_42 = Data_Input.PDS_42;
   pPDS_Input->PDS_46 = Data_Input.PDS_46;
   pPDS_Input->PDS_47 = Data_Input.PDS_47;
   pPDS_Input->PDS_51 = Data_Input.PDS_51;
   pPDS_Input->PDS_52 = Data_Input.PDS_52;

/*
*
* B.2       ASSIGN size of PDS_GRIB into uslength of struct PDS_INPUT
	    ** If encoding MEL GRIB messages, Pds Length should be 46 and
	    Octet 41 should equal the Extension Flag.
*/
   pPDS_Input->uslength = sizeof(PDS_GRIB);

/*
*
* B.3       DEBUG Print
*/
   DPRINT1("\t create_inpPDS:  uslength = %u (Size of PDS_GRIB)\n",
   pPDS_Input->uslength );
   DPRINT1("\t create_inpPDS:  usEd_num = %u\n", pPDS_Input->usEd_num );
   DPRINT1("\t create_inpPDS:  usParm_tbl = %u\n", pPDS_Input->usParm_tbl );
   DPRINT1("\t create_inpPDS:  usSub_tbl = %u\n", pPDS_Input->usSub_tbl);
   DPRINT1("\t create_inpPDS:  usCenter_id = %u\n", pPDS_Input->usCenter_id );
   DPRINT2("\t create_inpPDS:  usCenter_sub Oct26=%u, usExt_flag Oct41=%u\n", 
	pPDS_Input->usCenter_sub, pPDS_Input->usExt_flag);
   DPRINT1("\t create_inpPDS:  usProc_id = %u\n", pPDS_Input->usProc_id );
   DPRINT1("\t create_inpPDS:  usGrid_id = %u\n", pPDS_Input->usGrid_id );
   DPRINT1("\t create_inpPDS:  usGds_bms_id = %u\n", pPDS_Input->usGds_bms_id);
   DPRINT1("\t create_inpPDS:  usParm_id = %u\n", pPDS_Input->usParm_id );
   DPRINT1("\t create_inpPDS:  usParm_sub = %u\n", pPDS_Input->usParm_sub);
   DPRINT1("\t create_inpPDS:  usLevel_id = %u\n", pPDS_Input->usLevel_id );
   DPRINT1("\t create_inpPDS:  usHeight1 = %u\n", pPDS_Input->usHeight1 );
   DPRINT1("\t create_inpPDS:  usHeight2 = %u\n", pPDS_Input->usHeight2 );
   DPRINT1("\t create_inpPDS:  usCentury = %u\n", pPDS_Input->usCentury );
   DPRINT1("\t create_inpPDS:  usYear = %u\n", pPDS_Input->usYear );
   DPRINT1("\t create_inpPDS:  usDay = %u\n", pPDS_Input->usDay );
   DPRINT1("\t create_inpPDS:  usHour = %u\n", pPDS_Input->usHour );
   DPRINT1("\t create_inpPDS:  usMinute = %u\n", pPDS_Input->usMinute );
   DPRINT1("\t create_inpPDS:  usSecond = %u\n", pPDS_Input->usSecond );
   DPRINT1("\t create_inpPDS:  usP1 = %u\n", pPDS_Input->usP1);
   DPRINT1("\t create_inpPDS:  sDec_sc_fctr  = %d\n", pPDS_Input->sDec_sc_fctr);
   DPRINT1("\t create_inpPDS:  usTrack_num = %u\n", pPDS_Input->usTrack_num);
   DPRINT0("\t create_inpPDS:  WSI Extended PDS Section: \n");
   DPRINT1("\t create_inpPDS:  PDS_41 (Forecast time 1 unit id): %u\n",pPDS_Input->PDS_41);
   DPRINT1("\t create_inpPDS:  PDS_42 (Forecast time 1): %u\n",pPDS_Input->PDS_42);
   DPRINT1("\t create_inpPDS:  PDS_46 (Forecast time 2 unit id): %u\n",pPDS_Input->PDS_46);
   DPRINT1("\t create_inpPDS:  PDS_47 (Forecast time 2 unit id): %u\n",pPDS_Input->PDS_47);
   DPRINT1("\t create_inpPDS:  PDS_51 (Time range indicator): %u\n",pPDS_Input->PDS_51);
   DPRINT1("\t create_inpPDS:  PDS_52 (Top of atm): %u\n",pPDS_Input->PDS_52);

   
/*
*
* B.4       RETURN w/nothing
*/
   DPRINT0 ("Exiting create_inpPDS with no errors;\n");  

/*
*
* END OF FUNCTION
*
*
*/
}

/*
*
****************************************************************************
* C. FUNCTION:  inp2grib_PDS
*      Use the data from the internal structure to fill the Product 
*      Definition Section structure.
*
*    INTERFACE:
*      int inp2grib_PDS ( pPDS_Input, ppPDS_Grib, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  PDS_INPUT *pPDS_Input;
*           internal PDS structure, used for input
*      (O)  PDS_GRIB  **ppPDS_GRIB ;
*           pre-allocated structure to be filled;
*      (O)  char *errmsg;
*           empty array, returned filled if error occured;
*
*   RETURN CODE:
*      0> no errors;  PDS_GRIB filled;
*     99> unexpected null pointers, Errmsg filled;
****************************************************************************/

#if PROTOTYPE_NEEDED
int inp2grib_PDS ( PDS_INPUT 	*pPDS_Input,
		PDS_GRIB 	**ppPDS_Grib,
		char 		*errmsg)
#else
int inp2grib_PDS ( pPDS_Input, ppPDS_Grib, errmsg)
		PDS_INPUT 	*pPDS_Input;
		PDS_GRIB 	**ppPDS_Grib;
		char 		*errmsg;
#endif
{
   char		    *func= "inp2grib_PDS";
   unsigned char    ach3bytes[3];
   unsigned long    ulPDS_length = 0;
   short            sDec_sc_fctr = 0;
   int              nStatus = 0;
   int              i; 		/* loop counter */
   long		    lTemp;	/* working var */
   PDS_GRIB         *tpds; 	/* true grib pds, working var */
   short            tmp_byte2;  /* working var */
   long             tmp_byte4;  /* working var */

   DPRINT0  ( "Entering inp2grib_PDS......\n" );

/*
*
* C.1       IF (either Internal PDS_INPUT or True Grib PDS_GRIB is null) THEN
*              SET status = 99;
*              RETURN
*           ENDIF
*/
      if ( !ppPDS_Grib  || !pPDS_Input) {
	   sprintf(errmsg,
	   "%s: either PDS_GRIB /PDS_INPUT/or both are Null\n",func);
	   nStatus= 99;
	   goto BYE;
	}

/*
*
* C.2       ASSIGN local ptr to point to PDS_GRIB struct;
*/
      tpds = *ppPDS_Grib;

/*
*
* C.3       CREATE true Grib struct PDS_GRIB from internal PDS_INPUT
*/
      tpds->chParm_tbl =  ( unsigned char ) pPDS_Input->usParm_tbl;

      /* Commented out by Todd Hutchinson, WSI, when Extended PDS was replaced */
      /* tpds->chSub_tbl =  ( unsigned char ) pPDS_Input->usSub_tbl; */

      tpds->chCenter_id = ( unsigned char ) pPDS_Input->usCenter_id;
      tpds->chProc_id = ( unsigned char ) pPDS_Input->usProc_id;
      tpds->chGrid_id = ( unsigned char ) pPDS_Input->usGrid_id;
      tpds->chGds_bms_id = ( unsigned char ) pPDS_Input->usGds_bms_id;
      tpds->chParm_id = ( unsigned char ) pPDS_Input->usParm_id;

      /* Commented out by Todd Hutchinson, WSI, when Extended PDS was replaced */
      /* tpds->chParm_sub= (unsigned char )  pPDS_Input->usParm_sub; */

      tpds->chLevel_id = ( unsigned char ) pPDS_Input->usLevel_id;

      switch(pPDS_Input->usLevel_id){
        case   1:  /* surface(of the Earth, includes sea surface) level  */
        case   2:  /* cloud base level  */
        case   3:  /* cloud top level  */
        case   4:  /* 0 deg C isotherm level */
        case   5:  /* adiabatic condensation level  */
        case   6:  /* maximum wind speed level  */
        case   7:  /* tropopause level  */
        case   8:  /* nominal top of atmosphere level  */
        case   9:  /* sea bottom level  */
        case 100:  /* isobaric level            */
        case 103:  /* fixed height level        */
        case 105:  /* fixed height above ground */
        case 107:  /* sigma level               */
        case 111:  /* depth below land surface  */
        case 113:  /* isentropic (theta) level  */
        case 115:  /* sigma-z level  */
        case 119:  /* Eta Level */
        case 125:  /* height level above ground (high precision) */
        case 160:  /* depth below sea level     */
        case 200:  /* entire atmosphere considered as a single layer */
        case 201:  /* entire ocean considered as a single layer */
        case 212:  /* low cloud bottom level */
        case 213:  /* low cloud top level */
        case 222:  /* middle cloud bottom level */
        case 223:  /* middle cloud top level */
        case 232:  /* high cloud bottom level */
        case 233:  /* high cloud top level */
	  set_bytes(pPDS_Input->usHeight1, 2, tpds->achHeight);
          break;
        default:
	  set_bytes(pPDS_Input->usHeight2, 1, (tpds->achHeight));
	  set_bytes(pPDS_Input->usHeight1, 1, (tpds->achHeight)+1);
          break;
      }

      tpds->chYear = ( unsigned char ) pPDS_Input->usYear;
      tpds->chMonth = ( unsigned char ) pPDS_Input->usMonth;
      tpds->chDay = ( unsigned char ) pPDS_Input->usDay;
      tpds->chHour = ( unsigned char ) pPDS_Input->usHour;
      tpds->chMinute = ( unsigned char ) pPDS_Input->usMinute;
      tpds->chFcst_unit_id = ( unsigned char ) pPDS_Input->usFcst_unit_id;
      tpds->chP1 = ( unsigned char ) pPDS_Input->usP1;
      tpds->chP2 = ( unsigned char ) pPDS_Input->usP2;
      tpds->chTime_range = ( unsigned char ) pPDS_Input->usTime_range;

      set_bytes(pPDS_Input->usTime_range_avg,2,tpds->achTime_range_avg);

      tpds->chTime_range_mis = ( unsigned char ) pPDS_Input->usTime_range_mis;
      tpds->chCentury = ( unsigned char ) pPDS_Input->usCentury;
      tpds->chCenter_sub = ( unsigned char ) pPDS_Input->usCenter_sub;
      DPRINT1("Octet-26:  tpds->usCenter_sub= %d\n", (int)tpds->chCenter_sub);

      set_bytes(pPDS_Input->sDec_sc_fctr,2,tpds->achDec_sc_fctr);

      for(i=0;i<12;++i)
        tpds->achZero[i]= ( unsigned char ) pPDS_Input->ausZero[i];

      /* Commented out by Todd Hutchinson, WSI, when Extended PDS was replaced */
      /*
      tpds->chExt_flag = (unsigned char) pPDS_Input->usExt_flag;
      DPRINT1("Octet41:   tpds->chExt_flag= %d\n", (int)tpds->chExt_flag);

      tpds->chSecond = ( unsigned char ) pPDS_Input->usSecond;
      memcpy((void *)tpds->chTrack_num, 
		(void *)&(pPDS_Input->usTrack_num), 2);
      */
      
      /* Added by Todd Hutchinson, WSI.  Extended WSI PDS section */

      tpds->PDS_41 = (unsigned char)pPDS_Input->PDS_41;

      set_bytes(pPDS_Input->PDS_42,4,tpds->PDS_42);

      tpds->PDS_46 = (unsigned char)pPDS_Input->PDS_46;

      set_bytes(pPDS_Input->PDS_47,4,tpds->PDS_47);

      tpds->PDS_51 = (unsigned char)pPDS_Input->PDS_51;

      set_bytes(pPDS_Input->PDS_52,2,tpds->PDS_52);

      ulPDS_length= sizeof (PDS_GRIB);
      DPRINT1 ( "\t length of PDS_GRIB is %d\n", ulPDS_length );

      set_bytes(ulPDS_length, 3, tpds->achPDS_length);

     HDR_PRINT("encoded PDS", (unsigned char*)tpds, (int)ulPDS_length); 
/*
*
* C.4       RETURN with Status
*/
BYE:
      DPRINT1 ( "Exiting inp2grib_PDS(), stat=%d\n", nStatus);
      return ( nStatus );
/* 
* 
* END OF FUNCTION
*
*/ 
}
