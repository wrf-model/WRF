#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
/*
*
************************************************************************
* A.  FUNCTION  gribhdr2file
*       write out the Grib message stored in GRIB_HDR struct to stream;
*       if the 'shuffle' flag is set, write each individual section out, else
*       write 'entire_msg' all at once;
*               
*    INTERFACE:
*       int    gribhdr2file (gh, fn, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) GRIB_HDR *gh    holds the GRIB message to be written out
*      (I) FILE *stream    open strem to write to
*      (O) char *errmsg    array returned empty unless error occurred;
*
*     RETURN CODE:
*     0>  no errors,  GRIB file successfully created;
*     1>  error; errmsg is filled;
************************************************************************
*/
#if PROTOTYPE_NEEDED
int    gribhdr2file ( GRIB_HDR *gh, FILE *stream, char *errmsg)
#else
int    gribhdr2file ( gh, stream, errmsg)
			GRIB_HDR *gh; 
			FILE *stream; 
			char *errmsg;
#endif
{
  int fd;
  int stat;
  char *func= "gribhdr2file";

  fd = fileno(stream);
  if (fd == -1) 
    {
      DPRINT1 ("%s: Invalid file stream encountered.\n", func);
      return 1;
      
    }

  stat = gribhdr2filed ( gh, fd, errmsg);
  return stat;
  
}


/*
*
************************************************************************
* A.  FUNCTION  gribhdr2file
*       write out the Grib message stored in GRIB_HDR struct to file 
*       descriptor;
*       if the 'shuffle' flag is set, write each individual section out, else
*       write 'entire_msg' all at once;
*               
*    INTERFACE:
*       int    gribhdr2file (gh, fn, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) GRIB_HDR *gh    holds the GRIB message to be written out
*      (I) int f1          open file descriptor to write to
*      (O) char *errmsg    array returned empty unless error occurred;
*
*     RETURN CODE:
*     0>  no errors,  GRIB file successfully created;
*     1>  error; errmsg is filled;
************************************************************************
*/
#if PROTOTYPE_NEEDED
int    gribhdr2filed ( GRIB_HDR *gh, int f1, char *errmsg)
#else
int    gribhdr2filed ( gh, f1, errmsg)
			GRIB_HDR *gh; 
			int f1; 
			char *errmsg;
#endif
{
/*
*
* A.0     DEFAULT to error status of 1
*/
char *func= "gribhdr2file";
int  stat=1;		
char wrstring[4];
 int check;

/*
*
* A.1     IF (entire msg array is null or msg length is 0)
*         THEN 
*            RETURN error stat !errmsg filled
*         ENDIF
*/
  DPRINT1("Entering %s\n", func);
  if (gh->entire_msg == NULL || gh->msg_length <= 0) {
	DPRINT1 ("%s: GRIB_HDR message buffer is null, OR msg_length=0\n",func);
	sprintf(errmsg,"%s: GRIB_HDR message buffer is null, OR msg_length=0\n",
	func);
	goto BYE;
	}	

/*
*
* A.2     IF (in Shuffle mode)
*         THEN 
*            IF (length of EDS/PDS/BDS/EDS is 0) THEN
*               RETURN error stat !errmsg filled
*            ENDIF
*         ENDIF
*/
  if (gh->shuffled) {
	if (!gh->ids_len|| !gh->pds_len || !gh->bds_len|| !gh->eds_len) {
	   DPRINT1("%s:  Shuffle mode: Zero length encountered, quit\n", func);
	   sprintf(errmsg,
	   "%s:  Shuffle mode: Zero length encountered, quit\n", func);
	   goto BYE; }
	DPRINT1 ("%s:   this mesg is in shuffled mode;\n", func);
	}

/*
*
* A.4     IF (in shuffled mode) 
* A.4.a   THEN
*/
  if (gh->shuffled) {
/*
* A.4.a.1    IF (fails to write IDS OR fails to write PDS OR
*                (GDS exists AND fails to write GDS) OR 
*                (BMS exists AND fails to write BMS) OR 
*                fails to write BDS or fails to write EDS)
*            THEN
*               RETURN error stat !errmsg filled
*            ENDIF
*/
     if (write (f1, gh->ids_ptr , gh->ids_len) != gh->ids_len)
        {
          DPRINT1 ("%s:  failed to write IDS to file\n", func);
          sprintf(errmsg,"%s:  failed to write IDS to file\n", func);
          goto BYE;
        }
     if (write (f1, gh->pds_ptr , gh->pds_len) != gh->pds_len)
        {
          DPRINT1 ("%s:  failed to write PDS to file\n", func);
          sprintf(errmsg,"%s:  failed to write PDS to file\n", func);
          goto BYE;
        }
     if (gh->gds_len)
     if (write (f1, gh->gds_ptr , gh->gds_len) != gh->gds_len)
        {
          DPRINT1 ("%s:  failed to write GDS to file\n", func);
          sprintf(errmsg,"%s:  failed to write GDS to file\n", func);
          goto BYE;
        }
     if (gh->bms_len)
     if (write (f1, gh->bms_ptr , gh->bms_len) != gh->bms_len)
        {
          DPRINT1 ("%s:  failed to write BMS to file\n", func);
          sprintf(errmsg,"%s:  failed to write BMS to file\n", func);
          goto BYE;
        }
     if (write (f1, gh->bds_ptr , gh->bds_len) != gh->bds_len)
        {
          DPRINT1 ("%s:  failed to write BDS to file\n", func);
          sprintf(errmsg,"%s:  failed to write BDS to file\n", func);
          goto BYE;
        }
     if (write (f1, gh->eds_ptr , gh->eds_len) != gh->eds_len)
        {
          DPRINT1 ("%s:  failed to write EDS to file\n", func);
          sprintf(errmsg,"%s:  failed to write EDS to file\n", func);
          goto BYE;
        }
     DPRINT0 ("ALL Sections to written to file successfully\n");
     }
/*
* A.4.b   ELSE
*/
   else {
        DPRINT0 ("Writing gh->entire_msg (non-shuffled)\n");
/*
* A.4.b.1    IF (fails to write msg_length byte straight from Entire_msg)
*            THEN
*                RETURN error stat !errmsg filled
*            ENDIF
*/
        if ((check = write (f1, gh->entire_msg, gh->msg_length)) != 
	    gh->msg_length) {
	    DPRINT1( "%s:  failed to write GH's entire Msg to file\n",func);
	    sprintf(errmsg,
	    "%s:  failed to write GH's entire Msg to file %d\n",func,check);
	    /*	    goto BYE;  */
	   }
  	DPRINT0 ("write GH's entire_msg to file successful\n");
/*
* A.4     ENDIF
*/
        }
/*
*
* A.5     DONE, set status to 0  !no errors
*/
  stat = 0;

BYE:

/*
*
* A.7     RETURN with stat
*/
  DPRINT2 ("Leaving %s, stat=%d;\n", func, stat);
  return stat;
/*
*
* END OF FUNCTION
*/
}
