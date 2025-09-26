/* File:  grib_seek.c 		based on Decoder's trqgetmsg() func;
   Revised by:
   28oct96 Alice T. Nakajima (ATN), SAIC, Monterey
   18jun97 ATN check Edition before reading in entire msg;
   27aug97 ATN *SEEK_SET to 0 (gcc complains)
   20Oct97 ATN print #bytes read when fread fails;
   03nov97 ATN -Realloc
   22oct98 ATN *error msg;
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
**********************************************************************
* A.  FUNCTION: grib_seek
*       search the input file starting at the given offset for a GRIB 
*       message.  If found, return it in GRIB_HDR structure.
*
*    INTERFACE:
*       int grib_seek (InFile, offset, Read_Index,  gh, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *InFile;
*           name of input file to search for message;
*     (I&O) long  *offset;
*           number of bytes to skip from the beginning of file;
*           gets updated upon leaving to absolute #bytes from beginning of
*           file to beginning of message found;
*      (I)  int  Read_Index; 
*           if set, only proceed if 'GRIB' starts exactly at the given 
*           byte offset;
*      (O)  GRIB_HDR *gh;
*           empty upon entry;  to hold the Message found and its info;
*      (O)  char *errmsg;
*           empty array, only filled if error occurred;
*
*    RETURN CODE:  
*      0> no errors, may or may not have a valid message;
*	  If no Msg was Found: 
*          a)  errmsg will hold the Warning msg;
*	   If a valid Msg was Found: 
*	   a) long *offset: if succesful, gets updated to absolute 
*             beginning of Mesg;
*          b) struct GRIB_HDR holds its info:
*            entire_msg:  is assigned to newly MAlloced 
*                    unsigned char * array to hold entire message;
*	     msg_length:  size of entire_msg array in bytes;
*	     ids_len, pds_len, gds_len, bms_len, bds_len, eds_len:
*                    size of each defined sections in bytes;
*            ids_ptr:  pts to message's Ident Data Sect;
*            pds_ptr:  pts to message's Prod Defn Sect;
*            gds_ptr:  pts to message's Grid Defn Sect;
*            bms_ptr:  pts to message's Bitmap Defn Sect;
*            bds_ptr:  pts to message's Binary Data Sect;
*            eds_ptr:  pts to message's End Data Sect;
*         c) errmsg remains empty;
*      1> fseek/fread error, all ptrs in grib_hdr set to null; errmsg filled;
*      2> got end of file, all ptrs in grib_hdr set to null; errmsg filled;
*      3> Null entire_msg pointer; errmsg filled;
*      4> unable to open input file; errmsg filled;
*
**********************************************************************
*/

#if PROTOTYPE_NEEDED
int 	grib_seek (	char *InFile, long *offset, int Read_Index, 
			GRIB_HDR *gh, char *errmsg)
#else
int	grib_seek (InFile, offset, Read_Index,gh,errmsg)
			char *InFile; 
			long *offset; 
			int Read_Index; 
			GRIB_HDR *gh;
			char *errmsg;
#endif
{
   char *func="grib_seek";
   FILE  *fp=NULL;
   int status=0;

   DPRINT3 ("Entering %s\nfile=%s, offs=%ld \n", func, InFile, *offset);

/*
*
* A.2       OPEN Input file 
*           IF (fails) RETURN w/ error stat 4 !errmsg filled
*/
   if ((fp = fopen (InFile, "rb")) == NULL) {
     DPRINT2 ("%s: Cannot open input file %s\n", func,InFile );
     sprintf (errmsg,"%s: Cannot open input file '%s'\n", func,InFile );
     status = 4;
     goto DONE;
   }

   status = grib_fseek(fp,offset,Read_Index,gh,errmsg);

DONE:
/*
*
* A.4        CLOSE input file;     !get here when found no messages
*
*/
   if (fp) fclose(fp); 

/*
* A.6        RETURN with status 
*/
   return (status); 

}



#if PROTOTYPE_NEEDED
int 	grib_fseek (	FILE *fp, long *offset, int Read_Index, 
			GRIB_HDR *gh, char *errmsg)
#else
int	grib_fseek (fp, offset, Read_Index,gh,errmsg)
			FILE *fp; 
			long *offset; 
			int Read_Index; 
			GRIB_HDR *gh;
			char *errmsg;
#endif
 {
   char *func="grib_fseek";
   char  *GG, sm_blk[5004], *fwa_msg=NULL;
   unsigned long lMessageSize;
   unsigned long Edition;
   long pos;            /* current byte offs fr. beg. of file */
   int bytenum;         /* Index w/in sm_blk */
   int bytestoread=5004; /* #bytes to read into sm_blk at a time */
   int check_limit;     /* #bytes in sm_blk to check */ 
   int gotone = 0;	/* set if found good msg */
   int nread;           /* #bytes got back from Read */
   int status;
   unsigned long iskip;           /* for getbyte */
   int gdsbmsflag, bit_set, sect_len; /* working vars */
   char *ptr, *end_ptr;

   DPRINT2 ("Entering %s\n, offs=%ld \n", func, *offset);
/*
* A.1       INIT variables
*           !gh structure is cleared out
*/
   if (gh->entire_msg==NULL) {
	DPRINT1 ( "%s:  expecting non-null Grib Hdr;\n",func);
	sprintf(errmsg, "%s:  expecting non-NULL Grib Hdr;\n",func);
	status= 3;  
	goto DONE;
	}

   gh->msg_length = 0;
   gh->ids_ptr=gh->pds_ptr= 0;
   gh->gds_ptr=gh->bms_ptr=gh->bds_ptr=gh->eds_ptr=0;
   memset ((void *)gh->entire_msg, '\0', gh->abs_size);
   DPRINT2 ("gh= %ld, gh->entire_msg=%ld\n", gh, gh->entire_msg);

/*
*
* A.3       FOR (loop while no error)     !read a block at a time
*/
   
   for (status=0, pos= *offset, gotone= 0; status == 0; pos += check_limit)
   {
/*
* A.3.1        IF (cannot SET file position to correct place)
*              THEN
*                 SET Status to 1 !fseek err
*                 CONTINUE (Loop around to A.3)
*              ENDIF
*/
     if (fseek(fp, pos, 0)!=0) { 
	DPRINT2 ("%s: Got fseek error to pos= %ld\n",func, pos);
	sprintf(errmsg,"%s: Got fseek error to pos= %ld\n",func,pos);
	perror("");
	status = 1; 
	goto DONE; 
	}

/*
* A.3.2        IF (read less than 40 bytes)
*              THEN
*                 FILL error buffer
*                 RETURN status 2  !eof or <40 bytes left, errmsg filled
*              ENDIF
*/
     nread= fread (sm_blk,sizeof(char), bytestoread,fp);
     if (nread <= 40) 
	{ 
	  if (nread<=4) {
		DPRINT0 ("No bytes left to check for msg;\n"); 
		/* Errmsg left blank cuz its just EOF */
		}
	  else {
	    sprintf(errmsg,"%s: skip last %d bytes, too few for a Msg\n",
	    func, nread);
	    DPRINT1 ("Only read %d bytes, too few to check for msg;\n",nread); 
	    }
	  status= 2; 
	  goto DONE; 
	}
     else check_limit= nread - 4; 

/*
*              ! search block for the next the 'G'
*              ! load entire Msg if everything is ok;
*              ! if No 'G' found, then quit right away if no 'G'
*              ! if GRIB is not at absolute Offset address, quit too;
*              ! 
* A.3.3        WHILE (there is another 'G' in this block) DO
*/
     bytenum= 0;
     while ((GG= (char *) memchr (sm_blk, 'G', check_limit)))
	{
/*--- Saw 'G' ---*/
/*
* A.3.3.1          IF ('RIB' is not after 'G') THEN
*                     IF (Offset from Index file) THEN  
*                        ABORT search;   !Break out of loop
*                     ELSE
*                        CLEAR out the 'G' in temp block
*                        CONTINUE  !Loop around to A.3.3
*                     ENDIF
*                  ENDIF
*/
	   if (strncmp(GG, "GRIB",4))    /* not 'RIB' after the 'G' */
		if ( Read_Index) 
		   break;     /* Offset IS from Indexfile:  Quit here */
		else  
		{	      /* offset is NOT fr. IndexFile:  keep looping; */
		   *GG='-';   /* no RIB after G, clear it 		*/
		   continue;  /* let Memchr find next G in block      	*/
		}

/*--- Saw 'G R I B' ----*/
/*
* A.3.3.2           CALCULATE byte position within this block
*                   where this message begins
*/
	   bytenum = GG - sm_blk;  /* byte pos w/in this block */

/*
* A.3.3.3           IF (offset is from Indexfile  AND
*                       string GRIB found is not at Absolute IndexFile's offset)
*                   THEN abort search; ENDIF
*/
	  DPRINT1 ("Found string 'GRIB' at %ld\n", pos+bytenum);
	  if (Read_Index &&  *offset != (bytenum + pos))  {
	      sprintf(errmsg,
	      "%s:   No Grib msg found at offset= %ld;  check Index File\n",
	      func, *offset);
	      break;		/* Abort here, Ret w/ no errros & no msg too */
	    }

/*--- Read Mesg Length, Edition ---*/
/*
* A.3.3.4          FUNCTION gbyte !extract lMessageSize
*/
	  iskip=32;
          gbyte (sm_blk+bytenum ,&lMessageSize, &iskip,24);
          DPRINT0 ("lMessageSize\n");

/*--- Make sure it's Edition 1 first ---*/
/*
* A.3.3.5          FUNCTION gbyte !extract Grib Edition Number
*                  IF (not edition 1) THEN
*                      CLEAR out the 'G' in temp block
*                      CONTINUE  !Loop around to A.3.3
*                  ENDIF
*/
	  gbyte (sm_blk+bytenum, &Edition, &iskip, 8);
	  DPRINT0 ("Edition\n");
	  if (Edition != 1) {
	      DPRINT1 ("Edition (%d) is not 1, start over\n",
	      Edition);
              *GG='-';   /* blank out G of current GRIB location found */
              continue;  /* let Memchr find next G in block        */
	   }


/*
* A.3.3.6          IF (cannot MOVE ptr to start of the message) THEN
*                     RETURN status 1   !errmsg filled
*                  ENDIF
*/
          if (fseek(fp, (long)(pos+bytenum), 0)!=0) {
		DPRINT2 (
		"%s: FSEEK error to pos+bytenum= %ld\n",
		func, pos+bytenum);
		sprintf(errmsg,
		"%s: FSEEK error to pos+bytenum= %ld\n",
		func, pos+bytenum);
		status= 1;
		goto DONE;
		}

/*
* A.3.3.7          INIT all section length to zero
*/
	  gh->ids_len= gh->pds_len= gh->gds_len= 0;
	  gh->bds_len= gh->bms_len= gh->eds_len= 0;

/*
* A.3.3.8          EXPAND Entire_Msg array if it's smaller than msglen
*                  RETURN Malloc Err (stat=2) if fails  !errmsg filled
*/
	   if (lMessageSize > gh->abs_size  ) {
	      
/* 	      if (realloc((void *)gh->entire_msg, lMessageSize) == NULL) {..}
*	      gh->abs_size = lMessageSize;
*/
	      if (Expand_gribhdr (gh, lMessageSize, errmsg)) {
	          upd_child_errmsg (func, errmsg);
		  status = 1;   /* to get out of Outer loop */
		  goto DONE;
	       }

	      DPRINT1 ("Expanded entire_msg to be %ld bytes long\n",
	      gh->abs_size);
      	    } /* size changed */

/*--- READ ENTIRE MSG into GRIB HEADER's Entire_Msg ---*/
/*
*
* A.3.3.9          READ the entire message into Grib Hdr's Entire_Msg;
*                  IF (failed) THEN
*                      RETURN Fread error stat=1  !errmsg filled
*                  ENDIF
*/
	  fwa_msg = (char *)gh->entire_msg;
          if ((nread=fread (fwa_msg, 1, lMessageSize, fp)) != lMessageSize) 
	    {
            DPRINT2 ( "%s:  failed to Fread EntireMsg (sz=%ld)\n",
	    func, lMessageSize);
	    sprintf(errmsg,
            "%s has truncated msg @offs=%ld (got %ld out of %ld bytes)\n",
            func, *offset, nread, lMessageSize);
	    status= 1;  /* to get out of Outer loop */
	    goto DONE;  /* get out of WHILE */
	    }

/*--- if see '7777', asssign GH's pointers & len  ---*/
/*
* A.3.3.10         IF ('7777' is where expected) THEN
*/
          if (!strncmp((fwa_msg + lMessageSize - 4),"7777",4))
		/*  && fwa_msg[7]==1)  */
	  {  
 	        end_ptr = fwa_msg + lMessageSize;

		DPRINT0 ("Found string '7777' where expected\n");
		gh->msg_length= lMessageSize;
/*
* A.3.3.10.a.1         STORE loc & len of section 0 into Grib Hdr;
*/
		gh->ids_ptr= (unsigned char *)fwa_msg;    /* mark sect 0 */
		gh->ids_len= 8L;

/*
* A.3.3.10.a.2         STORE loc & len of PDS into Grib Hdr; 
*                     FUNCTION gbyte   !get 3-byte length
*/
	        ptr=  fwa_msg + gh->ids_len;
		gh->pds_ptr= (unsigned char *)ptr;            /* mark PDS */
		iskip= 0; gbyte(ptr ,(unsigned long *)&gh->pds_len,&iskip,24); 
		DPRINT0 (" pds length\n");
		iskip= 8*7; gbyte(ptr ,(unsigned long*)&gdsbmsflag, &iskip, 8);
		DPRINT1 (" (%x hex) Gds/Bms flag\n", gdsbmsflag);
/*
* A.3.3.10.a.3         IF (location of next Section  is out of bound) THEN
*                        PRINT message;
*                        GOTO drop this msg;
*                     ENDIF
*/
		ptr += gh->pds_len;
	        if (ptr > end_ptr) {
		    sprintf(errmsg,
		    "%s:  corrupt PDSlen= %ld, Totlen=%ld, drop msg @%ld;\n"
		    , func,  gh->pds_len, gh->msg_length, *offset);
		    gh->pds_len= 0;  		     /* reset */
		    goto DROPMSG_N_LOOP;
		    }

/*
*                     IF (Debug) FUNCTION hdr_print  !print PDS
*/
	        DPRINT1 ("gh->pds_len= %ld\n", gh->pds_len);
		HDR_PRINT("Grib_Seek's PDS",gh->pds_ptr, gh->pds_len);

/*
* A.3.3.10.a.4         IF (GDS is present) THEN
*                        STORE location & len of GDS into Grib Hdr's Gds_Ptr
*                        FUNCTION gbyte   !get 3-byte length
*                        IF (location of next Section  is out of bound) THEN
*                           PRINT message;
*                           DROP this msg & try to find another;
*                        ENDIF
*                        IF (Debug) FUNCTION hdr_print  !print GDS
*                     ENDIF
*/
		bit_set= gdsbmsflag >> 7 & 1;	/* mark GDS if present */
		if (bit_set) {
		    gh->gds_ptr= (unsigned char *)ptr;
		    iskip= 0; gbyte(ptr,(unsigned long*)&gh->gds_len,&iskip,24);
		    DPRINT0 (" Gds length\n");
		    ptr += gh->gds_len;	/* bump PTR to sect*/
	             if (ptr > end_ptr) {
		       sprintf(errmsg,
     		       "%s: corrupt GDSlen= %ld, Totlen=%ld, drop msg @%ld\n"
		        , func,  gh->gds_len, gh->msg_length, *offset);
		        gh->gds_len= 0;  		/* reset */
		        goto DROPMSG_N_LOOP;
     		        }
		       DPRINT1 ("gh->gds_len= %ld\n", gh->gds_len);
		       HDR_PRINT("Grib_Seek's GDS",gh->gds_ptr,gh->gds_len);
		    }

/*
* A.3.3.10.a.5         IF (BMS is present) THEN
*                        STORE location & len of BMS into Grib Hdr's Bms_Ptr
*                        FUNCTION gbyte   !get 3-byte length
*                        IF (location of next Section  is out of bound) THEN
*                           PRINT message;
*                           DROP this msg & try to find another;
*                        ENDIF
*                        IF (Debug) FUNCTION hdr_print   !byte dump
*                     ENDIF 
*/
		bit_set= gdsbmsflag >> 6 & 1;	/* mark BMS if present */
		if (bit_set) {
		    gh->bms_ptr= (unsigned char *)ptr; 	
		    iskip= 0; gbyte(ptr,(unsigned long*)&gh->bms_len,&iskip,24); 
		    DPRINT0 (" Bms length\n");

		    ptr += gh->bms_len;		/* bump PTR to sect */
	            if (ptr > end_ptr) {
		       sprintf(errmsg,
		       "%s: corrupt BMSlen= %ld, Totlen=%ld, drop msg @%ld\n"
			, func, gh->bms_len, gh->msg_length, *offset);
		        gh->bms_len= 0;  		/* reset */
		        goto DROPMSG_N_LOOP;
     		        }
		     DPRINT1 ("gh->bms_len= %ld\n", gh->bms_len);
		     HDR_PRINT ("Grib_Seek's BMS", gh->bms_ptr, 
				(gh->bms_len>100? 100: gh->bms_len));
		  }
/*
* A.3.3.10.a.6        STORE location and length of BDS into Grib Hdr's Bds_Ptr
*                    FUNCTION gbyte !get 3-byte length
*                    IF (location of next Section  is out of bound) THEN
*                        PRINT message;
*                        DROP this msg & try to find another;
*                    ENDIF
*                    IF (Debug) FUNCTION hdr_print   !byte dump
*/

		gh->bds_ptr= (unsigned char *)ptr; 	/* mark BDS */
		iskip= 0; gbyte(ptr,(unsigned long*)&gh->bds_len,&iskip,24);
		DPRINT0 (" Bds length\n");

		ptr += gh->bds_len;
	        if (ptr > end_ptr) {
		    sprintf(errmsg,
		    "%s:  corrupt BDSlen= %ld, Totlen=%ld, drop msg @%ld\n"
		    , func, gh->gds_len, gh->msg_length, *offset);
		    gh->gds_len= 0;  /* reset */
		    goto DROPMSG_N_LOOP;
		    }
		    DPRINT1 ("gh->bds_len= %ld\n", gh->bds_len);
	  	    HDR_PRINT ("Grib_Seek's BDS", gh->bds_ptr, 
			(gh->bds_len>100? 100: gh->bds_len));

/*
* A.3.3.10.a.7        STORE location & len of EDS into Grib Hdr's Eds_Ptr
*/
		gh->eds_ptr= (unsigned char *)ptr;  /* mark EDS */
		gh->eds_len= 4;   

/*
* A.3.3.10.a.8        SET 'gotone' flag 
*                    ! Return with Msg in Grib hdr, and good stat
*/
	    	gotone=1; 	/* to get out of FOR loop */
		status=0; 	/* Return with Msg , good stat */
	        goto DONE; 
/*
* A.3.3.10          ENDIF 
*/
	   } /* saw 77s  */
	else 
	if (Read_Index) {
		sprintf(errmsg,
		"%s:  no 7777 found for msg at %ld, check indexfile\n",
		func, *offset);
		}

/*
*
*          !====================================================
*          ! Drop Msg Area:   Only get here if :
*          ! - first G found not at Indexfile's offset;
*          ! - no RIB after G;
*          ! - GRIB string found not at Indexfile's offset;
*          ! - no 7777 at expected offset;
*          ! - got Corrupted Length; 
*          !====================================================
*/
DROPMSG_N_LOOP:
	   /* ERRMSG must already be loaded and the corrupted len reset to 0
	      so that Display GH won't go out of bound...
	    */
	   DPRINT1 ("\nDropping, cause=> %s\n", errmsg);

/*
* A.3.3.11         IF (Debug mode) THEN 
*                     FUNCTION display_gribhdr    !show what got loaded sofar
*                  ENDIF
*/
	  if (gh->msg_length > 0) 
	  DISPLAY_GRIBHDR(gh); /* before dropping msg*/

/*
*                  ! no message found yet, OR Msg Section lens are corrupted 
* A.3.3.12         CLEAR out header struct   !data in array is not valid
*/      
   	  gh->msg_length =0;
	  gh->ids_ptr=gh->pds_ptr= gh->eds_ptr=0;
   	  gh->gds_ptr=gh->bms_ptr=gh->bds_ptr= 0;
   	  memset ((void *)gh->entire_msg, '\0', gh->abs_size);

/*
* A.3.3.13         IF (Offset was read from Indexfile)   !quit searching
* A.3.3.13.a       THEN
*                      PUT Error msg in buffer
*                      RETURN with No Error status
* A.3.3.13.b       ELSE
*                      CLEAR out 'G' in tmp block        !go find next 'G' 
*                  ENDIF
*/
	  if (Read_Index) {
	      status = 0; 	/* Ret w/ no errors     */
	      goto DONE;	/* send CAUSE back in Errmsg */
	     }
	  else {
	     *GG='-';    /* let Memchr find next 'G' in curr. block */
	     DPRINT1 ("'GRIB' at location %ld is not a valid message\n",
	     bytenum+pos);
	     errmsg[0]='\0';  /* clear out buff */
	     }
/*
* A.3.3         ENDWHILE 
*/
     }  /* WHILE seeing 'G'*/


     if (Read_Index) {  /* Catch 3 cases:
			 - if no 'G' at all ;
			 - if no RIB after G ;
			 - if found GRIB but not at expected place
			*/
	   sprintf(errmsg,
	   "%s:  No Grib Msg found at IndexFile's offset = %ld; "\
	   " Check Index File\n" , func, *offset);
	   status = 0;  /* Return w/no errors  */
	   goto DONE;	/* but w/ Warn Msg in buff*/
	}

/*
A.3.4           DEBUG print  !no Sect0 found in this block
*/
      DPRINT2 ("No Section 0 found between address %ld and %ld\n", 
      pos, pos+check_limit);
/*
*
* A.3       ENDFOR    !Outer Loop,  stay until Status changes
*/
  }  /* check entire file */



DONE:

/*
* 
* A.5        IF (found a msg) THEN
*                BUMP caller's Offset to absolute Begining of Msg found;
*                DEBUG Print
*            ENDIF
*/
   if (gotone)  
       {
       *offset = (long)(pos+bytenum);  /* bump offset to abs. beg. of Msg */
       DPRINT3 ("Exiting %s w/stat=%d, offs=%d, msg in GRIB_HDR\n", 
	func, status, *offset);
       }
   else DPRINT2 ("Exiting %s w/stat=%d, no messages\n", func, status);

/*
* A.6        RETURN with status 
*/
   return (status); 
/*
*
* END OF FUNCTION
*
*/
}
