/*
file:  ld_enc_input.c   (Ld_enc_config, Ld_enc_ieeeff, Ld_enc_ffinfo)
Original version from previous Encoder Library;
Revisions:
10/28/96 by Alice T. Nakajima, SAIC
	replaced hard-coded $CONFIG_PATH[]/input.dat, 
	now passes in the name of the file (with absolute path)
	to ld as well as the Usr Input struct;
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*********************************************************************
* A. FUNCTION:  ld_enc_config
*      fill struct holding user's input from config_fn that
*      is passed in by user
*
*    INTERFACE:
*      int     ld_enc_config (config_fn, User_Input, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *config_fn;         name of file to load from;
*      (O)  USER_INPUT *User_Input;  filled with data read from file;
*      (O)  char *errmsg             returned filled if error occurred;
*
*    RETURN CODE:   
*       0>  success, file is read and closed, user_input is filled 
*       1>  error opening file;  errmsg filled;
*       2>  failed to get all expected arguments; errmsg filled;
*       3>  ferror in file; errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int  ld_enc_config (char *config_fn, USER_INPUT *User_Input, char *errmsg)
#else
int  ld_enc_config (config_fn, User_Input, errmsg)
		char *config_fn; USER_INPUT *User_Input; char *errmsg;
#endif
{
/* 
*
* A.1       DEFAULT to error stat=1;
*/
  char		 *func="ld_enc_config"; /* name of Func */
  char           temp[100], dummy[100], line[200], *p1;  
  int		 stat= 1;
  int		 linenum=1;  /* number of lines got from the files */
  int            num_expected= 9;  /* expecting 9 args from file */
  FILE           *infile;   /* user input file */

  DPRINT1 ("Entering %s\n", func);
/*
*
* A.2       OPEN Encoder Config file for reading
*/
  infile = fopen(config_fn, "r");
  if (infile==NULL) { 
	DPRINT2 ("%s: Failed to open %s\n", func,config_fn);
	sprintf (errmsg,"%s: Failed to open %s\n", func,config_fn);
	goto BYE;  
	}

  DPRINT2 ("loading %d args from file:\n'%s'\n", num_expected, config_fn);

/*
*
* A.3       WHILE (still more lines AND no error yet) DO
*/          
  while (!feof(infile) && !ferror(infile)) 
  {
/*
* A.3.1        GET a line from the file, quit loop if failed;
*              !format:   value  opt_comments
* A.3.2        IF line is empty OR is a comment, Loop again;
*/          
        if (fgets (line, sizeof(line)-1, infile) == NULL)  break;
	for (p1=line; *p1 && (iscntrl(*p1)|| isspace(*p1)); p1++);
	if (p1==NULL || *p1=='#') continue;
/*
* A.3.3        EXTRACT first non-space argument
*              IF (fails)  QUIT;
*              ELSE convert argument into a number
*/
        if (sscanf (line, "%s%s", temp, dummy) < 1) {
          DPRINT1 ( "%s:  failed to extract 1 arg from line\n", func);
          sprintf (errmsg, "%s:  failed to extract arg from line:\n%s", 
	  func, line); 
	  break; 
	  }

	DPRINT2("  case %d, data=%s:  ", linenum, temp);

/*
* A.3.4        SWITCH (what line number we're on)
*              ... USER_INPUT info ...
*                  line  1:  fill User_Input->chCase_id
*                  line  2:  fill User_Input->usParm_tbl
*                  line  3:  fill User_Input->usSub_tbl, ->usZero (oct26)
*                  line  4:  fill User_Input->usCenter_id
*                  line  5:  fill User_Input->usCenter_sub
*                  line  6:  fill User_Input->usTrack_num
*                  line  7:  if 1, set (0x80) bit of User_Input->usGds_bms_id
*                  line  8:  if 1, set (0x40) bit of User_Input->usGds_bms_id
*                  line  9:  filll User_Input->usBit_pack_num
*                  else   :  print skip line msg
*              ENDSWITCH
*/

        switch (linenum++) {

	/* USER_INPUT info: */
         case  1: User_Input->chCase_id= temp[0]; 
		  P_CHAR (User_Input->chCase_id); break;
         case  2: User_Input->usParm_tbl= (unsigned short)atoi(temp); 
		  P_USHORT (User_Input->usParm_tbl); break;
         case  3: User_Input->usSub_tbl= (unsigned short)atoi(temp);
		  P_USHORT (User_Input->usSub_tbl ); break;
         case  4: User_Input->usCenter_id= (unsigned short)atoi(temp);
		  P_USHORT (User_Input->usCenter_id ); break;
         case  5: User_Input->usCenter_sub= (unsigned short)atoi(temp);
		  P_USHORT (User_Input->usCenter_sub ); break;
         case  6: User_Input->usTrack_num= (unsigned short)atoi(temp);
		  P_USHORT (User_Input->usTrack_num ); break;
         case  7: if (atoi(temp)) 
		  User_Input->usGds_bms_id=0x80; 
		  P_USHORT (User_Input->usGds_bms_id ); break;
         case  8: if (atoi(temp)) 
		  User_Input->usGds_bms_id +=(unsigned short)0x40;
		  P_USHORT (User_Input->usGds_bms_id ); break;
         case  9: User_Input->usBit_pack_num= (unsigned short)atoi(temp);
		  P_USHORT (User_Input->usBit_pack_num ); break;
         default: fprintf(stdout,
		  "%s Warning: excess line from Configfile skipped=\n%s\n", 
		  func, line); 
		  break;
	 }
/*
* A.3       ENDWHILE !more to read
*/
    } /* while */

/*
*
* A.4       IF (got a reading error) THEN
*              RETURN Stat 3
*           ENDIF 
*/
  if (ferror(infile))  {
        DPRINT1 ( "%s:  got ferror(infile)\n", func);
        sprintf(errmsg, "%s: got ferror(infile)\n", func);
	stat=3; 
	}

/*
*
* A.5       CLOSE the input file
*/
  if (infile) fclose (infile);

/*
*
* A.6       IF (only received less than #required arguments) THEN
*               PRINT warning
*           ELSE
*               CHANGE return status to no errors
*           ENDIF
*/
  if (linenum-1 < num_expected) 
    {
     DPRINT4 ("%s: failed to load %s (%d/%d)\n", 
     func, config_fn, linenum-1, num_expected);
     sprintf(errmsg, "%s: failed to load %s (%d/%d)",
     func, config_fn, linenum-1, num_expected);
     stat= 2;
     }
  else  stat = 0;

/*
*
* A.7       RETURN with stat
*/
BYE:
  DPRINT2 ("Leaving %s, stat=%d;\n", func,stat);
  return (stat);
/*
*
* END OF FUNCTION 
*
*
*/ 
}

/*
*********************************************************************
* B. FUNCTION:  ld_enc_ieeeff
*       load user's pre-malloced float array with data from
*       binary flat file passed in by user (ie:  FF*);
*
*    INTERFACE:
*       int     ld_enc_ieeeff (ieee_fn, farr, elements, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) char *ieee_fn   name of IEEE Flat file (w/fullpath) to read
*      (O) float *farr     pre-malloced array to store data from read from file
*      (I) int elements;   number of float elements to read from file;
*      (O) char *errmsg    returned filled if error occurred;
*
*    RETURN CODE:   
*       0>  success, file is read and closed, float arr is filled 
*       1>  error opening file; errmsg filled;
*       2>  failed to get all expected elements;  errmsg filled;
*       3>  incoming float array is null;  errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int	ld_enc_ieeeff ( char 	*ieee_fn,
			float 	*farr,
			int 	elements,
			char	*errmsg)
#else
int	ld_enc_ieeeff ( ieee_fn, farr, elements, errmsg)
			char 	*ieee_fn;
			float 	*farr;
			int 	elements;
			char	*errmsg;
#endif
{
/* 
*
* B.1       DEFAULT no error stat=0
*/
  char		 *func="ld_enc_ieeeff";
  FILE           *infile= 0;
  int		 stat= 0;


  DPRINT1 ("Entering %s\n",func);
/* 
*
* B.2       IF (float array is null) THEN
*               SET stat =3
*                RETURN
*           ENDIF
*/
  if (farr == NULL) {
	DPRINT1 ("%s:  unexpected null Float array\n", func);
	sprintf (errmsg,"%s:  unexpected null Float array\n", func);
	stat = 3; goto BYE;
	}
/*
*
* B.3       OPEN the IEEE file for reading   !was 'temp.dat'
*           IF (failed) RETURN err 1;
*/
  infile = fopen(ieee_fn, "r");
  if (infile==NULL) 
	{ DPRINT2 ("%s: Failed to open %s\n", func,ieee_fn);
	  sprintf (errmsg,"%s: Failed to open %s\n", func,ieee_fn);
	  stat = 1; goto BYE;
	}
  DPRINT1 ("Read %s\n", ieee_fn);

/*
*
* B.4       READ float data from file
*           IF (didn't get all) THEN
*              SET status to 2
*              RETURN
*           ENDIF
*/          
      if ( fread(farr, sizeof(float), elements, infile) != elements)
        { 
        DPRINT3 ( "%s: failed to load %s (expecting %d float elements)\n",
        func, ieee_fn, elements);
        sprintf(errmsg,"%s: failed to load %s (expecting %d float elements)\n",
        func, ieee_fn, elements);
	stat= 2; 
	}
      else DPRINT1 ("Number of float elements read = %d\n", elements);

BYE:
/*
*
* B.5       CLOSE the input file 
*/
  if (infile) fclose (infile);

/*
*
* B.6       RETURN with stat
*/
  DPRINT2 ("Leaving %s, stat=%d\n", func,stat);
  return (stat);
/*
*
* END OF FUNCTION 
*
*
*/ 
}

/*
*********************************************************************
* C.  FUNCTION:  ld_enc_ffinfo
*      fill DATA_INPUT struct from file whose name is passed in
*
*    INTERFACE:
*      int  ld_enc_ffinfo (ieee_info_fn, Data_Input, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *ieee_info_fn      name of config info file to load
*      (O)  DATA_INPUT *Data_Input  to be filled with data read from config file
*      (O)  char *errmsg            filled if error occurred;
*
*     RETURN CODE:   
*       0;  success, file is read and closed, struct is filled 
*       1:  error opening file; 
*       2:  failed to get all expected arguments;
*       3:  Ferror;
**********************************************************************/

#if PROTOTYPE_NEEDED
int	ld_enc_ffinfo ( char 		*ieee_info_fn,
			DATA_INPUT  	*Data_Input,
			char		*errmsg)
#else
int	ld_enc_ffinfo ( ieee_info_fn, Data_Input, errmsg)
			char 		*ieee_info_fn;
			DATA_INPUT  	*Data_Input;
			char		*errmsg;
#endif
{
/* 
*
* C.1       DEFAULT to no error stat= 0;
*/
  int		 stat= 0;
  FILE           *infile;   /* user input file */
  char		 *func="ld_enc_ffinfo";
  char           temp[100], dummy[100], line[200], *p1;  
  int		 linenum=1;  /* number of lines got from the files */
  int            num_expected= 13;  /* expecting 13 args from file */
  int            mdl_indx, i, iTemp;

  DPRINT1 ("Entering %s\n",func);

/*
*
* C.2       OPEN input file for reading
*/
  infile = fopen(ieee_info_fn, "r");
  if (infile==NULL) { 
	DPRINT2 ("%s: Failed to open %s\n", func,ieee_info_fn);
	sprintf (errmsg,"%s: Failed to open %s\n", func,ieee_info_fn);
	stat=1; goto BYE;  
	}

/*
*
* C.3       WHILE (still more lines AND no error yet) DO
* C.3.1        GET a line from the file, quit loop if failed;
*              !format:   value  opt_comments
* C.3.2        IF line is empty OR is a comment, Loop again;
*/          
  while (!feof(infile) && !ferror(infile) ) 
   {
        if (fgets (line, sizeof(line)-1, infile) == NULL)  break;
	for (p1=line; *p1 && (iscntrl(*p1)|| isspace(*p1)); p1++);
	if (p1==NULL || *p1=='#') continue;
/*
* C.3.3        EXTRACT arguments from line
*              IF (fails)  QUIT;
*              ELSE convert the 2nd argument into an integer 
*/
        if ((i=sscanf (line, "%s", temp)) != 1)
        { 
	  DPRINT2( "%s:  failed to extract 1 arg reading %s\n", 
	  func,ieee_info_fn); 
	  sprintf(errmsg, "%s:  failed to extract 1 arg reading %s\n", 
	  func,ieee_info_fn); 
	  break; 
	}
	else iTemp = atoi(temp);
	DPRINT2("  case %d, value=%d:  ", linenum, iTemp);

/*
* C.3.4        SWITCH (what line number we're on)
*                  line  1: fill Data_Input->usProc_id field;
*                  line  2: fill Data_Input->usGrid_id field;
*                  line  3: fill Data_Input->usParm_id field;
*                  line  4: fill Data_Input->usParm_sub_id field;
*                  line  5: fill Data_Input->usLevel_id field;
*                  line  6: fill Data_Input->nLvl_1 field;
*                  line  7: fill Data_Input->nLvl_2 field;
*                  line  8: fill Data_Input->nYear field;
*                  line  9: fill Data_Input->nMonth field;
*                  line 10: fill Data_Input->nDay field;
*                  line 11: fill Data_Input->nHour field
*                  line 12: fill Data_Input->usFcst_per1 field;
*                  line 13: fill Data_Input->nDec_sc_fctr field;
*              ENDSWITCH
*/
        switch (linenum++) {
         case  1: Data_Input->usProc_id = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usProc_id); break;
         case  2: Data_Input->usGrid_id = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usGrid_id); break;
         case  3: Data_Input->usParm_id = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usParm_id); break;
         case  4: Data_Input->usParm_sub_id = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usParm_sub_id); break;
         case  5: Data_Input->usLevel_id = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usLevel_id); break;
         case  6: Data_Input->nLvl_1 = iTemp;
		  P_INT (Data_Input->nLvl_1); break;
         case  7: Data_Input->nLvl_2 = iTemp;
		  P_INT (Data_Input->nLvl_2); break;
         case  8: Data_Input->nYear = iTemp;
		  P_INT (Data_Input->nYear); break;
         case  9: Data_Input->nMonth = iTemp;
		  P_INT (Data_Input->nMonth); break;
         case 10: Data_Input->nDay = iTemp;
		  P_INT (Data_Input->nDay); break;
         case 11: Data_Input->nHour = iTemp;
		  P_INT (Data_Input->nHour); break;
         case 12: Data_Input->usFcst_per1 = (unsigned short) iTemp;
		  P_USHORT (Data_Input->usFcst_per1); break;
         case 13: Data_Input->nDec_sc_fctr = iTemp;
		  P_INT (Data_Input->nDec_sc_fctr); break;
         default: fprintf(stdout,
		  "%s Warning: excess line from file skipped=\n%s\n", 
		  func, line); 
		  break;
        }
/*
* C.3       ENDWHILE !more to read
*/
    } /* while */

/*
*
* C.4       IF (got a reading error) THEN
*              RETURN Stat 3
*           ENDIF 
*/
  if (ferror(infile))  {
        DPRINT1 ( "%s:  got ferror(infile)\n", func);
        sprintf(errmsg, "%s: got ferror(infile)\n", func);
	fclose(infile);
	stat=3; goto BYE;
	}

/*
*
* C.5       CLOSE the input file
*/
  fclose (infile);

/*
*
* C.6       IF (only received less than #required arguments) THEN
*               PRINT warning
*/
  if (linenum-1 < num_expected) {
     DPRINT4 ( "%s: failed to load %s (%d/%d)\n",
     func, ieee_info_fn, linenum-1, num_expected);

     sprintf(errmsg, "%s: failed to load %s (%d/%d)\n",
     func, ieee_info_fn, linenum-1, num_expected);
     stat= 2;
     }
/*
*           ELSE 
*              HARDCODE usFcst_id to 1 (code for Hours)
*           ENDIF
*/
  else {
     Data_Input->usFcst_id        = 1;    /* Fcst Time Unit default to Hours */
     DPRINT0("Hard-Code HOURS -> "); P_USHORT (Data_Input->usFcst_id);
    }

/*
*
* C.7       RETURN with stat
*/
BYE:
  DPRINT2 ("Leaving %s(), stat=%d;\n", func, stat);
  return (stat);
/*
*
* END OF FUNCTION 
*
*
*/ 
}
/*
*********************************************************************
* D.  FUNCTION:  ld_enc_geomfile
*       fill GEOM_IN struct from file whose name is passed in.
*     
*    INTERFACE:
*      int     ld_enc_geomfile (geom_fn, Geom_In, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *geom_fn          name of geom info file to load
*      (O)  GEOM_IN *Geom_In       to hold geom info read from file 
*      (O)  char  *errmsg          returned filled if error occurred
*
*     RETURN CODE:   
*       0>  success, file is read and closed, struct is filled 
*       1>  error opening file;  errmsg filled;
*       2>  failed to get all expected arguments; errmsg filled;
*       3>  Ferror; errmsg filled;
**********************************************************************
*/
#if PROTOTYPE_NEEDED
int	ld_enc_geomfile (char *geom_fn, GEOM_IN *Geom_In, char	*errmsg)
#else
int	ld_enc_geomfile (geom_fn, Geom_In, errmsg)
			char *geom_fn; 
			GEOM_IN *Geom_In; 
			char	*errmsg;
#endif
{
/* 
*
* D.1       DEFAULT to no error stat= 0;
*/
  int		 i, stat= 0;
  FILE           *infile;   /* user input file */
  char		 *func="ld_enc_geomfile";
  char           temp[100], dummy[100], line[200], *p1;
  int		 linenum=1;  /* number of lines got from the files */
  int            num_expected= 15;  /* expecting 15 args from file */

  DPRINT1 ("Entering %s\n",func);
/*
*
* D.2       OPEN input file for reading
*/
  infile = fopen(geom_fn, "r");
  if (infile==NULL) { 
	DPRINT2 ("%s: Failed to open %s\n", func,geom_fn);
	sprintf (errmsg,"%s: Failed to open %s\n", func,geom_fn);
	stat=1; goto BYE;  
	}

/*
*
* D.3       WHILE (still more lines AND no error yet) DO
* D.3.1        GET a line from the file, quit loop if failed;
*              !format:   value  opt_comments
* D.3.2        IF line is empty OR is a comment, Loop again;
*/          
  while (!feof(infile) && !ferror(infile) && linenum <= num_expected) 
  {
        if (fgets (line, sizeof(line)-1, infile) == NULL)  break;
	for (p1=line; *p1 && (iscntrl(*p1)|| isspace(*p1)); p1++);
	if (p1==NULL || *p1=='#') continue;
/*
* D.3.3        EXTRACT non-space arguments from line
*              !format:   value  opt_comments
*              IF (fails)  set Stat to error 2
*              ELSE convert the 2nd argument into an integer 
*/
        if (sscanf (line, "%s%s", temp, dummy) < 1) {
          DPRINT3 ("%s:  failed to extract arg from line %d of %s\n", 
	  func, linenum, geom_fn); 
          sprintf(errmsg,"%s:  failed to extract arg from line %d of %s\n", 
	  func, linenum, geom_fn); 
	  stat=2;
	  break;
	}

	DPRINT2("  case %2d val=%10s: ", linenum, temp);
/*
* D.3.4        SWITCH (what line number we're on)
*              ... GEOM_IN info ...
*                  line  1:  fill Geom_In->prjn_name
*                  line  2:  fill Geom_In->nx
*                  line  3:  fill Geom_In->ny
*                  line  4:  fill Geom_In->x_int_dis
*                  line  5:  fill Geom_In->y_int_dis
*                  line  6:  fill Geom_In->parm_1
*                  line  7:  fill Geom_In->parm_2
*                  line  8:  fill Geom_In->parm_3
*                  line  9:  fill Geom_In->first_lat
*                  line 10:  fill Geom_In->first_lon
*                  line 11:  fill Geom_In->last_lat
*                  line 12:  fill Geom_In->last_lon
*                  line 13:  fill Geom_In->scan
*              ... More info (previously in file input.dat)...
*                  line 14:  if 1, set (0x40) bit of Geom_In->usRes_flag
*                  line 15:  if 1, set (0x08) bit of Geom_In->usRes_flag
*                  else   :  print skip line msg
*              ENDSWITCH
*/
        switch (linenum++) 
        {
	/* ...Geometry info */
         case  1: strcpy (Geom_In->prjn_name, temp);
		  P_STRING (Geom_In->prjn_name); break;
         case  2: Geom_In->nx = (long) atoi(temp);
		  P_INT (Geom_In->nx); break;
         case  3: Geom_In->ny = (long) atoi(temp);
                  P_INT (Geom_In->ny); break;
         case  4: Geom_In->x_int_dis= (double) atof(temp);
		  P_DOUBLE (Geom_In->x_int_dis); break;
         case  5: Geom_In->y_int_dis= (double) atof(temp);
                  P_DOUBLE (Geom_In->y_int_dis); break;
         case  6: Geom_In->parm_1= (double) atof(temp);
		  P_DOUBLE (Geom_In->parm_1); break;
         case  7: Geom_In->parm_2= (double) atof(temp);
                  P_DOUBLE (Geom_In->parm_2); break;
         case  8: Geom_In->parm_3= (double) atof(temp);
                  P_DOUBLE (Geom_In->parm_3); break;
         case  9: Geom_In->first_lat= (double) atof(temp);
		  P_DOUBLE (Geom_In->first_lat); break;
         case 10: Geom_In->first_lon= (double) atof(temp);
                  P_DOUBLE (Geom_In->first_lon); break;
         case 11: Geom_In->last_lat= (double) atof(temp);
		  P_DOUBLE (Geom_In->last_lat); break;
         case 12: Geom_In->last_lon= (double) atof(temp);
                  P_DOUBLE (Geom_In->last_lon); break;
         case 13: Geom_In->scan= (unsigned short) atoi(temp);
                  P_USHORT (Geom_In->scan); break;

	/*... Misc. info */
         case 14: if (atoi(temp)) 
		  Geom_In->usRes_flag =(unsigned short)0x40;
		  P_USHORT (Geom_In->usRes_flag ); break;
         case 15: if (atoi(temp)) 
		  Geom_In->usRes_flag +=(unsigned short)0x08;
		  P_USHORT (Geom_In->usRes_flag ); break;
         default: fprintf(stdout,
		  "%s Warning: excess line from file skipped=\n%s\n", 
		  func, line); 
		  break;
        }
/*
* D.3       ENDWHILE !more to read
*/
    } /* while */

/*
*
* D.4       IF (got a reading error) THEN
*              RETURN Stat 3
*           ENDIF 
*/
  if (ferror(infile))  {
        DPRINT1 ( "%s:  got ferror(infile)\n", func);
        sprintf(errmsg, "%s: got ferror(infile)\n", func);
	fclose(infile);
	stat=3; goto BYE;
	}

/*
*
* D.5       CLOSE the input file
*/
  fclose (infile);
  
/*
*
* D.6       IF (Status is Good ) THEN
*               IF (received less than #required arguments) PRINT warning
*           ENDIF
*/
  if (stat == 0 ) {
     if (linenum <= num_expected) {
        DPRINT3 ( "%s: only loaded %d/%d args into Geom_In\n", 
        func, linenum-1, num_expected);

        sprintf(errmsg, "%s: only loaded %d/%d args into Geom_In\n", 
        func, linenum-1, num_expected);
        stat= 2;
        }
     else DPRINT1 ("Got all %d arguments; \n", num_expected);
   }

/*
*
* D.7       RETURN with stat
*/
BYE:
  DPRINT2 ("Leaving %s, stat=%d;\n", func, stat);
  return (stat);
/*
*
* END OF FUNCTION 
*
*
*/ 
}
