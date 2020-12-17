/*  FILENAME:   ld_dec_lookup.c 
Revision logs:
16Jul97 /atn: clear Decoder section of structs only; chg warnings to Stdout;
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "grib_lookup.h"  	/* combined lookup structs */
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*.....................................................
*  ld_dec_lookup.c defines the following global vars:
*.....................................................
*  PARM_DEFN  db_parm_tbl[NPARM*MAX_PARM_TBLS]   Parameter Conversion info
*  LVL_DEFN   db_lvl_tbl[NLEV]                  Level Conversion info
*  MODEL_DEFN db_mdl_tbl[NMODEL]                 Model Conversion info
*  GEOM_DEFN  db_geom_tbl[NGEOM]                 Geom Conversion info
*              
*/
PARM_DEFN   db_parm_tbl [NPARM*MAX_PARM_TBLS];/* GLOBVAR parm conversion info*/
LVL_DEFN    db_lvl_tbl [NLEV];    /* GLOBVAR level conversion info */
MODEL_DEFN  db_mdl_tbl [NMODEL];  /* GLOBVAR model conversion info */
GEOM_DEFN   db_geom_tbl [NGEOM];  /* GLOBVAR Geom conversion info */

/*
**********************************************************************
* A.  FUNCTION:      ld_dec_lookup
*        This function reads in the information from an external Lookup
*        table (ie: g1tab_2.1).  This info is used to convert
*        from the Database's parameter names to the GRIB Code Numbers.
*
*    INTERFACE:
*       int ld_dec_lookup (lookup_fn,  errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *lookup_fn
*           Name of Lookup file to read from (ie:  /abspath/g1tab_128_2.1)
*      (O)  char *errmsg
*           empty array, returned filled if error occurred;
*
*     RETURN CODE:
*        0>  successful, the 4 database tables Filled;
*        1>  file open error or got error/eof while reading;  errmsg filled;
**********************************************************************
*
-    REQUIREMENTS:  *** do not use the TAB character !!! ***
-     Rules for creating Decoder Lookup file:
-     a)  lines starting out with  '#' is Comment lines & are skipped;   
-     b)  the tables within the file are defined in this order:
-         'GRIB Table 2',
-         'GRIB Table 2 - Sub A',
-         'GRIB Table 2 - Sub B',
-         'GRIB Table 2 - Sub C',
-         'GRIB Table 2 - Sub D',
-         'GRIB Table 2 - Sub E',
-         'GRIB Table 3: Level Definitions',
-         'GRIB Table - Generating Process Definitions (Octet 6 of PDS)',
-         'GRIB Table - Pre-defined geometries (Octet 7 of PDS)'
-     c)  Each Header section MUST start out with "GRIB Table";
-         All Header Section except that of the LEVEL tbl MUST end with a line 
-         containing atleast 4 consecutives '=';
-     e)  Header lines are any number of lines before the '===' line which is
-         considered as a the last line of this section's Header;
-     f)  the Parameter defn (Table 2 & subTables) must have at least 2 spaces
-         between the Field Parameter and Unit fields;
-
-     While getting entries for current table, the program assumes it has
-     gotten to the end of current Table if Hdr SEction for next Table 
-     (string "GRIB Table").
*/

#if PROTOTYPE_NEEDED
int  ld_dec_lookup ( char *lookup_fn, char *errmsg)
#else
int  ld_dec_lookup ( lookup_fn, errmsg)
		     char *lookup_fn; 
		     char *errmsg;
#endif
{
  FILE  *infile;
  char  *func="ld_dec_lookup", line[200], temp[200], dummy;
  char  *ptr2, *ptr, strGribCode[50], grib_dsc[150], grib_unit_dsc[150]; 
  char  strScale[50], strOffset[50], strDSF[50];
  char  lvl_name_1[150], lvl_name_2[150];
  int   LineRead, indx, sub, stat=1, num, cnt, iOctets;
  char  strOctets[30];  
  int	GribCode;
  PARM_DEFN *parmptr;	/* ptr to cell w/in Parm array */

  DPRINT1 ("Entering %s\n", func);
/*
* A.0       CLEAR out all the lookup arrays's decoder part
*/
  for (num=0; num < NPARM ; num++) {
	db_parm_tbl[num].usParm_id=  num;
	db_parm_tbl[num].usParm_sub= 0;       /* not used for main tbl */
	db_parm_tbl[num].grib_dsc[0] ='\0';
	db_parm_tbl[num].grib_unit_dsc[0] ='\0';
	}
  for (num=NPARM; num < NPARM * MAX_PARM_TBLS; num++) {  /* for sub-tbls */
	db_parm_tbl[num].usParm_id=  249 + num / NPARM;
	db_parm_tbl[num].usParm_sub= num % NPARM;
	db_parm_tbl[num].grib_dsc[0] ='\0';
	db_parm_tbl[num].grib_unit_dsc[0] ='\0';
	}

  for (num=0; num < NLEV; num++) {
        db_lvl_tbl[num].usLevel_id =  num;
	db_lvl_tbl[num].grib_dsc[0] = '\0';
	db_lvl_tbl[num].lvl_name_1[0] = '\0';
	db_lvl_tbl[num].lvl_name_2[0] = '\0';
	db_lvl_tbl[num].num_octets = 0;
	}

  for (num=0; num < NMODEL; num++) {
	db_mdl_tbl[num].usModel_id  = num;
	db_mdl_tbl[num].grib_dsc[0] = '\0';
	}

  for (num=0; num < NGEOM; num++) {
	db_geom_tbl[num].usGeom_id  = num;
	db_geom_tbl[num].grib_dsc[0] = '\0';
	}

/*
*
* A.1       OPEN Lookup file for reading
*           RETURN 1 if fails;
*/
  infile = fopen(lookup_fn, "r");
  if (infile==NULL) { 
	DPRINT2 ("%s:  failed to open %s\n", func, lookup_fn);
	sprintf (errmsg ,"%s:  failed to open %s\n", func, lookup_fn);
	goto BYE;
	}
  DPRINT1 ("Loading Decoder file= '%s'\n", lookup_fn);

/**** PARM SECTION (TABLE 0/A/B/C/D/E) ***
 To be loaded continuously where Main tbl range is 0-255, B is 256-511, ...
Sample:
   GRIB Table 2
   Code Figure    Field Parameter                                       Unit
   ===========    ===============                                       ====
   000            Reserved                                              *
   001            Pressure                                              Pa
   002            Pressure reduced to MSL                               Pa
   003            Pressure tendency                                     Pa/s
   #####################################################################
   GRIB Table 2 - Sub A
   Code Figure    Field Parameter                                       Unit
   ===========    ===============                                       ====
   ######################################################################
   GRIB Table 2 - Sub B
   Code Figure    Field Parameter                                       Unit
   ===========    ===============                                       ====
   ######################################################################
   GRIB Table 2 - Sub C
   Code Figure    Field Parameter                                       Unit
   ===========    ===============                                       ====
   ######################################################################
   GRIB Table 2 - Sub D
   Code Figure    Field Parameter                                       Unit
   ===========    ===============                                       ====
   ######################################################################

*
*           *** Parameter Conversion info ***
* A.2       FOR (each Parameter Table/subTable to load) DO
*/
 LineRead = 0;
 for (sub=0; sub < 6; sub++)   { /* 6 Parm tables altogether (0/A/B/C/D/E) */
/*
* A.2.1       CALCULATE the index offset for this Table within the Parm array
*/
     indx = sub * 256; 

/*
* A.2.2       KEEP reading until end of comment line (line with '====')
*             RETURN error if fails
*/
     /* Read until last of Header line */
     for (line[0]='\0'; ! strstr(line,"====") ; ) 
     {
       fgets(line, sizeof(line), infile);  ++LineRead;
       if (feof(infile) || ferror(infile)) 
         { sprintf(errmsg,
	   "%s: got EOF/ERROR before PARM TABLE #%d info (Line %d in %s)\n", 
	    func, sub, LineRead, lookup_fn);
	   goto BYE;
         }
      }

/*
* A.2.3       FOR (successfully read a line from file) DO
*                LOOP if it's a comment line
*                BREAK out if already got to next Section (see "GRIB Table")
*                DROP line if it's a empty line
*/
  DPRINT2("*** %s: Start reading Tbl2- sub#%d ***\n", func, sub);
  for (cnt=0 ; fgets(line, sizeof(line), infile)!=NULL; ) 
    { 
        ++LineRead;
       /* skip additional comments,  Break if already got to next Table Defn,
	else replace tabs with spaces, newlines with null 
       */
	for (ptr=line; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
        if (strstr(line, "GRIB Table ") != NULL) break;  /* END OF CURR SECT */
        while (ptr=strchr(line,'\t')) *ptr=' '; 
        if (ptr=strchr(line,'\n')) *ptr='\0'; 
/*
*                EXTRACT line partially  !Parmid, 1st word of Description
*                DROP line out if extraction fails;
*                DROP line if parmid is invalid or out or range
*                >> Note: valid parm id range is 0-255 for main table, 
*                1-255 for sub tables.
*/
	/* DO a partial read, get parm_id and 1st word of Description */
    	if ((num=sscanf (line,"%s%s", strGribCode, temp )) !=2) {
	    if (num>0) fprintf(stdout,
	    "Warning: drop incomplete %s Line %d\n" ,lookup_fn, LineRead); 
	     continue;
	  }

       /* Make sure Parmid field has a Number */
       if (strspn (strGribCode, "0123456789") != strlen(strGribCode)) {
	   fprintf(stdout,"Warning: Invalid Parmid '%s', drop line#%d in %s\n", 
	   strGribCode, LineRead, lookup_fn);
	   continue; }

       GribCode =  atoi(strGribCode);
       /* check if id is out of range */
       if (GribCode < 0 || GribCode >= NPARM) {
	   fprintf(stdout, 
	   "Warning: Parm id '%d' out of range, drop %s:Line %d\n",
	   GribCode, lookup_fn, LineRead); continue; }

       /* 
	  Can only have Parm code 0 in Main table;
	  Donot load if parm is 0 and this is a Sub table;
	*/
       if (GribCode == 0 && sub!=0) {
	   fprintf(stdout,
	   "Warning: cannot have Gribcode 0 in Sub-tables, drop %s:Line %d\n",
	   lookup_fn, LineRead); continue; 
	}

/* 
*                EXTRACT Grib_Dsc & Grib_Unit_Dsc from line (both multi words)
*                !these 2 fields must be separated by atleast 2 spaces;
*                DROP line if cannot find Grib_Dsc/Unit;
*/
	/*
	  Now, get Grib_Desc and Grib_Unit_Dsc fields, both multi words...
	   TEMP has 1st word of the Grib_Desc;  This field ends when
	   we see 2 consecutive spaces;
	   locate Grib_Desc, move max of 75, then cap it where there are
           4 consecutive spaces 
	*/
        if (!(ptr= strstr (line, temp)) || !(ptr2= strstr (ptr, "  ")))
          { fprintf(stdout, 
	    "Warning:  cannot find Grib_Dsc, drop line#%d in %s\n",
	    LineRead, lookup_fn); continue;
	  }
        strncpy (grib_dsc, ptr, ptr2-ptr); /*sizeof(grib_dsc)-1); */
	grib_dsc[ptr2 - ptr] = '\0';

	/* Grib_Dsc now contains both "Desc & Unit", find where Unit begins
	   (look for 2 consecutive spaces) then put null terminator to cap
	   off Grib_Dsc 
        if ((ptr= strstr (grib_dsc, "  ")) == NULL)
	*/

	while (*ptr2==' ') ptr2++;
	if (! *ptr2) 
          { fprintf(stdout, "Warning: cannot find Unit, drop Line %d in %s\n" , 
	    LineRead, lookup_fn);
	    continue;
	  }
	else strcpy (grib_unit_dsc, ptr2);
	for (ptr=grib_unit_dsc + strlen(grib_unit_dsc)-1; *ptr==' '; ptr--)
		*ptr='\0';   /* remove ending spaces */

/*	
        *ptr= '\0';    	/# cap off GribDsc, where delimitor begins #/
	for (ptr= grib_unit_dsc+strlen(grib_unit_dsc) -1; 
	     *ptr=='\n' || *ptr == ' '; --ptr)
	    *ptr='\0';		/# rm ending spaces in Unit #/
*/
	
      
/*
*                DROP defn if this parmid is already defined;
*/
      parmptr =  db_parm_tbl + indx + GribCode;
      if (parmptr->grib_dsc[0] != '\0') { 
         fprintf(stdout,
         "Warning: duplic Parm defn #%d (Index=%d), drop line %d in %s\n",
         GribCode, PARMTBL_INDX (parmptr->usParm_id, parmptr->usParm_sub),
	 LineRead, lookup_fn);
         continue;
       }
/*
*                STORE info in the array cell whose index is 'parm_id'
*                !undefined parm ids are all set to zero;
*/
      /* depending on which Table Code it is, entry will be stored in
	 its corresponding Parameter Range;  
	 >>> ENTRIES ARE STORED IN ARRAY CELL WHOSE INDEX IS 'PARM_ID'
         >>> UNDEFINED IDS WILL HAVE CELL WITH EMPTY DEFNS;
      */
      /* Store entry just read in Parm Defn Array */

      if (sub == 0) { /* Main Table only */
	      parmptr->usParm_id = (unsigned short)GribCode;
	      parmptr->usParm_sub = 0;
	   }
      else {	/* for all Sub-Tables (non-zero sub) */
	      parmptr->usParm_id = sub + 249;  /* range 250 to 254 only */
	      parmptr->usParm_sub = (unsigned short)GribCode;
	   }
      strcpy (parmptr->grib_dsc, grib_dsc);
      strcpy (parmptr->grib_unit_dsc, grib_unit_dsc);

      DPRINT7(
	"(+D) T2-%d cd=%d: Parm=%d, ParmSub=%d, INDX=%d, Dscr='%s' Unit='%s'\n"
	,sub, GribCode,
	parmptr->usParm_id, parmptr->usParm_sub, indx+GribCode,
	parmptr->grib_dsc, parmptr->grib_unit_dsc);

      ++cnt;    /* keep track of #entries loaded */
/*
* A.2.3       ENDFOR 
*/
   }
   DPRINT2 ("Parameter table#%d has %d entries\n", sub, cnt);

/*
* A.2       ENDFOR   !load all 6 Parameter tables
*/
} /* load all 6 parm tables */
  

/******** GRIB's LEVEL TABLE *******
Sample:
   ######################################################################
   GRIB Table 3: Level Definitions
   Line 1: Level ID | Number of Octets | Meaning
   Line 2: Contents of octet 11 (optional)
   Line 3: Contents of octet 12 (optional)
   001  0  Ground or water surface
   002  0  Cloud base level
   ...
   100  2  Isobaric surface
   Pressure in hPa
   ...
   101  1  Layer between two isobaric surfaces
   Pressure of top in kPa
   Pressure of bottom in kPa
   ######################################################################

*
*           *** Level Conversion info ***
* A.3       LOOP until last line of comments ("Line3:" or "====");
*           RETURN error if fails
*/

  DPRINT1 ("*** %s: Start reading Level Defns  ***\n", func);
  /* Read until the last line of Comments */
  for (line[0]='\0'; ! strstr(line,"====") && ! strstr(line,"Line 3:") ; ) 
  {
    fgets(line, sizeof(line), infile); 
    ++LineRead;
    if (feof(infile) || ferror(infile)) 
      { sprintf(errmsg,
	"%s: got EOF/ERROR before loading LEVEL info (Line %d in %s)\n",
	func, LineRead, lookup_fn);
	goto BYE;
      }
   }

/*
* A.4       LOOP (successfully read a line from file) DO
*              SKIP if comment line 
*              BREAK out of loop if see next section "GRIB Table"
*/
  for (cnt=0; fgets(line, sizeof(line), infile) != NULL; )  
  {
    ++LineRead;
    /* skip additional comments,  Break if already got to next Table Defn,
	else replace tabs with spaces, newlines with null 
    */
    for (ptr=line; *ptr==' '; ptr++); if (*ptr == '#') continue;  
    if (strstr(line, "GRIB Table ") != NULL) break; /* end of CURR SECT */
     while (ptr=strchr(line,'\t')) *ptr=' '; 
     if (ptr=strchr(line,'\n')) *ptr='\0'; 

/*
*              EXTRACT next GRIB's Level info into Level Array:
*              DROP line if extraction fails;
*              ! line 1 format:  lvl id, #octets and Level_description
*              DROP line if level_id is invalid or out of range
*              DROP line if unable to extract level description 
*/
    /* --- Read Line 1 of Level:  frmt=  (Lvlid #octs  Multiwords Dscr) --*/
    if ((num= sscanf (line, "%s%s%s", strGribCode, strOctets, temp))!= 3) {
	 if (num>0) fprintf(stdout,
	"Warning: dropping incomplete Level defn (%s:Line %d)\n",
	lookup_fn,LineRead); 
	 continue;
	}
	
    /* Make sure Parmid field has a Number, and is within Range  */
    if (strspn (strGribCode, "0123456789") != strlen(strGribCode)) {
	   fprintf(stdout,"Warning: Invalid Levelid '%s', drop Line=%d in %s\n",
	   strGribCode, LineRead, lookup_fn); 
	   continue; }
    else GribCode =  atoi(strGribCode);

    if (GribCode < 0 || GribCode >= NLEV) {
	 fprintf(stdout, 
	 "Warning: Level Gribcode '%d' out of range, drop (Line %d in %s)",
	 GribCode, LineRead, lookup_fn); 
	 continue; 
	}

    /* Make sure #Octets field has a Number, and is within Range */
    if (strspn (strOctets, "0123456789") != strlen(strOctets)) {
	   fprintf(stdout,
	   "Warning:  Invalid NumOctets '%s' for Lvl %d, drop line#%d in %s\n", 
	   strOctets, GribCode, LineRead, lookup_fn); 
	   continue;
	}
    else iOctets = atoi(strOctets);
    if (iOctets < 0 || iOctets > 2) {
	   fprintf(stdout,
	   "Warning: Octets '%d' out of range (0-2 only), drop Line %d in %s\n",
	   iOctets, LineRead, lookup_fn); 
	   continue; 
	}

    /* TEMP here has 1st word of Lvl Descr, need to get rest of it */ 
        if ((ptr= strstr (line, temp)) == NULL)
          { fprintf(stdout,
	   "Warning:   Cannot find Lvl_Dsc, drop line#%d in %s\n",
	     LineRead, lookup_fn);
            continue;
	  }

        strncpy (grib_dsc, ptr, sizeof(grib_dsc)-1); 
 	if (ptr=strstr(grib_dsc,"  ")) *ptr='\0'; /* rm trail blanks */
/*
*              IF (0 #octets)
*                 SET lvl_name_1 and _2 to null;
*              ELSE if (1 #octets)
*                 READ in 2 more lines   !for lvl_name_1 & lvl_name_2
*              ELSE  !2 octets
*                 READ in 1 more line    !for lvl_name_1
*                 SET lvl_name_2 to null;
*              ENDIF
*/
    /* --- Get Optional Lvl_1 and Lvl_2 lines, depneding on #octs */
    switch (iOctets) {
        case 0:  lvl_name_1[0]= '\0'; lvl_name_2[0]= '\0';break;
	case 1:  if (!fgets(lvl_name_1, sizeof(lvl_name_1), infile) ||
		     !fgets(lvl_name_2, sizeof(lvl_name_2), infile)) {
		    fprintf(stdout,
		    "Warning: failed to get LvlName1/LvlName2; "
		    "drop Level %d defn (Line#%d in %s)\n",
		    GribCode, LineRead,  lookup_fn); 
		    continue;
		  }
    		 LineRead += 2; break;
	case 2:  if (!fgets(lvl_name_1, sizeof(lvl_name_1), infile) )
		  { 
		    fprintf(stdout,
		    "Warning: failed to get LvlName1; "
		    "drop Level %d defn (Line#%d in %s)\n",
		    GribCode, LineRead,  lookup_fn); 
		    continue;
		  }
		 lvl_name_2[0]='\0'; ++LineRead; break;
         }

   /* replace tabs w/space, replace Newline with Null terminator,
      and  rm trail blanks ;
   */
   if (lvl_name_1[0]) {
	 while (ptr=strchr(lvl_name_1,'\t')) *ptr=' '; 
	if (ptr=strchr(lvl_name_1,'\n')) *ptr='\0'; 
	if (ptr=strstr(lvl_name_1,"  ")) *ptr='\0'; 
	}
   if (lvl_name_2[0]) {
	 while (ptr=strchr(lvl_name_2,'\t')) *ptr=' '; 
	if (ptr=strchr(lvl_name_2,'\n')) *ptr='\0'; 
	if (ptr=strstr(lvl_name_2,"  ")) *ptr='\0'; 
	}

/*
*              DROP defn if this ID has already been defined;
*/ 
    if (db_lvl_tbl[GribCode].grib_dsc[0] != '\0') {
        fprintf(stdout,
        "Warning: drop duplic Level %d defn, currently at line %d in %s\n",
	GribCode, LineRead, lookup_fn);
	continue;
    }

/*
*              STORE all this info into Level Array cell whose index
*              equals the Level_id
*/
    db_lvl_tbl[GribCode].usLevel_id  = (unsigned short)GribCode;
    strncpy (db_lvl_tbl[GribCode].grib_dsc, grib_dsc, 
	    sizeof(db_lvl_tbl[GribCode].grib_dsc)-1);
    db_lvl_tbl[GribCode].num_octets  = iOctets;
    strncpy (db_lvl_tbl[GribCode].lvl_name_1, lvl_name_1, 
	    sizeof(db_lvl_tbl[GribCode].lvl_name_1)-1);
    strncpy (db_lvl_tbl[GribCode].lvl_name_2, lvl_name_2, 
	    sizeof(db_lvl_tbl[GribCode].lvl_name_2)-1);

/*
*              DEBUG print
*/
    switch (iOctets) {
     case 0:
        DPRINT3("(+D) Lvl=%d Dsc='%s' %d octs\n",
	db_lvl_tbl[GribCode].usLevel_id,
	db_lvl_tbl[GribCode].grib_dsc,
	db_lvl_tbl[GribCode].num_octets);break;
     case 1:
      DPRINT5(
	"(+D) Lvl=%d Dsc='%s' %d octs\n     Name1='%s'\n     Name2='%s'\n",
	db_lvl_tbl[GribCode].usLevel_id,
	db_lvl_tbl[GribCode].grib_dsc,
	db_lvl_tbl[GribCode].num_octets,
	db_lvl_tbl[GribCode].lvl_name_1,
	db_lvl_tbl[GribCode].lvl_name_2);break;
     case 2:
        DPRINT4 ("(+D) Lvl=%d Dsc='%s'  %d octs\n     Name1='%s'\n",
	db_lvl_tbl[GribCode].usLevel_id,
	db_lvl_tbl[GribCode].grib_dsc,
	db_lvl_tbl[GribCode].num_octets,
	db_lvl_tbl[GribCode].lvl_name_1);break;
     }
	 
    ++cnt;  /* number loaded */
/*
* A.4       ENDFOR !Level defns
*/
  }
  DPRINT1 ("Level table has %d entries\n", cnt);


/*** GRIB MODEL TABLE***
Sample:
	######################################################################
	GRIB Table - Generating Process Definitions (Octet 6 of PDS)
	Code Figure    Model Name
	===========    ==========
	001            NORAPS
	002            COAMPS
	003            NOGAPS
*
*           *** Model Conversion info ***
* A.5       WHILE (line is comment or header line) skip line;
*           RETURN error if fails
*/
  DPRINT1 ("*** %s: Start reading Model Defns  ***\n", func);
  /* Read until the last line of Comments */
  for (line[0]='\0'; ! strstr(line,"====") ; ) 
  {
    fgets(line, sizeof(line), infile); ++LineRead;
    if (feof(infile) || ferror(infile)) 
      { sprintf(errmsg,
	"%s: got EOF/ERROR before loading MODEL info %s Line %d\n", 
	func, lookup_fn, LineRead);
	goto BYE;
      }
   }

/*
*
* A.6       FOR (successfully read a line from file) DO
*              DROP line if comment
*              BREAK out if see next section "GRIB Table"
*/
  for (cnt=0; fgets(line, sizeof(line), infile)!=NULL; ) 
   {
    ++LineRead;
    /* skip additional comments,  Break if already got to next Table Defn,
	else replace tabs with spaces, newlines with null 
    */
     for (ptr=line; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
     if (strstr(line, "GRIB Table ") != NULL) break; /* end of CURR SECT */
     while (ptr=strchr(line,'\t')) *ptr=' '; 
     if (ptr=strchr(line,'\n')) *ptr='\0'; 
/*
*              EXTRACT from line the GRIB's Model info ;
*              DROP line if extraction fails;
*              ! frmat:    model_name model_id;
*              DROP line if modelid is invalid or out of range
*/
     if ((num= sscanf (line, "%s%s", strGribCode, temp))  !=2) {
	 if (num > 0) fprintf(stdout,
	"Warning: Drop incomplete Model line %d in %s\n", LineRead, lookup_fn); 
	 continue;
	}
     if (strspn (strGribCode, "0123456789") != strlen(strGribCode)) {
	   fprintf(stdout,"Warning:  Invalid Level '%s', drop line=%d in %s\n", 
	   strGribCode, LineRead, lookup_fn); 
	   continue; 
	}
     else GribCode =  atoi(strGribCode);

     if (GribCode < 0 || GribCode >= NMODEL) {
	 fprintf(stdout,
	 "Warning: Model '%d' out of range, drop %s Line %d\n",
	 GribCode, lookup_fn, LineRead); 
	 continue; 
	}

/*
*              DROP line if this model is already defined
*/
     if (db_mdl_tbl[GribCode].grib_dsc[0] != '\0') { 
               fprintf(stdout, 
		"Warning: duplic Model#%d defn , drop (%s Line %d)\n",
		 GribCode, lookup_fn, LineRead);
               continue;
            }
/*
*              STORE model info into model array cell whose index
*              equals the model_id;
*/
     db_mdl_tbl[GribCode].usModel_id = (unsigned short)GribCode;
     strncpy (db_mdl_tbl[GribCode].grib_dsc, 
	line+(strstr(line,temp)-line),
	sizeof(db_mdl_tbl[GribCode].grib_dsc)-1);  /* 1/more words */
     DPRINT2 ("(+D) Mdl=%d,  Gribdscr=%s\n", 
     db_mdl_tbl[GribCode].usModel_id, db_mdl_tbl[GribCode].grib_dsc);
     ++cnt;  /* number loaded */
/*
* A.6       ENDFOR 
*/
   }
  DPRINT1 ("Model table has %d entries\n", cnt);
     

/*** GRIB GEOMETRY TABLE*** 
Sample:
	######################################################################
	GRIB Table - Pre-defined geometries (Octet 7 of PDS)
	Code Figure    Geometry Name
	===========    =============
	001            mediterranean_109x82
	002            persian_gulf_NORAPS_63x63
	003            global_144x288
	255            Undefined grid, description in GDS
*
*           *** Geometry Conversion info ***
* A.7       WHILE (line is comment or header line) skip line;
*           RETURN error if fails
*/

  DPRINT1 ("*** %s: Start reading Geom Defns  ***\n", func);
  /* Read until the last line of Comments */
  for (line[0]='\0'; ! strstr(line,"====") ; ) {
    fgets(line, sizeof(line), infile); 
    ++LineRead;
    if (feof(infile) || ferror(infile)) 
      { sprintf(errmsg,
	"%s: got EOF/ERROR before loading GEOM info, %s Line %d\n",
	func, lookup_fn, LineRead);
	goto BYE;
      }
   }

/*
* A.8       FOR (successfully read a line from file) DO
*              DROP line if comment
*              BREAK out if see next section "GRIB Table"
*/
  for (cnt=0; fgets(line, sizeof(line), infile)!=NULL; ) 
   {
    ++LineRead;
    /* skip additional comments,  Break if already got to next Table Defn,
	else replace tabs with spaces, newlines with null 
    */
     for (ptr=line; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
     if (strstr(line, "GRIB Table ") != NULL) break; /* end of CURR SECT */
     while (ptr=strchr(line,'\t')) *ptr=' '; 
     if (ptr=strchr(line,'\n')) *ptr='\0'; 

/*
*              EXTRACT next GRIB's Geometry info into Geometry Array;
*              DROP line if extraction fails;
*              !format:  geom_id geom_descr
*              DROP line if geom_id is invalid or out of range
*/
     if ((num= sscanf (line, "%s%s", strGribCode, temp))  !=2) {
	 if (num > 0) fprintf(stdout,
	"Warning: drop incomplete Geom line %d in %s\n", LineRead, lookup_fn);
	 continue;
	}

     if (strspn (strGribCode, "0123456789") != strlen(strGribCode)) {
	   fprintf(stdout,"Warning: Invalid Geom_id '%s', drop %s line=%d\n", 
	   strGribCode, lookup_fn, LineRead); 
	   continue; }
     else GribCode =  atoi(strGribCode);

     if (GribCode < 0 || GribCode >= NGEOM) {
	 fprintf(stdout, "Warning: Geomid '%d' out of range, drop %s Line %d\n",
	 GribCode, lookup_fn, LineRead); 
	 continue; 
	}

/*
*              DROP line if geom_id is already defined
*/
     if (db_geom_tbl[GribCode].grib_dsc[0] != '\0') { 
               fprintf(stdout, "Warning: duplic GeomID=%d, drop %s line %d\n",
		GribCode,  lookup_fn, LineRead);
               continue;
            }
/*
*              STORE this geom info into array cell whose index
*              equals the geom_id;
*/
     db_geom_tbl[GribCode].usGeom_id = (unsigned short)GribCode;
     strncpy (db_geom_tbl[GribCode].grib_dsc, 
	line+(strstr(line,temp)-line),
	sizeof(db_geom_tbl[GribCode].grib_dsc)-1);  /* 1/more words */

     ++cnt;  /* number loaded */
     DPRINT2("(+D) Geom=%d,  Gribdscr=%s\n", 
     db_geom_tbl[GribCode].usGeom_id, db_geom_tbl[GribCode].grib_dsc);
/*
* A.8       ENDFOR 
*/
   }
  DPRINT1 ("Geometry table has %d entries\n", cnt);

/*
* 
* A.9      SET status to 0 !success
*/
  stat=0;

/*
*
* A.10     CLOSE Lookup file;
*/
BYE:
  if (infile) fclose(infile);
  DPRINT2 ("Leaving %s, stat=%d\n", func,stat);
/*
*
* A.11     RETURN with status
*/
  return (stat);
/*
* END OF FUNCTION
*
*/
}
