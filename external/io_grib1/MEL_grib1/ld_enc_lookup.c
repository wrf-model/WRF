/* Revision logs:
16Jul97 /atn:  only clr Encoder section of db tables; chg warning to stdout;
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "grib_lookup.h"  	/* combined lookup structs */
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

extern PARM_DEFN   db_parm_tbl[];  /* parm conversion info */
extern LVL_DEFN    db_lvl_tbl[];   /* level conversion info */
extern MODEL_DEFN  db_mdl_tbl[];   /* model conversion info */
extern GEOM_DEFN   db_geom_tbl[];  /* Geom conversion info */
/*
****************************************************************************
* A. FUNCTION: ld_enc_lookup
*      This function reads in the information from an external Lookup
*      table used by the GRIB Encoder (ie: neons2grib.tab).  This info 
*      is used to convert Databse codes to the GRIB Code Numbers.
*
*    INTERFACE:
*      int ld_enc_lookup (lookup_fn, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *lookup_fn;   Name of Lookup file to read from;
*      (O)  char *errmsg       REturned filled if error occurred;
*
*     RETURN CODE:
*       0>  successful, the following pre-defined arrays required for
*           encoding GRIB messages are filled=
*           PARM_DEFN  db_parm_tbl[NPARM * MAX_PARM_TBLS]; (parameter info)
*           LVL_DEFN db_lvl_tbl[NLEV];      (level info)
*           MODEL_DEFN db_mdl_tbl[NMODEL];  (model info)
*           GEOM_DEFN db_geom_tbl[NGEOM];   (geometry info)
*       1>  file open error or got error/eof while reading;  errmsg filled;
****************************************************************************
- only break out of curr Loop if sees next section's header string;

*/
#if PROTOTYPE_NEEDED
int ld_enc_lookup ( char *lookup_fn, char *errmsg)

#else
int ld_enc_lookup ( lookup_fn, errmsg)
		char *lookup_fn; char *errmsg;
#endif
{
  FILE  *infile;
  char  *func="ld_enc_lookup";     /* name of function */
  char  *ptr, temp[200], TableCode, dummy[100]; 
  int   stat=1, num, LineRead, cnt, code=0, indx0= 0;
  int   Indx, indxA=0, indxB=0, indxC=0, indxD=0, indxE= 0;
  int   *indxptr;
  char	*px;
  char             achDBsField[30];  /* DBs Field Name                  */
  char		   strDSF[50],strGribCode[50], strScale[50], strOffset[50];
  char             subtbl[20];    /* (0:maintbl) or (a/b/c/d/e:subtabl)*/
  int	GribCode;         /* GRIB Code Number                  */
  float            fScale;             /* Scale                             */
  float            fOffset;            /* Offset                            */
  short            sDSF;               /* DSF                               */
  PARM_DEFN 	   *parmptr;	       /* ptr to desired cell w/in Parm arr*/

  DPRINT2 ("Entering %s\nlookup %s\n", func, lookup_fn);
/*
*
* A.0       CLEAR out all lookup arrays  ! Encoder section only
*/
 for (num=0; num < NPARM ; num++) {
        db_parm_tbl[num].usParm_id=  num;
        db_parm_tbl[num].usParm_sub= 0;       /* not used for main tbl */
        db_parm_tbl[num].db_name[0] = '\0';
        db_parm_tbl[num].fScale     = 1.;
        db_parm_tbl[num].fOffset    = 0.;
        db_parm_tbl[num].sDSF 	    = 0;
        }
  for (num=NPARM; num < NPARM * MAX_PARM_TBLS; num++) {  /* for sub-tbls */
        db_parm_tbl[num].usParm_id=  250 + num / NPARM;
        db_parm_tbl[num].usParm_sub= num % NPARM;
        db_parm_tbl[num].db_name[0] = '\0';
        db_parm_tbl[num].fScale     = 1.;
        db_parm_tbl[num].fOffset    = 0.;
        db_parm_tbl[num].sDSF 	    = 0;
        }

  for (num=0; num < NLEV; num++) {
        db_lvl_tbl[num].usLevel_id =  num;
        db_lvl_tbl[num].db_name[0] = '\0';
        db_lvl_tbl[num].fScale	   =  1.;
        db_lvl_tbl[num].fOffset	   =  0.;
        }

  for (num=0; num < NMODEL; num++) {
        db_mdl_tbl[num].usModel_id  = num;
        db_mdl_tbl[num].db_name[0] = '\0';
        }

  for (num=0; num < NGEOM; num++) {
        db_geom_tbl[num].usGeom_id  = num;
        db_geom_tbl[num].db_name[0] = '\0';
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

/****Database's PARM TABLE -- (0/A/B/C/D/E) ***
Sample:
NEONS to GRIB Parameter Table
NEONS FIELD             TABLE CODE    GRIB CODE     SCALE     OFFSET     DSF
===========             ==========    =========     =====     ======     ===
pres                        0           001           1.0        0.0      1
pres                        0           002           1.0        0.0      1
*
*           *** Database's Parameter Defn conversion info ***
* A.2       KEEP reading until last of Header/Comment line (see '===')
*           RETURN error if fails
*/
  LineRead = 0;

  while (strstr (temp, "===") == NULL) {
  	fgets(temp, sizeof(temp), infile); /* skip Comment/Header lines */
	LineRead++; 
        if (feof(infile) || ferror(infile)) {
    	   DPRINT1 ("%s: got EOF/ERROR before loading DBs PARM info\n", func);
    	   sprintf(errmsg,
    	   "%s: got EOF/ERROR before loading DBs PARM info (%s:line %d)\n",
	    func , lookup_fn, LineRead);
    	   goto BYE;
    	  }
	}
/*
*
* A.3       FOR (successfully read a line from file) DO
*              BREAK out of loop if sees Header of next Table
*              EXTRACT next DBs's Parameter info from line read
*              IF (fails) SKIP rest of Parm defn ;
*              !parm_name, tblcode, parmid, scalefctr, offset, Decsclfctr;
*              DROP line if Parm id is out of range
*              DROP line if Table Code is not (0/A/B/C/D/E)
*              DROP defn if Code has already been defined;
*              STORE in Parm Array cell whose index equals Parmid
*           ENDFOR
*/
  for (cnt=0; fgets(temp, sizeof(temp), infile)!=NULL; ) {
      
	LineRead++; 
	for (ptr=temp; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
        if (strstr(temp, "to GRIB Level Table") != NULL) 
		break;  /* END OF CURR SECT */

    	if ((num= sscanf (temp,"%s%s%s%s%s%s%s", achDBsField, subtbl, 
	    strGribCode, strScale, strOffset, strDSF, dummy)) != 6) 
	  {
	    if (num>0)
	    fprintf(stdout,"Warning: unmatched Parm args, drop line %d in %s\n", 
		LineRead, lookup_fn);
	    continue;
	  }

       for (px=strGribCode; *px; px++) 
	  if (!isdigit(*px)) {
		fprintf(stdout,
		"Warning: invalid GRIB code, drop line %d in %s\n",
		LineRead, lookup_fn);
		continue;
		}

       GribCode =  atoi(strGribCode);
       fScale     = (float) atof(strScale);
       fOffset    = (float) atof(strOffset);
       sDSF       = (short) atoi(strDSF);

       if (GribCode < 0 || GribCode >= NPARM) {
	   fprintf(stdout,
	   "Warning:  ParmId '%d' out of range, drop line %d in %s\n",
	   GribCode,  LineRead, lookup_fn); continue; }

	if (strlen(subtbl) > 1) { fprintf(stdout,
	   "Warning:  Invalid bad TableCode '%s', drop line %d in %s;\n",
	   subtbl, LineRead, lookup_fn); continue; }

      /* depending on which Table Code it is, entry will be stored in
	 its corresponding Parameter Table;  
	 >>> if Tablecode is '0', store at array index PARM_ID;
	 >>> if Tablecode is 'A/B/C/D/E',
	 >>> then store at array index [NPARM*(PARM_ID-249) + PARM_SUB];
         >>> UNDEFINED IDS WILL HAVE EMPTY CELLS;
      */
      TableCode = (isalpha(*subtbl) ? tolower(*subtbl) : *subtbl);

      if   (TableCode=='0')  {
		Indx = 0;
      		parmptr 	    = db_parm_tbl + GribCode;
		parmptr->usParm_id  = (unsigned short) GribCode;
                parmptr->usParm_sub = 0; 
	}
      else if (TableCode>= 'a' && TableCode <= 'e')  {
	      if (GribCode==0) {
		fprintf(stdout,
	     	"Warning: Cannot use Parmid 0 for sub-tbl, drop line %d in %s\n"
		,  LineRead, lookup_fn);
		continue;
		}
	        Indx = 256 * (TableCode-'a'+1);   /* 'a':256-.. 'b':512-.. */ 
      		parmptr = db_parm_tbl + Indx + GribCode; /* actual Index */
	        parmptr->usParm_id  = 250 + (TableCode-'a');
	        parmptr->usParm_sub = (unsigned short) GribCode;
        }
      else {  fprintf(stdout,
	      "Warning:  Invalid Table '%s' (0,A-E only), drop line %d in %s\n",
	      subtbl,  LineRead, lookup_fn);
	      continue;
        }
	
      if (parmptr->db_name[0] != '\0')
	    { /* drop line if Duplicate */
               fprintf(stdout,
	   "Warning: duplic Parm %d in Tbl %c (Index=%d), drop line %d in %s\n",
		parmptr->usParm_id, TableCode,
		PARMTBL_INDX(parmptr->usParm_id, parmptr->usParm_sub),
		LineRead, lookup_fn);
              continue;
            }

      strcpy (parmptr->db_name, achDBsField);
      parmptr->fScale = fScale;
      parmptr->fOffset = fOffset;
      parmptr->sDSF = sDSF;
      ++cnt;    /* keep track of #entries loaded */

      DPRINT9(
 "(+)T2-%c cd=%d:  Parm=%d sub=%d, INDX=%d, dbnm=%s Scl=%.2f Ofs=%.2f D=%d\n"
	, TableCode, GribCode,
	parmptr->usParm_id, parmptr->usParm_sub,Indx+GribCode,  
	parmptr->db_name, parmptr->fScale, parmptr->fOffset, parmptr->sDSF);
   }
  
/*
*          DEBUG print
*/
 DPRINT1("Parameter table has %d entries\n", cnt);


/******** Database's LEVEL TABLE *******
Sample:
NEONS to GRIB Level Table (FNMOC VERSION)
NEONS Level Type        GRIB CODE     SCALE     OFFSET
================        =========     =====     ======
surface                   001           1.0        0.0
isth_lvl                  004           0.0      273.16
trpp_lvl                  007           1.0        0.0
*
*           *** DBs's Level Defn conversion info ***
* A.4       KEEP reading until last of Header/Comment line (see '===')
*           RETURN error if fails
*/
  while (strstr (temp, "====") == NULL) {
        fgets(temp, sizeof(temp), infile); /* skip Comment/Header lines */
	LineRead++; 
  	if (feof(infile) || ferror(infile)) {
	  DPRINT1 ("%s: got EOF/ERROR before loading DBs LEVEL info\n",func);
	  sprintf(errmsg,
	  "%s: got EOF/ERROR before loading LEVEL info (line %d in %s)\n",
	  func , LineRead, lookup_fn);
	  goto BYE;
	}
   }

/*
* A.5       FOR (successfully read a line from file) DO
*              BREAK out of loop if sees Header of next Table
*              EXTRACT next DBs's Level info into Level Array:
*              !format= level_type, level_id, scale, offset
*              DROP line if not enough arguments
*              DROP line if Duplicate Parm defn
*              STORE in Level  Array cell whose index equals Level id
*           ENDFOR
*/
  for (cnt=0; fgets(temp, sizeof(temp), infile) != NULL; )  
  {
    LineRead++;
    if (strstr(temp, "to GRIB Model Table") != NULL) 
	break;  /* Assume end of section */
    for (ptr=temp; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
    if ((num= sscanf (temp, "%s%s%s%s%s", 
	      achDBsField, strGribCode, strScale, strOffset, dummy)) != 4) 
	{
	  if (num>0)
	    fprintf(stdout,"Warning: unmatched Level args, drop line %d in %s\n",
	    LineRead, lookup_fn);
	  continue;
	}

    GribCode =  atoi(strGribCode);
    fScale     = (float) atof(strScale);
    fOffset    = (float) atof(strOffset);

    if (GribCode < 0 || GribCode >= NLEV) {
	 fprintf(stdout,
	"Warning: Level_id '%d' out of range, drop line %d in %s\n"
	,GribCode, LineRead, lookup_fn); 
	 continue; 
	}

    if (db_lvl_tbl[GribCode].db_name[0] != '\0') {
	fprintf(stdout,
	"Warning: duplic Level %d, drop defn ending on line %d in %s\n",
	GribCode, LineRead, lookup_fn);
	continue;
	}

    db_lvl_tbl[GribCode].usLevel_id  = (unsigned short) GribCode;
    strncpy (db_lvl_tbl[GribCode].db_name, achDBsField, 
	    sizeof(db_lvl_tbl[GribCode].db_name)-1);
    db_lvl_tbl[GribCode].fScale  = fScale;
    db_lvl_tbl[GribCode].fOffset = fOffset;
    ++cnt;  /* number loaded */

    DPRINT4("(+)  Level=%d,  db_name=%s,  Scl=%f, Offs=%f\n", 
	db_lvl_tbl[GribCode].usLevel_id, db_lvl_tbl[GribCode].db_name,
	db_lvl_tbl[GribCode].fScale, db_lvl_tbl[GribCode].fOffset);
  }
  
/*
*          DEBUG print
*/
  DPRINT1("Level table has %d entries\n", cnt);


/*** Database's MODEL TABLE***
Sample:
NEONS to GRIB Model ID Table (FNMOC VERSION)
NEONS Model Name        GRIB CODE
================        =========
NORAPS                    001
COAMPS                    002
*
*           *** Database 's Model Defn conversion info ***
* A.6       KEEP reading until last of Header/Comment line (see '===')
*           RETURN error if fails
*/
  while (strstr (temp, "====") == NULL) {
        fgets(temp, sizeof(temp), infile); /* skip Comment/Header lines */
	LineRead++;
        if (feof(infile) || ferror(infile)) {
          DPRINT1 ("%s: got EOF/ERROR before loading MODEL info\n",func);
          sprintf(errmsg,
          "%s: got EOF/ERROR before loading MODEL info (line %d in %s)\n",
          func , LineRead, lookup_fn);
          goto BYE;
        }
    }
/*
* A.7       FOR (successfully read a line from file) DO
*              BREAK out of loop if sees Header of next Table
*              EXTRACT next DBs's Model info into Model Array;
*              DROP line if not enough arguments
*              DROP line if Duplicate model defn
*              ! format= model_name, model_id;
*              STORE in Model  Array cell whose index equals Model id
*           ENDFOR
*/
  for (cnt=0; fgets(temp, sizeof(temp), infile)!=NULL; ) 
   {
     LineRead++;
     for (ptr=temp; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
     if (strstr(temp, "to GRIB Geometry Table") != NULL) 
	break; /*  Assume end of section */
     if ((num= sscanf (temp, "%s%s%s", achDBsField, strGribCode,dummy))  !=2) {
          if (num>0)
            fprintf(stdout,"Warning: unmatched Model args, drop line %d in %s\n",
            LineRead, lookup_fn);
          continue;
        }
    
     GribCode =  atoi(strGribCode);
     if (GribCode < 0 || GribCode >= NMODEL) {
         fprintf(stdout,
           "Warning:  Model_id '%d' out of range, drop line %d in %s",
           GribCode, LineRead, lookup_fn); continue; }

    if (db_mdl_tbl[GribCode].db_name[0] != '\0') {
	fprintf(stdout,
	"Warning: duplic Model %d, drop defn ending on line %d in %s\n",
	GribCode, LineRead, lookup_fn);
	continue;
	}

     db_mdl_tbl[GribCode].usModel_id = (unsigned short) GribCode;
     strcpy (db_mdl_tbl[GribCode].db_name, achDBsField);
     ++cnt;  /* number loaded */
     DPRINT2("(+)  Model=%d,  db_name=%s\n", 
	db_mdl_tbl[GribCode].usModel_id, db_mdl_tbl[GribCode].db_name);
   }
    
/*
*          DEBUG print
*/
  DPRINT1("Model table has %d entries\n", cnt);
     

/*** Database's GEOMETRY TABLE*** 
Sample:
NEONS to GRIB Geometry Table
NEONS Geometry Name     GRIB CODE
===================     =========
mediterranean_109x82       001
persian_gulf_NORAPS_63x63  002
global_144x288             003
*
*           *** Database's Geometry Defn conversion info ***
* A.8       KEEP reading until last of Header/Comment line (see '===')
*           RETURN error if fails
*/
  while (strstr (temp, "====") == NULL) {
     fgets(temp, sizeof(temp), infile); /* skip Comment/Header lines */
     LineRead++; 
     if (feof(infile) || ferror(infile)) {
          DPRINT1 ("%s: got EOF/ERROR before loading DBs GEOM info\n",func);
          sprintf(errmsg,
          "%s: got EOF/ERROR before loading GEOM info (line %d in %s)\n",
          func , LineRead, lookup_fn);
          goto BYE;
        }
    }
/*
* A.9       FOR (successfully read a line from file) DO
*              EXTRACT next DBs's Geometry info into Geometry Array;
*              IF (fails) SKIP rest of Geometry defn ;
*              !format= parm_name, parm_id, scalefctr, offset, dec scale factor;
*              STORE in Geom  Array cell whose index equals Geom id
*           ENDFOR
*/
  for (cnt=0; fgets(temp, sizeof(temp), infile)!=NULL; ) {
    LineRead++;
    for (ptr=temp; *ptr==' '; ptr++) ; if (*ptr == '#') continue;  
    if ((num= sscanf (temp, "%s%s%s", achDBsField, strGribCode, dummy))  !=2) {
          if (num>0)
            fprintf(stdout,"Warning: unmatched Geom args, drop line %d in %s\n",
            LineRead, lookup_fn);
          continue; 
        }

    GribCode =  atoi(strGribCode);
    if (GribCode < 0 || GribCode >= NGEOM) {
         fprintf(stdout,
           "Warning:  Geom_id '%d' out of range, drop line %d in %s\n",
           GribCode, LineRead, lookup_fn); continue; }

    if (db_geom_tbl[GribCode].db_name[0] != '\0') {
	fprintf(stdout,
	"Warning: duplic Geom %d, drop defn ending on line %d in %s\n",
	GribCode, LineRead, lookup_fn);
	continue;
	}

    db_geom_tbl[GribCode].usGeom_id= (unsigned short) GribCode; 
    strcpy (db_geom_tbl[GribCode].db_name, achDBsField);
    ++cnt;
    DPRINT2("(+) geom=%d,  db_name=%s\n", 
	db_geom_tbl[GribCode].usGeom_id, db_geom_tbl[GribCode].db_name);
   }
   
/*
*          DEBUG print
*/
  DPRINT1("Geometry table has %d entries\n", cnt);

/*
* 
* A.10     SET status to 0 !success
*/
  stat=0;

/*
*
* A.11     CLOSE Lookup file;
*/
BYE:
  if (infile) fclose(infile);
  DPRINT2 ("Leaving %s(), stat=%d\n", func,stat);
/*
*
* A.12     RETURN with status
*/
  return (stat);
/*
* END OF FUNCTION
*
*/
}
