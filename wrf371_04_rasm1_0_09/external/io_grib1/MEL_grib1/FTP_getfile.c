#include <stdio.h>
#include <stdlib.h>

#include "dprints.h"		/* Debug printing & function prototypes*/
#include "gribfuncs.h"		/* function prototypes */
/*
*
********************************************************************
* A. FUNCTION:  FTP_getfile
*       builds and executes a Bourne script file to retreive
*       the file specified from remote site via Ftp call;
*       Execute script to establish ftp session (under Userid 'anonymous' 
*       & passwd 'gribsimp22'):
*       Host info is retrieved from file "$pathnm/tables.cfg" whose content
*       is a one line entry= "eifel.nrlmry.navy.mil receive/GRIB_TABLES"
*
*    INTERFACE:
*       int     FTP_getfile (filenm, loc_pathnm, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*       (I)  char *filenm;       Name of file to ftp
*       (I)  char *loc_pathnm;   Full path leading to config file 'tables.cfg'
*       (O)  char *errmsg;       Empty array, Returns filled if error occurs;
*
*    RETURN CODE:
*        0> sucessfully ftp-ed;
*        1> error:  create script/ftp err/missing table.cfg;
********************************************************************
*/

#if PROTOTYPE_NEEDED
int 	FTP_getfile (char *filenm, char *loc_pathnm, char *errmsg)

#else
int	FTP_getfile (filenm, loc_pathnm, errmsg)
	char *filenm; 
	char *loc_pathnm; 
	char *errmsg;
#endif
{
FILE    *f1=NULL, *f2=NULL;
char    *func="FTP_getfile";
char    filename[200];
char    hostnm[100];    /* name of remote site  */
char    usernm[100];    /* using anonymous  */
char    passwd[100];    /* anonymous  */
char    pathnm[100];    /* full path of remote file to get  */
int     stat;		/* return status */
int     n;              /* working var */

  DPRINT3 ("Entering %s (%s/%s)\n", func, loc_pathnm, filenm);
/*
*
* A.1       SET up name of local config file     !$local_path/tables.cfg
*           IF (unable to open config file)
*               RETURN 1  !errmsg filled
*           ENDIF
*/

   /*   USE SAME CONFIG FILE -- 
	no matter if dnloading "g1tab* , or neon2gr*, or orig_ctr"
   */
  sprintf (filename, "%s/tables.cfg", loc_pathnm);
  DPRINT1 ("Read Remote host info from '%s'\n", filename);
  if ((f1=fopen (filename, "r"))==NULL) {
        sprintf(errmsg,"%s: failed to open '%s' for reading;\n",func,filename);
        stat=(1); goto BYE;
        }
/*
*
* A.2       READ hostname and remote pathname from file, then close it;
*           !config entry->    "eifel.nrlmry.navy.mil receive/GRIB_TABLES"
*
* A.3       CLOSE config file;
*/
   n = fscanf (f1, "%s%s", hostnm, pathnm);
   fclose(f1);     /* close Config File */
/*
*
* A.4       IF (read failed) RETURN 1  !errmsg filled;
*/
  if (n  != 2) {
        sprintf(errmsg,"%s: Fail to read 2 args from '%s'\n", func, filename);
        stat=(1); goto BYE;
        }

/*
*
* A.6       SET password to "gribsimp22", userid to "anonymous"
*/
  strcpy (passwd, "gribsimp22");

 /* Ready to build Bourne script: */
/*
*
* A.7       IF (create temp script file fails)
*               RETURN 1   !errmsg filled
*           ENDIF
*/
  if ((f1=fopen ("temp_ftp_script","w"))==NULL) {
        sprintf(errmsg,"%s:  failed to build FTP script\n", func);
        stat=(1); goto BYE;
        }
/*
*
* A.8       CREATE ftp script to download Host's "receive/GRIB_TABLES/$fn"
*           to $localPath/$fn locally; 
*
* A.9       CLOSE temp file
*/
        fprintf (f1,
		"#!/bin/sh\nexec 1>&-;exec 2>&-\nftp -in %s << STOP\n" \
		"user anonymous %s\ncd %s\nlcd %s\nget %s\nquit\n" \
		"STOP\nexit\n", 
		hostnm,  passwd, pathnm, loc_pathnm, filenm);
        fclose(f1);

        DPRINT5 ("execute ftp script: \n"
		"   #!/bin/sh\n   exec 1>&-;exec 2>&-\n"
		"   ftp -in %s << STOP\n   user anonymous %s\n"
		"   cd %s\n   lcd %s\n   get %s\n   quit\n   STOP\n   exit\n",
		hostnm,  passwd, pathnm, loc_pathnm, filenm);
/*
*
* A.10      EXECUTE script to download lookup file
*
* A.11      REMOVE temp script
*/
  fprintf(stdout,"Attempting to get remote '%s'\n", filenm);
  n= system ("chmod 755 temp_ftp_script;temp_ftp_script");
  unlink ("temp_ftp_script");

/*
*
* A.12      IF (execute script failed)
*               RETURN 1   !errmsg filled
*           ENDIF
*/
  if (n!=0) {   /* ck Stat of Systm call */
        sprintf(errmsg,"%s:  system call to ftp failed\n", func);
        stat=(1); goto BYE;
        }

/*
*
* A.13      CHECK if ftp-ed file is available & readable
*           IF (failed)
*               RETURN 1   !errmsg filled
*           ENDIF
*/
  sprintf (filename, "%s/%s", loc_pathnm, filenm);
  if ((f2= fopen(filename, "rb+"))==NULL) {
        sprintf(errmsg,"%s: '%s' not avail on %s in %s\n\n", func,
        filenm, hostnm, pathnm);
        stat=(1); goto BYE;
        }

  DPRINT0("file downloaded successfully\n");
  stat= 0;

BYE:
/*
*
* A.14      CLOSE up ftp-ed file
*/
  if (f2) fclose(f2);

/*
* A.15      RETURN 0 !success
*/
DPRINT2 ("Leaving %s, Stat=%d\n", func, stat);
return(stat);
/*
* END OF FUNCTION
*
*/
}
