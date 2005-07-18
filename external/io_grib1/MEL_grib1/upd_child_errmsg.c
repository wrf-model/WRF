#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*********************************************************************
* A. FUNCTION:  upd_child_errmsg
*       Tacks the given function name in front of the error message array
*       to show which level of the Nested Function calls the error 
*       occured at;
*
*    INTERFACE:
*       void    upd_child_errmsg (parent, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*       (I)   char *parent;    name of caller function
*       (I&O) char *errmsg;    already contain error message upon entry;
*                              will get name of parent tacked in front of 
*                              existing array content;
*    RETURN:   none;
*********************************************************************
*/
#if PROTOTYPE_NEEDED
void	upd_child_errmsg (char *parent, char *errmsg)
#else
void	upd_child_errmsg (parent, errmsg)
char *parent; 
char *errmsg;
#endif
{
char temp[500], *func="upd_child_errmsg";

    DPRINT1 ("Entering %s\n", func);
    DPRINT2 ("Tacking '%s' in front of '%s'\n", parent, errmsg);
/*
*
* A.1       IF (the error message is null) THEN
*               RETURN error msg "FuncName:  no Error msg avail!"
*           ELSE
*               RETURN error msg "FuncName: " + errmsg
*           ENDIF
*/
    if (errmsg[0]=='\0') 
	 sprintf (errmsg, "%s:  no Error msg avail!\n", parent); 
    else {
	 sprintf (temp, "%s: %s", parent, errmsg);
         strncpy (errmsg, temp, 500);
	}

    DPRINT1 ("ErrMsg is now-> %s\n", errmsg);
    DPRINT1 ("Leaving %s\n", func);
/*
*  END OF FUNCTION
*/
}
