/* 
 * Copyright (C) 2000-2005 Erik Edelmann <Erik.Edelmann@iki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License version 2 as published  by  the  Free  Software  
 *     Foundation.
 *
 *     This program is distributed in the hope that  it  will  be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS  FOR  A  PARTICULAR
 *     PURPOSE.   See  the  GNU  General  Public License for more
 *     details.
 *
 *     You should have received a copy of the GNU General  Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59  Temple  Place,  Suite  330,
 *     Boston, MA  02111-1307  USA
 */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "global.h"
#include "errormesg.h"
#include "xmalloc.h"


char *modfile_name (const char *modulename, const char *filename)
/* 
 * Create a "modfile" name out of 'modulename' and 'filename' using format given
 * by 'options.modfile_fmt'.
 */
{
    char *modfile;
    const char *fmt = options.modfile_fmt;
    int mi = 0, j, i;

    modfile = (char *)xmalloc (sizeof(char)*MODFILE_NAME_LEN);

    for (i = 0; fmt[i]; i++) {
        if (fmt[i] == '%') {
            i++;
            switch (fmt[i]) {
                case 'f':
                    for (j = 0; filename[j]; j++)
                        modfile[mi++] = filename[j];

                    /* Skip the file name extension */
                    for (mi--; modfile[mi] != '.'; mi--);
                    break;
                case 'm':
                    for (j = 0; modulename[j]; j++)
                        modfile[mi++] = tolower (modulename[j]);
                    break;
                case 'M':
                    for (j = 0; modulename[j]; j++)
                        modfile[mi++] = toupper (modulename[j]);
                    break;
                case '%':
                    modfile[mi++] = '%';
                    break;
                default:
                    warning ("Unknown modifier '%%%c' in modfile format '%s'",
                             fmt[i], fmt);
                    break;
            }
        } else {
            modfile[mi++] = fmt[i];
        }
    }
    modfile[mi] = '\0';

    return modfile;
}
