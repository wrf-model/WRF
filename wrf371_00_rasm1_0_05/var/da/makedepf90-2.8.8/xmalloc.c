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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "errormesg.h"
#include "xmalloc.h"

void *xmalloc (size_t bytes)
{
    void *new;
    
    new = malloc (bytes);
    if (!new)  fatal_error ("memory allocation error");

    return new;
}


void *xrealloc(void *ptr, size_t size)
{
    void *new;

    new = realloc(ptr, size);
    if (!new)  fatal_error ("memory allocation error");

    return new;
}


char *xstrdup (const char *str)
{
    char *new;

    new = (char *) xmalloc ((strlen(str) + 1)*sizeof(char));
    strcpy(new, str);

    return new;
}


char *xstrndup (const char *str, size_t maxl)
{
    char *new;

    assert (maxl >= 0);
   
    new = (char *) xmalloc ((maxl + 1)*sizeof(char));
    strncpy (new, str, maxl);
    new[maxl] = '\0';

    return new;
}
