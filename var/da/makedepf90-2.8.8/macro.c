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

#include <string.h>
#include <assert.h>
#include "macro.h"
#include "xmalloc.h"
#include "global.h"


Macro *macro_new ()
{
    Macro *m;

    m = (Macro *) xmalloc (sizeof(Macro));
    m->name = NULL;

    return m;
}


void macro_free (Macro *m)
{
    assert (m);
    if (m->name) free (m->name);
    free (m);
}


void macro_copy (Macro *dst, const Macro *src)
{
    assert (dst);
    assert (src);

    if (dst->name)  free (dst->name);
    dst->name = xstrdup (src->name);
}
    

int macrocmp (const void *m1, const void *m2)
{
    return strcmp (((Macro *)m1)->name, ((Macro *)m2)->name);
}

void macro_setname (Macro *m, const char *n) 
{ 
    if (m->name) free (m->name);
    m->name = xstrdup(n); 
}

