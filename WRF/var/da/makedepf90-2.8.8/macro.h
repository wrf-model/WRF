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

#ifndef MACRO_H_
#define MACRO_H_

#include <stdlib.h>
#include "xmalloc.h"

typedef struct {
    char *name;
} Macro;


Macro *macro_new ();
void macro_free (Macro *m);
void macro_copy (Macro *dst, const Macro *src);
int macrocmp (const void *d1, const void *d2);
void macro_setname (Macro *m, const char *n);

#endif /* MACRO_H_ */
