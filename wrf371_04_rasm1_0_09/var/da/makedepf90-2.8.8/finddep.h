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

#include "list.h"
#include "xmalloc.h"
#include "global.h"

typedef struct {
    char *sourcefile;
    List *targets;
    List *modules;
    List *includes;
} Dependency;


typedef struct {
    char *modulename;
    char *modfile_name;
    char *sourcefile;
} Module;


Module *module_new ();
Dependency *dependency_new ();

bool find_dep (char *file, Dependency *d, List **mods, const List *predef_macro);

int modstrcmp (const void *s, const void *m);
