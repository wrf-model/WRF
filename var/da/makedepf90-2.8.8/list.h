/* 
 * Copyright (C) 2000-2005 Erik Edelmann <Erik.Edelmann@iki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License version 2 as published  by  the  Free  Software  
 *     Foundation;
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

#ifndef LIST_H_
#define LIST_H_

/* Cast function pointers to function type used as comparision function by
 * inlist and add */
#define COMP_FUN(fnc) ((int (*)(const void *, const void *))fnc)

typedef struct List_ { 
    struct List_ *next; 
    void *data; 
} List;


void  list_destroy (List *l);
List *list_prepend (List *l, void *s);
List *list_append (List *l, void *s);
List *list_find (List *l, const void *s, 
                 int (*cmpfunc)(const void *, const void *));
void list_free (List *l);
List *list_remove (List *l, List *node);
int list_length (const List *l);

#endif  /* LIST_H_ */
