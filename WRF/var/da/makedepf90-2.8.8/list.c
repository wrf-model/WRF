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
#include <assert.h>
#include "list.h"
#include "xmalloc.h"


List *list_find (List *l, const void *data, 
                 int (*cmpfunc)(const void *, const void *))
/* 
 * If data matching 's' is found, return it's address.  If no such data is 
 * found, return NULL.
 *
 * 'cmpfunc' should return 0 for equal, non 0 for unequal
 */
{
    List *h;

    if (!l)  return NULL;

    for (h = l; h; h = h->next)
        if (cmpfunc(data, h->data) == 0)  return h;

    return NULL;
}


List *list_prepend (List *l, void *data)
/* 
 * Prepend 'data' to 'l', return address to the updated list.
 */
{
    List *new;

    new = (List *) xmalloc(sizeof(List));
    new->data = data;
    new->next = l;
    return new;
}


List *list_append (List *l, void *data)
/* 
 * Append 'data' to 'l', return address to the updated list.  This function is
 * slower than 'list_prepend', use only when ordering is important.
 */
{
    List *new, *h;

    new = (List *) xmalloc(sizeof(List));
    new->data = data;
    new->next = NULL;

    if (l == NULL)
        return new;
    else {
        for (h = l; h->next; h = h->next)   ;
        h->next = new;
        return l;
    }
}


void list_free (List *l)
{
    List *h;

    while (l) {
        h = l->next;
        free (l);
        l = h;
    }
}


List *list_remove (List *l, List *node)
/* Remove 'node' from 'l'.  Return address of the new updated list */
{
    List *h, *hold;

    if (!l) return NULL;
    if (!node) return l;

    hold = NULL;
    h = l;
    while (h != node && h != NULL) {
        hold = h;
        h = h->next;
    }

    assert (h != NULL);

    if (hold == NULL)
        l = h->next;
    else
        hold->next = h->next;

    node->next = NULL;

    return l;
}


int list_length (const List *l)
{
    int n = 0;
    const List *h;

    for (h = l; h; h = h->next) n++;

    return n;
}
