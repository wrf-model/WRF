/* 
 * Copyright (C) 2000-2005 Erik Edelmann <Erik.Edelmann@iki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License as published  by  the  Free  Software  Foundation;
 *     either  version  2 of the License, or (at your option) any
 *     later version.
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
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "errormesg.h"
#include "global.h"
#include "xmalloc.h"

/* Copy src to dest, converting uppercase letters to lowercase. */

void strtolower(char *dest, const char *src)
{
    int i;

    for (i = 0; src[i]; i++) 
        dest[i] = tolower(src[i]);
}
        
    
/* replace the suffix (everything after the last '.'), including the '.', with
 * 'new_suffix'.  If there is no suffix in 'filename', concatenate 'new_suffix'
 * to 'filename'.  */
    
char *replace_suffix(const char *filename, const char *new_suffix)
{
    char *rs;
    int fl, n, sl;

    sl = strlen(new_suffix);

    /* Search for last '.' in filename */
    fl = n = strlen(filename);
    while (filename[n] != '.' && n >= 0)  n--;

    if (n == -1) {
        /* if there was no '.' */
        rs = (char *)xmalloc ((fl+sl+2)*sizeof(char));
        strcpy(rs, filename);
        strcat(rs, new_suffix);
    } else {
        rs = (char *)xmalloc ((n+sl+2)*sizeof(char));
        strncpy(rs, filename, n);
        rs[n] = '\0';
        strcat(rs, new_suffix);
    }

    return rs;
}


/* If filename has no path, append 'path' to the beginning of the filename,
 * else replace the existing path (everything before the first '/') with 'path'.
 */

char *set_path(const char *filename, const char *path)
{
    char *rs;
    int fl, n, pl, nl;

    pl = strlen(path);

    fl = n = strlen(filename);
    while (filename[n] != '/' && n >= 0)  n--;
    nl = fl - n - 1;

    if (n == -1) {
        /* if there was no '/' */
        rs = (char *)xmalloc((fl+pl+2)*sizeof(char));
        strcpy(rs, path);
        strcat(rs, filename);
    } else {
        rs = (char *)xmalloc((nl+pl+2)*sizeof(char));
        strcpy(rs, path);
        strcat(rs, &filename[n+1]);
    }

    return rs;
}


/* Create and return a copy of s with all citationmarks (" and ') removed. */

char *remove_citation(const char *s)
{
    char *d;
    int i, j;

    d = (char *)xmalloc((strlen(s)+1)*sizeof(char));
    for (i = j = 0; s[i]; i++) 
        if (s[i] != '"' && s[i] != '\'')  d[j++] = s[i];
    d[j] = '\0';

    return d;
}


char *expand_rule(const char *r, const char *srcfile)
{
    char *rule;
    int i, j, k, rlen, slen;

    rule = (char *)xmalloc(RULE_LENGTH*sizeof(char));
    rlen = strlen(r);

    /* set slen = length of srcfile without suffix */
    for (slen = strlen(srcfile); slen > 0 && srcfile[slen] != '.'; slen--);
    
    /* If there was no suffix, set slen = strlen(srcfile) */
    if (slen == 0) slen = strlen(srcfile);

    rule[0] = '\t';
    k = 1;
    for (i = 0; i < rlen; i++) {
        if (r[i] != '%') 
            rule[k++] = r[i];
        else  {
            i++;
            switch (r[i]) {
                case 'f':
                    for (j = 0; j < slen; j++)  rule[k++] = srcfile[j];
                    break;
                case '%':
                    rule[k++] = '%';
                    break;
                default:
                    warning("Unknown modifier '%%%c' in rule '%s'", r[i], r);
                    break;
            }
        }
    }
    rule[k] = '\0';

    return rule;
}


/* Open 'fname' for reading; first look for 'fname' in the current working
 * directory, and, if not found, in the list of paths in 'path'.  Return NULL if
 * 'fname' isn't found anywhere. */

FILE *open_src_file(const char *fname, const List *path)
{
    FILE *fd;
    char *fn;
    const List *h;

    fd = fopen(fname, "r");
    if (fd == NULL && path) {
        for (h = path; h && fd == NULL; h = h->next) {
            fn = xmalloc(strlen(h->data) + strlen(fname) + 2);
            strcpy(fn, (char *)h->data);
            strcat(fn, "/");
            strcat(fn, fname);
            fd = fopen(fn, "r");
            free(fn);
        }
    }

    return fd;
}
