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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "config.h"


static const char *progname;


void set_progname (const char *prog)
{
    progname = prog;
}

static void print_errmesg (const char *kind, const char *fmt, va_list ap)
{
    char *s;
    int n;
    unsigned u;
    double d;
    char c;

    fprintf (stderr, "\n%s: %s: ", progname, kind);

    for (; *fmt; fmt++) {
        if (*fmt != '%') {
            fputc (*fmt, stderr);
        } else {
            fmt++;
            switch (*fmt) {
                case 's':
                    s = va_arg (ap, char *);
                    fputs (s, stderr);
                    break;
                case 'c':
                    c = va_arg (ap, int);
                    fputc(c, stderr);
                    break;
                case 'd':
                case 'i':
                    n = va_arg (ap, int);
                    fprintf (stderr,"%i", n);
                    break;
                case 'u':
                    u = va_arg (ap, unsigned);
                    fprintf (stderr, "%u", u);
                    break;
                case 'f':
                    d = va_arg (ap, double);
                    fprintf (stderr, "%f", d);
                    break;
                case 'g':
                    d = va_arg (ap, double);
                    fprintf (stderr, "%g", d);
                    break;
                case '%': 
                    fputc ('%', stderr);
                    break;
                case '\0': 
                    break;
                default:
                    assert (0);
                    break;
            }
        }
    }
    fputc ('\n', stderr);
}



void fatal_error (const char *fmt, ...)
/* 
 * Print a message on stderr and exit with exitcode 'EXIT_FAILURE'
 */
{
    va_list ap;

    va_start (ap, fmt);
    print_errmesg ("ERROR", fmt, ap);
    va_end(ap);
    exit (EXIT_FAILURE);
}


void warning (const char *fmt, ...)
/* 
 * Print a message on stderr.
 */
{
    va_list ap;

    va_start (ap, fmt);
    print_errmesg ("WARNING", fmt, ap);
    va_end(ap);
}
