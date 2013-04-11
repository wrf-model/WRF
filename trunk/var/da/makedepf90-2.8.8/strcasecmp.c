#include <ctype.h>

int strcasecmp (const char *s1, const char *s2)
/* 
 * Use in case no 'strcasecmp' is found in libc on the system
 */
{
    int diff;

    while (*s1 && *s2) {
        if ((diff = (int) tolower(*s1) - (int) tolower(*s2)) != 0)  
            return diff;
        s1++;
        s2++;
    }

    if (*s1 == '\0' && *s2 == '\0') 
        return 0;
    else if (*s1 == '\0')
        return -1;
    else
        return 1;
}
