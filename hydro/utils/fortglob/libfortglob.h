#include <glob.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

typedef struct globrec {
    size_t nfiles;
    char ** filenames;
    glob_t * __globptr;
} globrec;

globrec * globfiles_c(const char * pattern);
void freeglobrec(glob_t **rec);
