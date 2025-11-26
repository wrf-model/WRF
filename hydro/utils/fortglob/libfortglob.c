#include "libfortglob.h"

globrec * globfiles_c(const char * pattern) {
    globrec *record = malloc(sizeof(globrec));
    glob_t *globbuf = malloc(sizeof(glob_t));

    glob(pattern, 0, NULL, globbuf);
    record->nfiles = globbuf->gl_pathc;
    record->filenames = globbuf->gl_pathv;
    record->__globptr = globbuf;

    return record;
}

void freeglobrec(glob_t ** rec) {
    glob_t *glob = *rec;
    globfree(glob);
    free(glob);
}
