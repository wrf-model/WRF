#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

/*
 * simple routine to scan wgrib -s inventory
 *
 * returns byte position of matching record
 *   (search date, variable, level)
 *    date = 0 for wild card
 *    variable, level = NULL for wild card
 *
 * returns -1 for EOF
 */

long int scan3(FILE *input, int date, char *variable, char *level) {

    long int pos;
    char line[400], *pntr;
    int n_var, n_lev;

    n_var = variable != NULL ? strlen(variable) : 0;
    n_lev = level != NULL ? strlen(level) : 0;

    for (;;) {
	if (fgets(line, sizeof(line), input) == NULL) return -1;
#ifdef DEBUG
	printf("searching %s\n", line);
#endif
	pntr = line;

	while (*pntr++ != ':') ;

	/* get position */
	pos = atol(pntr);
	while (*pntr++ != ':') ;

	/* get date */
	if (date > 0 && date != atoi(pntr+2)) continue;
	while (*pntr++ != ':') ;

	/* get variable */
	if (n_var > 0 && strncmp(pntr,variable,n_var) != 0) continue;
	while (*pntr++ != ':') ;

	/* get level */
	if (n_lev > 0 && strncmp(pntr,level,n_lev) != 0) continue;
	while (*pntr++ != ':') ;
#ifdef DEBUG
	printf("scan3: match\n");
#endif
	return pos;
    }
}

