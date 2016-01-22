/*  symtab.c 

Symbol Table Handler -- Generic

The routine symget() returns a pointer to a C structure matching a
given lexeme.  If the lexeme does not already exist in the symbol
table, the routine will create a new symbol structure, store it, and
then return a pointer to the newly created structure.

It is up to the calling module to declare the symbol structure as
well as several routines for manipulating the symbol structure.  The
routines are passed to symget as pointers.

	name	   type		description

	newnode()   *char	returns a pointer to a symbol structure.

	nodename()  **char	retrieves the lexeme name from a symbol
				structure, returned as a pointer to a 
				character array.
	
	nodenext()  **char	retrieves pointer to the next field of
				the symbol structure (the next field
				is itself a pointer to a symbol structure)

For a sample main or calling program see the end of this file.

****
  REVISED 2-19-90.  Added code to make hashtable interchangible.
	new routine: create_ht()	creates new hashtable
	rev routine: symget()		added parameter to pass hash table
*/

#include <stdio.h>
#include <string.h>
#ifndef _WIN32
# include <strings.h>
#endif

#define HASHSIZE 1024

/*  commented out 2-29-90
static char * symtab[HASHSIZE] ;	
*/

void * malloc() ;
void * calloc() ;

char * symget(name,newnode,nodename,nodenext,symtab,flag)
char *name ;
char *(*newnode)(), **(*nodename)(), **(*nodenext)() ;
char *symtab[] ;
int flag ;		/* 1 is create if not there, 0 return NULL if not there */
{
    int index ; 
    int found ;
    register char *s ;
    register char *t ;
    char **x ;
    char *p ;

    index = hash( name ) ;
    p = symtab[index] ;
    found = 0 ;

    while (p) {
        s = name ;
	t = *(*nodename)(p) ;
	while (*s && *t && *s == *t ) {
	    s++ ;
	    t++ ;
	}
	if (!*s && !*t) {
	    found = 1 ;
	    break ;
	}
	p = *(*nodenext)(p) ;
    }

    if (!found ) {
      if (flag ) {
        p = (*newnode)() ;
        x =  (*nodename)(p) ;
        *x = (char *) malloc(strlen(name)+1) ;
        strcpy(*x,name) ;
        x =  (*nodenext)(p) ;
        *x = symtab[index] ;
        symtab[index] = p ;
      } else {
        return(NULL) ;
      }
    }

    return(p) ;
}

int
hash(name)
char * name ;
{
    register int result = 0  ;
    register char * p = name ;

    while (*p)
	result = 3*result + (int)*p++ ;
    
    result = result % HASHSIZE ;
    while (result < 0)
	result = result + HASHSIZE ;
    return(result) ;
}


/* added 2-19-90, attaches a new hash table to pointer  */

int
create_ht( p )
char *** p ; 
{
    *p = (char **) calloc( HASHSIZE , sizeof( char * ) ) ;
    return(0) ;
}


/* added 4-15-92.

This is a generic routine that, given a hash table pointer,
will traverse the hash table and apply a caller supplied
function to each entry

*/

int
sym_traverse( ht, nodenext, f )
char *ht[] ;
char **(*nodenext)() ;
void (*f)() ;
{
    char * p, **x ;
    int i ;
    for ( i = 0 ; i < HASHSIZE ; i++ )
    {
	if ( ( p = ht[i] ) != NULL )
	{
	    while ( p )
	    {
		 (*f)(p) ;
		 x = (*nodenext)(p) ;
		 p = *x ;
	    }
	}
    }
    return(0) ;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

#ifdef COMMENTOUTSAMPLE
/* sample_main.c

    sample main program for symget() in the file symtab.c

*/

#include <stdio.h>

struct symnode {
    char * name ;
    struct symnode *next ;
} ;

extern struct symnode * symget() ;

struct symnode *
newnode()
{
    struct symnode * malloc() ;
    return( malloc( sizeof( struct symnode ) ) ) ;
}

char **
nodename(p)
struct symnode *p ;
{
    char ** x ;
    x = &(p->name) ;
    return( x ) ;
}

struct symnode **
nodenext(p)
struct symnode *p ;
{
    struct symnode **x ;
    x = &(p->next) ;
    return( x ) ;
}

#endif

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

