/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/

/*
 * debug.c
 *
 * Various debugging things, including malloc debugging stuff
 */

dumdebug(j)
{
  return ;
}

/* #define NEXUS_MALLOC_DEBUG */
#ifdef NEXUS_MALLOC_DEBUG

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/access.h>

/*
 * Memory allocation debugging and diagnostics code.
 */

#define NEXUS_MALLOC_PAD 512
#define NEXUS_N_MALLOC_RECS 20000

typedef struct _malloc_rec_t
{
    char *addr;
    char *file;
    char *free_file;
    int size;
#ifdef crayx1
    int line;
    int free_line;
    int freed;
#else
    short line;
    short free_line;
    short freed;
#endif
} *malloc_rec_t;

static struct _malloc_rec_t malloc_recs[NEXUS_N_MALLOC_RECS];
static int next_malloc_rec = 0;

static char last_successful_file[1024];
static int last_successful_line;

static int initialized = 0;


#define START_MAGIC 0xf00dface
#define END_MAGIC 0xeeaaddff

/*
 * nexus_debug_malloc()
 *
 * Malloc wrapper that can print out the size and location
 * of allocations when the -Dmalloc argument has been given.
 *
 * The intent is to define a macro of the form
 *
 *	#ifdef NEXUS_DEBUG
 *	#define malloc(size) nexus_debug_malloc(size, __FILE__, __LINE__)
 *	#endif
 *
 * in order to trace memory allocation in detail.
 * 
 */
void *nexus_debug_malloc(int size, char *file, int line)
{
    void *rc, *addr;
    malloc_rec_t rec;
    int *p;

	nexus_debug_malloc_check(file, line);
	
	while ((size & 0x07) != 0)
	    size++;
	
	if (next_malloc_rec >= NEXUS_N_MALLOC_RECS)
	{
	    fprintf(stderr,"Too many malloc recs\n");
	    rc = malloc(size);
	}
	else
	{
	    rec = &malloc_recs[next_malloc_rec];
	    
	    addr = malloc(size + 2 * NEXUS_MALLOC_PAD);
	    rc = (char *) addr + NEXUS_MALLOC_PAD;
	    bzero( rc, size ) ; 	/* zero storage */
	    if (0 )
	    {
		printf("malloc(%d) at %s:%d returns %x idx=%d\n",
			    size, file, line, rc, next_malloc_rec);
	    }
	    
	    rec->addr = addr;
	    rec->file = file;
	    rec->line = line;
	    rec->size = size;
	    rec->free_file = (char *) NULL;
	    rec->free_line = -1;
	    rec->freed = 0;

	    if (NEXUS_MALLOC_PAD >= 4)
	    {
		*((int *) addr) = next_malloc_rec;
	    
		for (p = (int *) addr + 1; p < (int *) rc; p++)
		{
		    *p = START_MAGIC;
		}
		for (p = (int *) ((char *) addr + size + NEXUS_MALLOC_PAD);
		     p < (int *) ((char *) addr + size + 2 * NEXUS_MALLOC_PAD); p++)
		{
		    *p = END_MAGIC;
		}
	    }

	    next_malloc_rec++;
	}

    return rc;
} /* nexus_debug_malloc() */


/*
 * nexus_debug_malloc_check()
 *
 * Walk the list of allocated blocks looking for munged memory.
 */
nexus_debug_malloc_check(char *file, int line)
{
    int i;
    malloc_rec_t rec;
    int *p;

    if (NEXUS_MALLOC_PAD < 4)
	return;

    for (i = 0; i < next_malloc_rec; i++)
    {
	rec = &malloc_recs[i];

	if (rec->freed)
	    continue;

	if (*((int *) rec->addr) != i)
	{
	    fprintf(stderr,"Malloc check (start) failed for idx %d at %s:%d for allocation at %s:%d of size %d. Last successful check was %s:%d\n",
			i,
			file, line,
			rec->file, rec->line,
			rec->size,
			last_successful_file,
			last_successful_line);
	    exit(2) ;
	}

	for (p = (int *) rec->addr + 1; p < (int *) ((char *) rec->addr + NEXUS_MALLOC_PAD); p++)
	{
	    if (*p != START_MAGIC)
	    {
	    fprintf(stderr,"Malloc check (start) failed for idx %d at %s:%d for allocation at %s:%d of size %d Last successful check was %s:%d\n",
			    i,
			    file, line,
			    rec->file, rec->line,
			    rec->size,
			    last_successful_file,
			    last_successful_line);
	    exit(2) ;
	    }
	}
	
	for (p = (int *) ((char *) rec->addr + rec->size + NEXUS_MALLOC_PAD);
	     p < (int *) ((char *) rec->addr + rec->size + 2 * NEXUS_MALLOC_PAD); p++)
	{
	    if (*p != END_MAGIC)
	    {
	    fprintf(stderr,"Malloc check (end) failed for idx %d at %s:%d for allocation at %s:%d of size %d Last successful check was %s:%d\n",
			    i,
			    file, line,
			    rec->file, rec->line,
			    rec->size,
			    last_successful_file,
			    last_successful_line);
	    exit(2) ;
	    }
	}
    }
    strcpy(last_successful_file, file);
    last_successful_line = line;
} /* nexus_debug_malloc_check() */

void nexus_debug_mem_check(int size, void *address)
{
    int i;
    malloc_rec_t rec;
    char *pad1_start, *pad1_end, *pad2_start, *pad2_end, *a_start, *a_end;

    for (i = 0; i < next_malloc_rec; i++)
    {
	rec = &malloc_recs[i];

	if (rec->freed)
	    continue;

	pad1_start = rec->addr;
	pad1_end = pad1_start + NEXUS_MALLOC_PAD;

	pad2_start = rec->addr + rec->size + NEXUS_MALLOC_PAD;
	pad2_end = pad2_start + NEXUS_MALLOC_PAD;

	a_start = address;
	a_end = a_start + size - 1;

	if ((a_start >= pad1_start && a_start < pad1_end) ||
	    (a_end >= pad1_start && a_end < pad1_end) ||
	    (a_start >= pad2_start && a_start < pad2_end) ||
	    (a_end >= pad2_start && a_end < pad2_end) ||
	    (a_start < pad1_start && a_end > pad1_end) ||
	    (a_start < pad2_start && a_end > pad2_end))
	{
	    fprintf(stderr,"Malloc memory check for address %x length %s failed for idx %d for allocation at %s:%d of size %d.\n",
			address,
			size,
			i,
			rec->file, rec->line,
			rec->size);
	    exit(2) ;
	}
    }
} /* nexus_debug_mem_check() */


/*
 * nexus_debug_show_freed_blocks()
 *
 * Walk the list of allocated blocks looking blocks that
 * were never freed.
 */
void nexus_debug_show_freed_blocks()
{
    int i;
    malloc_rec_t rec;

    for (i = 0; i < next_malloc_rec; i++)
    {
	rec = &malloc_recs[i];

	if (!rec->freed)
	{
	    fprintf(stderr,"Unfreed block %d size=%5d at %s:%d\n",
			i, rec->size, rec->file, rec->line);
	}
    }
} /* nexus_debug_show_freed_blocks() */


/*
 * nexus_debug_show_malloc_stats()
 *
 * Print a summary of memory allocation statistics.
 */
void nexus_debug_show_malloc_stats()
{
    int i;
    malloc_rec_t rec;
    int bytes, bytes_freed;
    int n_blocks_freed;

    bytes = bytes_freed = n_blocks_freed = 0;

    for (i = 0; i < next_malloc_rec; i++)
    {
	rec = &malloc_recs[i];

	bytes += rec->size;

	if (rec->freed)
	{
	    bytes_freed += rec->size;
	    n_blocks_freed++;
	}
    }

    fprintf(stderr,"Malloc statistics:\n");
    fprintf(stderr,"\tbytes allocated:  %d\n", bytes);
    fprintf(stderr,"\tbytes freed:      %d\n", bytes_freed);
    fprintf(stderr,"\tbytes unfreed:    %d\n", bytes - bytes_freed);
    fprintf(stderr,"\tblocks allocated: %d\n", next_malloc_rec);
    fprintf(stderr,"\tblocks freed:     %d\n", n_blocks_freed);
    fprintf(stderr,"\tblocks unfreed:   %d\n", next_malloc_rec - n_blocks_freed);
} /* nexus_debug_show_malloc_stats() */


/*
 * nexus_debug_free()
 */
void nexus_debug_free(void *ptr, char *file, int line)
{
    void *addr;
    int idx;

	malloc_rec_t rec;

	if (NEXUS_MALLOC_PAD >= 4)
	{
	    addr = (char *) ptr - NEXUS_MALLOC_PAD;
	
	    idx = *((int *) addr);

	    if ( idx < 0 || idx >= NEXUS_N_MALLOC_RECS )
	    {
fprintf(stderr,"nexus_debug_free(): bad idx = %d; \n  possibly corrupt ptr %08x at %s line %d\n",
idx, ptr, file, line ) ;
	    }

	    rec = &(malloc_recs[idx]);

	    if (rec->freed == 1 )
	    {
		fprintf(stderr,"nexus_debug_free(): block %x idx %d allocated at %s:%d was freed twice at %s:%d and %s:%d\n",
			    ptr, idx,
			    rec->file, rec->line,
			    rec->free_file, rec->free_line,
			    file, line);
		exit(2) ;
	    }
	
	    rec->freed = 1;
	    rec->free_file = file;
	    rec->free_line = line;
	    free(addr);
	}
	else
	{
	    idx = -1;
	    free(ptr);
	}
	if (0)
	{
	    nexus_printf("free(%x) at %s:%d index=%d\n",
			ptr, file, line, idx);
	}
} /* nexus_debug_free() */



#endif /* NEXUS_DEBUG */

