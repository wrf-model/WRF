#define NNNMAX 9
#define NNN 5
#define ROUTINENAME alloc_space_field_core_5
MODULE module_alloc_space_5
CONTAINS

! Split the allocation bits into separate routines.
!
! The following bit of maximal foolishness brought to you by the makers of certain compilers
! that can't process large files of simple branches, assignment statements and calls to very simple
! procedures and functions without whining to their mommies about it being too complex and insisting 
! you call your service representative or wait geological time to produce code that old 1-pass 
! compilers (or compilers for other languages) could have managed efficiently in a second or two.
!

#include "module_alloc_space.h"

END MODULE module_alloc_space_5

