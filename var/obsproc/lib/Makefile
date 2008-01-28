#       Mid level Makefile for the 3dvar_obsproc libraries

include ../compiler_macros

.IGNORE:

all:
		( cd ncep ; $(MAKE) )
		( cd cwordsh ; $(MAKE) )

clean :
		( $(CD) ncep ; $(MAKE) clean "RM = $(RM)" ) 
		( $(CD) cwordsh ; $(MAKE) clean "RM = $(RM)" ) 

