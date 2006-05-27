#------------------------------------------------------------------------------
#  Make rules for building one or more unit test programs.  These are used to
#  test library modules. 
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/utest_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  This file defines the following rules for library modules:
#
#      all, exe
#
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULES for building one or more unit test programs. 
# 
all: exe 
exe: utest

utest: $(MAIN_OBJS) 
	@for o in $(MAIN_OBJS); do \
        p=`basename $$o '.o'` ; \
		echo "          Building test program $$p..." ; \
		echo "$(LDD) $(DEBUG) $(OPTIMIZE) -o $$p $$o $(DEP_LIBS)" ;\
	    $(LDD) $(DEBUG) $(OPTIMIZE) -o $$p $$o $(DEP_LIBS) ;\
	done

#
#  Include the RULES for compilation. 
#
include $(BUILD_DIR)/compile_rules.mk

#
#  RULE for building a library
#
#  For exe modules, these do nothing, but we define one so that make lib 
#  can be passed down to all source directories.
#
lib:
	@echo "make lib does nothing for unit test modules"

#
#  RULES for cleaning up derived files.
#
#  'clean' removes all objects produced by this file, as well as other 
#      extraneous artifacts of compiling and building libraries.
# 
#      A subsequent make will both recompile the source code and recreate
#      the executable.  clean also removes files core files and other
#      auxilliary files created during compilation.
#
clean: 
	@/bin/rm -f *.o core so_locations Makefile.bak *~ #*#
	@/bin/rm -fr ii_files
	@for o in $(MAIN_OBJS); do \
            p=`basename $$o '.o'` ; \
	    rm -f $$p;\
	done

#
#  RULES for creating the include dependencies.
#
OBJS=$(MAIN_OBJS)
include $(BUILD_DIR)/depend_rules.mk

clean_depend: generic_clean_depend

depend: generic_depend
