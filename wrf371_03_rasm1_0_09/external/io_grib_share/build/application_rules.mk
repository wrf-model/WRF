#------------------------------------------------------------------------------
#  Make rules for building an application program.
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/application_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULES for building one or more applications programs.
#  Each APPNAME is comprised of the same object files. 
# 
all: exe config

exe: $(OBJS) 
	@for p in $(APPNAMES); do \
		echo "Building application program $$p..." ; \
		$(LDD) $(DEBUG) $(OPTIMIZE) $(APP_DEFS) -o $$p $(OBJS) $(DEP_LIBS) ;\
		mv -f $$p $(BIN_DEST) ;\
	done

#
#  Include the RULES for compilation and installation of config files.
#
include $(BUILD_DIR)/compile_rules.mk
include $(BUILD_DIR)/config_rules.mk

#
#  RULE for building a library
#
#  For exe modules, these do nothing, but we define one so that make lib 
#  can be passed down to all source directories.
#
lib:
	@echo "make lib does nothing for application modules"

#
#  RULES for cleaning up derived files.
#
#  'clean' removes all objects produced by this file, as well as other 
#      extraneous artifacts of compiling and building applications.
# 
#      A subsequent make will both recompile the source code and recreate
#      the executable.  clean also removes files core files and other
#      auxilliary files created during compilation.
#
#  'clean_exe' removes application programs.
#
clean: 
	@/bin/rm -f *.o core so_locations Makefile.bak *~ #*#
	@/bin/rm -fr ii_files

clean_exe:
	@/bin/rm -f $(BIN_DEST)/$(APP_NAME)

#
#  RULES for creating the include dependencies.
#
include $(BUILD_DIR)/depend_rules.mk

clean_depend: generic_clean_depend

depend: generic_depend

