#------------------------------------------------------------------------------
#  Make rules for installing binary files to a specified destination
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/binary_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  This file defines the following rules for binary files:
#
#      all, binary, utest, exe, clean, clean_lib, clean_exe,
#      clean_depend, depend.
#
#  Copyright (C) 2002, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULES for installing scripts and script modules 
#
#    EXE_SRC specifies a list of files that contain executable scripts.
#    Each file in the list will be installed in the $(BIN_DEST) directory
#    and will be given executable permissions.  If the variable EXE_EXT is
#    set, this extension will be stripped from the end of the source file
#    when it is installed, e.g. if EXE_SRC = "foo.pl" and EXE_EXT = ".pl",
#    the executable installed in BIN_DEST will be named "foo". 
#
#    For tcl packages, MOD_SRC specifies a list of files that contain the
#    source code that will make up the package.  MOD_NAME specifies the 
#    file name for the package (this is typically the module name with a 
#    ".tcl" extension). In addition, the Makefile *MUST* specify a 
#    destination directory for installation.  Typically, this is set to a 
#    subdirectory of BASE_DIR, e.g. MOD_DEST = $(BASE_DIR)/perllib for perl 
#    modules. 
#
#    The EXE_SRC variable only needs to be set executable scripts need to be
#    built.  Likewise MOD_SRC determines if script modules should be built.
#    The logic to set "src" to "invalid" is used to prevent shell errors
#    if either or both of these variables are not set. 
#
#    However, if MOD_SRC is set, MOD_DEST must also be set to the location
#    of a valid directory.  The same is also true for EXE_SRC and BIN_DEST,
#    but BIN_DEST is properly set when the buildrc resource file is sourced. 
#
all : binary

binary:
	@src="$(BIN_SRC)" ; \
	if [ -z "$${src}" ]; then \
		src="invalid" ;\
	else if [ -z "$(BIN_DEST)" ]; then \
		echo "Error: Binary installation directory BIN_DEST not set" 1>&2;\
		src="invalid" ;\
	fi; fi; \
	for s in $${src}; do \
		if [ -f $(BIN_DEST)/$${s} ]; then \
			echo "    Removing $(BIN_DEST)/$${s}"; \
			rm -f $(BIN_DEST)/$${s}; \
		fi; \
		if [ "$${s}" = "invalid" ]; then \
			continue;\
		fi;\
		echo "    Copying $$s to $(BIN_DEST)/$${s}" ; \
		cp $$s $(BIN_DEST)/$${s} ; \
	done; \

#
#  Include rules for installation of configuration files.
#
include $(BUILD_DIR)/config_rules.mk

#
#  RULES that are not implemented. 
#
archive linked_lib: .FORCE
	@echo "    make $@ is not implemented for binary modules" 1>&2

#
#  RULE for building unit test programs. 
#
utest: .FORCE
	@echo "    make $@ is not implemented for binary modules" 1>&2

.FORCE:

#
#  RULES for cleaning up derived files.
#
#  'clean' removes any extraneous artifacts of producing script modules or
#      executables. make clean also removes files core files and backup
#      files. 
#
#  'clean_lib' removes the installed libraries or modules
#  'clean_exe' removes the installed executable scripts
#
#      A subsequent make will recreate the shared library from the compiled
#      object files.
#
clean_exe: 
	@echo "    make $@ is not implemented for binary modules" 1>&2

clean_lib: 
	@echo "    make $@ is not implemented for binary modules" 1>&2

clean: 
	@echo "    Cleaning up binary directory `pwd`" ;\
	/bin/rm -f Makefile.bak core *~ #*#

#
#  Rules for making dependencies.
#  These are not implemented for scripts, so the rules do nothing.
#
depend clean_depend: .FORCE
	@:

# DO NOT DELETE THIS LINE -- make depend depends on it.
