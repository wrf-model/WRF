#------------------------------------------------------------------------------
#  Make rules for producing a library module.
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/library_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  This file defines the following rules for library modules:
#
#      all, lib, archive, linked_lib, utest, exe, clean, clean_lib, 
#      clean_depend, depend.
#
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh
MAKE=make

#
#  RULES that can be passed through to subdirectories. 
#
#    Each of these rules executes the rule on this directory, then executes
#    the same rule on each subdirectory specified in SUB_DIRS.
#
#    The rules for this directory are specified as thisdir_<rulename> in
#    this file, e.g. "thisdir_all" implements "make all" for this directory. 
#
#    The SUB_DIRS variable only needs to be set if building of subdirectories
#    is desired.  In this case, the list of subdirectories ($$s) is set to the
#    no-op value of "foobar" to prevent the shell for seeing errors in the 
#    subsequent for loop when SUB_DIRS is not set. 
#
all lib archive linked_lib clean depend clean_depend clean_lib:
	@s="$(SUB_DIRS)" ; \
	if [ -z "$${s}" ]; then \
		s="foobar" ;\
	fi; \
	for d in $$s ; do \
		if [ "$$d" = "foobar" ]; then\
			continue ;\
		fi ; \
		if [ ! -d "$$d" ]; then \
			echo "        Error: subdir $$d is NOT a directory!"; \
			contiuue ;\
		fi ; \
		if [ ! -r "$$d/Makefile" ]; then\
			echo "        Error: subdir $$d does NOT contain a Makefile!"; \
			continue ;\
		fi ; \
		echo "        Doing make $@ on library subdirectory $$d" ;\
		cd $$d ; \
		$(MAKE) $@;\
		cd ..; \
	done; \
	$(MAKE) thisdir_$@

clean_exe:
	@echo "make clean_exe does nothing for library modules"

thisdir_all: thisdir_linked_lib config 

#
#  Include the RULES for compilation and installing config files
#
include $(BUILD_DIR)/compile_rules.mk
include $(BUILD_DIR)/config_rules.mk

#
#  RULES for building a library.
#
#  - 'lib' builds the library as a shared library.
#  - 'archive' builds the library as an archive library.
#  - 'linked_lib' builds the library as a shared library (if necessary), and
#    links the shared library to its dependent libraries.
#  - Specific library names are built depending on the update status of the
#    library of the same name installed in $(LIB_DEST).
#  - Libraries are built depending on the status of its object files (OBJS)
#
#  NOTE: Shared libraries are linked against the libraries upon which they
#        depend.  This is not possible with archive libraries.
# 
thisdir_lib: lib$(LIB_NAME).$(LIB_EXT)

lib$(LIB_NAME).$(LIB_EXT): $(LIB_DEST)/lib$(LIB_NAME).$(LIB_EXT)

$(LIB_DEST)/lib$(LIB_NAME).$(LIB_EXT): $(OBJS)
	$(CXX) $(LIB_FLAGS) -o $@ $(OBJS)

thisdir_linked_lib: $(OBJS)
	$(CXX) $(LIB_FLAGS) -o $(LIB_DEST)/lib$(LIB_NAME).$(LIB_EXT) $(OBJS) $(DEP_LIBS)
	@if [ `echo $(LIB_NAME) | grep Pkg` ] ; then \
		pwd = `pwd` ; \
		cd $(LIB_DEST) ; \
		echo "    Creating tcl package index in $(MOD_DEST)" ;\
		exec echo "pkg_mkIndex . \*Pkg.so \*.tcl" | tclsh;\
		cd "$$(pwd)"; \
	fi

thisdir_archive: $(OBJS) .FORCE 
	$(AR) $(ARFLAGS) $(LIB_DEST)/lib$(LIB_NAME).a $(OBJS)
	$(RANLIB) $(LIB_DEST)/lib$(LIB_NAME).a

#
#  RULE for building unit test programs. 
#

utest: .FORCE
	@if [ -d utest ] ; then \
		echo "Making unit tests for `pwd`"; \
		cd utest; \
		make; \
		cd ..; \
	fi 

.FORCE:

#
#  RULE for building an executable.
#
#  For library modules, these do nothing, but we define one so that make exe
#  can be passed down to all source directories.
#
exe:
	@echo "make exe does nothing for library modules"

#
#  RULES for cleaning up derived files.
#
#  'clean' removes all objects produced by this file, as well as other 
#      extraneous artifacts of compiling and building libraries.
# 
#      A subsequent make will both recompile the source code and recreate
#      the shared library.  clean also removes files core files and other
#      auxilliary files created during compilation.
#
#  'clean_lib' removes only the libraries, both shared and archive.
#
#      A subsequent make will recreate the shared library from the compiled
#      object files.
#
thisdir_clean: thisdir_clean_lib
	@/bin/rm -f *.o *.obj *.mod *.f90 core so_locations Makefile.bak *~ #*#
	@/bin/rm -fr ii_files
	@if [ -d utest ] ; then \
		echo "        Doing make clean on utest subdirectory"; \
		cd utest; \
		make clean; \
		cd ..; \
	fi 

thisdir_clean_lib: 
	@/bin/rm -f $(LIB_DEST)/lib$(LIB_NAME).*	

#
#  RULES for creating the include dependencies.
#
#  'depend' creates the dependencies and appends them to the end of this file.
#      Depend uses some customized logic to determine the name of the source
#      code file associated with each object file in OBJS.  This means that we
#      can mix source code extensions in the same library (e.g. use C and C++
#      source interchangeably).  See compile_rules.mk for supported file name
#      extensions.
#
#  'clean_depend' removes the dependencies from this file, which makes the
#      Makefile much smaller.  make clean_depend should be done before
#      checking a Makefile into revision control.
#
thisdir_clean_depend: generic_clean_depend 
	@if [ -d utest ] ; then \
		echo "        Doing make clean_depend on utest subdirectory"; \
		cd utest; \
		$(MAKE) clean_depend; \
		cd ..; \
	fi 

thisdir_depend: generic_depend
	@if [ -d utest ] ; then \
		echo "        Doing make depend on utest subdirectory"; \
		cd utest; \
		$(MAKE) depend; \
		cd ..; \
	fi 

include $(BUILD_DIR)/depend_rules.mk

