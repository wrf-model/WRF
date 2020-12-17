#------------------------------------------------------------------------------
#  Make rules for determining the dependencies between source files. 
#
#  This file is intended for use in a Makefile via the include directive, e.g.
#
#      include $(BUID_DIR)/depend_rules.mk
#
#  It may also be include by other rule files in this directory.
#
#  These rules rely upon proper setting of OBJS and SRC_EXTENSIONS.
#  
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULES for creating the source code dependencies.
#
#  These are typically used to implement the rules depend and clean_depend
#  in other make rule files.
#
#  'generic_depend' creates the dependencies and appends them to a separate 
#      file in the same directory as the source code, called .depend.   
#      Depend uses some customized logic to determine the name of the source
#      code file associated with each object file in OBJS.  This means that we
#      can mix source code extensions in the same library (e.g. use C and C++
#      source interchangeably).  See compile_rules.mk for supported file name
#      extensions for C and C++ development.
#
#  'generic_clean_depend' removes the dependencies file .depend.
#
generic_clean_depend:
	@rm -f .depend

generic_depend: 
	@srcs="" ;\
	if [ -z "$(OBJS)" ]; then\
		objs="foobar";\
	else\
		objs="$(OBJS)";\
	fi;\
	for o in $${objs}; do\
		if [ $$o = "foobar" ]; then\
			echo "Error: no objects were specified in OBJS.";\
			continue;\
		fi;\
		b=`echo $$o | sed -e 's/\.o//'` ;\
		f="" ;\
		for e in $(SRC_EXTENSIONS) ; do \
			s="$$b$$e" ;\
			if [ -r "$$s" ]; then\
				srcs="$${srcs} $$s" ;\
				f=1 ;\
				break ;\
			fi; \
		done ;\
		if [ -z "$$f" ]; then\
			echo "Could not find source file for object file $$o";\
		fi ;\
	done ;\
	make_opts="";\
	if [ "$(SYS_CXX_INCLUDES)" ]; then\
		make_opts="-I+ $(SYS_CXX_INCLUDES)";\
	fi;\
	if [ "$(SYS_C_INCLUDES)" ]; then\
		make_opts="$${make_opts} -I+ $(SYS_C_INCLUDES)";\
	fi;\
	if [ "$(C_INCLUDES)" ]; then\
		make_opts="$${make_opts} -I+ $(C_INCLUDES)";\
	fi;\
	if [ "$(CXX_INCLUDES)" ]; then\
		make_opts="$${make_opts} -I+ $(CXX_INCLUDES)";\
	fi;\
	makedepend $${make_opts} -f- $${srcs} > .depend;
