#------------------------------------------------------------------------------
#  Make rules for installing optional configuration files. 
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/config_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  This file defines the following rules:
#
#      config 
#
#  Copyright (C) 2002, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  The config rules install any files under the directory config into the
#  project's configuration directory.  Any directory structure beneath
#  the local config directory will be preserved.  However, the special 
#  CVS directory will NOT be copied.
#
config: .FORCE
	@if [ -d config ]; then\
		cd config;\
		l=* ; \
		if [ -z "$${l}" ]; then \
			echo "Error: empty config directory.";\
			l="CVS" ;\
		fi;\
		for f in $$l; do\
			if [ "$$f" = "CVS" ]; then\
				continue;\
			fi;\
			cp -ur $$f $(BASE_DIR)/config;\
			if [ -d $$f ]; then\
				find $(BASE_DIR)/config/$$f -name 'CVS' -exec rm -fr {} > /dev/null 2>&1 \;  ;\
			fi;\
		done;\
		cd ..;\
	fi 

.FORCE:


# DO NOT DELETE THIS LINE -- make depend depends on it.
