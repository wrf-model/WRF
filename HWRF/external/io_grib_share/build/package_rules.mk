#------------------------------------------------------------------------------
#  Make rules for building and installing a 3rdparty package.
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/package_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  This file defines the following rules for library modules:
#
#      all, clean, install 
#
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULE for building a package. 
#
#  make all will decompress the package from a compressed tar file. 
#  If decompression is successful, the tar file will be removed.
#
#  It will then configure, make, check, and install the package using the
#  standard GNU mechanisms.  It uses make -i to ignore errors.
#
#  After the standard GNU make is complete, an executable called custom_build 
#  will be executed, if such a script is found in the same directory as the 
#  Makefile that includes these rules.  The custom_build executable should
#  accept the PACKAGE and INSTALLDIR as arguments.
# 
#  This relies on settings for the variables:
#    BASEDIR    - absolute path where Makefile resides.
#    PACKAGE    - directory relative to BASEDIR containing package to build.
#    INSTALLDIR - directory relative to BASEDIR into which to install package.
# 
all:
	@if [ ! -d $(PACKAGE) ]; then \
		if [ -e $(PACKAGE).tar.gz ]; then \
			gunzip $(PACKAGE).tar.gz ; \
			tar -xf $(PACKAGE).tar ; \
			rm -f $(PACKAGE).tar ; \
		fi ; \
	fi ; \
	if [ ! -d $(PACKAGE) ]; then \
        echo "Could not find or successfully decompress package " $(PACKAGE) ;\
		exit 1 ; \
	fi ; \
	if [ ! -d $(INSTALLDIR) ]; then \
		mkdir $(INSTALLDIR) ; \
		if [ ! -d $(INSTALLDIR) ]; then \
			echo "Cannot create installation directory " $(INSTALLDIR) ;\
		fi ; \
	fi ; \
	cd $(PACKAGE) ;\
	sh ./configure --prefix=$(BASEDIR)/$(INSTALLDIR) ;\
	make ;\
	make check ;\
	make install ;\
	cd .. ;\
	if [ -x ./custom_build ]; then \
		./custom_build $(PACKAGE) $(INSTALLDIR) ;\
	fi;

#
#  RULE for installing a package.
#
#    This copies files from the packages install directory into the bin,
#    lib, or src/3rdparty directories associated with a product source tree. 
#
#    Relies on proper setting of INSTALLDIR (explained above).
#    Also relies on paths setup in buildrc. 
#
install:
	@cd $(INSTALLDIR) ; \
	if [ -d bin ]; then \
		cd bin ;\
		echo "    Installing executables in $(BIN_DEST)" ;\
		for f in *; do \
			if [ ! -d $$f ]; then \
				echo "      $$f"; \
				cp $$f $(BIN_DEST) ;\
			fi ;\
		done ;\
		cd .. ; \
	fi; \
	if [ -d lib ]; then \
		cd lib ;\
		echo "    Installing libraries in $(LIB_DEST)" ;\
		for f in *; do \
			if [ ! -d $$f ]; then \
				echo "      $$f"; \
				cp $$f $(LIB_DEST) ;\
			fi ;\
		done ;\
		cd .. ; \
	fi; \
	if [ -d include ]; then \
		cd include ;\
		echo "    Installing header files in $(THIRDPARTY_DIR)" ;\
		for f in *; do \
			if [ ! -d $$f ]; then \
				echo "      $$f"; \
				cp $$f $(THIRDPARTY_DIR) ;\
			fi ;\
		done ;\
		cd .. ; \
	fi; 

#
#  RULE for cleaning up a package.
#
#  make clean will do both the standard GNU make clean and make distclean rules.
#  After that, it will tar and compress the package.
#
clean:
	@if [ -d $(PACKAGE) ]; then \
		cd $(PACKAGE) ;\
		make clean;\
		make distclean;\
		cd ..;\
		tar -cf $(PACKAGE).tar $(PACKAGE);\
		rm -fr $(PACKAGE);\
		gzip $(PACKAGE).tar ; \
	fi;

