#================================================================================
#
# Makefile for JCSDA CRTM library build
#
# The default build target will work for most workstation platforms. For linux
# systems where there are several compiler choices, specific compiler targets
# are also provided. To modify the default Linux compiler, edit the make.macros
# file definition of LINUX_FLAGS.
#
# Note there are production and debug targets. Default build is production.
#
# $Id: makefile 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $
#
#================================================================================

# Define macros
include make.macros


# Main targets
all $(LIBRARY):  libsrc/all

test: $(LIBRARY) install test/all


# Cleanup targets
clean: libsrc/clean test/clean

distclean: clean
	$(REMOVE) -r $(LIB_DIR)
	$(REMOVE) -r $(INC_DIR)


# Installation targets
install: install_lib install_inc

install_lib: $(LIB_DIR) $(LIBRARY)
	@echo; echo "Installing $(LIBRARY) in `pwd`/$(LIB_DIR)"; \
	$(COPY) $(LIBSRC_DIR)/$(LIBRARY) $(LIB_DIR); \
	echo
        
install_inc: $(INC_DIR) $(LIBRARY)
	@echo; echo "Installing *.$(EXT_MOD) in `pwd`/$(INC_DIR)"; \
	$(COPY) $(LIBSRC_DIR)/*.$(EXT_MOD) $(INC_DIR); \
	echo
        
# ...Create directories if necessary
$(LIB_DIR) \
$(INC_DIR) :
	- test -d $@ || mkdir $@


# Test results update target
update: test/update


# Subtargets
libsrc/all       \
libsrc/clean     \
test/all         \
test/clean       \
test/update      \
:
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) SUBDIR=$$subdir TARGET=$$target subdir_target


subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	    echo "Making \`$(TARGET)' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) $(TARGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""
