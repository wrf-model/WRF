#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id: makefile 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $
#
#==============================================================================

include ../make.macros
include make.filelist


# Library build
all library: $(LIBRARY)

$(LIBRARY): $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $@ $(OBJ_FILES)


# Library cleanup
clean:
	$(REMOVE) $(OBJ_FILES) *.$(EXT_MOD) $(LIBRARY)


include make.dependencies
include ../make.rules
