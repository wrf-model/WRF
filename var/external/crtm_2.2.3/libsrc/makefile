
package = CRTM

include ../../../../configure.wrf

FCFLAGS_CRTM = $(FCFLAGS)

# The library name
LIBRARY = lib$(package).a

# The file definitions. This include must occur before targets.
include make.filelist

# The targets
all: library

$(LIBRARY): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@
        
library: $(LIBRARY)

clean:
	$(RM) *.o *.mod *.a

# File dependency and compilation rule include files
include make.dependencies

.SUFFIXES: .fpp .f90 .o

.f90.o:
	$(SFC) -c $(FCFLAGS_CRTM) $<

.fpp.o:
	$(RM) $@ $*.mod $*.f90
	$(CPP) $(CPP_FLAGS) $< > $*.f90
	$(SFC) $(FCFLAGS_CRTM) -c $*.f90
	$(RM) $*.f90
