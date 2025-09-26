
package = CRTM

include ../../../../configure.wrf

FCFLAGS_CRTM = $(FCFLAGS) $(RECORDLENGTH)

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
	@$(RM) *.o *.mod *.a

# File dependency and compilation rule include files
include make.dependencies

.SUFFIXES: .fpp .f90 .o

.f90.o:
	$(SFC) -c $(FCFLAGS_CRTM) $<
#	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
#        if [ $$x = "ifort" ] ; then \
#           $(SFC) -c $(FCFLAGS_CRTM) -assume byterecl $< ; \
#        else \
#           $(SFC) -c $(FCFLAGS_CRTM) $< ; \
#        fi

.fpp.o:
	$(RM) $@ $*.mod $*.f90
	$(CPP) $(CPP_FLAGS) $< > $*.f90
	$(SFC) $(FCFLAGS_CRTM) -c $*.f90
#	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
#        if [ $$x = "ifort" ] ; then \
#           $(SFC) $(FCFLAGS_CRTM) -assume byterecl -c $*.f90 ; \
#        else \
#           $(SFC) $(FCFLAGS_CRTM) -c $*.f90 ; \
#        fi
	$(RM) $*.f90

Sort_Utility.o:
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "pgf90" ] ; then \
           $(SFC) -c $(FCFLAGS_CRTM) -tp=px $*.f90 ; \
        else \
           $(SFC) -c $(FCFLAGS_CRTM) $*.f90 ; \
        fi

