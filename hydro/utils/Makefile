# Makefile 

.SUFFIXES: .o .F

include ../macros

wh_version := $(shell cat ../.version)
WRF_HYDRO_VERSION=\"$(wh_version)\"

nwm_version  := $(shell cat ../.nwm_version)
NWM_VERSION=\"$(nwm_version)\"

F90FLAGS += -DNWM_VERSION=$(NWM_VERSION) -DWRF_HYDRO_VERSION=$(WRF_HYDRO_VERSION)

OBJS = \
<<<<<<< HEAD
	module_version.o
=======
	module_version.o \
	module_hydro_stop.o
>>>>>>> 57f8f5508dbfff90bee4647192e98338870a4656

all:	$(OBJS)

## The insertion of compile-time constants strangely requires the capital F in the extension.
.F.o:
	@echo "Utils Makefile:"
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $(*).F
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#

clean:	
	rm -f *.o *.mod *.stb *~
