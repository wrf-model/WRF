# Makefile 
#
CMD = Run/wrf_hydro.exe

all: $(CMD)

$(CMD):
	@if [ ! -d "Run" ]; then \
		(mkdir Run);\
	fi
	(rm -f Run/wrf_hydro.exe   )
	(make -f Makefile.comm BASIC)
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; make) \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd lib;rm -f librapid.a); \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd Rapid_routing; make -f makefile.cpl rapid); \
	fi

	@if [ -d "LandModel" ]; then \
	(cd LandModel; make ) \
	fi

debug:: 
	@echo 'F90FLAGS := $$(DEBUGFLAGS) $$(F90FLAGS)' >> ./macros 
	@echo 'F90FLAGS := $$(DEBUGFLAGS) $$(F90FLAGS)' >> ./LandModel/user_build_options
debug:: $(CMD)

install:
	-rm -f ./Run/wrf_hydro.exe; \
	mv LandModel/run/hrldas.exe  ./Run/wrf_hydro.exe
test:
	@echo "No libraries or utilities are built, skip testing." 
clean:
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; make clean) \
	fi
	(make -f Makefile.comm clean)
	@if [ -d "LandModel" ]; then \
	(cd LandModel; make clean) \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd Rapid_routing; make -f makefile.cpl clean); \
	fi
	(rm -f */*.mod */*.o lib/*.a Run/wrf_hydro.exe)
