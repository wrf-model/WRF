# Makefile 
all:
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
	(cd LandModel; make ; rm -f ../../Run/wrf_hydro.exe; mv Run/Noah_hrldas_beta ../../Run/wrf_hydro.exe  ) \
	fi

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
