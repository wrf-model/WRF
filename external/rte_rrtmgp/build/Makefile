include Makefile.upstream
VPATH += ../extensions
FCINCLUDE += -I$(NETCDFPATH)/include

.PHONY: extensions
libs: extensions
extensions: mo_cloud_optics_rrtmgp.o mo_simple_netcdf.o mo_load_coefficients.o mo_load_cloud_coefficients.o \
            mo_cloud_sampling.o mo_fluxes_byband.o mo_heating_rates.o

mo_simple_netcdf.o: librte.a
mo_load_coefficients.o: librrtmgp.a mo_simple_netcdf.o
mo_load_cloud_coefficients.o: librrtmgp.a mo_simple_netcdf.o mo_cloud_optics_rrtmgp.o
mo_cloud_sampling.o: librte.a
mo_fluxes_byband.o: librte.a
mo_heating_rates.o: librrtmgp.a

mo_cloud_optics_rrtmgp.o: librrtmgp.a
