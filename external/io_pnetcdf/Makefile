#makefile to build a wrf_io with pnetCDF

FCOPTIM         = -O3
FCNOOPT         = -O0 -fno-inline -fno-ip
FCDEBUG         = # -g $(FCNOOPT) -traceback # -fpe0 -check all -ftrapuv -unroll0 -u
FORMAT_FIXED    = -FI
FORMAT_FREE     = -FR
FCSUFFIX        =
BYTESWAPIO      = -convert big_endian
FCBASEOPTS_NO_G = -ip -fp-model precise -w -ftz -align all -fno-alias $(FORMAT_FREE) $(BYTESWAPIO)
FCBASEOPTS      = $(FCBASEOPTS_NO_G) $(FCDEBUG)
FCFLAGS         = $(FCOPTIM) $(FCBASEOPTS)

NETCDFPATH     = /glade/p/work/huangwei/lib/pnetcdf/1.3.1
TRADFLAG        = -traditional
CPP             = /lib/cpp -P
AR              = ar
ARFLAGS         = ru
M4              = m4
RANLIB          = ranlib
DM_FC           = mpiifort
FC              = $(DM_FC)
FFLAGS		= $(FCDEBUG) $(FCBASEOPTS)

OBJSL   = wrf_io.o field_routines.o module_wrfsi_static.o
OBJS    = $(OBJSL)
CODE    = ext_pnc_get_dom_ti.code \
	  ext_pnc_get_var_td.code \
	  ext_pnc_get_var_ti.code \
	  ext_pnc_put_dom_ti.code \
	  ext_pnc_put_var_td.code \
	  ext_pnc_put_var_ti.code \
	  transpose.code 
FFLAGS  = $(FCFLAGS) -I$(NETCDFPATH)/include -I../ioapi_share
LIBS    = -L$(NETCDFPATH)/lib -lpnetcdf
CPP1    = $(CPP) -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o .code

all : libwrfio_pnf.a 

libwrfio_pnf.a:		$(OBJS) $(CODE)
			/bin/rm -f libwrfio_pnf.a
			$(AR) cr libwrfio_pnf.a $(OBJSL)
			$(RANLIB) libwrfio_pnf.a

wrf_io.o:               wrf_io.F90 $(CODE)
			$(CPP1) -I$(NETCDFPATH)/include -I../ioapi_share wrf_io.F90 | $(M4) - > wrf_io.f
			$(FC) $(FFLAGS) -c wrf_io.f

module_wrfsi_static.o:  module_wrfsi_static.F90
			$(CPP1) -I$(NETCDFPATH)/include -I../ioapi_share module_wrfsi_static.F90 > module_wrfsi_static.f
			$(FC) $(FFLAGS) -c module_wrfsi_static.f

field_routines.o:	field_routines.F90 wrf_io.o
			$(CPP1) -I$(NETCDFPATH)/include -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

superclean:
			/bin/rm -f *.f *.o testWRFWrite testWRFRead \
			*.mod libwrfio_pnf.a
