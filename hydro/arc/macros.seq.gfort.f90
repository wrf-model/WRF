.IGNORE:
ifeq ($(HYDRO_D),1)
HYDRO_D = -DHYDRO_D
else
HYDRO_D =   
endif


RMD		=	rm -f
COMPILER90=	gfortran
F90FLAGS  =       -w -c -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 
MODFLAG	=	-I./ -I../mod
LDFLAGS	=	
CPP	=       /lib/cpp
CPPFLAGS	=       -C -P -traditional -I ../Data_Rec $(HYDRO_D)
LIBS 	=	
NETCDFINC       =       $(NETCDF_INC)
NETCDFLIB       =       -L$(NETCDF_LIB) -lnetcdff -lnetcdf
