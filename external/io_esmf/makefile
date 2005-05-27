# these settings for compiling standalone on Compaq. Type "make -r"
#CPP = /lib/cpp 
#FC  = f90 -free

.SUFFIXES: .F90 .o

AR = ar
#RANLIB	= ranlib
RANLIB	= echo

OBJS = module_iostate_comp.o module_symbols_comp.o module_comp.o

TARGET = libwrfio_esmf.a

$(TARGET) : $(OBJS)
	$(AR) ru $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)

.F90.o :
	$(CPP) -C -P -DESMF_COUPLING $*.F90 > $*.f
	$(FC) -c -g -I../ioapi_share $*.f

superclean: 
	/bin/rm -f *.f *.o $(TARGET) *.mod

# DEPENDENCIES : only dependencies after this line 

module_comp.o : module_iostate_comp.o module_symbols_comp.o

