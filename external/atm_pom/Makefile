.SUFFIXES: .F .o
FFLAGS = -I../io_int

OBJ  =          module_PATCH_QUILT.o \
        atm_comm_pom.o  \
        atm_tiles.o     \
        cmpcomm.o       \
        mpi_more.o

AR = ar
ARFLAGS = cr
TARGET = libatm_pom.a

library: $(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

.F.o:
	$(CPP) $(CFLAGS)  $*.F  > $*.f90
	$(FC) -o $@ -c $(FFLAGS)  $*.f90 

clean:
	rm -f $(OBJ) $(TARGET)

superclean: clean
# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

atm_tiles.o: \
	atm_comm_pom.o \
	module_PATCH_QUILT.o

atm_comm_pom.o: \
	cmpcomm.o
atm_tiles.o:
cmpcomm.o:
mpi_more.o:

# DO NOT DELETE

