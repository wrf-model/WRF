CC = gcc
FC = pgf90
FFLAGS = -Mfreeform
CFLAGS = 

EXTERNAL = /machine/user/airs/libs
INTERNAL = /usr/local

# Note: The library locations can be a mixture
of internal (library installed by the sysad on the system) 
or external (library installed by the user on his/her system)
locations.
#

ZLIBLIB    =  $(INTERNAL)/zlib-1.2.3/lib
SZIPLIB    =  $(EXTERNAL)/szip-2.1/szip/lib
JPEGLIB    =  $(EXTERNAL)/jpeg-6b/lib
HDFEOS_LIB =  $(EXTERNAL)/hdfeos/lib/linux
HDFEOS_INC =  $(EXTERNAL)/hdfeos/include
HDFLIB     =  $(EXTERNAL)/hdf-4.2.5/lib
HDFINC     =  $(EXTERNAL)/hdfeos/include


.IGNORE:
.SUFFIXES: .c .f90 .o

.c.o:
	rm -f $@
	$(CC) $(CFLAGS) -c $*.c

.f90.o:
	rm -f $@ $*.mod
	cp $< $*.f
	$(FC) $(FFLAGS) -c $*.f
	rm -f $*.f

all: decode_airs.exe

decode_airs.exe: decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o
	$(FC) -o $@ decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o \
           -L${HDFEOS_LIB} -L${HDFLIB} -I${HDFEOS_INC} -I${HDFINC} -lhdfeos \
           -lGctp -lmfhdf -ldf -L${ZLIBLIB} -lz -L${JPEGLIB} -ljpeg -lm \
           -L${SZIPLIB} -lsz

decode_airs.o: module_read_airs.o geth_newdate.o calc_rh.o

module_read_airs.o:

geth_newdate.o:

calc_rh.o:

clean:
	rm -f decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o read_airs.mod

clobber:
	make clean; rm -f decode_airs.exe
