#
#                                 Makefile for example programs
R64   = 
ARCH  = linux
CNAME = 
#
# ecmwf bufr library:
PLACE = /data7/da/guo/Linux/ecmwf_bufr
# ----------------------------------------------------------------------
#
include $(PLACE)/config/config.$(ARCH)$(CNAME)$(R64)
#
include	$(PLACE)/options/options_$(ARCH)
#
TARGDIR  = ./
#
#
TARGETS  = all clean
EXECS    = decode_bufr read_bufr
#
#
all :$(EXECS)

decode_bufr: decode_bufr.o 
	$(FC) $(FFLAGS) -o $@ decode_bufr.o -L$(PLACE) -lbufr$(R64)

read_bufr: read_bufr.o 
	$(FC) $(FFLAGS) -o $@ read_bufr.o -L$(PLACE) -lbufr$(R64)
#bufr2crex   : bufr2crex.o
#$(FC) $(FFLAGS) -o $@ bufr2crex.o -L$(PLACE) -lbufr$(R64) -lcrex$(R64)

#crex2bufr   : crex2bufr.o
#	$(FC) $(FFLAGS) -o $@ crex2bufr.o -L$(PLACE) -lbufr$(R64) -lcrex$(R64)

clean   :
	@for name in $(EXECS); do\
	(rm -f $$name *.o ); \
	done 
