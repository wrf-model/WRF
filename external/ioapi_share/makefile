# Makefile for variants of wrf_io_flags.h and wrf_status_codes.h that live in 
# ../../inc

all: ../../inc/wrf_io_flags.h ../../inc/wrf_status_codes.h

# The if statement below modifies WRF data type codes for builds made with 
# compiler autopromotion of REAL -> DOUBLE.  
../../inc/wrf_io_flags.h : wrf_io_flags.h ../../configure.wrf
	/bin/rm -f ../../inc/wrf_io_flags.h
	/bin/cp wrf_io_flags.h ../../inc/wrf_io_flags.h

../../inc/wrf_status_codes.h : wrf_status_codes.h
	/bin/rm -f ../../inc/wrf_status_codes.h
	/bin/cp wrf_status_codes.h ../../inc

superclean: 
	@/bin/rm -f ../../inc/wrf_io_flags.h ../../inc/wrf_status_codes.h

# DEPENDENCIES : only dependencies after this line 

