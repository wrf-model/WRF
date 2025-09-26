# decode_l2_airs

DECODE_L2_AIRS_OBJS = geth_newdate.o calc_rh.o module_read_airs.o decode_airs.o

decode_airs : $(DECODE_L2_AIRS_OBJS)
	$(SFC) -o $@.exe $(DECODE_L2_AIRS_OBJS) -L$(HDFEOS)/lib -lhdfeos -lGctp -L$(HDF4)/lib -lmfhdf -ldf \
           -L$(ZLIB)/lib -lz  -L$(JPEG)/lib -ljpeg 
	(cd ../da; $(LN) ../build/$@ .)

calc_rh.o module_read_airs.o decode_airs.o :
	$(SFC) -c $(FCFLAGS) -I. -I./inc -I../inc -I$(NETCDF)/include -I$(BLAS) -I$(LAPACK) \
                  -I$(FFTPACK) -I$(HDF4)/include $*.f90

geth_newdate.o :
	$(SCC) -c $(CFLAGS) $*.c
