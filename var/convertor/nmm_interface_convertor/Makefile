#

include compiler_macros

all : convert_e2c.exe add_increments_for_wrf_nmm.exe

OBJS		= \
		kinds.o			\
		constants.o			\
		gridmod.o			\
                convert_e2c.o

convert_e2c.exe:	$(OBJS)
		$(FC) -o $@ $(FCFLAGS) $(OBJS) $(LIBS)

add_increments_for_wrf_nmm.exe : add_increments_for_wrf_nmm.o
		$(FC) -o $@ $(FCFLAGS) add_increments_for_wrf_nmm.o  $(LIBS)

convert_e2c.o: \
                kinds.o                 \
                constants.o                     \
                gridmod.o                       \
		convert_e2c.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		convert_e2c.F
kinds.o: \
		kinds.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		kinds.F
constants.o: \
		constants.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		constants.F
gridmod.o: \
		gridmod.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) \
		gridmod.F

add_increments_for_wrf_nmm.o : add_increments_for_wrf_nmm.F
		$(FC) -c $(FCFLAGS) $(FPPFLAG) add_increments_for_wrf_nmm.F

clean:	
	$(RM) $(RM_LIST)

