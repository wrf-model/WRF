all: da_thin.exe da_bilin.exe da_bdy.exe da_vp_bilin.exe da_vp_split.exe

include ../../configure.wrf
FCOPTION=$(FCFLAGS) $(PROMOTION) $(FCSUFFIX)
CCOPTION=$(CFLAGS)
LIB_EXTERNAL=-L${NETCDFPATH}/lib -lnetcdf -lnetcdff 

da_thin.exe: da_thin.o
	$(SFC) -o $@ da_thin.o ${FCOPTION} $(LIB_EXTERNAL)

da_thin.o: da_thin.f90
	$(SFC) -c ${FCOPTION} -I$(NETCDFPATH)/include $< -o $@

da_bilin.exe: da_bilin.o
	$(SFC) -o $@ ${FCOPTION} da_bilin.o $(LIB_EXTERNAL)

da_bilin.o: da_bilin.f90
	$(SFC) -c ${FCOPTION} -I$(NETCDFPATH)/include $< -o $@

da_vp_bilin.exe: da_vp_bilin.o
	$(SFC) -o $@ ${FCOPTION} da_vp_bilin.o

da_vp_bilin.o: da_vp_bilin.f90
	$(SFC) -c ${FCOPTION} $< -o $@

da_bdy.exe: da_bdy.o
	$(SFC) -o $@ ${FCOPTION} da_bdy.o $(LIB_EXTERNAL)

da_bdy.o: da_bdy.f90
	$(SFC) -c ${FCOPTION} -I$(NETCDFPATH)/include $< -o $@ 

task_for_point.o: task_for_point.c
	$(DM_CC) -c ${CCOPTION} $< -o $@

da_vp_split.exe: da_vp_split.o task_for_point.o
	$(DM_FC) -o $@ ${FCOPTION} da_vp_split.o task_for_point.o

da_vp_split.o: da_vp_split.f90
	$(DM_FC) -c ${FCOPTION} $< -o $@

clean: 
	rm -rf *.o da_thin.exe da_bilin.exe da_bdy.exe da_vp_bilin.exe da_vp_split.exe
