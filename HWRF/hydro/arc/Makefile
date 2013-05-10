# Makefile 
all:
	(make -f Makefile.comm BASIC)
	(cd Main; make)
	(cd HRLDAS; make; cp Run/Noah_hrldas_beta ../Main/Noah_hrldas_beta  )

clean:
	(cd Main; make clean)
	(make -f Makefile.comm clean)
	(cd HRLDAS; make clean)
	(rm -f */*.mod */*.o lib/*.a)
