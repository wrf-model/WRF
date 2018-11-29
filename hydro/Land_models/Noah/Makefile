
all: user_build_options
	(cd Utility_routines;		make)
	(cd Noah;			make)
	(cd IO_code;			make)
	(cd Run;			make -f Makefile)
#	(cd HRLDAS_COLLECT_DATA;	make)

clean:
	(cd Utility_routines;		make clean)
	(cd Noah;			make clean)
	(cd IO_code;			make clean)
	(cd Run;			make -f Makefile clean)
	(cd HRLDAS_COLLECT_DATA;	make clean)

test: all
	(cd TEST; ../HRLDAS_COLLECT_DATA/consolidate_grib.exe )
	(cd TEST; ../Run/Noah_hrldas_beta )

#all: check user_build_options
#	(cd Utility_routines;	make)
#	(cd Noah;		make)
#	(cd IO_code;		make)
#	(cd Run;		make)
#
#clean:
#	(cd Utility_routines;	make clean)
#	(cd Noah;		make clean)
#	(cd IO_code;		make clean)
#	(cd Run;		make clean)
#
#include user_build_options
#
#check:
#ifeq ($(COMPILERF90),)
#	@ echo ""
#	@ echo "*************************************************************"
#	@ echo "*************************************************************"
#	@ echo "*****                                                   *****"
#	@ echo "***** Did you set options in file 'user_build_options'? *****"
#	@ echo "*****                                                   *****"
#	@ echo "*************************************************************"
#	@ echo "*************************************************************"
#	@ echo ""
#	@ exit 1
#endif

