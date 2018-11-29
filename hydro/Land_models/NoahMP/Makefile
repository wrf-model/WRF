
all: user_build_options
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make)
	(cd run;			make)

clean:
	(cd Utility_routines;		make clean)
	(cd phys;			make clean)
	(cd IO_code;			make clean)
	(cd run;			make clean)

### explicitly point to other land model options
NoahMP: user_build_options
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make NoahMP MOD_OPT="-DNoahMP")
	(cd run;			make -f Makefile NoahMP)
