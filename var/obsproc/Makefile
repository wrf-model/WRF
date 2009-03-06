#	Top-level Makefile for 3DVAR

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -f Makefile
RM	=	/bin/rm -f
RM_LIST	=	*.o *.M core i.* *.kmo *.mod fort.* *.out namelist.* *~ */*~ *.exe

#	Targets for supported architectures

default:	make_rules
		( $(CD) lib ; $(MAKE) all )
		( $(CD) src ; $(MAKE) 3dvar_obs.exe )
		( $(RM) 3dvar_obs.exe ;   $(LN) src/3dvar_obs.exe . )

all:		make_rules
		find . -name \*.f -exec rm {} \;
		( $(CD) src ; $(MAKE) all )
		( $(CD) lib ; $(MAKE) all )
		( $(RM) 3dvar_obs.exe ;   $(LN) src/3dvar_obs.exe . )

tar:
		( $(CD) src ; $(MAKE) tar )

clean:
#	-@if [ -d MAP_plot ]; then $(CD) MAP_plot ; $(MAKE) clean ; fi
	$(RM) core .tmpfile compiler_macros
	$(RM) *.exe
	$(RM) *.out
	$(RM) fort.*
	$(RM) obs_gts.ascii.* *.diag
	$(RM) HEIGHT.txt PRES.txt TEMP.txt RH.txt WIND.txt
	$(RM) obs_ssmi*.3dvar obs_gts*.3dvar
	($(CD) src ; $(RM) *.o *.f core *.kmo ctmi* *.mod *.lst *_out_* output *.out 3dvar_obs.exe )
	($(CD) lib/ncep ; $(RM) *.o *.a *.lst *.prm )
	($(CD) lib/cwordsh ; $(RM) *.o *.exe *.lst )

make_rules:
	uname -a > .tmpfile
	@grep UNICOS .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray-X1"						; \
		echo "CPP		=	cpp"				>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DCRAYX1 "	>> compiler_macros	; \
		echo "FC		=	ftn"				>> compiler_macros	; \
		echo "FCFLAGS		=	-f free"			>> compiler_macros	; \
		echo "LDFLAGS		=	"				>> compiler_macros	; \
		echo "CCFLAGS		=	-DCRAYX1 -I."			>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	"				>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep CRAY .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray"						; \
		echo "CPP		=	cpp"				>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DCRAY "		>> compiler_macros	; \
		echo "FC		=	f90"				>> compiler_macros	; \
		echo "FCFLAGS		=	"				>> compiler_macros	; \
		echo "LDFLAGS		=	-Ca"				>> compiler_macros	; \
		echo "CCFLAGS		=	-DCRAY -I."			>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	"				>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep OSF .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"					; \
		echo "CPP		=	/usr/bin/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC"		>> compiler_macros	; \
		echo "FC		=	f90"				>> compiler_macros	; \
		echo "FCFLAGS		=	-C -free -O4 -Olimit 2000 -fpe0 -align dcommons -align records -pg -g1" >> compiler_macros	; \
		echo "FFLAGS		=	-C -fixed -O4 -Olimit 2000 -fpe0 -align dcommons -align records -pg -g1" >> compiler_macros	; \
		echo "CWFLAGS		=	-convert big_endian " >> compiler_macros	; \
		echo "FCUTILS		=	-C -O4 -Olimit 2000 -fpe0 -align dcommons -align records -pg -g1" >> compiler_macros	; \
		echo "LDFLAGS		=	-fast -O4 -pg"			>> compiler_macros	; \
		echo "CCFLAGS		=	-DDEC -DUNDERSCORE -I."		>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep HP .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for HP"						; \
		echo "CPP		=	/opt/langtools/lbin/cpp"	>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DHP "			>> compiler_macros	; \
		echo "FC		=	f90"				>> compiler_macros	; \
		echo "FCFLAGS		=	+langlvl=90"			>> compiler_macros	; \
		echo "FFLAGS		=	+langlvl=77"			>> compiler_macros	; \
		echo "LDFLAGS		=	" 				>> compiler_macros	; \
		echo "CCFLAGS		=	-DHP -DUNDERSCORE -I."		>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep AIX .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"						; \
		echo "CPP		=	/usr/lib/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM "		>> compiler_macros	; \
		echo "FC		=	xlf90"				>> compiler_macros	; \
		echo "FCFLAGS		=	-qlanglvl=90pure -qarch=auto -qnosave -qmaxmem=-1 -Q"	>> compiler_macros	; \
		echo "FFLAGS		=	-qfixed=72 -qarch=auto -qnosave -qmaxmem=-1 -Q"	>> compiler_macros	; \
		echo "LDFLAGS		=	-b64" 			>> compiler_macros	; \
		echo "CCFLAGS		=	-DIBM -I."			>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	 ../lib/ncep/lib_bufr.a"	>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep Darwin .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for Mac"						; \
		if [ $$FC = "xlf90_r" ] ; then echo "Compiling with xlf90_r"			; \
			echo "CPP		=	/usr/bin/cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -xassembler-with-cpp -DMac"	>> compiler_macros	; \
			echo "FC		=	xlf90_r"				>> compiler_macros	; \
			echo "FCFLAGS		=	-w -qfree=90 -qspill=20000 -O0 -qarch=auto -qsource -qmaxmem=32767"	>> compiler_macros	; \
			echo "FFLAGS		=	-w -qfixed=72 -qspill=20000 -O0 -qarch=auto -qmaxmem=32767 -qstrict \
				-qnosave -qintsize=4 -qrealsize=4 -qxlf77=leadzero"	>> compiler_macros	; \
			echo "LDFLAGS		=	-O0 -Wl,-stack_size,10000000,-stack_addr,0xc0000000" 		>> compiler_macros	; \
			echo "CCFLAGS		=	-DMac -I."			>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a -lSystemStubs"		>> compiler_macros	; \
			echo "RANLIB		=	ranlib"				>> compiler_macros	; \
			echo "SYSLIB		=	-lSystemStubs"				>> compiler_macros	; \
		fi ;														\
		if [ $$FC = "g95" ] ; then echo "Compiling with g95"				; \
			echo "CPP		=	/usr/bin/cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -xassembler-with-cpp -DMac"	>> compiler_macros	; \
			echo "FC		=	g95"				>> compiler_macros	; \
			echo "FCFLAGS		=	-ftrace=full -ffree-form -fno-underscoring -fno-second-underscore -O0 "	>> compiler_macros	; \
			echo "FFLAGS		=	-ftrace=full -fno-underscoring -fno-second-underscore -O0 "	>> compiler_macros	; \
			echo "LDFLAGS		=	" 		>> compiler_macros	; \
			echo "CCFLAGS		=	-DMac -I."			>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a -lSystemStubs"		>> compiler_macros	; \
			echo "RANLIB		=	ranlib"				>> compiler_macros	; \
			echo "SYSLIB		=	-lSystemStubs"				>> compiler_macros ; \
		fi ;														\
		if [ $$FC = "pgf90" ] ; then echo "Compiling with pgf90"			; \
			echo "CPP			=	cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -DLINUX -DUNDERSCORE -traditional -Dlinux "	>> compiler_macros	; \
			echo "FC			=	pgf90"				>> compiler_macros	; \
			echo "FCFLAGS		=	-Mfreeform -byteswapio "		>> compiler_macros	; \
			echo "FFLAGS		=	"			>> compiler_macros	; \
			echo "CWFLAGS		=	"		>> compiler_macros	; \
			echo "LDFLAGS		=	" 				>> compiler_macros	; \
			echo "CCFLAGS		=	-DLINUX -DUNDERSCORE -I."	>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
			echo "RANLIB		=	echo"				>> compiler_macros	; \
			echo "SYSLIB		=	"				>> compiler_macros	; \
		fi ;												\
	else grep IRIX .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for SGI"						; \
		echo "CPP		=	/lib/cpp"			>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI "		>> compiler_macros	; \
		echo "FC		=	f90"				>> compiler_macros	; \
		echo "FCFLAGS		=	-n32 -freeform -O2 -I. -OPT:Olimit=4533"	>> compiler_macros	; \
		echo "LDFLAGS		=	-n32 -O2 -lfastm"		>> compiler_macros	; \
		echo "CCFLAGS		=	-DSGI -DUNDERSCORE -I. -n32"	>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep SUN .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for SUN"						; \
		echo "CPP		=	/usr/ccs/lib/cpp"		>  compiler_macros	; \
		echo "CPPFLAGS		=	-I. -C -P -DSUN "		>> compiler_macros	; \
		echo "FC		=	f90"				>> compiler_macros	; \
		echo "FCFLAGS		=	-ansi"				>> compiler_macros	; \
		echo "FFLAGS		=	-ansi"				>> compiler_macros	; \
		echo "LDFLAGS		=	" 				>> compiler_macros	; \
		echo "CCFLAGS		=	-DSUN -DUNDERSCORE -I."		>> compiler_macros	; \
		echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
		echo "RANLIB		=	echo"				>> compiler_macros	; \
		echo "SYSLIB		=	"				>> compiler_macros	; \
	else grep Linux .tmpfile								; \
	if [ $$? = 0 ] ; then echo "Compiling for Linux"					; \
		if [ $$FC = "pgf90" ] ; then echo "Compiling with pgf90"			; \
			echo "CPP		=	/lib/cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -DLINUX -DUNDERSCORE -traditional -Dlinux "	>> compiler_macros	; \
			echo "FC		=	pgf90"				>> compiler_macros	; \
			echo "FCFLAGS		=	-Mfree -byteswapio "		>> compiler_macros	; \
			echo "FFLAGS		=	"		>> compiler_macros	; \
			echo "CWFLAGS		=	"		>> compiler_macros	; \
			echo "LDFLAGS		=	" 				>> compiler_macros	; \
			echo "CCFLAGS		=	-DLINUX -DUNDERSCORE -I."	>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
			echo "RANLIB		=	echo"				>> compiler_macros	; \
			echo "SYSLIB		=	"				>> compiler_macros	; \
		fi ;												\
		if [ $$FC = "gfortran" ] ; then echo "Compiling with gfortran"			; \
			echo "CPP		=	/lib/cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -DLINUX -DUNDERSCORE -traditional -Dlinux "	>> compiler_macros	; \
			echo "FC		=	gfortran"				>> compiler_macros	; \
			echo "FCFLAGS		=	-ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4"		>> compiler_macros	; \
			echo "FFLAGS		=	"		>> compiler_macros	; \
			echo "CWFLAGS		=	"		>> compiler_macros	; \
			echo "LDFLAGS		=	" 				>> compiler_macros	; \
			echo "CCFLAGS		=	-DLINUX -DUNDERSCORE -I."	>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
			echo "RANLIB		=	echo"				>> compiler_macros	; \
			echo "SYSLIB		=	"				>> compiler_macros	; \
		fi ;												\
		if [ $$FC = "ifort" ] ; then echo "Compiling with ifort"			; \
			echo "CPP		=	/lib/cpp"			>  compiler_macros	; \
			echo "CPPFLAGS		=	-I. -C -P -DLINUX -DUNDERSCORE -traditional -Dlinux "	>> compiler_macros	; \
			echo "FC		=	ifort"				>> compiler_macros	; \
			echo "FCFLAGS		=	-w -ftz -FR -convert big_endian"		>> compiler_macros	; \
			echo "FFLAGS		=	"		>> compiler_macros	; \
			echo "CWFLAGS		=	"		>> compiler_macros	; \
			echo "LDFLAGS		=	" 				>> compiler_macros	; \
			echo "CCFLAGS		=	-DLINUX -DUNDERSCORE -I."	>> compiler_macros	; \
			echo "LOCAL_LIBRARIES	=	../lib/ncep/lib_bufr.a"		>> compiler_macros	; \
			echo "RANLIB		=	echo"				>> compiler_macros	; \
			echo "SYSLIB		=	"				>> compiler_macros	; \
		fi ;												\
	else grep SX-5 .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for NEC SX5"					; \
                cat SX-5.Makefile                                                >  src/Makefile        ; \
	        cat SX-5.compiler_macros                                         >  compiler_macros     : \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." 			; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi

	echo "AR		=	$(AR)"		>> compiler_macros
	echo "RM		=	$(RM)"		>> compiler_macros
	echo "RM_LIST		=	$(RM_LIST)"	>> compiler_macros
	echo "CD		=	$(CD)"		>> compiler_macros
	echo "LN		=	$(LN)"		>> compiler_macros
	echo "MAKE		=	$(MAKE)"	>> compiler_macros
	echo "SHELL		=	/bin/sh"	>> compiler_macros
	echo "TOUCH		=	touch"		>> compiler_macros

