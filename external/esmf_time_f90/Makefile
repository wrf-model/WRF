# To build this by itself, use the make target esmf_time_f90_only 
# from the top-level WRF Makefile.  
# > cd ../..
# configure
# make esmf_time_f90_only

.SUFFIXES: .F90 .o .f

# get rid of single quotes after comments
# WARNING:  This will break if a quoted string is followed by a comment that has
#           a single quote.
SED_FTN = sed -e "/\!.*'/s/'//g"

RM = /bin/rm -f
AR = ar
#RANLIB	= ranlib
RANLIB	= echo
FGREP = fgrep -iq

OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o ESMF_AlarmClock.o

default: libesmf_time.a

tests: Test1_ESMF.exe Test1_WRFU.exe

libesmf_time.a : $(OBJS)
	$(RM) libesmf_time.a
	if [ "$(AR)" != "lib.exe" ] ; then \
          $(AR) ru libesmf_time.a $(OBJS) ; \
        else \
          $(AR) /out:libesmf_time.a $(OBJS) ; \
        fi
	$(RANLIB) libesmf_time.a

Test1_ESMF.f : Test1.F90
	$(RM) Test1_ESMF.b Test1_ESMF.f
	cp Test1.F90 Test1_ESMF.b
	$(CPP) -I. Test1_ESMF.b > Test1_ESMF.f

Test1_ESMF.exe : libesmf_time.a Test1_ESMF.o
	$(FC) -o Test1_ESMF.exe Test1_ESMF.o libesmf_time.a

Test1_WRFU.f : Test1.F90
	$(RM) Test1_WRFU.b Test1_WRFU.f
	sed -e "s/ESMF_Mod/module_utility/g" -e "s/ESMF_/WRFU_/g" Test1.F90 > Test1_WRFU.b
	$(CPP) -I. Test1_WRFU.b > Test1_WRFU.f

Test1_WRFU.exe : libesmf_time.a Test1_WRFU.o
	$(FC) -o Test1_WRFU.exe Test1_WRFU.o libesmf_time.a

.F90.o :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	$(FC) -o $@ -c $*.f

.F90.f :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi

.f.o :
	$(RM) $@
	$(RM) $*.b
	$(FC) -c $*.f

clean : testclean

testclean: 
	$(RM) *.b *.f *.o *.obj *.i libesmf_time.a *.mod Test1*.exe

superclean: testclean
	$(RM) Test1*.out make_tests.out

# DEPENDENCIES : only dependencies after this line 

#$$$ update dependencies!  

ESMF_Alarm.o : ESMF_Base.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_Base.o ESMF_Time.o ESMF_TimeInterval.o ESMF_Alarm.o
ESMF_AlarmClock.o : ESMF_Alarm.o ESMF_Clock.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_Time.o : ESMF_Base.o ESMF_BaseTime.o ESMF_TimeInterval.o ESMF_Calendar.o \
              ESMF_Stubs.o
ESMF_Base.o :
ESMF_Calendar.o : ESMF_Base.o ESMF_BaseTime.o
ESMF_Fraction.o : 
ESMF_TimeInterval.o : ESMF_Base.o ESMF_BaseTime.o ESMF_Calendar.o ESMF_Fraction.o
ESMF_Mod.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_AlarmClock.o
Meat.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
ESMF_Stubs.o : ESMF_Base.o ESMF_Calendar.o
module_utility.o :  module_symbols_util.o
module_symbols_util.o :  ESMF_Mod.o
Test1.o :  module_utility.o

