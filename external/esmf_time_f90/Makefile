# To build this by itself, use the make target esmf_time_f90_only 
# from the top-level WRF Makefile.  
# > cd ../..
# configure
# make esmf_time_f90_only

.SUFFIXES: .F90 .o .f

# a hack, should pass this in from top-level Makefile
SED_FTN = sed -f ../../arch/standard.sed

AR = ar
#RANLIB	= ranlib
RANLIB	= echo

OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o ESMF_AlarmClock.o

default: libesmf_time.a

tests: Test1.exe

libesmf_time.a : $(OBJS)
	\rm -f libesmf_time.a
	$(AR) ru libesmf_time.a $(OBJS)
	$(RANLIB) libesmf_time.a

Test1.exe : libesmf_time.a Test1.o
	$(FC) -o Test1.exe Test1.o libesmf_time.a

.F90.o :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -C -P -I. $*.b > $*.f
	$(RM) $*.b
	$(FC) -c $*.f

.F90.f :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -C -P -I. $*.b > $*.f
	$(RM) $*.b

.f.o :
	$(RM) $@
	$(RM) $*.b
	$(FC) -c $*.f

superclean: 
	/bin/rm -f *.b *.f *.o libesmf_time.a *.mod Test1.exe Test1.out

# DEPENDENCIES : only dependencies after this line 

#$$$ update dependencies!  

ESMF_Alarm.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_AlarmClock.o : ESMF_Alarm.o ESMF_Clock.o
ESMF_Time.o : ESMF_BaseTime.o ESMF_TimeInterval.o ESMF_Calendar.o \
              ESMF_Stubs.o
ESMF_Base.o :
ESMF_Calendar.o : ESMF_BaseTime.o
ESMF_Fraction.o : ESMF_BaseTime.o
ESMF_TimeInterval.o : ESMF_BaseTime.o ESMF_Calendar.o ESMF_Fraction.o
ESMF_Mod.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o Meat.o ESMF_Stubs.o ESMF_AlarmClock.o
Meat.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
ESMF_Stubs.o : ESMF_Base.o ESMF_Calendar.o
module_utility.o :  ESMF_Mod.o module_symbols_util.o
module_symbols_util.o :  ESMF_Mod.o
Test1.o :  module_utility.o

