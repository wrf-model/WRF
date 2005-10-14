# these settings for compiling standalone on Compaq. Type "make -r"
#CPP = /lib/cpp 
#FC  = f90 -free

.SUFFIXES: .F90 .o

AR = ar
#RANLIB	= ranlib
RANLIB	= echo

OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o ESMF_AlarmClock.o

libesmf_time.a : $(OBJS)
	$(AR) ru libesmf_time.a $(OBJS)
	$(RANLIB) libesmf_time.a

Test1 : Test1.F90 libesmf_time.a
	$(FC) -c Test1.F90
	$(FC) -o Test1 Test1.o libesmf_time.a

.F90.o :
	$(CPP) -C -P -I. -DF90_STANDALONE $*.F90 > $*.f
	$(FC) -c $*.f

superclean: 
	/bin/rm -f *.f *.o libesmf_time.a *.mod Test1

# DEPENDENCIES : only dependencies after this line 

ESMF_Alarm.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_AlarmClock.o : ESMF_Alarm.o ESMF_Clock.o
ESMF_Time.o : ESMF_BaseTime.o ESMF_TimeInterval.o
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
ESMF_Stubs.o : ESMF_Base.o ESMF_Calendar.o \
        ESMF_Alarm.o ESMF_Clock.o ESMF_TimeInterval.o ESMF_Time.o
module_utility.o :  ESMF_Mod.o module_symbols_util.o
module_symbols_util.o :  ESMF_Mod.o

