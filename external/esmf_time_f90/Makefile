# these settings for compiling standalone on Compaq. Type "make -r"
#CPP = /lib/cpp 
#FC  = f90 -free

.SUFFIXES: .F90 .o

AR = ar

OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Mod.o

libesmf_time.a : $(OBJS)
	$(AR) ru libesmf_time.a $(OBJS)

Test1 : Test1.F90 libesmf_time.a
	$(FC) -c -g Test1.F90
	$(FC) -o Test1 Test1.o libesmf_time.a

.F90.o :
	$(CPP) -C -P -I. -DF90_STANDALONE $*.F90 > $*.f
	$(FC) -c -g $*.f

superclean: 
	/bin/rm -f *.f *.o libesmf_time.a *.mod Test1

# DEPENDENCIES : only dependencies after this line 

ESMF_Alarm.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_Time.o : ESMF_BaseTime.o ESMF_TimeInterval.o
ESMF_Base.o :
ESMF_Calendar.o : ESMF_BaseTime.o
ESMF_Fraction.o : ESMF_BaseTime.o
ESMF_TimeInterval.o : ESMF_BaseTime.o ESMF_Calendar.o ESMF_Fraction.o
ESMF_Mod.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
Meat.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
