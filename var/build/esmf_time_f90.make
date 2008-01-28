# esmf_time

ESMF_TIME_F90_OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o   \
        ESMF_TimeInterval.o ESMF_Stubs.o ESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o ESMF_AlarmClock.o

libesmf_time.a : $(ESMF_TIME_F90_OBJS)
	$(AR) libesmf_time.a $(ESMF_TIME_F90_OBJS)
	$(RANLIB) libesmf_time.a

Test1 : Test1.F90 libesmf_time.a
	$(FC) -c -g Test1.F90
	$(FC) -o Test1 Test1.o libesmf_time.a
