# these settings for compiling standalone on Compaq. Type "make -r"
#CPP = /lib/cpp 
#FC  = f90 -free

.SUFFIXES: .F90 .o

AR = ar
#RANLIB	= ranlib
RANLIB	= echo

# TBH:  Now we can roll in our own stuff along with ESMF_Mod if desired.  
OBJS = ESMF_Indirection.o ESMF_Mod_Wrapper.o

TARGET = libesmfwrapper.a

$(TARGET) : $(OBJS)
	$(AR) ru $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)

.F90.o :
	$(CPP) -C -P -I. $*.F90 > $*.f
	$(FC) -c -g $*.f

superclean: 
	/bin/rm -f *.f *.o $(TARGET) *.mod

# DEPENDENCIES : only dependencies after this line 

ESMF_Indirection.o : 
ESMF_Mod_Wrapper.o : ESMF_Indirection.o

