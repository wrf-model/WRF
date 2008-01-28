# makefile to build NCEP cwordsh
#  4/6/06  JFB

# Compilation macros

include ../../compiler_macros

.SUFFIXES:	.f .c .o 

OBJS	=	cwordsh.o


cwordsh.exe:	$(OBJS) ../ncep/lib_bufr.a
		$(FC) -o cwordsh.exe $(OBJS) ../ncep/lib_bufr.a $(SYSLIB)

clean:
		$(RM) *.o *.exe *.lst 

$(OBJS): ../ncep/lib_bufr.a
	$(FC) -c $(FFLAGS) $(CWFLAGS) $*.f

