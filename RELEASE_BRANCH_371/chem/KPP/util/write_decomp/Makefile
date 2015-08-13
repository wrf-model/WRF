
.SUFFIXES: .F .f90 .o


CPATH = ../../..

SED_FTN2 = sed -f ${CPATH}/../arch/standard.sed


OBJS    =                               \
        write_decomp.o  


MODULES =                           \
	module_kpp_$(MECH)_Precision.o \
	module_kpp_$(MECH)_Parameters.o       \
        module_kpp_$(MECH)_JacobianSP.o     


all:  
	$(RM) decomp_uses.inc
	echo USE $(MECH)_Parameters > decomp_uses.inc
	echo USE $(MECH)_JacobianSP >> decomp_uses.inc
	$(LN)  ${CPATH}/module_kpp_$(MECH)_Precision.F
	$(LN)  ${CPATH}/module_kpp_$(MECH)_Parameters.F
	$(LN)  ${CPATH}/module_kpp_$(MECH)_JacobianSP.F
	$(LN)  ${CPATH}/module_kpp_$(MECH)_Integr.F
	$(MAKE) comp
	./write_decomp.exe
	$(MAKE) integr_edit
	./integr_edit.exe $(MECH)
	$(MAKE) clean 


comp:  $(MODULES) $(OBJS)
	$(SFC) -o write_decomp.exe $(MODULES) $(OBJS)
	echo $(MECH) > mech.tmp



integr_edit: integr_edit.o
	$(SCC) -o integr_edit.exe integr_edit.o


include ${CPATH}/../configure.wrf



.F.o:
	$(RM) $@
	$(SED_FTN2) $*.F > $*.b 
	$(CPP) -I${CPATH}/../inc $(CPPFLAGS) $*.b  > $*.f90
	$(RM) $*.b
	$(SFC) -c $(FCFLAGS) $(MODULE_DIRS) $(PROMOTION) $(FCSUFFIX) $*.f90


.c.o:
	$(RM) $@
	$(SCC) -c $(CFLAGS) $*.c

clean: 
	$(RM) *.o *.tmp *.mod module_kpp_* *~ *.inc *.exe *.f90




