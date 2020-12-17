#	Top-level Makefile for 3DVAR

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -f Makefile $(J) 
RM	=	/bin/rm -f
RM_LIST	=	*.o *.M core i.* *.kmo *.mod fort.* *.out namelist.* *~ */*~ *.exe
BUFR	=	$(BUFR_CPP)

#	Targets for supported architectures

all:
	( $(CD) src ; $(MAKE) obsproc.exe )
	( $(RM) obsproc.exe ;   $(LN) src/obsproc.exe . )

clean:
#	-@if [ -d MAP_plot ]; then $(CD) MAP_plot ; $(MAKE) clean ; fi
	@$(RM) core .tmpfile compiler_macros
	@$(RM) *.exe
	@$(RM) *.out
	@$(RM) fort.*
	@$(RM) obs_gts.ascii.* *.diag
	@$(RM) HEIGHT.txt PRES.txt TEMP.txt RH.txt WIND.txt
	@$(RM) obs_ssmi*.3dvar obs_gts*.3dvar
	@($(CD) src ; $(RM) *.o *.f core *.kmo ctmi* *.mod *.lst *_out_* output *.out obsproc.exe )

