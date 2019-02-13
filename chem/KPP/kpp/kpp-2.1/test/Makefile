#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPILER = GNU
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX

FC_GNU     = g95
FOPT_GNU   = -cpp -O -pg -fbounds-check 
FC_LAHEY   = lf95
FOPT_LAHEY = -Cpp --pca
#FOPT_LAHEY = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap
FC_INTEL   = ifort 
FOPT_INTEL = -cpp -O -mp -pc80 -prec_div -tpp7 -implicitnone
FC_PGF     = pgf90
FOPT_PGF   = -Mpreprocess -O -fast -pc 80 -Kieee
FC_HPUX    = f90
FOPT_HPUX  = -O -u +Oall +check=on

# define FULL_ALGEBRA for non-sparse integration
FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) # -DFULL_ALGEBRA

LIBS =
#LIBS = -llapack -lblas

# Command to create Matlab mex gateway routines 
# Note: use $(FC) as the mex Fortran compiler
MEX  = mex

GENSRC = saprc99_Precision.f90  \
	 saprc99_Parameters.f90     \
	 saprc99_Global.f90  

GENOBJ = saprc99_Precision.o    \
	 saprc99_Parameters.o       \
	 saprc99_Global.o     

FUNSRC = saprc99_Function.f90 
FUNOBJ = saprc99_Function.o 

JACSRC = saprc99_JacobianSP.f90  saprc99_Jacobian.f90
JACOBJ = saprc99_JacobianSP.o    saprc99_Jacobian.o

HESSRC = saprc99_HessianSP.f90   saprc99_Hessian.f90
HESOBJ = saprc99_HessianSP.o     saprc99_Hessian.o

STMSRC = saprc99_StoichiomSP.f90 saprc99_Stoichiom.f90 
STMOBJ = saprc99_StoichiomSP.o   saprc99_Stoichiom.o

UTLSRC = saprc99_Rates.f90 saprc99_Util.f90 saprc99_Monitor.f90
UTLOBJ = saprc99_Rates.o   saprc99_Util.o   saprc99_Monitor.o

LASRC  = saprc99_LinearAlgebra.f90 
LAOBJ  = saprc99_LinearAlgebra.o   

STOCHSRC = saprc99_Stochastic.f90 
STOCHOBJ = saprc99_Stochastic.o 

MAINSRC = saprc99_Main.f90   saprc99_Initialize.f90   saprc99_Integrator.f90 saprc99_Model.f90
MAINOBJ = saprc99_Main.o     saprc99_Initialize.o     saprc99_Integrator.o   saprc99_Model.o 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(GENOBJ) $(FUNOBJ) $(JACOBJ) $(HESOBJ) $(STMOBJ) \
	 $(UTLOBJ) $(LAOBJ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(ALLOBJ) $(MAINOBJ) 
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o saprc99.exe

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o saprc99_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O saprc99_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O saprc99_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O saprc99_mex_Hessian.f90 $(ALLOBJ)

clean:
	@rm -f saprc99*.o saprc99*.mod \
	saprc99*.dat saprc99.exe saprc99*.mexglx \
	saprc99.map

distclean:
	@rm -f saprc99*.o saprc99*.mod \
	saprc99*.dat saprc99.exe saprc99.map \
	saprc99*.f90 saprc99_*.mexglx

saprc99_Precision.o: saprc99_Precision.f90 
	$(FC) $(FOPT) -c $<

saprc99_Parameters.o: saprc99_Parameters.f90 \
	            saprc99_Precision.o
	$(FC) $(FOPT) -c $<

saprc99_Monitor.o: saprc99_Monitor.f90 \
	             saprc99_Precision.o
	$(FC) $(FOPT) -c $<

saprc99_Global.o: saprc99_Global.f90 \
	            saprc99_Parameters.o saprc99_Precision.o
	$(FC) $(FOPT) -c $<

saprc99_Initialize.o: saprc99_Initialize.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

saprc99_Function.o: saprc99_Function.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

saprc99_Stochastic.o: saprc99_Stochastic.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

saprc99_JacobianSP.o: saprc99_JacobianSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

saprc99_Jacobian.o: saprc99_Jacobian.f90  $(GENOBJ) saprc99_JacobianSP.o
	$(FC) $(FOPT) -c $<

saprc99_LinearAlgebra.o: saprc99_LinearAlgebra.f90 $(GENOBJ) saprc99_JacobianSP.o
	$(FC) $(FOPT) -c $<

saprc99_Rates.o: saprc99_Rates.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

saprc99_HessianSP.o: saprc99_HessianSP.f90  $(GENOBJ)
	$(FC) $(FOPT) -c $<

saprc99_Hessian.o:  saprc99_Hessian.f90 $(GENOBJ) saprc99_HessianSP.o
	$(FC) $(FOPT) -c $<

saprc99_StoichiomSP.o: saprc99_StoichiomSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

saprc99_Stoichiom.o: saprc99_Stoichiom.f90  $(GENOBJ) saprc99_StoichiomSP.o
	$(FC) $(FOPT) -c $<

saprc99_Util.o: saprc99_Util.f90  $(GENOBJ) saprc99_Monitor.o
	$(FC) $(FOPT) -c $<

saprc99_Main.o: saprc99_Main.f90  $(ALLOBJ) saprc99_Initialize.o saprc99_Model.o saprc99_Integrator.o
	$(FC) $(FOPT) -c $<

saprc99_Model.o: saprc99_Model.f90  $(ALLOBJ) saprc99_Integrator.o
	$(FC) $(FOPT) -c $<

saprc99_Integrator.o: saprc99_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<
