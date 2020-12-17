# Set here the desired C compiler and its optimization options
CC   = gcc
COPT = -O -Wall  

# To create Matlab gateway routines
# Note: use $(CC) as the mex C compiler
MEX  = mex

HEADERS = KPP_ROOT_Global.h  KPP_ROOT_Parameters.h  KPP_ROOT_Sparse.h

SPSRC = KPP_ROOT_JacobianSP.c \
	KPP_ROOT_HessianSP.c  \
	KPP_ROOT_StoichiomSP.c

SPOBJ = KPP_ROOT_JacobianSP.o \
	KPP_ROOT_HessianSP.o  \
	KPP_ROOT_StoichiomSP.o


SRC =   KPP_ROOT_Main.c      KPP_ROOT_Integrator.c   \
	KPP_ROOT_Function.c  KPP_ROOT_Initialize.c   \
	KPP_ROOT_Jacobian.c  KPP_ROOT_LinearAlgebra.c\
	KPP_ROOT_Rates.c     KPP_ROOT_Hessian.c      \
	KPP_ROOT_Stoichiom.c KPP_ROOT_Util.c         \
	KPP_ROOT_Monitor.c

OBJ =   KPP_ROOT_Main.o      KPP_ROOT_Integrator.o   \
	KPP_ROOT_Function.o  KPP_ROOT_Initialize.o   \
	KPP_ROOT_Jacobian.o  KPP_ROOT_LinearAlgebra.o\
	KPP_ROOT_Rates.o     KPP_ROOT_Hessian.o      \
	KPP_ROOT_Stoichiom.o KPP_ROOT_Util.o         \
	KPP_ROOT_Monitor.o

STOCHSRC = KPP_ROOT_Stochastic.c 
STOCHOBJ = KPP_ROOT_Stochastic.o 

all:    exe

exe:	$(HEADERS) $(SPOBJ) $(OBJ)
	$(CC) $(COPT) $(SPOBJ) $(OBJ) -lm -o KPP_ROOT.exe	

stochastic:$(HEADERS) $(SPOBJ) $(OBJ) $(STOCHOBJ)
	$(CC) $(COPT) $(SPOBJ) $(OBJ) $(STOCHOBJ) -lm \
	-o KPP_ROOT_stochastic.exe	

mex:    $(HEADERS) $(SPOBJ) $(OBJ)
	$(MEX) CC#$(CC) -O KPP_ROOT_mex_Fun.c     -lm $(SPOBJ) $(OBJ)
	$(MEX) CC#$(CC) -O KPP_ROOT_mex_Jac_SP.c  -lm $(SPOBJ) $(OBJ)
	$(MEX) CC#$(CC) -O KPP_ROOT_mex_Hessian.c -lm $(SPOBJ) $(OBJ)


clean:
	rm -f $(SPOBJ) $(OBJ) KPP_ROOT.exe KPP_ROOT_*.mexglx KPP_ROOT*.dat

distclean:
	rm -f $(SPOBJ) $(OBJ) KPP_ROOT.exe KPP_ROOT*.dat \
	KPP_ROOT_*.c KPP_ROOT_*.h KPP_ROOT_*.map KPP_ROOT_*.mexglx

KPP_ROOT_Monitor.o: KPP_ROOT_Monitor.c $(HEADERS)
	$(CC) $(COPT) -c $<

KPP_ROOT_JacobianSP.o: KPP_ROOT_JacobianSP.c $(HEADERS)
	$(CC) $(COPT) -c $<

KPP_ROOT_HessianSP.o: KPP_ROOT_HessianSP.c  $(HEADERS)
	$(CC) $(COPT) -c $<

KPP_ROOT_StoichiomSP.o: KPP_ROOT_StoichiomSP.c $(HEADERS)
	$(CC) $(COPT) -c $<

KPP_ROOT_Main.o: KPP_ROOT_Main.c KPP_ROOT_Initialize.o $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Integrator.o: KPP_ROOT_Integrator.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Initialize.o: KPP_ROOT_Initialize.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Function.o: KPP_ROOT_Function.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Stochastic.o: KPP_ROOT_Stochastic.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Jacobian.o: KPP_ROOT_Jacobian.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_LinearAlgebra.o: KPP_ROOT_LinearAlgebra.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Rates.o: KPP_ROOT_Rates.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Hessian.o:  KPP_ROOT_Hessian.c $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Stoichiom.o: KPP_ROOT_Stoichiom.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

KPP_ROOT_Util.o: KPP_ROOT_Util.c  $(HEADERS) $(SPOBJ)
	$(CC) $(COPT) -c $<

