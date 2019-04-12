DBG     =
#DBG     = -fvar-tracking -ggdb3 -O0
#DBG     = -ffpe-trap=‘denormal’,‘invalid’,‘overflow’,‘precision’,‘underflow’,‘zero’ -fvar-tracking -ggdb3 -O0
#DBG     = -g
HOPTS   = -I. -Iawaprogs/chap02/ -Iawaprogs/chap05/
CFLAGS_LOCAL  = $(CFLAGS) $(DBG) $(HOPTS)
FFLAGS_LOCAL  = $(FFLAGS) $(DBG)
# C objects:
COBJS	= cd.o dwtai.o dwtai_w.o dwtai2.o dwtai2_w.o DWTAPartition.o idwtai.o idwtai_w.o idwtai2.o idwtai2_w.o \
          l2norm.o PrintFilter.o qf.o qf_w.o qftest.o qftest_w.o TestFilter.o xp.o
# ForTran objects:
FOBJS   = dwta_partition.o print_filter.o test_filter.o
RM = rm -f

.PHONY: phony
.SUFFIXES: .c .F90 .o

phony:
			@echo make target should be "libWavelet.a" or "lib_wavelet.a"

lib_wavelet.a:		$(FOBJS)
			$(RM) $@
			$(AR) cr $@ $(FOBJS)

libWavelet.a:		$(COBJS)
			$(RM) $@
			$(AR) cr $@ $(COBJS)

cd.o:		        awaprogs/chap05/cd.c awaprogs/chap02/common.h awaprogs/chap02/real.h awaprogs/chap05/cd.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

dwtai.o:		dwtai.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

dwtai_w.o:		dwtai_w.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

dwtai2.o:		dwtai2.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

dwtai2_w.o:		dwtai2_w.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

DWTAPartition.o:	DWTAPartition.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

dwta_partition.o:	dwta_partition.F90
			$(FC) $(FFLAGS_LOCAL) -c dwta_partition.F90

idwtai.o:		idwtai.c awaprogs/chap02/common.h awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

idwtai_w.o:		idwtai_w.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

idwtai2.o:		idwtai2.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

idwtai2_w.o:		idwtai2_w.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

l2norm.o:		l2norm.c awaprogs/chap02/real.h
			$(CC) $(CFLAGS_LOCAL) -c $<

PrintFilter.o:		PrintFilter.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

print_filter.o:		print_filter.F90
			$(FC) $(FFLAGS_LOCAL) -c print_filter.F90

qf.o:			awaprogs/chap05/qf.c awaprogs/chap02/common.h awaprogs/chap02/real.h awaprogs/chap05/oqfs.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

qf_w.o:			qf_w.c awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

qftest.o:		qftest.c
			$(CC) $(CFLAGS_LOCAL) -c $<

qftest_w.o:		qftest_w.c
			$(CC) $(CFLAGS_LOCAL) -c $<

TestFilter.o:		TestFilter.c awaprogs/chap02/common.h awaprogs/chap02/real.h awaprogs/chap05/qf.h
			$(CC) $(CFLAGS_LOCAL) -c $<

test_filter.o:		test_filter.F90
			$(FC) $(FFLAGS_LOCAL) -c test_filter.F90

xp.o:			awaprogs/chap09/xp.c awaprogs/chap02/common.h awaprogs/chap02/real.h awaprogs/chap09/xp.h
			$(CC) $(CFLAGS_LOCAL) -c $<

clean : 
			@$(RM) *.f *.o *.mod *.obj *.i

superclean : clean
			@$(RM) *.a
