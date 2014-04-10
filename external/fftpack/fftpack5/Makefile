.SUFFIXES: .F .o

OBJ	= \
c1f2kb.o  cfft1b.o  cmf3kf.o  cosqb1.o  costmi.o   dcosq1f.o  dfftb1.o   mradb2.o  mrfti1.o  r1fgkf.o     rfft2i.o  sinqmi.o  z1f2kf.o  zfft1f.o  zmf4kb.o \
c1f2kf.o  cfft1f.o  cmf4kb.o  cosqf1.o  d1f2kb.o   dcosq1i.o  dfftf1.o   mradb3.o  msntb1.o  r4_factor.o  rfftb1.o  sint1b.o  z1f3kb.o  zfft1i.o  zmf4kf.o \
c1f3kb.o  cfft1i.o  cmf4kf.o  cosqmb.o  d1f2kf.o   dcosqb1.o  dffti1.o   mradb4.o  msntf1.o  r4_mcfti1.o  rfftf1.o  sint1f.o  z1f3kf.o  zfft2b.o  zmf5kb.o \
c1f3kf.o  cfft2b.o  cmf5kb.o  cosqmf.o  d1f3kb.o   dcosqf1.o  dsint1b.o  mradb5.o  r1f2kb.o  r4_tables.o  rffti1.o  sint1i.o  z1f4kb.o  zfft2f.o  zmf5kf.o \
c1f4kb.o  cfft2f.o  cmf5kf.o  cosqmi.o  d1f3kf.o   dcost1b.o  dsint1f.o  mradbg.o  r1f2kf.o  r8_factor.o  rfftmb.o  sintb1.o  z1f4kf.o  zfft2i.o  zmfgkb.o \
c1f4kf.o  cfft2i.o  cmfgkb.o  cost1b.o  d1f4kb.o   dcost1f.o  dsint1i.o  mradf2.o  r1f3kb.o  r8_mcfti1.o  rfftmf.o  sintf1.o  z1f5kb.o  zfftmb.o  zmfgkf.o \
c1f5kb.o  cfftmb.o  cmfgkf.o  cost1f.o  d1f4kf.o   dcost1i.o  dsintb1.o  mradf3.o  r1f3kf.o  r8_tables.o  rfftmi.o  sintmb.o  z1f5kf.o  zfftmf.o  zmfm1b.o \
c1f5kf.o  cfftmf.o  cmfm1b.o  cost1i.o  d1f5kb.o   dcostb1.o  dsintf1.o  mradf4.o  r1f4kb.o  rfft1b.o     sinq1b.o  sintmf.o  z1fgkb.o  zfftmi.o  zmfm1f.o \
c1fgkb.o  cfftmi.o  cmfm1f.o  costb1.o  d1f5kf.o   dcostf1.o  mcsqb1.o   mradf5.o  r1f4kf.o  rfft1f.o     sinq1f.o  sintmi.o  z1fgkf.o  zmf2kb.o           \
c1fgkf.o  cmf2kb.o  cosq1b.o  costf1.o  d1fgkb.o   dfft1b.o   mcsqf1.o   mradfg.o  r1f5kb.o  rfft1i.o     sinq1i.o  xercon.o  z1fm1b.o  zmf2kf.o           \
c1fm1b.o  cmf2kf.o  cosq1f.o  costmb.o  d1fgkf.o   dfft1f.o   mcstb1.o   mrftb1.o  r1f5kf.o  rfft2b.o     sinqmb.o  xerfft.o  z1fm1f.o  zmf3kb.o           \
c1fm1f.o  cmf3kb.o  cosq1i.o  costmf.o  dcosq1b.o  dfft1i.o   mcstf1.o   mrftf1.o  r1fgkb.o  rfft2f.o     sinqmf.o  z1f2kb.o  zfft1b.o  zmf3kf.o 


AR = ar
ARFLAGS = cr
TARGET = libfftpack.a

library: $(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

.F.o:
	$(CPP) $(CPPFLAGS) $*.F  > $*.f90
	$(FC) -c $(FFLAGS) $*.f90
	$(RM) -f $*.f90

clean:
	rm -f $(OBJ) $(TARGET) *.obj

superclean: clean
