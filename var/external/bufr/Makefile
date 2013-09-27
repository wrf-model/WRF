.SUFFIXES: .c .f .F .o

include make.filelist

AR = ar
ARFLAGS = cr
TARGET = libbufr.a

library:$(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

.c.o:
	$(RM) $@
	$(CC) -o $@ -c $(CFLAGS) -DUNDERSCORE $*.c

.f.o:
	$(FC) -c $(FFLAGS) $< 

.F.o:
	./preproc.sh $*.F
	$(FC) -c $(FFLAGS) $*.f

clean:
	rm -f $(OBJ) $(TARGET) irev.f wrdlen.f

superclean: clean
