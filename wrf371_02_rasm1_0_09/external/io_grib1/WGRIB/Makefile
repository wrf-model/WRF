.IGNORE:

.SUFFIXES: .c .o

CC=cc

.c.o:
	$(CC) -c -O $<

obj=wgrib_main.o seekgrib.o ibm2flt.o readgrib.o intpower.o cnames.o BDSunpk.o \
  flt2ieee.o wrtieee.o levels.o PDStimes.o missing.o  \
  nceptable_reanal.o nceptable_opn.o ensemble.o \
  ombtable.o ec_ext.o gribtable.o gds_grid.o PDS_date.o ectable_128.o \
  ectable_129.o ectable_130.o ectable_131.o ectable_140.o ectable_150.o \
  ectable_151.o ectable_160.o ectable_170.o ectable_180.o nceptab_129.o \
  dwdtable_002.o dwdtable_201.o dwdtable_202.o dwdtable_203.o \
  cptectable_254.o nceptab_130.o nceptab_131.o

all: wgrib.exe

archive: wgrib.exe

wgrib.exe: $(obj)
	$(CC) -o wgrib.exe $(obj) #-lm
	( cd .. ; \rm -f wgrib.exe ; \ln -sf WGRIB/wgrib.exe wgrib.exe ; cd WGRIB )

clean:	
	\rm -f $(obj) *.obj wgrib.exe
	( cd .. ; \rm -f wgrib.exe ; cd WGRIB )
