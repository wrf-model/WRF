#
DEVTOP = ../..
include ../../configure.user
#
OBJS=	\
        skewt_background.o  skewt_opngks.o    skewt_clsgks.o  \
        skewt_esat.o        skewt_mappings.o  skewt_wndbarb.o \
        skewt_os.o          skewt_tmr.o       skewt_tsa.o     \
        skewt_w.o           skewt.o    


SRCF=	\
        skewt_background.f  skewt_opngks.f    skewt_clsgks.f  \
        skewt_esat.f        skewt_mappings.f  skewt_wndbarb.f \
        skewt_os.f          skewt_tmr.f       skewt_tsa.f     \
        skewt_w.f           skewt.f 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	$(OBJS)
	$(AR) $(LIBDIR)$(FDVARLIB) $(OBJS)

source:	$(SRCF)

clean:
	$(RM_CMD) "#"*

