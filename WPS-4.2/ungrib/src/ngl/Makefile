include $(DEV_TOP)/configure.wps

all: w3/libw3.a g2/libg2_4.a

w3/libw3.a :
	( cd w3 ; $(MAKE) DEV_TOP="${DEV_TOP}" all ; $(RANLIB) libw3.a )
	$(LN) w3/libw3.a .

g2/libg2_4.a :
	( cd g2 ; $(MAKE) DEV_TOP="$(DEV_TOP)" all ; $(RANLIB) libg2_4.a )
	$(LN) g2/libg2_4.a .

clean:
	$(RM) *.a
	( cd w3 ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd g2 ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )

superclean:	clean
