include $(DEV_TOP)/configure.wps

bad_idea:
	clear
	@echo ' *************** '
	@echo " "
	@echo " "
	@echo "Go up a directory and type 'compile'"
	@echo " "
	@echo " "
	@echo ' *************** '

all:
	( cd src/ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" RANLIB="$(RANLIB)" all )
	( cd src ; \
        if [ "$(COMPILING_ON_CYGWIN_NT)" = yes ] ; then \
          WRF_DIR2=$(WRF_DIR) ; \
        else \
          WRF_DIR2=$(WRF_DIR_PRE)$(WRF_DIR) ; \
        fi ; \
	$(MAKE) $(TARGET) \
		WRF_DIR="$$WRF_DIR2" \
		FC="$(SFC)" \
		CC="$(CC)" \
		CPP="$(CPP)" \
		FFLAGS="$(FFLAGS)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		CPPFLAGS="$(CPPFLAGS) -D_$(CPP_TARGET)" )
	if [ -h $(TARGET) ] ; then \
	        $(RM) $(TARGET) ; \
	fi ; \
	if [ -h ../$(TARGET) ] ; then \
		$(RM) ../$(TARGET) ; \
	fi ; \
	if [ -e src/$(TARGET) ] ; then \
		$(LN) src/$(TARGET) . ; \
	fi

clean:
	( cd src/ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	if [ -h $(TARGET) ] ; then \
		$(RM) $(TARGET) ; \
	fi
	if [ -h ../$(TARGET) ] ; then \
		$(RM) ../$(TARGET) ; \
	fi
	( cd src ; $(MAKE) clean )

superclean: clean
