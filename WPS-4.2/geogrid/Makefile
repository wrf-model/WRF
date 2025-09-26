#	WPS component makefile

include ../configure.wps

bad_idea:
	clear ;
	@echo " "
	@echo " "
	@echo "go up a directory and type 'compile' to build WPS"
	@echo " "
	@echo " "

all:
	@if [ -e $(WRF_DIR_PRE)$(WRF_DIR)/frame/module_internal_header_util.o ] ; then \
		( cd src ; \
                  if [ "$(COMPILING_ON_CYGWIN_NT)" = yes ] ; then \
                    WRF_DIR2=$(WRF_DIR) ; \
                  else \
                    WRF_DIR2=$(WRF_DIR_PRE)$(WRF_DIR) ; \
                  fi ; \
		  $(MAKE) $(TARGET) \
			  WRF_DIR="$$WRF_DIR2" \
			  FC="$(FC)" \
			  CC="$(CC)" \
			  CPP="$(CPP)" \
			  FFLAGS="$(FFLAGS)" \
			  CFLAGS="$(CFLAGS)" \
			  LDFLAGS="$(LDFLAGS)" \
			  CPPFLAGS="$(CPPFLAGS) -D_$(CPP_TARGET)" ) ; \
		if [ -e $(TARGET) ] ; then \
			$(RM) $(TARGET) ; \
		fi ; \
		if [ -e ../$(TARGET) ] ; then \
			$(RM) ../$(TARGET) ; \
		fi ; \
		if [ -e src/$(TARGET) ] ; then \
			$(LN) src/$(TARGET) . ; \
		fi ; \
	else \
		echo " " ; \
		echo " ***************************************************************************************" ; \
		echo " Error : Not building geogrid. Check whether WRF is compiled in $(WRF_DIR_PRE)$(WRF_DIR)" ; \
		echo " ***************************************************************************************" ; \
		echo " " ; \
	fi

clean:
	if [ -e $(TARGET) ] ; then \
		$(RM) $(TARGET) ; \
	fi
	if [ -e ../$(TARGET) ] ; then \
		$(RM) ../$(TARGET) ; \
	fi
	( cd src ; $(MAKE) clean )

superclean: clean
