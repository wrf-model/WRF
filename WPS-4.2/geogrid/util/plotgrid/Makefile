MAKE = make -i -r

default:
	uname -s | grep AIX; \
	if [ $$? = 0 ]; then echo "Compiling for IBM"; \
	( cd src; \
	$(MAKE) FC="xlf90_r" CC="cc" FFLAGS="-qfree" CFLAGS="" LDFLAGS="" CPPFLAGS="-D_METGRID -DIO_NETCDF" ) \
	else \
	uname -s | grep OSF1; \
	if [ $$? = 0 ]; then echo "Compiling for DEC"; \
	( cd src; \
	$(MAKE) FC="f90" CC="cc" FFLAGS="" CFLAGS="" LDFLAGS="" CPPFLAGS="-D_METGRID -DIO_NETCDF" ) \
	else \
	uname -s | grep IRIX; \
	if [ $$? = 0 ]; then echo "Compiling for SGI"; \
	( cd src; \
	$(MAKE) FC="f90" CC="cc" FFLAGS="" CFLAGS="" LDFLAGS="" CPPFLAGS="-D_METGRID -DIO_NETCDF" ) \
	else \
	uname -s | grep Linux; \
	if [ $$? = 0 ]; then echo "Compiling for Linux"; \
	( cd src; \
	$(MAKE) WRF_DIR="/users/duda/repos/WRFV3" FC="ncargf90" CC="pgcc" FFLAGS="-Mfree -byteswapio -g -Mbounds" CFLAGS="" LDFLAGS="-g -Mbounds -byteswapio" CPPFLAGS="-D_METGRID -DIO_NETCDF -D_UNDERSCORE" ) \
	else \
	uname -s | grep SunOS; \
	if [ $$? = 0 ]; then echo "Compiling for Sun"; \
	( cd src; \
	$(MAKE) FC="f90" CC="cc" FFLAGS="" CFLAGS="" LDFLAGS="" CPPFLAGS="-D_METGRID -DIO_NETCDF" ) \
	else \
	echo "Unrecognized system."; \
	fi; \
	fi; \
	fi; \
	fi; \
	fi
	rm -f plotgrid.exe ; ln -s src/plotgrid.exe plotgrid.exe

clean:
	( rm -f plotgrid.exe; cd src ; make clean )
clobber:
	rm -f plotgrid.exe ; (cd src ; make clobber )
