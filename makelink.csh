#!/bin/csh -f

 set echo

 set dirlist = ( \
	/glade/apps/opt/pnetcdf/1.4.1/intel/13.1.2 \
	/glade/apps/opt/netcdf/4.3.0/intel/13.1.2 \
	/glade/apps/opt/hdf5-mpi/1.8.11/intel/13.1.2 \
	/glade/apps/opt/zlib/1.2.7/intel/12.1.4 \
	/glade/p/work/huangwei/lib/pio/1.8.12 \
	)

 cd include

 foreach dir ( $dirlist )
    ln -sf $dir/include/*.h .
    ln -sf $dir/include/*.inc .
    ln -sf $dir/include/*.mod .
 end

#ln -sf /glade/p/work/huangwei/lib/netcdf/fortran-4.4-beta1/include/*.inc .
#ln -sf /glade/p/work/huangwei/lib/netcdf/fortran-4.4-beta1/include/*.mod .

 cd ..

 cd lib

 foreach dir ( $dirlist )
    ln -sf $dir/lib/*.a .
    ln -sf $dir/lib/*.la .
 end

