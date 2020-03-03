# Installing WRF on Cygwin

1. Download https://cygwin.com/setup-x86_64.exe
2. (Optional) Download https://cygwin.com/setup-x86_64.exe.sig and verify the signature
3. Run `setup-x86_64.exe`
	- Select `Install from Internet`
	- Pick an installation root (installing directly into `C:\` is not
	  recommended)
	- Pick a directory for a download cache
		- If installing Cygwin on multiple computers, these files can
          be re-used, skipping the download step on those computers
	- `Direct Connection` is nice if it works, but `System Proxy Settings`
	  may fill in useful data from your OS.
	- Select a mirror near you
    - Change package view to `Full`
    - Select for install at least:
		- gcc-core (OpenMP for smpar)
		- gcc-fortran
		- libnetcdf-fortran-devel
		- openmpi (MPI for dmpar)
		- libopenmpi-devel (MPI for dmpar)
		- libjasper-devel (GRIB)
		- perl
		- tcsh
	- Select install
	- Accept the packages pulled in as dependencies
	- Wait for download, install, and postinstall steps.  This will
      take about an hour the first time through
	- Decide whether you want shortcuts from the Desktop or Start Menu
4. Open mintty
   - If you didn't install shortcuts, you can run the `Cygwin.bat`
     file in the root of the Cygwin install tree
5. Follow usual instructions for installing WRF
   - Download and unpack source
   - `./clean -a`
   - Export variables so WRF can find everything:
	 - `export NETCDF=/usr/ NETCDF4=1 HDF5=/usr`
	 - (Optional) `export JASPER=/usr JASPERLIB=/usr/lib JASPERINC=/usr/include`
	 - (Optional) `export WRFIO_NCD_NO_LARGE_FILE_SUPPORT=0 NETCDF_classic=0`
   - `./configure`, follow directions
   - `./compile`, follow directions
   - ...


If the compiler reports problems due to `mpi.mod` or `netcdf.mod`
being compiled with a different version of gfortran, subscribe to the
[cygwin mailing list](https://cygwin.com/lists.html) and send a
message there to remind the maintainer that `openmpi` or
`netcdf-fortran` was built with an old version of `gfortran` and to
ask them nicely to update it.

If the (volunteer) maintainer does not have time, or you want to build
this yourself, re-run `setup-x86_64.exe` and check the `Source?` box
next to `openmpi` or `libnetcdf-fortran-devel`, install `cygport` and
finish the install.  Unpack the archives in `/usr/src` (`tar xaf ...`
should do the trick), and `cd` into the new directory.  There will be
a `.cygport` file with the build script.  Edit this to add a `.1`
after the current release field.  Save this file, then run
`cygport ${package}.cygport download prep compile test install package`
If there are errors, try to fix them, then re-run
`cygport ${package}.cygport compile test install package`
Once it compiles, you can follow the directions in
https://cygwin.com/package-server.html#overlay
to set up a local package server and install from that, or you can run
```bash
cd /
for name in /usr/src/${package}-${version}*/${package}-${version}*/dist/**.tar.xz;
do
    tar xaf ${name}
done
```
then re-run setup to make sure everything still works.
The first method plays much nicer with other tools.

If you feel nice, you can tell the maintainer whether the package
compiled, whether the tests passed and what you needed to do to make
it do that.  The contents of
`/usr/src/${package}-${version}*/${package}-${version}*/patch` might
help with that.
