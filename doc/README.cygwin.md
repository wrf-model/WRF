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
