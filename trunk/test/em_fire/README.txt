The fire test problems are in subdirectories. At the moment, these are

small
nested
fireflux

If you want to make your own test case subdirectory, all you need to do 
is to create soft links for ideal.exe and wrf.exe pointing the the parent 
directory, and to create the files namelist.input and input_sounding 
(best by modifying a copy from another subdirectory).

Do not just copy one of the existing subdirectories, the soft links might not 
be copied properly.

