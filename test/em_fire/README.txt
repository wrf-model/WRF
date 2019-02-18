All files to run fire test cases are in this directory em_fire.
The files necessary for running the hill_simple case are linked directly 
in this directory.

Subdirectories for the other test cases and the links necessary for
running the test cases there are created by the script create_links.sh,
which is executed when running ./compile em_fire in the top level WRF 
directory, The subdirectories are destroyed when running ./clean -a.

If you want to make your own test case, you will simply need to 
create the files namelist.input and input_sounding that will
correspond to your case (best by modifying a copy from another case).
