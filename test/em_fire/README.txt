There are currently two fire test cases - hill_simple and two_fires.  
The files necessary for running these are in the top-level em_fire/
directory.  To run a fire case, it will be necessary to have files
named 'namelist.input' and 'input_sounding' in the em_fire/ directory.
If you wish to use one of the provided test cases,
you will need to link them to their generic names
(for example, for the two_fires case):

ln -sf namelist.input_two_fires namelist.input
ln -sf input_sounding_two_fires input_sounding

Currently the default namelist.input is linked to the hill_simple case.

If you want to make your own test case, you will simply need to 
create the files namelist.input and input_sounding that will
correspond to your case.
(best by modifying a copy from another case)


