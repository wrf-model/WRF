# https://cmake.org/cmake/help/latest/module/FindMPI.html#variables-for-locating-mpi
set( MPI_Fortran_COMPILER "{DM_FC}" )
set( MPI_C_COMPILER       "{DM_CC}" )

# https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER.html
set( CMAKE_Fortran_COMPILER "{SFC}" )
set( CMAKE_C_COMPILER       "{SCC}" )

# Our own addition
set( CMAKE_C_PREPROCESSOR       "{CPP}" )
set( CMAKE_C_PREPROCESSOR_FLAGS  {CPP_FLAGS} )

# https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_FLAGS_INIT.html
set( CMAKE_Fortran_FLAGS_INIT    "{SFC_FLAGS} {FCBASEOPTS} {BYTESWAPIO}" )
set( CMAKE_C_FLAGS_INIT          "{SCC_FLAGS} {CFLAGS_LOCAL}" )

# https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_FLAGS_CONFIG_INIT.html
set( CMAKE_Fortran_FLAGS_Debug_INIT    "{FCDEBUG}" )
set( CMAKE_Fortran_FLAGS_Release_INIT  "" )
set( CMAKE_C_FLAGS_Debug_INIT    "" )
set( CMAKE_C_FLAGS_Release_INIT  "" )

# Project specifics now
set( WRF_MPI_Fortran_FLAGS  "{DM_FC_FLAGS}"   )
set( WRF_MPI_C_FLAGS        "{DM_CC_FLAGS}"   )
set( WRF_ARCH_LOCAL         "{ARCH_LOCAL}"    )
set( WRF_M4_FLAGS           "{M4_FLAGS}"      )
set( WRF_FCOPTIM            "{FCOPTIM}"       )
set( WRF_FCNOOPT            "{FCNOOPT}"       )
set( WRF_LINK_FLAGS         "{LDFLAGS_LOCAL}" )
