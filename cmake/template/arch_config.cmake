# https://cmake.org/cmake/help/latest/module/FindMPI.html#variables-for-locating-mpi
set( MPI_Fortran_COMPILER "{DM_FC}" )
set( MPI_C_COMPILER       "{DM_CC}" )

set( CMAKE_Fortran_COMPILER "{SFC}" )
set( CMAKE_C_COMPILER       "{SCC}" )

set( CMAKE_Fortran_FLAGS    "{FCBASEOPTS} {BYTESWAPIO}" )
set( CMAKE_C_FLAGS          "{CFLAGS_LOCAL}" )

set( CMAKE_Fortran_FLAGS_DEBUG    "{FCDEBUG}" )

set( CMAKE_Fortran_FLAGS_RELEASE  "{FCOPTIM}" )

# Project specifics now
set( WRF_MPI_Fortran_FLAGS  "{DM_FC_FLAGS}" )
set( WRF_MPI_C_FLAGS        "{DM_CC_FLAGS}" )
set( WRF_ARCH_LOCAL         "{ARCH_LOCAL}"  )
set( WRF_M4_FLAGS           "{M4_FLAGS}"    )
set( WRF_FCOPTIM            "{FCOPTIM}"     )
set( WRF_FCNOOPT            "{FCNOOPT}"     )