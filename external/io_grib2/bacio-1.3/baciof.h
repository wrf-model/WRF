!     Include file to define variables for Fortran to C interface(s)
!     Robert Grumbine 16 March 1998
      INTEGER,PARAMETER:: BACIO_OPENR=1      ! Open file for read only
      INTEGER,PARAMETER:: BACIO_OPENW=2      ! Open file for write only
      INTEGER,PARAMETER:: BACIO_OPENRW=4     ! Open file for read or write
      INTEGER,PARAMETER:: BACIO_CLOSE=8      ! Close file
      INTEGER,PARAMETER:: BACIO_READ=16      ! Read from the file
      INTEGER,PARAMETER:: BACIO_WRITE=32     ! Write to the file
      INTEGER,PARAMETER:: BACIO_NOSEEK=64    ! Start I/O from previous spot
      INTEGER,PARAMETER:: BACIO_OPENWT=128   ! Open for write only with truncation
      INTEGER,PARAMETER:: BACIO_OPENWA=256   ! Open for write only with append
