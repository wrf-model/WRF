! The purpose of this program is to scan through all of the 
! lines of a Fortran file to detect if there are any character
! codes (excluding the ubiquitously used tab character) outside
! the range of 32 to 127 (the standard ASCII character set).  These
! characters can occasionally get introduced with physics routines
! when variable names or units use special characters (superscripts, 
! hats, etc), or when short- or long-dashes are used in citing
! references.

! This program detects the non-ASCII character set, and gives the 
! line number and column count (for subsequent editing).

! How to build the finder program: 
! gfortran -ffree-form non_ascii_finder.F

! usage:
! ./a.out file.F

! Typically, this is run twice.  The first time with the verbose
! switch set to 0, so that it can be used in a find command.  Once
! the list of problematic files is assembled, re-run with verbose
! set to 1, so to find the offending characters and line numbers.

! As a test, running the executable on this file will locate that
!   --> this line has a problem with the superscript numeral 2: [W/m²]

PROGRAM non_ascii_finder

   IMPLICIT NONE

   INTEGER , PARAMETER :: MAX_LENGTH  = 256
   INTEGER , PARAMETER :: FIRST_VALID =  32
   INTEGER , PARAMETER :: LAST_VALID  = 127
   INTEGER , PARAMETER :: TAB         =   9
   INTEGER , PARAMETER :: input_unit  =  10

   !  Information from a single line of the input file.

   CHARACTER (len=MAX_LENGTH) :: input_string
   CHARACTER (LEN=1  ) :: test_variable

   !  The name of the input file (the file that will be
   !  opened and read).

   CHARACTER (LEN=MAX_LENGTH) :: filename
   INTEGER :: filename_length

   INTEGER :: num_args

   CHARACTER (LEN=MAX_LENGTH) :: verbose_arg
   INTEGER :: arg_len
   INTEGER :: verbose  ! 0=typical of find output, just a filename when there are troubles
                         ! 1=typical of single file searching, what lines need fixing
                         ! 2=typical of debugging, output info on both OK files and problematic files

   INTEGER :: status
   INTEGER :: ind
   INTEGER :: line_count, problem_line_count

   !  Get the command line info.

   num_args = COMMAND_ARGUMENT_COUNT()

   !  Do we have enough arguments, we want two.

   IF ( num_args .NE. 2 ) THEN
      PRINT *,'Usage:'
      PRINT *,'./a.out <verbose level> <filename>'
      PRINT *,'where <verbose level> is -v when using this program with "find", and'
      PRINT *,'      <verbose level> is -V when processing a single file'
!     PRINT *,'      <verbose level> is -VV is for developers and debugging'
      PRINT *,'where <filename> is a WRF Fortran source file'
      STOP 1
   END IF

   CALL GET_COMMAND_ARGUMENT ( NUMBER=1, VALUE=verbose_arg, LENGTH=arg_len, STATUS=status )

   IF ( status .NE. 0 ) THEN
      PRINT *,'The verbose switch is either "-v" or "-V"'
      STOP 2
   END IF

   IF      ( verbose_arg(1:3) .EQ. "-VV" ) THEN
      verbose = 2      
   ELSE IF ( verbose_arg(1:2) .EQ. "-v"  ) THEN
      verbose = 0      
   ELSE IF ( verbose_arg(1:2) .EQ. "-V"  ) THEN
      verbose = 1      
   ELSE
      PRINT *,'The verbose switch is either "-v" or "-V"'
      PRINT *,'Entered: ',verbose_arg(1:arg_len)
      STOP 3
   END IF 

   CALL GET_COMMAND_ARGUMENT ( NUMBER=2, VALUE=filename, LENGTH=filename_length, STATUS=status )

   IF ( status .EQ. 0 ) THEN
      OPEN ( UNIT     = input_unit     , &
             FILE     = TRIM(filename) , & 
             ACCESS   = 'SEQUENTIAL'   , &
             ACTION   = 'READ'         , &
             FORM     = 'FORMATTED'    , &
             POSITION = 'ASIS'         , &
             STATUS   = 'OLD'          , &
             IOSTAT   = status           )
   END IF

   IF ( status .NE. 0 ) THEN
      PRINT *,'Hmmm, troubles trying to open ',TRIM(filename),' for READ.'
      STOP 4
   END IF

   !  Initializations

   line_count = 1
   problem_line_count = 0

   !  Loop over each line of the input file.

   big_read_loop : DO 

      DO ind = 1 , MAX_LENGTH
         input_string(ind:ind) = ' '
      END DO      
      READ (input_unit,FMT='(A)',IOSTAT=status) input_string

      IF ( status < 0 ) THEN
         IF ( verbose .EQ. 2 ) THEN
            PRINT *,TRIM(filename),', End of file after attempting to read line #',line_count
         END IF
         EXIT big_read_loop
      END IF

      DO ind = 1 , MAX_LENGTH
         IF ( ( ( ICHAR(input_string(ind:ind)) .LT. FIRST_VALID ) .OR. &
                ( ICHAR(input_string(ind:ind)) .GT. LAST_VALID  ) ) .AND. &
                ( ICHAR(input_string(ind:ind)) .NE. TAB         ) ) THEN
            problem_line_count = problem_line_count + 1
            IF ( verbose .EQ. 0 ) THEN
            ELSE 
               PRINT *,TRIM(filename)
               PRINT *,'Found something on line #',line_count
               PRINT *,TRIM(input_string)
               PRINT *,'Character # ',ind,' is a ',input_string(ind:ind),', which is character code ',ICHAR(input_string(ind:ind))
            END IF
         END IF
      END DO

      line_count = line_count + 1 

   END DO big_read_loop

   !  What actually happened for this file, let us find out.

   IF      ( problem_line_count .EQ. 0 ) THEN
      IF ( verbose .EQ. 2 ) THEN
         PRINT *,'OK, File uses only ASCII character codes from ',FIRST_VALID,' through',LAST_VALID
      END IF
   ELSE IF ( verbose .EQ. 1 ) THEN
      PRINT *,'Troubles, with ',problem_line_count,' lines.'
      PRINT *,'File uses character codes outside the standard ASCII range of ',FIRST_VALID,' to ',LAST_VALID
   ELSE IF ( verbose .EQ. 0 ) THEN
      PRINT *,TRIM(filename) ! , problem_line_count
   END IF

END PROGRAM non_ascii_finder
