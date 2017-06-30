! To remove the DOS carriage returns from Windows machines:
! perl -pi -e 's/\r\n|\n|\r/\n/g'   file-to-convert  # Convert to UNIX
! This program is not required for that.
! For completeness, from stackoverflow:
! perl -pi -e 's/\r\n|\n|\r/\r\n/g' file-to-convert  # Convert to DOS
!
! The purpose of this program is to scan through all of the 
! lines of a Fortran file to detect if there are any character
! codes (excluding the ubiquitously used tab character) outside
! the range of 32 to 127 (the standard printable ASCII character 
! set).

! The "Fortran 2003 Handbook" (Adams et al. 2009), in section 3.1.1,
! lists the standard Fortran character set, which is consistent 
! with the restriction of the ASCII character codes from 32 
! through 127 (inclusive).

! So far during source code testing, all of the special characters 
! outside of the ASCII printable character range have been used 
! within comments.  The WRF model strips comments before passing 
! the code through to the compiler.  These characters can 
! occasionally get introduced with physics routines when variable 
! names or units use special characters (superscripts, squared 
! terms, hats, etc), or when short- or long-dashes are used in 
! citing references.  Again, these sources are typical of 
! information that could be provided in a comment without 
! impacting the code to be built.

! How to build the finder program: 
! gfortran -ffree-form non_ascii_finder.F

! usage:
! ./a.out -v|-V file.F

! Typically, the program is run twice.  

! 1) The program is run the first time with the verbose flag 
! (mandatory) set to "-v".  Output only occurs when the input file 
! contains at least one offending character.  In this case, the 
! output is the file name containing the offending character.

! 2) Once the list of problematic files is assembled, the program 
! is re-run with verbose flag set to "-V".  For each file processed, 
! the program outputs the line of source code that contains the 
! offending character(s), and also output the line number (to help
! the user find the string).

! Here is an example of the two step usage before all of the offending
! characters were removed.  From the top WRF directory:

! <type on the command line>
! find . -name \*.F -exec tools/a.out -v {} \; 

! <output to screen>
! ./chem/module_cam_mam_newnuc.F
! ./chem/module_gocart_dmsemis.F
! ./chem/module_gocart_seasalt.F
! ./chem/module_mozcart_wetscav.F
! ./chem/module_sea_salt_emis.F
! ./dyn_em/module_sfs_driver.F
! ./dyn_em/module_sfs_nba.F
! ./frame/module_cpl.F
! ./hydro/Routing/module_gw_gw2d.F
! ./phys/module_bl_mfshconvpbl.F
! ./phys/module_gocart_seasalt.F
! ./phys/module_ltng_cpmpr92z.F
! ./phys/module_ltng_crmpr92.F
! ./phys/module_ltng_iccg.F
! ./phys/module_mp_nssl_2mom.F
! ./phys/module_mp_wdm6.F
! ./phys/module_sf_bem.F
! ./phys/module_sf_bep.F
! ./phys/module_sf_bep_bem.F
! ./tools/non_ascii_finder.F
! ./var/convertor/wave2grid_kma/pvchkdv.F

! The manufactured list of files (shown above) can be processed
! individually, now with the "-V" flag:

! <type on the command line>
! tools/a.out -V ./tools/non_ascii_finder.F

! <output to screen>
! ./non_ascii_finder.F
! Found something on line #          25
! !   --> this line has a problem with the superscript numeral 2: [W/m^2]
! Character #           69  is a ?, which is character code          194
! ./non_ascii_finder.F
! Found something on line #          25
! !   --> this line has a problem with the superscript numeral 2: [W/m^2]
! Character #           70  is a ?, which is character code          178
! Troubles, with            2  lines.
! File uses character codes outside the standard ASCII range of           32  to          127

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
   INTEGER :: verbose  ! from input -v  0=typical for "find" output, just a filename when there are troubles
                       ! from input -V  1=typical for single file searching, specifically: what lines need fixing
                       ! from input -VV 2=typical for debugging this program

   INTEGER :: status
   INTEGER :: ind
   INTEGER :: line_count, problem_line_count

   !  Get the command line info.

   num_args = COMMAND_ARGUMENT_COUNT()

   !  Do we have enough arguments, we want two.

   IF ( num_args .NE. 2 ) THEN
      PRINT *,'Usage:'
      PRINT *,'./a.out <mandatory verbose level> <mandatory filename>'
      PRINT *,'where <verbose level> is either -v or -V'
      PRINT *,'      -v: outputs the filename only iff offending characters were found'
      PRINT *,'      -V: outputs the line(s) containing the offending characters'
      PRINT *,'where <filename> is a Fortran source file'
      PRINT *,' '
      PRINT *,'For more information, please read the comments at the top of '
      PRINT *,"this program's source code: tools/non_ascii_finder.F"
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
