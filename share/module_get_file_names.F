MODULE module_get_file_names

!  This module is used by the ndown program.  We can have multiple output
!  files generated from the wrf program.  To remove the  what-are-the-
!  files-to-input-to-ndown task from the user, we use a couple of UNIX
!  commands.  These are activated from either the "system" command or 
!  the "exec" command.  Neither is part of the Fortran standard.

   INTEGER :: number_of_eligible_files
   INTEGER , PARAMETER :: max_number_of_eligible_files = 100
   CHARACTER(LEN=132) , DIMENSION(max_number_of_eligible_files) :: eligible_file_name

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE unix_ls ( root , id )

      IMPLICIT NONE
     
      CHARACTER (LEN=*) , INTENT(IN) :: root
      INTEGER , INTENT(IN) :: id

      CHARACTER (LEN=132) :: command
      INTEGER :: ierr , loop , loslen 
      LOGICAL :: unix_access_ok

      !  This is to make sure that we successfully use one of the available methods
      !  for getting at a UNIX command.  This is an initialized flag.

      unix_access_ok = .FALSE.

      !  Build a UNIX command, and "ls", of all of the files mnatching the "root*" prefix.

      loslen = LEN ( command )
      CALL all_spaces ( command , loslen ) 
      WRITE ( command , FMT='("ls -1 ",A,"*d",I2.2,"* > .foo")' ) TRIM ( root ) , id
      
      !  We stuck all of the matching files in the ".foo" file.  Now we place the 
      !  number of the those file (i.e. how many there are) in ".foo1".  Also, if we
      !  do get inside one of these CPP ifdefs, then we set our access flag to true.

#ifdef NONSTANDARD_SYSTEM 
      CALL SYSTEM ( TRIM ( command ) ) 
      CALL SYSTEM ( '( cat .foo | wc -l > .foo1 )' )
      unix_access_ok = .TRUE.
#endif
#ifdef NONSTANDARD_EXEC
      CALL SYSTEM ( TRIM ( command ) ) 
      CALL SYSTEM ( '( cat .foo | wc -l > .foo1 )' )
      unix_access_ok = .TRUE.
#endif

      !  Test to be sure that we did indeed hit one of the ifdefs.

      IF ( .NOT. unix_access_ok ) THEN
         PRINT *,'Oops, how can I access UNIX commands from Fortran?'
         CALL wrf_error_fatal ( 'system_or_exec_only' )
      END IF

      !  Read the number of files.

      OPEN (FILE   = '.foo1'       , &
            UNIT   = 112           , &
            STATUS = 'OLD'         , &
            ACCESS = 'SEQUENTIAL'  , &
            FORM   = 'FORMATTED'     )

      READ ( 112 , * ) number_of_eligible_files
      CLOSE ( 112 )

      !  If there are zero files, we are toast.

      IF ( number_of_eligible_files .LE. 0 ) THEN
         PRINT *,'Oops, we need at least ONE input file (wrfout*) for the ndown program to read.'
         CALL wrf_error_fatal ( 'need_wrfout_input_data' )
      END IF

      !  Open the file that has the list of filenames.

      OPEN (FILE   = '.foo'        , &
            UNIT   = 111           , &
            STATUS = 'OLD'         , &
            ACCESS = 'SEQUENTIAL'  , &
            FORM   = 'FORMATTED'     )

      !  Read all of the file names and store them.

      DO loop = 1 , number_of_eligible_files
         READ ( 111 , FMT='(A)' ) eligible_file_name(loop)
print *,TRIM(eligible_file_name(loop))
      END DO
      CLOSE ( 112 )

      !   We clean up our own messes.

#ifdef NONSTANDARD_SYSTEM 
      CALL SYSTEM ( '/bin/rm -f .foo'  )
      CALL SYSTEM ( '/bin/rm -f .foo1' )
#endif
#ifdef NONSTANDARD_EXEC
      CALL SYSTEM ( '/bin/rm -f .foo'  )
      CALL SYSTEM ( '/bin/rm -f .foo1' )
#endif

   END SUBROUTINE unix_ls

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE all_spaces ( command , length_of_char ) 

      IMPLICIT NONE

      INTEGER :: length_of_char
      CHARACTER (LEN=length_of_char) :: command
      INTEGER :: loop

      DO loop = 1 , length_of_char
         command(loop:loop) = ' '
      END DO

   END SUBROUTINE all_spaces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE init_module_get_file_names
   
      IMPLICIT NONE
      eligible_file_name = '                                                  ' // &
                           '                                                  ' // &
                           '                                '

   END SUBROUTINE init_module_get_file_names

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE module_get_file_names

!program foo
!USE module_get_file_names
!call init_module_get_file_names
!call unix_ls ( 'wrf_real' , 1 )
!end program foo
