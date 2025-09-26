!  Program to modify levels in the intermediate format.  Two input
!  files come in on the command line: input file and output file.
!  An additional namelist file is used to select which pressure levels
!  are to be kept.

!  NRCM helper, WPS toy code

PROGRAM mod_levs_prog

   USE module_debug
   USE read_met_module
   USE write_met_module
   USE misc_definitions_module

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   CHARACTER ( LEN =132 )            :: flnm, flnm2

   INTEGER :: istatus, iop
   integer :: idum, ilev

   TYPE (met_data)                   :: fg_data

   !  The namelist has a pressure array that we want.

   LOGICAL                           :: keep_this_one
   INTEGER                           :: l , max_pres_keep
   INTEGER , PARAMETER               :: num_pres_lev = 1000
   REAL, DIMENSION(num_pres_lev)     :: press_pa = -1.
   NAMELIST /mod_levs/ press_pa

   INTEGER , EXTERNAL :: lenner

   !  Open up the file with the pressure levels to process.

   OPEN ( UNIT   =  10            , &
          FILE   = 'namelist.wps' , &
          STATUS = 'OLD'          , &
          FORM   = 'FORMATTED'    , & 
          IOSTAT =  iop              )

   IF (iop .NE. 0) then
      print *, 'Error: Couldn''t open namelist.wps file.'
      STOP 
   END IF

   !  Input the pressure levels requested.

   READ ( 10 , mod_levs, err=1000, end=1001 ) 

   CLOSE ( 10 ) 

   !  How many pressure levels were asked for?

   DO l = 1 , num_pres_lev
      IF ( press_pa(l) .EQ. -1. ) THEN
         max_pres_keep = l-1
         EXIT
      END IF
   END DO

   !  Get the two files: input and output.

   CALL getarg ( 1 , flnm  )

   IF ( flnm(1:1) .EQ. ' ' ) THEN
      print *,'USAGE: mod_levs.exe FILE:2006-07-31_00 new_FILE:2006-07-31_00'
      STOP
   END IF

   CALL getarg ( 2 , flnm2 )

   l = lenner(flnm)
   IF ( flnm2(1:1) .EQ. ' ' ) THEN
      flnm2(5:l+4) = flnm(1:l)
      flnm2(1:4) = 'new_'
   END IF

   CALL set_debug_level(WARN)

   CALL read_met_init(TRIM(flnm), .true., '0000-00-00_00', istatus)

   IF ( istatus == 0 ) THEN

      CALL write_met_init(TRIM(flnm2), .true., '0000-00-00_00', istatus)

      IF ( istatus == 0 ) THEN

         CALL read_next_met_field(fg_data, istatus)

         DO WHILE (istatus == 0)
   
   
            keep_this_one = .FALSE.
            DO l = 1 , max_pres_keep
               IF ( fg_data%xlvl .EQ. press_pa(l) ) THEN
                  keep_this_one = .TRUE.
                  EXIT
               END IF
            END DO 

            IF (keep_this_one) THEN
               CALL write_next_met_field(fg_data, istatus)
            ELSE
               CALL mprintf(.true.,STDOUT,'Deleting level %f Pa',f1=fg_data%xlvl)
            END IF

            CALL mprintf(.true.,STDOUT,'Processed %s at level %f for time %s', &
                         s1=fg_data%field, f1=fg_data%xlvl, s2=fg_data%hdate)
            IF (ASSOCIATED(fg_data%slab)) DEALLOCATE(fg_data%slab)
   
            CALL read_next_met_field(fg_data, istatus)
         END DO

         CALL write_met_close()

      ELSE

         print *, 'File = ',TRIM(flnm2)
         print *, 'Problem with output file, I can''t open it'
         STOP

      END IF

      CALL read_met_close()
 
   ELSE

      print *, 'File = ',TRIM(flnm)
      print *, 'Problem with input file, I can''t open it'
      STOP

   END IF

   print *,'SUCCESSFUL COMPLETION OF PROGRAM MOD_LEVS'
   STOP

1000 print *,'Error while reading &mod_levs namelist.'
   STOP
1001 print *,'Error: Could not find &mod_levs namelist. Perhaps this namelist is not present in namelist.wps?'
   STOP

END PROGRAM mod_levs_prog
   
INTEGER FUNCTION lenner ( string ) 
   CHARACTER ( LEN = 132 ) ::  string
   INTEGER :: l
   DO l = 132 , 1 , -1
      IF ( ( ( string(l:l) .GE. 'A' ) .AND. ( string(l:l) .LE. 'Z' ) ) .OR. &
           ( ( string(l:l) .GE. 'a' ) .AND. ( string(l:l) .LE. 'z' ) ) .OR. &
           ( ( string(l:l) .GE. '0' ) .AND. ( string(l:l) .LE. '9' ) ) ) THEN
         lenner = l
         EXIT
      END IF
   END DO
END FUNCTION lenner
