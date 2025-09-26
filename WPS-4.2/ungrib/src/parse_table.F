!*****************************************************************************!
! Subroutine PARSE_TABLE                                                      !
!                                                                             !
! Purpose:                                                                    !
!    Read the Vtable, and fill arrays in the TABLE module with the Vtable     !
!    information.  Broadly, the Vtable file is how the user tells the         !
!    program what fields to extract from the archive files.                   !
!                                                                             !
! Argument list:                                                              !
!    Input: DEBUG_LEVEL:  0 = no prints, bigger numbers = more prints         !
!              
! Externals:                                                                  !
!    Module TABLE                                                             !
!    Subroutine ABORT                                                         !
!                                                                             !
! Side Effects:                                                               !
!                                                                             !
!    - File "Vtable" is opened, read, and closed as Fortran unit 10.          !
!                                                                             !
!    - Various prints, especially if DEBUG_PRINT = .TRUE.                     !
!                                                                             !
!    - Abort for some miscellaneous error conditions.                         !
!                                                                             !
!    - Variables in module TABLE are filled., specifically, variables         !
!        MAXVAR                                                               !
!        MAXOUT                                                               !
!                                                                             !
!    - Arrays in module TABLE are filled., specifically, arrays               !
!        NAMVAR                                                               !
!        NAMEOUT                                                              !
!        UNITOUT                                                              !
!        DESCOUT                                                              !
!        GCODE                                                                !
!        LCODE                                                                !
!        LEVEL1                                                               !
!        LEVEL2                                                               !
!        IPRTY                                                                !
!        DUNITS                                                               !
!        DDESC                                                                !
!                                                                             !
! Author: Kevin W. Manning                                                    !
!         NCAR/MMM                                                            !
!         Summer 1998, and continuing                                         !
!         SDG                                                                 !
!                                                                             !
!*****************************************************************************!

subroutine parse_table(debug_level,vtable_columns)
  use Table
  use module_debug
  use stringutil
  implicit none
  integer :: debug_level

  character(LEN=255) :: string = ' '
  integer :: ierr
  integer :: istart, ibar, i, j, ipcount
  integer :: jstart, jbar, jmax, tot_bars 
  integer :: vtable_columns
  integer :: nstart, maxtmp
  logical :: lexist
  character(len=9) :: tmp9

! added for IBM
  blankcode = -99
  splatcode = -88
! end added for IBM

! Open the file called "Vtable"

  open(10, file='Vtable', status='old', form='formatted', iostat=ierr)

! Check to see that the OPEN worked without error.

  if (ierr.ne.0) then
     inquire(file='Vtable', exist=LEXIST)
     call mprintf(.true.,STDOUT," ***** ERROR in Subroutine PARSE_TABLE:")
     call mprintf(.true.,LOGFILE," ***** ERROR in Subroutine PARSE_TABLE:")
     if (.not.lexist) then
       call mprintf(.true.,STDOUT,"Problem opening file Vtable.")
       call mprintf(.true.,STDOUT,"File ''Vtable'' does not exist.")
       call mprintf(.true.,LOGFILE,"Problem opening file Vtable.")
       call mprintf(.true.,LOGFILE,"File ''Vtable'' does not exist.")
     else
       call mprintf(.true.,STDOUT,"Problem opening file Vtable.")
       call mprintf(.true.,STDOUT,"File Vtable exists, but Fortran OPEN statement")
       call mprintf(.true.,STDOUT,"failed with error %i",i1=ierr)
       call mprintf(.true.,LOGFILE,"Problem opening file Vtable.")
       call mprintf(.true.,LOGFILE,"File Vtable exists, but Fortran OPEN statement")
       call mprintf(.true.,LOGFILE,"failed with error %i",i1=ierr)
     endif
     call mprintf(.true.,ERROR," ***** Stopping in Subroutine PARSE_TABLE")
  endif

! First, read past the headers, i.e., skip lines until we hit the first
! line beginning with '-'
  do while (string(1:1).ne.'-')
     read(10,'(A255)', iostat=ierr) string
     call mprintf ((ierr /= 0),ERROR,"Read error 1 in PARSE_TABLE.")
  enddo
  string = ' '

! Now interpret everything from here to the next '-' line:
!
  RDLOOP : do while (string(1:1).ne.'-')
     read(10,'(A255)', iostat=ierr) string
     call mprintf ((ierr /= 0),ERROR,"Read error 2 in PARSE_TABLE.")
     if (string(1:1).eq.'#') cycle RDLOOP
     if (len_trim(string) == 0) cycle RDLOOP
     if (string(1:1).eq.'-') then
        ! Skip over internal header lines
        BLOOP : do
           read(10,'(A255)', iostat=ierr) string
           if (ierr /= 0) exit RDLOOP
           if (len_trim(string) == 0) then
              cycle BLOOP
           else if (string(1:1) == '#') then
              cycle BLOOP
           else
              exit BLOOP
           endif
        enddo BLOOP
        do while (string(1:1).ne.'-')
           read(10,'(A255)', iostat=ierr) string
        call mprintf ((ierr /= 0),ERROR,"Read error 3 in PARSE_TABLE.")
        enddo
        string(1:1) = ' '
        
     elseif (string(1:1).ne.'-') then
        ! This is a line of values to interpret and parse.
        maxvar = maxvar + 1 ! increment the variable count

        ! --- Determine Grib1 or Grib2
        ! If there are seven fields this is a Grib1 Vtable, 
        ! if there are eleven fields this is a Grib2 Vtable.
        jstart = 1
        jmax=jstart
        tot_bars=0

        do j = 1, vtable_columns 
        ! The fields are delimited by '|'
           jbar = index(string(jstart:255),'|') + jstart - 2
           jstart = jbar + 2
           if (jstart.gt.jmax) then
             tot_bars=tot_bars+1
             jmax=jstart
           else
             cycle
           endif
        enddo

        call mprintf((tot_bars.eq.7.and.vtable_columns.ge.11),ERROR, &
          'Vtable does not contain Grib2 decoding information.'// &
          ' 11 or 12 columns of information is expected.'// &
          ' *** stopping parse_table ***')


        istart = 1
        ! There are seven fields (Grib1) or eleven fields (Grib2) to each line.
  PLOOP : do i = 1, vtable_columns 
        ! The fields are delimited by '|'

           ibar = index(string(istart:255),'|') + istart - 2

           if (i.eq.1) then
           ! The first field is the Grib1 param code number:

              if (string(istart:ibar) == ' ') then
                 gcode(maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') /= 0) then
	        call mprintf(.true.,ERROR,'Parse_table: Please give a '// &
	         'Grib1 parm code rather than $ in the first column of Vtable '// &
                 '*** stopping in parse_table ***')
              else
                 read(string(istart:ibar), * ) gcode(maxvar)
              endif

           elseif (i.eq.2) then
           ! The second field is the Grib1 level type:

              if (string(istart:ibar) == ' ') then
                 if (lcode(maxvar) /= blankcode) then
		  call mprintf(.true.,ERROR,'Parse_table: '// &
		   'Please supply a Grib1 level type in the Vtable: %s '// &
		   '*** stopping in parse_table ***',s1=string)
                 else
                    lcode(maxvar) = blankcode
                 endif
              elseif (scan(string(istart:ibar),'*') /= 0) then
	        call mprintf(.true.,ERROR,'Parse_table: '// &
	         "Used a * in Grib1 level type...don't do this! "// &
                 '*** stopping in parse_table ***')
              else
                 read(string(istart:ibar), *) lcode(maxvar)
              endif

           elseif (i.eq.3) then
           ! The third field is the Level 1 value, which may be '*':

              if (string(istart:ibar) == ' ') then
                 level1(maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') == 0) then
                 read(string(istart:ibar), *) level1(maxvar)
              else
                 level1(maxvar) = splatcode
              endif

           elseif (i.eq.4) then
           ! The fourth field is the Level 2 value, which may be blank:

              if (string(istart:ibar) == ' ') then
                 if ( (lcode(maxvar) == 112) .or.&
                      (lcode(maxvar) == 116) ) then
		  call mprintf(.true.,ERROR,'Parse_table: '// &
		   'Level Code  expects two Level values. '// &
		   '*** stopping in parse_table ***')
                 else
                    level2(maxvar) = blankcode
                 endif
              elseif (scan(string(istart:ibar),'*') /= 0) then
		 call mprintf(.true.,ERROR,'Parse_table: '// &
		  'Please give a Level 2 value (or blank), rather * in Vtable column 4 '// &
		  '*** stopping in parse_table ***')
              else
                 read(string(istart:ibar), *) level2(maxvar)
              endif

           elseif (i.eq.5) then
           ! The fifth field is the param name:

              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 namvar(maxvar) = string(istart+nstart:ibar)
              else
		 call mprintf(.true.,ERROR,'Parse_table: '// &
		 'A field name is missing in the Vtable. '// &
		 '*** stopping in parse_table ***')
              endif

           elseif (i.eq.6) then
           ! The sixth field is the Units string, which may be blank:

              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 Dunits(maxvar) = string(istart+nstart:ibar)
              else
                 Dunits(maxvar) = ' '
              endif

           elseif (i.eq.7) then
           ! The seventh field is the description string, which may be blank:

              if (string(istart:ibar).ne.' ') then
                 nstart = 0
                 do while (string(istart+nstart:istart+nstart).eq.' ')
                    nstart = nstart + 1
                 enddo
                 Ddesc(maxvar) = string(istart+nstart:ibar)

                 ! If the description string is not blank, this is a
                 ! field we want to output.  In that case, copy the
                 ! param name to the MAXOUT array:
                 maxout = maxout + 1
                 nameout(maxout) = namvar(maxvar)
                 unitout(maxout) = Dunits(maxvar)
                 descout(maxout) = Ddesc(maxvar)

              else
                 Ddesc(maxvar) = ' '
              endif

           elseif (i.eq.8) then
           ! The eighth field is the Grib2 Product Discipline (see the 
           ! Product Definition Template, Table 4.2).

              !cycle RDLOOP
              !read(string(istart:ibar), * ,eor=995) g2code(1,maxvar)

              if (string(istart:ibar) == ' ') then
                 g2code(1,maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') /= 0) then
		 call mprintf(.true.,STDOUT," ERROR reading Grib2 Discipline")
		 call mprintf(.true.,STDOUT,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,STDOUT," %s",s1=string)
		 call mprintf(.true.,LOGFILE," ERROR reading Grib2 Discipline")
		 call mprintf(.true.,LOGFILE,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,LOGFILE," %s",s1=string)
		 call mprintf(.true.,ERROR,"Stopping in PARSE_TABLE")
              else
                 read(string(istart:ibar), *) g2code(1,maxvar)
              endif

           elseif (i.eq.9) then
           ! The ninth field is the Grib2 Parameter Category per Discipline.

              if (string(istart:ibar) == ' ') then
                 g2code(2,maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') /= 0) then
		 call mprintf(.true.,STDOUT," ERROR reading Grib2 Category")
		 call mprintf(.true.,STDOUT,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,STDOUT," %s",s1=string)
		 call mprintf(.true.,LOGFILE," ERROR reading Grib2 Category")
		 call mprintf(.true.,LOGFILE,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,LOGFILE," %s",s1=string)
		 call mprintf(.true.,ERROR,"Stopping in PARSE_TABLE")
              else
                 read(string(istart:ibar), * ) g2code(2,maxvar)
              endif

           elseif (i.eq.10) then
           ! The tenth field is the Grib2 Parameter Number per Category.

              if (string(istart:ibar) == ' ') then
                 g2code(3,maxvar) = blankcode
              elseif (scan(string(istart:ibar),'*') /= 0) then
		 call mprintf(.true.,STDOUT, &
		  " ERROR reading Grib2 Parameter Number ")
		 call mprintf(.true.,STDOUT,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,STDOUT," %s",s1=string)
		 call mprintf(.true.,LOGFILE, &
		  " ERROR reading Grib2 Parameter Number ")
		 call mprintf(.true.,LOGFILE,  &
		    "This Grib2 Vtable line is incorrectly specified:")
	         call mprintf(.true.,LOGFILE," %s",s1=string)
		 call mprintf(.true.,ERROR,"Stopping in PARSE_TABLE")
              else
                 read(string(istart:ibar), * ) g2code(3,maxvar)
              endif

           elseif (i.eq.11) then
           ! The eleventh field is the Grib2 Level Type (see the Product
           ! Definition Template, Table 4.5).

              if (string(istart:ibar) == ' ') then
                 if (g2code(4,maxvar) /= blankcode) then
		   call mprintf(.true.,STDOUT," ERROR reading Grib2 Level Type ")
		   call mprintf(.true.,STDOUT,  &
		      "This Grib2 Vtable line is incorrectly specified:")
		   call mprintf(.true.,STDOUT," %s",s1=string)
		   call mprintf(.true.,LOGFILE," ERROR reading Grib2 Level Type ")
		   call mprintf(.true.,LOGFILE,  &
		      "This Grib2 Vtable line is incorrectly specified:")
		   call mprintf(.true.,LOGFILE," %s",s1=string)
		   call mprintf(.true.,ERROR,"Stopping in PARSE_TABLE")
                 else
                    g2code(4,maxvar) = blankcode
                 endif
              elseif (scan(string(istart:ibar),'*') /= 0) then
	         call mprintf(.true.,STDOUT,"ERROR in Subroutine Parse_table: ")
	         call mprintf(.true.,STDOUT, &
		  "Used a * in Grib2 level type...don't do this! ")
	         call mprintf(.true.,STDOUT," %s ",s1=string)
	         call mprintf(.true.,LOGFILE,"ERROR in Subroutine Parse_table: ")
	         call mprintf(.true.,LOGFILE, &
		  "Used a * in Grib2 level type...don't do this! ")
	         call mprintf(.true.,LOGFILE," %s ",s1=string)
		 call mprintf(.true.,ERROR," ***** Abort in Subroutine PARSE_TABLE")
              else
                 read(string(istart:ibar), *) g2code(4,maxvar)
              endif

           elseif (i.eq.12) then
           ! The twelfth field is the Grib2 Product Definition Template number
           ! Defaults to template 4.0, an instantaneous horizontal field.
           ! The only other supported value is 8 - an accumulated or averaged field.

            if (istart .lt. ibar) then
              if (string(istart:ibar) == ' ') then
                 g2code(5,maxvar) = 0
              elseif (scan(string(istart:ibar),'*') /= 0) then
                 call mprintf(.true.,STDOUT, &
                  " ERROR reading Grib2 Parameter Number ")
                 call mprintf(.true.,STDOUT,  &
                    "This Grib2 Vtable line is incorrectly specified:")
                 call mprintf(.true.,STDOUT," %s",s1=string)
                 call mprintf(.true.,LOGFILE, &
                  " ERROR reading Grib2 Parameter Number ")
                 call mprintf(.true.,LOGFILE,  &
                    "This Grib2 Vtable line is incorrectly specified:")
                 call mprintf(.true.,LOGFILE," %s",s1=string)
                 call mprintf(.true.,ERROR,"Stopping in PARSE_TABLE")
              else
                 read(string(istart:ibar), * ) g2code(5,maxvar)
              endif
             else     ! occurs when 11 columns are in the Vtable rather than 12.
               g2code(5,maxvar) = 0
             endif

           endif

           istart = ibar + 2

        enddo PLOOP ! 1,vtable_columns
     endif
!995  continue
  enddo RDLOOP
! Now we have finished reading the file.  
  close(10)

! Now remove duplicates from the NAMEOUT array.  Duplicates may arise
! when we have the same name referred to by different level or parameter
! codes in some dataset.

  maxtmp = maxout
  do i = 1, maxtmp-1
     do j = i+1, maxtmp
        if ((nameout(i).eq.nameout(j)).and.(nameout(j).ne.' ')) then
	   call mprintf(.true.,DEBUG,   &
	     "Duplicate name.  Removing %s from output list.",s1=nameout(j))
           nameout(j:maxlines-1) = nameout(j+1:maxlines)
           unitout(j:maxlines-1) = unitout(j+1:maxlines)
           descout(j:maxlines-1) = descout(j+1:maxlines)
           maxout = maxout - 1
        endif
     enddo
  enddo

! Compute a priority level based on position in the table:
! This assumes Grib.

! Priorities are used only for surface fields.  If it is not a
! surface fields, the priority is assigned a value of 100.

! For surface fields, priorities are assigned values of 100, 101,
! 102, etc. in the order the field names appear in the Vtable.

  ipcount = 99
  do i = 1, maxvar
     if ((lcode(i).eq.105).or.(lcode(i).eq.118)) then
        ipcount = ipcount + 1
        iprty(i) = ipcount
     elseif (lcode(i).eq.116.and.level1(i).le.50.and.level2(i).eq.0) then
        ipcount = ipcount + 1
        iprty(i) = ipcount
     else
        iprty(i) = 100
     endif
  enddo

  if (debug_level .gt. 0) then
     write(*,'(//"Read from file ''Vtable'' by subroutine PARSE_TABLE:")')
     call mprintf(.true.,DEBUG,   &
       "Read from file Vtable by subroutine PARSE_TABLE:")
     do i = 1, maxvar
        if (vtable_columns.ge.11) then
           write(*,'(4I6, 3x,A10, 5I6)')&
             gcode(i), lcode(i), level1(i), level2(i), namvar(i), &
             g2code(1,i), g2code(2,i), g2code(3,i), g2code(4,i), g2code(5,i)
	   write(tmp9,'(i9)') gcode(i)
           call mprintf(.true.,DEBUG,'%s ',s1=tmp9(4:9),newline=.false.)
	   write(tmp9,'(i9)') lcode(i)
           call mprintf(.true.,DEBUG,'%s ',s1=tmp9(4:9),newline=.false.)
	   write(tmp9,'(i9)') level1(i)
           call mprintf(.true.,DEBUG,'%s ',s1=tmp9(4:9),newline=.false.)
	   write(tmp9,'(i9)') level2(i)
           call mprintf(.true.,DEBUG,'%s ',s1=tmp9(4:9),newline=.false.)
	   write(tmp9,'(a9)') namvar(i)(1:9)
	   call right_justify(tmp9,9)
	   call mprintf(.true.,DEBUG,tmp9,newline=.false.)
           do j = 1, 5
	     write(tmp9,'(i9)') g2code(j,i)
             call mprintf(.true.,DEBUG,'%s ',s1=tmp9(4:9),newline=.false.)
	   enddo
	   call mprintf(.true.,DEBUG,' ',newline=.true.)
        else 
           write(*,'(4I6, 3x,A10)')&
             gcode(i), lcode(i), level1(i), level2(i), namvar(i)
        endif
     enddo
     write(*,'(//)')
  endif
        
end subroutine parse_table
