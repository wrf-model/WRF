      module gridtemplates
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:    gridtemplates 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-09
!
! ABSTRACT: This Fortran Module contains info on all the available 
!   GRIB2 Grid Definition Templates used in Section 3 (GDS).
!   Each Template has three parts: The number of entries in the template
!   (mapgridlen);  A map of the template (mapgrid), which contains the
!   number of octets in which to pack each of the template values; and
!   a logical value (needext) that indicates whether the Template needs 
!   to be extended.  In some cases the number of entries in a template 
!   can vary depending upon values specified in the "static" part of 
!   the template.  ( See Template 3.120 as an example )
!
!   This module also contains two subroutines.  Subroutine getgridtemplate
!   returns the octet map for a specified Template number, and
!   subroutine extgridtemplate will calculate the extended octet map
!   of an appropriate template given values for the "static" part of the 
!   template.  See docblocks below for the arguments and usage of these 
!   routines.
!
!   NOTE:  Array mapgrid contains the number of octets in which the 
!   corresponding template values will be stored.  A negative value in
!   mapgrid is used to indicate that the corresponding template entry can
!   contain negative values.  This information is used later when packing
!   (or unpacking) the template data values.  Negative data values in GRIB
!   are stored with the left most bit set to one, and a negative number
!   of octets value in mapgrid() indicates that this possibility should
!   be considered.  The number of octets used to store the data value
!   in this case would be the absolute value of the negative value in 
!   mapgrid().
!  
!
! PROGRAM HISTORY LOG:
! 2000-05-09  Gilbert
! 2003-09-02  Gilbert   -  Added GDT 3.31 - Albers Equal Area
!
! USAGE:    use gridtemplates
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,parameter :: MAXLEN=200,MAXTEMP=23

      type gridtemplate
          integer :: template_num
          integer :: mapgridlen
          integer,dimension(MAXLEN) :: mapgrid
          logical :: needext
      end type gridtemplate

      type(gridtemplate),dimension(MAXTEMP) :: templates

      data templates(1)%template_num /0/     !  Lat/Lon 
      data templates(1)%mapgridlen /19/
      data templates(1)%needext /.false./
      data (templates(1)%mapgrid(j),j=1,19) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1/

      data templates(2)%template_num /1/     !  Rotated Lat/Lon 
      data templates(2)%mapgridlen /22/
      data templates(2)%needext /.false./
      data (templates(2)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4/

      data templates(3)%template_num /2/     !  Stretched Lat/Lon 
      data templates(3)%mapgridlen /22/
      data templates(3)%needext /.false./
      data (templates(3)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,-4/

      data templates(4)%template_num /3/     !  Stretched & Rotated Lat/Lon 
      data templates(4)%mapgridlen /25/
      data templates(4)%needext /.false./
      data (templates(4)%mapgrid(j),j=1,25) 
     &       /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4,-4,4,-4/

      data templates(5)%template_num /10/     !  Mercator
      data templates(5)%mapgridlen /19/
      data templates(5)%needext /.false./
      data (templates(5)%mapgrid(j),j=1,19)
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,-4,-4,4,1,4,4,4/

      data templates(6)%template_num /20/     !  Polar Stereographic
      data templates(6)%mapgridlen /18/
      data templates(6)%needext /.false./
      data (templates(6)%mapgrid(j),j=1,18) 
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1/

      data templates(7)%template_num /30/     !  Lambert Conformal
      data templates(7)%mapgridlen /22/
      data templates(7)%needext /.false./
      data (templates(7)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1,-4,-4,-4,4/

      data templates(8)%template_num /40/     !  Gaussian Lat/Lon
      data templates(8)%mapgridlen /19/
      data templates(8)%needext /.false./
      data (templates(8)%mapgrid(j),j=1,19) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1/

      data templates(9)%template_num /41/     !  Rotated Gaussian Lat/Lon
      data templates(9)%mapgridlen /22/
      data templates(9)%needext /.false./
      data (templates(9)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4/

      data templates(10)%template_num /42/     !  Stretched Gaussian Lat/Lon
      data templates(10)%mapgridlen /22/
      data templates(10)%needext /.false./
      data (templates(10)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,-4/

      data templates(11)%template_num /43/     !  Strtchd and Rot'd Gaus Lat/Lon
      data templates(11)%mapgridlen /25/
      data templates(11)%needext /.false./
      data (templates(11)%mapgrid(j),j=1,25) 
     &          /1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4,-4,4,-4/

      data templates(12)%template_num /50/    !  Spherical Harmonic Coefficients
      data templates(12)%mapgridlen /5/
      data templates(12)%needext /.false./
      data (templates(12)%mapgrid(j),j=1,5) /4,4,4,1,1/

      data templates(13)%template_num /51/   !  Rotated Spherical Harmonic Coeff
      data templates(13)%mapgridlen /8/
      data templates(13)%needext /.false./
      data (templates(13)%mapgrid(j),j=1,8) /4,4,4,1,1,-4,4,4/

      data templates(14)%template_num /52/   !  Stretch Spherical Harmonic Coeff
      data templates(14)%mapgridlen /8/
      data templates(14)%needext /.false./
      data (templates(14)%mapgrid(j),j=1,8) /4,4,4,1,1,-4,4,-4/

      data templates(15)%template_num /53/   !  Strch and Rot Spher Harm Coeffs
      data templates(15)%mapgridlen /11/
      data templates(15)%needext /.false./
      data (templates(15)%mapgrid(j),j=1,11) /4,4,4,1,1,-4,4,4,-4,4,-4/

      data templates(16)%template_num /90/     !  Space view Perspective
      data templates(16)%mapgridlen /21/
      data templates(16)%needext /.false./
      data (templates(16)%mapgrid(j),j=1,21) 
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,4,4,4,4,1,4,4,4,4/

      data templates(17)%template_num /100/    !  Triangular grid (icosahedron)
      data templates(17)%mapgridlen /11/
      data templates(17)%needext /.false./
      data (templates(17)%mapgrid(j),j=1,11) /1,1,2,1,-4,4,4,1,1,1,4/

      data templates(18)%template_num /110/ !  Equatorial Azimuthal equidistant
      data templates(18)%mapgridlen /16/
      data templates(18)%needext /.false./
      data (templates(18)%mapgrid(j),j=1,16) 
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,4,4,1,1/

       data templates(19)%template_num /120/     !  Azimuth-range 
       data templates(19)%mapgridlen /7/
       data templates(19)%needext /.true./
       data (templates(19)%mapgrid(j),j=1,7) /4,4,-4,4,4,4,1/

       data templates(20)%template_num /1000/     !  Cross Section Grid 
       data templates(20)%mapgridlen /20/
       data templates(20)%needext /.true./
       data (templates(20)%mapgrid(j),j=1,20) 
     &              /1,1,4,1,4,1,4,4,4,4,-4,4,1,4,4,1,2,1,1,2/

       data templates(21)%template_num /1100/     !  Hovmoller Diagram Grid 
       data templates(21)%mapgridlen /28/
       data templates(21)%needext /.false./
       data (templates(21)%mapgrid(j),j=1,28) 
     &    /1,1,4,1,4,1,4,4,4,4,-4,4,1,-4,4,1,4,1,-4,1,1,-4,2,1,1,1,1,1/

       data templates(22)%template_num /1200/     !  Time Section Grid 
       data templates(22)%mapgridlen /16/
       data templates(22)%needext /.true./
       data (templates(22)%mapgrid(j),j=1,16) 
     &              /4,1,-4,1,1,-4,2,1,1,1,1,1,2,1,1,2/

      data templates(23)%template_num /31/     !  Albers Equal Area
      data templates(23)%mapgridlen /22/
      data templates(23)%needext /.false./
      data (templates(23)%mapgrid(j),j=1,22) 
     &              /1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1,-4,-4,-4,4/

      contains


         integer function getgridindex(number)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getgridindex
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2001-06-28
!
! ABSTRACT: This function returns the index of specified Grid
!   Definition Template 3.NN (NN=number) in array templates.
!
! PROGRAM HISTORY LOG:
! 2001-06-28  Gilbert
!
! USAGE:    index=getgridindex(number)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Grid Definition
!                Template 3.NN that is being requested.
!
! RETURNS:  Index of GDT 3.NN in array templates, if template exists.
!           = -1, otherwise.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number

           getgridindex=-1

           do j=1,MAXTEMP
              if (number.eq.templates(j)%template_num) then
                 getgridindex=j
                 return
              endif
           enddo

         end function


         subroutine getgridtemplate(number,nummap,map,needext,iret)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getgridtemplate 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-09
!
! ABSTRACT: This subroutine returns grid template information for a 
!   specified Grid Definition Template 3.NN.
!   The number of entries in the template is returned along with a map
!   of the number of octets occupied by each entry.  Also, a flag is
!   returned to indicate whether the template would need to be extended.
!
! PROGRAM HISTORY LOG:
! 2000-05-09  Gilbert
!
! USAGE:    CALL getgridtemplate(number,nummap,map,needext,iret)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Grid Definition 
!                Template 3.NN that is being requested.
!
!   OUTPUT ARGUMENT LIST:      
!     nummap   - Number of entries in the Template
!     map()    - An array containing the number of octets that each 
!                template entry occupies when packed up into the GDS.
!     needext  - Logical variable indicating whether the Grid Defintion
!                Template has to be extended.  
!     ierr     - Error return code.
!                0 = no error
!                1 = Undefine Grid Template number.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number
           integer,intent(out) :: nummap,map(*),iret
           logical,intent(out) :: needext

           iret=0

           index=getgridindex(number)

           if (index.ne.-1) then
              nummap=templates(index)%mapgridlen
              needext=templates(index)%needext
              map(1:nummap)=templates(index)%mapgrid(1:nummap)
           else
             nummap=0
             needext=.false.
             print *,'getgridtemplate: Grid Template ',number,
     &               ' not defined.'
             iret=1
           endif

         end subroutine


         subroutine extgridtemplate(number,list,nummap,map)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    extgridtemplate 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-09
!
! ABSTRACT: This subroutine generates the remaining octet map for a 
!   given Grid Definition Template, if required.  Some Templates can 
!   vary depending on data values given in an earlier part of the 
!   Template, and it is necessary to know some of the earlier entry
!   values to generate the full octet map of the Template.
!
! PROGRAM HISTORY LOG:
! 2000-05-09  Gilbert
!
! USAGE:    CALL extgridtemplate(number,list,nummap,map)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Grid Definition 
!                Template 3.NN that is being requested.
!     list()   - The list of values for each entry in 
!                the Grid Definition Template.
!
!   OUTPUT ARGUMENT LIST:      
!     nummap   - Number of entries in the Template
!     map()    - An array containing the number of octets that each 
!                template entry occupies when packed up into the GDS.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number,list(*)
           integer,intent(out) :: nummap,map(*)

           index=getgridindex(number)
           if (index.eq.-1) return

           if ( .not. templates(index)%needext ) return
           nummap=templates(index)%mapgridlen
           map(1:nummap)=templates(index)%mapgrid(1:nummap)

           if ( number.eq.120 ) then
              N=list(2)
              do i=1,N
                map(nummap+1)=2
                map(nummap+2)=-2
                nummap=nummap+2
              enddo
           elseif ( number.eq.1000 ) then
              N=list(20)
              do i=1,N
                map(nummap+1)=4
                nummap=nummap+1
              enddo
           elseif ( number.eq.1200 ) then
              N=list(16)
              do i=1,N
                map(nummap+1)=4
                nummap=nummap+1
              enddo
           endif

         end subroutine

         integer function getgdtlen(number)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getgdtlen
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2004-05-11
!
! ABSTRACT: This function returns the initial length (number of entries) in
!   the "static" part of specified Grid Definition Template 3.number.
!
! PROGRAM HISTORY LOG:
! 2004-05-11  Gilbert
!
! USAGE:    CALL getgdtlen(number)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Grid Definition
!                Template 3.NN that is being requested.
!
! RETURNS:     Number of entries in the "static" part of GDT 3.number
!              OR returns 0, if requested template is not found.
!
! REMARKS: If user needs the full length of a specific template that
!    contains additional entries based on values set in the "static" part
!    of the GDT, subroutine extgridtemplate can be used.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number

           getgdtlen=0

           index=getgridindex(number)

           if (index.ne.-1) then
              getgdtlen=templates(index)%mapgridlen
           endif

         end function


      end

