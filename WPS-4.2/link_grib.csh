#!/bin/csh -f

set alpha = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )
set i1 = 1
set i2 = 1
set i3 = 1

if ( ( ${#argv} == 1 ) || ( ( ${#argv} == 2 ) && ( ${2} == "." ) ) ) then

   rm -f GRIBFILE.??? >& /dev/null

   foreach f ( ${1}* )
   
      ln -sf ${f} GRIBFILE.$alpha[$i3]$alpha[$i2]$alpha[$i1]
      @ i1 ++
   
      if ( $i1 > 26 ) then
         set i1 = 1
         @ i2 ++
        if ( $i2 > 26 ) then
           set i2 = 1
           @ i3 ++
           if ( $i3 > 26 ) then
              echo "RAN OUT OF GRIB FILE SUFFIXES!"
           endif
        endif
      endif
   
   end
else if ( ${#argv} > 1 ) then

   rm -f GRIBFILE.??? >& /dev/null

   foreach f ( $* )
   
      if ( $f != "." ) then
         ln -sf ${f} GRIBFILE.$alpha[$i3]$alpha[$i2]$alpha[$i1]
         @ i1 ++
   
         if ( $i1 > 26 ) then
            set i1 = 1
            @ i2 ++
            if ( $i2 > 26 ) then
               set i2 = 1
               @ i3 ++
               if ( $i3 > 26 ) then
                  echo "RAN OUT OF GRIB FILE SUFFIXES!"
               endif
            endif
         endif
      endif
   
   end
else if ( ${#argv} == 0 ) then
   echo " " 
   echo " " 
   echo "   Please provide some GRIB data to link"
   echo "   usage: $0 path_to_grib_data/grib_data_root"
   echo " " 
   echo " " 
endif

