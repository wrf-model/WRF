#! /bin/csh
##################################################################
#  This deck is used to produce a map of mesoscale observations 
#  sites, and produce the data files for 4DVAR.
#
#                                    Yong-Run Guo 
#                                     07/28/1999
###################################################################
#set echo
 unalias mv rm


 set TIME_WINDOW_MIN = '1997072111'
 set TIME_ANALYSIS   = '1997072112'
 set TIME_WINDOW_MAX = '1997072113'

 set OBSDATA  = ../obs_gts.${TIME_ANALYSIS}.3dvar

 echo " "

 if (-e $OBSDATA) then

 if ( -e gmeta )         rm gmeta
 if ( -e namelist.file ) rm namelist.file

cat >! namelist.file << EOF

 &MAPBG
 PHIC  =  32.0 ,
 XLONC = -90.0,
 IEXP  =  0,          ; Domain expanded (1) or not (0) ?
 AEXP  =   120.,      ; Distance of expansion in km on each side of domain
 IPROJ = 'LAMCON',
 ;IPROJ = 'POLSTR',   ; MAP PROJECTION
 ;IPROJ = 'MERCAT',   ; MAP PROJECTION
 TRUELAT1=20.,
 TRUELAT2=40.,
 &

 &DOMAINS
 MAXNES =   4,
 NESTIX=   61,  109,  211, 223,
 NESTJX=   75,  124,  211, 256,
 DIS   =  81.,  27.,  9.,   3.,
 NUMC  =    1,    1,   2,    3,
 NESTI =    1,   10,  20,   52,
 NESTJ =    1,   17,  25,   65,
 ;RID  =  3.0,  1.5, 1.5,  1.5,
 &
 
 &TIME_WINDOW
  TIME_WINDOW_MIN = '$TIME_WINDOW_MIN', ; beginning of time window
  TIME_WINDOW_MAX = '$TIME_WINDOW_MAX', ; end of time window
 &

 &SKEW_PLOTS
  skewt_plot = .FALSE.,
 &
EOF

 sed -f no_comment.sed  namelist.file >! tmp.file
 mv tmp.file  namelist.file

# make clean
  make

 ln -s -f $OBSDATA        fort.99
 ln -s -f namelist.file   fort.15

 echo " "
 echo "Map.exe >&! Map.out.${TIME_ANALYSIS}"
       Map.exe >&! Map.out.${TIME_ANALYSIS}
 echo " "

 if (-e gmeta) \
 echo "Generate plots in metafile gmeta.${TIME_ANALYSIS}"
 mv gmeta gmeta.${TIME_ANALYSIS}

else
 echo "Cannot find input file $OBSDATA"
endif
 echo " "
