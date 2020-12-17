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


 set TIME_WINDOW_MIN = '2000052923'
 set TIME_ANALYSIS   = '2000053000'
 set TIME_WINDOW_MAX = '2000053001'

 set OBSDATA  = ../obs_gts.${TIME_ANALYSIS}.3dvar

 echo " "

 if (-e $OBSDATA) then

 if ( -e gmeta )         rm gmeta
 if ( -e namelist.file ) rm namelist.file

cat >! namelist.file << EOF

 &MAPBG
 PHIC  =  40.5 ,
 XLONC = -98.0,
 IEXP  =  0,          ; Domain expanded (1) or not (0) ?
 AEXP  =   360.,      ; Distance of expansion in km on each side of domain
 IPROJ = 'LAMCON',
 ;IPROJ = 'POLSTR',   ; MAP PROJECTION
 ;IPROJ = 'MERCAT',   ; MAP PROJECTION
 TRUELAT1=30.,
 TRUELAT2=60.,
 &

 &DOMAINS
 MAXNES =   2,
 NESTIX =  130,  124,  136,  181,  211, 
 NESTJX =  194,  121,  181,  196,  211,
 DIS    =  30.,  10.,  3.3,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   40,  28,    35,   45,
 NESTJ  =    1,   60,  25,    65,   55,
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
