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


 set TIME_WINDOW_MIN = '2001062711'
 set TIME_ANALYSIS   = '2001062712'
 set TIME_WINDOW_MAX = '2001062713'

 set OBSDATA  = /mmmtmp/guo/obs_gts.3dvar.domain1

 echo " "

 if (-e $OBSDATA) then

 if ( -e gmeta )         rm gmeta
 if ( -e namelist.file ) rm namelist.file

cat >! namelist.file << EOF

 &MAPBG
 PHIC  =  28.5 ,
 XLONC = 116.0,
 IEXP  =  0,          ; Domain expanded (1) or not (0) ?
 AEXP  =   120.,      ; Distance of expansion in km on each side of domain
 IPROJ = 'LAMCON',
 ;IPROJ = 'POLSTR',   ; MAP PROJECTION
 ;IPROJ = 'MERCAT',   ; MAP PROJECTION
 TRUELAT1=10.,
 TRUELAT2=45.,
 &

 &DOMAINS
 MAXNES =   2,
 NESTIX =   67,  100,  110,  130,  194,
 NESTJX =   81,  100,  110,  146,  146,
 DIS    = 135.,  45.,  45.,  15.,   5.,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   15,  18,    32,   30,
 NESTJ  =    1,   28,  33,    29,   50,
 &
 
 &TIME_WINDOW
  TIME_WINDOW_MIN = '$TIME_WINDOW_MIN', ; beginning of time window
  TIME_WINDOW_MAX = '$TIME_WINDOW_MAX', ; end of time window
 &

 &SKEW_PLOTS
  skewt_plot = .FALSE.,
  is_plot = .FALSE.,  ; .TRUE. to plot Tb19v innovation with different symbol.
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
