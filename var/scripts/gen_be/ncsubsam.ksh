#! /bin/ksh -x
#-----------------------------------------------------------------------
# Script ncsubsam.ksh
# Purpose: Use ncks to subsample WRF forecast files.
#-----------------------------------------------------------------------
 cd ~/research/wrf-da/var/gen_be/ENS/fc
 NVERT=5
 cat > namelist.v_interp << EOF
 &newlevels
 nvert   = ${NVERT},
 nlevels = 1.000,0.993,0.548,0.013,0.000
 /
EOF
 n=1
 typeset -Z3 n
# Sample Dowell data (west_east=350, south_north=450, bottom_top=34) at
# horizontal stride s:
 s=1
 ae=(/usr/local/nco-3.9.4/bin/ncatted                                 \
    -a BOTTOM-TOP_GRID_DIMENSION,global,o,i,$NVERT                    \
    -a BOTTOM-TOP_PATCH_END_STAG,global,o,i,$NVERT                    \
    -a BOTTOM-TOP_PATCH_END_UNSTAG,global,o,i,$(($NVERT-1))           \
    -a SOUTH-NORTH_GRID_DIMENSION,global,o,i,$((1+floor(450./$s.)))   \
    -a SOUTH-NORTH_PATCH_END_STAG,global,o,i,$((1+floor(450./$s.)))   \
    -a SOUTH-NORTH_PATCH_END_UNSTAG,global,o,i,$((1+floor(449./$s.))) \
    -a WEST-EAST_GRID_DIMENSION,global,o,i,$((1+floor(350./$s.)))     \
    -a WEST-EAST_PATCH_END_STAG,global,o,i,$((1+floor(350./$s.)))     \
    -a WEST-EAST_PATCH_END_UNSTAG,global,o,i,$((1+floor(349./$s.)))   )
 ae=${ae[*]}
 ks=(/usr/local/nco-3.9.4/bin/ncks                  \
    -d west_east,,,$s    -d west_east_stag,,,$s     \
    -d south_north,,,$s  -d south_north_stag,,,$s -O )
 ks=${ks[*]}
 ks=\cp
 vi=~/research/wrf-da/v_interp/v_interp
 for i in {1..30};do
    d=2007032903.e$n
    echo $d
    mkdir -p subsam/$d
    f=${d}/wrfout_d01_2007-03-29_03:00:00
    g=subsam/$f
    \rm -f tmp.nc
    $vi $f tmp.nc>${g}-v_interp.log
    $ks tmp.nc $g 
    $ae $g
    (( n += 1 ))
 done
 f=2007032903.e001/wrfout_d01_2007-03-29_02:00:00
 g=subsam/$f
 $vi $f tmp.nc>${g}-v_interp.log
 $ks tmp.nc $g
 $ae $g
