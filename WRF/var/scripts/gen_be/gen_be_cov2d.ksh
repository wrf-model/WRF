#!/bin/ksh
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

cd $RUN_DIR/working

echo "---------------------------------------------------------------"
echo "Run gen_be_cov2d."
echo "---------------------------------------------------------------"

ln -sf ${BUILD_DIR}/gen_be_cov2d.exe .

cat > gen_be_cov2d_nl.nl << EOF
  &gen_be_cov2d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT}, 
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}' /
EOF

./gen_be_cov2d.exe > gen_be_cov2d.$VARIABLE1.$VARIABLE2.log 2>&1

exit 0

