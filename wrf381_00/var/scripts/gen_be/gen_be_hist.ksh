#!/bin/ksh
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

cd $RUN_DIR/working

ln -sf ${BUILD_DIR}/gen_be_hist.exe .

cat > gen_be_hist_nl.nl << EOF
  &gen_be_hist_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    ne = ${NE},
    variable = '${VARIABLE}',
    Nstdev = 5,
    N_dim_hist = 21 /
EOF

./gen_be_hist.exe > gen_be_hist.$VARIABLE.log 2>&1

exit 0

