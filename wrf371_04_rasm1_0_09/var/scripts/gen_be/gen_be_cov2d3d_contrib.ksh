#!/bin/ksh
#------------------------------------------------------------------------
#  Purpose: Computes contribution of 2D Field on the balanced part
#           of 3D Field for WRFDA cv_options=6
#
#  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
#           & Monika Krysta, (CTBTO, Vienna, Austria)
#
#  Note: Please acknowledge author/institute in work that uses this code.
#------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

cd $RUN_DIR/working

ln -sf ${BUILD_DIR}/gen_be_cov2d3d_contrib.exe .

cat > gen_be_cov_contrib_nl.nl << EOF
  &gen_be_cov3d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    ne = ${NE},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}'/
EOF

./gen_be_cov2d3d_contrib.exe > gen_be_cov2d3d_contrib.$VARIABLE1.$VARIABLE2.log 2>&1

exit 0

