#!/bin/ksh
#########################################################################
# Script: da_run_nup.ksh
#
# Purpose: Run WRF's nup utility.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/nup}
export WORK_DIR=$RUN_DIR/working


export NUP_INPUT1_DIR=${NUP_INPUT1_DIR:-$RC_DIR}
export NUP_INPUT2_DIR=${NUP_INPUT2_DIR:-$RC_DIR}
export NUP_OUTPUT_DIR=${NUP_OUTPUT_DIR:-$RC_DIR}

if [[ ! -d $NUP_OUTPUT_DIR/$DATE ]]; then mkdir -p $NUP_OUTPUT_DIR/$DATE; fi

rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

#Get extra namelist variables:
. $SCRIPTS_DIR/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT nup</TITLE></HEAD><BODY>"
echo "<H1>$EXPT nup</H1><PRE>"

date    

echo 'REL_DIR          <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR          <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR          <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR         <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'NUP_INPUT1_DIR <A HREF="file:'$NUP_INPUT1_DIR'">'$NUP_INPUT1_DIR'</a>'
echo 'NUP_INPUT2_DIR <A HREF="file:'$NUP_INPUT2_DIR'">'$NUP_INPUT2_DIR'</a>'
echo 'NUP_OUTPUT_DIR <A HREF="file:'$NUP_OUTPUT_DIR'">'$NUP_OUTPUT_DIR'</a>'
echo "DATE             $DATE"
echo "END_DATE         $END_DATE"
echo "FCST_RANGE       $FCST_RANGE"

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

if $DUMMY; then
   echo "Dummy nup"
   echo Dummy nup > wrfinput_d${DOMAIN}
   echo Dummy nup > wrfbdy_d${DOMAIN}
else
   ln -fs ${WRF_DIR}/main/nup.exe .
   $RUN_CMD ./nup.exe
   RC=$?

   if [[ -f namelist.output ]]; then
     cp namelist.output $RUN_DIR/namelist.output
   fi

   rm -rf $RUN_DIR/rsl
   mkdir -p $RUN_DIR/rsl
   mv rsl* $RUN_DIR/rsl
   cd $RUN_DIR/rsl
   for FILE in rsl*; do
      echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
      echo "<H1>$FILE</H1><PRE>" >> $FILE.html
      cat $FILE >> $FILE.html
      echo "</PRE></BODY></HTML>" >> $FILE.html
      rm $FILE
   done
   cd $RUN_DIR

   echo '<A HREF="namelist.output">Namelist output</a>'
   echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
   echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
   echo '<A HREF="rsl">Other RSL output</a>'

   echo $(date +'%D %T') "Ended $RC"
fi

if [[ -f $WORK_DIR/wrfinput_d${DOMAIN} ]]; then
   mv $WORK_DIR/wrfinput_d${DOMAIN} $NUP_OUTPUT_DIR/$DATE
fi

if [[ -f $WORK_DIR/wrfbdy_d${DOMAIN} ]]; then
   mv $WORK_DIR/wrfbdy_d${DOMAIN} $NUP_OUTPUT_DIR/$DATE
fi

if $CLEAN; then
   rm -rf $WORK_DIR
fi

date

echo "</BODY></HTML>"

exit 0
