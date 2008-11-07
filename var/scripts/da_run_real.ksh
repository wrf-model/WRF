#!/bin/ksh
#########################################################################
# Script: da_run_real.ksh
#
# Purpose: Run WRF's real utility.
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/real}
export WORK_DIR=$RUN_DIR/working

export REAL_INPUT_DIR=${REAL_INPUT_DIR:-$RC_DIR}
export REAL_OUTPUT_DIR=${REAL_OUTPUT_DIR:-$RC_DIR}

if [[ ! -d $REAL_OUTPUT_DIR/$DATE ]]; then mkdir -p $REAL_OUTPUT_DIR/$DATE; fi

rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

#Get extra namelist variables:
. $SCRIPTS_DIR/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT real</TITLE></HEAD><BODY>"
echo "<H1>$EXPT real</H1><PRE>"

date    

echo 'REL_DIR         <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR         <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR         <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR        <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'REAL_INPUT_DIR  <A HREF="file:'$REAL_INPUT_DIR'">'$REAL_INPUT_DIR'</a>'
echo 'REAL_OUTPUT_DIR <A HREF="file:'$REAL_OUTPUT_DIR'">'$REAL_OUTPUT_DIR'</a>'
echo "DATE            $DATE"
echo "END_DATE        $END_DATE"
echo "FCST_RANGE      $FCST_RANGE"
echo "DOMAINS         $DOMAINS"

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600

export NL_AUXINPUT1_INNAME="met_em.d<domain>.<date>"

if [[ $WRF_NAMELIST'.' != '.' ]]; then
   ln -fs $WRF_NAMELIST namelist.input
elif [[ -f $WRF_DIR/inc/namelist_script.inc ]]; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

if $DUMMY; then
   echo "Dummy real"
   for DOMAIN in $DOMAINS; do
      echo Dummy real > wrfinput_d${DOMAIN}
      # echo Dummy real > wrflowinp_d${DOMAIN}
   done
   echo Dummy real > wrfbdy_d01
else
   if [[ -e $REAL_INPUT_DIR/$DATE ]]; then
      ln -fs $REAL_INPUT_DIR/$DATE/met_em.d* .
   else
      LOCAL_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} -3 2>/dev/null)
      ln -fs $REAL_INPUT_DIR/$LOCAL_DATE/met_em.d* .
   fi
   ln -fs ${WRF_DIR}/main/real.exe .
   $RUN_CMD ./real.exe
   RC=$?

   if [[ -f namelist.output ]]; then
     cp namelist.output $RUN_DIR/namelist.output
   fi
   echo '<A HREF="namelist.output">Namelist output</a>'

   if [[ -f rsl.out.0000 ]]; then
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
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'
   fi
   cd $RUN_DIR

   echo $(date +'%D %T') "Ended $RC"
fi

for DOMAIN in $DOMAINS; do
   if [[ -f $WORK_DIR/wrfinput_d${DOMAIN} ]]; then
      mv $WORK_DIR/wrfinput_d${DOMAIN}* $REAL_OUTPUT_DIR/$DATE
      this_date_wrf=$($BUILD_DIR/da_advance_time.exe $DATE 0 -W 2>/dev/null)
      (cd $REAL_OUTPUT_DIR/$DATE; ln -sf wrfinput_d${DOMAIN} wrfinput_d${DOMAIN}.$this_date_wrf)
   fi
done
mv $WORK_DIR/wrfbdy_d01 $REAL_OUTPUT_DIR/$DATE

if $CLEAN; then
   rm -rf $WORK_DIR
fi

date

echo "</BODY></HTML>"

exit $RC
