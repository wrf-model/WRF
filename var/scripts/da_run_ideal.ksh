#!/bin/ksh
#########################################################################
# Script: da_run_ideal.ksh
#
# Purpose: Run WRF's ideal utility.
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/ideal}
export WORK_DIR=$RUN_DIR/working

export SCENARIO=${SCENARIO:-em_b_wave}
export IDEAL_OUTPUT_DIR=${IDEAL_OUTPUT_DIR:-$RC_DIR}

if [[ ! -d $IDEAL_OUTPUT_DIR/$DATE ]]; then mkdir -p $IDEAL_OUTPUT_DIR/$DATE; fi

rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

#Get extra namelist variables:
. $SCRIPTS_DIR/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT ideal</TITLE></HEAD><BODY>"
echo "<H1>$EXPT ideal</H1><PRE>"

date    

echo 'REL_DIR           <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR           <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR           <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR          <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'IDEAL_OUTPUT_DIR  <A HREF="file:'$IDEAL_OUTPUT_DIR'">'$IDEAL_OUTPUT_DIR'</a>'
echo "SCENARIO          $SCENARIO"
echo "DOMAINS           $DOMAINS"
echo "DATE              $DATE"
echo "END_DATE          $END_DATE"

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600

export NL_AUXINPUT1_INNAME="met_em.d<domain>.<date>"

if [[ $WRF_NAMELIST'.' != '.' ]]; then
   ln -fs $WRF_NAMELIST namelist.input
elif [[ -f $WRF_DIR/inc/namelist_script.inc ]]; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/$SCENARIO/namelist.input .
fi

cp namelist.input $RUN_DIR

ln -fs $WRF_DIR/test/$SCENARIO/input* .

echo '<A HREF="namelist.input">Namelist input</a>'

if $DUMMY; then
   echo "Dummy ideal"
   for DOMAIN in $DOMAINS; do
      echo Dummy ideal > wrfinput_d${DOMAIN}
   done
   echo Dummy ideal > wrfbdy_d01
   # echo Dummy ideal > wrflowinp_d01
else
   ln -fs ${WRF_DIR}/main/ideal_${SCENARIO}.exe .
   $RUN_CMD ./ideal_${SCENARIO}.exe
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

for DOMAIN in $DOMAINS; do
   if [[ -f $WORK_DIR/wrfinput_d${DOMAIN} ]]; then
      mv $WORK_DIR/wrfinput_d${DOMAIN} $IDEAL_OUTPUT_DIR/$DATE
   fi
done
mv $WORK_DIR/wrfbdy_d01 $IDEAL_OUTPUT_DIR/$DATE
# mv $WORK_DIR/wrflowinp_d01 $IDEAL_OUTPUT_DIR/$DATE

date

echo "</BODY></HTML>"

exit $RC
