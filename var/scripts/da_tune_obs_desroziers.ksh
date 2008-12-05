#!/bin/ksh
#-------------------------------------------------------------------------------------------
# Script for Observation error tuning  (Desroziers method)
#   Ref: QJRMS (2001), 127, pp. 1433-1452,
#        Gerald Desroziers and Serguei Ivanov
#-------------------------------------------------------------------------------------------
# Input files :  
#  a) rand_obs_error WRF-Var output files with "omb_add_noise" & "put_rand_seed" as .TRUE.
#  b) pert_obs       WRF-Var output files with "omb_add_noise" & "put_rand_seed" as .TRUE.
#  c) unpert_obs     WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#  d) fort.48        WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#  e) rsl.out.0000   WRF-Var output files with "omb_add_noise" .FALSE. (Default option)
#-------------------------------------------------------------------------------------------
#  Note:  For radiance data tuning edit
#         "namelist.radiance" generated down this script
#
#-------------------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/desroziers}
export WORK_DIR=$RUN_DIR/working

echo ""
echo "Running script da_tune_obs_desroziers.ksh"
echo ""

export YP_DIR=${YP_DIR:-$REG_DIR/with_noise}      # perturbed run
export Y_DIR=${Y_DIR:-$REG_DIR/no_noise}         # unperturbed run

export FILE_PERT=fort.45      # fort.45,  random perturbation
export FILE_YP=fort.46        # fort.46,  perturbed y=Hdx
export FILE_Y=fort.47         # fort.47,   y=Hdx
export FILE_JO=fort.48        # fort.48,   Jo
export FILE_RSLOUT=fort.49    # fort.49,  rsl.out.0000

export NL_RTMINIT_NSENSOR=${NL_RTMINIT_NSENSOR:-2}
export NL_RTMINIT_PLATFORM=${NL_RTMINIT_PLATFORM:-1,1}
export NL_RTMINIT_SATID=${NL_RTMINIT_SATID:-15,16}
export NL_RTMINIT_SENSOR=${NL_RTMINIT_SENSOR:-3,3}
export NL_RTMINIT_NCHAN=${NL_RTMINIT_NCHAN:-15,15} # Not a Registry name

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR
 
cat > namelist.radiance << EOF
&rtminit
   rtminit_nsensor     = $NL_RTMINIT_NSENSOR,
   rtminit_platform    = $NL_RTMINIT_PLATFORM,
   rtminit_satid       = $NL_RTMINIT_SATID,
   rtminit_sensor      = $NL_RTMINIT_SENSOR,
   rtminit_nchan       = $NL_RTMINIT_NCHAN /
EOF

touch fort.45 fort.46 fort.47 fort.48 fort.49 

export DATE=$START_DATE

echo "Output to $WORK_DIR"

while [[ $DATE -le $END_DATE ]]; do

   echo "   Processing $DATE"

   if [[ -f $YP_DIR/run/${DATE}/wrfvar/working/rand_obs_error ]]; then
      cat $YP_DIR/run/${DATE}/wrfvar/working/rand_obs_error  >> $FILE_PERT        
   else
      echo " Please check file rand_obs_error in " $YP_DIR/run/${DATE}/wrfvar/working
      exit 1             
   fi

   if [[ -f $YP_DIR/run/${DATE}/wrfvar/working/pert_obs ]]; then                  
      cat $YP_DIR/run/${DATE}/wrfvar/working/pert_obs       >> $FILE_YP        
   else
      echo " Please check file pert_obs in " $YP_DIR/run/${DATE}/wrfvar/working
      exit 2         
   fi

   if [[ -f $Y_DIR/run/${DATE}/wrfvar/working/unpert_obs ]]; then
      cat $Y_DIR/run/${DATE}/wrfvar/working/unpert_obs      >> $FILE_Y        
   else
      echo " Please check file unpert_obs in " $Y_DIR/run/${DATE}/wrfvar/working
      exit 3          
   fi

   if [[ -f $Y_DIR/run/${DATE}/wrfvar/working/jo ]]; then
      cat $Y_DIR/run/${DATE}/wrfvar/working/jo                >> $FILE_JO       
   else
      echo " Please check file jo in " $Y_DIR/run/${DATE}/wrfvar/working
      exit 4          
   fi

   if [[ -f $Y_DIR/run/${DATE}/wrfvar/rsl/rsl.out.0000.html ]]; then
      cat  $Y_DIR/run/${DATE}/wrfvar/rsl/rsl.out.0000.html      >> ${FILE_RSLOUT}
   else
      echo " Please check file rsl.out.0000.html in " $Y_DIR/run/${DATE}/wrfvar
      exit 5       
   fi

   export DATE=$($BUILD_DIR/da_advance_time.exe $DATE $CYCLE_PERIOD)
done

# append ***** to file end
echo "*****" >> ${FILE_PERT}
echo "*****" >> ${FILE_YP}
echo "*****" >> ${FILE_Y}
echo "*****" >> ${FILE_JO}
echo "*****" >> ${FILE_RSLOUT}

$BUILD_DIR/da_tune_obs_desroziers.exe > errfac.dat   

rm -rf fort.45 fort.46 fort.47 fort.48 fort.49

echo
echo "da_tune_obs_desroziers.ksh completed"
echo

exit 0

