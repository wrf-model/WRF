#!/bin/ksh
#########################################################################
# Script: da_plot_psot.ksh
#
# Purpose:  A script to plot pseudo single observation test
#          
# By Hui Shao, NCAR DATC 08/27/2007
#########################################################################
#------------------------------------------------------------------------
#Set defaults for required environment variables
#------------------------------------------------------------------------
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

echo "expt_dir= $EXP_DIR"
export PLOT_DIR=${PLOT_DIR:-$EXP_DIR/plotpsot} #where will be the plots

if test ! -d $PLOT_DIR; then mkdir $PLOT_DIR; fi
cd $PLOT_DIR

export PLOT_WKS=${PLOT_WKS:-pdf}

#-------------------------------------------------------------------------
# convert pseudo list into array
#-------------------------------------------------------------------------
ivar=0
for var in $PSEUDO_VAR_LIST; do
   (( ivar=ivar+1 ))
   export PSEUDO_VAR[$ivar]=$var
done

ival=0
for var in $PSEUDO_VAL_LIST; do
   (( ival=ival+1 ))
   export PSEUDO_VAL[$ival]=$var
done

ierr=0
for var in $PSEUDO_ERR_LIST; do
   (( ierr=ierr+1 ))
   export PSEUDO_ERR[$ierr]=$var
done

ix=0
for var in $PSEUDO_X_LIST; do
   (( ix=ix+1 ))
   export PSEUDO_X[$ix]=$var
done

iy=0
for var in $PSEUDO_Y_LIST; do
   (( iy=iy+1 ))
   export PSEUDO_Y[$iy]=$var
done

iz=0
for var in $PSEUDO_Z_LIST; do
   (( iz=iz+1 ))
   export PSEUDO_Z[$iz]=$var
done

#-------------------------------------------------------------------------
# plot
#-------------------------------------------------------------------------
iv=1
for var in ${PSEUDO_VAR[*]}; do

   expt=${EXPT}_psot$iv
   xlon=${PSEUDO_X[$iv]}
   xlat=${PSEUDO_Y[$iv]}
   kl=${PSEUDO_Z[$iv]}
   omb=${PSEUDO_VAL[$iv]}
   err=${PSEUDO_ERR[$iv]}

   if [[ $var = u ]]; then unit="m s-1"; fi
   if [[ $var = v ]]; then unit="m s-1"; fi
   if [[ $var = t ]]; then unit="K"; fi
   if [[ $var = q ]]; then unit="kg kg-1"; fi

   DATE=$INITIAL_DATE

      export FIRST_GUESS=${EXP_DIR}/$DATE/wrfvar/working/wrfinput_d${DOMAINS}
      export ANALYSIS=${EXP_DIR}/fc/$DATE/wrfinput_d01


      NCL_COMMAND_LINE="'works=\"${PLOT_WKS}\"' 'expt=\"$expt\"'  \
                      'kl=$kl' 'xlon=$xlon' 'xlat=$xlat' 'var=\"$var\"' 'date=\"$DATE\"'  \
                      'omb=\"$omb\"' 'err=\"$err\"' 'varunit=\"$unit\"' \
                      'bakfile=\"$FIRST_GUESS\"' 'analfile=\"$ANALYSIS\"'"

      rm -f run1 run2 run3

      echo "ncl ${NCL_COMMAND_LINE} $WRFVAR_DIR/var/graphics/ncl/psot_xy_auto.ncl" > run1
      chmod +x run1
      ./run1

      echo "ncl ${NCL_COMMAND_LINE} $WRFVAR_DIR/var/graphics/ncl/psot_xz_auto.ncl" > run2
      chmod +x run2
      ./run2

      echo "ncl ${NCL_COMMAND_LINE} $WRFVAR_DIR/var/graphics/ncl/psot_yz_auto.ncl" > run3
      chmod +x run3
      ./run3

   (( iv=iv+1 ))
done

exit 0
