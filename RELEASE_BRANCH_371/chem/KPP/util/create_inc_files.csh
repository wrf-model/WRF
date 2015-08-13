#!/bin/csh -f


if ( $#argv != 1 ) then
echo Usage: create_inc_files.csh name_of_mechanism
endif 

set model =  $argv[1]
set incm_dir = inc/${model}


# make directory (if not already there)
   if ( ! -e inc/${model} ) then
    mkdir ${incm_dir}
   endif

   
# if not already there, then put empty .inc files into directory 

    set inc_list = "u l b a ibu ib ia e"

    foreach inam ( $inc_list )
       if (! -e ${incm_dir}/kpp_mechd_${inam}_${model}.inc ) then
          echo ! > ${incm_dir}/kpp_mechd_${inam}_${model}.inc
       endif
    end

    if (! -e ${incm_dir}/extra_args_to_update_rconst_${model}.inc ) then
          echo ! > ${incm_dir}/extra_args_to_update_rconst_${model}.inc
    endif


    if (! -e ${incm_dir}/extra_args_update_rconst_${model}.inc ) then
          echo ! > ${incm_dir}/extra_args_update_rconst_${model}.inc
    endif

    if (! -e ${incm_dir}/extra_decls_update_rconst_${model}.inc ) then
          echo ! > ${incm_dir}/extra_decls_update_rconst_${model}.inc
    endif



# link .inc files in chem/KPP/$(model)/inc


set kincfiles = ( ${incm_dir}/*.inc )



foreach kincfile ( $kincfiles )

 set incf = `echo $kincfile:t`


 if ( ! -e ${WRFC_ROOT}/inc/${incf} ) then

   echo ln -s ../chem/${WKC_DIRNAME}/${kincfile}  ${WRFC_ROOT}/inc
   ln -s ../chem/${WKC_DIRNAME}/${kincfile}  ${WRFC_ROOT}/inc
 
 endif 

end



exit 0
