#
set source_data = /karri3/guo/Verif_data
set source_file = obs_gts.3dvar
#
set directory   = /karri3/guo/Verif_V3
mkdir -p ${directory}
cd ${directory}
#
#(1) To get the data files for generating the "filtered obs" files:
#
set initial_time = 2007082100
set end_time     = 2007082500
set interval     = 06
#
set num = 0
set time = $initial_time
while ( $time <= $end_time )
    @ num ++
    echo ${num}:  $time
    mkdir ${time}
    cd $time
# get the original obs files:
     cp ${source_data}/${time}/wrfinput_d01.${time}  wrfinput_d01
     cp ${source_data}/${time}/${source_file}_${time}_d1  .
     ln -sf ${source_file}_${time}_d1  ob.ascii
#    rm -rf wrfvar
#    rm ob.ascii
    cd ..
    set time = `da_advance_time  $time $interval`
end
#
# (2) To get the forecast files for experiments for verification:
#
cd ${directory}
mkdir -p fc
cd fc
#
set EXP = exp1
mkdir $EXP
cd $EXP
#
set initial_time = 2007082100
set end_time     = 2007082200
set interval     = 12
#
set num = 0
set time = $initial_time
while ( $time <= $end_time )
    @ num ++
    echo ${num}:  $time
    mkdir ${time}
    cd $time
      cp ${source_data}/fc/${EXP}/${time}/wrfout* .
    cd ..
    set time = `da_advance_time  $time $interval`
end
#


