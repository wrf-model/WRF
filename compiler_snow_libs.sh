
# ENTER the snpack folder
cd ./snpack_for_wrf

mkdir -p ./snow_libs

# compile meteoio
bash ./compiler.meteoio

# compile snowpack
bash ./compiler.snowpack

# compile coupler
bash ./compiler.coupler

export SNOWLIBS=$(pwd)
cd ..
