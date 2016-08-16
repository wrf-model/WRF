#
set SHELL_DIR = `pwd`
set kma_bufr_convertor = ${SHELL_DIR}/kma_bufr_src
set kma_bufr_obs_dir   = ${SHELL_DIR}/kma_bufr_obs
set ecmwf_lib          = /data7/da/guo/Linux
#
#--------------------------------------------------------------------
# Link the ecmwf bufr library:
ln -sf ${ecmwf_lib}/ecmwf_bufr  ecmwf_lib
#====================================================================
cd ${kma_bufr_convertor}
make clean
make
#
#
cd ${kma_bufr_obs_dir}
if ( -e read_bufr ) rm -f read_bufr
ln -sf ${kma_bufr_convertor}/read_bufr  read_bufr
#
echo "read_bufr...."
./read_bufr >& ${SHELL_DIR}/read_bufr.out
mv littler    ${SHELL_DIR}/LITTLE_R 
rm read_bufr  
echo "read_bufr completed."
#
#
