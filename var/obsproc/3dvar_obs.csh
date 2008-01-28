#! /bin/csh -f
# 
make
#
ln -sf /datatmp1/guo/DUJUAN_data/2003083012/namelist.3dvar_obs \
                                            namelist.3dvar_obs
#
3dvar_obs.exe >& 3dvar_obs.out
#
