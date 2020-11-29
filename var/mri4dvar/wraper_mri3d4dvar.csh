#!/bin/tcsh -f
#   script            1:VAR4D 2:MULTI_INC 3:use_cvt 4:use_vp 5:WORK_DIR  6/7:THIN_FACTOR 8:BE1 9:BE2

./run_mri3d4dvar.csh_pbs  true  true  false true ztd30min_mri4dvar_6km6km_512core 3 3  6km  6km > &! log.66

#-------- 3DVAR runs with interpolation of CVT
#./run_mri3d4dvar.csh  false  true  true false mri3dvar_2km2km_cvt   1 1  2km  2km > &! log.22_cvt
#./run_mri3d4dvar.csh  false  true  true false mri3dvar_6km6km_cvt   3 3  6km  6km > &! log.66_cvt
#./run_mri3d4dvar.csh  false  true  true false mri3dvar_18km6km_cvt  9 3 18km  6km > &! log.186_cvt
#-------- 3DVAR runs with Inverse of transform U and interpolation of vp
#./run_mri3d4dvar.csh  false  true false  true mri3dvar_2km2km_vp    1 1  2km  2km > &! log.22_vp
#./run_mri3d4dvar.csh  false  true false  true mri3dvar_6km6km_vp    3 3  6km  6km > &! log.66_vp
#./run_mri3d4dvar.csh  false  true false  true mri3dvar_18km6km_vp   9 3 18km 6km  > &! log.186_vp
#-------- 3DVAR runs with cvt=0 for the second outer loop
#./run_mri3d4dvar.csh  false  true false false mri3dvar_2km2km_cvt0  1 1  2km  2km > &! log.22_cvt0
#./run_mri3d4dvar.csh  false  true false false mri3dvar_6km6km_cvt0  3 3  6km  6km > &! log.66_cvt0
#./run_mri3d4dvar.csh  false  true false false mri3dvar_18km6km_cvt0 9 3  18km 6km > &! log.186_cvt0
#-------------
#./run_mri3d4dvar.csh  true  true  true false mri3dvar_6km6km_cvt   3 3  6km  6km > &! log.66_cvt
#./run_mri3d4dvar.csh  true  true  true false mri3dvar_18km6km_cvt  9 3 18km  6km > &! log.186_cvt
#-------- 4DVAR runs with Inverse of transform U and interpolation of vp
#./run_mri3d4dvar.csh  true  true false  true mri3dvar_2km2km_vp    1 1  2km  2km > &! log.22_vp
#./run_mri3d4dvar.csh  true  true false  true mri3dvar_6km6km_vp    3 3  6km  6km > &! log.66_vp
#./run_mri3d4dvar.csh  true  true false  true mri3dvar_18km6km_vp   9 3 18km 6km  > &! log.186_vp
#-------- 4DVAR runs with cvt=0 for the second outer loop
#./run_mri3d4dvar.csh  true  true false false mri3dvar_2km2km_cvt0  1 1  2km  2km > &! log.22_cvt0
#./run_mri3d4dvar.csh  true  true false false mri3dvar_6km6km_cvt0  3 3  6km  6km > &! log.66_cvt0
#./run_mri3d4dvar.csh  true  true false false mri3dvar_18km6km_cvt0 9 3  18km 6km > &! log.186_cvt0
#-------- 4DVAR runs with interpolation of CVT
#./run_mri3d4dvar.csh  true  false  false false ztd30min_4dvar_2km2km   1 1  2km  2km > &! log.22
