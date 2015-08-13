#!/bin/csh

onintr clnup

#clean -a
set hst=`hostname | cut -c1-2`

if ( "$hst" == "be" ) then   # bluefire
  if ( -f ~michalak/sourceme_esmf ) then  # bluefire
    source ~michalak/sourceme_esmf
    set EXEC_WRF='bsub -K < ../tools/test4_0_ESMFSST.lsf.csh'
    (cd run ; tar xf ~michalak/jan00_esmftest.tar.gz )
    echo 3 | ./configure
    setenv J "-j 4"
  endif
endif
if ( "$hst" == "ma" ) then   # manta
  if ( -f ~johnm/sourceme_esmf ) then
    source ~johnm/sourceme_esmf
    (cd run ; tar xf ~johnm/jan00_esmftest.tar.gz )
    echo 3 | ./configure
    setenv J "-j 4"
    set EXEC_WRF="qsub -sync y wrf.pbs"
    cat > wrf.pbs << H2
#!/bin/csh
#PBS -l nodes=1:ppn=4
#PBS -m ae
#PBS -N WRF_ESMF_TEST
cd ${cwd}/run
setenv P4_GLOBMEMSIZE 20000000
time mpirun -machinefile \$PBS_NODEFILE -np 4 ./wrf_SST_ESMF.exe
H2
  endif
endif
./compile wrf
skip:
if ( -x main/wrf_SST_ESMF.exe ) then
  cd run
  /bin/cp ../test/em_esmf_exp/namelist.input.jan00.ESMFSST namelist.input
  /bin/rm -f rsl.*
  echo $EXEC_WRF >! com
  sh com
  if ( -f rsl.error.0000 ) then
    grep -q SUCCESS rsl.error.0000
    if ( $status == 0 ) then
      echo good show
    else
      echo failure
    endif
  else
    echo failure
  endif
  cd ..
endif

clnup:

