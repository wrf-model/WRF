  INTEGER                  ::   ids2,ide2, jds2,jde2, kds2,kde2
  REAL, DIMENSION( CHUNK , kte ) ::   t_,q_,p_,delz_,den_
  REAL, DIMENSION( CHUNK , kte, 2 ) ::   qci_, qrs_
  REAL, DIMENSION( CHUNK ) :: rain_,rainncv_,sr_,snow_,snowncv_
  INTEGER ::               i,j,k
  INTEGER ::               ii,ic,ip
  INTEGER :: iii
  INTEGER :: ntds
  INTEGER, EXTERNAL :: omp_get_max_threads
