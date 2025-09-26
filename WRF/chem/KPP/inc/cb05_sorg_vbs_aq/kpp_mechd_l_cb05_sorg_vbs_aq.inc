REAL(KIND=dp) :: rolto,roltho,rolto3,roltno3,            &
                 rolio,roliho,rolio3,rolino3,            &
                 rtolho,rxylho,rcslho,rcslno3,           &
                 rhc5ho,rhc8ho,                          &
                 risoo,risoho,risoo3,risono3,            &
                 rapiho,rapio3,rbpiho,rbpio3,rbpino3,    &
                 rterpo,rterpho,rterpo3,rterpno3,        &
                 rociho,rlimho,rsesqho,                  &
                 rxo2no,rxo2nno,rxo2ho2,rxo2nho2,        &
                 rxo2co3,rxo2cxo3,rxo2xo2,rxo2xo2n,rxo2nxo2n

REAL(KIND=dp) , DIMENSION(ldrog_vbs)  :: PRDROG
REAL(KIND=dp) :: ro2loss

 REAL :: tempk
 REAL :: pressure

 INTEGER, PARAMETER :: BLKSIZE = 1
 INTEGER :: NCELL

 REAL, PARAMETER :: CMIN = 1.001D-30
 REAL, PARAMETER :: PMDGMIN = 1.0D-9

 REAL :: PMNUM ( NASECT )
 REAL :: PMSG ( NASECT )
 REAL :: DRAT
 REAL :: DPLO
 REAL :: DPHI
 INTEGER :: iphase
 INTEGER :: itype
 INTEGER :: INASEC
 INTEGER :: l

 REAL :: RTDAT_AE ( NRXNAERO )

 REAL(KIND=dp) :: rtdat_ae_so2 
