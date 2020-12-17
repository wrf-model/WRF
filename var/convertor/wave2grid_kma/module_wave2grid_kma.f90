      MODULE module_wave2grid_kma

      use da_control, only : gravity, gas_constant
     
      PARAMETER (KMAX=30)
C==== T213 (640*321)
C     PARAMETER(IMAXE=640,JMAXE=321)
C     PARAMETER(IMAX=640,JMAX=320)
C     PARAMETER(IDIM=640,JDIM=320)
C     PARAMETER(MEND1 =214,NEND1=214,JEND1=214)

C==== T213 (428*215)
      PARAMETER(IMAXE=428,JMAXE=215)
      PARAMETER(IMAX=428,JMAX=214)
      PARAMETER(IDIM=428,JDIM=214)
      PARAMETER(MEND1 =214,NEND1=214,JEND1=214)

C==== T63 (192*97)
C     PARAMETER(IMAXE=192,JMAXE=97)
C     PARAMETER(IMAX=192,JMAX=96)
C     PARAMETER(IDIM=192,JDIM=96)
C     PARAMETER(MEND1=64,NEND1=64,JEND1=64)

      PARAMETER(ISST=360,JSST=181)
      PARAMETER(ISNW=360,JSNW=180)
      PARAMETER(MAXJZ=16)
      PARAMETER(JMAXHF=JMAX/2)
      PARAMETER(MNWAV=MEND1*(MEND1+1)/2)
      PARAMETER(IVAR=6,IMX=IMAX+2)

      CONTAINS

#include  "BSSLZ1.inc"
#include  "CR8I2V.inc"
#include  "CVDATE.inc"
#include  "GAUSS.inc"
#include  "GH2TV.inc"
#include  "LT2GAU.inc"
#include  "GPLHGT.inc"
#include  "MINMAX.inc"
#include  "MONTWO.inc"
#include  "OUTZ.inc"
#include  "PRESUB.inc"
#include  "REDANL.inc"
#include  "REDDAT.inc"
#include  "REDDAT_ASCII.inc"
#include  "REDDAT_BIN.inc"
#include  "REDGES.inc"
#include  "REDHED.inc"
#include  "RESET.inc"
#include  "SPLDIF3_H.inc"
#include  "TETEN.inc"
#include  "PACK.inc"
#include  "VPRM.inc"
#include  "WRTDAT.inc"
#include  "WRTEOF.inc"
#include  "WRTHED.inc"
#include  "ZE2TVE.inc"
#include  "ZMNLAT.inc"
#include  "ZMNT.inc"
#include  "RELHUM.inc"
#include  "PREGSM.inc"
#include  "PREGSM1.inc"
#include  "Einc_to_Ganl.inc"

      END MODULE  module_wave2grid_kma
