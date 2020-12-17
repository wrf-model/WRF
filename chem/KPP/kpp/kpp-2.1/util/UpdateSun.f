      SUBROUTINE Update_SUN()

      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

      KPP_REAL SunRise, SunSet
      KPP_REAL Thour, Tlocal, Ttmp 
   
      SunRise = 4.5
      SunSet  = 19.5
      Thour = TIME/3600.
      Tlocal = Thour - (INT(Thour)/24)*24

      IF ((Tlocal.GE.SunRise).AND.(Tlocal.LE.SunSet)) THEN
        Ttmp = (2.0*Tlocal-SunRise-SunSet)/(SunSet-SunRise)
        IF (Ttmp.GT.0) THEN
          Ttmp =  Ttmp*Ttmp
        ELSE
          Ttmp = -Ttmp*Ttmp
        END IF
        SUN = ( 1.0 + COS(PI*Ttmp) )/2.0 
      ELSE
        SUN = 0.0
      END IF

      RETURN
      END

