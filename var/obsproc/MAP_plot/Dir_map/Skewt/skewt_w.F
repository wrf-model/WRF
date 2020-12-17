      FUNCTION SKEWT_W(T, P)
C  W(GRAMS WATER VAPOR/KILOGRAM DRY AIR ), P(MILLIBAR )
      IF (T .GE. 999.) THEN
         W = 0.0
      ELSE
         X =  SKEWT_ESAT(T)
         SKEWT_W = 621.97 * X / (P - X)
      ENDIF
      RETURN
      END
