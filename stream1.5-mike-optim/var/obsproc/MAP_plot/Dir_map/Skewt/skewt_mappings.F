      subroutine skewt_mappings(P, T, X, Y)
C
C  MAPPINGS FROM (P,T) TO CM ON SKEWT
C
      Y = 132.182 - 44.061 * ALOG10(P)
      X = 0.54 * T + 0.90692 * Y

      return
      end
