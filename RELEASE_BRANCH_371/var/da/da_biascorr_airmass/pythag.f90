FUNCTION pythag(a,b)
IMPLICIT NONE
INTEGER, PARAMETER :: sp = 8
REAL(sp), INTENT(IN) :: a,b
REAL(sp) :: pythag
!Computes (a2 + b2)1/2 without destructive under.ow or over.ow.
REAL(sp) :: absa,absb
absa=abs(a)
absb=abs(b)
if (absa > absb) then
pythag=absa*sqrt(1.0+(absb/absa)**2)
else
if (absb == 0.0) then
pythag=0.0
else
pythag=absb*sqrt(1.0+(absa/absb)**2)
end if
end if
END FUNCTION pythag                     
