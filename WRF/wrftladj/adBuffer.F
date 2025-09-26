C$Id: adBuffer.f 3723 2011-02-24 13:34:42Z llh $

c PISTES D'AMELIORATIONS:
c  Attention aux IF qui peuvent couter cher.
c  On pourrait aussi bufferiser les bits avec N entiers,
c   (1 bit par entier), passer tout le paquet a C et laisser
c   C faire les jongleries de bitsets.
c  On pourrait aussi optimiser en -O3 les primitives de ADFirstAidKit
c  Regarder l'assembleur (option -S (et -o toto.s))
c  Pourchasser les divisions!

c======================== BITS ==========================:
      BLOCK DATA BITS
      INTEGER adbitbuf, adbitlbuf
      INTEGER adbitibuf, adbitilbuf
      LOGICAL adbitinlbuf
      COMMON /adbitfbuf/adbitbuf,adbitlbuf,
     +       adbitibuf,adbitilbuf,adbitinlbuf
      DATA adbitbuf/0/
      DATA adbitlbuf/0/
      DATA adbitibuf/0/
      DATA adbitilbuf/-1/
      DATA adbitinlbuf/.FALSE./
      END

c [0,31] are the bit indices we can use in an INTEGER

      SUBROUTINE PUSHBIT(bit)
      LOGICAL bit
      INTEGER adbitbuf, adbitlbuf
      INTEGER adbitibuf, adbitilbuf
      LOGICAL adbitinlbuf
      COMMON /adbitfbuf/adbitbuf,adbitlbuf,
     +       adbitibuf,adbitilbuf,adbitinlbuf
c
      IF (adbitilbuf.ne.-1) THEN
         adbitilbuf = -1
         adbitinlbuf = .FALSE.
      ENDIF
      IF (bit) THEN
         adbitbuf = IBSET(adbitbuf, adbitibuf)
      ELSE
         adbitbuf = IBCLR(adbitbuf, adbitibuf)
      ENDIF
      IF (adbitibuf.ge.31) THEN
         CALL PUSHINTEGER4(adbitbuf)
         adbitbuf = 0
         adbitibuf = 0
      ELSE
         adbitibuf = adbitibuf+1
      ENDIF
      END

      LOGICAL FUNCTION LOOKBIT()
      INTEGER adbitbuf, adbitlbuf
      INTEGER adbitibuf, adbitilbuf
      LOGICAL adbitinlbuf
      COMMON /adbitfbuf/adbitbuf,adbitlbuf,
     +       adbitibuf,adbitilbuf,adbitinlbuf
c
      IF (adbitilbuf.eq.-1) THEN
         adbitilbuf=adbitibuf
         adbitlbuf = adbitbuf
      ENDIF
      IF (adbitilbuf.le.0) THEN
         CALL LOOKINTEGER4(adbitlbuf)
         adbitilbuf = 31
      ELSE
         adbitilbuf = adbitilbuf-1
      ENDIF
      LOOKBIT = BTEST(adbitlbuf, adbitilbuf)
      END

      LOGICAL FUNCTION POPBIT()
      INTEGER adbitbuf, adbitlbuf
      INTEGER adbitibuf, adbitilbuf
      LOGICAL adbitinlbuf
      COMMON /adbitfbuf/adbitbuf,adbitlbuf,
     +       adbitibuf,adbitilbuf,adbitinlbuf
c
      IF (adbitilbuf.ne.-1) THEN
         adbitilbuf = -1
         adbitinlbuf = .FALSE.
      ENDIF
      IF (adbitibuf.le.0) THEN
         CALL POPINTEGER4(adbitbuf)
         adbitibuf = 31
      ELSE
         adbitibuf = adbitibuf-1
      ENDIF
      POPBIT = BTEST(adbitbuf, adbitibuf)
      END

c====================== CONTROL =========================:

      SUBROUTINE PUSHCONTROL1B(cc)
      INTEGER cc
      CALL PUSHBIT(cc.ne.0)
      END

      SUBROUTINE POPCONTROL1B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 1
      ELSE
         cc = 0
      ENDIF
      END

      SUBROUTINE LOOKCONTROL1B(cc)
      INTEGER cc
      LOGICAL LOOKBIT
      IF (LOOKBIT()) THEN
         cc = 1
      ELSE
         cc = 0
      ENDIF
      END

      SUBROUTINE PUSHCONTROL2B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      END

      SUBROUTINE POPCONTROL2B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 2
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE LOOKCONTROL2B(cc)
      INTEGER cc
      LOGICAL LOOKBIT
      IF (LOOKBIT()) THEN
         cc = 2
      ELSE
         cc = 0
      ENDIF
      IF (LOOKBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL3B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      END

      SUBROUTINE POPCONTROL3B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 4
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE LOOKCONTROL3B(cc)
      INTEGER cc
      LOGICAL LOOKBIT
      IF (LOOKBIT()) THEN
         cc = 4
      ELSE
         cc = 0
      ENDIF
      IF (LOOKBIT()) cc = IBSET(cc,1)
      IF (LOOKBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL4B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      END

      SUBROUTINE POPCONTROL4B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 8
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE LOOKCONTROL4B(cc)
      INTEGER cc
      LOGICAL LOOKBIT
      IF (LOOKBIT()) THEN
         cc = 8
      ELSE
         cc = 0
      ENDIF
      IF (LOOKBIT()) cc = IBSET(cc,2)
      IF (LOOKBIT()) cc = IBSET(cc,1)
      IF (LOOKBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL5B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      CALL PUSHBIT(BTEST(cc,4))
      END

      SUBROUTINE POPCONTROL5B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 16
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,3)
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE LOOKCONTROL5B(cc)
      INTEGER cc
      LOGICAL LOOKBIT
      IF (LOOKBIT()) THEN
         cc = 16
      ELSE
         cc = 0
      ENDIF
      IF (LOOKBIT()) cc = IBSET(cc,3)
      IF (LOOKBIT()) cc = IBSET(cc,2)
      IF (LOOKBIT()) cc = IBSET(cc,1)
      IF (LOOKBIT()) cc = IBSET(cc,0)
      END

c======================= BOOLEANS =========================

      SUBROUTINE PUSHBOOLEAN(x)
      LOGICAL x
      CALL PUSHBIT(x)
      END

      SUBROUTINE LOOKBOOLEAN(x)
      LOGICAL x, LOOKBIT
      x = LOOKBIT()
      END

      SUBROUTINE POPBOOLEAN(x)
      LOGICAL x, POPBIT
      x = POPBIT()
      END

c===================== CHARACTERS =======================:
      BLOCK DATA CHARACTERS
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
      DATA ads1ibuf/1/
      DATA ads1ilbuf/-1/
      DATA ads1inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
c
      CALL addftraffic(1)
      IF (ads1ilbuf.ne.-1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf.ge.512) THEN
         ads1buf(512) = x
         CALL PUSHCHARACTERARRAY(ads1buf, 512)
      CALL addftraffic(-512)
         ads1ibuf = 1
      ELSE
         ads1buf(ads1ibuf) = x
         ads1ibuf = ads1ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
c
      IF (ads1ilbuf.eq.-1) THEN
         ads1ilbuf=ads1ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (ads1ilbuf.le.1) THEN
         CALL LOOKCHARACTERARRAY(ads1lbuf, 512)
         ads1inlbuf = .TRUE.
         ads1ilbuf = 512
         x = ads1lbuf(512)
      ELSE
         ads1ilbuf = ads1ilbuf-1
         if (ads1inlbuf) THEN
            x = ads1lbuf(ads1ilbuf)
         ELSE
            x = ads1buf(ads1ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
c
      IF (ads1ilbuf.ne.-1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf.le.1) THEN
         CALL POPCHARACTERARRAY(ads1buf, 512)
         ads1ibuf = 512
         x = ads1buf(512)
      ELSE
         ads1ibuf = ads1ibuf-1
         x = ads1buf(ads1ibuf)
      ENDIF
      END

c======================= INTEGER*4 =========================:
      BLOCK DATA INTEGERS4
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
      DATA adi4ibuf/1/
      DATA adi4ilbuf/-1/
      DATA adi4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
c
      CALL addftraffic(4)
      IF (adi4ilbuf.ne.-1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf.ge.512) THEN
         adi4buf(512) = x
         CALL PUSHINTEGER4ARRAY(adi4buf, 512)
         CALL addftraffic(-2048)
         adi4ibuf = 1
      ELSE
         adi4buf(adi4ibuf) = x
         adi4ibuf = adi4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
c
      IF (adi4ilbuf.eq.-1) THEN
         adi4ilbuf=adi4ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adi4ilbuf.le.1) THEN
         CALL LOOKINTEGER4ARRAY(adi4lbuf, 512)
         adi4inlbuf = .TRUE.
         adi4ilbuf = 512
         x = adi4lbuf(512)
      ELSE
         adi4ilbuf = adi4ilbuf-1
         if (adi4inlbuf) THEN
            x = adi4lbuf(adi4ilbuf)
         ELSE
            x = adi4buf(adi4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER4(x)
      INTEGER*4 x, adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
c
      IF (adi4ilbuf.ne.-1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf.le.1) THEN
         CALL POPINTEGER4ARRAY(adi4buf, 512)
         adi4ibuf = 512
         x = adi4buf(512)
      ELSE
         adi4ibuf = adi4ibuf-1
         x = adi4buf(adi4ibuf)
      ENDIF
      END

c======================= INTEGER*8 =========================
      BLOCK DATA INTEGERS8
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
      DATA adi8ibuf/1/
      DATA adi8ilbuf/-1/
      DATA adi8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
c
      CALL addftraffic(8)
      IF (adi8ilbuf.ne.-1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf.ge.512) THEN
         adi8buf(512) = x
         CALL PUSHINTEGER8ARRAY(adi8buf, 512)
      CALL addftraffic(-4096)
         adi8ibuf = 1
      ELSE
         adi8buf(adi8ibuf) = x
         adi8ibuf = adi8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
c
      IF (adi8ilbuf.eq.-1) THEN
         adi8ilbuf=adi8ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adi8ilbuf.le.1) THEN
         CALL LOOKINTEGER8ARRAY(adi8lbuf, 512)
         adi8inlbuf = .TRUE.
         adi8ilbuf = 512
         x = adi8lbuf(512)
      ELSE
         adi8ilbuf = adi8ilbuf-1
         if (adi8inlbuf) THEN
            x = adi8lbuf(adi8ilbuf)
         ELSE
            x = adi8buf(adi8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER8(x)
      INTEGER*8 x, adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
c
      IF (adi8ilbuf.ne.-1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf.le.1) THEN
         CALL POPINTEGER8ARRAY(adi8buf, 512)
         adi8ibuf = 512
         x = adi8buf(512)
      ELSE
         adi8ibuf = adi8ibuf-1
         x = adi8buf(adi8ibuf)
      ENDIF
      END

c======================= REAL*4 =========================
      BLOCK DATA REALS4
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
      DATA adr4ibuf/1/
      DATA adr4ilbuf/-1/
      DATA adr4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
c
      CALL addftraffic(4)
      IF (adr4ilbuf.ne.-1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf.ge.512) THEN
         adr4buf(512) = x
         CALL PUSHREAL4ARRAY(adr4buf, 512)
      CALL addftraffic(-2048)
         adr4ibuf = 1
      ELSE
         adr4buf(adr4ibuf) = x
         adr4ibuf = adr4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
c
      IF (adr4ilbuf.eq.-1) THEN
         adr4ilbuf=adr4ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adr4ilbuf.le.1) THEN
         CALL LOOKREAL4ARRAY(adr4lbuf, 512)
         adr4inlbuf = .TRUE.
         adr4ilbuf = 512
         x = adr4lbuf(512)
      ELSE
         adr4ilbuf = adr4ilbuf-1
         if (adr4inlbuf) THEN
            x = adr4lbuf(adr4ilbuf)
         ELSE
            x = adr4buf(adr4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL4(x)
      REAL*4 x, adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
c
      IF (adr4ilbuf.ne.-1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf.le.1) THEN
         CALL POPREAL4ARRAY(adr4buf, 512)
         adr4ibuf = 512
         x = adr4buf(512)
      ELSE
         adr4ibuf = adr4ibuf-1
         x = adr4buf(adr4ibuf)
      ENDIF
      END

c======================= REAL*8 =========================
      BLOCK DATA REALS8
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
      DATA adr8ibuf/1/
      DATA adr8ilbuf/-1/
      DATA adr8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
c
      CALL addftraffic(8)
      IF (adr8ilbuf.ne.-1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf.ge.512) THEN
         adr8buf(512) = x
         CALL PUSHREAL8ARRAY(adr8buf, 512)
      CALL addftraffic(-4096)
         adr8ibuf = 1
      ELSE
         adr8buf(adr8ibuf) = x
         adr8ibuf = adr8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
c
      IF (adr8ilbuf.eq.-1) THEN
         adr8ilbuf=adr8ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adr8ilbuf.le.1) THEN
         CALL LOOKREAL8ARRAY(adr8lbuf, 512)
         adr8inlbuf = .TRUE.
         adr8ilbuf = 512
         x = adr8lbuf(512)
      ELSE
         adr8ilbuf = adr8ilbuf-1
         if (adr8inlbuf) THEN
            x = adr8lbuf(adr8ilbuf)
         ELSE
            x = adr8buf(adr8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL8(x)
      REAL*8 x, adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
c
      IF (adr8ilbuf.ne.-1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf.le.1) THEN
         CALL POPREAL8ARRAY(adr8buf, 512)
         adr8ibuf = 512
         x = adr8buf(512)
      ELSE
         adr8ibuf = adr8ibuf-1
         x = adr8buf(adr8ibuf)
      ENDIF
      END

c======================= REAL*16 =========================
      BLOCK DATA REALS16
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
      DATA adr16ibuf/1/
      DATA adr16ilbuf/-1/
      DATA adr16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
c
      CALL addftraffic(16)
      IF (adr16ilbuf.ne.-1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf.ge.512) THEN
         adr16buf(512) = x
         CALL PUSHREAL16ARRAY(adr16buf, 512)
      CALL addftraffic(-8192)
         adr16ibuf = 1
      ELSE
         adr16buf(adr16ibuf) = x
         adr16ibuf = adr16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
c
      IF (adr16ilbuf.eq.-1) THEN
         adr16ilbuf=adr16ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adr16ilbuf.le.1) THEN
         CALL LOOKREAL16ARRAY(adr16lbuf, 512)
         adr16inlbuf = .TRUE.
         adr16ilbuf = 512
         x = adr16lbuf(512)
      ELSE
         adr16ilbuf = adr16ilbuf-1
         if (adr16inlbuf) THEN
            x = adr16lbuf(adr16ilbuf)
         ELSE
            x = adr16buf(adr16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
c
      IF (adr16ilbuf.ne.-1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf.le.1) THEN
         CALL POPREAL16ARRAY(adr16buf, 512)
         adr16ibuf = 512
         x = adr16buf(512)
      ELSE
         adr16ibuf = adr16ibuf-1
         x = adr16buf(adr16ibuf)
      ENDIF
      END

c======================= COMPLEX*8 =========================
      BLOCK DATA COMPLEXS8
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
      DATA adc8ibuf/1/
      DATA adc8ilbuf/-1/
      DATA adc8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
c
      CALL addftraffic(8)
      IF (adc8ilbuf.ne.-1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf.ge.512) THEN
         adc8buf(512) = x
         CALL PUSHCOMPLEX8ARRAY(adc8buf, 512)
      CALL addftraffic(-4096)
         adc8ibuf = 1
      ELSE
         adc8buf(adc8ibuf) = x
         adc8ibuf = adc8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
c
      IF (adc8ilbuf.eq.-1) THEN
         adc8ilbuf=adc8ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adc8ilbuf.le.1) THEN
         CALL LOOKCOMPLEX8ARRAY(adc8lbuf, 512)
         adc8inlbuf = .TRUE.
         adc8ilbuf = 512
         x = adc8lbuf(512)
      ELSE
         adc8ilbuf = adc8ilbuf-1
         if (adc8inlbuf) THEN
            x = adc8lbuf(adc8ilbuf)
         ELSE
            x = adc8buf(adc8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
c
      IF (adc8ilbuf.ne.-1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf.le.1) THEN
         CALL POPCOMPLEX8ARRAY(adc8buf, 512)
         adc8ibuf = 512
         x = adc8buf(512)
      ELSE
         adc8ibuf = adc8ibuf-1
         x = adc8buf(adc8ibuf)
      ENDIF
      END

c======================= COMPLEX*16 =========================
      BLOCK DATA COMPLEXS16
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
      DATA adc16ibuf/1/
      DATA adc16ilbuf/-1/
      DATA adc16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
c
      CALL addftraffic(16)
      IF (adc16ilbuf.ne.-1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf.ge.512) THEN
         adc16buf(512) = x
         CALL PUSHCOMPLEX16ARRAY(adc16buf, 512)
      CALL addftraffic(-8192)
         adc16ibuf = 1
      ELSE
         adc16buf(adc16ibuf) = x
         adc16ibuf = adc16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
c
      IF (adc16ilbuf.eq.-1) THEN
         adc16ilbuf=adc16ibuf
         CALL RESETADLOOKSTACK()
      ENDIF
      IF (adc16ilbuf.le.1) THEN
         CALL LOOKCOMPLEX16ARRAY(adc16lbuf, 512)
         adc16inlbuf = .TRUE.
         adc16ilbuf = 512
         x = adc16lbuf(512)
      ELSE
         adc16ilbuf = adc16ilbuf-1
         if (adc16inlbuf) THEN
            x = adc16lbuf(adc16ilbuf)
         ELSE
            x = adc16buf(adc16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
c
      IF (adc16ilbuf.ne.-1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf.le.1) THEN
         CALL POPCOMPLEX16ARRAY(adc16buf, 512)
         adc16ibuf = 512
         x = adc16buf(512)
      ELSE
         adc16ibuf = adc16ibuf-1
         x = adc16buf(adc16ibuf)
      ENDIF
      END

C=========== MEASUREMENT OF PUSH/POP TRAFFIC ==========

      BLOCK DATA MEMTRAFFIC
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
      DATA mmftraffic/0/
      DATA mmftrafficM/0/
      END

      subroutine addftraffic(n)
      INTEGER n
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
c
      mmftraffic = mmftraffic+n
      if (mmftraffic.ge.1000000) then
 100     mmftraffic = mmftraffic-1000000
         mmftrafficM = mmftrafficM+1
         if (mmftraffic.ge.1000000) then
            goto 100
         else
            goto 300
         endif
      else if (mmftraffic.lt.0) then
 200     mmftraffic = mmftraffic+1000000
         mmftrafficM = mmftrafficM-1
         if (mmftraffic.lt.0) then
            goto 200
         else
            goto 300
         endif
      endif
 300  continue
      END

      SUBROUTINE PRINTTRAFFIC()
      INTEGER*8 mmftraffic,mmftrafficM
      COMMON /mmcomtraffic/mmftraffic,mmftrafficM
      CALL printctraffic()
      CALL printftrafficinc(mmftrafficM, 1000000, mmftraffic)
c       write (6,1001) ' F Traffic: ',mmftrafficM,' Mb and ',
c      +     (((mmftraffic*1000)/1024)*1000)/1024, ' millionths'
c 1001  format(a,i6,a,i6,a)
      END

C ============ PRINTING THE SIZE OF STACKS AND BUFFERS ==========

      SUBROUTINE PRINTBUFFERTOP()
      integer*4 SMALLSTACKSIZE
      integer*4 size

      size = SMALLSTACKSIZE()
      print *,'Buffer size:',size,' bytes i.e. ',size/1024.0,' Kbytes'
      END

      FUNCTION SMALLSTACKSIZE()
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
c      LOGICAL adl4buf(512), adl4lbuf(512)
c      INTEGER adl4ibuf,adl4ilbuf
c      LOGICAL adl4inlbuf
c      COMMON /adl4fbuf/adl4buf,adl4lbuf,
c     +       adl4ibuf,adl4ilbuf,adl4inlbuf
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
c           INTEGER*16 adi16buf(512), adi16lbuf(512)
c           INTEGER adi16ibuf,adi16ilbuf
c           LOGICAL adi16inlbuf
c           COMMON /adi16fbuf/adi16buf,adi16lbuf,
c          +       adi16ibuf,adi16ilbuf,adi16inlbuf
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
c           REAL*32 x, adr32buf(512), adr32lbuf(512)
c           INTEGER adr32ibuf,adr32ilbuf
c           LOGICAL adr32inlbuf
c           COMMON /adr32fbuf/adr32buf,adr32lbuf,
c          +       adr32ibuf,adr32ilbuf,adr32inlbuf
c           COMPLEX*4 adc4buf(512), adc4lbuf(512)
c           INTEGER adc4ibuf,adc4ilbuf
c           LOGICAL adc4inlbuf
c           COMMON /adc4fbuf/adc4buf,adc4lbuf,
c          +       adc4ibuf,adc4ilbuf,adc4inlbuf
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
c           COMPLEX*32 adc32buf(512), adc32lbuf(512)
c           INTEGER adc32ibuf,adc32ilbuf
c           LOGICAL adc32inlbuf
c           COMMON /adc32fbuf/adc32buf,adc32lbuf,
c          +       adc32ibuf,adc32ilbuf,adc32inlbuf
      integer*4 smallstacksize
c     
      smallstacksize = 0
      smallstacksize = smallstacksize + (ads1ibuf-1)*1
c           smallstacksize = smallstacksize + (adl4ibuf-1)*4
      smallstacksize = smallstacksize + (adi4ibuf-1)*4
      smallstacksize = smallstacksize + (adi8ibuf-1)*8
c           smallstacksize = smallstacksize + (adi16ibuf-1)*16
      smallstacksize = smallstacksize + (adr4ibuf-1)*4
      smallstacksize = smallstacksize + (adr8ibuf-1)*8
      smallstacksize = smallstacksize + (adr16ibuf-1)*16
c           smallstacksize = smallstacksize + (adr32ibuf-1)*32
c           smallstacksize = smallstacksize + (adc4ibuf-1)*4
      smallstacksize = smallstacksize + (adc8ibuf-1)*8
      smallstacksize = smallstacksize + (adc16ibuf-1)*16
c           smallstacksize = smallstacksize + (adc32ibuf-1)*32
c
      end

C FOR INTERNAL DEBUGS ONLY:
      SUBROUTINE SHOWALLSTACKS()
      INTEGER adbitbuf, adbitlbuf
      INTEGER adbitibuf, adbitilbuf
      LOGICAL adbitinlbuf
      COMMON /adbitfbuf/adbitbuf,adbitlbuf,
     +       adbitibuf,adbitilbuf,adbitinlbuf
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,
     +       ads1ibuf,ads1ilbuf,ads1inlbuf
      INTEGER*4 adi4buf(512), adi4lbuf(512)
      INTEGER adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,
     +       adi4ibuf,adi4ilbuf,adi4inlbuf
      INTEGER*8 adi8buf(512), adi8lbuf(512)
      INTEGER adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,
     +       adi8ibuf,adi8ilbuf,adi8inlbuf
      REAL*4 adr4buf(512), adr4lbuf(512)
      INTEGER adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,
     +       adr4ibuf,adr4ilbuf,adr4inlbuf
      REAL*8 adr8buf(512), adr8lbuf(512)
      INTEGER adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,
     +       adr8ibuf,adr8ilbuf,adr8inlbuf
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,
     +       adr16ibuf,adr16ilbuf,adr16inlbuf
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,
     +       adc8ibuf,adc8ilbuf,adc8inlbuf
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,
     +       adc16ibuf,adc16ilbuf,adc16inlbuf
      INTEGER i
c
      write (6,1010) 'BIT STACK       : ',adbitbuf,'==',adbitbuf,
     +     ' (',adbitibuf,')'
1010  format(a,i20,a,z16,a,i2,a)
      write (6,1011) 'INTEGER*8 BUFFER[',adi8ibuf-1,']: ',
     +     (adi8buf(i),i=1,adi8ibuf-1)
      write (6,1011) 'INTEGER*4 BUFFER[',adi4ibuf-1,']: ',
     +     (adi4buf(i),i=1,adi4ibuf-1)
1011  format(a,i3,a,512(i40))
      write (6,1012) 'REAL*16   BUFFER:[',adr16ibuf-1,']: ',
     +     (adr16buf(i),i=1,adr16ibuf-1)
      write (6,1012) 'REAL*8    BUFFER:[',adr8ibuf-1, ']: ',
     +     (adr8buf(i),i=1,adr8ibuf-1)
      write (6,1012) 'REAL*4    BUFFER:[',adr4ibuf-1, ']: ',
     +     (adr4buf(i),i=1,adr4ibuf-1)
1012  format(a,512(e10.2))
      call showrecentcstack()
c
      END

C========================================================
C PUSH* POP* SUBROUTINES FOR OTHER DATA TYPES
C Uncomment if these types are available on your compiler
C  and they are needed by the reverse differentiated code
C Don't forget to uncomment the corresponding lines in
C  subroutine PRINTBUFFERTOP, otherwise these types'
C  contribution to buffer occupation will not be seen.
C  (not very important anyway...)

c======================= INTEGER*16 =========================
c           BLOCK DATA INTEGERS16
c           INTEGER*16 adi16buf(512), adi16lbuf(512)
c           INTEGER adi16ibuf,adi16ilbuf
c           LOGICAL adi16inlbuf
c           COMMON /adi16fbuf/adi16buf,adi16lbuf,
c          +       adi16ibuf,adi16ilbuf,adi16inlbuf
c           DATA adi16ibuf/1/
c           DATA adi16ilbuf/-1/
c           DATA adi16inlbuf/.FALSE./
c           END
c     c
c           SUBROUTINE PUSHINTEGER16(x)
c           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
c           INTEGER adi16ibuf,adi16ilbuf
c           LOGICAL adi16inlbuf
c           COMMON /adi16fbuf/adi16buf,adi16lbuf,
c          +       adi16ibuf,adi16ilbuf,adi16inlbuf
c     c
c           CALL addftraffic(16)
c           IF (adi16ilbuf.ne.-1) THEN
c              adi16ilbuf = -1
c              adi16inlbuf = .FALSE.
c           ENDIF
c           IF (adi16ibuf.ge.512) THEN
c              adi16buf(512) = x
c              CALL PUSHINTEGER16ARRAY(adi16buf, 512)
c           CALL addftraffic(-8192)
c              adi16ibuf = 1
c           ELSE
c              adi16buf(adi16ibuf) = x
c              adi16ibuf = adi16ibuf+1
c           ENDIF
c           END
c     
c           SUBROUTINE LOOKINTEGER16(x)
c           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
c           INTEGER adi16ibuf,adi16ilbuf
c           LOGICAL adi16inlbuf
c           COMMON /adi16fbuf/adi16buf,adi16lbuf,
c          +       adi16ibuf,adi16ilbuf,adi16inlbuf
c     c
c           IF (adi16ilbuf.eq.-1) THEN
c              adi16ilbuf=adi16ibuf
c              CALL RESETADLOOKSTACK()
c           ENDIF
c           IF (adi16ilbuf.le.1) THEN
c              CALL LOOKINTEGER16ARRAY(adi16lbuf, 512)
c              adi16inlbuf = .TRUE.
c              adi16ilbuf = 512
c              x = adi16lbuf(512)
c           ELSE
c              adi16ilbuf = adi16ilbuf-1
c              if (adi16inlbuf) THEN
c                 x = adi16lbuf(adi16ilbuf)
c              ELSE
c                 x = adi16buf(adi16ilbuf)
c              ENDIF
c           ENDIF
c           END
c     
c           SUBROUTINE POPINTEGER16(x)
c           INTEGER*16 x, adi16buf(512), adi16lbuf(512)
c           INTEGER adi16ibuf,adi16ilbuf
c           LOGICAL adi16inlbuf
c           COMMON /adi16fbuf/adi16buf,adi16lbuf,
c          +       adi16ibuf,adi16ilbuf,adi16inlbuf
c     c
c           IF (adi16ilbuf.ne.-1) THEN
c              adi16ilbuf = -1
c              adi16inlbuf = .FALSE.
c           ENDIF
c           IF (adi16ibuf.le.1) THEN
c              CALL POPINTEGER16ARRAY(adi16buf, 512)
c              adi16ibuf = 512
c              x = adi16buf(512)
c           ELSE
c              adi16ibuf = adi16ibuf-1
c              x = adi16buf(adi16ibuf)
c           ENDIF
c           END

c======================= REAL*32 =========================
c           BLOCK DATA REALS32
c           REAL*32 adr32buf(512), adr32lbuf(512)
c           INTEGER adr32ibuf,adr32ilbuf
c           LOGICAL adr32inlbuf
c           COMMON /adr32fbuf/adr32buf,adr32lbuf,
c          +       adr32ibuf,adr32ilbuf,adr32inlbuf
c           DATA adr32ibuf/1/
c           DATA adr32ilbuf/-1/
c           DATA adr32inlbuf/.FALSE./
c           END
c     c
c           SUBROUTINE PUSHREAL32(x)
c           REAL*32 x, adr32buf(512), adr32lbuf(512)
c           INTEGER adr32ibuf,adr32ilbuf
c           LOGICAL adr32inlbuf
c           COMMON /adr32fbuf/adr32buf,adr32lbuf,
c          +       adr32ibuf,adr32ilbuf,adr32inlbuf
c     c
c           CALL addftraffic(32)
c           IF (adr32ilbuf.ne.-1) THEN
c              adr32ilbuf = -1
c              adr32inlbuf = .FALSE.
c           ENDIF
c           IF (adr32ibuf.ge.512) THEN
c              adr32buf(512) = x
c              CALL PUSHREAL32ARRAY(adr32buf, 512)
c           CALL addftraffic(-16384)
c              adr32ibuf = 1
c           ELSE
c              adr32buf(adr32ibuf) = x
c              adr32ibuf = adr32ibuf+1
c           ENDIF
c           END
c     
c           SUBROUTINE LOOKREAL32(x)
c           REAL*32 x, adr32buf(512), adr32lbuf(512)
c           INTEGER adr32ibuf,adr32ilbuf
c           LOGICAL adr32inlbuf
c           COMMON /adr32fbuf/adr32buf,adr32lbuf,
c          +       adr32ibuf,adr32ilbuf,adr32inlbuf
c     c
c           IF (adr32ilbuf.eq.-1) THEN
c              adr32ilbuf=adr32ibuf
c              CALL RESETADLOOKSTACK()
c           ENDIF
c           IF (adr32ilbuf.le.1) THEN
c              CALL LOOKREAL32ARRAY(adr32lbuf, 512)
c              adr32inlbuf = .TRUE.
c              adr32ilbuf = 512
c              x = adr32lbuf(512)
c           ELSE
c              adr32ilbuf = adr32ilbuf-1
c              if (adr32inlbuf) THEN
c                 x = adr32lbuf(adr32ilbuf)
c              ELSE
c                 x = adr32buf(adr32ilbuf)
c              ENDIF
c           ENDIF
c           END
c     
c           SUBROUTINE POPREAL32(x)
c           REAL*32 x, adr32buf(512), adr32lbuf(512)
c           INTEGER adr32ibuf,adr32ilbuf
c           LOGICAL adr32inlbuf
c           COMMON /adr32fbuf/adr32buf,adr32lbuf,
c          +       adr32ibuf,adr32ilbuf,adr32inlbuf
c     c
c           IF (adr32ilbuf.ne.-1) THEN
c              adr32ilbuf = -1
c              adr32inlbuf = .FALSE.
c           ENDIF
c           IF (adr32ibuf.le.1) THEN
c              CALL POPREAL32ARRAY(adr32buf, 512)
c              adr32ibuf = 512
c              x = adr32buf(512)
c           ELSE
c              adr32ibuf = adr32ibuf-1
c              x = adr32buf(adr32ibuf)
c           ENDIF
c           END

c======================= COMPLEX*4 =========================
c           BLOCK DATA COMPLEXS4
c           COMPLEX*4 adc4buf(512), adc4lbuf(512)
c           INTEGER adc4ibuf,adc4ilbuf
c           LOGICAL adc4inlbuf
c           COMMON /adc4fbuf/adc4buf,adc4lbuf,
c          +       adc4ibuf,adc4ilbuf,adc4inlbuf
c           DATA adc4ibuf/1/
c           DATA adc4ilbuf/-1/
c           DATA adc4inlbuf/.FALSE./
c           END
c     c
c           SUBROUTINE PUSHCOMPLEX4(x)
c           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
c           INTEGER adc4ibuf,adc4ilbuf
c           LOGICAL adc4inlbuf
c           COMMON /adc4fbuf/adc4buf,adc4lbuf,
c          +       adc4ibuf,adc4ilbuf,adc4inlbuf
c     c
c           CALL addftraffic(4)
c           IF (adc4ilbuf.ne.-1) THEN
c              adc4ilbuf = -1
c              adc4inlbuf = .FALSE.
c           ENDIF
c           IF (adc4ibuf.ge.512) THEN
c              adc4buf(512) = x
c              CALL PUSHCOMPLEX4ARRAY(adc4buf, 512)
c           CALL addftraffic(-2048)
c              adc4ibuf = 1
c           ELSE
c              adc4buf(adc4ibuf) = x
c              adc4ibuf = adc4ibuf+1
c           ENDIF
c           END
c     
c           SUBROUTINE LOOKCOMPLEX4(x)
c           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
c           INTEGER adc4ibuf,adc4ilbuf
c           LOGICAL adc4inlbuf
c           COMMON /adc4fbuf/adc4buf,adc4lbuf,
c          +       adc4ibuf,adc4ilbuf,adc4inlbuf
c     c
c           IF (adc4ilbuf.eq.-1) THEN
c              adc4ilbuf=adc4ibuf
c              CALL RESETADLOOKSTACK()
c           ENDIF
c           IF (adc4ilbuf.le.1) THEN
c              CALL LOOKCOMPLEX4ARRAY(adc4lbuf, 512)
c              adc4inlbuf = .TRUE.
c              adc4ilbuf = 512
c              x = adc4lbuf(512)
c           ELSE
c              adc4ilbuf = adc4ilbuf-1
c              if (adc4inlbuf) THEN
c                 x = adc4lbuf(adc4ilbuf)
c              ELSE
c                 x = adc4buf(adc4ilbuf)
c              ENDIF
c           ENDIF
c           END
c     
c           SUBROUTINE POPCOMPLEX4(x)
c           COMPLEX*4 x, adc4buf(512), adc4lbuf(512)
c           INTEGER adc4ibuf,adc4ilbuf
c           LOGICAL adc4inlbuf
c           COMMON /adc4fbuf/adc4buf,adc4lbuf,
c          +       adc4ibuf,adc4ilbuf,adc4inlbuf
c     c
c           IF (adc4ilbuf.ne.-1) THEN
c              adc4ilbuf = -1
c              adc4inlbuf = .FALSE.
c           ENDIF
c           IF (adc4ibuf.le.1) THEN
c              CALL POPCOMPLEX4ARRAY(adc4buf, 512)
c              adc4ibuf = 512
c              x = adc4buf(512)
c           ELSE
c              adc4ibuf = adc4ibuf-1
c              x = adc4buf(adc4ibuf)
c           ENDIF
c           END

c======================= COMPLEX*32 =========================
c           BLOCK DATA COMPLEXS32
c           COMPLEX*32 adc32buf(512), adc32lbuf(512)
c           INTEGER adc32ibuf,adc32ilbuf
c           LOGICAL adc32inlbuf
c           COMMON /adc32fbuf/adc32buf,adc32lbuf,
c          +       adc32ibuf,adc32ilbuf,adc32inlbuf
c           DATA adc32ibuf/1/
c           DATA adc32ilbuf/-1/
c           DATA adc32inlbuf/.FALSE./
c           END
c     c
c           SUBROUTINE PUSHCOMPLEX32(x)
c           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
c           INTEGER adc32ibuf,adc32ilbuf
c           LOGICAL adc32inlbuf
c           COMMON /adc32fbuf/adc32buf,adc32lbuf,
c          +       adc32ibuf,adc32ilbuf,adc32inlbuf
c     c
c           CALL addftraffic(32)
c           IF (adc32ilbuf.ne.-1) THEN
c              adc32ilbuf = -1
c              adc32inlbuf = .FALSE.
c           ENDIF
c           IF (adc32ibuf.ge.512) THEN
c              adc32buf(512) = x
c              CALL PUSHCOMPLEX32ARRAY(adc32buf, 512)
c           CALL addftraffic(-16384)
c              adc32ibuf = 1
c           ELSE
c              adc32buf(adc32ibuf) = x
c              adc32ibuf = adc32ibuf+1
c           ENDIF
c           END
c     
c           SUBROUTINE LOOKCOMPLEX32(x)
c           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
c           INTEGER adc32ibuf,adc32ilbuf
c           LOGICAL adc32inlbuf
c           COMMON /adc32fbuf/adc32buf,adc32lbuf,
c          +       adc32ibuf,adc32ilbuf,adc32inlbuf
c     c
c           IF (adc32ilbuf.eq.-1) THEN
c              adc32ilbuf=adc32ibuf
c              CALL RESETADLOOKSTACK()
c           ENDIF
c           IF (adc32ilbuf.le.1) THEN
c              CALL LOOKCOMPLEX32ARRAY(adc32lbuf, 512)
c              adc32inlbuf = .TRUE.
c              adc32ilbuf = 512
c              x = adc32lbuf(512)
c           ELSE
c              adc32ilbuf = adc32ilbuf-1
c              if (adc32inlbuf) THEN
c                 x = adc32lbuf(adc32ilbuf)
c              ELSE
c                 x = adc32buf(adc32ilbuf)
c              ENDIF
c           ENDIF
c           END
c     
c           SUBROUTINE POPCOMPLEX32(x)
c           COMPLEX*32 x, adc32buf(512), adc32lbuf(512)
c           INTEGER adc32ibuf,adc32ilbuf
c           LOGICAL adc32inlbuf
c           COMMON /adc32fbuf/adc32buf,adc32lbuf,
c          +       adc32ibuf,adc32ilbuf,adc32inlbuf
c     c
c           IF (adc32ilbuf.ne.-1) THEN
c              adc32ilbuf = -1
c              adc32inlbuf = .FALSE.
c           ENDIF
c           IF (adc32ibuf.le.1) THEN
c              CALL POPCOMPLEX32ARRAY(adc32buf, 512)
c              adc32ibuf = 512
c              x = adc32buf(512)
c           ELSE
c              adc32ibuf = adc32ibuf-1
c              x = adc32buf(adc32ibuf)
c           ENDIF
c           END

C========================================================
C        HOW TO CREATE PUSH* POP* SUBROUTINES
C              YET FOR OTHER DATA TYPES
C ** Duplicate the commented program lines below
c ** In the duplicated subroutines, replace:
c            TTTT by the basic name of the type
c            z9   by the initial and size of the type
c                (integer:i real:r complex:c boolean:b character:s)
c            9    by the size of the type
c ** Uncomment the duplicated subroutines
C ** Don't forget to insert the corresponding lines in
C  subroutine PRINTBUFFERTOP, otherwise these types'
C  contribution to buffer occupation will not be seen.
C  (not very important anyway...)

c======================= TTTT*9 =========================
c           BLOCK DATA TTTTS9
c           TTTT*9 adz9buf(512), adz9lbuf(512)
c           INTEGER adz9ibuf,adz9ilbuf
c           LOGICAL adz9inlbuf
c           COMMON /adz9fbuf/adz9buf,adz9lbuf,
c          +       adz9ibuf,adz9ilbuf,adz9inlbuf
c           DATA adz9ibuf/1/
c           DATA adz9ilbuf/-1/
c           DATA adz9inlbuf/.FALSE./
c           END
c     c
c           SUBROUTINE PUSHTTTT9(x)
c           TTTT*9 x, adz9buf(512), adz9lbuf(512)
c           INTEGER adz9ibuf,adz9ilbuf
c           LOGICAL adz9inlbuf
c           COMMON /adz9fbuf/adz9buf,adz9lbuf,
c          +       adz9ibuf,adz9ilbuf,adz9inlbuf
c     c
c           CALL addftraffic(9)
c           IF (adz9ilbuf.ne.-1) THEN
c              adz9ilbuf = -1
c              adz9inlbuf = .FALSE.
c           ENDIF
c           IF (adz9ibuf.ge.512) THEN
c              adz9buf(512) = x
c              CALL PUSHTTTT9ARRAY(adz9buf, 512)
c           CALL addftraffic(-9*512)
c              adz9ibuf = 1
c           ELSE
c              adz9buf(adz9ibuf) = x
c              adz9ibuf = adz9ibuf+1
c           ENDIF
c           END
c     
c           SUBROUTINE LOOKTTTT9(x)
c           TTTT*9 x, adz9buf(512), adz9lbuf(512)
c           INTEGER adz9ibuf,adz9ilbuf
c           LOGICAL adz9inlbuf
c           COMMON /adz9fbuf/adz9buf,adz9lbuf,
c          +       adz9ibuf,adz9ilbuf,adz9inlbuf
c     c
c           IF (adz9ilbuf.eq.-1) THEN
c              adz9ilbuf=adz9ibuf
c              CALL RESETADLOOKSTACK()
c           ENDIF
c           IF (adz9ilbuf.le.1) THEN
c              CALL LOOKTTTT9ARRAY(adz9lbuf, 512)
c              adz9inlbuf = .TRUE.
c              adz9ilbuf = 512
c              x = adz9lbuf(512)
c           ELSE
c              adz9ilbuf = adz9ilbuf-1
c              if (adz9inlbuf) THEN
c                 x = adz9lbuf(adz9ilbuf)
c              ELSE
c                 x = adz9buf(adz9ilbuf)
c              ENDIF
c           ENDIF
c           END
c     
c           SUBROUTINE POPTTTT9(x)
c           TTTT*9 x, adz9buf(512), adz9lbuf(512)
c           INTEGER adz9ibuf,adz9ilbuf
c           LOGICAL adz9inlbuf
c           COMMON /adz9fbuf/adz9buf,adz9lbuf,
c          +       adz9ibuf,adz9ilbuf,adz9inlbuf
c     c
c           IF (adz9ilbuf.ne.-1) THEN
c              adz9ilbuf = -1
c              adz9inlbuf = .FALSE.
c           ENDIF
c           IF (adz9ibuf.le.1) THEN
c              CALL POPTTTT9ARRAY(adz9buf, 512)
c              adz9ibuf = 512
c              x = adz9buf(512)
c           ELSE
c              adz9ibuf = adz9ibuf-1
c              x = adz9buf(adz9ibuf)
c           ENDIF
c           END
