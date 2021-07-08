C ************************ integer*4 ************************
      BLOCK DATA INTEGERS4
      INTEGER*4 adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      DATA adi4ibuf/1/
      END

      SUBROUTINE PUSHINTEGER4(x)
      INTEGER*4 x, adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      CALL addftraffic(4)
      adi4buf(adi4ibuf) = x
      IF (adi4ibuf.eq.512) THEN
         CALL PUSHINTEGER4ARRAY(adi4buf, 512)
         CALL addftraffic(-512*4)
         adi4ibuf = 1
      ELSE
         adi4ibuf = adi4ibuf+1
      ENDIF
      END

      SUBROUTINE POPINTEGER4(x)
      INTEGER*4 x, adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      IF (adi4ibuf.le.1) THEN
         CALL POPINTEGER4ARRAY(adi4buf, 512)
         adi4ibuf = 512
      ELSE
         adi4ibuf = adi4ibuf-1         
      ENDIF
      x = adi4buf(adi4ibuf)
      END

C ************************ integer*8 ************************
      BLOCK DATA INTEGERS8
      INTEGER*8 adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      DATA adi8ibuf/1/
      END

      SUBROUTINE PUSHINTEGER8(x)
      INTEGER*8 x, adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      CALL addftraffic(8)
      adi8buf(adi8ibuf) = x
      IF (adi8ibuf.eq.512) THEN
         CALL PUSHINTEGER8ARRAY(adi8buf, 512)
         CALL addftraffic(-8*512)
         adi8ibuf = 1
      ELSE
         adi8ibuf = adi8ibuf+1
      ENDIF
      END

      SUBROUTINE POPINTEGER8(x)
      INTEGER*8 x, adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      IF (adi8ibuf.le.1) THEN
         CALL POPINTEGER8ARRAY(adi8buf, 512)
         adi8ibuf = 512
      ELSE
         adi8ibuf = adi8ibuf-1         
      ENDIF
      x = adi8buf(adi8ibuf)
      END

C ************************ real*4 ************************
      BLOCK DATA REALS4
      REAL*4 adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      DATA adr4ibuf/1/
      END

      SUBROUTINE PUSHREAL4(x)
      REAL*4 x, adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      CALL addftraffic(4)
      adr4buf(adr4ibuf) = x
      IF (adr4ibuf.eq.512) THEN
         CALL PUSHREAL4ARRAY(adr4buf, 512)
         CALL addftraffic(-4*512)
         adr4ibuf = 1
      ELSE
         adr4ibuf = adr4ibuf+1
      ENDIF
      END

      SUBROUTINE POPREAL4(x)
      REAL*4 x, adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      IF (adr4ibuf.le.1) THEN
         CALL POPREAL4ARRAY(adr4buf, 512)
         adr4ibuf = 512
      ELSE
         adr4ibuf = adr4ibuf-1         
      ENDIF
      x = adr4buf(adr4ibuf)
      END

C ************************ real*8 ************************
      BLOCK DATA REALS8
      REAL*8 adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      DATA adr8ibuf/1/
      END

      SUBROUTINE PUSHREAL8(x)
      REAL*8 x, adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      CALL addftraffic(8)
      adr8buf(adr8ibuf) = x
      IF (adr8ibuf.eq.512) THEN
         CALL PUSHREAL8ARRAY(adr8buf, 512)
         CALL addftraffic(-8*512)
         adr8ibuf = 1
      ELSE
         adr8ibuf = adr8ibuf+1
      ENDIF
      END

      SUBROUTINE POPREAL8(x)
      REAL*8 x, adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      IF (adr8ibuf.le.1) THEN
         CALL POPREAL8ARRAY(adr8buf, 512)
         adr8ibuf = 512
      ELSE
         adr8ibuf = adr8ibuf-1         
      ENDIF
      x = adr8buf(adr8ibuf)
      END

C ************************ complex*8 ************************
      BLOCK DATA COMPLEXS8
      COMPLEX*8 adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      DATA adc8ibuf/1/
      END

      SUBROUTINE PUSHCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      CALL addftraffic(8)
      adc8buf(adc8ibuf) = x
      IF (adc8ibuf.eq.512) THEN
         CALL PUSHCOMPLEX8ARRAY(adc8buf, 512)
         CALL addftraffic(-8*512)
         adc8ibuf = 1
      ELSE
         adc8ibuf = adc8ibuf+1
      ENDIF
      END

      SUBROUTINE POPCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      IF (adc8ibuf.le.1) THEN
         CALL POPCOMPLEX8ARRAY(adc8buf, 512)
         adc8ibuf = 512
      ELSE
         adc8ibuf = adc8ibuf-1         
      ENDIF
      x = adc8buf(adc8ibuf)
      END

C ************************ complex*16 ************************
      BLOCK DATA COMPLEXS16
      COMPLEX*16 adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      DATA adc16ibuf/1/
      END

      SUBROUTINE PUSHCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      CALL addftraffic(16)
      adc16buf(adc16ibuf) = x
      IF (adc16ibuf.eq.512) THEN
         CALL PUSHCOMPLEX16ARRAY(adc16buf, 512)
         CALL addftraffic(-16*512)
         adc16ibuf = 1
      ELSE
         adc16ibuf = adc16ibuf+1
      ENDIF
      END

      SUBROUTINE POPCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      IF (adc16ibuf.le.1) THEN
         CALL POPCOMPLEX16ARRAY(adc16buf, 512)
         adc16ibuf = 512
      ELSE
         adc16ibuf = adc16ibuf-1         
      ENDIF
      x = adc16buf(adc16ibuf)
      END

C ************************ character ************************
      BLOCK DATA CHARACTERS
      CHARACTER ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      DATA ads1ibuf/1/
      END

      SUBROUTINE PUSHCHARACTER(x)
      CHARACTER x, ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      CALL addftraffic(1)
      ads1buf(ads1ibuf) = x
      IF (ads1ibuf.eq.512) THEN
         CALL PUSHNARRAY(ads1buf, 512, 1)
         CALL addftraffic(-512)
         ads1ibuf = 1
      ELSE
         ads1ibuf = ads1ibuf+1
      ENDIF
      END

      SUBROUTINE POPCHARACTER(x)
      CHARACTER x, ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      IF (ads1ibuf.le.1) THEN
         CALL POPNARRAY(ads1buf, 512, 1)
         ads1ibuf = 512
      ELSE
         ads1ibuf = ads1ibuf-1         
      ENDIF
      x = ads1buf(ads1ibuf)
      END

C ******************* bit (hidden primitives) ***************
      BLOCK DATA BITS
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
      DATA adbitbuf/0/
      DATA adbitibuf/0/
      END

      SUBROUTINE PUSHBIT(bit)
      LOGICAL bit
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
      IF (bit) THEN
         adbitbuf = IBSET(adbitbuf, adbitibuf)
      ELSE
         adbitbuf = IBCLR(adbitbuf, adbitibuf)
      ENDIF
      IF (adbitibuf.ge.31) THEN
         CALL PUSHNARRAY(adbitbuf, 4, 1)
         adbitbuf = 0
         adbitibuf = 0
      ELSE
         adbitibuf = adbitibuf+1
      ENDIF
      END

      LOGICAL FUNCTION POPBIT()
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
      IF (adbitibuf.le.0) THEN
         CALL POPNARRAY(adbitbuf, 4, 1)
         adbitibuf = 31
      ELSE
         adbitibuf = adbitibuf-1
      ENDIF
      POPBIT = BTEST(adbitbuf, adbitibuf)
      END

C *************************** boolean *************************
      SUBROUTINE PUSHBOOLEAN(x)
      LOGICAL x
      CALL PUSHBIT(x)
      END

      SUBROUTINE POPBOOLEAN(x)
      LOGICAL x, POPBIT
      x = POPBIT()
      END

C ************************* control ***********************

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

      SUBROUTINE PUSHCONTROL6B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      CALL PUSHBIT(BTEST(cc,4))
      CALL PUSHBIT(BTEST(cc,5))
      END

      SUBROUTINE POPCONTROL6B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 32
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,4)
      IF (POPBIT()) cc = IBSET(cc,3)
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL7B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      CALL PUSHBIT(BTEST(cc,4))
      CALL PUSHBIT(BTEST(cc,5))
      CALL PUSHBIT(BTEST(cc,6))
      END

      SUBROUTINE POPCONTROL7B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 64
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,5)
      IF (POPBIT()) cc = IBSET(cc,4)
      IF (POPBIT()) cc = IBSET(cc,3)
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL8B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      CALL PUSHBIT(BTEST(cc,4))
      CALL PUSHBIT(BTEST(cc,5))
      CALL PUSHBIT(BTEST(cc,6))
      CALL PUSHBIT(BTEST(cc,7))
      END

      SUBROUTINE POPCONTROL8B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 128
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,6)
      IF (POPBIT()) cc = IBSET(cc,5)
      IF (POPBIT()) cc = IBSET(cc,4)
      IF (POPBIT()) cc = IBSET(cc,3)
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

      SUBROUTINE PUSHCONTROL9B(cc)
      INTEGER cc
      CALL PUSHBIT(BTEST(cc,0))
      CALL PUSHBIT(BTEST(cc,1))
      CALL PUSHBIT(BTEST(cc,2))
      CALL PUSHBIT(BTEST(cc,3))
      CALL PUSHBIT(BTEST(cc,4))
      CALL PUSHBIT(BTEST(cc,5))
      CALL PUSHBIT(BTEST(cc,6))
      CALL PUSHBIT(BTEST(cc,7))
      CALL PUSHBIT(BTEST(cc,8))
      END

      SUBROUTINE POPCONTROL9B(cc)
      INTEGER cc
      LOGICAL POPBIT
      IF (POPBIT()) THEN
         cc = 256
      ELSE
         cc = 0
      ENDIF
      IF (POPBIT()) cc = IBSET(cc,7)
      IF (POPBIT()) cc = IBSET(cc,6)
      IF (POPBIT()) cc = IBSET(cc,5)
      IF (POPBIT()) cc = IBSET(cc,4)
      IF (POPBIT()) cc = IBSET(cc,3)
      IF (POPBIT()) cc = IBSET(cc,2)
      IF (POPBIT()) cc = IBSET(cc,1)
      IF (POPBIT()) cc = IBSET(cc,0)
      END

C ************************* pointer ************************
c Don't know how to write a PUSH/POPPOINTER() in Fortran
c Maybe one should always call the C version instead...

C *********************************************************
C         HOW TO CREATE PUSH* POP* SUBROUTINES
C              YET FOR OTHER DATA TYPES
C  Duplicate and uncomment the commented code below.
C  In the duplicated and uncommented code, replace:
C    tttt -> BASIC TAPENADE TYPE NAME
C      (in character, boolean, integer, real, complex, pointer,...)
C    z7   -> LETTERSIZE FOR TYPE
C      (LETTER in s, b, i, r, c, p, ...) (SIZE is type size in bytes)
C    7    -> TYPE SIZE IN BYTES
C *********************************************************/

C ************************* TTTT*7 ************************
c      BLOCK DATA TTTTS7
c      TTTT*7 adz7buf(512)
c      INTEGER adz7ibuf
c      COMMON /adz7fbuf/adz7buf,adz7ibuf
c      DATA adz7ibuf/1/
c      END
c
c      SUBROUTINE PUSHTTTT7(x)
c      TTTT*7 x, adz7buf(512)
c      INTEGER adz7ibuf
c      COMMON /adz7fbuf/adz7buf,adz7ibuf
c      CALL addftraffic(7)
c      adz7buf(adz7ibuf) = x
c      IF (adz7ibuf.eq.512) THEN
c         CALL PUSHTTTT7ARRAY(adz7buf, 512)
c         CALL addftraffic(-7*512)
c         adz7ibuf = 1
c      ELSE
c         adz7ibuf = adz7ibuf+1
c      ENDIF
c      END
c
c      SUBROUTINE POPTTTT7(x)
c      TTTT*7 x, adz7buf(512)
c      INTEGER adz7ibuf
c      COMMON /adz7fbuf/adz7buf,adz7ibuf
c      IF (adz7ibuf.le.1) THEN
c         CALL POPTTTT7ARRAY(adz7buf, 512)
c         adz7ibuf = 512
c      ELSE
c         adz7ibuf = adz7ibuf-1         
c      ENDIF
c      x = adz7buf(adz7ibuf)
c      END

C *************** REPEATED ACCESS MECHANISM *********************
C     5 nested repeat levels should be more than enough!!
      BLOCK DATA BUFFERREPEAT
      INTEGER nbbufrepeat
      INTEGER indexi4repeats(5)
      INTEGER indexi8repeats(5)
      INTEGER indexr4repeats(5)
      INTEGER indexr8repeats(5)
      INTEGER indexc8repeats(5)
      INTEGER indexc16repeats(5)
      INTEGER indexs1repeats(5)
      INTEGER indexbitrepeats(5)
      INTEGER indexptrrepeats(5)
      COMMON /allbufferrepeats/indexi4repeats, indexi8repeats,
     +     indexr4repeats, indexr8repeats, indexc8repeats,
     +     indexc16repeats, indexs1repeats, indexbitrepeats,
     +     indexptrrepeats, nbbufrepeat
      DATA nbbufrepeat/0/
      END

      SUBROUTINE ADSTACK_STARTREPEAT()
      INTEGER nbbufrepeat
      INTEGER indexi4repeats(5)
      INTEGER indexi8repeats(5)
      INTEGER indexr4repeats(5)
      INTEGER indexr8repeats(5)
      INTEGER indexc8repeats(5)
      INTEGER indexc16repeats(5)
      INTEGER indexs1repeats(5)
      INTEGER indexbitrepeats(5)
      INTEGER indexptrrepeats(5)
      COMMON /allbufferrepeats/indexi4repeats, indexi8repeats,
     +     indexr4repeats, indexr8repeats, indexc8repeats,
     +     indexc16repeats, indexs1repeats, indexbitrepeats,
     +     indexptrrepeats, nbbufrepeat
      INTEGER*4 adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      INTEGER*8 adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      REAL*4 adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      REAL*8 adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      COMPLEX*8 adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      COMPLEX*16 adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      CHARACTER ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
c Create a new "buffers" repeat level:
      nbbufrepeat = nbbufrepeat+1
c Also create a new repeat level for the main stack:
      CALL STARTSTACKREPEAT1()
c Push all local buffers on the main stack.
c 3rd arg is 0 to deactivate the check for stack read-only zone:
      if (adi4ibuf.gt.1) CALL PUSHNARRAY(adi4buf, 4*(adi4ibuf-1), 0)
      if (adi8ibuf.gt.1) CALL PUSHNARRAY(adi8buf, 8*(adi8ibuf-1), 0)
      if (adr4ibuf.gt.1) CALL PUSHNARRAY(adr4buf, 4*(adr4ibuf-1), 0)
      if (adr8ibuf.gt.1) CALL PUSHNARRAY(adr8buf, 8*(adr8ibuf-1), 0)
      if (adc8ibuf.gt.1) CALL PUSHNARRAY(adc8buf, 8*(adc8ibuf-1), 0)
      if (adc16ibuf.gt.1)CALL PUSHNARRAY(adc16buf,16*(adc16ibuf-1),0)
      if (ads1ibuf.gt.1) CALL PUSHNARRAY(ads1buf, ads1ibuf-1, 0)
      CALL PUSHNARRAY(adbitbuf, 4, 0)
c      if (adptribuf.gt.1) CALL PUSHNARRAY(adptrbuf, 8*(adptribuf-1), 0)
      indexi4repeats(nbbufrepeat) = adi4ibuf
      indexi8repeats(nbbufrepeat) = adi8ibuf
      indexr4repeats(nbbufrepeat) = adr4ibuf
      indexr8repeats(nbbufrepeat) = adr8ibuf
      indexc8repeats(nbbufrepeat) = adc8ibuf
      indexc16repeats(nbbufrepeat) = adc16ibuf
      indexs1repeats(nbbufrepeat) = ads1ibuf
      indexbitrepeats(nbbufrepeat) = adbitibuf
c      indexptrrepeats(nbbufrepeat) = adptribuf
c Store current location as repeat location of new repeat level.
c Note that this repeat location protects below as read-only.
c Make the new repeat level the current repeat level  for the main stack:
      CALL STARTSTACKREPEAT2()
      END

c Note: ADSTACK_RESETREPEAT() forces exit from any internal checkpointed sequence,
c   i.e. all nested push'es are forced popped.
      SUBROUTINE ADSTACK_RESETREPEAT()
      INTEGER nbbufrepeat
      INTEGER indexi4repeats(5)
      INTEGER indexi8repeats(5)
      INTEGER indexr4repeats(5)
      INTEGER indexr8repeats(5)
      INTEGER indexc8repeats(5)
      INTEGER indexc16repeats(5)
      INTEGER indexs1repeats(5)
      INTEGER indexbitrepeats(5)
      INTEGER indexptrrepeats(5)
      COMMON /allbufferrepeats/indexi4repeats, indexi8repeats,
     +     indexr4repeats, indexr8repeats, indexc8repeats,
     +     indexc16repeats, indexs1repeats, indexbitrepeats,
     +     indexptrrepeats, nbbufrepeat
      INTEGER*4 adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      INTEGER*8 adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      REAL*4 adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      REAL*8 adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      COMPLEX*8 adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      COMPLEX*16 adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      CHARACTER ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
c First stage of reset repeat for the main stack:
      CALL RESETSTACKREPEAT1()
c Restore all local buffers:
      adi4ibuf  = indexi4repeats(nbbufrepeat)
      adi8ibuf  = indexi8repeats(nbbufrepeat)
      adr4ibuf  = indexr4repeats(nbbufrepeat)
      adr8ibuf  = indexr8repeats(nbbufrepeat)
      adc8ibuf  = indexc8repeats(nbbufrepeat)
      adc16ibuf = indexc16repeats(nbbufrepeat)
      ads1ibuf  = indexs1repeats(nbbufrepeat)
      adbitibuf = indexbitrepeats(nbbufrepeat)
c      adptribuf = indexptrrepeats(nbbufrepeat)
c      if (adptribuf.gt.1) CALL POPNARRAY(adptrbuf, 8*(adptribuf-1),0)
      CALL POPNARRAY(adbitbuf, 4, 0)
c 3rd arg is 0 to deactivate the check for stack read-only zone:
      if (ads1ibuf.gt.1) CALL POPNARRAY(ads1buf, ads1ibuf-1, 0)
      if (adc16ibuf.gt.1)CALL POPNARRAY(adc16buf,16*(adc16ibuf-1),0)
      if (adc8ibuf.gt.1) CALL POPNARRAY(adc8buf, 8*(adc8ibuf-1), 0)
      if (adr8ibuf.gt.1) CALL POPNARRAY(adr8buf, 8*(adr8ibuf-1), 0)
      if (adr4ibuf.gt.1) CALL POPNARRAY(adr4buf, 4*(adr4ibuf-1), 0)
      if (adi8ibuf.gt.1) CALL POPNARRAY(adi8buf, 8*(adi8ibuf-1), 0)
      if (adi4ibuf.gt.1) CALL POPNARRAY(adi4buf, 4*(adi4ibuf-1), 0)
c Second stage of reset repeat for the main stack:
      CALL RESETSTACKREPEAT2()
      END

c Note: ADSTACK_ENDREPEAT() forces exit from any internal checkpointed sequence,
c   i.e. all nested push'es are forced popped.
      SUBROUTINE ADSTACK_ENDREPEAT()
      INTEGER nbbufrepeat
      INTEGER indexi4repeats(5)
      INTEGER indexi8repeats(5)
      INTEGER indexr4repeats(5)
      INTEGER indexr8repeats(5)
      INTEGER indexc8repeats(5)
      INTEGER indexc16repeats(5)
      INTEGER indexs1repeats(5)
      INTEGER indexbitrepeats(5)
      INTEGER indexptrrepeats(5)
      COMMON /allbufferrepeats/indexi4repeats, indexi8repeats,
     +     indexr4repeats, indexr8repeats, indexc8repeats,
     +     indexc16repeats, indexs1repeats, indexbitrepeats,
     +     indexptrrepeats, nbbufrepeat
c End repeat for the main stack:
      CALL ENDSTACKREPEAT() ;
c Remove top repeat level:
      nbbufrepeat = nbbufrepeat-1
      END

      SUBROUTINE SHOWI4BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      INTEGER*4 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      I4:',512(a2,i10.1),' REPEATS:',5i3)
      END

      SUBROUTINE SHOWI8BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      INTEGER*8 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      I8:',512(a2,i20.1),' REPEATS:',5i3)
      END

      SUBROUTINE SHOWR4BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      REAL*4 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      R4:',512(a2,e12.5),' REPEATS:',5i3)
      END

      SUBROUTINE SHOWR8BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      REAL*8 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      R8:',512(a2,d12.5),' REPEATS:',5i3)
      END

      SUBROUTINE SHOWC8BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      COMPLEX*8 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      C8:',512(a2,'(',e12.5,' i',e12.5,')'),
     +     ' REPEATS:',5i3)
      END

      SUBROUTINE SHOWC16BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      COMPLEX*16 xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      C16:',512(a2,'(',d12.5,' i',d12.5,')'),
     +     ' REPEATS:',5i3)
      END

      SUBROUTINE SHOWS1BUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      CHARACTER xbuf(512)
      INTEGER xibuf, xrepeats(5), nbbufrepeat
      CHARACTER(len=3) seps(513)
      INTEGER i
      DO i=1,513
         seps(i) = ''
      ENDDO
      seps(xibuf) = ' |'
      WRITE (6,991) (seps(i),xbuf(i),i=1,512),
     +     (xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      STR:',512(a2,a1),
     +     ' REPEATS:',5i3)
      END

      SUBROUTINE SHOWBITBUFFERANDREPEATS(xbuf,xibuf,
     +     xrepeats,nbbufrepeat)
      INTEGER*4 xbuf
      INTEGER xibuf, xrepeats(5), nbbufrepeat,i
      WRITE (6,991) xibuf,xbuf,(xrepeats(i),i=1,nbbufrepeat)
 991  FORMAT('      BITS:',i2,' in ',z8,'     REPEATS:',5i3)
      END

      SUBROUTINE SHOWSTACKANDBUFFERS(locationName)
      CHARACTER(*) locationName
      INTEGER nbbufrepeat
      INTEGER indexi4repeats(5)
      INTEGER indexi8repeats(5)
      INTEGER indexr4repeats(5)
      INTEGER indexr8repeats(5)
      INTEGER indexc8repeats(5)
      INTEGER indexc16repeats(5)
      INTEGER indexs1repeats(5)
      INTEGER indexbitrepeats(5)
      INTEGER indexptrrepeats(5)
      COMMON /allbufferrepeats/indexi4repeats, indexi8repeats,
     +     indexr4repeats, indexr8repeats, indexc8repeats,
     +     indexc16repeats, indexs1repeats, indexbitrepeats,
     +     indexptrrepeats, nbbufrepeat
      INTEGER*4 adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      INTEGER*8 adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      REAL*4 adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      REAL*8 adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      COMPLEX*8 adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      COMPLEX*16 adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      CHARACTER ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
      print *,locationName
      CALL SHOWSTACK()
      CALL SHOWI4BUFFERANDREPEATS(adi4buf,adi4ibuf,
     +     indexi4repeats,nbbufrepeat)
      CALL SHOWI8BUFFERANDREPEATS(adi8buf,adi8ibuf,
     +     indexi8repeats,nbbufrepeat)
      CALL SHOWR4BUFFERANDREPEATS(adr4buf,adr4ibuf,
     +     indexr4repeats,nbbufrepeat)
      CALL SHOWR8BUFFERANDREPEATS(adr8buf,adr8ibuf,
     +     indexr8repeats,nbbufrepeat)
      CALL SHOWC8BUFFERANDREPEATS(adc8buf,adc8ibuf,
     +     indexc8repeats,nbbufrepeat)
      CALL SHOWC16BUFFERANDREPEATS(adc16buf,adc16ibuf,
     +     indexc16repeats,nbbufrepeat)
      CALL SHOWS1BUFFERANDREPEATS(ads1buf,ads1ibuf,
     +     indexs1repeats,nbbufrepeat)
      CALL SHOWBITBUFFERANDREPEATS(adbitbuf,adbitibuf,
     +     indexbitrepeats,nbbufrepeat)
c No pointer buffer so far...
      END

      SUBROUTINE SHOWSTACKANDBUFFERSSIZE()
      INTEGER*4 adi4buf(512)
      INTEGER adi4ibuf
      COMMON /adi4fbuf/adi4buf,adi4ibuf
      INTEGER*8 adi8buf(512)
      INTEGER adi8ibuf
      COMMON /adi8fbuf/adi8buf,adi8ibuf
      REAL*4 adr4buf(512)
      INTEGER adr4ibuf
      COMMON /adr4fbuf/adr4buf,adr4ibuf
      REAL*8 adr8buf(512)
      INTEGER adr8ibuf
      COMMON /adr8fbuf/adr8buf,adr8ibuf
      COMPLEX*8 adc8buf(512)
      INTEGER adc8ibuf
      COMMON /adc8fbuf/adc8buf,adc8ibuf
      COMPLEX*16 adc16buf(512)
      INTEGER adc16ibuf
      COMMON /adc16fbuf/adc16buf,adc16ibuf
      CHARACTER ads1buf(512)
      INTEGER ads1ibuf
      COMMON /ads1fbuf/ads1buf,ads1ibuf
      INTEGER*4 adbitbuf
      INTEGER adbitibuf
      COMMON /adbitfbuf/adbitbuf, adbitibuf
      CALL SHOWSTACKSIZE(adi4ibuf-1,adi8ibuf-1,adr4ibuf-1,adr8ibuf-1,
     +     adc8ibuf-1,adc16ibuf-1,ads1ibuf-1,adbitibuf-1,0)
      END

C=========== MEASUREMENT OF PUSH TRAFFIC ==========

      BLOCK DATA BUFTRAFFICBLOCK
      INTEGER*8 buffertraffic
      COMMON /BUFTRAFFIC/buffertraffic
      DATA buffertraffic/0/
      END

      subroutine addftraffic(n)
      INTEGER n
      INTEGER*8 buffertraffic
      COMMON /BUFTRAFFIC/buffertraffic
      buffertraffic = buffertraffic+n
      END

      SUBROUTINE ADSTACK_SHOWTRAFFIC()
      INTEGER*8 buffertraffic
      COMMON /BUFTRAFFIC/buffertraffic
      call SHOWTOTALTRAFFIC(buffertraffic)
      END
