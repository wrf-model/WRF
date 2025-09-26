      SUBROUTINE WRDLEN

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRDLEN
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE FIGURES OUT SOME IMPORTANT INFORMATION
C   ABOUT THE LOCAL MACHINE ON WHICH THE BUFR ARCHIVE LIBRARY SOFTWARE
C   IS BEING RUN AND STORES THIS INTO COMMON BLOCK /HRDWRD/.  SUCH
C   INFORMATION INCLUDES DETERMINING THE NUMBER OF BITS AND THE NUMBER
C   OF BYTES IN A MACHINE WORD AS WELL AS DETERMINING WHETHER THE
C   MACHINE USES THE ASCII OR EBCDIC CHARACTER SET.
C
C   NOTE: IT IS ONLY NECESSARY FOR THIS SUBROUTINE TO BE CALLED ONCE,
C   AND THIS IS NORMALLY DONE DURING THE FIRST CALL TO BUFR ARCHIVE
C   LIBRARY SUBROUTINE OPENBF.  HOWEVER, THE SUBROUTINE DOES KEEP TRACK
C   OF WHETHER IT HAS ALREADY BEEN CALLED; THUS, IF IT IS CALLED AGAIN
C   LATER BY A DIFFERENT BUFR ARCHIVE LIBRARY SUBROUTINE, IT WILL JUST
C   QUIETLY RETURN WITHOUT (RE)COMPUTING ALL OF THE INFORMATION WITHIN
C   COMMON BLOCK /HRDWRD/.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY OR FOR INFORMATIONAL PURPOSES;
C                           NBYTW INITIALIZED AS ZERO THE FIRST TIME
C                           THIS ROUTINE IS CALLED (BEFORE WAS
C                           UNDEFINED WHEN FIRST REFERENCED)
C 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST FLAG AND IMMEDIATE
C                           RETURN IF IFIRST=1
C 2007-01-19  J. ATOR    -- BIG-ENDIAN VS. LITTLE-ENDIAN IS NOW
C                           DETERMINED AT COMPILE TIME AND CONFIGURED
C                           WITHIN BUFRLIB VIA CONDITIONAL COMPILATION
C                           DIRECTIVES
C 2009-03-23  J. ATOR    -- CALL BVERS TO GET VERSION NUMBER
C
C USAGE:    CALL WRDLEN
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     BVERS    ERRWRT   IUPM
C    THIS ROUTINE IS CALLED BY: COBFL    COPYBF   DATEBF   DATELEN
C                               DUMPBF   IUPBS01  MESGBC   MESGBF
C                               OPENBF   RDMTBB   UPDS3
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   CINT,DINT,CVSTR
      CHARACTER*6   CNDIAN,CLANG
      EQUIVALENCE   (CINT,INT)
      EQUIVALENCE   (DINT,JNT)
      LOGICAL       PRINT

      DATA IFIRST/0/

      SAVE IFIRST

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     HAS THIS SUBROUTINE ALREADY BEEN CALLED?

      IF(IFIRST.EQ.0) THEN

C        NO, SO CHECK WHETHER DIAGNOSTIC INFORMATION SHOULD BE PRINTED
C        AND THEN PROCEED THROUGH THE REST OF THE SUBROUTINE.

         PRINT = IPRT.GE.1
         IFIRST = 1
      ELSE

C        YES, SO THERE IS NO NEED TO PROCEED ANY FURTHER.

         RETURN
      ENDIF

C  COUNT THE BITS IN A WORD - MAX 64 ALLOWED
C  -----------------------------------------

      INT = 1
      DO I=1,65
      INT = ISHFT(INT,1)
      IF(INT.EQ.0) GOTO 10
      ENDDO
c  .... DK: Can the below ever happen since upper loop bounds is 65?
   10 IF(I.GE.65)       GOTO 900
      IF(MOD(I,8).NE.0) GOTO 901

C  NBITW is no. of bits in a word, NBYTW is no. of bytes in a word
C  ---------------------------------------------------------------

      NBITW = I
      NBYTW = I/8

C  INDEX THE BYTE STORAGE ORDER -  HIGH BYTE TO LOW BYTE
C  -----------------------------------------------------

      JNT = 0

      DO I = 1,8
         IORD(I) = 9999
      ENDDO

      DO I=1,NBYTW
         INT = ISHFT(1,(NBYTW-I)*8)
         DO J=1,NBYTW
            IF(CINT(J:J).NE.DINT(J:J)) GOTO 20
         ENDDO
c  .... DK: Can the below ever happen since upper loop bounds is NBYTW?
   20    IF(J.GT.NBYTW) GOTO 902
         IORD(I) = J
      ENDDO

C  SETUP AN ASCII/EBCDIC TRANSLATOR AND DETERMINE WHICH IS NATIVE
C  --------------------------------------------------------------

      IA = IUPM('A',8)
      IF(IA.EQ. 65) THEN
         IASCII = 1
         CLANG  = 'ASCII '
      ELSEIF(IA.EQ.193) THEN
         IASCII = 0
         CLANG  = 'EBCDIC'
      ELSE
         GOTO 903
      ENDIF

      DO I=0,255
      IETOA(I) = 0
      IATOE(I) = 0
      ENDDO

      IETOA(  1) =   1
      IATOE(  1) =   1
      IETOA(  2) =   2
      IATOE(  2) =   2
      IETOA(  3) =   3
      IATOE(  3) =   3
      IETOA(  5) =   9
      IATOE(  9) =   5
      IETOA(  7) = 127
      IATOE(127) =   7
      IETOA( 11) =  11
      IATOE( 11) =  11
      IETOA( 12) =  12
      IATOE( 12) =  12
      IETOA( 13) =  13
      IATOE( 13) =  13
      IETOA( 14) =  14
      IATOE( 14) =  14
      IETOA( 15) =  15
      IATOE( 15) =  15
      IETOA( 16) =  16
      IATOE( 16) =  16
      IETOA( 17) =  17
      IATOE( 17) =  17
      IETOA( 18) =  18
      IATOE( 18) =  18
      IETOA( 19) =  19
      IATOE( 19) =  19
      IETOA( 22) =   8
      IATOE(  8) =  22
      IETOA( 24) =  24
      IATOE( 24) =  24
      IETOA( 25) =  25
      IATOE( 25) =  25
      IETOA( 29) =  29
      IATOE( 29) =  29
      IETOA( 31) =  31
      IATOE( 31) =  31
      IETOA( 34) =  28
      IATOE( 28) =  34
      IETOA( 37) =  10
      IATOE( 10) =  37
      IETOA( 38) =  23
      IATOE( 23) =  38
      IETOA( 39) =  27
      IATOE( 27) =  39
      IETOA( 45) =   5
      IATOE(  5) =  45
      IETOA( 46) =   6
      IATOE(  6) =  46
      IETOA( 47) =   7
      IATOE(  7) =  47
      IETOA( 50) =  22
      IATOE( 22) =  50
      IETOA( 53) =  30
      IATOE( 30) =  53
      IETOA( 55) =   4
      IATOE(  4) =  55
      IETOA( 60) =  20
      IATOE( 20) =  60
      IETOA( 61) =  21
      IATOE( 21) =  61
      IETOA( 63) =  26
      IATOE( 26) =  63
      IETOA( 64) =  32
      IATOE( 32) =  64
      IETOA( 74) =  91
      IATOE( 91) =  74
      IETOA( 75) =  46
      IATOE( 46) =  75
      IETOA( 76) =  60
      IATOE( 60) =  76
      IETOA( 77) =  40
      IATOE( 40) =  77
      IETOA( 78) =  43
      IATOE( 43) =  78
      IETOA( 79) =  33
      IATOE( 33) =  79
      IETOA( 80) =  38
      IATOE( 38) =  80
      IETOA( 90) =  93
      IATOE( 93) =  90
      IETOA( 91) =  36
      IATOE( 36) =  91
      IETOA( 92) =  42
      IATOE( 42) =  92
      IETOA( 93) =  41
      IATOE( 41) =  93
      IETOA( 94) =  59
      IATOE( 59) =  94
      IETOA( 95) =  94
      IATOE( 94) =  95
      IETOA( 96) =  45
      IATOE( 45) =  96
      IETOA( 97) =  47
      IATOE( 47) =  97
      IETOA(106) = 124
      IATOE(124) = 106
      IETOA(107) =  44
      IATOE( 44) = 107
      IETOA(108) =  37
      IATOE( 37) = 108
      IETOA(109) =  95
      IATOE( 95) = 109
      IETOA(110) =  62
      IATOE( 62) = 110
      IETOA(111) =  63
      IATOE( 63) = 111
      IETOA(121) =  96
      IATOE( 96) = 121
      IETOA(122) =  58
      IATOE( 58) = 122
      IETOA(123) =  35
      IATOE( 35) = 123
      IETOA(124) =  64
      IATOE( 64) = 124
      IETOA(125) =  39
      IATOE( 39) = 125
      IETOA(126) =  61
      IATOE( 61) = 126
      IETOA(127) =  34
      IATOE( 34) = 127
      IETOA(129) =  97
      IATOE( 97) = 129
      IETOA(130) =  98
      IATOE( 98) = 130
      IETOA(131) =  99
      IATOE( 99) = 131
      IETOA(132) = 100
      IATOE(100) = 132
      IETOA(133) = 101
      IATOE(101) = 133
      IETOA(134) = 102
      IATOE(102) = 134
      IETOA(135) = 103
      IATOE(103) = 135
      IETOA(136) = 104
      IATOE(104) = 136
      IETOA(137) = 105
      IATOE(105) = 137
      IETOA(145) = 106
      IATOE(106) = 145
      IETOA(146) = 107
      IATOE(107) = 146
      IETOA(147) = 108
      IATOE(108) = 147
      IETOA(148) = 109
      IATOE(109) = 148
      IETOA(149) = 110
      IATOE(110) = 149
      IETOA(150) = 111
      IATOE(111) = 150
      IETOA(151) = 112
      IATOE(112) = 151
      IETOA(152) = 113
      IATOE(113) = 152
      IETOA(153) = 114
      IATOE(114) = 153
      IETOA(161) = 126
      IATOE(126) = 161
      IETOA(162) = 115
      IATOE(115) = 162
      IETOA(163) = 116
      IATOE(116) = 163
      IETOA(164) = 117
      IATOE(117) = 164
      IETOA(165) = 118
      IATOE(118) = 165
      IETOA(166) = 119
      IATOE(119) = 166
      IETOA(167) = 120
      IATOE(120) = 167
      IETOA(168) = 121
      IATOE(121) = 168
      IETOA(169) = 122
      IATOE(122) = 169
      IETOA(173) =  91
      IATOE( 91) = 173
      IETOA(176) =  48
      IATOE( 48) = 176
      IETOA(177) =  49
      IATOE( 49) = 177
      IETOA(178) =  50
      IATOE( 50) = 178
      IETOA(179) =  51
      IATOE( 51) = 179
      IETOA(180) =  52
      IATOE( 52) = 180
      IETOA(181) =  53
      IATOE( 53) = 181
      IETOA(182) =  54
      IATOE( 54) = 182
      IETOA(183) =  55
      IATOE( 55) = 183
      IETOA(184) =  56
      IATOE( 56) = 184
      IETOA(185) =  57
      IATOE( 57) = 185
      IETOA(189) =  93
      IATOE( 93) = 189
      IETOA(192) = 123
      IATOE(123) = 192
      IETOA(193) =  65
      IATOE( 65) = 193
      IETOA(194) =  66
      IATOE( 66) = 194
      IETOA(195) =  67
      IATOE( 67) = 195
      IETOA(196) =  68
      IATOE( 68) = 196
      IETOA(197) =  69
      IATOE( 69) = 197
      IETOA(198) =  70
      IATOE( 70) = 198
      IETOA(199) =  71
      IATOE( 71) = 199
      IETOA(200) =  72
      IATOE( 72) = 200
      IETOA(201) =  73
      IATOE( 73) = 201
      IETOA(208) = 125
      IATOE(125) = 208
      IETOA(209) =  74
      IATOE( 74) = 209
      IETOA(210) =  75
      IATOE( 75) = 210
      IETOA(211) =  76
      IATOE( 76) = 211
      IETOA(212) =  77
      IATOE( 77) = 212
      IETOA(213) =  78
      IATOE( 78) = 213
      IETOA(214) =  79
      IATOE( 79) = 214
      IETOA(215) =  80
      IATOE( 80) = 215
      IETOA(216) =  81
      IATOE( 81) = 216
      IETOA(217) =  82
      IATOE( 82) = 217
      IETOA(224) =  92
      IATOE( 92) = 224
      IETOA(226) =  83
      IATOE( 83) = 226
      IETOA(227) =  84
      IATOE( 84) = 227
      IETOA(228) =  85
      IATOE( 85) = 228
      IETOA(229) =  86
      IATOE( 86) = 229
      IETOA(230) =  87
      IATOE( 87) = 230
      IETOA(231) =  88
      IATOE( 88) = 231
      IETOA(232) =  89
      IATOE( 89) = 232
      IETOA(233) =  90
      IATOE( 90) = 233
      IETOA(240) =  48
      IATOE( 48) = 240
      IETOA(241) =  49
      IATOE( 49) = 241
      IETOA(242) =  50
      IATOE( 50) = 242
      IETOA(243) =  51
      IATOE( 51) = 243
      IETOA(244) =  52
      IATOE( 52) = 244
      IETOA(245) =  53
      IATOE( 53) = 245
      IETOA(246) =  54
      IATOE( 54) = 246
      IETOA(247) =  55
      IATOE( 55) = 247
      IETOA(248) =  56
      IATOE( 56) = 248
      IETOA(249) =  57
      IATOE( 57) = 249

C  SHOW SOME RESULTS
C  -----------------

      IF(PRINT) THEN
         CALL BVERS(CVSTR)
#ifdef BIG_ENDIAN
         CNDIAN = '  BIG '
#else
         CNDIAN = 'LITTLE'
#endif
      WRITE (  UNIT=ERRSTR, FMT='(2A)' )
     & '=============== 
     & WELCOME TO THE BUFR ARCHIVE LIBRARY', ' =============='
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(A,I2)' )
     & ' MACHINE CHARACTERISTICS: NUMBER OF BYTES PER WORD =', NBYTW
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(A,I3)' )
     & '                          NUMBER OF BITS PER WORD =', NBITW
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(3A)' )
     & '                          BYTE ORDER IS ', CNDIAN,
     & ' ENDIAN'
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(3A)' )
     & '                          ', CLANG,
     & ' IS THE NATIVE LANGUAGE'
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(3A)' )
     & '====================== VERSION: ', CVSTR,
     & '=========================='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

      RETURN
  900 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - MACHINE WORD LENGTH IS
     & LIMITED TO 64 BITS (THIS MACHINE APPARENTLY HAS",I4," BIT
     & WORDS!)")') I
      CALL BORT(BORT_STR)
  901 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - MACHINE WORD LENGTH (",I4,"
     & ) IS NOT A MULTIPLE OF 8 (THIS MACHINE HAS WORDS NOT ON WHOLE
     &  BYTE BOUNDARIES!)")') I
      CALL BORT(BORT_STR)
  902 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - BYTE ORDER CHECKING MISTAKE
     & , LOOP INDEX J (HERE =",I3,") IS .GT. NO. OF BYTES PER WORD
     & ON THIS MACHINE (",I3,")")') J,NBYTW
      CALL BORT(BORT_STR)
  903 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - CAN''T DETERMINE MACHINE
     & NATIVE LANGUAGE (CHAR. A UNPACKS TO INT.",I4," NEITHER ASCII
     &  (65) NOR EBCDIC (193)")') IA
      CALL BORT(BORT_STR)
      END
