      SUBROUTINE SETUP                                                           SETUP.1
C                                                                                SETUP.2
C   PURPOSE :  INITIALIZES THE CONSTANTS AND VARIABLES WHICH CAN BE              SETUP.3
C              DETERMINED BASED ON THE INPUT NAMELIST VARIABLES.                 SETUP.4
C   CALLS :    THE INTRINSIC FUNCTIONS SIN, COS, TAN, ALOG, AND ALOG10.          SETUP.5
C   CALLED BY :THE MAIN PROGRAM TERRAIN.                                         SETUP.6
C   COMMENTS :                                                                   SETUP.7
C  THE COMMON BLOCKS /MAPS/, /NESTDMN/, /FUDG/, /TRFUDGE/, /OPTION/,             SETUP.8
C  /LTDATA/, AND /HEADER/ ARE USED IN THIS SUBROUTINE. THERE ARE FIVE            SETUP.9
C  NAMELISTS /MAPBG/, /DOMAIN/, /OPTN/, /FUDGE/, AND /FUDGET/ IN THIS            SETUP.10
C  SUBROUTINE. THIS SUBROUTINE HAS NO DUMMY ARGUMENTS. PLEASE SEE                SETUP.11
C  SECTION 5.1.1 AND 5.2.1 FOR DETAILS.                                          SETUP.12
C                                                                                SETUP.13
# include "maps.incl"
# include "nestdmn.incl"
# include "namelist1.incl"
C
      REAL TRUELAT0

      DATA CONV/57.29578/, A/6370./                                              SETUP.56
      DATA NPROJ/'LAMCON','POLSTR','MERCAT'/                                     SETUP.57
C
        PRINT *,' ==> CALL SETUP <=='                                            SETUP.60
C                                                                                SETUP.61

C ==> READ "MAPBG"
         READ(15,MAPBG,ERR=55,END=55)
c .. Namelist  MAPBG available:   
         GO TO 56
c .. Namelist  MAPBG unavailable:
 55      rewind (15)

C ==> READ "DOMAIN"
 56      READ(15,DOMAINS,ERR=66,END=66)
c .. Namelist DOMAINS available:
         GO TO 67
c .. Namelist DOMAINS unavailable:
 66      rewind (15)

 67      CONTINUE
C        PRINT *, MAPBG
C        PRINT *, DOMAINS

      if (iproj.eq.'CYLEQU') then
         PROJECT='CE'
         goto 1002
      endif
C                                                                                SETUP.146
C   DEFINE THE PARAMETERS OF MAP BASED ON THE IPROJ:                             SETUP.147
C                                                                                SETUP.148
         XN = -1.0E36                                                            SETUP.150
      IF(PHIC.LT.0) THEN                                                         SETUP.151
      SIGN=-1.      ! SOUTH HEMESPHERE                                           SETUP.152
      ELSE                                                                       SETUP.153
      SIGN=1.       ! NORTH HEMESPHERE                                           SETUP.154
      ENDIF                                                                      SETUP.155
         POLE = 90.                                                              SETUP.156

      IF (IPROJ.EQ.NPROJ(1)) THEN                                                SETUP.157

        print '("Map projection=",a)',NPROJ(1)
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.158
      TRUELAT1=60.                                                               SETUP.159
      TRUELAT2=30.                                                               SETUP.160
      TRUELAT1=SIGN*TRUELAT1                                                     SETUP.161
      TRUELAT2=SIGN*TRUELAT2                                                     SETUP.162
      ENDIF                                                                      SETUP.163

! In case of TRUELAT1 == TRUELAT2 (YRG 09/22/2005):

      if (abs(TRUELAT1-TRUELAT2)==0.0) then
         TRUELAT0 = TRUELAT1
         TRUELAT1 = TRUELAT0 - 1.e-2
         TRUELAT2 = TRUELAT0 + 1.e-2
      endif

      XN = ALOG10(COS(TRUELAT1 / CONV)) -                                        SETUP.164
     *     ALOG10(COS(TRUELAT2 / CONV))                                          SETUP.165
      XN = XN/(ALOG10(TAN((45.0 -  SIGN*TRUELAT1/2.0) / CONV)) -                 SETUP.166
     *                 ALOG10(TAN((45.0 - SIGN*TRUELAT2/2.0) / CONV)))           SETUP.167
 
c         TRUELAT1 = TRUELAT0
c         TRUELAT2 = TRUELAT0

      PSI1=90.-SIGN*TRUELAT1                                                     SETUP.168
         PROJECT='LC'                                                            SETUP.169

      ELSE IF (IPROJ.EQ.NPROJ(2)) THEN                                           SETUP.170

        print '("Map projection=",a)',NPROJ(2)
         XN = 1.0                                                                SETUP.171
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.172
      TRUELAT1=60.                                                               SETUP.173
      TRUELAT2=0.                                                                SETUP.174
      TRUELAT1=SIGN*TRUELAT1                                                     SETUP.175
      TRUELAT2=SIGN*TRUELAT2                                                     SETUP.176
      ENDIF                                                                      SETUP.177
C PSI1 IS THE PSEUDO-LATITUDE                                                    SETUP.178
      PSI1=90.-SIGN*TRUELAT1                                                     SETUP.179
         PROJECT='ST'                                                            SETUP.180

      ELSE IF (IPROJ.EQ.NPROJ(3)) THEN                                           SETUP.181

         XN = 0.                                                                 SETUP.182
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.183
      TRUELAT1=0.                                                                SETUP.184
      TRUELAT2=0.                                                                SETUP.185
      ENDIF                                                                      SETUP.186

C -------------------------------------------------------------------
C For Map plotting on Mercator projection, adjusted the
C TRUELAT1 and TRUELAT2 to zero by increasing the grid
C distance because MAPDRV can only support the TRUELAT=0.0
C                            Y.R. Guo  09/23/2005

        if (TRUELAT1 .NE. TRUELAT2) THEN
          TRUELAT1 = max(TRUELAT1,TRUELAT2)
          TRUELAT2 = max(TRUELAT1,TRUELAT2)
        endif
        print '(/"MERCATOR MAP PROJECTION=",a,a,2F8.2)',
     -           IPROJ, "  TRUELAT 1,2=",TRUELAT1, TRUELAT2

      IF(TRUELAT1.NE.0.) THEN                                                    SETUP.187

        do nm = 1, maxnes 
           print '(I2," Original dis=",f10.3)', nm, dis(nm) 
           dis(nm) = dis(nm) / COS(TRUELAT1*3.1415926/180.)
           print '("   Adjusted DIS=",F10.3)', dis(nm)
        enddo
        TRUELAT1=0.                                                               SETUP.190
        TRUELAT2=0.                                                               SETUP.190
c        print '("TRUELAT1 and 2:",2F8.2/)', TRUELAT1, TRUELAT2
        
c      PRINT *,'TERRAIN AND MAPDRV ONLY SUPPORT MERCATOR PROJECTION               SETUP.188
c     *AT 0 DEGREE TURE LATITUDE, RESET TRUELAT1=0.'                              SETUP.189
C ---------------------------------------------------------------------

      ENDIF 
      PSI1=0.                                                                    SETUP.192
         PROJECT='ME'                                                            SETUP.193
      END IF                                                                     SETUP.194
C                                                                                SETUP.195
      PSI1 = PSI1/CONV                                                           SETUP.196
      IF (PHIC.LT.0.) THEN                                                       SETUP.197
        PSI1 = -PSI1                                                             SETUP.198
        POLE = -POLE                                                             SETUP.199
      ENDIF                                                                      SETUP.200
C                                                                                SETUP.201
C--------CALCULATE R                                                             SETUP.202
      IF (IPROJ.NE.NPROJ(3)) THEN                                                SETUP.203
         PSX = (POLE-PHIC)/CONV                                                  SETUP.204
         IF (IPROJ.EQ.NPROJ(1)) THEN                                             SETUP.205
            CELL  = A*SIN(PSI1)/XN                                               SETUP.206
            CELL2 = (TAN(PSX/2.))/(TAN(PSI1/2.))                                 SETUP.207
         ENDIF                                                                   SETUP.208
         IF (IPROJ.EQ.NPROJ(2)) THEN                                             SETUP.209
            CELL  = A*SIN(PSX)/XN                                                SETUP.210
            CELL2 = (1. + COS(PSI1))/(1. + COS(PSX))                             SETUP.211
         ENDIF                                                                   SETUP.212
         R = CELL*(CELL2)**XN                                                    SETUP.213
         XCNTR = 0.0                                                             SETUP.214
         YCNTR = -R                                                              SETUP.215
      ENDIF                                                                      SETUP.216
C-----FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1             SETUP.217
      IF (IPROJ.EQ.NPROJ(3)) THEN                                                SETUP.218
         C2     = A*COS(PSI1)                                                    SETUP.219
         XCNTR  = 0.0                                                            SETUP.220
         PHICTR = PHIC/CONV                                                      SETUP.221
         CELL   = COS(PHICTR)/(1.0+SIN(PHICTR))                                  SETUP.222
         YCNTR  = - C2*ALOG(CELL)                                                SETUP.223
      ENDIF                                                                      SETUP.224
      PRINT 10,XCNTR,YCNTR                                                       SETUP.226
10    FORMAT(1X,'COARSE GRID CENTER IS AT X =',F8.1,' KM AND Y = ',              SETUP.227
     1       F8.1,' KM ')                                                        SETUP.228
C                                                                                SETUP.230

 1002 continue

C   CHECK THE COMPATIBILITY OF NEST DOMAINS WITH THE COARSE DOMAINS              SETUP.231
C     AND CALCULATE THE IRATIOS, INORTHS, ISOUTHS, IWESTS AND IEASTS             SETUP.232
C                                                                                SETUP.233
C     A) EXTENDING THE COARSE DOMAIN IF IEXP = 1                                 SETUP.234
C                                                                                SETUP.235
        IXEX = NESTIX(1)                                                         SETUP.236
        JXEX = NESTJX(1)                                                         SETUP.237
        IOFFST = 0                                                               SETUP.238
        JOFFST = 0                                                               SETUP.239
      IF (IEXP.EQ.1) THEN                                                        SETUP.240
        INCR = INT(AEXP/DIS(1) + 1.001)                                          SETUP.241
        IXEX = NESTIX(1) + INCR*2                                                SETUP.242
        JXEX = NESTJX(1) + INCR*2                                                SETUP.243
        IOFFST = INCR                                                            SETUP.244
        JOFFST = INCR                                                            SETUP.245
C                                                                                SETUP.252
        PRINT 20,INCR                                                            SETUP.253
        PRINT 22,IXEX,JXEX                                                       SETUP.254
20    FORMAT(2X,'$$$$ GRID IS EXPANDED BY ',I3,                                  SETUP.255
     1                         ' GRID POINTS ON EACH EDGE $$$$')                 SETUP.256
22    FORMAT(5X,'EXTENDED IXEX IS ',I3,5X,'EXTENDED JXEX IS ',I3)                SETUP.257
      ENDIF                                                                      SETUP.258
C-----CENTER OF GRID IN THE COARSE DOMAIN                                        SETUP.259
      CNTRJ0 = FLOAT(JXEX+1)/2.                                                  SETUP.260
      CNTRI0 = FLOAT(IXEX+1)/2.                                                  SETUP.261
        PRINT 21,NESTIX(1),NESTJX(1)                                             SETUP.262
21    FORMAT(2X,'COARSE DOMAIN SIZE (IX,JX) =',2I5)                              SETUP.263
C                                                                                SETUP.264
C  MIX, MJX ARE USED IN SUB. TFUDGE:                                             SETUP.265
      MIX = IXEX                                                                 SETUP.266
      MJX = JXEX                                                                 SETUP.267
      DO 23 M = 1, MAXNES                                                        SETUP.268
      MIX = MAX0(NESTIX(M),MIX)                                                  SETUP.269
      MJX = MAX0(NESTJX(M),MJX)                                                  SETUP.270
23    CONTINUE                                                                   SETUP.271
      PRINT 24, MIX,MJX                                                          SETUP.275
24    FORMAT('   ++> THE MAXIMUM DIMENSION = (',2I5,') <++')                     SETUP.276
C                                                                                SETUP.277
C  CHECK IF POLE IS INSIDE THE DOMAIN OR NOT FOR LAMBERT PROJECTION:             SETUP.278
      HDSIZE = (IXEX-1)*DIS(1)/2.                                                SETUP.279
      IF (HDSIZE.GT.ABS(YCNTR) .AND. IPROJ.EQ.NPROJ(1))THEN                      SETUP.280
         PRINT *,'-------------------------------------------------'             SETUP.281
         PRINT *,'HALF DOMAIN SIZE IN Y-DIRECTION = ',HDSIZE                     SETUP.282
         PRINT *,'    DISTANCE FROM CNTER TO POLE = ',ABS(YCNTR)                 SETUP.283
         PRINT *,'NOT MAKE SENSE WITH THE POLE IS INSIDE THE DOMAIN '            SETUP.284
         PRINT *,'    FOR LAMBERT CONFORMAL PROJECTION!'                         SETUP.285
         PRINT *,'=== PLEASE RE-SPECIFY THE CENTER OR DOMAIN SIZE. ==='          SETUP.286
         STOP                                                                    SETUP.287
      ENDIF                                                                      SETUP.288
C                                                                                SETUP.289
C     B) CALCULATING THE IRATIOS, INORTHS AND IEASTS:                            SETUP.290
C                                                                                SETUP.291
      IRATIO(1) = 1                                                              SETUP.292
      NRATIO(1) = 1                                                              SETUP.293
      XSOUTH(1) = 1.                                                             SETUP.294
      XWEST(1)  = 1.                                                             SETUP.295
      XNORTH(1) = FLOAT(IXEX)                                                    SETUP.296
      XEAST(1)  = FLOAT(JXEX)                                                    SETUP.297
      XJC = (XEAST(1) + 1.0)/2.                                                  SETUP.298
C
      PRINT 27,XSOUTH(1),XWEST(1),XNORTH(1),XEAST(1),DIS(1),                     SETUP.300
     1         IRATIO(1),NRATIO(1)                                               SETUP.301
 27   FORMAT(1X,'XSOUTH(1)= ',F6.1,2X,'XWEST(1)= ',F6.1,2X,                      SETUP.302
     1'XNORTH(1)= ',F6.1,2X,'XEAST(1)= ',F6.1,2X,'DIS(1)= ',F6.1,                SETUP.303
     22X,'IRATIO(1)= ',I3,2X,'NRATIO(1)= ',I3)                                   SETUP.304
C                                                                                SETUP.306
      MISMATCH = 0                                                               SETUP.307
                                                                                 SETUP.308
      DO 30 NM = 2, MAXNES                                                       SETUP.309
C                                                                                SETUP.310
C                                                                                SETUP.311
C  DOMAINS' CONSISTENCY CHECK:                                                   SETUP.312
C                                                                                SETUP.313
      NMC = NUMC(NM)                                                            SETUP.314
      IF (AMOD((DIS(NMC)+0.09),DIS(NM)) .GT. 0.1) THEN                           SETUP.315
         MISMATCH = MISMATCH + 1                                                 SETUP.316
         PRINT 29, NM,NMC                                                        SETUP.317
         PRINT 31,NM,DIS(NM),NMC,DIS(NMC)                                        SETUP.318
29      FORMAT(2X,'DOMAIN ',I2,' HAS INCORRECT GRID SIZE ',                      SETUP.319
     1'  IT HAS TO BE THE MULTIPLE OF DOMAIN ',I2)                               SETUP.320
31      FORMAT(2X,'DOMAIN ',I2,' GRID SIZE= ',F6.1,' KM',                        SETUP.321
     1         '  DOMAIN ',I2,' GRID SIZE= ',F6.1,' KM')                         SETUP.322
        GO TO 30                                                                 SETUP.323
      ENDIF                                                                      SETUP.324
      IRATIO(NM) = NINT(DIS(NMC)/DIS(NM))                                        SETUP.325
      NRATIO(NM) = NINT(DIS(1)/DIS(NM))                                          SETUP.326
C                                                                                SETUP.327
C   MAKE SURE THE 4 CORNER POINTS OF NEST DOMAINS ARE ON THE                     SETUP.328
C   PREVIOUS DOMAIN GRID-POINTS                                                  SETUP.329
C                                                                                SETUP.330
      IF (MOD((NESTIX(NM)-1),IRATIO(NM)).NE.0) THEN                              SETUP.331
        MISMATCH = MISMATCH + 1                                                  SETUP.332
        IIMXN = (INT(FLOAT(NESTIX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1           SETUP.333
        PRINT 32,NM,NESTIX(NM),IRATIO(NM),IIMXN                                  SETUP.334
32      FORMAT(2X,'NESTIX(',I2,')=',I4,' AND IRATIO=',I2,' DOES NOT MATC         SETUP.335
     1H, YOU MAY SET NESTIX TO ',I4)                                             SETUP.336
      ENDIF                                                                      SETUP.337
      IF (MOD((NESTJX(NM)-1),IRATIO(NM)).NE.0) THEN                              SETUP.338
        MISMATCH = MISMATCH + 1                                                  SETUP.339
        JJMXN = (INT(FLOAT(NESTJX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1           SETUP.340
        PRINT 33,NM,NESTJX(NM),IRATIO(NM),JJMXN                                  SETUP.341
33      FORMAT(2X,'NESTJX(',I2,')=',I4,' AND IRATIO=',I2,' DOES NOT MATC         SETUP.342
     1H, YOU MAY SET NESTJX TO ',I4)                                             SETUP.343
      ENDIF                                                                      SETUP.344
C                                                                                SETUP.345
C--------REDEFINE LOCATION OF LOWER LEFT CORNER OF FINE MESH (IN TERMS           SETUP.346
C        OF EXTENDED COARSE MESH - DOMAIN 1 INDICES) IF USING EXTENDED G         SETUP.347
                                                                                 SETUP.348
       PRINT 34,NM,NESTIX(NM),NESTJX(NM),DIS(NM),NESTI(NM),NESTJ(NM)             SETUP.349
     1, NUMC(NM),IRATIO(NM),NRATIO(NM),IEXP                                     SETUP.350
34     FORMAT(/1X,'DOMAIN ',I2,2X,'IX=',I3,2X,'JX=',I3,2X,                       SETUP.351
     1 'DS= ',F6.1,2X,'ICNS=',I4,2X,'JCNS=',I4,2X,                               SETUP.352
     2 'NUMC= ',I2,2X,'IRATIO= ',I2,2X,'NRATIO= ',I2,2X,                        SETUP.353
     3 'IEXP= ',I2)                                                              SETUP.354
C                                                                                SETUP.355
      XDIS = 0.0                                                                 SETUP.356
      YDIS = 0.0                                                                 SETUP.357
      ND1 = NM                                                                   SETUP.358
      ND2 = NMC                                                                  SETUP.359
40    CONTINUE                                                                   SETUP.360
      XDIS = (NESTI(ND1)-1)*DIS(ND2) + XDIS                                      SETUP.361
      YDIS = (NESTJ(ND1)-1)*DIS(ND2) + YDIS                                      SETUP.362
      IF (ND2 .GT. 1) THEN                                                       SETUP.363
        ND1 = ND2                                                                SETUP.364
        ND2 = NUMC(ND2)                                                         SETUP.365
        GO TO 40                                                                 SETUP.366
      ENDIF                                                                      SETUP.367
C                                                                                SETUP.368
       XSOUTH(NM) = XDIS/DIS(1) + FLOAT(IOFFST) + 1                              SETUP.369
       XWEST(NM)  = YDIS/DIS(1) + FLOAT(JOFFST) + 1                              SETUP.370
       XNORTH(NM) = XSOUTH(NM) + FLOAT(NESTIX(NM)-1)*DIS(NM)/DIS(1)              SETUP.371
       XEAST(NM)  =  XWEST(NM) + FLOAT(NESTJX(NM)-1)*DIS(NM)/DIS(1)              SETUP.372
                                                                                 SETUP.373
       PRINT 35                                                                  SETUP.374
       PRINT 36,XSOUTH(NM),XWEST(NM),XNORTH(NM),XEAST(NM)                        SETUP.375
35     FORMAT(2X,'COARSE MESH INDICES FOR THE 4 CORNER POINTS ARE')              SETUP.376
36     FORMAT(2X,'SOUTH=',F6.1,3X,'WEST=',F6.1,3X,'NORTH =',F6.1,3X,             SETUP.377
     1        'EAST = ',F6.1)                                                    SETUP.378
C                                                                                SETUP.379
30     CONTINUE                                                                  SETUP.380
C                                                                                SETUP.381
       IF (MISMATCH.GT.0) THEN                                                   SETUP.382
       PRINT *, 'TERRAIN STOP IN SUBROUTINE SETUP DUE TO INCORRECT NEST          SETUP.383
     1 DOMAIN SET UP'                                                            SETUP.384
cc       STOP 1111                                                                 SETUP.385
       ENDIF                                                                     SETUP.386
C                                                                                SETUP.387
       RETURN                                                                    SETUP.440
       END                                                                       SETUP.441
