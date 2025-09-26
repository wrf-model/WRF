       SUBROUTINE SBYTE(IPACKD,IUNPKD,NOFF,NBITS)                            
C THIS PROGRAM WRITTEN BY.....                                                  
C             DR. ROBERT C. GAMMILL, CONSULTANT                                 
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C             JULY 1972 
C
C THIS IS THE FORTRAN 32 bit VERSION OF SBYTE.
C             Changes for SiliconGraphics IRIS-4D/25
C             SiliconGraphics 3.3 FORTRAN 77
C             MARCH 1991  RUSSELL E. JONES
C             NATIONAL WEATHER SERVICE
C
C***********************************************************************
C
C SUBROUTINE SBYTE (IPACKD,IUNPKD,NOFF,NBITS)
C
C PURPOSE                GIVEN A BYTE, RIGHT-JUSTIFIED IN A WORD, TO
C                        PACK THE BYTE INTO A TARGET WORD OR ARRAY.
C                        BITS SURROUNDING THE BYTE IN THE TARGET
C                        AREA ARE UNCHANGED.
C
C USAGE                  CALL SBYTE (IPACKD,IUNPKD,NOFF,NBITS)
C
C ARGUMENTS
C ON INPUT               IPACKD
C                          THE WORD OR ARRAY WHICH WILL CONTAIN THE
C                          PACKED BYTE.  BYTE MAY CROSS WORD BOUNDARIES.
C
C                        IUNPKD
C                          THE WORD CONTAINING THE RIGHT-JUSTIFIED BYTE
C                          TO BE PACKED.
C
C                        NOFF
C                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
C                          IN 'IPACKD' IN ORDER TO LOCATE WHERE THE
C                          BYTE IS TO BE PACKED.
C
C                        NBITS
C                          NUMBER OF BITS IN THE BYTE TO BE PACKED.
C                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
C                          BITS ON 32 BIT MACHINE.
C
C ON OUTPUT              IPACKD
C                          WORD OR CONSECUTIVE WORDS CONTAINING THE
C                          REQUESTED BYTE.
C***********************************************************************

      INTEGER    IUNPKD
      INTEGER    IPACKD(*)
      INTEGER    MASKS(64)
C
      SAVE
C
      DATA IFIRST/1/
      IF(IFIRST.EQ.1) THEN
         CALL W3FI01(LW)
         NBITSW = 8 * LW
         JSHIFT = -1 * NINT(ALOG(FLOAT(NBITSW)) / ALOG(2.0))
         MASKS(1) = 1
         DO I=2,NBITSW-1
            MASKS(I) = 2 * MASKS(I-1) + 1
         ENDDO
         MASKS(NBITSW) = -1
         IFIRST = 0
      ENDIF
C
C NBITS MUST BE LESS THAN OR EQUAL TO NBITSW
C
        ICON  = NBITSW - NBITS
        IF (ICON.LT.0) RETURN   
        MASK  = MASKS(NBITS)
C
C INDEX TELLS HOW MANY WORDS INTO IOUT THE NEXT BYTE IS TO BE STORED.           
C
        INDEX = ISHFT(NOFF,JSHIFT) 
C
C II TELLS HOW MANY BITS IN FROM THE LEFT SIDE OF THE WORD TO STORE IT.         
C
        II    = MOD(NOFF,NBITSW)
C
        J     = IAND(MASK,IUNPKD)    
        MOVEL = ICON - II         
C                                                                               
C BYTE IS TO BE STORED IN MIDDLE OF WORD.  SHIFT LEFT.                          
C
        IF (MOVEL.GT.0) THEN          
          MSK           = ISHFT(MASK,MOVEL)  
          IPACKD(INDEX+1) = IOR(IAND(NOT(MSK),IPACKD(INDEX+1)),
     &    ISHFT(J,MOVEL))
C                                                                               
C THE BYTE IS TO BE SPLIT ACROSS A WORD BREAK.                                  
C
        ELSE IF (MOVEL.LT.0) THEN          
          MSK           = MASKS(NBITS+MOVEL)      
          IPACKD(INDEX+1) = IOR(IAND(NOT(MSK),IPACKD(INDEX+1)),
     &    ISHFT(J,MOVEL))  
          ITEMP         = IAND(MASKS(NBITSW+MOVEL),IPACKD(INDEX+2))
          IPACKD(INDEX+2) = IOR(ITEMP,ISHFT(J,NBITSW+MOVEL))
C             
C BYTE IS TO BE STORED RIGHT-ADJUSTED.                                          
C
        ELSE
          IPACKD(INDEX+1) = IOR(IAND(NOT(MASK),IPACKD(INDEX+1)),J)
        ENDIF
C
      RETURN
      END
