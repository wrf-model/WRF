      SUBROUTINE UPBB (NVAL, NBITS, IBIT, IBAY) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UPBB                                                   
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE UNPACKS AND RETURNS A BINARY INTEGER        
!   CONTAINED WITHIN NBITS BITS OF IBAY, STARTING WITH BIT (IBIT+1).    
!   THIS IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE UPB, EXCEPT IN   
!   UPBB IBIT IS NOT UPDATED UPON OUTPUT (AND THE ORDER OF ARGUMENTS IS 
!   DIFFERENT).                                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-  
!                           LINING CODE WITH FPP DIRECTIVES             
! 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS   
!                           IN DECODER VERSION)                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- ADDED CHECK FOR NBITS EQUAL TO ZERO;        
!                           MODIFIED LOGIC TO MAKE IT CONSISTENT WITH   
!                           LOGIC IN UPB; UNIFIED/PORTABLE FOR WRF;     
!                           ADDED DOCUMENTATION (INCLUDING HISTORY)     
!                                                                       
! USAGE:    CALL UPBB (NVAL, NBITS, IBIT, IBAY)                         
!   INPUT ARGUMENT LIST:                                                
!     NBITS    - INTEGER: NUMBER OF BITS OF IBAY WITHIN WHICH TO UNPACK 
!                NVAL                                                   
!     IBIT     - INTEGER: BIT POINTER WITHIN IBAY TO START UNPACKING    
!                FROM                                                   
!     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING PACKED  
!                NVAL                                                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NVAL     - INTEGER: UNPACKED INTEGER                              
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IREV                                    
!    THIS ROUTINE IS CALLED BY: RCSTPL   RDTREE   UFBGET   UFBTAB       
!                               UFBTAM   WRITLC                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / HRDWRD / NBYTW, NBITW, NREV, IORD (8) 
                                                                        
      DIMENSION IBAY ( * ) 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!  IF NBITS=0, THEN JUST SET NVAL=0 AND RETURN                          
!  -------------------------------------------                          
                                                                        
      IF (NBITS.EQ.0) THEN 
      NVAL = 0 
      GOTO 100 
      ENDIF 
                                                                        
      NWD = IBIT / NBITW + 1 
      NBT = MOD (IBIT, NBITW) 
      INT = ISHFT (IREV (IBAY (NWD) ), NBT) 
      INT = ISHFT (INT, NBITS - NBITW) 
      LBT = NBT + NBITS 
      IF (LBT.GT.NBITW) THEN 
      JNT = IREV (IBAY (NWD+1) ) 
      INT = IOR (INT, ISHFT (JNT, LBT - 2 * NBITW) ) 
      ENDIF 
      NVAL = INT 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE UPBB                           
