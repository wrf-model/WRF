      SUBROUTINE WRITCP (LUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    WRITCP                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT: THIS SUBROUTINE SHOULD ONLY BE CALLED WHEN LOGICAL UNIT     
!   LUNIT HAS BEEN OPENED FOR OUTPUT OPERATIONS.  IT NOW SIMPLY CALLS   
!   BUFR ARCHIVE LIBRARY SUBROUTINE CMPMSG TO TOGGLE ON MESSAGE         
!   COMPRESSION, FOLLOWED BY A CALL TO WRITSB TO PACK UP THE CURRENT    
!   SUBSET WITHIN MEMORY AND TRY TO ADD IT TO THE COMPRESSED BUFR       
!   MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR THIS LUNIT,        
!   FOLLOWED BY ANOTHER CALL TO CMPMSG TO TOGGLE OFF MESSAGE            
!   COMPRESSION.  THIS SUBROUTINE USES THE SAME INPUT AND OUTPUT        
!   PARAMETERS AS WRITSB.                                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2005-03-09  J. ATOR    -- MODIFIED TO USE CMPMSG AND WRITSB           
!                                                                       
! USAGE:    CALL WRITCP (LUNIT)                                         
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        CMPMSG   WRITSB                         
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CALL CMPMSG ('Y') 
                                                                        
      CALL WRITSB (LUNIT) 
                                                                        
      CALL CMPMSG ('N') 
                                                                        
      RETURN 
      END SUBROUTINE WRITCP                         
