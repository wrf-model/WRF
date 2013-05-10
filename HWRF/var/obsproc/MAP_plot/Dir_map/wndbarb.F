C                                                                               
      subroutine wndbarb (xbase,ybase,u,v)                                      
C  ===> the unit of u, v is knots. flag is 50 knots. <===                       
C**************************************************************************     
C wndbarb - for the graph portion of GRIN                                       
C  This routine draws a single wind barb per call.                              
C                                                                               
C on input - four variables come in.  xbase contains the horizontal coor-       
C            dinate of the base of the wind barb.  ybase contains the           
C            vertical coordinate of the base of the wind barb.  u contains      
C            the east-west wind component in meters per second.  v contains     
C            the north-south wind component in meters per second.               
C                                                                               
C on output - one barb has been drawn to unit number 2, which corresponds       
C            to GMETA.CGM, the GKS output meta code file.                       
C                                                                               
C assumptions - this routine assumes that GKS has been opened and a work-       
C            station has been set.                                              
C                                                                               
C revised by - Jeremy Asbill on May 3, 1990.                                    
C**************************************************************************     
                                                                                
C  Input variable declarations ...                                              
                                                                                
      real      xbase,ybase,                                                    
     *          u,v                                                             
                                                                                
C  Local parameter ...                                                          
C    sc specifies in the normalized (fractional) graphics coordinate            
C       system how long the barb shaft is to be                                 
C    Coordinate systems are explained in NCAR Graphics User's Guide Version     
C    2.00 on page 46.                                                           
                                                                                
c      parameter (sc = 0.05493)                                                 
      parameter (sc = 0.04493)                                                  
                                                                                
C  Local variable declarations ...                                              
                                                                                
      integer   llsv                 ! save variable, scaling for SET           
      logical done                   ! T => subroutine ends, F => loop again    
      real      windvct,             ! wind vector magnitude                    
     *          flsv,frsv,fbsv,ftsv, ! save variables, fractional coordinates   
     *          ulsv,ursv,ubsv,utsv, ! save variables, incoming user coord's    
     *          newxbase,            ! fractional x coord. for barb base        
     *          newybase,            ! fractional y coord. for barb base        
     *          xcomp,               ! x component of graphical vector (shaft)  
     *          ycomp,               ! y component of graphical vector (shaft)  
     *          pk,                  ! place keeper                             
     *          fethlenx,            ! x component of graphical vect. (feather) 
     *          fethleny             ! y component of graphical vect. (feather) 
                                                                                
C  Local array declarations ...                                                 
                                                                                
      integer   ijunk(5)             ! calculation array for SFSGFA             
      real      pointx(3),           ! used to specify points to draw between   
     *          pointy(3),           ! used to specify points to draw between   
     *          junk(7)              ! calculation array for SFSGFA             
                                                                                
C***************************** subroutine begin ******************************C 
                                                                                
C  Initialize loop, boolean indicator                                           
                                                                                
      done = .false.                                                            
                                                                                
C  Set color index for polylines to white                                       
                                                                                
c     call gsplci (2)                                                           
c                                                                               
C  Calculate the wind vector magnitude in .not. knots                           
                                                                                
      if ((u .eq. 0) .and. (v .eq. 0)) then                                     
        windvct = 1.0                                                           
      else                                                                      
        windvct = sqrt(u**2 + v**2)                                             
c        windvct = sqrt(u**2 + v**2) * 1.94                                     
      end if                                                                    
                                                                                
C  Save incoming user coordinates and change back to normalized coordinates     
C  Documentation for SET and GETSET can be found in NCAR Graphics User's        
C  Guide Version 2.00 on pages 49 (GETSET) and 53 (SET).                        
                                                                                
      call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)                
      call set    (flsv,frsv,fbsv,ftsv, 0.0, 1.0, 0.0, 1.0, 1)                  
                                                                                
C  Determine where the base of the barb is in the normalized coordinates        
                                                                                
      newxbase = (xbase - ulsv)/(ursv - ulsv)                                   
      newybase = (ybase - ubsv)/(utsv - ubsv)                                   
                                                                                
C  Calculate the x distance and y distance from the base of the barb that       
C  defines the barbs tip (normalized coord's)                                   
                                                                                
       xcomp = -sc * u       /windvct                                           
       ycomp = -sc * v       /windvct                                           
c        xcomp = -sc * u * 1.94/windvct                                         
c        ycomp = -sc * v * 1.94/windvct                                         
                                                                                
C  Determine the actual location in normalized coordinates of the barb's tip    
                                                                                
      pointx(1) = newxbase + xcomp                                              
      pointy(1) = newybase + ycomp                                              
                                                                                
C  Draw the barb shaft, documentation for the LINE subroutine can be found      
C  in NCAR Graphics User's Guide Version 2.00 on page 50                        
                                                                                
      call line (newxbase,newybase,pointx(1),pointy(1))                         
                                                                                
C  Determine the feather length                                                 
                                                                                
      fethlenx = 0.3 * ycomp                                                    
      fethleny = -0.3 * xcomp                                                   
                                                                                
C  Set the place keeper and boost the wind magnitude                            
                                                                                
      pk = 0.9                                                                  
      windvct = windvct + 2.5                                                   
                                                                                
C  Begin making feathers                                                        
                                                                                
10    continue                                                                  
                                                                                
C    Draw a flag for every 50 knots wind magnitude                              
                                                                                
      if (windvct .ge. 50.0) then                                               
                                                                                
C      Determine the position of the flag tip, point_(2)                        
C      and determine position where flag bottom meets the shaft, point_(3)      
                                                                                
        pointx(2) = pointx(1) + fethlenx + 0.0005                               
        pointy(2) = pointy(1) + fethleny + 0.0005                               
        pointx(3) = pk * xcomp + newxbase                                       
        pointy(3) = pk * ycomp + newybase                                       
                                                                                
C      Draw flag                                                                
                                                                                
        call line (pointx(1),pointy(1),pointx(2),pointy(2))                     
        call line (pointx(3),pointy(3),pointx(2),pointy(2))                     
                                                                                
C      Fill in flag, documentation for SFSGFA can be found in NCAR              
C      Graphics Guide to New Utilities Version 3.00 on page 4-8                 
                                                                                
        call sfsetr ('SP',0.000001)                                             
        call sfsgfa (pointx,pointy,3,junk,5,ijunk,7,2)                          
                                                                                
C      Remove 50 knots from wind magnitude (already drawn in)                   
                                                                                
        windvct = windvct - 50.0                                                
                                                                                
C      Determine new begin point for next flag or feather                       
                                                                                
        pk = pk - 0.05                                                          
        pointx(1) = pk * xcomp + newxbase                                       
        pointy(1) = pk * ycomp + newybase                                       
        pk = pk - 0.1                                                           
                                                                                
C    Draw a full feather for wind magnitude of every 10 knots                   
                                                                                
      else if (windvct .ge. 10.0) then                                          
                                                                                
C      Calculate position of feather end                                        
                                                                                
        pointx(2) = pointx(1) + fethlenx + 0.0005                               
        pointy(2) = pointy(1) + fethleny + 0.0005                               
                                                                                
C      Draw feather                                                             
                                                                                
        call line (pointx(1),pointy(1),pointx(2),pointy(2))                     
                                                                                
C      Remove 10 knots from wind magnitude (already drawn in)                   
                                                                                
        windvct = windvct - 10.0                                                
                                                                                
C      Determine new start point for next feather or flag                       
                                                                                
        pointx(1) = pk * xcomp + newxbase                                       
        pointy(1) = pk * ycomp + newybase                                       
        pk = pk - 0.1                                                           
                                                                                
C    Draw a half feather for every 5 knots of wind magnitude                    
                                                                                
      else if (windvct .ge. 5.0) then                                           
                                                                                
C      Calculate position of tip of half feather                                
                                                                                
        pointx(2) = pointx(1) + 0.5 * fethlenx + 0.0005                         
        pointy(2) = pointy(1) + 0.5 * fethleny + 0.0005                         
                                                                                
C      Draw in feather                                                          
                                                                                
        call line (pointx(1),pointy(1),pointx(2),pointy(2))                     
                                                                                
C      Tell loop to quit                                                        
                                                                                
        done = .true.                                                           
      else                                                                      
        done = .true.                                                           
      end if                                                                    
                                                                                
C  If there is still more wind magnitude (>= 5 knots) loop again                
                                                                                
      if (.not. done) goto 10                                                   
                                                                                
C  Reset user coordinates to the incoming values                                
                                                                                
      call set (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)                   
c                                                                               
      return                                                                    
      end                                                                       
