MODULE module_sf_bem
! -----------------------------------------------------------------------
!  Variables and constants used in the BEM module
! -----------------------------------------------------------------------
         
        real emins		!emissivity of the internal walls
        parameter (emins=0.9) 
        real albins	        !albedo of the internal walls
!!      parameter (albins=0.5)
        parameter (albins=0.3)

        real thickwin           !thickness of the window [m] 
        parameter (thickwin=0.006)
        real cswin		!Specific heat of the windows [J/(m3.K)]
        parameter(cswin= 2.268e+06)

        real temp_rat            !power of the A.C. heating/cooling the indoor air [K/s]
        parameter(temp_rat=0.001)

        real hum_rat            !power of the A.C. drying/moistening the indoor air [(Kg/kg)/s]
        parameter(hum_rat=1.e-06)


    CONTAINS

!====6================================================================72
!====6================================================================72	
	
	subroutine BEM(nzcanm,nlev,nhourday,dt,bw,bl,dzlev,            &
                       nwal,nflo,nrof,ngrd,hswalout,gswal,             &
                       hswinout,hsrof,gsrof,                           &
                       latent,sigma,albwal,albwin,albrof,              &
     		       emrof,emwal,emwin,rswal,rlwal,rair,cp,          &
     		       rhoout,tout,humout,press,                       &
     		       rs,rl,dzwal,cswal,kwal,pwin,cop,beta,sw_cond,   &
                       timeon,timeoff,targtemp,gaptemp,targhum,gaphum, &
                       perflo,hsesf,hsequip,dzflo,                     &
     		       csflo,kflo,dzgrd,csgrd,kgrd,dzrof,csrof,        &
     		       krof,tlev,shumlev,twal,twin,tflo,tgrd,trof,     &
     		       hsout,hlout,consump,hsvent,hlvent)


! ---------------------------------------------------------------------
	implicit none
	
! ---------------------------------------------------------------------	
!		       TOP
!	      ---------------------	
!	      !	----------------- !--->roof	(-) : level number	
!	      !	!		! !		rem: the windows are given 
!	      !	!---------------! !                  with respect to the 
!	      !	!---------------! !                  vertical walls-->win(2) 
!	   (n)! !(1)	     (1)!-!(n)
!	      !	!---------------! !		2D vision of the building
!   WEST      ! !-------4-------! !	EAST
!	    I ! ! 1    ilev    2! ! II               ^
!	      !	!-------3--------! !		     !  	
!	      ! !---------------! !--->floor 1	     ! 				
!	      !	!		! !                  !
!	      ! !		! !                  !
!	      !	----------------- !          <--------------(n)  	
!	      ------------------------>ground	------------(1)
!		     BOTTOM
!				i(6)			
!			        i
!                     +---------v-----+ 
!                    /|              /|    3D vision of a room	
!                   / | 4           / |		
!                  /  |            /  |
!                 /   |           /   |
!                /    |          /    |
!               +---------------+     |
!               |  1   |        |  2  |
!               |     +---------|-----+
!       dzlev   |    /          |    /
!               |   /    3      |   /
!               |  /bw          |  /
!               | /             | /  
!               |/              |/
!               +---------------+
!                     ^ bl
!		      i          
!                     i
!		     (5)	
!-----------------------------------------------------------------------


! Input:
! -----	

	real dt				!time step [s]
                                       
        integer nzcanm                  !Maximum number of vertical levels in the urban grid
	integer nlev			!number of floors in the building
	integer nwal                    !number of levels inside the wall
	integer nrof                    !number of levels inside the roof
	integer nflo                    !number of levels inside the floor
	integer ngrd                    !number of levels inside the ground
	real dzlev			!vertical grid resolution [m]			
	real bl				!Building length [m]
	real bw                         !Building width [m]
	
	real albwal			!albedo of the walls 				
	real albwin		 	!albedo of the windows
	real albrof			!albedo of the roof
	
	real emwal 	          	!emissivity of the walls
	
	real emrof			!emissivity of the roof
        real emwin                      !emissivity of the windows

	real pwin                       !window proportion
	real,    intent(in) :: cop      !Coefficient of performance of the A/C systems
	real,    intent(in) :: beta     !Thermal efficiency of the heat exchanger
        integer, intent(in) :: sw_cond  ! Air Conditioning switch
        real,    intent(in) :: timeon   ! Initial local time of A/C systems
        real,    intent(in) :: timeoff  ! Ending local time of A/C systems
        real,    intent(in) :: targtemp ! Target temperature of A/C systems
        real,    intent(in) :: gaptemp  ! Comfort range of indoor temperature
        real,    intent(in) :: targhum  ! Target humidity of A/C systems
        real,    intent(in) :: gaphum   ! Comfort range of specific humidity
        real,    intent(in) :: perflo   ! Peak number of occupants per unit floor area
        real,    intent(in) :: hsesf    ! 
        real,    intent(in) :: hsequip(24) ! 
	
	real cswal(nwal)		!Specific heat of the wall [J/(m3.K)] 
	
	real csflo(nflo)		!Specific heat of the floor [J/(m3.K)]
	real csrof(nrof)		!Specific heat of the roof [J/(m3.K)] 
	real csgrd(ngrd)		!Specific heat of the ground [J/(m3.K)]
	
	real kwal(nwal+1)		!Thermal conductivity in each layers of the walls (face) [W/(m.K)]
	real kflo(nflo+1)		!Thermal diffusivity in each layers of the floors (face) [W/(m.K)]
	real krof(nrof+1)		!Thermal diffusivity in each layers of the roof (face) [W/(m.K)]
	real kgrd(ngrd+1)		!Thermal diffusivity in each layers of the ground (face) [W/(m.K)]
	
	real dzwal(nwal)		!Layer sizes of walls [m]
	real dzflo(nflo)		!Layer sizes of floors [m]
	real dzrof(nrof)		!Layer sizes of roof [m]
	real dzgrd(ngrd)		!Layer sizes of ground [m]
	
	real latent                      !latent heat of evaporation [J/Kg]	


	real rs				!external short wave radiation [W/m2]
	real rl				!external long wave radiation [W/m2]
	real rswal(4,nzcanm)		!short wave radiation reaching the exterior walls [W/m2]
        real rlwal(4,nzcanm)		!long wave radiation reaching the walls [W/m2]	
	real rhoout(nzcanm)		!exterior air density [kg/m3]
	real tout(nzcanm)		!external temperature [K]
	real humout(nzcanm)		!absolute humidity [Kgwater/Kgair]
	real press(nzcanm)		!external air pressure [Pa]
	
	real hswalout(4,nzcanm)	        !outside walls sensible heat flux [W/m2]
	real hswinout(4,nzcanm)	        !outside window sensible heat flux [W/m2]
	real hsrof			!Sensible heat flux at the roof [W/m2]
	
	real rair			!ideal gas constant  [J.kg-1.K-1]
	real sigma			!parameter (wall is not black body) [W/m2.K4]
	real cp				!specific heat of air [J/kg.K]
       
	
!Input-Output
!------------
	real tlev(nzcanm)		!temperature of the floors [K] 
	real shumlev(nzcanm)		!specific humidity of the floor [kg/kg]
	real twal(4,nwal,nzcanm)	!walls temperatures [K]
	real twin(4,nzcanm)		!windows temperatures [K]	
	real tflo(nflo,nzcanm-1)	!floor temperatures [K]
	real tgrd(ngrd)		        !ground temperature [K]
	real trof(nrof)		        !roof temperature [K]
	real hsout(nzcanm)		!sensible heat emitted outside the floor [W]
	real hlout(nzcanm)		!latent heat emitted outside the floor [W]
        real consump(nzcanm)            !Consumption for the a.c. in each floor [W]
	real hsvent(nzcanm)		!sensible heat generated by natural ventilation [W]
	real hlvent(nzcanm)		!latent heat generated by natural ventilation [W] 
        real gsrof                      !heat flux flowing inside the roof [W/m²]
        real gswal(4,nzcanm)             !heat flux flowing inside the floors [W/m²]

! Local:
! -----
	integer swwal                   !swich for the physical coefficients calculation
	integer ilev			!index for rooms	
	integer iwal			!index for walls
	integer iflo			!index for floors
	integer ivw			!index for vertical walls
        integer igrd                    !index for ground
        integer irof                    !index for roof 
        real hseqocc(nzcanm)		!sensible heat generated by equipments and occupants [W]
	real hleqocc(nzcanm)		!latent heat generated by occupants [W]
        real hscond(nzcanm)		!sensible heat generated by wall conduction [W]
        real hslev(nzcanm)		!sensible heat flux generated inside the room [W]
        real hllev(nzcanm)		!latent heat flux generatd inside the room [W]
	real surwal(6,nzcanm)	        !Surface of the walls [m2]
	real surwal1D(6)	        !wall surfaces of a generic room [m2]
	real rsint(6)		        !short wave radiation reaching the indoor walls[W/m2]
	real rswalins(6,nzcanm)	        !internal short wave radiation for the building [W/m2]
	real twin1D(4)		        !temperature of windows for a particular room [K]
	real twal_int(6)		!temperature of the first internal layers of a room [K]
	real rlint(6)		        !internal wall long wave radiation [w/m2]
	real rlwalins(6,nzcanm)	        !internal long wave radiation for the building [W/m2]	
	real hrwalout(4,nzcanm)	        !external radiative flux to the walls [W/m2]
	real hrwalins(6,nzcanm)	        !inside radiative flux to the walls [W/m2] 
	real hrwinout(4,nzcanm)	        !external radiative flux to the window [W/m2]
	real hrwinins(4,nzcanm)	        !inside radiative flux to the window [W/m2] 
	real hrrof			!external radiative flux to the roof [W/m2]
	real hs
        real hsneed(nzcanm)		!sensible heat needed by the room [W]
	real hlneed(nzcanm)		!latent heat needed by the room [W]	
        real hswalins(6,nzcanm)	        !inside walls sensible heat flux [W/m2]
	real hswalins1D(6)
	real hswinins(4,nzcanm)	        !inside window sensible heat flux [W/m2]
	real hswinins1D(4)	
	real htot(2)			!total heat flux at the wall [W/m2]
	real twal1D(nwal)
	real tflo1D(nflo)	
        real tgrd1D(ngrd)
        real trof1D(nrof)
	real rswal1D(4)
	real Qb				!Overall heat capacity of the indoor air [J/K]
	real vollev			!volume of the room [m3]
	real rhoint			!density of the internal air [Kg/m3]
	real cpint			!specific heat of the internal air [J/kg.K]
        real humdry                     !specific humidiy of dry air [kg water/kg dry air]
	real radflux                    !Function to compute the total radiation budget
	real consumpbuild               !Energetic consumption for the entire building [KWh/s]
        real hsoutbuild                 !Total sensible heat ejected into the atmosphere[W]
                                        !by the air conditioning system and per building
        real nhourday                   !number of hours from midnight, local time
!--------------------------------------------
!Initialization
!--------------------------------------------

       do ilev=1,nzcanm
          hseqocc(ilev)=0.
          hleqocc(ilev)=0.
          hscond(ilev)=0.
          hslev(ilev)=0.
          hllev(ilev)=0.
       enddo	

!Calculation of the surfaces of the building 
!--------------------------------------------
	
       
	do ivw=1,6
	do ilev=1,nzcanm
	 surwal(ivw,ilev)=1.   !initialisation
	end do
	end do

	do ilev=1,nlev
	  do ivw=1,2
	   surwal(ivw,ilev)=dzlev*bw
	  end do
	  do ivw=3,4
	   surwal(ivw,ilev)=dzlev*bl
	  end do
	  do ivw=5,6 		
	   surwal(ivw,ilev)=bw*bl
	  end do 
	end do


! Calculation of the short wave radiations at the internal walls
! ---------------------------------------------------------------
	

	do ilev=1,nlev	
	  
	  do ivw=1,4
	    rswal1D(ivw)=rswal(ivw,ilev)
	  end do	

	  do ivw=1,6
	    surwal1D(ivw)=surwal(ivw,ilev)
	  end do 		
	
	  call int_rsrad(albwin,albins,pwin,rswal1D,&
                         surwal1D,bw,bl,dzlev,rsint)

	  do ivw=1,6
	    rswalins(ivw,ilev)=rsint(ivw)
	  end do
          
	end do !ilev
	
	 

! Calculation of the long wave radiation at the internal walls
!-------------------------------------------------------------


!Intermediate rooms
       
       if (nlev.gt.2) then
	do ilev=2,nlev-1

	  do ivw=1,4
	    twin1D(ivw)=twin(ivw,ilev)
	    twal_int(ivw)=twal(ivw,1,ilev)
	  end do
	    
	   twal_int(5)=tflo(nflo,ilev-1)
	   twal_int(6)=tflo(1,ilev)		
		 
	   call int_rlrad(emins,emwin,sigma,twal_int,twin1D,&
     			  pwin,bw,bl,dzlev,rlint)
	  
	  
	  do ivw=1,6
	    rlwalins(ivw,ilev)=rlint(ivw)
	  end do
	    
	end do	!ilev 
      end if	 
	

      if (nlev.ne.1) then  

!bottom room

	  do ivw=1,4
	    twin1D(ivw)=twin(ivw,1)
	    twal_int(ivw)=twal(ivw,1,1)
	  end do
	  
	  twal_int(5)=tgrd(ngrd)
	  twal_int(6)=tflo(1,1)		
	  
	  						  	   
	   call int_rlrad(emins,emwin,sigma,twal_int,twin1D,&
     			  pwin,bw,bl,dzlev,rlint)
	  
	  do ivw=1,6
	    rlwalins(ivw,1)=rlint(ivw)
	  end do	  
            
!top room
	 
	  do ivw=1,4
	    twin1D(ivw)=twin(ivw,nlev)
	    twal_int(ivw)=twal(ivw,1,nlev)
	  end do
	  
	  twal_int(5)=tflo(nflo,nlev-1)
	  twal_int(6)=trof(1)		
	  
					
	   call int_rlrad(emins,emwin,sigma,twal_int,twin1D,&
     			  pwin,bw,bl,dzlev,rlint)
	  
	  do ivw=1,6
	    rlwalins(ivw,nlev)=rlint(ivw)
	  end do
	  
      else   !Top <---> Bottom
	  
	  do ivw=1,4
	    twin1D(ivw)=twin(ivw,1)
	    twal_int(ivw)=twal(ivw,1,1)
	  end do
	  
	  twal_int(5)=tgrd(ngrd)      
      	  twal_int(6)=trof(1)
	  
	  call int_rlrad(emins,emwin,sigma,twal_int,twin1D, &
     			 pwin,bw,bl,dzlev,rlint)
     	  
	  do ivw=1,6
	    rlwalins(ivw,1)=rlint(ivw)
	  end do
	
      end if  
	

! Calculation of the radiative fluxes
! -----------------------------------

!External vertical walls and windows

        do ilev=1,nlev
	 do ivw=1,4	 
	 call radfluxs(radflux,albwal,rswal(ivw,ilev),     &
     	                    emwal,rlwal(ivw,ilev),sigma,   &
                            twal(ivw,nwal,ilev))
	
         hrwalout(ivw,ilev)=radflux
      	 						
	 hrwinout(ivw,ilev)=emwin*rlwal(ivw,ilev)- &
     	                    emwin*sigma*(twin(ivw,ilev)**4)
	 
	 
	 end do ! ivw
	end do  ! ilev
	
!Roof

        call radfluxs(radflux,albrof,rs,emrof,rl,sigma,trof(nrof))

        hrrof=radflux

!Internal walls for intermediate rooms

      if(nlev.gt.2) then
       
	do ilev=2,nlev-1
	 do ivw=1,4
         
	 call radfluxs(radflux,albins,rswalins(ivw,ilev),     &
     	                    emins,rlwalins(ivw,ilev),sigma,   &
                            twal(ivw,1,ilev))
	 
	 hrwalins(ivw,ilev)=radflux

	 end do !ivw						

	 call radfluxs(radflux,albins,rswalins(5,ilev), &
     	                      emins,rlwalins(5,ilev),sigma,&
                              tflo(nflo,ilev-1))

         hrwalins(5,ilev)=radflux

         call radfluxs(radflux,albins,rswalins(6,ilev), &
                              emins,rlwalins(6,ilev),sigma,&
                              tflo(1,ilev))
         hrwalins(6,ilev)=radflux

       end do !ilev

      end if 	


!Internal walls for the bottom and the top room	 
!
      if (nlev.ne.1) then 

!bottom floor

	 do ivw=1,4

	    call radfluxs(radflux,albins,rswalins(ivw,1),  &
     	                    emins,rlwalins(ivw,1),sigma,   &
                            twal(ivw,1,1))
	
            hrwalins(ivw,1)=radflux

	 end do
	
	
	  call radfluxs(radflux,albins,rswalins(5,1),&
                           emins,rlwalins(5,1),sigma,&    !bottom
                           tgrd(ngrd))

          hrwalins(5,1)=radflux

	   
          call radfluxs(radflux,albins,rswalins(6,1),&
     	                   emins,rlwalins(6,1),sigma,&
                           tflo(1,1))  
	 
          hrwalins(6,1)=radflux

!roof floor

         do ivw=1,4
   
          call radfluxs(radflux,albins,rswalins(ivw,nlev),     &
     	                        emins,rlwalins(ivw,nlev),sigma,&
                                twal(ivw,1,nlev))

	  hrwalins(ivw,nlev)=radflux

	 end do                                          !top

	
         call radfluxs(radflux,albins,rswalins(5,nlev),    &
     	                      emins,rlwalins(5,nlev),sigma,&
                              tflo(nflo,nlev-1))

         hrwalins(5,nlev)=radflux

	 call radfluxs(radflux,albins,rswalins(6,nlev), &
                              emins,rlwalins(6,nlev),sigma,&
                              trof(1))

         hrwalins(6,nlev)=radflux
      
      else       ! Top <---> Bottom room
      
	 do ivw=1,4

	    call radfluxs(radflux,albins,rswalins(ivw,1),&
     	                    emins,rlwalins(ivw,1),sigma, &
                            twal(ivw,1,1))

            hrwalins(ivw,1)=radflux

         end do
     
     	    call radfluxs(radflux,albins,rswalins(5,1),&
                           emins,rlwalins(5,1),sigma,  &
                           tgrd(ngrd))

            hrwalins(5,1)=radflux
     
     	    call radfluxs(radflux,albins,rswalins(6,nlev),     &
                                  emins,rlwalins(6,nlev),sigma,&
                                  trof(1))
            hrwalins(6,1)=radflux

      end if
      
		
!Windows

	 do ilev=1,nlev
	  do ivw=1,4
	     hrwinins(ivw,ilev)=emwin*rlwalins(ivw,ilev)-    &
                                emwin*sigma*(twin(ivw,ilev)**4)
	  end do
	 end do
	
		
! Calculation of the sensible heat fluxes
! ---------------------------------------

!Vertical fluxes for walls
	
	do ilev=1,nlev
         do ivw=1,4
		
               call hsinsflux (2,2,tlev(ilev),twal(ivw,1,ilev),hs)		
	       
               hswalins(ivw,ilev)=hs 
         
         end do ! ivw     
        end do ! ilev
       
      
!Vertical fluxes for windows

	do ilev=1,nlev

         do ivw=1,4
	 
	       call hsinsflux (2,1,tlev(ilev),twin(ivw,ilev),hs)
	       
               hswinins(ivw,ilev)=hs 
			
         end do ! ivw	
	
	end do !ilev      

!Horizontal fluxes
       
      if (nlev.gt.2) then
       
        do ilev=2,nlev-1
                
	       call hsinsflux (1,2,tlev(ilev),tflo(nflo,ilev-1),hs)

	       hswalins(5,ilev)=hs
            
	       call hsinsflux (1,2,tlev(ilev),tflo(1,ilev),hs)

	       hswalins(6,ilev)=hs

        end do ! ilev
       
      end if
       
      if (nlev.ne.1) then
       
       	        call hsinsflux (1,2,tlev(1),tgrd(ngrd),hs)

		hswalins(5,1)=hs				!Bottom room
		
		call hsinsflux (1,2,tlev(1),tflo(1,1),hs)

		hswalins(6,1)=hs				
	 
       	        call hsinsflux (1,2,tlev(nlev),tflo(nflo,nlev-1),hs)

		hswalins(5,nlev)=hs			        !Top room

		call hsinsflux (1,2,tlev(nlev),trof(1),hs)

		hswalins(6,nlev)=hs	       
      
      else  ! Bottom<--->Top 
      
                call hsinsflux (1,2,tlev(1),tgrd(ngrd),hs)
		
		hswalins(5,1)=hs
		
		call hsinsflux (1,2,tlev(nlev),trof(1),hs)
		
		hswalins(6,nlev)=hs
      
      end if


!Calculation of the temperature for the different surfaces 
! --------------------------------------------------------

! Vertical walls	
        
       swwal=1
       do ilev=1,nlev
        do ivw=1,4  

	   htot(1)=hswalins(ivw,ilev)+hrwalins(ivw,ilev)	
           htot(2)=hswalout(ivw,ilev)+hrwalout(ivw,ilev)
           gswal(ivw,ilev)=htot(2)

	   do iwal=1,nwal
	      twal1D(iwal)=twal(ivw,iwal,ilev)
	   end do
	  
	   call wall(swwal,nwal,dt,dzwal,kwal,cswal,htot,twal1D)
	
	   do iwal=1,nwal
	      twal(ivw,iwal,ilev)=twal1D(iwal)
	   end do
           
	end do ! ivw
       end do ! ilev
       
! Windows

       do ilev=1,nlev
        do ivw=1,4
       
         htot(1)=hswinins(ivw,ilev)+hrwinins(ivw,ilev)	
         htot(2)=hswinout(ivw,ilev)+hrwinout(ivw,ilev)	

         twin(ivw,ilev)=twin(ivw,ilev)+(dt/(cswin*thickwin))* &
                        (htot(1)+htot(2))
	
	end do ! ivw
       end do ! ilev   

! Horizontal floors


      if (nlev.gt.1) then
       swwal=1
       do ilev=1,nlev-1
 
          htot(1)=hrwalins(6,ilev)+hswalins(6,ilev)
          htot(2)=hrwalins(5,ilev+1)+hswalins(5,ilev+1)	

	  do iflo=1,nflo
	     tflo1D(iflo)=tflo(iflo,ilev)
	  end do
        
	  call wall(swwal,nflo,dt,dzflo,kflo,csflo,htot,tflo1D)
	
	 do iflo=1,nflo
	    tflo(iflo,ilev)=tflo1D(iflo)
	 end do

       end do ! ilev
      end if 
        

! Ground 	
        
	swwal=1

	htot(1)=0.	!Diriclet b.c. at the internal boundary    
	htot(2)=hswalins(5,1)+hrwalins(5,1)   
   
        do igrd=1,ngrd
           tgrd1D(igrd)=tgrd(igrd)
        end do

         call wall(swwal,ngrd,dt,dzgrd,kgrd,csgrd,htot,tgrd1D)

        do igrd=1,ngrd
           tgrd(igrd)=tgrd1D(igrd)
        end do

        
! Roof
        
      swwal=1    

      htot(1)=hswalins(6,nlev)+hrwalins(6,nlev)     	
      htot(2)=hsrof+hrrof     
      gsrof=htot(2)

      do irof=1,nrof
         trof1D(irof)=trof(irof)
      end do     
      
      call wall(swwal,nrof,dt,dzrof,krof,csrof,htot,trof1D)
 
      do irof=1,nrof
         trof(irof)=trof1D(irof)
      end do
      
! Calculation of the heat fluxes and of the temperature of the rooms
! ------------------------------------------------------------------

 	do ilev=1,nlev
	  	  
	 !Calculation of the heat generated by equipments and occupants
	 
	 call fluxeqocc(nhourday,bw,bl,perflo,hsesf,hsequip,hseqocc(ilev),hleqocc(ilev))

     	 !Calculation of the heat generated by natural ventilation
	
	  vollev=bw*bl*dzlev
          humdry=shumlev(ilev)/(1.-shumlev(ilev))
	  rhoint=(press(ilev))/(rair*(1.+0.61*humdry)*tlev(ilev))
	  cpint=cp*(1.+0.84*humdry)
          
 	  
	  call fluxvent(cpint,rhoint,vollev,tlev(ilev),tout(ilev),     &
                        latent,humout(ilev),rhoout(ilev),shumlev(ilev),&
                        beta,hsvent(ilev),hlvent(ilev))
	      
         !Calculation of the heat generated by conduction
	  
	   do iwal=1,6
	     hswalins1D(iwal)=hswalins(iwal,ilev)
	     surwal1D(iwal)=surwal(iwal,ilev)
	  end do
	  
	   do iwal=1,4
	     hswinins1D(iwal)=hswinins(iwal,ilev)
	   end do
	
	  call fluxcond(hswalins1D,hswinins1D,surwal1D,pwin,&
                        hscond(ilev))

	!Calculation of the heat generated inside the room
 	
	  call fluxroo(hseqocc(ilev),hleqocc(ilev),hsvent(ilev), &
               hlvent(ilev),hscond(ilev),hslev(ilev),hllev(ilev))

	  
	!Evolution of the temperature and of the specific humidity 

	  Qb=rhoint*cpint*vollev

        ! temperature regulation

          call regtemp(sw_cond,nhourday,dt,Qb,hslev(ilev),       &
                       tlev(ilev),timeon,timeoff,targtemp,gaptemp,hsneed(ilev))

        ! humidity regulation 

	  call reghum(sw_cond,nhourday,dt,vollev,rhoint,latent, &
                      hllev(ilev),shumlev(ilev),timeon,timeoff,&
                      targhum,gaphum,hlneed(ilev))
!
!performance of the air conditioning system for the test
!	
	        
          call air_cond(hsneed(ilev),hlneed(ilev),dt, &
                        hsout(ilev),hlout(ilev),consump(ilev), cop)
    	         	
 	  tlev(ilev)=tlev(ilev)+(dt/Qb)*(hslev(ilev)-hsneed(ilev))
          	  	  
	  shumlev(ilev)=shumlev(ilev)+(dt/(vollev*rhoint*latent))* &
                        (hllev(ilev)-hlneed(ilev))
           
	end do !ilev
        
        call consump_total(nzcanm,nlev,consumpbuild,hsoutbuild, &
                           hsout,consump)
                
      return
      end subroutine BEM

!====6=8===============================================================72
!====6=8===============================================================72

	subroutine wall(swwall,nz,dt,dz,k,cs,flux,temp)
	
!______________________________________________________________________

!The aim of this subroutine is to solve the 1D heat fiffusion equation
!for roof, walls and streets:
!
!	dT/dt=d/dz[K*dT/dz] where:
!
!	-T is the surface temperature(wall, street, roof)
!      	-Kz is the heat diffusivity inside the material.
!
!The resolution is done implicitly with a FV discretisation along the
!different layers of the material:

!	____________________________
!     n             *
!                   *
!                   *
!     	____________________________
!    i+2
!              	    I+1                 
!	____________________________        
!    i+1        
!                    I                ==>   [T(I,n+1)-T(I,n)]/DT= 
!	____________________________        [F(i+1)-F(i)]/DZI
!    i
!                   I-1               ==> A*T(n+1)=B where:
!	____________________________         
!   i-1              *                   * A is a TRIDIAGONAL matrix.
!                    *                   * B=T(n)+S takes into account the sources.
!                    *
!     1	____________________________

!________________________________________________________________

	implicit none
		
!Input:
!-----
	integer nz		!Number of layers inside the material
	real dt			!Time step
	real dz(nz)		!Layer sizes [m]
	real cs(nz)		!Specific heat of the material [J/(m3.K)] 
	real k(nz+1)		!Thermal conductivity in each layers (face) [W/(m.K)]
	real flux(2)		!Internal and external flux terms.

!Input-Output:
!-------------

	integer swwall          !swich for the physical coefficients calculation
	real temp(nz)		!Temperature at each layer

!Local:
!-----	

      real a(-1:1,nz)          !  a(-1,*) lower diagonal      A(i,i-1)
                               !  a(0,*)  principal diagonal  A(i,i)
                               !  a(1,*)  upper diagonal      A(i,i+1).
      
      real b(nz)	       !Coefficients of the second term.	
      real k1(20)
      real k2(20)
      real kc(20)
      save k1,k2,kc
      integer iz
        	
!________________________________________________________________
!
!Calculation of the coefficients
	
	if (swwall.eq.1) then
	
           if (nz.gt.20) then
              write(*,*) 'number of layers in the walls/roofs too big ',nz
              write(*,*) 'please decrease under of',20
              stop
           endif

	   call wall_coeff(nz,dt,dz,cs,k,k1,k2,kc)
	   swwall=0

	end if
 	
!Computation of the first value (iz=1) of A and B:
	
		 a(-1,1)=0.
		 a(0,1)=1+k2(1)
		 a(1,1)=-k2(1)

                 b(1)=temp(1)+flux(1)*kc(1)

!!
!!We can fixed the internal temperature	
!!
!!		 a(-1,1)=0.
!!		 a(0,1)=1
!!		 a(1,1)=0.		 	 
!!		 
!!		 b(1)=temp(1)
!!
!Computation of the internal values (iz=2,...,n-1) of A and B:

	do iz=2,nz-1
		a(-1,iz)=-k1(iz)
		a(0,iz)=1+k1(iz)+k2(iz)
     		a(1,iz)=-k2(iz)
		b(iz)=temp(iz)
	end do		

!Computation of the external value (iz=n) of A and B:
	
		a(-1,nz)=-k1(nz)
		a(0,nz)=1+k1(nz)
		a(1,nz)=0.
	
		b(nz)=temp(nz)+flux(2)*kc(nz)

!Resolution of the system A*T(n+1)=B

	call tridia(nz,a,b,temp)

        return
	end  subroutine wall	

!====6=8===============================================================72
!====6=8===============================================================72

	subroutine wall_coeff(nz,dt,dz,cs,k,k1,k2,kc)

	implicit none
	
!---------------------------------------------------------------------
!Input
!-----
	integer nz		!Number of layers inside the material
	real dt			!Time step
	real dz(nz)		!Layer sizes [m]
	real cs(nz)		!Specific heat of the material [J/(m3.K)] 
	real k(nz+1)		!Thermal diffusivity in each layers (face) [W/(m.K)]


!Input-Output
!------------

	real flux(2)		!Internal and external flux terms.


!Output
!------
        real k1(20)
        real k2(20)
        real kc(20)

!Local
!-----	
	integer iz
	real kf(nz)

!------------------------------------------------------------------

	do iz=2,nz
	 kc(iz)=dt/(dz(iz)*cs(iz))
	 kf(iz)=2*k(iz)/(dz(iz)+dz(iz-1))
	end do 
	
	kc(1)=dt/(dz(1)*cs(1))
        kf(1)=2*k(1)/(dz(1))

	do iz=1,nz
	 k1(iz)=kc(iz)*kf(iz)
	end do
	
	do iz=1,nz-1
	 k2(iz)=kc(iz)*kf(iz+1)*cs(iz)/cs(iz+1)
	end do

	return
	end subroutine wall_coeff

!====6=8===============================================================72  
!====6=8===============================================================72
	subroutine hsinsflux(swsurf,swwin,tin,tw,hsins)	
	
	implicit none
	
! --------------------------------------------------------------------
! This routine computes the internal sensible heat flux.
! The swsurf, makes rhe difference between a vertical and a 
! horizontal surface. 
! The values of the heat conduction coefficients hc are obtained from the book
! "Energy Simulation in Building Design". J.A. Clarke. 
! Adam Hilger, Bristol, 362 pp.
! --------------------------------------------------------------------
!Input
!----
	integer swsurf  !swich for the type of surface (horizontal/vertical) 
        integer swwin   !swich for the type of surface (window/wall)
	real tin	!Inside temperature [K]
	real tw		!Internal wall temperature [K]  	


!Output
!------
	real hsins	!internal sensible heat flux [W/m2]
!Local
!-----
	real hc		!heat conduction coefficient [W/°C.m2]
!--------------------------------------------------------------------

	if (swsurf.eq.2) then	!vertical surface
         if (swwin.eq.1) then
            hc=5.678*0.99        !window surface (smooth surface)
         else
            hc=5.678*1.09        !wall surface (rough surface)
         endif
	 hsins=hc*(tin-tw)	
	endif
	
	if (swsurf.eq.1)  then   !horizontal surface
         if (swwin.eq.1) then
           hc=5.678*0.99        !window surface (smooth surface)
         else
           hc=5.678*1.09        !wall surface (rough surface)
         endif
         hsins=hc*(tin-tw)
        endif 		

	return
	end subroutine hsinsflux
!====6=8===============================================================72  
!====6=8===============================================================72

	subroutine int_rsrad(albwin,albwal,pwin,rswal,&
                             surwal,bw,bl,zw,rsint)
	
! ------------------------------------------------------------------
	implicit none
! ------------------------------------------------------------------	

!Input
!-----
	real albwin		!albedo of the windows
	real albwal		!albedo of the internal wall					
	real rswal(4)		!incoming short wave radiation [W/m2]
        real surwal(6) 		!surface of the indoor walls [m2]
	real bw,bl		!width of the walls [m]
	real zw			!height of the wall [m]
	real pwin               !window proportion
	
!Output
!------
	real rsint(6)		!internal walls short wave radiation [W/m2]

!Local
!-----
	real transmit   !transmittance of the direct/diffused radiation
        real rstr	!solar radiation transmitted through the windows	
        real surtotwal  !total indoor surface of the walls in the room
	integer iw
	real b(6)	!second member for the system
	real a(6,6)	!matrix for the system

!-------------------------------------------------------------------


! Calculation of the solar radiation transmitted through windows
                    
            rstr = 0.
            do iw=1,4
               transmit=1.-albwin
               rstr = rstr+(surwal(iw)*pwin)*(transmit*rswal(iw))
            enddo

!We suppose that the radiation is spread isotropically within the
!room when it passes through the windows, so the flux [W/m²] in every 
!wall is:

            surtotwal=0.
            do iw=1,6
               surtotwal=surtotwal+surwal(iw)
            enddo
            
            rstr=rstr/surtotwal
 		
!Computation of the short wave radiation reaching the internal walls
	
	    call algebra_short(rstr,albwal,albwin,bw,bl,zw,pwin,a,b)
		
	    call gaussjbem(a,6,b,6)
	
            do iw=1,6
               rsint(iw)=b(iw)
            enddo

	    return
	    end subroutine int_rsrad

!====6=8===============================================================72  
!====6=8===============================================================72

	subroutine int_rlrad(emwal,emwin,sigma,twal_int,twin,&
     			     pwin,bw,bl,zw,rlint)
	
! ------------------------------------------------------------------
	implicit none
! ------------------------------------------------------------------	

!Input
!-----

	real emwal	!emissivity of the internal walls
	real emwin	!emissivity of the window
	real sigma	!Stefan-Boltzmann constant [W/m2.K4]
	real twal_int(6)!temperature of the first internal layers of a room [K]
	real twin(4)	!temperature of the windows [K]
	real bw		!width of the wall
	real bl		!length of the wall
	real zw		!height of the wall
	real pwin       !window proportion	

!Output
!------

	real rlint(6)	!internal walls long wave radiation [W/m2]

!Local
!------
	
	real b(6)	!second member vector for the system
	real a(6,6)	!matrix for the system
        integer iw
!----------------------------------------------------------------

!Compute the long wave radiation reachs the internal walls

	call algebra_long(emwal,emwin,sigma,twal_int,twin,pwin,&
                          bw,bl,zw,a,b)
  			  
	call gaussjbem(a,6,b,6)

        do iw=1,6
           rlint(iw)=b(iw)
        enddo
            
	return
	end subroutine int_rlrad	

!====6=8===============================================================72  
!====6=8===============================================================72

	subroutine algebra_short(rstr,albwal,albwin,aw,bw,zw,pwin,a,b)
    
!--------------------------------------------------------------------
!This routine calculates the algebraic system that will be solved for 
!the computation of the total shortwave radiation that reachs every 
!indoor wall in a floor.
!Write the matrix system ax=b to solve
!
!     -Rs(1)+a(1,2)Rs(2)+.................+a(1,6)Rs(6)=-Rs=b(1)
!a(2,1)Rs(1)-      Rs(2)+.................+a(2,6)Rs(6)=-Rs=b(2)
!a(3,1)Rs(1)+a(3,2)Rs(3)-Rs(3)+...........+a(3,6)Rs(6)=-Rs=b(3)
!a(4,1)Rs(1)+.................-Rs(4)+.....+a(4,6)Rs(6)=-Rs=b(4)
!a(5,1)Rs(1)+.......................-Rs(5)+a(5,6)Rs(6)=-Rs=b(5)
!a(6,1)Rs(1)+....................................-R(6)=-Rs=b(6)
!
!This version suppose the albedo of the indoor walls is the same.
!--------------------------------------------------------------------
	implicit none
!--------------------------------------------------------------------

!Input
!-----
	real rstr	!solar radiation transmitted through the windows		
	real albwal	!albedo of the internal walls
	real albwin	!albedo of the windows.
	real bw		!length of the wall
	real aw		!width of the wall
	real zw		!height of the wall
	real fprl_int	!view factor
	real fnrm_int	!view factor
	real pwin       !window proportion
!Output
!------
	real a(6,6)		!Matrix for the system
	real b(6)		!Second member for the system
!Local
!-----
	integer iw,jw	
	real albm               !averaged albedo
!----------------------------------------------------------------

!Initialise the variables

	do iw=1,6
           b(iw)= 0.
	  do jw=1,6
           a(iw,jw)= 0. 
          enddo
        enddo 

!Calculation of the second member b

	do iw=1,6
	 b(iw)=-rstr
	end do	

!Calculation of the averaged albedo

	albm=pwin*albwin+(1-pwin)*albwal
	
!Calculation of the matrix a

            a(1,1)=-1.

            call fprl_ints(fprl_int,aw/bw,zw/bw)

            a(1,2)=albm*fprl_int

            call fnrm_ints(fnrm_int,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))

            a(1,3)=albm*(bw/aw)*fnrm_int

            a(1,4)=a(1,3)

            call fnrm_ints(fnrm_int,zw/aw,bw/aw,(bw*bw+zw*zw)/(aw*aw))

            a(1,5)=albwal*(bw/zw)*fnrm_int

            a(1,6)=a(1,5)


            a(2,1)=a(1,2)
            a(2,2)=-1.
            a(2,3)=a(1,3)
            a(2,4)=a(1,4)
            a(2,5)=a(1,5)
            a(2,6)=a(1,6)
 
	
            call fnrm_ints(fnrm_int,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))

            a(3,1)=albm*(aw/bw)*fnrm_int
	    a(3,2)=a(3,1)
	    a(3,3)=-1.

            call fprl_ints(fprl_int,zw/aw,bw/aw)

	    a(3,4)=albm*fprl_int

            call fnrm_ints(fnrm_int,zw/bw,aw/bw,(aw*aw+zw*zw)/(bw*bw))

	    a(3,5)=albwal*(aw/zw)*fnrm_int
            a(3,6)=a(3,5)
	

            a(4,1)=a(3,1)
            a(4,2)=a(3,2)
            a(4,3)=a(3,4)
            a(4,4)=-1.
            a(4,5)=a(3,5)
            a(4,6)=a(3,6)

            call fnrm_ints(fnrm_int,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw)) 

            a(5,1)=albm*(zw/bw)*fnrm_int
                   
            a(5,2)=a(5,1)

            call fnrm_ints(fnrm_int,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))

            a(5,3)=albm*(zw/aw)*fnrm_int
           	   
            a(5,4)=a(5,3)
            a(5,5)=-1.

            call fprl_ints(fprl_int,aw/zw,bw/zw)

            a(5,6)=albwal*fprl_int


            a(6,1)=a(5,1)
            a(6,2)=a(5,2)
            a(6,3)=a(5,3)
            a(6,4)=a(5,4)
            a(6,5)=a(5,6)
            a(6,6)=-1.
	
	return
	end subroutine algebra_short

!====6=8===============================================================72  
!====6=8===============================================================72

	subroutine algebra_long(emwal,emwin,sigma,twalint,twinint,&
     				pwin,aw,bw,zw,a,b)

!--------------------------------------------------------------------
!This routine computes the algebraic system that will be solved to 
!compute the longwave radiation that reachs the indoor
!walls in a floor. 
!Write the matrix system ax=b to solve
!
!a(1,1)Rl(1)+.............................+Rl(6)=b(1)
!a(2,1)Rl(1)+.................+Rl(5)+a(2,6)Rl(6)=b(2)
!a(3,1)Rl(1)+.....+Rl(3)+...........+a(3,6)Rl(6)=b(3)
!a(4,1)Rl(1)+...........+Rl(4)+.....+a(4,6)Rl(6)=b(4)
!      Rl(1)+.......................+a(5,6)Rl(6)=b(5)
!a(6,1)Rl(1)+Rl(2)+.................+a(6,6)Rl(6)=b(6)
!
!--------------------------------------------------------------------
        implicit none 
	
!--------------------------------------------------------------------

!Input
!-----

	real pwin       !window proportion 
	real emwal	!emissivity of the internal walls
	real emwin	!emissivity of the window
	real sigma	!Stefan-Boltzmann constant [W/m2.K4]
	real twalint(6) !temperature of the first internal layers of a room [K]
	real twinint(4)	!temperature of the windows [K]
	real aw		!width of the wall
	real bw		!length of the wall
	real zw		!height of the wall
	real fprl_int	!view factor
	real fnrm_int	!view factor	
        real fnrm_intx	!view factor
        real fnrm_inty	!view factor

!Output
!------
	real b(6)	!second member vector for the system
	real a(6,6)	!matrix for the system
!Local
!-----
	integer iw,jw
	real b_wall(6)	
	real b_wind(6)
	real emwal_av		!averadge emissivity of the wall
	real emwin_av		!averadge emissivity of the window
	real em_av		!averadge emissivity
        real twal_int(6)        !twalint 
	real twin(4)   		!twinint 
!------------------------------------------------------------------

!Initialise the variables
!-------------------------

	 do iw=1,6
            b(iw)= 0.
            b_wall(iw)=0.
            b_wind(iw)=0.
          do jw=1,6
            a(iw,jw)= 0. 
          enddo
         enddo

         do iw=1,6
            twal_int(iw)=twalint(iw)
         enddo

         do iw=1,4
            twin(iw)=twinint(iw)
         enddo
	 
!Calculation of the averadge emissivities
!-----------------------------------------

	emwal_av=(1-pwin)*emwal
	emwin_av=pwin*emwin
	em_av=emwal_av+emwin_av
	
!Calculation of the second term for the walls
!-------------------------------------------

            call fprl_ints(fprl_int,aw/zw,bw/zw)
            call fnrm_ints(fnrm_intx,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))
            call fnrm_ints(fnrm_inty,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw))

            b_wall(1)=(emwal*sigma*(twal_int(5)**4)*           &
     	         fprl_int)+                                    &
                 (sigma*(emwal_av*(twal_int(3)**4)+            &
                  emwal_av*(twal_int(4)**4))*                  &
                 (zw/aw)*fnrm_intx)+                           &
                 (sigma*(emwal_av*(twal_int(1)**4)+            &
                  emwal_av*(twal_int(2)**4))*                  & 
                 (zw/bw)*fnrm_inty)

            call fprl_ints(fprl_int,aw/zw,bw/zw)
            call fnrm_ints(fnrm_intx,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))
            call fnrm_ints(fnrm_inty,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw))
	
            b_wall(2)=(emwal*sigma*(twal_int(6)**4)*           &
              	   fprl_int)+                                  &
                  (sigma*(emwal_av*(twal_int(3)**4)+           &
                  emwal_av*(twal_int(4)**4))*                  & 
                 (zw/aw)*fnrm_intx)+                           &
                 (sigma*(emwal_av*(twal_int(1)**4)+            &
                 emwal_av*(twal_int(2)**4))*                   &
                 (zw/bw)*fnrm_inty)

            call fprl_ints(fprl_int,zw/aw,bw/aw)
            call fnrm_ints(fnrm_intx,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))
            call fnrm_ints(fnrm_inty,zw/bw,aw/bw,(aw*aw+zw*zw)/(bw*bw))

            b_wall(3)=(emwal_av*sigma*(twal_int(4)**4)*        &
        	  fprl_int)+                                   &
                 (sigma*(emwal_av*(twal_int(2)**4)+            &
                  emwal_av*(twal_int(1)**4))*                  &
                 (aw/bw)*fnrm_intx)+                           &
                 (sigma*(emwal*(twal_int(5)**4)+               &
                  emwal*(twal_int(6)**4))*                     &
                 (aw/zw)*fnrm_inty)

            call fprl_ints(fprl_int,zw/aw,bw/aw)
            call fnrm_ints(fnrm_intx,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))
            call fnrm_ints(fnrm_inty,zw/bw,aw/bw,(aw*aw+zw*zw)/(bw*bw))

            b_wall(4)=(emwal_av*sigma*(twal_int(3)**4)*        &
     	          fprl_int)+                                   &
                 (sigma*(emwal_av*(twal_int(2)**4)+            &
                  emwal_av*(twal_int(1)**4))*                  &
                 (aw/bw)*fnrm_intx)+                           &
                 (sigma*(emwal*(twal_int(5)**4)+               &
                  emwal*(twal_int(6)**4))*                     &
                 (aw/zw)*fnrm_inty)

            call fprl_ints(fprl_int,aw/bw,zw/bw)
            call fnrm_ints(fnrm_intx,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))
            call fnrm_ints(fnrm_inty,zw/aw,bw/aw,(bw*bw+zw*zw)/(aw*aw))
          
            b_wall(5)=(emwal_av*sigma*(twal_int(2)**4)*        &
     	          fprl_int)+                                   &
                 (sigma*(emwal_av*(twal_int(3)**4)+            &
                  emwal_av*(twal_int(4)**4))*                  &
                 (bw/aw)*fnrm_intx)+                           &
                 (sigma*(emwal*(twal_int(5)**4)+               &
                  emwal*(twal_int(6)**4))*                     &
                 (bw/zw)*fnrm_inty)

            call fprl_ints(fprl_int,aw/bw,zw/bw)
            call fnrm_ints(fnrm_intx,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))
            call fnrm_ints(fnrm_inty,zw/aw,bw/aw,(bw*bw+zw*zw)/(aw*aw))

            b_wall(6)=(emwal_av*sigma*(twal_int(1)**4)*        &
     	         fprl_int)+                                    &
                 (sigma*(emwal_av*(twal_int(3)**4)+            &
                  emwal_av*(twal_int(4)**4))*                  &
                 (bw/aw)*fnrm_intx)+                           &
                 (sigma*(emwal*(twal_int(5)**4)+               &
                 emwal*(twal_int(6)**4))*                      &
                 (bw/zw)*fnrm_inty)
	
!Calculation of the second term for the windows
!---------------------------------------------
            call fnrm_ints(fnrm_intx,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))
            call fnrm_ints(fnrm_inty,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw))

            b_wind(1)=(sigma*(emwin_av*(twin(3)**4)+          &
                  emwin_av*(twin(4)**4))*                     &
                 (zw/aw)*fnrm_intx)+                          &
                 (sigma*(emwin_av*(twin(1)**4)+               &
                  emwin_av*(twin(2)**4))*                     &
                 (zw/bw)*fnrm_inty)

            call fnrm_ints(fnrm_intx,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))
            call fnrm_ints(fnrm_inty,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw))

            b_wind(2)=(sigma*(emwin_av*(twin(3)**4)+          &
                  emwin_av*(twin(4)**4))*                     &
                 (zw/aw)*fnrm_intx)+                          &
                 (sigma*(emwin_av*(twin(1)**4)+               &
                  emwin_av*(twin(2)**4))*                     &
                 (zw/bw)*fnrm_inty)

            call fprl_ints(fprl_int,zw/aw,bw/aw)
            call fnrm_ints(fnrm_int,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))
          
            b_wind(3)=emwin_av*sigma*(twin(4)**4)*            &
                 fprl_int+(sigma*(emwin_av*                   &
                 (twin(2)**4)+emwin_av*(twin(1)**4))*         &
                 (aw/bw)*fnrm_int)

            call fprl_ints(fprl_int,zw/aw,bw/aw)
            call fnrm_ints(fnrm_int,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))

            b_wind(4)=emwin_av*sigma*(twin(3)**4)*            &
                 fprl_int+(sigma*(emwin_av*                   &
                  (twin(2)**4)+emwin_av*(twin(1)**4))*        &
                 (aw/bw)*fnrm_int)

            call fprl_ints(fprl_int,aw/bw,zw/bw)
            call fnrm_ints(fnrm_int,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))
          
            b_wind(5)=emwin_av*sigma*(twin(2)**4)*            &
                 fprl_int+(sigma*(emwin_av*                   &
                 (twin(3)**4)+emwin_av*(twin(4)**4))*         &
                 (bw/aw)*fnrm_int)
 
            call fprl_ints(fprl_int,aw/bw,zw/bw)
            call fnrm_ints(fnrm_int,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))

            b_wind(6)=emwin_av*sigma*(twin(1)**4)*            &
                 fprl_int+(sigma*(emwin_av*                   &
                 (twin(3)**4)+emwin_av*(twin(4)**4))*         &
                 (bw/aw)*fnrm_int)
     
!Calculation of the total b term
!-------------------------------

	do iw=1,6
	 b(iw)=b_wall(iw)+b_wind(iw)
	end do     


!Calculation of the matrix of the system
!----------------------------------------

         call fnrm_ints(fnrm_int,bw/aw,zw/aw,(bw*bw+zw*zw)/(aw*aw))         

         a(1,1)=(em_av-1.)*(zw/bw)*fnrm_int
     	        
         a(1,2)=a(1,1)

         call fnrm_ints(fnrm_int,aw/bw,zw/bw,(aw*aw+zw*zw)/(bw*bw))

         a(1,3)=(em_av-1.)*(zw/aw)*fnrm_int
         	 
         a(1,4)=a(1,3)

         call fprl_ints(fprl_int,aw/zw,bw/zw)

         a(1,5)=(emwal-1.)*fprl_int
         a(1,6)=1.

         a(2,1)=a(1,1)
         a(2,2)=a(1,2)
         a(2,3)=a(1,3)
         a(2,4)=a(1,4)
         a(2,5)=1.
         a(2,6)=a(1,5)

         call fnrm_ints(fnrm_int,bw/zw,aw/zw,(bw*bw+aw*aw)/(zw*zw))

         a(3,1)=(em_av-1.)*(aw/bw)*fnrm_int
     	        
         a(3,2)=a(3,1)
         a(3,3)=1.

         call fprl_ints(fprl_int,zw/aw,bw/aw) 

         a(3,4)=(em_av-1.)*fprl_int

         call fnrm_ints(fnrm_int,zw/bw,aw/bw,(aw*aw+zw*zw)/(bw*bw))

         a(3,5)=(emwal-1.)*(aw/zw)*fnrm_int
     	        
         a(3,6)=a(3,5)

         a(4,1)=a(3,1)
         a(4,2)=a(3,2)
         a(4,3)=a(3,4)
         a(4,4)=1.
         a(4,5)=a(3,5)
         a(4,6)=a(3,6)

         a(5,1)=1.

         call fprl_ints(fprl_int,aw/bw,zw/bw)

         a(5,2)=(em_av-1.)*fprl_int

         call fnrm_ints(fnrm_int,aw/zw,bw/zw,(aw*aw+bw*bw)/(zw*zw))

         a(5,3)=(em_av-1.)*(bw/aw)*fnrm_int
     	        
         a(5,4)=a(5,3)

         call fnrm_ints(fnrm_int,zw/aw,bw/aw,(bw*bw+zw*zw)/(aw*aw))

         a(5,5)=(emwal-1.)*(bw/zw)*fnrm_int
     	        
         a(5,6)=a(5,5)

         a(6,1)=a(5,2)
         a(6,2)=1.
         a(6,3)=a(5,3)
         a(6,4)=a(5,4)
         a(6,5)=a(5,5)
         a(6,6)=a(6,5)

      return
      end subroutine algebra_long

!====6=8===============================================================72 
!====6=8===============================================================72 


	subroutine fluxroo(hseqocc,hleqocc,hsvent,hlvent, &
                           hscond,hslev,hllev) 
	
!-----------------------------------------------------------------------
!This routine calculates the heat flux generated inside the room
!and the heat ejected to the atmosphere.
!----------------------------------------------------------------------	

	implicit none

!-----------------------------------------------------------------------

!Input
!-----
	real hseqocc		!sensible heat generated by equipments and occupants [W]
	real hleqocc		!latent heat generated by occupants [W]
	real hsvent		!sensible heat generated by natural ventilation [W]
	real hlvent		!latent heat generated by natural ventilation [W] 
	real hscond		!sensible heat generated by wall conduction 

!Output
!------
	real hslev		!sensible heat flux generated inside the room [W]
	real hllev		!latent heat flux generatd inside the room


!Calculation of the total sensible heat generated inside the room

	hslev=hseqocc+hsvent+hscond 
 
!Calculation of the total latent heat generated inside the room
	
	hllev=hleqocc+hlvent
        
	return
	end subroutine fluxroo

!====6=8===============================================================72 
!====6=8===============================================================72

	subroutine phirat(nhourday,rocc)

!----------------------------------------------------------------------
!This routine calculates the occupation ratio of a floor
!By now we suppose a constant value
!----------------------------------------------------------------------

        implicit none

!Input
!-----

	real nhourday	! number of hours from midnight (local time)
	
!Output
!------
	real rocc       !value between 0 and 1

!!TEST
        rocc=1.

	return
	end subroutine phirat

!====6=8===============================================================72 
!====6=8===============================================================72

	subroutine phiequ(nhourday,hsesf,hsequip,hsequ)

!----------------------------------------------------------------------
!This routine calculates the sensible heat gain from equipments
!----------------------------------------------------------------------
        implicit none
!Input
!-----

	real nhourday ! number of hours from midnight, Local time
        real, intent(in) :: hsesf
        real, intent(in), dimension(24) :: hsequip
	
!Output
!------
	real hsequ    !sensible heat gain from equipment [Wm¯2]

!---------------------------------------------------------------------	

        hsequ = hsequip(int(nhourday)+1) * hsesf
        
	end subroutine phiequ
!====6=8===============================================================72 
!====6=8===============================================================72

	subroutine fluxeqocc(nhourday,bw,bl,perflo,hsesf,hsequip,hseqocc,hleqocc)
	
	implicit none

!---------------------------------------------------------------------
!This routine calculates the sensible and the latent heat flux 
!generated by equipments and occupants
!---------------------------------------------------------------------	

!Input
!-----
	real bw			!Room width [m]
	real bl			!Room lengzh [m]
	real nhourday		!number of hours from the beginning of the day
        real, intent(in) :: perflo ! Peak number of occupants per unit floor area
        real, intent(in) :: hsesf
        real, intent(in), dimension(24) :: hsequip

!Output
!------
	real hseqocc		!sensible heat generated by equipments and occupants [W]
	real hleqocc		!latent heat generated by occupants [W]
!Local
!-----
	real Af			!Air conditioned floor area [m2]
	real rocc		!Occupation ratio of the floor [0,1]
        real hsequ		!Heat generated from equipments 

        real hsocc		!Sensible heat generated by a person [W/Person]
                                !Source Boundary Layer Climates,page 195 (book)
        parameter (hsocc=160.)

        real hlocc		!Latent heat generated by a person [W/Person]
                                !Source Boundary Layer Climates,page 225 (book)
        parameter (hlocc=1.96e6/86400.)

!------------------------------------------------------------------
!			Sensible heat flux
!			------------------

	 Af=bw*bl

	 call phirat(nhourday,rocc)

         call phiequ(nhourday,hsesf,hsequip,hsequ)

         hseqocc=Af*rocc*perflo*hsocc+Af*hsequ

!
!			Latent heat
!			-----------
!

         hleqocc=Af*rocc*perflo*hlocc

	return
	end subroutine fluxeqocc

!====6=8===============================================================72 
!====6=8===============================================================72
	
	subroutine fluxvent(cpint,rhoint,vollev,tlev,tout,latent,&
                            humout,rhoout,humlev,beta,hsvent,hlvent)
	
	implicit none

!---------------------------------------------------------------------
!This routine calculates the sensible and the latent heat flux 
!generated by natural ventilation
!---------------------------------------------------------------------

!Input
!-----
	real cpint		!specific heat of the indoor air [J/kg.K]
	real rhoint		!density of the indoor air [Kg/m3]	
	real vollev		!volume of the room [m3]
	real tlev		!Room temperature [K]
	real tout		!outside air temperature [K]
	real latent		!latent heat of evaporation [J/Kg]
	real humout		!outside absolute humidity [Kgwater/Kgair]
	real rhoout		!air density [kg/m3]
	real humlev		!Specific humidity of the indoor air [Kgwater/Kgair]
        real, intent(in) :: beta!Thermal efficiency of the heat exchanger 
	
!Output
!------
	real hsvent		!sensible heat generated by natural ventilation [W]
	real hlvent		!latent heat generated by natural ventilation [W] 

!Local
!-----       
        
!----------------------------------------------------------------------

!			Sensible heat flux
!			------------------
        
	hsvent=(1.-beta)*cpint*rhoint*(vollev/3600.)*  &
               (tout-tlev)
	
!			Latent heat flux
!			----------------
       
	hlvent=(1.-beta)*latent*rhoint*(vollev/3600.)* &
     	       (humout-humlev)


	return
	end subroutine fluxvent

!====6=8===============================================================72 
!====6=8===============================================================72
	
	subroutine fluxcond(hswalins,hswinins,surwal,pwin,hscond)
	
	implicit none

!---------------------------------------------------------------------
!This routine calculates the sensible heat flux generated by 
!wall conduction.
!---------------------------------------------------------------------

!Input
!-----
	real hswalins(6)	!sensible heat at the internal layers of the wall [W/m2]
	real hswinins(4)	!internal window sensible heat flux [W/m2]
	real surwal(6)	        !surfaces of the room walls [m2]
	real pwin               !window proportion	


!Output
!------
	
	real hscond		!sensible heat generated by wall conduction [W]
	
!Local
!-----

	integer ivw

!----------------------------------------------------------------------

	  hscond=0.

	do ivw=1,4
	   hscond=hscond+surwal(ivw)*(1-pwin)*hswalins(ivw)+ &
                  surwal(ivw)*pwin*hswinins(ivw)	         
	end do

	do ivw=5,6
    	   hscond=hscond+surwal(ivw)*hswalins(ivw)	
	end do
!           
!Finally we must change the sign in hscond to be proportional
!to the difference (Twall-Tindoor).
!
        hscond=(-1)*hscond 

	return
	end subroutine fluxcond

!====6=8===============================================================72 
!====6=8===============================================================72
	
	subroutine regtemp(swcond,nhourday,dt,Qb,hsroo,          &
                           tlev,timeon,timeoff,targtemp,gaptemp,hsneed)
	
	implicit none

!---------------------------------------------------------------------
!This routine calculates the sensible heat fluxes, 
!after anthropogenic regulation (air conditioning)
!---------------------------------------------------------------------

!Input:
!-----.
        integer swcond       !swich air conditioning
	real nhourday        !number of hours from the beginning of the day real
	real dt	             !time step [s]
	real Qb		     !overall heat capacity of the indoor air [J/K]
        real hsroo           !sensible heat flux generated inside the room [W]
        real tlev            !room air temperature [K]
        real, intent(in) :: timeon  ! Initial local time of A/C systems
        real, intent(in) :: timeoff ! Ending local time of A/C systems
        real, intent(in) :: targtemp! Target temperature of A/C systems
        real, intent(in) :: gaptemp ! Comfort range of indoor temperature
        

!Local:
!-----.

        real templev         !hipotetical room air temperature [K]
        real alpha           !variable to control the heating/cooling of 
                             !the air conditining system
!Output:
!-----.
	real hsneed	     !sensible heat extracted to the indoor air [W]
!---------------------------------------------------------------------
!initialize variables
!---------------------
        templev = 0.
        alpha   = 0.

        if (swcond.eq.0) then ! there is not air conditioning in the floor
            hsneed = 0.
            goto 100
        else
            if ((nhourday.ge.timeon).and.(nhourday.le.timeoff)) then
               templev=tlev+(dt/Qb)*hsroo
               goto 200
            else
               hsneed = 0.     ! air conditioning is switched off
               goto 100
            endif
        endif

200     continue

        if (abs(templev-targtemp).le.gaptemp) then
           hsneed = 0.
        else 
           if (templev.gt.(targtemp+gaptemp)) then
              hsneed=hsroo-(Qb/dt)*(targtemp+gaptemp-tlev)
              alpha=(abs(hsneed-hsroo)/Qb)
              if (alpha.gt.temp_rat) then
                  hsneed=hsroo+temp_rat*Qb
                  goto 100
              else
                  goto 100
              endif
           else 
              hsneed=hsroo-(Qb/dt)*(targtemp-gaptemp-tlev)
              alpha=(abs(hsneed-hsroo)/Qb)
              if (alpha.gt.temp_rat) then
                  hsneed=hsroo-temp_rat*Qb
                  goto 100
              else
                  goto 100
              endif
           endif
        endif 

100     continue
	return
	end subroutine regtemp
     
!====6=8==============================================================72
!====6=8==============================================================72
         
	 subroutine reghum(swcond,nhourday,dt,volroo,rhoint,latent, &
                           hlroo,shumroo,timeon,timeoff,targhum,gaphum,hlneed)

	 implicit none

!---------------------------------------------------------------------
!This routine calculates the latent heat fluxes, 
!after anthropogenic regulation (air conditioning)
!---------------------------------------------------------------------

!Input:
!-----.
        integer swcond    !swich air conditioning
	real nhourday     !number of hours from the beginning of the day real[h]
	real dt	          !time step [s]
	real volroo       !volume of the room [m3]
        real rhoint       !density of the internal air [Kg/m3]
        real latent       !latent heat of evaporation [J/Kg]
        real hlroo        !latent heat flux generated inside the room [W]
        real shumroo      !specific humidity of the indoor air [kg/kg]
        real, intent(in) :: timeon  ! Initial local time of A/C systems
        real, intent(in) :: timeoff ! Ending local time of A/C systems
        real, intent(in) :: targhum ! Target humidity of the A/C systems
        real, intent(in) :: gaphum  ! comfort range of the specific humidity

!Local:
!-----.

        real humlev       !hipotetical specific humidity of the indoor [kg/kg]
        real betha        !variable to control the drying/moistening of
                          !the air conditioning system
!Output:
!-----.
	real hlneed	  !latent heat extracted to the indoor air [W]
!------------------------------------------------------------------------
!initialize variables
!---------------------
        humlev = 0.
        betha  = 0.

        if (swcond.eq.0) then ! there is not air conditioning in the floor
            hlneed = 0.
            goto 100
        else
            if ((nhourday.ge.timeon).and.(nhourday.le.timeoff)) then
               humlev=shumroo+(dt/(latent*rhoint*volroo))*hlroo
               goto 200
            else
               hlneed = 0.     ! air conditioning is switched off
               goto 100
            endif
        endif

200     continue

        if (abs(humlev-targhum).le.gaphum) then
           hlneed = 0.
        else 
           if (humlev.gt.(targhum+gaphum)) then
              hlneed=hlroo-((latent*rhoint*volroo)/dt)* &
                          (targhum+gaphum-shumroo)
              betha=abs(hlneed-hlroo)/(latent*rhoint*volroo)
              if (betha.gt.hum_rat) then
                  hlneed=hlroo+hum_rat*(latent*rhoint*volroo)
                  goto 100
              else
                  goto 100
              endif
           else 
              hlneed=hlroo-((latent*rhoint*volroo)/dt)* &
                          (targhum-gaphum-shumroo)
              betha=abs(hlneed-hlroo)/(latent*rhoint*volroo)
              if (betha.gt.hum_rat) then
                  hlneed=hlroo-hum_rat*(latent*rhoint*volroo)
                  goto 100
              else
                  goto 100
              endif
           endif
        endif 
	
100     continue
	return
	end subroutine reghum

!====6=8==============================================================72
!====6=8==============================================================72
         
         subroutine air_cond(hsneed,hlneed,dt,hsout,hlout,consump,cop)

         implicit none

!
!Performance of the air conditioning system        
!
!INPUT/OUTPUT VARIABLES
         real, intent(in) :: cop
!
!INPUT/OUTPUT VARIABLES
!       
         real hsneed     !sensible heat that is necessary for cooling/heating
                         !the indoor air temperature [W] 
         real hlneed     !latent heat that is necessary for controling
                         !the humidity of the indoor air [W]
         real dt         !time step [s]
!
!OUTPUT VARIABLES
!
         real hsout      !sensible heat pumped out into the atmosphere [W]
         real hlout      !latent heat pumped out into the atmosphere [W]
         real consump    !Electrical consumption of the air conditioning system [W] 
                         
    
!
!Performance of the air conditioning system
!
         if (hsneed.gt.0) then         ! air conditioning is cooling 
                                       ! and the heat is pumped out into the atmosphere  
	  hsout=(1/cop)*(abs(hsneed)+abs(hlneed))+hsneed
          hlout=hlneed
          consump=(1./cop)*(abs(hsneed)+abs(hlneed))
!!        hsout=0.             
!!        hlout=0.

         else if(hsneed.eq.0.) then !air conditioning is not working to regulate the indoor temperature
               hlneed=0.       !no humidity regulation is considered 
               hsout=0.        !no output into the atmosphere (sensible heat) 
               hlout=0.        !no output into the atmosphere (latent heat)
               consump=0.      !no electrical consumption

              else  !! hsneed < 0. !air conditioning is heating 
               hlneed=0.           !no humidity regulation is considered
               hlout=0.            !no output into the atmosphere (latent heat) 
               consump=(1./cop)*(abs(hsneed)+abs(hlneed))
!
!!We have two possibilities 
! 
!!             hsout=(1./cop)*(abs(hsneed)+abs(hlneed)) !output into the atmosphere 
               hsout=0.                            !no output into the atmosphere                        
         end if

         return 
         end subroutine air_cond

!====6=8==============================================================72
!====6=8==============================================================72

        subroutine consump_total(nzcanm,nlev,consumpbuild,hsoutbuild, &
                                 hsout,consump)

        implicit none
        
!-----------------------------------------------------------------------
!Compute the total consumption in kWh/s (1kWh=3.6e+6 J) and sensible heat
!ejected into the atmosphere per building
!------------------------------------------------------------------------
!
!INPUT VARIABLES
!
!
        integer nzcanm            !Maximum number of vertical levels in the urban grid
        real hsout(nzcanm)        !sensible heat emitted outside the room [W]
        real consump(nzcanm)      !Electricity consumption for the a.c. in each floor[W]
!
!OUTPUT VARIABLES
!
	real consumpbuild         !Energetic consumption for the entire building[kWh/s]
        real hsoutbuild           !Total sensible heat ejected into the atmosphere
                                  !by the air conditioning systems per building [W]        
!
!LOCAL  VARIABLES
!
        integer ilev

!
!INPUT VARIABLES
!
        integer nlev     
        
!
!INITIALIZE VARIABLES
!
        consumpbuild=0.
        hsoutbuild=0.
!
        do ilev=1,nlev
           consumpbuild=consumpbuild+consump(ilev)
           hsoutbuild=hsoutbuild+hsout(ilev)
        enddo !ilev

        consumpbuild=consumpbuild/(3.6e+06)

        return 
        end subroutine consump_total
!====6=8==============================================================72
!====6=8==============================================================72
        subroutine tridia(n,a,b,x)

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     +    by A. Clappier,     EPFL, CH 1015 Lausanne                  +
!     +                        phone: ++41-(0)21-693-61-60             +
!     +                        email:alain.clappier@epfl.ch            +
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! ----------------------------------------------------------------------
!        Resolution of a * x = b    where a is a tridiagonal matrix
!
! ----------------------------------------------------------------------

        implicit none

! Input
        integer n
        real a(-1:1,n)           !  a(-1,*) lower diagonal      A(i,i-1)
                               !  a(0,*)  principal diagonal  A(i,i)
                               !  a(1,*)  upper diagonal      A(i,i+1)
        real b(n)

! Output
        real x(n)

! Local
        integer i

! ----------------------------------------------------------------------

        do i=n-1,1,-1
           b(i)=b(i)-a(1,i)*b(i+1)/a(0,i+1)
           a(0,i)=a(0,i)-a(1,i)*a(-1,i+1)/a(0,i+1)
        enddo

        do i=2,n
           b(i)=b(i)-a(-1,i)*b(i-1)/a(0,i-1)
        enddo

        do i=1,n
           x(i)=b(i)/a(0,i)
        enddo

        return
        end subroutine tridia    
!====6=8===============================================================72     
!====6=8===============================================================72     
      
       subroutine gaussjbem(a,n,b,np)

! ----------------------------------------------------------------------
! This routine solve a linear system of n equations of the form
!              A X = B
!  where  A is a matrix a(i,j)
!         B a vector and X the solution
! In output b is replaced by the solution     
! ----------------------------------------------------------------------

       implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
       integer np
       real a(np,np)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
       real b(np)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer nmax
      parameter (nmax=150)

      real big,dum
      integer i,icol,irow
      integer j,k,l,ll,n
      integer ipiv(nmax)
      real pivinv

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       
       do j=1,n
          ipiv(j)=0.
       enddo
       
      do i=1,n
         big=0.
         do j=1,n
            if(ipiv(j).ne.1)then
               do k=1,n
                  if(ipiv(k).eq.0)then
                     if(abs(a(j,k)).ge.big)then
                        big=abs(a(j,k))
                        irow=j
                        icol=k
                     endif
                  elseif(ipiv(k).gt.1)then
                     pause 'singular matrix in gaussjbem'
                  endif
               enddo
            endif
         enddo
         
         ipiv(icol)=ipiv(icol)+1
         
         if(irow.ne.icol)then
            do l=1,n
               dum=a(irow,l)
               a(irow,l)=a(icol,l)
               a(icol,l)=dum
            enddo
            
            dum=b(irow)
            b(irow)=b(icol)
            b(icol)=dum
          
         endif
         
         if(a(icol,icol).eq.0)pause 'singular matrix in gaussjbem'
         
         pivinv=1./a(icol,icol)
         a(icol,icol)=1
         
         do l=1,n
            a(icol,l)=a(icol,l)*pivinv
         enddo
         
         b(icol)=b(icol)*pivinv
         
         do ll=1,n
            if(ll.ne.icol)then
               dum=a(ll,icol)
               a(ll,icol)=0.
               do l=1,n
                  a(ll,l)=a(ll,l)-a(icol,l)*dum
               enddo
               
               b(ll)=b(ll)-b(icol)*dum
               
            endif
         enddo
      enddo
      
      return
      end subroutine gaussjbem
         
!====6=8===============================================================72     
!====6=8===============================================================72     

      subroutine radfluxs(radflux,alb,rs,em,rl,sigma,twal)

      implicit none
!-------------------------------------------------------------------
!This function calculates the radiative fluxe at a surface
!-------------------------------------------------------------------

	
	real alb	!albedo of the surface
	real rs		!shor wave radiation
	real em		!emissivity of the surface
	real rl 	!lon wave radiation
	real sigma	!parameter (wall is not black body) [W/m2.K4]
	real twal	!wall temperature [K]
	real radflux
	
	 radflux=(1.-alb)*rs+em*rl-em*sigma*twal**4
	
      return
      end subroutine radfluxs

!====6=8==============================================================72 
!====6=8==============================================================72
!       
!       we define the view factors fprl and fnrm, which are the angle 
!       factors between two equal and parallel planes, fprl, and two 
!       equal and orthogonal planes, fnrm, respectively
!       
        subroutine fprl_ints(fprl_int,vx,vy)
        
        implicit none

	real vx,vy
	real fprl_int
        
	fprl_int=(2./(3.141592653*vx*vy))*                       &
             (log(sqrt((1.+vx*vx)*(1.+vy*vy)/(1.+vx*vx+vy*vy)))+ &
              (vy*sqrt(1.+vx*vx)*atan(vy/sqrt(1.+vx*vx)))+       &
              (vx*sqrt(1.+vy*vy)*atan(vx/sqrt(1.+vy*vy)))-       &
              vy*atan(vy)-vx*atan(vx))

        return
        end subroutine fprl_ints

!====6=8==============================================================72 
!====6=8==============================================================72
!       
!       we define the view factors fprl and fnrm, which are the angle 
!       factors between two equal and parallel planes, fprl, and two 
!       equal and orthogonal planes, fnrm, respectively
!       

        subroutine fnrm_ints(fnrm_int,wx,wy,wz)

        implicit none
        
	real wx,wy,wz
	real fnrm_int
	
        fnrm_int=(1./(3.141592653*wy))*(wy*atan(1./wy)+wx*atan(1./wx)- &
              (sqrt(wz)*atan(1./sqrt(wz)))+                            &
              (1./4.)*(log((1.+wx*wx)*(1.+wy*wy)/(1.+wz))+             &
              wy*wy*log(wy*wy*(1.+wz)/(wz*(1.+wy*wy)))+                &
              wx*wx*log(wx*wx*(1.+wz)/(wz*(1.+wx*wx)))))
        
        return
        end subroutine fnrm_ints

!====6=8==============================================================72 
!====6=8==============================================================72
END MODULE module_sf_bem
