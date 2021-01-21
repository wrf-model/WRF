MODULE module_sf_bem




use module_wrf_error
         
        real emins		
        parameter (emins=0.9) 
        real albins	        

        parameter (albins=0.3)

        real thickwin           
        parameter (thickwin=0.006)
        real cswin		
        parameter(cswin= 2.268e+06)

        real temp_rat            
        parameter(temp_rat=0.001)

        real hum_rat            
        parameter(hum_rat=1.e-06)


    CONTAINS



	
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



	implicit none
	













































	real dt				
                                       
        integer nzcanm                  
	integer nlev			
	integer nwal                    
	integer nrof                    
	integer nflo                    
	integer ngrd                    
	real dzlev			
	real bl				
	real bw                         
	
	real albwal			
	real albwin		 	
	real albrof			
	
	real emwal 	          	
	
	real emrof			
        real emwin                      

	real pwin                       
	real,    intent(in) :: cop      
	real,    intent(in) :: beta     
        integer, intent(in) :: sw_cond  
        real,    intent(in) :: timeon   
        real,    intent(in) :: timeoff  
        real,    intent(in) :: targtemp 
        real,    intent(in) :: gaptemp  
        real,    intent(in) :: targhum  
        real,    intent(in) :: gaphum   
        real,    intent(in) :: perflo   
        real,    intent(in) :: hsesf    
        real,    intent(in) :: hsequip(24) 
	
	real cswal(nwal)		
	
	real csflo(nflo)		
	real csrof(nrof)		
	real csgrd(ngrd)		
	
	real kwal(nwal+1)		
	real kflo(nflo+1)		
	real krof(nrof+1)		
	real kgrd(ngrd+1)		
	
	real dzwal(nwal)		
	real dzflo(nflo)		
	real dzrof(nrof)		
	real dzgrd(ngrd)		
	
	real latent                      


	real rs				
	real rl				
	real rswal(4,nzcanm)		
        real rlwal(4,nzcanm)		
	real rhoout(nzcanm)		
	real tout(nzcanm)		
	real humout(nzcanm)		
	real press(nzcanm)		
	
	real hswalout(4,nzcanm)	        
	real hswinout(4,nzcanm)	        
	real hsrof			
	
	real rair			
	real sigma			
	real cp				
       
	


	real tlev(nzcanm)		
	real shumlev(nzcanm)		
	real twal(4,nwal,nzcanm)	
	real twin(4,nzcanm)		
	real tflo(nflo,nzcanm-1)	
	real tgrd(ngrd)		        
	real trof(nrof)		        
	real hsout(nzcanm)		
	real hlout(nzcanm)		
        real consump(nzcanm)            
	real hsvent(nzcanm)		
	real hlvent(nzcanm)		
        real gsrof                      
        real gswal(4,nzcanm)             



	integer swwal                   
	integer ilev			
	integer iwal			
	integer iflo			
	integer ivw			
        integer igrd                    
        integer irof                    
        real hseqocc(nzcanm)		
	real hleqocc(nzcanm)		
        real hscond(nzcanm)		
        real hslev(nzcanm)		
        real hllev(nzcanm)		
	real surwal(6,nzcanm)	        
	real surwal1D(6)	        
	real rsint(6)		        
	real rswalins(6,nzcanm)	        
	real twin1D(4)		        
	real twal_int(6)		
	real rlint(6)		        
	real rlwalins(6,nzcanm)	        
	real hrwalout(4,nzcanm)	        
	real hrwalins(6,nzcanm)	        
	real hrwinout(4,nzcanm)	        
	real hrwinins(4,nzcanm)	        
	real hrrof			
	real hs
        real hsneed(nzcanm)		
	real hlneed(nzcanm)		
        real hswalins(6,nzcanm)	        
	real hswalins1D(6)
	real hswinins(4,nzcanm)	        
	real hswinins1D(4)	
	real htot(2)			
	real twal1D(nwal)
	real tflo1D(nflo)	
        real tgrd1D(ngrd)
        real trof1D(nrof)
	real rswal1D(4)
	real Qb				
	real vollev			
	real rhoint			
	real cpint			
        real humdry                     
	real radflux                    
	real consumpbuild               
        real hsoutbuild                 
                                        
        real nhourday                   




       do ilev=1,nzcanm
          hseqocc(ilev)=0.
          hleqocc(ilev)=0.
          hscond(ilev)=0.
          hslev(ilev)=0.
          hllev(ilev)=0.
       enddo	



	
       
	do ivw=1,6
	do ilev=1,nzcanm
	 surwal(ivw,ilev)=1.   
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
          
	end do 
	
	 






       
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
	    
	end do	
      end if	 
	

      if (nlev.ne.1) then  



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
	  
      else   
	  
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
	






        do ilev=1,nlev
	 do ivw=1,4	 
	 call radfluxs(radflux,albwal,rswal(ivw,ilev),     &
     	                    emwal,rlwal(ivw,ilev),sigma,   &
                            twal(ivw,nwal,ilev))
	
         hrwalout(ivw,ilev)=radflux
      	 						
	 hrwinout(ivw,ilev)=emwin*rlwal(ivw,ilev)- &
     	                    emwin*sigma*(twin(ivw,ilev)**4)
	 
	 
	 end do 
	end do  
	


        call radfluxs(radflux,albrof,rs,emrof,rl,sigma,trof(nrof))

        hrrof=radflux



      if(nlev.gt.2) then
       
	do ilev=2,nlev-1
	 do ivw=1,4
         
	 call radfluxs(radflux,albins,rswalins(ivw,ilev),     &
     	                    emins,rlwalins(ivw,ilev),sigma,   &
                            twal(ivw,1,ilev))
	 
	 hrwalins(ivw,ilev)=radflux

	 end do 

	 call radfluxs(radflux,albins,rswalins(5,ilev), &
     	                      emins,rlwalins(5,ilev),sigma,&
                              tflo(nflo,ilev-1))

         hrwalins(5,ilev)=radflux

         call radfluxs(radflux,albins,rswalins(6,ilev), &
                              emins,rlwalins(6,ilev),sigma,&
                              tflo(1,ilev))
         hrwalins(6,ilev)=radflux

       end do 

      end if 	




      if (nlev.ne.1) then 



	 do ivw=1,4

	    call radfluxs(radflux,albins,rswalins(ivw,1),  &
     	                    emins,rlwalins(ivw,1),sigma,   &
                            twal(ivw,1,1))
	
            hrwalins(ivw,1)=radflux

	 end do
	
	
	  call radfluxs(radflux,albins,rswalins(5,1),&
                           emins,rlwalins(5,1),sigma,&    
                           tgrd(ngrd))

          hrwalins(5,1)=radflux

	   
          call radfluxs(radflux,albins,rswalins(6,1),&
     	                   emins,rlwalins(6,1),sigma,&
                           tflo(1,1))  
	 
          hrwalins(6,1)=radflux



         do ivw=1,4
   
          call radfluxs(radflux,albins,rswalins(ivw,nlev),     &
     	                        emins,rlwalins(ivw,nlev),sigma,&
                                twal(ivw,1,nlev))

	  hrwalins(ivw,nlev)=radflux

	 end do                                          

	
         call radfluxs(radflux,albins,rswalins(5,nlev),    &
     	                      emins,rlwalins(5,nlev),sigma,&
                              tflo(nflo,nlev-1))

         hrwalins(5,nlev)=radflux

	 call radfluxs(radflux,albins,rswalins(6,nlev), &
                              emins,rlwalins(6,nlev),sigma,&
                              trof(1))

         hrwalins(6,nlev)=radflux
      
      else       
      
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
      
		


	 do ilev=1,nlev
	  do ivw=1,4
	     hrwinins(ivw,ilev)=emwin*rlwalins(ivw,ilev)-    &
                                emwin*sigma*(twin(ivw,ilev)**4)
	  end do
	 end do
	
		




	
	do ilev=1,nlev
         do ivw=1,4
		
               call hsinsflux (2,2,tlev(ilev),twal(ivw,1,ilev),hs)		
	       
               hswalins(ivw,ilev)=hs 
         
         end do 
        end do 
       
      


	do ilev=1,nlev

         do ivw=1,4
	 
	       call hsinsflux (2,1,tlev(ilev),twin(ivw,ilev),hs)
	       
               hswinins(ivw,ilev)=hs 
			
         end do 
	
	end do 


       
      if (nlev.gt.2) then
       
        do ilev=2,nlev-1
                
	       call hsinsflux (1,2,tlev(ilev),tflo(nflo,ilev-1),hs)

	       hswalins(5,ilev)=hs
            
	       call hsinsflux (1,2,tlev(ilev),tflo(1,ilev),hs)

	       hswalins(6,ilev)=hs

        end do 
       
      end if
       
      if (nlev.ne.1) then
       
       	        call hsinsflux (1,2,tlev(1),tgrd(ngrd),hs)

		hswalins(5,1)=hs				
		
		call hsinsflux (1,2,tlev(1),tflo(1,1),hs)

		hswalins(6,1)=hs				
	 
       	        call hsinsflux (1,2,tlev(nlev),tflo(nflo,nlev-1),hs)

		hswalins(5,nlev)=hs			        

		call hsinsflux (1,2,tlev(nlev),trof(1),hs)

		hswalins(6,nlev)=hs	       
      
      else  
      
                call hsinsflux (1,2,tlev(1),tgrd(ngrd),hs)
		
		hswalins(5,1)=hs
		
		call hsinsflux (1,2,tlev(nlev),trof(1),hs)
		
		hswalins(6,nlev)=hs
      
      end if






        
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
           
	end do 
       end do 
       


       do ilev=1,nlev
        do ivw=1,4
       
         htot(1)=hswinins(ivw,ilev)+hrwinins(ivw,ilev)	
         htot(2)=hswinout(ivw,ilev)+hrwinout(ivw,ilev)	

         twin(ivw,ilev)=twin(ivw,ilev)+(dt/(cswin*thickwin))* &
                        (htot(1)+htot(2))
	
	end do 
       end do 




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

       end do 
      end if 
        


        
	swwal=1

	htot(1)=0.	
	htot(2)=hswalins(5,1)+hrwalins(5,1)   
   
        do igrd=1,ngrd
           tgrd1D(igrd)=tgrd(igrd)
        end do

         call wall(swwal,ngrd,dt,dzgrd,kgrd,csgrd,htot,tgrd1D)

        do igrd=1,ngrd
           tgrd(igrd)=tgrd1D(igrd)
        end do

        

        
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
      



 	do ilev=1,nlev
	  	  
	 
	 
	 call fluxeqocc(nhourday,bw,bl,perflo,hsesf,hsequip,hseqocc(ilev),hleqocc(ilev))

     	 
	
	  vollev=bw*bl*dzlev
          humdry=shumlev(ilev)/(1.-shumlev(ilev))
	  rhoint=(press(ilev))/(rair*(1.+0.61*humdry)*tlev(ilev))
	  cpint=cp*(1.+0.84*humdry)
          
 	  
	  call fluxvent(cpint,rhoint,vollev,tlev(ilev),tout(ilev),     &
                        latent,humout(ilev),rhoout(ilev),shumlev(ilev),&
                        beta,hsvent(ilev),hlvent(ilev))
	      
         
	  
	   do iwal=1,6
	     hswalins1D(iwal)=hswalins(iwal,ilev)
	     surwal1D(iwal)=surwal(iwal,ilev)
	  end do
	  
	   do iwal=1,4
	     hswinins1D(iwal)=hswinins(iwal,ilev)
	   end do
	
	  call fluxcond(hswalins1D,hswinins1D,surwal1D,pwin,&
                        hscond(ilev))

	
 	
	  call fluxroo(hseqocc(ilev),hleqocc(ilev),hsvent(ilev), &
               hlvent(ilev),hscond(ilev),hslev(ilev),hllev(ilev))

	  
	

	  Qb=rhoint*cpint*vollev

        

          call regtemp(sw_cond,nhourday,dt,Qb,hslev(ilev),       &
                       tlev(ilev),timeon,timeoff,targtemp,gaptemp,hsneed(ilev))

        

	  call reghum(sw_cond,nhourday,dt,vollev,rhoint,latent, &
                      hllev(ilev),shumlev(ilev),timeon,timeoff,&
                      targhum,gaphum,hlneed(ilev))



	        
          call air_cond(hsneed(ilev),hlneed(ilev),dt, &
                        hsout(ilev),hlout(ilev),consump(ilev), cop)
    	         	
 	  tlev(ilev)=tlev(ilev)+(dt/Qb)*(hslev(ilev)-hsneed(ilev))
          	  	  
	  shumlev(ilev)=shumlev(ilev)+(dt/(vollev*rhoint*latent))* &
                        (hllev(ilev)-hlneed(ilev))
           
	end do 
        
        call consump_total(nzcanm,nlev,consumpbuild,hsoutbuild, &
                           hsout,consump)
                
      return
      end subroutine BEM




	subroutine wall(swwall,nz,dt,dz,k,cs,flux,temp)
	


































	implicit none
		


	integer nz		
	real dt			
	real dz(nz)		
	real cs(nz)		
	real k(nz+1)		
	real flux(2)		




	integer swwall          
	real temp(nz)		




      real a(-1:1,nz)          
                               
                               
      
      real b(nz)	       
      real k1(20)
      real k2(20)
      real kc(20)
      save k1,k2,kc
      integer iz
        	



	
	if (swwall.eq.1) then
	
           if (nz.gt.20) then
              write(*,*) 'number of layers in the walls/roofs too big ',nz
              write(*,*) 'please decrease under of',20
              stop
           endif

	   call wall_coeff(nz,dt,dz,cs,k,k1,k2,kc)
	   swwall=0

	end if
 	

	
		 a(-1,1)=0.
		 a(0,1)=1+k2(1)
		 a(1,1)=-k2(1)

                 b(1)=temp(1)+flux(1)*kc(1)












	do iz=2,nz-1
		a(-1,iz)=-k1(iz)
		a(0,iz)=1+k1(iz)+k2(iz)
     		a(1,iz)=-k2(iz)
		b(iz)=temp(iz)
	end do		


	
		a(-1,nz)=-k1(nz)
		a(0,nz)=1+k1(nz)
		a(1,nz)=0.
	
		b(nz)=temp(nz)+flux(2)*kc(nz)



	call tridia(nz,a,b,temp)

        return
	end  subroutine wall	




	subroutine wall_coeff(nz,dt,dz,cs,k,k1,k2,kc)

	implicit none
	



	integer nz		
	real dt			
	real dz(nz)		
	real cs(nz)		
	real k(nz+1)		





	real flux(2)		




        real k1(20)
        real k2(20)
        real kc(20)



	integer iz
	real kf(nz)



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



	subroutine hsinsflux(swsurf,swwin,tin,tw,hsins)	
	
	implicit none
	










	integer swsurf  
        integer swwin   
	real tin	
	real tw		




	real hsins	


	real hc		


	if (swsurf.eq.2) then	
         if (swwin.eq.1) then
            hc=5.678*0.99        
         else
            hc=5.678*1.09        
         endif
	 hsins=hc*(tin-tw)	
	endif
	
	if (swsurf.eq.1)  then   
         if (swwin.eq.1) then
           hc=5.678*0.99        
         else
           hc=5.678*1.09        
         endif
         hsins=hc*(tin-tw)
        endif 		

	return
	end subroutine hsinsflux



	subroutine int_rsrad(albwin,albwal,pwin,rswal,&
                             surwal,bw,bl,zw,rsint)
	

	implicit none




	real albwin		
	real albwal		
	real rswal(4)		
        real surwal(6) 		
	real bw,bl		
	real zw			
	real pwin               
	


	real rsint(6)		



	real transmit   
        real rstr	
        real surtotwal  
	integer iw
	real b(6)	
	real a(6,6)	





                    
            rstr = 0.
            do iw=1,4
               transmit=1.-albwin
               rstr = rstr+(surwal(iw)*pwin)*(transmit*rswal(iw))
            enddo





            surtotwal=0.
            do iw=1,6
               surtotwal=surtotwal+surwal(iw)
            enddo
            
            rstr=rstr/surtotwal
 		

	
	    call algebra_short(rstr,albwal,albwin,bw,bl,zw,pwin,a,b)
		
	    call gaussjbem(a,6,b,6)
	
            do iw=1,6
               rsint(iw)=b(iw)
            enddo

	    return
	    end subroutine int_rsrad




	subroutine int_rlrad(emwal,emwin,sigma,twal_int,twin,&
     			     pwin,bw,bl,zw,rlint)
	

	implicit none





	real emwal	
	real emwin	
	real sigma	
	real twal_int(6)
	real twin(4)	
	real bw		
	real bl		
	real zw		
	real pwin       




	real rlint(6)	



	
	real b(6)	
	real a(6,6)	
        integer iw




	call algebra_long(emwal,emwin,sigma,twal_int,twin,pwin,&
                          bw,bl,zw,a,b)
  			  
	call gaussjbem(a,6,b,6)

        do iw=1,6
           rlint(iw)=b(iw)
        enddo
            
	return
	end subroutine int_rlrad	




	subroutine algebra_short(rstr,albwal,albwin,aw,bw,zw,pwin,a,b)
    















	implicit none




	real rstr	
	real albwal	
	real albwin	
	real bw		
	real aw		
	real zw		
	real fprl_int	
	real fnrm_int	
	real pwin       


	real a(6,6)		
	real b(6)		


	integer iw,jw	
	real albm               




	do iw=1,6
           b(iw)= 0.
	  do jw=1,6
           a(iw,jw)= 0. 
          enddo
        enddo 



	do iw=1,6
	 b(iw)=-rstr
	end do	



	albm=pwin*albwin+(1-pwin)*albwal
	


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




	subroutine algebra_long(emwal,emwin,sigma,twalint,twinint,&
     				pwin,aw,bw,zw,a,b)















        implicit none 
	





	real pwin       
	real emwal	
	real emwin	
	real sigma	
	real twalint(6) 
	real twinint(4)	
	real aw		
	real bw		
	real zw		
	real fprl_int	
	real fnrm_int	
        real fnrm_intx	
        real fnrm_inty	



	real b(6)	
	real a(6,6)	


	integer iw,jw
	real b_wall(6)	
	real b_wind(6)
	real emwal_av		
	real emwin_av		
	real em_av		
        real twal_int(6)        
	real twin(4)   		





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
	 



	emwal_av=(1-pwin)*emwal
	emwin_av=pwin*emwin
	em_av=emwal_av+emwin_av
	



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
     



	do iw=1,6
	 b(iw)=b_wall(iw)+b_wind(iw)
	end do     





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





	subroutine fluxroo(hseqocc,hleqocc,hsvent,hlvent, &
                           hscond,hslev,hllev) 
	





	implicit none





	real hseqocc		
	real hleqocc		
	real hsvent		
	real hlvent		
	real hscond		



	real hslev		
	real hllev		




	hslev=hseqocc+hsvent+hscond 
 

	
	hllev=hleqocc+hlvent
        
	return
	end subroutine fluxroo




	subroutine phirat(nhourday,rocc)






        implicit none




	real nhourday	
	


	real rocc       


        rocc=1.

	return
	end subroutine phirat




	subroutine phiequ(nhourday,hsesf,hsequip,hsequ)




        implicit none



	real nhourday 
        real, intent(in) :: hsesf
        real, intent(in), dimension(24) :: hsequip
	


	real hsequ    



        hsequ = hsequip(int(nhourday)+1) * hsesf
        
	end subroutine phiequ



	subroutine fluxeqocc(nhourday,bw,bl,perflo,hsesf,hsequip,hseqocc,hleqocc)
	
	implicit none








	real bw			
	real bl			
	real nhourday		
        real, intent(in) :: perflo 
        real, intent(in) :: hsesf
        real, intent(in), dimension(24) :: hsequip



	real hseqocc		
	real hleqocc		


	real Af			
	real rocc		
        real hsequ		

        real hsocc		
                                
        parameter (hsocc=160.)

        real hlocc		
                                
        parameter (hlocc=1.96e6/86400.)





	 Af=bw*bl

	 call phirat(nhourday,rocc)

         call phiequ(nhourday,hsesf,hsequip,hsequ)

         hseqocc=Af*rocc*perflo*hsocc+Af*hsequ






         hleqocc=Af*rocc*perflo*hlocc

	return
	end subroutine fluxeqocc



	
	subroutine fluxvent(cpint,rhoint,vollev,tlev,tout,latent,&
                            humout,rhoout,humlev,beta,hsvent,hlvent)
	
	implicit none








	real cpint		
	real rhoint		
	real vollev		
	real tlev		
	real tout		
	real latent		
	real humout		
	real rhoout		
	real humlev		
        real, intent(in) :: beta
	


	real hsvent		
	real hlvent		



        




        
	hsvent=(1.-beta)*cpint*rhoint*(vollev/3600.)*  &
               (tout-tlev)
	


       
	hlvent=(1.-beta)*latent*rhoint*(vollev/3600.)* &
     	       (humout-humlev)


	return
	end subroutine fluxvent



	
	subroutine fluxcond(hswalins,hswinins,surwal,pwin,hscond)
	
	implicit none








	real hswalins(6)	
	real hswinins(4)	
	real surwal(6)	        
	real pwin               




	
	real hscond		
	



	integer ivw



	  hscond=0.

	do ivw=1,4
	   hscond=hscond+surwal(ivw)*(1-pwin)*hswalins(ivw)+ &
                  surwal(ivw)*pwin*hswinins(ivw)	         
	end do

	do ivw=5,6
    	   hscond=hscond+surwal(ivw)*hswalins(ivw)	
	end do




        hscond=(-1)*hscond 

	return
	end subroutine fluxcond



	
	subroutine regtemp(swcond,nhourday,dt,Qb,hsroo,          &
                           tlev,timeon,timeoff,targtemp,gaptemp,hsneed)
	
	implicit none








        integer swcond       
	real nhourday        
	real dt	             
	real Qb		     
        real hsroo           
        real tlev            
        real, intent(in) :: timeon  
        real, intent(in) :: timeoff 
        real, intent(in) :: targtemp
        real, intent(in) :: gaptemp 
        




        real templev         
        real alpha           
                             


	real hsneed	     



        templev = 0.
        alpha   = 0.

        if (swcond.eq.0) then 
            hsneed = 0.
            goto 100
        else
            if ((nhourday.ge.timeon).and.(nhourday.le.timeoff)) then
               templev=tlev+(dt/Qb)*hsroo
               goto 200
            else
               hsneed = 0.     
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
     


         
	 subroutine reghum(swcond,nhourday,dt,volroo,rhoint,latent, &
                           hlroo,shumroo,timeon,timeoff,targhum,gaphum,hlneed)

	 implicit none








        integer swcond    
	real nhourday     
	real dt	          
	real volroo       
        real rhoint       
        real latent       
        real hlroo        
        real shumroo      
        real, intent(in) :: timeon  
        real, intent(in) :: timeoff 
        real, intent(in) :: targhum 
        real, intent(in) :: gaphum  




        real humlev       
        real betha        
                          


	real hlneed	  



        humlev = 0.
        betha  = 0.

        if (swcond.eq.0) then 
            hlneed = 0.
            goto 100
        else
            if ((nhourday.ge.timeon).and.(nhourday.le.timeoff)) then
               humlev=shumroo+(dt/(latent*rhoint*volroo))*hlroo
               goto 200
            else
               hlneed = 0.     
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



         
         subroutine air_cond(hsneed,hlneed,dt,hsout,hlout,consump,cop)

         implicit none





         real, intent(in) :: cop



         real hsneed     
                         
         real hlneed     
                         
         real dt         



         real hsout      
         real hlout      
         real consump    
                         
    



         if (hsneed.gt.0) then         
                                       
	  hsout=(1/cop)*(abs(hsneed)+abs(hlneed))+hsneed
          hlout=hlneed
          consump=(1./cop)*(abs(hsneed)+abs(hlneed))



         else if(hsneed.eq.0.) then 
               hlneed=0.       
               hsout=0.        
               hlout=0.        
               consump=0.      

              else  
               hlneed=0.           
               hlout=0.            
               consump=(1./cop)*(abs(hsneed)+abs(hlneed))




               hsout=0.                            
         end if

         return 
         end subroutine air_cond




        subroutine consump_total(nzcanm,nlev,consumpbuild,hsoutbuild, &
                                 hsout,consump)

        implicit none
        








        integer nzcanm            
        real hsout(nzcanm)        
        real consump(nzcanm)      



	real consumpbuild         
        real hsoutbuild           
                                  



        integer ilev




        integer nlev     
        



        consumpbuild=0.
        hsoutbuild=0.

        do ilev=1,nlev
           consumpbuild=consumpbuild+consump(ilev)
           hsoutbuild=hsoutbuild+hsout(ilev)
        enddo 

        consumpbuild=consumpbuild/(3.6e+06)

        return 
        end subroutine consump_total


        subroutine tridia(n,a,b,x)












        implicit none


        integer n
        real a(-1:1,n)           
                               
                               
        real b(n)


        real x(n)


        integer i



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


      
       subroutine gaussjbem(a,n,b,np)









       implicit none




       integer np
       real a(np,np)




       real b(np)




      integer nmax
      parameter (nmax=150)

      real big,dum
      integer i,icol,irow
      integer j,k,l,ll,n
      integer ipiv(nmax)
      real pivinv




       
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
                     call wrf_error_fatal3("<stdin>",2225,&
'singular matrix in gaussjbem' )
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
         
         if(a(icol,icol).eq.0) call wrf_error_fatal3("<stdin>",2247,&
'singular matrix in gaussjbem' )
         
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
         



      subroutine radfluxs(radflux,alb,rs,em,rl,sigma,twal)

      implicit none




	
	real alb	
	real rs		
	real em		
	real rl 	
	real sigma	
	real twal	
	real radflux
	
	 radflux=(1.-alb)*rs+em*rl-em*sigma*twal**4
	
      return
      end subroutine radfluxs








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



END MODULE module_sf_bem
