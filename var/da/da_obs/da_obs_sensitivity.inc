subroutine da_obs_sensitivity(ktr, iv)

   !-------------------------------------------------------------------------
   ! Purpose:        Apply R-1 to KT.dF/dx to obtain Observation sensitivity
   !                 stored in "iv" for every observation
   !
   ! Called from da_minimise_lz
   !
   ! History: 12/12/2008  Creation (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none

   type (y_type), intent(in)         :: ktr         
   type (iv_type), intent(inout)     :: iv      
   
   integer, parameter                :: nbvar = 6
   integer, parameter                :: npredmax = 8

   integer, parameter                :: u     = 1
   integer, parameter                :: v     = 2
   integer, parameter                :: t     = 3
   integer, parameter                :: p     = 4
   integer, parameter                :: q     = 5
   integer, parameter                :: rv    = 6

   integer                           :: i, j, k, n, inst
   integer                           :: npred, ipred
   real                              :: ktd(25,nbvar)
   real                              :: ktdrad(iv%num_inst)
   real                              :: ktbrad(iv%num_inst,npredmax) 
   real                              :: ktd_global(25,nbvar)
   real                              :: ktdrad_global(iv%num_inst)
   real                              :: ktbrad_global(iv%num_inst, npredmax) 

   if (trace_use) call da_trace_entry("da_obs_sensitivity")

   ktd    = 0.0
   ktdrad = 0.0
   ktbrad = 0.0
       
        if (num_pseudo > 0) then
            do n=1, iv%info(pseudo)%nlocal
	       if (.not. iv%info(pseudo)%proc_domain(1,n)) cycle
	       iv%pseudo(n)%u%sens = ktr%pseudo(n)%u / (iv%pseudo(n)%u%error **2)
	       iv%pseudo(n)%v%sens = ktr%pseudo(n)%v / (iv%pseudo(n)%v%error **2)
	       iv%pseudo(n)%t%sens = ktr%pseudo(n)%t / (iv%pseudo(n)%t%error **2)
	       iv%pseudo(n)%p%sens = ktr%pseudo(n)%p / (iv%pseudo(n)%p%error **2)
	       iv%pseudo(n)%q%sens = ktr%pseudo(n)%q / (iv%pseudo(n)%q%error **2)	   
	       
	       iv%pseudo(n)%u%inv  = iv%pseudo(n)%u%inv * iv%pseudo(n)%u%sens
	       iv%pseudo(n)%v%inv  = iv%pseudo(n)%v%inv * iv%pseudo(n)%v%sens
	       iv%pseudo(n)%t%inv  = iv%pseudo(n)%t%inv * iv%pseudo(n)%t%sens
	       iv%pseudo(n)%p%inv  = iv%pseudo(n)%p%inv * iv%pseudo(n)%p%sens
	       iv%pseudo(n)%q%inv  = iv%pseudo(n)%q%inv * iv%pseudo(n)%q%sens	  
	       
	       ktd(pseudo,u) = ktd(pseudo,u) + iv%pseudo(n)%u%inv
               ktd(pseudo,v) = ktd(pseudo,v) + iv%pseudo(n)%v%inv 
	       ktd(pseudo,t) = ktd(pseudo,t) + iv%pseudo(n)%t%inv 
               ktd(pseudo,p) = ktd(pseudo,p) + iv%pseudo(n)%p%inv 
	       ktd(pseudo,q) = ktd(pseudo,q) + iv%pseudo(n)%q%inv	       	             	       	            	       
	    end do
         end if

         if (iv%info(synop)%nlocal > 0) then
            do n=1, iv%info(synop)%nlocal
	       if (.not. iv%info(synop)%proc_domain(1,n)) cycle
	       iv%synop(n)%u%sens = ktr%synop(n)%u / (iv%synop(n)%u%error **2)
	       iv%synop(n)%v%sens = ktr%synop(n)%v / (iv%synop(n)%v%error **2)
	       iv%synop(n)%t%sens = ktr%synop(n)%t / (iv%synop(n)%t%error **2)
	       iv%synop(n)%p%sens = ktr%synop(n)%p / (iv%synop(n)%p%error **2)
	       iv%synop(n)%q%sens = ktr%synop(n)%q / (iv%synop(n)%q%error **2)	        	       
	       
	       iv%synop(n)%u%inv  = iv%synop(n)%u%inv * iv%synop(n)%u%sens
	       iv%synop(n)%v%inv  = iv%synop(n)%v%inv * iv%synop(n)%v%sens
	       iv%synop(n)%t%inv  = iv%synop(n)%t%inv * iv%synop(n)%t%sens
	       iv%synop(n)%p%inv  = iv%synop(n)%p%inv * iv%synop(n)%p%sens
	       iv%synop(n)%q%inv  = iv%synop(n)%q%inv * iv%synop(n)%q%sens	  
	       
	       ktd(synop,u) = ktd(synop,u) + iv%synop(n)%u%inv
               ktd(synop,v) = ktd(synop,v) + iv%synop(n)%v%inv 
	       ktd(synop,t) = ktd(synop,t) + iv%synop(n)%t%inv 
               ktd(synop,p) = ktd(synop,p) + iv%synop(n)%p%inv 
	       ktd(synop,q) = ktd(synop,q) + iv%synop(n)%q%inv	       	             	       	            	       
	    end do
	 end if   
	       
         if (iv%info(ships)%nlocal > 0) then
            do n=1, iv%info(ships)%nlocal
	       if (.not. iv%info(ships)%proc_domain(1,n)) cycle
	       iv%ships(n)%u%sens = ktr%ships(n)%u / (iv%ships(n)%u%error **2)
	       iv%ships(n)%v%sens = ktr%ships(n)%v / (iv%ships(n)%v%error **2)
	       iv%ships(n)%t%sens = ktr%ships(n)%t / (iv%ships(n)%t%error **2)
	       iv%ships(n)%p%sens = ktr%ships(n)%p / (iv%ships(n)%p%error **2)
	       iv%ships(n)%q%sens = ktr%ships(n)%q / (iv%ships(n)%q%error **2)	        	       
	       
	       iv%ships(n)%u%inv  = iv%ships(n)%u%inv * iv%ships(n)%u%sens
	       iv%ships(n)%v%inv  = iv%ships(n)%v%inv * iv%ships(n)%v%sens
	       iv%ships(n)%t%inv  = iv%ships(n)%t%inv * iv%ships(n)%t%sens
	       iv%ships(n)%p%inv  = iv%ships(n)%p%inv * iv%ships(n)%p%sens
	       iv%ships(n)%q%inv  = iv%ships(n)%q%inv * iv%ships(n)%q%sens	  
	       
	       ktd(ships,u) = ktd(ships,u) + iv%ships(n)%u%inv
               ktd(ships,v) = ktd(ships,v) + iv%ships(n)%v%inv 
	       ktd(ships,t) = ktd(ships,t) + iv%ships(n)%t%inv 
               ktd(ships,p) = ktd(ships,p) + iv%ships(n)%p%inv 
	       ktd(ships,q) = ktd(ships,q) + iv%ships(n)%q%inv	       	             	       	            	       
	    end do
	 end if   
	       
         if (iv%info(metar)%nlocal > 0) then
            do n=1, iv%info(metar)%nlocal
	       if (.not. iv%info(metar)%proc_domain(1,n)) cycle
	       iv%metar(n)%u%sens = ktr%metar(n)%u / (iv%metar(n)%u%error **2)
	       iv%metar(n)%v%sens = ktr%metar(n)%v / (iv%metar(n)%v%error **2)
	       iv%metar(n)%t%sens = ktr%metar(n)%t / (iv%metar(n)%t%error **2)
	       iv%metar(n)%p%sens = ktr%metar(n)%p / (iv%metar(n)%p%error **2)
	       iv%metar(n)%q%sens = ktr%metar(n)%q / (iv%metar(n)%q%error **2)	        	       
	       
	       iv%metar(n)%u%inv  = iv%metar(n)%u%inv * iv%metar(n)%u%sens
	       iv%metar(n)%v%inv  = iv%metar(n)%v%inv * iv%metar(n)%v%sens
	       iv%metar(n)%t%inv  = iv%metar(n)%t%inv * iv%metar(n)%t%sens
	       iv%metar(n)%p%inv  = iv%metar(n)%p%inv * iv%metar(n)%p%sens
	       iv%metar(n)%q%inv  = iv%metar(n)%q%inv * iv%metar(n)%q%sens	  
	       
	       ktd(metar,u) = ktd(metar,u) + iv%metar(n)%u%inv
               ktd(metar,v) = ktd(metar,v) + iv%metar(n)%v%inv 
	       ktd(metar,t) = ktd(metar,t) + iv%metar(n)%t%inv 
               ktd(metar,p) = ktd(metar,p) + iv%metar(n)%p%inv 
	       ktd(metar,q) = ktd(metar,q) + iv%metar(n)%q%inv	       	             	       	            	       
	    end do
	 end if   
	 
         if (iv%info(buoy)%nlocal > 0) then
            do n=1, iv%info(buoy)%nlocal
	       if (.not. iv%info(buoy)%proc_domain(1,n)) cycle
	       iv%buoy(n)%u%sens = ktr%buoy(n)%u / (iv%buoy(n)%u%error **2)
	       iv%buoy(n)%v%sens = ktr%buoy(n)%v / (iv%buoy(n)%v%error **2)
	       iv%buoy(n)%t%sens = ktr%buoy(n)%t / (iv%buoy(n)%t%error **2)
	       iv%buoy(n)%p%sens = ktr%buoy(n)%p / (iv%buoy(n)%p%error **2)
	       iv%buoy(n)%q%sens = ktr%buoy(n)%q / (iv%buoy(n)%q%error **2)	        	       
	       
	       iv%buoy(n)%u%inv  = iv%buoy(n)%u%inv * iv%buoy(n)%u%sens
	       iv%buoy(n)%v%inv  = iv%buoy(n)%v%inv * iv%buoy(n)%v%sens
	       iv%buoy(n)%t%inv  = iv%buoy(n)%t%inv * iv%buoy(n)%t%sens
	       iv%buoy(n)%p%inv  = iv%buoy(n)%p%inv * iv%buoy(n)%p%sens
	       iv%buoy(n)%q%inv  = iv%buoy(n)%q%inv * iv%buoy(n)%q%sens	  
	       
	       ktd(buoy,u) = ktd(buoy,u) + iv%buoy(n)%u%inv
               ktd(buoy,v) = ktd(buoy,v) + iv%buoy(n)%v%inv 
	       ktd(buoy,t) = ktd(buoy,t) + iv%buoy(n)%t%inv 
               ktd(buoy,p) = ktd(buoy,p) + iv%buoy(n)%p%inv 
	       ktd(buoy,q) = ktd(buoy,q) + iv%buoy(n)%q%inv	       	             	       	            	       
	    end do
	 end if   
	 
         if (iv%info(sound)%nlocal > 0) then
            do n=1, iv%info(sound)%nlocal
	       if (.not. iv%info(sound)%proc_domain(1,n)) cycle
	       iv%sonde_sfc(n)%u%sens = ktr%sonde_sfc(n)%u / (iv%sonde_sfc(n)%u%error **2)
	       iv%sonde_sfc(n)%v%sens = ktr%sonde_sfc(n)%v / (iv%sonde_sfc(n)%v%error **2)
	       iv%sonde_sfc(n)%t%sens = ktr%sonde_sfc(n)%t / (iv%sonde_sfc(n)%t%error **2)
	       iv%sonde_sfc(n)%p%sens = ktr%sonde_sfc(n)%p / (iv%sonde_sfc(n)%p%error **2)
	       iv%sonde_sfc(n)%q%sens = ktr%sonde_sfc(n)%q / (iv%sonde_sfc(n)%q%error **2)	        	       
	       
	       iv%sonde_sfc(n)%u%inv  = iv%sonde_sfc(n)%u%inv * iv%sonde_sfc(n)%u%sens
	       iv%sonde_sfc(n)%v%inv  = iv%sonde_sfc(n)%v%inv * iv%sonde_sfc(n)%v%sens
	       iv%sonde_sfc(n)%t%inv  = iv%sonde_sfc(n)%t%inv * iv%sonde_sfc(n)%t%sens
	       iv%sonde_sfc(n)%p%inv  = iv%sonde_sfc(n)%p%inv * iv%sonde_sfc(n)%p%sens
	       iv%sonde_sfc(n)%q%inv  = iv%sonde_sfc(n)%q%inv * iv%sonde_sfc(n)%q%sens	  
	       
	       ktd(sound,u) = ktd(sound,u) + iv%sonde_sfc(n)%u%inv
               ktd(sound,v) = ktd(sound,v) + iv%sonde_sfc(n)%v%inv 
	       ktd(sound,t) = ktd(sound,t) + iv%sonde_sfc(n)%t%inv 
               ktd(sound,p) = ktd(sound,p) + iv%sonde_sfc(n)%p%inv 
	       ktd(sound,q) = ktd(sound,q) + iv%sonde_sfc(n)%q%inv	       	             	       	            	       
	    end do
	 end if   
	 
         if (iv%info(qscat)%nlocal > 0) then
            do n=1, iv%info(qscat)%nlocal
	       if (.not. iv%info(qscat)%proc_domain(1,n)) cycle
	       iv%qscat(n)%u%sens = ktr%qscat(n)%u / (iv%qscat(n)%u%error **2)
	       iv%qscat(n)%v%sens = ktr%qscat(n)%v / (iv%qscat(n)%v%error **2)
	       
	       iv%qscat(n)%u%inv  = iv%qscat(n)%u%inv * iv%qscat(n)%u%sens
	       iv%qscat(n)%v%inv  = iv%qscat(n)%v%inv * iv%qscat(n)%v%sens
	       
	       ktd(qscat,u) = ktd(qscat,u) + iv%qscat(n)%u%inv
               ktd(qscat,v) = ktd(qscat,v) + iv%qscat(n)%v%inv 
	    end do
	 end if   
	 
         if (iv%info(sound)%nlocal > 0) then
            do n=1, iv%info(sound)%nlocal
               do k=1, iv%info(sound)%levels(n)
	          if (.not. iv%info(sound)%proc_domain(k,n)) cycle
	          iv%sound(n)%u(k)%sens = ktr%sound(n)%u(k) / (iv%sound(n)%u(k)%error **2)
	          iv%sound(n)%v(k)%sens = ktr%sound(n)%v(k) / (iv%sound(n)%v(k)%error **2)
	          iv%sound(n)%t(k)%sens = ktr%sound(n)%t(k) / (iv%sound(n)%t(k)%error **2)
	          iv%sound(n)%q(k)%sens = ktr%sound(n)%q(k) / (iv%sound(n)%q(k)%error **2)		  
	       
	          iv%sound(n)%u(k)%inv  = iv%sound(n)%u(k)%inv * iv%sound(n)%u(k)%sens
	          iv%sound(n)%v(k)%inv  = iv%sound(n)%v(k)%inv * iv%sound(n)%v(k)%sens
	          iv%sound(n)%t(k)%inv  = iv%sound(n)%t(k)%inv * iv%sound(n)%t(k)%sens
	          iv%sound(n)%q(k)%inv  = iv%sound(n)%q(k)%inv * iv%sound(n)%q(k)%sens	  
	       
	          ktd(sound,u) = ktd(sound,u) + iv%sound(n)%u(k)%inv
                  ktd(sound,v) = ktd(sound,v) + iv%sound(n)%v(k)%inv 
	          ktd(sound,t) = ktd(sound,t) + iv%sound(n)%t(k)%inv 
	          ktd(sound,q) = ktd(sound,q) + iv%sound(n)%q(k)%inv	       	             	       	            	       
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(mtgirs)%nlocal > 0) then
            do n=1, iv%info(mtgirs)%nlocal
               do k=1, iv%info(mtgirs)%levels(n)
	          if (.not. iv%info(mtgirs)%proc_domain(k,n)) cycle
	          iv%mtgirs(n)%u(k)%sens = ktr%mtgirs(n)%u(k) / (iv%mtgirs(n)%u(k)%error **2)
	          iv%mtgirs(n)%v(k)%sens = ktr%mtgirs(n)%v(k) / (iv%mtgirs(n)%v(k)%error **2)
	          iv%mtgirs(n)%t(k)%sens = ktr%mtgirs(n)%t(k) / (iv%mtgirs(n)%t(k)%error **2)
	          iv%mtgirs(n)%q(k)%sens = ktr%mtgirs(n)%q(k) / (iv%mtgirs(n)%q(k)%error **2)
	       
	          iv%mtgirs(n)%u(k)%inv  = iv%mtgirs(n)%u(k)%inv * iv%mtgirs(n)%u(k)%sens
	          iv%mtgirs(n)%v(k)%inv  = iv%mtgirs(n)%v(k)%inv * iv%mtgirs(n)%v(k)%sens
	          iv%mtgirs(n)%t(k)%inv  = iv%mtgirs(n)%t(k)%inv * iv%mtgirs(n)%t(k)%sens
	          iv%mtgirs(n)%q(k)%inv  = iv%mtgirs(n)%q(k)%inv * iv%mtgirs(n)%q(k)%sens	  
	       
	          ktd(mtgirs,u) = ktd(mtgirs,u) + iv%mtgirs(n)%u(k)%inv
                  ktd(mtgirs,v) = ktd(mtgirs,v) + iv%mtgirs(n)%v(k)%inv 
	          ktd(mtgirs,t) = ktd(mtgirs,t) + iv%mtgirs(n)%t(k)%inv 
	          ktd(mtgirs,q) = ktd(mtgirs,q) + iv%mtgirs(n)%q(k)%inv	       	             	       	            	       
	       end do
	    end do
	 end if   
	       
         if (iv%info(bogus)%nlocal > 0) then
            do n=1, iv%info(bogus)%nlocal
               do k=1, iv%info(bogus)%levels(n)
	          if (.not. iv%info(bogus)%proc_domain(k,n)) cycle
	          iv%bogus(n)%u(k)%sens = ktr%bogus(n)%u(k) / (iv%bogus(n)%u(k)%error **2)
	          iv%bogus(n)%v(k)%sens = ktr%bogus(n)%v(k) / (iv%bogus(n)%v(k)%error **2)
	          iv%bogus(n)%t(k)%sens = ktr%bogus(n)%t(k) / (iv%bogus(n)%t(k)%error **2)
	          iv%bogus(n)%q(k)%sens = ktr%bogus(n)%q(k) / (iv%bogus(n)%q(k)%error **2)
	       
	          iv%bogus(n)%u(k)%inv  = iv%bogus(n)%u(k)%inv * iv%bogus(n)%u(k)%sens
	          iv%bogus(n)%v(k)%inv  = iv%bogus(n)%v(k)%inv * iv%bogus(n)%v(k)%sens
	          iv%bogus(n)%t(k)%inv  = iv%bogus(n)%t(k)%inv * iv%bogus(n)%t(k)%sens
	          iv%bogus(n)%q(k)%inv  = iv%bogus(n)%q(k)%inv * iv%bogus(n)%q(k)%sens	  
	       
	          ktd(bogus,u) = ktd(bogus,u) + iv%bogus(n)%u(k)%inv
                  ktd(bogus,v) = ktd(bogus,v) + iv%bogus(n)%v(k)%inv 
	          ktd(bogus,t) = ktd(bogus,t) + iv%bogus(n)%t(k)%inv 
	          ktd(bogus,q) = ktd(bogus,q) + iv%bogus(n)%q(k)%inv	       	             	       	            	       
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(pilot)%nlocal > 0) then
            do n=1, iv%info(pilot)%nlocal
               do k=1, iv%info(pilot)%levels(n)
	          if (.not. iv%info(pilot)%proc_domain(k,n)) cycle
	          iv%pilot(n)%u(k)%sens = ktr%pilot(n)%u(k) / (iv%pilot(n)%u(k)%error **2)
	          iv%pilot(n)%v(k)%sens = ktr%pilot(n)%v(k) / (iv%pilot(n)%v(k)%error **2)
	       
	          iv%pilot(n)%u(k)%inv  = iv%pilot(n)%u(k)%inv * iv%pilot(n)%u(k)%sens
	          iv%pilot(n)%v(k)%inv  = iv%pilot(n)%v(k)%inv * iv%pilot(n)%v(k)%sens
	       
	          ktd(pilot,u) = ktd(pilot,u) + iv%pilot(n)%u(k)%inv
                  ktd(pilot,v) = ktd(pilot,v) + iv%pilot(n)%v(k)%inv 
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(airep)%nlocal > 0) then
            do n=1, iv%info(airep)%nlocal
               do k=1, iv%info(airep)%levels(n)
	          if (.not. iv%info(airep)%proc_domain(k,n)) cycle
	          iv%airep(n)%u(k)%sens = ktr%airep(n)%u(k) / (iv%airep(n)%u(k)%error **2)
	          iv%airep(n)%v(k)%sens = ktr%airep(n)%v(k) / (iv%airep(n)%v(k)%error **2)
	          iv%airep(n)%t(k)%sens = ktr%airep(n)%t(k) / (iv%airep(n)%t(k)%error **2)
	       
	          iv%airep(n)%u(k)%inv  = iv%airep(n)%u(k)%inv * iv%airep(n)%u(k)%sens
	          iv%airep(n)%v(k)%inv  = iv%airep(n)%v(k)%inv * iv%airep(n)%v(k)%sens
	          iv%airep(n)%t(k)%inv  = iv%airep(n)%t(k)%inv * iv%airep(n)%t(k)%sens
	       
	          ktd(airep,u) = ktd(airep,u) + iv%airep(n)%u(k)%inv
                  ktd(airep,v) = ktd(airep,v) + iv%airep(n)%v(k)%inv 
	          ktd(airep,t) = ktd(airep,t) + iv%airep(n)%t(k)%inv 
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(geoamv)%nlocal > 0) then
            do n=1, iv%info(geoamv)%nlocal
               do k=1, iv%info(geoamv)%levels(n)
	          if (.not. iv%info(geoamv)%proc_domain(k,n)) cycle
	          iv%geoamv(n)%u(k)%sens = ktr%geoamv(n)%u(k) / (iv%geoamv(n)%u(k)%error **2)
	          iv%geoamv(n)%v(k)%sens = ktr%geoamv(n)%v(k) / (iv%geoamv(n)%v(k)%error **2)
	       
	          iv%geoamv(n)%u(k)%inv  = iv%geoamv(n)%u(k)%inv * iv%geoamv(n)%u(k)%sens
	          iv%geoamv(n)%v(k)%inv  = iv%geoamv(n)%v(k)%inv * iv%geoamv(n)%v(k)%sens
	       
	          ktd(geoamv,u) = ktd(geoamv,u) + iv%geoamv(n)%u(k)%inv
                  ktd(geoamv,v) = ktd(geoamv,v) + iv%geoamv(n)%v(k)%inv 
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(polaramv)%nlocal > 0) then
            do n=1, iv%info(polaramv)%nlocal
               do k=1, iv%info(polaramv)%levels(n)
	          if (.not. iv%info(polaramv)%proc_domain(k,n)) cycle
	          iv%polaramv(n)%u(k)%sens = ktr%polaramv(n)%u(k) / (iv%polaramv(n)%u(k)%error **2)
	          iv%polaramv(n)%v(k)%sens = ktr%polaramv(n)%v(k) / (iv%polaramv(n)%v(k)%error **2)
	       
	          iv%polaramv(n)%u(k)%inv  = iv%polaramv(n)%u(k)%inv * iv%polaramv(n)%u(k)%sens
	          iv%polaramv(n)%v(k)%inv  = iv%polaramv(n)%v(k)%inv * iv%polaramv(n)%v(k)%sens
	       
	          ktd(polaramv,u) = ktd(polaramv,u) + iv%polaramv(n)%u(k)%inv
                  ktd(polaramv,v) = ktd(polaramv,v) + iv%polaramv(n)%v(k)%inv 
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(profiler)%nlocal > 0) then
            do n=1, iv%info(profiler)%nlocal
               if (.not. iv%info(profiler)%proc_domain(1,n)) cycle
	       do k=1, iv%info(profiler)%levels(n)
	          iv%profiler(n)%u(k)%sens = ktr%profiler(n)%u(k) / (iv%profiler(n)%u(k)%error **2)
	          iv%profiler(n)%v(k)%sens = ktr%profiler(n)%v(k) / (iv%profiler(n)%v(k)%error **2)
	       
	          iv%profiler(n)%u(k)%inv  = iv%profiler(n)%u(k)%inv * iv%profiler(n)%u(k)%sens
	          iv%profiler(n)%v(k)%inv  = iv%profiler(n)%v(k)%inv * iv%profiler(n)%v(k)%sens
	       
	          ktd(profiler,u) = ktd(profiler,u) + iv%profiler(n)%u(k)%inv
                  ktd(profiler,v) = ktd(profiler,v) + iv%profiler(n)%v(k)%inv 
	       end do
	    end do
	 end if   
	       	       
         if (iv%info(satem)%nlocal > 0) then
            do n=1, iv%info(satem)%nlocal
               if (.not. iv%info(satem)%proc_domain(1,n)) cycle
	       do k=1, iv%info(satem)%levels(n)	    
	          iv%satem(n)%thickness(k)%sens = ktr%satem(n)%thickness(k) / (iv%satem(n)%thickness(k)%error **2)
	       
	          iv%satem(n)%thickness(k)%inv  = iv%satem(n)%thickness(k)%inv * iv%satem(n)%thickness(k)%sens
	       
	          ktd(satem,rv) = ktd(satem,rv) + iv%satem(n)%thickness(k)%inv
               end do
	    end do
	 end if   
	       
         if (iv%info(gpspw)%nlocal > 0) then
            do n=1, iv%info(gpspw)%nlocal
	       if (.not. iv%info(gpspw)%proc_domain(1,n)) cycle
	       iv%gpspw(n)%tpw%sens = ktr%gpspw(n)%tpw / (iv%gpspw(n)%tpw%error **2)
	       
	       iv%gpspw(n)%tpw%inv  = iv%gpspw(n)%tpw%inv * iv%gpspw(n)%tpw%sens
	       
	       ktd(gpspw,q) = ktd(gpspw,q) + iv%gpspw(n)%tpw%inv
	    end do
	 end if   
	       
          if (iv%info(gpsref)%nlocal > 0) then
            do n=1, iv%info(gpsref)%nlocal
               if (.not. iv%info(gpsref)%proc_domain(1,n)) cycle
               do k=1, iv%info(gpsref)%levels(n)
	          if (iv%gpsref(n)%ref(k)%qc < obs_qc_pointer) cycle	    
	          iv%gpsref(n)%ref(k)%sens = ktr%gpsref(n)%ref(k) / (iv%gpsref(n)%ref(k)%error **2)
 	       
	          iv%gpsref(n)%ref(k)%inv  = iv%gpsref(n)%ref(k)%inv * iv%gpsref(n)%ref(k)%sens
	       
	          ktd(gpsref,rv) = ktd(gpsref,rv) + iv%gpsref(n)%ref(k)%inv
              end do
	    end do
	 end if   
	       
         if (iv%info(ssmi_rv)%nlocal > 0) then
            do n=1, iv%info(ssmi_rv)%nlocal
	       if (.not. iv%info(ssmi_rv)%proc_domain(1,n)) cycle
	       iv%ssmi_rv(n)%Speed%sens = ktr%ssmi_rv(n)%Speed / (iv%ssmi_rv(n)%Speed%error **2)
	       
	       iv%ssmi_rv(n)%Speed%inv  = iv%ssmi_rv(n)%Speed%inv * iv%ssmi_rv(n)%Speed%sens
	       
	       ktd(ssmi_rv,rv) = ktd(ssmi_rv,rv) + iv%ssmi_rv(n)%Speed%inv
	    end do
	 end if   
	       
         if (iv%num_inst > 0) then
            do inst = 1, iv%num_inst                                       ! loop for sensor
               if (iv%instid(inst)%num_rad < 1) cycle
	       do n= 1, iv%instid(inst)%num_rad                            ! loop for pixel
                  if (.not. iv%instid(inst)%info%proc_domain(1,n)) cycle
                  do k=1, iv%instid(inst)%nchan	                           ! loop for channel
		     if ( iv%instid(inst)%tb_qc(k,n) < obs_qc_pointer ) cycle
		     iv%instid(inst)%tb_sens(k,n) = ktr%instid(inst)%tb(k,n) / (iv%instid(inst)%tb_error(k,n) **2)
		     
		     iv%instid(inst)%tb_inv(k,n) = iv%instid(inst)%tb_inv(k,n) * iv%instid(inst)%tb_sens(k,n)
		     
	             ktdrad(inst) = ktdrad(inst) + iv%instid(inst)%tb_inv(k,n)
		     
                   ! Impact of Bias Predictors 
		     npred = iv%instid(inst)%varbc(k)%npred
	             do i = 1, npred
		        ipred = iv%instid(inst)%varbc(k)%ipred(i)
	                ktbrad(inst,ipred) = ktbrad(inst,ipred) - &
			                     iv%instid(inst)%varbc(k)%param(i) * &
					     iv%instid(inst)%varbc_info%pred(ipred,n) * &
					     iv%instid(inst)%tb_inv(k,n)
		     end do
		     
                  end do                                                   ! loop for channel
	       end do                                                      ! loop for pixel
	    end do                                                         ! loop for sensor
	 end if   
	       
	! Sum across processors
	 do i = 1, nbvar 
            call wrf_dm_sum_reals(ktd(:,i), ktd_global(:,i))
	 end do

	 call wrf_dm_sum_reals(ktdrad, ktdrad_global)

	 do i = 1, npredmax 
            call wrf_dm_sum_reals(ktbrad(:,i), ktbrad_global(:,i))
	 end do
         
         write(stdout,*) 'Impact of Conventional Observations: U, V, T, P, Q, GPS'
	 write(stdout,*) SUM(ktd_global,dim=1)
	 write(stdout,*) 'Impact of Conventional Observations: Sound, Synop, Pilot, Satem, GeoAMV, PolarAMV, AIREP, GPSZTD, GPSRF, METAR'
	 write(stdout,*) 'Ships, SSMI_RV, SSMI_TB, SSMT1, SSMT2, QSCAT, Profiler, Buoy, Bogus, Pseudo, Radar, AIRSR, Sonde_sfc, MTGIRS, TAMDAR'
	 write(stdout,*) SUM(ktd_global,dim=2)
	 write(stdout,*) 'Impact of Satellite Radiances for each instrument: '
	 write(stdout,*) ktdrad_global
	 write(stdout,*) 'Impact of Satellite Bias Correction for each predictor: ' 
	 write(stdout,*) SUM(ktbrad_global,dim=1)
	 write(stdout,*) 'Impact of Satellite Bias Correction for each instrument:' 
	 write(stdout,*) SUM(ktbrad_global,dim=2)
	 write(stdout,*) 'ADJOINT_TEST1:', SUM(ktd_global)+SUM(ktdrad_global)

   if (trace_use) call da_trace_exit("da_obs_sensitivity")

end subroutine da_obs_sensitivity

