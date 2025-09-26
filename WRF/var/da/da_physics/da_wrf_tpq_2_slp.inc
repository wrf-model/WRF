subroutine da_wrf_tpq_2_slp (grid)

   !---------------------------------------------------------------------
   ! Purpose: computes sea level pressure from the rule                
   !          t1/t2=(p1/p2)**(gamma*r/g).                              
   !                                                                     
   ! input       t        temperature                cross    3d       
   !             q        mixing ratio               cross    2d       
   !             p        pressure perturbation      cross    2d       
   !             terr     terrain                    cross    2d       
   !             psfc     surface pressure           cross    2d       
   !                                                                       
   ! output      slp      sea level pressure         cross    2d    
   !---------------------------------------------------------------------

   implicit none   
              

   type (domain), intent(inout) :: grid

   integer              :: I, J, K, KLO, KHI, ij
   real                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL

   real, parameter      :: GAMMA = 6.5E-3,  &
                          TC=t_kelvin+17.5,  &
                          PCONST=10000.0 ,  &
                          EPS   = 0.622

   if (trace_use_dull) call da_trace_entry("da_wrf_tpq_2_slp")

                                                                     
   ! sea level pressure                                            
                                                                         
   xterm=gamma* gas_constant / gravity                                                   
                                                                       
   ! compute pressure at pconst mb above surface (pl)              
                                                                          

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (ij, i, j, k, PL, klo, khi, tlo, thi, tl, ts, t0)
   do ij = 1, grid%num_tiles

   j_loop: do j=grid%j_start(ij), grid%j_end(ij)
      i_loop: do i=its, ite
         if (grid%xb%terr(i,j) <= 0.0) then
            grid%xb%slp(i,j) = grid%xb%psfc(i,j)
            cycle i_loop
         end if

         PL  = grid%xb%psfc(i,j) - PCONST                                        
         klo = 0

         ! Find 2 levels on sigma surfaces surrounding PL at each I,J    

         k_loop: do k=kts, kte-1
            if ((grid%xb%p(i,j,k) >= pl) .and. (grid%xb%p(i,j,k+1) < pl)) then
               khi = k+1
               klo = k
               exit k_loop
            end if
         end do k_loop

         if (klo < 1) then 
            write(unit=message(1),fmt='(A,F11.3,A)') &
               'Cannot find pressure level ',PCONST,' Pa above the surface'
            write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') &
               'PL=',PL,'  PSFC=',grid%xb%psfc(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))                                               
         end if                                                         

         ! get temperature at pl (tl), extrapolate t at surface (ts)     
         ! and t at sea level (t0) with 6.5 k/km lapse rate              

         tlo=grid%xb%t(i,j,klo) * (eps+grid%xb%q(i,j,klo))/(eps*(1.0+grid%xb%q(i,j,klo)))
         thi=grid%xb%t(i,j,khi) * (eps+grid%xb%q(i,j,khi))/(eps*(1.0+grid%xb%q(i,j,khi)))
         tl=thi-(thi-tlo)*log(pl/grid%xb%p(i,j,khi)) &
                      /log(grid%xb%p(i,j,klo)/grid%xb%p(i,j,khi))               
         ts=tl*(grid%xb%psfc(i,j)/pl)**xterm                           
         t0=ts +gamma*grid%xb%terr(i,j)

         ! correct sea level temperature if too hot                      

         if (t0 >= tc) then
            if (ts <= tc) then
               t0 = tc
            else
               t0 = tc-0.005*(ts-tc)**2
            end if
         end if

         ! compute sea level pressure                                    

         grid%xb%slp(i,j)=grid%xb%psfc(i,j)*EXP(2.0*gravity*grid%xb%terr(i,j)/ &
            (gas_constant*(TS+T0)))
      end do i_loop
   end do j_loop

   end do 
   !$OMP END PARALLEL DO  

   if (trace_use_dull) call da_trace_exit("da_wrf_tpq_2_slp")

end subroutine da_wrf_tpq_2_slp


