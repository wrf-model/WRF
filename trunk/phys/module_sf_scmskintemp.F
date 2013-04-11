!
!
!
!
module module_sf_scmskintemp
contains
!
!-------------------------------------------------------------------
!
   subroutine scmskintemp(tsk, julian_in, itimestep,                            &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                                  its, ite, jts, jte, kts, kte, &
                                                  itimestep        
!   
   real, intent(in)      ::                                          julian_in
!
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                tsk

!  local vars

   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   real                  ::                         julian_s, julian_e, fc_int  
   real, dimension( 0:n_max ) ::                             fc_tsk, fc_julian
   logical               ::                                        end_of_file
!
!-----open scmtemp_bdy and read the julian_s, julian_e, fc_int
!
   open(unit=11, file='scmtemp_bdy', form='formatted', status='old')
   read(11,*) julian_s, julian_e, fc_int
!
     end_of_file = .false.
     n=1
     do while (.not. end_of_file)
       read(11,*,end=100) fc_tsk(n)
       fc_julian(n)=julian_s+real(n-1)*fc_int/86400.
       n=n+1
       go to 110
 100   end_of_file = .true.  
 110   continue
     enddo
     nt=n-1
   close(11)
!
!-----linear interpolation of the skin temperature for each time step
!
   do n=1,nt 
     if (julian_in.ge.fc_julian(n) .and. julian_in.lt.fc_julian(n+1)) then
       fc_tsk(0)= fc_tsk(n)                                                     &
                +(fc_tsk(n+1)-fc_tsk(n))*(julian_in-fc_julian(n))/fc_int/86400.
     endif
   enddo
!
!-----compute skin temperature
!
   do j=jts,jte
     do i=its,ite
       tsk(i,j)=fc_tsk(0)
     enddo
   enddo 

   end subroutine scmskintemp
!-------------------------------------------------------------------
end module module_sf_scmskintemp
