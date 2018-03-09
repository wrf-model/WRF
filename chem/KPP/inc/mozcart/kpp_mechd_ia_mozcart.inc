!
   IF( irr_option(dm) ) THEN
     DO n = param_first_scalar,num_irr_diag
       irr_rates(i,k,j,n) = real( IRR_WRK(irr_diag_ndx(n-1,dm)),kind=4 )
     ENDDO
   ENDIF
