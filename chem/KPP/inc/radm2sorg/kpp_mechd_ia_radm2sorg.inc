!
!
! CHECK, LUMPING
! 



        if(p_nu0.gt.1)then

 !Note: the indices in the equation fiel are currently shifted by one starting
 ! from eq. 24 or so (check this) 

            rxylho =   RCONST(59)     !ARR2(1.89D-11, -116.0_dp, TEMP); 
            rtolho =   RCONST(58)     !ARR2(2.1D-12, -322.0_dp, TEMP);
            rcslho =   RCONST(60)     !4.0D-11 ;
            rcslno3 =  RCONST(96)     !2.2D-11 ;
            rhc8ho =   RCONST(54)     !ARR2(3.64D-11, 380.0_dp, TEMP);
            roliho =   RCONST(57)     !ARR2(1.07D-11, -549.0_dp, TEMP);
            rolino3 =  RCONST(99)    !ARR2(3.23D-11, 975.0_dp, TEMP);
            rolio3 =   RCONST(103)    !ARR2(7.29D-15, 1136.0_dp, TEMP);
            roltho =   RCONST(56)     !ARR2(5.32D-12, -504.0_dp, TEMP);
            roltno3 =  RCONST(98)     !ARR2(1.0D-11, 1895.0_dp, TEMP);
            rolto3 =   RCONST(102)    !ARR2(1.32D-14, 2105.0_dp, TEMP);

rapiho = 0._dp  
rapino3 = 0._dp
rapio3 = 0._dp
rlimho = 0._dp
rlimno3 =  0._dp
rlimo3 = 0._dp


!!            rapiho =   RCONST(71)     !ARR(1.21e-11,-444.0,t_phy(i,k,j))))
!!            rapino3 =  RCONST(103)    !ARR(1.19e-12,-490.0,t_phy(i,k,j))))
!!            rapio3 =   RCONST(111)    !ARR(1.01e-15,736.0,t_phy(i,k,j)))) !! 732!!
!!            rlimho =   RCONST(72)     !ARR(1.71e-10,0.0,t_phy(i,k,j))))
!!            rlimno3 =  RCONST(104)    !ARR(1.22e-11,0.0,t_phy(i,k,j))))
!!            rlimo3 =   RCONST(112)    !ARR(2.00e-16,0.0,t_phy(i,k,j))))

            PRDROG(PXYL)  = rxylho * var(ind_xyl)*var(ind_oh)
            PRDROG(PTOL)  = rtolho * var(ind_tol)*var(ind_oh)
            PRDROG(PCSL1) = rcslho * var(ind_csl)*var(ind_oh)
            PRDROG(PCSL2) = 0.50_dp * rcslno3* var(ind_csl)*var(ind_no3)
            PRDROG(PHC8)  = rhc8ho * var(ind_hc8)*var(ind_oh)
            PRDROG(POLI1) = roliho * var(ind_oli)*var(ind_oh)
            PRDROG(POLI2) = rolino3 * var(ind_oli)*var(ind_no3)
            PRDROG(POLI3) = rolio3 * var(ind_oli)*var(ind_o3)
            PRDROG(POLT1) = roltho * var(ind_olt)*var(ind_oh)
            PRDROG(POLT2) = roltno3 * var(ind_olt)*var(ind_no3)
            PRDROG(POLT3) = rolto3 * var(ind_olt)*var(ind_o3)
!!            PRDROG(PAPI1) = rapiho * var(ind_api)*var(ind_oh)
!!            PRDROG(PAPI2) = rapino3 * var(ind_api)*var(ind_no3)
!!            PRDROG(PAPI3) = rapio3 * var(ind_api)*var(ind_o3)
!!            PRDROG(PLIM1) = rlimho * var(ind_lim)*var(ind_oh)
!!            PRDROG(PLIM2) = rlimno3 * var(ind_lim)*var(ind_no3)
!!            PRDROG(PLIM3) = rlimo3 * var(ind_lim)*var(ind_o3)

PRDROG(PAPI1) = 0._dp
PRDROG(PAPI2) = 0._dp
PRDROG(PAPI3) = 0._dp
PRDROG(PLIM1) = 0._dp
PRDROG(PLIM2) = 0._dp
PRDROG(PLIM3) = 0._dp

            DO n = 1, LDROG
               VDROG3( i,k,j, n ) =  oconv * PRDROG( n ) * DTSTEPC
               VDROG3( i,k,j,n  ) = MAX( 0., VDROG3( i,k,j, n ) )
            ENDDO
        endif

