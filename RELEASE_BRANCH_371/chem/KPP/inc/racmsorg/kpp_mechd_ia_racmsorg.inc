!
!
! corrupted from RS
! 

        if(p_nu0.gt.1)then
            rxylho =   RCONST(74)     !ARR(7.30e-12,-355.0,t_phy(i,k,j))))
            rtolho =   RCONST(73)     !ARR(1.81e-12,-355.0,t_phy(i,k,j))))
            rcslho =   RCONST(75)     !ARR(6.00e-11,0.0,t_phy(i,k,j))))
            rcslno3 =  RCONST(97)     !ARR(2.20e-11,0.0,t_phy(i,k,j))))
            rhc8ho =   RCONST(65)     !ARR(1.64e-11,125.0,t_phy(i,k,j))))
            roliho =   RCONST(68)     !ARR(1.33e-11,-500.0,t_phy(i,k,j))))
            rolino3 =  RCONST(100)    !ARR(8.64e-13,-450.0,t_phy(i,k,j))))
            rolio3 =   RCONST(108)    !ARR(4.40e-15,845.0,t_phy(i,k,j))))
            roltho =   RCONST(67)     !ARR(5.72e-12,-500.0,t_phy(i,k,j))))
            roltno3 =  RCONST(99)     !ARR(1.79e-13,450.0,t_phy(i,k,j))))
            rolto3 =   RCONST(107)    !ARR(4.33e-15,1800.0,t_phy(i,k,j))))
            rapiho =   RCONST(71)     !ARR(1.21e-11,-444.0,t_phy(i,k,j))))
            rapino3 =  RCONST(103)    !ARR(1.19e-12,-490.0,t_phy(i,k,j))))
            rapio3 =   RCONST(111)    !ARR(1.01e-15,736.0,t_phy(i,k,j)))) !! 732!!
            rlimho =   RCONST(72)     !ARR(1.71e-10,0.0,t_phy(i,k,j))))
            rlimno3 =  RCONST(104)    !ARR(1.22e-11,0.0,t_phy(i,k,j))))
            rlimo3 =   RCONST(112)    !2.00D-16
            PRDROG(PXYL)  = rxylho * var(ind_xyl)*var(ind_ho)
            PRDROG(PTOL)  = rtolho * var(ind_tol)*var(ind_ho)
            PRDROG(PCSL1) = rcslho * var(ind_csl)*var(ind_ho)
            PRDROG(PCSL2) = 0.50_dp * rcslno3* var(ind_csl)*var(ind_no3)
            PRDROG(PHC8)  = rhc8ho * var(ind_hc8)*var(ind_ho)
            PRDROG(POLI1) = roliho * var(ind_oli)*var(ind_ho)
            PRDROG(POLI2) = rolino3 * var(ind_oli)*var(ind_no3)
            PRDROG(POLI3) = rolio3 * var(ind_oli)*var(ind_o3)
            PRDROG(POLT1) = roltho * var(ind_olt)*var(ind_ho)
            PRDROG(POLT2) = roltno3 * var(ind_olt)*var(ind_no3)
            PRDROG(POLT3) = rolto3 * var(ind_olt)*var(ind_o3)
            PRDROG(PAPI1) = rapiho * var(ind_api)*var(ind_ho)
            PRDROG(PAPI2) = rapino3 * var(ind_api)*var(ind_no3)
            PRDROG(PAPI3) = rapio3 * var(ind_api)*var(ind_o3)
            PRDROG(PLIM1) = rlimho * var(ind_lim)*var(ind_ho)
            PRDROG(PLIM2) = rlimno3 * var(ind_lim)*var(ind_no3)
            PRDROG(PLIM3) = rlimo3 * var(ind_lim)*var(ind_o3)
            DO n = 1, LDROG
               VDROG3( i,k,j, n ) =  oconv * PRDROG( n ) * DTSTEPC
               VDROG3( i,k,j,n  ) = MAX( 0., VDROG3( i,k,j, n ) )
            ENDDO
        endif

