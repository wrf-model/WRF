!
!		couple CB05 with MADE/SORGAM, NCSU
!

        if(p_nu0.gt.1)then

            rxylho =   ARR2(1.7D-11, -116.0_dp, TEMP); !ARR2(1.89D-11, -116.0_dp, TEMP);
            rtolho =   ARR2(1.8D-12, -355.0_dp, TEMP); !ARR2(2.1D-12, -322.0_dp, TEMP);
            rcslho =   4.1D-11;                        !4.0D-11 ;
            rcslno3 =  2.2D-11;
            rhc8ho =   1.97D-11;                       !ARR2(3.64D-11, 380.0_dp, TEMP);
            roliho =   ARR2(1.0D-11, -550.0_dp, TEMP); !ARR2(1.07D-11, -549.0_dp, TEMP);
            rolino3 =  ARR2(9.6D-13, 270.0_dp, TEMP);  !ARR2(3.23D-11, 975.0_dp, TEMP);
            rolio3 =   ARR2(8.4D-15, 1100.0_dp, TEMP); !ARR2(7.29D-15, 1136.0_dp, TEMP);
            roltho =   3.2D-11;                        !ARR2(5.32D-12, -504.0_dp, TEMP);
            roltno3 =  ARR2(7.0D-13, 2160.0_dp, TEMP); !ARR2(1.0D-11, 1895.0_dp, TEMP);
            rolto3 =   ARR2(6.5D-15, 1900.0_dp, TEMP); !ARR2(1.32D-14, 2105.0_dp, TEMP);
            rapiho =   5.37D-11;                       !ARR2(1.21D-11, -444.0_dp, TEMP);
            rapino3 =  2.31D-12;                       !ARR2(1.19D-12, -490.0_dp, TEMP);
            rapio3 =   8.66D-17;                       !ARR2(1.01D-15, 736.0_dp, TEMP);
            rlimho =   1.71D-10;
            rlimno3 =  1.22D-11;
            rlimo3 =   2.00D-16;

            PRDROG(PXYL)  = rxylho * var(ind_xyl)*var(ind_oh)
            PRDROG(PTOL)  = rtolho * var(ind_tol)*var(ind_oh)
            PRDROG(PCSL1) = rcslho * var(ind_cres)*var(ind_oh)
            PRDROG(PCSL2) = rcslno3* var(ind_cres)*var(ind_no3)
            PRDROG(PHC8)  = rhc8ho * var(ind_alkh)*var(ind_oh)
            PRDROG(POLI1) = roliho * var(ind_iole)*var(ind_oh)
            PRDROG(POLI2) = rolino3 * var(ind_iole)*var(ind_no3)
            PRDROG(POLI3) = rolio3 * var(ind_iole)*var(ind_o3)
            PRDROG(POLT1) = roltho * var(ind_ole)*var(ind_oh)
            PRDROG(POLT2) = roltno3 * var(ind_ole)*var(ind_no3)
            PRDROG(POLT3) = rolto3 * var(ind_ole)*var(ind_o3)
            PRDROG(PAPI1) = rapiho * var(ind_apin)*var(ind_oh)
            PRDROG(PAPI2) = rapino3 * var(ind_apin)*var(ind_no3)
            PRDROG(PAPI3) = rapio3 * var(ind_apin)*var(ind_o3)
            PRDROG(PLIM1) = rlimho * var(ind_lim)*var(ind_oh)
            PRDROG(PLIM2) = rlimno3 * var(ind_lim)*var(ind_no3)
            PRDROG(PLIM3) = rlimo3 * var(ind_lim)*var(ind_o3)

            DO n = 1, LDROG
               VDROG3( i,k,j, n ) =  oconv * PRDROG( n ) * DTSTEPC
               VDROG3( i,k,j,n  ) = MAX( 0., VDROG3( i,k,j, n ) )
            ENDDO

        endif

