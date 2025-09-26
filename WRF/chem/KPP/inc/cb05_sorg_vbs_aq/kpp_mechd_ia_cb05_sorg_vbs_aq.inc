! RAR: modified to handle the new SOA mechanism based on the VBS approach and multi-generational
! VOC oxidation mechanism
! Correspondence among CB05, RACM, and SAPRC-99 SOA precursors
! KW CB05   RACM   SAPRC99
! 1) OLE  -> OLT -> OLE1
! 2) IOLE -> OLI -> OLE2
! 3) TOL  -> TOL -> ARO1
! 4) XYL  -> XYL -> ARO2
! 5) CRES -> CSL -> ARO2
! 6) PAR  -> HC5 -> ALK4 !KW temporary setup
! 7) ALKH -> HC8 -> ALK5
! 8) ISOP -> ISO -> ISO
! 9) APIN -> API -> TERP
!    TERP
!    BPIN
!    OCI
!10) LIM  -> LIM -> TERP
!11) HUM  -> SESQ-> SESQ
!
if(p_nu0.gt.1)then

     ! OLE/OLT
     rolto   =  RCONST(116)
     roltho  =  RCONST(117)
     rolto3  =  RCONST(118)
     roltno3 =  RCONST(119)

     ! IOLE/OLI
     rolio   =  RCONST(124)
     roliho  =  RCONST(125)
     rolio3  =  RCONST(126)
     rolino3 =  RCONST(127)

     ! TOL/TOL
     rtolho =   RCONST(128)

     ! XYL/XYL
     rxylho  =  RCONST(138)

     ! CRES/CSL
     rcslho  =  RCONST(131)
     rcslno3 =  RCONST(132)

     ! PAR/HC5
     rhc5ho  =  RCONST(112)

     ! ALKH/HC8
     rhc8ho  =  RCONST(190)

     ! ISOP/ISO
     risoo   =  RCONST(141)
     risoho  =  RCONST(142)
     risoo3  =  RCONST(143)
     risono3 =  RCONST(144)

     ! APIN,BPIN,TERP,OCI/API
     rapiho  =  RCONST(184)
     rapio3  =  RCONST(185)
     rbpiho  =  RCONST(186)
     rbpio3  =  RCONST(187)
     rbpino3 =  RCONST(188)
     rterpo  =  RCONST(149)
     rterpho =  RCONST(150)
     rterpo3 =  RCONST(151)
     rterpno3=  RCONST(152)
     rociho  =  RCONST(183)

     ! LIM/LIM
     rlimho  =  RCONST(182)

     ! HUM,SESQ
     rsesqho =  RCONST(181)

     ! XO2 radical
     rxo2no   =  RCONST(54)
     rxo2nno  =  RCONST(55)
     rxo2ho2  =  RCONST(56)
     rxo2nho2 =  RCONST(57)
     rxo2co3  =  RCONST(93)
     rxo2cxo3 =  RCONST(109)
     rxo2xo2  =  RCONST(58)
     rxo2xo2n =  RCONST(59)
     rxo2nxo2n=  RCONST(60)

     ! production from anthropogenic VOCs
     PRDROG(PALK4)= 0.22*rhc5ho*var(ind_par)*var(ind_oh)
     PRDROG(PALK5)= 0.36*rhc5ho*var(ind_par)*var(ind_oh)+rhc8ho*var(ind_alkh)*var(ind_oh)

     PRDROG(POLE1)= rolto*var(ind_ole)*var(ind_o)+roltho*var(ind_ole)*var(ind_oh) + rolto3*var(ind_ole)*var(ind_o3) + roltno3*var(ind_ole)*var(ind_no3)
     PRDROG(POLE2)= rolio*var(ind_iole)*var(ind_o)+roliho*var(ind_iole)*var(ind_oh) + rolio3*var(ind_iole)*var(ind_o3) + rolino3*var(ind_iole)*var(ind_no3)

     PRDROG(PARO1)= rtolho*var(ind_tol)*var(ind_oh)

     PRDROG(PARO2)= rxylho*var(ind_xyl)*var(ind_oh)
     PRDROG(PARO2)= PRDROG(PARO2) + rcslho*var(ind_cres)*var(ind_oh) + rcslno3*var(ind_cres)*var(ind_no3)
 
     ! Biogenic
     PRDROG(PISOP)= risoo*var(ind_isop)*var(ind_o)+risoho*var(ind_isop)*var(ind_oh) + risoo3*var(ind_isop)*var(ind_o3) + risono3*var(ind_isop)*var(ind_no3)

     PRDROG(PTERP)= rterpo*var(ind_terp)*var(ind_o)+rterpho*var(ind_terp)*var(ind_oh) + rterpo3*var(ind_terp)*var(ind_o3) + rterpno3*var(ind_terp)*var(ind_no3)
     PRDROG(PTERP)= PRDROG(PTERP) + rapiho*var(ind_apin)*var(ind_oh) + rapio3*var(ind_apin)*var(ind_o3)
     PRDROG(PTERP)= PRDROG(PTERP) + rbpiho*var(ind_bpin)*var(ind_oh) + rbpio3*var(ind_bpin)*var(ind_o3) + rbpino3*var(ind_bpin)*var(ind_no3)
     PRDROG(PTERP)= PRDROG(PTERP) + rociho*var(ind_oci)*var(ind_oh) + rlimho*var(ind_lim)*var(ind_oh)

     PRDROG(PSESQ)= rsesqho*var(ind_hum)*var(ind_oh)

! RAR: to calculate the branching ratios to determine high NOx versus low NOx

     PRDROG(PBRCH)= rxo2no*var(ind_no)+rxo2nno*var(ind_no)

     ! VDROG carrying the branching ratios
     if (PRDROG(PBRCH)>1.E-12) then
        ro2loss= PRDROG(PBRCH) + rxo2ho2*var(ind_ho2) + rxo2nho2*var(ind_ho2) + rxo2co3*var(ind_c2o3) + rxo2cxo3*var(ind_cxo3) &
                               + rxo2xo2*var(ind_xo2) + rxo2xo2n*var(ind_xo2) + rxo2nxo2n*var(ind_xo2n)
        VDROG3_VBS( i,k,j,LDROG_VBS )= MIN( 1.D0,(PRDROG(PBRCH)/ro2loss) )
     else
         VDROG3_VBS( i,k,j,LDROG_VBS )= 0.
     end if

     DO n = 1, LDROG_VBS-1
        VDROG3_VBS( i,k,j,n ) =  oconv* PRDROG( n ) * DTSTEPC
        VDROG3_VBS( i,k,j,n ) =  MAX( 0., VDROG3_VBS( i,k,j,n ) )
     ENDDO
endif

! RAR: debugging
!if (i==8 .AND. j==18) then
!   if (k==1) then
!        write(*,*)'rhch5ho',rhc5ho,'rhc8ho',rhc8ho,'rhc8ho',roltho,'roliho',roliho, &
!                  'rtolho',rtolho,'rxylho',rxylho,'rsesqno3',rsesqno3
!        write(*,*)'ind_tol',ind_tol,'var(ind_tol)',var(ind_tol)
!        write(*,*)'ind_ho',ind_ho,'var(ind_ho)',var(ind_ho)
!        write(*,*)'ind_iso',ind_iso,'risoho',risoho
!        write(*,*)'PRDROG(PBRCH)', PRDROG(PBRCH),'ro2loss=',ro2loss        
!        write(*,*)'VDROG3(8,1,18,:)', VDROG3(i,k,j,:)
!   end if
!end if
!
!if (j==18 .AND. k==1) then
!        write(*,*)'VDROG3(:,18,1,:)', VDROG3(i,k,j,:)
!end if
