! RAR: modified to handle the new SOA mechanism based on the VBS approach and multi-generational
! VOC oxidation mechanism
! Correspondence between RACM and SAPRC-99 SOA precursors
! 1) OLT -> OLE1
! 2) OLI -> OLE2
! 3) TOL -> ARO1
! 4) XYL -> ARO2
! 5) CSL -> ARO2
! 6) HC5 -> ALK4
! 7) HC8 -> ALK5
! 8) ISO -> ISO
! 9) API -> TERP
!10) LIM -> TERP
!11) SESQ-> SESQ
!
if(p_nu0.gt.1)then
!liqy modify the rconst according to the new gaseous mechanism.
     ! OLT
     roltho =   RCONST(70)
     rolto3 =   RCONST(110)
     roltno3 =  RCONST(102)

     ! OLI
     roliho =   RCONST(71)
     rolio3  =  RCONST(111)
     rolino3 =  RCONST(103)

     ! TOL
     rtolho =   RCONST(76)

     ! XYL
     rxylho  =  RCONST(77)

     ! CSL
     rcslho  =  RCONST(78)
     rcslno3 =  RCONST(100)

     ! HC5
     rhc5ho  =  RCONST(67)

     ! HC8
     rhc8ho  =  RCONST(68)

     ! ISO
     risoho  =  RCONST(73)
     risoo3  =  RCONST(113)
     risono3 =  RCONST(105)

     ! API
     rapiho  =  RCONST(74)
     rapio3  =  RCONST(114)
     rapino3 =  RCONST(106)

     ! LIM
     rlimho  =  RCONST(75)
     rlimo3  =  RCONST(115)
     rlimno3 =  RCONST(107)

     ! SESQ
     rsesqho  =  RCONST(249)
     rsesqo3  =  RCONST(250)
     rsesqno3 =  RCONST(251)

     ! Isoprene radical
     risopno  =  RCONST(142)
     risopho2 =  RCONST(161)
     risopmo2 =  RCONST(182)
     risopaco3=  RCONST(202)
     risopisop=  RCONST(241)
!liqy-20140926

     ! production from anthropogenic VOCs
     PRDROG(PALK4)= rhc5ho*var(ind_hc5)*var(ind_ho)
     PRDROG(PALK5)= rhc8ho*var(ind_hc8)*var(ind_ho)

     PRDROG(POLE1)= roltho*var(ind_olt)*var(ind_ho) + rolto3*var(ind_olt)*var(ind_o3) + roltno3*var(ind_olt)*var(ind_no3)
     PRDROG(POLE2)= roliho*var(ind_oli)*var(ind_ho) + rolio3*var(ind_oli)*var(ind_o3) + rolino3*var(ind_oli)*var(ind_no3)

     PRDROG(PARO1)= rtolho*var(ind_tol)*var(ind_ho)

     PRDROG(PARO2)= rxylho*var(ind_xyl)*var(ind_ho)
     PRDROG(PARO2)= PRDROG(PARO2) + rcslho*var(ind_csl)*var(ind_ho) + rcslno3*var(ind_csl)*var(ind_no3)
 
     ! Biogenic
     PRDROG(PISOP)= risoho*var(ind_iso)*var(ind_ho) + risoo3*var(ind_iso)*var(ind_o3) + risono3*var(ind_iso)*var(ind_no3)

     PRDROG(PTERP)= rapiho*var(ind_api)*var(ind_ho) + rapio3*var(ind_api)*var(ind_o3) + rapino3*var(ind_api)*var(ind_no3)
     PRDROG(PTERP)= PRDROG(PTERP) + rlimho*var(ind_lim)*var(ind_ho) + rlimo3*var(ind_lim)*var(ind_o3) + rlimno3*var(ind_lim)*var(ind_no3)

     PRDROG(PSESQ)= rsesqho*var(ind_sesq)*var(ind_ho) + rsesqo3*var(ind_sesq)*var(ind_o3) + rsesqno3*var(ind_sesq)*var(ind_no3)

! RAR: to calculate the branching ratios to determine high NOx versus low NOx

     PRDROG(PBRCH)= risopno*var(ind_no)

     ! VDROG carrying the branching ratios
     if (PRDROG(PBRCH)>1.E-12) then
        ro2loss= PRDROG(PBRCH) + risopho2*var(ind_ho2) + risopmo2*var(ind_mo2) + risopaco3*var(ind_aco3) +  &
                               risopisop*var(ind_isop)
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
