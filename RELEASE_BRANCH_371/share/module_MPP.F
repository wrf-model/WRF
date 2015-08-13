!
      MODULE MODULE_MPP
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!***  THE RANK OF THIS TASK
!
      INTEGER :: MYPE
!----------------------------------------------------------------------
!
!***  NUMBER OF TASKS
!
      INTEGER :: INPES,JNPES,NPES
!
!***  FUNDAMENTAL GLOBAL AND LOCAL ARRAY EXTENTS ON EACH TASK
!
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB               &
                ,MY_IS_LOC,MY_IE_LOC,MY_JS_LOC,MY_JE_LOC
!----------------------------------------------------------------------
!
!***  SUB-DOMAIN LOOP LIMITS THAT PENETRATE HALOES
!
      INTEGER :: MYIS,MYIE,MYJS,MYJE                                   &
                ,MYIS1,MYIS2,MYIS3,MYIS4,MYIS5                         &
                ,MYIE1,MYIE2,MYIE3,MYIE4,MYIE5                         &
                ,MYIS_P1,MYIS_P2,MYIS_P3,MYIS_P4,MYIS_P5               &
                ,MYIS1_P1,MYIS1_P2,MYIS1_P3,MYIS1_P4,MYIS1_P5          &
                ,MYIS2_P1,MYIS2_P2,MYIS2_P3,MYIS2_P4,MYIS2_P5          &
                ,MYIS3_P1,MYIS3_P2,MYIS3_P3,MYIS3_P4,MYIS3_P5          &
                ,MYIS4_P1,MYIS4_P2,MYIS4_P3,MYIS4_P4,MYIS4_P5          &
                ,MYIS5_P1,MYIS5_P2,MYIS5_P3,MYIS5_P4,MYIS5_P5          &
                ,MYIE_P1,MYIE_P2,MYIE_P3,MYIE_P4,MYIE_P5               &
                ,MYIE1_P1,MYIE1_P2,MYIE1_P3,MYIE1_P4,MYIE1_P5          &
                ,MYIE2_P1,MYIE2_P2,MYIE2_P3,MYIE2_P4,MYIE2_P5          &
                ,MYIE3_P1,MYIE3_P2,MYIE3_P3,MYIE3_P4,MYIE3_P5          &
                ,MYIE4_P1,MYIE4_P2,MYIE4_P3,MYIE4_P4,MYIE4_P5          &
                ,MYIE5_P1,MYIE5_P2,MYIE5_P3,MYIE5_P4,MYIE5_P5          &
                ,MYJS1,MYJS2,MYJS3,MYJS4,MYJS5                         &
                ,MYJE1,MYJE2,MYJE3,MYJE4,MYJE5                         &
                ,MYJS_P1,MYJS_P2,MYJS_P3,MYJS_P4,MYJS_P5               &
                ,MYJS1_P1,MYJS1_P2,MYJS1_P3,MYJS1_P4,MYJS1_P5          &
                ,MYJS2_P1,MYJS2_P2,MYJS2_P3,MYJS2_P4,MYJS2_P5          &
                ,MYJS3_P1,MYJS3_P2,MYJS3_P3,MYJS3_P4,MYJS3_P5          &
                ,MYJS4_P1,MYJS4_P2,MYJS4_P3,MYJS4_P4,MYJS4_P5          &
                ,MYJS5_P1,MYJS5_P2,MYJS5_P3,MYJS5_P4,MYJS5_P5          &
                ,MYJE_P1,MYJE_P2,MYJE_P3,MYJE_P4,MYJE_P5               &
                ,MYJE1_P1,MYJE1_P2,MYJE1_P3,MYJE1_P4,MYJE1_P5          &
                ,MYJE2_P1,MYJE2_P2,MYJE2_P3,MYJE2_P4,MYJE2_P5          &
                ,MYJE3_P1,MYJE3_P2,MYJE3_P3,MYJE3_P4,MYJE3_P5          &
                ,MYJE4_P1,MYJE4_P2,MYJE4_P3,MYJE4_P4,MYJE4_P5          &
                ,MYJE5_P1,MYJE5_P2,MYJE5_P3,MYJE5_P4,MYJE5_P5

!
!----------------------------------------------------------------------
!
!***  MPI_COMM_COMP IS THE INTRACOMMUNICATOR FOR ALL TASKS.
!
      INTEGER :: MPI_COMM_COMP

!----------------------------------------------------------------------
      END MODULE MODULE_MPP
