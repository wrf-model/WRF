#!/bin/bash
cd /home/lsurl/WCV/WRF/chem/KPP/kpp/kpp-2.1/src/
flex scan.l
sed -i '
1 i \
#define INITIAL 0 \
#define CMD_STATE 1 \
#define INC_STATE 2 \
#define MOD_STATE 3 \
#define INT_STATE 4 \
#define PRM_STATE 5 \
#define DSP_STATE 6 \
#define SSP_STATE 7 \
#define INI_STATE 8 \
#define EQN_STATE 9 \
#define EQNTAG_STATE 10 \
#define RATE_STATE 11 \
#define LMP_STATE 12 \
#define CR_IGNORE 13 \
#define SC_IGNORE 14 \
#define ATM_STATE 15 \
#define LKT_STATE 16 \
#define INL_STATE 17 \
#define MNI_STATE 18 \
#define TPT_STATE 19 \
#define USE_STATE 20 \
#define COMMENT 21 \
#define COMMENT2 22 \
#define EQN_ID 23 \
#define INL_CODE 24 
' /home/lsurl/WCV/WRF/chem/KPP/kpp/kpp-2.1/src/lex.yy.c
cd /home/lsurl/WCV/WRF/
