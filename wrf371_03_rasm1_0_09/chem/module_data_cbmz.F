!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! CBMZ module: see module_cbmz.F for references and terms of use
!**********************************************************************************  
!   file module_data_cbmf.f
!-----------------------------------------------------------------------------

      module module_data_cbmz


      integer nfixed_kppmax
      parameter (nfixed_kppmax =   20)

      integer nreact_kppmax
      parameter (nreact_kppmax =  256)

      real avognumkpp
      parameter (avognumkpp = 6.02252e23)	! molecules/mole


      integer nvar_r01_kpp, nfix_r01_kpp, nreact_r01_kpp,   &
              lu_nonzero_v_r01_kpp
      parameter ( nvar_r01_kpp = 28 )
      parameter ( nfix_r01_kpp = 5 )
      parameter ( nreact_r01_kpp = 74 )
      parameter ( lu_nonzero_v_r01_kpp = 194 )

      integer nvar_r02_kpp, nfix_r02_kpp, nreact_r02_kpp,   &
              lu_nonzero_v_r02_kpp
      parameter ( nvar_r02_kpp = 48 )
      parameter ( nfix_r02_kpp = 5 )
      parameter ( nreact_r02_kpp = 118 )
      parameter ( lu_nonzero_v_r02_kpp = 459 )

      integer nvar_r03_kpp, nfix_r03_kpp, nreact_r03_kpp,   &
              lu_nonzero_v_r03_kpp
      parameter ( nvar_r03_kpp = 53 )
      parameter ( nfix_r03_kpp = 5 )
      parameter ( nreact_r03_kpp = 134 )
      parameter ( lu_nonzero_v_r03_kpp = 564 )

      integer nvar_r04_kpp, nfix_r04_kpp, nreact_r04_kpp,   &
              lu_nonzero_v_r04_kpp
      parameter ( nvar_r04_kpp = 39 )
      parameter ( nfix_r04_kpp = 5 )
      parameter ( nreact_r04_kpp = 106 )
      parameter ( lu_nonzero_v_r04_kpp = 334 )

      integer nvar_r05_kpp, nfix_r05_kpp, nreact_r05_kpp,   &
              lu_nonzero_v_r05_kpp
      parameter ( nvar_r05_kpp = 59 )
      parameter ( nfix_r05_kpp = 5 )
      parameter ( nreact_r05_kpp = 150 )
      parameter ( lu_nonzero_v_r05_kpp = 606 )

      integer nvar_r06_kpp, nfix_r06_kpp, nreact_r06_kpp,   &
              lu_nonzero_v_r06_kpp
      parameter ( nvar_r06_kpp = 64 )
      parameter ( nfix_r06_kpp = 5 )
      parameter ( nreact_r06_kpp = 166 )
      parameter ( lu_nonzero_v_r06_kpp = 715 )


      integer                                                   &
        ino_z,              ino2_z,             ino3_z,         &
        in2o5_z,            ihono_z,            ihno3_z,        &
        ihno4_z,            io3_z,              io1d_z,         &
        io3p_z,             ioh_z,              iho2_z,         &
        ih2o2_z,            ico_z,              iso2_z,         &
        ih2so4_z,           inh3_z,             ihcl_z,         &
        ich4_z,             ic2h6_z,            ich3o2_z,       &
        iethp_z,            ihcho_z,            ich3oh_z,       &
        ic2h5oh_z,          ich3ooh_z,          iethooh_z,      &
        iald2_z,            ihcooh_z,           ipar_z,         &
        iaone_z,            imgly_z,            ieth_z,         &
        iolet_z,            iolei_z,            itol_z,         &
        ixyl_z,             icres_z,            ito2_z,         &
        icro_z,             iopen_z,            ionit_z,        &
        ipan_z,             ircooh_z,           irooh_z,        &
        ic2o3_z,            iro2_z,             iano2_z,        &
        inap_z,             ixo2_z,             ixpar_z,        &
        iisop_z,            iisoprd_z,          iisopp_z,       &
        iisopn_z,           iisopo2_z,          idms_z,         &
        imsa_z,             idmso_z,            idmso2_z,       &
        ich3so2h_z,         ich3sch2oo_z,       ich3so2_z,      &
        ich3so3_z,          ich3so2oo_z,        ich3so2ch2oo_z, &
        imtf_z,                                                 &
        ih2o_z,             io2_z,              in2_z,          &
        ih2_z

      parameter (                                                  &
        ino_z=01,           ino2_z=02,          ino3_z=03,         &
        in2o5_z=04,         ihono_z=05,         ihno3_z=06,        &
        ihno4_z=07,         io3_z=08,           io1d_z=09,         &
        io3p_z=10,          ioh_z=11,           iho2_z=12,         &
        ih2o2_z=13,         ico_z=14,           iso2_z=15,         &
        ih2so4_z=16,        inh3_z=17,          ihcl_z=18,         &
        ich4_z=19,          ic2h6_z=20,         ich3o2_z=21,       &
        iethp_z=22,         ihcho_z=23,         ich3oh_z=24,       &
        ic2h5oh_z=25,       ich3ooh_z=26,       iethooh_z=27,      &
        iald2_z=28,         ihcooh_z=29,        ipar_z=30,         &
        iaone_z=31,         imgly_z=32,         ieth_z=33,         &
        iolet_z=34,         iolei_z=35,         itol_z=36,         &
        ixyl_z=37,          icres_z=38,         ito2_z=39,         &
        icro_z=40,          iopen_z=41,         ionit_z=42,        &
        ipan_z=43,          ircooh_z=44,        irooh_z=45,        &
        ic2o3_z=46,         iro2_z=47,          iano2_z=48,        &
        inap_z=49,          ixo2_z=50,          ixpar_z=51,        &
        iisop_z=52,         iisoprd_z=53,       iisopp_z=54,       &
        iisopn_z=55,        iisopo2_z=56,       idms_z=57,         &
        imsa_z=58,          idmso_z=59,         idmso2_z=60,       &
        ich3so2h_z=61,      ich3sch2oo_z=62,    ich3so2_z=63,      &
        ich3so3_z=64,       ich3so2oo_z=65,     ich3so2ch2oo_z=66, &
        imtf_z=67,                                                 &
        ih2o_z=68,          io2_z=69,           in2_z=70,          &
        ih2_z=71 )


      integer ngas_z
      parameter (ngas_z=71)

      character(len=12), save :: name_z(ngas_z) = (/               &
        'no          ',     'no2         ',     'no3         ',    &
        'n2o5        ',     'hono        ',     'hno3        ',    &
        'hno4        ',     'o3          ',     'o1d         ',    &
        'o3p         ',     'oh          ',     'ho2         ',    &
        'h2o2        ',     'co          ',     'so2         ',    &
        'h2so4       ',     'nh3         ',     'hcl         ',    &
        'ch4         ',     'c2h6        ',     'ch3o2       ',    &
        'ethp        ',     'hcho        ',     'ch3oh       ',    &
        'c2h5oh      ',     'ch3ooh      ',     'ethooh      ',    &
        'ald2        ',     'hcooh       ',     'par         ',    &
        'aone        ',     'mgly        ',     'eth         ',    &
        'olet        ',     'olei        ',     'tol         ',    &
        'xyl         ',     'cres        ',     'to2         ',    &
        'cro         ',     'open        ',     'onit        ',    &
        'pan         ',     'rcooh       ',     'rooh        ',    &
        'c2o3        ',     'ro2         ',     'ano2        ',    &
        'nap         ',     'xo2         ',     'xpar        ',    &
        'isop        ',     'isoprd      ',     'isopp       ',    &
        'isopn       ',     'isopo2      ',     'dms         ',    &
        'msa         ',     'dmso        ',     'dmso2       ',    &
        'ch3so2h     ',     'ch3sch2oo   ',     'ch3so2      ',    &
        'ch3so3      ',     'ch3so2oo    ',     'ch3so2ch2oo ',    &
        'mtf         ',                                            &
        'h2o         ',     'o2          ',     'n2          ',    &
        'h2          ' /)


! photolyzing species reactions
      integer   &
       jphoto_no2,        jphoto_no3,       jphoto_hono,   &
       jphoto_hno3,       jphoto_hno4,      jphoto_n2o5,   &
       jphoto_o3a,        jphoto_o3b,       jphoto_h2o2,   &
       jphoto_hchoa,      jphoto_hchob,     jphoto_ch3ooh, &
       jphoto_ethooh,     jphoto_ald2,      jphoto_aone,   &
       jphoto_mgly,       jphoto_open,      jphoto_rooh,   &
       jphoto_onit,       jphoto_isoprd
      parameter (   &
       jphoto_no2=1,      jphoto_no3=2,     jphoto_hono=3,    &
       jphoto_hno3=4,     jphoto_hno4=5,    jphoto_n2o5=6,    &
       jphoto_o3a=7,      jphoto_o3b=8,     jphoto_h2o2=9,    &
       jphoto_hchoa=10,   jphoto_hchob=11,  jphoto_ch3ooh=12, &
       jphoto_ethooh=13,  jphoto_ald2=14,   jphoto_aone=15,   &
       jphoto_mgly=16,    jphoto_open=17,   jphoto_rooh=18,   &
       jphoto_onit=19,    jphoto_isoprd=20 )


!
! parameter statements from cbmz_local.h
!

      integer ngas_m1, nrxn_m1, ngas_m2, nrxn_m2,   &
              ngas_m3, nrxn_m3, ngas_m4, nrxn_m4
      parameter(ngas_m1 = 31, nrxn_m1 = 74, 	& ! background troposphere
                ngas_m2 = 19, nrxn_m2 = 53,	& ! urban
                ngas_m3 =  5, nrxn_m3 = 16,	& ! rural continental
                ngas_m4 = 11, nrxn_m4 = 32)	  ! remote marine

      integer ngas_tot
      parameter(ngas_tot= ngas_m1 + ngas_m2 + ngas_m3 + ngas_m4)

      integer ngas_r1, ngas_r2, ngas_r3, ngas_r4, ngas_r5, ngas_r6
      parameter(ngas_r1 = ngas_m1)				! regime 1
      parameter(ngas_r2 = ngas_m1 + ngas_m2)			! regime 2
      parameter(ngas_r3 = ngas_m1 + ngas_m2 + ngas_m3)		! regime 3
      parameter(ngas_r4 = ngas_m1 + ngas_m4)			! regime 4
      parameter(ngas_r5 = ngas_m1 + ngas_m2 + ngas_m4)		! regime 5
      parameter(ngas_r6 = ngas_m1 + ngas_m2 + ngas_m3 + ngas_m4)! regime 6

      integer nperox
      parameter(nperox=10)		! total number of alkylperoxy radicals

      integer nphoto
      parameter(nphoto=20)		! total number of photolysis rxn rates


! permutation reactions of alkyl/acyl peroxy radicals
      integer   &
          jch3o2,      jethp,       jro2,        jc2o3,       jano2,   &
          jnap,        jisopp,      jisopn,      jisopo2,     jxo2
      parameter (   &
          jch3o2=1,    jethp=2,     jro2=3,      jc2o3=4,     jano2=5,   &
          jnap=6,      jisopp=7,    jisopn=8,    jisopo2=9,   jxo2=10 )


      end module module_data_cbmz
