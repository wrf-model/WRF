MODULE module_data_megan2

  !
  ! This module contains parameters for biogenic emissions model
  ! MEGAN v2.04
  !
  !  Reference: 
  !
  !    Estimates of global terrestial isoprene emissions using MEGAN
  !    (Model of Emissions of Gases and Aerosols from Nature )
  !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, P.I. Palmer, and C. Geron
  !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006
  !
  !    MEGAN v2.0 Documentation
  !
  ! August, 2007
  !
  ! Serena H. Chung          Washington State University
  ! Tan Sakulyanontvittaya   University of Colorado
  ! Christine Wiedinmyer     National Center for Atmospheric Research
  !

  IMPLICIT NONE

  !=======================================================================
  ! The following contains 20 clases of species for MEGAN v2.04
  !  Originally from Tan's SPC_MGN.EXT (12/02/2006)
  !=======================================================================

  INTEGER, PARAMETER :: n_mgn_spc  = 20
  INTEGER, PARAMETER :: n_spc_char = 16
  CHARACTER (len=n_spc_char), DIMENSION (n_mgn_spc) :: mgn_spc
  
  ! Indices for 20 Classes of MEGAN v2.04 Species
  !    The ordering here follows that of Table 2 of MEGAN v2.0
  !    Documentation.  For programming, the ordering does not matter, 
  !    except that imgn_isop should always be 1.
  INTEGER, PARAMETER ::     &
       &  imgn_isop  =  1   & ! isoprene
       & ,imgn_mbo   =  2   & ! MBO (only (2-methyl-3-ene-2-ol)-methylbutenol) 
       & ,imgn_myrc  =  3   & ! myrcene
       & ,imgn_sabi  =  4   & ! sabinene
       & ,imgn_limo  =  5   & ! limonene
       & ,imgn_3car  =  6   & ! 3 carene
       & ,imgn_ocim  =  7   & ! trans-beta-ocimene
       & ,imgn_bpin  =  8   & ! beta-pinene
       & ,imgn_apin  =  9   & ! alpha-pinene
       & ,imgn_afarn = 10   & ! alpha-farnescene
       & ,imgn_bcar  = 11   & ! beta-caryophyllene
       & ,imgn_meoh  = 12   & ! methanol
       & ,imgn_acto  = 13   & ! acetone
       & ,imgn_acta  = 14   & ! acetaldehyde and ethanol
       & ,imgn_form  = 15   & ! formic acid, formaldehyde, acetic acid
       & ,imgn_ch4   = 16   & ! methane
       & ,imgn_no    = 17   & ! nitric oxide (and NO2 and NH3 )
       & ,imgn_omtp  = 18   & ! other monoterpenes
       & ,imgn_osqt  = 19   & ! other sesquiterpenes
       & ,imgn_co    = 20     ! CO and other VOC                                          

  !
  ! For reference, below is how Tan's ordereing the 20 classes of species
  ! in his SPC_MGN.EXT.  They are different from the ordering above.
  ! The following is provide for convenience when copying/pasting
  ! Tan's code to here.
  !
  ! Tan's Order        Species Class
  !   1                isop
  !   2                myrc
  !   3                sabi
  !   4                limo
  !   5                3car
  !   6                ocim
  !   7                bpin
  !   8                apin
  !   9                omtp
  !  10                afarn
  !  11                bcar
  !  12                osqt
  !  13                mbo
  !  14                meoh
  !  15                acto
  !  16                ch4
  !  17                no
  !  18                acta
  !  19                form
  !  20                co        


  ! Names of the 20 calses of Species
  ! (These are not used, except for testing stage)
  DATA  mgn_spc( imgn_isop  )   / 'isoprene        ' /  ! isoprene                                              
  DATA  mgn_spc( imgn_mbo   )   / 'MBO             ' /  ! MBO (only (2-methyl-3-ene-2-ol)-methylbutenol)               
  DATA  mgn_spc( imgn_myrc  )   / 'myrcene         ' /  ! myrcene                                               
  DATA  mgn_spc( imgn_sabi  )   / 'sabinene        ' /  ! sabinene                                              
  DATA  mgn_spc( imgn_limo  )   / 'limonene        ' /  ! limonene                                              
  DATA  mgn_spc( imgn_3car  )   / '3carene         ' /  ! 3 carene                                              
  DATA  mgn_spc( imgn_ocim  )   / 'ocimene         ' /  ! trans-beta-ocimene                                    
  DATA  mgn_spc( imgn_bpin  )   / 'b-pinene        ' /  ! beta-pinene                                           
  DATA  mgn_spc( imgn_apin  )   / 'a-pininen       ' /  ! alpha-pinene                                          
  DATA  mgn_spc( imgn_afarn )   / 'a-farnescene    ' /  ! alpha-farnescene                                      
  DATA  mgn_spc( imgn_bcar  )   / 'b-caryophyllene ' /  ! beta-caryophyllene                                    
  DATA  mgn_spc( imgn_meoh  )   / 'methanol        ' /  ! methanol                                              
  DATA  mgn_spc( imgn_acto  )   / 'actone          ' /  ! acetone                                               
  DATA  mgn_spc( imgn_acta  )   / 'acta            ' /  ! acetaldehyde and ethanol
  DATA  mgn_spc( imgn_form  )   / 'form            ' /  ! formic acid, formaldehyde, acetic acid  
  DATA  mgn_spc( imgn_ch4   )   / 'methane         ' /  ! methane                                               
  DATA  mgn_spc( imgn_no    )   / 'NO              ' /  ! nitric oxide  (and NO2 and NH3 )
  DATA  mgn_spc( imgn_omtp  )   / 'other_monoterp  ' /  ! other monoterpenes                                    
  DATA  mgn_spc( imgn_osqt  )   / 'other_sesquiterp' /  ! other sesquiterpenes                                  
  DATA  mgn_spc( imgn_co    )   / 'CO              ' /  ! co and other voc                                      

  !=====================================================================
  !  The following contains "temperature dependent" parameter for
  !  non-isoprene species.  This is the parameter "beta" provided in
  !  Table 2 of MEGAN v2.0 Documentaion.
  !  Originally from Tan's TEMPD_PRM.EXT (11/30/2006)
  !  Updated on 08/13/2007 based on Alex Guenther's email.
  !=====================================================================

  REAL , DIMENSION(n_mgn_spc) :: tdf_prm

  DATA  tdf_prm ( imgn_isop  )  / 0.09  / ! isoprene  (but not used for isoprene)
  DATA  tdf_prm ( imgn_mbo   )  / 0.09  / ! MBO (only (2-methyl-3-ene-2-ol)-methylbutenol)
  DATA  tdf_prm ( imgn_myrc  )  / 0.10  / ! myrcene                                
  DATA  tdf_prm ( imgn_sabi  )  / 0.10  / ! sabinene                               
  DATA  tdf_prm ( imgn_limo  )  / 0.10  / ! limonene                               
  DATA  tdf_prm ( imgn_3car  )  / 0.10  / ! 3 carene                               
  DATA  tdf_prm ( imgn_ocim  )  / 0.10  / ! trans-beta-ocimene                     
  DATA  tdf_prm ( imgn_bpin  )  / 0.10  / ! beta-pinene                            
  DATA  tdf_prm ( imgn_apin  )  / 0.10  / ! alpha-pinene                           
  DATA  tdf_prm ( imgn_afarn )  / 0.17  / ! alpha-farnescene                       
  DATA  tdf_prm ( imgn_bcar  )  / 0.17  / ! beta-caryophyllene                     
  DATA  tdf_prm ( imgn_meoh  )  / 0.08  / ! methanol                               
  DATA  tdf_prm ( imgn_acto  )  / 0.11  / ! acetone                                
  DATA  tdf_prm ( imgn_acta  )  / 0.13  / ! acetaldehyde and ethanol               
  DATA  tdf_prm ( imgn_form  )  / 0.09  / ! formic acid, formaldehyde, acetic acid
  DATA  tdf_prm ( imgn_ch4   )  / 0.05  / ! methane                                
  DATA  tdf_prm ( imgn_no    )  / 0.11  / ! nitric oxide (and NO2 and NH3)                          
  DATA  tdf_prm ( imgn_omtp  )  / 0.10  / ! other monoterpenes                     
  DATA  tdf_prm ( imgn_osqt  )  / 0.17  / ! other sesquiterpenes                   
  DATA  tdf_prm ( imgn_co    )  / 0.09  / ! co and other voc                       

  !=====================================================================
  !  The following contains the fraction of total emisssions
  !  for which light-dependency is applied (via GAMMA_P).  This is the
  !  parameter "LDF" provided in Table 2 of MEGAN v2.0 Documentaion.
  !  Originally from Tan's LD_FCT.EXT (11/30/2006)
  !  Updated 08/13/2007 based on Alex Guenther's email.
  !=====================================================================

  REAL , DIMENSION(n_mgn_spc) :: ldf_fct

  DATA  ldf_fct ( imgn_isop  )  / 0.9999 /  ! isoprene                               
  DATA  ldf_fct ( imgn_mbo   )  / 0.9999 /  ! MBO (only (2-methyl-3-ene-2-ol)-methylbutenol)
  DATA  ldf_fct ( imgn_myrc  )  / 0.05   /  ! myrcene                                
  DATA  ldf_fct ( imgn_sabi  )  / 0.10   /  ! sabinene                               
  DATA  ldf_fct ( imgn_limo  )  / 0.05   /  ! limonene                               
  DATA  ldf_fct ( imgn_3car  )  / 0.05   /  ! 3 carene                               
  DATA  ldf_fct ( imgn_ocim  )  / 0.80   /  ! trans-beta-ocimene                     
  DATA  ldf_fct ( imgn_bpin  )  / 0.10   /  ! beta-pinene                            
  DATA  ldf_fct ( imgn_apin  )  / 0.10   /  ! alpha-pinene                           
  DATA  ldf_fct ( imgn_afarn )  / 0.50   /  ! alpha-farnescene                       
  DATA  ldf_fct ( imgn_bcar  )  / 0.50   /  ! beta-caryophyllene                     
  DATA  ldf_fct ( imgn_meoh  )  / 0.75   /  ! methanol                         
  DATA  ldf_fct ( imgn_acto  )  / 0.25   /  ! acetone                                
  DATA  ldf_fct ( imgn_acta  )  / 0.50   /  ! acetaldehyde and ethanol ??          
  DATA  ldf_fct ( imgn_form  )  / 0.50   /  ! formic acid, formaldehyde, acetic acid ??
  DATA  ldf_fct ( imgn_ch4   )  / 0.75   /  ! methane                                
  DATA  ldf_fct ( imgn_no    )  / 0.00   /  ! nitric oxide (and NO2 and NH3 )                          
  DATA  ldf_fct ( imgn_omtp  )  / 0.10   /  ! other monoterpenes                     
  DATA  ldf_fct ( imgn_osqt  )  / 0.50   /  ! other sesquiterpenes                   
  DATA  ldf_fct ( imgn_co    )  / 0.50   /  ! CO and other VOC                       

  !=====================================================================
  !  The following contains "production and loss within canopy" factors.
  !  See Eqn (1)  and Section 3.3 of Guenther et al. [2006].
  !  Originally from Tan's PDT_LOS_CP.EXT (11/30/2006)
  !=====================================================================

  REAL , DIMENSION(n_mgn_spc) :: rho_fct
  
  ! RHO_FCT are all 1.0 right now
  DATA  rho_fct ( imgn_isop  )  / 1.0  / ! isoprene                               
  DATA  rho_fct ( imgn_mbo   )  / 1.0  / ! MBO (only (2-methyl-3-ene-2-ol)-methylbutenol)
  DATA  rho_fct ( imgn_myrc  )  / 1.0  / ! myrcene                                
  DATA  rho_fct ( imgn_sabi  )  / 1.0  / ! sabinene                               
  DATA  rho_fct ( imgn_limo  )  / 1.0  / ! limonene                               
  DATA  rho_fct ( imgn_3car  )  / 1.0  / ! 3 carene                               
  DATA  rho_fct ( imgn_ocim  )  / 1.0  / ! trans-beta-ocimene                     
  DATA  rho_fct ( imgn_bpin  )  / 1.0  / ! beta-pinene                            
  DATA  rho_fct ( imgn_apin  )  / 1.0  / ! alpha-pinene                           
  DATA  rho_fct ( imgn_afarn )  / 1.0  / ! alpha-farnescene                       
  DATA  rho_fct ( imgn_bcar  )  / 1.0  / ! beta-caryophyllene                     
  DATA  rho_fct ( imgn_meoh  )  / 1.0  / ! methanol                               
  DATA  rho_fct ( imgn_acto  )  / 1.0  / ! acetone                                
  DATA  rho_fct ( imgn_acta  )  / 1.0  / ! acetaldehyde and ethanol               
  DATA  rho_fct ( imgn_form  )  / 1.0  / ! formic acid, formaldehyde, acetic acid 
  DATA  rho_fct ( imgn_ch4   )  / 1.0  / ! methane                                
  DATA  rho_fct ( imgn_no    )  / 1.0  / ! nitric oxide (and NO2 and NH3 )                           
  DATA  rho_fct ( imgn_omtp  )  / 1.0  / ! other monoterpenes                     
  DATA  rho_fct ( imgn_osqt  )  / 1.0  / ! other sesquiterpenes                   
  DATA  rho_fct ( imgn_co    )  / 1.0  / ! CO and other VOC                       


  !=====================================================================
  !  The following contains relative emission rates assigned to each
  !  canopy fraction.  See Eqn (16) of Guenther et al. [2006] and
  !  MEGAN v2.0 Documentation.
  !  Originally Tan's REL_EM_ACT.EXT (11/30/06)
  !=====================================================================

  INTEGER, PARAMETER ::        n_cat=5
  REAL, DIMENSION(n_cat) ::    anew, agro, amat, aold

  DATA  anew (1) , agro (1) , amat (1) , aold (1)  /  1.0  , 1.0 , 1.0   , 1.0  /
  DATA  anew (2) , agro (2) , amat (2) , aold (2)  /  2.0  , 1.8 , 0.95  , 1.0  /
  DATA  anew (3) , agro (3) , amat (3) , aold (3)  /  0.4  , 0.6 , 1.075 , 1.0  /
  DATA  anew (4) , agro (4) , amat (4) , aold (4)  /  3.0  , 2.6 , 0.85  , 1.0  /
  DATA  anew (5) , agro (5) , amat (5) , aold (5)  /  0.05 , 0.6 , 1.125 , 1.0  /

  !=====================================================================
  !  The following contains the base emission factors (microgram m-2 hr-1) for
  !  n_mgn_spc= 20 MEGAN v2.04 classes of compounds for the following
  !  four plant functional types:
  !     BT = breadleaf tree
  !     NT = needle leaf tree
  !     SB = shrub and brush
  !     HB = herb/crops/grasses
  !  Originally Tan's EF_MGN20_Alex.EXT
  !  Updated 08/13/2007 based on Alex's email.
  !=====================================================================
!shc  real, dimension (n_mgn_spc) :: EF_BT, EF_NT, EF_SB, EF_HB
  INTEGER, PARAMETER :: n_pft = 4 ! number of plant functional types
  REAL, DIMENSION (n_mgn_spc,n_pft) :: EF

  ! Plant functional type indices
  !  k_bt, k_nt, k_sb, and k_bh are hardwired by the column-order
  !  of the data entry for EF and EF_frac.  Do not change their values or
  !  order of the columns in the data entry.

  INTEGER, PARAMETER :: k_bt = 1, k_nt = 2, k_sb=3, k_hb=4
  
  !                                   BT        NT        SB          HB
  DATA EF( imgn_isop  , 1:n_pft )  /  13000.00 , 2000.00 , 11000.00 ,  400.00  / !not used for isoprene
  DATA EF( imgn_mbo   , 1:n_pft )  /      0.10 ,  100.00 ,     1.00 ,    0.01  /
  DATA EF( imgn_myrc  , 1:n_pft )  /     20.00 ,   75.00 ,    22.00 ,    0.30  /
  DATA EF( imgn_sabi  , 1:n_pft )  /     45.00 ,   70.00 ,    50.00 ,    0.70  /
  DATA EF( imgn_limo  , 1:n_pft )  /     45.00 ,  100.00 ,    52.00 ,    0.70  /
  DATA EF( imgn_3car  , 1:n_pft )  /     18.00 ,  160.00 ,    25.00 ,    0.30  /
  DATA EF( imgn_ocim  , 1:n_pft )  /     90.00 ,   60.00 ,    85.00 ,    1.00  /
  DATA EF( imgn_bpin  , 1:n_pft )  /     90.00 ,  300.00 ,   100.00 ,    1.50  /
  DATA EF( imgn_apin  , 1:n_pft )  /    180.00 ,  450.00 ,   200.00 ,    2.00  /
  DATA EF( imgn_afarn , 1:n_pft )  /     35.00 ,   30.00 ,    30.00 ,    0.50  /
  DATA EF( imgn_bcar  , 1:n_pft )  /     30.00 ,   60.00 ,    45.00 ,    0.90  /
  DATA EF( imgn_meoh  , 1:n_pft )  /    800.00 ,  800.00 ,   800.00 ,  800.00  /
  DATA EF( imgn_acto  , 1:n_pft )  /    240.00 ,  240.00 ,   240.00 ,   80.00  /
  DATA EF( imgn_acta  , 1:n_pft )  /    240.00 ,  240.00 ,   240.00 ,   80.00  /
  DATA EF( imgn_form  , 1:n_pft )  /     70.00 ,   70.00 ,    70.00 ,   70.00  /
  DATA EF( imgn_ch4   , 1:n_pft )  /     30.00 ,   30.00 ,    30.00 ,   30.00  /
  DATA EF( imgn_no    , 1:n_pft )  /      5.00 ,    6.00 ,    30.00 ,   70.00  /
  DATA EF( imgn_omtp  , 1:n_pft )  /     90.00 ,  180.00 ,   110.00 ,    4.80  /
  DATA EF( imgn_osqt  , 1:n_pft )  /     75.00 ,  110.00 ,    85.00 ,    1.40  /
  DATA EF( imgn_co    , 1:n_pft )  /   1000.00 , 1000.00 ,  1000.00 , 1000.00  /

 
  !=======================================================================
  ! The following contains MEGAN 138 speciated species for MEGAN v2.04.
  ! See Table 3 of MEGAN v2.0 Documentation.
  ! Originally from Tan's SPC_SPCAT.EXT (12/02/2006)
  !=======================================================================


  INTEGER, PARAMETER :: n_spca_spc = 138                         ! number of speciated species
  CHARACTER(len=n_spc_char), DIMENSION(n_spca_spc) :: spca_spc   ! speciated species names
  REAL                     , DIMENSION(n_spca_spc) :: spca_mwt   ! speciated species molecular weight

  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  ! _a  = alpha     , _b  = beta      , _c  = cis       , _al = allo    ,
  ! _g  = gamma     , _d  = delta     , _t  = trans     , _m  = methyl  ,
  ! _p  = para      , _o  = ortho     , _e  = ene       , _ol = ol      ,
  ! met = methyl    , 2met= dimethyl  , MBO = methylbutenol             ,
  ! 2s  = disulfide , s   = sulfide   , OXD = oxide     , ACT = acetate ,
  ! PPPP= propenylpropyl              , DCTT= decatetraene              ,
  ! CCO = acetaldehyde
  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

  ! Indices for 138 MEGAN speciated species
  !    The ordering below follows that of Tan's SPC_SPCA.TXT file.
  !    For programming, the ordering does not matter, except that 
  !    is_isoprene should always be 1.
  INTEGER, PARAMETER ::              &
       &  is_isoprene         =   1  & ! isoprene
       & ,is_myrcene          =   2  & ! myrcene
       & ,is_sabinene         =   3  & ! sabiene
       & ,is_limonene         =   4  & ! limonene
       & ,is_carene_3         =   5  & ! 3-carene
       & ,is_ocimene_t_b      =   6  & ! trans-beta-ocimene
       & ,is_pinene_b         =   7  & ! beta-pinene
       & ,is_pinene_a         =   8  & ! alpha-pinene
       & ,is_2met_styrene     =   9  & ! dimethyl styrene
       & ,is_cymene_p         =  10  & ! para-cymene
       & ,is_cymene_o         =  11  & ! ortho-cymene
       & ,is_phellandrene_a   =  12  & ! alpha-phellandrene
       & ,is_thujene_a        =  13  & ! alpha-thujene
       & ,is_terpinene_a      =  14  & ! alpha-terpinene
       & ,is_terpinene_g      =  15  & ! gamma-terpinene
       & ,is_terpinolene      =  16  & ! terpinolene
       & ,is_phellandrene_b   =  17  & ! beta-phellandrene
       & ,is_camphene         =  18  & ! camphene
       & ,is_bornene          =  19  & ! bornene
       & ,is_fenchene_a       =  20  & ! alpha-fenchene
       & ,is_ocimene_al       =  21  & ! allo ocimene
       & ,is_ocimene_c_b      =  22  & ! cis beta-ocimene
       & ,is_tricyclene       =  23  & ! tricyclene
       & ,is_estragole        =  24  & ! estrogole
       & ,is_camphor          =  25  & ! camphor
       & ,is_fenchone         =  26  & ! fenchone
       & ,is_piperitone       =  27  & ! piperitone
       & ,is_thujone_a        =  28  & ! alpha-thujone
       & ,is_thujone_b        =  29  & ! beta thujone
       & ,is_cineole_1_8      =  30  & ! (1,8)-cineole
       & ,is_borneol          =  31  & ! borneol
       & ,is_linalool         =  32  & ! linalool
       & ,is_terpineol_4      =  33  & ! 4-terpineol
       & ,is_terpineol_a      =  34  & ! alpha-terpineol
       & ,is_linalool_OXD_c   =  35  & ! cis linalool oxide
       & ,is_linalool_OXD_t   =  36  & ! trans linalool oxide
       & ,is_ionone_b         =  37  & ! beta-ionone
       & ,is_bornyl_ACT       =  38  & ! bornyl acetate
       & ,is_farnescene_a     =  39  & ! alpha-farnescene
       & ,is_caryophyllene_b  =  40  & ! beta-caryophyllene
       & ,is_acoradiene       =  41  & ! acoradiene
       & ,is_aromadendrene    =  42  & ! aromadendrene
       & ,is_bergamotene_a    =  43  & ! alpha-bergamotene
       & ,is_bergamotene_b    =  44  & ! beta-bergamotene
       & ,is_bisabolene_a     =  45  & ! alpha-bisabolene
       & ,is_bisabolene_b     =  46  & ! beta-bisabolene
       & ,is_bourbonene_b     =  47  & ! beta-bourbonene
       & ,is_cadinene_d       =  48  & ! delta-cadinene
       & ,is_cadinene_g       =  49  & ! gamma-cadinene
       & ,is_cedrene_a        =  50  & ! alpha-cedrene
       & ,is_copaene_a        =  51  & ! alpha-copaene
       & ,is_cubebene_a       =  52  & ! alpha-cubebene 
       & ,is_cubebene_b       =  53  & ! beta-cubebene
       & ,is_elemene_b        =  54  & ! beta-elemene
       & ,is_farnescene_b     =  55  & ! beta-farnescene
       & ,is_germacrene_B     =  56  & ! B germacrene
       & ,is_germacrene_D     =  57  & ! D germacrene
       & ,is_gurjunene_b      =  58  & ! beta-gurjunene
       & ,is_humulene_a       =  59  & ! alph-humulene
       & ,is_humulene_g       =  60  & ! gamma-humulene
       & ,is_isolongifolene   =  61  & ! isolongifolene
       & ,is_longifolene      =  62  & ! longifolene
       & ,is_longipinene      =  63  & ! longipinene
       & ,is_muurolene_a      =  64  & ! alpha-muurolene
       & ,is_muurolene_g      =  65  & ! gamma-muurolene
       & ,is_selinene_b       =  66  & ! beta-selinene
       & ,is_selinene_d       =  67  & ! delta-selinene
       & ,is_nerolidol_c      =  68  & ! cis-nerolidol
       & ,is_nerolidol_t      =  69  & ! trans-nerolidol
       & ,is_cedrol           =  70  & ! cedrol
       & ,is_MBO_2m3e2ol      =  71  & !  
       & ,is_methanol         =  72  & ! methanol
       & ,is_acetone          =  73  & ! acetone
       & ,is_methane          =  74  & ! methane
       & ,is_ammonia          =  75  & ! ammonia
       & ,is_nitrous_OXD      =  76  & ! nitrous oxide
       & ,is_nitric_OXD       =  77  & ! nitric oxide
       & ,is_acetaldehyde     =  78  & ! acetaldehyde
       & ,is_ethanol          =  79  & ! ethanol
       & ,is_formic_acid      =  80  & ! formic acid
       & ,is_formaldehyde     =  81  & ! formaldehye
       & ,is_acetic_acid      =  82  & ! acetic acid
       & ,is_MBO_3m2e1ol      =  83  & ! (3-methyl-2-ene-1-ol)-methybutenol 
       & ,is_MBO_3m3e1ol      =  84  & ! (3-methyl-3-ene-1-ol)-methybutenol 
       & ,is_benzaldehyde     =  85  & ! benzaldehye
       & ,is_butanone_2       =  86  & ! 2-butonene
       & ,is_decanal          =  87  & ! decanal
       & ,is_dodecene_1       =  88  & ! 1-dodecene
       & ,is_geranyl_acetone  =  89  & ! geranyl acetone
       & ,is_heptanal         =  90  & ! heptanal
       & ,is_heptane          =  91  & ! heptane
       & ,is_hexane           =  92  & ! hexane
       & ,is_met_benzoate     =  93  & ! methyl-benzoate
       & ,is_met_heptenone    =  94  & ! methyl-heptonene
       & ,is_neryl_acetone    =  95  & ! neryl-acetone
       & ,is_nonanal          =  96  & ! nonanal
       & ,is_nonenal          =  97  & ! nonenal
       & ,is_octanal          =  98  & ! octanal
       & ,is_octanol          =  99  & ! octanol
       & ,is_octenol_1e3ol    = 100  & ! (1-ene-3-ol)-octenol
       & ,is_oxopentanal      = 101  & ! oxopentanal
       & ,is_pentane          = 102  & ! pentane
       & ,is_phenyl_CCO       = 103  & ! phenylacetaldehyde
       & ,is_pyruvic_acid     = 104  & ! pyruvic acid
       & ,is_terpinyl_ACT_a   = 105  & ! alpha-terpinyl-acetate
       & ,is_tetradecene_1    = 106  & ! 1-tetradecene
       & ,is_toluene          = 107  & ! toluene
       & ,is_carbon_monoxide  = 108  & ! CO
       & ,is_butene           = 109  & ! butene
       & ,is_ethane           = 110  & ! ethane
       & ,is_ethene           = 111  & ! ethene
       & ,is_hydrogen_cyanide = 112  & ! hydrogen cyanide
       & ,is_propane          = 113  & ! propane
       & ,is_propene          = 114  & ! propene
       & ,is_carbon_2s        = 115  & ! carbon disulfide
       & ,is_carbonyl_s       = 116  & ! caronyl sulfide
       & ,is_diallyl_2s       = 117  & ! diallyl disulfide
       & ,is_2met_2s          = 118  & ! dimethyl disulfide
       & ,is_2met_s           = 119  & ! dimethyl sulfide
       & ,is_met_chloride     = 120  & ! methyl chloride
       & ,is_met_bromide      = 121  & ! methyl bromide
       & ,is_met_iodide       = 122  & ! methyl iodide
       & ,is_hydrogen_s       = 123  & ! hydrogen sulfide
       & ,is_met_mercaptan    = 124  & ! methyl mercaptan
       & ,is_met_propenyl_2s  = 125  & ! methyl propenyl disulfide
       & ,is_PPPP_2s          = 126  & ! propenylpropyldisulfdie
       & ,is_2met_nonatriene  = 127  & ! dimethyl nonatriene
       & ,is_met_salicylate   = 128  & ! methyl salicylate
       & ,is_indole           = 129  & ! indole
       & ,is_jasmone          = 130  & ! jasmone
       & ,is_met_jasmonate    = 131  & ! methyl jasmonate
       & ,is_3met_3DCTT       = 132  & ! trimethyl tridecatetraene
       & ,is_hexanal          = 133  & ! hexanal
       & ,is_hexanol_1        = 134  & ! 1-hexanol
       & ,is_hexenal_c3       = 135  & ! cis-3-hexenal
       & ,is_hexenal_t2       = 136  & ! trans-2-hexenal
       & ,is_hexenol_c3       = 137  & ! cis-3-hexenol
       & ,is_hexenyl_ACT_c3   = 138    ! cis-3-hexenyl acetate

  ! Names and Molecular Weights of 138 MEGAN speciated species  
  ! (The names are not used in WRF-Chem, except for testing stage)
  DATA spca_spc(is_isoprene        ), spca_mwt(is_isoprene        ) / 'isoprene        ' ,  68.12 /
  DATA spca_spc(is_myrcene         ), spca_mwt(is_myrcene         ) / 'myrcene         ' , 136.23 /
  DATA spca_spc(is_sabinene        ), spca_mwt(is_sabinene        ) / 'sabinene        ' , 136.23 /
  DATA spca_spc(is_limonene        ), spca_mwt(is_limonene        ) / 'limonene        ' , 136.23 /
  DATA spca_spc(is_carene_3        ), spca_mwt(is_carene_3        ) / 'carene_3        ' , 136.23 /
  DATA spca_spc(is_ocimene_t_b     ), spca_mwt(is_ocimene_t_b     ) / 'ocimene_t_b     ' , 136.23 /
  DATA spca_spc(is_pinene_b        ), spca_mwt(is_pinene_b        ) / 'pinene_b        ' , 136.23 /
  DATA spca_spc(is_pinene_a        ), spca_mwt(is_pinene_a        ) / 'pinene_a        ' , 136.23 /
  DATA spca_spc(is_2met_styrene    ), spca_mwt(is_2met_styrene    ) / '2met_styrene    ' , 132.20 /
  DATA spca_spc(is_cymene_p        ), spca_mwt(is_cymene_p        ) / 'cymene_p        ' , 134.22 /
  DATA spca_spc(is_cymene_o        ), spca_mwt(is_cymene_o        ) / 'cymene_o        ' , 134.22 /
  DATA spca_spc(is_phellandrene_a  ), spca_mwt(is_phellandrene_a  ) / 'phellandrene_a  ' , 136.23 /
  DATA spca_spc(is_thujene_a       ), spca_mwt(is_thujene_a       ) / 'thujene_a       ' , 136.23 /
  DATA spca_spc(is_terpinene_a     ), spca_mwt(is_terpinene_a     ) / 'terpinene_a     ' , 136.23 /
  DATA spca_spc(is_terpinene_g     ), spca_mwt(is_terpinene_g     ) / 'terpinene_g     ' , 136.23 /
  DATA spca_spc(is_terpinolene     ), spca_mwt(is_terpinolene     ) / 'terpinolene     ' , 136.23 /
  DATA spca_spc(is_phellandrene_b  ), spca_mwt(is_phellandrene_b  ) / 'phellandrene_b  ' , 136.23 /
  DATA spca_spc(is_camphene        ), spca_mwt(is_camphene        ) / 'camphene        ' , 136.23 /
  DATA spca_spc(is_bornene         ), spca_mwt(is_bornene         ) / 'bornene         ' , 136.23 /
  DATA spca_spc(is_fenchene_a      ), spca_mwt(is_fenchene_a      ) / 'fenchene_a      ' , 136.23 /
  DATA spca_spc(is_ocimene_al      ), spca_mwt(is_ocimene_al      ) / 'ocimene_al      ' , 136.23 /
  DATA spca_spc(is_ocimene_c_b     ), spca_mwt(is_ocimene_c_b     ) / 'ocimene_c_b     ' , 136.23 /
  DATA spca_spc(is_tricyclene      ), spca_mwt(is_tricyclene      ) / 'tricyclene      ' , 136.23 /
  DATA spca_spc(is_estragole       ), spca_mwt(is_estragole       ) / 'estragole       ' , 148.20 /
  DATA spca_spc(is_camphor         ), spca_mwt(is_camphor         ) / 'camphor         ' , 152.23 /
  DATA spca_spc(is_fenchone        ), spca_mwt(is_fenchone        ) / 'fenchone        ' , 152.23 /
  DATA spca_spc(is_piperitone      ), spca_mwt(is_piperitone      ) / 'piperitone      ' , 152.23 /
  DATA spca_spc(is_thujone_a       ), spca_mwt(is_thujone_a       ) / 'thujone_a       ' , 152.23 /
  DATA spca_spc(is_thujone_b       ), spca_mwt(is_thujone_b       ) / 'thujone_b       ' , 152.23 /
  DATA spca_spc(is_cineole_1_8     ), spca_mwt(is_cineole_1_8     ) / 'cineole_1_8     ' , 154.25 /
  DATA spca_spc(is_borneol         ), spca_mwt(is_borneol         ) / 'borneol         ' , 154.25 /
  DATA spca_spc(is_linalool        ), spca_mwt(is_linalool        ) / 'linalool        ' , 154.25 /
  DATA spca_spc(is_terpineol_4     ), spca_mwt(is_terpineol_4     ) / 'terpineol_4     ' , 154.25 /
  DATA spca_spc(is_terpineol_a     ), spca_mwt(is_terpineol_a     ) / 'terpineol_a     ' , 154.25 /
  DATA spca_spc(is_linalool_oxd_c  ), spca_mwt(is_linalool_oxd_c  ) / 'linalool_oxd_c  ' , 170.25 /
  DATA spca_spc(is_linalool_oxd_t  ), spca_mwt(is_linalool_oxd_t  ) / 'linalool_oxd_t  ' , 170.25 /
  DATA spca_spc(is_ionone_b        ), spca_mwt(is_ionone_b        ) / 'ionone_b        ' , 192.30 /
  DATA spca_spc(is_bornyl_act      ), spca_mwt(is_bornyl_act      ) / 'bornyl_act      ' , 196.29 /
  DATA spca_spc(is_farnescene_a    ), spca_mwt(is_farnescene_a    ) / 'farnescene_a    ' , 204.35 /
  DATA spca_spc(is_caryophyllene_b ), spca_mwt(is_caryophyllene_b ) / 'caryophyllene_b ' , 204.35 /
  DATA spca_spc(is_acoradiene      ), spca_mwt(is_acoradiene      ) / 'acoradiene      ' , 204.35 /
  DATA spca_spc(is_aromadendrene   ), spca_mwt(is_aromadendrene   ) / 'aromadendrene   ' , 204.35 /
  DATA spca_spc(is_bergamotene_a   ), spca_mwt(is_bergamotene_a   ) / 'bergamotene_a   ' , 204.35 /
  DATA spca_spc(is_bergamotene_b   ), spca_mwt(is_bergamotene_b   ) / 'bergamotene_b   ' , 204.35 /
  DATA spca_spc(is_bisabolene_a    ), spca_mwt(is_bisabolene_a    ) / 'bisabolene_a    ' , 204.35 /
  DATA spca_spc(is_bisabolene_b    ), spca_mwt(is_bisabolene_b    ) / 'bisabolene_b    ' , 204.35 /
  DATA spca_spc(is_bourbonene_b    ), spca_mwt(is_bourbonene_b    ) / 'bourbonene_b    ' , 204.35 /
  DATA spca_spc(is_cadinene_d      ), spca_mwt(is_cadinene_d      ) / 'cadinene_d      ' , 204.35 /
  DATA spca_spc(is_cadinene_g      ), spca_mwt(is_cadinene_g      ) / 'cadinene_g      ' , 204.35 /
  DATA spca_spc(is_cedrene_a       ), spca_mwt(is_cedrene_a       ) / 'cedrene_a       ' , 204.35 /
  DATA spca_spc(is_copaene_a       ), spca_mwt(is_copaene_a       ) / 'copaene_a       ' , 204.35 /
  DATA spca_spc(is_cubebene_a      ), spca_mwt(is_cubebene_a      ) / 'cubebene_a      ' , 204.35 /
  DATA spca_spc(is_cubebene_b      ), spca_mwt(is_cubebene_b      ) / 'cubebene_b      ' , 204.35 /
  DATA spca_spc(is_elemene_b       ), spca_mwt(is_elemene_b       ) / 'elemene_b       ' , 204.35 /
  DATA spca_spc(is_farnescene_b    ), spca_mwt(is_farnescene_b    ) / 'farnescene_b    ' , 204.35 /
  DATA spca_spc(is_germacrene_b    ), spca_mwt(is_germacrene_B    ) / 'germacrene_b    ' , 204.35 /
  DATA spca_spc(is_germacrene_d    ), spca_mwt(is_germacrene_D    ) / 'germacrene_d    ' , 204.35 /
  DATA spca_spc(is_gurjunene_b     ), spca_mwt(is_gurjunene_b     ) / 'gurjunene_b     ' , 204.35 /
  DATA spca_spc(is_humulene_a      ), spca_mwt(is_humulene_a      ) / 'humulene_a      ' , 204.35 /
  DATA spca_spc(is_humulene_g      ), spca_mwt(is_humulene_g      ) / 'humulene_g      ' , 204.35 /
  DATA spca_spc(is_isolongifolene  ), spca_mwt(is_isolongifolene  ) / 'isolongifolene  ' , 204.35 /
  DATA spca_spc(is_longifolene     ), spca_mwt(is_longifolene     ) / 'longifolene     ' , 204.35 /
  DATA spca_spc(is_longipinene     ), spca_mwt(is_longipinene     ) / 'longipinene     ' , 204.35 /
  DATA spca_spc(is_muurolene_a     ), spca_mwt(is_muurolene_a     ) / 'muurolene_a     ' , 204.35 /
  DATA spca_spc(is_muurolene_g     ), spca_mwt(is_muurolene_g     ) / 'muurolene_g     ' , 204.35 /
  DATA spca_spc(is_selinene_b      ), spca_mwt(is_selinene_b      ) / 'selinene_b      ' , 204.35 /
  DATA spca_spc(is_selinene_d      ), spca_mwt(is_selinene_d      ) / 'selinene_d      ' , 204.35 /
  DATA spca_spc(is_nerolidol_c     ), spca_mwt(is_nerolidol_c     ) / 'nerolidol_c     ' , 222.37 /
  DATA spca_spc(is_nerolidol_t     ), spca_mwt(is_nerolidol_t     ) / 'nerolidol_t     ' , 222.37 /
  DATA spca_spc(is_cedrol          ), spca_mwt(is_cedrol          ) / 'cedrol          ' , 222.37 /
  DATA spca_spc(is_mbo_2m3e2ol     ), spca_mwt(is_mbo_2m3e2ol     ) / 'mbo_2m3e2ol     ' , 86.13  /
  DATA spca_spc(is_methanol        ), spca_mwt(is_methanol        ) / 'methanol        ' , 32.04  /
  DATA spca_spc(is_acetone         ), spca_mwt(is_acetone         ) / 'acetone         ' , 58.08  /
  DATA spca_spc(is_methane         ), spca_mwt(is_methane         ) / 'methane         ' , 16.04  /
  DATA spca_spc(is_ammonia         ), spca_mwt(is_ammonia         ) / 'ammonia         ' , 17.03  /
  DATA spca_spc(is_nitrous_oxd     ), spca_mwt(is_nitrous_oxd     ) / 'nitrous_oxd     ' , 44.01  /
  DATA spca_spc(is_nitric_oxd      ), spca_mwt(is_nitric_oxd      ) / 'nitric_oxd      ' , 30.01  /
  DATA spca_spc(is_acetaldehyde    ), spca_mwt(is_acetaldehyde    ) / 'acetaldehyde    ' , 44.05  /
  DATA spca_spc(is_ethanol         ), spca_mwt(is_ethanol         ) / 'ethanol         ' , 46.07  /
  DATA spca_spc(is_formic_acid     ), spca_mwt(is_formic_acid     ) / 'formic_acid     ' , 46.03  /
  DATA spca_spc(is_formaldehyde    ), spca_mwt(is_formaldehyde    ) / 'formaldehyde    ' , 30.03  /
  DATA spca_spc(is_acetic_acid     ), spca_mwt(is_acetic_acid     ) / 'acetic_acid     ' , 60.05  /
  DATA spca_spc(is_mbo_3m2e1ol     ), spca_mwt(is_mbo_3m2e1ol     ) / 'mbo_3m2e1ol     ' , 86.13  /
  DATA spca_spc(is_mbo_3m3e1ol     ), spca_mwt(is_mbo_3m3e1ol     ) / 'mbo_3m3e1ol     ' , 86.13  /
  DATA spca_spc(is_benzaldehyde    ), spca_mwt(is_benzaldehyde    ) / 'benzaldehyde    ' , 106.12 /
  DATA spca_spc(is_butanone_2      ), spca_mwt(is_butanone_2      ) / 'butanone_2      ' , 72.11  /
  DATA spca_spc(is_decanal         ), spca_mwt(is_decanal         ) / 'decanal         ' , 156.27 /
  DATA spca_spc(is_dodecene_1      ), spca_mwt(is_dodecene_1      ) / 'dodecene_1      ' , 168.32 /
  DATA spca_spc(is_geranyl_acetone ), spca_mwt(is_geranyl_acetone ) / 'geranyl_acetone ' , 194.31 /
  DATA spca_spc(is_heptanal        ), spca_mwt(is_heptanal        ) / 'heptanal        ' , 114.19 /
  DATA spca_spc(is_heptane         ), spca_mwt(is_heptane         ) / 'heptane         ' , 100.20 /
  DATA spca_spc(is_hexane          ), spca_mwt(is_hexane          ) / 'hexane          ' , 86.18  /
  DATA spca_spc(is_met_benzoate    ), spca_mwt(is_met_benzoate    ) / 'met_benzoate    ' , 136.15 /
  DATA spca_spc(is_met_heptenone   ), spca_mwt(is_met_heptenone   ) / 'met_heptenone   ' , 126.20 /
  DATA spca_spc(is_neryl_acetone   ), spca_mwt(is_neryl_acetone   ) / 'neryl_acetone   ' , 194.31 /
  DATA spca_spc(is_nonanal         ), spca_mwt(is_nonanal         ) / 'nonanal         ' , 142.24 /
  DATA spca_spc(is_nonenal         ), spca_mwt(is_nonenal         ) / 'nonenal         ' , 140.22 /
  DATA spca_spc(is_octanal         ), spca_mwt(is_octanal         ) / 'octanal         ' , 128.21 /
  DATA spca_spc(is_octanol         ), spca_mwt(is_octanol         ) / 'octanol         ' , 130.23 /
  DATA spca_spc(is_octenol_1e3ol   ), spca_mwt(is_octenol_1e3ol   ) / 'octenol_1e3ol   ' , 128.21 /
  DATA spca_spc(is_oxopentanal     ), spca_mwt(is_oxopentanal     ) / 'oxopentanal     ' , 100.12 /
  DATA spca_spc(is_pentane         ), spca_mwt(is_pentane         ) / 'pentane         ' , 72.15  /
  DATA spca_spc(is_phenyl_cco      ), spca_mwt(is_phenyl_cco      ) / 'phenyl_cco      ' , 120.15 /
  DATA spca_spc(is_pyruvic_acid    ), spca_mwt(is_pyruvic_acid    ) / 'pyruvic_acid    ' , 88.06  /
  DATA spca_spc(is_terpinyl_act_a  ), spca_mwt(is_terpinyl_act_a  ) / 'terpinyl_act_a  ' , 196.29 /
  DATA spca_spc(is_tetradecene_1   ), spca_mwt(is_tetradecene_1   ) / 'tetradecene_1   ' , 196.37 /
  DATA spca_spc(is_toluene         ), spca_mwt(is_toluene         ) / 'toluene         ' , 92.14  /
  DATA spca_spc(is_carbon_monoxide ), spca_mwt(is_carbon_monoxide ) / 'carbon_monoxide ' , 28.01  /
  DATA spca_spc(is_butene          ), spca_mwt(is_butene          ) / 'butene          ' , 56.11  /
  DATA spca_spc(is_ethane          ), spca_mwt(is_ethane          ) / 'ethane          ' , 30.07  /
  DATA spca_spc(is_ethene          ), spca_mwt(is_ethene          ) / 'ethene          ' , 28.05  /
  DATA spca_spc(is_hydrogen_cyanide), spca_mwt(is_hydrogen_cyanide) / 'hydrogen_cyanide' , 27.03  /
  DATA spca_spc(is_propane         ), spca_mwt(is_propane         ) / 'propane         ' , 44.10  /
  DATA spca_spc(is_propene         ), spca_mwt(is_propene         ) / 'propene         ' , 42.08  /
  DATA spca_spc(is_carbon_2s       ), spca_mwt(is_carbon_2s       ) / 'carbon_2s       ' , 76.14  /
  DATA spca_spc(is_carbonyl_s      ), spca_mwt(is_carbonyl_s      ) / 'carbonyl_s      ' , 60.08  /
  DATA spca_spc(is_diallyl_2s      ), spca_mwt(is_diallyl_2s      ) / 'diallyl_2s      ' , 146.28 /
  DATA spca_spc(is_2met_2s         ), spca_mwt(is_2met_2s         ) / '2met_2s         ' , 94.20  /
  DATA spca_spc(is_2met_s          ), spca_mwt(is_2met_s          ) / '2met_s          ' , 62.14  /
  DATA spca_spc(is_met_chloride    ), spca_mwt(is_met_chloride    ) / 'met_chloride    ' , 50.49  /
  DATA spca_spc(is_met_bromide     ), spca_mwt(is_met_bromide     ) / 'met_bromide     ' , 94.94  /
  DATA spca_spc(is_met_iodide      ), spca_mwt(is_met_iodide      ) / 'met_iodide      ' , 141.94 /
  DATA spca_spc(is_hydrogen_s      ), spca_mwt(is_hydrogen_s      ) / 'hydrogen_s      ' , 34.08  /
  DATA spca_spc(is_met_mercaptan   ), spca_mwt(is_met_mercaptan   ) / 'met_mercaptan   ' , 48.11  /
  DATA spca_spc(is_met_propenyl_2s ), spca_mwt(is_met_propenyl_2s ) / 'met_propenyl_2s ' , 120.24 /
  DATA spca_spc(is_pppp_2s         ), spca_mwt(is_pppp_2s         ) / 'pppp_2s         ' , 148.29 /
  DATA spca_spc(is_2met_nonatriene ), spca_mwt(is_2met_nonatriene ) / '2met_nonatriene ' , 150.26 /
  DATA spca_spc(is_met_salicylate  ), spca_mwt(is_met_salicylate  ) / 'met_salicylate  ' , 152.15 /
  DATA spca_spc(is_indole          ), spca_mwt(is_indole          ) / 'indole          ' , 117.15 /
  DATA spca_spc(is_jasmone         ), spca_mwt(is_jasmone         ) / 'jasmone         ' , 164.24 /
  DATA spca_spc(is_met_jasmonate   ), spca_mwt(is_met_jasmonate   ) / 'met_jasmonate   ' , 224.30 /
  DATA spca_spc(is_3met_3dctt      ), spca_mwt(is_3met_3dctt      ) / '3met_3dctt      ' , 218.38 /
  DATA spca_spc(is_hexanal         ), spca_mwt(is_hexanal         ) / 'hexanal         ' , 100.16 /
  DATA spca_spc(is_hexanol_1       ), spca_mwt(is_hexanol_1       ) / 'hexanol_1       ' , 102.17 /
  DATA spca_spc(is_hexenal_c3      ), spca_mwt(is_hexenal_c3      ) / 'hexenal_c3      ' , 98.14  /
  DATA spca_spc(is_hexenal_t2      ), spca_mwt(is_hexenal_t2      ) / 'hexenal_t2      ' , 98.14  /
  DATA spca_spc(is_hexenol_c3      ), spca_mwt(is_hexenol_c3      ) / 'hexenol_c3      ' , 100.16 /
  DATA spca_spc(is_hexenyl_act_c3  ), spca_mwt(is_hexenyl_act_c3  ) / 'hexenyl_act_c3  ' , 142.20 /


  !=====================================================================
  ! The following is a "table" for mapping nmegan=n_spca_spc=138 
  ! MEGAN v2.04 species to n_mgn_spc=20 classes of species.  
  ! See Table 3 of MEGAN v2.0 Documentation.
  ! Originally from Tan's MAP_MGN20T138.EXT.EXT (12/02/2006)
  !=====================================================================
                                                                                                     
  INTEGER, DIMENSION(n_spca_spc) :: mg20_map
                                                                                   
  !---------------------------------------------------------------------
  ! Some naming convention
  ! _a  = alpha     , _b  = beta      , _c  = cis       , _al = allo    ,
  ! _g  = gamma     , _d  = delta     , _t  = trans     , _m  = methyl  ,
  ! _p  = para      , _o  = ortho     , _e  = ene       , _ol = ol      ,
  ! met = methyl    , 2met= dimethyl  , MBO = methylbutenol             ,
  ! 2s  = disulfide , s   = sulfide   , OXD = oxide     , ACT = acetate ,
  ! PPPP= propenylpropyl              , DCTT= decatetraene              ,
  ! CCO = acetaldehyde
  !--------------------------------------------------------------------

  DATA    mg20_map ( is_isoprene        )        /  imgn_isop     /
  DATA    mg20_map ( is_myrcene         )        /  imgn_myrc     /
  DATA    mg20_map ( is_sabinene        )        /  imgn_sabi     /
  DATA    mg20_map ( is_limonene        )        /  imgn_limo     /
  DATA    mg20_map ( is_carene_3        )        /  imgn_3car     /
  DATA    mg20_map ( is_ocimene_t_b     )        /  imgn_ocim     /
  DATA    mg20_map ( is_pinene_b        )        /  imgn_bpin     /
  DATA    mg20_map ( is_pinene_a        )        /  imgn_apin     /
  DATA    mg20_map ( is_2met_styrene    )        /  imgn_omtp     /
  DATA    mg20_map ( is_cymene_p        )        /  imgn_omtp     /
  DATA    mg20_map ( is_cymene_o        )        /  imgn_omtp     /
  DATA    mg20_map ( is_phellandrene_a  )        /  imgn_omtp     /
  DATA    mg20_map ( is_thujene_a       )        /  imgn_omtp     /
  DATA    mg20_map ( is_terpinene_a     )        /  imgn_omtp     /
  DATA    mg20_map ( is_terpinene_g     )        /  imgn_omtp     /
  DATA    mg20_map ( is_terpinolene     )        /  imgn_omtp     /
  DATA    mg20_map ( is_phellandrene_b  )        /  imgn_omtp     /
  DATA    mg20_map ( is_camphene        )        /  imgn_omtp     /
  DATA    mg20_map ( is_bornene         )        /  imgn_omtp     /
  DATA    mg20_map ( is_fenchene_a      )        /  imgn_omtp     /
  DATA    mg20_map ( is_ocimene_al      )        /  imgn_omtp     /
  DATA    mg20_map ( is_ocimene_c_b     )        /  imgn_omtp     /
  DATA    mg20_map ( is_tricyclene      )        /  imgn_omtp     /
  DATA    mg20_map ( is_estragole       )        /  imgn_omtp     /
  DATA    mg20_map ( is_camphor         )        /  imgn_omtp     /
  DATA    mg20_map ( is_fenchone        )        /  imgn_omtp     /
  DATA    mg20_map ( is_piperitone      )        /  imgn_omtp     /
  DATA    mg20_map ( is_thujone_a       )        /  imgn_omtp     /
  DATA    mg20_map ( is_thujone_b       )        /  imgn_omtp     /
  DATA    mg20_map ( is_cineole_1_8     )        /  imgn_omtp     /
  DATA    mg20_map ( is_borneol         )        /  imgn_omtp     /
  DATA    mg20_map ( is_linalool        )        /  imgn_omtp     /
  DATA    mg20_map ( is_terpineol_4     )        /  imgn_omtp     /
  DATA    mg20_map ( is_terpineol_a     )        /  imgn_omtp     /
  DATA    mg20_map ( is_linalool_oxd_c  )        /  imgn_omtp     /
  DATA    mg20_map ( is_linalool_oxd_t  )        /  imgn_omtp     /
  DATA    mg20_map ( is_ionone_b        )        /  imgn_omtp     /
  DATA    mg20_map ( is_bornyl_act      )        /  imgn_omtp     /
  DATA    mg20_map ( is_farnescene_a    )        /  imgn_afarn    /
  DATA    mg20_map ( is_caryophyllene_b )        /  imgn_bcar     /
  DATA    mg20_map ( is_acoradiene      )        /  imgn_osqt     /
  DATA    mg20_map ( is_aromadendrene   )        /  imgn_osqt     /
  DATA    mg20_map ( is_bergamotene_a   )        /  imgn_osqt     /
  DATA    mg20_map ( is_bergamotene_b   )        /  imgn_osqt     /
  DATA    mg20_map ( is_bisabolene_a    )        /  imgn_osqt     /
  DATA    mg20_map ( is_bisabolene_b    )        /  imgn_osqt     /
  DATA    mg20_map ( is_bourbonene_b    )        /  imgn_osqt     /
  DATA    mg20_map ( is_cadinene_d      )        /  imgn_osqt     /
  DATA    mg20_map ( is_cadinene_g      )        /  imgn_osqt     /
  DATA    mg20_map ( is_cedrene_a       )        /  imgn_osqt     /
  DATA    mg20_map ( is_copaene_a       )        /  imgn_osqt     /
  DATA    mg20_map ( is_cubebene_a      )        /  imgn_osqt     /
  DATA    mg20_map ( is_cubebene_b      )        /  imgn_osqt     /
  DATA    mg20_map ( is_elemene_b       )        /  imgn_osqt     /
  DATA    mg20_map ( is_farnescene_b    )        /  imgn_osqt     /
  DATA    mg20_map ( is_germacrene_b    )        /  imgn_osqt     /
  DATA    mg20_map ( is_germacrene_d    )        /  imgn_osqt     /
  DATA    mg20_map ( is_gurjunene_b     )        /  imgn_osqt     /
  DATA    mg20_map ( is_humulene_a      )        /  imgn_osqt     /
  DATA    mg20_map ( is_humulene_g      )        /  imgn_osqt     /
  DATA    mg20_map ( is_isolongifolene  )        /  imgn_osqt     /
  DATA    mg20_map ( is_longifolene     )        /  imgn_osqt     /
  DATA    mg20_map ( is_longipinene     )        /  imgn_osqt     /
  DATA    mg20_map ( is_muurolene_a     )        /  imgn_osqt     /
  DATA    mg20_map ( is_muurolene_g     )        /  imgn_osqt     /
  DATA    mg20_map ( is_selinene_b      )        /  imgn_osqt     /
  DATA    mg20_map ( is_selinene_d      )        /  imgn_osqt     /
  DATA    mg20_map ( is_nerolidol_c     )        /  imgn_osqt     /
  DATA    mg20_map ( is_nerolidol_t     )        /  imgn_osqt     /
  DATA    mg20_map ( is_cedrol          )        /  imgn_osqt     /
  DATA    mg20_map ( is_mbo_2m3e2ol     )        /  imgn_mbo      /
  DATA    mg20_map ( is_methanol        )        /  imgn_meoh     /
  DATA    mg20_map ( is_acetone         )        /  imgn_acto     /
  DATA    mg20_map ( is_methane         )        /  imgn_ch4      /
  DATA    mg20_map ( is_ammonia         )        /  imgn_no       /
  DATA    mg20_map ( is_nitrous_oxd     )        /  imgn_no       /
  DATA    mg20_map ( is_nitric_oxd      )        /  imgn_no       /
  DATA    mg20_map ( is_acetaldehyde    )        /  imgn_acta     /
  DATA    mg20_map ( is_ethanol         )        /  imgn_acta     /
  DATA    mg20_map ( is_formic_acid     )        /  imgn_form     /
  DATA    mg20_map ( is_formaldehyde    )        /  imgn_form     /
  DATA    mg20_map ( is_acetic_acid     )        /  imgn_form     /
  DATA    mg20_map ( is_mbo_3m2e1ol     )        /  imgn_co       /
  DATA    mg20_map ( is_mbo_3m3e1ol     )        /  imgn_co       /
  DATA    mg20_map ( is_benzaldehyde    )        /  imgn_co       /
  DATA    mg20_map ( is_butanone_2      )        /  imgn_co       /
  DATA    mg20_map ( is_decanal         )        /  imgn_co       /
  DATA    mg20_map ( is_dodecene_1      )        /  imgn_co       /
  DATA    mg20_map ( is_geranyl_acetone )        /  imgn_co       /
  DATA    mg20_map ( is_heptanal        )        /  imgn_co       /
  DATA    mg20_map ( is_heptane         )        /  imgn_co       /
  DATA    mg20_map ( is_hexane          )        /  imgn_co       /
  DATA    mg20_map ( is_met_benzoate    )        /  imgn_co       /
  DATA    mg20_map ( is_met_heptenone   )        /  imgn_co       /
  DATA    mg20_map ( is_neryl_acetone   )        /  imgn_co       /
  DATA    mg20_map ( is_nonanal         )        /  imgn_co       /
  DATA    mg20_map ( is_nonenal         )        /  imgn_co       /
  DATA    mg20_map ( is_octanal         )        /  imgn_co       /
  DATA    mg20_map ( is_octanol         )        /  imgn_co       /
  DATA    mg20_map ( is_octenol_1e3ol   )        /  imgn_co       /
  DATA    mg20_map ( is_oxopentanal     )        /  imgn_co       /
  DATA    mg20_map ( is_pentane         )        /  imgn_co       /
  DATA    mg20_map ( is_phenyl_cco      )        /  imgn_co       /
  DATA    mg20_map ( is_pyruvic_acid    )        /  imgn_co       /
  DATA    mg20_map ( is_terpinyl_act_a  )        /  imgn_co       /
  DATA    mg20_map ( is_tetradecene_1   )        /  imgn_co       /
  DATA    mg20_map ( is_toluene         )        /  imgn_co       /
  DATA    mg20_map ( is_carbon_monoxide )        /  imgn_co       /
  DATA    mg20_map ( is_butene          )        /  imgn_co       /
  DATA    mg20_map ( is_ethane          )        /  imgn_co       /
  DATA    mg20_map ( is_ethene          )        /  imgn_co       /
  DATA    mg20_map ( is_hydrogen_cyanide)        /  imgn_co       /
  DATA    mg20_map ( is_propane         )        /  imgn_co       /
  DATA    mg20_map ( is_propene         )        /  imgn_co       /
  DATA    mg20_map ( is_carbon_2s       )        /  imgn_co       /
  DATA    mg20_map ( is_carbonyl_s      )        /  imgn_co       /
  DATA    mg20_map ( is_diallyl_2s      )        /  imgn_co       /
  DATA    mg20_map ( is_2met_2s         )        /  imgn_co       /
  DATA    mg20_map ( is_2met_s          )        /  imgn_co       /
  DATA    mg20_map ( is_met_chloride    )        /  imgn_co       /
  DATA    mg20_map ( is_met_bromide     )        /  imgn_co       /
  DATA    mg20_map ( is_met_iodide      )        /  imgn_co       /
  DATA    mg20_map ( is_hydrogen_s      )        /  imgn_co       /
  DATA    mg20_map ( is_met_mercaptan   )        /  imgn_co       /
  DATA    mg20_map ( is_met_propenyl_2s )        /  imgn_co       /
  DATA    mg20_map ( is_pppp_2s         )        /  imgn_co       /
  DATA    mg20_map ( is_2met_nonatriene )        /  imgn_co       /
  DATA    mg20_map ( is_met_salicylate  )        /  imgn_co       /
  DATA    mg20_map ( is_indole          )        /  imgn_co       /
  DATA    mg20_map ( is_jasmone         )        /  imgn_co       /
  DATA    mg20_map ( is_met_jasmonate   )        /  imgn_co       /
  DATA    mg20_map ( is_3met_3dctt      )        /  imgn_co       /
  DATA    mg20_map ( is_hexanal         )        /  imgn_co       /
  DATA    mg20_map ( is_hexanol_1       )        /  imgn_co       /
  DATA    mg20_map ( is_hexenal_c3      )        /  imgn_co       /
  DATA    mg20_map ( is_hexenal_t2      )        /  imgn_co       /
  DATA    mg20_map ( is_hexenol_c3      )        /  imgn_co       /
  DATA    mg20_map ( is_hexenyl_act_c3  )        /  imgn_co       /

  !====================================================================
  ! The following is a table of for the speciation fractions 
  ! (dimensionless) of the base emission factors EF of n_mgn_spc=20 
  ! classes of species into n_spca_spc=138 species.  
  ! See Table 3 of MEGAN v2.0 Documentation.
  ! Originally from Tan's EFFS_MGN20T138.EXT (12/02/2006)
  ! Updated on 08/13/2007 based on Alex Guenther's email.
  !====================================================================
  REAL, DIMENSION (n_spca_spc, n_pft) :: EF_frac

  ! The row order of the data statements below can be changed; however
  ! the column order is hardwreid to k_bt=1, k_nt=2, k_sb=3, and
  ! k_hb=4 (declared above).  Do not change the column order
  ! of the data entry below.

  ! so many decimal places?
  !                                                   BT            NT            SB            HB
  DATA  EF_frac( is_isoprene        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     / !not used for isoprene
  DATA  EF_frac( is_myrcene         , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_sabinene        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_limonene        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_carene_3        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_ocimene_t_b     , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_pinene_b        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_pinene_a        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_2met_styrene    , k_bt:k_hb )   / 0.011470    , 0.005540    , 0.009240    , 0.010400     /
  DATA  EF_frac( is_cymene_p        , k_bt:k_hb )   / 0.057340    , 0.055430    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_cymene_o        , k_bt:k_hb )   / 0.034400    , 0.016630    , 0.027730    , 0.031190     /
  DATA  EF_frac( is_phellandrene_a  , k_bt:k_hb )   / 0.045870    , 0.055430    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_thujene_a       , k_bt:k_hb )   / 0.011470    , 0.033260    , 0.036970    , 0.041580     /
  DATA  EF_frac( is_terpinene_a     , k_bt:k_hb )   / 0.057340    , 0.055430    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_terpinene_g     , k_bt:k_hb )   / 0.057340    , 0.055430    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_terpinolene     , k_bt:k_hb )   / 0.057340    , 0.066520    , 0.055450    , 0.062370     /
  DATA  EF_frac( is_phellandrene_b  , k_bt:k_hb )   / 0.057340    , 0.166300    , 0.092420    , 0.103950     /
  DATA  EF_frac( is_camphene        , k_bt:k_hb )   / 0.172020    , 0.249450    , 0.184840    , 0.145530     /
  DATA  EF_frac( is_bornene         , k_bt:k_hb )   / 0.010320    , 0.004990    , 0.008320    , 0.008320     /
  DATA  EF_frac( is_fenchene_a      , k_bt:k_hb )   / 0.003440    , 0.001660    , 0.002770    , 0.004160     /
  DATA  EF_frac( is_ocimene_al      , k_bt:k_hb )   / 0.011470    , 0.005540    , 0.009240    , 0.010400     /
  DATA  EF_frac( is_ocimene_c_b     , k_bt:k_hb )   / 0.045870    , 0.022170    , 0.036970    , 0.041580     /
  DATA  EF_frac( is_tricyclene      , k_bt:k_hb )   / 0.011470    , 0.005540    , 0.009240    , 0.010400     /
  DATA  EF_frac( is_estragole       , k_bt:k_hb )   / 0.003440    , 0.001660    , 0.002770    , 0.004160     /
  DATA  EF_frac( is_camphor         , k_bt:k_hb )   / 0.034400    , 0.033260    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_fenchone        , k_bt:k_hb )   / 0.011470    , 0.005540    , 0.009240    , 0.010400     /
  DATA  EF_frac( is_piperitone      , k_bt:k_hb )   / 0.003440    , 0.001660    , 0.002770    , 0.004160     /
  DATA  EF_frac( is_thujone_a       , k_bt:k_hb )   / 0.011470    , 0.027720    , 0.046210    , 0.041580     /
  DATA  EF_frac( is_thujone_b       , k_bt:k_hb )   / 0.002290    , 0.005540    , 0.009240    , 0.010400     /
  DATA  EF_frac( is_cineole_1_8     , k_bt:k_hb )   / 0.057340    , 0.011090    , 0.036970    , 0.041580     /
  DATA  EF_frac( is_borneol         , k_bt:k_hb )   / 0.008030    , 0.003880    , 0.006470    , 0.006240     /
  DATA  EF_frac( is_linalool        , k_bt:k_hb )   / 0.137610    , 0.066520    , 0.110910    , 0.124740     /
  DATA  EF_frac( is_terpineol_4     , k_bt:k_hb )   / 0.006880    , 0.003330    , 0.005550    , 0.006240     /
  DATA  EF_frac( is_terpineol_a     , k_bt:k_hb )   / 0.034400    , 0.016630    , 0.027730    , 0.031190     /
  DATA  EF_frac( is_linalool_oxd_c  , k_bt:k_hb )   / 0.006880    , 0.003330    , 0.005550    , 0.006240     /
  DATA  EF_frac( is_linalool_oxd_t  , k_bt:k_hb )   / 0.034400    , 0.016630    , 0.027730    , 0.031190     /
  DATA  EF_frac( is_ionone_b        , k_bt:k_hb )   / 0.002290    , 0.001110    , 0.001850    , 0.002080     /
  DATA  EF_frac( is_bornyl_act      , k_bt:k_hb )   / 0.001150    , 0.002770    , 0.002770    , 0.002080     /
  DATA  EF_frac( is_farnescene_a    , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_caryophyllene_b , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_acoradiene      , k_bt:k_hb )   / 0.018570    , 0.015950    , 0.019160    , 0.021860     /
  DATA  EF_frac( is_aromadendrene   , k_bt:k_hb )   / 0.007430    , 0.006380    , 0.007660    , 0.010930     /
  DATA  EF_frac( is_bergamotene_a   , k_bt:k_hb )   / 0.083570    , 0.143540    , 0.095790    , 0.098360     /
  DATA  EF_frac( is_bergamotene_b   , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_bisabolene_a    , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_bisabolene_b    , k_bt:k_hb )   / 0.055710    , 0.119620    , 0.067050    , 0.076500     /
  DATA  EF_frac( is_bourbonene_b    , k_bt:k_hb )   / 0.027860    , 0.023920    , 0.028740    , 0.032790     /
  DATA  EF_frac( is_cadinene_d      , k_bt:k_hb )   / 0.013930    , 0.011960    , 0.014370    , 0.016390     /
  DATA  EF_frac( is_cadinene_g      , k_bt:k_hb )   / 0.009290    , 0.007970    , 0.009580    , 0.010930     /
  DATA  EF_frac( is_cedrene_a       , k_bt:k_hb )   / 0.005570    , 0.004780    , 0.005750    , 0.005460     /
  DATA  EF_frac( is_copaene_a       , k_bt:k_hb )   / 0.009290    , 0.007970    , 0.009580    , 0.010930     /
  DATA  EF_frac( is_cubebene_a      , k_bt:k_hb )   / 0.013930    , 0.011960    , 0.014370    , 0.016390     /
  DATA  EF_frac( is_cubebene_b      , k_bt:k_hb )   / 0.009290    , 0.007970    , 0.009580    , 0.010930     /
  DATA  EF_frac( is_elemene_b       , k_bt:k_hb )   / 0.018570    , 0.015950    , 0.019160    , 0.021860     /
  DATA  EF_frac( is_farnescene_b    , k_bt:k_hb )   / 0.278550    , 0.239230    , 0.287360    , 0.218580     /
  DATA  EF_frac( is_germacrene_b    , k_bt:k_hb )   / 0.009290    , 0.007970    , 0.009580    , 0.010930     /
  DATA  EF_frac( is_germacrene_d    , k_bt:k_hb )   / 0.027860    , 0.023920    , 0.028740    , 0.032790     /
  DATA  EF_frac( is_gurjunene_b     , k_bt:k_hb )   / 0.004640    , 0.003990    , 0.004790    , 0.005460     /
  DATA  EF_frac( is_humulene_a      , k_bt:k_hb )   / 0.139280    , 0.199360    , 0.172410    , 0.163930     /
  DATA  EF_frac( is_humulene_g      , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_isolongifolene  , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_longifolene     , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_longipinene     , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_muurolene_a     , k_bt:k_hb )   / 0.013930    , 0.011960    , 0.014370    , 0.016390     /
  DATA  EF_frac( is_muurolene_g     , k_bt:k_hb )   / 0.046430    , 0.039870    , 0.047890    , 0.054640     /
  DATA  EF_frac( is_selinene_b      , k_bt:k_hb )   / 0.185700    , 0.079740    , 0.114940    , 0.109290     /
  DATA  EF_frac( is_selinene_d      , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_nerolidol_c     , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_nerolidol_t     , k_bt:k_hb )   / 0.004640    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_cedrol          , k_bt:k_hb )   / 0.001860    , 0.001590    , 0.001920    , 0.005460     /
  DATA  EF_frac( is_mbo_2m3e2ol     , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_methanol        , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_acetone         , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_methane         , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_ammonia         , k_bt:k_hb )   / 0.000000    , 0.000000    , 0.000000    , 0.000000     /
  DATA  EF_frac( is_nitrous_oxd     , k_bt:k_hb )   / 0.000000    , 0.000000    , 0.000000    , 0.000000     /
  DATA  EF_frac( is_nitric_oxd      , k_bt:k_hb )   / 1.000000    , 1.000000    , 1.000000    , 1.000000     /
  DATA  EF_frac( is_acetaldehyde    , k_bt:k_hb )   / 0.500000    , 0.500000    , 0.500000    , 0.500000     /
  DATA  EF_frac( is_ethanol         , k_bt:k_hb )   / 0.500000    , 0.500000    , 0.500000    , 0.500000     /
  DATA  EF_frac( is_formic_acid     , k_bt:k_hb )   / 0.285710    , 0.285710    , 0.285710    , 0.285710     /
  DATA  EF_frac( is_formaldehyde    , k_bt:k_hb )   / 0.428570    , 0.428570    , 0.428570    , 0.428570     /
  DATA  EF_frac( is_acetic_acid     , k_bt:k_hb )   / 0.285710    , 0.285710    , 0.285710    , 0.285710     /
  DATA  EF_frac( is_mbo_3m2e1ol     , k_bt:k_hb )   / 0.000520    , 0.000520    , 0.000520    , 0.000520     /
  DATA  EF_frac( is_mbo_3m3e1ol     , k_bt:k_hb )   / 0.000520    , 0.000520    , 0.000520    , 0.000520     /
  DATA  EF_frac( is_benzaldehyde    , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_butanone_2      , k_bt:k_hb )   / 0.001030    , 0.001030    , 0.001030    , 0.001030     /
  DATA  EF_frac( is_decanal         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_dodecene_1      , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_geranyl_acetone , k_bt:k_hb )   / 0.003100    , 0.003100    , 0.003100    , 0.003100     /
  DATA  EF_frac( is_heptanal        , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_heptane         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_hexane          , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_met_benzoate    , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_met_heptenone   , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_neryl_acetone   , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_nonanal         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_nonenal         , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_octanal         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_octanol         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_octenol_1e3ol   , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_oxopentanal     , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_pentane         , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_phenyl_cco      , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_pyruvic_acid    , k_bt:k_hb )   / 0.002060    , 0.002060    , 0.002060    , 0.002060     /
  DATA  EF_frac( is_terpinyl_act_a  , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_tetradecene_1   , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_toluene         , k_bt:k_hb )   / 0.002060    , 0.002060    , 0.002060    , 0.002060     /
  DATA  EF_frac( is_carbon_monoxide , k_bt:k_hb )   / 0.619070    , 0.619070    , 0.619070    , 0.619070     /
  DATA  EF_frac( is_butene          , k_bt:k_hb )   / 0.036110    , 0.036110    , 0.036110    , 0.036110     /
  DATA  EF_frac( is_ethane          , k_bt:k_hb )   / 0.002060    , 0.002060    , 0.002060    , 0.002060     /
  DATA  EF_frac( is_ethene          , k_bt:k_hb )   / 0.134130    , 0.134130    , 0.134130    , 0.134130     /
  DATA  EF_frac( is_hydrogen_cyanide, k_bt:k_hb )   / 0.004130    , 0.004130    , 0.004130    , 0.004130     /
  DATA  EF_frac( is_propane         , k_bt:k_hb )   / 0.001030    , 0.001030    , 0.001030    , 0.001030     /
  DATA  EF_frac( is_propene         , k_bt:k_hb )   / 0.082540    , 0.082540    , 0.082540    , 0.082540     /
  DATA  EF_frac( is_carbon_2s       , k_bt:k_hb )   / 0.000310    , 0.000310    , 0.000310    , 0.000310     /
  DATA  EF_frac( is_carbonyl_s      , k_bt:k_hb )   / 0.000620    , 0.000620    , 0.000620    , 0.000620     /
  DATA  EF_frac( is_diallyl_2s      , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_2met_2s         , k_bt:k_hb )   / 0.000310    , 0.000310    , 0.000310    , 0.000310     /
  DATA  EF_frac( is_2met_s          , k_bt:k_hb )   / 0.001240    , 0.001240    , 0.001240    , 0.001240     /
  DATA  EF_frac( is_met_chloride    , k_bt:k_hb )   / 0.001030    , 0.001030    , 0.001030    , 0.001030     /
  DATA  EF_frac( is_met_bromide     , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_met_iodide      , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_hydrogen_s      , k_bt:k_hb )   / 0.000520    , 0.000520    , 0.000520    , 0.000520     /
  DATA  EF_frac( is_met_mercaptan   , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_met_propenyl_2s , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_pppp_2s         , k_bt:k_hb )   / 0.000100    , 0.000100    , 0.000100    , 0.000100     /
  DATA  EF_frac( is_2met_nonatriene , k_bt:k_hb )   / 0.020640    , 0.020640    , 0.020640    , 0.020640     /
  DATA  EF_frac( is_met_salicylate  , k_bt:k_hb )   / 0.002060    , 0.002060    , 0.002060    , 0.002060     /
  DATA  EF_frac( is_indole          , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_jasmone         , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_met_jasmonate   , k_bt:k_hb )   / 0.002060    , 0.002060    , 0.002060    , 0.002060     /
  DATA  EF_frac( is_3met_3dctt      , k_bt:k_hb )   / 0.000210    , 0.000210    , 0.000210    , 0.000210     /
  DATA  EF_frac( is_hexanal         , k_bt:k_hb )   / 0.003100    , 0.003100    , 0.003100    , 0.003100     /
  DATA  EF_frac( is_hexanol_1       , k_bt:k_hb )   / 0.003100    , 0.003100    , 0.003100    , 0.003100     /
  DATA  EF_frac( is_hexenal_c3      , k_bt:k_hb )   / 0.025790    , 0.025790    , 0.025790    , 0.025790     /
  DATA  EF_frac( is_hexenal_t2      , k_bt:k_hb )   / 0.015480    , 0.015480    , 0.015480    , 0.015480     /
  DATA  EF_frac( is_hexenol_c3      , k_bt:k_hb )   / 0.015480    , 0.015480    , 0.015480    , 0.015480     /
  DATA  EF_frac( is_hexenyl_act_c3  , k_bt:k_hb )   / 0.015480    , 0.015480    , 0.015480    , 0.015480     / 


END MODULE module_data_megan2
