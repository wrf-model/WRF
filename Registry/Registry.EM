#  Registry file, EM

#INCLUDES
include registry.dimspec
include registry.em_shared_collection

# added to output 5 for ESMF
state   real   landmask            ij    misc          1     -     i0125rh056d=(interp_fcnm_imask)u=(copy_fcnm)   "LANDMASK"      "LAND MASK (1 FOR LAND, 0 FOR WATER)"  ""
state   real   lakemask            ij    misc          1     -     i012rhd=(interp_fcnm_imask)u=(copy_fcnm)      "LAKEMASK"      "LAKE MASK (1 FOR LAKE, 0 FOR NON-LAKE)"  ""

# Masked SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c_mask:lu_index,tslb,num_soil_layers,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"
# Simple SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state    real   SST              ij     misc        1         -     i01245rh0d=(interp_mask_field:lu_index,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state    real   SST_INPUT        ij     misc        1         -     rh   "SST_INPUT"              "SEA SURFACE TEMPERATURE FROM WRFLOWINPUT FILE" "K"


# Registry entries that are exclusive to Registry.EM

# Chem Scalars
rconfig   integer     chem_opt            namelist,physics	max_domains    0       rh       "chem_opt"              ""      ""
state   real    -              ikjftb   chem        1         -     -    -                                       
#
# Tracer Scalars: If you want full resolved and non-resolved dispersion, compile WRF-Chem
#
state   real    -          ikjftb  tracer        1         -     -    -                                       
state   real    tr17_1     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_1"         "tr17_1"     "Dimensionless"
state   real    tr17_2     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_2"         "tr17_2"     "Dimensionless"
state   real    tr17_3     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_3"         "tr17_3"     "Dimensionless"
state   real    tr17_4     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_4"         "tr17_4"     "Dimensionless"
state   real    tr17_5     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_5"         "tr17_5"     "Dimensionless"
state   real    tr17_6     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_6"         "tr17_6"     "Dimensionless"
state   real    tr17_7     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_7"         "tr17_7"     "Dimensionless"
state   real    tr17_8     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_8"         "tr17_8"     "Dimensionless"

package   tracer_test1  tracer_opt==2       -             tracer:tr17_1,tr17_2,tr17_3,tr17_4,tr17_5,tr17_6,tr17_7,tr17_8

package   restofwrf      use_wps_input==0            -             -
 
package   realonly       use_wps_input==1            -            state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,lai12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,cl_gc,cf_gc,qv_gc,qr_gc,qc_gc,qs_gc,qi_gc,qg_gc,qh_gc,qni_gc,qnc_gc,icefrac_gc,prho_gc,pdrho_gc,qnr_gc,qns_gc,qng_gc,qnh_gc,soil_layers,soil_levels,st,sm,sw,soilt,soilm,sm000007,sm007028,sm028100,sm100255,st000007,st007028,st028100,st100255,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300,sw000010,sw010040,sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100,st100200,st010200,soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,qnwfa_gc,qnwfa_now,qnwfa_jan,qnwfa_feb,qnwfa_mar,qnwfa_apr,qnwfa_may,qnwfa_jun,qnwfa_jul,qnwfa_aug,qnwfa_sep,qnwfa_oct,qnwfa_nov,qnwfa_dec,qnifa_gc,qnifa_now,qnifa_jan,qnifa_feb,qnifa_mar,qnifa_apr,qnifa_may,qnifa_jun,qnifa_jul,qnifa_aug,qnifa_sep,qnifa_oct,qnifa_nov,qnifa_dec,qntemp,qntemp2,hgtmaxw,pmaxw,tmaxw,umaxw,vmaxw,hgttrop,ptrop,ttrop,utrop,vtrop,urb_param

package   tconly         use_wps_input==2            -             state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,qv_gc,qr_gc,qc_gc,qs_gc,qi_gc,qg_gc,qh_gc,qni_gc,qnc_gc,qnr_gc,qns_gc,qng_gc,qnh_gc,icefrac_gc

# Distributed AHE inclusion
state    real   AHE            ij       misc        1        -     rh01du=(copy_fcnm)  "AHE "   "anthropogenic heat emission"  "w m-2"
state    real   AME            ij       misc        1        -     rh01du=(copy_fcnm)  "AME "   "anthropogenic mositure emission"  "w m-2"
state    real   AHE0           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE0"   "anthropogenic heat emission"  "w m-2"
state    real   AME0           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME0"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE1           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE1"   "anthropogenic heat emission"  "w m-2"
state    real   AME1           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME1"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE2           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE2"   "anthropogenic heat emission"  "w m-2"
state    real   AME2           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME2"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE3           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE3"   "anthropogenic heat emission"  "w m-2"
state    real   AME3           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME3"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE4           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE4"   "anthropogenic heat emission"  "w m-2"
state    real   AME4           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME4"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE5           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE5"   "anthropogenic heat emission"  "w m-2"
state    real   AME5           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME5"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE6           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE6"   "anthropogenic heat emission"  "w m-2"
state    real   AME6           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME6"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE7           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE7"   "anthropogenic heat emission"  "w m-2"
state    real   AME7           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME7"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE8           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE8"   "anthropogenic heat emission"  "w m-2"
state    real   AME8           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME8"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE9           ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE9"   "anthropogenic heat emission"  "w m-2"
state    real   AME9           ij       misc        1        -     i01r01du=(copy_fcnm)  "AME9"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE10          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE10"   "anthropogenic heat emission"  "w m-2"
state    real   AME10          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME10"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE11          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE11"   "anthropogenic heat emission"  "w m-2"
state    real   AME11          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME11"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE12          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE12"   "anthropogenic heat emission"  "w m-2"
state    real   AME12          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME12"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE13          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE13"   "anthropogenic heat emission"  "w m-2"
state    real   AME13          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME13"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE14          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE14"   "anthropogenic heat emission"  "w m-2"
state    real   AME14          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME14"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE15          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE15"   "anthropogenic heat emission"  "w m-2"
state    real   AME15          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME15"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE16          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE16"   "anthropogenic heat emission"  "w m-2"
state    real   AME16          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME16"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE17          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE17"   "anthropogenic heat emission"  "w m-2"
state    real   AME17          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME17"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE18          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE18"   "anthropogenic heat emission"  "w m-2"
state    real   AME18          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME18"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE19          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE19"   "anthropogenic heat emission"  "w m-2"
state    real   AME19          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME19"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE20          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE20"   "anthropogenic heat emission"  "w m-2"
state    real   AME20          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME20"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE21          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE21"   "anthropogenic heat emission"  "w m-2"
state    real   AME21          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME21"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE22          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE22"   "anthropogenic heat emission"  "w m-2"
state    real   AME22          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME22"   "anthropogenic mositure emission"  "w m-2"
state    real   AHE23          ij       misc        1        -     i01r01du=(copy_fcnm)  "AHE23"   "anthropogenic heat emission"  "w m-2"
state    real   AME23          ij       misc        1        -     i01r01du=(copy_fcnm)  "AME23"   "anthropogenic mositure emission"  "w m-2"
