#  Registry file, EM

#INCLUDES
include registry.dimspec
include Registry.EM_COMMON
include registry.io_boilerplate

include registry.fire
include registry.avgflx
include registry.stoch
include registry.les
include registry.cam
include registry.clm
include registry.ssib
include registry.diags
include registry.bdy_perturb

# added to output 5 for ESMF
state   real   landmask            ij    misc          1     -     i0125rh05d=(interp_fcnm_imask)u=(copy_fcnm)   "LANDMASK"      "LAND MASK (1 FOR LAND, 0 FOR WATER)"  ""

# Masked SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c_mask:lu_index,tslb,num_soil_layers,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"
# Simple SST interpolation from the CG
#state    real   SST              ij    misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)f=(p2c)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state    real   SST              ij     misc        1         -     i01245rh05d=(interp_mask_field:lu_index,iswater)   "SST"              "SEA SURFACE TEMPERATURE" "K"


# Registry entries that are exclusive to Registry.EM

# Chem Scalars
rconfig   integer     chem_opt            namelist,physics	max_domains    0       rh       "chem_opt"              ""      ""
state   real    -              ikjftb   chem        1         -     -    -                                       
#
# Tracer Scalars: If you want full resolved and non-resolved dispersion, compile WRF-Chem
#
state   real    -          ikjftb  tracer        1         -     -    -                                       
state   real    tr17_1     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_1"         "tr17_1"          -
state   real    tr17_2     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_2"         "tr17_2"          -
state   real    tr17_3     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_3"         "tr17_3"          -
state   real    tr17_4     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_4"         "tr17_4"          -
state   real    tr17_5     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_5"         "tr17_5"          -
state   real    tr17_6     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_6"         "tr17_6"          -
state   real    tr17_7     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_7"         "tr17_7"          -
state   real    tr17_8     ikjftb  tracer        1         -     irhusdf=(bdy_interp:dt)    "tr17_8"         "tr17_8"

package   tracer_test1  tracer_opt==2       -             tracer:tr17_1,tr17_2,tr17_3,tr17_4,tr17_5,tr17_6,tr17_7,tr17_8


 
