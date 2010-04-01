#  Registry file, EM
#                                               
# At the present time this file is managed manually and edited by hand.                                         
#                                               
################################################################################
# Dimension specifications
#
# This section of the Registry file is used to specify the dimensions
# that will be used to define arrays. Dim is the one-letter name of the
# dimension.  How defined can either be "standard_domain", which means
# that the dimension (1) is one of the three spatial dimensions and (2)
# it will be set using the standard namelist mechanism and domain data
# structure dimension fields (e.g. sd31,ed31,sd32...).
#
# Order refers to which of the three sets of just-mentioned internal
# dimension variables the dimension is referred to by in the driver.
# That is, is it the first, second, or third dimension.  The registry
# infers the mapping of its internal dimensions according to the
# combination of Order and Coord-axis that are specified in this table.
# Note that it is all right to more than one dimension name for, say, the
# x dimension.  However, the Order and Coord-axis relationship must be
# consistent throughout.
# 
# Note: these entries do not enforce storage order on a particular field.
# That is determined by the dimension strings for each field. But it does
# relate the dimspec to the internal data structures that the driver uses
# to maintain the three physical domain dimensions.
# 
# "How defined" can also specify the name of a namelist variable from which
# the definition for the dimension will come; this is specified as
# "namelist=<variable name>".  The namelist variable must have been
# defined as an integer and with only one entry in the rconfig table. Or
# a constant can be specified.  The coordinate axis for the dimension is
# either X, Y, Z, or C (for "not a spatial dimension").  The Dimname is
# the descriptive name of the dimension that will be included in the
# metadata in data sets.  Note that the b, f, and t modifiers that appear
# as the last characters of dimension strings used # in state and # i1
# registry definitions are not dimensions and do not need to be declared
# here.
#

include registry.dimspec

################################################################################
################################################################################
################################################################################

#state    real   floob       ikjb     dyn_em      1         -    
#state    real   floob_x     ikjx     dyn_em      1         -     
#state    real   floob_y     ikjy     dyn_em      1         - 
#xpose FLOOB dyn_em floob,floob_x,floob_y

#state real xxx ijk misc 2 - h6ud
#halo  HALO_FLOOB dyn_em  4:xxx_2

# Lines that start with the word 'state' form a table that is                                           
# used by the script use_registry to generate module_state_descript.F                                           
# and other files.  Also see documentation in use_registry.                                             
#                                               
# table entries are of the form                                         
#<Table> <Type> <Sym>         <Dims>   <Use>   <NumTLev> <Stagger> <IO>     <DNAME>             <DESCRIP>     <UNITS>   
#

# It is required that LU_INDEX appears before any variable that is
# interpolated with a mask, as lu_index supplies that mask.
# this next 1 is for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real  LU_INDEX         ij      misc        1         -     i012rh01d=(interp_fcnm)u=(copy_fcnm)   "LU_INDEX"              "LAND USE CATEGORY"         ""
state    real  LU_MASK          ij      misc        1         -     i3h1     "LU_MASK"              "0 land 1 water"         ""

# znw, znu, dzs, and zs must be listed before any 3-d fields
# in order for the grib output module to work correctly.  The grib output
# module retrieves the vertical levels from these parameters.  If znw, znu
# dzs, and zs are not listed first, vertical level will not be encoded at 
# time 0.

state    real    znu            k       dyn_em      1         -     irh       "znu"  "eta values on half (mass) levels"  ""
state    real    znw            k       dyn_em      1         Z     i0rh     "znw"  "eta values on full (w) levels"  ""
state    real   ZS              l        misc      -         Z     irh       "ZS"                    "DEPTHS OF CENTERS OF SOIL LAYERS"         "m"
state    real   DZS             l        misc      -         Z     irh       "DZS"                   "THICKNESSES OF SOIL LAYERS"               "m"

#
# Variables from WPS
#
state    real   u_gc           igj      dyn_em      1        XZ    i1  "UU"     "x-wind component"    "m s-1"
state    real   v_gc           igj      dyn_em      1        YZ    i1  "VV"     "y-wind component"    "m s-1"
state    real   t_gc           igj      dyn_em      1        Z     i1  "TT"     "temperature"         "K"
state    real   rh_gc          igj      dyn_em      1        Z     i1  "RH"    "relative humidity"   "%"
state    real   ght_gc         igj      dyn_em      1        Z     i1  "GHT"   "geopotential height" "m"
state    real   p_gc           igj      dyn_em      1        Z     i1  "PRES"   "pressure"            "Pa"
state    real   xlat_gc        ij       dyn_em      1        -     i1  "XLAT_M" "latitude, positive north" "degrees"
state    real   xlong_gc       ij       dyn_em      1        -     i1  "XLONG_M" "longitude, positive east" "degrees"
state    real   ht_gc          ij       dyn_em      1        -     i1  "HGT_M" "topography elevation" "m"
state    real   tsk_gc         ij       dyn_em      1        -     i1  "SKINTEMP"  "skin temperature"  "K"
state    real   tavgsfc        ij       dyn_em      1        -     i1  "TAVGSFC"  "daily mean of surface air temperature"  "K"
state    real   tmn_gc         ij       dyn_em      1        -     i1  "SOILTEMP"  "annual mean deep soil temperature"  "K"
state    real   pslv_gc        ij       dyn_em      1        -     i1  "PMSL"  "sea level pressure"  "Pa"
state    real   sct_dom_gc     ij       dyn_em      1        -     i1  "SCT_DOM"  "Dominant soil (top) category from GEOGRID"  "cat"
state    real   scb_dom_gc     ij       dyn_em      1        -     i1  "SCB_DOM"  "Dominant soil (bottom) category from GEOGRID"  "cat"
state    real   greenfrac      imj      dyn_em      1        Z     i1  "GREENFRAC" "monthly greenness fraction" "0 - 1 fraction"
state    real   albedo12m      imj      dyn_em      1        Z     i1  "ALBEDO12M" "background albedo" "0 - 1 fraction"
state    real   pd_gc          igj      dyn_em      1        Z     -   "PD"    "dry pressure"        "Pa"
state    real   psfc_gc        ij       dyn_em      1        -      -  "PSFC_GC"     "surface pressure"            "Pa"
state    real   intq_gc        ij       dyn_em      1        -     -   "INTQ"  "integrated mixing ratio" "Pa"
state    real   pdhs           ij       dyn_em      1        -     -   "PDHS"  "hydrostatic dry surface pressure" "Pa"
state    real   qv_gc          igj      dyn_em      1        Z     i1  "QV"     "mixing ratio"        "kg kg-1"
state    real   sh_gc          igj      dyn_em      1        Z     i1  "SPECHUMD"  "Specific humidity"        "kg kg-1"
ifdef RUC_CLOUD
state    real   qr_gc          igj      dyn_em      1        Z     i1  "QR"    "rain water mixing ratio"   "kg kg-1"
state    real   qc_gc          igj      dyn_em      1        Z     i1  "QC"    "cloud water mixing ratio"   "kg kg-1"
state    real   qs_gc          igj      dyn_em      1        Z     i1  "QS"    "snow mixing ratio"   "kg kg-1"
state    real   qi_gc          igj      dyn_em      1        Z     i1  "QI"    "cloud ice mixing ratio"   "kg kg-1"
state    real   qg_gc          igj      dyn_em      1        Z     i1  "QG"    "graupel mixing ratio"   "kg kg-1"
state    real   qh_gc          igj      dyn_em      1        Z     i1  "QH"    "hail mixing ratio"   "kg kg-1"
state    real   qni_gc         igj      dyn_em      1        Z     i1  "QNI"   "ice no concentration"   "m-3"
endif

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                               
# Variables for Eulerian mass coordinate dynamics                                            
#                                               


# Velocities
#
# U Vel
state    real   u              ikjb     dyn_em      2         X     \
     i0rhusdf=(bdy_interp:dt)       "U"                      "x-wind component"   "m s-1"
state    real   ru             ikj     dyn_em      1         X      -        "MU_U"        "mu-coupled u"   "Pa m s-1"
state    real   ru_m           ikj     dyn_em      1         X      -        "ru_m"        ""   ""
state    real   ru_tend        ikj     dyn_em      1         X      -        "ru_tend"        ""   ""                                   
i1       real   ru_tendf       ikj     dyn_em      1         X                                          
state    real   u_save         ikj     dyn_em      1         X      -        "u_save"
state    real   z_force        |       dyn_em      1         -      i3rh     "Z_FORCE" "height of forcing input" "m"
state    real   z_force_tend   |       dyn_em      1         -      i3rh     "Z_FORCE_TEND" "tendency height of forcing input" "m"
state    real   u_g            |       dyn_em      1         -      i3rh     "U_G" "x-direction geostrophic wind" "m s-1"
state    real   u_g_tend       |       dyn_em      1         -      i3rh     "U_G_TEND" "tendency x-direction geostrophic wind" "m s-1"
#                                               
# V Vel
state    real   v              ikjb     dyn_em      2         Y     \
     i0rhusdf=(bdy_interp:dt)        "V"                     "y-wind component"   "m s-1"
state    real   rv             ikj     dyn_em      1         Y      -        "MU_V"        "mu-coupled v"   "Pa m s-1"
state    real   rv_m           ikj     dyn_em      1         Y      -        "rv_m"
state    real   rv_tend        ikj     dyn_em      1         Y      -        "rv_tend"
i1       real   rv_tendf       ikj     dyn_em      1         Y                                          
state    real   v_save         ikj     dyn_em      1         Y      -        "v_save"                   
state    real   v_g            |       dyn_em      1         -      i3rh     "V_G" "y-direction geostrophic wind" "m s-1"
state    real   v_g_tend       |       dyn_em      1         -      i3rh     "V_G_TEND" "tendency y-direction geostrophic wind" "m s-1"
#                                               
# Vertical Vel                                          
state    real   w              ikjb     dyn_em      2         Z     \
        irhusdf=(bdy_interp:dt)  "w"                          "z-wind component"   "m s-1"
state    real   ww             ikj     dyn_em      1         Z      r         "ww"   "mu-coupled eta-dot"    "Pa s-1"
state    real   rw             ikj     dyn_em      1         Z      -         "rw"   "mu-coupled w"          "Pa m s-1"
i1       real   ww1            ikj     dyn_em      1         Z                                          
state    real   ww_m           ikj     dyn_em      1         Z      r         "ww_m"   "time-avg mu-coupled eta-dot"    "Pa s-1"
i1       real   wwp            ikj     dyn_em      1         Z                                          
i1       real   rw_tend        ikj     dyn_em      1         Z                                          
i1       real   rw_tendf       ikj     dyn_em      1         Z                                          
i1       real   w_save         ikj     dyn_em      1         Z                                          

state    real   w_subs         |       dyn_em      1         -      i3rh      "W_SUBS" "large-scale vertical velocity" "m s-1"
state    real   w_subs_tend    |       dyn_em      1         -      i3rh      "W_SUBS_TEND" "tendency large-scale vertical velocity" "m s-1"

# Geopotential
state    real   ph             ikjb     dyn_em      2         Z     \
       irhusdf=(bdy_interp:dt)   "ph"   "perturbation geopotential"  "m2 s-2"
state    real   phb            ikj     dyn_em      1         Z     irhdus "phb"  "base-state geopotential"  "m2 s-2"
state    real   phb_fine       ikj     dyn_em      1         Z      -     "phb_fine"  "for nesting, temp holding interpolated coarse grid phb"  "m2 s-2"
state    real   ph0            ikj     dyn_em      1         Z      r     "ph0"  "initial geopotential"     "m2 s-2"
state    real   php            ikj     dyn_em      1         -      r     "php"  "geopotential"             "m2 s-2"
i1       real   ph_tend        ikj     dyn_em      1         Z 
i1       real   ph_tendf       ikj     dyn_em      1         Z 
i1       real   ph_save        ikj     dyn_em      1         Z 

# Potential Temperature
state    real   t              ikjb     dyn_em      2         -     \
       i0rhusdf=(bdy_interp:dt)   "t"      "perturbation potential temperature (theta-t0)" "K"

state    real   t_init         ikj     dyn_em      1         -      ir       "t_init" "initial potential temperature" "K"
i1       real   t_tend         ikj     dyn_em      1         -  
i1       real   t_tendf        ikj     dyn_em      1         -  
state    real   tp             ikj     dyn_em      2         -  
i1       real   t_2save        ikj     dyn_em      1         -   
state    real   t_save         ikj     dyn_em      1         -               "t_save"

state    real   th_upstream_x       |     dyn_em      1      -      i3rh     "TH_UPSTREAM_X" "upstream theta x-advection" "K s-1"
state    real   th_upstream_x_tend  |     dyn_em      1      -      i3rh     "TH_UPSTREAM_X_TEND" "tendency upstream theta x-advection" "K s-2"
state    real   th_upstream_y       |     dyn_em      1      -      i3rh     "TH_UPSTREAM_Y" "upstream theta y-advection" "K s-1"
state    real   th_upstream_y_tend  |     dyn_em      1      -      i3rh     "TH_UPSTREAM_Y_TEND" "tendency upstream theta y-advection" "K s-2"

state    real   qv_upstream_x       |     dyn_em      1      -      i3rh     "QV_UPSTREAM_X" "upstream qv x-advection" "kg kg-1 s-1"
state    real   qv_upstream_x_tend  |     dyn_em      1      -      i3rh     "QV_UPSTREAM_X_TEND" "tendency upstream qv x-advection" "kg kg-1 s-2"
state    real   qv_upstream_y       |     dyn_em      1      -      i3rh     "QV_UPSTREAM_Y" "upstream qv y-advection" "kg kg-1 s-1"
state    real   qv_upstream_y_tend  |     dyn_em      1      -      i3rh     "QV_UPSTREAM_Y_TEND" "tendency upstream qv y-advection" "kg kg-1 s-2"

state    real   u_upstream_x        |     dyn_em      1      -      i3rh     "U_UPSTREAM_X" "upstream u x-advection" "m s-2"
state    real   u_upstream_x_tend   |     dyn_em      1      -      i3rh     "U_UPSTREAM_X_TEND" "tendency upstream u x-advection" "m s-3"
state    real   u_upstream_y        |     dyn_em      1      -      i3rh     "U_UPSTREAM_Y" "upstream u y-advection" "m s-2"
state    real   u_upstream_y_tend   |     dyn_em      1      -      i3rh     "U_UPSTREAM_Y_TEND" "tendency upstream u y-advection" "m s-3"

state    real   v_upstream_x        |     dyn_em      1      -      i3rh     "V_UPSTREAM_X" "upstream v x-advection" "m s-2"
state    real   v_upstream_x_tend   |     dyn_em      1      -      i3rh     "V_UPSTREAM_X_TEND" "tendency upstream v x-advection" "m s-3"
state    real   v_upstream_y        |     dyn_em      1      -      i3rh     "V_UPSTREAM_Y" "upstream v y-advection" "m s-2"
state    real   v_upstream_y_tend   |     dyn_em      1      -      i3rh     "V_UPSTREAM_Y_TEND" "tendency upstream v y-advection" "m s-3"

state    real   tau_x               |     dyn_em      1      -      i3rh     "TAU_X" "X-direction advective timescale" "s"
state    real   tau_x_tend          |     dyn_em      1      -      i3rh     "TAU_X_TEND" "tendency X-direction advective timescale" ""
state    real   tau_y               |     dyn_em      1      -      i3rh     "TAU_Y" "Y-direction advective timescale" "s"
state    real   tau_y_tend          |     dyn_em      1      -      i3rh     "TAU_Y_TEND" "tendency Y-direction advective timescale" ""

# Mass
state    real   mu              ijb     dyn_em      2         -     \
     irhusdf=(bdy_interp:dt)   "mu"  "perturbation dry air mass in column" "Pa"
state    real   mub             ij     dyn_em      1         -     irhdus       "mub" "base state dry air mass in column" "Pa"
state    real   mub_fine        ij     dyn_em      1         -      -           "mub_fine" "nest temp, holds interpolated coarse grid mub" "Pa"
state    real   mub_save        ij     dyn_em      1         -      -           "mub_save" "nest temp, holds orig fine grid mub" "Pa"
state    real   mu0             ij     dyn_em      1         -      i1          "mu0" "initial dry mass in column" "Pa"
state    real   mudf            ij     dyn_em      1         -      -           "mudf" "" ""
state    real   muu             ij     dyn_em      1          -                 "muu"
i1       real   muus            ij     dyn_em      1          -     
state    real   muv             ij     dyn_em      1          -                 "muv"
i1       real   muvs            ij     dyn_em      1          -     
state    real   mut             ij     dyn_em      1          -                 "mut"
state    real   muts            ij     dyn_em      1          -                 "muts"
i1       real   muave           ij     dyn_em      1          -     
i1       real   mu_save         ij     dyn_em      1          -     
i1       real   mu_tend         ij     dyn_em      1          -     
i1       real   mu_tendf        ij     dyn_em      1          -     

#diagnostic for looking at nest position in output. A mungy version of terrain height.
state    real   nest_pos        ij     misc        1   -   rhu=(mark_domain)  "NEST_POS"
state    real   nest_mask       ij     misc        1   -   ru=(mark_domain)   "NEST_MASK"     "LOCATION OF NEST IF ANY"
state    real   ht_coarse       ij     misc        1   -     r                -              "STORAGE FOR LOW-RES TERRAIN"


# TKE
state    real   tke            ikj     dyn_em      2         -       r        "tke"          "TURBULENCE KINETIC ENERGY"     "m2 s-2"
i1       real   tke_tend       ikj     dyn_em      1         -      

# Pressure and Density
state    real   p              ikj     dyn_em      1         -      irh       "p"           "perturbation pressure"         "Pa"
state    real   al             ikj     dyn_em      1         -      r         "al"          "inverse perturbation density"  "m3 kg-1"
state    real   alt            ikj     dyn_em      1         -      r         "alt"         "inverse density"               "m3 kg-1"
state    real   alb            ikj     dyn_em      1         -      rdus      "alb"         "inverse base density"          "m3 kg-1"
state    real   zx             ikj     dyn_em      1         X      -         " "  " "  " "    
state    real   zy             ikj     dyn_em      1         Y      -         " "  " "  " "   
state    real   rdz            ikj     dyn_em      1         Z      -         " "  " "  " "   
state    real   rdzw           ikj     dyn_em      1         Z       -        " "  " "  " "   
state    real   pb             ikj     dyn_em      1         -      irhdus    "pb"          "BASE STATE PRESSURE "          "Pa"

#                                               
# Other dyn                                             
#                                               
i1       real   advect_tend    ikj     dyn_em      1         -                                          
i1       real   alpha          ikj     dyn_em      1         -                                                  
i1       real   a              ikj     dyn_em      1         -                                                  
i1       real   gamma          ikj     dyn_em      1         -                                                  
i1       real   c2a            ikj     dyn_em      1         -     -
i1       real   rho            ikj     dyn_em      1         -     -
i1       real   phm            ikj     dyn_em      1         -     -
i1       real   cqu            ikj     dyn_em      1         -     -
i1       real   cqv            ikj     dyn_em      1         -     -
i1       real   cqw            ikj     dyn_em      1         -     -
i1       real   pm1            ikj     dyn_em      1         -     -
state    real    fnm            k       dyn_em      1         -     irh       "fnm"  "upper weight for vertical stretching"  ""
state    real    fnp            k       dyn_em      1         -     irh       "fnp"  "lower weight for vertical stretching"  ""
state    real    rdnw           k       dyn_em      1         -     irh       "rdnw"  "inverse d(eta) values between full (w) levels"   ""
state    real    rdn            k       dyn_em      1         -     irh       "rdn"  "inverse d(eta) values between half (mass) levels"   ""
state    real    dnw            k       dyn_em      1         -     irh       "dnw" "d(eta) values between full (w) levels"   ""
state    real    dn             k       dyn_em      1         -     irh       "dn " "d(eta) values between half (mass) levels"   ""
state    real    t_base         k       dyn_em      1         -     ir        "t_base"               "BASE STATE T IN IDEALIZED CASES"         "K"      
state    real    z              ikj     dyn_em      1         -     -         " " " " " "
state    real    z_at_w         ikj     dyn_em      1         Z 
state    real    cfn            -       misc      -         -     irh       "cfn"    "extrapolation constant"  ""
state    real    cfn1           -       misc      -         -     irh       "cfn1"   "extrapolation constant"  ""
state    integer step_number    -       misc      -         -     ir        "step_number"  ""

# For the adaptive timestep restart
state    logical stepping_to_time       - misc    -         -      r     "stepping_to_time"         ""
state    integer last_step_updated      - misc    -         -      r     "last_step_updated"        ""
state    logical adapt_step_using_child - misc    -         -      r     "adapt_step_using_child"   ""
state    integer last_dt_sec            - misc    -         -      r     "last_dt_sec"              "Whole seconds for last timestep"  "sec"
state    integer last_dt_sec_num        - misc    -         -      r     "last_dt_sec_num"          "Fractional secs, numerator"       "sec"
state    integer last_dt_sec_den        - misc    -         -      r     "last_dt_sec_den"          "Fractional secs, denominator"     "sec"
state    integer last_dt_yr             - misc    -         -      r     "last_dt_yr"               "Relative year"                    "years"
state    integer last_dt_mm             - misc    -         -      r     "last_dt_mm"               "Relative month"                   "months"

# hydrostatic pressure vars
state    real   p_hyd           ikj     dyn_em      1         -      irh       "p_hyd"       "hydrostatic pressure"         "Pa"
state    real   p_hyd_w         ikj     dyn_em      1         Z       r        "p_hyd_w"     "hydrostatic pressure at full levels"         "Pa"

# 2m and 10m output diagnostics
state    real   Q2               ij     misc        1         -     irhd      "Q2"                   "QV at 2 M"         "kg kg-1"
state    real   T2               ij     misc        1         -     irhd      "T2"                   "TEMP at 2 M"       "K"
state    real   TH2              ij     misc        1         -     irhd      "TH2"                  "POT TEMP at 2 M"   "K"
state    real   PSFC             ij     misc        1         -     i01rhd    "PSFC"                 "SFC PRESSURE"      "Pa"

# these next 2 are for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real   U10              ij     misc        1         -     irh01d      "U10"                "U at 10 M"         "m s-1"
state    real   V10              ij     misc        1         -     irh01d      "V10"                "V at 10 M"         "m s-1"

# these next 4 are for observational nudging
state    real   uratx           ij      misc        1         -      r          "URATX"            "Ratio of U over U10 on mass points "         "dimensionless"
state    real   vratx           ij      misc        1         -      r          "VRATX"            "Ratio of V over V10 on mass points "         "dimensionless"   
state    real   tratx           ij      misc        1         -      r          "TRATX"            "Ratio of T over TH2 on mass points "         "dimensionless"   
state    real   obs_savwt      hikj     dyn_em      1         X      -       "OBS_SAVWT"

# Other
state   real    rdx            -        misc      -         -     irh       "rdx"                   "INVERSE X GRID LENGTH"         ""      
state   real    rdy            -        misc      -         -     irh       "rdy"                   "INVERSE Y GRID LENGTH"         ""      
state   real    dts            -        misc      -         -     ir        "dts"                   "SMALL TIMESTEP"         ""      
state   real    dtseps         -        misc      -         -     ir        "dtseps"                "TIME WEIGHT CONSTANT FOR SMALL STEPS"         ""      
state   real    resm           -        misc      -         -     irh       "resm"                  "TIME WEIGHT CONSTANT FOR SMALL STEPS"         ""      
state   real    zetatop        -        misc      -         -     irh       "zetatop"               "ZETA AT MODEL TOP"         ""      
state   real    cf1            -        misc      -         -     irh       "cf1"                   "2nd order extrapolation constant"         ""      
state   real    cf2            -        misc      -         -     irh       "cf2"                   "2nd order extrapolation constant"         ""      
state   real    cf3            -        misc      -         -     irh       "cf3"                   "2nd order extrapolation constant"         ""      
state   integer number_at_same_level    -        -          -         -     -        "number_at_same_level"  ""         ""      

# State for derived time quantities.  
state   integer itimestep      -        -          -         -     rh         "itimestep"             ""         ""      
state   real    xtime          -        -          -         -     rh         "xtime"                 "minutes since simulation start"         ""      
state   real    julian         -        -          -         -     -          "julian"                "day of year, 0.0 at 0Z on 1 Jan."         "days"      


# input file descriptor for lbcs on parent domain                                               
state   integer lbc_fid        -        -          -         -     -         "lbc_fid"               ""         ""      
# indicates if tiling has been computed                                         
state   logical tiled          -        -          -         -     -         "tiled"                 ""         ""      
# indicates if patches have been computed                                               
state   logical patched        -        -          -         -     -         "patched"               ""         ""      
# indicates whether to read input from file or generate                                         
#state   logical input_from_file        -        -          -         -     -         "input_from_file"         ""         ""    
# indicates whether to recompute mu                                                             
state   logical press_adj      -        -          -         -     -         "press_adj"         "T/F flag adjust mu"         ""    

# Mask for moving nest interpolations
state    integer imask_nostag         ij      misc     1     -
state    integer imask_xstag          ij      misc     1     X
state    integer imask_ystag          ij      misc     1     Y
state    integer imask_xystag         ij      misc     1     XY 
# vortex center indices; need for restarts of moving nests
state    real    xi                   -       misc     -     -    r
state    real    xj                   -       misc     -     -    r
state    real    vc_i                 -       misc     -     -    r
state    real    vc_j                 -       misc     -     -    r

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scalar (4D) arrays

# Moist Scalars
#                                               
# The first line ensures that there will be identifiers named moist and                                         
# moist_tend even if there are not any moist scalars (so the essentially                                                
# dry code will will still link properly)                                               
#                                               
state   real    -              ikjftb   moist       1         -     -    -
state   real    qv             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QVAPOR"           "Water vapor mixing ratio"      "kg kg-1"
state   real    qc             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QCLOUD"           "Cloud water mixing ratio"      "kg kg-1"
state   real    qr             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QRAIN"            "Rain water mixing ratio"       "kg kg-1"
state   real    qi             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QICE"             "Ice mixing ratio"              "kg kg-1"
state   real    qs             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QSNOW"            "Snow mixing ratio"             "kg kg-1"
state   real    qg             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QGRAUP"           "Graupel mixing ratio"          "kg kg-1"
state   real    qh             ikjftb   moist       1         -     \
   i0rhusdf=(bdy_interp:dt)  "QHAIL"            "Hail mixing ratio"             "kg kg-1"
state   real    -              ikjftb   dfi_moist       1         -     -    -
state   real    dfi_qv         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QVAPOR"       "Water vapor mixing ratio"      "kg kg-1"
state   real    dfi_qc         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QCLOUD"       "Cloud water mixing ratio"      "kg kg-1"
state   real    dfi_qr         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QRAIN"        "Rain water mixing ratio"       "kg kg-1"
state   real    dfi_qi         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QICE"         "Ice mixing ratio"              "kg kg-1"
state   real    dfi_qs         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QSNOW"        "Snow mixing ratio"             "kg kg-1"
state   real    dfi_qg         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QGRAUP"       "Graupel mixing ratio"          "kg kg-1"
state   real    dfi_qh         ikjftb   dfi_moist       1         -     \
   rusdf=(bdy_interp:dt)  "DFI_QHAIL"        "Hail mixing ratio"             "kg kg-1"


# LES---------------!JDM

include registry.les

#-------------------


# Chem Scalars
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

# Other Scalars
state   real    -              ikjftb  scalar      1         -     -   -
state   real    qndrop         ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNDROP"        "Droplet number mixing ratio"        "# kg-1"
state   real    qni            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNICE"         "Ice Number concentration" "# kg-1"
state   real    qt             ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "CWM"           "Total condensate mixing ratio"      "kg kg-1"
state   real    qns            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNSNOW"         "Snow Number concentration"   "# kg(-1)"
state   real    qnr            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNRAIN"        "Rain Number concentration"   "# kg(-1)"
state   real    qng            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNGRAUPEL"     "Graupel Number concentration" "# kg(-1)"
state   real    qnh            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNHAIL"        "Hail Number concentration" "# kg(-1)"

#state   real    qzr            ikjftb  scalar      1         -     \
#   i0rhusdf=(bdy_interp:dt)    "QZRAIN"        "Rain reflectivity" "# m(6) kg(-1)"
#state   real    qzi            ikjftb  scalar      1         -     \
#   i0rhusdf=(bdy_interp:dt)    "QZICE"        "Ice reflectivity" "# m(6) kg(-1)"
#state   real    qzs            ikjftb  scalar      1         -     \
#   i0rhusdf=(bdy_interp:dt)    "QZSNOW"        "Snow reflectivity" "# m(6) kg(-1)"
#state   real    qzg            ikjftb  scalar      1         -     \
#   i0rhusdf=(bdy_interp:dt)    "QZGRPL"        "Graupel reflectivity" "# m(6) kg(-1)"
#state   real    qzh            ikjftb  scalar      1         -     \
#   i0rhusdf=(bdy_interp:dt)    "QZHAIL"        "Hail reflectivity" "# m(6) kg(-1)"

state   real    qnn            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNCCN"         "CCN Number concentration" "# kg(-1)"
state   real    qnc            ikjftb  scalar      1         -     \
   i0rhusdf=(bdy_interp:dt)    "QNCLOUD"       "cloud water Number concentration" "# kg(-1)"
state   real    -              ikjftb  dfi_scalar      1         -     -   -
state   real    dfi_qndrop     ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNDROP"    "Droplet number mixing ratio"        "# kg-1"
state   real    dfi_qni        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNICE"     "Ice Number concentration" "# kg-1"
state   real    dfi_qt         ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_CWM"       "Total condensate mixing ratio"      "kg kg-1"
state   real    dfi_qns        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNSNOW"    "Snow Number concentration"   "# kg(-1)"
state   real    dfi_qnr        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNRAIN"    "Rain Number concentration"   "# kg(-1)"
state   real    dfi_qng        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNGRAUPEL" "Graupel Number concentration" "# kg(-1)"
state   real    dfi_qnh        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNHAIL" "Hail Number concentration" "# kg(-1)"

#state   real    dfi_qzr        ikjftb  dfi_scalar      1         -     \
#   rusdf=(bdy_interp:dt)    "DFI_QZRAIN" "Rain reflectivity" "m(6) kg(-1)"
#state   real    dfi_qzi        ikjftb  dfi_scalar      1         -     \
#   rusdf=(bdy_interp:dt)    "DFI_QZICE" "Ice reflectivity" "m(6) kg(-1)"
#state   real    dfi_qzs        ikjftb  dfi_scalar      1         -     \
#   rusdf=(bdy_interp:dt)    "DFI_QZSNOW" "Snow reflectivity" "m(6) kg(-1)"
#state   real    dfi_qzg        ikjftb  dfi_scalar      1         -     \
#   rusdf=(bdy_interp:dt)    "DFI_QZGRPL" "Graupel reflectivity" "m(6) kg(-1)"
#state   real    dfi_qzh        ikjftb  dfi_scalar      1         -     \
#   rusdf=(bdy_interp:dt)    "DFI_QZHAIL" "Hail reflectivity" "m(6) kg(-1)"

state   real    dfi_qnn        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNCC"      "CNN Number concentration"   "# kg(-1)"
state   real    dfi_qnc        ikjftb  dfi_scalar      1         -     \
   rusdf=(bdy_interp:dt)    "DFI_QNCLOUD"   "Cloud Number concentration" "# kg(-1)"

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Arrays for Specified LBCs  (lbc arrays REMOVED; Boundary arrays are now specified with the state array; see above, 20050413 JM )

state    real   fcx            w         misc     -         -      ir       "fcx"                  "RELAXATION TERM FOR BOUNDARY ZONE"         ""
state    real   gcx            w         misc     -         -      ir       "gcx"                  "2ND RELAXATION TERM FOR BOUNDARY ZONE"         ""
state    real   dtbc            -        misc     -         -      ir       "dtbc"                 "TIME SINCE BOUNDARY READ"         ""

#-------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------
# Physics Related State Varibles

#-------------------------------------------------------------------------------------------------------------------------------------------
# SI - start variables from netCDF format from Standard Initialization, most eventually for use in LSM schemes
#-------------------------------------------------------------------------------------------------------------------------------------------

state   real   soil_layers    i{lin}j    misc          1     Z     i1      "SOIL_LAYERS"   "SOIL LAYERS"         "cm"
state   real   soil_levels    i{lin}j    misc          1     Z     i1      "SOIL_LEVELS"   "SOIL LEVELS"         "cm"
state   real   st             i{lin}j    misc          1     Z     i1      "ST"            "SOIL TEMPERATURES"   "K"
state   real   sm             i{lin}j    misc          1     Z     i1      "SM"            "SOIL MOISTURES"      "m3 m-3"
state   real   sw             i{lin}j    misc          1     Z     i1      "SW"            "SOIL LIQUIDS"        "m3 m-3"
state   real   soilt          i{lin}j    misc          1     Z     i1      "SOILT"         "RUC SOIL TEMPERATURES"  "K"
state   real   soilm          i{lin}j    misc          1     Z     i1      "SOILM"         "RUC SOIL MOISTURES"  "m3 m-3"
state   real   sm000007            ij    misc          1     -     i1      "SM000007"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm007028            ij    misc          1     -     i1      "SM007028"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm028100            ij    misc          1     -     i1      "SM028100"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm100255            ij    misc          1     -     i1      "SM100255"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   st000007            ij    misc          1     -     i1      "ST000007"      "LAYER SOIL TEMPERATURE" "K"
state   real   st007028            ij    misc          1     -     i1      "ST007028"      "LAYER SOIL TEMPERATURE" "K"
state   real   st028100            ij    misc          1     -     i1      "ST028100"      "LAYER SOIL TEMPERATURE" "K"
state   real   st100255            ij    misc          1     -     i1      "ST100255"      "LAYER SOIL TEMPERATURE" "K"
state   real   sm000010            ij    misc          1     -     i1      "SM000010"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm010040            ij    misc          1     -     i1      "SM010040 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm040100            ij    misc          1     -     i1      "SM040100 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm100200            ij    misc          1     -     i1      "SM100200 "     "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sm010200            ij    misc          1     -     i1      "SM010200"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm000            ij    misc          1     -     i1      "SOILM000"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm005            ij    misc          1     -     i1      "SOILM005"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm020            ij    misc          1     -     i1      "SOILM020"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm040            ij    misc          1     -     i1      "SOILM040"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm160            ij    misc          1     -     i1      "SOILM160"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   soilm300            ij    misc          1     -     i1      "SOILM300"      "LAYER SOIL MOISTURE" "m3 m-3"
state   real   sw000010            ij    misc          1     -     i1      "SW000010"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw010040            ij    misc          1     -     i1      "SW010040"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw040100            ij    misc          1     -     i1      "SW040100"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw100200            ij    misc          1     -     i1      "SW100200"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   sw010200            ij    misc          1     -     i1      "SW010200"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw000            ij    misc          1     -     i1      "SOILW000"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw005            ij    misc          1     -     i1      "SOILW005"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw020            ij    misc          1     -     i1      "SOILW020"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw040            ij    misc          1     -     i1      "SOILW040"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw160            ij    misc          1     -     i1      "SOILW160"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   soilw300            ij    misc          1     -     i1      "SOILW300"      "LAYER SOIL LIQUID" "m3 m-3"
state   real   st000010            ij    misc          1     -     i1      "ST000010"      "LAYER SOIL TEMPERATURE" "K"
state   real   st010040            ij    misc          1     -     i1      "ST010040"      "LAYER SOIL TEMPERATURE" "K"
state   real   st040100            ij    misc          1     -     i1      "ST040100"      "LAYER SOIL TEMPERATURE" "K"
state   real   st100200            ij    misc          1     -     i1      "ST100200"      "LAYER SOIL TEMPERATURE" "K"
state   real   st010200            ij    misc          1     -     i1      "ST010200"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt000            ij    misc          1     -     i1      "SOILT000"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt005            ij    misc          1     -     i1      "SOILT005"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt020            ij    misc          1     -     i1      "SOILT020"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt040            ij    misc          1     -     i1      "SOILT040"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt160            ij    misc          1     -     i1      "SOILT160"      "LAYER SOIL TEMPERATURE" "K"
state   real   soilt300            ij    misc          1     -     i1      "SOILT300"      "LAYER SOIL TEMPERATURE" "K"
state   real   landmask            ij    misc          1     -     i012rhd=(interp_fcnm)u=(copy_fcnm)   "LANDMASK"      "LAND MASK (1 FOR LAND, 0 FOR WATER)"  ""
state   real   topostdv            ij    misc          1     -     i12     "TOPOSTDV"      "ELEVATION STD DEV"  "m"
state   real   toposlpx            ij    misc          1     -     i012rdu "TOPOSLPX"      "ELEVATION X SLOPE"  ""
state   real   toposlpy            ij    misc          1     -     i012rdu "TOPOSLPY"      "ELEVATION Y SLOPE"  ""
state   real   slope               ij    misc          1     -     rdu     "SLOPE"         "ELEVATION SLOPE"  ""
state   real   slp_azi             ij    misc          1     -     rdu     "SLP_AZI"       "ELEVATION SLOPE AZIMUTH"  "rad"
state   real   shdmax              ij    misc          1     -     i012rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)   "SHDMAX"        "ANNUAL MAX VEG FRACTION" ""
state   real   shdmin              ij    misc          1     -     i012rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)   "SHDMIN"        "ANNUAL MIN VEG FRACTION" ""
state   real   snoalb              ij    misc          1     -     i012r   "SNOALB"        "ANNUAL MAX SNOW ALBEDO IN FRACTION" ""
state   real   slopecat            ij    misc          1     -     i12     "SLOPECAT"      "SLOPE CATEGORY"  ""
state   real   toposoil            ij    misc          1     -     i12     "SOILHGT"       "ELEVATION OF LSM DATA"  "m"
state   real   landusef            iuj   misc          1     Z     i012r   "LANDUSEF"      "LANDUSE FRACTION BY CATEGORY"  ""
state   real   soilctop            isj   misc          1     Z     i012r   "SOILCTOP"      "SOIL CAT FRACTION (TOP)"  ""
state   real   soilcbot            isj   misc          1     Z     i012r   "SOILCBOT"      "SOIL CAT FRACTION (BOTTOM)"  ""
state   real   soilcat             ij    misc          1     -     i12     "SOILCAT"       "SOIL CAT DOMINANT TYPE" ""
state   real   vegcat              ij    misc          1     -     i12     "VEGCAT"        "VEGETATION CAT DOMINANT TYPE" ""
#---------------------------------------------------------------------------------------------------------------------------------------
# SI - end variables from netCDF format from Standard Initialization
#---------------------------------------------------------------------------------------------------------------------------------------

# soil model variables  (Note that they are marked as staggered in the vertical dimension
# because they are "fully dimensioned" -- they use every element in that dim
state    real   TSLB           ilj       misc      1         Z     i02rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)           "TSLB"     "SOIL TEMPERATURE"   "K"

# Time series variables
state    real   ts_hour         ?!       misc      -         -      -        "TS_HOUR"        "Model integration time, hours"
state    real   ts_u            ?!       misc      -         -      -        "TS_U"           "Surface wind U-component, earth-relative"
state    real   ts_v            ?!       misc      -         -      -        "TS_V"           "Surface wind V-component, earth-relative"
state    real   ts_q            ?!       misc      -         -      -        "TS_Q"           "Surface mixing ratio"
state    real   ts_t            ?!       misc      -         -      -        "TS_T"           "Surface temperature"
state    real   ts_psfc         ?!       misc      -         -      -        "TS_PSFC"        "Surface pressure"
state    real   ts_glw          ?!       misc      -         -      -        "TS_GLW"         "Downward long wave flux at surface"
state    real   ts_gsw          ?!       misc      -         -      -        "TS_GSW"         "Net short wave flux at surface"
state    real   ts_hfx          ?!       misc      -         -      -        "TS_HFX"         "Upward heat flux at surface"
state    real   ts_lh           ?!       misc      -         -      -        "TS_LH"          "Upward moisture flux at surface"
state    real   ts_tsk          ?!       misc      -         -      -        "TS_TSK"         "Skin temperature"
state    real   ts_tslb         ?!       misc      -         -      -        "TS_TSLB"        "Soil temperature"
state    real   ts_clw          ?!       misc      -         -      -        "TS_CLW"         "Column integrated cloud water"
state    real   ts_rainc        ?!       misc      -         -      -        "TS_RAINC"       "Cumulus precip"
state    real   ts_rainnc       ?!       misc      -         -      -        "TS_RAINNC"      "Grid-scale precip"

# urban model variables
state    real   DZR             l        em      -            Z     r        "DZR"            "THICKNESSES OF ROOF LAYERS"                      "m"
state    real   DZB             l        em      -            Z     r        "DZB"            "THICKNESSES OF WALL LAYERS"                      "m"
state    real   DZG             l        em      -            Z     r        "DZG"            "THICKNESSES OF ROAD LAYERS"                      "m"

# lsm State Variables

state    real   SMOIS            ilj     -          1         Z     i02rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SMOIS"            "SOIL MOISTURE"     "m3 m-3"
state    real   SH2O             ilj     -          1         Z     i02rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SH2O"             "SOIL LIQUID WATER" "m3 m-3"
state    real   SMCREL           ilj     -          1         Z     i02rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SMCREL"           "RELATIVE SOIL MOISTURE" ""
state    real   XICE             ij     misc        1         -     i0124rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)  "SEAICE"             "SEA ICE FLAG"  ""
state    real   XICEM            ij     misc        1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)  "XICEM"             "SEA ICE FLAG (PREVIOUS STEP)"  ""
state    real   SMSTAV           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)       "SMSTAV"           "MOISTURE AVAILABILITY" ""
state    real   SMSTOT           ij     misc        1         -      r                                          "SMSTOT"           "TOTAL SOIL MOISTURE" "m3 m-3"

state    real   SFCRUNOFF        ij     misc        1         -      rhd=(interp_mask_land_field:lu_index)      "SFROFF"           "SURFACE RUNOFF"     "mm"
state    real   UDRUNOFF         ij     misc        1         -      rhd=(interp_mask_land_field:lu_index)      "UDROFF"           "UNDERGROUND RUNOFF" "mm"
state  integer  IVGTYP           ij     misc        1         -     i02rhd=(interp_fcni)u=(copy_fcni)            "IVGTYP"           "DOMINANT VEGETATION CATEGORY" ""
state  integer  ISLTYP           ij     misc        1         -     i02rhd=(interp_fcni)u=(copy_fcni)            "ISLTYP"           "DOMINANT SOIL CATEGORY"       ""
state    real   VEGFRA           ij     misc        1         -     i024rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)   "VEGFRA"           "VEGETATION FRACTION" ""
state    real   SFCEVP           ij     misc        1         -      r                                          "SFCEVP"           "SURFACE EVAPORATION" "kg m-2"
state    real   GRDFLX           ij     misc        1         -      rh                                         "GRDFLX"           "GROUND HEAT FLUX" "W m-2"
state    real   ACGRDFLX         ij     misc        1         -      rhdu                                       "ACGRDFLX"         "ACCUMULATED GROUND HEAT FLUX" "J m-2"
state    real   SFCEXC           ij     misc        1         -      r                                          "SFCEXC "          "SURFACE EXCHANGE COEFFICIENT"   "m s-1"

state    real   ACSNOW           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "ACSNOW"           "ACCUMULATED SNOW"         "kg m-2"
state    real   ACSNOM           ij     misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "ACSNOM"           "ACCUMULATED MELTED SNOW"  "kg m-2"
state    real   SNOW             ij     misc        1         -     i012rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SNOW"             "SNOW WATER EQUIVALENT"    "kg m-2"
state    real   SNOWH            ij     misc        1         -     i012rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "SNOWH"            "PHYSICAL SNOW DEPTH"      "m"
#state    real   RHOSN            ij     misc        1         -     i02rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "RHOSN"            " SNOW DENSITY"      "kg m-3" 
state    real   CANWAT           ij     misc        1         -     i012rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)    "CANWAT"           "CANOPY WATER"             "kg m-2"
state    real   SST              ij     misc        1         -     i0124rhd=(interp_mask_water_field:lu_index)   "SST"              "SEA SURFACE TEMPERATURE" "K"
state  integer  IFNDSNOWH        -      misc        1         -     i         "FNDSNOWH" "SNOWH_LOGICAL"
state  integer  IFNDSOILW        -      misc        1         -     i         "FNDSOILW" "SOILW_LOGICAL"
# SKIN SST
state    real   SSTSK            ij     misc        1         -     rhd=(interp_mask_water_field:lu_index)   "SSTSK"              "SKIN SEA SURFACE TEMPERATURE" "K"
state    real   DTW              ij     misc        1         -     r   "DTW"              "WARM LAYER TEMP DIFF" "C"

# DFI variables
state   real   hcoeff         {ndfi} misc        1         -     -    "HCOEFF"               "initialization weights"
state   real   hcoeff_tot       -    misc        1         -     -    "HCOEFF_TOT"               "initialization weights"
state   real   dfi_p           ikj   misc        1         -     r    "P_DFI"           "perturbation pressure"         "Pa"
state   real   dfi_al          ikj   misc        1         -     r    "AL_DFI"          "inverse perturbation density"  "m3 kg-1"
state   real   dfi_mu          ij    misc        1         -     r    "MU_DFI"  "perturbation dry air mass in column" "Pa"
state   real   dfi_phb         ikj   misc        1         Z     r    "PHB_DFI"  "base-state geopotential"  "m2 s-2"
state   real   dfi_ph0         ikj   misc        1         Z     r    "PH0_DFI"  "initial geopotential"     "m2 s-2"
state   real   dfi_php         ikj   misc        1         Z     r    "PHP_DFI"  "geopotential"             "m2 s-2"
state   real   dfi_u           ikj   misc        1         -     r    "U_DFI"               "u accumulation array"          "   "
state   real   dfi_v           ikj   misc        1         -     r    "V_DFI"               "v accumulation array"          "   "
state   real   dfi_w           ikj   misc        1         -     r    "W_DFI"               "w accumulation array"          "   "
state   real   dfi_ww          ikj   misc        1         Z     r    "WW_DFI"              "mu-coupled eta-dot"    "Pa s-1"
state   real   dfi_t           ikj   misc        1         -     r    "TT_DFI"               "t accumulation array"          "   "
state   real   dfi_rh          ikj   misc        1         -     r    "RH_DFI"               "initial relative humidity"     "   "
state   real   dfi_ph          ikj   misc        1         -     r    "PH_DFI"               "p accumulation array"          "   "
state   real   dfi_pb          ikj   misc        1         -     r    "PB_DFI"               "pb accumulation array"          "   "
state   real   dfi_alt         ikj   misc        1         -     r    "ALT_DFI"             "1/rho accumulation array"          "   "
state   real   dfi_tke         ikj   misc        1         -     r    "TKE_DFI"          "TURBULENCE KINETIC ENERGY"     "m2 s-2"
state   real   dfi_tten_rad    ikj   misc        1         -     irh  "RAD_TTEN_DFI"     "RADAR POT. TEMP. TENDENCY"     "K s-1"
state    real  dfi_TSLB        ilj   misc        1         Z     r    "TSLB_dfi"         "SOIL TEMPERATURE"   "K"
state    real  dfi_SMOIS       ilj    -          1         Z     r    "SMOIS_dfi"        "SOIL MOISTURE"     "m3 m-3"
state    real  dfi_SNOW        ij    misc        1         -     r    "SNOW_dfi"         "SNOW WATER EQUIVALENT"    "kg m-2"
state    real  dfi_SNOWH       ij    misc        1         -     r    "SNOWH_dfi"        "PHYSICAL SNOW DEPTH"      "m"
state    real  dfi_CANWAT      ij    misc        1         -     r    "CANWAT_dfi"       "CANOPY WATER"             "kg m-2"
state    real  dfi_SMFR3D      ilj   misc        1         Z     r    "SMFR3D_dfi"           "SOIL ICE" ""
state    real  dfi_KEEPFR3DFLAG ilj  misc        1         Z     r    "KEEPFR3DFLAG_dfi"     "FLAG - 1. FROZEN SOIL YES, 0 - NO"             ""

# urban state variables
state    real   TR_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TR_URB"              "URBAN ROOF SKIN TEMPERATURE"        "K"
state    real   TB_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TB_URB"              "URBAN WALL SKIN TEMPERATURE"        "K" 
state    real   TG_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TG_URB"              "URBAN ROAD SKIN TEMPERATURE"        "K" 
state    real   TC_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TC_URB"              "URBAN CANOPY TEMPERATURE"           "K"
state    real   QC_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "QC_URB"              "URBAN CANOPY HUMIDITY"          "kg kg{-1}"
state    real   UC_URB2D         ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "UC_URB"              "URBAN CANOPY WIND"          "m s{-1}"
state    real   XXXR_URB2D       ij     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "XXXR_URB" "M-O LENGTH ABOVE URBAN ROOF"   "dimensionless"
state    real   XXXB_URB2D       ij    misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "XXXB_URB" "M-O LENGTH ABOVE URBAN WALL"   "dimensionless"
state    real   XXXG_URB2D       ij    misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "XXXG_URB" "M-O LENGTH ABOVE URBAN ROAD"   "dimensionless"
state    real   XXXC_URB2D       ij    misc        1         -      rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)       "XXXC_URB" "M-O LENGTH ABOVE URBAN CANOPY" "dimensionless"
state    real   TRL_URB3D        ilj    misc        1         Z     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "TRL_URB" "ROOF LAYER TEMPERATURE"          "K"
state    real   TBL_URB3D        ilj    misc        1         Z     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "TBL_URB" "WALL LAYER TEMPERATURE"          "K"
state    real   TGL_URB3D        ilj    misc        1         Z     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)      "TGL_URB" "ROAD LAYER TEMPERATURE"          "K"
state    real   SH_URB2D        ij    misc        1         -     r       "SH_URB"  "SENSIBLE HEAT FLUX FROM URBAN SFC"  "W m{-2}"
state    real   LH_URB2D        ij    misc        1         -     r       "LH_URB"  "LATENT HEAT FLUX FROM URBAN SFC"    "W m{-2}"
state    real   G_URB2D         ij    misc        1         -     r        "G_URB"  "GROUND HEAT FLUX INTO URBAN"        "W m{-2}"
state    real   RN_URB2D        ij    misc        1         -     r       "RN_URB"  "NET RADIATION ON URBAN SFC"         "W m{-2}"
state    real   TS_URB2D        ij    misc        1         -     r       "TS_URB"  "SKIN TEMPERATURE"          "K"
state    real   FRC_URB2D       ij    misc        1         -     rd=(interp_fcnm)u=(copy_fcnm)       "FRC_URB"  "URBAN FRACTION"         "dimensionless"
state    integer   UTYPE_URB2D  ij    misc        1         -     rd=(interp_fcnm)u=(copy_fcnm)       "UTYPE_URB"  "URBAN TYPE"         "dimensionless"
state    real   TRB_URB4D       i{ulay}j    misc       1         Z     r      "TRB_URB4D" "ROOF LAYER TEMPERATURE"          "K"
state    real   TW1_URB4D       i{ulay}j    misc       1         Z     r      "TW1_URB4D" "WALL LAYER TEMPERATURE"          "K"
state    real   TW2_URB4D       i{ulay}j    misc       1         Z     r      "TW2_URB4D" "WALL LAYER TEMPERATURE"          "K"
state    real   TGB_URB4D       i{ulay}j    misc       1         Z     r      "TGB_URB4D" "ROAD LAYER TEMPERATURE"          "K"
state    real   TLEV_URB3D      i{ulay}j    misc       1         Z     r      "TLEV_URB3D" "INDOOR TEMPERATURE"             "K"
state    real   QLEV_URB3D      i{ulay}j    misc       1         Z     r      "QLEV_URB3D" "SPECIFIC HUMIDITY"              "dimensionless"
state    real   TW1LEV_URB3D    i{ulay}j    misc       1         Z     r      "TW1LEV_URB3D" "WINDOW TEMPERATURE"           "K"
state    real   TW2LEV_URB3D    i{ulay}j    misc       1         Z     r      "TW2LEV_URB3D" "WINDOW TEMPERATURE"           "K"
state    real   TGLEV_URB3D     i{ulay}j    misc       1         Z     r      "TGLEV_URB3D" "GROUND TEMPERATURE BELOW A BUILDING"     "K"
state    real   TFLEV_URB3D     i{ulay}j    misc       1         Z     r      "TFLEV_URB3D" "FLOOR TEMPERATURE"                       "K"
state    real   SF_AC_URB3D     ij          misc       1         -     r      "SF_AC_URB3D"  "SENSIBLE HEAT FLUX FROM THE AIR COND." "W m{-2}"
state    real   LF_AC_URB3D     ij          misc       1         -     r      "LF_AC_URB3D"  "LATENT HEAT FLUX FROM THE AIR COND." "W m{-2}"
state    real   CM_AC_URB3D     ij          misc       1         -     r      "CM_AC_URB3D"  "CONSUMPTION OF THE AIR COND." "W m{-2}"
state    real   SFVENT_URB3D    ij          misc       1         -     r      "SFVENT_URB3D" "SENSIBLE HEAT FLUX FROM URBAN VENTILATION" "W m{-2}"
state    real   LFVENT_URB3D    ij          misc       1         -     r      "LFVENT_URB3D" "LATENT HEAT FLUX FROM URBAN VENTILATION" "W m{-2}"
state    real   SFWIN1_URB3D    i{ulay}j    misc       1         Z     r      "SFWIN1_URB3D" "SENSIBLE HEAT FLUX FROM URBAN SFC WINDOW"  "W m{-2}"
state    real   SFWIN2_URB3D    i{ulay}j    misc       1         Z     r      "SFWIN2_URB3D" "SENSIBLE HEAT FLUX FROM URBAN SFC WINDOW"  "W m{-2}"
state    real   SFW1_URB3D      i{ulay}j    misc       1         Z     r      "SFW1_URB3D"  "SENSIBLE HEAT FLUX FROM URBAN SFC"  "W m{-2}"
state    real   SFW2_URB3D      i{ulay}j    misc       1         Z     r      "SFW2_URB3D"  "SENSIBLE HEAT FLUX FROM URBAN SFC"  "W m{-2}"
state    real   SFR_URB3D       i{ulay}j    misc       1         Z     r      "SFR_URB3D"  "SENSIBLE HEAT FLUX FROM URBAN SFC"  "W m{-2}"
state    real   SFG_URB3D       i{ulay}j    misc       1         Z     r      "SFG_URB3D"  "SENSIBLE HEAT FLUX FROM URBAN SFC"  "W m{-2}"
state    real   CMR_SFCDIF      ij          misc       1         -     r      "CMR_SFCDIF" "" ""
state    real   CHR_SFCDIF      ij          misc       1         -     r      "CHR_SFCDIF" "" ""
state    real   CMC_SFCDIF      ij          misc       1         -     r      "CMC_SFCDIF" "" ""
state    real   CHC_SFCDIF      ij          misc       1         -     r      "CHC_SFCDIF" "" ""


# solar location variables from radiation driver
state    real   COSZEN           ij     misc        1         -      r       "COSZEN"  "COS of SOLAR ZENITH ANGLE"     "dimensionless"
state    real   HRANG            ij     misc        1         -      r       "HRANG"   "SOLAR HOUR ANGLE"          "radians"
state    real   DECLIN            -     misc        1         -      r       "DECLIN"  "SOLAR DECLINATION"         "radians"
state    real   SOLCON            -     misc        1         -      r       "SOLCON"  "SOLAR CONSTANT"         "W m-2"


# RUC LSM
state    real   SMFR3D           ilj    misc        1         Z      r        "SMFR3D"               "SOIL ICE" ""
state    real   KEEPFR3DFLAG     ilj    misc        1         Z      r        "KEEPFR3DFLAG"          "FLAG - 1. FROZEN SOIL YES, 0 - NO"             ""

# Additional for P-X PBL and LSM
state    real   RA               ij     misc        1         -      r        "RA"           "AERODYNAMIC RESISTANCE"   "s m-1"
state    real   RS               ij     misc        1         -      r        "RS"           "SURFACE RESISTANCE"       "s m-1"
state    real   LAI              ij     misc        1         -      i0124rh       "LAI"          "Leaf area index"          "area/area"
state    real   VEGF_PX          ij     misc        1         -      r        "VEGF_PX"      "Vegetation Fraction for PX LSM"    "area/area"
state    real   T2OBS            ij     misc        1         -      r        "T2OBS"        "2-m temperature from analysis "          "K"
state    real   Q2OBS            ij     misc        1         -      r        "Q2OBS"        "2-m mixing ratio from analysis "         "kg/kg"

# MRF PBL variables
i1      real   PSIM           ij     misc        1         -     -         "PSIM"                "SIMILARITY FUNCTION FOR MOMENTUM"     ""
i1      real   PSIH           ij     misc        1         -     -         "PSIH"                "SIMILARITY FUNCTION FOR HEAT"         ""
i1      real   WSPD           ij     misc        1         -     -         "WSPD"                "Wind speed"                           "m s-1"
i1      real   GZ1OZ0         ij     misc        1         -     -         "GZ1OZ0"              "LOG OF Z1 over Z0"                     ""
i1      real   BR             ij     misc        1         -     -         "BR"                  "Bulk Richardson"                       ""

# MYJ PBL variables
state    real   tke_myj        ikj     misc        1         -      rh       "tke_myj"               "TKE FROM MELLOR-YAMADA-JANJIC"      "m2 s-2"
state    real   EL_MYJ          ikj     misc        1         -     h        "el_myj"                "MIXING LENGTH FROM MELLOR-YAMADA-JANJIC"     "m"
state    real   EXCH_H          ikj     misc        1         -     r          "EXCH_H"               "EXCHANGE COEFFICIENTS "
state    real   EXCH_M          ikj     misc        1         -     r          "EXCH_M"               "EXCHANGE COEFFICIENTS "
state    real  CT              ij      misc        1         -      r        "CT"                    "COUNTERGRADIENT TERM"    "K"
state   real   THZ0             ij     misc        1         -      r        "THZ0"                  "POTENTIAL TEMPERATURE AT ZNT"                 "K"
state    real  Z0               ij     misc        1         -      r        "Z0"                    "Background ROUGHNESS LENGTH"                  "m"
state   real   QZ0              ij     misc        1         -      r        "QZ0"                   "SPECIFIC HUMIDITY AT ZNT"                     "kg kg-1"
state   real   UZ0              ij     misc        1         -      r        "UZ0"                   "U WIND COMPONENT AT ZNT"                      "m s-1"
state   real   VZ0              ij     misc        1         -      r        "VZ0"                   "V WIND COMPONENT AT ZNT"                      "m s-1"
state   real   QSFC             ij     misc        1         -      r        "QSFC"                  "SPECIFIC HUMIDITY AT LOWER BOUNDARY"          "kg kg-1"
state   real   AKHS             ij     misc        1         -      r        "AKHS"                  "SFC EXCH COEFF FOR HEAT"                      "m s-1"    
state   real   AKMS             ij     misc        1         -      r        "AKMS"                  "SFC EXCH COEFF FOR MOMENTUM"                  "m s-1"    
state   integer KPBL            ij     misc        1         -     r         "KPBL"                  "LEVEL OF PBL TOP"                             ""
state   real   TSHLTR           ij     misc        1         -      r        "TSHLTR"                "SHELTER THETA FROM MYJ"                       "K"
state   real   QSHLTR           ij     misc        1         -      r        "QSHLTR"                "SHELTER SPECIFIC HUMIDITY FROM MYJ"           "kg kg-1"
state   real   PSHLTR           ij     misc        1         -      r        "PSHLTR"                "SHELTER PRESSURE FROM MYJ"           "Pa"
state   real   TH10             ij     misc        1         -      r        "TH10"                  "10-M THETA FROM MYJ"                          "K"
state   real   Q10              ij     misc        1         -      r        "Q10"                   "10-M SPECIFIC HUMIDITY FROM MYJ"              "kg kg-1"
i1      real   CHKLOWQ          ij     misc        1         -     -         "CHKLOWQ"               "SURFACE SATURATION FLAG"        ""

# MYNN PBL variables
state   real   qke             ikj     misc        1         -      rh       "qke"               "twice TKE FROM MYNN"      "m2 s-2"
state   real   tsq             ikj     misc        1         -      r        "tsq"               "liquid water pottemp variance"      "K2"
state   real   qsq             ikj     misc        1         -      r        "qsq"               "liquid water variance"      "(kg/kg)**2"
state   real   cov             ikj     misc        1         -      r        "cov"               "liquid water-liquid water pottemp covaria\
nce"      "K kg/kg"
state   real   ch               ij     misc        1         -      -        "ch"                "drag coeff for heat"    ""
#state   real   K_m             ikj     misc        1         -     -          "K_m"               "EXCHANGE COEFFICIENT for momentum "
#state   real   K_h             ikj     misc        1         -     -          "K_h"               "EXCHANGE COEFFICIENT for heat "
#state   real   K_q             ikj     misc        1         -     -          "K_q"               "EXCHANGE COEFFICIENT for qke "

# Additional for gravity wave drag
state   real   DUSFCG           ij     misc        1         -     -         "DUSFCG"                "COLUMN U STRESS DIAGOSTIC"   ""   ""
state   real   DVSFCG           ij     misc        1         -     -         "DVSFCG"                "COLUMN V STRESS DIAGOSTIC"   ""   ""
state   real   VAR2D            ij     misc        1         -     i012rhdus "VAR"                   "OROGRAPHIC VARIANCE"   ""   ""
state   real   OC12D            ij     misc        1         -     i012rhdus "CON"                   "OROGRAPHIC CONVEXITY"   ""   ""
state   real   OA1              ij     misc        1         -     i012rhdus "OA1"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OA2              ij     misc        1         -     i012rhdus "OA2"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OA3              ij     misc        1         -     i012rhdus "OA3"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OA4              ij     misc        1         -     i012rhdus "OA4"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OL1              ij     misc        1         -     i012rhdus "OL1"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OL2              ij     misc        1         -     i012rhdus "OL2"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OL3              ij     misc        1         -     i012rhdus "OL3"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""
state   real   OL4              ij     misc        1         -     i012rhdus "OL4"                   "OROGRAPHIC DIRECTION ASYMMETRY FUNCTION"   ""   ""

# BEP urban scheme  variables
state    real    a_u_bep       ikj     misc        1         Z      -        "a_u_bep"               "IMPLICIT FOR X-COMP."      "s-1"
state    real    a_v_bep       ikj     misc        1         Z      -        "a_v_bep"               "IMPLICIT FOR Y-COMP."      "s-1"
state    real    a_t_bep       ikj     misc        1         Z      -        "a_t_bep"               "IMPLICIT FOR Pot. Temp"    "s-1"
state    real    a_q_bep       ikj     misc        1         Z      -        "a_q_bep"               "IMPLICIT FOR Moisture"     "s-1"
state    real    a_e_bep       ikj     misc        1         Z      -        "a_e_bep"               "IMPLICIT FOR TKE"          "s-1"
state    real    b_u_bep       ikj     misc        1         Z      -        "b_u_bep"               "EXPLICIT FOR X-COMP."      "m s-2"
state    real    b_v_bep       ikj     misc        1         Z      -        "b_v_bep"               "EXPLICIT FOR Y-COMP."      "m s-2"
state    real    b_t_bep       ikj     misc        1         Z      -        "b_t_bep"               "EXPLICIT FOR Pot. Temp"    "K s-1"
state    real    b_q_bep       ikj     misc        1         Z      -        "b_q_bep"               "EXPLICIT FOR Moisture"     "kg s-1"
state    real    b_e_bep       ikj     misc        1         Z      -        "b_e_bep"               "EXPLICIT FOR TKE"          "m2 s-3"
state    real    dlg_bep       ikj     misc        1         Z      -        "dlg_bep"               "length scale 1"            "m"
state    real    dl_u_bep      ikj     misc        1         Z      -        "dl_u_bep"              "urban length scale"        "m"
state    real    sf_bep        ikj     misc        1         Z      -        "sf_bep"                "surface grid"              "-"
state    real    vl_bep        ikj     misc        1         Z      -        "vl_bep"                "volume grid"               "-"
# BOULAC PBL variables
state    real   tke_pbl        ikj     misc        1         -      rh        "tke_pbl"               "TKE FROM Bougeault and Lacarrere"      "m2 s-2"
state    real   el_pbl        ikj     misc        1         -       h        "el_pbl"               "Length scale FROM Bougeault and Lacarrere"      "m"
# Diagnostic PBL variables
state    real   wu_tur         ikj     misc        1         -      r        "wu_tur"               "Turbulent flux of momentum(x)"      "m2 s-2"
state    real   wv_tur         ikj     misc        1         -      r        "wv_tur"               "Turbulent flux of momentum(y)"      "m2 s-2"
state    real   wt_tur         ikj     misc        1         -      r        "wt_tur"               "Turbulent flux of temperature"      "K m s-1"
state    real   wq_tur         ikj     misc        1         -      r        "wq_tur"               "Turbulent flux of water vapor"      "- m s-1"

# gfdl (eta) radiation State Variables
state    real    HTOP            ij     misc        1         -      r        "HTOP"                 "TOP OF CONVECTION LEVEL"         ""
state    real    HBOT            ij     misc        1         -      r        "HBOT"                 "BOT OF CONVECTION LEVEL"         ""
state    real    HTOPR           ij     misc        1         -      r        "HTOPR"                "TOP OF CONVECTION LEVEL FOR RADIATION"    ""
state    real    HBOTR           ij     misc        1         -      r        "HBOTR"                "BOT OF CONVECTION LEVEL FOR RADIATION"    ""
state    real    CUTOP           ij     misc        1         -      r        "CUTOP"                "TOP OF CONVECTION LEVEL FROM CUMULUS PAR"    ""
state    real    CUBOT           ij     misc        1         -      r        "CUBOT"                "BOT OF CONVECTION LEVEL FROM CUMULUS PAR"    ""
state    REAL      CUPPT         ij     misc        1         -      r        "CUPPT"                "ACCUMULATED CONVECTIVE RAIN SINC LAST CALL TO THE RADIATION"         ""
state    real   rswtoa           ij     misc        1         -      -
state    real   rlwtoa           ij     misc        1         -      -
state    real   czmean           ij     misc        1         -      -
state    real   cfracl           ij     misc        1         -      -
state    real   cfracm           ij     misc        1         -      -
state    real   cfrach           ij     misc        1         -      -
state    real   acfrst           ij     misc        1         -      -
state integer   ncfrst           ij     misc        1         -      -
state    real   acfrcv           ij     misc        1         -      -
state integer   ncfrcv           ij     misc        1         -      -

# cam radiation variables
state  real    -       i{ls}jf ozmixm      1    -   -     -
state  real   mth01    i{ls}jf ozmixm      1    -   -     -
state  real   mth02    i{ls}jf ozmixm      1    -   -     -
state  real   mth03    i{ls}jf ozmixm      1    -   -     -
state  real   mth04    i{ls}jf ozmixm      1    -   -     -
state  real   mth05    i{ls}jf ozmixm      1    -   -     -
state  real   mth06    i{ls}jf ozmixm      1    -   -     -
state  real   mth07    i{ls}jf ozmixm      1    -   -     -
state  real   mth08    i{ls}jf ozmixm      1    -   -     -
state  real   mth09    i{ls}jf ozmixm      1    -   -     -
state  real   mth10    i{ls}jf ozmixm      1    -   -     -
state  real   mth11    i{ls}jf ozmixm      1    -   -     -
state  real   mth12    i{ls}jf ozmixm      1    -   -     -
state  real   pin       {ls}     misc      1    -   -      "PIN"             "PRESSURE LEVEL OF OZONE MIXING RATIO"  "millibar"
state  real   m_ps       ij   misc      2    -   -      "m_ps"            "PS from MATCH on WRF grids"
state  real    -       idjf aerosolc    2    -   -       -
state  real   SUL      idjf aerosolc    2    -   -     "SUL"        "SUL aerosol concentration"
state  real   SSLT     idjf aerosolc    2    -   -     "SSLT"        "SSLT aerosol concentration"
state  real   DUST1    idjf aerosolc    2    -   -     "DUST1"        "DUST1 aerosol concentration"
state  real   DUST2    idjf aerosolc    2    -   -     "DUST2"        "DUST2 aerosol concentration"
state  real   DUST3    idjf aerosolc    2    -   -     "DUST3"        "DUST3 aerosol concentration"
state  real   DUST4    idjf aerosolc    2    -   -     "DUST4"        "DUST4 aerosol concentration"
state  real   OCPHO    idjf aerosolc    2    -   -     "OCPHO"        "OCPHO aerosol concentration"
state  real   BCPHO    idjf aerosolc    2    -   -     "BCPHO"        "BCPHO aerosol concentration"
state  real   OCPHI    idjf aerosolc    2    -   -     "OCPHI"        "OCPHI aerosol concentration"
state  real   BCPHI    idjf aerosolc    2    -   -     "BCPHI"        "BCPHI aerosol concentration"
state  real   BG       idjf aerosolc    2    -   -     "BG"        "BG aerosol concentration"
state  real   VOLC     idjf aerosolc    2    -   -     "VOLC"        "VOLC aerosol concentration"
state  real   m_hybi    d     misc      1    -   -     "m_hybi"         "MATCH hybi"

# new eta microphpysics State Variables
state   real    F_ICE_PHY      ikj     misc         1         -      rhdu     "F_ICE_PHY"            "FRACTION OF ICE"         ""
state   real    F_RAIN_PHY     ikj     misc         1         -      rhdu     "F_RAIN_PHY"           "FRACTION OF RAIN "         ""
state   real    F_RIMEF_PHY    ikj     misc         1         -      rhdu     "F_RIMEF_PHY"          "MASS RATIO OF RIMED ICE "         ""
state   real    qndropsource   ikj     misc         1         -      -        "qndropsource"         "Droplet number source"   "#/kg/s"

# Other Misc State Variables                                            
state   real    h_diabatic     ikj     misc         1         -      rdu      "h_diabatic"            "PREVIOUS TIMESTEP CONDENSATIONAL HEATING"         "K s-1"      
state   real    msft           ij      misc         1         -     i012rhdu=(copy_fcnm)    "MAPFAC_M"         "Map scale factor on mass grid" ""
state   real    msfu           ij      misc         1         X     i012rhdu=(copy_fcnm)    "MAPFAC_U"         "Map scale factor on u-grid" ""
state   real    msfv           ij      misc         1         Y     i012rhdu=(copy_fcnm)    "MAPFAC_V"         "Map scale factor on v-grid" ""
state   real    msftx          ij      misc         1         -     i012rhdu=(copy_fcnm)    "MAPFAC_MX"         "Map scale factor on mass grid, x direction" ""
state   real    msfty          ij      misc         1         -     i012rhdu=(copy_fcnm)    "MAPFAC_MY"         "Map scale factor on mass grid, y direction" ""
state   real    msfux          ij      misc         1         X     i012rhdu=(copy_fcnm)    "MAPFAC_UX"         "Map scale factor on u-grid, x direction" ""
state   real    msfuy          ij      misc         1         X     i012rhdu=(copy_fcnm)    "MAPFAC_UY"         "Map scale factor on u-grid, y direction" ""
state   real    msfvx          ij      misc         1         Y     i012rhdu=(copy_fcnm)    "MAPFAC_VX"         "Map scale factor on v-grid, x direction" ""
state   real    msfvx_inv      ij      misc         1         Y     i012rhdu=(copy_fcnm)    "MF_VX_INV"         "Inverse map scale factor on v-grid, x direction" ""
state   real    msfvy          ij      misc         1         Y     i012rhdu=(copy_fcnm)    "MAPFAC_VY"         "Map scale factor on v-grid, y direction" ""

state   real    f              ij      misc         1         -     i012rhdu=(copy_fcnm)    "f"                "Coriolis sine latitude term"  "s-1"
state   real    e              ij      misc         1         -     i012rhdu=(copy_fcnm)    "e"                "Coriolis cosine latitude term"  "s-1"
state   real    sina           ij      misc         1         -     i012rhdu=(copy_fcnm)    "SINALPHA"         "Local sine of map rotation"   ""
state   real    cosa           ij      misc         1         -     i012rhdu=(copy_fcnm)    "COSALPHA"         "Local cosine of map rotation"  ""
state   real    ht             ij      misc         1         -     i012rhdus  "HGT"              "Terrain Height"   "m"
state   real    ht_fine        ij      misc         1         -     -          "HGT_FINE"         "Fine Terrain Height"   "m"
state   real    ht_int         ij      misc         1         -     -          "HGT_INT"          "Terrain Height Horizontally Interpolated"   "m"
state   real    ht_input       ij      misc         1         -     -          "HGT_INPUT"        "Terrain Height from FG Input File"   "m"
state   real    ht_shad        ijb     misc         1         -     df=(bdy_interp:dt)         "HGT_SHAD"        "Height of orographic shadow"   "m"
i1      real    ht_loc         ij      misc         1         -     - 
state   integer  shadowmask    ij      misc         1         -     - 
state   integer min_ptchsz     -       misc         1         -     r

state   real    TSK            ij      misc         1         -     i012rhdu=(copy_fcnm)    "TSK"                   "SURFACE SKIN TEMPERATURE"                  "K"
state   real    dfi_TSK        ij      misc         1         -     r                       "TSK_dfi"               "saved SURFACE SKIN TEMPERATURE"
state   real    TSK_SAVE       ij      misc         1         -     -         "TSK_SAVE" "SURFACE SKIN TEMPERATURE, EXTRA COPY FOR SEA ICE TESTS in REAL"       "K"
state   real    u_base         k       misc         1         -     ir        "u_base"                "BASE STATE X WIND IN IDEALIZED CASES"         ""      
state   real    v_base         k       misc         1         -     ir        "v_base"                "BASE STATE Y WIND IN IDEALIZED CASES"         ""      
state   real    qv_base        k       misc         1         -     ir        "qv_base"               "BASE STATE QV IN IDEALIZED CASES"         ""      
state   real    z_base         k       misc         1         -     ir        "z_base"                "BASE STATE HEIGHT IN IDEALIZED CASES"         ""      
state   real    u_frame        -       misc         1         -     ir        "u_frame"               "FRAME X WIND"         "m s-1"      
state   real    v_frame        -       misc         1         -     ir        "v_frame"               "FRAME Y WIND"         "m s-1"      
# p_top appears as metadata between SI and real but as a state variable in real and WRF
# since it is a scalar and a constant, it makes sense to have it as metadata -- there
# are, however, probably post-processing programs that expect to see it as an I/O record
# another problem: share/input_wrf tries to read this as metadata (fine for real reading
# SI, but with model reading real output, it generates a warning when debug is > 0 in
# namelist and causes repeated questions from users.  A third problem is the potential
# collision between a metadata name and a field record in the I/O data
# resolve this how?  Have the real program throw a switch to tell the code to get it
# from the metadata?  Otherwise it's a field?
state   real    p_top          -       misc         -         -     irh       "p_top"                 "PRESSURE TOP OF THE MODEL"  "Pa"
state   real    t00            -       misc         -         -     i02rh     "t00"                   "BASE STATE TEMPERATURE   "  "K"
state   real    p00            -       misc         -         -     i02rh     "p00"                   "BASE STATE PRESURE"         "Pa"
state   real    tlp            -       misc         -         -     i02rh     "tlp"                   "BASE STATE LAPSE RATE    "  ""
state   real    tiso           -       misc         -         -     i02rh     "tiso"                  "TEMP AT WHICH THE BASE T TURNS CONST"  "K"
state   real    max_msftx      -       misc         -         -      rh       "max_mstfx"             "Max map factor in domain"  ""
state   real    max_msfty      -       misc         -         -      rh       "max_mstfy"             "Max map factor in domain"  ""
                                                
# Other physics variables
                                                
state    real  RTHCUTEN        ikj      misc        1         -      r        "RTHCUTEN"              "COUPLED THETA TENDENCY DUE TO CUMULUS SCHEME"     "Pa K s-1"
state    real  RQVCUTEN        ikj      misc        1         -      r        "RQVCUTEN"              "COUPLED Q_V TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQRCUTEN        ikj      misc        1         -      r        "RQRCUTEN"              "COUPLED Q_R TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQCCUTEN        ikj      misc        1         -      r        "RQCCUTEN"              "COUPLED Q_C TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQSCUTEN        ikj      misc        1         -      r        "RQSCUTEN"              "COUPLED Q_S TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  RQICUTEN        ikj      misc        1         -      r        "RQICUTEN"              "COUPLED Q_I TENDENCY DUE TO CUMULUS SCHEME"       "Pa kg kg-1 s-1"
state    real  W0AVG           ikj      misc        1         -      r        "W0AVG"                 "AVERAGE VERTICAL VELOCITY FOR KF CUMULUS SCHEME"         "m s-1"
                                                
state    real  RAINC            ij      misc        1         -      rhdu     "RAINC"                 "ACCUMULATED TOTAL CUMULUS PRECIPITATION"                 "mm"      
state    real  RAINNC           ij      misc        1         -      rhdu     "RAINNC"                "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"              "mm"      
state  integer I_RAINC          ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_RAINC"            "BUCKET FOR RAINC"                    ""
state  integer I_RAINNC         ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_RAINNC"           "BUCKET FOR RAINNC"                   ""
state    real  PRATEC           ij      misc        1         -      r        "PRATEC"                "PRECIP RATE FROM CUMULUS SCHEME"                         "mm s-1"
state    real  RAINCV           ij      misc        1         -      r        "RAINCV"                "TIME-STEP CUMULUS PRECIPITATION"                         "mm"      
state    real  RAINNCV          ij      misc        1         -      r        "RAINNCV"               "TIME-STEP NONCONVECTIVE PRECIPITATION"                   "mm"
state    real  RAINBL           ij      misc        1         -      r        "RAINBL"                "PBL TIME-STEP TOTAL PRECIPITATION"                       "mm"      
state    real  SNOWNC           ij      misc        1         -      rhdu     "SNOWNC"                "ACCUMULATED TOTAL GRID SCALE SNOW AND ICE"               "mm"
state    real  GRAUPELNC        ij      misc        1         -      rhdu     "GRAUPELNC"             "ACCUMULATED TOTAL GRID SCALE GRAUPEL"                    "mm"
state    real  HAILNC           ij      misc        1         -      rhdu     "HAILNC"                "ACCUMULATED TOTAL GRID SCALE HAIL"                       "mm"
state    real  SNOWNCV          ij      misc        1         -      r        "SNOWNCV"               "TIME-STEP NONCONVECTIVE SNOW AND ICE"                    "mm"
state    real  GRAUPELNCV       ij      misc        1         -      r        "GRAUPELNCV"            "TIME-STEP NONCONVECTIVE GRAUPEL"                         "mm"
state    real  HAILNCV          ij      misc        1         -      r        "HAILNCV"               "TIME-STEP NONCONVECTIVE HAIL"                            "mm"
state    real    refl_10cm      ikj     dyn_em      1         -     hdu     "refl_10cm" "Radar reflectivity (lamda = 10 cm)"  "dBZ"
state    real  NCA              ij      misc        1         -      r        "NCA"                   "COUNTER OF THE CLOUD RELAXATION TIME IN KF CUMULUS SCHEME"    ""      
state    integer  LOWLYR        ij      misc        1         -     -         "LOWLYR"                "INDEX OF LOWEST MODEL LAYER ABOVE THE GROUND IN BMJ SCHEME"   ""      
state    real  MASS_FLUX        ij      misc        1         -      r        "MASS_FLUX"             "DOWNDRAFT MASS FLUX FOR IN GRELL CUMULUS SCHEME"  "mb hour-1"
state    real  apr_gr           ij      misc        1         -      r       "APR_GR"                "PRECIP FROM CLOSURE OLD_GRELL"   "mm hour-1"
state    real  apr_w            ij      misc        1         -      r       "APR_W"                 "PRECIP FROM CLOSURE W"           "mm hour-1"
state    real  apr_mc           ij      misc        1         -      r       "APR_MC"                "PRECIP FROM CLOSURE KRISH MV"    "mm hour-1"
state    real  apr_st           ij      misc        1         -      r       "APR_ST"                "PRECIP FROM CLOSURE STABILITY"   "mm hour-1"
state    real  apr_as           ij      misc        1         -      r       "APR_AS"                "PRECIP FROM CLOSURE AS-TYPE"     "mm hour-1"
state    real  apr_capma        ij      misc        1         -      r       "APR_CAPMA"             "PRECIP FROM MAX CAP"             "mm hour-1"
state    real  apr_capme        ij      misc        1         -      r       "APR_CAPME"             "PRECIP FROM MEAN CAP"            "mm hour-1"
state    real  apr_capmi        ij      misc        1         -      r       "APR_CAPMI"             "PRECIP FROM MIN CAP"             "mm hour-1"
state    real  edt_out          ij      misc        1         -      -        "EDT_OUT"             "EDT FROM GD SCHEME"             ""
state    real  xmb_shallow      ij      misc        1         -      rh       "XMB_SHALLOW"             "MASSFLUX FROM SHALLOW CONVECTION (G3 only)"             ""
state    integer  k22_shallow   ij      misc        1         -      rh       "K22_SHALLOW"             "K22 LEVEL FROM SHALLOW CONVECTION (G3 only)"             ""
state    integer  kbcon_shallow   ij      misc        1         -    rh       "KBCON_SHALLOW"             "KBCON LEVEL FROM SHALLOW CONVECTION (G3 only)"             ""
state    integer  ktop_shallow   ij      misc        1         -     rh       "KTOP_SHALLOW"             "KTOP LEVEL FROM SHALLOW CONVECTION (G3 only)"             ""
state    real  xf_ens           ije     misc        1         Z      r        "XF_ENS"                "MASS FLUX PDF IN GRELL CUMULUS SCHEME"     "mb hour-1"
state    real  pr_ens           ije     misc        1         Z      r        "PR_ENS"                "PRECIP RATE PDF IN GRELL CUMULUS SCHEME"    "mb hour-1"
state    real  cugd_tten        ikj     misc        1         -      h        "CUGD_TTEN"             "INITIAL TTENDENCY OUT OFF GRELL CUMULUS SCHEME"  "K s-1"
state    real  cugd_qvten       ikj     misc        1         -      h        "CUGD_QVTEN"            "INITIAL QTENDENCY OUT OFF GRELL CUMULUS SCHEME"  "K s-1"
state    real  cugd_ttens       ikj     misc        1         -      h        "CUGD_TTENS"            "INITIAL SUBSIDENCE TTENDENCY OUT OFF GRELL CUMULUS SCHEME"  "K s-1"
state    real  cugd_qvtens      ikj     misc        1         -      h        "CUGD_QVTENS"           "INITIAL SUBSIDNCE QTENDENCY OUT OFF GRELL CUMULUS SCHEME"  "K s-1"
state    real  cugd_qcten       ikj     misc        1         -      h        "CUGD_QCTEN"            "INITIAL TEMPERATURE TENDENCY OUT OFF GRELL CUMULUS SCHEME"  "K s-1"
state    real  RTHFTEN          ikj     misc        1         -      r        "RTHFTEN"               "TEMPERATURE TENDENCY USED IN GRELL CUMULUS SCHEME"  "K s-1"      
state    real  RQVFTEN          ikj     misc        1         -      r        "RQVFTEN"               "MOISTURE TENDENCY USED IN GRELL CUMULUS SCHEME"     "kg s-1"

state integer  STEPCU          -        misc        1         -      r        "STEPCU"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN CONVECTION CALLS"  ""
                                                
state    real  RTHRATEN        ikj      misc        1         -      rd       "RTHRATEN"              "COUPLED THETA TENDENCY DUE TO RADIATION"              "Pa K s-1"
state    real  RTHRATENLW      ikj      misc        1         -      r        "RTHRATLW"              "UNCOUPLED THETA TENDENCY DUE TO LONG WAVE RADIATION"    "K s-1"
state    real  RTHRATENSW      ikj      misc        1         -      r        "RTHRATSW"              "UNCOUPLED THETA TENDENCY DUE TO SHORT WAVE RADIATION"   "K s-1"
state    real  CLDFRA          ikj      misc        1         -      r        "CLDFRA"                "CLOUD FRACTION"                                       ""

state    real  SWDOWN           ij      misc        1         -      rhd      "SWDOWN"                "DOWNWARD SHORT WAVE FLUX AT GROUND SURFACE"           "W m-2"      
state    real  SWDOWNC          ij      misc        1         -      -        "SWDOWNC"               "DOWNWARD CLEAR-SKY SHORT WAVE FLUX AT GROUND SURFACE"           "W m-2"      
state    real  GSW              ij      misc        1         -      rd       "GSW"                   "NET SHORT WAVE FLUX AT GROUND SURFACE"           "W m-2"      
state    real  GLW              ij      misc        1         -      rhd      "GLW"                   "DOWNWARD LONG WAVE FLUX AT GROUND SURFACE"            "W m-2"      
state    real  SWNORM           ij      misc        1         -      rhd      "SWNORM"                "NORMAL SHORT WAVE FLUX AT GROUND SURFACE (SLOPE-DEPENDENT)"           "W m-2"      

# upward and downward clearsky and total diagnostic fluxes for CAM radiation
state    real  ACSWUPT          ij      misc        1         -      rhdu     "ACSWUPT"               "ACCUMULATED UPWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state    real  ACSWUPTC         ij      misc        1         -      rhdu     "ACSWUPTC"              "ACCUMULATED UPWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state    real  ACSWDNT          ij      misc        1         -      rhdu     "ACSWDNT"               "ACCUMULATED DOWNWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state    real  ACSWDNTC         ij      misc        1         -      rhdu     "ACSWDNTC"              "ACCUMULATED DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state    real  ACSWUPB          ij      misc        1         -      rhdu     "ACSWUPB"               "ACCUMULATED UPWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state    real  ACSWUPBC         ij      misc        1         -      rhdu     "ACSWUPBC"              "ACCUMULATED UPWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state    real  ACSWDNB          ij      misc        1         -      rhdu     "ACSWDNB"               "ACCUMULATED DOWNWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state    real  ACSWDNBC         ij      misc        1         -      rhdu     "ACSWDNBC"              "ACCUMULATED DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state    real  ACLWUPT          ij      misc        1         -      rhdu     "ACLWUPT"               "ACCUMULATED UPWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state    real  ACLWUPTC         ij      misc        1         -      rhdu     "ACLWUPTC"              "ACCUMULATED UPWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state    real  ACLWDNT          ij      misc        1         -      rhdu     "ACLWDNT"               "ACCUMULATED DOWNWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state    real  ACLWDNTC         ij      misc        1         -      rhdu     "ACLWDNTC"              "ACCUMULATED DOWNWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state    real  ACLWUPB          ij      misc        1         -      rhdu     "ACLWUPB"               "ACCUMULATED UPWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state    real  ACLWUPBC         ij      misc        1         -      rhdu     "ACLWUPBC"              "ACCUMULATED UPWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"
state    real  ACLWDNB          ij      misc        1         -      rhdu     "ACLWDNB"               "ACCUMULATED DOWNWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state    real  ACLWDNBC         ij      misc        1         -      rhdu     "ACLWDNBC"              "ACCUMULATED DOWNWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"
state integer  I_ACSWUPT        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWUPT"     "BUCKET FOR UPWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state integer  I_ACSWUPTC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWUPTC"    "BUCKET FOR UPWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state integer  I_ACSWDNT        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWDNT"     "BUCKET FOR DOWNWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state integer  I_ACSWDNTC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWDNTC"    "BUCKET FOR DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state integer  I_ACSWUPB        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWUPB"     "BUCKET FOR UPWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state integer  I_ACSWUPBC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWUPBC"    "BUCKET FOR UPWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state integer  I_ACSWDNB        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWDNB"     "BUCKET FOR DOWNWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state integer  I_ACSWDNBC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACSWDNBC"    "BUCKET FOR DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state integer  I_ACLWUPT        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWUPT"     "BUCKET FOR UPWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state integer  I_ACLWUPTC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWUPTC"    "BUCKET FOR UPWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state integer  I_ACLWDNT        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWDNT"     "BUCKET FOR DOWNWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state integer  I_ACLWDNTC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWDNTC"    "BUCKET FOR DOWNWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state integer  I_ACLWUPB        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWUPB"     "BUCKET FOR UPWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state integer  I_ACLWUPBC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWUPBC"    "BUCKET FOR UPWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"
state integer  I_ACLWDNB        ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWDNB"     "BUCKET FOR DOWNWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state integer  I_ACLWDNBC       ij      misc        1         -      rhd=(interp_fcni)u=(copy_fcni)     "I_ACLWDNBC"    "BUCKET FOR DOWNWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"
state    real  SWUPT            ij      misc        1         -      rhdu     "SWUPT"                 "INSTANTANEOUS UPWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state    real  SWUPTC           ij      misc        1         -      rhdu     "SWUPTC"                "INSTANTANEOUS UPWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state    real  SWDNT            ij      misc        1         -      rhdu     "SWDNT"                 "INSTANTANEOUS DOWNWELLING SHORTWAVE FLUX AT TOP"          "J m-2"
state    real  SWDNTC           ij      misc        1         -      rhdu     "SWDNTC"                "INSTANTANEOUS DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" "J m-2"
state    real  SWUPB            ij      misc        1         -      rhdu     "SWUPB"                 "INSTANTANEOUS UPWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state    real  SWUPBC           ij      misc        1         -      rhdu     "SWUPBC"                "INSTANTANEOUS UPWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state    real  SWDNB            ij      misc        1         -      rhdu     "SWDNB"                 "INSTANTANEOUS DOWNWELLING SHORTWAVE FLUX AT BOTTOM"          "J m-2"
state    real  SWDNBC           ij      misc        1         -      rhdu     "SWDNBC"                "INSTANTANEOUS DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" "J m-2"
state    real  LWUPT            ij      misc        1         -      rhdu     "LWUPT"                 "INSTANTANEOUS UPWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state    real  LWUPTC           ij      misc        1         -      rhdu     "LWUPTC"                "INSTANTANEOUS UPWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state    real  LWDNT            ij      misc        1         -      rhdu     "LWDNT"                 "INSTANTANEOUS DOWNWELLING LONGWAVE FLUX AT TOP"          "J m-2"
state    real  LWDNTC           ij      misc        1         -      rhdu     "LWDNTC"                "INSTANTANEOUS DOWNWELLING CLEAR SKY LONGWAVE FLUX AT TOP" "J m-2"
state    real  LWUPB            ij      misc        1         -      rhdu     "LWUPB"                 "INSTANTANEOUS UPWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state    real  LWUPBC           ij      misc        1         -      rhdu     "LWUPBC"                "INSTANTANEOUS UPWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"
state    real  LWDNB            ij      misc        1         -      rhdu     "LWDNB"                 "INSTANTANEOUS DOWNWELLING LONGWAVE FLUX AT BOTTOM"          "J m-2"
state    real  LWDNBC           ij      misc        1         -      rhdu     "LWDNBC"                "INSTANTANEOUS DOWNWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" "J m-2"

state    real  SWCF             ij      misc        1         -      r         "SWCF"                  "SHORT WAVE CLOUD FORCING AT TOA"                     "W m-2"
state    real  LWCF             ij      misc        1         -      r         "LWCF"                  "LONG WAVE CLOUD FORCING AT TOA"                      "W m-2"
state    real  OLR              ij      misc        1         -      rh        "OLR"                   "TOA OUTGOING LONG WAVE"                              "W m-2"

# these next 2 are for the HFSoLE/PET demo; writing these to auxhist1 output over MCEL for coupling
# with wave model, only if compiled with -DMCELIO, JM 2003/05/29
state    real  XLAT             ij      misc        1         -     i0123rh01du=(copy_fcnm)      "XLAT"                "LATITUDE, SOUTH IS NEGATIVE"                                          "degree_north"
state    real  XLONG            ij      misc        1         -     i0123rh01du=(copy_fcnm)      "XLONG"               "LONGITUDE, WEST IS NEGATIVE"                                          "degree_east"
state    real  XLAT_U           ij      dyn_em      1         X     i012rh01du=(copy_fcnm)       "XLAT_U"              "LATITUDE, SOUTH IS NEGATIVE"                                          "degree_north"
state    real  XLONG_U          ij      dyn_em      1         X     i012rh01du=(copy_fcnm)       "XLONG_U"             "LONGITUDE, WEST IS NEGATIVE"                                          "degree_east"
state    real  XLAT_V           ij      dyn_em      1         Y     i012rh01du=(copy_fcnm)       "XLAT_V"              "LATITUDE, SOUTH IS NEGATIVE"                                          "degree_north"
state    real  XLONG_V          ij      dyn_em      1         Y     i012rh01du=(copy_fcnm)       "XLONG_V"             "LONGITUDE, WEST IS NEGATIVE"                                          "degree_east"
state    real  ALBEDO           ij      misc        1         -      rh          "ALBEDO"                   "ALBEDO"
state    real  CLAT             ij      misc        1         -     i012rdu=(copy_fcnm)          "CLAT"                "COMPUTATIONAL GRID LATITUDE, SOUTH IS NEGATIVE"                       "degree_north"
state    real  CLONG            ij      misc        1         -     i012rdu=(copy_fcnm)          "CLONG"               "COMPUTATIONAL GRID LONGITUDE, WEST IS NEGATIVE"                       "degree_east"
state    real  ALBBCK           ij      misc        1         -     i0124rh   "ALBBCK"                "BACKGROUND ALBEDO"        ""
state    real  EMBCK            ij      misc        1         -      r        "EMBCK"                 "BACKGROUND EMISSIVITY"         ""
state    real  EMISS            ij      misc        1         -      rh       "EMISS"                 "SURFACE EMISSIVITY"         "" 
state    real  SNOTIME          ij      misc        1         -      r        "SNOTIME"               "SNOTIME"         "" 
state    real  NOAHRES          ij      misc        1         -      h        "NOAHRES"               "RESIDUAL OF THE NOAH SURFACE ENERGY BUDGET" "W m{-2}"
state    real  CLDEFI           ij      misc        1         -      r        "CLDEFI"                "precipitation efficiency in BMJ SCHEME"    ""      
state integer  STEPRA          -        misc        1         -      r        "STEPRA"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN RADIATION CALLS"        ""
                                                
state    real  RUBLTEN         ikj      misc        1         -      r        "RUBLTEN"               "COUPLED X WIND TENDENCY DUE TO PBL PARAMETERIZATION"  "Pa m s-2"
state    real  RVBLTEN         ikj      misc        1         -      r        "RVBLTEN"               "COUPLED Y WIND TENDENCY DUE TO PBL PARAMETERIZATION"  "Pa m s-2"
state    real  RTHBLTEN        ikj      misc        1         -      r        "RTHBLTEN"              "COUPLED THETA TENDENCY DUE TO PBL PARAMETERIZATION"   "Pa K s-1"
state    real  RQVBLTEN        ikj      misc        1         -      r        "RQVBLTEN"              "COUPLED Q_V TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"
state    real  RQCBLTEN        ikj      misc        1         -      r        "RQCBLTEN"              "COUPLED Q_C TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"
state    real  RQIBLTEN        ikj      misc        1         -      r        "RQIBLTEN"              "COUPLED Q_I TENDENCY DUE TO PBL PARAMETERIZATION"     "Pa kg kg-1 s-1"      

# State vector for etampnew microphysics. Must be declared state because it is not read-once and is needed for restarting.
state    real  mp_restart_state   p      misc        1         -      r       "MP_RESTART_STATE"       "STATE VECTOR FOR MICROPHYSICS RESTARTS"
state    real  tbpvs_state        p      misc        1         -      r        "TBPVS_STATE"           "STATE FOR ETAMPNEW MICROPHYSICS"
state    real  tbpvs0_state       p      misc        1         -      r        "TBPVS0_STATE"          "STATE FOR ETAMPNEW MICROPHYSICS"

# State variables for landuse_init, Must be declared state because they are read in and needed for restarts. Had been SAVE vars in
# landuse_init (phys/module_physics_init.F)
state    integer  landuse_isice   -      misc       -          -     r
state    integer  landuse_lucats  -      misc       -          -     r
state    integer  landuse_luseas  -      misc       -          -     r
state    integer  landuse_isn     -      misc       -          -     r
state    real     lu_state        p      misc       -          -     r

i1       real  th_phy          ikj      misc        1         -                                          
i1       real  pi_phy          ikj      misc        1         -                                          
i1       real  p_phy           ikj      misc        1         -                                          
i1       real  t_phy           ikj      misc        1         -                                          
i1       real  u_phy           ikj      misc        1         -                                          
i1       real  v_phy           ikj      misc        1         -                                          
i1       real  dz8w            ikj      misc        1         Z                                          
i1       real  p8w             ikj      misc        1         Z                                          
i1       real  t8w             ikj      misc        1         Z                                          
i1       real  rho_phy         ikj      misc        1         -                                          
i1    logical  CU_ACT_FLAG     ij       misc        1         -

                                                
state    real  TMN              ij      misc        1         -     i012rhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TMN"   "SOIL TEMPERATURE AT LOWER BOUNDARY"           "K"
state    real  TYR              ij      misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TYR"        "ANNUAL MEAN SFC TEMPERATURE"           "K"
state    real  TYRA             ij      misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TYRA"   "ACCUMULATED YEARLY SFC TEMPERATURE FOR CURRENT YEAR"  "K"
state    real  TDLY             ij      misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TDLY"     "ACCUMULATED DAILY SFC TEMPERATURE FOR CURRENT DAY"  "K"
state    real  TLAG             i&j     misc        1         -     rd=(interp_mask_land_field:lu_index)u=(copy_fcnm)     "TLAG"         "DAILY MEAN SFC TEMPERATURE OF PRIOR DAYS"       "K"
state    integer NYEAR          -       misc        1         -     r        "NYEAR"                  "ACCUM DAYS IN A YEAR"                         ""
state    integer NDAY           -       misc        1         -     r        "NDAY"                  "ACCUM TIMESTEPS IN A DAY"                      ""
state    real  XLAND            ij      misc        1         -     i02rhd=(interp_fcnm)u=(copy_fcnm)       "XLAND"                 "LAND MASK (1 FOR LAND, 2 FOR WATER)"          ""      
state    real  ZNT              ij      misc        1         -      i3r      "ZNT"                   "TIME-VARYING ROUGHNESS LENGTH"                "m"      
state    real  CK               ij      misc        1         -      r        "CK"                    "ENTHALPY EXCHANGE COEFF AT 10 m"                      ""
state    real  CKA              ij      misc        1         -      r        "CKA"                   "ENTHALPY EXCHANGE COEFF AT LOWEST MODEL LVL"          ""
state    real  CD               ij      misc        1         -      r        "CD"                    "DRAG COEFF AT 10m"                      ""
state    real  CDA              ij      misc        1         -      r        "CDA"                   "DRAG COEFF AT LOWEST MODEL LVL"                      ""
state    real  UST              ij      misc        1         -      rh       "UST"                   "U* IN SIMILARITY THEORY"                      "m s-1"      
state    real  USTM             ij      misc        1         -      r        "USTM"                  "U* IN SIMILARITY THEORY WITHOUT VCONV"        "m s-1"
i1       real  HOL              ij      misc        1         -      -        "HOL"                   "PBL HEIGHT OVER MONIN-OBUKHOV LENGTH"         ""
state    real  RMOL             ij      misc        1         -      r        "RMOL"                  "1./Monin Ob. Length"                      ""
state    real  MOL              ij      misc        1         -      r        "MOL"                   "T* IN SIMILARITY THEORY"                      "K"      
state    real  PBLH             ij      misc        1         -      rh       "PBLH"                  "PBL HEIGHT"         "m"      
state    real  CAPG             ij      misc        1         -      r        "CAPG"                  "HEAT CAPACITY FOR SOIL"                       "J K-1 m-3"      
state    real  THC              ij      misc        1         -      r        "THC"                   "THERMAL INERTIA"                              "Cal cm-1 K-1 s-0.5"      
state    real  HFX              ij      misc        1         -      rh       "HFX"                   "UPWARD HEAT FLUX AT THE SURFACE"              "W m-2"      
state    real  QFX              ij      misc        1         -      rh       "QFX"                   "UPWARD MOISTURE FLUX AT THE SURFACE"          "kg m-2 s-1"      
state    real  LH               ij      misc        1         -      rh       "LH"                    "LATENT HEAT FLUX AT THE SURFACE"              "W m-2"
state    real  ACHFX            ij      misc        1         -      rhdu     "ACHFX"                 "ACCUMULATED UPWARD HEAT FLUX AT THE SURFACE"  "J m-2"
state    real  ACLHF            ij      misc        1         -      rhdu     "ACLHF"                 "ACCUMULATED UPWARD LATENT HEAT FLUX AT THE SURFACE"  "J m-2"
state    real  FLHC             ij      misc        1         -      r        "FLHC"                  "SURFACE EXCHANGE COEFFICIENT FOR HEAT"       ""
state    real  FLQC             ij      misc        1         -      r        "FLQC"                  "SURFACE EXCHANGE COEFFICIENT FOR MOISTURE"   ""
state    real  QSG              ij      misc        1         -      r        "QSG"                   "SURFACE SATURATION WATER VAPOR MIXING RATIO"   "kg kg-1"
state    real  QVG              ij      misc        1         -      r        "QVG"                   "WATER VAPOR MIXING RATIO AT THE SURFACE"      "kg kg-1"
state    real  dfi_QVG          ij      misc        1         -      r        "QVG_dfi"               "WATER VAPOR MIXING RATIO AT THE SURFACE"      "kg kg-1"
state    real  QCG              ij      misc        1         -      r        "QCG"                   "CLOUD WATER MIXING RATIO AT THE GROUND SURFACE"      "kg kg-1"
state    real  DEW              ij      misc        1         -      r        "DEW"                   "DEW MIXING RATIO AT THE SURFACE"              "kg kg-1"
state    real  SOILT1           ij      misc        1         -      i012rh   "SOILT1"                "TEMPERATURE INSIDE SNOW "    "K"
state    real  dfi_SOILT1       ij      misc        1         -      r        "SOILT1_dfi"            "TEMPERATURE INSIDE SNOW "    "K"
state    real  TSNAV            ij      misc        1         -      r        "TSNAV"                 "AVERAGE SNOW TEMPERATURE "                    "C"
state    real  dfi_TSNAV        ij      misc        1         -      r        "TSNAV_dfi"             "AVERAGE SNOW TEMPERATURE "                    "C"
state    real  REGIME           ij      misc        1         -      r        "REGIME"  "FLAGS: 1=Night/Stable, 2=Mechanical Turbulent, 3=Forced Conv, 4=Free Conv" ""
state    real  SNOWC            ij      misc        1         -     irhd=(interp_mask_land_field:lu_index)u=(copy_fcnm)  "SNOWC"  "FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"  ""
state    real  dfi_SNOWC        ij      misc        1         -      r        "SNOWC_dfi"             "FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"         ""
state    real  MAVAIL           ij      misc        1         -      r        "MAVAIL"                "SURFACE MOISTURE AVAILABILITY"                ""
                                                
state   real   tkesfcf          ij      misc        1         -      r        "tkesfcf"               "TKE AT THE SURFACE"                           "m2 s-2"      
state    real   sr             ij      dyn_em      1         -      irh         "sr" "fraction of frozen precipitation"
state    real   potevp         ij      dyn_em      1         -       rh         "potevp" "accumulated potential evaporation" "W m-2"
state    real   snopcx         ij      dyn_em      1         -       rh         "snopcx" "snow phase change heat flux" "W m-2"
state    real   soiltb         ij      dyn_em      1         -        h         "soiltb" "bottom soil temperature" "K"
                                                
state integer  STEPBL          -        misc        1         -      r        "STEPBL"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN PBL CALLS" ""      
state    real  taucldi         ikj      misc        1         -      r        "TAUCLDI"               "CLOUD OPTICAL THICKNESS FOR ICE"              ""
state    real  taucldc         ikj      misc        1         -      r        "TAUCLDC"               "CLOUD OPTICAL THICKNESS FOR WATER"            ""
                                                
state   real  defor11          ikj      misc        1         -     r         "defor11"               "DEFORMATION 11"              "s-1"      
state   real  defor22          ikj      misc        1         -     r         "defor22"               "DEFORMATION 22"              "s-1"      
state   real  defor12          ikj      misc        1         -     r         "defor12"               "DEFORMATION 12"              "s-1"      
state   real  defor33          ikj      misc        1         z     r         "defor33"               "DEFORMATION 33"              "s-1"      
state   real  defor13          ikj      misc        1         z     r         "defor13"               "DEFORMATION 13"              "s-1"      
state   real  defor23          ikj      misc        1         z     r         "defor23"               "DEFORMATION 23"              "s-1"      
state   real   xkmv            ikj      misc        1         -     r         "xkmv"                  "VERTICAL EDDY VISCOSITY"     "m2 s-1"      
state   real   xkmh            ikj      misc        1         -     r         "xkmh"                  "HORIZONTAL EDDY VISCOSITY"   "m2 s-1"      
state   real   xkhv            ikj      misc        1         -     r         "xkhv"                  "VERTICAL EDDY DIFFUSIVITY OF HEAT"                               "m2 s-1"      
state   real   xkhh            ikj      misc        1         -     r         "xkhh"                  "HORIZONTAL EDDY DIFFUSIVITY OF HEAT"                             "m2 s-1"      
state   real    div            ikj      misc        1         -     r         "div"                   "DIVERGENCE"                                                      "s-1"
state   real    BN2            ikj      misc        1         -     r         "BN2"                   "BRUNT-VAISALA FREQUENCY"                                         "s-2"
state  logical warm_rain        -       misc        1         -     -         "warm_rain"              "WARM_RAIN_LOGICAL"
state  logical adv_moist_cond   -       misc        1         -     -         "adv_moist_cond"         "ADVECT MOIST CONDENSATES LOGICAL"
state  integer save_topo_from_real -    dyn_em	    1         -    irh        "save_topo_from_real"    "1=original topo from real/0=topo modified by WRF"      "flag"

## FDDA variables

state integer  STEPFG            -        misc     1         -      r        "STEPFG"                "NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN FDDA GRID CALLS" ""
state    real  RUNDGDTEN         ikj      misc     1         X      r        "RUNDGDTEN"               "COUPLED X WIND TENDENCY DUE TO FDDA GRID NUDGING"  "Pa m s-2"
state    real  RVNDGDTEN         ikj      misc     1         Y      r        "RVNDGDTEN"               "COUPLED Y WIND TENDENCY DUE TO FDDA GRID NUDGING"  "Pa m s-2"
state    real  RTHNDGDTEN        ikj      misc     1         -      r        "RTHNDGDTEN"              "COUPLED THETA TENDENCY DUE TO FDDA GRID NUDGING"   "Pa K s-1"
state    real  RPHNDGDTEN        ikj      misc     1         -      r        "RPHNDGDTEN"              "COUPLED GEOPOTENTIAL TENDENCY DUE TO FDDA GRID NUDGING"   "Pa m2 s-3"
state    real  RQVNDGDTEN        ikj      misc     1         -      r        "RQVNDGDTEN"              "COUPLED Q_V TENDENCY DUE TO FDDA GRID NUDGING"     "Pa kg kg-1 s-1"
state    real  RMUNDGDTEN        ij       misc     1         -      r        "RMUNDGDTEN"              "MU TENDENCY DUE TO FDDA GRID NUDGING"     "Pa s-1"
state    real    -               ikjf     fdda3d   1         -     -    -
state    real  U_NDG_NEW         ikjf     fdda3d   1         X      i{10}r   "U_NDG_NEW"               "NEW X WIND FOR FDDA GRID NUDGING"  "m s-1"
state    real  V_NDG_NEW         ikjf     fdda3d   1         Y      i{10}r   "V_NDG_NEW"               "NEW Y WIND FOR FDDA GRID NUDGING"  "m s-1"
state    real  T_NDG_NEW         ikjf     fdda3d   1         -      i{10}r   "T_NDG_NEW"               "NEW PERT POT TEMP FOR FDDA GRID NUDGING"  "K"
state    real  Q_NDG_NEW         ikjf     fdda3d   1         -      i{10}r   "Q_NDG_NEW"               "NEW WATER VAPOR MIX RATIO FOR FDDA GRID NUDGING"  "kg/kg"
state    real  PH_NDG_NEW        ikjf     fdda3d   1         Z      i{10}r   "PH_NDG_NEW"              "NEW PERT GEOPOTENTIAL FOR FDDA GRID NUDGING"  "kg/kg"
state    real  U_NDG_OLD         ikjf     fdda3d   1         X      i{10}r   "U_NDG_OLD"               "OLD X WIND FOR FDDA GRID NUDGING"  "m s-1"
state    real  V_NDG_OLD         ikjf     fdda3d   1         Y      i{10}r   "V_NDG_OLD"               "OLD Y WIND FOR FDDA GRID NUDGING"  "m s-1"
state    real  T_NDG_OLD         ikjf     fdda3d   1         -      i{10}r   "T_NDG_OLD"               "OLD PERT POT TEMP FOR FDDA GRID NUDGING"  "K"
state    real  Q_NDG_OLD         ikjf     fdda3d   1         -      i{10}r   "Q_NDG_OLD"               "OLD WATER VAPOR MIX RATIO FOR FDDA GRID NUDGING"  "kg/kg"
state    real  PH_NDG_OLD        ikjf     fdda3d   1         Z      i{10}r   "PH_NDG_OLD"              "OLD PERT GEOPOTENTIAL FOR FDDA GRID NUDGING"  "kg/kg"
state    real    -               ivjf     fdda2d   1         Z     -    -
state    real  MU_NDG_NEW        ivjf     fdda2d   1         -      i{10}r      "MU_NDG_NEW"              "NEW PERT COLUMN DRY MASS FOR FDDA GRID NUDGING"  "Pa"
state    real  MU_NDG_OLD        ivjf     fdda2d   1         -      i{10}r      "MU_NDG_OLD"              "OLD PERT COLUMN DRY MASS FOR FDDA GRID NUDGING"  "Pa"
state    real  U10_NDG_OLD       ij         misc      1         X      i9r      "U10_NDG_OLD"             "OLD U10 FOR SURFACE FDDA GRID NUDGING"  "m s-1"
state    real  U10_NDG_NEW       ij         misc      1         X      i9r      "U10_NDG_NEW"             "NEW U10 FOR SURFACE FDDA GRID NUDGING"  "m s-1"
state    real  V10_NDG_OLD       ij         misc      1         Y      i9r      "V10_NDG_OLD"             "OLD V10 FOR SURFACE FDDA GRID NUDGING"  "m s-1"
state    real  V10_NDG_NEW       ij         misc      1         Y      i9r      "V10_NDG_NEW"             "NEW V10 FOR SURFACE FDDA GRID NUDGING"  "m s-1"
state    real  T2_NDG_OLD        ij         misc      1         -      i9r      "T2_NDG_OLD"              "OLD T2 FOR SURFACE FDDA GRID NUDGING"  "K"
state    real  T2_NDG_NEW        ij         misc      1         -      i9r      "T2_NDG_NEW"              "NEW T2 FOR SURFACE FDDA GRID NUDGING"  "K"
state    real  TH2_NDG_OLD       ij         misc      1         -      i9r      "TH2_NDG_OLD"             "OLD TH2 FOR SURFACE FDDA GRID NUDGING"  "K"
state    real  TH2_NDG_NEW       ij         misc      1         -      i9r      "TH2_NDG_NEW"             "NEW TH2 FOR SURFACE FDDA GRID NUDGING"  "K"
state    real  Q2_NDG_OLD        ij         misc      1         -      i9r      "Q2_NDG_OLD"              "OLD Q2 FOR SURFACE FDDA GRID NUDGING"  "kg kg-1"
state    real  Q2_NDG_NEW        ij         misc      1         -      i9r      "Q2_NDG_NEW"              "NEW Q2 FOR SURFACE FDDA GRID NUDGING"  "kg kg-1"
state    real  RH_NDG_OLD        ij         misc      1         -      i9r      "RH_NDG_OLD"              "OLD RH FOR SURFACE FDDA GRID NUDGING"  "%"
state    real  RH_NDG_NEW        ij         misc      1         -      i9r      "RH_NDG_NEW"              "NEW RH FOR SURFACE FDDA GRID NUDGING"  "%"
state    real  PSL_NDG_OLD       ij         misc      1         -      i9r      "PSL_NDG_OLD"             "OLD PSL FOR SURFACE FDDA GRID NUDGING"  "Pa"
state    real  PSL_NDG_NEW       ij         misc      1         -      i9r      "PSL_NDG_NEW"             "NEW PSL FOR SURFACE FDDA GRID NUDGING"  "Pa"
state    real  PS_NDG_OLD        ij         misc      1         -      i9r      "PS_NDG_OLD"              "OLD PS FOR SURFACE FDDA GRID NUDGING"  "Pa"
state    real  PS_NDG_NEW        ij         misc      1         -      i9r      "PS_NDG_NEW"              "NEW PS FOR SURFACE FDDA GRID NUDGING"  "Pa"
state    real  TOB_NDG_OLD       ij         misc      1         -      i9r      "TOB_NDG_OLD"             "OLD TOB FOR SURFACE FDDA GRID NUDGING"  ""
state    real  ODIS_NDG_OLD      ij         misc      1         -      i9r      "ODIS_NDG_OLD"            "OLD ODIS FOR SURFACE FDDA GRID NUDGING"  "km"
state    real  TOB_NDG_NEW       ij         misc      1         -      i9r      "TOB_NDG_NEW"             "NEW TOB FOR SURFACE FDDA GRID NUDGING"  ""
state    real  ODIS_NDG_NEW      ij         misc      1         -      i9r      "ODIS_NDG_NEW"            "NEW ODIS FOR SURFACE FDDA GRID NUDGING"  "km"
state    real  SN_NDG_NEW        ij         misc      1         -      i9r      "SN_NDG_NEW"              "NEW Snow Depth"  "m"
state    real  SN_NDG_OLD        ij         misc      1         -      i9r      "SN_NDG_OLD"              "OLD Snow Depth"  "m"


# flag for nest movement
state  logical moved            -       misc        1         -     -          

# special cam radiation restart arrays
state  real   abstot   ikcj   misc      1    Z   -     ""   ""  " "
state  real   absnxt   ikaj   misc      1    -   -     ""   ""  " "
state  real   emstot   ikj    misc      1    Z   -     ""   ""  " "

# model diagnostics
state   real  dpsdt            ij       misc        1         -     -         "dpsdt"           "surface pressure tendency"                         "Pa/sec"
state   real  dmudt            ij       misc        1         -     -         "dmudt"           "mu tendency"                                       "Pa/sec"
state   real  pk1m             ij       misc        1         -     -         "pk1m"            "surface pressure at previous step"                 "Pa"
state   real  mu_2m            ij       misc        1         -     -         "mu_2m"           "mu_2 at previous step"                             "Pa"

state   real    max_cfl         -       misc        1         -     -       "max_cfl"           "maximum CFL value in grid at a time" "-"

state   real  prec_acc_c       ij       misc        1         -     rh       "prec_acc_c"       "ACCUMULATED CUMULUS PRECIPITATION OVER prec_acc_dt PERIODS OF TIME"       "mm"
state   real  prec_acc_nc      ij       misc        1         -     rh       "prec_acc_nc"      "ACCUMULATED GRID SCALE  PRECIPITATION OVER prec_acc_dt PERIODS OF TIME"   "mm"
state   real  snow_acc_nc      ij       misc        1         -     rh       "snow_acc_nc"      "ACCUMULATED SNOW WATER EQUIVALENT OVER prec_acc_dt PERIODS OF TIME"       "mm"

# Ocean Mixed-Layer State Variables
state   real    TML            ij      misc         1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "TML"    "OCEAN MIXED-LAYER TEMPERATURE"   "K"
state   real    T0ML           ij      misc         1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "T0ML"   "INITIAL OCEAN MIXED-LAYER TEMPERATURE"   "K"
state   real    HML            ij      misc         1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "HML"    "OCEAN MIXED-LAYER DEPTH"   "m"
state   real    H0ML           ij      misc         1         -     i012rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "H0ML"   "INITIAL OCEAN MIXED-LAYER DEPTH"   "m"
state   real    HUML           ij      misc         1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "HUML"   "OCEAN MIXED-LAYER DEPTH * U-CURRENT"  " m2s-1 "
state   real    HVML           ij      misc         1         -     rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm)    "HVML"   "OCEAN MIXED-LAYER DEPTH * V-CURRENT"  " m2s-1 " 
state   real    TMOML          ij      misc         1         -     i012rhd=(interp_mask_water_field:lu_index)u=(copy_fcnm) "TMOML"  "OCEAN LAYER MEAN TEMPERATURE   "   "K"
#
#---------------------------------------------------------------------------------------------------------------------------------------
#                                               

######                                          
#                                               
# Variables that are set at run-time to control configuration  (namelist-settable)                                              
#                                               
#<Table>  <Type>  <Sym>                   <How set>          <Nentries>   <Default>                                             

# Time Control
rconfig   integer run_days                namelist,time_control		1             0       irh   "run_days"              "NUMBER OF DAYS TO RUN"
rconfig   integer run_hours               namelist,time_control		1             0       irh   "run_hours"             "NUMBER OF HOURS TO RUN"
rconfig   integer run_minutes             namelist,time_control		1             0       irh   "run_minutes"           "NUMBER OF MINUTES TO RUN"
rconfig   integer run_seconds             namelist,time_control		1             0       irh   "run_seconds"           "NUMBER OF SECONDS TO RUN"
rconfig   integer start_year              namelist,time_control 	max_domains    1993    irh   "start_year"            "4 DIGIT YEAR OF START OF MODEL" "YEARS"
rconfig   integer start_month             namelist,time_control		max_domains      03    irh   "start_month"           "2 DIGIT MONTH OF THE YEAR OF START OF MODEL, 1-12" "MONTHS"
rconfig   integer start_day               namelist,time_control		max_domains      13    irh   "start_day"             "2 DIGIT DAY OF THE MONTH OF START OF MODEL, 1-31" "DAYS"
rconfig   integer start_hour              namelist,time_control		max_domains      12    irh   "start_hour"            "2 DIGIT HOUR OF THE DAY OF START OF MODEL, 0-23" "HOURS"
rconfig   integer start_minute            namelist,time_control		max_domains      00    irh   "start_minute"          "2 DIGIT MINUTE OF THE HOUR OF START OF MODEL, 0-59" "MINUTES"
rconfig   integer start_second            namelist,time_control		max_domains      00    irh   "start_second"          "2 DIGIT SECOND OF THE MINUTE OF START OF MODEL, 0-59" "SECONDS"
rconfig   integer end_year                namelist,time_control		max_domains    1993    irh   "end_year"              "4 DIGIT YEAR OF END OF MODEL" "YEARS"
rconfig   integer end_month               namelist,time_control		max_domains      03    irh   "end_month"             "2 DIGIT MONTH OF THE YEAR OF END OF MODEL, 1-12" "MONTHS"
rconfig   integer end_day                 namelist,time_control		max_domains      14    irh   "end_day"               "2 DIGIT DAY OF THE MONTH OF END OF MODEL, 1-31" "DAYS"
rconfig   integer end_hour                namelist,time_control		max_domains      12    irh   "end_hour"              "2 DIGIT HOUR OF THE DAY OF END OF MODEL, 0-23" "HOURS"
rconfig   integer end_minute              namelist,time_control		max_domains      00    irh   "end_minute"            "2 DIGIT MINUTE OF THE HOUR OF END OF MODEL, 0-59" "MINUTES"
rconfig   integer end_second              namelist,time_control		max_domains      00    irh   "end_second"            "2 DIGIT SECOND OF THE MINUTE OF END OF MODEL, 0-59" "SECONDS"
rconfig   integer interval_seconds        namelist,time_control		1             43200    irh   "interval_seconds"      "SECONDS BETWEEN ANALYSIS AND BOUNDARY PERIODS" "SECONDS"
rconfig   logical input_from_file         namelist,time_control		max_domains    .false. irh    "input_from_file"      "T/F INPUT FOR THIS DOMAIN FROM A SEPARATE INPUT FILE"  ""
rconfig   integer fine_input_stream       namelist,time_control		max_domains    0       irh    "fine_input_stream"      "0 THROUGH 11, WHAT INPUT STREAM IS FINE GRID IC FROM"  ""
rconfig   logical input_from_hires        namelist,time_control		max_domains    .false. irh    "input_from_hires"     "T/F INPUT FOR THIS DOMAIN FROM USGS HI RES TERRAIN"  ""
rconfig   character rsmas_data_path       namelist,time_control		1              "."     -    "rsmas_data_path"      ""  ""
rconfig   logical all_ic_times            namelist,time_control		1              .false. irh    "all_ic_times"     "T/F WRITE ALL IC TIME PERIODS"  ""

include registry.io_boilerplate

rconfig   integer JULYR                   namelist,time_control		max_domains    0       h    "JULYR"                 ""      ""
rconfig   integer JULDAY                  namelist,time_control		max_domains    1       h    "JULDAY"                ""      ""
rconfig   real    GMT                     namelist,time_control		max_domains    0.      h    "GMT"           ""      ""
rconfig   character  input_inname      namelist,time_control		1  "wrfinput_d<domain>"          -     "name of input   infile"   ""      ""
rconfig   character  input_outname     namelist,time_control		1  "wrfinput_d<domain>"          -     "name of input   outfile"  ""      ""
rconfig   character  bdy_inname        namelist,time_control		1  "wrfbdy_d<domain>"            -     "name of boundary infile"  ""      ""
rconfig   character  bdy_outname       namelist,time_control		1  "wrfbdy_d<domain>"            -     "name of boundary outfile" ""      ""
rconfig   character  rst_inname        namelist,time_control		1  "wrfrst_d<domain>_<date>"     -     "name of restrt infile"    ""      ""
rconfig   character  rst_outname       namelist,time_control		1  "wrfrst_d<domain>_<date>"     -     "name of restrt outfile"   ""      ""
rconfig   logical write_input             namelist,time_control		1             .false. -    "write input data for 3dvar etc."              ""      ""
rconfig   logical write_restart_at_0h     namelist,time_control		1             .false. h    "write_restart_at_0h"              ""      ""
rconfig   logical adjust_output_times     namelist,time_control         1             .false. -    "adjust_output_times"
rconfig   logical adjust_input_times      namelist,time_control         1             .false. -    "adjust_input_times"

rconfig   integer diag_print              namelist,time_control         1              0      -    "print out time series of model diagnostics"
rconfig   logical nocolons                namelist,time_control         1             .false. -    "nocolons"
rconfig   logical cycling                 namelist,time_control         1             .false. -    "true for cycling (using wrfout file as input data)"

# DFI namelist
rconfig   integer dfi_opt                namelist,dfi_control   1       0     rh   "dfi_opt"                  ""      ""
rconfig   integer dfi_radar              namelist,dfi_control   1       0     rh   "dfi_radar"                "DFI radar switch"      ""
rconfig   integer dfi_nfilter            namelist,dfi_control   1       7     rh   "dfi_nfilter"              "Digital filter type"      ""
rconfig   logical dfi_write_filtered_input  namelist,dfi_control  1  .true.   rh   "dfi_write_filtered_input" "Write a wrfinput_filtered_d0n file?"      ""
rconfig   logical dfi_write_dfi_history  namelist,dfi_control   1   .false.   rh   "dfi_write_dfi_history"    "Write history files during filtering?"      ""
rconfig   integer dfi_cutoff_seconds     namelist,dfi_control   1    3600     rh   "dfi_cutoff_seconds"       "Digital filter cutoff time"      ""
rconfig   integer dfi_time_dim           namelist,dfi_control   1    1000     rh   "dfi_time_dim"             "MAX DIMENSION FOR HCOEFF"
rconfig   integer dfi_fwdstop_year       namelist,dfi_control   1    2004     rh   "dfi_fwdstop_year"         "4 DIGIT YEAR OF START OF DFI" "YEARS"
rconfig   integer dfi_fwdstop_month      namelist,dfi_control   1      03     rh   "dfi_fwdstop_month"        "2 DIGIT MONTH OF THE YEAR OF START OF DFI" "MONTHS"
rconfig   integer dfi_fwdstop_day        namelist,dfi_control   1      13     rh   "dfi_fwdstop_day"          "2 DIGIT DAY OF THE MONTH OF START OF DFI" "DAYS"
rconfig   integer dfi_fwdstop_hour       namelist,dfi_control   1      12     rh   "dfi_fwdstop_hour"         "2 DIGIT HOUR OF THE DAY OF START OF DFI" "HOURS"
rconfig   integer dfi_fwdstop_minute     namelist,dfi_control   1      00     rh   "dfi_fwdstop_minute"       "2 DIGIT MINUTE OF THE HOUR OF START OF DFI" "MINUTES"
rconfig   integer dfi_fwdstop_second     namelist,dfi_control   1      00     rh   "dfi_fwdstop_second"       "2 DIGIT SECOND OF THE MINUTE OF START OF DFI" "SECONDS"
rconfig   integer dfi_bckstop_year       namelist,dfi_control   1    2004     rh   "dfi_bckstop_year"         "4 DIGIT YEAR OF END OF DFI" "YEARS"
rconfig   integer dfi_bckstop_month      namelist,dfi_control   1      03     rh   "dfi_bckstop_month"        "2 DIGIT MONTH OF THE YEAR OF END OF DFI" "MONTHS"
rconfig   integer dfi_bckstop_day        namelist,dfi_control   1      14     rh   "dfi_bckstop_day"          "2 DIGIT DAY OF THE MONTH OF END OF DFI" "DAYS"
rconfig   integer dfi_bckstop_hour       namelist,dfi_control   1      12     rh   "dfi_bckstop_hour"         "2 DIGIT HOUR OF THE DAY OF END OF DFI" "HOURS"
rconfig   integer dfi_bckstop_minute     namelist,dfi_control   1      00     rh   "dfi_bckstop_minute"       "2 DIGIT MINUTE OF THE HOUR OF END OF DFI" "MINUTES"
rconfig   integer dfi_bckstop_second     namelist,dfi_control   1      00     rh   "dfi_bckstop_second"       "2 DIGIT SECOND OF THE MINUTE OF END OF DFI" "SECONDS"

# Domains
rconfig   integer time_step               namelist,domains	1             -       ih   "time_step"     
rconfig   integer time_step_fract_num     namelist,domains	1             0       ih   "time_step_fract_num"     
rconfig   integer time_step_fract_den     namelist,domains	1             1       ih   "time_step_fract_den"     
rconfig   integer time_step_dfi           namelist,domains      1             -       ih   "time_step_dfi"

rconfig   integer min_time_step           namelist,domains      max_domains   -1      h    "min_time_step"
rconfig   integer max_time_step           namelist,domains      max_domains   -1      h    "max_time_step"
rconfig   real    target_cfl              namelist,domains      max_domains  1.2      h    "target_cfl"
rconfig   integer max_step_increase_pct   namelist,domains      max_domains    5      h    "max_step_increase_pct"
rconfig   integer starting_time_step      namelist,domains      max_domains   -1      h    "starting_time_step"
rconfig   logical step_to_output_time     namelist,domains      1         .true.      h    "step_to_output_time"
rconfig   integer adaptation_domain       namelist,domains      1              1      h    "adaptation_domain"
rconfig   logical use_adaptive_time_step  namelist,domains      1         .false.     h    "use_adaptive_time_step"

rconfig   integer max_dom                 namelist,domains	1             1       irh  "max_dom"               ""      ""
rconfig   integer s_we                    namelist,domains	max_domains    1       irh    "s_we"          ""      ""
rconfig   integer e_we                    namelist,domains	max_domains    32      irh    "e_we"          ""      ""
rconfig   integer s_sn                    namelist,domains	max_domains    1       irh    "s_sn"          ""      ""
rconfig   integer e_sn                    namelist,domains	max_domains    32      irh    "e_sn"          ""      ""
rconfig   integer s_vert                  namelist,domains	max_domains    1       irh    "s_vert"                ""      ""
rconfig   integer e_vert                  namelist,domains	max_domains    31      irh    "e_vert"                ""      ""
rconfig   integer num_metgrid_levels      namelist,domains	1              27      irh    "num_metgrid_levels"                ""      ""
rconfig   integer num_metgrid_soil_levels namelist,domains	1             4        irh    "num_metgrid_soil_levels"               "number of input levels or layers in 3D sm, st, sw arrays"      ""
rconfig   real    p_top_requested         namelist,domains      1              5000    irh    "p_top_requested" "Pa"      ""
rconfig   integer interp_type             namelist,domains	1              2       irh    "interp_type"  "1=interp in pressure, 2=interp in LOG pressure"  ""
rconfig   integer vert_refine_fact        namelist,domains      1              1       irh    "vertical refinment factor for ndown"  ""
rconfig   integer extrap_type             namelist,domains	1              2       irh    "extrap_type"  "1= use 2 lowest levels, 2=constant"  ""
rconfig   integer t_extrap_type           namelist,domains	1              2       irh    "t_extrap_type"  "1=isothermal, 2=6.5 K/km, 3=adiabatic"   ""
rconfig   logical lowest_lev_from_sfc     namelist,domains	1             .false.  irh    "lowest_lev_from_sfc"                ""      ""
rconfig   logical use_levels_below_ground namelist,domains	1             .true.   irh    "use_levels_below_ground"   "T/F: use input data levels below input sfc pres" ""
rconfig   logical use_tavg_for_tsk        namelist,domains	1             .false.  irh    "use_tavg_for_tsk"   "T/F: use diurnal avg sfc temp for tsk" ""
rconfig   logical use_surface             namelist,domains	1             .true.   irh    "use_surface"   "T/F: use input surface level in interpolation" ""
rconfig   integer lagrange_order          namelist,domains	1              1       irh    "lagrange_order"   "1=linear, 2=quadratic vertical interpolation"      ""
rconfig   integer force_sfc_in_vinterp    namelist,domains	1              1       irh    "force_sfc_in_vinterp"   "number of eta levels forced to use sfc in vert interp"      ""
rconfig   real    zap_close_levels        namelist,domains	1              500     irh    "zap_close_levels"   "delta p where level is removed in vert interp"      "Pa"
rconfig   logical sfcp_to_sfcp            namelist,domains	1              .false. irh    "sfcp_to_sfcp"   "T/F use incoming sfc pres to compute new sfc pres"      "flag"
rconfig   logical adjust_heights          namelist,domains	1              .false. irh    "adjust_heights"   "T/F adjust pressure level input to match 500 mb height"      "flag"
rconfig   logical smooth_cg_topo          namelist,domains	1              .false. irh    "smooth_cg_topo"   "T/F smooth CG topo on boundarries" "flag"
rconfig   logical rh2qv_wrt_liquid        namelist,domains	1              .true.  irh    "rh2qv_wrt_liquid" "T = rh=>Qv assumes RH wrt liquid water, F = allows ice"
rconfig   real    qv_max_p_safe           namelist,domains      1              10000   irh    "qv_max_p_safe" "Threshhold pressure, Qv > flag set to value"      "Pa"
rconfig   real    qv_max_flag             namelist,domains      1              1.E-5   irh    "qv_max_flag" "Qv flag for max"  "kg kg{-1}"
rconfig   real    qv_max_value            namelist,domains      1              3.E-6   irh    "qv_max_value" "Qv value for max"  "kg kg{-1}"
rconfig   real    qv_min_p_safe           namelist,domains      1              110000  irh    "qv_min_p_safe" "Threshhold pressure, Qv < flag set to value"      "Pa"
rconfig   real    qv_min_flag             namelist,domains      1              1.E-6   irh    "qv_min_flag" "Qv flag for min"  "kg kg{-1}"
rconfig   real    qv_min_value            namelist,domains      1              1.E-6   irh    "qv_min_value" "Qv value for min"  "kg kg{-1}"
rconfig   real    dx                      namelist,domains     max_domains    200     h     "dx"        "X HORIZONTAL RESOLUTION"   "METERS"
rconfig   real    dy                      namelist,domains   	max_domains    200     h     "dy"        "Y HORIZONTAL RESOLUTION"   "METERS"
rconfig   integer grid_id                 namelist,domains	max_domains    1       irh    "id"            ""      ""
rconfig   logical grid_allowed            namelist,domains	max_domains    .true.  irh    "allowed"            ""      ""
rconfig   integer parent_id               namelist,domains	max_domains    0       h     "parent_id"             ""      ""
rconfig   integer i_parent_start          namelist,domains	max_domains    1       rh     "i_parent_start"                ""      ""
rconfig   integer j_parent_start          namelist,domains	max_domains    1       rh     "j_parent_start"                ""      ""
rconfig   integer parent_grid_ratio       namelist,domains	max_domains    1       h     "parent_grid_ratio"             ""      ""
rconfig   integer parent_time_step_ratio  namelist,domains	max_domains    1       h     "parent_time_step_ratio"                ""      ""
rconfig   integer feedback                namelist,domains	1    1       h     "feedback"          ""      ""
rconfig   integer smooth_option           namelist,domains	1    2       h     "smooth_option"          ""      ""
rconfig   integer blend_width             namelist,domains	1    5       h     "blend_width"  "width of cg fg terrain blended zone"      ""
rconfig   real    ztop                    namelist,domains	max_domains    15000.  h    "ztop"          ""      ""
rconfig   integer moad_grid_ratio         namelist,domains	max_domains    1       h     "moad_grid_ratio"               ""      ""
rconfig   integer moad_time_step_ratio    namelist,domains	max_domains    1       h     "moad_time_step_ratio"          ""      ""
rconfig   integer shw                     namelist,domains	max_domains    2       h     "stencil_half_width"   "HORIZONTAL INTERPOLATION STENCIL HALF-WIDTH"  "GRID POINTS"
rconfig   integer tile_sz_x               namelist,domains	1             0       -      "tile_sz_x"             ""      ""
rconfig   integer tile_sz_y               namelist,domains	1             0       -      "tile_sz_y"             ""      ""
rconfig   integer numtiles                namelist,domains	1             1       -      "numtiles"              ""      ""
rconfig   integer nproc_x                 namelist,domains	1             -1       -      "nproc_x"              "-1 means not set"      ""
rconfig   integer nproc_y		  namelist,domains	1             -1       -      "nproc_y"              "-1 means not set"      ""
rconfig   integer irand                   namelist,domains	1             0       -      "irand"           ""      ""
rconfig   real    dt                      derived              max_domains    2.      h     "dt"        "TEMPORAL RESOLUTION"      "SECONDS"
rconfig   integer   num_moves       namelist,domains    1                0
rconfig   integer   ts_buf_size     namelist,domains    1                200          -       "ts_buf_size"   "Size of time series buffer"
rconfig   integer   max_ts_locs     namelist,domains    1                5            -       "max_ts_locs"   "Maximum number of time series locations"
rconfig   integer   vortex_interval  namelist,domains   max_domains      15  -  "" "" "minutes"
rconfig   integer   max_vortex_speed namelist,domains   max_domains      40  -  "" "" "meters per second"
rconfig   integer   corral_dist     namelist,domains    max_domains      8
rconfig   integer   track_level     namelist,domains    1                50000
rconfig   real      time_to_move    namelist,domains    max_domains      0.  -  "" "" "minutes"
rconfig   integer   move_id         namelist,domains    max_moves        0
rconfig   integer   move_interval   namelist,domains    max_moves        999999999
rconfig   integer   move_cd_x       namelist,domains    max_moves        0
rconfig   integer   move_cd_y       namelist,domains    max_moves        0
rconfig   logical   swap_x          namelist,domains    max_domains    .false. rh    "swap_x"            ""      ""
rconfig   logical   swap_y          namelist,domains    max_domains    .false. rh    "swap_y"            ""      ""
rconfig   logical   cycle_x         namelist,domains    max_domains    .false. rh    "cycle_x"            ""      ""
rconfig   logical   cycle_y         namelist,domains    max_domains    .false. rh    "cycle_y"            ""      ""
rconfig   logical   reorder_mesh    namelist,domains    1              .false. rh    "reorder_mesh"       ""      ""
rconfig   logical   perturb_input   namelist,domains    1              .false. h     "" "" ""
rconfig   real      eta_levels      namelist,domains    max_eta        -1.
rconfig   real      max_dz          namelist,domains    1               1000.

# TC (tropical cyclone bogusing)
rconfig   logical insert_bogus_storm   namelist,tc     	1              .false. irh    "insert_bogus_storm"   "T/F for inserting a bogus typhoon"    "flag"
rconfig   logical remove_storm         namelist,tc     	1              .false. irh    "remove_storm"  "T/F for only removing the original typhoon"  "flag"
rconfig   integer num_storm            namelist,tc     	1              1       irh    "num_storm"     "Number of bogus typhoons"  ""
rconfig   real    latc_loc             namelist,tc     	max_bogus      -999.   irh    "latc_loc"      "center latitude of the bogus tyhoon"  "DEGREES"
rconfig   real    lonc_loc             namelist,tc     	max_bogus      -999.   irh    "lonc_loc"      "center longitude of the bogus tyhoon"  "DEGREES"
rconfig   real    vmax_meters_per_second   namelist,tc  max_bogus      -999.   irh    "vmax_meters_per_second"  "vmax of bogus storm in meters per second"  ""
rconfig   real    rmax                 namelist,tc     	max_bogus      -999.   irh    "rmax"  "maximum radius outward from storm center"  ""
rconfig   real    vmax_ratio           namelist,tc      max_bogus      -999.   irh    "vmax_ratio"  ""  ""


# Physics
rconfig   integer     mp_physics          namelist,physics	max_domains    0       rh       "mp_physics"            ""      ""
#rconfig   integer     milbrandt_ccntype   namelist,physics      max_domains    0       rh       "milbrandt select maritime(1)/continental(2)"  ""      ""
rconfig   integer     gsfcgce_hail        namelist,physics      1              0       rh       "gsfcgce select hail/graupel"  ""      ""
rconfig   integer     gsfcgce_2ice        namelist,physics      1              0       rh       "gsfcgce select 2ice/3ice"  ""      ""
rconfig   integer     progn               namelist,physics      max_domains    0       rh       "progn"                 ""      ""
rconfig   integer     ra_lw_physics       namelist,physics	max_domains    0       rh       "ra_lw_physics"         ""      ""
rconfig   integer     ra_sw_physics       namelist,physics	max_domains    0       rh       "ra_sw_physics"         ""      ""
rconfig   real    radt                    namelist,physics	max_domains    0       h    "RADT"          ""      ""
rconfig   real    naer                    namelist,physics      max_domains    1e9     rh   "NAER"          ""      ""
rconfig   integer     sf_sfclay_physics   namelist,physics	max_domains    0       rh       "sf_sfclay_physics"             ""      ""
rconfig   integer     sf_surface_physics  namelist,physics	max_domains    0       rh       "sf_surface_physics"            ""      ""
rconfig   integer     bl_pbl_physics      namelist,physics	max_domains    0       rh       "bl_pbl_physics"                ""      ""
rconfig   integer     sf_urban_physics    namelist,physics      max_domains    0       rh       "sf_urban_physics"     "activate urban model  0=no, 1=Noah_UCM 2=BEP_UCM"   ""
rconfig   real    BLDT                    namelist,physics	max_domains    0       h    "BLDT"          ""      ""
rconfig   integer     cu_physics          namelist,physics	max_domains    0       rh       "cu_physics"            ""      ""
rconfig   real    CUDT                    namelist,physics	max_domains    0       h    "CUDT"          ""      ""
rconfig   real    GSMDT                   namelist,physics	max_domains    0       h    "GSMDT"          ""      ""
rconfig   integer ISFFLX                  namelist,physics 	1             1       irh    "ISFFLX"                        ""      ""
rconfig   integer IFSNOW                  namelist,physics	1             0       irh    "IFSNOW"                        ""      ""
rconfig   integer ICLOUD                  namelist,physics	1             1       irh    "ICLOUD"                        ""      ""
rconfig   real    swrad_scat              namelist,physics	1             1       irh    "SWRAD_SCAT" "SCATTERING FACTOR IN SWRAD"      ""
rconfig   integer surface_input_source    namelist,physics	1             1       irh    "surface_input_source"          "1=static (fractional), 2=time dependent (dominant), 3=hybrid (not yet implemented)"      ""
rconfig   integer num_soil_layers         namelist,physics	1             5       irh    "num_soil_layers"               ""      ""
rconfig   integer num_urban_layers        namelist,physics	1            400       irh    "num_urban_layers"               ""      ""
rconfig   integer num_months              namelist,physics      1            12       irh    "num_months"               ""      ""
rconfig   integer maxiens                 namelist,physics	1             1       irh    "maxiens"                    ""      ""
rconfig   integer maxens                  namelist,physics	1             3       irh    "maxens"                    ""      ""
rconfig   integer maxens2                 namelist,physics	1             3       irh    "maxens2"                    ""      ""
rconfig   integer maxens3                 namelist,physics	1            16       irh    "maxens3"                    ""      ""
rconfig   integer ensdim                  namelist,physics	1            144      irh    "ensdim"                    ""      ""
rconfig   integer cugd_avedx              namelist,physics      1            1      irh    "cugd_avedx"                    ""      ""
rconfig   integer clos_choice             namelist,physics      1             0       rh    "clos_choice"                    ""      ""
rconfig   integer imomentum               namelist,physics      1             0       rh    "imomentum"                    "momentum transport in G3 scheme"      ""
rconfig   integer ishallow                namelist,physics      1             0       rh    "ishallow"  "shallow convection in G3 scheme"      ""
rconfig   integer     chem_opt            namelist,physics	max_domains    0       rh       "chem_opt"              ""      ""
rconfig   integer num_land_cat            namelist,physics	1            24       -      "num_land_cat"                  ""      ""
rconfig   integer num_soil_cat            namelist,physics	1            16       -      "num_soil_cat"                  ""      ""
rconfig   integer mp_zero_out             namelist,physics	1             0       -      "mp_zero_out"  "microphysics fields set to zero  0=no action taken, 1=all fields but Qv, 2=all fields including Qv"      "flag"
rconfig   real mp_zero_out_thresh         namelist,physics	1          1.e-8      -      "mp_zero_out_thresh"  "minimum threshold for non-Qv moist fields, below are set to zero"  "kg/kg"
rconfig   real    seaice_threshold        namelist,physics	1            271       h    "seaice_threshold"  "tsk below which which water points are set to sea ice for slab scheme"   "K"
rconfig   integer sst_update              namelist,physics	1            0         h    "sst_update"  "update sst from wrflowinp file  0=no, 1=yes"   ""
rconfig   integer sst_skin                namelist,physics      1            0         h    "sst_skin" "calculate sst skin temperature 0=no, 1=yes"   ""
rconfig   integer tmn_update              namelist,physics      1            0         h    "tmn_update" "update tmn from calculation  0=no, 1=yes"   ""
rconfig   logical usemonalb               namelist,physics      1            .false.   h    "usemonalb"   "use 2d field vs table values  false=table, True=2d"   ""
rconfig   logical rdmaxalb                namelist,physics      1            .true.    h     "rdmaxalb"    "false set it to table values"   ""
rconfig   logical rdlai2d                 namelist,physics      1            .false.   h     "rdlai2d"     "false set it to table values"   ""
rconfig   integer co2tf                   namelist,physics	1            1         -    "co2tf" "GFDL radiation co2 flag" ""
rconfig   integer ra_call_offset          namelist,physics	1            0         -    "ra_call_offset" "radiation call offset in timesteps (-1=old, 0=new offset)" ""
rconfig   real    cam_abs_freq_s          namelist,physics      1         21600.      -      "cam_abs_freq_s" "CAM radiation frequency for clear-sky longwave calculations" "s"
rconfig   integer levsiz                  namelist,physics      1             1       -      "levsiz" "Number of ozone data levels for CAM radiation (59)"  ""
rconfig   integer paerlev                 namelist,physics      1             1       -      "paerlev" "Number of aerosol data levels for CAM radiation (29)"  ""
rconfig   integer cam_abs_dim1            namelist,physics      1             1       -      "cam_abs_dim1" "dimension for absnxt in CAM radiation"  ""
rconfig   integer cam_abs_dim2            namelist,physics      1             1       -      "cam_abs_dim2" "dimension for abstot in CAM radiation"  ""
rconfig   integer lagday                  namelist,physics      1             1       -      "lagday"         ""      ""
rconfig   logical cu_rad_feedback         namelist,physics      max_domains   .false.  -     "feedback cumulus to radiation"  ""
rconfig   integer pxlsm_smois_init        namelist,physics      max_domains   1       irh    "PXLSM_SMOIS_INIT"    "Soil moisture initialization option 0-From analysis 1-From MAVAIL"   ""
rconfig   integer omlcall                 namelist,physics      1            0         h     "omlcall"     "activate simple ocean mixed layer model  0=no, 1=yes"   ""
rconfig   real    oml_hml0                namelist,physics      1            50        h     "oml_hml0"    "oml initial mixed layer depth value"   "m"
rconfig   real    oml_gamma               namelist,physics      1            0.14      h     "oml_gamma"   "oml deep water lapse rate"   "K m-1"
rconfig   integer isftcflx                namelist,physics      1            0         h     "isftcflx"    "switch to control sfc fluxes"   ""
rconfig   integer iz0tlnd                 namelist,physics      1            0         h     "iz0tlnd"    "switch to control land thermal roughness length"   ""
rconfig   real    shadlen                 namelist,physics      1            25000.    -     "shadow_length" "maximum length of orographic shadow" "m"
rconfig   integer slope_rad               namelist,physics      max_domains    0       -     "slope_rad"  "1: use slope-dependent radiation, 0:not" ""
rconfig   integer topo_shading            namelist,physics      max_domains    0       -     "topo_shading" "1: apply topographic shading to radiation, 0:not" ""
rconfig   integer no_mp_heating           namelist,physics      1              0       -     "no_mp_heating" "switch to turn of latent heating in mp schemes"   ""
rconfig   integer fractional_seaice       namelist,physics      1              0       -     "fractional_seaice" "Fractional sea-ice option"
rconfig   real    bucket_mm               namelist,physics      1             -1.      h     "bucket_mm" "bucket reset value for water accumulations -1: inactive"   ""
rconfig   real    bucket_J                namelist,physics      1             -1.      h     "bucket_J"  "bucket reset value for energy accumulations -1: inactive"   ""
rconfig   real    mp_tend_lim             namelist,physics      1             10.      -     "mp_tend_lim" "limit on temp tendency from mp latent heating"   "K/s"
rconfig   real    prec_acc_dt             namelist,physics      max_domains    0.      h     "prec_acc_dt" "bucket reset time interval between outputs for cumulus or grid scale precipitation"   "minutes"
rconfig   integer prec_acc_opt            derived               1              0       -     "prec_acc_opt" "option to output precip in a time window"          ""
rconfig   integer bucketr_opt             derived               1              0       -     "bucketr_opt"  "option to output water accum based on bucket_mm"          ""

rconfig   integer grav_settling           namelist,physics      max_domains  0         h    "grav_settling"     "activate gravitationalsettling of fog  0=no, 1=yes"

#FDDA namelist parameters
rconfig   real    FGDT                    namelist,fdda         max_domains    0       h        "FGDT"          ""      ""
rconfig   integer  FGDTZERO               namelist,fdda         max_domains    0       rh       "FGDTZERO"             ""      ""
rconfig   integer  grid_fdda              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  grid_sfdda             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_no_pbl_nudging_uv   namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_no_pbl_nudging_t    namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_no_pbl_nudging_ph   namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_no_pbl_nudging_q    namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_zfac_uv             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer   k_zfac_uv             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_zfac_t              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer   k_zfac_t              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_zfac_ph             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer   k_zfac_ph             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  if_zfac_q              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer   k_zfac_q              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   integer  dk_zfac_uv             namelist,fdda         max_domains    1       rh       "grid_fdda"            ""      ""
rconfig   integer  dk_zfac_t              namelist,fdda         max_domains    1       rh       "grid_fdda"            ""      ""
rconfig   integer  dk_zfac_ph             namelist,fdda         max_domains    1       rh       "grid_fdda"            ""      ""
rconfig   real        guv                 namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        guv_sfc             namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        gt                  namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        gt_sfc              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        gq                  namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        gq_sfc              namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real        gph                 namelist,fdda         max_domains    0       rh       "grid_fdda"            ""      ""
rconfig   real    dtramp_min              namelist,fdda         1              0       h        "grid_fdda"            ""      ""
rconfig   integer if_ramping              namelist,fdda         1              0       h        "grid_fdda"            ""      ""
rconfig   real    rinblw                  namelist,fdda         1              0       h        "grid_fdda"            ""      ""
rconfig   integer xwavenum                namelist,fdda         max_domains    0       rh       "grid_fdda"            "top wave number to nudge in x direction"      "" 
rconfig   integer ywavenum                namelist,fdda         max_domains    0       rh       "grid_fdda"            "top wave number to nudge in y direction"      ""

#Observational Nudging
rconfig   integer     obs_nudge_opt       namelist,fdda            max_domains    0       rh       "obs_nudge_opt"     "Obs-nudging flag for domain"          ""
rconfig   integer     max_obs             namelist,fdda            1              0       h        "max_obs"           "Maximum number of observations"       ""
rconfig   real        fdda_start          namelist,fdda            max_domains    0       rh       "fdda_start"        "Nudging start time for domain"        "min"
rconfig   real        fdda_end            namelist,fdda            max_domains    0       rh       "fdda_end"          "Nudging end time for domain"          "min"
rconfig   integer     obs_nudge_wind      namelist,fdda            max_domains    0       rh       "obs_nudge_wind"    "Wind-nudging flag for domain"         ""
rconfig   real        obs_coef_wind       namelist,fdda            max_domains    0       rh       "obs_coef_wind"     "Wind-nudging coeficient for domain"   "s-1"
rconfig   integer     obs_nudge_temp      namelist,fdda            max_domains    0       rh       "obs_nudge_temp"    "Temperature-nudging flag for domain"  ""
rconfig   real        obs_coef_temp       namelist,fdda            max_domains    0       rh       "obs_coef_temp"     "Temperature-nudging coef for domain"  "s-1"
rconfig   integer     obs_nudge_mois      namelist,fdda            max_domains    0       rh       "obs_nudge_mois"    "Moisture-nudging flag for domain"     ""
rconfig   real        obs_coef_mois       namelist,fdda            max_domains    0       rh       "obs_coef_mois"     "Moisture-nudging coef for domain"     "s-1"
rconfig   integer     obs_nudge_pstr      namelist,fdda            max_domains    0       rh       "obs_nudge_pstr"    "Not used"                             ""
rconfig   real        obs_coef_pstr       namelist,fdda            max_domains    0       rh       "obs_coef_pstr"     "Not used"                             ""
rconfig   integer     obs_no_pbl_nudge_uv namelist,fdda            max_domains    0       rh       "obs_no_pbl_nudge_uv" "1=no wind-nudging within pbl"          ""
rconfig   integer     obs_no_pbl_nudge_t  namelist,fdda            max_domains    0       rh       "obs_no_pbl_nudge_t"  "1=no temperature-nudging within pbl"   ""
rconfig   integer     obs_no_pbl_nudge_q  namelist,fdda            max_domains    0       rh       "obs_no_pbl_nudge_q"  "1=no moisture-nudging within pbl"      ""
rconfig   real        obs_nudgezfullr1_uv namelist,fdda            1              50      rh       "obs_nudgezfullr1_uv" "Vert infl full weight  height for LML obs, regime 1, winds"        ""
rconfig   real        obs_nudgezrampr1_uv namelist,fdda            1              50      rh       "obs_nudgezrampr1_uv" "Vert infl ramp-to-zero height for LML obs, regime 1, winds"        ""
rconfig   real        obs_nudgezfullr2_uv namelist,fdda            1              50      rh       "obs_nudgezfullr2_uv" "Vert infl full weight  height for LML obs, regime 2, winds"        ""
rconfig   real        obs_nudgezrampr2_uv namelist,fdda            1              50      rh       "obs_nudgezrampr2_uv" "Vert infl ramp-to-zero height for LML obs, regime 2, winds"        ""
rconfig   real        obs_nudgezfullr4_uv namelist,fdda            1              -5000   rh       "obs_nudgezfullr4_uv" "Vert infl full weight  height for LML obs, regime 4, winds"        ""
rconfig   real        obs_nudgezrampr4_uv namelist,fdda            1              50      rh       "obs_nudgezrampr4_uv" "Vert infl ramp-to-zero height for LML obs, regime 4, winds"        ""
rconfig   real        obs_nudgezfullr1_t  namelist,fdda            1              50      rh       "obs_nudgezfullr1_t" "Vert infl full weight  height for LML obs, regime 1, temperature"  ""
rconfig   real        obs_nudgezrampr1_t  namelist,fdda            1              50      rh       "obs_nudgezrampr1_t" "Vert infl ramp-to-zero height for LML obs, regime 1, temperature"  ""
rconfig   real        obs_nudgezfullr2_t  namelist,fdda            1              50      rh       "obs_nudgezfullr2_t" "Vert infl full weight  height for LML obs, regime 2, temperature"  ""
rconfig   real        obs_nudgezrampr2_t  namelist,fdda            1              50      rh       "obs_nudgezrampr2_t" "Vert infl ramp-to-zero height for LML obs, regime 2, temperature"  ""
rconfig   real        obs_nudgezfullr4_t  namelist,fdda            1              -5000   rh       "obs_nudgezfullr4_t" "Vert infl full weight  height for LML obs, regime 4, temperature"  ""
rconfig   real        obs_nudgezrampr4_t  namelist,fdda            1              50      rh       "obs_nudgezrampr4_t" "Vert infl ramp-to-zero height for LML obs, regime 4, temperature"  ""
rconfig   real        obs_nudgezfullr1_q  namelist,fdda            1              50      rh       "obs_nudgezfullr1_q" "Vert infl full weight  height for LML obs, regime 1, moisture"     ""
rconfig   real        obs_nudgezrampr1_q  namelist,fdda            1              50      rh       "obs_nudgezrampr1_q" "Vert infl ramp-to-zero height for LML obs, regime 1, moisture"     ""
rconfig   real        obs_nudgezfullr2_q  namelist,fdda            1              50      rh       "obs_nudgezfullr2_q" "Vert infl full weight  height for LML obs, regime 2, moisture"     ""
rconfig   real        obs_nudgezrampr2_q  namelist,fdda            1              50      rh       "obs_nudgezrampr2_q" "Vert infl ramp-to-zero height for LML obs, regime 2, moisture"     ""
rconfig   real        obs_nudgezfullr4_q  namelist,fdda            1              -5000   rh       "obs_nudgezfullr4_q" "Vert infl full weight  height for LML obs, regime 4, moisture"     ""
rconfig   real        obs_nudgezrampr4_q  namelist,fdda            1              50      rh       "obs_nudgezrampr4_q" "Vert infl ramp-to-zero height for LML obs, regime 4, moisture"     ""
rconfig   real        obs_nudgezfullmin   namelist,fdda            1                 50   rh       "obs_nudgezfullmin" "Minimum depth through which vertical influence fcn remains 1.0"     "m"
rconfig   real        obs_nudgezrampmin   namelist,fdda            1                 50   rh       "obs_nudgezrampmin" "Minimum depth through which vertical influence fcn decreases from 1.0 to 0.0" "m"
rconfig   real        obs_nudgezmax       namelist,fdda            1               3000   rh       "obs_nudgezmax" "Maximum depth in which vertical influence function is nonzero" "m"
rconfig   real        obs_sfcfact         namelist,fdda            1              1.0     h        "obs_sfcfact"       "Scale factor applied to time window for surface obs"     ""
rconfig   real        obs_sfcfacr         namelist,fdda            1              1.0     h        "obs_sfcfacr"       "Scale factor applied to horiz radius of influence for surface obs"     ""
rconfig   real        obs_dpsmx           namelist,fdda            1              7.5     h        "obs_dpsmx"         "Max pressure change allowed within horiz radius of influence"     "centibars"
rconfig   real        obs_rinxy           namelist,fdda            max_domains    0       rh       "obs_rinxy"         "Horizontal radius of influence"       "km"
rconfig   real        obs_rinsig          namelist,fdda            1              0       h        "obs_rinsig"        "Vertical radius of influence"         "sigma"
rconfig   real        obs_twindo          namelist,fdda            max_domains    0       rh       "obs_twindo"        "Half-period time window for nudging"  "hrs"
rconfig   integer     obs_npfi            namelist,fdda            1              0       h        "obs_npfi"          "Freq in cg timesteps for diag print"  ""
rconfig   integer     obs_ionf            namelist,fdda            max_domains    1       rh       "obs_ionf"          "Freq in cg timesteps for obs input and error calc"   ""
rconfig   integer     obs_idynin          namelist,fdda            1              0       h        "obs_idynin"        "Flag for dynamic initialization"      ""
rconfig   real        obs_dtramp          namelist,fdda            1              0       h        "obs_dtramp"        "Time period for ramping (idynin)"     "min"
rconfig   integer     obs_prt_max         namelist,fdda            1              1000    rh       "obs_prt_max"       "Maximum allowed obs entries in diagnostic printout"      ""
rconfig   integer     obs_prt_freq        namelist,fdda            max_domains    1000    rh       "obs_prt_freq"      "Frequency in obs index for diagnostic printout."   ""
rconfig   logical     obs_ipf_in4dob      namelist,fdda            1              .false. h        "obs_ipf_in4dob"    "Print obs input diagnostics"   ""
rconfig   logical     obs_ipf_errob       namelist,fdda            1              .false. h        "obs_ipf_errob"     "Print obs error diagnostics"   ""
rconfig   logical     obs_ipf_nudob       namelist,fdda            1              .false. h        "obs_ipf_nudob"     "Print obs nudge diagnostics"   ""
rconfig   logical     obs_ipf_init        namelist,fdda            1              .true.  h        "obs_ipf_init"      "Enable obs init warning messages"   ""

# Single-column model (SCM)
rconfig   integer scm_force               namelist,scm  1       0           rh   "scm_force"            "SCM forcing switch" ""
rconfig   real    scm_force_dx            namelist,scm  1       4000.       rh   "scm_force_dx"         "DX for SCM forcing" "m"
rconfig   integer num_force_layers        namelist,scm  1       8           rh   "num_force_layers"     "Number of SCM forcing layers" ""
rconfig   integer scm_lu_index            namelist,scm  1       2           rh   "scm_lu_index"         "SCM landuse index" ""
rconfig   integer scm_isltyp              namelist,scm  1       4           rh   "scm_isltyp"           "SCM soil category" ""
rconfig   real    scm_vegfra              namelist,scm  1       0.5         rh   "scm_vegfra"           "SCM vegetation fraction" ""
rconfig   integer scm_canwat              namelist,scm  1       0.0         rh   "scm_canwat"           "SCM canopy water" "kg m-2"
rconfig   real    scm_lat                 namelist,scm  1        36.605     rh   "scm_lat"              "SCM latitude" "degrees"
rconfig   real    scm_lon                 namelist,scm  1       -97.485     rh   "scm_lon"              "SCM longitude" "degrees"
rconfig   logical scm_th_adv              namelist,scm  1       .true.      rh   "scm_th_adv"           "Turn on theta advection in SCM"      ""
rconfig   logical scm_wind_adv            namelist,scm  1       .true.      rh   "scm_wind_adv"         "Turn on wind advection in SCM"      ""
rconfig   logical scm_qv_adv              namelist,scm  1       .true.      rh   "scm_qv_adv"           "Turn on qv advection in SCM"      ""
rconfig   logical scm_vert_adv            namelist,scm  1       .true.      rh   "scm_vert_adv"         "Turn on vertical advection in SCM"      ""

# Dynamics
# dynamics option (see package definitions, below)
rconfig   integer dyn_opt                 namelist,dynamics	1             - 
rconfig   integer rk_ord                  namelist,dynamics	1             3       irh   "rk_order"               ""      ""
rconfig   integer w_damping               namelist,dynamics	1             0       irh    "w_damping"             ""      ""
# diff_opt 1=old diffusion, 2=new
rconfig   integer diff_opt                namelist,dynamics	1             1       irh    "diff_opt"              ""      ""
# km_opt   1=old coefs, 2=tke, 3=Smagorinksy
rconfig   integer km_opt                  namelist,dynamics	1             1       irh    "km_opt"                ""      ""
# km_opt_dfi is needed for backward integration in dfi
rconfig   integer km_opt_dfi              namelist,dynamics	1             1       irh    "km_opt_dfi"                ""      ""
rconfig   integer damp_opt                namelist,dynamics	1             0       irh    "damp_opt"              ""      ""
rconfig   integer gwd_opt                 namelist,dynamics     1             0       irh    "gwd_opt"              ""      ""
rconfig   real    zdamp                   namelist,dynamics	max_domains    5000.   h    "zdamp"         ""      ""
rconfig   real    dampcoef                namelist,dynamics     max_domains    0.      h    "dampcoef"              ""      ""
rconfig   real    khdif                   namelist,dynamics	max_domains    0       h    "khdif"         ""      ""
rconfig   real    kvdif                   namelist,dynamics	max_domains    0       h    "kvdif"         ""      ""
rconfig   real    diff_6th_factor         namelist,dynamics     max_domains    0.12    h    "diff_6th_factor" "factor that controls rate of 6th-order numerical diffusion"
rconfig   integer diff_6th_opt            namelist,dynamics     max_domains    0      irh   "diff_6th_opt" "switch for 6th-order numerical diffusion"
rconfig   real    c_s                     namelist,dynamics	max_domains    0.25    h    "c_s"         "Smagorinsky coeff"      ""
rconfig   real    c_k                     namelist,dynamics	max_domains    0.15    h    "c_k"         "TKE coeff"      ""
rconfig   real    smdiv                   namelist,dynamics	max_domains    0.1     h    "smdiv"         ""      ""
rconfig   real    emdiv                   namelist,dynamics	max_domains    0.01    h    "emdiv"         ""      ""
rconfig   real    epssm                   namelist,dynamics	max_domains    .1      h    "epssm"         ""      ""
rconfig   logical non_hydrostatic         namelist,dynamics	max_domains  .true.   irh  "non_hydrostatic"    ""   ""
rconfig   integer time_step_sound         namelist,dynamics	max_domains    0       h     "time_step_sound"               ""      ""
rconfig   integer     h_mom_adv_order     namelist,dynamics	max_domains    5       rh       "h_mom_adv_order"               ""      ""
rconfig   integer     v_mom_adv_order     namelist,dynamics	max_domains    3       rh       "v_mom_adv_order"               ""      ""
rconfig   integer     h_sca_adv_order     namelist,dynamics	max_domains    5       rh       "h_sca_adv_order"               ""      ""
rconfig   integer     v_sca_adv_order     namelist,dynamics	max_domains    3       rh       "v_sca_adv_order"               ""      ""
rconfig   integer  moist_adv_opt          namelist,dynamics	max_domains    1       rh    "moist_adv_opt"         "positive-definite RK3 transport switch"      ""
rconfig   integer  moist_adv_dfi_opt      namelist,dynamics	max_domains    0       rh    "moist_adv_dfi_opt"    "positive-definite RK3 transport switch"      ""
rconfig   integer  chem_adv_opt           namelist,dynamics	max_domains    1       rh    "chem_adv_opt"          "positive-definite RK3 transport switch"      ""
rconfig   integer  tracer_adv_opt         namelist,dynamics	max_domains    1       rh    "tracer_adv_opt"        "positive-definite RK3 transport switch"      ""
rconfig   integer  scalar_adv_opt         namelist,dynamics	max_domains    1       rh    "scalar_adv_opt"        "positive-definite RK3 transport switch"      ""
rconfig   integer  tke_adv_opt            namelist,dynamics	max_domains    1       rh    "tke_adv_opt"           "positive-definite RK3 transport switch"      ""
rconfig   logical top_radiation           namelist,dynamics	max_domains    .false. rh    "top_radiation"         ""      ""
rconfig   integer mix_isotropic           namelist,dynamics	max_domains    0       h    "mix_isotropic"            "0=anistropic, 1=isotropic"      ""
rconfig   real    mix_upper_bound         namelist,dynamics	max_domains    0.1     h    "mix_upper_bound"          "non-dimensional limit"      ""
rconfig   logical top_lid                 namelist,dynamics     max_domains    .false. rh    "top_lid"               ""      ""
rconfig   real    tke_upper_bound         namelist,dynamics	max_domains    1000.   h    "tke_upper_bound"            ""      ""
rconfig   real    tke_drag_coefficient    namelist,dynamics	max_domains    0.      h    "tke_drag_coefficient"       ""      "dimensionless"
rconfig   real    tke_heat_flux           namelist,dynamics	max_domains    0.      h    "tke_heat_flux"              ""      "K m s-1"
rconfig   logical pert_coriolis           namelist,dynamics	max_domains  .false.  irh  "pert_coriolis"    ""   ""
rconfig   logical coriolis2d              namelist,dynamics	max_domains  .false.  irh  "coriolis2d"    ""   ""
rconfig   logical mix_full_fields         namelist,dynamics     max_domains  .false.  irh  "mix_full_field"   ""   ""
rconfig   real    base_pres               namelist,dynamics	1          100000.     h    "base_pres"  "Base state pressure - do not change (10^5 Pa), real only"      "Pa"
rconfig   real    base_temp               namelist,dynamics	1             290.     h    "base_temp"  "Base state sea level temperature, real only"      "K"
rconfig   real    base_lapse              namelist,dynamics	1              50.     h    "base_lapse" "Base state temperature difference between base pres and 1/e of atm depth - do not change, real only"      "K"
rconfig   real    iso_temp                namelist,dynamics	1               0.     h    "iso_temp"   "Isothermal temperature in stratosphere, real only"      "K"
rconfig   logical use_baseparam_fr_nml    namelist,dynamics	1            .false.  irh   "use_baseparam_fr_nml"    ""   ""
rconfig   real    fft_filter_lat          namelist,dynamics     1              91.     h    "fft_filter_lat"   ""   "grid latitude to start polar filter"
rconfig   logical rotated_pole            namelist,dynamics     1            .false.  irh   "rotated_pole"    ""   ""
rconfig   logical do_coriolis             namelist,dynamics	max_domains  .true.   irh  "do_coriolis"    ""   ""
rconfig   logical do_curvature            namelist,dynamics	max_domains  .true.   irh  "do_curvature"   ""   ""
rconfig   logical do_gradp                namelist,dynamics	max_domains  .true.   irh  "do_gradp"    ""   ""
rconfig   integer     tracer_opt          namelist,dynamics     max_domains    0       rh    "tracer_opt"          ""      ""

# Bdy_control
rconfig   integer spec_bdy_width          namelist,bdy_control		1             5       irh    "spec_bdy_width"                ""      ""
rconfig   integer spec_zone               namelist,bdy_control		1             1       irh    "spec_zone"                     ""      ""
rconfig   integer relax_zone              namelist,bdy_control		1             4       irh    "relax_zone"                    ""      ""
rconfig   logical specified               namelist,bdy_control	max_domains    .false. rh    "specified"             ""      ""
rconfig   logical constant_bc             namelist,bdy_control          1      .false. rh    "constant_bc"           ""      ""
rconfig   logical periodic_x              namelist,bdy_control	max_domains    .false. rh    "periodic_x"            ""      ""
rconfig   logical symmetric_xs            namelist,bdy_control	max_domains    .false. rh    "symmetric_xs"          ""      ""
rconfig   logical symmetric_xe            namelist,bdy_control	max_domains    .false. rh    "symmetric_xe"          ""      ""
rconfig   logical open_xs                 namelist,bdy_control	max_domains    .false. rh    "open_xs"               ""      ""
rconfig   logical open_xe                 namelist,bdy_control	max_domains    .false. rh    "open_xe"               ""      ""
rconfig   logical periodic_y              namelist,bdy_control	max_domains    .false. rh    "periodic_y"            ""      ""
rconfig   logical symmetric_ys            namelist,bdy_control	max_domains    .false. rh    "symmetric_ys"          ""      ""
rconfig   logical symmetric_ye            namelist,bdy_control	max_domains    .false. rh    "symmetric_ye"          ""      ""
rconfig   logical open_ys                 namelist,bdy_control	max_domains    .false. rh    "open_ys"               ""      ""
rconfig   logical open_ye                 namelist,bdy_control	max_domains    .false. rh    "open_ye"               ""      ""
rconfig   logical polar                   namelist,bdy_control	max_domains    .false. rh    "polar"                 ""      ""
rconfig   logical nested                  namelist,bdy_control	max_domains    .false. rh    "nested"                ""      ""
rconfig   real    spec_exp                namelist,bdy_control          1     0.      irh    "spec_exp"              ""      ""
rconfig   integer real_data_init_type     namelist,bdy_control		1                 1    irh   "real_data_init_type"   "REAL DATA INITIALIZATION OPTIONS: 1=SI, 2=MM5, 3=GENERIC" "PRE-PROCESSOR TYPES"

rconfig   integer background_proc_id      namelist,grib2 	        1     255    rh    "background_proc_id"    "Background processing id for grib2"  ""
rconfig   integer forecast_proc_id        namelist,grib2 	        1     255    rh    "forecast_proc_id"      "Analysis and forecast processing id for grib2"  ""
rconfig   integer production_status       namelist,grib2 	        1     255    rh    "production_status"     "Background processing id for grib2"  ""
rconfig   integer compression             namelist,grib2 	        1      40    rh    "compression"           "grib2 compression, 40 for JPEG2000 or 41 for PNG"  ""

# NAMELIST DERIVED
rconfig   integer nobs_ndg_vars           derived                       1         6       -        "num_ndg_vars"         "Number of nudging variables"          ""
rconfig   integer nobs_err_flds           derived                       1         9       -        "num_err_flds"         "Number of error fields"               ""
rconfig   real    cen_lat                 derived                  max_domains    0       -        "cen_lat"              "center latitude"      "degrees, negative is south"
rconfig   real    cen_lon                 derived                  max_domains    0       -        "cen_lon"              "central longitude"      "degrees, negative is west"
rconfig   real    truelat1                derived                  max_domains    0       -        "true_lat1"             "first standard parallel"      "degrees, negative is south"
rconfig   real    truelat2                derived                  max_domains    0       -        "true_lat2"             "second standard parallel"      "degrees, negative is south"
rconfig   real    moad_cen_lat            derived                  max_domains    0       -        "moad_cen_lat"             "center latitude of the most coarse grid"      "degrees, negative is south"
rconfig   real    stand_lon               derived                  max_domains    0       -        "stand_lon"             "standard longitude, parallel to j-direction, perpendicular to i-direction "      "degrees, negative is west"
rconfig   real    pole_lat                derived                  max_domains    0       -        "pole_lat"              "projection info: latitude of pole" "degrees, positive north"
rconfig   real    pole_lon                derived                  max_domains    0       -        "pole_lon"              "projection info: longitude of pole" "degrees, positive east"
rconfig   integer  FLAG_METGRID           derived                      1          0       -        "FLAG_METGRID"         "Flag in global attributes for metgrid data"
rconfig   integer  FLAG_SNOW              derived                      1          0       -        "FLAG_SNOW"             "Flag for snow in the global attributes for metgrid data"
rconfig   integer  FLAG_PSFC              derived                      1          0       -        "FLAG_PSFC"             "Flag for surface pressure in the global attributes for metgrid data"
rconfig   integer  FLAG_SM000010          derived                      1          0       -        "FLAG_SM000010"         "Flag for soil moisture in the global attributes for metgrid data"
rconfig   integer  FLAG_SM010040          derived                      1          0       -        "FLAG_SM010040"         "Flag for soil moisture in the global attributes for metgrid data"
rconfig   integer  FLAG_SM040100          derived                      1          0       -        "FLAG_SM040100"         "Flag for soil moisture in the global attributes for metgrid data"
rconfig   integer  FLAG_SM100200          derived                      1          0       -        "FLAG_SM100200"         "Flag for soil moisture in the global attributes for metgrid data"
rconfig   integer  FLAG_ST000010          derived                      1          0       -        "FLAG_ST000010"         "Flag for soil temperature in the global attributes for metgrid data"
rconfig   integer  FLAG_ST010040          derived                      1          0       -        "FLAG_ST000010"         "Flag for soil temperature  in the global attributes for metgrid data"
rconfig   integer  FLAG_ST040100          derived                      1          0       -        "FLAG_ST010040"         "Flag for soil temperature  in the global attributes for metgrid data"
rconfig   integer  FLAG_ST100200          derived                      1          0       -        "FLAG_ST100200"         "Flag for soil temperature  in the global attributes for metgrid data"
rconfig   integer  FLAG_SLP               derived                      1          0       -        "FLAG_SLP"              "Flag for sea level pressure in the global attributes for metgrid data"
rconfig   integer  FLAG_SOILHGT           derived                      1          0       -        "FLAG_SOILHGT"          "Flag for soil height in the global attributes for metgrid data"
rconfig   integer  FLAG_MF_XY             derived                      1          0       -        "FLAG_MF_XY"            "Flag for MF_XYin the global attributes for metgrid data"
rconfig   real    bdyfrq                  derived                  max_domains    0       -        "bdyfrq"               "lateral boundary input frequency"      "seconds"
rconfig   character mminlu                derived                  max_domains    " "     -        "mminlu"               "land use dataset"             ""
rconfig   integer iswater                 derived                  max_domains    0       -        "iswater"              "land use index of water"      "index category"
rconfig   integer islake                  derived                  max_domains    0       -        "islake"               "land use index of inland lake"      "index category"
rconfig   integer isice                   derived                  max_domains    0       -        "isice"                "land use index of ice"        "index category"
rconfig   integer isurban                 derived                  max_domains    0       -        "isurban"              "land use index for 'urban and built-up"     "index category"
rconfig   integer isoilwater              derived                  max_domains    0       -        "isoilwater"           "land use index of water for soil"        "index category"
rconfig   integer map_proj                derived                  max_domains    0       -        "map_proj"             "domain map projection"      "0=none (Cylindrical), 1=Lambert, 2=polar, 3=Mercator"
rconfig   integer use_wps_input           derived                       1         0       -        "use_wps_input"        "0/1 flag, using wps input"      "0=no, 1=yes"
rconfig   integer dfi_stage               derived                       1         3       -        "dfi_stage"            "current stage of DFI processing"      "0=DFI setup, 1=DFI backward integration, 2=DFI forward integration, 3=WRF forecast"
rconfig   integer mp_physics_dfi          derived                  max_domains   -1       -        "mp_physics_dfi"       ""      "-1 = no DFI and so no need to allocate DFI moist and scalar variables, >0 = running with DFI, so allocate DFI moist and scalar variables appropriate for selected microphysics package"

#
# Single dummy declaration to define a nodyn dyn option
state integer nodyn_dummy - dyn_nodyn -  -  -  "" "" ""      

#
#---------------------------------------------------------------------------------------------------------------------------------------
# Package Declarations
#                                               

#key      package       associated                package          associated 4d scalars
#         name          namelist choice           state vars

#package   passivec1     chem_opt==0                  -             
package   passiveqv     mp_physics==0                -             moist:qv
package   kesslerscheme mp_physics==1                -             moist:qv,qc,qr
package   linscheme     mp_physics==2                -             moist:qv,qc,qr,qi,qs,qg
package   wsm3scheme    mp_physics==3                -             moist:qv,qc,qr
package   wsm5scheme    mp_physics==4                -             moist:qv,qc,qr,qi,qs
package   etampnew      mp_physics==5                -             moist:qv,qc,qr,qs;scalar:qt;state:f_ice_phy,f_rain_phy,f_rimef_phy
package   wsm6scheme    mp_physics==6                -             moist:qv,qc,qr,qi,qs,qg
package   gsfcgcescheme mp_physics==7                -             moist:qv,qc,qr,qi,qs,qg
package   thompson      mp_physics==8                -             moist:qv,qc,qr,qi,qs,qg;scalar:qni,qnr
package   milbrandt2mom mp_physics==9                -             state:refl_10cm;moist:qv,qc,qr,qi,qs,qg,qh;scalar:qnc,qnr,qni,qns,qng,qnh
package   morr_two_moment  mp_physics==10            -             moist:qv,qc,qr,qi,qs,qg;scalar:qni,qns,qnr,qng
#package   milbrandt3mom mp_physics==12               -             moist:qv,qc,qr,qi,qs,qg,qh;scalar:qnc,qnr,qni,qns,qng,qnh,qzr,qzi,qzs,qzg,qzh
package   wdm5scheme    mp_physics==14               -             moist:qv,qc,qr,qi,qs;scalar:qnn,qnc,qnr
package   wdm6scheme    mp_physics==16               -             moist:qv,qc,qr,qi,qs,qg;scalar:qnn,qnc,qnr
package   thompson07    mp_physics==98               -             moist:qv,qc,qr,qi,qs,qg;scalar:qni

package   nodfimoist        mp_physics_dfi==-1       -             -
package   passiveqv_dfi     mp_physics_dfi==0        -             dfi_moist:dfi_qv
package   kesslerscheme_dfi mp_physics_dfi==1        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr
package   linscheme_dfi     mp_physics_dfi==2        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg
package   wsm3scheme_dfi    mp_physics_dfi==3        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr
package   wsm5scheme_dfi    mp_physics_dfi==4        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs
package   etampnew_dfi      mp_physics_dfi==5        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qs;dfi_scalar:dfi_qt
package   wsm6scheme_dfi    mp_physics_dfi==6        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg
package   gsfcgcescheme_dfi mp_physics_dfi==7        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg
package   thompson_dfi      mp_physics_dfi==8        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg;dfi_scalar:dfi_qni,dfi_qnr
package   milbrandt2mom_dfi mp_physics_dfi==9        -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg,dfi_qh;dfi_scalar:dfi_qnc,dfi_qnr,dfi_qni,dfi_qns,dfi_qng,dfi_qnh
package   morr_two_moment_dfi  mp_physics_dfi==10    -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg;dfi_scalar:dfi_qni,dfi_qns,dfi_qnr,dfi_qng
#package   milbrandt3mom_dfi mp_physics_dfi==12       -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg,dfi_qh;dfi_scalar:dfi_qnc,dfi_qnr,dfi_qni,dfi_qns,dfi_qng,dfi_qnh,dfi_qzr,dfi_qzi,dfi_qzs,dfi_qzg,dfi_qzh
package   wdm5scheme_dfi    mp_physics_dfi==14       -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs;dfi_scalar:dfi_qnn,dfi_qnc,dfi_qnr
package   wdm6scheme_dfi    mp_physics_dfi==16       -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg;dfi_scalar:dfi_qnn,dfi_qnc,dfi_qnr
package   thompson07_dfi    mp_physics_dfi==98       -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg;dfi_scalar:dfi_qni

package   noprogn       progn==0                     -             -
package   progndrop     progn==1                     -             scalar:qndrop;dfi_scalar:dfi_qndrop

package   rrtmscheme    ra_lw_physics==1             -             -
package   camlwscheme   ra_lw_physics==3             -             ozmixm:mth01,mth02,mth03,mth04,mth05,mth06,mth07,mth08,mth09,mth10,mth11,mth12;aerosolc:sul,sslt,dust1,dust2,dust3,dust4,ocpho,bcpho,ocphi,bcphi,bg,volc;state:acswupt,acswuptc,acswdnt,acswdntc,acswupb,acswupbc,acswdnb,acswdnbc,aclwupt,aclwuptc,aclwdnt,aclwdntc,aclwupb,aclwupbc,aclwdnb,aclwdnbc,i_acswupt,i_acswuptc,i_acswdnt,i_acswdntc,i_acswupb,i_acswupbc,i_acswdnb,i_acswdnbc,i_aclwupt,i_aclwuptc,i_aclwdnt,i_aclwdntc,i_aclwupb,i_aclwupbc,i_aclwdnb,i_aclwdnbc,swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc,lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc
package   rrtmg_lwscheme  ra_lw_physics==4           -             state:aclwupt,aclwuptc,aclwdnt,aclwdntc,aclwupb,aclwupbc,aclwdnb,aclwdnbc,i_aclwupt,i_aclwuptc,i_aclwdnt,i_aclwdntc,i_aclwupb,i_aclwupbc,i_aclwdnb,i_aclwdnbc,lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc
package   gfdllwscheme  ra_lw_physics==99            -             -
package   heldsuarez    ra_lw_physics==31            -             -

package   swradscheme   ra_sw_physics==1             -             -
package   gsfcswscheme  ra_sw_physics==2             -             -
package   camswscheme   ra_sw_physics==3             -             ozmixm:mth01,mth02,mth03,mth04,mth05,mth06,mth07,mth08,mth09,mth10,mth11,mth12;aerosolc:sul,sslt,dust1,dust2,dust3,dust4,ocpho,bcpho,ocphi,bcphi,bg,volc;state:acswupt,acswuptc,acswdnt,acswdntc,acswupb,acswupbc,acswdnb,acswdnbc,aclwupt,aclwuptc,aclwdnt,aclwdntc,aclwupb,aclwupbc,aclwdnb,aclwdnbc,i_acswupt,i_acswuptc,i_acswdnt,i_acswdntc,i_acswupb,i_acswupbc,i_acswdnb,i_acswdnbc,i_aclwupt,i_aclwuptc,i_aclwdnt,i_aclwdntc,i_aclwupb,i_aclwupbc,i_aclwdnb,i_aclwdnbc,swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc,lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc
package   rrtmg_swscheme  ra_sw_physics==4           -             state:acswupt,acswuptc,acswdnt,acswdntc,acswupb,acswupbc,acswdnb,acswdnbc,i_acswupt,i_acswuptc,i_acswdnt,i_acswdntc,i_acswupb,i_acswupbc,i_acswdnb,i_acswdnbc,swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc
package   gfdlswscheme  ra_sw_physics==99            -             -

package   sfclayscheme   sf_sfclay_physics==1        -             -
package   myjsfcscheme   sf_sfclay_physics==2        -             state:tke_myj
package   gfssfcscheme   sf_sfclay_physics==3        -             -
package   qnsesfcscheme  sf_sfclay_physics==4        -             -
package   mynnsfcscheme  sf_sfclay_physics==5        -             state:qke,tsq,qsq,cov
package   pxsfcscheme    sf_sfclay_physics==7        -             -

package   noahucmscheme  sf_urban_physics==1         -             state:trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,a_u_bep,a_v_bep,a_t_bep,a_q_bep,a_e_bep,b_u_bep,b_v_bep,b_t_bep,b_q_bep,b_e_bep,dlg_bep,dl_u_bep,sf_bep,vl_bep
package   bepscheme      sf_urban_physics==2         -             state:a_u_bep,a_v_bep,a_t_bep,a_q_bep,a_e_bep,b_u_bep,b_v_bep,b_t_bep,b_q_bep,b_e_bep,dlg_bep,dl_u_bep,sf_bep,vl_bep,trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d
package   bep_bemscheme  sf_urban_physics==3         -             state:a_u_bep,a_v_bep,a_t_bep,a_q_bep,a_e_bep,b_u_bep,b_v_bep,b_t_bep,b_q_bep,b_e_bep,dlg_bep,dl_u_bep,sf_bep,vl_bep,trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,tlev_urb3d,qlev_urb3d,tw1lev_urb3d,tw2lev_urb3d,tglev_urb3d,tflev_urb3d,sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,sfvent_urb3d,lfvent_urb3d,sfwin1_urb3d,sfwin2_urb3d,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d

package   slabscheme     sf_surface_physics==1       -             -
package   lsmscheme      sf_surface_physics==2       -             -
package   ruclsmscheme   sf_surface_physics==3       -             state:smfr3d,keepfr3dflag,soilt1
package   pxlsmscheme    sf_surface_physics==7       -             state:t2_ndg_new,q2_ndg_new,t2_ndg_old,q2_ndg_old

package   ysuscheme      bl_pbl_physics==1           -             -
package   myjpblscheme   bl_pbl_physics==2           -             state:tke_myj,el_myj
package   gfsscheme      bl_pbl_physics==3           -             -
package   qnsepblscheme  bl_pbl_physics==4           -             state:tke_myj,el_myj
package   mynnpblscheme2 bl_pbl_physics==5           -             state:qke,tsq,qsq,cov
package   mynnpblscheme3 bl_pbl_physics==6           -             state:qke,tsq,qsq,cov
package   acmpblscheme   bl_pbl_physics==7           -             -
package   boulacscheme   bl_pbl_physics==8           -             state:el_pbl,tke_pbl,wu_tur,wv_tur,wt_tur,wq_tur
package   mrfscheme      bl_pbl_physics==99          -             -

package   kfetascheme    cu_physics==1               -             -
package   bmjscheme      cu_physics==2               -             -
package   gdscheme       cu_physics==3               -             -
package   sasscheme      cu_physics==4               -             -
package   g3scheme       cu_physics==5               -             state:cugd_qvten,cugd_tten,cugd_qvtens,cugd_ttens,cugd_qcten,xmb_shallow,k22_shallow,kbcon_shallow,ktop_shallow
package   kfscheme       cu_physics==99              -             -

package   psufddagd      grid_fdda==1                -             fdda3d:u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,ph_ndg_old,u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,ph_ndg_new;fdda2d:mu_ndg_old,mu_ndg_new;state:rundgdten,rvndgdten,rthndgdten,rphndgdten,rqvndgdten,rmundgdten
package   psusfddagd     grid_sfdda==1               -             state:u10_ndg_old,v10_ndg_old,t2_ndg_old,th2_ndg_old,q2_ndg_old,rh_ndg_old,psl_ndg_old,ps_ndg_old,u10_ndg_new,v10_ndg_new,t2_ndg_new,th2_ndg_new,q2_ndg_new,rh_ndg_new,psl_ndg_new,ps_ndg_new,tob_ndg_old,odis_ndg_old,tob_ndg_new,odis_ndg_new

package   spnudging      grid_fdda==2               -              fdda3d:u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,ph_ndg_old,u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,ph_ndg_new;fdda2d:mu_ndg_old,mu_ndg_new;state:rundgdten,rvndgdten,rthndgdten,rphndgdten,rqvndgdten,rmundgdten

package   slopeopt       slope_rad==1               -              state:swnorm
package   gwdopt         gwd_opt==1                 -              state:var2d,oc12d,oa1,oa2,oa3,oa4,ol1,ol2,ol3,ol4
package   omlopt         omlcall==1                 -              state:tml,t0ml,hml,h0ml,huml,hvml,tmoml
package   scmopt         scm_force==1               -              state:z_force,z_force_tend,u_g,u_g_tend,v_g,v_g_tend,w_subs,w_subs_tend,th_upstream_x,th_upstream_x_tend,th_upstream_y,th_upstream_y_tend,qv_upstream_x,qv_upstream_x_tend,qv_upstream_y,qv_upstream_y_tend,u_upstream_x,u_upstream_x_tend,u_upstream_y,u_upstream_y_tend,v_upstream_x,v_upstream_x_tend,v_upstream_y,v_upstream_y_tend,tau_x,tau_x_tend,tau_y,tau_y_tend
package   prec_acc       prec_acc_opt==1            -              state:prec_acc_c,prec_acc_nc,snow_acc_nc
package   bucketropt     bucketr_opt==1             -              state:i_rainc,i_rainnc

package   restofwrf      use_wps_input==0            -             -

package   original         moist_adv_opt==0            -             -
package   positivedef    moist_adv_opt==1            -             -
package   monotonic      moist_adv_opt==2            -             -
package   tracer_test1  tracer_opt==2       -             tracer:tr17_1,tr17_2,tr17_3,tr17_4,tr17_5,tr17_6,tr17_7,tr17_8

package   dfi_setup      dfi_stage==0                -             -
package   dfi_bck        dfi_stage==1                -             -
package   dfi_fwd        dfi_stage==2                -             -
package   dfi_fst        dfi_stage==3                -             -

#package   digifilter     dfi_opt==1                  -             dfi_moist:dfi_qv,dfi_qc,dfi_qr,dfi_qi,dfi_qs,dfi_qg;dfi_scalar:dfi_qndrop,dfi_qni,dfi_qt,dfi_qns,dfi_qnr,dfi_qng;state:dfi_u,dfi_v,dfi_w,dfi_ph,dfi_phb,dfi_ph0,dfi_php,dfi_t,dfi_p,dfi_ww,dfi_mu,dfi_tke,dfi_pb,dfi_al,dfi_alt,dfi_rh,dfi_tten_rad
package   dfi_nodfi     dfi_opt==0                  -             -
package   dfi_dfl       dfi_opt==1                  -             state:dfi_u,dfi_v,dfi_w,dfi_ph,dfi_phb,dfi_ph0,dfi_php,dfi_t,dfi_p,dfi_ww,dfi_mu,dfi_tke,dfi_pb,dfi_al,dfi_alt,dfi_TSLB,dfi_SMOIS,dfi_SNOW,dfi_SNOWH,dfi_CANWAT,dfi_SMFR3D,dfi_KEEPFR3DFLAG,dfi_TSK,dfi_SOILT1,dfi_TSNAV,dfi_SNOWC,dfi_QVG,dfi_rh,dfi_tten_rad
package   dfi_ddfi      dfi_opt==2                  -             state:dfi_u,dfi_v,dfi_w,dfi_ph,dfi_phb,dfi_ph0,dfi_php,dfi_t,dfi_p,dfi_ww,dfi_mu,dfi_tke,dfi_pb,dfi_al,dfi_alt,dfi_TSLB,dfi_SMOIS,dfi_SNOW,dfi_SNOWH,dfi_CANWAT,dfi_SMFR3D,dfi_KEEPFR3DFLAG,dfi_TSK,dfi_SOILT1,dfi_TSNAV,dfi_SNOWC,dfi_QVG,dfi_rh,dfi_tten_rad
package   dfi_tdfi      dfi_opt==3                  -             state:dfi_u,dfi_v,dfi_w,dfi_ph,dfi_phb,dfi_ph0,dfi_php,dfi_t,dfi_p,dfi_ww,dfi_mu,dfi_tke,dfi_pb,dfi_al,dfi_alt,dfi_TSLB,dfi_SMOIS,dfi_SNOW,dfi_SNOWH,dfi_CANWAT,dfi_SMFR3D,dfi_KEEPFR3DFLAG,dfi_TSK,dfi_SOILT1,dfi_TSNAV,dfi_SNOWC,dfi_QVG,dfi_rh,dfi_tten_rad
ifdef RUC_CLOUD
package   realonly       use_wps_input==1            -             state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,qv_gc,qr_gc,qc_gc,qs_gc,qi_gc,qg_gc,qni_gc
endif
ifndef RUC_CLOUD
package   realonly       use_wps_input==1            -             state:u_gc,v_gc,t_gc,rh_gc,ght_gc,p_gc,xlat_gc,xlong_gc,ht_gc,tsk_gc,tavgsfc,tmn_gc,pslv_gc,sct_dom_gc,scb_dom_gc,greenfrac,albedo12m,pd_gc,psfc_gc,intq_gc,pdhs,sh_gc,qv_gc
endif


# only need to specify these once; not for every io_form* variable
package   io_intio    io_form_restart==1                     -             -
package   io_netcdf   io_form_restart==2                     -             -
# Placeholders for additional packages (we can go beyond zzz
# but that will entail modifying frame/module_io.F and frame/md_calls.m4)
# Please note these are placeholders; HDF has not been implemented yet.
package   io_hdf      io_form_restart==3                     -             -
package   io_phdf5    io_form_restart==4                     -             -
package   io_grib1    io_form_restart==5                     -             -
package   io_mcel     io_form_restart==6                     -             -
package   io_esmf     io_form_restart==7                     -             -
package   io_yyy      io_form_restart==8                     -             -
package   io_zzz      io_form_restart==9                     -             -
package   io_grib2    io_form_restart==10                    -             -
package   io_pnetcdf  io_form_restart==11                     -             -
                                                
#---------------------------------------------------------------------------------------------------------------------------------------
## communications                                               

### 8. Edit the Registry file and create a halo-exchange for x_1.

# Halo Update Communications

halo      HALO_EM_INIT_1 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,ph_1,ph_2
halo      HALO_EM_INIT_2 dyn_em 48:t_1,t_2,mu_1,mu_2,tke_1,tke_2,ww,phb
halo      HALO_EM_INIT_3 dyn_em 48:ph0,php,t_init,mub,mu0,p,al,alt,alb
halo      HALO_EM_INIT_4 dyn_em 48:pb,h_diabatic,msftx,msfty,msfux,msfuy,msfvx,msfvy,msfvx_inv,f,e,sina,cosa,ht,potevp,snopcx,soiltb,xlat,xlong,xlat_u,xlat_v,xlong_u,xlong_v,clat,clong
halo      HALO_EM_INIT_5 dyn_em 48:moist,chem,scalar,tracer
halo      HALO_EM_VINTERP_UV_1 dyn_em 8:pd_gc,pb
halo      HALO_EM_A dyn_em  8:ru,rv,rw,ww,php,alt,al,p,muu,muv,mut
halo      HALO_EM_PHYS_A  dyn_em 4:u_2,v_2
halo      HALO_EM_PHYS_PBL dyn_em        4:rublten,rvblten
halo      HALO_EM_FDDA dyn_em            4:rundgdten,rvndgdten
halo      HALO_EM_FDDA_SFC dyn_em 48:z,z_at_w,pblh,regime,znt,odis_ndg_old,odis_ndg_new
halo      HALO_EM_PHYS_DIFFUSION dyn_em  4:defor11,defor22,defor12,defor13,defor23,div,xkmv,xkmh,xkhv,xkhh,tke_1,tke_2
halo      HALO_EM_TKE_ADVECT_3 dyn_em 24:tke_2
halo      HALO_EM_TKE_ADVECT_5 dyn_em 48:tke_2
halo      HALO_EM_TKE_A dyn_em 4:ph_2,phb
halo      HALO_EM_TKE_B dyn_em 4:z,rdz,rdzw,zx,zy
halo      HALO_EM_TKE_C dyn_em 8:u_2,v_2,z,zx,zy,rdz,rdzw,ustm
halo      HALO_EM_TKE_D dyn_em 8:defor11,defor22,defor33,defor12,defor13,defor23,div
halo      HALO_EM_TKE_E dyn_em 8:xkmv,xkmh,xkhv,xkhh,BN2,moist
halo      HALO_EM_TKE_3 dyn_em   24:tke_1,tke_2
halo      HALO_EM_TKE_5 dyn_em   48:tke_1,tke_2
halo      HALO_EM_TKE_7 dyn_em   80:tke_1,tke_2
halo      HALO_EM_TKE_F dyn_em   48:tke_1,tke_2
halo      HALO_EM_TKE_OLD_E_5 dyn_em   48:tke_1
halo      HALO_EM_TKE_OLD_E_7 dyn_em   80:tke_1
halo      HALO_EM_B dyn_em 4:ph_2,al,p,t_1,t_save,u_save,v_save,mu_1,mu_2,mudf,php,alt,pb
halo      HALO_EM_B2 dyn_em 4:ru_tend,rv_tend
halo      HALO_EM_C dyn_em    4:u_2,v_2
halo      HALO_EM_C2 dyn_em    4:ph_2,al,p,mu_2,muts,mudf
halo      HALO_EM_D dyn_em    24:ru_m,rv_m,ww_m,mut,muts
halo      HALO_EM_D2_3 dyn_em 24:u_2,v_2,w_2,t_2,ph_2;24:moist,chem,tracer,scalar;4:mu_2,al
halo      HALO_EM_D2_5 dyn_em 48:u_2,v_2,w_2,t_2,ph_2;24:moist,chem,tracer,scalar;4:mu_2,al
halo      HALO_EM_D3_3 dyn_em 24:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,moist,chem,tracer,scalar;4:mu_1,mu_2
halo      HALO_EM_D3_5 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,moist,chem,tracer,scalar;4:mu_1,mu_2
halo      HALO_EM_E_3 dyn_em 24:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2
halo      HALO_EM_E_5 dyn_em 48:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2
halo      HALO_EM_MOIST_E_3 dyn_em 24:moist
halo      HALO_EM_MOIST_E_5 dyn_em 48:moist
halo      HALO_EM_MOIST_E_7 dyn_em 80:moist
halo      HALO_CUP_G3_IN dyn_em 24:RTHFTEN,RQVFTEN,w_2
halo      HALO_CUP_G3_OUT dyn_em 48:cugd_tten,cugd_qvten,cugd_ttens,cugd_qvtens,raincv
halo      HALO_EM_CHEM_E_3 dyn_em 24:chem
halo      HALO_EM_CHEM_E_5 dyn_em 48:chem
halo      HALO_EM_CHEM_E_7 dyn_em 80:chem
halo      HALO_EM_TRACER_E_3 dyn_em 24:tracer
halo      HALO_EM_TRACER_E_5 dyn_em 48:tracer
halo      HALO_EM_TRACER_E_7 dyn_em 80:tracer
halo      HALO_EM_SCALAR_E_3 dyn_em 24:scalar
halo      HALO_EM_SCALAR_E_5 dyn_em 48:scalar
halo      HALO_EM_SCALAR_E_7 dyn_em 80:scalar
halo      HALO_TOPOSHAD phys 24:ht_shad

halo      HALO_EM_MOIST_OLD_E_3 dyn_em 24:moist_old
halo      HALO_EM_MOIST_OLD_E_5 dyn_em 48:moist_old
halo      HALO_EM_MOIST_OLD_E_7 dyn_em 80:moist_old
halo      HALO_EM_CHEM_OLD_E_3 dyn_em 24:chem_old
halo      HALO_EM_CHEM_OLD_E_5 dyn_em 48:chem_old
halo      HALO_EM_CHEM_OLD_E_7 dyn_em 80:chem_old
halo      HALO_EM_TRACER_OLD_E_3 dyn_em 24:tracer_old
halo      HALO_EM_TRACER_OLD_E_5 dyn_em 48:tracer_old
halo      HALO_EM_TRACER_OLD_E_7 dyn_em 80:tracer_old
halo      HALO_EM_SCALAR_OLD_E_3 dyn_em 24:scalar_old
halo      HALO_EM_SCALAR_OLD_E_5 dyn_em 48:scalar_old
halo      HALO_EM_SCALAR_OLD_E_7 dyn_em 80:scalar_old

halo      HALO_EM_FEEDBACK   dyn_em 48:ht
halo      HALO_EM_HYDRO_UV   dyn_em 8:u_2,v_2

halo      HALO_EM_COUPLE_A   dyn_em 24:mub,mu_1,mu_2
period    PERIOD_EM_COUPLE_A dyn_em 2:mub,mu_1,mu_2
halo      HALO_EM_COUPLE_B   dyn_em 48:ph_1,ph_2,w_1,w_2,t_1,t_2,u_1,u_2,v_1,v_2,\
                                       moist,chem,TRACER,scalar
period    PERIOD_EM_COUPLE_B dyn_em 3:ph_1,ph_2,w_1,w_2,t_1,t_2,u_1,u_2,v_1,v_2,\
                                       moist,chem,tracer,scalar

## For moving nests
#halo      em_shift_halo_y  dyn_em 48:imask_nostag,imask_xstag,imask_ystag,imask_xystag,u_2,v_2,t_2
#halo      em_shift_halo_x  dyn_em 48:imask_nostag,imask_xstag,imask_ystag,imask_xystag,u_2,v_2,t_2

# For observational nudging
halo      HALO_OBS_NUDGE dyn_em 24:ph_2,p,uratx,vratx,tratx,kpbl

# Periodic Boundary Communications

period    PERIOD_BDY_EM_INIT dyn_em 3:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,t_init,phb,ph0,php,pb,al,alt,alb,mu_1,mu_2,mub,mu0,ht,msftx,msfty,msfux,msfuy,msfvx,msfvy,msfvx_inv,sina,cosa,e,f

# Monotonic, positive definite advection requires 4 values for moist, chem, tke, and scalar for periodic lateral boundaries

period    PERIOD_BDY_EM_MOIST dyn_em 4:moist
period    PERIOD_BDY_EM_CHEM dyn_em 4:chem
period    PERIOD_BDY_EM_TRACER dyn_em 4:tracer
period    PERIOD_BDY_EM_SCALAR dyn_em 4:scalar
period    PERIOD_BDY_EM_TKE dyn_em 4:tke_2
period    PERIOD_BDY_EM_MOIST2 dyn_em 4:moist
period    PERIOD_BDY_EM_CHEM2 dyn_em 4:chem
period    PERIOD_BDY_EM_TRACER2 dyn_em 4:tracer
period    PERIOD_BDY_EM_SCALAR2 dyn_em 4:scalar
period    PERIOD_BDY_EM_MOIST_OLD dyn_em 4:moist_old
period    PERIOD_BDY_EM_CHEM_OLD dyn_em 4:chem_old
period    PERIOD_BDY_EM_TRACER_OLD dyn_em 4:tracer_old
period    PERIOD_BDY_EM_SCALAR_OLD dyn_em 4:scalar_old
period    PERIOD_BDY_EM_TKE_OLD dyn_em 4:tke_1

period    PERIOD_BDY_EM_E dyn_em 2:u_2,v_2,ht
period    PERIOD_EM_HYDRO_UV dyn_em 1:u_2,v_2
period    PERIOD_BDY_EM_A dyn_em 2:ru,rv,rw,ww,php,alt,p,muu,muv,mut,ph_2,al
period    PERIOD_BDY_EM_A1  dyn_em 3:rdzw,rdz,z,zx,zy,ustm
period    PERIOD_BDY_EM_PHY_BC dyn_em 2:rublten,rvblten,xkmh,xkmv,xkhh,xkhv,div,defor11,defor22,defor12,defor13,defor23,defor33,tke_2
period    PERIOD_BDY_EM_FDDA_BC dyn_em 2:rundgdten,rvndgdten
period    PERIOD_BDY_EM_B dyn_em 2:ru_tend,rv_tend,ph_2,al,p,t_1,t_save,u_save,v_save,mu_1,mu_2,mudf,php,alt,pb
period    PERIOD_BDY_EM_B3 dyn_em 2:ph_2,al,p,mu_2,muts,mudf
period    PERIOD_BDY_EM_B2 dyn_em 2:ru_tend,rv_tend
period    PERIOD_BDY_EM_C dyn_em 2:u_2,u_save,v_2,v_save,t_2,t_save,muv,msfvx,msfvy,muu,msfux,msfuy,msfvx_inv
period    PERIOD_BDY_EM_D dyn_em 3:u_2,v_2,w_2,t_2,ph_2,mu_2,tke_2
period    PERIOD_BDY_EM_D3 dyn_em 3:u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,mu_1,mu_2
period    PERIOD_EM_DA dyn_em 2:ru_m,rv_m,ww_m,mut,muts

#
#swap SWAP_ETAMP_NEW  dyn_em 1:dz8w,p_phy,pi_phy,rho,th_phy,moist,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,RAINNC,RAINNCV,SR,LOWLYR
#swap SWAP_WSM3       dyn_em 1:th_phy,moist,w_2,rho,pi_phy,p_phy,dz8w,rainnc,rainncv
#cycle CYCLE_TEST       dyn_em 1:xlong

##

# FDDA (Observational-nudging) Variables
typedef fdob_type integer domain_tot   # total number of domains to apply obs-nudging
typedef fdob_type integer IEODI        # end of obs data flag for current model step
typedef fdob_type integer IWTSIG       # flag for nudging on pressure surfaces
typedef fdob_type integer NSTAT        # number of obs stations used to nudge current model step
typedef fdob_type integer NSTAW        # number of obs stations within current time window
typedef fdob_type integer KTAUR        # restart model step
typedef fdob_type integer LEVIDN(max_domains)   # level of nest
typedef fdob_type integer REFPRT(max_domains)   # reference obs index for diagnostic printout
typedef fdob_type real    WINDOW       # time window half-period for nudging (in minutes) 
typedef fdob_type real    RTLAST       # time in hours of last obs used in current model step
typedef fdob_type real    DATEND       # time in minutes after which data are asuumed to have ended
typedef fdob_type logical NUDGE_UV_PBL # Flag for wind nudging within the PBL
typedef fdob_type logical NUDGE_T_PBL  # Flag for temperature nudging within the PBL
typedef fdob_type logical NUDGE_Q_PBL  # Flag for moisture nudging within the PBL
typedef fdob_type real    SFCFACT      # scale factor applied to time window for surface obs
typedef fdob_type real    SFCFACR      # scale factor applied to horiz radius of influence for surface obs
typedef fdob_type real    RINFMN       # minimum radius of influence
typedef fdob_type real    RINFMX       # maximum radius of influence
typedef fdob_type real    PFREE        # pressure level (cb) where terrain effect becomes small
typedef fdob_type real    DCON         # 1/DPSMX
typedef fdob_type real    DPSMX        # max pres change (cb) allowed within infl range of surf obs 
typedef fdob_type real    TFACI        # scale factor used for ramp-down in dynamic initialization
typedef fdob_type real    KNOWN_LAT    # Latitude  of origin point (i,j)=(1,1)
typedef fdob_type real    KNOWN_LON    # Longitude of origin point (i,j)=(1,1)
typedef fdob_type character SDATE      # domain starting date (YYYY-MM-DD_hh:mm:ss)
typedef fdob_type real    XTIME_AT_REST # xtime at restart time
typedef fdob_type real    VIF_UV(6)    # Vertical influence function parameters for wind nudging
typedef fdob_type real    VIF_T(6)     # Vertical influence function parameters for temperature nudging
typedef fdob_type real    VIF_Q(6)     # Vertical influence function parameters for moisture nudging
typedef fdob_type real    VIF_FULLMIN  # Minimum depth through which vert infl fcn remains 1.0 (m)
typedef fdob_type real    VIF_RAMPMIN  # Minimum depth through which vif decreases 1.0 to 0.0 (m)
typedef fdob_type real    VIF_MAX      # Maximum depth in which vif is nonzero (m)

# table entries are of the form
#      <Table>  <Type>  <Sym>                <Dims>   <Use>   <NumTLev> <Stagger> <IO>     <DNAME>             <DESCRIP>     <UNITS>
#Grid variables
typedef fdob_type real    varobs         {nndgvar}{obs} -         1        -       -       "varobs"          "observational values in each variable"
typedef fdob_type real    errf              h{obs}      -         1        -       -       "errf"            "errors between model and obs values"
typedef fdob_type real    timeob             {obs}      -         1        -       -       "timeob"          "model times for each observation"          "hours"
typedef fdob_type real    nlevs_ob           {obs}      -         1        -       -       "nlevs_ob"        "numbers of levels in sounding obs"
typedef fdob_type real    lev_in_ob          {obs}      -         1        -       -       "lev_in_ob"       "level in sounding-type obs"
typedef fdob_type real    plfo               {obs}      -         1        -       -       "plfo"            "index for type of obs-platform"
typedef fdob_type real    elevob             {obs}      -         1        -       -       "elevob"          "elevation of observation"                  "meters"
typedef fdob_type real    rio                {obs}      -         1        -       -       "rio"             "west-east grid coordinate"
typedef fdob_type real    rjo                {obs}      -         1        -       -       "rjo"             "south-north grid coordinate"
typedef fdob_type real    rko                {obs}      -         1        -       -       "rko"             "vertical grid coordinate"
typedef fdob_type integer obsprt               [        -         1        -       -       "obsprt"          "obs index for diagnostic printout"
typedef fdob_type real    latprt               [        -         1        -       -       "latprt"          "obs latitude for diagnostic printout"
typedef fdob_type real    lonprt               [        -         1        -       -       "lonprt"          "obs longitude for diagnostic printout"
typedef fdob_type real    mlatprt              [        -         1        -       -       "mlatprt"         "model latitude at obs location"
typedef fdob_type real    mlonprt              [        -         1        -       -       "mlonprt"         "model longitude at obs location"
typedef fdob_type integer stnidprt       {obsstid}{[}   -         1        -       -       "stnidprt"        "obs station id for diagnostic printout"
typedef fdob_type real    base_state           k        -         1        -       -       "base_state"      "base-state height on half (mass) levels"   "meters"

state fdob_type fdob - -

# xpose variables for polar fft
state    real   t_xxx          ikjx    -           1        -
state    real   u_xxx          ikjx    -           1        X
state    real   ru_xxx         ikjx    -           1        X
state    real   v_xxx          ikjx    -           1        Y
state    real   rv_xxx         ikjx    -           1        Y
state    real   w_xxx          ikjx    -           1        Z
state    real   ww_xxx         ikjx    -           1        Z
state    real   ph_xxx         ikjx    -           1        Z
state    real   dum_yyy        ikjy    -           1        -
state    real   fourd_xxx      ikjx    -           1        -
state    real   clat_xxx       ijx     -           1        -
state    real   ht_xxx         ijx     -           1        -
state    real   mf_xxx         ijx     -           1        -

xpose XPOSE_POLAR_FILTER_TOPO dyn_em t_init,t_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_T  dyn_em t_2,t_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_U  dyn_em u_2,u_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_RU dyn_em ru_m,ru_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_V  dyn_em v_2,v_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_RV dyn_em rv_m,rv_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_W  dyn_em w_2,w_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_WW dyn_em ww_m,ww_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_PH dyn_em ph_2,ph_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_MOIST dyn_em moist,fourd_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_CHEM dyn_em chem,fourd_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_TRACER dyn_em tracer,fourd_xxx,dum_yyy
xpose XPOSE_POLAR_FILTER_SCALAR dyn_em scalar,fourd_xxx,dum_yyy

# xpose variables for spectral nudging
state    real   dif_analysis     ikj     -           1
state    real   dif_xxx          ikjx    -           1     
state    real   dif_yyy          ikjy    -           1     

xpose XPOSE_SPECTRAL_NUDGING dyn_em dif_analysis,dif_xxx,dif_yyy
##

include registry.fire
include registry.avgflx

