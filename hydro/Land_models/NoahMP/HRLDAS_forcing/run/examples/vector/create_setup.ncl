load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"  

begin

; manually create a SETUP file from observations
;    file will contain both initial conditions and location information

initial_date = "2012020100"        ; time you want to start simulation
number_stations = 3

setup_filename = "HRLDAS_setup_"+initial_date+"_d1"

system("if [ -e "+setup_filename+".nc ]; then rm -f "+setup_filename+ ".nc;fi")

outfile = addfile(setup_filename+".nc","c")
 filedimdef(outfile,(/"Time","south_north",    "west_east","soil_layers_stag"/), \
                    (/     1,            1,number_stations,                 4/), \
		    (/  True,        False,          False,             False/))

; Define some temporary variables

 vartmp = new((/1,1,number_stations/),"float")
 vartmp!0 = "Time"
 vartmp!1 = "south_north"
 vartmp!2 = "west_east"
 
 var3tmp = new((/1,4,1,number_stations/),"float")
 var3tmp!0 = "Time"
 var3tmp!1 = "soil_layers_stag"
 var3tmp!2 = "south_north"
 var3tmp!3 = "west_east"
 
 ivartmp = new((/1,1,number_stations/),"integer")
 ivartmp!0 = "Time"
 ivartmp!1 = "south_north"
 ivartmp!2 = "west_east"

 varztmp = new((/1,4/),"float")
 varztmp!0 = "Time"
 varztmp!1 = "soil_layers_stag"
 
; Set up the time

 timestring = "2012-02-01_00:00:00"
 timetemp = stringtochar(timestring)
 timechar = new((/1,19/),"character")
 timechar(0,:) = timetemp(0:18)
 timechar!0 = "Time"
 timechar!1 = "DateStrLen"
 
   outfile->Times = timechar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up the location-specific information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 vartmp(0,0,:) = (/31.62,31.04,30.78/)     ;  set the latitude
   vartmp@units = "degrees_north"
 
   outfile->XLAT = vartmp

 vartmp(0,0,:) = (/-95.08,-97.25,-96.72/)  ;  set the longitude
   vartmp@units = "degrees_east"
  
   outfile->XLONG = vartmp

 vartmp(0,0,:) = (/290.0,290.0,290.0/)     ;  set the deep soil temperature
   vartmp@units = "K"
 
   outfile->TMN = vartmp

 vartmp(0,0,:) = (/100.0,100.0,100.0/)     ;  set the elevation
   vartmp@units = "m"
 
   outfile->HGT = vartmp

 vartmp(0,0,:) = (/0.0,0.0,0.0/)           ;  set the seaice (shouldn't be used since the points
   vartmp@units = ""                       ;   should be land
                                           
   outfile->SEAICE = vartmp

 vartmp(0,0,:) = (/0.0,0.0,0.0/)           ;  for future use
   vartmp@units = ""
 
   outfile->MAPFAC_MX = vartmp

 vartmp(0,0,:) = (/0.0,0.0,0.0/)           ;  for future use
   vartmp@units = ""
 
   outfile->MAPFAC_MY = vartmp

 vartmp(0,0,:) = (/95.0,95.0,95.0/)        ;  set the maximum annual vegetation fraction
   vartmp@units = "%"
 
   outfile->SHDMAX = vartmp

 vartmp(0,0,:) = (/95.0,95.0,95.0/)        ;  set the minimum annual vegetation fraction
   vartmp@units = "%"
 
   outfile->SHDMIN = vartmp

 vartmp(0,0,:) = (/2.0,2.0,2.0/)           ;  set the LAI (will initialized dynamic vegetation)
   vartmp@units = "m^2/m^2"
 
   outfile->LAI = vartmp

 ivartmp(0,0,:) = (/1,1,1/)                ;  set the landmask (1 = land)
   ivartmp@units = ""
 
 outfile->XLAND = ivartmp

 ivartmp(0,0,:) = (/10,10,10/)             ;  set the land class types of each point
   ivartmp@units = ""                      ;  needs to be based on either MODIS or USGS class scheme
					   ;  make sure you set these below for consistency:
					   ;      outfile@MMINLU
					   ;      outfile@ISWATER
					   ;      outfile@ISURBAN
					   ;      outfile@ISICE
  
   outfile->IVGTYP = ivartmp

 ivartmp(0,0,:) = (/12,9,3/)               ;  set the soil texture class types of each point
   ivartmp@units = ""
 
 outfile->ISLTYP = ivartmp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  State initialization here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 vartmp(0,0,:) = (/0.0,0.0,0.0/)           ;  set the initial SWE
   vartmp@units = "kg/m^2"
 
 outfile->SNOW = vartmp

 vartmp(0,0,:) = (/0.0,0.0,0.0/)           ;  set the initial canopy water content
   vartmp@units = "kg/m^2"
 
 outfile->CANWAT = vartmp

 vartmp(0,0,:) = (/300.0,300.0,300.0/)     ;  set the initial surface temperature
   vartmp@units = "K"
 
 outfile->TSK = vartmp
 
 varztmp(0,:) = (/0.1,0.3,0.6,1.0/)        ;  set the soil layer thicknesses
   varztmp@units = "m"
 
 outfile->DZS = varztmp
 
 varztmp(0,:) = (/0.05,0.25,0.7,1.5/)      ;  set the soil layer nodes
   varztmp@units = "m"
 
 outfile->ZS = varztmp
 
 var3tmp(0,0,0,:) = (/300.0,300.0,300.0/)  ;  set the initial soil temperature: layer 1
 var3tmp(0,1,0,:) = (/300.0,300.0,300.0/)  ;  set the initial soil temperature: layer 2
 var3tmp(0,2,0,:) = (/300.0,300.0,300.0/)  ;  set the initial soil temperature: layer 3
 var3tmp(0,3,0,:) = (/300.0,300.0,300.0/)  ;  set the initial soil temperature: layer 4
   var3tmp@units = "K"
 
 outfile->TSLB = var3tmp

 var3tmp(0,0,0,:) = (/0.2,0.2,0.2/)        ;  set the initial soil moisture: layer 1
 var3tmp(0,1,0,:) = (/0.2,0.2,0.2/)        ;  set the initial soil moisture: layer 2
 var3tmp(0,2,0,:) = (/0.2,0.2,0.2/)        ;  set the initial soil moisture: layer 3
 var3tmp(0,3,0,:) = (/0.2,0.2,0.2/)        ;  set the initial soil moisture: layer 4
   var3tmp@units = "m^3/m^3"
 
 outfile->SMOIS = var3tmp

;;;;;;;;;;;;;;;;;;;;
; Global attributes, many are not used and should be removed
;;;;;;;;;;;;;;;;;;;;


outfile@TITLE = "OUTPUT FROM VECTOR CREATION SCRIPTS: m.barlage v20150608" ;
outfile@DX        = 1000.0  ; not used
outfile@DY        = 1000.0  ; not used
outfile@TRUELAT1  = 45.0    ; not used
outfile@TRUELAT2  = 45.0    ; not used
outfile@STAND_LON = 45.0    ; not used
outfile@MAP_PROJ  = 1       ; not used
outfile@GRID_ID   = 1       ; used for grid labeling
outfile@ISWATER   = 17      ; water type in land classification       (17 for MODIS; 16 for USGS)
outfile@ISURBAN   = 13      ; urban type in land classification       (13 for MODIS;  1 for USGS)
outfile@ISICE     = 15      ; snow/ice type in land classification    (15 for MODIS; 24 for USGS)
outfile@MMINLU    = "MODIFIED_IGBP_MODIS_NOAH"  ; land classification (USGS or MODIFIED_IGBP_MODIS_NOAH)


 system("mv "+setup_filename+".nc "+setup_filename)  ; get rid of the .nc

end
