;_______________________________________________________________________________________
;To run the script type:
;       ncl plotfmt_nc.ncl {input file}
;
;       e.g.
;               ncl plotfmt_nc.ncl 'inputFILE="FILE:2005-06-01_00.nc"'
;_______________________________________________________________________________________

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

; Make sure we have a datafile to work with
  if (.not. isvar("inputFILE") ) then
    print(" ")
    print(" ### MUST SUPPLY a inputFILE ### ")
    print(" ")
    print("     Something like: ")
    print("     ncl plotfmt_nc.ncl inputFILE=FILE:2005-06-01_00.nc")
    print("          REMEMBER TO ADD QUOTES" )
    print("          Refer to the information at the top of this file for more info and syntax" )
    exit
  end if

  inFILE = addfile(inputFILE,"r")

; We generate plots, but what kind do we prefer?
  type = "x11"
; type = "pdf"
; type = "ps"
; type = "ncgm"

  outf = "plotfmt_nc"
  wks = gsn_open_wks(type,"outf")


  vNames = getfilevarnames (inFILE) 
  nNames = dimsizes (vNames)   

  res = True
  res@cnFillOn = True
  res@gsnSpreadColors  = True
  res@lbLabelAutoStride= True

  do n=0,nNames-1  

     print("VAR = " + vNames(n) )
     var = inFILE->$vNames(n)$

     dims = dimsizes(var)
     lat = new ( dims(0), float)
     lon = new ( dims(1), float)
     lon@units = "degrees_east"
     lat@units = "degrees_north"
     lat(0) = var@startlat
     lon(0) = var@startlon

     if (var@projection .eq. 0) then         ;Cylindrical Equidistant
       do i=1,dims(0)-1
         lat(i) = lat(i-1) + var@deltalat
       end do
       do i=1,dims(1)-1
         lon(i) = lon(i-1) + var@deltalon
       end do
     end if

     if (var@projection .eq. 1) then          ; Mercator
       res1 = True
       res1@MAP_PROJ  = 3
       res1@TRUELAT1  = var@truelat1
       res1@DX        = var@dx*1000.
       res1@DY        = var@dy*1000.
       res1@REF_LAT   = lat(0)
       res1@REF_LON   = lon(0)
       res1@POLE_LAT  = 90.0
       res1@POLE_LON  =  0.0
       res1@LATINC    = 0.0
       res1@LONINC    = 0.0
       res1@KNOWNI    = 1.0
       res1@KNOWNJ    = 1.0
       loc = wrf_ij_to_ll (var@nx,var@ny,res1)
       
       res@gsnAddCyclic = False
       res@mpLimitMode = "Corners"
       res@mpLeftCornerLatF = lat(0)
       res@mpLeftCornerLonF = lon(0)
       res@mpRightCornerLatF = loc(1)
       res@mpRightCornerLonF = loc(0)
       res@tfDoNDCOverlay = True
       res@mpProjection = "mercator"
     end if

     if (var@projection .eq. 3) then          ; Lambert Conformal
       res1 = True
       res1@MAP_PROJ  = 1
       res1@TRUELAT1  = var@truelat1
       res1@TRUELAT2  = var@truelat2
       res1@STAND_LON = var@xlonc
       res1@DX        = var@dx*1000.
       res1@DY        = var@dy*1000.
       res1@REF_LAT   = lat(0)
       res1@REF_LON   = lon(0)
       res1@POLE_LAT  = 90.0
       res1@POLE_LON  =  0.0
       res1@LATINC    = 0.0
       res1@LONINC    = 0.0
       res1@KNOWNI    = 1.0
       res1@KNOWNJ    = 1.0
       loc = wrf_ij_to_ll (var@nx,var@ny,res1)

       res@gsnAddCyclic = False
       res@mpLimitMode = "Corners"
       res@mpLeftCornerLatF = lat(0)
       res@mpLeftCornerLonF = lon(0)
       res@mpRightCornerLatF = loc(1)
       res@mpRightCornerLonF = loc(0)
       res@tfDoNDCOverlay = True
       res@mpProjection = "LambertConformal"
       res@mpLambertParallel1F = var@truelat1
       res@mpLambertParallel2F = var@truelat2
       res@mpLambertMeridianF = var@xlonc
     end if

     if (var@projection .eq. 4) then        ;Gaussian
       delta = 2.*(var@startlat)/(2.*var@nlats-1)
       if (var@startlat .ge. 80.) then
         delta = -1.0*delta
       end if
       do i=1,dims(0)-1
         lat(i) = lat(i-1) + delta
       end do
       do i=1,dims(1)-1
         lon(i) = lon(i-1) + var@deltalon
       end do
     end if

     var!1 = "lon"
     var!0 = "lat"
     var&lon = lon
     var&lat = lat

     var@description = var@level +"   "+ var@description

     ;map = gsn_csm_contour_map_ce(wks,var,res)
     map = gsn_csm_contour_map(wks,var,res)
     delete(lat)
     delete(lon)
     delete(var)

  end do


   
end
