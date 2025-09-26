
;   Script display location of model domains
;   Only works for ARW domains
;   Only works for NCL versions 6.2 or later
;   Reads namelist file directly

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin
;

; Check the version of NCL
  version = systemfunc("ncl -V")
  if(version.lt.6.2) then
    print("You need NCL V6.2 or later to run this script. Try running plotgrids.ncl. Stopping now...")
    return
  end if

; We generate plots, but what kind do we prefer?
  type = "x11"
; type = "pdf"
; type = "ps"
; type = "ncgm"
  wks = gsn_open_wks(type,"wps_show_dom")

; read the following namelist file
  filename = "namelist.wps"

; Set the colors to be used
  colors = (/"white","black","White","ForestGreen","DeepSkyBlue","Red","Blue"/)
  gsn_define_colormap(wks, colors)  


; Set some map information ; line and text information
  mpres = True
  mpres@mpFillOn = True
  mpres@mpFillColors  = (/"background","DeepSkyBlue","ForestGreen","DeepSkyBlue", "transparent"/)
  mpres@mpDataBaseVersion           = "Ncarg4_1"
  mpres@mpGeophysicalLineColor      = "Black"
  mpres@mpGridLineColor             = "Black"
  mpres@mpLimbLineColor             = "Black"
  mpres@mpNationalLineColor         = "Black"
  mpres@mpPerimLineColor            = "Black"
  mpres@mpUSStateLineColor          = "Black"
;  mpres@mpOutlineBoundarySets       = "AllBoundaries"
  ;mpres@mpGridSpacingF              = 45
  mpres@tiMainString                = " WPS Domain Configuration  "

  lnres = True 
  lnres@gsLineThicknessF = 2.5
  lnres@domLineColors    = (/ "white", "Red" , "Red" , "Blue" /)

  txres = True
  txres@txFont = "helvetica-bold"
  ;txres@txJust = "BottomLeft"
  txres@txJust = "TopLeft"
  txres@txPerimOn = False
  txres@txFontHeightF = 0.015

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Do not change anything between the ";;;;;" lines

  maxdom = 21
  nvar = 19
  parent_idn = new (maxdom,integer)
  parent_grid_ration = new (maxdom,integer)
  i_parent_startn = new (maxdom,integer)
  j_parent_startn = new (maxdom,integer)
  e_wen = new (maxdom,integer)
  e_snn = new (maxdom,integer)
  plotvar = new((/maxdom,nvar/),float)
  plotvar@_FillValue = -999.0

  plotvar = wrf_wps_read_nml(filename)

  mpres@max_dom = floattointeger(plotvar(0,0))
  mpres@dx = plotvar(0,1)
  mpres@dy = plotvar(0,2)
  if (.not.ismissing(plotvar(0,3))) then
    mpres@ref_lat = plotvar(0,3)
  else
    mpres@ref_lat = 0.0
  end if
  if (.not.ismissing(plotvar(0,4))) then
    mpres@ref_lon = plotvar(0,4)
  else
    mpres@ref_lon = 0.0
  end if
  if (.not.ismissing(plotvar(0,5))) then
    mpres@ref_x = plotvar(0,5)
  end if
  if (.not.ismissing(plotvar(0,6))) then
    mpres@ref_y = plotvar(0,6)
  end if
  mpres@truelat1 = plotvar(0,7)
  mpres@truelat2 = plotvar(0,8)
  mpres@stand_lon = plotvar(0,9)
  mproj_int = plotvar(0,10)
  mpres@pole_lat = plotvar(0,11)
  mpres@pole_lon = plotvar(0,12)

  do i = 0,maxdom-1
    parent_idn(i) = floattointeger(plotvar(i,13))
    parent_grid_ration(i) = floattointeger(plotvar(i,14))
    i_parent_startn(i) = floattointeger(plotvar(i,15))
    j_parent_startn(i) = floattointeger(plotvar(i,16))
    e_wen(i) = floattointeger(plotvar(i,17))
    e_snn(i) = floattointeger(plotvar(i,18))
  end do


  if(mpres@max_dom .gt. 1) then
    do i = 1,mpres@max_dom-1

      ;Making sure edge is nested grid is at least 5 grid points from mother domain.
      if(i_parent_startn(i) .lt. 5) then
        print("Warning: Western edge of grid must be at least 5 grid points from mother domain!")
      end if
      if(j_parent_startn(i) .lt. 5) then
        print("Warning: Southern edge of grid must be at least 5 grid points from mother domain!")
      end if
      pointwe = (e_wen(i)-1.)/parent_grid_ration(i)
      pointsn = (e_snn(i)-1.)/parent_grid_ration(i)
      gridwe = e_wen(parent_idn(i)-1)-(pointwe+i_parent_startn(i))
      gridsn = e_snn(parent_idn(i)-1)-(pointsn+j_parent_startn(i))
      if(gridwe .lt. 5) then
        print("Warning: Eastern edge of grid must be at least 5 grid points from mother domain!")
      end if
      if(gridsn .lt. 5) then
        print("Warning: Northern edge of grid must be at least 5 grid points from mother domain!")
      end if

      ;Making sure nested grid is fully contained in mother domain.
      gridsizewe = (((e_wen(parent_idn(i)-1)-4)-i_parent_startn(i))*parent_grid_ration(i))-(parent_grid_ration(i)-1)
      gridsizesn = (((e_snn(parent_idn(i)-1)-4)-j_parent_startn(i))*parent_grid_ration(i))-(parent_grid_ration(i)-1)
      if(gridwe .lt. 5) then
        print("Warning: Inner nest (domain = " + (i+1) + ") is not fully contained in mother nest (domain = " + parent_idn(i) + ")!")
        print("For the current setup of mother domain = " + parent_idn(i) + ", you can only have a nest of size " + gridsizewe + "X" + gridsizesn + ". Stopping Program!")
        exit
      end if
      if(gridsn .lt. 5) then
        print("Warning: Inner nest (domain = " + (i+1) + ") is not fully contained in mother nest (domain = " + parent_idn(i) + ")!")
        print("For the current setup of mother domain = " + parent_idn(i) + ", you can only have a nest of size " + gridsizewe + "X" + gridsizesn + ". Stopping Program!")
        exit
      end if

      ;Making sure the nest ends at a mother grid domain point.
      pointwetrunc = decimalPlaces(pointwe,0,False)
      pointsntrunc = decimalPlaces(pointsn,0,False)
      if((pointwe-pointwetrunc) .ne. 0.) then
        nest_we_up = (ceil(pointwe)*parent_grid_ration(i))+1
        nest_we_dn = (floor(pointwe)*parent_grid_ration(i))+1
        print("Nest does not end on mother grid domain point. Try " + nest_we_dn + " or " + nest_we_up + ".")
      end if
      if((pointsn-pointsntrunc) .ne. 0.) then
        nest_sn_up = (ceil(pointsn)*parent_grid_ration(i))+1
        nest_sn_dn = (floor(pointsn)*parent_grid_ration(i))+1
        print("Nest does not end on mother grid domain point. Try " + nest_sn_dn + " or " + nest_sn_up + ".")
      end if

    end do
  end if

  mpres@parent_id = parent_idn(0:mpres@max_dom-1)
  mpres@parent_grid_ratio = parent_grid_ration(0:mpres@max_dom-1)
  mpres@i_parent_start = i_parent_startn(0:mpres@max_dom-1)
  mpres@j_parent_start = j_parent_startn(0:mpres@max_dom-1)
  mpres@e_we = e_wen(0:mpres@max_dom-1)
  mpres@e_sn = e_snn(0:mpres@max_dom-1)

  if(mproj_int .eq. 1) then
    mpres@map_proj = "lambert"
    mpres@pole_lat = 0.0
    mpres@pole_lon = 0.0
  else if(mproj_int .eq. 2) then
    mpres@map_proj = "mercator"
    mpres@pole_lat = 0.0
    mpres@pole_lon = 0.0
  else if(mproj_int .eq. 3) then
    mpres@map_proj = "polar"
    mpres@pole_lat = 0.0
    mpres@pole_lon = 0.0
  else if(mproj_int .eq. 4) then
    mpres@map_proj = "lat-lon"
  end if
  end if
  end if
  end if

; Deal with global wrf domains that don't have dx or dy

  if (mpres@dx.lt.1e-10 .and. mpres@dx.lt.1e-10) then
    mpres@dx = 360./(mpres@e_we(0) - 1)
    mpres@dy = 180./(mpres@e_sn(0) - 1)
    mpres@ref_lat = 0.0
    mpres@ref_lon = 180.0
  end if

  mp = wrf_wps_dom (wks,mpres,lnres,txres)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now you can add some information to the plot. 
; Below is an example of adding a white dot over the DC location.
  ;pmres = True
  ;pmres@gsMarkerColor = "White"
  ;pmres@gsMarkerIndex = 16
  ;pmres@gsMarkerSizeF = 0.01
  ;gsn_polymarker(wks,mp,-77.26,38.56,pmres)


  frame(wks)           ; lets frame the plot - do not delete

end
