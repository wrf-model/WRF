;****************************************************************
;The Program read directly "ANALYSIS INCREMENT" file from wrfvar
;Plot Vertical profile of the variables
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Copyright (C) 2006
; University Corporation for Atmospheric Research
; All Rights Reserved
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Author: Ashish Routray , S.R.H. Rizvi and Ananda Kumar Das
;         National Center for Atmospheric Research
;         Boulder, CO 80301
;
; Date: 01/17/07
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;=========================================================;
; Note:  Make sure following.
;         a) You have NCL Version: 4.2.0.a.034 upward
;         b) "wrf_user_fortran_util_0.so" file is created by using
;            "RWRAPIT wrf_user_fortran_util_0.f" command
;****************************************************************************
;
;****************Change lines below *****************************************
;
load "/ptmp/rizvi/trunk/graphics/ncl/WRF_contributed.ncl.test"
load "/ptmp/rizvi/trunk/graphics/ncl/WRFUserARW.ncl"
external wrf_user_fortaran_util_0 "/ptmp/rizvi/trunk/graphics/ncl/wrf_user_fortran_util_0.so"

;****************************************************************************
;

begin
   ;************************************************
   ; read in data
   ;************************************************
    work ="x11"
   ; var_name = "P"
   ;         n1 and n2 are the x and y grid locations for profile ploting
   n1=75
   n2=75

   ; sigma_lev = 1

   ;*************** Path for Input file *************
   ;expt = "vert_"+var_name+"_"+n1+"_"+n2+"_"+sigma_lev

   expt = "vetrical profile at grid location "+ n1+" & "+n2
   filename_inc = "/mmm/mmmtmp/rizvi/data/india_30km/psot_with_cv_options_hum_rh_md2/run/2005080500/wrfvar_psot1/working/analysis_increments"
   setfileoption("bin","ReadByteOrder","BigEndian")
   ;***************************************************

   var1="T"
   units1="K"
   var2="U"
   units2="mps"
   var3="V"
   units3="mps"
   var4="QV"
   units4="gm/Kg"

   ;**********************No need change lines here onward **********************
   ;********************************************************
   ;Read records  from ANALYSIS INCREMENT file
   ;********************************************************

   rec_date=0
   rec_ni_nj_nk=1
   rec_map_proj  = 2
   rec_truelat  = 3
   rec_sigma = 4
   rec_field = 5
   rec_lat_lon = 6

   ;********************************************************
   ni_nj_nk = fbinrecread( filename_inc, rec_ni_nj_nk, 6, "integer" )
   nx = ni_nj_nk(1)
   ny = ni_nj_nk(3)
   nz = ni_nj_nk(5)
   nk = 7*(nz+1)+2
   inc_all = fbinrecread( filename_inc, rec_field, (/ nk, ny+1, nx+1 /) , "double" )
   lat_lon = fbinrecread( filename_inc, rec_lat_lon, (/ 10, ny+1, nx+1 /) , "double" )

   ;print("nx "+ni_nj_nk)


   print ("nx " +nx)
   print ("ny " +ny)
   print ("nz " +nz)

   inc_u = new ((/nz+1,ny+1,nx+1/),"double")
   inc_v = new ((/nz+1,ny+1,nx+1/),"double")
   inc_t = new ((/nz+1,ny+1,nx+1/),"double")
   inc_q = new ((/nz+1,ny+1,nx+1/),"double")
   inc_p = new ((/nz+1,ny+1,nx+1/),"double")
   lat = new ((/ny+1,nx+1/),"double")
   lon = new ((/ny+1,nx+1/),"double")
   print("All dims "+dimsizes(inc_all))

   do ik = 0, nz
      inc_u (ik,:,:) = inc_all(ik,:,:)
      inc_v (ik,:,:) = inc_all(nz+1+ik,:,:)
      inc_p (ik,:,:) = inc_all(3*nz+3+ik,:,:)
      inc_t (ik,:,:) = inc_all(4*nz+4+ik,:,:)
      inc_q (ik,:,:) = inc_all(5*nz+5+ik,:,:)
   end do
   lat(:,:) = lat_lon(4,:,:)
   lon(:,:) = lat_lon(5,:,:)

   print("Min Lat " +min(lat) )
   print("Max Lat " +max(lat) )
   print("Min Lon " +min(lon) )
   print("Max Lon " +max(lon) )

   print("inc_u " +min(inc_u) )
   print("inc_u " +max(inc_u) )

   print("inc_v " +min(inc_v) )
   print("inc_v " +max(inc_v) )

   print("inc_t " +min(inc_t) )
   print("inc_t " +max(inc_t) )

   print("inc_q " +min(inc_q) )
   print("inc_q " +max(inc_q) )

   ;*************Read Ploting Variables ****************
   U =inc_u(:,n1,n2) 
   V =inc_v(:,n1,n2)
   T =inc_t(:,n1,n2)
   Q =inc_q(:,n1,n2)*1000.
   ;*************Read Ploting Variables end **************

   y = new((/nz+1/),integer)
   y = ispan(1,nz+1,1)

        xlat = lat(n1,n2)
        xlon = lon(n1,n2)
      fmt = "%5.1f"             ;format for lat & long
        wlat = sprintf(fmt,xlat)
        wlon = sprintf(fmt,xlon)

   ;************************************************
   ; plotting parameters
   ;************************************************

   wks   = gsn_open_wks (work,"xy")                   ; open workstation
   ; wks   = gsn_open_wks ("pdf",expt)                   ; open workstation

   plts                      = new (2,"graphic")

   resT                   = True                       ; plot mods desired
   resT@xyDashPatterns    = 15                         ; choose dash patterns

   ;============================ Legend=============
   ; title_name = "Var="+var_name+", "  +"Lat="+wlat+", " +"Lon="+wlon+", " +"Sigma Lev="+sigma_lev+", " +"Err=1, Val=1, Scale=1"
   title_name = "AT Lat="+wlat+", " +"Lon="+wlon
   resT@tiMainString    = title_name

   resT@pmLegendDisplayMode    = "Always"            ; turn on legend
   resT@lgPerimOn              = False               ; Legend border box 
   ; resT@pmLegendSide           = "Bottom"            ; Change location of
   ; resT@pmLegendSide           = "Top"            ; Change location of
   resT@pmLegendParallelPosF   = 0.90                ; move units right
   resT@pmLegendOrthogonalPosF = -0.95                ; move units down
   resT@pmLegendWidthF         = 0.05                ; Change width and
   resT@pmLegendHeightF        = 0.05                ; height of legend.
   resT@xyExplicitLegendLabels =(/"T"/)        ; Legend 
   ;============================ Legend=============
   ;============================ Axis =============
   resT@tiYAxisString          = "Sigma Levels"       ; Change the font size.
   resT@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resT@tiYAxisFontHeightF     = 0.02
   resT@trYMinF = 1		; set minimum Y-axis value
   resT@trYMaxF = nz        	; set maximum Y-axis value

   ;============================ Axis =============

   resT@xyLineColors     = (/"blue"/)
   resT@xyLineThicknesses = 4.0
   ; printVarSummary(x)

   resT@xyDashPatterns =  0
   resT@gsnDraw               = False                ; (a) do not draw
   resT@gsnFrame              = False                ; (b) do not advance 'frame'


   resQ                   = True                       ; plot mods desired
   resQ@xyDashPatterns    = 15                         ; choose dash patterns

   ;============================ Legend=============
   resQ@pmLegendDisplayMode    = "Always"            ; turn on legend
   resQ@lgPerimOn              = False               ; Legend border box 
   resQ@pmLegendParallelPosF   = 0.90                ; move units right
   resQ@pmLegendOrthogonalPosF = -0.95               ; move units down
   resQ@pmLegendWidthF         = 0.05                ; Change width and
   resQ@pmLegendHeightF        = 0.05                ; height of legend.
   resQ@xyExplicitLegendLabels =(/"Q"/)        ; Legend 
   ;============================ Legend=============
   ;============================ Axis =============
   resQ@tiYAxisString          = "Sigma Levels"       ; Change the font size.
   resQ@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resQ@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resQ@tiYAxisFontHeightF     = 0.02

   ;============================ Axis =============

   resQ@xyLineColors     = (/"red"/)
   resQ@xyLineThicknesses = 4.0
   ; printVarSummary(x)

   resQ@xyDashPatterns =  1

   resQ@tmXTLabelFontColor = "red"
   resT@tmXBLabelFontColor = "blue"
   resT@tmXBMajorLineColor     = "blue"
   resQ@tmXTMajorLineColor     = "red"
   resT@tmXBMinorLineColor     = "blue"
   resQ@tmXTMinorLineColor     = "red"
   resQ@gsnDraw               = False                ; (a) do not draw
   resQ@gsnFrame              = False                ; (b) do not advance 'frame'

   plts(0) = gsn_csm_x2y (wks,T,Q,y,resT,resQ) ; create plot

   ;--------------  2nd Panel ---------------------------------------------------
   resU                   = True                       ; plot mods desired
   resU@xyDashPatterns    = 15                         ; choose dash patterns

   ;============================ Legend=============
   resU@pmLegendDisplayMode    = "Always"            ; turn on legend
   resU@lgPerimOn              = False               ; Legend border box 
   resU@pmLegendParallelPosF   = 0.90                ; move units right
   resU@pmLegendOrthogonalPosF = -0.95                ; move units down
   resU@pmLegendWidthF         = 0.05                ; Change width and
   resU@pmLegendHeightF        = 0.05                ; height of legend.
   resU@xyExplicitLegendLabels =(/"U"/)        ; Legend 
   ;============================ Legend=============
   ;============================ Axis =============
   resU@tiYAxisString          = "Sigma Levels"       ; Change the font size.
   resU@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resU@tiYAxisFontHeightF     = 0.02
   resU@trYMinF = 1		; set minimum Y-axis value
   resU@trYMaxF = nz        	; set maximum Y-axis value

   ;============================ Axis =============

   resU@xyLineColors     = (/"blue"/)
   resU@xyLineThicknesses = 4.0

   resU@xyDashPatterns =  0
   resU@gsnDraw               = False                ; (a) do not draw
   resU@gsnFrame              = False                ; (b) do not advance 'frame'

   resV                   = True                       ; plot mods desired
   resV@xyDashPatterns    = 15                         ; choose dash patterns

   ;============================ Legend=============
   resV@pmLegendDisplayMode    = "Always"            ; turn on legend
   resV@lgPerimOn              = False               ; Legend border box 
   resV@pmLegendParallelPosF   = 0.90                ; move units right
   resV@pmLegendOrthogonalPosF = -0.95               ; move units down
   resV@pmLegendWidthF         = 0.05                ; Change width and
   resV@pmLegendHeightF        = 0.05                ; height of legend.
   resV@xyExplicitLegendLabels =(/"V"/)        ; Legend 
   ;============================ Legend=============
   ;============================ Axis =============
   resV@tiYAxisString          = "Sigma Levels"       ; Change the font size.
   resV@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resV@tiYAxisFontHeightF     = 0.02

   ;============================ Axis =============

   resV@xyLineColors     = (/"red"/)
   resV@xyLineThicknesses = 4.0

   resV@xyDashPatterns =  1
   resV@gsnDraw               = False                ; (a) do not draw
   resV@gsnFrame              = False                ; (b) do not advance 'frame'

   resV@tmXTLabelFontColor = "red"
   resU@tmXBLabelFontColor = "blue"
   resU@tmXBMajorLineColor     = "blue"
   resV@tmXTMajorLineColor     = "red"
   resU@tmXBMinorLineColor     = "blue"
   resV@tmXTMinorLineColor     = "red"

   plts(1) = gsn_csm_x2y (wks,U,V,y,resU,resV)         ; create plot

   ;**********************************************************
   ; create panel: panel plots have their own set of resources
   ;**********************************************************
   resP                  = True                   ; modify the panel plot
   ;  resP@txString         = "Analysis Increment with Single Obs."

   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/1,1/),resP)               ; now draw as one plot

end
