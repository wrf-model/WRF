;The script read "analysis" file from wrfvar
;plot Vertcal Profile of the variables 
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
;=========================================================;
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
;************************************************
begin
   ;************************************************
   ; read in data
   ;************************************************
   work = "x11"
 analysis = addfile("/mmm/mmmtmp/rizvi/data/india_30km/run_cv_options_hum_rh_md1/fc/2005080500/wrfinput_d01.nc", "r")   

   nlev = 50
   nx = 151
   ny = 151
   time = 0 

   ; grid point location

   n1=75
   n2=75

   T  = analysis->T(0,:,:,:)      ; Theta
   U  = analysis->U(0,:,:,:)       ; u-wind compt.
   V  = analysis->V(0,:,:,:)       ; u-wind compt.
   QA = analysis->QVAPOR(0,:,:,:)  ; qvapor

   u = U(:,n1,n2)
   v = V(:,n1,n2)
   t = T(:,n1,n2)
   q = QA(:,n1,n2) 
   Q = q * 1000

   ;**********************No need change lines here onward **********************

   y = new((/nlev/),integer)
   y = ispan(0,nlev-1,1)

   lat  = analysis->XLAT(0,:,:)
   lon  = analysis->XLONG(0,:,:)
   xlat = lat(75,75)
   xlon = lon(75,75)
   fmt  = "%5.1f"             ;format for lat & long
   varname_label = "("+sprintf(fmt,xlat)+","+sprintf(fmt,xlon)+")"

   ;************************************************
   ; plotting parameters
   ;************************************************

   wks   = gsn_open_wks (work,"xy")                   ; open workstation

   plts                      = new (2,"graphic")

   resT                   = True                       ; plot mods desired
   resT@xyDashPatterns    = 15                         ; choose dash patterns
   ;============================ Legend=============
   ; resT@tiUseMainAttributes    = False
   resT@tiXAxisOn              = False
   resT@pmLegendDisplayMode    = "Always"            ; turn on legend
   resT@lgPerimOn              = False               ; Legend border box 
   resT@pmLegendParallelPosF   = 0.90                ; move units right
   resT@pmLegendOrthogonalPosF = -0.95                ; move units down
   resT@pmLegendWidthF         = 0.05                ; Change width and
   resT@pmLegendHeightF        = 0.05                ; height of legend.
   resT@xyExplicitLegendLabels =(/"T"/)        ; Legend 
   ;============================ Legend=============
   ;============================ Axis =============
   resT@tiYAxisString          = "Sigma Levels"       ; Change the font size.
   resT@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resT@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resT@tiYAxisFontHeightF     = 0.02

   ;============================ Axis =============

   resT@xyLineColors     = (/"blue"/)
   resT@xyLineThicknesses = 4.0

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

   resQ@xyDashPatterns =  1

   resQ@tmXTLabelFontColor = "red"
   resT@tmXBLabelFontColor = "blue"
   resT@tmXBMajorLineColor     = "blue"
   resQ@tmXTMajorLineColor     = "red"
   resT@tmXBMinorLineColor     = "blue"
   resQ@tmXTMinorLineColor     = "red"
   resQ@gsnDraw               = False                ; (a) do not draw
   resQ@gsnFrame              = False                ; (b) do not advance 'frame'

   plts(0) = gsn_csm_x2y (wks,t,Q,y,resT,resQ) ; create plot

   ;--------------  2nd Panel ---------------------------------------------------
   resU                   = True                       ; plot mods desired
   resU@xyDashPatterns    = 15                         ; choose dash patterns
   y = ispan(0,nlev-1,1)

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
   resU@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   resU@tiYAxisFontHeightF     = 0.02

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

   plts(1) = gsn_csm_x2y (wks,u,v,y,resU,resV) ; create plot
   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = "Vertical profile at " +varname_label
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   resP@gsnPanelYWhiteSpacePercent = 2.0
   gsn_panel(wks,plts,(/1,1/),resP)               ; now draw as one plot

end


