;********************************************************
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Author : Syed RH Rizvi   MMM/NCAR   09/22/2007
; Purpose: Plots WRF-Var analysis output
;          Horizntal crossection for 
;          u,v, theeta & q (gm/Kg) ) in one panel
;=========================================================;
;
; 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
; Note:  Make sure following.
;         a) You have NCL Version: 4.2.0.a.034 upward
;         b) "wrf_user_fortran_util_0.so" file is created by using 
;            "WRAPIT wrf_user_fortran_util_0.f" command
;****************************************************************************
;  Fix WRFVAR_DIR & location of analysis file here                               
;
load "/ptmp/rizvi/trunk/graphics/ncl/WRF_contributed.ncl.test"
;analysis = addfile("/mmm/mmmtmp/rizvi/data/india_30km/trunk_with_radar/fc/2005090806/wrfinput_d01"+".nc", "r") 
analysis = addfile("/mmm/mmmtmp/rizvi/data/india_30km/run_cv_options_hum_rh_md2/fc/2005080500/wrfinput_d01"+".nc", "r") 
;
;****************************************************************************
begin
;****************************************************************************

   works = "x11"
   thinning_factor = 0.025
   vector_magnitude = 20.0
   vector_length    = 0.05  

   sigma_lev=10
   var_U="U"
   var_V="V"
   var_T="T"
   var_Q="Q"

   unit_Q="gm/Kg"

   T = analysis->T(0,sigma_lev-1,:,:)      ; Theta
   U = analysis->U(0,sigma_lev-1,:,:)       ; u-wind compt.
   V = analysis->V(0,sigma_lev-1,:,:)       ; v-wind compt.
   Q = analysis->QVAPOR(0,sigma_lev-1,:,:)  ; qvapor    

   QgmpKg = 1000.0 * Q
   unit_Q="gm/Kg"

  print ("min Theeta = "+min(T))
  print ("max Theeta = "+max(T))
  print ("min U = "+min(U))
  print ("max U = "+max(U))
  print ("min V = "+min(V))
  print ("max V = "+max(V))
  print ("min QV (gm/Kg.) = "+min(Q))
  print ("max QV (gm/Kg.) = "+max(Q))


   lat = analysis->XLAT(0,:,:)
   lon = analysis->XLONG(0,:,:)
   dims = dimsizes(lat)

   dimU  = dimsizes(U)
   lonU = dimU(1)

   dimV  = dimsizes(V)
   latV = dimV(0)

   UU = 0.5*(U(:,0:lonU-2)+U(:,1:lonU-1))
   VV = 0.5*(V(0:latV-2,:)+V(1:latV-1,:))

   UU@units = U@units
   VV@units = V@units

   ;**********************No need change lines here onward **********************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"analysis_sigma_"+sigma_lev)          ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")           ; select color map

   res                       = True            ; plot mods desired
   res@gsnDraw               = False                ; (a) do not draw
   res@gsnFrame              = False                ; (b) do not advance 'frame'

   res@gsnMaximize           = True            ; uncomment to maximize size

  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@cnLineLabelsOn        = False           ; turn off contour line labels
  res@gsnSpreadColors       = True            ; use entire color map
  res@gsnScalarContour      = True            ; contours desired


  res@mpGridAndLimbOn       = True            ; trun on lat/lon lines
  res@pmTickMarkDisplayMode = "Always"        ; turn on tickmarks
  res@tmXTOn = False                          ; turn off top   labels
  res@tmYROn = False                          ; turn off right labels

  res@vcGlyphStyle       = "CurlyVector"      ; curly vectors
  res@vcRefMagnitudeF    = vector_magnitude   ; define vector ref mag
  res@vcRefLengthF       = vector_length      ; define vector ref length
  res@vcMinDistanceF        = thinning_factor  ; larger means sparser

   WRF_map_c(analysis,res,0)                ; set map resources

   ;
   ; Select portion of the map to view.
   ;
   res@mpLimitMode           = get_res_value_keep(res, "mpLimitMode","Corners")
   res@mpLeftCornerLatF      = get_res_value_keep(res, "mpLeftCornerLatF",lat(0,0))
   res@mpLeftCornerLonF      = get_res_value_keep(res, "mpLeftCornerLonF",lon(0,0))
   res@mpRightCornerLatF     = get_res_value_keep(res, "mpRightCornerLatF", lat(dims(0)-1,dims(1)-1))
   res@mpRightCornerLonF     = get_res_value_keep(res, "mpRightCornerLonF", lon(dims(0)-1,dims(1)-1))
   res@pmTickMarkDisplayMode       = get_res_value_keep(res, "pmTickMarkDisplayMode","Always")

   ;
   ; Tick mark resources
   ;
;   res@tmXBMajorLengthF      = get_res_value(res, "tmXBMajorLengthF",0.01)
;   res@tmYLMajorLengthF      = get_res_value(res, "tmYLMajorLengthF",0.01)
;   res@tmXTOn                = get_res_value(res,"tmXTOn",False)
;   res@tmYROn                = get_res_value(res,"tmYROn",False)
;   res@tmYRLabelsOn          = get_res_value(res,"tmYRLabelsOn",True)
;   res@tmXBBorderOn          = get_res_value(res,"tmXBBorderOn",True)
;   res@tmXTBorderOn          = get_res_value(res,"tmXTBorderOn",True)
;   res@tmYLBorderOn          = get_res_value(res,"tmYLBorderOn",True)
;   res@tmYRBorderOn          = get_res_value(res,"tmYRBorderOn",True)
   res@lbLabelStride = 2           ; Color bar value writing

   res@gsnSpreadColors       = True           ; use entire color map
   res@gsnLeftString         = "U"
   res@gsnRightString        = UU@units
   plts(0)                   = gsn_csm_vector_scalar_map(wks,UU,VV,UU,res) ;vector overlaid 
   res@gsnLeftString         = "V"    
   res@gsnRightString        = VV@units
   res@cnFillOn              = True           ; turn on color
   plts(1)                   = gsn_csm_vector_scalar_map(wks,UU,VV,VV,res) ;vector overlaid 

; resource for T and QV (contour)
  res1                       = True           ; plot mods desired
  res1@gsnDraw               = False          ; (a) do not draw
  res1@gsnFrame              = False          ; (b) do not advance 'frame'
  res1@gsnMaximize           = True           ; uncomment to maximize size
  res1@lbLabelStride = 2                      ; Color bar value writing

  res1@cnFillOn              = True           ; turn on color
  res1@cnLineLabelsOn        = False          ; turn off contour line labels
  res1@gsnSpreadColors       = True           ; use entire color map

  WRF_map_c(analysis,res1,0)                ; set map resources


  res1@mpGridAndLimbOn       = True           ; trun on lat/lon lines
  res1@pmTickMarkDisplayMode = "Always"       ; turn on tickmarks
  res1@tmXTOn = False                         ; turn off top   labels
  res1@tmYROn = False                         ; turn off right labels




   res1@gsnLeftString         = "Theta"   
   res1@gsnRightString        = T@units
   plts(2)                   = gsn_csm_contour_map(wks,T,res1)

   res1@gsnLeftString         = "QV"
   res1@gsnRightString        = unit_Q 
   plts(3)                   = gsn_csm_contour_map(wks,QgmpKg,res1)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@gsnFrame         = False                  ; don't advance panel plot
   resP@txString         = "Analysis"
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot
  txres               = True
  txres@txFontHeightF = 0.012
 
  gsn_text_ndc(wks,"Horizontal Cross-section at sigma_level="+sigma_lev ,0.5,0.94,txres)
  frame(wks)


end
