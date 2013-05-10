;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "../WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN.

begin
   ;********************************************************
   ; open file 
   ;********************************************************

   works = "x11"
   filename_fg = "/smoke/dmbarker/data/con200/noobs/2003010100/wrfout_d01_2003-01-01_12:00:00"

   k = 9 ; level

   first_guess = addfile(filename_fg+".nc", "r")

   znu = first_guess->ZNU(0,:)

   ;********************************************************
   ;Specify fields:
   ;********************************************************

   fg1 = first_guess->U(0,k,:,:)              ; import data + meta info
   fg2 = first_guess->V(0,k,:,:)              ; import data + meta info
   fg3 = first_guess->T(0,k,:,:)              ; import data + meta info
   fg4 = first_guess->QVAPOR(0,k,:,:)              ; import data + meta info

   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"gen_be_stage0_xa") ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")            ; select color map

   res                       = True            ; plot mods desired
   res@gsnMaximize           = True            ; uncomment to maximize size

   res@cnFillOn              = True            ; turn on color
   res@gsnSpreadColors       = True            ; use entire color map
   ;  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
   res@cnLevelSelectionMode      = "ManualLevels"
   ;  res@tiMainString          = directory
   res@gsnCenterString         = "Eta = "+znu(k)
   WRF_map_c(first_guess,res,0)                ; set map resources

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   ;  res@gsnLeftString         = "U"
   res@cnMinLevelValF        = -30.0
   res@cnMaxLevelValF        = 30.0
   res@cnLevelSpacingF        = 5.0
   plts(0)                   = gsn_csm_contour_map(wks,fg1,res)

   ;  res@gsnLeftString         = "PH"
   res@cnMinLevelValF        = -30.0
   res@cnMaxLevelValF        = 30.0
   res@cnLevelSpacingF        = 5.0
   plts(1)                   = gsn_csm_contour_map(wks,fg2,res)

   ;  res@gsnLeftString         = "Theta'"
   res@cnMinLevelValF        = -20.0 
   res@cnMaxLevelValF        = 20.0
   res@cnLevelSpacingF        = 4.0
   plts(2)                   = gsn_csm_contour_map(wks,fg3,res)

   ;  res@gsnLeftString         = "QV"
   res@cnMinLevelValF        = 0.0
   res@cnMaxLevelValF        = 0.001
   res@cnLevelSpacingF        = 0.0001
   plts(3)                   = gsn_csm_contour_map(wks,fg4,res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = ""
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
