;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "./WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN.

begin
   ;********************************************************
   ; open file 
   ;********************************************************

   works = "pdf"
   ni = 44
   nj = 44
   nk = 27
   k = 12 ; Level
   date = "2003010212"
   directory = "/smoke/dmbarker/data/con200/xwang/"+date+"/ep2/"
   var1 = "u"
   var1_units = "m/s"
   var2 = "t"
   var2_units = "K"

   ; WRF template (use only for Map):
   wrf_file = "/smoke/dmbarker/data/con200/xwang/2003010200/wrf_inputout_d01_0"
   wrf = addfile(wrf_file+".nc", "r")
   fg = wrf->T(0,k,:,:)              ; import data + meta info

   ;Field 1
   filename = directory + var1 + "/" + var1 + ".mean"
   field_meand = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   filename = directory + var1 + "/" + var1 + ".mnsq"
   field_mnsqd  = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )

   field_stdvd = sqrt( field_mnsqd - field_meand * field_meand )
   field_mean = dble2flt(field_meand(k,:,:))
   field_stdv = dble2flt(field_stdvd(k,:,:))
   mean1 = fg
   mean1 = field_mean
   stdv1 = fg
   stdv1 = field_stdv

   ;Field 2
   filename = directory + var2 + "/" + var2 + ".mean"
   field_meand = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   filename = directory + var2 + "/" + var2 + ".mnsq"
   field_mnsqd  = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )

   field_stdvd = sqrt( field_mnsqd - field_meand * field_meand )
   field_mean = dble2flt(field_meand(k,:,:))
   field_stdv = dble2flt(field_stdvd(k,:,:))

   mean2 = fg
   mean2 = field_mean
   stdv2 = fg
   stdv2 = field_stdv

   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"etkf_perts_ep2")          ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")           ; select color map

   res                       = True            ; plot mods desired
   res@gsnMaximize           = True            ; uncomment to maximize size

   res@cnFillOn              = True            ; turn on color
   res@gsnSpreadColors       = True            ; use entire color map
   ;  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
   res@cnLevelSelectionMode      = "ManualLevels"
   ;  res@tiMainString          = directory
   znu = wrf->ZNU(0,:)
   res@gsnCenterString         = "Eta = "+znu(k)
   WRF_map_c(wrf,res,0)                ; set map resources

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var1
   res@gsnCenterString       = "ETKF Mean"
   res@gsnRightString        = var1_units
   res@cnMinLevelValF        = -25.0
   res@cnMaxLevelValF        =  25.0
   res@cnLevelSpacingF        =  5.0
   plts(0) = gsn_csm_contour_map(wks,mean1,res)

   res@gsnLeftString         = var1
   res@gsnCenterString       = "ETKF StDev"
   res@gsnRightString        = var1_units
   res@cnMinLevelValF        =  0.5
   res@cnMaxLevelValF        =  7.0
   res@cnLevelSpacingF        = 0.5
   plts(1) = gsn_csm_contour_map(wks,stdv1,res)

   res@gsnLeftString         = var2
   res@gsnCenterString       = "ETKF Mean"
   res@gsnRightString        = var2_units
   res@cnMinLevelValF        =  225.0
   res@cnMaxLevelValF        =  275.0
   res@cnLevelSpacingF        =  5.0
   plts(2) = gsn_csm_contour_map(wks,mean2,res)

   res@gsnLeftString         = var2
   res@gsnCenterString       = "ETKF StDev"
   res@gsnRightString        = var2_units
   res@cnMinLevelValF        =  0.25
   res@cnMaxLevelValF        =  2.5
   res@cnLevelSpacingF        = 0.25
   plts(3) = gsn_csm_contour_map(wks,stdv2,res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = date + ", Level " + k
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end

