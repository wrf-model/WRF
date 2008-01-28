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
   ep = 1
   ep = 2
   var1 = "u"
   ; var1 = "psi"
   var1_units = "m/s"
   ; var1_units = "m/s**2"
   scale1 = 1.0 
   ; scale1 = 0.000001
   min1a = -25.0 
   max1a =  25.0
   int1a = 5.0
   min1b = 0.5 
   max1b = 7.0
   int1b = 0.5

   var2 = "t"
   ; var2 = "chi_u"
   var2_units = "K"
   ; var2_units = "m/s**2"
   scale2 = 1.0
   ; scale2 = 0.000001
   min2a = 225.0 
   max2a = 275.0
   int2a = 5.0
   ; min2a = -3.0 
   ; max2a = 3.0
   ; int2a = 0.5
   min2b = 0.25 
   max2b = 2.25
   int2b = 0.25
   ; min2b = 0.2  
   ; max2b = 1.4
   ; int2b = 0.2 

   directory = "/smoke/dmbarker/data/con200/xwang/"+date+"/ep" + ep + "/"

   ; WRF template (use only for Map):
   wrf_file = "/smoke/dmbarker/data/con200/xwang/2003010200/wrf_inputout_d01_0"
   wrf = addfile(wrf_file+".nc", "r")
   fg = wrf->T(0,k,:,:)              ; import data + meta info

   ;Field 1
   filename = directory + var1 + "/" + date + "." + var1 + ".mean"
   field_meand = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   filename = directory + var1 + "/" + date + "." + var1 + ".stdv"
   field_stdvd  = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   print (min(field_stdvd))
   print (max(field_stdvd))
   field_mean = dble2flt(field_meand(k,:,:))
   field_stdv = dble2flt(field_stdvd(k,:,:))
   mean1 = fg
   mean1 = scale1 * field_mean
   stdv1 = fg
   stdv1 = scale1 * field_stdv

   ;Field 2
   filename = directory + var2 + "/" + date + "." + var2 + ".mean"
   field_meand = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   filename = directory + var2 + "/" + date + "." + var2 + ".stdv"
   field_stdvd  = fbinrecread( filename, 1, (/ nk, nj, ni /) , "double" )
   print (min(field_stdvd))
   print (max(field_stdvd))

   field_mean = dble2flt(field_meand(k,:,:))
   field_stdv = dble2flt(field_stdvd(k,:,:))

   mean2 = fg
   mean2 = scale2 * field_mean
   stdv2 = fg
   stdv2 = scale2 * field_stdv

   print (min(stdv1))
   print (max(stdv1))
   print (min(stdv2))
   print (max(stdv2))
   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"etkf_perts_ep"+ep+"_"+var1+"_"+var2)          ; ps,pdf,x11,ncgm,eps
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
   res@cnMinLevelValF        =  min1a
   res@cnMaxLevelValF        =  max1a
   res@cnLevelSpacingF        = int1a
   plts(0) = gsn_csm_contour_map(wks,mean1,res)

   res@gsnLeftString         = var1
   res@gsnCenterString       = "ETKF StDv"
   res@gsnRightString        = var1_units
   res@cnMinLevelValF        =  min1b
   res@cnMaxLevelValF        =  max1b
   res@cnLevelSpacingF        = int1b
   plts(1) = gsn_csm_contour_map(wks,stdv1,res)

   res@gsnLeftString         = var2
   res@gsnCenterString       = "ETKF Mean"
   res@gsnRightString        = var2_units
   res@cnMinLevelValF        = min2a
   res@cnMaxLevelValF        = max2a
   res@cnLevelSpacingF       = int2a
   plts(2) = gsn_csm_contour_map(wks,mean2,res)

   res@gsnLeftString         = var2
   res@gsnCenterString       = "ETKF Stdv"
   res@gsnRightString        = var2_units
   res@cnMinLevelValF        = min2b
   res@cnMaxLevelValF        = max2b
   res@cnLevelSpacingF       = int2b
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

