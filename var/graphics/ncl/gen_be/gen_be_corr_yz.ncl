;================================================;
;  conwomap_2.ncl
;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;
begin
   ;=================================================;
   ; open file and read in data
   ;=================================================;

   works = getenv("GRAPHIC_WORKS")

   region = getenv("REGION")
   dir    = getenv("BE_DIR")
   nk   = stringtointeger(getenv("NUM_LEVELS"))
   ni   = stringtointeger(getenv("NUM_WE"))
   nj   = stringtointeger(getenv("NUM_SN"))


   be_method = "dat"

   filename1 = "chi_u.chi." + be_method
   data1=asciiread(dir+"/"+filename1,(/nk,nj,ni/),"float")
   f1 = dim_sum(data1) / ni
   f1 = 1.0 - f1

   f1!0 = "y"
   f1!1 = "x"
   ;Global: f1&x = -90.0 + ispan(0,nj-1,1) * 180.0 / nj + 90.0 / nj
   xcoord = "latitude"
   ;Regional: 
   f1&x = ispan(1,nj,1)
   xcoord = "Grid j"

   f1&y = ispan(1,nk,1)

   f2 = f1
   filename2 = "t_u.t." + be_method
   data2=asciiread(dir+"/"+filename2,(/nk,nj,ni/),"float")
   f2 = dim_sum(data2) / ni
   f2 = 1.0 - f2

   ;=================================================;
   ; PLOT 2
   ;=================================================;
   wks  = gsn_open_wks(works,"gen_be_corr_yz_"+region)    ; open a ncgm file
   gsn_define_colormap(wks,"gui_default")  ; choose a colormap

   res                     = True          ; plot mods desired
   res@tiXAxisString   = xcoord                ; Label for the X axis
   res@tiYAxisString   = "Model Level"             ; Label for the Y axis
   res@cnLinesOn           = False          ; turn on color
   res@cnLineLabelsOn           = False          ; turn on color
   res@cnFillOn            = True          ; turn on color
   res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
   res@gsnSpreadColors     = True          ; use full range of colormap

   ;  res@vpWidthF            = 0.4           ; change aspect ratio of plot
   ;  res@vpHeightF           = 0.3
   res@lbAutoManage          = False
   res@lbOrientation         = "Vertical"
   res@lgLabelFontHeightF     = .02                 ; label font height
   res@pmLabelBarOrthogonalPosF = -0.02    ; move label bar close
   res@pmLabelBarSide  = "Top"

   plts                      = new (2,"graphic")
   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@lbLabelBarOn = False

   res@trXMinF  =  f1&x(0)                   ; min value on x-axis
   res@trXMaxF  =  f1&x(nj-1)                ; max value on x-axis
   res@trYMinF  =  1.0                   ; min value on y-axis
   res@trYMaxF  =  nk                    ; max value on y-axis
   res@cnMinLevelValF =  0.0                 ; set mimimum contour level
   res@cnMaxLevelValF =  0.9                  ; set maximum contour level
   res@cnLevelSpacingF =  0.1                  ; set maximum contour level
   res@tiMainString    = " Chi_b.Chi correlation"     ; Main title
   plts(0) = gsn_csm_contour(wks,f1,res)       ; contour the variable

   res@trXMinF  =  f2&x(0)                   ; min value on x-axis
   res@trXMaxF  =  f2&x(nj-1)                ; max value on x-axis
   res@trYMinF  =  1.0                   ; min value on y-axis
   res@trYMaxF  =  nk                    ; max value on y-axis
   res@cnMinLevelValF =  0.0                 ; set mimimum contour level
   res@cnMaxLevelValF =  0.9                  ; set maximum contour level
   res@cnLevelSpacingF =  0.05                 ; set maximum contour level
   res@tiYAxisOn = False
   res@tiMainString    = " t_b.t correlation"     ; Main title
   plts(1) = gsn_csm_contour(wks,f2,res)       ; contour the variable

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPaperOrientation = "portrait"
   resP@gsnPaperWidth  = 6.0                   ; specify 1 top, 2 lower level
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   resP@gsnPanelLabelBar = True
   resP@pmLabelBarWidthF = 0.8
   gsn_panel(wks,plts,(/2,1/),resP)               ; now draw as one plot

end
