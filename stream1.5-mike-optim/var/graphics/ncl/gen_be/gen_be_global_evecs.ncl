; ***********************************************
; xy_1.ncl
; ***********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
   ;************************************************
   ; read in data
   ;************************************************

   works = getenv("GRAPHIC_WORKS")

   region = getenv("REGION")
   dir    = getenv("BE_DIR")
   nrow   = stringtointeger(getenv("NUM_LEVELS"))

   ncol = 6
   y = new((/nrow/),float)
   x = new((/5,nrow/),float)

   var1="psi"
   filename1="fort.174"

   var2="chi_u"
   filename2="fort.178"

   var3="t_u"
   filename3 = "fort.182"

   var4="rh"
   filename4 = "fort.186"

   data1=asciiread(dir+"/"+filename1,(/nrow,ncol/),"float")
   data2=asciiread(dir+"/"+filename2,(/nrow,ncol/),"float")
   data3=asciiread(dir+"/"+filename3,(/nrow,ncol/),"float")
   data4=asciiread(dir+"/"+filename4,(/nrow,ncol/),"float")

   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks (works,"gen_be_global_evecs_"+region)  ; open workstation

   res                  = True                     ; plot mods desired
   res@trXMinF  =  -1.0                  ; min value on x-axis
   res@trXMaxF  =  1.0                   ; max value on x-axis
   res@trYMinF  =  1.0                   ; min value on y-axis
   res@trYMaxF  =  nrow                  ; max value on y-axis
   res@tiXAxisString   = "Eigenvector"                  ; Label for the X axis
   res@tiYAxisString   = "Model Level"             ; Label for the Y axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
   res@xyLineColors      = (/"black","red","blue","green","purple"/)          ; change line color
   res@xyDashPatterns    = (/0,1,2,3,4/)                         ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
   res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
   res@xyMarkers       = (/1,2,3,4,5/)                 ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors         ; Marker colors
   res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
   res@tiXAxisFontHeightF     = 0.03               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.03
   ; res@xyLabelMode            = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.03        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .02                 ; label font height
   res@xyExplicitLegendLabels = (/"m=1","m=2","m=3","m=4","m=5"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Bottom"               ; Change location of 
   res@pmLegendParallelPosF   = 0.15                ; move units right
   res@pmLegendOrthogonalPosF = -0.55                ; move units down
   res@pmLegendWidthF         = 0.2                 ; Change width and
   res@pmLegendHeightF        = 0.2                 ; height of legend.

   plts                      = new (4,"graphic")

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   y(:)   = data1(:,0)
   x(0,:) = data1(:,1)
   x(1,:) = data1(:,2)
   x(2,:) = data1(:,3)
   x(3,:) = data1(:,4)
   x(4,:) = data1(:,5)
   res@tiMainString     = var1         ; add title
   plts(0) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data2(:,0)
   x(0,:) = data2(:,1)
   x(1,:) = data2(:,2)
   x(2,:) = data2(:,3)
   x(3,:) = data2(:,4)
   x(4,:) = data2(:,5)
   res@tiMainString     = var2         ; add title
   plts(1) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data3(:,0)
   x(0,:) = data3(:,1)
   x(1,:) = data3(:,2)
   x(2,:) = data3(:,3)
   x(3,:) = data3(:,4)
   x(4,:) = data3(:,5)
   res@tiMainString     = var3         ; add title
   plts(2) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data4(:,0)
   x(0,:) = data4(:,1)
   x(1,:) = data4(:,2)
   x(2,:) = data4(:,3)
   x(3,:) = data4(:,4)
   x(4,:) = data4(:,5)
   res@tiMainString     = var4         ; add title
   plts(3) = gsn_csm_xy (wks,x,y,res) ; create plot

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
    resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
   ; resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
