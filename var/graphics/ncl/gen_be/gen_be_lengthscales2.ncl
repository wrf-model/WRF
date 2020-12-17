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

   works = "pdf"

   dira =  "/smoke/dmbarker/data/con200/noobs/gen_be.0200-2512/"
   dirb =  "/smoke/dmbarker/data/con200/xwang/gen_be.0200-2512.2006091512/"
   label1 = "NMC"
   label2 = "ETKF"
   nrow = 27
   resolution = 200.0

   ncol = 2

   var1="psi"
   filename1 = "fort.194"
   data1a=asciiread(dira+filename1,(/nrow,ncol/),"float")
   data1b=asciiread(dirb+filename1,(/nrow,ncol/),"float")

   var2="chi_u"
   filename2 = "fort.195"
   data2a=asciiread(dira+filename2,(/nrow,ncol/),"float")
   data2b=asciiread(dirb+filename2,(/nrow,ncol/),"float")

   var3="t_u"
   filename3 = "fort.196"
   data3a=asciiread(dira+filename3,(/nrow,ncol/),"float")
   data3b=asciiread(dirb+filename3,(/nrow,ncol/),"float")

   var4="rh"
   filename4 = "fort.197"
   data4a=asciiread(dira+filename4,(/nrow,ncol/),"float")
   data4b=asciiread(dirb+filename4,(/nrow,ncol/),"float")

   y1 = new((/2,nrow/),float)
   y2 = new((/2,nrow/),float)
   y3 = new((/2,nrow/),float)
   y4 = new((/2,nrow/),float)
   x  = new((/nrow/),float)
   x = ispan(1,nrow,1)
   y1(0,:) = resolution * data1a(:,1)
   y1(1,:) = resolution * data1b(:,1)
   y2(0,:) = resolution * data2a(:,1)
   y2(1,:) = resolution * data2b(:,1)
   y3(0,:) = resolution * data3a(:,1)
   y3(1,:) = resolution * data3b(:,1)
   y4(0,:) = resolution * data4a(:,1)
   y4(1,:) = resolution * data4b(:,1)

   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks (works,"gen_be_lengthscales")  ; open workstation

   res                  = True                     ; plot mods desired
   res@trXMinF  =  1.0                  ; min value on x-axis
   res@trXMaxF  =  nrow                 ; max value on x-axis
   res@trYMinF  =  0.0                   ; min value on y-axis
   res@tiXAxisString   = "Vertical Mode"                  ; Label for the X axis
   res@tiYAxisString   = "Lengthscale (km)"             ; Label for the Y axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
   res@xyLineColors      = (/"red","blue","green","purple"/)          ; change line color
   res@xyDashPatterns    = (/0,4,2,3,4/)                         ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
   res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
   res@xyMarkers       = (/3,4,3,4,5/)                 ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors         ; Marker colors
   res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
   res@tiXAxisFontHeightF     = 0.03               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.03
  ; res@xyLabelMode            = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.03        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .02                 ; label font height
   res@xyExplicitLegendLabels = (/label1,label2,"50km","m=5"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Bottom"               ; Change location of 
   res@pmLegendParallelPosF   = 0.75                ; move units right
   res@pmLegendOrthogonalPosF = -1.2                 ; move units down
   res@pmLegendWidthF         = 0.2                 ; Change width and
   res@pmLegendHeightF        = 0.2                 ; height of legend.

   plts                      = new (4,"graphic")

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@tiMainString     = var1         ; add title
   res@trYMaxF  =  max(y1)           ; max value on y-axis
   plts(0) = gsn_csm_xy (wks,x,y1,res) ; create plot

   res@tiMainString     = var2         ; add title
   res@trYMaxF  =  max(y2)           ; max value on y-axis
   plts(1) = gsn_csm_xy (wks,x,y2,res) ; create plot

   res@tiMainString     = var3         ; add title
   res@trYMaxF  =  max(y3)           ; max value on y-axis
   plts(2) = gsn_csm_xy (wks,x,y3,res) ; create plot

   res@tiMainString     = var4         ; add title
   res@trYMaxF  =  max(y4)           ; max value on y-axis
   plts(3) = gsn_csm_xy (wks,x,y4,res) ; create plot

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
   ; resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
