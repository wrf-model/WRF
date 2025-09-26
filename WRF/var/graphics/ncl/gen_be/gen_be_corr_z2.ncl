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

   works = "pdf"

   dira =  "/smoke/dmbarker/data/con200/noobs/gen_be.0200-2512/"
   dirb =  "/smoke/dmbarker/data/con200/xwang/gen_be.0200-2512.2006091512/"
   label1 = "NMC"
   label2 = "ETKF"

   ni = 44
   nj = 44
   nk = 27
   nij = ni * nj

   var1 = "chi"
   var2 = "t"

   filename1a = var1+"_u."+var1+".dat"
   filename1b = var1+"_u."+var1+".dat"
   filename2a = var2+"_u."+var2+".dat"
   filename2b = var2+"_u."+var2+".dat"

   data1a=asciiread(dira+filename1a,(/nk,nij/),"float")
   data1b=asciiread(dirb+filename1b,(/nk,nij/),"float")
   data2a=asciiread(dira+filename2a,(/nk,nij/),"float")
   data2b=asciiread(dirb+filename2b,(/nk,nij/),"float")

   avg1a = dim_sum(data1a) / nij
   avg1b = dim_sum(data1b) / nij
   avg2a = dim_sum(data2a) / nij
   avg2b = dim_sum(data2b) / nij

   x1 = new((/2,nk/),float)
   x1(0,:) = 1.0 - avg1a
   x1(1,:) = 1.0 - avg1b
   x2 = new((/2,nk/),float)
   x2(0,:) = 1.0 - avg2a
   x2(1,:) = 1.0 - avg2b

   y = new((/nk/),float)
   y = ispan(1,nk,1)

   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks (works,"gen_be_corr_z2")      ; open workstation

   res                  = True                     ; plot mods desired
   res@trXMinF  =  0.0                   ; min value on x-axis
   res@trXMaxF  =  1.0                  ; max value on x-axis
   res@trYMinF  =  1.0                   ; min value on y-axis
   res@trYMaxF  =  nk                    ; max value on y-axis

   res@tiXAxisString   = "<x . xb> / <x . x>"                 ; Label for the X axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
   res@xyLineColors      = (/"red","blue","green","purple"/)          ; change line color
   ; res@trYReverse        = True                       ; reverse Y-axis
   res@xyDashPatterns    = (/0,0,1,2,3,4/)                         ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
   res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
   res@xyMarkers       = (/3,4,5/)                 ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors         ; Marker colors
   res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
   res@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.02
   ; res@xyLabelMode            = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.02        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .015                ; label font height
   res@xyExplicitLegendLabels = (/label1,label2,"m=3","m=4","m=5"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Bottom"               ; Change location of
   res@pmLegendParallelPosF   = 0.85                ; move units right
   res@pmLegendOrthogonalPosF = -1.15                ; move units down
   res@pmLegendWidthF         = 0.15                ; Change width and
   res@pmLegendHeightF        = 0.18                ; height of legend.

   res@tiYAxisString   = "Model Level"              ; Label for the Y axis

   plts                      = new (2,"graphic")
   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'


   res@tiMainString     = "Chi"         ; add title
   plts(0) = gsn_csm_xy (wks,x1,y,res) ; create plot
   res@tiMainString     = "T"         ; add title
   plts(1) = gsn_csm_xy (wks,x2,y,res) ; create plot
   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
   ; resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end

