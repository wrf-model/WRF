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

   file_out = "corr"
   directory = "/ocotillo1/dmbarker/data/KMA_T63_input_2004102712/wrfvar.singlet.alpha1/"
   nrow = 200
   ncol = 3

   x = new((/3,nrow/),float)
   y = new((/3,nrow/),float)

   filename = "fort.61"
   var1 = "Expontential"
   data1=asciiread(directory+filename,(/nrow,ncol/),"float")
   filename = "fort.62"
   var2 = "SOAR"
   data2=asciiread(directory+filename,(/nrow,ncol/),"float")
   filename = "fort.63"
   var3 = "Gaussian"
   data3=asciiread(directory+filename,(/nrow,ncol/),"float")

   x(0,:) = data1(:,1)
   x(1,:) = data2(:,1)
   x(2,:) = data3(:,1)
   y(0,:) = data1(:,2)
   y(1,:) = data2(:,2)
   y(2,:) = data3(:,2)

   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks ("pdf",file_out)                ; open workstation

   res                  = True                     ; plot mods desired
   res@trXMinF  =  0.0                   ; min value on x-axis
   res@trXMaxF  =  10000.0                ; max value on x-axis
   res@trYMinF  =  0.0                   ; min value on y-axis
   res@trYMaxF  =  1.0                 ; max value on y-axis

   ; res@tiMainString     = "Basic XY plot"          ; add title
   res@tiXAxisString   = "Distance (km)" ; Label for the X axis
   res@tiYAxisString   = "Correlation"             ; Label for the Y axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
   res@xyLineColors      = (/"black","blue","red","red"/)          ; change line color
   ; res@trYReverse        = True                       ; reverse Y-axis
   res@xyDashPatterns    = (/1,0,1,0/)                         ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
   res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
   res@xyMarkers       = (/3,4,5,4,4,5/)                 ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors         ; Marker colors
   res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
   res@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.02
   ; res@xyLabelMode            = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.02        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .015                ; label font height
   res@xyExplicitLegendLabels = (/var1,var2,var3,"p Ob Levels"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Top"               ; Change location of 
   res@pmLegendParallelPosF   = .65                 ; move units right
   res@pmLegendOrthogonalPosF = -0.4                ; move units down
   res@pmLegendWidthF         = 0.15                ; Change width and
   res@pmLegendHeightF        = 0.18                ; height of legend.

   plot  = gsn_csm_xy (wks,x,y,res) ; create plot

end
