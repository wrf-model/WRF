;*************************************************
; ***********************************************
; xy_1.ncl
; ***********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   ;************************************************
   ; open file and read in data
   ;************************************************

   works = "x11"
;  works = "pdf"

   date1 = "2006102712"
   date2 = "2006102800"
   year = "2006"
   month = "10"
   day = "28"
   hour = "00"
   fcst_range = "T+00"
   region = "t64a"
   expt = "test"
   run_dir = "wrfvar.u250.ctrl"
   run_dir = "wrfvar.u250.h6"
   run_dir = "wrfvar.ctrl"
   run_dir = "wrfvar.jb0.5"
   run_dir = "wrfvar"
;   file_type = "wrfout_d01"
;   file_type = "wrfinput_d01"

   directory = "/mmm/users/xinzhang/hybrid/" + region + "/" + expt
   dir = directory + "/run/" + date2 + "/" + run_dir + "/"
   filename = "cost_fn"

   nrow = 28
   ncol = 9

   x = new((/4,nrow/),float)
   y = new((/4,nrow/),float)

   data1=asciiread(dir + filename,(/nrow,ncol/),"float")
   x(0,:) = data1(:,2)
   x(1,:) = data1(:,2)
   x(2,:) = data1(:,2)
   x(3,:) = data1(:,2)
   y(0,:) = data1(:,3)
   y(1,:) = data1(:,4)
   y(2,:) = data1(:,5)
   y(3,:) = data1(:,7)
   min_x = 0.0
   max_x = 30.0
   min_y = 0.0
   max_y = 5000.0

   wks = gsn_open_wks(works, "fig4_"+run_dir)

   res                  = True                     ; plot mods desired
   res@trXMinF  =  min_x                 ; min value on x-axis
   res@trXMaxF  =  max_x                  ; max value on x-axis
   res@trYMinF  =  min_y                 ; min value on y-axis
   res@trYMaxF  =  max_y               ; max value on y-axis

   ; res@tiMainString     = "Basic XY plot"          ; add title
   res@tiXAxisString   = "Iteration" ; Label for the X axis
   res@tiYAxisString   = "Cost Function"           ; Label for the Y axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
   res@xyLineColors      = (/"blue","red","green","orange"/)          ; change line color
   ; res@trYReverse        = True                       ; reverse Y-axis
   res@xyDashPatterns    = (/0,1,2,3/)                         ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
;   res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
   res@xyMarkers       = (/0,0,0,0,0,0/)                 ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors         ; Marker colors
   res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
   res@tiXAxisFontHeightF     = 0.02               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.02
   ; res@xyLabelMode            = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.02        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .015                ; label font height
   res@xyExplicitLegendLabels = (/"J","Jb","Jo","Je"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Top"               ; Change location of 
   res@pmLegendParallelPosF   = .65                 ; move units right
   res@pmLegendOrthogonalPosF = -0.4                ; move units down
   res@pmLegendWidthF         = 0.15                ; Change width and
   res@pmLegendHeightF        = 0.18                ; height of legend.

   plot  = gsn_csm_xy (wks,x,y,res) ; create plot

end
