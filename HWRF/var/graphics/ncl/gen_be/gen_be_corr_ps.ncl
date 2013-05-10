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
   ni   = stringtointeger(getenv("NUM_WE"))
   nj   = stringtointeger(getenv("NUM_SN"))


   be_method = "dat"

   filename1 = "ps_u.ps." + be_method
   data1=asciiread(dir+"/"+filename1,(/nj,ni/),"float")
   f1 = dim_sum(data1) / ni
   ;f1 = 1.0 - f1

   y = new((/nj/),float)
   y = 1.0 - f1

   x = new((/nj/),float)
   x = ispan(1,nj,1)
   
   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks (works,"gen_be_corr_ps_"+region)      ; open workstation

   res                  = True           ; plot mods desired
   res@trXMinF  =  1.0                   ; min value on x-axis
   res@trXMaxF  =  nj                    ; max value on x-axis
   res@trYMinF  =  0.0                   ; min value on y-axis
   res@trYMaxF  =  1.0                    ; max value on y-axis

   res@tiXAxisString   = "Grid j"                 ; Label for the X axis
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)      ; make 2nd lines thicker
   res@xyLineColors      = (/"red","blue","green","purple"/)          ; change line color
   ; res@trYReverse        = True                       ; reverse Y-axis
   res@xyDashPatterns    = (/0,0,1,2,3,4/)              ; choose dash patterns
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
   res@xyExplicitLegendLabels = (/"","m=3","m=4","m=5"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "NoCreate"            ; turn on legend
   res@pmLegendSide           = "Bottom"               ; Change location of
   res@pmLegendParallelPosF   = 0.85                ; move units right
   res@pmLegendOrthogonalPosF = -1.15                ; move units down
   res@pmLegendWidthF         = 0.15                ; Change width and
   res@pmLegendHeightF        = 0.18                ; height of legend.

   res@tiMainString     = "Ps_b.Ps Correlation"      ; add title
   res@tiYAxisString   = "Correlation"              ; Label for the Y axis
   plot = gsn_csm_xy (wks,x,y,res) ; create plot

  end
