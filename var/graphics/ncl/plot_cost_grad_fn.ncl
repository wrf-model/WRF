;============================================================
;  Authori: Syed RH Rizvi    NCAR/MMM    
;  Purpose:
;           NCL script for ploting "cost_fn" & "grad_fn"
;           written by WRF-Var  
;  
;   Note: Make sure that you removed first two lines in 
;         cost_fn" & grad_fn" files
;============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
begin
;============================================================
; Fix the following
;============================================================
; Input file directory
; Fix the Workstation
 works = "X11"

;============================================================
; Input file directory
 dir = "/karri/users/xinzhang/support/con200/run_cpu1/2007010200/wrfvar"
; Number of inner iterations WRF-Var took to stop the minimization
 iter = 22
 domain_name="con200"          

;============================================================
;
;  No changes required below this line
;============================================================
; read in data
;************************************************
 nrow = iter+1
 ncol = 9
 Fill_ValueF = -999.
 x    = new((/2,nrow/),float)
 cost = new((/2,nrow/),float)
 grad = new((/2,nrow/),float)

 cost@_FillValue = Fill_ValueF
 grad@_FillValue = Fill_ValueF

 filename_cost=dir+"/cost_fn"
 filename_grad=dir+"/grad_fn"

 data_cost=asciiread(filename_cost,(/nrow,ncol/),"float")
 data_grad=asciiread(filename_grad,(/nrow,ncol-1/),"float")
 do i=0,nrow-1
 x(0,i)    = data_cost(i,2)
 cost(0,i) = data_cost(i,3)
 grad(0,i) = data_grad(i,3)
 end do
print("Max/min cost "+max(cost)+" "+min(cost))
print("Max/min grad "+max(grad)+" "+min(grad))

;************************************************
; plotting parameters
;************************************************
 wks   = gsn_open_wks (works,"cost_grad")  ; open workstation

 res                  = True                     ; plot mods desired
; res@trXMinF  =  0                     ; min value on x-axis
; res@trXMaxF  =  100                   ; max value on x-axis
 res@tiXAxisString   = "Iterations"                   ; Label for the X axis
 res@xyLineThicknesses = (/2.0,2.0,4.0,4.0/)             ; make 2nd lines thicker
 res@xyLineColors      = (/"red","blue","green","purple"/)          ; change line color
 res@xyDashPatterns    = (/0,0,3,4/)                         ; choose dash patterns
; res@tiMainFont      = "Helvetica"               ; Font for title
; res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
; res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
; res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
; res@xyMarkers       = (/1,2,3,4,5/)                 ; (none, dot, asterisk)
; res@xyMarkerColors   = res@xyLineColors         ; Marker colors
; res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
 res@tiXAxisFontHeightF     = 0.03               ; Change the font size.
 res@tiYAxisFontHeightF     = 0.03
; res@xyLabelMode            = "Custom"    ; Label XY curves.
 res@xyLineLabelFontHeightF = 0.03        ; Font size and color
 res@xyLineLabelFontColor   = 2           ; for line labels

 res@lgPerimOn              = False               ; turn off box around
 res@lgLabelFontHeightF     = .02                 ; label font height
; res@xyExplicitLegendLabels = (/"nothin","thin","m=3","m=4","m=5"/)         ; create explicit labels
; res@pmLegendDisplayMode    = "Always"            ; turn on legend
 res@pmLegendSide           = "Bottom"               ; Change location of 
 res@pmLegendParallelPosF   = 0.65                ; move units right
 res@pmLegendOrthogonalPosF = -1.05                ; move units down
; res@pmLegendWidthF         = 0.2                 ; Change width and
; res@pmLegendHeightF        = 0.2                 ; height of legend.

 plts                      = new (2,"graphic")

 res@gsnDraw               = False           ; (a) do not draw
 res@gsnFrame              = False            ; (b) do not advance 'frame'
 res@tiYAxisString   = "Cost"          ; Label for the Y axis

 res@tiMainString     = " Cost function minimization for "+domain_name  
; res@trYMinF  =  min(grad)-1000                      ; min value on y-axis
; res@trYMaxF  =  max(grad)+1000                      ; max value on y-axis

 plts(0) = gsn_csm_xy (wks,x,cost,res) ; create plot

;************************************************
; plotting parameters
;************************************************

; res@tiMainString     = expt
 res@tiYAxisString   = "Gradient"      ; Label for the Y axis
; res@trYMinF  =  min(grad)-1000                      ; min value on y-axis
; res@trYMaxF  =  max(grad)+1000                      ; max value on y-axis

 res@tiMainString     = " Gradient function for "+domain_name  
 plts(1) = gsn_csm_xy (wks,x,grad,res) ; create plot
;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
;  resP@txString         = directory
; resP@gsnMaximize      = True                   ; maximize panel area
;  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
;resP@txString   = main_title

  gsn_panel(wks,plts,(/2,1/),resP)               ; now draw as one plot

end
