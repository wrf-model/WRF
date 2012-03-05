;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;

begin
   setfileoption ("bin","Readbyteorder","BigEndian")
   works = getenv("GRAPHIC_WORKS")
   region = getenv("REGION")
   dir    = getenv("BE_DIR")
   ni   = stringtointeger(getenv("NUM_WE"))
   nj   = stringtointeger(getenv("NUM_SN"))
   nk   = stringtointeger(getenv("NUM_LEVELS"))
;-------------------------------------------------------------
;   works = "x11"
;   region ="con200"
;   dir    = "/ptmp/rizvi/data/con200/run_gen_mbe/gen_be5"
;   ni   = 44
;   nj   = 44 
;   nk   = 27
;-------------------------------------------------------------

   nij = ni * nj
   data = new((/nk,nij/), "float")
   data1 = new((/nj,ni/), "float")

   y = new((/nk/),float)
   y = ispan(1,nk,1)

; Process chi-field
   print("nk="+nk)
   print("nij="+nij)
   chi = new((/nk/),float)
   filename = "psi.chi.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   chi(:) = avg1
   print("chi avge="+chi(:))
; Process t-field
   t = new((/3,nk/),float)
   filename = "psi.t.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   t(0,:) = avg1
   filename = "chi_u.t.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   t(1,:) = avg1

   t(2,:) = t(0,:) + t(1,:)

; Process rh-field
   rh = new((/5,nk/),float)

   filename = "psi.rh.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   rh(0,:) = avg1

   filename = "chi_u.rh.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   rh(1,:) = avg1

   filename = "t_u.rh.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   rh(2,:) = avg1

   filename = "ps_u.rh.bin"                       
   do k = 0, nk - 1
   data(k,:) = fbinrecread(dir+"/"+filename, k, nij, "float")
   end do
   avg1 = dim_sum(data) / nij
   rh(3,:) = avg1

   rh(4,:) = rh(0,:) + rh(1,:) + rh(2,:) + rh(3,:)


; Process ps-field
   ps = new((/3,nj/),float)
   xx = new((/nj/),float)
   xx = ispan(1,nj,1)
   bar_var = new((/3/),float)
   bar_val = ispan(1,3,1)

   filename = "psi.ps.bin"                       
   do k = 0, nj - 1
   data1(k,:) = fbinrecread(dir+"/"+filename, k, ni, "float")
   end do
   avg2 = dim_sum(data1) / ni
   ps(0,:) = avg2
   filename = "chi_u.ps.bin"                       
   do k = 0, nj - 1
   data1(k,:) = fbinrecread(dir+"/"+filename, k, ni, "float")
   end do
   avg2 = dim_sum(data1) / ni
   ps(1,:) = avg2

   ps(2,:) = ps(0,:) + ps(1,:)


   ;************************************************
   ; plotting parameters
   ;************************************************
   plts                      = new (4,"graphic")
   wks   = gsn_open_wks (works,"gen_mbe_contrib_"+region)

;  plot chi, t & rh  diagnostics with latitudes
   res                  = True                     ; plot mods desired
   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@tmXBMode = "Explicit"
   res@tmXBValues = (/.1 , .2 , .3, .4, .5, .6, .7/)
   res@tmXBLabels = (/".1" , ".2" , ".3", ".4", ".5", ".6", ".7" /)
    res@tiXAxisFont     = "Times-roman"                ; Font for X axis label
    res@tiYAxisFont     = "Times-roman"                ; Font for Y axis label
    res@tiXAxisFontThicknessF  = 2.5
    res@tiYAxisFontThicknessF  = 2.5
    res@tiXAxisFontHeightF     = 0.03                  ; Change the font size.
    res@tiYAxisFontHeightF     = 0.03
    res@tiXAxisFontAspectF     = 1.5
    res@tiYAxisFontAspectF     = 1.5


   res@trXMinF  =  0                    ; min value on x-axis
   res@trXMaxF  =  max(chi) + .2          ; max value on x-axis
   res@trYMinF  =  1.0                  ; min value on y-axis
   res@trYMaxF  =  nk                   ; max value on y-axis

;   res@tiXAxisString   = "Correlation"                        ; Label for the X axis
   res@xyLineThicknesses = (/4,4,4,4,4/)             ; make 2nd lines thicker
   ; res@trYReverse        = True                       ; reverse Y-axis
   res@xyDashPatterns    = (/0,0,0,0,0/)              ; choose dash patterns
   res@tiMainFont      = "Helvetica"               ; Font for title
   res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
   res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
   res@tiXAxisFontHeightF     = 0.03               ; Change the font size.
   res@tiYAxisFontHeightF     = 0.03
   ; res@xyLabelMode          = "Custom"    ; Label XY curves.
   res@xyLineLabelFontHeightF = 0.03        ; Font size and color
   res@xyLineLabelFontColor   = 2           ; for line labels

   res@lgPerimOn              = False               ; turn off box around
   res@lgLabelFontHeightF     = .025                ; label font height
   res@tmXTOn = False                               ; turn off top   labels
   res@tmYROn = False                               ; turn off top   labels

   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendParallelPosF   = 0.80                ; move units right
   res@pmLegendOrthogonalPosF = -.95                ; move units down
   res@pmLegendWidthF         = 0.1                 ; Change width and
   res@tiYAxisString   = "Model Levels"              ; Label for the Y axis
   res@trXMinF  =  0                    ; min value on x-axis


   res@xyLineColors      = (/"black","red","green","blue","purple"/)          ; change line color

   res@xyExplicitLegendLabels = (/"psi"," ", " "," "," "/)     ; create explicit labels
   res@lgLabelFontThicknessF  = 0.04
   res@tiMainString    = "contribute to chi"                         ; Main title           
   res@tiMainFontColor = "black"                        ; Title color          
   res@tiMainFontHeightF = 0.04  

   plts(0) = gsn_csm_xy (wks,chi,y,res) ; create plot

   res@trXMaxF  =  max(t) + .2          ; max value on x-axis

  res@xyLineColors      = (/"blue","red","black","green","purple"/)          ; change line color
   res@xyExplicitLegendLabels = (/"psi","chi_u", "blance"," "," "/)     ; create explicit labels

   res@tiMainString    = "contribute to t"                   ; Main title           
   plts(1) = gsn_csm_xy (wks,t,y,res) ; create plot

   res@trXMaxF  =  max(rh) + .2          ; max value on x-axis

   res@xyLineColors      = (/"blue","red","green","purple","black"/)          ; change line color
   res@xyExplicitLegendLabels = (/"psi","chi_u","t_u", "ps_u", "blance"/)     ; create explicit labels
   res@tiMainString    = "contribut to rh"                   ; Main title           
   plts(2) = gsn_csm_xy (wks,rh,y,res) ; create plot

;  plot ps diagnostics with latitudes

   res1                  = True                     ; plot mods desired
   res1@gsnDraw               = False            ; (a) do not draw
   res1@gsnFrame              = False            ; (b) do not advance 'frame'
    res1@gsnXYBarChart         = True
    res1@gsnXYBarChartBarWidth = 0.25                ; change bar widths
    res1@gsnYRefLine           = 0.0
    res1@gsnXYBarChartColors2  = (/"blue","red","black"/)          ; change line color
   ;   res@gsnXYBarChartOutlineOnly = True
   res1@trXMinF  =  0          ; min value on x-axis
   res1@trXMaxF  =  dimsizes(bar_val) + 1          ; max value on x-axis

    res1@tmXBMode         = "Explicit"
    res1@tmXBLabels       = (/"psi","chi_u", "blance"/)     ; create explicit labels
    res1@tmXBValues       = bar_val 
;    res1@tmXBLabelAngleF  = 0.
    res1@tmXTBorderOn     = True                        ; set up the top border
    res1@tmXTOn           = False                       ; set the top tickmark
    res1@tmYRBorderOn     = True                        ; set up the Right border
    res1@tmYROn           = False                       ; set the Right tickmark
    res1@tiXAxisFont     = "Times-roman"                ; Font for X axis label
    res1@tiYAxisFont     = "Times-roman"                ; Font for Y axis label
    res1@tiXAxisFontThicknessF  = 2.5
    res1@tiYAxisFontThicknessF  = 2.5
    res1@tiXAxisFontHeightF     = 0.03                  ; Change the font size.
    res1@tiYAxisFontHeightF     = 0.03
    res1@tiXAxisFontAspectF     = 1.5
    res1@tiYAxisFontAspectF     = 1.5


   res1@tiMainString    = "contribut to ps"                         ; Main title           
   res1@tiMainFontColor = "black"                        ; Title color          
   res1@tiMainFontHeightF = 0.04  
   plts(3) = gsn_csm_xy (wks,bar_val,ps(:,0),res1) ; create plot

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
    resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot


   delete (plts)


  end
