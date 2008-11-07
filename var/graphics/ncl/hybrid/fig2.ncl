;*************************************************
; WRF: panel three different variables at the same time step
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "../WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN. 

begin
   ;************************************************
   ; open file and read in data
   ;************************************************

   works = "pdf"
;  works = "x11"

   date = "2006102712"
   year = "2006"
   month = "10"
   day = "28"
   hour = "00"
   fcst_range = "T+12"
   region = "t64a"
   expt = "test"
   file_type = "wrfout_d01"
;   file_type = "wrfinput_d01"

   directory = "/mmm/users/xinzhang/hybrid/" + region + "/"
   dir = directory + expt + "/fc/" + date + "/"
   filename1 = file_type + "_" + year + "-" + month + "-" + day + "_" + hour + ":00:00"
   filename2 = file_type + "_" + year + "-" + month + "-" + day + "_" + hour + ":00:00.vari"
   file1 = addfile(dir + filename1 +".nc", "r")
   file2 = addfile(dir + filename2 +".nc", "r")

   ;************************************************
   ; Read vertical coordinate for plot labels
   ;************************************************

   znu   = file1->ZNU(0,:)                          ; (Time, bottom_top)

   ;************************************************
   ; Read fields
   ;************************************************

   var = "U"
   units = " (m/s)"
   kl = 32
;   var = "Theta"
;   units = " (degK)"
;   kl = 9
   f1 = file1->U
   f2 = file2->U
   d1 = sqrt((f2-(f1*f1))) ; Convert variance to standard deviation.

   con1_min = -60.0
   con1_max =  60.0
   con1_int =   5.0
   con2_min = 0.0
   con2_max = 6.0
   con2_int = 0.25

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, "fig2")  ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

   res                       = True             ; plot mods desired
   res@gsnSpreadColors       = True             ; use full range of colormap
   res@cnFillOn              = True             ; color plot desired
   res@cnLinesOn             = False            ; turn off contour lines
   res@cnLineLabelsOn        = False            ; turn off contour labels
   res@lbLabelAutoStride     = True             ; let NCL figure lb stride
   WRF_map_c(file1,res,0)                          ; set map resources    

   ;************************************************
   ; allocate array for 6 plots
   ;************************************************
   plts                      = new (2,"graphic")   
   ;************************************************
   ; Specify (arbitrarily chosen) subscripts 
   ; This could also be done in a do loop or explicitly specified
   ;************************************************
   nt                        = 0                ; last time step
   ;************************************************
   ; Tell NCL not to draw or advance frame for individual plots
   ;************************************************
   res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var + units
   res@gsnRightString        = "Level " + kl

   res@cnMinLevelValF = con1_min        ; set mimimum contour level
   res@cnMaxLevelValF = con1_max        ; set maximum contour level
   res@cnMinLevelValF = min(f1(nt,kl,:,:))        ; set mimimum contour level
   res@cnMaxLevelValF = max(f1(nt,kl,:,:))        ; set mimimum contour level
   res@cnLevelSpacingF = con1_int       ; set contour spacing
   plts(0)                   = gsn_csm_contour_map(wks,f1(nt,kl,:,:),res)


   res@cnMinLevelValF = con2_min        ; set mimimum contour level
   res@cnMaxLevelValF = con2_max        ; set maximum contour level
   res@cnLevelSpacingF = con2_int       ; set contour spacing
   plts(1)                   = gsn_csm_contour_map(wks,d1(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = date + " " + fcst_range
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,1/),resP)               ; now draw as one plot

end

