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

   date1 = "2006102712"
   date2 = "2006102800"
   year = "2006"
   month = "10"
   day = "28"
   hour = "00"
   fcst_range = "T+00"
   region = "t64a"
   expt = "test"
   run_dir = "wrfvar.u250.h6"
   run_dir = "wrfvar.ctrl"
   run_dir = "wrfvar.jb0.0"
   run_dir = "wrfvar.u250.ctrl"
   run_dir = "wrfvar"
   file_type = "wrfout_d01"
;   file_type = "wrfinput_d01"

   directory = "/mmm/users/dmbarker/data/" + region + "/" + expt
   directory = "/mmm/users/xinzhang/hybrid/" + region + "/" + expt
   dir1 = directory + "/fc/" + date1 + "/"
   dir2 = directory + "/run/" + date2 + "/" + run_dir + "/working/"
   filename1 = file_type + "_" + year + "-" + month + "-" + day + "_" + hour + ":00:00"
   filename2 = "wrfvar_output"
   file1 = addfile(dir1 + filename1 +".nc", "r") ; First-guess
   file2 = addfile(dir2 + filename2 +".nc", "r") ; Analysis

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
   f1 = file1->U
   f2 = file2->U
   d1 = f2 - f1
   cont_min = -10.0
   cont_max = 10.0
   cont_int = 0.5 
   cont_min = -0.4
   cont_max = 0.4
   cont_int = 0.04 

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, "fig3_"+run_dir)  ; ps,pdf,x11,ncgm,eps
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
;   plts                      = new (2,"graphic")   
   ;************************************************
   ; Specify (arbitrarily chosen) subscripts 
   ; This could also be done in a do loop or explicitly specified
   ;************************************************
   nt                        = 0                ; last time step
   ;************************************************
   ; Tell NCL not to draw or advance frame for individual plots
   ;************************************************
   res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
;   res@gsnDraw               = False            ; (a) do not draw
;   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var + units
   res@gsnRightString        = " HYBRID Level " + kl

   res@cnMinLevelValF = cont_min        ; set mimimum contour level
   res@cnMaxLevelValF = cont_max        ; set maximum contour level
   res@cnLevelSpacingF = cont_int       ; set contour spacing
   plot                     = gsn_csm_contour_map(wks,d1(nt,kl,:,:),res)
;   plts(1)                   = gsn_csm_contour_map(wks,d1(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
;   resP                  = True                   ; modify the panel plot
;   resP@txString         = date2 + " " + fcst_range
;   resP@gsnMaximize      = True                   ; maximize panel area
;   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
;   gsn_panel(wks,plts,(/2,1/),resP)               ; now draw as one plot

end

