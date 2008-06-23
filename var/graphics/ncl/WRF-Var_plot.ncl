;****************************************************************
; Multi-purpose NCL script for display of WRF-Var input/output
;
;        Author: Syed RH Rizvi,  MMM/NCAR  
;        Date  : 03/21/2008   
;
; Purpose: It can plot FG, Analysis and also (Analysis - FG)
;          fg & an (defined below) needs to be defined suitably
;
;   Note:  You need to specify the following:
;          (a) "frst_guess" & "analysis" files
;          (b) Desired level (kl)
;          (c) Fix plot_data (See logic below)
;          (d) For display   execute the following comman
;              ncl  WRF-Var_plot.ncl
;****************************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "./WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN. 

begin
   ;************************************************
   ; open file and read in data
   ;************************************************

   works = "pdf"

   first_guess = addfile("/karri/users/xinzhang/support/con200/run_cpu1/2007010200/wrfvar/working/wrfinput_d01"+".nc", "r")
   analysis = addfile("/karri/users/xinzhang/support/con200/run_cpu1/2007010200/wrfvar/working/wrfvar_output"+".nc", "r")

   ;************************************************
   ; Read vertical coordinate for plot labels
   ;************************************************

   znu   = first_guess->ZNU(0,:)                          ; (Time, bottom_top)

   ;************************************************
   ; Read fields
   ;************************************************
;    Fix the desired level below  
   kl = 18

   var = "U"
   fg = first_guess->U   ;Theta-T0
   an = analysis->U      ;Theta-T0
;   plot_data = an - fg
   plot_data = an

;   var = "T"
;   fg = first_guess->T ;Theta- 300
;   an = analysis->T    ;Theta- 300
;   plot_data = an - fg
;   plot_data = fg + 300.0

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, "WRF-VAR_"+var+"_level_"+kl)       ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

   res                       = True             ; plot mods desired
   res@gsnSpreadColors       = True             ; use full range of colormap
   res@cnFillOn              = True             ; color plot desired
   res@cnLinesOn             = False            ; turn off contour lines
   res@cnLineLabelsOn        = False            ; turn off contour labels
   res@lbLabelAutoStride     = True             ; let NCL figure lb stride
   WRF_map_c(first_guess,res,0)                           ; set map resources    

   ;************************************************
   ; allocate array for 6 plots
   ;************************************************
   ;  plts                      = new (2,"graphic")   
   ;************************************************
   ; Specify (arbitrarily chosen) subscripts 
   ; This could also be done in a do loop or explicitly specified
   ;************************************************
   nt                        = 0                ; last time step
   ;************************************************
   ; Tell NCL not to draw or advance frame for individual plots
   ;************************************************
;   res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
;   res@cnMinLevelValF = -10               ; set mimimum contour level
;   res@cnMaxLevelValF = 10               ; set maximum contour level
;   res@cnLevelSpacingF = 1               ; set contour spacing

   ;  res@gsnDraw               = False            ; (a) do not draw
   ;  res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var+"("+kl+")" 
   plot                      = gsn_csm_contour_map(wks,plot_data(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   ;  resP                  = True                   ; modify the panel plot
   ;  resP@txString         = directory
   ;  resP@gsnMaximize      = True                   ; maximize panel area
   ;  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  ;  gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot


end

