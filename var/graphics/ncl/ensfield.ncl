;*************************************************
; WRF: panel three different variables at the same time step
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "./WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN. 

begin
   ;************************************************
   ; open file and read in data
   ;************************************************

   works = "pdf"

   date1= "2006100300"
   date2= "2006100312"
   year = "2006"
   month = "10"
   day = "03"
   hour = "12"
   expt = "test"
   file_type = "wrfout_d01_"
   ; file_type = "wrfinput_d01"

   dir1 = "/rap/datc/data/jme/" + expt + "/fc/" + date1+ "/"
   dir2 = "/rap/datc/data/jme/" + expt + "/fc/" + date2+ "/"
   filename1 = file_type + year + "-" + month + "-" + day + "_" + hour + ":00:00"
   filename2 = file_type + year + "-" + month + "-" + day + "_" + hour + ":00:00.vari"
   file1 = addfile(dir1 + filename1 +".nc", "r")
   file2 = addfile(dir1 + filename2 +".nc", "r")
   file3 = addfile(dir2 + filename1 +".nc", "r")
   file4 = addfile(dir2 + filename2 +".nc", "r")

   ;************************************************
   ; Read vertical coordinate for plot labels
   ;************************************************

   znu   = file1->ZNU(0,:)                          ; (Time, bottom_top)

   ;************************************************
   ; Read fields
   ;************************************************

   var = "U"
   units = " (m/s)"
   kl = 30
   f1 = file1->U
   f2 = file2->U
   d1 = sqrt(f2-(f1*f1)) ; Convert variance to standard deviation.

   f3 = file3->U
   f4 = file4->U
   d2 = sqrt(f4-(f3*f3)) ; Convert variance to standard deviation.

   cont_min = 0
   cont_max = 10
   cont_spacing = 0.5

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, expt+date2+"."+"."+var+kl)  ; ps,pdf,x11,ncgm,eps
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

   res@cnMinLevelValF = cont_min        ; set mimimum contour level
   res@cnMaxLevelValF = cont_max        ; set maximum contour level
   res@cnLevelSpacingF = cont_spacing   ; set contour spacing
   plts(0)                   = gsn_csm_contour_map(wks,d1(nt,kl,:,:),res)
   plts(1)                   = gsn_csm_contour_map(wks,d2(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = date2
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end

