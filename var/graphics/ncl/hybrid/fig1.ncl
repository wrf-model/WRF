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

;  works = "x11"
    works = "pdf"

   date = "2006102712"
   year = "2006"
   month = "10"
   day = "28"
   hour = "00"
   fcst_range = "T+12"
   expt = "test"
   region = "t64a"
   directory = "/mmm/users/xinzhang/hybrid/" + region + "/" + expt + "/fc/" + date
   file_type = "wrfout_d01"
   filename  = file_type + "_" + year + "-" + month + "-" + day + "_" + hour + ":00:00.nc"
   dir  = directory + "/"
   dir1 = directory + ".e001/"
   dir2 = directory + ".e002/"
   dir3 = directory + ".e003/"
   dir4 = directory + ".e004/"
   dir5 = directory + ".e005/"
   dir6 = directory + ".e006/"
   dir7 = directory + ".e007/"
   dir8 = directory + ".e008/"
   dir9 = directory + ".e009/"
   dir10= directory + ".e010/"
   file0 = addfile(dir  + filename, "r")
   file1 = addfile(dir1 + filename, "r")
   file2 = addfile(dir2 + filename, "r")
   file3 = addfile(dir3 + filename, "r")
   file4 = addfile(dir4 + filename, "r")
   file5 = addfile(dir5 + filename, "r")
   file6 = addfile(dir6 + filename, "r")
   file7 = addfile(dir7 + filename, "r")
   file8 = addfile(dir8 + filename, "r")
   file9 = addfile(dir9 + filename, "r")
   file10= addfile(dir10+ filename, "r")

   ;************************************************
   ; Read vertical coordinate for plot labels
   ;************************************************

   znu   = file0->ZNU(0,:)                          ; (Time, bottom_top)

   ;************************************************
   ; Read fields
   ;************************************************

   var = "U"
   units = " (m/s)"
   kl = 32
   f  = file0->U
   f1 = file1->U
   f2 = file2->U
   f3 = file3->U
   f4 = file4->U
   f5 = file5->U
   f6 = file6->U
   f7 = file7->U
   f8 = file8->U
   f9 = file9->U
   f10 = file10->U
   cont_min = -6
   cont_max = 6 
   cont_spacing = 0.25

   df1= f1 - f
   df2= f2 - f
   df3= f3 - f
   df4= f4 - f
   df5= f5 - f
   df6= f6 - f
   df7= f7 - f
   df8= f8 - f
   df9= f9 - f
   df10= f10 - f

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, "fig1")  ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

   res                       = True             ; plot mods desired
   res@gsnSpreadColors       = True             ; use full range of colormap
   res@cnFillOn              = True             ; color plot desired
   res@cnLinesOn             = False            ; turn off contour lines
   res@cnLineLabelsOn        = False            ; turn off contour labels
   res@lbLabelAutoStride     = True             ; let NCL figure lb stride
   WRF_map_c(file0,res,0)                          ; set map resources    

   ;************************************************
   ; allocate array for 6 plots
   ;************************************************
   plts                      = new (10,"graphic")   
   ;************************************************
   ; Specify (arbitrarily chosen) subscripts 
   ; This could also be done in a do loop or explicitly specified
   ;************************************************
   nt                        = 0                ; last time step
   ;************************************************
   ; Tell NCL not to draw or advance frame for individual plots
   ;************************************************
   res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
   res@cnMinLevelValF = cont_min         ; set mimimum contour level
   res@cnMaxLevelValF = cont_max         ; set maximum contour level
   res@cnLevelSpacingF = cont_spacing    ; set contour spacing

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var + units
   res@gsnRightString        = "Member 1"
   plts(0)                   = gsn_csm_contour_map(wks,df1(nt,kl,:,:),res)
   res@gsnRightString        = "Member 2"
   plts(1)                   = gsn_csm_contour_map(wks,df2(nt,kl,:,:),res)
   res@gsnRightString        = "Member 3"
   plts(2)                   = gsn_csm_contour_map(wks,df3(nt,kl,:,:),res)
   res@gsnRightString        = "Member 4"
   plts(3)                   = gsn_csm_contour_map(wks,df4(nt,kl,:,:),res)
   res@gsnRightString        = "Member 5"
   plts(4)                   = gsn_csm_contour_map(wks,df5(nt,kl,:,:),res)
   res@gsnRightString        = "Member 6"
   plts(5)                   = gsn_csm_contour_map(wks,df6(nt,kl,:,:),res)
   res@gsnRightString        = "Member 7"
   plts(6)                   = gsn_csm_contour_map(wks,df7(nt,kl,:,:),res)
   res@gsnRightString        = "Member 8"
   plts(7)                   = gsn_csm_contour_map(wks,df8(nt,kl,:,:),res)
   res@gsnRightString        = "Member 8"
   plts(8)                   = gsn_csm_contour_map(wks,df9(nt,kl,:,:),res)
   res@gsnRightString        = "Member 10"
   plts(9)                   = gsn_csm_contour_map(wks,df10(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = date + " Level " + kl + " " + fcst_range
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/4,4,4/),resP)               ; now draw as one plot

end

