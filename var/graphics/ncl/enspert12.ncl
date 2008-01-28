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

   date = "2006100300"
   year = "2006"
   month = "10"
   day = "03"
   hour = "12"
   fcst_range = "T+12"
   expt = "etkf"
   file_type = "wrfout_d01_"
   ; file_type = "wrfinput_d01"
   dir  = "/rap/datc/data/jme/" + expt + "/fc/" + date + "/"
   filename  = file_type + year + "-" + month + "-" + day + "_" + hour + ":00:00"
   filename1 = filename + ".e001"
   filename2 = filename + ".e002"
   filename3 = filename + ".e003"
   filename4 = filename + ".e004"
   filename5 = filename + ".e005"
   filename6 = filename + ".e006"
   filename7 = filename + ".e007"
   filename8 = filename + ".e008"
   filename9 = filename + ".e009"
   filename10 = filename + ".e010"
   filename11 = filename + ".e011"
   filename12 = filename + ".e012"
   file0 = addfile(dir + filename +".nc", "r")
   file1 = addfile(dir + filename1+".nc", "r")
   file2 = addfile(dir + filename2+".nc", "r")
   file3 = addfile(dir + filename3+".nc", "r")
   file4 = addfile(dir + filename4+".nc", "r")
   file5 = addfile(dir + filename5+".nc", "r")
   file6 = addfile(dir + filename6+".nc", "r")
   file7 = addfile(dir + filename7+".nc", "r")
   file8 = addfile(dir + filename8+".nc", "r")
   file9 = addfile(dir + filename9+".nc", "r")
   file10 = addfile(dir + filename10+".nc", "r")
   file11 = addfile(dir + filename11+".nc", "r")
   file12 = addfile(dir + filename12+".nc", "r")

   ;************************************************
   ; Read vertical coordinate for plot labels
   ;************************************************

   znu   = file0->ZNU(0,:)                          ; (Time, bottom_top)

   ;************************************************
   ; Read fields
   ;************************************************

   var = "U"
   units = " (m/s)"
   kl = 30
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
   f11 = file11->U
   f12 = file12->U
   cont_min = -2
   cont_max = 2 
   cont_spacing = 0.1

   ; var = "T"
   ; units = " (degK)"
   ; kl = 5
   ; f  = file0->T ;Theta-T0
   ; f1 = file1->T ;Theta-T0
   ; f2 = file2->T ;Theta-T0
   ; f3 = file3->T ;Theta-T0
   ; f4 = file4->T ;Theta-T0
   ; f5 = file5->T ;Theta-T0
   ; f6 = file6->T ;Theta-T0
   ; f7 = file7->T ;Theta-T0
   ; f8 = file8->T ;Theta-T0
   ; f9 = file9->T ;Theta-T0
   ; f10 = file10->T ;Theta-T0
   ; f11 = file11->T ;Theta-T0
   ; f12 = fil12->T ;Theta-T0
   ; cont_min = -3.0
   ; cont_max =  3.0

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
   df11= f11 - f
   df12= f12 - f

   ;************************************************
   ; create plots  
   ;************************************************

   wks = gsn_open_wks(works, "ep."+date+"."+fcst_range+"."+var+kl)  ; ps,pdf,x11,ncgm,eps
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
   plts                      = new (12,"graphic")   
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
   res@gsnRightString        = "Level " + kl
   plts(0)                   = gsn_csm_contour_map(wks,df1(nt,kl,:,:),res)
   plts(1)                   = gsn_csm_contour_map(wks,df2(nt,kl,:,:),res)
   plts(2)                   = gsn_csm_contour_map(wks,df3(nt,kl,:,:),res)
   plts(3)                   = gsn_csm_contour_map(wks,df4(nt,kl,:,:),res)
   plts(4)                   = gsn_csm_contour_map(wks,df5(nt,kl,:,:),res)
   plts(5)                   = gsn_csm_contour_map(wks,df6(nt,kl,:,:),res)
   plts(6)                   = gsn_csm_contour_map(wks,df7(nt,kl,:,:),res)
   plts(7)                   = gsn_csm_contour_map(wks,df8(nt,kl,:,:),res)
   plts(8)                   = gsn_csm_contour_map(wks,df9(nt,kl,:,:),res)
   plts(9)                   = gsn_csm_contour_map(wks,df10(nt,kl,:,:),res)
   plts(10)                  = gsn_csm_contour_map(wks,df11(nt,kl,:,:),res)
   plts(11)                  = gsn_csm_contour_map(wks,df12(nt,kl,:,:),res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = date + " " + fcst_range
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/4,4,4/),resP)               ; now draw as one plot

end

