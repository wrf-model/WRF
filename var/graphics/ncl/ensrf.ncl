;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "./WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN.

begin
   ;********************************************************
   ; open file 
   ;********************************************************

   works = "pdf"
   directory = "/smoke/dmbarker/data/con200/xwang/2003011100/"
   directory = "/smoke/dmbarker/data/con200/xwang/2003010112/"
   directory1 = "/smoke/dmbarker/data/con200/xwang/2003010100/"
   filename = "wrfout_d01_2003-01-01_12:00:00.e001"
   ;expt = "wrfvar.singlet/wrfvar"
   ni = 44
   nj = 44
   nk = 27
   var = "T"
   k = 12 ; Level

   ;Read WRF template:
   wrf = addfile(directory1+filename+".nc", "r")
   fg1 = wrf->T(0,k,:,:)              ; import data + meta info

   ;Read prior mean:
   file1 = directory+"ensrf/"+var+"/"+var+".prior.mean"
   field1d = fbinrecread( file1, 1, (/ nk, nj, ni /) , "double" )
   field1 = dble2flt(field1d)
   prior = fg1
   prior(:,:) = field1(k,:,:) ; Overwrite field with priori mean.
   print(min(prior))
   print(max(prior))
   ;Read posterior mean:
   file2 = directory+"ensrf/"+var+"/"+var+".posterior.mean"
   field2d = fbinrecread( file2, 1, (/ nk, nj, ni /) , "double" )
   field2 = dble2flt(field2d)
   inc = fg1
   inc(:,:) = field2(k,:,:) - prior(:,:)
   print(min(inc))
   print(max(inc))

   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (2,"graphic")

   wks = gsn_open_wks(works,"ensrf")          ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")           ; select color map

   res                       = True            ; plot mods desired
   res@gsnMaximize           = True            ; uncomment to maximize size

   res@cnFillOn              = True            ; turn on color
   res@gsnSpreadColors       = True            ; use entire color map
   ;  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
   res@cnLevelSelectionMode      = "ManualLevels"
   ;  res@tiMainString          = directory
   znu = wrf->ZNU(0,:)
   res@gsnCenterString         = "Eta = "+znu(k)
   WRF_map_c(wrf,res,0)                ; set map resources

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = var
   res@gsnCenterString       = "Prior"
   res@cnMinLevelValF        = 230.0
   res@cnMaxLevelValF        = 280.0
   res@cnLevelSpacingF        =  5.0
   plts(0) = gsn_csm_contour_map(wks,prior,res)

   res@gsnLeftString         = var
   res@gsnCenterString       = "Mean Increment"
   res@cnMinLevelValF        = -1.5
   res@cnMaxLevelValF        = 1.5
   res@cnLevelSpacingF        = 0.2
   plts(1) = gsn_csm_contour_map(wks,inc,res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = "EnSRF Mean Prior/Increment"
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/1,2/),resP)               ; now draw as one plot

end
