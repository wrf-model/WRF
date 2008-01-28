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
   expt = "wrfvar.singlet.alpha56.ep1.nmc.old"
   directory = "/smoke/dmbarker/data/con200_2003011100/"+expt+"/"

   first_guess     = addfile(directory+"working/wrfvar_input.nc", "r")
   analysis        = addfile(directory+"da/2003011100/wrfvar_output.nc", "r")
   ;first_guess        = addfile(directory1+"da/2003011100/wrfvar_output.nc", "r")
   ;expt2 = "wrfvar.singlet.alpha1.ep1.ens"
   ;directory2 = "/smoke/dmbarker/data/con200_2003011100/"+expt2+"/"

   kl=12
   znu = first_guess->ZNU(0,:)

   ;********************************************************
   ; Read W at Time=6, south_north=0, convert to cm/sec
   ;********************************************************
   fg1 = first_guess->U(0,kl,:,:)              ; import data + meta info
   an1 = analysis->U(0,kl,:,:)             ; import data + meta info    
   df1 = an1
   df1 = an1 - fg1
   print (min(df1))
   print (max(df1))
   fg2 = first_guess->PH(0,kl,:,:)              ; import data + meta info
   an2  = analysis->PH(0,kl,:,:)             ; import data + meta info   
   df2 = an2
   df2 = an2 - fg2
   print (min(df2))
   print (max(df2))

   fg3 = first_guess->T(0,kl,:,:)              ; import data + meta info
   an3  = analysis->T(0,kl,:,:)             ; import data + meta info
   df3 = an3
   df3 =  an3 - fg3
   print (min(df3))
   print (max(df3))

   fg4 = first_guess->QVAPOR(0,kl,:,:)              ; import data + meta info
   an4  = analysis->QVAPOR(0,kl,:,:)             ; import data + meta info
   df4 = an4
   df4 = an4 - fg4
   df4 = 1000.0 * df4
   df4@units = "g/kg"
   print (min(df4))
   print (max(df4))

   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (4,"graphic")

   wks = gsn_open_wks(works,"psot_h."+expt)          ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")           ; select color map

   res                       = True            ; plot mods desired
   res@gsnMaximize           = True            ; uncomment to maximize size

   res@cnFillOn              = True            ; turn on color
   res@gsnSpreadColors       = True            ; use entire color map
   ;  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
   res@cnLevelSelectionMode      = "ManualLevels"
   ;  res@tiMainString          = directory
   res@gsnCenterString         = "Eta = "+znu(kl)
   WRF_map_c(first_guess,res,0)                ; set map resources

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   res@gsnLeftString         = "U"
   res@cnMinLevelValF        = -0.15
   res@cnMaxLevelValF        = 0.15
   res@cnLevelSpacingF        = 0.02 
   plts(0)                   = gsn_csm_contour_map(wks,df1,res)

   res@gsnLeftString         = "PH"
   res@cnMinLevelValF        = 0  
   res@cnMaxLevelValF        = 26
   res@cnLevelSpacingF        = 2  
   plts(1)                   = gsn_csm_contour_map(wks,df2,res)

   res@gsnLeftString         = "Theta'"
   res@cnMinLevelValF        = 0.0  
   res@cnMaxLevelValF        = 0.3 
   res@cnLevelSpacingF        = 0.03
   plts(2)                   = gsn_csm_contour_map(wks,df3,res)

   res@gsnLeftString         = "QV"
   res@cnMinLevelValF        = -0.1
   res@cnMaxLevelValF        = 0.1
   res@cnLevelSpacingF        = 0.01
   plts(3)                   = gsn_csm_contour_map(wks,df4,res)

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   resP                  = True                   ; modify the panel plot
   resP@txString         = expt 
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
