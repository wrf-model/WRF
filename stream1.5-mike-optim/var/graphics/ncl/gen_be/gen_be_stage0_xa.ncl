;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "../WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN.

begin
   ;********************************************************
   ; open file 
   ;********************************************************

   ni = 44
   nj = 44
   nk = 27
   k = 15 ; level
   works = "x11"

   ;Ensemble mean:
   filename_fg = "/smoke/dmbarker/data/con200/noobs/2003010100/wrfout_d01_2003-01-01_12:00:00"
   first_guess = addfile(filename_fg+".nc", "r")
   fg = first_guess->T(0,k,:,:)              ; import data + meta info

   ;Ensemble increment:
   filename_inc = "/smoke/dmbarker/data/con200/xwang/2003010100/ep/pert.xa.2003010112.e001"
   rec = 1 ; u
   ;rec = 2 ; v
   ;rec = 3 ; t
   ;rec = 4 ; q
   ;rec = 5 ; ps

   incrementd = fbinrecread( filename_inc, rec, (/ ni, nj, nk /) , "double" )
   increment = dble2flt(incrementd)
   inc = fg
   inc(:,:) = increment(:,:,k)

   ;********************************************************
   ; create plots  
   ;********************************************************

   wks = gsn_open_wks(works,"gen_be_stage0_xa") ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")            ; select color map

   res1                      = True            ; plot mods desired
   res1@gsnDraw              = False           ; don't draw
   res1@gsnFrame             = False           ; don't advance frame
   res1@cnLevelSelectionMode = "ManualLevels"
   res1@cnMinLevelValF       = -5.0
   res1@cnMaxLevelValF       = 5.0
   res1@cnLevelSpacingF      = 0.5
   res1@lbOrientiation       = "Vertical"      ; vertical label bar
   res1@lbLabelAutoStride    = True            ; let NCL determine label spacing
   res1@gsnSpreadColors      = True            ; use entire color map
   res1@cnFillDrawOrder      = "predraw"       ; areas before map gets
   res1@gsnCenterString      = "Perturbation"

   res2                      = True            ; plot mods desired
   res2@cnLevelSelectionMode = "ManualLevels"
   res2@cnMinLevelValF       = -30.0
   res2@cnMaxLevelValF       = 30.0
   res2@cnLevelSpacingF      = 4.0
   res2@cnLineLabelsOn       = True            ; line labels on
   res2@gsnContourZeroLineThicknessF = 2.      ; doubles thickness of zero contour line.
   res2@gsnContourNegLineDashPattern = 1       ; sets -ve contours to dashed pattern 1.


   znu = first_guess->ZNU(0,:)
   res1@gsnCenterString         = "Eta = "+znu(k)
   WRF_map_c(first_guess,res1,0)                ; set map resources

   ;  plot                   = gsn_csm_contour_map_overlay(wks,inc,fg,res1,res2)
   plot                   = gsn_csm_contour_map(wks,fg,res2)
   draw(plot)
   frame(wks)

end
