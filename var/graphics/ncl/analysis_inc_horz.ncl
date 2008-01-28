;***************************************************************
;The Program read directly "ANALYSIS INCREMENT" file from wrfvar 
;Plot Horiz. cross section at particular Level
;****************************************************************
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Copyright (C) 2006
; University Corporation for Atmospheric Research
; All Rights Reserved
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; Author: Ashish Routray , S.R.H. Rizvi and Ananda Kumar Das
;         National Center for Atmospheric Research
;         Boulder, CO 80301
;
; Date: 01/17/07
;
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;=========================================================;
; Note:  Make sure following.
;         a) You have NCL Version: 4.2.0.a.034 upward
;         b) "wrf_user_fortran_util_0.so" file is created by using
;            "RWRAPIT wrf_user_fortran_util_0.f" command
;****************************************************************************
;
;****************Change lines below *****************************************
;
load "/ptmp/rizvi/trunk/graphics/ncl/WRF_contributed.ncl.test"
load "/ptmp/rizvi/trunk/graphics/ncl/WRFUserARW.ncl"
external wrf_user_fortaran_util_0 "/ptmp/rizvi/trunk/graphics/ncl/wrf_user_fortran_util_0.so"
;=========================================================;
begin
   ;********************************************************
   ; open file 
   ;********************************************************

   ;*********** only change below line ***************
   works = "x11"
   ;   works = "pdf"
   ;   works = "ncgm"
   lev =1
   var_name = "t"
   ;   n1=75
   ;   n2=75
   ;   expt = var_name+"_"+n1+"_"+n2+"_"+lev
   expt="test" 

   sigma_lev=12 

   ;************ here read input file ***************
   filename_inc = "/mmm/mmmtmp/rizvi/data/india_30km/psot_with_cv_options_hum_rh_md2/run/2005080500/wrfvar_psot1/working/analysis_increments"
   setfileoption("bin","ReadByteOrder","BigEndian") ; Read BigEndian(IBM) file in LittleEndian(Linux) Mach.
   ;*************************************************
   var1="T"
   units1="K"
   var2="U"
   units2="mps"
   var3="QV"
   units3="gm/Kg"

   ;**********************No need change lines here onward **********************

   ;****************************************************
   ; Read records from ANALYSIS INCREMENT file
   ;****************************************************

   rec_date=0
   rec_ni_nj_nk=1
   rec_map_proj  = 2
   rec_truelat  = 3
   rec_sigma = 4
   rec_field = 5
   rec_lat_lon = 6

   ;*****************************************************
   ni_nj_nk = fbinrecread( filename_inc, rec_ni_nj_nk, 6, "integer" )
   nx = ni_nj_nk(1)
   ny = ni_nj_nk(3)
   nz = ni_nj_nk(5)
   nk = 7*(nz+1)+2
   map_proj_etc= fbinrecread( filename_inc, rec_map_proj, 3, "integer" )
   truelat = fbinrecread( filename_inc, rec_truelat, 17, "double" )
   inc_all = fbinrecread( filename_inc, rec_field, (/ nk, ny+1, nx+1 /) , "double" )
   lat_lon = fbinrecread( filename_inc, rec_lat_lon, (/ 10, ny+1, nx+1 /) , "double" )


   print("map proj "+map_proj_etc(0))
   map_proj=map_proj_etc(0)
   print ("nx " +nx)
   print ("ny " +ny)
   print ("nz " +nz)

   inc_u = new ((/nz+1,ny+1,nx+1/),"double")
   inc_v = new ((/nz+1,ny+1,nx+1/),"double")
   inc_t = new ((/nz+1,ny+1,nx+1/),"double")
   inc_q = new ((/nz+1,ny+1,nx+1/),"double")
   inc_p = new ((/nz+1,ny+1,nx+1/),"double")
   lat = new ((/ny+1,nx+1/),"double")
   lon = new ((/ny+1,nx+1/),"double")


   do ik = 0, nz
      inc_u (ik,:,:) = inc_all(ik,:,:) 
      inc_v (ik,:,:) = inc_all(nz+1+ik,:,:) 
      inc_p (ik,:,:) = inc_all(3*nz+3+ik,:,:)
      inc_t (ik,:,:) = inc_all(4*nz+4+ik,:,:) 
      inc_q (ik,:,:) = inc_all(5*nz+5+ik,:,:)
   end do
   cen_lon  = truelat(4)
   truelat1 = truelat(6)
   truelat2 = truelat(7)

   print ("cen_lon " +cen_lon)
   print ("truelat1 " +truelat1)
   print ("truelat2 " +truelat2)

   lat(:,:) = lat_lon(4,:,:) 
   lon(:,:) = lat_lon(5,:,:) 

   ;************* Read Ploting Variables ****************
   T= inc_t(sigma_lev-1,:,:)
   U= inc_u(sigma_lev-1,:,:)
   Q =inc_q(sigma_lev-1,:,:)*1000.
   P =inc_p(sigma_lev-1,:,:)

   ;************* Read Ploting Variables end ****************

   print(" T Min "+min(T))
   print(" T Max "+max(T))

   ;********************************************************
   ; create plots  
   ;********************************************************

   plts                      = new (3,"graphic")

   ; wks = gsn_open_wks(works,"psot_h."+expt)          ; ps,pdf,x11,ncgm,eps
   wks = gsn_open_wks(works,expt)           ; ps,pdf,x11,ncgm,eps
   gsn_define_colormap(wks,"gui_default")             ; select color map

   res                       = True                   ; plot mods desired
   res@gsnDraw               = False                   ; (a) do not draw
   res@gsnFrame              = False                   ; (b) do not advance 'frame'

   res@gsnMaximize           = True                   ; uncomment to maximize size

   res@cnFillOn              = True                   ; turn on color
   res@lbLabelAutoStride     = True                   ; let NCL determine label spacing



   dims = dimsizes(lat)
   nlat = dimsizes(lat)
   nlon = dimsizes(lon)
   ; xlat = lat(n1,n2)                               ;Lat & Lon value at this point
   ; xlon = lon(n1,n2)
   fmt = "%5.1f"                                    ;format for lat & long
   ;     wlat=sprintf(fmt,xlat)
   ;     wlon=sprintf(fmt,xlon)

   ; varname_label = var_name+"("+sprintf(fmt,xlat)+","+sprintf(fmt,xlon)+","+sigma_lev+")"
   ;
   ; Select portion of the map to view.

   ;*************** Loop for Map Projection *************

   ; For Lambert Projection

   if (map_proj .eq. 1) then
      print ("cen_lon " +cen_lon)
      print ("truelat1 " +truelat1)
      print ("truelat2 " +truelat2)
      res@mpProjection          = "LambertConformal"
      res@mpLambertParallel1F   = truelat1           ;truelat1
      res@mpLambertParallel2F   = truelat2           ;truelat2
      res@mpLambertMeridianF    = cen_lon            ;cen_lon
      res@mpLimitMode           = "Corners"
      res@mpLeftCornerLatF      = lat(0,0)
      res@mpLeftCornerLonF      = lon(0,0)
      res@mpRightCornerLatF     = lat(ny-1,nx-1)
      res@mpRightCornerLonF     = lon(ny-1,nx-1)
      res@mpOutlineDrawOrder    = "PostDraw"  ; draw continental outline last
      res@mpFillDrawOrder       = "PreDraw"
      res@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
      res@mpFillOn              = False       ; turn off map fill
   end if

   ; For Polar Projection

   if (map_proj .eq. 2) then
      res@gsnPolar             = "NH"               ; specify the hemisphere
   end if

   ; For Mercator Projection

   if (map_proj .eq. 3) then
      res@mpProjection          = "mercator"                                            ; projection
      res@mpLimitMode           = get_res_value_keep(res, "mpLimitMode","Corners")      ; corner point 
      res@mpLeftCornerLatF      = get_res_value_keep(res, "mpLeftCornerLatF",lat(0,0))  ; Lat corner point
      res@mpLeftCornerLonF      = get_res_value_keep(res, "mpLeftCornerLonF",lon(0,0))  ;Lon corner point
      res@mpRightCornerLatF           = max(lat)         ; Max Lat                      ; Max Lat
      res@mpRightCornerLonF           = max(lon)         ; Max Lon                      ; Max Lon
      res@pmTickMarkDisplayMode       = get_res_value_keep(res, "pmTickMarkDisplayMode","Always") ; Tick mark
   end if

   ;*************** Loop for Map Proj end *************

   res@tfDoNDCOverlay              = True             ; do not transform data
   ;
   ; Tick mark resources
   ;
   res@tmXBMajorLengthF      = get_res_value(res, "tmXBMajorLengthF",0.01) ;Size of tick mark in x-axis
   res@tmYLMajorLengthF      = get_res_value(res, "tmYLMajorLengthF",0.01) ;Size of tick mark in y-axis
   res@tmXTOn                = get_res_value(res,"tmXTOn",False)
   res@tmYROn                = get_res_value(res,"tmYROn",False)
   res@tmYRLabelsOn          = get_res_value(res,"tmYRLabelsOn",True)
   res@tmXBBorderOn          = get_res_value(res,"tmXBBorderOn",True)
   res@tmXTBorderOn          = get_res_value(res,"tmXTBorderOn",True)
   res@tmYLBorderOn          = get_res_value(res,"tmYLBorderOn",True)
   res@tmYRBorderOn          = get_res_value(res,"tmYRBorderOn",True)


   ;*********** Plot the Variables ******************
   res@gsnLeftString         = var1+"("+units1+")"     
   plts(0)                   = gsn_csm_contour_map(wks,T,res)
   ;plot(0)                   = gsn_csm_contour_map_polar(wks,T, res) ; for Plor projection 

   ; res@lbLabelStride = 2                                            ; Color bar value writing
   res@gsnLeftString         = var2+"("+units2+")"     
   plts(1)                   = gsn_csm_contour_map(wks,U,res)
   ; plot(1)                   = gsn_csm_contour_map_polar(wks,U, res) ; for Plor projection 

   res@gsnLeftString         = var3+"("+units3+")"     
   plts(2)                   = gsn_csm_contour_map(wks,Q,res)
   ; plot(2)                   = gsn_csm_contour_map_polar(wks,Q, res) ; for Plor projection 

   ;*********** Plot the Variables end ******************

   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
   ; title_1st = "Analysis Increment with Single Obs. "
   ; title_2nd = "Var="+var_name+", "  +"Lat="+wlat+", " +"Lon="+wlon+", " +"Sigma Lev="+sigma_lev+", " +"Err=1, Val=1, Scale=1"
   title_2nd = "Analysis increments at Sigma Lev= "+sigma_lev
   ;-------------------------------------------------------------------------------
   resP                  = True                   ; modify the panel plot
   resP@gsnFrame = False
   ; resP@gsnPanelBottom = 0.15
   resP@gsnPanelTop = 0.9 
   resP@txFontHeightF = 0.01
   resP@txString         = title_2nd
   resP@gsnMaximize      = True                   ; maximize panel area
   resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts,(/2,1/),resP)               ; now draw as one plot
   ;************************************************
   ; add text to plot
   ;************************************************
   txres               = True                     ; text mods desired
   txres@txFontHeightF = 0.02                      ; font smaller. default big
   ; gsn_text_ndc (wks,title_1st,0.5,0.98,txres)
   frame(wks)                                     ; now advance frame

end
