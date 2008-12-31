;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
; Description:
;    This ncl script works with da_plot_psot.ksh in $WRFVAR_DIR/scripts.
;    It was designed for plotting PSOT figures. 
;    If used without da_run_plot.ksh, it requires the following variables
;    defined beforehand:
;      works: string for the type of workstation to open.
;      expt : string for experiment name (for plot title)
;      date=2006120100: date for data
;      kl=12: grid number of PSO eta level(z) 
;      xlon=112: grid number of PSO longitude(x)
;      xlat=64 : grid number of PSO latitude(y)
;      var="q" : variable name for this PSOT
;      varunit : variable unit for this PSOT
;      omb="1" : observation-background value
;      err="1" : sigma_o (error for observation)
;      bakfile : path for the background file  "wrfinput_d01"
;      analfile: path for the analysis file "analysis"
; Usage:
;    An example to use this script:
;    (1) define all environment variables in NCL command line:
;    run the following command:
;    >ncl 'works="pdf"' 'expt="psot_psot5"' \
;         'kl=12' 'xlon=112' 'xlat=64' 'var="q"' 'date="2006120100"' \
;         'omb="0.001"' 'err="0.001"' 'varunit="kg kg-1"'\
;         'bakfile="/rap/datc/huishao/data/caa1/rc/2006120100/wrfinput_d01"'\ 
;         'analfile="/ptmp/huishao/data/caa1/psot/fc/psot5/2006120100/analysis"' psot_xy_auto.ncl
;     (2) define all enrivonment variables in the ncl script:
;     add the following line into the script:
;       works="pdf"
;       expt="psot"
;       kl=12
;       xlon=112
;       xlat=64
;       var="q"
;       date=2006120100
;       omb="0.001"
;       err="0.001"
;       varunit="kg/kg"
;       bakfile="/rap/datc/huishao/data/caa1/rc/2006120100/wrfinput_d01"
;       analfile="/ptmp/huishao/data/caa1/psot/fc/psot5/2006120100/analysis"
;  Author: 
;    Hui Shao NCAR/DATC 09/19/2007
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$WRFVAR_DIR/var/graphics/ncl/WRF_contributed.ncl.test"     

begin
;********************************************************
; open file 
;********************************************************
  first_guess     = addfile(bakfile+".nc", "r")
  analysis        = addfile(analfile+".nc", "r")

  znu = first_guess->ZNU(0,:)
  plong = first_guess->XLONG(0,xlat-1,xlon-1)
  plat = first_guess->XLAT(0,xlat-1,xlon-1)

  title="PSOT"+" - "+var
  subtitle="Lon="+plong+",Lat="+plat+",Eta = "+znu(kl-1)+",Val="+omb+varunit+",Err="+err+varunit
;********************************************************
;
  fg1 = first_guess->U(0,kl-1,:,:)              ; import data + meta info
  an1 = analysis->U(0,kl-1,:,:)             ; import data + meta info    
  df1 = an1
  df1 = an1 - fg1
  print ("min U  = "+min(df1))
  print ("max U  = "+max(df1))

  fg2 = first_guess->V(0,kl-1,:,:)              ; import data + meta info
  an2  = analysis->V(0,kl-1,:,:)             ; import data + meta info   
  df2 = an2
  df2 = an2 - fg2
  print ("min V  = "+min(df2))
  print ("max V  = "+max(df2))

  fg3 = first_guess->T(0,kl-1,:,:)              ; import data + meta info
  an3  = analysis->T(0,kl-1,:,:)             ; import data + meta info
  df3 = an3
  df3 =  an3 - fg3
  print ("min T  = "+min(df3))
  print ("max T  = "+max(df3))
 
  fg4 = first_guess->QVAPOR(0,kl-1,:,:)              ; import data + meta info
  an4  = analysis->QVAPOR(0,kl-1,:,:)             ; import data + meta info
  df4 = an4
  df4 = an4 - fg4
  df4 = 1000.0 * df4
  df4@units = "g kg-1"
  print ("min QV = "+min(df4))
  print ("max QV = "+max(df4))

; Use array syntax to average adjacent components to mass grid points
; .  create u and v on a common grid for visualization
; http://www.ncl.ucar.edu/Applications/Scripts/WRF_lc_7.ncl
  dimdf1 = dimsizes(df1)
  mlondf1 = dimdf1(1)

  dimdf2 = dimsizes(df2)
  nlatdf2 = dimdf2(0)

  df1g  = 0.5*(df1(:,0:mlondf1-2)+df1(:,1:mlondf1-1))  
  df2g  = 0.5*(df2(0:nlatdf2-2,:)+df2(1:nlatdf2-1,:))

  df1g@units = fg1@units
  df2g@units = fg2@units

;********************************************************
; create plots  
;********************************************************

  plts                      = new (4,"graphic")

  wks = gsn_open_wks(works,"xy_"+expt)          ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks,"gui_default")           ; select color map

; resource for U and V (vector and scalar overlaid)
  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@cnLineLabelsOn        = False           ; turn off contour line labels
  res@gsnSpreadColors       = True            ; use entire color map
  res@gsnScalarContour      = True            ; contours desired

  WRF_map_c(first_guess,res,0)                ; set map resources

  res@gsnDraw               = False           ; (a) do not draw
  res@gsnFrame              = False           ; (b) do not advance 'frame'

  res@mpGridAndLimbOn       = True            ; trun on lat/lon lines
  res@pmTickMarkDisplayMode = "Always"        ; turn on tickmarks
  res@tmXTOn = False                          ; turn off top   labels 
  res@tmYROn = False                          ; turn off right labels
  res@lbLabelStride = 2                       ; Color bar value writing


  res@vcGlyphStyle       = "CurlyVector"      ; curly vectors
  res@vcRefMagnitudeF    = 0.2                ; define vector ref mag
  res@vcRefLengthF       = 0.045              ; define length of vec ref
  res@gsnRightString     = " "                ; turn off right string
  res@gsnLeftString      = " "                ; turn off left string
  res@tiXAxisString      = " "                ; turn off axis label 
  res@vcRefAnnoOrthogonalPosF = -.535         ; move ref vector into plot
  res@vcMinDistanceF        = 0.02            ; larger means sparser
; resource for T and QV (contour)
  res1                       = True           ; plot mods desired
  res1@gsnMaximize           = True           ; uncomment to maximize size

  res1@cnFillOn              = True           ; turn on color
  res1@cnLineLabelsOn        = False          ; turn off contour line labels
  res1@gsnSpreadColors       = True           ; use entire color map

  WRF_map_c(first_guess,res1,0)                ; set map resources

  res1@gsnDraw               = False          ; (a) do not draw
  res1@gsnFrame              = False          ; (b) do not advance 'frame'

  res1@mpGridAndLimbOn       = True           ; trun on lat/lon lines
  res1@pmTickMarkDisplayMode = "Always"       ; turn on tickmarks
  res1@tmXTOn = False                         ; turn off top   labels
  res1@tmYROn = False                         ; turn off right labels
  res1@lbLabelStride = 2                      ; Color bar value writing


  res@gsnLeftString         = "U"
  res@gsnRightString        = df1g@units
  plts(0)                   = gsn_csm_vector_scalar_map(wks,df1g,df2g,df1g,res) ;vector overlaid

  res@gsnLeftString         = "V"
  res@gsnRightString        = df2g@units
  plts(1)                   = gsn_csm_vector_scalar_map(wks,df1g,df2g,df2g,res) ;vector overlaid

  res1@gsnLeftString         = "Theta"
  plts(2)                   = gsn_csm_contour_map(wks,df3,res1)
;  plts(2)                   = gsn_csm_vector_scalar_map(wks,df1g,df2g,df3,res1) ;no vector overlaid

  res1@gsnLeftString         = "QV"
  plts(3)                   = gsn_csm_contour_map(wks,df4,res1)
;  plts(3)                   = gsn_csm_vector_scalar_map(wks,df1g,df2g,df4,res1) ;no vector overlaid


;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@gsnFrame         = False                  ; don't advance panel plot
  resP@txString         = title
  resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

  txres               = True
  txres@txFontHeightF = 0.012
  gsn_text_ndc(wks,subtitle,0.5,0.94,txres)
  frame(wks)


end
