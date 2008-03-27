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
;         'analfile="/ptmp/huishao/data/caa1/psot/fc/psot5/2006120100/analysis"' psot_xz_auto.ncl
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

begin
;********************************************************
; open file 
;********************************************************
  first_guess     = addfile(bakfile+".nc", "r")
  analysis        = addfile(analfile+".nc", "r")

  znu  = first_guess->ZNU(0,:)
  xlong    = first_guess->XLONG(0,xlat-1,:)
  xlong@units = "degrees_east"
  xlong_u    = first_guess->XLONG_U(0,xlat-1,:)
  xlong_u@units = "degrees_east"
  xlong_v    = first_guess->XLONG_V(0,xlat-1,:)
  xlong_v@units = "degrees_east"

  plong = first_guess->XLONG(0,xlat-1,xlon-1)
  plat = first_guess->XLAT(0,xlat-1,xlon-1)

  title="PSOT"+" - "+var
  subtitle="Lon="+plong+",Lat="+plat+",Eta = "+znu(kl-1)+",Val="+omb+varunit+",Err="+err+varunit

;********************************************************
; Read W at Time=6, south_north=0, convert to cm/sec
;********************************************************
  fg1 = first_guess->U(0,:,xlat-1,:)              ; import data + meta info
  an1 = analysis->U(0,:,xlat-1,:)             ; import data + meta info    
  df1 = an1
  df1 = an1 - fg1
  df1!0 = "lev"
  df1&lev = znu
  df1!1 = "lon"
  df1&lon = xlong_u
  print ("min U  = "+min(df1))
  print ("max U  = "+max(df1))

  fg2 = first_guess->V(0,:,xlat-1,:)              ; import data + meta info
  an2  = analysis->V(0,:,xlat-1,:)             ; import data + meta info   
  df2 = an2
  df2 = an2 - fg2
  df2!0 = "lev"
  df2&lev = znu
  df2!1 = "lon"
  df2&lon = xlong_v
  print ("min V  = "+min(df2))
  print ("max V  = "+max(df2))

  fg3 = first_guess->T(0,:,xlat-1,:)              ; import data + meta info
  an3  = analysis->T(0,:,xlat-1,:)             ; import data + meta info
  df3 = an3
  df3 =  an3 - fg3
  df3!0 = "lev"
  df3&lev = znu
  df3!1 = "lon"
  df3&lon = xlong
  print ("min T  = "+min(df3))
  print ("max T  = "+max(df3)) 
 
  fg4 = first_guess->QVAPOR(0,:,xlat-1,:)              ; import data + meta info
  an4  = analysis->QVAPOR(0,:,xlat-1,:)             ; import data + meta info
  df4 = an4
  df4 = an4 - fg4
  df4 = 1000.0 * df4
  df4@units = "g kg-1"
  df4!0 = "lev"
  df4&lev = znu
  df4!1 = "lon"
  df4&lon = xlong
  print ("min QV = "+min(df4))
  print ("max QV = "+max(df4))

;********************************************************
; create plots  
;********************************************************

  plts                      = new (4,"graphic")

  wks = gsn_open_wks(works,"xz_"+expt)          ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks,"gui_default")           ; select color map

  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@gsnSpreadColors       = True            ; use entire color map
  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
  res@trYReverse            = True            ; reverse y axis
  res@tmXBTickSpacingF      = 15.              ; force labeling every 5 deg lon

  res@gsnDraw               = False            ; (a) do not draw
  res@gsnFrame              = False            ; (b) do not advance 'frame'


  res@gsnLeftString         = "U"
  plts(0)                   = gsn_csm_contour(wks,df1,res)

  res@gsnLeftString         = "V"
  plts(1)                   = gsn_csm_contour(wks,df2,res)

  res@gsnLeftString         = "Theta'"
  plts(2)                   = gsn_csm_contour(wks,df3,res)

  res@gsnLeftString         = "QV"
  plts(3)                   = gsn_csm_contour(wks,df4,res)

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
