;============================================================;
; Purpose:  Plots vertical profiles 
;============================================================;
; Author: Ananda Kumar Das (Visiting Scientist)
;         National Center for Atmospheric Research
;         Boulder, CO 80301
; Date   : 11/27/2006
;
; Updates:
;          08/23/2007  Hui Shao & Syed RH Rizvi, MMM/NCAR
;          Added significance test  &
;          justify the plot vitual position
;
;          08/30/2007    Syed RH Rizvi,  MMM/NCAR
;          Added parallel option for ploting both OMB & OMA
;
;          
;=============================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;=============================================================;
begin
 Fill_ValueF = -99.99
 Fill_ValueI = -99
;----------------------------------------------------------------------------------------
run_dir=getenv("PWD")+"/"
plot_dir = run_dir
read_headers = asciiread(run_dir+"header_main",-1,"string")
;----------------------------------------------------------------------------------------
Title = read_headers(0)
diag_var1 = read_headers(1)
diag_var2 = read_headers(2)
num_expt = stringtointeger(read_headers(3))
pdat_dirs = (/read_headers(4 : 3+num_expt)/)
if (get_omboma_plots .eq. "True" .or. \
    get_omboma_plots .eq. "true" .or. \
    get_omboma_plots .eq. ".True." .or. \
    get_omboma_plots .eq. ".true."      ) then
omboma_string="omboma_"
omboma=1
nexp=2*num_expt
legend_line_cols=new((/2*num_expt/),"string")
new_legends=new((/2*num_expt/),"string")
do i=1,num_expt
legend_line_cols(2*i-2)=exp_line_cols(i-1)
legend_line_cols(2*i-1)= legend_line_cols(2*i-2)
new_legends(2*i-2)=exp_legends(i-1)+" omb"
new_legends(2*i-1)= exp_legends(i-1)+" oma"
end do
else
omboma_string=""
omboma=0
nexp=num_expt
legend_line_cols=new((/num_expt/),"string")
new_legends=new((/num_expt/),"string")
do i=1,num_expt
legend_line_cols(i-1)=exp_line_cols(i-1)
new_legends(i-1)     = exp_legends(i-1)
end do
end if

;----------------------------------------------------------------------------------------
num_d_levs=dimsizes(select_levs)
num_d_scor=dimsizes(select_scores)
num_obs = dimsizes(read_headers) - (num_expt + 4)
fnames_obs = new((/num_obs/),"string")
do iob = 1, num_obs
fnames_obs(iob-1)= read_headers(num_expt+4+iob-1)
end do
;----------------------------------------------------------------------------------------
 upper_air_file="fnames_gupr"
 do iob = 1, num_obs
  if ( fnames_obs(iob-1) .eq. upper_air_file ) then
      file_names =  fnames_obs(iob-1)
      nlev = 16
  end if 
 end do
;----------------------------------------------------------------------------------------
 varheads = asciiread(run_dir+file_names,-1,"string")
 nrow = stringtointeger(varheads(0))
 ncol = stringtointeger(varheads(1))
 varnames = (/varheads(2 : :)/)

 num_vars = dimsizes(varnames)/3
 varfiles = new((/2,num_vars/),"string")
 varunits = new((/num_vars/),"string")

do iv = 1, num_vars
  varfiles(0,iv-1)= varnames(3*(iv-1))
  varfiles(1,iv-1)= varnames(3*(iv-1)+1)
  varunits(iv-1)  = varnames(3*(iv-1)+2)
end do

;----------------------------------------------------------------------------------------
 out_name = "Profile_"+omboma_string
 q_level = 9
 nv = num_vars
;
 data_omb = new ((/nv,num_expt,ncol/),"float",Fill_ValueF)
 data_oma = new ((/nv,num_expt,ncol/),"float",Fill_ValueF)

 do iexp = 1, num_expt
 do iv = 1, nv
    data_omb(iv-1,iexp-1,:)=asciiread(pdat_dirs(iexp-1)+varfiles(0,iv-1),(/ncol/),"float")
    data_oma(iv-1,iexp-1,:)=asciiread(pdat_dirs(iexp-1)+varfiles(1,iv-1),(/ncol/),"float")
 end do
 end do

;**********************************************************************************************
; Data Format:  In each row total (1 + 16 x 6 = 97) values for Upper-air
; Data Format:  In each row total (1 + 1 x 6 = 7) values for Surface
;
; Date,  For each level triplets for NUM, BIAS Absolute BIAS & RMSE
; levels:  1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10
; index      1   2   3   4   5   6   7   8   9  10  11  12 13 14 15 16
;
;   Type :   1     2       3
;          BIAS  ABIAS    RMSE
;*********************************************************************************************
 plevels=(/1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20,10/)
;*********************************************************************************************
 diag_types=(/"BIAS","ABIAS","RMSE"/)
 diag_names=(/"Bias","Absolute Bias","RMSE"/)
;----------------------------------------------------------------------------------------
do idtype =1, num_d_scor   ;loop for score
;----------------------------------------------------------------------------------------

  do ii = 1, 3
    if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
      Type = ii
    end if
  end do
  diag_name=diag_names(Type-1)
;----------------------------------------------------------------------------------------
 x = new((/nv,nexp,nlev/),"float",Fill_ValueF)
 bar = new((/nv,nexp,nlev/),"float",Fill_ValueF)
 sigt = new((/nv,nexp,nlev/),"float",Fill_ValueF)

  do iv = 1, nv
  do iexp = 1, num_expt
    do ilev = 1 , nlev
     if( omboma .eq. 1) then
      x(iv-1,2*iexp-2,ilev-1)    = data_omb(iv-1,iexp-1,1+(ilev-1)*6+Type)
      bar(iv-1,2*iexp-2,ilev-1)  = data_omb(iv-1,iexp-1,5+(ilev-1)*6)
      sigt(iv-1,2*iexp-2,ilev-1) = data_omb(iv-1,iexp-1,6+(ilev-1)*6)

      x(iv-1,2*iexp-1,ilev-1)    = data_oma(iv-1,iexp-1,1+(ilev-1)*6+Type)
      bar(iv-1,2*iexp-1,ilev-1)  = data_oma(iv-1,iexp-1,5+(ilev-1)*6)
      sigt(iv-1,2*iexp-1,ilev-1) = data_oma(iv-1,iexp-1,6+(ilev-1)*6)
     else
      x((iv-1),(iexp-1),(ilev-1)) = data_omb((iv-1),(iexp-1),(1+(ilev-1)*6+Type))
      bar((iv-1),(iexp-1),(ilev-1)) = data_omb((iv-1),(iexp-1),5+(ilev-1)*6)
      sigt((iv-1),(iexp-1),(ilev-1)) = data_omb((iv-1),(iexp-1),6+(ilev-1)*6)
     end if
    end do
   end do
   end do

;----------------------------------------------------------------------------------------
 wks   = gsn_open_wks (wksdev,plot_dir+out_name+diag_types(Type-1))     ; open workstation
;----------------------------------------------------------------------------------------
 
 plts  = new (nv,"graphic")
 y = new((/nlev/),"integer",Fill_ValueI)
 y = ispan(0,nlev-1,1)

 centers=new((/nv,nexp,nlev/),graphic)
 error_bar = new((/nv,nexp,nlev/),graphic)
;----------------------------------------------------------------------------------------
do iv =1, nv   ;loop for variable
;----------------------------------------------------------------------------------------
 res          = True                              ; plot mods desired
 res@xyLineThicknesses = (/2.0,2.0,2.0,2.0,4.0,4.0,4.0,4.0/)            ; make 2nd lines thicker
 res@xyLineColors      = legend_line_cols                               ; change line color
 res@xyDashPatterns    = (/0,1,0,1,0,1,0,1,0,1/)                        ; choose dash patterns
 res@xyMarkers         = (/2,2,2,2,4,4,5,5/)                            ; (none, dot, asterisk)
 res@xyMarkerColors    = res@xyLineColors             ; Marker colors
 res@xyMarkerSizeF          = 0.01                    ; Marker size (default is 0.01)
 res@xyLineLabelFontHeightF = 0.02                    ; Font size and color
 res@xyLineLabelFontColor   = 2                       ; for line labels

 res@tmXTBorderOn           = True                    ; set up the top border
 res@tmXTOn                 = False                   ; set the top tickmark

;res@tmYRBorderOn           = False                   ; set up the Right border
;9999 res@tmYRLabelsOn           = True               ; set the Right axis
 res@tmYRLabelsOn           = False                   ; setup right axis label
;9999 res@tmYROn                 = True                    ; set the Right tickmark
 res@tmYROn                 = False                    ; set the Right tickmark
 res@tmYRMode               = "Explicit"
 res@tmYLMode               = "Explicit"
 res@tmLabelAutoStride      = True 
 res@tmYUseLeft             = False


 res@tiXAxisFont            = "Times-roman"           ; Font for X axis label
 res@tiYAxisFont            = "Times-roman"           ; Font for Y axis label
 res@tiXAxisFontThicknessF  = 3.0
 res@tiYAxisFontThicknessF  = 3.0
 res@tiXAxisFontHeightF     = 0.03                    ; Change the font size.
 res@tiYAxisFontHeightF     = 0.03
 res@tiXAxisFontAspectF     = 1.5
 res@tiYAxisFontAspectF     = 1.5
 res@tiYAxisString   = " Pressure (hPa)"              ; Label for the X axis

 res@tmYLValues  = y                              ; Assign y-axis labels
 res@tmYRValues  = y
 res@tmYLLabels  = plevels

 res@tiXAxisString   = varunits(iv-1)             ; Label for the Y axis
  if (Type .ne. 3) then
   res@gsnXRefLine = 0.0                            ; create a reference line
   res@gsnXRefLineDashPattern = 11                  ; create a reference line
  end if

  res@vpXF       = 0.3                        ; this controls the size and location of the plot
  res@vpWidthF  = 0.5
  res@vpYF      = 0.83
  res@vpHeightF = 0.5
 
;-------------------------------------------------------------------------------------

 if ( iv .eq. nv ) then
  res@trYMaxF = q_level
 else
  res@trYMaxF = nlev-1
 end if

 res@gsnDraw               = False                    ; (a) do not draw
 res@gsnFrame              = False                    ; (b) do not advance 'frame'
 plts(iv-1) = gsn_csm_xy(wks,x(iv-1,:,:),y,res)

; add ploymarkers              
  polyres = True               
  polyres@gsMarkerIndex        = 1                ; index for circle
  polyres@gsMarkerSizeF        = .02              ; size
  polyres@gsMarkerThicknessF   = 5                ; thickness

 do iexp=1,nexp
 do ilev=1,nlev
  if (.not. ismissing(x(iv-1,iexp-1,ilev-1))) then
   polyres@gsMarkerColor     = legend_line_cols(iexp-1)                         ; color
   polyres@gsLineColor = legend_line_cols(iexp-1)
   error_bar(iv-1,iexp-1,ilev-1)=gsn_add_polyline(wks,plts(iv-1), \
                               (/x(iv-1,iexp-1,ilev-1)+\
                               bar(iv-1,iexp-1,ilev-1),\
                               x(iv-1,iexp-1,ilev-1)-bar(iv-1,iexp-1,ilev-1)/),\
                               (/y(ilev-1),y(ilev-1)/), polyres)
    if(sigt(iv-1,iexp-1,ilev-1) .gt. 0 ) then
     centers(iv-1,iexp-1,ilev-1)= \
     gsn_add_polymarker(wks,plts(iv-1),x(iv-1,iexp-1,ilev-1),y(ilev-1), polyres)
    end if
  end if
 end do
 end do

;----------------------------------------------------------------------------------------
end do  ;loop for variable (iv)
;----------------------------------------------------------------------------------------
 legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.2
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
    "lgItemCount"              : nexp                    ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 2.0
    "lgDashIndexes"            : (/0,1,0,1,0,1,0,1,0,1,0,1/)             ; line paters
    "lgLineLabelStrings"       : new_legends
    "lgLineColors"             : legend_line_cols
    "lgMonoLineLabelFontColor" : True                    ; one label color
  end create
;--------------------------------------------------------------------
;  panel: panel plots have their own set of resources
;----------------------------------------------------------------------------------------
  resP                  = True                   ; modify the panel plot
  resP@gsnMaximize      = True                    ; maximize panel area
  resP@gsnFrame         = False
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
;  resP@gsnPaperOrientation = "auto"
  resP@gsnPaperOrientation = "portrait"

  resP@gsnPanelXWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
  resP@gsnPanelYWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
  resP@lbLabelFontHeightF = 0.008                ; set font height of Label Bar labels
  resP@gsnPanelBottom   = 0.25                   ; shrink panel plot by setting bottom edge of plot
  resP@gsnPanelTop      = 1.0                    ; shrink panel plot by setting top edge of plot

  resP@txString   = diag_name+" Profiles  "+Title

  gsn_panel(wks,plts,(/nv-2,2/),resP)               ; now draw as one plot
  draw(legend)

 frame(wks)
 delete(wks)
 delete(plts)
 delete(centers)
 delete(error_bar)
 delete(x)
 delete(y)
;----------------------------------------------------------------------------------------
end do     ;loop for score (idtype) 
;----------------------------------------------------------------------------------------
delete(varheads)
delete(varnames)
delete(varfiles)
delete(varunits)
delete(data_omb)
delete(data_oma)
;----------------------------------------------------------------------------------------

end
