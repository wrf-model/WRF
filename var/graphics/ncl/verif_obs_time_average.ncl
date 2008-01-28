;===========================================================================================;
;  verif_anal_time_average.ncl
; Author: Syed RH Rizvi  MMM/NCAR                 
; Date  : 10/12/2007
;=========================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;===========================================================================================;
begin
   Fill_ValueF = -99.99
   Fill_ValueI = -99
   ;-----Assign all possible file names in header_main -------
   sfc_file="fnames_sfc"
   upr_file="fnames_upr"
   gupr_file="fnames_gupr"
   gpsref_file="fnames_gpsref"
   ggpsref_file="fnames_ggpsref"
   ;-----------------------------------------------------------
   ;===========================================================================================;
   ; open file and read in data
   ;-----------------------------------------------------------
   run_dir=getenv("PWD")+"/"
   plot_dir = run_dir
   read_headers = asciiread(run_dir+"header_main",-1,"string")
   ;-----------------------------------------------------------
   Xaxis_title = read_headers(0)
   diag_var1 = read_headers(1)
   diag_var2 = read_headers(2)
   num_expt = stringtointeger(read_headers(3))
   pdat_dirs = (/read_headers(4 : 3+num_expt(0))/)
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
   ;-----------------------------------------------------------
   num_d_levs=dimsizes(select_levs)
   num_d_scor=dimsizes(select_scores)
   ;-----------------------------------------------------------
   num_obs = dimsizes(read_headers) - (num_expt + 4)
   fnames_obs = new((/num_obs/),"string")
   do iob = 1, num_obs
   fnames_obs(iob-1)= read_headers(num_expt+4+iob-1)
   end do

   ;-----------------------------------------------------------
   do iob = 1, num_obs
   ;------------------
   if (     fnames_obs(iob-1) .eq. upr_file    \
       .or. fnames_obs(iob-1) .eq. gpsref_file  \
       .or. fnames_obs(iob-1) .eq. ggpsref_file) then
   continue
   end if
   ;-----------------------------------------------------------
   varheads = asciiread(run_dir+fnames_obs(iob-1),-1,"string")
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

   ;----------------------------------------------------------
   sfc_file="fnames_sfc"
   if ( fnames_obs(iob-1) .eq. sfc_file ) then
   nlev =1
   ifsfc = True
   out_name="Time_Average_SFC_"+omboma_string
   else
   nlev = 16
   ifsfc = False
   out_name="Time_Average_UPA_"+omboma_string
   end if
   ;--------------------------------------------
   q_levels = 9
   nv = num_vars
   data_omb = new ((/nv,num_expt,nrow,ncol/),"float",Fill_ValueF)
   data_oma = new ((/nv,num_expt,nrow,ncol/),"float",Fill_ValueF)

   do iexp = 1, num_expt
      do iv = 1, nv
        data_omb((iv-1),(iexp-1),:,:)=\
        asciiread(pdat_dirs(iexp-1)+varfiles(0,iv-1),(/nrow,ncol/),"float")
        data_oma((iv-1),(iexp-1),:,:)=\
        asciiread(pdat_dirs(iexp-1)+varfiles(1,iv-1),(/nrow,ncol/),"float")
      end do
   end do
   ;********************************************************************************************
   ; Data Format:  In each row total (1 + 16 x 6 = 97) values for Upper-air
   ; Data Format:  In each row total (1 + 1 x 6 = 7) values for Surface
   ;
   ; Date,  For each level triplets for NUM, BIAS Absolute BIAS & RMSE 
   ; levels:  1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10
   ; index      1   2   3   4   5   6   7   8   9  10  11  12 13 14 15 16
   ;
   ;   Type :   1     2       3     4     5
   ;          BIAS  ABIAS    RMSE  error significance
   ;********************************************************************************************
    plevels=(/1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20,10/)
   ;********************************************************************************************
   diag_types=(/"BIAS","ABIAS","RMSE"/)	
   diag_names=(/"Bias","Absolute Bias","RMSE"/) 
   ;----------------------------------------------------------
   ;----------------------------------------------------------
   do idtype =1, num_d_scor  
     do ii = 1, 3
       if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
         Type = ii
       end if
     end do
     diag_name=diag_names(Type-1)
   ;----------------------------------------------------------
   first = True
   do idlev = 1, num_d_levs 
      do ll = 1, nlev
        if ( select_levs(idlev-1) .eq. plevels(ll-1) ) then
           ilev = ll
        end if
   end do
   ;----------------------------------------------------------
   if (first) then
     wks   = gsn_open_wks (wksdev,plot_dir+out_name+select_scores(idtype-1))   ; open workstation
     first = False
   end if
   ;----------------------------------------------------------
   if (ifsfc) then
      level = 1
      plevel = "Surface  "
   else
     level=ilev
     plevel = plevels(level-1)+" hPa  "
   end if

   ;----------------------------------------------------------
   plts                      = new (nv,"graphic")   ; define number of panels


 x = new((/nexp/),"integer",Fill_ValueI)
 y_bar = new((/nv,nexp/),"float",Fill_ValueF)

 x = ispan(1,nexp,1)
;----------------------------------------------------------
do iv = 1, nv
  do iexp = 1, num_expt
   if( omboma .eq. 1) then
   y_bar(iv-1,2*iexp-2) = avg(data_omb((iv-1),(iexp-1),:,(1+(level-1)*6+Type)))
   y_bar(iv-1,2*iexp-1) = avg(data_oma((iv-1),(iexp-1),:,(1+(level-1)*6+Type)))
   else
   y_bar(iv-1,iexp-1) = avg(data_omb((iv-1),(iexp-1),:,(1+(level-1)*6+Type)))
   end if
 end do
end do

   x@_FillValue = Fill_ValueI
   y_bar@_FillValue = Fill_ValueF

;----------------------------------------------------------
 res                       = True                   ; plot mods desired
 res@gsnDraw               = False                  ; (a) do not draw
 res@gsnFrame              = False                  ; (b) do not advance 'frame'
 res@gsnXYBarChart         = True
 res@gsnXYBarChartBarWidth = 0.5                 ; change bar widths
 res@gsnYRefLine           = 0.0
 res@gsnXYBarChartColors2  = legend_line_cols
;   res@gsnXYBarChartOutlineOnly = True
 res@tmXBMode         = "Explicit"
 res@tmXBValues       =  x
 res@tmXBLabels       = new_legends
 res@tmXBLabelAngleF  = bar_label_angle
 res@tmXTBorderOn     = True                        ; set up the top border
 res@tmXTOn           = False                       ; set the top tickmark
 res@tmYRBorderOn     = True                        ; set up the Right border
 res@tmYROn           = False                       ; set the Right tickmark
 res@tiXAxisFont     = "Times-roman"                ; Font for X axis label
 res@tiYAxisFont     = "Times-roman"                ; Font for Y axis label
 res@tiXAxisFontThicknessF  = 2.5
 res@tiYAxisFontThicknessF  = 2.5
 res@tiXAxisFontHeightF     = 0.03                  ; Change the font size.
 res@tiYAxisFontHeightF     = 0.03
 res@tiXAxisFontAspectF     = 1.5
 res@tiYAxisFontAspectF     = 1.5
;res@tiXAxisString   = Xaxis_title                  ; Label for the X axis
;--------------------------------------------------------------------
if (level .le. q_levels ) then
do iv = 1, nv

 res@tiYAxisString   = varunits(iv-1)                ; Label for the Y axis
 plts(iv-1) = gsn_csm_xy(wks,x,y_bar(iv-1,:),res)

end do
else
do iv = 1, nv-1

 res@tiYAxisString   = varunits(iv-1)             ; Label for the Y axis
 plts(iv-1) = gsn_csm_xy(wks,x,y_bar(iv-1,:),res)
  
end do
 
end if
;--------------------------------------------------------------------------------------
; create panel: panel plots have their own set of resources
;--------------------------------------------------------------------------------------
  resP                  = True                    ; modify the panel plot
  resP@gsnMaximize      = True                    ; maximize panel area
  resP@gsnFrame         = False                   ; don't advance the frame
  resP@gsnPanelXWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
  resP@gsnPanelYWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
  resP@gsnPanelRowSpec  = True                    ; specify 1 top, 2 lower level
  resP@gsnPanelBottom   = 0.25			  ; shrink panel plot by setting bottom edge of plot
  resP@gsnPanelTop      = 1.00  		  ; shrink panel plot by setting top edge of plot
  resP@gsnPaperOrientation = "portrait"

  resP@txString   = plevel+"  "+diag_name

 gsn_panel(wks,plts,(/nv-2,2/),resP)                ; now draw as one plot
;-------------------------------------------------------------------------------------

;  LabelBar = create "LabelBar" labelBarClass wks
;    "vpXF"                     : 0.25                    ; position on virtual page
;    "vpYF"                     : 0.19
;    "vpWidthF"                 : 0.5                     ; width
;    "vpHeightF"                : 0.1                     ; height
;    "lbBoxMajorExtentF"        : 0.36                    ; puts space between color boxes
;    "lbBoxCount"               : num_expt
;    "lbFillColors"             : legend_line_cols 
;    "lbMonoFillPattern"        : True
;    "lbLabelFontHeightF"       : 0.03
;    "lbLabelJust"              : "CenterLeft"
;    "lbLabelStrings"           : new_legends
;    "lbPerimOn"                : False                   ; no perimeter
;    "lbPerimColor"             : "white" 
;  end create
;  draw(LabelBar)
;-------------------------------------------------------------------------------------

frame(wks)
 if (ifsfc) then
   break
 end if

end do                            ; level loop over
  delete(wks)
  delete(x)
  delete(y_bar)
end do                            ; Type loop over

delete(varheads)
delete(varnames)
delete(varfiles)
delete(varunits)
delete(data_omb)
delete(data_oma)
delete(plts)

end do

end
