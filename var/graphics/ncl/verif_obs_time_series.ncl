;===========================================================================================;
;  verif_anal_time_series.ncl
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
   ;===========================================================================================;
   ; open file and read in data
   ;-----------------------------------------------------------
   run_dir=getenv("PWD")+"/"
   plot_dir = run_dir
   read_headers = asciiread(run_dir+"header_main",-1,"string")
   ;-----------------------------------------------------------
   Title = read_headers(0)
   diag_var1 = read_headers(1)
   diag_var2 = read_headers(2)
   num_expt = stringtointeger(read_headers(3))
   pdat_dirs = (/read_headers(4 : 3+num_expt)/)
   if (get_omboma_plots .eq. "True" .or. \
       get_omboma_plots .eq. "true" .or. \
       get_omboma_plots .eq. ".True." .or. \
       get_omboma_plots .eq. ".true."      ) then
   omboma=1    
   omboma_string="omboma_"
   nexp=2*num_expt
   legend_line_cols=new((/2*num_expt/),"string")
   legend_line_cols_N=new((/num_expt/),"string")
   new_legends=new((/2*num_expt/),"string")
   new_legends_N=new((/num_expt/),"string")
   do i=1,num_expt
   legend_line_cols(2*i-2)=exp_line_cols(i-1)
   legend_line_cols(2*i-1)= legend_line_cols(2*i-2)
   legend_line_cols_N(i-1)=exp_line_cols(i-1)
   new_legends(2*i-2)=exp_legends(i-1)+" omb"
   new_legends(2*i-1)= exp_legends(i-1)+" oma"
   new_legends_N(i-1)= exp_legends(i-1)
   end do
   else
   omboma=0    
   omboma_string=""
   nexp=num_expt
   legend_line_cols=new((/num_expt/),"string")
   legend_line_cols_N=new((/num_expt/),"string")
   new_legends=new((/num_expt/),"string")
   new_legends_N=new((/num_expt/),"string")
   do i=1,num_expt
   legend_line_cols(i-1)=exp_line_cols(i-1)
   legend_line_cols_N(i-1)=exp_line_cols(i-1)
   new_legends(i-1)     = exp_legends(i-1)
   new_legends_N(i-1)     = exp_legends(i-1)
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
   ;     Assign all possible file names in header_main          
   ;-----------------------------------------------------------
   sfc_file="fnames_sfc"
   upr_file="fnames_upr"
   gupr_file="fnames_gupr"
   gpsref_file="fnames_gpsref"
   ggpsref_file="fnames_ggpsref"
   ;-----------------------------------------------------------
   do iob = 1, num_obs           ;Skip global profiles 
   ;-----------------------------------------------------------
   if (     fnames_obs(iob-1) .eq. gupr_file    \
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
   anal_var = new((/num_vars/),"string")
   if( num_vars .eq. 5) then
   anal_var =(/"U","V","T","Q","Ps"/)
   else
   anal_var =(/"U","V","T","Q"/)
   end if

   do iv = 1, num_vars
   varfiles(0,iv-1)= varnames(3*(iv-1))
   varfiles(1,iv-1)= varnames(3*(iv-1)+1)
   varunits(iv-1)  = varnames(3*(iv-1)+2)
   end do

   ;----------------------------------------------------------
   if (fnames_obs(iob-1) .eq. sfc_file ) then
    nlev =1
    ifsfc = True
    out_name="Time_Series_SFC_"+omboma_string
   else
    ifsfc = False
   end if
   if (fnames_obs(iob-1) .eq. upr_file) then
    nlev = 16
    ifupr = True
    out_name="Time_Series_UPA_"+omboma_string
   else
    ifupr = False
   end if
   ;--------------------------------------------
   q_levels = 9
   nv = num_vars
   data_omb = new ((/nv,num_expt,nrow,ncol/),"float")
   data_oma = new ((/nv,num_expt,nrow,ncol/),"float")

   do iexp = 1, num_expt
   do iv = 1, nv
    data_omb((iv-1),(iexp-1),:,:)=asciiread(pdat_dirs(iexp-1)+varfiles(0,iv-1),(/nrow,ncol/),"float")
    data_oma((iv-1),(iexp-1),:,:)=asciiread(pdat_dirs(iexp-1)+varfiles(1,iv-1),(/nrow,ncol/),"float")
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
   ;   Type :   1     2       3      4        5
   ;          BIAS  ABIAS    RMSE   error   significance
   ;********************************************************************************************
    plevels=(/1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20,10/)
   ;********************************************************************************************
    diag_types=(/"BIAS","ABIAS","RMSE"/)	
    diag_names=(/"Bias","Absolute Bias","RMSE"/) 
   ;----------------------------------------------------------
    dvals = (/1.0,1.0,1.0,1.0,0.10/)
   ;----------------------------------------------------------
   do idtype =1, num_d_scor   ;type loop
   ;----------------------------------------------------------
     do ii = 1, 3
       if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
         Type = ii
       end if
     end do
     diag_name=diag_names(Type-1)
   ;----------------------------------------------------------
   first = True
   do idlev = 1, num_d_levs   ;level loop
   ;----------------------------------------------------------
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
    end if
    if (ifupr) then
      level=ilev
      plevel = plevels(level-1)+" hPa  "
    end if

   ;----------------------------------------------------------
    plts  = new (nv,"graphic")   ; define number of panels
    pltsN = new (nv,"graphic")   ; define number of panels

    x = new((/nrow/),"integer",Fill_ValueI)
    y = new((/nv,nexp,nrow/),"float",Fill_ValueF)
    bar  = new((/nv,nexp,nrow/),"float",Fill_ValueF)
    sigt = new((/nv,nexp,nrow/),"float",Fill_ValueF)
    y_num = new((/nv,num_expt,nrow/),"float",Fill_ValueF)

    x = ispan(1,nrow,1)
   ;----------------------------------------------------------
   do iv = 1, nv
      do iexp = 1, num_expt
          do irow = 1, nrow
           if( omboma .eq. 1) then
             y(iv-1,2*iexp-2,irow-1)    = data_omb(iv-1,iexp-1,irow-1,1+(level-1)*6+Type)
             bar(iv-1,2*iexp-2,irow-1)  = data_omb(iv-1,iexp-1,irow-1,5+(level-1)*6)
             sigt(iv-1,2*iexp-2,irow-1) = data_omb(iv-1,iexp-1,irow-1,6+(level-1)*6)

             y(iv-1,2*iexp-1,irow-1)    = data_oma(iv-1,iexp-1,irow-1,1+(level-1)*6+Type)
             bar(iv-1,2*iexp-1,irow-1)  = data_oma(iv-1,iexp-1,irow-1,5+(level-1)*6)
             sigt(iv-1,2*iexp-1,irow-1) = data_oma(iv-1,iexp-1,irow-1,6+(level-1)*6)

           else
             y(iv-1,iexp-1,irow-1)    = data_omb(iv-1,iexp-1,irow-1,1+(level-1)*6+Type)
             bar(iv-1,iexp-1,irow-1)  = data_omb(iv-1,iexp-1,irow-1,5+(level-1)*6)
             sigt(iv-1,iexp-1,irow-1) = data_omb(iv-1,iexp-1,irow-1,6+(level-1)*6)
           end if
   
             y_num(iv-1,iexp-1,irow-1) = data_omb(iv-1,iexp-1,irow-1,1+(level-1)*6)
          end do
       end do
   end do

    x@_FillValue = Fill_ValueI
    y@_FillValue = Fill_ValueF
    bar@_FillValue = Fill_ValueF
    sigt@_FillValue = Fill_ValueF
    y_num@_FillValue = Fill_ValueF

   ; y = smth9(y,0.50,-0.25,False)
   ;----------------------------------------------------------
    res          = True                              ; plot mods desired
    res@gsnDraw               = False                ; (a) do not draw
    res@gsnFrame              = False                ; (b) do not advance 'frame'
    res@xyLineThicknesses = (/2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0/)    ; make lines thicker
    res@xyLineColors      = legend_line_cols                              ; change line color
    res@xyDashPatterns    = (/0,1,0,1,0,1,0,1,0,1,0,1/)                   ; choose dash patterns
    res@xyMarkers         = (/2,2,2,2,2,2,2,2,2,2,2,2/)                   ; (none, dot, asterisk)
    res@xyMarkerColors   = res@xyLineColors          ; Marker colors
    res@xyMarkerSizeF   = 0.01                       ; Marker size (default is 0.01)
    res@xyLineLabelFontHeightF = 0.02                ; Font size and color
    res@xyLineLabelFontColor   = 2                   ; for line labels
    res@tmXTBorderOn     = True                      ; set up the top border
    res@tmXTOn           = False                     ; set the top tickmark
    res@tmYRBorderOn     = True                      ; set up the Right border
    res@tmYROn           = False                     ; set the Right tickmark
    res@tiXAxisFont     = "Times-roman"              ; Font for X axis label
    res@tiYAxisFont     = "Times-roman"              ; Font for Y axis label
    res@tiXAxisFontThicknessF  = 2.5
    res@tiYAxisFontThicknessF  = 2.5
    res@tiXAxisFontHeightF     = 0.03                ; Change the font size.
    res@tiYAxisFontHeightF     = 0.03
    res@tiXAxisFontAspectF     = 1.5
    res@tiYAxisFontAspectF     = 1.5
    res@tiXAxisString   = Title                      ; Label for the X axis
   ;----------------------------------------------------------
   ; Set resources for Obs number ploting
   ;----------------------------------------------------------
    resN          = True                              ; plot mods desired
    resN@gsnDraw               = False                ; (a) do not draw
    resN@gsnFrame              = False                ; (b) do not advance 'frame'
    resN@xyLineThicknesses = (/2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0/)    ; make lines thicker
    resN@xyLineColors      = legend_line_cols_N                            ; change line color
    resN@xyDashPatterns    = (/1,1,1,1,1,1,1,1,1,1,1,1/)                   ; choose dash patterns
    resN@xyMarkers         = (/2,2,2,2,2,2,2,2,2,2,2,2/)                   ; (none, dot, asterisk)
    resN@xyMarkerColors   = resN@xyLineColors         ; Marker colors
    resN@xyMarkerSizeF   = 0.01                       ; Marker size (default is 0.01)
    resN@xyLineLabelFontHeightF = 0.02                ; Font size and color
    resN@xyLineLabelFontColor   = 2                   ; for line labels
    resN@tmXTBorderOn     = True                      ; set up the top border
    resN@tmXTOn           = False                     ; set the top tickmark
    resN@tmYLBorderOn     = True                      ; set up the Right border
    resN@tmYROn           = True                      ; set the Right tickmark
    resN@tiXAxisFont     = "Times-roman"              ; Font for X axis label
    resN@tiYAxisFont     = "Times-roman"              ; Font for Y axis label
    resN@tiXAxisFontThicknessF  = 2.5
    resN@tiYAxisFontThicknessF  = 2.5
    resN@tiXAxisFontHeightF     = 0.03                ; Change the font size.
    resN@tiYAxisFontHeightF     = 0.03
    resN@tiXAxisFontAspectF     = 1.5
    resN@tiYAxisFontAspectF     = 1.5
    resN@tiXAxisString   = Title                      ; Label for the X axis
   ;--------------------------------------------------------------------
    res@trXMaxF = nrow
    resN@trXMaxF = nrow
    res@trXMinF = 1
    resN@trXMinF = 1
   if (level .le. q_levels ) then
   do iv = 1, nv
   
    res@tiYAxisString  = varunits(iv-1)              ; Label for the Y axis
    resN@tiYAxisString = anal_var(iv-1)+ "- Numbers"  ; Label for the Y axis

    plts(iv-1)  = gsn_csm_xy(wks,x,y(iv-1,:,:),res)
    if ( idtype .eq. 1)then
    pltsN(iv-1) = gsn_csm_xy(wks,x,y_num(iv-1,:,:),resN)
    end if

   end do
   else
   do iv = 1, nv-1
   
    res@tiYAxisString   = varunits(iv-1)             ; Label for the Y axis
    resN@tiYAxisString   = "Numbers"                 ; Label for the Y axis

    plts(iv-1)  = gsn_csm_xy(wks,x,y(iv-1,:,:),res)
    if ( idtype .eq. 1)then
    pltsN(iv-1) = gsn_csm_xy(wks,x,y_num(iv-1,:,:),resN)
    end if
   
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
   resP@lbLabelFontHeightF = 0.008		  ; set font height of Label Bar labels
   resP@gsnPanelBottom   = 0.25			  ; shrink panel plot by setting bottom edge of plot
   resP@gsnPanelTop      = 1.00  		  ; shrink panel plot by setting top edge of plot
   resP@gsnPaperOrientation = "portrait"
 
   resP@txString   = plevel+"  "+diag_name

   gsn_panel(wks,plts,(/nv-2,2/),resP)                ; now draw as one plot
   ;-------------------------------------------------------------------------------------
   legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.15
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
   ;for having num_obs    "lgItemCount"              : (nexp +1)                 ; how many
    "lgItemCount"              : (nexp )                 ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 2.0
    "lgDashIndexes"            : (/0,1,0,1,0,1,0,1,0,1,0,1/)             ; line paters
    "lgLineLabelStrings"       : new_legends
    "lgLineColors"             : legend_line_cols
    "lgMonoLineLabelFontColor" : True                    ; one label color
   end create
   draw(legend)
   if ( idtype .eq. 1)then
   frame(wks)
   ;-------------------------------------------------------------------------------------
   ; create panel numobs : panel plots have their own set of resources
   ;-------------------------------------------------------------------------------------
   resPN                = True                    ; modify the panel plot
   resPN@gsnMaximize      = True                    ; maximize panel area
   resPN@gsnFrame         = False                   ; don't advance the frame
   resPN@gsnPanelXWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
   resPN@gsnPanelYWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
   resPN@gsnPanelRowSpec  = True                    ; specify 1 top, 2 lower level
   resPN@lbLabelFontHeightF = 0.008		  ; set font height of Label Bar labels
   resPN@gsnPanelBottom   = 0.25			  ; shrink panel plot by setting bottom edge of plot
   resPN@gsnPanelTop      = 1.00  		  ; shrink panel plot by setting top edge of plot
   resPN@gsnPaperOrientation = "portrait"
 
   ;  resPN@txString   = "Observation Numbers"

    gsn_panel(wks,pltsN,(/nv-2,2/),resPN)                ; now draw as one plot
   ;-------------------------------------------------------------------------------------
   legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.15
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
    "lgItemCount"              : (num_expt )             ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 2.0
    "lgDashIndexes"            : (/0,0,0,0,0,0,0,0,0,0,0,0/)             ; line paters
    "lgLineLabelStrings"       : new_legends_N
    "lgLineColors"             : legend_line_cols_N
    "lgMonoLineLabelFontColor" : True                    ; one label color
   end create
   draw(legend)
   end if
   ;-------------------------------------------------------------------------------------

    frame(wks)
    if (ifsfc) then
      break
    end if
   end do                            ; level loop over
     delete(wks)
     delete(x)
     delete(y)
     delete(y_num)
     delete(bar)
     delete(sigt)
     delete(plts)
     delete(pltsN)
   end do                            ; Type loop over

   delete(varheads)
   delete(varnames)
   delete(varfiles)
   delete(varunits)
   delete(anal_var)  
   delete(data_omb)
   delete(data_oma)

   end do  ; loop over num_obs

   end
