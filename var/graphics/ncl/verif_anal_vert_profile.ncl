;===========================================================================================;
;  verif_anal_vert_profile.ncl
; Author: Syed RH Rizvi  MMM/NCAR
;         Hui Shao       NCAR/RAL/DATC:
; Date  : 10/12/2007
;=============================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;=============================================================;
begin
   ;=============================================================;
   run_dir=getenv("PWD")+"/"
   plot_dir = run_dir
   read_headers = asciiread(run_dir+"header_main",-1,"string")
   ;----------------------------------------------------------
    Title = read_headers(0)
    diag_var = read_headers(1)
    num_expt = stringtointeger(read_headers(2))
    pdat_dirs = (/read_headers(3 : 2+num_expt(0))/)
   ;----------------------------------------------------------
   num_legend=num_expt
   legend_line_cols=new((/num_legend/),"string")
   legend_line_cols(0:num_legend-1)=exp_line_cols(0:num_legend-1)

   delete(num_legend)
   num_legend=num_expt
   new_legends=new((/num_legend/),"string")
   new_legends(0:num_legend-1)=exp_legends(0:num_legend-1)
   ;----------------------------------------------------------
   num_d_scor=dimsizes(select_scores)
   ;----------------------------------------------------------
    num_obs = dimsizes(read_headers) - (num_expt + 3)
    fnames_obs = new((/num_obs/),"string")

    upper_air_file="fnames_upr"
    do iob = 1, num_obs
     fnames_obs(iob-1)= read_headers(num_expt+3+iob-1)
     if ( fnames_obs(iob-1) .eq. upper_air_file ) then
      file_names =  fnames_obs(iob-1)
      nlev = 13
     end if 
    end do
   ;----------------------------------------------------------
    varheads = asciiread(run_dir+file_names,-1,"string")
    nrow = stringtointeger(varheads(0))
    ncol = stringtointeger(varheads(1))
    varnames = (/varheads(2 : :)/)

    num_vars = dimsizes(varnames)/2
    varfiles = new((/num_vars/),"string")
    varunits = new((/num_vars/),"string")

    do iv = 1, num_vars
      varfiles(iv-1)= varnames(2*(iv-1))
      varunits(iv-1)= varnames(2*(iv-1)+1)
    end do
   ;-----------------------------------------------------------
    out_name = "Profile_"
    q_level = 8
    nv = num_vars
    nexp = num_expt
    data = new ((/nv,nexp,nrow,ncol/),"float")

    do iexp = 1, nexp
    do iv = 1, nv
    data((iv-1),(iexp-1),:,:)=asciiread(pdat_dirs(iexp-1)+varfiles(iv-1),(/nrow,ncol/),"float")
    end do
    end do
   ;**********************************************************************************************
   ; Data Format:  In each row total (1 + 16 x 4 = 65) values for Upper-air
   ; Data Format:  In each row total (1 + 1 x 4 = 5) values for Surface
   ;
   ; Date,  For each level triplets for NUM, BIAS Absolute BIAS & RMSE
   ; levels:  1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10
   ; index      1   2   3   4   5   6   7   8   9  10  11  12 13 14 15 16
   ;
   ;
   ;   Type :   1     2       3
   ;          BIAS  RMSE     ABIAS
   ;*********************************************************************************************
       plevels=(/1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20/)
   ;*********************************************************************************************
    diag_types=(/"BIAS","RMSE","ABIAS"/)
    diag_names=(/"Bias","RMSE","Absolute Bias"/)
   ;-----------------------------------------------------------
    Fill_Value = -99.99
   
   rdmax = (/9.0,7.0,2.5,2.5/)
   rdmin = (/1.0,1.0,0.0,0.0/)
   ;rdmax = (/7.0,7.0,2.5,2.0/)
   ;rdmin = (/0.0,0.0,0.0,0.0/)

   bdmax = (/3.0,2.0,2.0,1.0/)
   bdmin = (/-7.0,-2.0,-2.0,-1.0/)
   ;bdmax = (/2.0,2.0,2.0,1.0/)
   ;bdmin = (/-3.0,-2.0,-2.0,-1.0/)
   ;-----------------------------------------------------------
   do idtype =1, num_d_scor
     do ii = 1, 3
       if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
         Type = ii
       end if
     end do
     diag_name=diag_names(Type-1)
   ;-----------------------------------------------------------
    xavg = new((/nv,nexp,nlev/),"float")
    ob_tot = new((/nv,nlev/),"float")
    data@_FillValue = Fill_Value
   
   do iv = 1, nv
     do iexp = 1, nexp
       do ilev = 1 , nlev
        if (iv .eq. 4) then
        xavg((iv-1),(iexp-1),(ilev-1)) = 1000.0*avg(data((iv-1),(iexp-1),:,(1+(ilev-1)*4+Type)))
        else
        xavg((iv-1),(iexp-1),(ilev-1)) = avg(data((iv-1),(iexp-1),:,(1+(ilev-1)*4+Type)))
        end if
       end do
     end do
   end do
   data@_FillValue = -99
   do iv = 1, nv
      do ilev = 1 , nlev
        ob_tot((iv-1),(ilev-1)) = sum(data((iv-1),0,:,(1+(ilev-1)*4)))
      end do
   end do
   ob_tot@_FillValue = -999
   ;----------------------------------------------------------------------------------------
    wks   = gsn_open_wks (wksdev,plot_dir+out_name+diag_types(Type-1)+"-hr"+verify_hour)     ; open workstation
   ;----------------------------------------------------------------------------------------
    plts                      = new (nv,"graphic")
   
    x = new((/nv,nexp,nlev/),"float")
    y = new((/nlev/),"integer")
    y = ispan(0,nlev-1,1)

   do iv =1, nv
   
   pnumber = flt2string(ob_tot(iv-1,:))
   ;----------------------------------------------------------------------------------------
    res          = True                              ; plot mods desired
   ;----------------------------------------------------------------------------------------
    res@xyLineThicknesses = (/2.0,2.0,2.0,2.0,4.0,4.0,4.0,4.0/)            ; make 2nd lines thicker
    res@xyLineColors      = legend_line_cols                               ; change line color
    res@xyDashPatterns    = (/0,0,0,0,2,2,3,3,4,4/)                        ; choose dash patterns
    res@xyMarkers         = (/2,2,2,2,4,4,5,5/)                            ; (none, dot, asterisk)
    res@xyMarkerColors    = res@xyLineColors             ; Marker colors
    res@xyMarkerSizeF          = 0.01                    ; Marker size (default is 0.01)
    res@xyLineLabelFontHeightF = 0.02                    ; Font size and color
    res@xyLineLabelFontColor   = 2                       ; for line labels
   
    res@tmXTBorderOn           = True                    ; set up the top border
    res@tmXTOn                 = False                   ; set the top tickmark
   
   ;res@tmYRBorderOn           = False                   ; set up the Right border
   ;res@tmYRLabelsOn           = True    
    res@tmYROn                 = False                   ; set the Right tickmark
   ;res@tmYRMode               = "Explicit"
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

    res@gsnDraw               = False                    ; (a) do not draw
    res@gsnFrame              = False                    ; (b) do not advance 'frame'
   ;--------------------------------------------------------------------
    res@tmYLValues  = y                              ; Assign y-axis labels
   ;res@tmYRValues  = y
   ;res@tmYRLabels  = pnumber
    res@tmYLLabels  = plevels

    res@tiXAxisString   = varunits(iv-1)             ; Label for the Y axis
   ;----------------------------------------------
   ;res@gsnRightString = "Obs Num"
   ;res@gsnRightStringParallelPosF = 1.15
   ;res@gsnRightStringOrthogonalPosF = 0.03
    if ( iv .eq. nv ) then
     res@trYMaxF = q_level
    else
     res@trYMaxF = nlev-1
    end if
   
   ;Hui
   ; if (Type .eq. 2) then
   ;    res@trXMaxF     = bdmax(iv-1)
   ;    res@trXMinF     = bdmin(iv-1)
   ; end if
   ;
   ; if (Type .eq. 1) then
   ;    res@trXMaxF     = rdmax(iv-1)
   ;    res@trXMinF     = rdmin(iv-1)
   ; end if
   
    x(iv-1,:,:) = xavg(iv-1,:,:)
    plts(iv-1) = gsn_csm_xy(wks,x(iv-1,:,:),y,res)
   
    print(x(3,0,3))
    print("exp2")
    print(x(3,1,3))
   ;----------------------------------------------
   end do
   ;--------------------------------------------------------------------
   ;  panel: panel plots have their own set of resources
   ;--------------------------------------------------------------------
     resP                  = True                   ; modify the panel plot
    resP@gsnMaximize      = True                    ; maximize panel area
    resP@gsnFrame         = False
    resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
    resP@gsnPaperOrientation = "auto"
    resP@gsnPanelXWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
    resP@gsnPanelYWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
    resP@lbLabelFontHeightF = 0.008                ; set font height of Label Bar labels
    resP@gsnPanelBottom   = 0.16                   ; shrink panel plot by setting bottom edge of plot
    ; resP@gsnPanelTop      = 1.0                    ; shrink panel plot by setting top edge of plot

    resP@txString   = diag_name+"  "+Title

    gsn_panel(wks,plts,(/nv-2,2/),resP)               ; now draw as one plot
   ;-------------------------------------------------------------------------------------
   legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.15
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
    "lgItemCount"              : nexp                    ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 2.0
    "lgDashIndexes"            : (/0,0,0,0,1/)             ; line paters
    "lgLineLabelStrings"       : new_legends
    "lgLineColors"             : legend_line_cols
    "lgMonoLineLabelFontColor" : True                    ; one label color
    end create

   draw(legend)
   ;-------------------------------------------------------------------------------------
   frame(wks)
   delete(wks)

   end do
   ;-----------------------
   delete(varheads)
   delete(varnames)
   delete(varfiles)
   delete(varunits)
   delete(x)
   delete(ob_tot)
   delete(y)
   delete(data)
   delete(plts)
   ;-----------------------

   end
