;
; Author: Hui-Chuan Lin   MMM/NCAR
;
; Purpose: to read in a series of VARBC.out to plot timeseries of VarBC parameters
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "./advance_cymdh.ncl"

begin

   ;***********************************************************************
   ; user-defined settings
   ;***********************************************************************
   datdir = "/ptmp/hclin/test_varbc/"    ; path before date
   datdir2 = "/wrfvar/"                  ; path after  date
   ; infile = datdir+date(itime)+datdir2+"VARBC.out" will be defined inside the ntime_loop below
   plotdir = "./"
   start_date   = "2007081600"
   end_date     = "2007091300"
   cycle_period = 6
   ; instrument = "metop-2-amsua"
   ; chanindex = (/ 4, 5, 6, 7, 8, 9, 10, 11 /)
   instrument = "eos-2-airs"
   chanindex = (/ 6, 69, 84, 110, 156, 198 /)
   out_type = "pdf"
   ;***********************************************************************
   ; end of user-defined settings
   ;***********************************************************************
   ;
   ; find out how many dates to process
   ;
   ntime = 0
   valid_date = start_date
   do while ( valid_date .le. end_date )
      ntime = ntime + 1
      valid_date = advance_cymdh(valid_date,cycle_period)
   end do
   date = new(ntime,string)
   mmddhh = new(ntime,string)
   date(0) = start_date
   cdate = stringtochar(date(0))
   mmddhh(0) = chartostring(cdate(4:9))
   do i = 1, ntime-1
      date(i) = advance_cymdh(date(i-1),cycle_period)
      cdate = stringtochar(date(i))
      mmddhh(i) = chartostring(cdate(4:9))
   end do
   ; print(date)
   xc = ispan(1,ntime,1)   ; set the x-axis values for statistics time-series plot
   ;
   ; begin the big ntime loop for reading in data
   ;
   do itime = 0, ntime-1   ; this is ntime_loop

      infile = datdir+date(itime)+datdir2+"VARBC.out"
      if ( .not. isfilepresent(infile) ) then
         print("Can not find the file "+infile+"  Will skip it")
      else
         print("Reading "+infile)
         data = asciiread(infile, -1, "string")
         cdata = stringtochar(data)
         nline = dimsizes(cdata(:,0))
         if ( itime .eq. 0 ) then
            do n = 0, nline-1
               if ( cdata(n,0:11) .eq. " Platform_id" ) then
                  indx = n+3      ; line number(starting from 1) of inst info, used in linux sed command
                  platid = stringtointeger(systemfunc("sed -n "+indx+"p "+infile+" | awk '{print $1}'"))
                  satid = stringtointeger(systemfunc("sed -n "+indx+"p "+infile+" | awk '{print $2}'"))
                  instid = stringtointeger(systemfunc("sed -n "+indx+"p "+infile+" | awk '{print $3}'"))
                  if ( platid .eq. 1 ) then
                     platname = "noaa"
                  end if
                  if ( platid .eq. 2 ) then
                     platname = "dmsp"
                  end if
                  if ( platid .eq. 9 ) then
                     platname = "eos"
                  end if
                  if ( platid .eq. 10 ) then
                     platname = "metop"
                  end if
                  if ( instid .eq. 3 ) then
                     instname = "amsua"
                  end if
                  if ( instid .eq. 4 ) then
                     instname = "amsub"
                  end if
                  if ( instid .eq. 15 ) then
                     instname = "mhs"
                  end if
                  if ( instid .eq. 10 ) then
                     instname = "ssmis"
                  end if
                  if ( instid .eq. 11 ) then
                     instname = "airs"
                  end if
                  if ( instrument .eq. platname+"-"+satid+"-"+instname ) then
                     nchanl = stringtointeger(systemfunc("sed -n "+indx+"p "+infile+" | awk '{print $4}'"))
                     npred = stringtointeger(systemfunc("sed -n "+indx+"p "+infile+" | awk '{print $5}'"))
                     ; indx_inst = indx - 1  ; line numner starting from 0, used in NCL language
                     indx_beg = indx+6 ; line numner starting from 1, used in linux sed command
                     indx_end = indx_beg+nchanl-1  ; line numner starting from 1, used in linux sed command
                     nchan = nchanl
                  end if
               end if
            end do  ; end line-by-line loop
            ichan = new(nchan,integer)
            ; ichan(:) = stringtointeger(charactertostring(cdata(indx_inst+6:indx_inst+5+nchan,4:9)))
            ; print("sed -n "+indx_beg+","+indx_end+"p "+infile+" | awk '{print $2}'")
            ichan = stringtointeger(systemfunc("sed -n "+indx_beg+","+indx_end+"p "+infile+" | awk '{print $2}'"))
            ichan!0 = "channel"
            ichan&channel = (/ ichan /)
            ; print(ichan)
            iuse = new((/nchan,ntime,npred/),integer)
            param = new((/nchan,ntime,npred/),float)
            param!0 = "channel"
            param!1 = "date"
            param!2 = "pred"
            param&channel = (/ ichan /)
            param&date = (/ stringtointeger(date) /)
         end if  ; end of if first itime
         vp = new(nchan,integer)
         vp(:) = npred + 2
         do np = 1, npred
            ip = np + 2
            iuse(:,itime,np-1) = stringtointeger(systemfunc("sed -n "+indx_beg+","+indx_end+"p "+infile+" | awk '{print $"+ip+"}'"))
            do ic = 1, nchan
               if ( iuse(ic-1,itime,np-1) .eq. 1 ) then
                  vp(ic-1) = vp(ic-1) + 1
                  pindx = indx_beg + ic - 1
                  param(ic-1,itime,np-1) = stringtofloat(systemfunc("sed -n "+pindx+"p "+infile+" | awk '{print $"+vp(ic-1)+"}'"))
               end if
            end do
         end do
      end if

   end do  ; end ntime_loop
   ; lglabels = new(nchan,string)
   lglabels = new(dimsizes(chanindex),string)
   lglabels = "ch"+ichan({channel|chanindex(:)})

   wks = gsn_open_wks(out_type,plotdir+"param_"+instrument)
   gsn_define_colormap(wks,"default")
   ;
   ; plot
   ;
   res                        = True
   res@gsnPaperOrientation    = "portrait"
   ; res@gsnPaperOrientation    = "landscape"
   res@gsnMaximize            = False               ; Maximize plot in frame.
   res@gsnFrame               = False               ; Don't advance the frame
   ; res@vpHeightF              = 0.25                ; change aspect ratio of plot
   ; res@vpWidthF               = 0.90
   res@tmYROn                 = False
   res@tmXTOn                 = False
   res@tmYLMinorOn            = False
   res@tmXBMinorOn            = False
   res@tmYLMajorThicknessF    = 1.0                 ; default 2.0
   res@tmXBMajorThicknessF    = 1.0                 ; default 2.0
   res@xyMarkLineMode         = "MarkLines"
   res@xyMarkers              = (/ 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3 /)
   res@xyMarkerColors         = (/ 2, 3, 4, 6, 7, 8, 10, 12, 13, 23 /)
   ; res@xyMarkerSizes          = (/ 0.02, 0.02 /)    ; default 0.01
   res@xyLineThicknesses      = (/ 2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0 /)      ; default 1.0
   res@xyLineColors           = (/ 2, 3, 4, 6, 7, 8, 10, 12, 13, 23 /)
   res@gsnYRefLine            = 0.0
   res@tmXBMode               = "Explicit"
   res@tmXBLabels             = mmddhh
   res@tmXBValues             = xc
   res@tmLabelAutoStride      = True                ; Auto stride
   res@tmXBLabelAngleF        = 15.
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Right"             ; Change location of
   res@pmLegendWidthF         = 0.1                 ; Change width and default 0.55
   ; res@pmLegendHeightF        = 0.15                ; height of legend. default 0.18
   ; res@lgLabelFontHeightF     = .015                ; change font height default 0.02
   res@xyExplicitLegendLabels = lglabels     ; explicit labels
   if ( num(.not.ismissing(param({channel|chanindex(:)},date|:,pred|0))) .gt. 0 ) then
      res@tiMainString = instrument+"   Offset"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|0),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|1))) .gt. 0 ) then
      res@tiMainString = instrument+"   1000-300 thickness"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|1),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|2))) .gt. 0 ) then
      res@tiMainString = instrument+"   200-50 thickness"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|2),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|3))) .gt. 0 ) then
      res@tiMainString = instrument+"   Skin temperature"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|3),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|4))) .gt. 0 ) then
      res@tiMainString = instrument+"   total column precipitable water"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|4),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|5))) .gt. 0 ) then
      res@tiMainString = instrument+"   satellite scan position"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|5),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|6))) .gt. 0 ) then
      res@tiMainString = instrument+"   satellite scan position**2"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|6),res)
      frame(wks)
   end  if
   if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|7))) .gt. 0 ) then
      res@tiMainString = instrument+"   satellite scan position**3"
      plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|7),res)
      frame(wks)
   end  if
   if ( npred .gt. 8 ) then
      if ( num(.not.ismissing(param({channel|chanindex},date|:,pred|8))) .gt. 0 ) then
         res@tiMainString = instrument+"   Gamma"
         plot = gsn_csm_xy(wks,xc,param({channel|chanindex},date|:,pred|8),res)
         frame(wks)
      end  if
   end if
   delete(res)
   destroy(wks)

end
