;
; Author: Hui-Chuan Lin   MMM/NCAR
;
; Purpose: to read in gts_omb_oma (from WRF-Var V2.2) and 
;          plot geographical distributions (before and after QC),
;               OBS vs BAK scatter plots (before and after QC), 
;               OBS vs ANA scatter plots
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;
;==============================================================================
; plot options
;==============================================================================
;
plot_gts      = True
plot_radar    = False

out_type     = "pdf"    ; or ncgm
plotdir      = "/sausage/hclin/exps/amps60/test/plotobs/"
datdir1      = "/sausage/hclin/exps/amps60/test/"          ; the path before DATE
datdir2      = "/wrfvar/working/"                          ; the path after DATE
filename_gts   = "gts_omb_oma"
prefix_radar   = "oma_radar"
start_date   = "2008012912"
end_date     = "2008012912"
cycle_period = 06
fname = "/sausage/hclin/exps/amps60/test/008012912/fg01"  ; for retrieving mapping info
;
; set pressure levels for plotting AIREP, GEOAMV, POLARAMV, PILOT, PROFILER, SOUND, BOGUS, AIRSR
; method 1: set a 2-D array of bounded pressure levels in hPa
; plevs  = (/ (/1005.,995./),(/930.,920./),(/855.,845/),(/705.,695./),(/505.,495./),(/405.,395./),\
;             (/305.,295./),(/255.,245./),(/205.,195./),(/155.,145./),(/105.,95./),(/75.,65./), \
;             (/55.,45./), (/45.,0./)  /)
 plevs    = (/ (/1050.,0./),(/-999.,-999./) /)   ; a (/-999.,-999./) pair is required if only
                                                  ; one bounded pair is desired
;
; method 2: set a 1-D array of fixed pressure levels in hPa
; plevs  = (/ 1000., 925., 850., 700., 500., 400., 300., 250., 200., 150., 100., 70., 50. /)
;
; set height levels for plotting RADAR and GPSREF
; method 1: set a 2-D array of bounded height levels in meters. Recommended for GPSREF
;hlevs  = (/ (/2000., 5000./), (/5000., 8500./) /)
hlevs  = (/ (/0., 25000./), (/-999., -999./) /)  ; (/-999.,-999./) pair is required if only
                                                  ; one bounded pair is desired
;
; method 2: set a 1-D array of fixed height levels in meters
; hlevs  = (/ 1000., 2000., 3000., 4000., 5000., 6000., 7000., 8000., 10000. /)

; mapinfo_from_file = False
  mapinfo_from_file = True
if ( mapinfo_from_file .and. .not.isfilepresent(fname) ) then
   print("file: "+fname+" does not exist")
   print("Can not extract mapping info. Stop.")
   exit
end if

; subdomain = True
  subdomain = False
if ( subdomain ) then   ; subdomain needs to be a lat-lon box
   maxlat = 35.
   minlat = 25.
   maxlon = -75.
   minlon = -90.
end if

if ( (.not.mapinfo_from_file) .and. (.not.subdomain) ) then
   ; mapproj = "LambertConformal"
   mapproj = "Mercator"
   if ( mapproj .eq. "LambertConformal" ) then
      truelat1 = 30.
      truelat2 = 60.
      clon     = -98.
   end if
   lat_ll = 0.1810659
   lon_ll = 60.26209
   lat_ur = 37.62015
   lon_ur = 100.7379
end if

system("mkdir -p "+plotdir)

markertype1 = 0         ; for scatter plot
markertype2 = 16        ; for distribution plot
markercolor1 = "blue"   ; BAK vs OBS before QC
markercolor2 = "red"    ; BAK vs OBS after QC
markercolor3 = "purple" ; ANA vs OBS
markersize1  = 0.008    ; MarkerSizeF efault 0.007
markersize2  = 0.006
markersize3  = 0.007
markersize4  = 0.003
linesize     = 1.5
linecolor    = "gray"

;==============================================================================
; end of plot options
;==============================================================================
if ( .not. ismissing(getenv("INITIAL_DATE")) ) then
   ; probably running from a script
   ; replace some selected variables with those from environment variables
   delete(plotdir)
   delete(datdir1)
   delete(start_date)
   delete(end_date)
   delete(cycle_period)
   delete(fname)
   plotdir = getenv("RUN_DIR")+"/"
   datdir1 = getenv("RUN_DIR")+"/"  ; datdir1 is the path before date
                                    ; Here I assume wrfvar is running under
                                    ; $RUN_DIR/$DATE/wrfvar/working
   start_date = getenv("INITIAL_DATE")
   end_date   = getenv("FINAL_DATE")
   cycle_period = stringtointeger(getenv("CYCLE_PERIOD"))
   fname = getenv("DA_FIRST_GUESS")
end if

plotwnd = False
colormap = "rainbow+gray"

if ( mapinfo_from_file .and. isfilepresent(fname) ) then
   wrf_file=addfile(fname+".nc","r")
   if ( wrf_file@MAP_PROJ .eq. 1 ) then
      mapproj = "LambertConformal"
      truelat1 = wrf_file@TRUELAT1
      truelat2 = wrf_file@TRUELAT2
      clon     = wrf_file@STAND_LON
   end if
   if ( wrf_file@MAP_PROJ .eq. 2 ) then
      mapproj = "Stereographic"
      truelat1 = wrf_file@TRUELAT1
      truelat2 = wrf_file@TRUELAT2
      clon     = wrf_file@CEN_LON
      clat     = wrf_file@CEN_LAT
   end if
   if ( wrf_file@MAP_PROJ .eq. 3 ) then
      mapproj = "Mercator"
   end if
   dsizes = getfiledimsizes(wrf_file)
   nx = dsizes(2)
   ny = dsizes(3)
   xlat=wrf_file->XLAT
   xlon=wrf_file->XLONG
   lat_ll  = xlat(0,0,0)
   lat_ur  = xlat(0,ny-1,nx-1)
   lon_ll  = xlon(0,0,0)
   lon_ur  = xlon(0,ny-1,nx-1)
   if ( subdomain .and. wrf_file@MAP_PROJ .eq. 3 ) then
      maxlat = lat_ur
      minlat = lat_ll
      maxlon = lon_ur
      minlon = lon_ll
   end if
end if

plevs = plevs * 100.
plevs@_FillValue = -999.
hlevs@_FillValue = -999.
nhlevs = dimsizes(hlevs)
nplevs = dimsizes(plevs)
rank_hlevs = dimsizes(dimsizes(hlevs))
rank_plevs = dimsizes(dimsizes(plevs))
;
; define and setup the lablebar colors (used by radar plots)
;
wks = gsn_open_wks("ncgm","tmp")
gsn_define_colormap(wks,colormap)                 ; define colormap
cmap = gsn_retrieve_colormap(wks)                 ; retreive colormap
if ( colormap .eq. "rainbow+gray" ) then
   num_bins = 208      ; rainbow+gray colormap has 239 colors, use 16-223
   colors = new(num_bins,integer)
   colors(0) = 16
   colors(num_bins-1) = 223
   do i = 1, num_bins-2
       colors(i) = colors(0) + i
   end do
end if
if ( colormap .eq. "rainbow" ) then
   num_bins = 175      ; rainbow colormap has 190 colors, use 15-189
   colors = new(num_bins,integer)
   colors(0) = 15
   colors(num_bins-1) = 189
   do i = 1, num_bins-2
       colors(i) = colors(0) + i
   end do
end if
if ( colormap .eq. "gui_default" ) then
   num_bins = 22       ; gui_default colormap has 24 colors, use 2-23
   colors = new(num_bins,integer)
   colors(0) = 2
   colors(num_bins-1) = 22
   do i = 1, num_bins-2
      colors(i) = colors(0) + i
   end do
end if
; set label bar RGB color triplets
labelbarcolors        = new((/num_bins,3/),float)
do n = 0, num_bins-1
   labelbarcolors(n,:) = cmap(colors(n),:)
end do
destroy(wks)
system("\rm -f tmp.ncgm")
;
;---------------------------------------------------------------------------
; functions and procedures
;---------------------------------------------------------------------------
;
; setup map resources
;
undef("setmpres")
function setmpres(title:string, str1:string, str2:string)
begin
   mpres                         = True
   mpres@gsnPaperOrientation     = "portrait"
   mpres@gsnMaximize             = False            ; Maximize plot in frame.
   mpres@gsnDraw                 = False
   mpres@gsnFrame                = False            ; Don't advance the frame

   if ( subdomain ) then
      mpres@mpProjection         = "CylindricalEquidistant"
      mpres@mpLimitMode          = "LatLon"
      mpres@mpMinLatF            = minlat
      mpres@mpMaxLatF            = maxlat
      mpres@mpMinLonF            = minlon
      mpres@mpMaxLonF            = maxlon
   else
      mpres@mpProjection            = mapproj          ; choose projection
      if ( mapproj .eq. "LambertConformal" ) then
         mpres@mpLambertParallel1F     = truelat1      ; two parallels
         mpres@mpLambertParallel2F     = truelat2
         mpres@mpLambertMeridianF      = clon          ; central meridian
      end if
      if ( mapproj .eq. "Stereographic" ) then
         mpres@mpCenterLatF         = clat
         mpres@mpCenterLonF         = clon
      end if
      mpres@mpLimitMode             = "Corners"
      mpres@mpLeftCornerLatF        = lat_ll
      mpres@mpLeftCornerLonF        = lon_ll
      mpres@mpRightCornerLatF       = lat_ur
      mpres@mpRightCornerLonF       = lon_ur
   end if

   mpres@pmTickMarkDisplayMode   = "Always"
   ; mpres@tmYROn                  = False
   ; mpres@tmXBOn                  = False
   mpres@tmXTMajorLengthF        = 0
   mpres@tmYLMajorLengthF        = 0
   mpres@tmXBMajorLengthF        = 0
   mpres@tmYRMajorLengthF        = 0
   mpres@mpOutlineBoundarySets   = "GeophysicalAndUSStates" ; state boundaries
   mpres@tfDoNDCOverlay          = True

   mpres@mpGeophysicalLineColor  = "Navy"
   mpres@mpGridAndLimbOn         = True
   mpres@mpGridLineDashPattern   = 2
   mpres@mpGridLineDashSegLenF   = 0.06            ; default 0.15

   mpres@mpDataBaseVersion       = "MediumRes"

   ; gsn resources:
   mpres@gsnStringFontHeightF    = 0.015
   mpres@gsnLeftString           = str1         ; add left string
   mpres@gsnRightString          = str2         ; add right string

   ; Title resources:
   mpres@tiMainString            = title
   mpres@tiMainOffsetYF          = 0.0             ; Move the title down.
   mpres@tiMainFontHeightF       = 0.015

   return(mpres)
end
;
; setup scatter plot resources
;
undef("setscres")
function setscres(title:string, xlabel:string, ylabel:string, linesize, linecolor)
begin
   scres                     = True
   scres@gsnPaperOrientation = "portrait"
   ; scres@gsnMaximize         = True
   scres@gsnDraw             = False
   scres@gsnFrame            = False
   scres@tmYROn              = False
   scres@tmXTOn              = False
   scres@tmYRBorderOn        = False
   scres@tmXTBorderOn        = False
   scres@tmYLMinorOn         = False
   scres@tmXBMinorOn         = False
   scres@gsnDraw             = False               ; don't draw yet
   scres@gsnFrame            = False               ; don't advance frame yet
   scres@tiMainString        = title
   scres@tiXAxisString       = xlabel              ; xaxis string
   scres@tiYAxisString       = ylabel              ; yaxis string
   scres@tmLabelAutoStride   = True                ; nice tick mark labels
   scres@tmYLMajorThicknessF = 1.0                 ; default 2.0
   scres@tmXBMajorThicknessF = 1.0                 ; default 2.0
   scres@xyLineColor         = linecolor           ; line color
   scres@xyDashPattern       = 0                   ; default 0 solid line
   scres@xyLineThicknessF    = linesize            ; line thicker
   return(scres)
end

undef("do_valueplot")
function do_valueplot(wks,data,lat,lon,lbdata,binwidth,colors,nbin,subdomain,mpres)
local data,xmax,xmin,lat,lon,colors,lbcolors,nbin,subdomain,mpres,gsres,lbres
begin
   lat_new = new((/nbin,dimsizes(data)/),float,-999)
   lon_new = new((/nbin,dimsizes(data)/),float,-999)
   do i = 0, nbin-1
      if ( i .eq. nbin-1 ) then
         indexes = ind(data.ge.lbdata(i).and.data.le.(lbdata(i+1)+0.5*binwidth))
      else
         indexes = ind(data.ge.lbdata(i).and.data.lt.lbdata(i+1))
      end if
      if (.not.any(ismissing(indexes))) then
         npts_range = dimsizes(indexes)   ; # of points in this range.
         lat_new(i,0:npts_range-1) = lat(indexes)
         lon_new(i,0:npts_range-1) = lon(indexes)
      end if
      delete(indexes)
   end do
   mpres@gsnMaximize             = True                ; Maximize plot in frame.
   if ( subdomain )
      plot = gsn_csm_map_ce(wks,mpres)
   else
      plot = gsn_csm_map(wks,mpres)
   end if
   gsres               = True
   gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
   do i = 0, nbin-1
      if (.not.ismissing(lat_new(i,0)))
         gsres@gsMarkerColor  = colors(i)
         gsres@gsMarkerSizeF  = 0.001          ; default 0.007
         str = unique_string("polymarker")
         plot@$str$ = gsn_add_polymarker(wks,plot,lon_new(i,:),lat_new(i,:),gsres)
         ; gsn_polymarker(wks,map,lon_new(i,:),lat_new(i,:),gsres)
      end if
   end do
   return(plot)
   delete(lat_new)
   delete(lon_new)
end

undef("do_scatterplot")
function do_scatterplot(wks, xdata, ydata, xlab, ylab, title, markertype, markersize, markercolor, \
                        linesize, linecolor)
local xdata, ydata, xlab, ylab, title, markertype, markersize, markercolor, \
      linesize, linecolor
begin  ; begin of function do_scatterplot
   ;
   data = xdata-ydata
   npts = num(.not.ismissing(data))
   info = " "
   if ( npts .gt. 0 ) then
      xmean = sum(data)/tofloat(npts)
      xrms  = sqrt(sum(data*data)/tofloat(npts))
      xstd  = sqrt(sum((data-xmean)*(data-xmean))/tofloat(npts))
      info  = "mean: "+sprintf("%10.4f",xmean)+"   rms: "+sprintf("%10.4f",xrms)+"   std: "+sprintf("%10.4f",xstd)
   end if
   minXY   = min( (/ min(xdata), min(ydata) /) )
   maxXY   = max( (/ max(xdata), max(ydata) /) )
   xy = (/ minXY, maxXY /)
   scres = setscres(title,xlab,ylab,linesize,linecolor)
   scres@gsnLeftString = info
   plot  = gsn_csm_xy (wks,xy,xy,scres)
   markres                     = True
   markres@gsMarkerIndex       = markertype                 ; choose type of marker
   markres@gsMarkerColor       = markercolor                ; Marker color
   markres@gsMarkerSizeF       = markersize                 ; Marker size (default 0.01)
   str = unique_string("polymarker")
   plot@$str$ = gsn_add_polymarker(wks,plot,xdata,ydata,markres) ; add the markers
   ; text       = gsn_add_text(wks,plot,info,0.75,0.35 ,False)
   return(plot)
end

undef("plot_sfc")
procedure plot_sfc(otype:string, wks:graphic,nobs, date, lat:float, lon:float, \
                   u:float, v:float, t:float, p:float, q:float, tpw:float, spd:float)
local plot, dummy, title, info, mpres, gsres, plon, plat, pnres, data1, data2, \
      minXY, maxXY, scres, markres, xy
begin  ; begin of procedure plot_sfc

   print("Processing "+otype+"  "+nobs)
   if ( otype .ne. "GPSPW" .and. otype .ne. "SSMIR" ) then
      if ( subdomain ) then
         do k = 0, 4   ; obs, inv, qc
            u(:,k) = mask(u(:,k),lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                 lat(:).ge.minlat.and.lat(:).le.maxlat,True)
            v(:,k) = mask(v(:,k),lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                 lat(:).ge.minlat.and.lat(:).le.maxlat,True)
         end do
      end if
      num_u_all = num(.not.ismissing(u(:,0)))
      num_u_qc  = num(.not.ismissing(mask(u(:,0),u(:,2).ge.0,True)))
      num_v_all = num(.not.ismissing(v(:,0)))
      num_v_qc  = num(.not.ismissing(mask(v(:,0),v(:,2).ge.0,True)))
      if ( otype.ne."QSCAT" ) then
         if ( subdomain ) then
            do k = 0, 4   ; obs, inv, qc
               t(:,k) = mask(t(:,k),lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                    lat(:).ge.minlat.and.lat(:).le.maxlat,True)
               p(:,k) = mask(p(:,k),lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                    lat(:).ge.minlat.and.lat(:).le.maxlat,True)
               q(:,k) = mask(q(:,k),lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                    lat(:).ge.minlat.and.lat(:).le.maxlat,True)
            end do
         end if
         num_t_all = num(.not.ismissing(t(:,0)))
         num_t_qc  = num(.not.ismissing(mask(t(:,0),t(:,2).ge.0,True)))
         num_p_all = num(.not.ismissing(p(:,0)))
         num_p_qc  = num(.not.ismissing(mask(p(:,0),p(:,2).ge.0,True)))
         num_q_all = num(.not.ismissing(q(:,0)))
         num_q_qc  = num(.not.ismissing(mask(q(:,0),q(:,2).ge.0,True)))
         num_qc    = num(.not.ismissing(mask(lat(:),u(:,2).ge.0.or.v(:,2).ge.0.or.\
                         t(:,2).ge.0.or.p(:,2).ge.0.or.q(:,2).ge.0,True)))
      else
         num_qc    = num(.not.ismissing(mask(lat(:),u(:,2).ge.0.or.v(:,2).ge.0,True)))
      end if

      if ( num_qc .gt. 0 ) then
         ;
         ; plot integrated distribution
         ;
         ; title = date+"  "+otype+"  "+nobs+"/"+num_qc
         title = otype
         str1  = date
         str2  = nobs+"/"+num_qc
         mpres = setmpres(title,str1,str2)
         mpres@gsnMaximize             = True                ; Maximize plot in frame.
         if ( subdomain ) then
            map = gsn_csm_map_ce(wks,mpres)
         else
            map = gsn_csm_map(wks,mpres)
         end if
         if ( otype .eq. "QSCAT" ) then
            plon = mask(lon(:),u(:,2).ge.0.or.v(:,2).ge.0,True)
            plat = mask(lat(:),u(:,2).ge.0.or.v(:,2).ge.0,True)
         else
            plon = mask(lon(:),u(:,2).ge.0.or.v(:,2).ge.0.or.t(:,2).ge.0.or.q(:,2).ge.0.or.p(:,2).ge.0,True)
            plat = mask(lat(:),u(:,2).ge.0.or.v(:,2).ge.0.or.t(:,2).ge.0.or.q(:,2).ge.0.or.p(:,2).ge.0,True)
            if ( plotwnd ) then
               txres               = True
               txres@txFontHeightF = 0.010
               txres@txFont        = "helvetica"
               txres@txFontColor    = "blue"
               text = gsn_add_text(wks,map,sprintf("%4.1f",t(:,0)-273.15),plon-0.35 ,plat+0.1,txres)
               txres@txFontColor    = "red"
               text = gsn_add_text(wks,map,sprintf("%6.0f",p(:,0)*0.01),plon+0.3 ,plat+0.1,txres)
            end if
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "navy"
         gsres@gsMarkerSizeF  = markersize1          ; default 0.007
         dummy = gsn_add_polymarker(wks,map,plon(:),plat(:),gsres)
         draw(map)
         if ( plotwnd .and. systemfunc("ncl -V") .gt. "4.2.0.a034" ) then
            ; wmsetp("unt", 1)   ; use metric units. this does not work, need to convert the unit manually
            wmsetp("col", 10)
            wmsetp("wbd", .2)    ; space between ticks
            wmsetp("wbs", .035)  ; size of the shaft.
            wmsetp("wbt", .48)   ; size of the tick.
            wmbarbmap(wks, plat, plon, mask(u(:,0)*1.94,u(:,2).ge.0,True) ,mask(v(:,0)*1.94,v(:,2).ge.0,True) )
            ; wmbarbmap(wks, plat, plon, mask(u(:,0),u(:,2).ge.0,True) ,mask(v(:,0),v(:,2).ge.0,True) )
         end if
         frame(wks)
         delete(dummy)
         delete(plon)
         delete(plat)
         delete(map)
         ;
         ; plot distribution for each variable
         ;
         plot = new(4,graphic)
         dummy = new(4,graphic)
         title = otype+" U (All: "+num_u_all+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(0) = gsn_csm_map_ce(wks,mpres)
         else
            plot(0) = gsn_csm_map(wks,mpres)
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "blue"
         if ( otype .eq. "QSCAT" ) then
            gsres@gsMarkerSizeF  = markersize2          ; default 0.007
         else
            gsres@gsMarkerSizeF  = markersize3          ; default 0.007
         end if
         plon = mask(lon(:),.not.ismissing(u(:,0)),True)
         plat = mask(lat(:),.not.ismissing(u(:,0)),True)
         ; txres = True
         ; txres@txFontHeightF = 0.008
         ; txres@txFontColor   = "black"
         ; text = gsn_add_text(wks,plot(0),sprintf("%5.1f",u(:,0)),plon,plat+0.1,txres)
         dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" U (QCed: "+num_u_qc+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(1) = gsn_csm_map_ce(wks,mpres)
         else
            plot(1) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),u(:,2).ge.0,True)
         plat = mask(lat(:),u(:,2).ge.0,True)
         ; txres = True
         ; txres@txFontHeightF = 0.008
         ; txres@txFontColor   = "black"
         ; text = gsn_add_text(wks,plot(1),sprintf("%5.1f",u(:,0)),plon,plat+0.1,txres)
         gsres@gsMarkerColor  = "red"
         dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" V (All: "+num_v_all+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(2) = gsn_csm_map_ce(wks,mpres)
         else
            plot(2) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),.not.ismissing(v(:,0)),True)
         plat = mask(lat(:),.not.ismissing(v(:,0)),True)
         ; txres = True
         ; txres@txFontHeightF = 0.008
         ; txres@txFontColor   = "black"
         ; text = gsn_add_text(wks,plot(2),sprintf("%5.1f",v(:,0)),plon,plat+0.1,txres)
         gsres@gsMarkerColor  = "blue"
         dummy(2) = gsn_add_polymarker(wks,plot(2),plon(:),plat(:),gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" V (QCed: "+num_v_qc+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(3) = gsn_csm_map_ce(wks,mpres)
         else
            plot(3) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),v(:,2).ge.0,True)
         plat = mask(lat(:),v(:,2).ge.0,True)
         ; txres = True
         ; txres@txFontHeightF = 0.008
         ; txres@txFontColor   = "black"
         ; text = gsn_add_text(wks,plot(3),sprintf("%5.1f",v(:,0)),plon,plat+0.1,txres)
         gsres@gsMarkerColor  = "red"
         dummy(3) = gsn_add_polymarker(wks,plot(3),plon,plat,gsres)
         delete(plon)
         delete(plat)
         pnres = True
         pnres@gsnPanelBottom = 0.03              ; add some space at bottom 
         pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         gsn_panel(wks,plot,(/2,2/),pnres)
         delete(dummy)
         delete(plot)
         ;
         if ( otype.ne."QSCAT" ) then
            plot = new(4,graphic)
            dummy = new(4,graphic)
            ;
            title = otype+" T (All: "+num_t_all+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(0) = gsn_csm_map_ce(wks,mpres)
            else
               plot(0) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),.not.ismissing(t(:,0)),True)
            plat = mask(lat(:),.not.ismissing(t(:,0)),True)
            gsres@gsMarkerColor  = "blue"
            dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
            delete(plon)
            delete(plat)
            ;
            title = otype+" T (QCed: "+num_t_qc+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(1) = gsn_csm_map_ce(wks,mpres)
            else
               plot(1) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),t(:,2).ge.0,True)
            plat = mask(lat(:),t(:,2).ge.0,True)
            gsres@gsMarkerColor  = "red"
            dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
            delete(plon)
            delete(plat)
            ;
            title = otype+" Q (All: "+num_q_all+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(2) = gsn_csm_map_ce(wks,mpres)
            else
               plot(2) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),.not.ismissing(q(:,0)),True)
            plat = mask(lat(:),.not.ismissing(q(:,0)),True)
            gsres@gsMarkerColor  = "blue"
            dummy(2) = gsn_add_polymarker(wks,plot(2),plon(:),plat(:),gsres)
            delete(plon)
            delete(plat)
            ;
            title = otype+" Q (QCed: "+num_q_qc+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(3) = gsn_csm_map_ce(wks,mpres)
            else
               plot(3) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),q(:,2).ge.0,True)
            plat = mask(lat(:),q(:,2).ge.0,True)
            gsres@gsMarkerColor  = "red"
            dummy(3) = gsn_add_polymarker(wks,plot(3),plon,plat,gsres)
            delete(plon)
            delete(plat)
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,plot,(/2,2/),pnres)
            delete(dummy)
            delete(plot)
            ;
            plot = new(4,graphic)
            dummy = new(4,graphic)
            ;
            title = otype+" P (All: "+num_p_all+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(0) = gsn_csm_map_ce(wks,mpres)
            else
               plot(0) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),.not.ismissing(p(:,0)),True)
            plat = mask(lat(:),.not.ismissing(p(:,0)),True)
            gsres@gsMarkerColor  = "blue"
            dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
            delete(plon)
            delete(plat)
            ;
            title = otype+" P (QCed: "+num_p_qc+")"
            mpres = setmpres(title,"","")
            if ( subdomain ) then
               plot(1) = gsn_csm_map_ce(wks,mpres)
            else
               plot(1) = gsn_csm_map(wks,mpres)
            end if
            plon = mask(lon(:),p(:,2).ge.0,True)
            plat = mask(lat(:),p(:,2).ge.0,True)
            gsres@gsMarkerColor  = "red"
            dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
            delete(plon)
            delete(plat)
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,plot,(/2,2/),pnres)
            delete(dummy)
            delete(plot)

         end if  ; end of if otype .ne. QSCAT
         ;
         ; scatter plots
         ;
         if ( num_u_all.gt.1 .and. num_u_qc.gt.1 .and. num_v_all.gt.1 .and. num_v_qc.gt.1 ) then
            ;
            ; OBS and BAK(Background) scatter plots
            ;
            ombplot = new(4,graphic)
            data1 = u(:,0)               ; OBS
            data2 = u(:,0)-u(:,1)        ; BAK = OBS - OMB
            ombplot(0) = do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "BAK U",  \
                            "All  "+num(.not.ismissing(data2)),               \
                            markertype1, markersize2, markercolor1, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = mask(u(:,0),u(:,2).ge.0,True)               ; OBS
            data2 = mask(u(:,0)-u(:,1),u(:,2).ge.0,True)        ; BAK
            ombplot(1) =  do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "BAK U",  \
                             "QCed  "+num(.not.ismissing(data2)),              \
                             markertype1, markersize1, markercolor2, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = v(:,0)               ; OBS
            data2 = v(:,0)-v(:,1)        ; BAK
            ombplot(2) =  do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "BAK V",  \
                             "All  "+num(.not.ismissing(data2)),               \
                             markertype1, markersize1, markercolor1, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = mask(v(:,0),v(:,2).ge.0,True)               ; OBS
            data2 = mask(v(:,0)-v(:,1),v(:,2).ge.0,True)        ; BAK
            ombplot(3) =  do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "BAK V",  \
                             "QCed  "+num(.not.ismissing(data2)),              \
                             markertype1, markersize1, markercolor2, linesize, linecolor)
            delete(data1)
            delete(data2)
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,ombplot,(/2,2/),pnres)
            ;
            ; OBS and ANA(Analysis) scatter plots
            ;
            omaplot = new(4,graphic)
            omaplot(0) = ombplot(1)
            data1 = mask(u(:,0),u(:,2).ge.0,True)               ; OBS
            data2 = mask(u(:,0)-u(:,4),u(:,2).ge.0,True)        ; ANA = OBS - OMA
            omaplot(1) =  do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "ANA U",  \
                             "QCed  "+num(.not.ismissing(data2)),              \
                             markertype1, markersize1, markercolor3, linesize, linecolor)
            delete(data1)
            delete(data2)
            omaplot(2) = ombplot(3)
            data1 = mask(v(:,0),v(:,2).ge.0,True)               ; OBS
            data2 = mask(v(:,0)-v(:,4),v(:,2).ge.0,True)        ; ANA = OBS - OMA
            omaplot(3) =  do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "ANA V",  \
                             "QCed  "+num(.not.ismissing(data2)),              \
                             markertype1, markersize1, markercolor3, linesize, linecolor)
            delete(data1)
            delete(data2)
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,omaplot,(/2,2/),pnres)
            delete(ombplot)
            delete(omaplot)
         end if

         if ( otype.ne."QSCAT" ) then
            ;
            ; OBS and BAK(Background) scatter plots
            ;
            if ( num_t_all.gt.1 .and. num_t_qc.gt.1 ) then
               ombplot = new(4,graphic)
               data1 = t(:,0)               ; OBS
               data2 = t(:,0)-t(:,1)        ; BAK
               ombplot(0) =  do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                                "All  "+num(.not.ismissing(data2)),               \
                                markertype1, markersize1, markercolor1, linesize, linecolor)
               delete(data1)
               delete(data2)
               data1 = mask(t(:,0),t(:,2).ge.0,True)               ; OBS
               data2 = mask(t(:,0)-t(:,1),t(:,2).ge.0,True)        ; BAK
               ombplot(1) =  do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                                "QCed  "+num(.not.ismissing(data2)),              \
                                markertype1, markersize1, markercolor2, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if

            if ( num_q_all.gt.1 .and. num_q_qc.gt.1 ) then
               if ( .not.isvar("ombplot") ) then
                  ombplot = new(4,graphic)
               end if
               data1 = q(:,0)               ; OBS
               data2 = q(:,0)-q(:,1)        ; BAK
               ombplot(2) =  do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "BAK Q",  \
                                "All  "+num(.not.ismissing(data2)),               \
                                markertype1, markersize1, markercolor1, linesize, linecolor)
               delete(data1)
               delete(data2)
               data1 = mask(q(:,0),q(:,2).ge.0,True)               ; OBS
               data2 = mask(q(:,0)-q(:,1),q(:,2).ge.0,True)        ; BAK
               ombplot(3) =  do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "BAK Q",  \
                                "QCed  "+num(.not.ismissing(data2)),              \
                                markertype1, markersize1, markercolor2, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if

            if ( isvar("ombplot") ) then
               pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
               gsn_panel(wks,ombplot,(/2,2/),pnres)
            end if
            ;
            ; OBS and ANA(Analysis) scatter plots
            ;
            if ( num_t_all.gt.1 .and. num_t_qc.gt.1 ) then
               omaplot = new(4,graphic)
               omaplot(0) = ombplot(1)
               data1 = mask(t(:,0),t(:,2).ge.0,True)               ; OBS
               data2 = mask(t(:,0)-t(:,4),t(:,2).ge.0,True)        ; ANA = OBS - OMA
               omaplot(1) =  do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "ANA T",  \
                                "QCed  "+num(.not.ismissing(data2)),              \
                                markertype1, markersize1, markercolor3, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if

            if ( num_q_all.gt.1 .and. num_q_qc.gt.1 ) then
               if ( .not.isvar("omaplot") )  then
                  omaplot = new(4,graphic)
               end if
               omaplot(2) = ombplot(3)
               data1 = mask(q(:,0),q(:,2).ge.0,True)               ; OBS
               data2 = mask(q(:,0)-q(:,4),q(:,2).ge.0,True)        ; ANA = OBS - OMA
               omaplot(3) =  do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "ANA Q",  \
                                "QCed  "+num(.not.ismissing(data2)),              \
                                markertype1, markersize1, markercolor3, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if

            if ( isvar("omaplot") ) then
               pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
               gsn_panel(wks,omaplot,(/2,2/),pnres)
               delete(ombplot)
               delete(omaplot)
            end if

         end if  ; end of if otype .ne. QSCAT
         ;
         if ( otype .ne. "QSCAT" .and. num_p_all.gt.1 .and. num_p_qc.gt.1 ) then
            plot = new(4,graphic)
            data1 = 0.01*p(:,0)               ; OBS
            data2 = 0.01*(p(:,0)-p(:,1))      ; BAK
            plot(0) =  do_scatterplot(wks, data1, data2, "OBS P  (hPa)", "BAK P",  \
                          "All  "+num(.not.ismissing(data2)),               \
                          markertype1, markersize1, markercolor1, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = 0.01*(mask(p(:,0),p(:,2).ge.0,True))              ; OBS
            data2 = 0.01*(mask(p(:,0)-p(:,1),p(:,2).ge.0,True))       ; BAK
            plot(1) =  do_scatterplot(wks, data1, data2, "OBS P  (hPa)", "BAK P",  \
                          "QCed  "+num(.not.ismissing(data2)),              \
                          markertype1, markersize1, markercolor2, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = 0.01*mask(p(:,0),p(:,2).ge.0,True)               ; OBS
            data2 = 0.01*mask(p(:,0)-p(:,4),p(:,2).ge.0,True)        ; ANA = OBS - OMA
            plot(3) =  do_scatterplot(wks, data1, data2, "OBS P  (hPa)", "ANA P",  \
                          "QCed  "+num(.not.ismissing(data2)),              \
                          markertype1, markersize1, markercolor3, linesize, linecolor)
            delete(data1)
            delete(data2)
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,plot,(/2,2/),pnres)
            delete(plot)
         end if

      end if   ; end of if num_qc > 0

    end if
;   else   ; for GPSPW
    if ( otype .eq. "GPSPW" ) then   ; for GPSPW

      if ( subdomain ) then
         tpw(:,0) = mask(tpw(:,0),lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                  lat(:).ge.minlat.and.lat(:).le.maxlat,True)
      end if
      num_tpw_all = num(.not.ismissing(tpw(:,0)))
      num_tpw_qc  = num(.not.ismissing(mask(tpw(:,0),tpw(:,2).ge.0,True)))
      num_qc = num_tpw_qc

      if ( num_qc .gt. 0 ) then
         plot = new(4,graphic)
         dummy = new(4,graphic)
         ;
         title = otype+" TPW (All: "+num_tpw_all+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(0) = gsn_csm_map_ce(wks,mpres)
         else
            plot(0) = gsn_csm_map(wks,mpres)
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "blue"
         gsres@gsMarkerSizeF  = markersize1          ; default 0.007
         plon = mask(lon(:),.not.ismissing(tpw(:,0)),True)
         plat = mask(lat(:),.not.ismissing(tpw(:,0)),True)
         dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" TPW (QCed: "+num_tpw_qc+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(1) = gsn_csm_map_ce(wks,mpres)
         else
            plot(1) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),tpw(:,2).ge.0,True)
         plat = mask(lat(:),tpw(:,2).ge.0,True)
         gsres@gsMarkerColor  = "red"
         dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
         delete(plon)
         delete(plat)
         pnres = True
         ; pnres@gsnPanelBottom = 0.08              ; add some space at bottom 
         pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         gsn_panel(wks,plot,(/2,2/),pnres)
         delete(plot)
         delete(dummy)

         if ( num_tpw_all .gt. 1 .and. num_tpw_qc .gt. 1  ) then
            plot = new(4,graphic)
            data1 = tpw(:,0)               ; OBS
            data2 = tpw(:,0)-tpw(:,1)      ; BAK
            plot(0) = do_scatterplot(wks, data1, data2, "OBS TPW  (cm)", "BAK TPW",  \
                         "All  "+num(.not.ismissing(data2)),                   \
                         markertype1, markersize2, markercolor1, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = mask(tpw(:,0),tpw(:,2).ge.0,True)               ; OBS
            data2 = mask(tpw(:,0)-tpw(:,1),tpw(:,2).ge.0,True)      ; BAK
            plot(1) = do_scatterplot(wks, data1, data2, "OBS TPW  (cm)", "BAK TPW",  \
                         "QCed  "+num(.not.ismissing(data2)),                  \
                         markertype1, markersize2, markercolor2, linesize, linecolor)
            delete(data1)
            delete(data2)
            data1 = mask(tpw(:,0),tpw(:,2).ge.0,True)               ; OBS
            data2 = mask(tpw(:,0)-tpw(:,4),tpw(:,2).ge.0,True)      ; ANA
            plot(3) = do_scatterplot(wks, data1, data2, "OBS TPW  (cm)", "ANA TPW",  \
                         "QCed  "+num(.not.ismissing(data2)),                  \
                         markertype1, markersize2, markercolor3, linesize, linecolor)
            delete(data1)
            delete(data2)
            pnres = True
            ; pnres@gsnPanelBottom = 0.08              ; add some space at bottom 
            pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            gsn_panel(wks,plot,(/2,2/),pnres)
            delete(plot)
         end if

      end if   ; end of if num_qc > 0

   end if  ; end of GPSPW check

   if ( otype .eq. "SSMIR" ) then   ; for SSMIR SSM/I Retrieval

      if ( subdomain ) then
         spd(:,0) = mask(spd(:,0),lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                  lat(:).ge.minlat.and.lat(:).le.maxlat,True)
      end if
      num_spd_all = num(.not.ismissing(spd(:,0)))
      num_spd_qc  = num(.not.ismissing(mask(spd(:,0),spd(:,2).ge.0,True)))
      num_qc = num_spd_qc

      if ( num_qc .gt. 0 ) then
         ;
         ; title = date+"  "+otype+"  "+nobs+"/"+num_qc
         title = "SSM/I Retrieval"
         str1  = date
         str2  = nobs+"/"+num_qc
         mpres = setmpres(title,str1,str2)
         mpres@gsnMaximize = True
         if ( subdomain ) then
            map = gsn_csm_map_ce(wks,mpres)
         else
            map = gsn_csm_map(wks,mpres)
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "navy"
         gsres@gsMarkerSizeF  = markersize4          ; default 0.007
         plon = mask(lon(:),.not.ismissing(spd(:,0)),True)
         plat = mask(lat(:),.not.ismissing(spd(:,0)),True)
         dummy = gsn_add_polymarker(wks,map,plon(:),plat(:),gsres)
         draw(map)
         frame(wks)
         delete(plon)
         delete(plat)
         delete(map)
         delete(dummy)
         ;
      end if

   end if  ; end of SSMIR check

end  ; end of procedure plot_sfc

undef("plot_upr")
procedure plot_upr(otype:string, wks:graphic,nobs, date, p1, p2, pres, lat:float, lon:float, \
                   u:float, v:float, t:float, q:float, ref:float, slp:float)
local k, pu, pv, pt, pq, pu_qc, pv_qc, pt_qc, pq_qc
begin

   info = p1+" Pa - "+p2+" Pa"
   print("Processing "+otype+" between "+info)
   num_u_all = 0
   num_u_qc = 0
   num_v_all = 0
   num_v_qc = 0
   num_t_all = 0
   num_t_qc = 0
   num_q_all = 0
   num_q_qc = 0

   if ( otype .eq. "BOGUS" ) then
      num_slp_all = 0
      num_slp_qc  = 0
      pslp    = slp
      pslp_qc = slp
      do k = 0, 4   ; obs, inv, qc
         if ( subdomain ) then
            pslp(:,k) = mask(slp(:,k), lon(:).ge.minlon.and.lon(:).le.maxlon.and. \
                                       lat(:).ge.minlat.and.lat(:).le.maxlat, True)
         else
            pslp(:,k) = slp(:,k)
         end if
      end do
      pslp_qc(:,0) = mask(pslp(:,0),pslp(:,2).ge.0,True)
      num_slp_all  = num(.not.ismissing(pslp(:,0)))
      num_slp_qc   = num(.not.ismissing(pslp_qc(:,0)))
   end if

   if ( otype .ne. "GPSREF" ) then
      pu = u
      pv = v
      pt = t
      pq = q
      do k = 0, 4   ; obs, inv, qc
         if ( otype .ne. "AIRSR" ) then
            if ( subdomain ) then
               pu(:,k) = mask(u(:,k), pres.le.p1.and.pres.ge.p2.and.              \
                                      lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                      lat(:).ge.minlat.and.lat(:).le.maxlat, True)
               pv(:,k) = mask(v(:,k), pres.le.p1.and.pres.ge.p2.and.              \
                                      lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                      lat(:).ge.minlat.and.lat(:).le.maxlat, True)
            else
               pu(:,k) = mask(u(:,k), pres.le.p1.and.pres.ge.p2, True)
               pv(:,k) = mask(v(:,k), pres.le.p1.and.pres.ge.p2, True)
            end if
         end if
         if ( otype.eq."SOUND" .or. otype.eq."AIREP" .or. otype.eq."AIRSR" ) then
            if ( subdomain ) then
               pt(:,k) = mask(t(:,k), pres.le.p1.and.pres.ge.p2.and.              \
                                      lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                      lat(:).ge.minlat.and.lat(:).le.maxlat, True)
            else
               pt(:,k) = mask(t(:,k), pres.le.p1.and.pres.ge.p2, True)
            end if
         end if
         if ( otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
            if ( subdomain ) then
               pq(:,k) = mask(q(:,k), pres.le.p1.and.pres.ge.p2.and.              \
                                      lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                      lat(:).ge.minlat.and.lat(:).le.maxlat, True)
            else
               pq(:,k) = mask(q(:,k), pres.le.p1.and.pres.ge.p2, True)
            end if
         end if
      end do
      if ( otype .ne. "AIRSR" ) then
         pu_qc = pu
         pv_qc = pv
         pu_qc(:,0) = mask(pu(:,0),pu(:,2).ge.0,True)
         pv_qc(:,0) = mask(pv(:,0),pv(:,2).ge.0,True)
         num_u_all = num(.not.ismissing(pu(:,0)))
         num_u_qc  = num(.not.ismissing(pu_qc(:,0)))
         num_v_all = num(.not.ismissing(pv(:,0)))
         num_v_qc  = num(.not.ismissing(pv_qc(:,0)))
      end if
      if ( otype.eq."SOUND" .or. otype.eq."AIREP" .or. otype.eq."AIRSR" ) then
         pt_qc = pt
         pt_qc(:,0) = mask(pt(:,0),pt(:,2).ge.0,True)
         num_t_all = num(.not.ismissing(pt(:,0)))
         num_t_qc  = num(.not.ismissing(pt_qc(:,0)))
      end if
      if ( otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
         pq_qc = pq
         pq_qc(:,0) = mask(pq(:,0),pq(:,2).ge.0,True)
         num_q_all = num(.not.ismissing(pq(:,0)))
         num_q_qc  = num(.not.ismissing(pq_qc(:,0)))
      end if

      if ( otype.eq."BOGUS" ) then
         ; num_qc = num(.not.ismissing(pu_qc(:,2).ge.0.and.pv_qc(:,2).ge.0.and.pslp_qc(:,2).ge.0))
         num_qc = num(.not.ismissing(pu_qc(:,2).ge.0.and.pv_qc(:,2).ge.0))
         plon = mask(lon(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
         plat = mask(lat(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
      end if
      if ( otype.eq."GEOAMV" .or. otype.eq."POLARAMV" .or. otype.eq."PILOT" .or. otype.eq."PROFILER" ) then
         num_qc = num(.not.ismissing(pu_qc(:,2).ge.0.and.pv_qc(:,2).ge.0))
         plon = mask(lon(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
         plat = mask(lat(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
      end if
      if ( otype.eq."SOUND" ) then
         num_qc = num(.not.ismissing(pu_qc(:,2).ge.0.and.pv_qc(:,2).ge.0.and.pt_qc(:,2).ge.0.and.pq_qc(:,2).ge.0))
         plon = mask(lon(:),pu(:,2).ge.0.or.pv(:,2).ge.0.or.pt(:,2).ge.0.or.pq(:,2).ge.0,True)
         plat = mask(lat(:),pu(:,2).ge.0.or.pv(:,2).ge.0.or.pt(:,2).ge.0.or.pq(:,2).ge.0,True)
      end if
      if ( otype.eq."AIREP" ) then
         num_qc = num(.not.ismissing(pu_qc(:,2).ge.0.and.pv_qc(:,2).ge.0.and.pt_qc(:,2).ge.0))
         plon = mask(lon(:),pu(:,2).ge.0.or.pv(:,2).ge.0.or.pt(:,2).ge.0,True)
         plat = mask(lat(:),pu(:,2).ge.0.or.pv(:,2).ge.0.or.pt(:,2).ge.0,True)
      end if
      if ( otype.eq."AIRSR" ) then
         num_qc = num(.not.ismissing(pt_qc(:,2).ge.0.and.pq_qc(:,2).ge.0))
         plon = mask(lon(:),pt(:,2).ge.0.or.pq(:,2).ge.0,True)
         plat = mask(lat(:),pt(:,2).ge.0.or.pq(:,2).ge.0,True)
      end if

      ; if ( num_u_all.gt.0 .or. num_u_qc.gt.0 .or. num_v_all.gt.0 .or. num_v_qc.gt.0 ) then
      if ( num_qc .gt. 0 ) then
         ;
         ; plot integrated distribution
         ;
         ; title = date+"    "+otype+"   "+info+"  "+num_qc
         title = otype+"    "+info
         str1  = date
         str2  = ""+num_qc
         mpres = setmpres(title,str1,str2)
         mpres@gsnMaximize             = True                ; Maximize plot in frame.
         if ( subdomain ) then
            map = gsn_csm_map_ce(wks,mpres)
         else
            map = gsn_csm_map(wks,mpres)
         end if
         ; plon = mask(lon(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
         ; plat = mask(lat(:),pu(:,2).ge.0.or.pv(:,2).ge.0,True)
         if ( plotwnd .and. (otype.eq."SOUND" .or. otype.eq."AIREP" .or. otype.eq."AIRSR") ) then
            txres               = True
            txres@txFontHeightF = 0.010
            txres@txFont        = "helvetica"
            txres@txFontColor    = "blue"
            text = gsn_add_text(wks,map,sprintf("%4.1f",t(:,0)-273.15),plon-0.35 ,plat+0.1,txres)
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "navy"
         gsres@gsMarkerSizeF  = markersize1          ; default 0.007
         dummy = gsn_add_polymarker(wks,map,plon(:),plat(:),gsres)
         draw(map)
         if ( plotwnd .and. systemfunc("ncl -V") .gt. "4.2.0.a034" ) then
            ; wmsetp("unt", 1)   ; use metric units
            wmsetp("col", 10) 
            wmsetp("wbd", .2)   ; space between ticks
            wmsetp("wbs", .03 )  ; size of the shaft.
            wmsetp("wbt", .48)  ; size of the tick.
            wmbarbmap(wks, plat, plon, pu_qc(:,0)*1.94 , pv_qc(:,0)*1.94)  ; Plot barbs.
            ; wmbarbmap(wks, plat, plon, pu_qc(:,0) , pv_qc(:,0))  ; Plot barbs.
         end if
         frame(wks)
         delete(dummy)
         delete(plon)
         delete(plat)
         delete(map)
         ;
         ; plot distribution for each variable
         ;
         plot = new(4,graphic)
         dummy = new(4,graphic)
         ;
         title = otype+" U (All: "+num_u_all+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(0) = gsn_csm_map_ce(wks,mpres)
         else
            plot(0) = gsn_csm_map(wks,mpres)
         end if
         gsres               = True
         gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
         gsres@gsMarkerColor  = "blue"
         gsres@gsMarkerSizeF  = markersize3          ; default 0.007
         plon = mask(lon(:),.not.ismissing(pu(:,0)),True)
         plat = mask(lat(:),.not.ismissing(pu(:,0)),True)
         dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" U (QCed: "+num_u_qc+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(1) = gsn_csm_map_ce(wks,mpres)
         else
            plot(1) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),pu(:,2).ge.0,True)
         plat = mask(lat(:),pu(:,2).ge.0,True)
         gsres@gsMarkerColor  = "red"
         dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
         ; wmsetp("col", 2)    ; Draw in red.
         ; wmsetp("wbs", .01)  ; Increase the size of the barb.
         ; wmbarbmap(wks, plat, plon,  mask(pu(:,0),pu(:,2).ge.0,True), mask(pv(:,0),pv(:,2).ge.0,True))
         delete(plon)
         delete(plat)
         ;
         title = otype+" V (All: "+num_v_all+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(2) = gsn_csm_map_ce(wks,mpres)
         else
            plot(2) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),.not.ismissing(pv(:,0)),True)
         plat = mask(lat(:),.not.ismissing(pv(:,0)),True)
         gsres@gsMarkerColor  = "blue"
         dummy(2) = gsn_add_polymarker(wks,plot(2),plon(:),plat(:),gsres)
         delete(plon)
         delete(plat)
         ;
         title = otype+" V (QCed: "+num_v_qc+")"
         mpres = setmpres(title,"","")
         if ( subdomain ) then
            plot(3) = gsn_csm_map_ce(wks,mpres)
         else
            plot(3) = gsn_csm_map(wks,mpres)
         end if
         plon = mask(lon(:),pv(:,2).ge.0,True)
         plat = mask(lat(:),pv(:,2).ge.0,True)
         gsres@gsMarkerColor  = "red"
         dummy(3) = gsn_add_polymarker(wks,plot(3),plon,plat,gsres)
         delete(plon)
         delete(plat)
         pnres = True
         pnres@gsnPanelBottom = 0.03              ; add some space at bottom 
         ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"    "+info
         gsn_panel(wks,plot,(/2,2/),pnres)
         delete(dummy)
         delete(plot)

         if ( otype.eq. "AIREP" .or. otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
            plot = new(4,graphic)
            dummy = new(4,graphic)
            ;
            if ( num_t_all.gt.0 .or. num_t_qc.gt.0 ) then
               title = otype+" T (All: "+num_t_all+")"
               mpres = setmpres(title,"","")
               if ( subdomain ) then
                  plot(0) = gsn_csm_map_ce(wks,mpres)
               else
                  plot(0) = gsn_csm_map(wks,mpres)
               end if
               plon = mask(lon(:),.not.ismissing(pt(:,0)),True)
               plat = mask(lat(:),.not.ismissing(pt(:,0)),True)
               gsres = True
               gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
               if ( otype .eq. "AIRSR" ) then
                  gsres@gsMarkerSizeF  = markersize2          ; default 0.007
               else
                  gsres@gsMarkerSizeF  = markersize3          ; default 0.007
               end if
               gsres@gsMarkerColor  = "blue"
               dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
               delete(plon)
               delete(plat)
               ;
               title = otype+" T (QCed: "+num_t_qc+")"
               mpres = setmpres(title,"","")
               if ( subdomain ) then
                  plot(1) = gsn_csm_map_ce(wks,mpres)
               else
                  plot(1) = gsn_csm_map(wks,mpres)
               end if
               plon = mask(lon(:),pt(:,2).ge.0,True)
               plat = mask(lat(:),pt(:,2).ge.0,True)
               gsres@gsMarkerColor  = "red"
               dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
               delete(plon)
               delete(plat)
               ;
               if ( otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
                  if ( num_q_all.gt.0 .or. num_q_qc.gt.0 ) then
                     title = otype+" Q (All: "+num_q_all+")"
                     mpres = setmpres(title,"","")
                     if ( subdomain ) then
                        plot(2) = gsn_csm_map_ce(wks,mpres)
                     else
                        plot(2) = gsn_csm_map(wks,mpres)
                     end if
                     plon = mask(lon(:),.not.ismissing(pq(:,0)),True)
                     plat = mask(lat(:),.not.ismissing(pq(:,0)),True)
                     gsres@gsMarkerColor  = "blue"
                     dummy(2) = gsn_add_polymarker(wks,plot(2),plon(:),plat(:),gsres)
                     delete(plon)
                     delete(plat)
                     ;
                     title = otype+" Q (QCed: "+num_q_qc+")"
                     mpres = setmpres(title,"","")
                     if ( subdomain ) then
                        plot(3) = gsn_csm_map_ce(wks,mpres)
                     else
                        plot(3) = gsn_csm_map(wks,mpres)
                     end if
                     plon = mask(lon(:),pq(:,2).ge.0,True)
                     plat = mask(lat(:),pq(:,2).ge.0,True)
                     gsres@gsMarkerColor  = "red"
                     dummy(3) = gsn_add_polymarker(wks,plot(3),plon,plat,gsres)
                     delete(plon)
                     delete(plat)
                  end if
               end if
               pnres = True
               ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
               pnres@txString = date+"    "+otype+"    "+info
               gsn_panel(wks,plot,(/2,2/),pnres)
               delete(dummy)
               delete(plot)
            end if
         end if
      end if
      ;
      ; scatter plot
      ;
      if ( otype .eq. "BOGUS" .and. num_slp_all.gt.1 .and. num_slp_qc.gt.1 ) then
         plot = new(4,graphic)
         data1 = pslp(:,0)               ; OBS
         data2 = pslp(:,0)-pslp(:,1)     ; BAK
         plot(0) = do_scatterplot(wks, data1, data2, "OBS SLP  (hPa)", "BAK SLP",  \
                      "All  "+num(.not.ismissing(data2)),                   \
                      markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pslp_qc(:,0),pslp_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(pslp_qc(:,0)-pslp_qc(:,1),pslp_qc(:,2).ge.0,True)  ; BAK
         plot(1) = do_scatterplot(wks, data1, data2, "OBS SLP  (hPa)", "BAK SLP",  \
                      "QCed  "+num(.not.ismissing(data2)),                  \
                      markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pslp(:,0),pslp(:,2).ge.0,True)               ; OBS
         data2 = mask(pslp(:,0)-pslp(:,4),pslp(:,2).ge.0,True)     ; ANA
         plot(3) = do_scatterplot(wks, data1, data2, "OBS SLP  (hPa)", "ANA SLP",  \
                      "QCed  "+num(.not.ismissing(data2)),                  \
                      markertype1, markersize2, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
         pnres = True
         ; pnres@gsnPanelBottom = 0.08              ; add some space at bottom 
         pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         gsn_panel(wks,plot,(/2,2/),pnres)
         delete(plot)
      end if

      if ( num_u_all.gt.1 .and. num_u_qc.gt.1 .and. num_v_all.gt.1 .and. num_v_qc.gt.1 ) then
         ;
         ; OBS and BAK(Background) scatter plot
         ;
         ombplot = new(4,graphic)
         ;
         data1 = pu(:,0)               ; OBS
         data2 = pu(:,0)-pu(:,1)       ; BAK
         ombplot(0) = do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "BAK U",  \
                         "All  "+num(.not.ismissing(data2)),               \
                         markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pu_qc(:,0),pu_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(pu_qc(:,0)-pu_qc(:,1),pu_qc(:,2).ge.0,True)    ; BAK
         ombplot(1) = do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "BAK U",  \
                         "QCed  "+num(.not.ismissing(data2)),              \
                         markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = pv(:,0)               ; OBS
         data2 = pv(:,0)-pv(:,1)       ; BAK
         ombplot(2) = do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "BAK V",  \
                         "All  "+num(.not.ismissing(data2)),               \
                         markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pv_qc(:,0),pv_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(pv_qc(:,0)-pv_qc(:,1),pv_qc(:,2).ge.0,True)    ; BAK
         ombplot(3) = do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "BAK V",  \
                         "QCed  "+num(.not.ismissing(data2)),              \
                         markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
         ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"    "+info
         gsn_panel(wks,ombplot,(/2,2/),pnres)
         ;
         ; OBS and ANA(Analysis) scatter plot
         ;
         omaplot = new(4,graphic)
         omaplot(0) = ombplot(1)
         data1 = mask(pu_qc(:,0),pu_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(pu_qc(:,0)-pu_qc(:,4),pu_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
         omaplot(1) =  do_scatterplot(wks, data1, data2, "OBS U  (m/s)", "ANA U",  \
                          "QCed  "+num(.not.ismissing(data2)),              \
                          markertype1, markersize1, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
         omaplot(2) = ombplot(3)
         data1 = mask(pv_qc(:,0),pv_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(pv_qc(:,0)-pv_qc(:,4),pv_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
         omaplot(3) =  do_scatterplot(wks, data1, data2, "OBS V  (m/s)", "ANA V",  \
                          "QCed  "+num(.not.ismissing(data2)),              \
                          markertype1, markersize1, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
         ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"    "+info
         gsn_panel(wks,omaplot,(/2,2/),pnres)
         delete(ombplot)
         delete(omaplot)
      end if

      if ( otype.eq. "AIREP" .or. otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
         ;
         ; OBS and BAK(Background) scatter plot
         ;
         if ( num_t_all.gt.1 .and. num_t_qc.gt.1 ) then
            ombplot = new(4,graphic)
            data1 = pt(:,0)               ; OBS
            data2 = pt(:,0)-pt(:,1)       ; BAK
            if ( otype .eq. "AIRSR" ) then
               ombplot(0) = do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                               "All  "+num(.not.ismissing(data2)),               \
                               markertype1, markersize1, markercolor1, linesize, linecolor)
            else
               ombplot(0) = do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                               "All  "+num(.not.ismissing(data2)),               \
                               markertype1, markersize2, markercolor1, linesize, linecolor)
            end if
            delete(data1)
            delete(data2)
            data1 = mask(pt_qc(:,0),pt_qc(:,2).ge.0,True)               ; OBS
            data2 = mask(pt_qc(:,0)-pt_qc(:,1),pt_qc(:,2).ge.0,True)    ; BAK
            if ( otype .eq. "AIRSR" ) then
               ombplot(1) = do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                               "QCed  "+num(.not.ismissing(data2)), \
                               markertype1, markersize1, markercolor2, linesize, linecolor)
            else
               ombplot(1) = do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "BAK T",  \
                               "QCed  "+num(.not.ismissing(data2)),              \
                               markertype1, markersize2, markercolor2, linesize, linecolor)
            end if
            delete(data1)
            delete(data2)
         end if

         if ( otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
            if ( num_q_all.gt.1 .and. num_q_qc.gt.1 ) then
               if ( .not.isvar("ombplot") ) then
                  ombplot = new(4,graphic)
               end if
               data1 = pq(:,0)               ; OBS
               data2 = pq(:,0)-pq(:,1)       ; BAK
               ombplot(2) = do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "BAK Q", \
                               "All  "+num(.not.ismissing(data2)),              \
                               markertype1, markersize1, markercolor1, linesize, linecolor)
               delete(data1)
               delete(data2)
               data1 = mask(pq_qc(:,0),pq_qc(:,2).ge.0,True)               ; OBS
               data2 = mask(pq_qc(:,0)-pq_qc(:,1),pq_qc(:,2).ge.0,True)    ; BAK
               ombplot(3) = do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "BAK Q", \
                               "QCed  "+num(.not.ismissing(data2)),             \
                               markertype1, markersize1, markercolor2, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if
         end if

         if ( isvar("ombplot") ) then
            ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            pnres@txString = date+"    "+otype+"    "+info
            gsn_panel(wks,ombplot,(/2,2/),pnres)
         end if
         ;
         ; OBS and ANA(Analysis) scatter plot
         ;
         if ( num_t_all.gt.1 .and. num_t_qc.gt.1 ) then
            omaplot = new(4,graphic)
            omaplot(0) = ombplot(1)
            data1 = mask(pt_qc(:,0),pt_qc(:,2).ge.0,True)               ; OBS
            data2 = mask(pt_qc(:,0)-pt_qc(:,4),pt_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
            omaplot(1) =  do_scatterplot(wks, data1, data2, "OBS T  (degree K)", "ANA T", \
                             "QCed  "+num(.not.ismissing(data2)),             \
                             markertype1, markersize1, markercolor3, linesize, linecolor)
            delete(data1)
            delete(data2)
         end if

         if ( otype.eq."SOUND" .or. otype.eq."AIRSR" ) then
            if ( num_q_all.gt.1 .and. num_q_qc.gt.1 ) then
               if ( .not.isvar("omaplot") ) then
                  omaplot = new(4,graphic)
               end if
               omaplot(2) = ombplot(3)
               data1 = mask(pq_qc(:,0),pq_qc(:,2).ge.0,True)               ; OBS
               data2 = mask(pq_qc(:,0)-pq_qc(:,4),pq_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
               omaplot(3) =  do_scatterplot(wks, data1, data2, "OBS Q  (kg/kg)", "ANA Q", \
                                "QCed  "+num(.not.ismissing(data2)),             \
                                markertype1, markersize1, markercolor3, linesize, linecolor)
               delete(data1)
               delete(data2)
            end if
         end if

         if ( isvar("omaplot") ) then
            ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
            pnres@txString = date+"    "+otype+"    "+info
            gsn_panel(wks,omaplot,(/2,2/),pnres)
            delete(ombplot)
            delete(omaplot)
         end if

      end if
   end if
end  ; end of procedure plot_upr

undef("plot_upr_h")
procedure plot_upr_h(otype:string, wks:graphic,nobs, date, h1, h2, hgt, lat:float, lon:float, \
                   rv:float, rf:float, ref:float)
local k, prv, prf, pref, prv_qc, prf_qc, pref_qc
begin
   info = h1+" m - "+h2+" m"
   print("Processing "+otype+" between "+info)
   if ( otype .eq. "RADAR" ) then
      num_rv_all = 0
      num_rv_qc  = 0
      num_rf_all = 0
      num_rf_qc  = 0
      prv    = rv
      prf    = rf
      do k = 0, 4   ; obs, inv, qc
         if ( subdomain ) then
            prv(:,k) = mask(rv(:,k),hgt.le.h2.and.hgt.ge.h1.and.                \
                                    lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                    lat(:).ge.minlat.and.lat(:).le.maxlat, True)
            prf(:,k) = mask(rf(:,k),hgt.le.h2.and.hgt.ge.h1.and.                \
                                    lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                    lat(:).ge.minlat.and.lat(:).le.maxlat, True)
         else
            prv(:,k) = mask(rv(:,k), hgt.le.h2.and.hgt.ge.h1, True)
            prf(:,k) = mask(rf(:,k), hgt.le.h2.and.hgt.ge.h1, True)
         end if
      end do
      prv_qc = prv
      prf_qc = prf
      prv_qc(:,0) = mask(prv(:,0),prv(:,2).ge.0,True)
      prf_qc(:,0) = mask(prf(:,0),prf(:,2).ge.0,True)
      num_rv_all  = num(.not.ismissing(prv(:,0)))
      num_rv_qc   = num(.not.ismissing(prv_qc(:,0)))
      num_rf_all  = num(.not.ismissing(prf(:,0)))
      num_rf_qc   = num(.not.ismissing(prf_qc(:,0)))
      num_qc = num(.not.ismissing(prv_qc(:,2).ge.0.and.prf_qc(:,2).ge.0))
   end if

   if ( otype .eq. "GPSREF" ) then
      num_ref_all = 0
      num_ref_qc = 0
      pref = ref
      do k = 0, 4   ; obs, inv, qc
         if ( subdomain ) then
            pref(:,k) = mask(ref(:,k),hgt.le.h2.and.hgt.ge.h1.and.                \
                                      lon(:).ge.minlon.and.lon(:).le.maxlon.and.  \
                                      lat(:).ge.minlat.and.lat(:).le.maxlat, True)
         else
            pref(:,k) = mask(ref(:,k), hgt.le.h2.and.hgt.ge.h1, True)
         end if
      end do
      pref_qc = pref
      pref_qc(:,0) = mask(pref(:,0),pref(:,2).ge.0,True)
      num_ref_all  = num(.not.ismissing(pref(:,0)))
      num_ref_qc   = num(.not.ismissing(pref_qc(:,0)))
      num_qc = num_ref_qc
   end if

   if ( otype .eq. "RADAR" .and. num_qc .gt. 0 ) then
      ;
      ; plot distribution for each variable
      ;
      plot = new(4,graphic)
      dummy = new(4,graphic)
      ;
      title = otype+" RV (All: "+num_rv_all+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
         plot(0) = gsn_csm_map_ce(wks,mpres)
      else
         plot(0) = gsn_csm_map(wks,mpres)
      end if
      gsres               = True
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "blue"
      gsres@gsMarkerSizeF  = 0.001          ; default 0.007
      plon = mask(lon(:),.not.ismissing(prv(:,0)),True)
      plat = mask(lat(:),.not.ismissing(prv(:,0)),True)
      dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
      delete(plon)
      delete(plat)
      ;
      title = otype+" RV (QCed: "+num_rv_qc+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
         plot(1) = gsn_csm_map_ce(wks,mpres)
      else
         plot(1) = gsn_csm_map(wks,mpres)
      end if
      plon = mask(lon(:),prv(:,2).ge.0,True)
      plat = mask(lat(:),prv(:,2).ge.0,True)
      gsres@gsMarkerColor  = "red"
      dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
      delete(plon)
      delete(plat)
      ;
      title = otype+" RF (All: "+num_rf_all+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
         plot(2) = gsn_csm_map_ce(wks,mpres)
      else
         plot(2) = gsn_csm_map(wks,mpres)
      end if
      plon = mask(lon(:),.not.ismissing(prf(:,0)),True)
      plat = mask(lat(:),.not.ismissing(prf(:,0)),True)
      gsres@gsMarkerColor  = "blue"
      dummy(2) = gsn_add_polymarker(wks,plot(2),plon(:),plat(:),gsres)
      delete(plon)
      delete(plat)
      ;
      title = otype+" RF (QCed: "+num_rf_qc+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
         plot(3) = gsn_csm_map_ce(wks,mpres)
      else
         plot(3) = gsn_csm_map(wks,mpres)
      end if
      plon = mask(lon(:),prf(:,2).ge.0,True)
      plat = mask(lat(:),prf(:,2).ge.0,True)
      gsres@gsMarkerColor  = "red"
      dummy(3) = gsn_add_polymarker(wks,plot(3),plon,plat,gsres)
      delete(plon)
      delete(plat)
      pnres = True
      pnres@gsnPanelBottom = 0.03              ; add some space at bottom
      ; pnres@txString = date+"    "+otype+"    "+nobs+" / "+num_qc
      pnres@txString = date+"    "+otype+"    "+info
      gsn_panel(wks,plot,(/2,2/),pnres)
      delete(dummy)
      delete(plot)

      if ( num_rv_all.gt.0 .and. num_rv_qc.gt.0 ) then
         valplot = new(4,graphic)
         obs = mask(prv_qc(:,0),prv_qc(:,2).ge.0,True)
         bak = mask(prv_qc(:,0)-prv_qc(:,1),prv_qc(:,2).ge.0,True)
         omb = mask(prv_qc(:,1),prv_qc(:,2).ge.0,True)
         ana = mask(prv_qc(:,0)-prv_qc(:,4),prv_qc(:,2).ge.0,True)
         xmax = max((/max(obs),max(bak),max(omb),max(ana)/))
         xmin = min((/min(obs),min(bak),min(omb),min(ana)/))
         range = xmax - xmin
         binwidth = range/tofloat(num_bins)
         lbdata = new(num_bins+1,float)
         do i = 0, num_bins
            lbdata(i) = xmin + i * binwidth
         end do
         mpres@tiMainString = "OBS RV"
         valplot(0) = do_valueplot(wks,obs,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "BAK RV"
         valplot(1) = do_valueplot(wks,bak,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "OMB RV"
         valplot(2) = do_valueplot(wks,omb,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "ANA RV"
         valplot(3) = do_valueplot(wks,ana,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         lbres = True
         lbres@lbPerimOn            = False               ; no label bar box
         lbres@lbOrientation        = "Horizontal"        ; orientation
         lbres@vpWidthF             = 0.9                ; size
         lbres@vpHeightF            = 0.06
         lbres@lbLabelFontHeightF   = 0.01                ; label font height default 0.02
         lbres@lbLabelAlignment     = "InteriorEdges"     ; where to label
         lbres@lbLabelAutoStride    = True                ; Auto stride
         lbres@lbMonoFillPattern    = True                ; fill sold
         lbres@lbFillColors         = labelbarcolors            ; must be RGB triplets
         lbres@lbBoxLinesOn         = False
         xloc = 0.05
         yloc = 0.05
         if ( max(lbdata) .le. 10. ) then
            gsn_labelbar_ndc (wks,num_bins,sprintf("%5.2f",lbdata),xloc,yloc,lbres)
         else
            if ( max(lbdata) .le. 100. ) then
               gsn_labelbar_ndc (wks,num_bins,sprintf("%4.1f",lbdata),xloc,yloc,lbres)
            else
               gsn_labelbar_ndc (wks,num_bins,""+tointeger(lbdata),xloc,yloc,lbres)
            end if
         end if
         pnres = True
         pnres@gsnPanelBottom = 0.03              ; add some space at bottom 
         pnres@txString = date+"    "+otype+"   "+info
         gsn_panel(wks,valplot,(/2,2/),pnres)
         delete(valplot)
         delete(obs)
         delete(bak)
         delete(omb)
         delete(ana)
         delete(lbdata)
      end if
      if ( num_rf_all.gt.0 .and. num_rf_qc.gt.0 ) then
         valplot = new(4,graphic)
         obs = mask(prf_qc(:,0),prf_qc(:,2).ge.0,True)
         bak = mask(prf_qc(:,0)-prf_qc(:,1),prf_qc(:,2).ge.0,True)
         omb = mask(prf_qc(:,1),prf_qc(:,2).ge.0,True)
         ana = mask(prf_qc(:,0)-prf_qc(:,4),prf_qc(:,2).ge.0,True)
         xmax = max((/max(obs),max(bak),max(omb),max(ana)/))
         xmin = min((/min(obs),min(bak),min(omb),min(ana)/))
         range = xmax - xmin
         binwidth = range/tofloat(num_bins)
         lbdata = new(num_bins+1,float)
         do i = 0, num_bins
            lbdata(i) = xmin + i * binwidth
         end do
         mpres@tiMainString = "OBS RF"
         valplot(0) = do_valueplot(wks,obs,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "BAK RF"
         valplot(1) = do_valueplot(wks,bak,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "OMB RF"
         valplot(2) = do_valueplot(wks,omb,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         mpres@tiMainString = "ANA RF"
         valplot(3) = do_valueplot(wks,ana,lat,lon,lbdata,binwidth,colors,num_bins,subdomain,mpres) 
         lbres = True
         lbres@lbPerimOn            = False               ; no label bar box
         lbres@lbOrientation        = "Horizontal"        ; orientation
         lbres@vpWidthF             = 0.9                ; size
         lbres@vpHeightF            = 0.06
         lbres@lbLabelFontHeightF   = 0.01                ; label font height default 0.02
         lbres@lbLabelAlignment     = "InteriorEdges"     ; where to label
         lbres@lbLabelAutoStride    = True                ; Auto stride
         lbres@lbMonoFillPattern    = True                ; fill sold
         lbres@lbFillColors         = labelbarcolors            ; must be RGB triplets
         lbres@lbBoxLinesOn         = False
         xloc = 0.05
         yloc = 0.05
         if ( max(lbdata) .le. 10. ) then
            gsn_labelbar_ndc (wks,num_bins,sprintf("%5.2f",lbdata),xloc,yloc,lbres)
         else
            if ( max(lbdata) .le. 100. ) then
               gsn_labelbar_ndc (wks,num_bins,sprintf("%4.1f",lbdata),xloc,yloc,lbres)
            else
               gsn_labelbar_ndc (wks,num_bins,""+tointeger(lbdata),xloc,yloc,lbres)
            end if
         end if
         pnres = True
         pnres@gsnPanelBottom = 0.03              ; add some space at bottom 
         pnres@txString = date+"    "+otype+"   "+info
         gsn_panel(wks,valplot,(/2,2/),pnres)
         delete(valplot)
         delete(obs)
         delete(bak)
         delete(omb)
         delete(ana)
         delete(lbdata)
      end if
      ;
      ; scatter plots
      ;
      ; OBS and BAK(Background) scatter plots
      ;
      if ( num_rv_all.gt.1 .and. num_rv_qc.gt.1 ) then
         ombplot = new(4,graphic)
         data1 = prv(:,0)               ; OBS
         data2 = prv(:,0)-prv(:,1)       ; BAK
         ombplot(0) = do_scatterplot(wks, data1, data2, "OBS RV", "BAK RV",  \
                         "All  "+num(.not.ismissing(data2)),                 \
                         markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(prv_qc(:,0),prv_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(prv_qc(:,0)-prv_qc(:,1),prv_qc(:,2).ge.0,True)    ; BAK
         ombplot(1) = do_scatterplot(wks, data1, data2, "OBS RV", "BAK RV",  \
                         "QCed  "+num(.not.ismissing(data2)),                \
                         markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
      end if

      if ( num_rf_all.gt.1 .and. num_rf_qc.gt.1 ) then
         if ( .not.isvar("ombplot") ) then
            ombplot = new(4,graphic)
         end if
         data1 = prf(:,0)               ; OBS
         data2 = prf(:,0)-prf(:,1)       ; BAK
         ombplot(2) = do_scatterplot(wks, data1, data2, "OBS RF", "BAK RF",  \
                         "All  "+num(.not.ismissing(data2)),                 \
                         markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(prf_qc(:,0),prf_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(prf_qc(:,0)-prf_qc(:,1),prf_qc(:,2).ge.0,True)    ; BAK
         ombplot(3) = do_scatterplot(wks, data1, data2, "OBS RF", "BAK RF",  \
                         "QCed  "+num(.not.ismissing(data2)),                \
                         markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
      end if

      if ( isvar("ombplot") ) then
         ; pnres@txString = date+"    "+otype+"   "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"   "+info
         gsn_panel(wks,ombplot,(/2,2/),pnres)
      end if
      ;
      ; OBS and ANA(Analysis) scatter plot
      ;
      if ( num_rv_all.gt.1 .and. num_rv_qc.gt.1 ) then
         omaplot = new(4,graphic)
         omaplot(0) = ombplot(1)
         data1 = mask(prv_qc(:,0),prv_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(prv_qc(:,0)-prv_qc(:,4),prv_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
         omaplot(1) =  do_scatterplot(wks, data1, data2, "OBS RV", "ANA RV",  \
                          "QCed  "+num(.not.ismissing(data2)),                \
                          markertype1, markersize1, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
      end if

      if ( num_rf_all.gt.1 .and. num_rf_qc.gt.1 ) then
         if ( .not.isvar("omaplot") ) then
            omaplot = new(4,graphic)
         end if
         omaplot(2) = ombplot(3)
         data1 = mask(prf_qc(:,0),prf_qc(:,2).ge.0,True)               ; OBS
         data2 = mask(prf_qc(:,0)-prf_qc(:,4),prf_qc(:,2).ge.0,True)    ; ANA = OBS - OMA
         omaplot(3) =  do_scatterplot(wks, data1, data2, "OBS RF", "ANA RF",  \
                          "QCed  "+num(.not.ismissing(data2)),                \
                          markertype1, markersize1, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
      end if

      if ( isvar("omaplot") ) then
         ; pnres@txString = date+"    "+otype+"   "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"   "+info
         gsn_panel(wks,omaplot,(/2,2/),pnres)
         delete(ombplot)
         delete(omaplot)
      end if

   end if  ; end of if otype = RADAR and num_qc > 0

   if ( otype .eq. "GPSREF" .and. num_qc .gt. 0 ) then
      ;
      ; plot integrated distribution
      ;
      ; title = date+"    "+otype+"   "+info+"  "+num_qc
      title = otype+"   "+info
      str1  = date
      str2  = ""+num_qc
      mpres = setmpres(title,str1,str2)
      mpres@gsnMaximize             = True                ; Maximize plot in frame.
      if ( subdomain ) then
         map = gsn_csm_map_ce(wks,mpres)
      else
         map = gsn_csm_map(wks,mpres)
      end if
      plon = mask(lon(:),pref(:,2).ge.0,True)
      plat = mask(lat(:),pref(:,2).ge.0,True)
      gsres               = True
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,plon(:),plat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
      delete(plon)
      delete(plat)
      delete(map)

      ; distribution plot

      plot = new(4,graphic)
      dummy = new(4,graphic)
      ;
      title = otype+" REF (All: "+num_ref_all+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
         plot(0) = gsn_csm_map_ce(wks,mpres)
      else
         plot(0) = gsn_csm_map(wks,mpres)
      end if
      gsres               = True
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "blue"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      plon = mask(lon(:),.not.ismissing(pref(:,0)),True)
      plat = mask(lat(:),.not.ismissing(pref(:,0)),True)
      dummy(0) = gsn_add_polymarker(wks,plot(0),plon(:),plat(:),gsres)
      delete(plon)
      delete(plat)

      title = otype+" REF (QCed: "+num_ref_qc+")"
      mpres = setmpres(title,"","")
      if ( subdomain ) then
        plot(1) = gsn_csm_map_ce(wks,mpres)
      else
        plot(1) = gsn_csm_map(wks,mpres)
      end if
      plon = mask(lon(:),pref(:,2).ge.0,True)
      plat = mask(lat(:),pref(:,2).ge.0,True)
      gsres@gsMarkerColor  = "red"
      dummy(1) = gsn_add_polymarker(wks,plot(1),plon,plat,gsres)
      delete(plon)
      delete(plat)

      pnres = True
      ; pnres@gsnPanelBottom = 0.08              ; add some space at bottom 
      ; pnres@txString = date+"    "+otype+"   "+nobs+" / "+num_qc
      pnres@txString = date+"    "+otype+"   "+info
      gsn_panel(wks,plot,(/2,2/),pnres)
      delete(plot)
      delete(dummy)
      ;
      ; scatter plots
      ;
      if ( num_ref_all.gt.1 .and. num_ref_qc.gt.1 ) then
         plot = new(4,graphic)
         data1 = pref(:,0)               ; OBS
         data2 = pref(:,0)-pref(:,1)      ; BAK
         plot(0) = do_scatterplot(wks, data1, data2, "OBS REF", "BAK REF",  \
                      "All  "+num(.not.ismissing(data2)),                   \
                      markertype1, markersize2, markercolor1, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pref(:,0),pref(:,2).ge.0,True)               ; OBS
         data2 = mask(pref(:,0)-pref(:,1),pref(:,2).ge.0,True)      ; BAK
         plot(1) = do_scatterplot(wks, data1, data2, "OBS REF", "BAK REF",  \
                      "QCed  "+num(.not.ismissing(data2)),                  \
                      markertype1, markersize2, markercolor2, linesize, linecolor)
         delete(data1)
         delete(data2)
         data1 = mask(pref(:,0),pref(:,2).ge.0,True)               ; OBS
         data2 = mask(pref(:,0)-pref(:,4),pref(:,2).ge.0,True)      ; ANA
         plot(3) = do_scatterplot(wks, data1, data2, "OBS REF", "ANA REF",  \
                      "QCed  "+num(.not.ismissing(data2)),                  \
                      markertype1, markersize2, markercolor3, linesize, linecolor)
         delete(data1)
         delete(data2)
         pnres = True
         ; pnres@gsnPanelBottom = 0.08              ; add some space at bottom 
         ; pnres@txString = date+"    "+otype+"   "+nobs+" / "+num_qc
         pnres@txString = date+"    "+otype+"   "+info
         gsn_panel(wks,plot,(/2,2/),pnres)
         delete(plot)
      end if

   end if  ; end of if otype = GPSREF and num_qc > 0

end  ; end of procedure plot_upr_h

function read_info(ndata:integer, idata:integer, cdata:character, nlevel:integer, ilevel:integer, type:string)
local info, ibeg, iend, n, is, ie
begin ; begin of function read_info
   if ( type .eq. "id" ) then
      ibeg = 16
      iend = 20
   end if
   if ( type .eq. "lat" ) then
      ibeg = 21
      iend = 29
   end if
   if ( type .eq. "lon" ) then
      ibeg = 30
      iend = 38
   end if
   if ( type .eq. "pre" .or. type .eq. "hgt" ) then
      ibeg = 39
      iend = 55
   end if
   if ( type .eq. "id" ) then
      info = new(sum(nlevel),string)
   else
      info = new(sum(nlevel),float)
   end if
   do n = 0, ndata-1
      if ( nlevel(n) .gt. 0 ) then
         if ( n .eq. 0 ) then
            is = 0
            ie = nlevel(n) - 1
         else
            is = ie + 1
            ie = is + nlevel(n) - 1
         end if
         if ( type .eq. "id" ) then
            info(is:ie) = charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg:iend))
         else
            info(is:ie) = stringtofloat(charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg:iend)))
         end if
      end if
   end do
   return(info)
end ; end of function read_info

function read_data(ndata:integer, idata:integer, cdata:character, nlevel:integer, ilevel:integer, \
                   otype:string, field:string)
local data, k, ibeg, iend, n, is, ie
begin ; begin of function read_data
   data = new((/sum(nlevel),5/),float)
   data@_FillValue = -888888
   if ( otype .eq. "synop" .or. otype .eq. "metar" .or. otype .eq. "ships" .or. \
        otype .eq. "buoy"  .or. otype .eq. "sonde_sfc" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
      if ( field .eq. "p" )  then
         ibeg = (/284,301,318,326,343/)
         iend = (/300,317,325,342,359/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/360,377,394,402,419/)
         iend = (/376,393,401,418,435/)
      end if
   end if
   if ( otype .eq. "geoamv" .or. otype .eq. "polaramv" .or.   \
        otype .eq. "pilot"  .or. otype .eq. "profiler" .or.   \
        otype .eq. "qscat"  .or. otype .eq. "bogus" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if
   if ( otype .eq. "gpspw" ) then
      if ( field .eq. "tpw" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "ssmir" ) then
      if ( field .eq. "spd" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "gpsref" ) then
      if ( field .eq. "ref" ) then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
   end if
   if ( otype .eq. "airsr" ) then
      if ( field .eq. "t" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if
   if ( otype .eq. "sound" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
      if ( field .eq. "q" )  then
         ibeg = (/284,301,318,326,343/)
         iend = (/300,317,325,342,359/)
      end if
   end if
   if ( otype .eq. "airep" ) then
      if ( field .eq. "u" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "v" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
      if ( field .eq. "t" )  then
         ibeg = (/208,225,242,250,267/)
         iend = (/224,241,249,266,283/)
      end if
   end if
   if ( otype .eq. "radar" ) then
      if ( field .eq. "rv" )  then
         ibeg = (/56,73,90,98,115/)
         iend = (/72,89,97,114,131/)
      end if
      if ( field .eq. "rf" )  then
         ibeg = (/132,149,166,174,191/)
         iend = (/148,165,173,190,207/)
      end if
   end if

   do n = 0, ndata-1
      if ( nlevel(n) .gt. 0 ) then
         if ( n .eq. 0 ) then
            is = 0
            ie = nlevel(n) - 1
         else
            is = ie + 1
            ie = is + nlevel(n) - 1
         end if
         do k = 0, 4   ; obs, inv, qc, err, inc
            data(is:ie,k) = stringtofloat(charactertostring(cdata(ilevel(n)+1:ilevel(n)+nlevel(n),ibeg(k):iend(k))))
         end do
      end if
   end do
   return(data)
end ; end of function read_data

function read_bogus_slp(ndata:integer, idata:integer, cdata:character, nlevel:integer, ilevel:integer, \
                   otype:string, field:string)
local data, k, ibeg, iend, n, is, ie
begin ; begin of function read_bogus_slp
   data = new((/ndata,5/),float)
   data@_FillValue = -888888
   if ( otype.eq."bogus" .and. field.eq."slp" ) then
      ibeg = (/56,73,90,98,115/)
      iend = (/72,89,97,114,131/)
      do n = 0, ndata-1
         do k = 0, 4   ; obs, inv, qc, err, inc
            data(n,k) = stringtofloat(charactertostring(cdata(ilevel(n)-1,ibeg(k):iend(k))))
         end do
      end do
   end if
   return(data)
end ; end of function read_bogus_slp

undef("change_date")
function change_date(ccyy:integer, mm:integer, dd:integer, delta:integer)
local mmday, newday
begin
   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   if ( isleapyear(ccyy) ) then
      mmday(1) = 29
   end if
   dd = dd + delta
   if ( dd .eq. 0 ) then
      mm = mm - 1
      if ( mm .eq. 0 ) then
         mm = 12
         ccyy = ccyy - 1
      end if
      dd = mmday(mm-1)
   else
      if ( dd .gt. mmday(mm-1) ) then
         dd = 1
         mm = mm + 1
         if ( mm .gt. 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if
   end if
   newday = ccyy*10000 + mm*100 + dd
   ;   newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   return(newday)
end 

undef("advance_cymdh")
function advance_cymdh(currentdatestr:string, dh:integer)
local ccyy, mm, dd, hh, newday, newdatestr
begin
   currentdate = stringtochar(currentdatestr)
   ccyy = stringtointeger((/currentdate(0:3)/))
   mm   = stringtointeger((/currentdate(4:5)/))
   dd   = stringtointeger((/currentdate(6:7)/))
   hh   = stringtointeger((/currentdate(8:9)/))
   hh = hh + dh
   newday = ccyy*10000 + mm*100 + dd
   ;   newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   do while (hh .lt. 0)
      hh = hh + 24
      newday = change_date( ccyy, mm, dd, -1 )
   end do
   do while (hh .gt. 23)
      hh = hh - 24
      newday = change_date( ccyy, mm, dd, 1 )
   end do
   ;   newdate = newday*100 + hh
   newdatestr = sprinti("%0.8i",newday) + sprinti("%0.2i",hh)
   return(newdatestr)
end
;---------------------------------------------------------------------------
; main script
;---------------------------------------------------------------------------
begin;  begin of the main script

   ; find out how many dates to process

   ndate = 0   ; number of dates
   valid_date = start_date
   do while ( valid_date .le. end_date )
      ndate = ndate + 1
      valid_date = advance_cymdh(valid_date,cycle_period)
   end do

   date = start_date

   do while ( date .le. end_date )  ; big date loop

      if ( plot_radar ) then
         ;
         ; first find out the total number of radar obs
         ; and collect all oma_radar.nnn into one file
         ;
         nradar = 0
         radar_files = systemfunc("cd "+datdir1+date+datdir2+";"+"\ls "+prefix_radar+".*")
         if ( num(.not.ismissing(radar_files)) .eq. 0 ) then
            print("Can not find "+prefix_radar+".* files  Will skip them")
         else
            system("\rm -f "+plotdir+"tmp.oma_radar")
            print("Collecting oma_radar files, this may take a while...")
            do i = 0, dimsizes(radar_files)-1
               fullname = datdir1+date+datdir2+radar_files(i)
               if ( stringtointeger(systemfunc("wc -l "+fullname)) .gt. 0 ) then
                  system("touch "+plotdir+"tmp.oma_radar")
                  nradar = nradar + stringtointeger(systemfunc("head -1 "+fullname+" | cut -c21-28"))
                  data  = asciiread(fullname, -1, "string")
                  nline = dimsizes(data)-1
                  system("tail -"+nline+" "+fullname+" >> "+plotdir+"tmp.oma_radar")
                  delete(data)
               end if
            end do
            if ( nradar .gt. 0 ) then
               ;
               ; read data
               ;
               data  = asciiread(plotdir+"tmp.oma_radar", -1, "string")
               cdata = stringtochar(data)
               nlevel_radar = new(nradar,integer)
               ilevel_radar = new(nradar,integer)
               iradar = 0
               ilevel_radar(0) = 0
               nlevel_radar(0) = stringtointeger(charactertostring(cdata(ilevel_radar(0),0:7)))
               do k = 1, nradar-1
                  ilevel_radar(k) = ilevel_radar(k-1)+nlevel_radar(k-1)+1
                  nlevel_radar(k) = stringtointeger(charactertostring(cdata(ilevel_radar(k),0:7)))
               end do
               radar_id  = read_info(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "id")
               radar_lat = read_info(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "lat")
               radar_lon = read_info(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "lon")
               radar_hgt = read_info(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "hgt")
               radar_rv  = read_data(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "radar", "rv")
               radar_rf  = read_data(nradar, iradar, cdata, nlevel_radar, ilevel_radar, "radar", "rf")
               ;
               ; plot
               ;
               dum = radar_rv
               wks = gsn_open_wks(out_type,plotdir+"radar_"+date)
               gsn_define_colormap(wks,colormap)

               if ( rank_hlevs .eq. 1 ) then
                  do k = 0, nhlevs(0)-1
                     plot_upr_h("RADAR",wks,nradar,date,hlevs(k),hlevs(k),radar_hgt,radar_lat,radar_lon,radar_rv,radar_rf,dum)
                  end do
               end if
               if ( rank_hlevs .eq. 2 ) then
                  do k = 0, nhlevs(0)-1
                     if ( .not.ismissing(hlevs(k,0)) .and. .not.ismissing(hlevs(k,1)) ) then
                        plot_upr_h("RADAR",wks,nradar,date,hlevs(k,0),hlevs(k,1),radar_hgt,radar_lat,radar_lon,radar_rv,radar_rf,dum)
                     end if
                  end do
               end if

               delete(dum)
               destroy(wks)
               delete(colors)
               delete(labelbarcolors)
               delete(ilevel_radar)
               delete(nlevel_radar)
               system("\rm -f "+plotdir+"tmp.oma_radar")
               delete(data)
               delete(cdata)
            end if   ; end of if nradar > 0
         end if   ; end of if found oma_radar* files
      end if   ; end of if plot_radar

      if ( plot_gts ) then
         fullname = datdir1+date+datdir2+filename_gts
         if ( .not. isfilepresent(fullname) ) then
            print("Can not find the file "+fullname+"  Will skip it")
         else
            if ( stringtointeger(systemfunc("wc -l "+fullname)) .eq. 0 ) then
               print("no data found in "+fullname+"  Will skip it")
            else
               print("Reading data from "+fullname)
               print("This may take a while......")
               data  = asciiread(fullname, -1, "string")    ; -1 means read all rows.
               cdata = stringtochar(data)
               nline = dimsizes(cdata(:,0))

               nsynop = 0
               nmetar = 0
               nships = 0
               nbuoy  = 0
               nsonde_sfc = 0
               ngeoamv = 0
               npolaramv = 0
               ngpspw = 0
               nsound = 0
               nairep = 0
               npilot = 0
               nprofiler = 0
               nssmir = 0
               nqscat = 0
               nbogus = 0
               nairsr = 0
               ngpsref = 0
               nssmir = 0
               do n = 0, nline-1    ; loop of parsing the file line by line
                  if ( cdata(n,0:19) .eq. "               synop" ) then
                     nsynop = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     isynop = n                                                  ; line index
                     nlevel_synop = new(nsynop,integer)
                     ilevel_synop = new(nsynop,integer)
                     ilevel_synop(0) = isynop+1
                     nlevel_synop(0) = stringtointeger(charactertostring(cdata(ilevel_synop(0),0:7)))
                     do k = 1, nsynop-1
                        ilevel_synop(k) = ilevel_synop(k-1)+nlevel_synop(k-1)+1
                        nlevel_synop(k) = stringtointeger(charactertostring(cdata(ilevel_synop(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               metar" ) then
                     nmetar = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     imetar = n                                                  ; line index
                     nlevel_metar = new(nmetar,integer)
                     ilevel_metar = new(nmetar,integer)
                     ilevel_metar(0) = imetar+1
                     nlevel_metar(0) = stringtointeger(charactertostring(cdata(ilevel_metar(0),0:7)))
                     do k = 1, nmetar-1
                        ilevel_metar(k) = ilevel_metar(k-1)+nlevel_metar(k-1)+1
                        nlevel_metar(k) = stringtointeger(charactertostring(cdata(ilevel_metar(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               ships" ) then
                     nships = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     iships = n                                                  ; line index
                     nlevel_ships = new(nships,integer)
                     ilevel_ships = new(nships,integer)
                     ilevel_ships(0) = iships+1
                     nlevel_ships(0) = stringtointeger(charactertostring(cdata(ilevel_ships(0),0:7)))
                     do k = 1, nships-1
                        ilevel_ships(k) = ilevel_ships(k-1)+nlevel_ships(k-1)+1
                        nlevel_ships(k) = stringtointeger(charactertostring(cdata(ilevel_ships(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "                buoy" ) then
                     nbuoy = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     ibuoy = n                                                  ; line index
                     nlevel_buoy = new(nbuoy,integer)
                     ilevel_buoy = new(nbuoy,integer)
                     ilevel_buoy(0) = ibuoy+1
                     nlevel_buoy(0) = stringtointeger(charactertostring(cdata(ilevel_buoy(0),0:7)))
                     do k = 1, nbuoy-1
                        ilevel_buoy(k) = ilevel_buoy(k-1)+nlevel_buoy(k-1)+1
                        nlevel_buoy(k) = stringtointeger(charactertostring(cdata(ilevel_buoy(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "           sonde_sfc" ) then
                     nsonde_sfc = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     isonde_sfc = n                                                  ; line index
                     nlevel_sonde_sfc = new(nsonde_sfc,integer)
                     ilevel_sonde_sfc = new(nsonde_sfc,integer)
                     ilevel_sonde_sfc(0) = isonde_sfc+1
                     nlevel_sonde_sfc(0) = stringtointeger(charactertostring(cdata(ilevel_sonde_sfc(0),0:7)))
                     do k = 1, nsonde_sfc-1
                        ilevel_sonde_sfc(k) = ilevel_sonde_sfc(k-1)+nlevel_sonde_sfc(k-1)+1
                        nlevel_sonde_sfc(k) = stringtointeger(charactertostring(cdata(ilevel_sonde_sfc(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "              geoamv" ) then
                     ngeoamv = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     igeoamv = n                                                  ; line index
                     nlevel_geoamv = new(ngeoamv,integer)  ; number of levels of each obs
                     ilevel_geoamv = new(ngeoamv,integer)  ; starting line index of each obs
                     ilevel_geoamv(0) = igeoamv+1
                     nlevel_geoamv(0) = stringtointeger(charactertostring(cdata(ilevel_geoamv(0),0:7)))
                     do k = 1, ngeoamv-1
                        ilevel_geoamv(k) = ilevel_geoamv(k-1)+nlevel_geoamv(k-1)+1
                        nlevel_geoamv(k) = stringtointeger(charactertostring(cdata(ilevel_geoamv(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "            polaramv" ) then
                     npolaramv = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     ipolaramv = n                                                  ; line index
                     nlevel_polaramv = new(npolaramv,integer)
                     ilevel_polaramv = new(npolaramv,integer)
                     ilevel_polaramv(0) = ipolaramv+1
                     nlevel_polaramv(0) = stringtointeger(charactertostring(cdata(ilevel_polaramv(0),0:7)))
                     do k = 1, npolaramv-1
                        ilevel_polaramv(k) = ilevel_polaramv(k-1)+nlevel_polaramv(k-1)+1
                        nlevel_polaramv(k) = stringtointeger(charactertostring(cdata(ilevel_polaramv(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               gpspw" ) then
                     ngpspw = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     igpspw = n                                                  ; line index
                     nlevel_gpspw = new(ngpspw,integer)
                     ilevel_gpspw = new(ngpspw,integer)
                     ilevel_gpspw(0) = igpspw+1
                     nlevel_gpspw(0) = stringtointeger(charactertostring(cdata(ilevel_gpspw(0),0:7)))
                     do k = 1, ngpspw-1
                        ilevel_gpspw(k) = ilevel_gpspw(k-1)+nlevel_gpspw(k-1)+1
                        nlevel_gpspw(k) = stringtointeger(charactertostring(cdata(ilevel_gpspw(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               sound" ) then
                     nsound = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     isound = n                                                  ; line index
                     nlevel_sound = new(nsound,integer)
                     ilevel_sound = new(nsound,integer)
                     ilevel_sound(0) = isound+1
                     nlevel_sound(0) = stringtointeger(charactertostring(cdata(ilevel_sound(0),0:7)))
                     do k = 1, nsound-1
                        ilevel_sound(k) = ilevel_sound(k-1)+nlevel_sound(k-1)+1
                        nlevel_sound(k) = stringtointeger(charactertostring(cdata(ilevel_sound(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               airep" ) then
                     nairep = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     iairep = n                                                  ; line index
                     nlevel_airep = new(nairep,integer)
                     ilevel_airep = new(nairep,integer)
                     ilevel_airep(0) = iairep+1
                     nlevel_airep(0) = stringtointeger(charactertostring(cdata(ilevel_airep(0),0:7)))
                     do k = 1, nairep-1
                        ilevel_airep(k) = ilevel_airep(k-1)+nlevel_airep(k-1)+1
                        nlevel_airep(k) = stringtointeger(charactertostring(cdata(ilevel_airep(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               pilot" ) then
                     npilot = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     ipilot = n                                                  ; line index
                     nlevel_pilot = new(npilot,integer)
                     ilevel_pilot = new(npilot,integer)
                     ilevel_pilot(0) = ipilot+1
                     nlevel_pilot(0) = stringtointeger(charactertostring(cdata(ilevel_pilot(0),0:7)))
                     do k = 1, npilot-1
                        ilevel_pilot(k) = ilevel_pilot(k-1)+nlevel_pilot(k-1)+1
                        nlevel_pilot(k) = stringtointeger(charactertostring(cdata(ilevel_pilot(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "            profiler" ) then
                     nprofiler = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     iprofiler = n                                                  ; line index
                     nlevel_profiler = new(nprofiler,integer)
                     ilevel_profiler = new(nprofiler,integer)
                     ilevel_profiler(0) = iprofiler+1
                     nlevel_profiler(0) = stringtointeger(charactertostring(cdata(ilevel_profiler(0),0:7)))
                     do k = 1, nprofiler-1
                        ilevel_profiler(k) = ilevel_profiler(k-1)+nlevel_profiler(k-1)+1
                        nlevel_profiler(k) = stringtointeger(charactertostring(cdata(ilevel_profiler(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               ssmir" ) then
                     nssmir = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     issmir = n                                                  ; line index
                     nlevel_ssmir = new(nssmir,integer)
                     ilevel_ssmir = new(nssmir,integer)
                     ilevel_ssmir(0) = issmir+1
                     nlevel_ssmir(0) = stringtointeger(charactertostring(cdata(ilevel_ssmir(0),0:7)))
                     do k = 1, nssmir-1
                        ilevel_ssmir(k) = ilevel_ssmir(k-1)+nlevel_ssmir(k-1)+1
                        nlevel_ssmir(k) = stringtointeger(charactertostring(cdata(ilevel_ssmir(k),0:7)))
                     end do
                  end if
                  ; if ( cdata(n,0:19) .eq. "               ssmiT" ) then
                  ;   nssmiT = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                  ;   issmiT = n                                                  ; line index
                  ; end if
                  ; if ( cdata(n,0:19) .eq. "               satem" ) then
                  ;   nsatem = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                  ;   isatem = n                                                  ; line index
                  ; end if
                  ; if ( cdata(n,0:19) .eq. "               ssmt1" ) then
                  ;   nssmt1 = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                  ;   issmt1 = n                                                  ; line index
                  ; end if
                  ; if ( cdata(n,0:19) .eq. "               ssmt2" ) then
                  ;   nssmt2 = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                  ;   issmt2 = n                                                  ; line index
                  ; end if
                  if ( cdata(n,0:19) .eq. "               qscat" ) then
                     nqscat = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     iqscat = n                                                  ; line index
                     nlevel_qscat = new(nqscat,integer)
                     ilevel_qscat = new(nqscat,integer)
                     ilevel_qscat(0) = iqscat+1
                     nlevel_qscat(0) = stringtointeger(charactertostring(cdata(ilevel_qscat(0),0:7)))
                     do k = 1, nqscat-1
                        ilevel_qscat(k) = ilevel_qscat(k-1)+nlevel_qscat(k-1)+1
                        nlevel_qscat(k) = stringtointeger(charactertostring(cdata(ilevel_qscat(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               bogus" ) then
                     nbogus = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     ibogus = n                                                  ; line index
                     nlevel_bogus = new(nbogus,integer)
                     ilevel_bogus = new(nbogus,integer)
                     ilevel_bogus(0) = ibogus+3          ; one bogus has two parts: slp and upper-level
                     nlevel_bogus(0) = stringtointeger(charactertostring(cdata(ilevel_bogus(0),0:7)))
                     do k = 1, nbogus-1
                        ilevel_bogus(k) = ilevel_bogus(k-1)+nlevel_bogus(k-1)+3
                        nlevel_bogus(k) = stringtointeger(charactertostring(cdata(ilevel_bogus(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "               airsr" ) then
                     nairsr = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     iairsr = n                                                  ; line index
                     nlevel_airsr = new(nairsr,integer)
                     ilevel_airsr = new(nairsr,integer)
                     ilevel_airsr(0) = iairsr+1
                     nlevel_airsr(0) = stringtointeger(charactertostring(cdata(ilevel_airsr(0),0:7)))
                     do k = 1, nairsr-1
                        ilevel_airsr(k) = ilevel_airsr(k-1)+nlevel_airsr(k-1)+1
                        nlevel_airsr(k) = stringtointeger(charactertostring(cdata(ilevel_airsr(k),0:7)))
                     end do
                  end if
                  if ( cdata(n,0:19) .eq. "              gpsref" ) then
                     ngpsref = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
                     igpsref = n                                                  ; line index
                     nlevel_gpsref = new(ngpsref,integer)
                     ilevel_gpsref = new(ngpsref,integer)
                     ilevel_gpsref(0) = igpsref+1
                     nlevel_gpsref(0) = stringtointeger(charactertostring(cdata(ilevel_gpsref(0),0:7)))
                     do k = 1, ngpsref-1
                        ilevel_gpsref(k) = ilevel_gpsref(k-1)+nlevel_gpsref(k-1)+1
                        nlevel_gpsref(k) = stringtointeger(charactertostring(cdata(ilevel_gpsref(k),0:7)))
                     end do
                  end if
               end do  ; end loop of parsing the file line by line

               if ( nsynop .gt. 0 ) then
                  synop_id  = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "id")
                  synop_lat = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "lat")
                  synop_lon = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "lon")
                  synop_pre = read_info(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "pre")
                  synop_u   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "u")
                  synop_v   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "v")
                  synop_t   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "t")
                  synop_p   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "p")
                  synop_q   = read_data(nsynop, isynop, cdata, nlevel_synop, ilevel_synop, "synop", "q")
                  wks = gsn_open_wks(out_type,plotdir+"synop_"+date)
                  dum = synop_u
                  plot_sfc("SYNOP",wks,nsynop,date,synop_lat,synop_lon,synop_u,synop_v,synop_t,synop_p,synop_q,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_synop)
                  delete(nlevel_synop)
                  delete(synop_id)
                  delete(synop_lat)
                  delete(synop_lon)
                  delete(synop_pre)
                  delete(synop_u)
                  delete(synop_v)
                  delete(synop_t)
                  delete(synop_p)
                  delete(synop_q)
               end if  ; end of nsynop check
;
               if ( nmetar .gt. 0 ) then
                  metar_id  = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "id")
                  metar_lat = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "lat")
                  metar_lon = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "lon")
                  metar_pre = read_info(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "pre")
                  metar_u   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "u")
                  metar_v   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "v")
                  metar_t   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "t")
                  metar_p   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "p")
                  metar_q   = read_data(nmetar, imetar, cdata, nlevel_metar, ilevel_metar, "metar", "q")
                  wks = gsn_open_wks(out_type,plotdir+"metar_"+date)
                  dum = metar_u
                  plot_sfc("METAR",wks,nmetar,date,metar_lat,metar_lon,metar_u,metar_v,metar_t,metar_p,metar_q,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_metar)
                  delete(nlevel_metar)
                  delete(metar_id)
                  delete(metar_lat)
                  delete(metar_lon)
                  delete(metar_pre)
                  delete(metar_u)
                  delete(metar_v)
                  delete(metar_t)
                  delete(metar_p)
                  delete(metar_q)
               end if   ; end of nmetar check

               if ( nships .gt. 0 ) then
                  ships_id  = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "id")
                  ships_lat = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "lat")
                  ships_lon = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "lon")
                  ships_pre = read_info(nships, iships, cdata, nlevel_ships, ilevel_ships, "pre")
                  ships_u   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "u")
                  ships_v   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "v")
                  ships_t   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "t")
                  ships_p   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "p")
                  ships_q   = read_data(nships, iships, cdata, nlevel_ships, ilevel_ships, "ships", "q")
                  wks = gsn_open_wks(out_type,plotdir+"ships_"+date)
                  dum = ships_u
                  plot_sfc("SHIPS",wks,nships,date,ships_lat,ships_lon,ships_u,ships_v,ships_t,ships_p,ships_q,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_ships)
                  delete(nlevel_ships)
                  delete(ships_id)
                  delete(ships_lat)
                  delete(ships_lon)
                  delete(ships_pre)
                  delete(ships_u)
                  delete(ships_v)
                  delete(ships_t)
                  delete(ships_p)
                  delete(ships_q)
               end if  ; end of nships check

               if ( nbuoy .gt. 0 ) then
                  buoy_id  = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "id")
                  buoy_lat = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "lat")
                  buoy_lon = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "lon")
                  buoy_pre = read_info(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "pre")
                  buoy_u   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "u")
                  buoy_v   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "v")
                  buoy_t   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "t")
                  buoy_p   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "p")
                  buoy_q   = read_data(nbuoy, ibuoy, cdata, nlevel_buoy, ilevel_buoy, "buoy", "q")
                  wks = gsn_open_wks(out_type,plotdir+"buoy_"+date)
                  dum = buoy_u
                  plot_sfc("BUOY",wks,nbuoy,date,buoy_lat,buoy_lon,buoy_u,buoy_v,buoy_t,buoy_p,buoy_q,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_buoy)
                  delete(nlevel_buoy)
                  delete(buoy_id)
                  delete(buoy_lat)
                  delete(buoy_lon)
                  delete(buoy_pre)
                  delete(buoy_u)
                  delete(buoy_v)
                  delete(buoy_t)
                  delete(buoy_p)
                  delete(buoy_q)
               end if   ; end of nbuoy check

               if ( nsonde_sfc .gt. 0 ) then
                  sonde_sfc_id  = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "id")
                  sonde_sfc_lat = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "lat")
                  sonde_sfc_lon = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "lon")
                  sonde_sfc_pre = read_info(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "pre")
                  sonde_sfc_u   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "u")
                  sonde_sfc_v   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "v")
                  sonde_sfc_t   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "t")
                  sonde_sfc_p   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "p")
                  sonde_sfc_q   = read_data(nsonde_sfc, isonde_sfc, cdata, nlevel_sonde_sfc, ilevel_sonde_sfc, "sonde_sfc", "q")
                  wks = gsn_open_wks(out_type,plotdir+"sonde_sfc_"+date)
                  dum = sonde_sfc_u
                  plot_sfc("SONDE_SFC",wks,nsonde_sfc,date,sonde_sfc_lat,sonde_sfc_lon,\
                                       sonde_sfc_u,sonde_sfc_v,sonde_sfc_t,sonde_sfc_p,sonde_sfc_q,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_sonde_sfc)
                  delete(nlevel_sonde_sfc)
                  delete(sonde_sfc_id)
                  delete(sonde_sfc_lat)
                  delete(sonde_sfc_lon)
                  delete(sonde_sfc_pre)
                  delete(sonde_sfc_u)
                  delete(sonde_sfc_v)
                  delete(sonde_sfc_t)
                  delete(sonde_sfc_p)
                  delete(sonde_sfc_q)
               end if   ; end of nsonde_sfc check

               if ( ngeoamv .gt. 0 ) then
                  geoamv_id  = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "id")
                  geoamv_lat = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "lat")
                  geoamv_lon = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "lon")
                  geoamv_pre = read_info(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "pre")
                  geoamv_u   = read_data(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "geoamv", "u")
                  geoamv_v   = read_data(ngeoamv, igeoamv, cdata, nlevel_geoamv, ilevel_geoamv, "geoamv", "v")
                  wks = gsn_open_wks(out_type,plotdir+"geoamv_"+date)
                  dum = geoamv_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("GEOAMV",wks,ngeoamv,date,plevs(k),plevs(k),geoamv_pre,geoamv_lat,geoamv_lon,\
                                          geoamv_u,geoamv_v,dum,dum,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("GEOAMV",wks,ngeoamv,date,plevs(k,0),plevs(k,1),geoamv_pre,geoamv_lat,geoamv_lon,\
                                             geoamv_u,geoamv_v,dum,dum,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_geoamv)
                  delete(nlevel_geoamv)
                  delete(geoamv_id)
                  delete(geoamv_lat)
                  delete(geoamv_lon)
                  delete(geoamv_pre)
                  delete(geoamv_u)
                  delete(geoamv_v)
               end if   ; end of ngeoamv check

               if ( npolaramv .gt. 0 ) then
                  polaramv_id  = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "id")
                  polaramv_lat = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "lat")
                  polaramv_lon = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "lon")
                  polaramv_pre = read_info(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "pre")
                  polaramv_u   = read_data(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "polaramv", "u")
                  polaramv_v   = read_data(npolaramv, ipolaramv, cdata, nlevel_polaramv, ilevel_polaramv, "polaramv", "v")
                  wks = gsn_open_wks(out_type,plotdir+"polaramv_"+date)
                  dum = polaramv_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("POLARAMV",wks,npolaramv,date,plevs(k),plevs(k),polaramv_pre,polaramv_lat,polaramv_lon,\
                                            polaramv_u,polaramv_v,dum,dum,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("POLARAMV",wks,npolaramv,date,plevs(k,0),plevs(k,1),polaramv_pre,polaramv_lat,polaramv_lon,\
                                               polaramv_u,polaramv_v,dum,dum,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_polaramv)
                  delete(nlevel_polaramv)
                  delete(polaramv_id)
                  delete(polaramv_lat)
                  delete(polaramv_lon)
                  delete(polaramv_pre)
                  delete(polaramv_u)
                  delete(polaramv_v)
               end if   ; end of npolaramv check

               if ( ngpspw .gt. 0 ) then
                  gpspw_id  = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "id")
                  gpspw_lat = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "lat")
                  gpspw_lon = read_info(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "lon")
                  gpspw_tpw = read_data(ngpspw, igpspw, cdata, nlevel_gpspw, ilevel_gpspw, "gpspw", "tpw")
                  ngpspw_tpw_all = num(.not.ismissing(gpspw_tpw(:,0)))
                  ngpspw_tpw_qc  = num(.not.ismissing(mask(gpspw_tpw(:,0),gpspw_tpw(:,2).ge.0,True)))
                  ngpspw_qc      = ngpspw_tpw_qc
                  wks = gsn_open_wks(out_type,plotdir+"gpspw_"+date)
                  dum = gpspw_tpw
                  plot_sfc("GPSPW",wks,ngpspw,date,gpspw_lat,gpspw_lon,dum,dum,dum,dum,dum,gpspw_tpw,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_gpspw)
                  delete(nlevel_gpspw)
                  delete(gpspw_id)
                  delete(gpspw_lat)
                  delete(gpspw_lon)
                  delete(gpspw_tpw)
               end if   ; end of ngpspw check

               if ( nsound .gt. 0 ) then
                  sound_id  = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "id")
                  sound_lat = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "lat")
                  sound_lon = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "lon")
                  sound_pre = read_info(nsound, isound, cdata, nlevel_sound, ilevel_sound, "pre")
                  sound_u   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "u")
                  sound_v   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "v")
                  sound_t   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "t")
                  sound_q   = read_data(nsound, isound, cdata, nlevel_sound, ilevel_sound, "sound", "q")
                  wks = gsn_open_wks(out_type,plotdir+"sound_"+date)
                  dum = sound_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("SOUND",wks,nsound,date,plevs(k),plevs(k),sound_pre,sound_lat,sound_lon,\
                                         sound_u,sound_v,sound_t,sound_q,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("SOUND",wks,nsound,date,plevs(k,0),plevs(k,1),sound_pre,sound_lat,sound_lon,\
                                            sound_u,sound_v,sound_t,sound_q,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_sound)
                  delete(nlevel_sound)
                  delete(sound_id)
                  delete(sound_lat)
                  delete(sound_lon)
                  delete(sound_pre)
                  delete(sound_u)
                  delete(sound_v)
                  delete(sound_t)
                  delete(sound_q)
               end if   ; end of nsound check

               if ( nairep .gt. 0 ) then
                  airep_id  = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "id")
                  airep_lat = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "lat")
                  airep_lon = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "lon")
                  airep_pre = read_info(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "pre")
                  airep_u   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "u")
                  airep_v   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "v")
                  airep_t   = read_data(nairep, iairep, cdata, nlevel_airep, ilevel_airep, "airep", "t")
                  wks = gsn_open_wks(out_type,plotdir+"airep_"+date)
                  dum = airep_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("AIREP",wks,nairep,date,plevs(k),plevs(k),airep_pre,airep_lat,airep_lon,\
                                         airep_u,airep_v,airep_t,dum,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("AIREP",wks,nairep,date,plevs(k,0),plevs(k,1),airep_pre,airep_lat,airep_lon,\
                                            airep_u,airep_v,airep_t,dum,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_airep)
                  delete(nlevel_airep)
                  delete(airep_id)
                  delete(airep_lat)
                  delete(airep_lon)
                  delete(airep_pre)
                  delete(airep_u)
                  delete(airep_v)
                  delete(airep_t)
               end if   ; end of nairep check

               if ( npilot .gt. 0 ) then
                  pilot_id  = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "id")
                  pilot_lat = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "lat")
                  pilot_lon = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "lon")
                  pilot_pre = read_info(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pre")
                  pilot_u   = read_data(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pilot", "u")
                  pilot_v   = read_data(npilot, ipilot, cdata, nlevel_pilot, ilevel_pilot, "pilot", "v")
                  wks = gsn_open_wks(out_type,plotdir+"pilot_"+date)
                  dum = pilot_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("PILOT",wks,npilot,date,plevs(k),plevs(k),pilot_pre,pilot_lat,pilot_lon,\
                                         pilot_u,pilot_v,dum,dum,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("PILOT",wks,npilot,date,plevs(k,0),plevs(k,1),pilot_pre,pilot_lat,pilot_lon,\
                                            pilot_u,pilot_v,dum,dum,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_pilot)
                  delete(nlevel_pilot)
                  delete(pilot_id)
                  delete(pilot_lat)
                  delete(pilot_lon)
                  delete(pilot_pre)
                  delete(pilot_u)
                  delete(pilot_v)
               end if   ; end of npilot check

               if ( nprofiler .gt. 0 ) then
                  profiler_id  = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "id")
                  profiler_lat = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "lat")
                  profiler_lon = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "lon")
                  profiler_pre = read_info(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "pre")
                  profiler_u   = read_data(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "profiler", "u")
                  profiler_v   = read_data(nprofiler, iprofiler, cdata, nlevel_profiler, ilevel_profiler, "profiler", "v")
                  wks = gsn_open_wks(out_type,plotdir+"profiler_"+date)
                  dum = profiler_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("PROFILER",wks,nprofiler,date,plevs(k),plevs(k),profiler_pre,profiler_lat,profiler_lon,\
                                            profiler_u,profiler_v,dum,dum,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("PROFILER",wks,nprofiler,date,plevs(k,0),plevs(k,1),profiler_pre,profiler_lat,profiler_lon,\
                                               profiler_u,profiler_v,dum,dum,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_profiler)
                  delete(nlevel_profiler)
                  delete(profiler_id)
                  delete(profiler_lat)
                  delete(profiler_lon)
                  delete(profiler_pre)
                  delete(profiler_u)
                  delete(profiler_v)
               end if   ; end of nprofiler check

               if ( nssmir .gt. 0 ) then
                  ssmir_id  = read_info(nssmir, issmir, cdata, nlevel_ssmir, ilevel_ssmir, "id")
                  ssmir_lat = read_info(nssmir, issmir, cdata, nlevel_ssmir, ilevel_ssmir, "lat")
                  ssmir_lon = read_info(nssmir, issmir, cdata, nlevel_ssmir, ilevel_ssmir, "lon")
                  ssmir_spd   = read_data(nssmir, issmir, cdata, nlevel_ssmir, ilevel_ssmir, "ssmir", "spd")
                  nssmir_spd_all = num(.not.ismissing(ssmir_spd(:,0)))
                  nssmir_spd_qc  = num(.not.ismissing(mask(ssmir_spd(:,0),ssmir_spd(:,2).ge.0,True)))
                  nssmir_qc      = nssmir_spd_qc
                  wks = gsn_open_wks(out_type,plotdir+"ssmir_"+date)
                  dum = ssmir_spd
                  plot_sfc("SSMIR",wks,nssmir,date,ssmir_lat,ssmir_lon,dum,dum,dum,dum,dum,dum,ssmir_spd)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_ssmir)
                  delete(nlevel_ssmir)
                  delete(ssmir_id)
                  delete(ssmir_lat)
                  delete(ssmir_lon)
                  delete(ssmir_spd)
               end if   ; end of nssmir check

               if ( nqscat .gt. 0 ) then
                  qscat_id  = read_info(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "id")
                  qscat_lat = read_info(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "lat")
                  qscat_lon = read_info(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "lon")
                  qscat_pre = read_info(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "pre")
                  qscat_u   = read_data(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "qscat", "u")
                  qscat_v   = read_data(nqscat, iqscat, cdata, nlevel_qscat, ilevel_qscat, "qscat", "v")
                  wks = gsn_open_wks(out_type,plotdir+"qscat_"+date)
                  dum = qscat_u
                  plot_sfc("QSCAT",wks,nqscat,date,qscat_lat,qscat_lon,qscat_u,qscat_v,dum,dum,dum,dum,dum)
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_qscat)
                  delete(nlevel_qscat)
                  delete(qscat_id)
                  delete(qscat_lat)
                  delete(qscat_lon)
                  delete(qscat_pre)
                  delete(qscat_u)
                  delete(qscat_v)
               end if   ; end of nqscat check

               if ( nbogus .gt. 0 ) then
                  ; bogus_id  = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "id")
                  bogus_lat = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "lat")
                  bogus_lon = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "lon")
                  bogus_pre = read_info(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "pre")
                  bogus_slp = read_bogus_slp(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "bogus", "slp")
                  bogus_u   = read_data(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "bogus", "u")
                  bogus_v   = read_data(nbogus, ibogus, cdata, nlevel_bogus, ilevel_bogus, "bogus", "v")
                  wks = gsn_open_wks(out_type,plotdir+"bogus_"+date)
                  dum = bogus_u
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("BOGUS",wks,nbogus,date,plevs(k),plevs(k),bogus_pre,bogus_lat,bogus_lon,\
                                         bogus_u,bogus_v,dum,dum,dum,bogus_slp*0.01)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("BOGUS",wks,nbogus,date,plevs(k,0),plevs(k,1),bogus_pre,bogus_lat,bogus_lon,\
                                            bogus_u,bogus_v,dum,dum,dum,bogus_slp*0.01)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_bogus)
                  delete(nlevel_bogus)
                  ; delete(bogus_id)
                  delete(bogus_lat)
                  delete(bogus_lon)
                  delete(bogus_pre)
                  delete(bogus_slp)
                  delete(bogus_u)
                  delete(bogus_v)
               end if   ; end of nbogus check

               if ( nairsr .gt. 0 ) then
                  airsr_id  = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "id")
                  airsr_lat = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "lat")
                  airsr_lon = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "lon")
                  airsr_pre = read_info(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "pre")
                  airsr_t   = read_data(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "airsr", "t")
                  airsr_q   = read_data(nairsr, iairsr, cdata, nlevel_airsr, ilevel_airsr, "airsr", "q")
                  wks = gsn_open_wks(out_type,plotdir+"airsr_"+date)
                  dum = airsr_t
                  if ( rank_plevs .eq. 1 ) then
                     do k = 0, nplevs(0)-1
                        plot_upr("AIRSR",wks,nairsr,date,plevs(k),plevs(k),airsr_pre,airsr_lat,airsr_lon,\
                                         dum,dum,airsr_t,airsr_q,dum,dum)
                     end do
                  end if
                  if ( rank_plevs .eq. 2 ) then
                     do k = 0, nplevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr("AIRSR",wks,nairsr,date,plevs(k,0),plevs(k,1),airsr_pre,airsr_lat,airsr_lon,\
                                            dum,dum,airsr_t,airsr_q,dum,dum)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_airsr)
                  delete(nlevel_airsr)
                  delete(airsr_id)
                  delete(airsr_lat)
                  delete(airsr_lon)
                  delete(airsr_pre)
                  delete(airsr_t)
                  delete(airsr_q)
               end if   ; end of nairsr check

               if ( ngpsref .gt. 0 ) then
                  gpsref_id  = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "id")
                  gpsref_lat = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "lat")
                  gpsref_lon = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "lon")
                  gpsref_hgt = read_info(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "hgt")
                  gpsref_ref = read_data(ngpsref, igpsref, cdata, nlevel_gpsref, ilevel_gpsref, "gpsref", "ref")
                  wks = gsn_open_wks(out_type,plotdir+"gpsref_"+date)
                  dum = gpsref_ref
                  if ( rank_hlevs .eq. 1 ) then
                     do k = 0, nhlevs(0)-1
                        plot_upr_h("GPSREF",wks,ngpsref,date,hlevs(k),hlevs(k),gpsref_hgt,gpsref_lat,gpsref_lon,\
                                            dum,dum,gpsref_ref)
                     end do
                  end if
                  if ( rank_hlevs .eq. 2 ) then
                     do k = 0, nhlevs(0)-1
                        if ( .not.ismissing(plevs(k,0)) .and. .not.ismissing(plevs(k,1)) ) then
                           plot_upr_h("GPSREF",wks,ngpsref,date,hlevs(k,0),hlevs(k,1),gpsref_hgt,gpsref_lat,gpsref_lon,\
                                               dum,dum,gpsref_ref)
                        end if
                     end do
                  end if
                  delete(dum)
                  destroy(wks)
                  delete(ilevel_gpsref)
                  delete(nlevel_gpsref)
                  delete(gpsref_id)
                  delete(gpsref_lat)
                  delete(gpsref_lon)
                  delete(gpsref_hgt)
                  delete(gpsref_ref)
               end if   ; end of ngpsref check
 
               delete(data)
               delete(cdata)

            end if   ; end of if file contains data
         end if   ; end of if file exist
      end if   ; end of if plot_gts

      date = advance_cymdh(date,cycle_period)

   end do  ; end of big date loop
   
end

; 'synop' , 'metar' , 'ships' , 'buoy' , 'sonde_sfc'
; 'geoamv' , 'polaramv'
; 'gpspw'
; 'sound'
; 'airep'
; 'pilot' , 'profiler'
; 'ssmir'
; 'ssmiT'
; 'satem'
; 'ssmt1' , 'ssmt2'
; 'qscat'
; 'bogus'
; 'airsr'
; 'gpsref'
