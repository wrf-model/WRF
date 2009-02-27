;
; Author: Hui-Chuan Lin   MMM/NCAR
;
; Purpose: plot the distribution of observations contained in obsproc output
;          i.e. WRF-Var ob input (ob.ascii).
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

;***********************************************************************
; user-defined settings
;***********************************************************************
date = "2007010200"
obfile = "/data1/class13/test/obsproc/obs_gts_2007-01-02_00:00:00.3DVAR"
fgfile = "/wrfhelp/DATA/VAR/Con200/rc/2007010200/wrfinput_d01"   ; for retrieving mapping info
out_type = "pdf"
plotname = "./obsloc"+date
proc_synop = True
proc_metar = True
proc_ship  = True
proc_buoy  = True
proc_satem = True
proc_satob = True
proc_gpspw = True
proc_sound = True
proc_airep = True
proc_pilot = True
proc_profl = True
proc_qscat = True
proc_bogus = True
proc_airsr = True
proc_gpsrf = True
;***********************************************************************
; end of user-defined settings
;***********************************************************************

if ( isfilepresent(fgfile) ) then
   wrf_file=addfile(fgfile+".nc","r")
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
else
   print("Error: no first guess found for retrieving mapping info")
   exit
end if

colormap = "BkBlAqGrYeOrReViWh200"

undef("setmpres")
function setmpres(title:string, str1:string, str2:string)
begin
   mpres                         = True
   mpres@gsnPaperOrientation     = "portrait"
   mpres@gsnMaximize             = False            ; Maximize plot in frame.
   mpres@gsnDraw                 = False
   mpres@gsnFrame                = False            ; Don't advance the frame

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

   mpres@pmTickMarkDisplayMode   = "Always"
   ; mpres@tmYROn                  = False
   ; mpres@tmXBOn                  = False
   mpres@tmXTMajorLengthF        = 0
   mpres@tmYLMajorLengthF        = 0
   mpres@tmXBMajorLengthF        = 0
   mpres@tmYRMajorLengthF        = 0
   mpres@mpOutlineBoundarySets   = "GeophysicalAndUSStates" ; state boundaries
   ; mpres@mpFillOn = False
   mpres@mpFillOn = True
   mpres@mpLandFillColor = "gray"
   mpres@tfDoNDCOverlay          = True

   mpres@mpUSStateLineColor = "gray"
   mpres@mpNationalLineColor = "gray"
   mpres@mpGeophysicalLineColor  = "gray"
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

begin  ; main script

   system("rm -f "+plotname+"tmp")
   system("sed -n '/^FM/,$p' "+obfile+" > "+plotname+"tmp")
   obfile = plotname+"tmp"
   data  = asciiread(obfile, -1, "string")    ; -1 means read all rows.
   cdata = stringtochar(data)
   nline = dimsizes(cdata(:,0))
   nsynop = 0
   nmetar = 0
   nship  = 0
   nbuoy  = 0
   nbogus = 0
   nsound = 0
   nairep = 0
   npilot = 0
   nsatem = 0
   nsatob = 0
   ngpspw = 0
   ngpsrf = 0
   nqscat = 0
   nprofl = 0
   nairsr = 0

   ; find out the number of each observation type
   do n = 0, nline-1  ; loop1 of parsing the file line by line
      if ( cdata(n,0:5) .eq. "FM-12 " ) then
         nsynop = nsynop + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-15 " .or. cdata(n,0:5) .eq. "FM-16 ") then
         nmetar = nmetar + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-13 " ) then
         nship = nship + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-18 " .or. cdata(n,0:5) .eq. "FM-19 " ) then
         nbuoy = nbuoy + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-135" ) then
         nbogus = nbogus + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-35 " .or. cdata(n,0:5) .eq. "FM-36 ") then
         nsound = nsound + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-42 " .or. cdata(n,0:5) .eq. "FM-97 ") then
         nairep = nairep + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-32 " ) then
         npilot = npilot + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-86 " ) then
         nsatem = nsatem + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-88 " ) then
         nsatob = nsatob + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-111" ) then
         ngpspw = ngpspw + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-116" ) then
         ngpsrf = ngpsrf + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-281" ) then
         nqscat = nqscat + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-132" ) then
         nprofl = nprofl + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-133" ) then
         nairsr = nairsr + 1
      end if
   end do  ; end loop1 of parsing the file line by line

   if ( nsynop .gt. 0  )then
      synop_lat = new(nsynop, float)
      synop_lon = new(nsynop, float)
   end if
   if ( nmetar .gt. 0  )then
      metar_lat = new(nmetar, float)
      metar_lon = new(nmetar, float)
   end if
   if ( nship .gt. 0  )then
      ship_lat  = new(nship,  float)
      ship_lon  = new(nship,  float)
   end if
   if ( nbuoy .gt. 0  )then
      buoy_lat  = new(nbuoy,  float)
      buoy_lon  = new(nbuoy,  float)
   end if
   if ( nbogus .gt. 0  )then
      bogus_lat = new(nbogus, float)
      bogus_lon = new(nbogus, float)
   end if
   if ( nsound .gt. 0  )then
      sound_lat = new(nsound, float)
      sound_lon = new(nsound, float)
   end if
   if ( nairep .gt. 0  )then
      airep_lat = new(nairep, float)
      airep_lon = new(nairep, float)
   end if
   if ( npilot .gt. 0  )then
      pilot_lat = new(npilot, float)
      pilot_lon = new(npilot, float)
   end if
   if ( nsatem .gt. 0  )then
      satem_lat = new(nsatem, float)
      satem_lon = new(nsatem, float)
   end if
   if ( nsatob .gt. 0  )then
      satob_lat = new(nsatob, float)
      satob_lon = new(nsatob, float)
   end if
   if ( ngpspw .gt. 0  )then
      gpspw_lat = new(ngpspw, float)
      gpspw_lon = new(ngpspw, float)
   end if
   if ( ngpsrf .gt. 0  )then
      gpsrf_lat = new(ngpsrf, float)
      gpsrf_lon = new(ngpsrf, float)
   end if
   if ( nqscat .gt. 0  )then
      qscat_lat = new(nqscat, float)
      qscat_lon = new(nqscat, float)
   end if
   if ( nprofl .gt. 0  )then
      profl_lat = new(nprofl, float)
      profl_lon = new(nprofl, float)
   end if
   if ( nairsr .gt. 0  )then
      airsr_lat = new(nairsr, float)
      airsr_lon = new(nairsr, float)
   end if

   isynop = 0
   imetar = 0
   iship  = 0
   ibuoy  = 0
   ibogus = 0
   isound = 0
   iairep = 0
   ipilot = 0
   isatem = 0
   isatob = 0
   igpspw = 0
   igpsrf = 0
   iqscat = 0
   iprofl = 0
   iairsr = 0

   do n = 0, nline-1  ; loop2 of parsing the file line by line
      if ( cdata(n,0:5) .eq. "FM-12 " ) then
         synop_lat(isynop) = stringtofloat(charactertostring(cdata(n,80:91)))
         synop_lon(isynop) = stringtofloat(charactertostring(cdata(n,103:114)))
         isynop = isynop + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-15 " .or. cdata(n,0:5) .eq. "FM-16 ") then
         metar_lat(imetar) = stringtofloat(charactertostring(cdata(n,80:91)))
         metar_lon(imetar) = stringtofloat(charactertostring(cdata(n,103:114)))
         imetar = imetar + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-13 " ) then
         ship_lat(iship) = stringtofloat(charactertostring(cdata(n,80:91)))
         ship_lon(iship) = stringtofloat(charactertostring(cdata(n,103:114)))
         iship = iship + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-18 " .or. cdata(n,0:5) .eq. "FM-19 " ) then
         buoy_lat(ibuoy) = stringtofloat(charactertostring(cdata(n,80:91)))
         buoy_lon(ibuoy) = stringtofloat(charactertostring(cdata(n,103:114)))
         ibuoy = ibuoy + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-135" ) then
         bogus_lat(ibogus) = stringtofloat(charactertostring(cdata(n,80:91)))
         bogus_lon(ibogus) = stringtofloat(charactertostring(cdata(n,103:114)))
         ibogus = ibogus + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-35 " .or. cdata(n,0:5) .eq. "FM-36 ") then
         sound_lat(isound) = stringtofloat(charactertostring(cdata(n,80:91)))
         sound_lon(isound) = stringtofloat(charactertostring(cdata(n,103:114)))
         isound = isound + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-42 " .or. cdata(n,0:5) .eq. "FM-97 ") then
         airep_lat(iairep) = stringtofloat(charactertostring(cdata(n,80:91)))
         airep_lon(iairep) = stringtofloat(charactertostring(cdata(n,103:114)))
         iairep = iairep + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-32 " ) then
         pilot_lat(ipilot) = stringtofloat(charactertostring(cdata(n,80:91)))
         pilot_lon(ipilot) = stringtofloat(charactertostring(cdata(n,103:114)))
         ipilot = ipilot + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-86 " ) then
         satem_lat(isatem) = stringtofloat(charactertostring(cdata(n,80:91)))
         satem_lon(isatem) = stringtofloat(charactertostring(cdata(n,103:114)))
         isatem = isatem + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-88 " ) then
         satob_lat(isatob) = stringtofloat(charactertostring(cdata(n,80:91)))
         satob_lon(isatob) = stringtofloat(charactertostring(cdata(n,103:114)))
         isatob = isatob + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-111" ) then
         gpspw_lat(igpspw) = stringtofloat(charactertostring(cdata(n,80:91)))
         gpspw_lon(igpspw) = stringtofloat(charactertostring(cdata(n,103:114)))
         igpspw = igpspw + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-116" ) then
         gpsrf_lat(igpsrf) = stringtofloat(charactertostring(cdata(n,80:91)))
         gpsrf_lon(igpsrf) = stringtofloat(charactertostring(cdata(n,103:114)))
         igpsrf = igpsrf + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-281" ) then
         qscat_lat(iqscat) = stringtofloat(charactertostring(cdata(n,80:91)))
         qscat_lon(iqscat) = stringtofloat(charactertostring(cdata(n,103:114)))
         iqscat = iqscat + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-132" ) then
         profl_lat(iprofl) = stringtofloat(charactertostring(cdata(n,80:91)))
         profl_lon(iprofl) = stringtofloat(charactertostring(cdata(n,103:114)))
         iprofl = iprofl + 1
      end if
      if ( cdata(n,0:5) .eq. "FM-133" ) then
         airsr_lat(iairsr) = stringtofloat(charactertostring(cdata(n,80:91)))
         airsr_lon(iairsr) = stringtofloat(charactertostring(cdata(n,103:114)))
         iairsr = iairsr + 1
      end if
   end do  ; end loop2 of parsing the file line by line

   markersize1 = 0.006
   wks = gsn_open_wks(out_type, plotname)
   gsn_define_colormap(wks,colormap)
   nc1 = NhlNewColor(wks,.8,.8,.8)  ; add light gray to colormap
   gsres = True
   if ( proc_synop .and. nsynop .gt. 0 ) then
      mpres = setmpres("SYNOP",date,nsynop+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,synop_lon(:),synop_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_metar .and. nmetar .gt. 0 ) then
      mpres = setmpres("METAR",date,nmetar+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,metar_lon(:),metar_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_ship .and. nship .gt. 0 ) then
      mpres = setmpres("SHIP",date,nship+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,ship_lon(:),ship_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_buoy .and. nbuoy .gt. 0 ) then
      mpres = setmpres("BUOY",date,nbuoy+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,buoy_lon(:),buoy_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_bogus .and. nbogus .gt. 0 ) then
      mpres = setmpres("BOGUS",date,nbogus+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,bogus_lon(:),bogus_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_sound .and. nsound .gt. 0 ) then
      mpres = setmpres("sound",date,nsound+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,sound_lon(:),sound_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if (proc_airep .and. nairep .gt. 0 ) then
      mpres = setmpres("AIREP",date,nairep+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,airep_lon(:),airep_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_pilot .and. npilot .gt. 0 ) then
      mpres = setmpres("PILOT",date,npilot+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,pilot_lon(:),pilot_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_satem .and. nsatem .gt. 0 ) then
      mpres = setmpres("SATEM",date,nsatem+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,satem_lon(:),satem_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_satob .and. nsatob .gt. 0 ) then
      mpres = setmpres("SATOB",date,nsatob+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,satob_lon(:),satob_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_gpspw .and. ngpspw .gt. 0 ) then
      mpres = setmpres("GPSPW",date,ngpspw+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,gpspw_lon(:),gpspw_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_gpsrf .and. ngpsrf .gt. 0 ) then
      mpres = setmpres("GSPREF",date,ngpsrf+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,gpsrf_lon(:),gpsrf_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_qscat .and. nqscat .gt. 0 ) then
      mpres = setmpres("QSCAT",date,nqscat+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,qscat_lon(:),qscat_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_profl .and. nprofl .gt. 0 ) then
      mpres = setmpres("PROFILER",date,nprofl+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,profl_lon(:),profl_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   if ( proc_airsr .and. nairsr .gt. 0 ) then
      mpres = setmpres("AIRS Retrieval",date,nairsr+"")
      map = gsn_csm_map(wks,mpres)
      gsres@gsMarkerIndex = 16                    ; Use filled dots for markers.
      gsres@gsMarkerColor  = "navy"
      gsres@gsMarkerSizeF  = markersize1          ; default 0.007
      dummy = gsn_add_polymarker(wks,map,airsr_lon(:),airsr_lat(:),gsres)
      draw(map)
      frame(wks)
      delete(dummy)
   end if
   system("rm -f "+plotname+"tmp")
end
