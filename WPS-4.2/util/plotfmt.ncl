;_______________________________________________________________________________________
;To run the script type:
;       ncl plotfmt.ncl {input file}
;
;       e.g.
;               ncl plotfmt.ncl 'filename="FILE:2005-06-01_00"'
;
;
;  This script can only be used in NCL V6.2 or later!!!!!
;_______________________________________________________________________________________

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin

; Make sure we have a datafile to work with
  if (.not. isvar("filename") ) then
    print(" ")
    print(" ### MUST SUPPLY a filename ### ")
    print(" ")
    print("     Something like: ")
    print("     ncl plotfmt.ncl filename=FILE:2005-06-01_00")
    print("          REMEMBER TO ADD QUOTES" )
    print("          Refer to the information at the top of this file for more info and syntax" )
    exit
  end if

  head_real  = new(14,float)
  field      = new(1,string)
  hdate      = new(1,string)
  units      = new(1,string)
  map_source = new(1,string)
  desc       = new(1,string)

; We generate plots, but what kind do we prefer?
 type = "x11"
; type = "pdf"
; type = "ps"
; type = "ncgm"

  outf = "plotfmt_bin"
  wks = gsn_open_wks(type,outf)

  res = True
  res@cnFillOn = True

  ;Open binary file

  istatus =  wrf_wps_open_int(filename)

  if(istatus.eq.0) then

    ;Read header of file
   wrf_wps_rdhead_int(istatus,head_real,field,hdate, \
                      units,map_source,desc)

    do while (istatus.eq.0)

      version = toint(head_real(0))
      xfcst   = head_real(1)
      xlvl    = head_real(2)
      nx      = toint(head_real(3))
      ny      = toint(head_real(4))
      iproj   = toint(head_real(5))
      print("==================================================")
      print("VAR = " + field + "__" + xlvl )

      lat0      = head_real(6)
      lon0      = head_real(7)

      print("hdate         = '" + hdate + "'")
      print("units         = '" + units + "'")
      print("desc          = '" + desc + "'")
      print("field         = '" + field + "'")
      print("map_source    = '" + map_source + "'")
      print("version       = " + version)
      print("xfcst         = " + xfcst)
      print("xlvl          = " + xlvl)
      print("nx/ny         = " + nx + "/" + ny )
      print("iproj         = " + iproj)
      print("lat0/lon0     = " + lat0 + "/" + lon0)
      print("head_real(8)  = " + head_real(8))
      print("head_real(9)  = " + head_real(9))
      print("head_real(10) = " + head_real(10))
      print("head_real(11) = " + head_real(11))
      print("head_real(12) = " + head_real(12))
      print("head_real(13) = " + head_real(13))


      if (iproj.eq.0) then         ;Cylindrical Equidistant
        lat = lat0 + ispan(0,ny-1,1)*head_real(8)
        lon = lon0 + ispan(0,nx-1,1)*head_real(9)
;---Turn these into 1D coordinate arrays
        lat!0     = "lat"
        lon!0     = "lon"
        lon@units = "degrees_east"
        lat@units = "degrees_north"
        lat&lat   = lat
        lon&lon   = lon
      end if

      if (iproj.eq.1) then          ; Mercator
        dx = head_real(8)
        dy = head_real(9)
        truelat1 = head_real(10)
        res1 = True
        res1@MAP_PROJ  = 3
        res1@TRUELAT1  = truelat1
        res1@DX        = dx*1000.
        res1@DY        = dy*1000.
        res1@REF_LAT   = lat0
        res1@REF_LON   = lon0
        res1@POLE_LAT  = 90.0
        res1@POLE_LON  =  0.0
        res1@LATINC    = 0.0
        res1@LONINC    = 0.0
        res1@KNOWNI    = 1.0
        res1@KNOWNJ    = 1.0
        loc = wrf_ij_to_ll (nx,ny,res1)
       
        res@gsnAddCyclic = False
        res@mpLimitMode = "Corners"
        res@mpLeftCornerLatF = lat0
        res@mpLeftCornerLonF = lon0
        res@mpRightCornerLatF = loc(1)
        res@mpRightCornerLonF = loc(0)
        res@tfDoNDCOverlay = True
        res@mpProjection = "mercator"
      end if

      if (iproj.eq.3) then          ; Lambert Conformal
        dx = head_real(8)
        dy = head_real(9)
        xlonc = head_real(10)
        truelat1 = head_real(11)
        truelat2 = head_real(12)
        res1 = True
        res1@MAP_PROJ  = 1
        res1@TRUELAT1  = truelat1
        res1@TRUELAT2  = truelat2
        res1@STAND_LON = xlonc
        res1@DX        = dx*1000.
        res1@DY        = dy*1000.
        res1@REF_LAT   = lat0
        res1@REF_LON   = lon0
        res1@POLE_LAT  = 90.0
        res1@POLE_LON  =  0.0
        res1@LATINC    = 0.0
        res1@LONINC    = 0.0
        res1@KNOWNI    = 1.0
        res1@KNOWNJ    = 1.0
        loc = wrf_ij_to_ll (nx,ny,res1)

        res@gsnAddCyclic = False
        res@mpLimitMode = "Corners"
        res@mpLeftCornerLatF = lat0
        res@mpLeftCornerLonF = lon0
        res@mpRightCornerLatF = loc(1)
        res@mpRightCornerLonF = loc(0)
        res@tfDoNDCOverlay = True
        res@mpProjection = "LambertConformal"
        res@mpLambertParallel1F = truelat1
        res@mpLambertParallel2F = truelat2
        res@mpLambertMeridianF = xlonc
      end if

      if (iproj.eq.4) then        ;Gaussian
        nlats    = head_real(8)
        deltalon = head_real(9)
        deltalat = 2.*(lat0)/(2.*nlats-1)
        if (lat0 .ge. 80.) then
          deltalat = -1.0*deltalat
        end if
        lat = lat0 + ispan(0,ny-1,1)*deltalat
        lon = lon0 + ispan(0,nx-1,1)*deltalon
;---Turn these into 1D coordinate arrays
        lat!0     = "lat"
        lon!0     = "lon"
        lon@units = "degrees_east"
        lat@units = "degrees_north"
        lat&lat   = lat
        lon&lon   = lon
      end if

      istatus = 0

      ; Read 2D data

      slab = wrf_wps_rddata_int(istatus,nx,ny)

      slab@_FillValue = -1e+30
      slab!1 = "lon"
      slab!0 = "lat"
      slab&lon = lon
      slab&lat = lat
      slab@units = units

      slab@description = xlvl +"  "+ desc
;     printVarSummary(slab)

      map = gsn_csm_contour_map(wks,slab,res)
      delete(lat)
      delete(lon)
      delete(slab)

      wrf_wps_rdhead_int(istatus,head_real,field,hdate, \
                         units,map_source,desc)

    end do

  end if
   
end
