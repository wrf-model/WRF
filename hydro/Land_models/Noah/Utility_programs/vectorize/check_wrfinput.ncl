load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;;;;;;;;;;;;
;  GEO_EM check
;;;;;;;;;;;;
print("GEO_EM check")
print("")

outfile = addfile("/d1/model/WPS/geo_em.d01.nc","r")

lat_2d = 2200  ;
lon_2d = 503  ; 503 - 505 ARE FOREST

twod_lat = outfile->XLAT_M(0,lat_2d,lon_2d)
twod_lon = outfile->XLONG_M(0,lat_2d,lon_2d)
twod_land = outfile->LU_INDEX(0,lat_2d,lon_2d)
twod_green = outfile->GREENFRAC(0,:,lat_2d,lon_2d)

print("2D variables")
print((/twod_lat/))
print((/twod_lon/))
print((/twod_land/))
print((/twod_green/))

;outfile = addfile("/d1/data/vector/WRF/geo_em.FOREST.nc","r")
outfile = addfile("/d1/data/vector/WRF/geo_em.d01.nc","r")

lat_1d = 983302  ; 503->983302  504->983303  505->983304  

vector_lat = outfile->XLAT_M(0,0,lat_1d)
vector_lon = outfile->XLONG_M(0,0,lat_1d)
vector_green = outfile->GREENFRAC(0,:,0,lat_1d)

print("")
print("vector variables")
print((/vector_lat/))
print((/vector_lon/))
print((/vector_green/))

;;;;;;;;;;;;
;  WRFINPUT check
;;;;;;;;;;;;
print("")
print("WRFINPUT check")
print("")

outfile = addfile("/d1/model/WRFV3/run/wrfinput_d01.nc","r")

twod_veg = outfile->IVGTYP(0,lat_2d,lon_2d)
twod_soil = outfile->ISLTYP(0,lat_2d,lon_2d)
twod_hgt = outfile->HGT(0,lat_2d,lon_2d)
twod_lat = outfile->XLAT(0,lat_2d,lon_2d)
twod_lon = outfile->XLONG(0,lat_2d,lon_2d)
twod_tmn = outfile->TMN(0,lat_2d,lon_2d)

print("2D variables")
print((/twod_veg/))
print((/twod_soil/))
print((/twod_hgt/))
print((/twod_lat/))
print((/twod_lon/))
print((/twod_tmn/))

;outfile = addfile("/d1/data/vector/WRF/wrfinput.FOREST.nc","r")
outfile = addfile("/d1/data/vector/WRF/wrfinput_d01.nc","r")

vector_veg = outfile->IVGTYP(0,0,lat_1d)
vector_soil = outfile->ISLTYP(0,0,lat_1d)
vector_hgt = outfile->HGT(0,0,lat_1d)
vector_lat = outfile->XLAT(0,0,lat_1d)
vector_lon = outfile->XLONG(0,0,lat_1d)
vector_tmn = outfile->TMN(0,0,lat_1d)

print("")
print("vector variables")
print((/vector_veg/))
print((/vector_soil/))
print((/vector_hgt/))
print((/vector_lat/))
print((/vector_lon/))
print((/vector_tmn/))

print("Number forest points: "+num(outfile->IVGTYP.le.5))

;;;;;;;;;;;;
;  LDASIN init check
;;;;;;;;;;;;
print("")
print("LDASIN init check")
print("")

outfile = addfile("/d1/model/trunk/HRLDAS_COLLECT_DATA/run/2005010100.LDASIN_DOMAIN1.nc","r")

twod_CANWAT = outfile->CANWAT(0,lat_2d,lon_2d)
twod_SKINTEMP = outfile->SKINTEMP(0,lat_2d,lon_2d)
twod_STEMP_1 = outfile->STEMP_1(0,lat_2d,lon_2d)
twod_STEMP_2 = outfile->STEMP_2(0,lat_2d,lon_2d)
twod_STEMP_3 = outfile->STEMP_3(0,lat_2d,lon_2d)
twod_STEMP_4 = outfile->STEMP_4(0,lat_2d,lon_2d)
twod_SMOIS_1 = outfile->SMOIS_1(0,lat_2d,lon_2d)
twod_SMOIS_2 = outfile->SMOIS_2(0,lat_2d,lon_2d)
twod_SMOIS_3 = outfile->SMOIS_3(0,lat_2d,lon_2d)
twod_SMOIS_4 = outfile->SMOIS_4(0,lat_2d,lon_2d)
twod_VEGFRA = outfile->VEGFRA(0,lat_2d,lon_2d)
twod_GVFMIN = outfile->GVFMIN(0,lat_2d,lon_2d)
twod_GVFMAX = outfile->GVFMAX(0,lat_2d,lon_2d)
twod_Z2D = outfile->Z2D(0,lat_2d,lon_2d)

print("2D variables")
print((/twod_CANWAT/))
print((/twod_SKINTEMP/))
print((/twod_STEMP_1/))
print((/twod_STEMP_2/))
print((/twod_STEMP_3/))
print((/twod_STEMP_4/))
print((/twod_SMOIS_1/))
print((/twod_SMOIS_2/))
print((/twod_SMOIS_3/))
print((/twod_SMOIS_4/))
print((/twod_VEGFRA/))
print((/twod_GVFMIN/))
print((/twod_GVFMAX/))
print((/twod_Z2D/))

outfile = addfile("/d1/data/vector/ldasin/2005010100.LDASIN_DOMAIN1.nc","r")

vector_CANWAT = outfile->CANWAT(0,0,lat_1d)
vector_SKINTEMP = outfile->SKINTEMP(0,0,lat_1d)
vector_STEMP_1 = outfile->STEMP_1(0,0,lat_1d)
vector_STEMP_2 = outfile->STEMP_2(0,0,lat_1d)
vector_STEMP_3 = outfile->STEMP_3(0,0,lat_1d)
vector_STEMP_4 = outfile->STEMP_4(0,0,lat_1d)
vector_SMOIS_1 = outfile->SMOIS_1(0,0,lat_1d)
vector_SMOIS_2 = outfile->SMOIS_2(0,0,lat_1d)
vector_SMOIS_3 = outfile->SMOIS_3(0,0,lat_1d)
vector_SMOIS_4 = outfile->SMOIS_4(0,0,lat_1d)
vector_VEGFRA = outfile->VEGFRA(0,0,lat_1d)
vector_GVFMIN = outfile->GVFMIN(0,0,lat_1d)
vector_GVFMAX = outfile->GVFMAX(0,0,lat_1d)
vector_Z2D = outfile->Z2D(0,0,lat_1d)

print("")
print("vector variables")
print((/vector_CANWAT/))
print((/vector_SKINTEMP/))
print((/vector_STEMP_1/))
print((/vector_STEMP_2/))
print((/vector_STEMP_3/))
print((/vector_STEMP_4/))
print((/vector_SMOIS_1/))
print((/vector_SMOIS_2/))
print((/vector_SMOIS_3/))
print((/vector_SMOIS_4/))
print((/vector_VEGFRA/))
print((/vector_GVFMIN/))
print((/vector_GVFMAX/))
print((/vector_Z2D/))

;;;;;;;;;;;;
;  LDASIN check
;;;;;;;;;;;;
print("")
print("LDASIN check")
print("")

outfile = addfile("/d1/model/trunk/HRLDAS_COLLECT_DATA/run/2005010100.LDASIN_DOMAIN1.nc","r")

twod_T2D = outfile->T2D(0,lat_2d,lon_2d)
twod_Q2D = outfile->Q2D(0,lat_2d,lon_2d)
twod_U2D = outfile->U2D(0,lat_2d,lon_2d)
twod_V2D = outfile->V2D(0,lat_2d,lon_2d)
twod_PSFC = outfile->PSFC(0,lat_2d,lon_2d)
twod_RAINRATE = outfile->RAINRATE(0,lat_2d,lon_2d)
twod_SWDOWN = outfile->SWDOWN(0,lat_2d,lon_2d)
twod_LWDOWN = outfile->LWDOWN(0,lat_2d,lon_2d)
twod_WEASD = outfile->WEASD(0,lat_2d,lon_2d)

print("2D variables")
print((/twod_T2D/))
print((/twod_Q2D/))
print((/twod_U2D/))
print((/twod_V2D/))
print((/twod_PSFC/))
print((/twod_RAINRATE/))
print((/twod_SWDOWN/))
print((/twod_LWDOWN/))
print((/twod_WEASD/))

outfile = addfile("/d1/data/vector/ldasin/2005010100.LDASIN_DOMAIN1.nc","r")

vector_T2D = outfile->T2D(0,0,lat_1d)
vector_Q2D = outfile->Q2D(0,0,lat_1d)
vector_U2D = outfile->U2D(0,0,lat_1d)
vector_V2D = outfile->V2D(0,0,lat_1d)
vector_PSFC = outfile->PSFC(0,0,lat_1d)
vector_RAINRATE = outfile->RAINRATE(0,0,lat_1d)
vector_SWDOWN = outfile->SWDOWN(0,0,lat_1d)
vector_LWDOWN = outfile->LWDOWN(0,0,lat_1d)
vector_WEASD = outfile->WEASD(0,0,lat_1d)

print("")
print("vector variables")
print((/vector_T2D/))
print((/vector_Q2D/))
print((/vector_U2D/))
print((/vector_V2D/))
print((/vector_PSFC/))
print((/vector_RAINRATE/))
print((/vector_SWDOWN/))
print((/vector_LWDOWN/))
print((/vector_WEASD/))

end


;vector_lat = outfile->XLAT_M
;vector_lon = outfile->XLONG_M

;do i = 0,1563015
;  if(vector_lat(0,0,i).gt.twod_lat(0,0)-0.01 .and. vector_lat(0,0,i).lt.twod_lat(0,0)+0.01 .and. \
;     vector_lon(0,0,i).gt.twod_lon(0,0)-0.01 .and. vector_lon(0,0,i).lt.twod_lon(0,0)+0.01) then
;    print(vector_lat(0,0,i)+" "+twod_lat(0,0))
;    print(vector_lon(0,0,i)+" "+twod_lon(0,0))
;    print("i: "+i)
;  end if
;end do

