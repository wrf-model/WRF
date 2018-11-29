load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

var2check = "HFX"
twodlist = systemfunc("ls /qnap/yingz/test/20050101*")
onedlist = systemfunc("ls /d1/output/vector/test/20050101*")

twod = new(24,float)
oned = new(24,float)

lat_2d = 2200  ;
lon_2d = 503  ; 503 - 505 ARE FOREST

do i=0,23
 outfile = addfile(twodlist(i)+".nc","r")
 twod(i) = outfile->$var2check$(0,lat_2d,lon_2d)
end do

print("2D variables")
print((/twod/))

lat_1d = 983302  ; 503->983302  504->983303  505->983304  

do i=0,23
 outfile = addfile(onedlist(i)+".nc","r")
 oned(i) = outfile->$var2check$(0,0,lat_1d)
end do

print("")
print("vector variables")
print((/oned/))

print("")
print("maximum diffrence: "+max(twod-oned))

end

