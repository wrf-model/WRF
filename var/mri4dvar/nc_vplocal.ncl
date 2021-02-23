load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin


   filename = "vp_0020"

   varnames = (/"psi","chi_u","t_u","rh","ps_u"/)
   nvar = dimsizes(varnames)

   setfileoption("bin","ReadByteOrder","BigEndian")
   dims = fbinrecread(filename,0,15,"integer")   
   ni=dims(12)
   nj=dims(13)
   nk=dims(14)
   print("ni, nj, nk = "+ni+", "+nj+", "+nk)

   v = fbinrecread(filename,1,(/5,nk,nj,ni/),"double")

   system("/bin/rm -f "+filename+".nc")   ; remove any pre-existing file
   ncdf = addfile(filename+".nc" ,"c")  ; open output netCDF file

    ;===================================================================
    ; make time an UNLIMITED dimension; recommended  for most applications
    ;===================================================================
    ;   filedimdef(ncdf,"time",-1,True) 

   ;v1 = new((/nk, nj,ni/),double)
   ;v2 = new((/nk, nj,ni/),double)
   ;v3 = new((/nk, nj,ni/),double)
   v4 = new((/nk, nj,ni/),double)
   v5 = new((/nk, nj,ni/),double)

   ;do k = 0, nk-1
   ;do j = 0, nj-1
   ;do i = 0, ni-1
   ;    v1(k,j,i) = v(i,j,k,1)
   ;    v2(k,j,i) = v(i,j,k,2)
   ;    v3(k,j,i) = v(i,j,k,3)
   ;    v4(k,j,i) = v(i,j,k,4) 
   ;    ;v5(k,j,i) = v(i,j,k,5)
   ;end do
   ;end do
   ;end do

   ;v1(:,:,:) = v(1,:,:,:)
   ;v2(:,:,:) = v(2,:,:,:)
   ;v3(:,:,:) = v(3,:,:,:)
   v4(:,:,:) = v(4,:,:,:)
   ;v5(:,:,:) = v(5,:,:,:)

   ;ncdf->v1 = v1
   ;ncdf->v2 = v2
   ;ncdf->v3 = v3
   ncdf->v4 = v4
   ;ncdf->v5 = v5

   ;************************************************
   ; end of reading be.dat data
   ;************************************************

end
