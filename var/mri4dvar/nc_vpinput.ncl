load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin


   filename = "vp_input.0000"

   varnames = (/"psi","chi_u","t_u","rh","ps_u"/)
   nvar = dimsizes(varnames)

   setfileoption("bin","ReadByteOrder","BigEndian")
   dims = fbinrecread(filename,0,6,"integer")   
   ni=dims(1)-dims(0)+1
   nj=dims(3)-dims(2)+1
   nk=dims(5)-dims(4)+1
   print("ni, nj, nk = "+ni+", "+nj+", "+nk)

   vv = fbinrecread(filename,1,(/5,nk,nj,ni/),"double")

   system("/bin/rm -f "+filename+".nc")   ; remove any pre-existing file
   ncdf = addfile(filename+".nc" ,"c")  ; open output netCDF file

    ;===================================================================
    ; make time an UNLIMITED dimension; recommended  for most applications
    ;===================================================================
    ;   filedimdef(ncdf,"time",-1,True) 

    u = new((/nk, nj,ni/),double)
    v = new((/nk, nj,ni/),double)
    t = new((/nk, nj,ni/),double)
   rh = new((/nk, nj,ni/),double)
   ps = new((/nk, nj,ni/),double)

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

    u(:,:,:) = vv(0,:,:,:)
    v(:,:,:) = vv(1,:,:,:)
    t(:,:,:) = vv(2,:,:,:)
   rh(:,:,:) = vv(3,:,:,:)
   ps(:,:,:) = vv(4,:,:,:)

   ncdf->u = u
   ncdf->v = v
   ncdf->t = t
   ncdf->rh = rh
   ncdf->ps = ps

   ;************************************************
   ; end of reading be.dat data
   ;************************************************

end
