load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin


   filename = "analysis_increments"

   varnames = (/"psi","chi_u","t_u","rh","ps_u"/)
   nvar = dimsizes(varnames)

   setfileoption("bin","ReadByteOrder","BigEndian")
   dims = fbinrecread(filename,0,6,"integer")   
   ni=dims(1) ;;+ 1
   nj=dims(3) ;;+ 1
   nk=dims(5) ;;+ 1
   print("ni, nj, nk = "+ni+", "+nj+", "+nk)

   vv = fbinrecread(filename,1,(/4,nk,nj,ni/),"double")

   system("/bin/rm -f "+filename+".nc")   ; remove any pre-existing file
   ncdf = addfile(filename+".nc" ,"c")  ; open output netCDF file

    ;===================================================================
    ; make time an UNLIMITED dimension; recommended  for most applications
    ;===================================================================
    ;   filedimdef(ncdf,"time",-1,True) 

   u = new((/nk, nj,ni/),double)
   v = new((/nk, nj,ni/),double)
   ;w = new((/nk, nj,ni/),double)
   ;p = new((/nk, nj,ni/),double)
   t = new((/nk, nj,ni/),double)
   q = new((/nk, nj,ni/),double)
   ;z = new((/nk, nj,ni/),double)

   u(:,:,:) = vv(0,:,:,:)
   v(:,:,:) = vv(1,:,:,:)
   ;w(:,:,:) = vv(2,:,:,:)
   ;p(:,:,:) = vv(3,:,:,:)
   t(:,:,:) = vv(2,:,:,:)
   q(:,:,:) = vv(3,:,:,:)
   ;z(:,:,:) = vv(6,:,:,:)
   

   ncdf->u = u
   ncdf->v = v
   ;ncdf->w = w
   ;ncdf->p = p
   ncdf->t = t
   ncdf->q = q
   ;ncdf->z = z

end

