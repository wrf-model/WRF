; ncl script to stuff text info into existing netCDF file
;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; right now this is simple:
; w = 0;
; time-invariant u_g, v_g;
; no advection

begin

inFName = "simple_forcing"
outFName = "force_ideal.nc"
initTime = "2003-05-03_12:00:00"; need to be in WRF format
endTime = "2004-05-03_12:00:00"

nz = 8
nt = 2 ; only need a start and end

; open output file 
oFl = addfile(outFName,"rw")

indat = asciiread(inFName,(/nz,4/),"float")
z = indat(:,0)
u_g = indat(:,1)
v_g = indat(:,2)
w = indat(:,3)
delete(indat)

TimeHolder = oFl->Times
dims = dimsizes(TimeHolder)
DateLen = dims(1)

DateStr = stringtochar(initTime)
oFl->Times(0,:) = (/DateStr(0:DateLen-1)/)
DateStr = stringtochar(endTime)
oFl->Times(1,:) = (/DateStr(0:DateLen-1)/)

; have to loop since Time is unlimited
do itime = 0, nt-1
   oFl->Z_FORC(itime,:) = (/z/)
   oFl->U_G(itime,:)= (/u_g/)
   oFl->V_G(itime,:)= (/v_g/)
   oFl->W_SUBS(itime,:)=(/w/)
   oFl->TH_UPSTREAM_X(itime,:)=(/0.0/)
   oFl->TH_UPSTREAM_Y(itime,:)=(/0.0/)
   oFl->QV_UPSTREAM_X(itime,:)=(/0.0/)
   oFl->QV_UPSTREAM_Y(itime,:)=(/0.0/)
   oFl->U_UPSTREAM_X(itime,:)=(/0.0/)
   oFl->U_UPSTREAM_Y(itime,:)=(/0.0/)
   oFl->V_UPSTREAM_X(itime,:)=(/0.0/)
   oFl->V_UPSTREAM_Y(itime,:)=(/0.0/)
end do

do itime = 0, nt-2
   oFl->U_G_TEND(itime,:)=(/0.0/)
   oFl->V_G_TEND(itime,:)=(/0.0/)
   oFl->W_SUBS_TEND(itime,:)=(/0.0/)
   oFl->TH_UPSTREAM_X_TEND(itime,:)=(/0.0/)
   oFl->TH_UPSTREAM_Y_TEND(itime,:)=(/0.0/)
   oFl->QV_UPSTREAM_X_TEND(itime,:)=(/0.0/)
   oFl->QV_UPSTREAM_Y_TEND(itime,:)=(/0.0/)
   oFl->U_UPSTREAM_X_TEND(itime,:)=(/0.0/)
   oFl->U_UPSTREAM_Y_TEND(itime,:)=(/0.0/)
   oFl->V_UPSTREAM_X_TEND(itime,:)=(/0.0/)
   oFl->V_UPSTREAM_Y_TEND(itime,:)=(/0.0/)
end do


end
