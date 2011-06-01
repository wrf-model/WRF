; PURPOSE:
; 1.  stuff text info into existing netCDF forcing file
;   
; AUTHOR: Josh Hacker, NCAR 
;
; NOTES:
;
; 1.  YOU are responsible for creating the input_soil and input_sounding 
; manually - it did not seem to make sense to tie these together
;
; 2.  Format for input text file to this script is the surfaca on the first line:
;    Terrain U_G V_G W
;
; followed by the profile.
;
;    Z U_g V_g W    
;
; All heights are AMSL in meters.  The real GABLS II test is at 436 m AMSL.  
; For simplicity I ignored this here, but the surface does not need to be at
; 0 m AMSL.  The only requirement is that the surface be at a height less than
; or equal to the lower WRF layer.  For forcing, this value is only used if 
; needed for interpolation (thus the non-zero W there).  The actual surface 
; height for the WRF simulation is set in the first entry in file 
; input_sounding.
;
; 3.  Requires cdl file forcing_file.cdl and ncgen to create the netCDF file
;
; 4.  This script does not compute any advection

begin

inFName = "GABLS_II_forcing.txt"
outFName = "force_ideal.nc"
cdlFName = "forcing_file.cdl"

; dates determine solar insolation at the top of the atmosphere
initTime = "1999-10-22_19:00:00"; need to be in WRF format

nz = 8
nt = 1 ; only need a start and a tendency for this case

;-------END USER MODIFICATIONS-----------------

; check for existence of cdl file
ffile = systemfunc("ls "+cdlFName) 
if ( ismissing(ffile) ) then
 print("Please supply a template "+cdlFName+" that is consistent with "+inFName)
 exit
end if

; create forcing file
ierr = systemfunc("/bin/rm -f "+outFName)
ierr = systemfunc("ncgen -o "+outFName+" "+cdlFName)

; open output file 
oFl = addfile(outFName,"rw")

indat = asciiread(inFName,(/nz,4/),"float")
z = indat(:,0)
u_g = indat(:,1)
v_g = indat(:,2)
w = indat(:,3)
delete(indat)

; Get the length of the time strings
dims = filevardimsizes(oFl,"Times")
DateLen = dims(1)
delete(dims)

DateStr = stringtochar(initTime)
oFl->Times(0,:) = (/DateStr(0:DateLen-1)/)

; have to loop since Time is unlimited
do itime = 0, nt-1
  oFl->Z_FORCE(itime,:) = (/z/)
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
  oFl->Z_FORCE_TEND(itime,:)=(/0.0/)
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
  oFl->TAU_X(itime,:)=(/0.0/)
  oFl->TAU_X_TEND(itime,:)=(/0.0/)
  oFl->TAU_Y(itime,:)=(/0.0/)
  oFl->TAU_Y_TEND(itime,:)=(/0.0/)
end do

end
