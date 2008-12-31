; PURPOSE:
; 1.  stuff text info into existing netCDF forcing file
;   
; AUTHOR: Josh Hacker, NCAR 
;
; NOTES:
; 1.  Assumes the run is at sea-levels (terrain height 0).
; 2.  Makes a soil input that is zero-gradiant in T; 
;      - soil T is equal to surface T
; 3.  YOU are responsible for creating the input_soil and input_sounding 
; manually - it did not seem to make sense to tie these together
; 4.  Format for input text file to this script is the surface (2-m) values
; followed by the profile.
;     0.0  2-m T   2-m Qv sfcP  
;     Z      U       V     W    
; The level 0 is only so that interpolation to the lowest WRF level is available
; 5.  Requires cdl file forcing_file.cdl

begin

inFName = "GABLS_II_forcing.txt"
outFName = "force_ideal.nc"
cdlFName = "forcing_file.cdl"

; dates determine solar insolation at the top of the atmosphere
initTime = "1999-10-22_19:00:00"; need to be in WRF format
endTime = "1999-10-25_06:00:00"; 59 hours in GABLS II

nz = 8
nt = 2 ; only need a start and end

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

TimeHolder = oFl->Times
dims = dimsizes(TimeHolder)
DateLen = dims(1)

DateStr = stringtochar(initTime)
oFl->Times(0,:) = (/DateStr(0:DateLen-1)/)
DateStr = stringtochar(endTime)
oFl->Times(1,:) = (/DateStr(0:DateLen-1)/)

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
