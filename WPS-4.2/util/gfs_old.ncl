;***************************************************************************************************************
; This is a sample NCL script to process GRIB data, plot surface variable (SKINTEMP and MSLP) and upper-air 
; variables (U, V, T, RH) in pressure level.  
;
;**************************************************************************************************************
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
begin
  grb_file = addfile( "gfs_2010042700_12.grib2","r")
  names = getfilevarnames(grb_file)  ; Get the variable names in the
  print(names)                       ; GRIB file and print them out.

;  atts = getfilevaratts(grb_file,names(0)) ; Get the variable attributes and
;  dims = getfilevardims(grb_file,names(0)) ; dimension names from the GRIB
  pres= grb_file->lv_ISBL0
  npres=dimsizes(pres)
  presRH= grb_file->lv_ISBL6
  npresRH=dimsizes(presRH)
  soil= grb_file->lv_DBLL2_l1
  nsoil=dimsizes(soil)

  wks = gsn_open_wks("pdf","gfs") ; Open an X11 workstation.

;----------- MCHEN Begin plot -----------------------------------------
  resources             = True
  resources@cnFillOn                    = True                 ; turn on color
  resources@cnLinesOn                   = False                ; no contour lines
  resources@gsnSpreadColors             = True                 ; use full color map
  resources@lbLabelAutoStride           = True                    ; every other label
  resources@cnLevelSelectionMode        = "ManualLevels"       ; manual levels

  do i = 0,dimsizes(names)-1
    names_char = stringtochar(names(i))
; PMSL
    if(names(i).eq."PRMSL_P0_L101_GLL0") then   
      MSLP = grb_file->PRMSL_P0_L101_GLL0
        if(isatt(MSLP,"units").eq.True)  
          resources@tiMainString = "MSLP ( " + MSLP@units  + ")" 
        else
          resources@tiMainString = "PRMSL_P0_L101_GLL0"
        end if               
    plot = gsn_csm_contour_map_ce(wks,MSLP,resources)
    end if
; SKINTEMP
    if(names(i).eq."TMP_P0_L1_GLL0") then   
      SKINTEMP = grb_file->TMP_P0_L1_GLL0
        if(isatt(SKINTEMP,"units").eq.True)  
          resources@tiMainString = "SKINTEMP ( " + SKINTEMP@units + ")"
        else
          resources@tiMainString = "TMP_P0_L1_GLL0"
        end if               
    plot = gsn_csm_contour_map_ce(wks,SKINTEMP,resources)
    end if
; T2
    if(names(i).eq."TMP_P0_L103_GLL0") then   
      T2 = grb_file->TMP_P0_L103_GLL0
        if(isatt(T2,"units").eq.True)  
          resources@tiMainString = "T2    (" + T2@units + ")"
        else
          resources@tiMainString = "TMP_P0_L103_GLL0"
        end if               
    plot = gsn_csm_contour_map_ce(wks,T2,resources)
    end if
; U10
    if(names(i).eq."UGRD_P0_L103_GLL0") then   
      U10 = grb_file->UGRD_P0_L103_GLL0
        if(isatt(T2,"units").eq.True)  
          resources@tiMainString = "U10   (" + U10@units + ")"
        else
          resources@tiMainString = "UGRD_P0_L103_GLL0"
        end if               
    plot = gsn_csm_contour_map_ce(wks,U10,resources)
    end if
; V10
    if(names(i).eq."VGRD_P0_L103_GLL0") then   
      V10 = grb_file->VGRD_P0_L103_GLL0
        if(isatt(T2,"units").eq.True)  
          resources@tiMainString = "V10,UNIT: " + V10@units + ")"
        else
          resources@tiMainString = "VGRD_P0_L103_GLL0"
        end if               
    plot = gsn_csm_contour_map_ce(wks,V10,resources)
    end if
;  SOILM in 4 levels
    if(names(i).eq."SOILW_P0_2L106_GLL0") then   
      SOILM = grb_file->SOILW_P0_2L106_GLL0
        if(isatt(SOILM,"units").eq.True)  
          MainString = "SOILM, UNIT:" + SOILM@units + ")"
        end if               
     do nlev=0,nsoil-1 
          resources@tiMainString = MainString + " At " + soil(nlev)  + " m"
       plot = gsn_csm_contour_map_ce(wks,SOILM(nlev,:,:),resources)
     end do
    end if
;  SOILT in 4 levels
    if(names(i).eq."TMP_P0_2L106_GLL0") then   
      SOILT = grb_file->TMP_P0_2L106_GLL0
        if(isatt(SOILT,"units").eq.True)  
          MainString = "SOILT  (" + SOILT@units + ")"
        end if               
     do nlev=0,nsoil-1 
          resources@tiMainString = MainString + " At " + soil(nlev) + " m" 
       plot = gsn_csm_contour_map_ce(wks,SOILT(nlev,:,:),resources)
     end do
    end if
;  T in Pres Level
 ;   if(names_char(0:15).eq."TMP_P0_L100_GLL0") then   
    if(names(i).eq."TMP_P0_L100_GLL0") then   
      T = grb_file->TMP_P0_L100_GLL0
        if(isatt(T,"units").eq.True)  
          MainString = "TEMPERATURE ( " + T@units + ")"
        end if               
     do nlev=0,npres-1 
          resources@tiMainString = MainString + " At " + pres(nlev)/100 +"hpa"
       plot = gsn_csm_contour_map_ce(wks,T(nlev,:,:),resources)
     end do
    end if
;  RH in Pres Level
  ;  if(names_char(0:14).eq."RH_P0_L100_GLL0") then   
    if(names(i).eq."RH_P0_L100_GLL0") then   
      RH = grb_file->RH_P0_L100_GLL0
        if(isatt(RH,"units").eq.True)  
          MainString = "RH  (" + RH@units  + ")"
        end if               
     do nlev=0,npresRH-1 
          resources@tiMainString = MainString + " At " + presRH(nlev)/100 +"hpa"
       plot = gsn_csm_contour_map_ce(wks,RH(nlev,:,:),resources)
     end do
    end if
; UGRD in pressure
   ; if(names_char(0:16).eq."UGRD_P0_L100_GLL0") then   
    if(names(i).eq."UGRD_P0_L100_GLL0") then   
      U = grb_file->UGRD_P0_L100_GLL0
        if(isatt(U,"units").eq.True)  
          MainString = "U  ( " + U@units + ")"
        end if               
     do nlev=0,npres-1 
          resources@tiMainString = MainString + " At " + pres(nlev)/100 +"hpa"
       plot = gsn_csm_contour_map_ce(wks,U(nlev,:,:),resources)
     end do
    end if
; VGRD in pressure
  ;  if(names_char(0:11).eq."VGRD_P0_L100") then   
    if(names(i).eq."VGRD_P0_L100_GLL0") then   
      V = grb_file->VGRD_P0_L100_GLL0
        if(isatt(V,"units").eq.True)  
          MainString = "V ( " + V@units  + ")"
        end if               
     do nlev=0,npres-1 
          resources@tiMainString = MainString + " At " + pres(nlev)/100 +"hpa"
       plot = gsn_csm_contour_map_ce(wks,V(nlev,:,:),resources)
     end do
    end if
    delete(names_char)
  end do                        
end
