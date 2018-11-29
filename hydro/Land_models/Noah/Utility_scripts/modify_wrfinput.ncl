

; Run with, for example:
;    ncl ldasout=\"2003070412.LDASOUT_DOMAIN1\" wrfinput=\"wrfinput_d01\" modify_wrfinput.ncl



f = addfile(wrfinput+".nc", "w")
g = addfile(ldasout+".nc", "r")


f->CANWAT = where(f->LANDMASK.eq.1, g->CANWAT,   f->CANWAT)
; f->TSK    = where(f->LANDMASK.eq.1, g->SKINTEMP, f->TSK)

printVarSummary(f->LANDMASK)
printVarSummary(f->TSLB)
printVarSummary(g->SOIL_T)

dims = dimsizes(f->TSLB)
kdim = dims(1)

do k=0,kdim-1
   f->TSLB (:,k,:,:) = where(f->LANDMASK.eq.1, g->SOIL_T(:,k,:,:), f->TSLB (:,k,:,:))
   f->SMOIS(:,k,:,:) = where(f->LANDMASK.eq.1, g->SOIL_M(:,k,:,:), f->SMOIS(:,k,:,:))
   f->SH2O (:,k,:,:) = where(f->LANDMASK.eq.1, g->SOIL_W(:,k,:,:), f->SH2O (:,k,:,:))
end do
