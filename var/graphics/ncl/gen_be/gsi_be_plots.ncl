load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

   ; just edit the filename for your case

   filename = "/Volumes/sugar2/hclin/data/tmp_afwa/gen_be/run_gsi_be_lat_bin_size_5.0_lnps/wrf-arw-gsi_be.gcv"
   ;filename = "/Volumes/sugar2/hclin/code/GSI/trunk/fix/nam_nmm_berror.f77.gcv"
   ;filename = "/Volumes/sugar2/hclin/code/GSI/trunk/fix/nam_glb_berror.f77.gcv"

   plot_dir = "."
   out_name = "gsi_be_plots"
   out_type = "pdf"

   Fill_Color = True
   By_Levels = True
   sig_values_for_plot=(/0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1/) ; settings for By_Levels = False
   sig_array_for_plot=(/" .9"," .8"," .7"," .6"," .5"," .4"," .3"," .2"," .1"/) ; settings for By_Levels = False
   ;tmMode = "Explicit"
   tmMode = "Automatic"
   lat_values_for_plot =(/ 0, 10, 20, 30, 40, 50, 60, 70/)  ; settings for tmMode = "Explicit"
   lat_array_for_plot = (/"0N","10N","20N","30N","40N","50N","60N","70N"/)  ; ; settings for tmMode = "Explicit"
   plot_dims_aero = (/5,4/)

   varnames_met = (/ "sf", "vp", "t", "q", "ps" /)
   nvar_met = dimsizes(varnames_met)

   varnames_aero = (/ "BC1", "BC2", "OC1", "OC2", "SEAS_1", "SEAS_2", "SEAS_3", "SEAS_4", \
                      "DUST_1", "DUST_2", "DUST_3", "DUST_4", "DUST_5", "sulf", "P25" /)
   nvar_aero = dimsizes(varnames_aero)

   if ( .not. isfilepresent(filename) ) then
      print("Error: can not find "+filename)
      exit
   end if

   ;-----------------
   ; read in data
   ;-----------------
   setfileoption("bin","ReadByteOrder","BigEndian")
   dims = fbinrecread(filename,0,2,"integer")   
   nsig=dims(0)
   nlat=dims(1)
   print("nsig, nlat = "+nsig+", "+nlat)

   bytes_in_file = stringtointeger(systemfunc("wc -c "+filename))

   ; one record = 4 bytes
   ; one character = 1 byte
   ; end of record = 8 bytes
   nrec_met = 3+4*(nvar_met-1)+3
   ncount_met = 2+(nlat+nsig)+(nsig*nsig*(nlat+2))+(nsig*(nlat+2))+(nsig*(nlat+2))+ \
                (nlat*nsig)*nvar_met+nlat+ \
                nlat+2+(nvar_met-1)*((nlat+2)*nsig+(nlat+2)*nsig) +nvar_met
   nbyte_met = ncount_met*4+5*nvar_met+8*nrec_met

   nrec_aero = 4*nvar_aero
   ncount_aero = nvar_aero*(1+(nlat*nsig)+(nlat+2)*nsig+(nlat+2)*nsig)
   nbyte_aero = ncount_aero*4+5*nvar_aero+8*nrec_aero
   nbyte_tot = nbyte_met+nbyte_aero

   read_aero = False
   nrec_tot = nrec_met
   nvar_tot = nvar_met
   varnames_tot = varnames_met
   if ( bytes_in_file .eq. nbyte_tot ) then
      read_aero = True
      nrec_tot = nrec_met + nrec_aero
      nvar_tot = nvar_met + nvar_aero
      delete(varnames_tot)
      varnames_tot = array_append_record (varnames_met, varnames_aero, 0)
   end if

   rlat = new(nlat, "float")
   rsig = new(nsig, "float")
   agv  = new((/nsig,nsig,nlat+2/), "float")
   bv   = new((/nsig,nlat+2/), "float")
   wgv  = new((/nsig,nlat+2/), "float")
   cov  = new((/nvar_tot,nsig,nlat/), "float")
   covq2 = new((/nsig,nlat/), "float")
   ;hwllp = new(nlat+2, "float")
   hwll  = new((/nvar_tot,nsig,nlat+2/), "float")
   ;vzs   = new((/nvar_tot,nsig,nlat+2/), "float")
   vzs   = new((/nvar_tot,nlat+2,nsig/), "float")

   rtmp = fbinrecread(filename,1,(nlat+nsig),"float")
   rlat = rtmp(0:nlat-1)
   rsig = rtmp(nlat:nlat+nsig-1)
   delete(rtmp)

   yc = ispan(1,nsig,1)
   rlatp2 = new(nlat+2, "float")
   rlatp2(0) = rlat(0)
   rlatp2(nlat+1) = rlat(nlat-1)
   rlatp2(1:nlat) = rlat(0:nlat-1)
   ; do k = 0, nlat-2
   ;   rlatp2(k+1) = 0.5*(rlat(k)+rlat(k+1))
   ;end do
   ;rlatp2(nlat)   = 0.5*(rlatp2(nlat-1)+rlat(nlat-1))
   ;rlatp2(nlat+1) = rlat(nlat-1)

   dsig = new(nsig, "float")
   dsig(0)=log(rsig(0))-log(rsig(1))
   do k=1,nsig-2
      dsig(k)=0.5*(log(rsig(k-1))-log(rsig(k+1)))
   end do
   dsig(nsig-1)=log(rsig(nsig-2))-log(rsig(nsig-1))
 
   cov!0 = "var"
   cov!1 = "lev"
   cov!2 = "lat"
   cov&var = varnames_tot
   cov&lat = rlat

   hwll!0 = "var"
   hwll!1 = "lev"
   hwll!2 = "lat"
   hwll&var = varnames_tot
   hwll&lat = rlatp2

   vzs!0 = "var"
   vzs!1 = "lat"
   vzs!2 = "lev"
   vzs&var = varnames_tot
   vzs&lat = rlatp2

   if ( .not. By_Levels ) then
      var&lev = rsig
      hwll&lev = rsig
      vzs&lev = rsig
   else
      cov&lev = yc
      hwll&lev = yc
      vzs&lev = yc
   end if

   reclen = (nsig*nsig*(nlat+2))+(nsig*(nlat+2))+(nsig*(nlat+2))
   rtmp = fbinrecread(filename,2,reclen,"float")
   is = 0
   ie = nsig*nsig*(nlat+2)-1
   agv = onedtond(rtmp(is:ie),(/nsig,nsig,nlat+2/))
   is = nsig*nsig*(nlat+2)
   ie = (nsig*nsig*(nlat+2))+(nsig*(nlat+2))-1
   bv  = onedtond(rtmp(is:ie),(/nsig,nlat+2/))
   is = (nsig*nsig*(nlat+2))+(nsig*(nlat+2))
   ie = (nsig*nsig*(nlat+2))+(nsig*(nlat+2))+(nsig*(nlat+2))-1
   wgv  = onedtond(rtmp(is:ie),(/nsig,nlat+2/))
   delete(rtmp)

   do i = 1, nvar_met
      irec = 4*(i-1)+3
      rtmp = fbinrecread(filename,irec,9,"character")  ; 5 bytes plus 4 bytes
      var = chartostring(rtmp(0:4))
      print("Reading "+var)
      delete(rtmp)

      irec = 4*(i-1)+4
      if ( var .eq. "q    " ) then
         reclen = 2*nlat*nsig
      else
         if ( var .ne. "ps   " ) then
            reclen = nlat*nsig
         else
            reclen = nlat
         end if
      end if
      rtmp = fbinrecread(filename,irec,reclen,"float")
      if ( var .eq. "q    " ) then
         cov(i-1,:,:) = onedtond(rtmp(0:nlat*nsig-1),(/nsig,nlat/))
         covq2(:,:) = onedtond(rtmp(nlat*nsig:2*nlat*nsig-1),(/nsig,nlat/))
         cov(i-1,:,:) = cov(i-1,:,:)*100.0
         covq2(:,:) = covq2(:,:)*100.0
      else
         if ( var .ne. "ps   " ) then
            cov(i-1,:,:) = onedtond(rtmp,(/nsig,nlat/))
            if ( var .eq. "sf   " .or. \
                 var .eq. "vp   " ) then
               cov(i-1,:,:) = cov(i-1,:,:) * 0.000001
            end if
         else
            cov(i-1,0,:) = (/rtmp/)
         end if
      end if
      delete(rtmp)

      if ( var .ne. "ps   " ) then
         irec = 4*(i-1)+5
         reclen = (nlat+2)*nsig
         rtmp = fbinrecread(filename,irec,reclen,"float")
         hwll(i-1,:,:) = onedtond(rtmp,(/nsig,nlat+2/))
         delete(rtmp)
         irec = 4*(i-1)+6
         reclen = (nlat+2)*nsig
         rtmp = fbinrecread(filename,irec,reclen,"float")
         vzs(i-1,:,:) = onedtond(rtmp,(/nlat+2,nsig/))
         delete(rtmp)
      else
         irec = 4*(i-1)+5
         reclen = nlat+2
         rtmp = fbinrecread(filename,irec,reclen,"float")
         hwll(i-1,0,:) = (/rtmp/)
         delete(rtmp)
      end if
   end do

   if ( read_aero ) then
      do i = nvar_met+1, nvar_tot
         irec = 4*(i-1)+2
         rtmp = fbinrecread(filename,irec,9,"character")  ; 5 bytes plus 4 bytes
         var = chartostring(rtmp(0:4))
         print("Reading "+var)
         delete(rtmp)

         irec = 4*(i-1)+3
         reclen = nlat*nsig
         rtmp = fbinrecread(filename,irec,reclen,"float")
         cov(i-1,:,:) = onedtond(rtmp,(/nsig,nlat/))
         delete(rtmp)

         irec = 4*(i-1)+4
         reclen = (nlat+2)*nsig
         rtmp = fbinrecread(filename,irec,reclen,"float")
         hwll(i-1,:,:) = onedtond(rtmp,(/nsig,nlat+2/))
         delete(rtmp)
         irec = 4*(i-1)+5
         reclen = (nlat+2)*nsig
         rtmp = fbinrecread(filename,irec,reclen,"float")
         vzs(i-1,:,:) = onedtond(rtmp,(/nlat+2,nsig/))
         delete(rtmp)
      end do
   end if

   hwll    = hwll*0.001 ; km
   do i = 0, nvar_tot-1
      do j = 0, nlat+1
         do k = 0, nsig-1
            if ( .not. ismissing(vzs(i,j,k)) ) then
               if ( vzs(i,j,k) .gt. 0.0 ) then
                  vzs(i,j,k) = 1.0/vzs(i,j,k)/dsig(k)
               else
                  vzs(i,j,k) = 0.0
               end if
            end if
         end do
      end do
   end do
   reg_t   = agv*1000000000.0
   reg_chi = bv
   reg_ps  = wgv*1000000000.0

   reg_t!0   = "lev"
   reg_t!1   = "lev"
   reg_t!2   = "lat"
   reg_t&lat = rlatp2

   reg_chi!0   = "lev"
   reg_chi!1   = "lat"
   reg_chi&lat = rlatp2

   reg_ps!0   = "lev"
   reg_ps!1   = "lat"
   reg_ps&lat = rlatp2

   if ( .not. By_Levels ) then
      reg_t&lev   = rsig
      reg_ps&lev  = rsig
      reg_chi&lev = rsig
   else
      reg_t&lev   = yc
      reg_ps&lev  = yc
      reg_chi&lev = yc
   end if

   ; averages (over latitudes) for each level
   cov_avg  = dim_avg_n_Wrap(cov, 2)  ; (nvar, nsig, nlat)->(nvar,nsig)
   hwll_avg = dim_avg_n_Wrap(hwll, 2) ; (nvar, nsig, nlat+2)->(nvar,nsig)
   vzs_avg  = dim_avg_n_Wrap(vzs, 1)  ; (nvar, nlat+2, nsig)->(nvar,nsig)

   ;----------------------
   ; start plotting
   ;----------------------
   wks = gsn_open_wks (out_type,plot_dir+"/"+out_name)  ; open workstation
   gsn_define_colormap(wks,"rainbow+white+gray")

   ;--------------------
   ; Plot Variance
   ;--------------------
   print(" 1. Plotting Standard Deviation")
   plts = new (nvar_tot,"graphic")

   res                       = True
   res@cnFillOn              = Fill_Color
   res@gsnSpreadColors       = True
   res@gsnSpreadColorStart   = 20
   res@gsnSpreadColorEnd     = -2
   res@lbLabelAutoStride     = True
   res@lbOrientation         = "Vertical"
   res@gsnDraw               = False
   res@gsnFrame              = False
   res@tmXTOn                = False
   res@tmYROn                = False
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse = True
      res@tmYLMode   = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
   end if
   res@tiXAxisString = "Latitude"
   res@tmXBMode         = tmMode
   if( tmMode .eq. "Explicit") then
      res@tmXBValues    = lat_values_for_plot
      res@tmXBLabels    = lat_array_for_plot
   end if
   res@gsnYAxisIrregular2Linear=True

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Standard Deviation"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0,nvar_met-2
      res@gsnLeftString = str_squeeze(varnames_met(kk))
      res@gsnRightString = ""
      if ( kk .eq. 0 .or. kk .eq. 1 ) then  ; sf, vp
         res@gsnRightString = "x 10**6"
      end if
      if ( kk .eq. 3 ) then  ; q
         res@gsnRightString = "x 0.01"
      end if
      plts(kk) = gsn_csm_contour (wks,cov(kk,:,:),res)
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      delete(res@gsnRightString)
      do kk = 0,nvar_aero-1
         res@gsnLeftString = str_squeeze(varnames_aero(kk))
         plts(kk+nvar_met) = gsn_csm_contour (wks,cov(kk+nvar_met,:,:),res)
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res)
   delete (resP)
   delete (plts)

   ;--------------------------------
   ; Plot Average Variance Profile
   ;--------------------------------
   print(" 2. Plotting Averaged Standard Deviation Profile")
   plts = new (nvar_tot,"graphic")

   res                    = True
   res@gsnDraw            = False
   res@gsnFrame           = False
   res@tiYAxisString      = "Sigma Values"
   res@xyLineThicknesses  = (/4.0,4.0,4.0,4.0/)
   res@xyDashPatterns     = (/0,4,2,3/)
   res@xyLineColors       = (/"blue"/)
   res@tmXTOn             = False
   res@tmYROn             = False
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse    = True
      res@tmYLMode      = "Explicit"
      res@tmYLValues    = sig_values_for_plot
      res@tmYLLabels    = sig_array_for_plot
   else
      res@trYMinF  =  1.0
      res@trYMaxF  =  nsig + 0.0
      res@tiYAxisString   = "Sigma Levels"
   end if

   res@lgPerimOn              = False
   res@lgLabelFontHeightF     = 0.02
   res@pmLegendDisplayMode    = "Always"
   res@pmLegendSide           = "Bottom"
   res@pmLegendParallelPosF   = 0.20
   res@pmLegendOrthogonalPosF = -0.45
   res@pmLegendWidthF         = 0.2
   res@pmLegendHeightF        = 0.2

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Averaged Standard Deviation Profile"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0, nvar_met-2
      res@xyExplicitLegendLabels = varnames_met(kk)
      res@tiXAxisString   = "Standard Deviation"
      if ( .not. By_Levels ) then
         plts(kk) = gsn_csm_xy (wks,cov_avg(kk,:),rsig,res)
      else
         plts(kk) = gsn_csm_xy (wks,cov_avg(kk,:),yc,res)
      end if
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      do kk = 0, nvar_aero-1
         res@xyExplicitLegendLabels = varnames_aero(kk)
         res@tiXAxisString   = "Standard Deviation"
         if ( .not. By_Levels ) then
            plts(kk+nvar_met) = gsn_csm_xy (wks,cov_avg(kk+nvar_met,:),rsig,res)
         else
            plts(kk+nvar_met) = gsn_csm_xy (wks,cov_avg(kk+nvar_met,:),yc,res)
         end if
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res )
   delete (resP )
   delete (plts)

   ;-----------------------------
   ; Plot Horizontal len scales
   ;-----------------------------
   print(" 3. Plotting Horizontal Length-scale")
   plts = new (nvar_tot,"graphic")

   res                       = True
   res@cnFillOn              = Fill_Color
   res@gsnSpreadColors       = True
   res@gsnSpreadColorStart   = 20
   res@gsnSpreadColorEnd     = -2
   res@lbLabelAutoStride     = True
   res@lbOrientation         = "Vertical"
   res@gsnDraw               = False
   res@gsnFrame              = False
   res@tmXTOn                = False
   res@tmYROn                = False
   res@gsnYAxisIrregular2Linear=True
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse = True
      res@tmYLMode   = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
   end if
   res@tiXAxisString = "Latitude"
   res@tmXBMode = tmMode
   if ( res@tmXBMode .eq. "Explicit" ) then
      res@tmXBValues = lat_values_for_plot
      res@tmXBLabels = lat_array_for_plot
   end if

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Horizontal Length-scale (Km)"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0, nvar_met-2
      res@gsnLeftString = str_squeeze(varnames_met(kk))
      plts(kk) = gsn_csm_contour (wks,hwll(kk,:,1:nlat),res)
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      do kk = 0, nvar_aero-1
         res@gsnLeftString = str_squeeze(varnames_aero(kk))
         plts(kk+nvar_met) = gsn_csm_contour (wks,hwll(kk+nvar_met,:,1:nlat),res)
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res)
   delete (resP)
   delete (plts)

   ;-------------------------------------
   ; Plot Avergaed Horizontal len scales
   ;-------------------------------------
   print(" 4. Plotting Averaged Horizontal Length-scale Profile")
   plts = new (nvar_tot,"graphic")

   res                    = True
   res@gsnDraw            = False
   res@gsnFrame           = False
   res@tiXAxisString      = "Horizontal Length-scale (Km)"
   res@xyLineThicknesses  = (/4.0,4.0,4.0,4.0/)
   res@xyLineColors       = (/"blue","blue","green","purple"/)
   res@xyDashPatterns     = (/0,4,2,3,4/)
   res@tmXTOn             = False
   res@tmYROn             = False
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse    = True
      res@tmYLMode      = "Explicit"
      res@tmYLValues    = sig_values_for_plot
      res@tmYLLabels    = sig_array_for_plot
   else
      res@trYMinF  =  1.0
      res@trYMaxF  =  nsig + 0.0
      res@tiYAxisString   = "Sigma Levels"
   end if

   res@lgPerimOn              = False
   res@lgLabelFontHeightF     = 0.02
   res@pmLegendDisplayMode    = "Always"
   res@pmLegendSide           = "Bottom"
   res@pmLegendParallelPosF   = 0.83
   res@pmLegendOrthogonalPosF = -0.40
   res@pmLegendWidthF         = 0.2
   res@pmLegendHeightF        = 0.2

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Averaged Horizontal Length-scale (km)"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0, nvar_met-2
      res@xyExplicitLegendLabels = varnames_met(kk)
      if ( .not. By_Levels ) then
         plts(kk) = gsn_csm_xy (wks,hwll_avg(kk,:),rsig,res)
      else
         plts(kk) = gsn_csm_xy (wks,hwll_avg(kk,:),yc,res)
      end if
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      do kk = 0, nvar_aero-1
         res@xyExplicitLegendLabels = varnames_aero(kk)
         if ( .not. By_Levels ) then
            plts(kk+nvar_met) = gsn_csm_xy (wks,hwll_avg(kk+nvar_met,:),rsig,res)
         else
            plts(kk+nvar_met) = gsn_csm_xy (wks,hwll_avg(kk+nvar_met,:),yc,res)
         end if
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res)
   delete (resP)
   delete (plts)

   ;---------------
   ; Plot Ps
   ;--------------- 
   print(" 5. Plotting Variance & Horizontal Length-scale for surface pressure")
   plts = new (2,"graphic")

   res                   = True
   res@gsnDraw           = False
   res@gsnFrame          = False
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)
   res@xyDashPatterns    = (/0,4,2,3/)
   res@xyLineColors      = (/"blue"/)
   res@tmXTOn            = False
   res@tmYROn            = False
   res@tiXAxisString   = "Latitude"
   res@tmXBMode = tmMode
   if ( res@tmXBMode .eq. "Explicit" ) then
      res@tmXBValues = lat_values_for_plot
      res@tmXBLabels = lat_array_for_plot
   end if
   res@tiYAxisString   = "Standard Deviation (hPa)"

   res@lgPerimOn              = False
   res@lgLabelFontHeightF     = 0.02
   res@pmLegendDisplayMode    = "Always"
   res@pmLegendSide           = "Bottom"
   res@pmLegendParallelPosF   = 0.75
   res@pmLegendOrthogonalPosF = -0.45
   res@pmLegendWidthF         = 0.2
   res@pmLegendHeightF        = 0.2
   res@xyExplicitLegendLabels = "ps_u"

   plts(0) = gsn_csm_xy(wks,rlat,cov(4,0,:),res)

   res@tiYAxisString   = "Lengthscale (Km)"
   plts(1) = gsn_csm_xy(wks,rlat,hwll(4,0,1:nlat),res)

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0
   gsn_panel(wks,plts,(/2,1/),resP)

   delete (res)
   delete (resP)
   delete (plts)

   ;--------------------------
   ; Plot Vertical len scales
   ;--------------------------
   print(" 6. Plotting Vertical Length-scale ")
   plts  = new (nvar_tot,"graphic")

   res                       = True
   res@gsnDraw               = False
   res@gsnFrame              = False
   res@cnFillOn              = Fill_Color
   res@gsnSpreadColors       = True
   res@gsnSpreadColorStart   = 20
   res@gsnSpreadColorEnd     = -2
   res@lbLabelAutoStride     = True
   res@lbOrientation         = "Vertical"
   res@tmXTOn                = False
   res@tmYROn                = False
   if ( .not. By_Levels ) then
      res@tiYAxisString   = "Sigma Values"
      res@trYReverse        = True
      res@tmYLMode = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiYAxisString   = "Sigma Levels"
   end if
   res@tiXAxisString = "Latitude"
   res@tmXBMode = tmMode
   if ( res@tmXBMode .eq. "Explicit" ) then
      res@tmXBMode      = "Explicit"
      res@tmXBValues    = lat_values_for_plot
      res@tmXBLabels    = lat_array_for_plot
   end if

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Vertical Length-scale (sigma units)"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0, nvar_met-2
      res@gsnLeftString         = str_squeeze(varnames_met(kk))
      plts(kk) = gsn_csm_contour(wks,vzs(var|kk,lev|:,lat|1:nlat),res)
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      do kk = 0, nvar_aero-1
         res@gsnLeftString = str_squeeze(varnames_aero(kk))
         plts(kk+nvar_met) = gsn_csm_contour(wks,vzs(var|(kk+nvar_met),lev|:,lat|1:nlat),res)
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res)
   delete (resP)
   delete (plts)

   ;-----------------------------------
   ; Plot Avergaed Vertical len scales
   ;-----------------------------------
   print(" 7. Plotting Averaged Vertical Length-scale Profile")
   plts = new (nvar_tot,"graphic")

   res                   = True
   res@gsnDraw           = False
   res@gsnFrame          = False
   res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)
   res@xyLineColors      = (/"blue","blue","green","purple"/)
   res@xyDashPatterns    = (/0,4,2,3,4/)
   res@tmXTOn            = False
   res@tmYROn            = False
   if ( .not. By_Levels ) then
      res@tiYAxisString  = "Sigma Values"
      res@trYReverse = True
      res@tmYLMode   = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
      res@trYMinF       =  1.0
      res@trYMaxF       =  nsig + 0.0
   end if
   res@tiXAxisString     = "Vertical Lengthscale (sigma units)"

   res@lgPerimOn              = False
   res@lgLabelFontHeightF     = 0.02
   res@pmLegendDisplayMode    = "Always"
   res@pmLegendSide           = "Bottom"
   res@pmLegendParallelPosF   = 0.85
   res@pmLegendOrthogonalPosF = -1.25
   res@pmLegendWidthF         = 0.2
   res@pmLegendHeightF        = 0.2

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@txString            = "Averaged Vertical Length-scale (sigma units)"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0

   do kk = 0, nvar_met-2
      res@xyExplicitLegendLabels = varnames_met(kk)
      if ( .not. By_Levels ) then
         plts(kk) = gsn_csm_xy (wks,vzs_avg(kk,:),rsig,res)
      else
         plts(kk) = gsn_csm_xy (wks,vzs_avg(kk,:),yc,res)
      end if
   end do
   gsn_panel(wks,plts(0:nvar_met-2),(/2,2/),resP)

   if ( read_aero ) then
      do kk = 0, nvar_aero-1
         res@xyExplicitLegendLabels = varnames_aero(kk)
         if ( .not. By_Levels ) then
            plts(kk+nvar_met) = gsn_csm_xy (wks,vzs_avg(kk+nvar_met,:),rsig,res)
         else
            plts(kk+nvar_met) = gsn_csm_xy (wks,vzs_avg(kk+nvar_met,:),yc,res)
         end if
      end do
      gsn_panel(wks,plts(nvar_met:nvar_tot-1),plot_dims_aero,resP)
   end if

   delete (res)
   delete (resP)
   delete (plts)

   ;-----------------------------------
   ; Plot Chi Regression coefficients
   ;-----------------------------------
   print(" 8. Plotting Chi Regression coefficients")
   plts = new (2,"graphic")

   res                       = True
   res@gsnMaximize           = True
   res@gsnDraw               = False
   res@gsnFrame              = False
   res@tmXTOn                = False
   res@tmYROn                = False
   res@cnFillOn              = Fill_Color
   res@lbLabelAutoStride     = True
   res@lbOrientation         = "Vertical"
   res@gsnSpreadColors       = True
   res@gsnSpreadColorStart   = 20
   res@gsnSpreadColorEnd     = -2
   res@gsnYAxisIrregular2Linear=True
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse = True
      res@tmYLMode   = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
      res@trYMinF       =  1.0
      res@trYMaxF       =  nsig + 0.0
   end if
   res@tiXAxisString = "Latitude"
   res@tmXBMode = tmMode
   if ( res@tmXBMode .eq. "Explicit" ) then
      res@tmXBValues = lat_values_for_plot
      res@tmXBLabels = lat_array_for_plot
   end if
   res@gsnLeftString   = "Chi - Regression Coefficients"
   plts(0) = gsn_csm_contour (wks,reg_chi(:,1:nlat),res)
   delete (res)

   ;--------------------------------------------------------------------
   ;  Plot averaged Vertical profile of Chi regression coeff
   ;--------------------------------------------------------------------

   reg_chi_avg = dim_avg_n_Wrap(reg_chi,1)

   res                   = True
   res@gsnMaximize       = True
   res@gsnDraw           = False
   res@gsnFrame          = False
   res@tmXTOn            = False
   res@tmYROn            = False
   res@xyLineThicknesses = (/4.0,4.0/)
   res@xyDashPatterns    = (/0,4/)
   res@xyLineColors      = (/"blue"/)

   res@tiXAxisString = "Regression Coefficient"
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse    = True
      res@tmYLMode      = "Explicit"
      res@tmYLValues    = sig_values_for_plot
      res@tmYLLabels    = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
      res@trYMinF       =  1.0
      res@trYMaxF       =  nsig + 0.0
   end if

   res@xyExplicitLegendLabels = "chi_u"
   res@gsnLeftString   = "Chi - Averaged Regression Coefficients"
   if ( .not. By_Levels ) then
      plts(1) = gsn_csm_xy (wks,reg_chi_avg,rsig,res)
   else
      plts(1) = gsn_csm_xy (wks,reg_chi_avg,yc,res)
   end if

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0
   resP@gsnPanelBottom      = 0.10
   gsn_panel(wks,plts,(/2,1/),resP)

   delete (res)
   delete (resP)
   delete (plts)

   ;------------------------------------
   ; Plot Psfc Regression coefficients
   ;------------------------------------
   print(" 9. Plotting Psfc Regression coefficients")
   plts = new (2,"graphic")

   res                      = True
   res@gsnMaximize          = True
   res@gsnDraw              = False
   res@gsnFrame             = False
   res@tmXTOn               = False
   res@tmYROn               = False
   res@cnFillOn             = Fill_Color
   res@lbLabelAutoStride     = True
   res@lbOrientation         = "Vertical"
   res@gsnSpreadColors      = True
   res@gsnSpreadColorStart  = 20
   res@gsnSpreadColorEnd    = -2
   res@gsnYAxisIrregular2Linear=True

   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse    = True
      res@tmYLMode      = "Explicit"
      res@tmYLValues    = sig_values_for_plot
      res@tmYLLabels    = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
      res@trYMinF       =  1.0
      res@trYMaxF       =  nsig + 0.0
   end if
   res@tiXAxisString = "Latitude"
   res@tmXBMode      = tmMode
   if ( res@tmXBMode .eq. "Explicit" ) then
      res@tmXBValues = lat_values_for_plot
      res@tmXBLabels = lat_array_for_plot
   end if

   res@gsnLeftString = "10**9 x Psfc-Regression Coefficients"
   plts(0) = gsn_csm_contour (wks,reg_ps(:,1:nlat),res)
   delete (res)

   ;--------------------------------------------------------------------
   ;  Plot Vertical profile of Psfc regression coeff
   ;--------------------------------------------------------------------
   reg_ps_avg = dim_avg_Wrap(reg_ps)

   res                   = True
   res@gsnMaximize       = True
   res@gsnDraw           = False
   res@gsnFrame          = False
   res@tmXTOn            = False
   res@tmYROn            = False
   res@xyLineThicknesses = (/4.0,4.0/)
   res@xyDashPatterns    = (/0,4/)
   res@xyLineColors      = (/"blue"/)

   res@tiXAxisString = "Regression Coefficient"
   if ( .not. By_Levels ) then
      res@tiYAxisString = "Sigma Values"
      res@trYReverse    = True
      res@tmYLMode      = "Explicit"
      res@tmYLValues    = sig_values_for_plot
      res@tmYLLabels    = sig_array_for_plot
   else
      res@tiYAxisString = "Sigma Levels"
      res@trYMinF       =  1.0
      res@trYMaxF       =  nsig + 0.0
   end if

   res@xyExplicitLegendLabels = "ps"
   res@gsnLeftString   = "10**9 x Psfc - Averaged Regression Coefficients"
   if ( .not. By_Levels ) then
      plts(1) = gsn_csm_xy (wks,reg_ps_avg,rsig,res)
   else
      plts(1) = gsn_csm_xy (wks,reg_ps_avg,yc,res)
   end if

   resP                     = True
   resP@gsnMaximize         = True
   resP@gsnPaperOrientation = "portrait"
   resP@gsnPanelYWhiteSpacePercent = 5.0  ; default 1.0
   resP@gsnPanelBottom      = 0.10
   gsn_panel(wks,plts,(/2,1/),resP)

   delete (res)
   delete (resP)
   delete (plts)

   ;------------------------------------------
   ; Plot Temperature Regression coefficients
   ;------------------------------------------
   print("10. Plotting Temperature Regression coefficients")

   res                     = True
   res@cnFillOn            = Fill_Color
   res@gsnSpreadColors     = True
   res@gsnSpreadColorStart = 20
   res@gsnSpreadColorEnd   = -2
   res@lbLabelAutoStride   = True
   res@lbOrientation       = "Vertical"
   res@gsnDraw             = False
   res@gsnFrame            = False
   res@tmXTOn              = False
   res@tmYROn              = False
   res@gsnXAxisIrregular2Linear=True
   res@gsnYAxisIrregular2Linear=True
   if ( .not. By_Levels ) then
      ; Reverses X-axis
      res@tiXAxisString = "Sigma Values"
      res@tiYAxisString = "Sigma Values"
      res@trXReverse = True
      res@tmXBMode   = "Explicit"
      res@tmXBValues = sig_values_for_plot
      res@tmXBLabels = sig_array_for_plot
      ; Reverses Y-axis
      res@trYReverse = True
      res@tmYLMode   = "Explicit"
      res@tmYLValues = sig_values_for_plot
      res@tmYLLabels = sig_array_for_plot
   else
      res@tiXAxisString = "Sigma Levels"
      res@tiYAxisString = "Sigma Levels"
   end if

   res@gsnLeftString = "10**9 x Averaged Temp Regression coeff "
   plts = gsn_csm_contour (wks,dim_avg_Wrap(reg_t),res) ; create plot
   draw(plts)
   frame(wks)

   delete (res)
   delete(plts)
   destroy(wks)
