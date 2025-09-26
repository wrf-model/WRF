#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep  8 20:32:39 2019

@author: vsharma
"""

import os
import glob
import sys
import platform
import numpy as np
import datetime as dt
from netCDF4 import Dataset
from scipy import spatial
from scipy import interpolate


"""
A function that writes out a .sno file suitable to be read into WRF
INPUT: loc_dens-local density profile, loc_temp-local temperature profile,thickness-layer size
       loc_v-local vapor fraction profile, loc_lat,loc_lon - local lat/lon
OUTPUT: NO OUTPUT
"""
def make_sno_file(loc_dens,loc_temp,thickness,loc_v,loc_lat,loc_lon,in_i,in_j,loc_hgt,grainNEW):
    numb_elem = int(len(loc_dens)) 
        # Write text file in format .met
    fn_out = "./snpack_" + str(1) + "_" + str(in_j+1) + "_" + str(in_i+1) + ".sno" # VERY IMPORTANT - notice how j is printed first and then i. Also note the +1 for each j and i
    file = open(fn_out,"w")
    file.write("SMET 1.1 ASCII\n")
    file.write("[HEADER]\n")
    file.write("station_id       = snpack_"+str(1)+"_"+str(in_j+1) + "_" + str(in_i+1)+"\n") # VERY IMPORTANT - notice how j is printed first and then i. Also note the +1 for each j and i
    file.write("station_name     = Antarctica:domeA_erodible\n")
    file.write("latitude         = " + "%.4f" % loc_lat + "\n")
    file.write("longitude        = " + "%.4f" % loc_lon + "\n")
    file.write("altitude         = " + "%.2f" % loc_hgt + "\n")
    file.write("nodata           = -999\n")
    file.write("source           = IMAU_to_WRF; VSharma, " + dt.datetime.now().strftime("%Y-%m-%d") + "\n")
    file.write("ProfileDate      = " + dt.datetime(2010,7, 1, 0).isoformat()[:] + "\n")
    file.write("HS_Last          = " + "%.6f" % np.sum(thickness) + "\n") # [m]
    file.write("SlopeAngle       = 0.00\n")
    file.write("SlopeAzi         = 0.00\n")
    file.write("nSoilLayerData   = 0\n")
    file.write("nSnowLayerData   = " + str(numb_elem) + "\n")
    file.write("SoilAlbedo       = 0.09\n")
    file.write("BareSoil_z0      = 0.020\n")
    file.write("CanopyHeight     = 0.00\n")
    file.write("CanopyLeafAreaIndex = 0.00\n")
    file.write("CanopyDirectThroughfall = 1.00\n")
    file.write("WindScalingFactor = 1.00\n")
    file.write("ErosionLevel     = "  + str(numb_elem - 1) + "\n")
    file.write("TimeCountDeltaHS = 0.000000\n")
    file.write("fields           = timestamp Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V  Vol_Frac_S Rho_S Conduc_S HeatCapac_S  rg  rb  dd  sp  mk mass_hoar ne CDot metamo\n")
    file.write("[DATA]\n")
    for m in range(numb_elem):
        if(numb_elem-m-1<2):
          no_lays=5
        elif (numb_elem-m-1>=2) and (numb_elem-m-1<5) :
          no_lays=2
        else:
          no_lays=1
        file.write(dt.datetime(1982, 7, 1, 0).isoformat()[:] + "   " +
                    "%6.3f" % thickness[m] + "   " +    # layer-thickness [m]
                    "%7.3f" % loc_temp[m] + "   " +   # temperature [K]
                    "%5.3f" % loc_dens[m] + "   " +            # volume fraction (ice) [-]
                    "%5.3f" % 0.0 + "   " +            # volume fraction (water) [-]
                    "%5.3f" % loc_v[m] + "   " +            # volume fraction (air) [-]
                    "%5.3f" % 0.0 + "   " +            # volume fraction (soil) [-]
                    "%8.3f" % 0.0 + "   " +         # soil dry dens. [kg m-3]
                    "%6.3f" % 0.0 + "   " +            # soil heat cond. [W m-1 K-1]
                    "%8.3f" % 0.0 + "   " +        # soil heat capacity [J m-3 K-1]
                    "%9.3f" % (grainNEW[m]*1000.0) + "   " +             # grain radius [mm]
                    "%9.3f" % (grainNEW[m]*1000.0*0.2) + "   " +             # bond radius [mm]
                    "%5.3f" % 0.0 + "   " +            # dendricity (0: old snow) [-]
                    "%5.3f" % 1.0 + "   " +            # sphericity (1: rounded) [-]
                    str(1) + "   " +                   # grain marker [-] (set to 7 -> glacier ice ?) ##########################
                    str(0.0) + "   " +                 # Mass of surface hoar [kg m-2]
                    str(no_lays) + "   " +                   # Numb. of fin. elem. in layer [-]
                    str(0.0) + "   " +                 # Stress rate [Pa s-1]
                    str(0.0) + "   " +  "\n")          # Keep track of metamorphism [-]
    file.close()
    print("Input file " + fn_out + " created")

# Loading Density file from IMAU
imau_files = Dataset('/project/s938/gerberf/SNOWPACK_WRF_input/FDM/ANT27_dens_July_2010.nc','r')
lon_imau  = imau_files.variables["lon"][:]
lat_imau  = imau_files.variables["lat"][:]
dens_imau = imau_files.variables["dens"][:]
dens_mask = np.ma.getmask(dens_imau[0,:,:])
lat_masked = np.ma.masked_array(lat_imau, mask=dens_mask)
lon_masked = np.ma.masked_array(lon_imau, mask=dens_mask)
lat_masked.set_fill_value(-999)
lon_masked.set_fill_value(-999)

lat_masked = lat_masked.filled()
lon_masked = lon_masked.filled()

depth = imau_files.variables["depth"][:]
depth = depth[0:400]
depth = depth[::-1]    
thickness = -1*np.diff(depth)

imau_files.close()

imau_files = Dataset('/project/s938/gerberf/SNOWPACK_WRF_input/FDM/ANT27_temp_July_2010.nc','r')
temp_imau = imau_files.variables["temp"][:]
imau_files.close()

wrf_files = Dataset('../wrfinput_d01','r')
lon_wrf  = wrf_files.variables["XLONG"][0,:,:]
lat_wrf  = wrf_files.variables["XLAT"][0,:,:]
isltyp = wrf_files.variables["ISLTYP"][0,:,:]
hgt = wrf_files.variables["HGT"][0,:,:]

kkk = Dataset('/project/s938/gerberf/SNOWPACK_WRF_input/FDM/dz_RACMO2.3p2_ANT27_Jul2010.nc','r')
dz = kkk.variables["dz"][:]
dz = np.flip(dz,axis=1)
depth_grainsize = np.cumsum(dz,axis=1)
depth_grainsize = np.flip(depth_grainsize,axis=1)
depth_grainsize = np.squeeze(depth_grainsize)
kkk.close()

kkk = Dataset('/project/s938/gerberf/SNOWPACK_WRF_input/FDM/grainsize_RACMO2.3p2_ANT27_Jul2010.nc','r')
grainR = kkk.variables["grainsize"][:]
grainR = np.squeeze(grainR)
kkk.close()

#tree = spatial.KDTree(list(zip(lon_imau.ravel(),lat_imau.ravel())))
tree = spatial.KDTree(list(zip(lon_masked.ravel(),lat_masked.ravel())))

max_x_wrf,max_y_wrf = np.shape(lat_wrf)
max_x_imau,max_y_imau = np.shape(lat_imau)

for i in range(max_x_wrf):
   for j in range(max_y_wrf):
       loc_lat = lat_wrf[i,j]
       loc_lon = lon_wrf[i,j]
       loc_hgt = hgt[i,j]
       if (isltyp[i,j] == 8):
          pts = np.array([[loc_lon, loc_lat]])
          _,loc_id  = tree.query(pts[0])
          loc_x_imau,loc_y_imau = np.unravel_index(loc_id,(max_x_imau,max_y_imau))
          loc_dens = dens_imau[:,loc_x_imau,loc_y_imau]
          loc_dens = loc_dens[0:400]
          loc_dens = loc_dens[::-1]
          loc_dens = loc_dens[1:400]
          loc_dens = loc_dens / 920.0
          loc_v = 1.0 - loc_dens
          loc_temp = temp_imau[:,loc_x_imau,loc_y_imau]
          loc_temp = loc_temp[0:400]
          loc_temp = loc_temp[::-1]
          loc_temp = loc_temp[1:400]
          f = interpolate.interp1d(depth_grainsize[:,loc_x_imau,loc_y_imau],
                                   grainR[:,loc_x_imau,loc_y_imau],bounds_error=False,
                                   fill_value=grainR[0,loc_x_imau,loc_y_imau])          
          grainNEW = f(depth.data)
          make_sno_file(loc_dens,loc_temp,thickness,loc_v,loc_lat,loc_lon,i,j,loc_hgt,grainNEW)
       #print(loc_lat,loc_lon,lat_imau[loc_x_imau,loc_y_imau],lon_imau[loc_x_imau,loc_y_imau])
       
