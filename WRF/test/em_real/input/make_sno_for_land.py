#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb  8 14:59:05 2021

@author: varun
"""
import os
import glob
import sys
import platform
import numpy as np
import datetime as dt
import xarray as xr
from scipy import spatial
from scipy import interpolate
import fileinput
import matplotlib.pyplot as plt

def make_sno_file(loc_lat,loc_lon,in_i,in_j,loc_hgt,loc_snowh,soil_thickness,soil_T,soil_loc_w,
                  soil_loc_v,soil_loc_s,no_soil_layers,t_snow,loc_vol_i,loc_vol_v,no_snow_lays,
                  loc_soildens,loc_hcond,loc_hcap,loc_gr,loc_alb,loc_z0,datestr,soil_loc_i) :
                  
    fn_out = "./snpack_" + str(1) + "_" + str(in_j+1) + "_" + str(in_i+1) + ".sno"
    if(loc_snowh > 0):
        loc_snlayers=30
    else:
        loc_snlayers=0
    file = open(fn_out,"w")
    file.write("SMET 1.1 ASCII\n")
    file.write("[HEADER]\n")
    file.write("station_id       = snpack_"+str(1)+"_"+str(in_j+1) + "_" + str(in_i+1)+"\n") # very important - notice how j is printed first and then i. also note the +1 for each j and i
    file.write("station_name     = Swiss:WRF\n")
    file.write("latitude         = " + "%.4f" % loc_lat + "\n")
    file.write("longitude        = " + "%.4f" % loc_lon + "\n")
    file.write("altitude         = " + "%.2f" % loc_hgt + "\n")
    file.write("nodata           = -999\n")
    file.write("source           = COSMO_to_WRF; VSharma, " + dt.datetime.now().strftime("%Y-%m-%d") + "\n")
    file.write("ProfileDate      = " + datestr.isoformat()[:] + "\n")
    file.write("HS_Last          = " + "%.6f" % loc_snowh + "\n") # [m]
    file.write("SlopeAngle       = 0.00\n")
    file.write("SlopeAzi         = 0.00\n")
    file.write("nSoilLayerData   = 6\n")
    file.write("nSnowLayerData   = " + str(loc_snlayers) + "\n")
    file.write("SoilAlbedo       = " + str(loc_alb) + "\n")
    file.write("BareSoil_z0      = " + str(loc_z0)  + "\n")
    file.write("CanopyHeight     = 0.00\n")
    file.write("CanopyLeafAreaIndex = 0.00\n")
    file.write("CanopyDirectThroughfall = 1.00\n")
    file.write("WindScalingFactor = 1.00\n")
    file.write("ErosionLevel     = "  + str(loc_snlayers) + "\n")
    file.write("TimeCountDeltaHS = 0.000000\n")
    file.write("fields           = timestamp Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V  Vol_Frac_S Rho_S Conduc_S HeatCapac_S  rg  rb  dd  sp  mk mass_hoar ne CDot metamo\n")
    file.write("[DATA]\n")
    for m in range(6):
        file.write(dt.datetime(1982, 7, 1, 0).isoformat()[:] + "   " +
                   "%6.3f" % soil_thickness[m] + "   " +  # layer-thickness [m]
                   "%7.3f" % soil_T[m] + "   " +   # temperature [K]
                   "%5.3f" % soil_loc_i[m] + "   " +   # volume fraction (ice) [-]
                   "%5.3f" % soil_loc_w[m] + "   " +           # volume fraction (water) [-]
                   "%5.3f" % soil_loc_v[m] + "   " +      # volume fraction (air) [-]
                   "%5.3f" % soil_loc_s[m] + "   " +           # volume fraction (soil) [-]
                   "%8.3f" % loc_soildens + "   " +           # soil dry dens. [kg m-3]
                   "%6.3f" % loc_hcond   + "   " +           # soil heat cond. [W m-1 K-1]
                   "%8.3f" % loc_hcap + "   " +           # soil heat capacity [J m-3 K-1]
                   "%9.3f" % (loc_gr) + "   " +                 # grain radius [mm]
                   "%9.3f" % (0.0) + "   " +             # bond radius [mm]
                   "%5.3f" % 0.0 + "   " +            # dendricity (0: old snow) [-]
                   "%5.3f" % 1.0 + "   " +            # sphericity (1: rounded) [-]
                   str(1) + "   " +                   # grain marker [-] (set to 7 -> glacier ice ?) ##########################
                   str(0.0) + "   " +                 # Mass of surface hoar [kg m-2]
                   str(no_soil_layers[m]) + "   " +             # Numb. of fin. elem. in layer [-]
                   str(0.0) + "   " +                 # Stress rate [Pa s-1]
                   str(0.0) + "   " +  "\n")          # Keep track of metamorphism [-]
    if(loc_snowh > 0):
        for m in range(30):
            file.write(datestr.isoformat()[:] + "   " +
                       "%6.3f" % (loc_snowh/30.0) + "   " +  # layer-thickness [m]
                       "%7.3f" % t_snow[m] + "   " +   # temperature [K]
                       "%5.3f" % loc_vol_i + "   " +   # volume fraction (ice) [-]
                       "%5.3f" % 0.0 + "   " +           # volume fraction (water) [-]
                       "%5.3f" % loc_vol_v + "   " +      # volume fraction (air) [-]
                       "%5.3f" % 0.0 + "   " +           # volume fraction (soil) [-]
                       "%8.3f" % 0.0 + "   " +           # soil dry dens. [kg m-3]
                       "%6.3f" % 0.0    + "   " +           # soil heat cond. [W m-1 K-1]
                       "%8.3f" % 0.0 + "   " +           # soil heat capacity [J m-3 K-1]
                       "%9.3f" % (0.4) + "   " +                 # grain radius [mm]
                       "%9.3f" % (0.4*0.25) + "   " +             # bond radius [mm]
                       "%5.3f" % 0.0 + "   " +            # dendricity (0: old snow) [-]
                       "%5.3f" % 1.0 + "   " +            # sphericity (1: rounded) [-]
                       str(1) + "   " +                   # grain marker [-] (set to 7 -> glacier ice ?) ##########################
                       str(0.0) + "   " +                 # Mass of surface hoar [kg m-2]
                       str(int(no_snow_lays[m])) + "   " +             # Numb. of fin. elem. in layer [-]
                       str(0.0) + "   " +                 # Stress rate [Pa s-1]
                       str(0.0) + "   " +  "\n")          # Keep track of metamorphism [-]
    file.close()
    print("INPUT FILE: " + fn_out + "  created !")

## END OF FILE WRITING FUNCTION

               
wrf_input = xr.open_mfdataset('/home/varun/WRF/install_folder/code/WRF/test/em_real/wrfinput_d01')
lon_wrf  = wrf_input["XLONG"].values[0,:,:]
lat_wrf  = wrf_input["XLAT"].values[0,:,:]

datestr = dt.datetime.strptime(str(wrf_input.attrs['JULYR'])+ ' ' + str(wrf_input.attrs['JULDAY']), '%Y %j')
hgt = wrf_input["HGT"].values[0,:,:]
TSK = wrf_input["TSK"].values[0,:,:]

start_date = wrf_input.attrs['START_DATE']

met_file = xr.open_mfdataset('/home/varun/WRF/install_folder/code/WRF/test/em_real/'+'met_em.d01.'+start_date+'.nc')


T_SOIL=np.concatenate((met_file['T_SO0162'].values, \
                       met_file['T_SO0054'].values, \
                       met_file['T_SO0018'].values, \
                       met_file['T_SO0006'].values, \
                       met_file['T_SO0002'].values, \
                       met_file['T_SO0001'].values),axis=0)

W_SOIL=np.concatenate((met_file['W_SO0162'].values, \
                       met_file['W_SO0054'].values, \
                       met_file['W_SO0018'].values, \
                       met_file['W_SO0006'].values, \
                       met_file['W_SO0002'].values, \
                       met_file['W_SO0001'].values),axis=0)    

lu_index = met_file['LU_INDEX'].values[0,:,:]    
soil_layer_thickness = np.array([1.62,0.54,0.18,0.06,0.02,0.01])   

soil_layer_ne = np.array([3,3,3,2,2,1])

# Convert units of W_SOIL from m H2O to m3/m3
W_SOIL = W_SOIL/(1000.0 * soil_layer_thickness[:,None,None])

SOIL_VOL_W = np.zeros(np.shape(W_SOIL))
SOIL_VOL_S = np.ones(np.shape(W_SOIL)) * 0.7
SOIL_VOL_V = 1.0 - SOIL_VOL_S
SOIL_VOL_I = np.zeros(np.shape(W_SOIL))
SOIL_VOL_W = np.minimum(W_SOIL,SOIL_VOL_V)
SOIL_VOL_V = SOIL_VOL_V - SOIL_VOL_W

# SNOW LAYERS
SNOW  = met_file['SNOW'].values[0,:,:]
SNOWH = met_file['SNOWH'].values[0,:,:]

# LIMIT SNOWH TO only those more than 3 cm
SNOWH[SNOWH <= 0.03] = 0.0


SKINTEMP   = met_file['SKINTEMP'].values[0,:,:]
T_SOIL_TOP = met_file['T_SO0001'].values[0,:,:]


RHO_SNOW = SNOW/SNOWH
VOL_I = RHO_SNOW/918.0
VOL_V = 1.0 - VOL_I

VOL_W = np.zeros(np.shape(VOL_V))
VOL_S = np.zeros(np.shape(VOL_V))

T_SNOW = np.linspace(T_SOIL_TOP,SKINTEMP,num=30,endpoint=True) # number of snow layers = 30
T_SNOW = np.minimum(T_SNOW,273.15)

no_snow_lays = np.ones([30])
no_snow_lays[-2::]=2

# shape of lat_wrf
max_x_wrf,max_y_wrf = np.shape(lat_wrf)

#i=153
#j=57
for i in range(max_x_wrf):
    for j in range(max_y_wrf):
        loc_lat = lat_wrf[i,j]
        loc_lon = lon_wrf[i,j]
        loc_hgt = hgt[i,j]
        loc_snowh = SNOWH[i,j]
        soil_T = T_SOIL[:,i,j]
        soil_loc_w = SOIL_VOL_W[:,i,j]
        soil_loc_v = SOIL_VOL_V[:,i,j]
        soil_loc_s = SOIL_VOL_S[:,i,j]
        soil_loc_i = SOIL_VOL_I[:,i,j]
        loc_snow_t = T_SNOW[:,i,j]
        loc_vol_i = VOL_I[i,j]
        loc_vol_v = VOL_V[i,j]
        
        loc_soildens = 2400.0
        loc_hcond = 2.0
        loc_hcap = 2000.0
        loc_gr = 1000.0
        loc_z0 = 0.02
        loc_alb = 0.2
        
        if(lu_index[i,j] == 16):
            print( ' water point: ' + str(j) + ',' + str(i) )
            loc_soildens = 1000.0
            loc_hcond = 0.6
            loc_hcap = 4217.0
            loc_gr = 10000.0
            loc_alb = 0.4
            loc_z0 = 0.0002
            soil_loc_v[:] = 0.01
            soil_loc_w[:] = 0.00
            soil_loc_s[:] = 0.99
            soil_loc_i[:] = 0.0

            soil_T[:] = np.maximum(SKINTEMP[i,j],278.15)
            
        make_sno_file(loc_lat,loc_lon,i,j,loc_hgt, loc_snowh,   \
                      soil_layer_thickness, soil_T, soil_loc_w, \
                      soil_loc_v, soil_loc_s, soil_layer_ne, loc_snow_t, \
                      loc_vol_i, loc_vol_v, no_snow_lays, loc_soildens,loc_hcond \
                      ,loc_hcap,loc_gr,loc_alb,loc_z0,datestr,soil_loc_i)      

    
# MAKE COSMO.smet from template.smet
sys_command = 'cp template.smet COSMO.smet'   
os.system(sys_command)

tmpdate = datestr

for line in fileinput.input('./COSMO.smet',inplace=True):
    if line.startswith('station_id'):
        print( line.replace( line.split("=")[1], ' ' + 'Swiss:WRF' ),end='\n' )
    elif line.startswith('station_name'):
        print( line.replace( line.split("=")[1], ' ' + 'Swiss:WRF' ),end='\n' )
    elif line.startswith('2019-12-01'):  
        print( line.replace( line.split(" ")[0], tmpdate.isoformat()[:] ),end='' )
        tmpdate = tmpdate + dt.timedelta(minutes=15)
    else:
        print(line,end='')
