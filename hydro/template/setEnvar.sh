#!/bin/bash

# WRF-Hydro compile time options

# This is a WRF environment variable. Always set to 1=On for compiling WRF-Hydro.
export WRF_HYDRO=1

# Enhanced diagnostic output for debugging: 0=Off, 1=On.
export HYDRO_D=0

# Spatially distributed parameters for NoahMP: 0=Off, 1=On.
export SPATIAL_SOIL=0

# RAPID model: 0=Off, 1=On.
export WRF_HYDRO_RAPID=0

# WCOSS file units: 0=Off, 1=On. 
export NCEP_WCOSS=0

# Streamflow nudging: 0=Off, 1=On.
export WRF_HYDRO_NUDGING=0
