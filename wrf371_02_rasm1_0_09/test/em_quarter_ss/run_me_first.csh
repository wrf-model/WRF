#!/bin/csh

echo Setting up quarter_ss case by linking data files into this directory

echo linking to RRTM_DATA in ../../run directory
ln -sf ../../run/RRTM_DATA .

echo done
