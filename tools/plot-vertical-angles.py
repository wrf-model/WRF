#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""

Utility to plot the maximum vertical angle between two adjacent grid points.

This is a useful tool to check vertical levels configuration.
Angles exceeding 45° should be avoided.
The angle can be reduced by adding vertical levels or by topology smoothing.


BACKGROUND
Complex topography and high-resolution can lead to steep angles between grid points.
Mahrer (1984) notes that numerical errors in the calculation of horizontal derivatives occur when large grid aspect ratios are used with sloping coordinates, and states that the minimum vertical grid spacing must be larger than the elevation difference over a grid cell.

Mahrer, Y., 1984: An improved numerical approximation of the horizontal gradients in a terrain-following coordinate system. Mon. Wea. Rev., 112, 918–922, doi:https://doi.org/10.1175/1520-0493(1984)112<0918:AINAOT>2.0.CO;2


REQUIREMENTS
pip3 install wrf-python matplotlib


USAGE
python3 ./plot-vertical-angles.py [WRFOUT FILE]


NOTES
For visualisation, Y coordinates are reversed.


AUTHOR
Nicolas Baldeck - n@baldeck.net - 2019
https://openmeteodata.com

"""


from netCDF4 import Dataset
from wrf import getvar
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import sys

filename = sys.argv[1]

wrfout = Dataset(filename)

height = getvar(wrfout, 'height')
nz, ny, nx = height.shape

dh = np.sqrt(wrfout.DX**2 + wrfout.DY**2)

dz_x = np.diff(height, n=1, axis=1)
dz_y = np.diff(height, n=1, axis=2)

angle_x = np.abs(np.arctan2(dz_x, dh) * 180 / np.pi)
angle_y = np.abs(np.arctan2(dz_y, dh) * 180 / np.pi)

max_x = np.amax(angle_x, axis=0)
max_y = np.amax(angle_y, axis=0)

cmap = plt.get_cmap('jet')
norm = mpl.colors.BoundaryNorm(np.arange(0, 55, 5), cmap.N)


plt.figure(1)
plt.title('Max vert angle on X (°), max=%s' % np.max(max_x))
im = plt.imshow(np.flip(max_x[:,:], axis=0), cmap=cmap, norm=norm)
plt.colorbar(im)


plt.figure(2)
plt.title('Max vert angle on Y (°), max=%s' % np.max(max_y))
im2 = plt.imshow(np.flip(max_y[:,:], axis=0), cmap=cmap, norm=norm)
plt.colorbar(im2)

plt.show()

