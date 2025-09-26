GRIB | Level| Level| Level| metgrid  |  metgrid | metgrid                                  |
Code | Code |   1  |   2  | Name     |  Units   | Description                              |
-----+------+------+------+----------+----------+------------------------------------------+
 130 | 109  |   *  |      | TT       | K        | Temperature                              |
 131 | 109  |   *  |      | UU       | m s-1    | U                                        |
 132 | 109  |   *  |      | VV       | m s-1    | V                                        |
 133 | 109  |   *  |      | SPECHUMD | kg kg-1  | Specific humidity                        |
 165 |  1   |   0  |      | UU       | m s-1    | U                                        | At 10 m
 166 |  1   |   0  |      | VV       | m s-1    | V                                        | At 10 m
 167 |  1   |   0  |      | TT       | K        | Temperature                              | At 2 m
 168 |  1   |   0  |      | DEWPT    | K        |                                          | At 2 m
     |  1   |   0  |      | RH       | %        | Relative Humidity at 2 m                 | At 2 m
 172 |  1   |   0  |      | LANDSEA  | 0/1 Flag | Land/Sea flag                            |
 129 |  1   |   0  |      | SOILGEO  | m2 s-2   |                                          |
     |  1   |   0  |      | SOILHGT  | m        | Terrain field of source analysis         |
 134 |  1   |   0  |      | PSFC     | Pa       | Surface Pressure                         |
 151 |  1   |   0  |      | PMSL     | Pa       | Sea-level Pressure                       |
 235 |  1   |   0  |      | SKINTEMP | K        | Sea-Surface Temperature                  |
  31 |  1   |   0  |      | SEAICE   | fraction | Sea-Ice-Fraction                         |
  34 |  1   |   0  |      | SST      | K        | Sea-Surface Temperature                  |
  33 |  1   |   0  |      | SNOW_DEN | kg m-3   |                                          |
 141 |  1   |   0  |      | SNOW_EC  | m        |                                          |
     |  1   |   0  |      | SNOW     | kg m-2   |Water Equivalent of Accumulated Snow Depth|
     |  1   |   0  |      | SNOWH    | m        | Physical Snow Depth                      |
 139 | 112  |   0  |   7  | ST000007 | K        | T of 0-7 cm ground layer                 |
 170 | 112  |   7  |  28  | ST007028 | K        | T of 7-28 cm ground layer                |
 183 | 112  |  28  | 100  | ST028100 | K        | T of 28-100 cm ground layer              |
 236 | 112  | 100  | 255  | ST100289 | K        | T of 100-289 cm ground layer             |
  39 | 112  |   0  |   7  | SM000007 | m3 m-3   | Soil moisture of 0-7 cm ground layer     |
  40 | 112  |   7  |  28  | SM007028 | m3 m-3   | Soil moisture of 7-28 cm ground layer    |
  41 | 112  |  28  | 100  | SM028100 | m3 m-3   | Soil moisture of 28-100 cm ground layer  |
  42 | 112  | 100  | 255  | SM100289 | m3 m-3   | Soil moisture of 100-289 cm ground layer |
-----+------+------+------+----------+----------+------------------------------------------+
#
#  For use with ERA-interim model-level output.
#
#  Grib codes are from Table 128
#  http://www.ecmwf.int/services/archive/d/parameters/order=grib_parameter/table=128/
#  
# snow depth is converted to the proper units in rrpr.F
#
#  For ERA-interim data at NCAR, use the ml (sc and uv) and sfc sc files. 
