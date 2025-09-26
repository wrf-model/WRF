module met_data_module

   ! Derived types
   type met_data
      integer                       :: version, nx, ny, iproj
      real                          :: xfcst, xlvl, startlat, startlon, starti, startj, &
                                       deltalat, deltalon, dx, dy, xlonc, &
                                       centerlat, centerlon,  &   
                                       pole_lat, pole_lon,  &   
                                       truelat1, truelat2, earth_radius
      real, pointer, dimension(:,:) :: slab
      logical                       :: is_wind_grid_rel
      character (len=9)             :: field
      character (len=24)            :: hdate
      character (len=25)            :: units
      character (len=32)            :: map_source
      character (len=46)            :: desc
   end type met_data

end module met_data_module
