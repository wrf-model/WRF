#define _GDSTOOL_

/*
 * 3/29/2001 fix in new_polar_GDS by Gerry Wiener
 */


unsigned char *GDStool(unsigned char *pds, unsigned char *gds, ...);

enum g_tool {g_end, g_init, g_byte, g_2bytes, g_s2bytes,
   g_3bytes, g_s3bytes, g_bit, g_and, g_or};           

/* choose one set */

/*
   for fortran grid: real data(nlon,nlat)

   nlat/nlon = number of lat/lon rows/columns
   lat1/lon1 = lat/lon of data(1,1)
   lat2/lon2 = lat/lon of data(nlon,nlat)

   for S or W, use negative value

   dlon, dlat = degrees of lon/lat of grid interval

 */

#define new_LatLon_GDS(pds,nlon,nlat,lon1,lat1,lon2,lat2,dlon,dlat) \
	GDStool(pds,NULL,g_init,32,0,g_2bytes,6,nlon,g_2bytes,8,nlat, \
	g_s3bytes,10,(int) (1000.0*(lat1+(lat1/abs(lat1))*0.0005)), \
        g_s3bytes,13,(int) (1000.0*(lon1+(lon1/abs(lon1))*0.0005)), \
	g_s3bytes,17,(int) (1000.0*(lat2+(lat2/abs(lat2))*0.0005)), \
	g_s3bytes,20,(int) (1000.0*(lon2+(lon2/abs(lon2))*0.0005)), \
	g_s2bytes,23,(int) (1000.0*(dlon+(dlon/abs(dlon))*0.0005)), \
        g_s2bytes,25,(int) (1000.0*(dlat+(dlat/abs(dlat))*0.0005)), \
	g_or, 27, ((lat1) < (lat2)) * 64, \
	g_byte,16,128,g_byte,4,255,g_end)

/*
#define new_LatLon_GDS(pds,nlon,nlat,lon1,lat1,lon2,lat2,dlon,dlat) \
	GDStool(pds,NULL,g_init,32,0,g_2bytes,6,nlon,g_2bytes,8,nlat, \
	g_s3bytes,10,lat1,g_s3bytes,13,lon1, \
	g_s3bytes,17,lat2,g_s3bytes,20,lon2, \
	g_s2bytes,23,dlon,g_s2bytes,25,dlat, \
	g_or, 27, 64, \
	g_byte,16,128,g_byte,4,255,g_end)
*/
/*
#define new_LatLon_GDS(pds,nlon,nlat,lon1,lat1,lon2,lat2,dlon,dlat) \
	GDStool(pds,NULL,g_init,32,0,g_2bytes,6,35,g_2bytes,8,14, \
	g_s3bytes,10,-117,g_s3bytes,13,-307, \
	g_s3bytes,17,117,g_s3bytes,20,307, \
	g_s2bytes,23,18,g_s2bytes,25,18, \
	g_or, 27, ((lat1) < (lat2)) * 64, \
	g_byte,16,128,g_byte,4,255,g_end)
*/

/*
   For gaussian grid(nlon,nlat)

   nlon/nlat = number of lon/lat points
   lon1,lat1 = coordinates of grid(1,1)

  */

#define new_Gaussian_GDS(pds,nlon,nlat,lon1,lat1) \
	GDStool(pds,NULL,g_init,32,0,g_2bytes,6,nlon,g_2bytes,8,nlat, \
	g_s3bytes,10,(int) (1000.0*(lat1+(lat1/abs(lat1))*0.0005)), \
	g_s3bytes,13,(int) (1000.0*(lon1+(lon1/abs(lon1))*0.0005)), \
	g_s3bytes,17,(int) (-1000.0*(lat1+(lat1/abs(lat1))*0.0005)), \
        g_s3bytes,20,(int) (1000.0*((lon1+(lon1/abs(lon1))*0.0005)+360.0-360.0/(nlon))), \
	g_s2bytes,23,(int) (1000.0*360.0 / nlon),g_s2bytes,25, nlat / 2, \
	g_or, 27, ((lat1) < 0.0) * 64, \
	g_byte,16,128,g_byte,4,255,g_byte,5,4,g_end)
	
/*
   For mercator grid(nlon,nlat)

   nlon/nlat = number of lon/lat points
   lon1,lat1 = coordinates of grid(1,1)
   lon2,lat2 = coordinates of last grid point
   latin = latitude at which Mercator projection cylinder intersects the earth.
   Di = longitudinal direction increment in meters at latin
   Dj = latitudinal direction increment in meters at latin
  */

#define new_Mercator_GDS(pds,nlon,nlat,lon1,lat1,lon2,lat2,latin,scan_s2n,Di,Dj) \
	GDStool(pds,NULL,g_init,42,1,g_2bytes,6,nlon,g_2bytes,8,nlat, \
	g_s3bytes,10,(int) (1000.0*(lat1+(lat1/abs(lat1))*0.0005)), \
	g_s3bytes,13,(int) (1000.0*(lon1+(lon1/abs(lon1))*0.0005)), \
        g_byte,16,128, \
	g_s3bytes,17,(int) (-1000.0*(lat2+(lat2/abs(lat2))*0.0005)), \
        g_s3bytes,20,(int) (1000.0*(lon2+(lon2/abs(lon2))*0.0005)), \
	g_s3bytes,23,(int) (1000.0*latin+(latin/abs(latin))*0.0005), \
        g_byte,26,0, \
        g_byte,27,scan_s2n*64, \
        g_s3bytes,28,1000.0*(Di+0.0005), \
        g_s3bytes,31,1000.0*(Dj+0.0005), \
        g_s3bytes,34,0, \
        g_s3bytes,37,0, \
        g_s2bytes,40,0, \
	g_end)
	
/*
 * make a polar stereographic GDS
 *
 * (nx,ny) = grid dimensions
 * (lon1,lat1) = lon/lat of first grid point (deg)
 * lov = the orientation of the grids
 * (dx,dy) = grid length (km) at 60N or 60S
 * south_pole = 1 for south polar stereographics, 0 for nps
 * scan_s2n = 1 if grid is written from south to north else 0
 */

#define new_polar_GDS(pds,nx,ny,lon1,lat1,lov,dx,dy,south_pole,scan_s2n) \
        GDStool(pds,NULL,g_init,32,0,g_byte,5,5,g_byte,4,255,\
	g_2bytes,6,nx,g_2bytes,8,ny, \
        g_s3bytes,10,(int) (1000.0*(lat1+(lat1/abs(lat1))*0.0005)), \
	g_s3bytes,13,(int) (1000.0*(lon1+(lon1/abs(lon1))*0.0005)), \
        g_byte,16,128, \
	g_s3bytes,17,(int) (1000.0*(lov+(lov/abs(lov))*0.0005)), \
        g_s3bytes,20,(int) (1000.0*(dx+0.0005)), \
	g_s3bytes,23,(int) (1000.0*(dy+0.0005)),g_byte,26,south_pole*128,\
	g_byte,27,scan_s2n*64,g_end)


/*
 * make a lambert conformal GDS
 *
 * (nx,ny) = grid dimensions
 * (lon1,lat1) = lon/lat of first grid point (deg)
 * lov = the orientation of the grids
 * (dx,dy) = grid length (km) at 60N or 60S
 * south_pole = 1 for south polar stereographics, 0 for nps
 * scan_s2n = 1 if grid is written from south to north else 0
 * latin1 = true latitude 1
 * latin2 = true latitude 2
 */

#define new_lambert_GDS(pds,nx,ny,lon1,lat1,lov,dx,dy,south_pole,latin1,latin2,scan_s2n) \
        GDStool(pds,NULL,g_init,42,0,g_byte,5,3,g_byte,4,255,\
	g_2bytes,6,nx,g_2bytes,8,ny,g_s3bytes,10,\
        (int) (1000.0*(lat1+(lat1/abs(lat1))*0.0005)),\
	g_s3bytes,13,(int) (1000.0*(lon1+(lon1/abs(lon1))*0.0005)), \
        g_byte,16,128,\
	g_s3bytes,17,(int) (1000.0*(lov+(lov/abs(lov))*0.0005)),\
        g_s3bytes,20,(int) (1000.0*(dx+0.0005)),\
	g_s3bytes,23,(int) (1000.0*(dy+0.0005)),g_byte,26,south_pole*128,\
	g_byte,27,scan_s2n*64, \
        g_s3bytes,28,(int) (1000.0*(latin1+(latin1/abs(latin1))*0.0005)),\
        g_s3bytes,31,(int) (1000.0*(latin2+(latin2/abs(latin2))*0.0005)), \
	g_s3bytes,34,-90000,g_s3bytes,37,0,g_byte,40,0,g_byte,41,0,g_end)


