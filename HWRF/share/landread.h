#ifndef LANDREAD_H
#define LANDREAD_H

int GET_TERRAIN (        float *adx,
                         float *xlat,
                         float *xlon,
                         float       *terrain,
                         int   *mix,
                         int   *mjx,
                         int   *iyyn,
                         int   *jxxn,
                         int   *ipath , int * ipathlen) ;

/* int    nint(const double x); */
double aint(const double x);
double anint(const double x);
float  tsGetValue(const double aix, const double aiy);
float  tsGetValueInt(const int aix, const int aiy);
float  tsGetValueInterp(const double ix, const double iy);
float  tsGetValueLatLon(const double lat, const double lon);
int    tsCloseTileSet(void);
int    tsInitTileSet(const char *fn);
int    tsPrintTileSetInto(void);
int    tsLatLonToGridpoint(const double  lat,
			   const double  lon,
			   double       *ix,
			   double       *iy);

#endif
