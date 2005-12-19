#ifndef CRAY
# ifdef NOUNDERSCORE
#      define GET_TERRAIN get_terrain
# else
#   ifdef F2CSTYLE
#      define GET_TERRAIN get_terrain__
#   else
#      define GET_TERRAIN get_terrain_
#   endif
# endif
#endif

#ifdef LANDREAD_STUB
#include <stdio.h>

int GET_TERRAIN (        float *adx,
                         float *xlat,
                         float *xlon,
                         float       *terrain,
                         int   *mix,
                         int   *mjx,
                         int   *iyyn,
                         int   *jxxn,
                         int   *ipath , int * ipathlen)  /* integer coded ASCII string from Funtran and len */

{
 fprintf(stderr, "***************************************************************\n" ) ;
 fprintf(stderr, "Access to RSMAS Topo Ingest Code is by Special Arrangement Only\n" ) ;
 fprintf(stderr, "in WRF 2.1 .  Please contact wrfhelp@ucar.edu .                \n" ) ;
 fprintf(stderr, "***************************************************************\n" ) ;
 return(0) ;
}

#else

#ifdef FSEEKO_OK
#  define _FILE_OFFSET_BITS 64
#endif
#include <stdio.h>
#include <rpc/xdr.h>
#include <math.h>
#include <malloc.h>
#include <string.h>
#include "landread.h"
#define MAXTOPOFILES  100
#define MAXLEN        4096


typedef struct
{
  /* Filenames. */
  char  fn[MAXTOPOFILES][MAXLEN];

  /* Grid spacings in km. */
  float dx[MAXTOPOFILES];

  /* Number of entries. */
  int num;
} TsFileInfo;

static float vmiss;

static int    numHeaderBytes;
static int    globalNx;
static int    globalNy;
static int    tileNx;
static int    tileNy;
static int    extraNx;
static int    extraNy;
static int    numTilesX;
static int    numTilesY;
static double dlat;
static double dlon;
static double lat0;
static double lon0;
static int    ntiles;
static int    wrapx;
static int    wrapy;

/* File information. */
static XDR  *xdrs;
static FILE *fp;

#if 0
 int nint(const double x)
{
  if ( x > 0.0 ) { return( (int)(x + 0.5) ) ; }
  return((int)(x - 0.5));
}
#endif

double aint(const double x)
{
  int ix = (int)(x);
  return((double)(ix));
}

double anint(const double x)
{
  if (x > 0.0) return((double)((int)(x + 0.5)));
  return((double)((int)(x - 0.5)));
}

static double normalizeAngle(double ang)
{
  for (;;)
    {
      if (ang >= 360.0)
        {
          ang -= 360.0;
        }
      else if (ang < 0.0)
        {
          ang += 360.0;
        }
      else
        {
          break;
        }
    }
  
  return(ang);
}

static double lonDistNowrap(double lon1, double lon2)
{
  double lon11 = normalizeAngle(lon1);
  double lon22 = normalizeAngle(lon2);
  if (lon22 < lon11) lon22 += 360.0;
  return(fabs(lon22 - lon11));
}

int tsLatLonToGridpoint(const double  lat,
			const double  lon,
			double       *ix,
			double       *iy)
{
  *ix = lonDistNowrap(lon0, lon) / dlon;
  *iy = (lat - lat0) / dlat;
  return(1);
}

static int areEqual(const double v1, const double v2)
{
  if (fabs(v1-v2) < 0.001) return(1);
  return(0);
}

static int setWrapAroundFlags(void)
{
  /* Compute the end gridpoint location in x. */
  double lon1  = lon0 + dlon*(globalNx);
  double lon2  = lon0 + dlon*(globalNx-1);
  double lat1  = lat0 + dlat*(globalNy);
  double lon0n = normalizeAngle(lon0);
  double lon1n = normalizeAngle(lon1);
  double lon2n = normalizeAngle(lon2);

  wrapx = 0;
  if (areEqual(lon0n, lon1n))
    {
      /* Here the first and last indices in x are one grid interval
         apart. */
      wrapx = 1;
    }
  else if (areEqual(lon0n, lon2n))
    {
      /* Here the first and last indices in x are coincident. */
      wrapx = 2;
    }

  wrapy = 0;
  if (areEqual(lat0, -90.0))
    {
      /* Here the first and last indices in x are one grid interval
         apart. */
      wrapy += 1;
    }
  if (areEqual(lat1, 90.0))
    {
      /* Here the first and last indices in x are coincident. */
      wrapy += 2;
    }

  return(1);
}

static int isMissing(const float v)
{
  if (fabs(vmiss - v) < 0.1) return(1);
  return(0);
}

float tsGetValueInt(const int aix, const int aiy)
{
  float f = vmiss;
  
  int iy = aiy;
  int ix = aix;

  /* Perform bounds checking. */
  if (iy < 0)
    {
      return(f);
    }
  else if (iy > globalNy - 1)
    {
      return(f);
    }

  if (aix < 0)
    {
      if (wrapx == 1)
	{
	  int n  = -(aix - (globalNx - 1)) / globalNx;
	  ix += n*globalNx;
	}
      else if (wrapx == 2)
	{
	  int nx = globalNx - 1;
	  int n  = -(aix - (nx - 1)) / nx;
	  ix += n*nx;
	}
      else
	{
	  return(f);
	}
    }

  if (ix > globalNx-1)
    {
      if (wrapx == 1)
	{
	  int n  = aix / globalNx;
	  ix -= n*globalNx;
	}
      else if (wrapx == 2)
	{
	  int nx = globalNx - 1;
	  int n  = aix / nx;
	  ix -= n*nx;
	}
      else
	{
	  return(f);
	}
    }

  int tx  = ix / tileNx;
  int ty  = iy / tileNy;
  int tn  = tx + ty*numTilesX;
  int txg = ix - tx*tileNx;
  int tyg = iy - ty*tileNy;
  int gn  = txg + tyg*tileNx;

  long long ll_gn = gn;
  long long ll_numHeaderBytes  = numHeaderBytes;
  long long ll_tileNx = tileNx;
  long long ll_tileNy = tileNy;

#ifdef FSEEKO64_OK 
  /* This is used on machines that support fseeko64. Tested for in ./configure script */
  long long loc = ll_numHeaderBytes + ll_tileNx*ll_tileNy*sizeof(float)*tn +
    ll_gn*sizeof(float);

  /* Seek to the proper location in the file and get the data value. */
  fseeko64(fp, loc, SEEK_SET);
#else
#  ifdef FSEEKO_OK
  /* This is used on machines that support _FILE_OFFSET_BITS=64 which makes
     off_t be 64 bits, and for which fseeko can handle 64 bit offsets.  This
     is tested in the ./configure script */
  off_t loc = ll_numHeaderBytes + ll_tileNx*ll_tileNy*sizeof(float)*tn +
    ll_gn*sizeof(float);

  fseeko(fp, loc, SEEK_SET);
#  else
  /* Note, this will not work correctly for very high resolution terrain input
     because the offset is only 32 bits.   */
  off_t loc = ll_numHeaderBytes + ll_tileNx*ll_tileNy*sizeof(float)*tn +
    ll_gn*sizeof(float);

  fseek(fp, loc, SEEK_SET);
#  endif
#endif
  xdr_float(xdrs, (float *) &f);

  return(f);
}

float tsGetValue(const double ix, const double iy)
{
  int i0 = (int)(floor(ix));
  int j0 = (int)(floor(iy));
  int i1 = (int)(ceil(ix));
  int j1 = (int)(ceil(iy));
  
  /* Interpolate linearly to (oiloc, ojloc). */
  float v0 = tsGetValueInt(i0,j0);
  float v1 = tsGetValueInt(i0,j1);
  float v2 = tsGetValueInt(i1,j0);
  float v3 = tsGetValueInt(i1,j1);
  
  if (isMissing(v0)) return(vmiss);
  if (isMissing(v1)) return(vmiss);
  if (isMissing(v2)) return(vmiss);
  if (isMissing(v3)) return(vmiss);

  double w0 = ix - i0;
  double w1 = iy - j0;

  float v4 = v2*w0 + v0*(1.0-w0);
  float v5 = v3*w0 + v1*(1.0-w0);
  float v6 = w1*v5 + (1.0-w1)*v4;
  float val = v6;

  return(val);
}

float tsGetValueLatLon(const double lat, const double lon)
{
  double ix, iy;
  tsLatLonToGridpoint(lat,lon,&ix,&iy);
  return(tsGetValue(ix,iy));
}

int tsCloseTileSet(void)
{
  if (xdrs)
    {
      xdr_destroy(xdrs);
      free(xdrs);
      xdrs = 0;
    }
  
  if (fp)
    {
      fclose(fp);
      fp = 0;
    }

  return(1);
}

int tsInitTileSet(const char *fn)
{
  vmiss = -100000000.00;

  xdrs = 0;
  fp   = 0;

  /* fp = (FILE *) fopen64(fn, "r"); */
  if (( fp = (FILE *) fopen(fn, "r")) == NULL ) {
    fprintf(stderr,"tsInitTileSet: cannot open %s\n",fn) ;
    exit(2) ;
  }
  xdrs = (XDR *) malloc(sizeof(XDR));
  xdrstdio_create(xdrs, fp, XDR_DECODE);

  numHeaderBytes = 5000;

  xdr_int(xdrs,    (int *)    &globalNx);
  xdr_int(xdrs,    (int *)    &globalNy);
  xdr_int(xdrs,    (int *)    &tileNx);
  xdr_int(xdrs,    (int *)    &tileNy);
  xdr_int(xdrs,    (int *)    &extraNx);
  xdr_int(xdrs,    (int *)    &extraNy);
  xdr_int(xdrs,    (int *)    &numTilesX);
  xdr_int(xdrs,    (int *)    &numTilesY);
  xdr_double(xdrs, (double *) &dlat);
  xdr_double(xdrs, (double *) &dlon);
  xdr_double(xdrs, (double *) &lat0);
  xdr_double(xdrs, (double *) &lon0);
  xdr_int(xdrs,    (int *)    &ntiles);

  setWrapAroundFlags();

  return(1);
}

int tsPrintTileSetInto(void)
{
  return(1);
}

#ifdef TERRAIN_AND_LANDUSE
int get_terrain_landuse_(const float &adx,
			 const float *xlat,
			 const float *xlon,
			 float       *terrain,
			 float       *landuse,
			 const int   &mix,
			 const int   &mjx,
			 const int   &iyyn,
			 const int   &jxxn)
#else

int GET_TERRAIN (        float *adx,
                         float *xlat,
                         float *xlon,
                         float       *terrain,
                         int   *mix,
                         int   *mjx,
                         int   *iyyn,
                         int   *jxxn, 
                         int   *ipath , int * ipathlen)  /* integer coded ASCII string from Funtran and len */
#endif
{
  TsFileInfo tsfTopo;
  TsFileInfo tsfOcean;
  TsFileInfo tsfLU;
  int i, j ;
  char path[1024] ;

  tsfTopo.num  = 0;
  tsfOcean.num = 0;
  tsfLU.num    = 0;

#if 0
  /* Read in the list of topography/land use filenames. */
  {
    FILE *fp = fopen("landFilenames", "r");

    for (;;)
      {
	char type[MAXLEN];
	char res[MAXLEN];
	char fn[MAXLEN];

	if (fscanf(fp, "%s %s %s", type, res, fn) == EOF) break;

	float dx;
	sscanf(res, "%f", &dx);

	if (strcmp(type, "landuse") == 0)
	  {
	    tsfLU.dx[tsfLU.num] = dx;
	    strcpy(tsfLU.fn[tsfLU.num], fn);
	    tsfLU.num++;
	  }
	else if (strcmp(type, "topography") == 0)
	  {
	    tsfTopo.dx[tsfTopo.num] = dx;
	    strcpy(tsfTopo.fn[tsfTopo.num], fn);
	    tsfTopo.num++;
	  }
	else if (strcmp(type, "bathymetry") == 0)
	  {
	    tsfOcean.dx[tsfOcean.num] = dx;
	    strcpy(tsfOcean.fn[tsfOcean.num], fn);
	    tsfOcean.num++;
	  }
      }
    fclose(fp);
  }
#else
  for (i = 0 ; i < *ipathlen ; i++ ) {
    path[i] = ipath[i] ;
  }
  path[*ipathlen] = '\0' ;

# if 0
  fprintf(stderr,"path: %s\n",path) ;
# endif
tsfTopo.num  = 0;
tsfTopo.dx[tsfTopo.num] =  1; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  1); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  2; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  2); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  3; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  3); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  4; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  4); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  5; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  5); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  6; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  6); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  7; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  7); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  8; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  8); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] =  9; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path,  9); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] = 10; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, 10); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] = 20; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, 20); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] = 30; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, 30); tsfTopo.num++ ;
tsfTopo.dx[tsfTopo.num] = 40; sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, 40); tsfTopo.num++ ;

# if 0
  for ( i = 0 ; i < tsfTopo.num ; i++ ) {
    fprintf(stderr,"%02d. %s\n",i, tsfTopo.fn[i] ) ;
  }
# endif
#endif


  /* First get the terrain from GTOPO30. */
  {
    /* Use the data with the largest spacing less than the grid
       spacing specified in the argument list. */
    float maxdx = 0.0;
    char fn[MAXLEN];
    int first = 1;
    for (i = 0; i < tsfTopo.num; i++)
      {
# if 0
fprintf(stderr,"%d %d file %f adx %f max %f\n",i,first,tsfTopo.dx[i],*adx , maxdx ) ;
# endif
	if (tsfTopo.dx[i] < maxdx) continue;
	if (first || tsfTopo.dx[i] < *adx)
	  {
	    first = 0;
	    maxdx = tsfTopo.dx[i];
	    strcpy(fn, tsfTopo.fn[i]);
	  }
      }

    if (!tsInitTileSet(fn))
      {
	return(0);
      }

    for ( j = 0; j < *jxxn; j++)
      {
	for ( i = 0; i < *iyyn; i++)
	  {
	    float lat = xlat[*mix*j + i];
	    float lon = xlon[*mix*j + i];
	    
	    double fix;
	    double fiy;
	    tsLatLonToGridpoint(lat,lon,&fix,&fiy);
	    float tv = tsGetValue(fix, fiy);
	    terrain[*mix*j + i] = tv;
	  }
      }

    tsCloseTileSet();
  }

#ifdef TERRAIN_AND_LANDUSE
  /* Next get the terrain from TBASE. */
  {
    /* Use the data with the largest spacing less than the grid
       spacing specified in the argument list. */
    float maxdx = 0.0;
    char fn[MAXLEN];
    int first = 1;
    for ( i = 0; i < tsfOcean.num; i++)
      {
	if (tsfOcean.dx[i] < maxdx) continue;
	if (first || tsfOcean.dx[i] < *adx)
	  {
	    first = 0;
	    maxdx = tsfOcean.dx[i];
	    strcpy(fn, tsfOcean.fn[i]);
	  }
      }

    if (!tsInitTileSet(fn))
      {
	return(0);
      }

    for ( j = 0; j < *jxxn; j++)
      {
	for ( i = 0; i < *iyyn; i++)
	  {
	    float lat = xlat[*mix*j + i];
	    float lon = xlon[*mix*j + i];
	    
	    double fix;
	    double fiy;
	    tsLatLonToGridpoint(lat,lon,fix,fiy);
	    float tv = tsGetValue(fix, fiy);
	    if (isMissing(terrain[*mix*j+i]))
	      {
		if (tv < 0.0) tv = 0.0;
		terrain[*mix*j + i] = tv;
	      }
	  }
      }
    tsCloseTileSet();
  }

  /* Next get the land use. */
  {
    /* Use the data with the largest spacing less than the grid
       spacing specified in the argument list. */
    float maxdx = 0.0;
    char fn[MAXLEN];
    int first = 1;
    for ( i = 0; i < tsfLU.num; i++)
      {
	if (tsfLU.dx[i] < maxdx) continue;
	if (first || tsfLU.dx[i] < *adx)
	  {
	    first = 0;
	    maxdx = tsfLU.dx[i];
	    strcpy(fn, tsfLU.fn[i]);
	  }
      }

    if (!tsInitTileSet(fn))
      {
	return(0);
      }

    for ( j = 0; j < *jxxn; j++)
      {
	for ( i = 0; i < *iyyn; i++)
	  {
	    float lat = xlat[*mix*j + i];
	    float lon = xlon[*mix*j + i];
	    
	    double fix;
	    double fiy;
	    tsLatLonToGridpoint(lat,lon,fix,fiy);
	    int ix = nint(fix);
	    int iy = nint(fiy);
	    float tv = tsGetValueInt(ix, iy);

            /* Set out-of-range values to water. */
            if (tv < 0.9 || tv > 24.1) tv = 16.0;

	    landuse[*mix*j + i] = tv;
	  }
      }
    tsCloseTileSet();
  }
#endif

  return(1);
}

#ifdef TERRAIN_AND_LANDUSE
int get_bathymetry_(const float &tadx,
		    const float *xlat,
		    const float *xlon,
		    float       *depth,
		    const int   &mix,
		    const int   &mjx,
		    const int   &iyyn,
		    const int   &jxxn,
		    const float &mindepth,
		    const float &zlimww3)
{
  /* Set grid resolution to .1 km to get highest resolution data possible. */
  float adx = 0.1;

  TsFileInfo tsfOcean;
  TsFileInfo tsfLU;

  tsfOcean.num = 0;
  tsfLU.num    = 0;

  /* Read in the list of topography/land use filenames. */
  {
    FILE *fp = fopen("landFilenames", "r");

    for (;;)
      {
	char type[MAXLEN];
	char res[MAXLEN];
	char fn[MAXLEN];

	if (fscanf(fp, "%s %s %s", type, res, fn) == EOF) break;

	float dx;
	sscanf(res, "%f", &dx);

	if (strcmp(type, "landuse") == 0)
	  {
	    tsfLU.dx[tsfLU.num] = dx;
	    strcpy(tsfLU.fn[tsfLU.num], fn);
	    tsfLU.num++;
	  }
	else if (strcmp(type, "bathymetry") == 0)
	  {
	    tsfOcean.dx[tsfOcean.num] = dx;
	    strcpy(tsfOcean.fn[tsfOcean.num], fn);
	    tsfOcean.num++;
	  }
      }

    fclose(fp);
  }

  /* Get the water depth from TBASE. */
  {
    /* Use the data with highest resolution possible. */
    float maxdx = 0.0;
    char fn[MAXLEN];
    int first = 1;
    for (int i = 0; i < tsfOcean.num; i++)
      {
	if (tsfOcean.dx[i] < maxdx) continue;
	if (first || tsfOcean.dx[i] < adx)
	  {
	    first = 0;
	    maxdx = tsfOcean.dx[i];
	    strcpy(fn, tsfOcean.fn[i]);
	  }
      }

    if (!tsInitTileSet(fn))
      {
	return(0);
      }

    for (int i = 0; i < mix*mjx; i++)
      {
	depth[i] = vmiss;
      }
    
    for (int j = 0; j < jxxn; j++)
      {
	for (int i = 0; i < iyyn; i++)
	  {
	    float lat = xlat[mix*j + i];
	    float lon = xlon[mix*j + i];
	    
	    double fix;
	    double fiy;
	    tsLatLonToGridpoint(lat,lon,fix,fiy);
	    float tv = tsGetValue(fix, fiy);
	    if (isMissing(depth[mix*j+i]))
	      {
		depth[mix*j + i] = -tv;
	      }
	  }
      }
    tsCloseTileSet();
  }

  /* Next get the land use. */
  {
    /* Use the data with the largest spacing less than the grid
       spacing specified in the argument list. */
    float maxdx = 0.0;
    char fn[MAXLEN];
    int first = 1;
    for (int i = 0; i < tsfLU.num; i++)
      {
	if (tsfLU.dx[i] < maxdx) continue;
	if (first || tsfLU.dx[i] < adx)
	  {
	    first = 0;
	    maxdx = tsfLU.dx[i];
	    strcpy(fn, tsfLU.fn[i]);
	  }
      }

    if (!tsInitTileSet(fn))
      {
	return(0);
      }

    for (int j = 0; j < jxxn; j++)
      {
	for (int i = 0; i < iyyn; i++)
	  {
	    float lat = xlat[mix*j + i];
	    float lon = xlon[mix*j + i];
	    
	    double fix;
	    double fiy;
	    tsLatLonToGridpoint(lat,lon,fix,fiy);
	    int ix = nint(fix);
	    int iy = nint(fiy);
	    float tv = tsGetValueInt(ix, iy);

            /* Set out-of-range values to water. */
            if (tv < 0.9 || tv > 24.1) tv = 16.0;

	    if (fabs(tv - 16.0) < 0.1)
	      {
		/* Water. */
		if (1)
		  {
		    if (depth[mix*j + i] < mindepth) depth[mix*j + i] = mindepth;
		  }
		else
		  {
		    if (depth[mix*j + i] < -zlimww3)
		      {
			/* Water depth below zlimww3, so turn this point 
			   into land. */
			depth[mix*j + i] = -0.1;		    
		      }
		    else if (depth[mix*j + i] < mindepth)
		      {
			depth[mix*j + i] = mindepth;
		      }
		  }
	      }
	    else
	      {
		/* Land. Set depth to 0.0. */
		depth[mix*j + i] = 0.0;
	      }
	  }
      }
    tsCloseTileSet();
  }

  return(1);
}
#endif

#endif 
