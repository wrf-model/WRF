#ifndef CRAY
# ifdef NOUNDERSCORE
#      define GET_TERRAIN get_terrain
#      define GET_LANDUSE get_landuse
# else
#   ifdef F2CSTYLE
#      define GET_TERRAIN get_terrain__
#      define GET_LANDUSE get_landuse__
#   else
#      define GET_TERRAIN get_terrain_
#      define GET_LANDUSE get_landuse_
#   endif
# endif
#endif

#ifdef LANDREAD_STUB
#ifndef MS_SUA
# include <stdio.h>
#endif

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
#ifndef MS_SUA
 fprintf(stderr, "***************************************************************\n" ) ;
 fprintf(stderr, "Access to RSMAS Topo Ingest Code is by Special Arrangement Only\n" ) ;
 fprintf(stderr, "in WRF.  Please contact wrfhelp@ucar.edu.                      \n" ) ;
 fprintf(stderr, "***************************************************************\n" ) ;
#endif
 return(0) ;
}

int GET_LANDUSE (        float *adx,
                         float *xlat,
                         float *xlon,
                         float       *landuse,
                         int   *mix,
                         int   *mjx,
                         int   *iyyn,
                         int   *jxxn,
                         int   *ipath , int * ipathlen ) /* integer coded ASCII string from Funtran and len */

{
#ifndef MS_SUA
 fprintf(stderr, "***************************************************************\n" ) ;
 fprintf(stderr, "Access to RSMAS Topo Ingest Code is by Special Arrangement Only\n" ) ;
 fprintf(stderr, "in WRF.  Please contact wrfhelp@ucar.edu.                      \n" ) ;
 fprintf(stderr, "***************************************************************\n" ) ;
#endif
 return(0) ;
}
#else

#ifdef FSEEKO_OK
#  define _FILE_OFFSET_BITS 64
#endif
#ifndef MS_SUA
# include <stdio.h>
#endif
#if (RPC_TYPES == 1)
# include <rpc/types.h>
# include <rpc/xdr.h>
#elif (RPC_TYPES == 2)
# include <tirpc/rpc/types.h>
# include <tirpc/rpc/xdr.h>
#endif
#include <math.h>
#ifndef MACOS
# include <malloc.h>
#else
# include <malloc/malloc.h>
#endif
#include <unistd.h>
#include <string.h>
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

static TsFileInfo tsfTopo;
static TsFileInfo tsfOcean;
static TsFileInfo tsfLU;

static int tsFileInfo_initialized = 0;
/* static float last_adx = 0.0 ; */

static char tsfTopo_fn[MAXLEN];
static char tsfLU_fn[MAXLEN];
static char tsfOcean_fn[MAXLEN];

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

int nint(double x)
{
  if ( x > 0.0 ) { return( (int)(x + 0.5) ) ; }
  return((int)(x - 0.5));
}

double aint(double x)
{
  int ix = (int)(x);
  return((double)(ix));
}

double anint(double x)
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

int tsLatLonToGridpoint(double  lat,
			double  lon,
			double       *ix,
			double       *iy)
{
  *ix = lonDistNowrap(lon0, lon) / dlon;
  *iy = (lat - lat0) / dlat;
  return(0);
}

static int areEqual(double v1, double v2)
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

  return(0);
}

static int isMissing(float v)
{
  if (fabs(vmiss - v) < 0.1) return(1);
  return(0);
}

float tsGetValueInt(int aix, int aiy)
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

float tsGetValue(double ix, double iy)
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

float tsGetValueLatLon(double lat, double lon)
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
      xdrs = NULL;
    }
  
  if (fp)
    {
      fclose(fp);
      fp = NULL;
    }

  return(0);
}

int tsInitTileSet(char *fn)
{
  vmiss = -100000000.00;

  xdrs = NULL;
  fp   = NULL;

# if 0
  fprintf(stderr,"Open %s\n", fn) ;
# endif

  /* fp = (FILE *) fopen64(fn, "r"); */
  if (( fp = fopen(fn, "r")) == NULL ) {
#ifndef MS_SUA
    fprintf(stderr,"tsInitTileSet: can not open %s\n",fn) ;
#endif
    return(1) ;
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

  return(0);
}

int tsPrintTileSetInfo()
{
  return(0);
}

int tsInitFileInfo (char path[])
{
  int i, n;
  char type[MAXLEN];
  char  res[MAXLEN];
  char   fn[MAXLEN];
  char buff[MAXLEN];
  float dx;

  tsfTopo.num  = 0;
  tsfOcean.num = 0;
  tsfLU.num    = 0;

  tsfLU.dx[0] = 0.;
  tsfTopo.dx[0] = 0.;
  tsfOcean.dx[0] = 0. ;

  if (access("RSMAS_Topo_Land.TBL", F_OK) == 0 ) {
  /* Read in the list of topography/land use filenames. */
    fp = fopen("RSMAS_Topo_Land.TBL", "r");
    if ( fp == NULL ) {
#ifndef MS_SUA
      fprintf(stderr, "tsInitFileInfo : can not open RSMAS_Topo_Land.TBL\n");
#endif
      return(-1);
    }

    /* skipps header */
    fgets(buff, MAXLEN, fp);

    while (fscanf(fp, "%s %s %s", type, res, fn) != EOF) {
        sscanf(res, "%f", &dx);
        if (strcmp(type, "landuse") == 0)
          {
            if ( tsfLU.num >= MAXTOPOFILES ) {continue;}
            n = tsfLU.num ;
            for ( i = 0 ; i < tsfLU.num ; i++ ) {
              if ( tsfLU.dx[i] > dx ) {
                n = i ;
                break;
              }
            }
            for ( i = tsfLU.num ; i > n ; i-- ) {
               tsfLU.dx[i]=tsfLU.dx[i-1];
               strcpy(tsfLU.fn[i], tsfLU.fn[i-1]);
            }
            tsfLU.dx[n] = dx;
            strcpy(tsfLU.fn[n], fn);
            tsfLU.num++;
          }
        else if (strcmp(type, "topography") == 0)
          {
            if ( tsfTopo.num >= MAXTOPOFILES ) {continue;}
            n = tsfTopo.num;
            for ( i = 0 ; i < tsfTopo.num ; i++ ) {
              if ( tsfTopo.dx[i] > dx ) {
                n = i ;
                break;
              }
            }
            for ( i = tsfTopo.num ; i > n ; i-- ) {
               tsfTopo.dx[i]=tsfTopo.dx[i-1];
               strcpy(tsfTopo.fn[i], tsfTopo.fn[i-1]);
            }
            tsfTopo.dx[n] = dx;
            strcpy(tsfTopo.fn[n], fn);
            tsfTopo.num++;
          }
        else if (strcmp(type, "bathymetry") == 0)
          {
            if ( tsfOcean.num >= MAXTOPOFILES ) {continue;}
            n = tsfOcean.num;
            for ( i = 0 ; i < tsfOcean.num ; i++ ) {
              if ( tsfOcean.dx[i] > dx ) {
                n = i ;
                break;
              }
            }
            for ( i = tsfOcean.num ; i > n ; i-- ) {
               tsfOcean.dx[i]=tsfOcean.dx[i-1];
               strcpy(tsfOcean.fn[i], tsfOcean.fn[i-1]);
            }
            tsfOcean.dx[n] = dx;
            strcpy(tsfOcean.fn[n], fn);
            tsfOcean.num++;
          }
    }

    fclose(fp);

  } else {

    for ( i = 1; i < 10 ; i++ ) {
      sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, i);
      if (access(tsfTopo.fn[tsfTopo.num], F_OK) == 0) { tsfTopo.dx[tsfTopo.num] = i; tsfTopo.num++ ; }
    }
    for ( i = 10; i<=40 ; i += 10 ) {
      sprintf(tsfTopo.fn[tsfTopo.num], "%s/topo.%02dkm.ts", path, i);
      if (access(tsfTopo.fn[tsfTopo.num], F_OK) == 0) { tsfTopo.dx[tsfTopo.num] = i; tsfTopo.num++ ; }
    }

    for ( i = 1; i < 10 ; i++ ) {
      sprintf(tsfLU.fn[tsfLU.num], "%s/glcc.usgs20.%02dkm.ts", path, i);
      if (access(tsfLU.fn[tsfLU.num], F_OK) == 0) { tsfLU.dx[tsfLU.num] = i; tsfLU.num++ ; }
    }
    for ( i = 10; i<=40 ; i += 10 ) {
      sprintf(tsfLU.fn[tsfLU.num], "%s/glcc.usgs20.%02dkm.ts", path, i);
      if (access(tsfLU.fn[tsfLU.num], F_OK) == 0) { tsfLU.dx[tsfLU.num] = i; tsfLU.num++ ; }
    }

    for ( i = 1; i < 10 ; i++ ) {
      sprintf(tsfOcean.fn[tsfOcean.num], "%s/tbase.%02dkm.ts", path, i);
      if (access(tsfOcean.fn[tsfOcean.num], F_OK) == 0) { tsfOcean.dx[tsfLU.num] = i; tsfOcean.num++ ; }
    }
    for ( i = 10; i<=40 ; i += 10 ) {
      sprintf(tsfOcean.fn[tsfOcean.num], "%s/tbase.%02dkm.ts", path, i);
      if (access(tsfOcean.fn[tsfOcean.num], F_OK) == 0) { tsfOcean.dx[tsfOcean.num] = i; tsfOcean.num++ ; }
    }
  }
# if 0
  for ( i = 0 ; i < tsfTopo.num ; i++ ) {
    fprintf(stderr,"%02d. %s\n",i, tsfTopo.fn[i]) ;
  }
  for ( i =0 ; i < tsfLU.num ; i++ ) {
    fprintf(stderr,"%02d. %s\n",i, tsfLU.fn[i]) ;
  }
  for ( i =0 ; i < tsfOcean.num ; i++ ) {
    fprintf(stderr,"%02d. %s\n",i, tsfOcean.fn[i]) ;
  }
# endif

  return(1);

}

int GET_LANDUSE (        float *adx,
			 float *xlat,
			 float *xlon,
			 float       *landuse,
			 int   *mix,
			 int   *mjx,
			 int   *iyyn,
			 int   *jxxn,
                         int   *ipath , int * ipathlen ) /* integer coded ASCII string from Funtran and len */
{
  int i, j ;
  char path[256];
  int ix,iy, offset ;
  float lat, lon, tv;
  double fix, fiy;

  static float last_adx = 0.0 ;

  if ( tsFileInfo_initialized == 0 ) {
     for (i = 0 ; i < *ipathlen ; i++ ) {
       path[i] = ipath[i] ;
     }
     path[*ipathlen] = '\0' ;
     tsFileInfo_initialized = tsInitFileInfo(path);
     if ( tsFileInfo_initialized == -1 ) { return(1); }
  }

/* if ( fabs(last_adx - *adx)  > 1.0E-6 ) { */
/*   last_adx = *adx; */
    if ( tsfLU.num > 0 ) {
      strcpy(tsfLU_fn, tsfLU.fn[tsfLU.num-1]);
      for ( i = 0; i < tsfLU.num; i++) {
# if 0
          fprintf(stderr,"%d fn %s dx %f adx %f\n",i,tsfLU.fn[i],tsfLU.dx[i],*adx) ;
# endif
          if (tsfLU.dx[i] > *adx) {
              strcpy(tsfLU_fn, tsfLU.fn[i-1]);
              break;
            }
        }
    } else {
# ifndef MS_SUA
        fprintf(stderr, "Not found LANDUSE datasets!\n");
# endif
        return(1);
    }
/* } */

  /* Get the land use. */
  if (tsInitTileSet(tsfLU_fn)) { return(1); }

  for ( j = 0; j < *jxxn; j++) {
    offset = *mix*j;
    for ( i = 0; i < *iyyn; i++) {
      lat = xlat[offset + i];
      lon = xlon[offset + i];

      tsLatLonToGridpoint(lat,lon,&fix,&fiy);
      ix = nint(fix);
      iy = nint(fiy);
      tv = tsGetValueInt(ix, iy);

      /* Set out-of-range values to water. */
      if (tv < 0.9 || tv > 24.1) tv = 16.0;

      landuse[offset + i] = tv;
    }
  }
  tsCloseTileSet();
}

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
  int i, j ;
  char path[256];
  int ix, iy, offset ;
  float lon, lat, tv;
  double fix, fiy;

  static float last_adx = 0.0 ;

  if ( tsFileInfo_initialized == 0 ) { 
     for (i = 0 ; i < *ipathlen ; i++ ) {
       path[i] = ipath[i] ;
     }
     path[*ipathlen] = '\0' ;
     tsFileInfo_initialized = tsInitFileInfo(path); 
     if ( tsFileInfo_initialized == -1 ) { return(1); }
  }

  /* Use the data with the largest spacing less than the grid
     spacing specified in the argument list. */
/* if ( fabs(last_adx - *adx)  > 1.0E-6 ) { */
/*  last_adx = *adx; */
    if ( tsfTopo.num > 0 ) {
      strcpy(tsfTopo_fn, tsfTopo.fn[tsfTopo.num-1]);
      for (i = 0; i < tsfTopo.num; i++) {
#if 0
fprintf(stderr,"%d fn %s dx %f adx %f\n",i,tsfTopo.fn[i],tsfTopo.dx[i],*adx ) ;
#endif
	  if ( tsfTopo.dx[i] > *adx) {
	      strcpy(tsfTopo_fn, tsfTopo.fn[i-1]);
              break;
	    }
        }
    } else {
#ifndef MS_SUA
      fprintf(stderr,"Not found GTOPO datasets\n");
#endif
      return(1);
    }

#ifdef TERRAIN_TBASE
    if ( tsfOcean.num > 0 ) {
      strcpy(tsfOcean_fn, tsfOcean.fn[tsfOcean.num-1]);
      for ( i = 0; i < tsfOcean.num; i++) {
          if (tsfOcean.dx[i] > *adx) {
            strcpy(tsfOcean_fn, tsfOcean.fn[i-1]);
            break;
          }
      }
    } else {
# ifndef MS_SUA
        fprintf(stderr, "Not found TBASE datasets!\n");
# endif
        return(1);
    }
#endif
/* } */

  /* First get the terrain from GTOPO30. */
    if (tsInitTileSet(tsfTopo_fn)) { return(1); }

    for ( j = 0; j < *jxxn; j++)
      { offset = *mix*j;
	for ( i = 0; i < *iyyn; i++)
	  {
	    lat = xlat[offset + i];
	    lon = xlon[offset + i];
	    
	    tsLatLonToGridpoint(lat,lon,&fix,&fiy);
	    tv = tsGetValue(fix, fiy);
	    terrain[offset + i] = tv;
	  }
      }
    tsCloseTileSet();

#ifdef TERRAIN_TBASE
  /* Next get the terrain from TBASE. */
    if (tsInitTileSet(tsfOcean_fn)) { return(1); }

    for ( j = 0; j < *jxxn; j++)
      { offset = *mix*j;
	for ( i = 0; i < *iyyn; i++)
	  {
	    lat = xlat[offset + i];
	    lon = xlon[offset + i];
	    
	    tsLatLonToGridpoint(lat,lon,&fix,&fiy);
	    tv = tsGetValue(fix, fiy);
	    if (isMissing(terrain[offset+i]))
	      {
		if (tv < 0.0) tv = 0.0;
		terrain[offset + i] = tv;
	      }
	  }
      }
    tsCloseTileSet();
#endif
  return(0);
}

#ifdef BATHYMETRY
int get_bathymetry_(float *tadx,
		    float *xlat,
		    float *xlon,
		    float       *depth,
		    int   *mix,
		    int   *mjx,
		    int   *iyyn,
		    int   *jxxn,
		    float *mindepth,
		    float *zlimww3,
                    int   *ipath , int * ipathlen)  /* integer coded ASCII string from Funtran and len */
{
  int i, j;
  char fn[MAXLEN];
  char path[1024];
  float maxdx, tv;
  double fix, fiy, lat, lon;
  int ix,iy,nx,ny;
  int offset;

  nx=*mix;
  ny=*mjx;

  /* Set grid resolution to .1 km to get highest resolution data possible. */
  float adx = 0.1;

  if ( tsFileInfo_initialized == 0 ) {
     for (i = 0 ; i < *ipathlen ; i++ ) {
       path[i] = ipath[i] ;
     }
     path[*ipathlen] = '\0' ;
     tsFileInfo_initialized = tsInitFileInfo(path);
     if ( tsFileInfo_initialized == -1 ) { return(1); }
  }

  /* Get the water depth from TBASE. */
  if ( tsfOcean.num > 0 ) {
    /* Use the data with highest resolution possible. */
    maxdx = 0.0;
    strcpy(fn, tsfOcean.fn[tsfOcean.num-1]);
    for ( i = 0; i < tsfOcean.num; i++)
      {
	if (tsfOcean.dx[i] < maxdx) continue;
	if ( tsfOcean.dx[i] < adx)
	  {
	    maxdx = tsfOcean.dx[i];
	    strcpy(fn, tsfOcean.fn[i]);
	  }
      }

    if (tsInitTileSet(fn))
      {
	return(1);
      }

    for ( i = 0; i < nx*ny; i++)
      {
	depth[i] = vmiss;
      }
    
    for ( j = 0; j < *jxxn; j++)
      { offset = nx * j;
	for ( i = 0; i < *iyyn; i++)
	  {
	    lat = xlat[offset + i];
	    lon = xlon[offset + i];
	    
	    tsLatLonToGridpoint(lat,lon,&fix,&fiy);
	    tv = tsGetValue(fix, fiy);
	    if (isMissing(depth[offset+i]))
	      {
		depth[offset + i] = -tv;
	      }
	  }
      }
    tsCloseTileSet();
  } else {
# ifndef MS_SUA
        fprintf(stderr, "Not found TBASE datasets!\n");
# endif
        return(1);
  }

  /* Next get the land use. */
  if ( tsfLU.num > 0 ) {
    /* Use the data with the largest spacing less than the grid
       spacing specified in the argument list. */
    maxdx = 0.0;
    strcpy(fn, tsfLU.fn[tsfLU.num-1]);
    for ( i = 0; i < tsfLU.num; i++)
      {
	if (tsfLU.dx[i] < maxdx) continue;
	if (tsfLU.dx[i] < adx)
	  {
	    maxdx = tsfLU.dx[i];
	    strcpy(fn, tsfLU.fn[i]);
	  }
      }

    if (tsInitTileSet(fn))
      {
	return(1);
      }

    for ( j = 0; j < *jxxn; j++)
      { offset = nx*j;
	for ( i = 0; i < *iyyn; i++)
	  {
	    lat = xlat[offset + i];
	    lon = xlon[offset + i];
	    
	    tsLatLonToGridpoint(lat,lon,&fix,&fiy);
	    ix = nint(fix);
	    iy = nint(fiy);
	    tv = tsGetValueInt(ix, iy);

            /* Set out-of-range values to water. */
            if (tv < 0.9 || tv > 24.1) tv = 16.0;

	    if (fabs(tv - 16.0) < 0.1)
	      {
		/* Water. */
		if (1)
		  {
		    if (depth[offset + i] < *mindepth) depth[offset + i] = *mindepth;
		  }
		else
		  {
		    if (depth[offset + i] < -(*zlimww3) )
		      {
			/* Water depth below zlimww3, so turn this point 
			   into land. */
			depth[offset + i] = -0.1;		    
		      }
		    else if (depth[offset + i] < *mindepth)
		      {
			depth[offset + i] = *mindepth;
		      }
		  }
	      }
	    else
	      {
		/* Land. Set depth to 0.0. */
		depth[offset + i] = 0.0;
	      }
	  }
      }
    tsCloseTileSet();
  } else {
# ifndef MS_SUA
        fprintf(stderr, "Not found LANDUSE datasets!\n");
# endif
        return(1);
  }
  return(0);
}
#endif

#endif 
