#include "gribfuncs.h"
#include "read_grib.h"
#include "gribmap.h"

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define GET_FILEINDEX_SIZE get_fileindex_size
#      define INDEX_FILE index_file
#      define GET_METADATA_VALUE get_metadata_value
#      define GET_GRIB_INDEX get_grib_index
#      define GET_GRIB_INDEX_GUESS get_grib_index_guess
#      define GET_GRIB_INDEX_VALIDTIME get_grib_index_validtime
#      define GET_GRIB_INDEX_VALIDTIME_GUESS get_grib_index_validtime_guess
#      define GET_GRIB_INDICES get_grib_indices
#      define ALLOC_INDEX_FILE alloc_index_file
#      define FREE_INDEX_FILE free_index_file
#      define GET_NUM_TIMES get_num_times
#      define GET_LEVEL1 get_level1
#      define GET_LEVEL2 get_level2
#      define GET_TIME get_time
#      define GET_GRID_INFO_SIZE get_grid_info_size
#      define LOAD_GRID_INFO load_grid_info
#      define FREE_GRID_INFO free_grid_info
#      define PRINT_GRID_INFO print_grid_info
#      define READ_GRIB read_grib
#      define WRITE_GRIB write_grib
#      define GET_GRIB_SEARCH_SIZE get_grib_search_size
#      define LOAD_GRIB_SEARCH load_grib_search
#      define FREE_GRIB_SEARCH free_grib_search
#      define GET_SIZEOF_GRID get_sizeof_grid
#      define GET_GRIB_CENTER get_grib_center
#      define GET_GRIB_SUBCENTER get_grib_subcenter
#      define GET_GRIB_TBLVERSION get_grib_tblversion
#      define GET_GRIB_PROCID get_grib_procid
# else
#   ifdef F2CSTYLE
#      define GET_FILEINDEX_SIZE get_fileindex_size__
#      define INDEX_FILE index_file__
#      define GET_METADATA_VALUE get_metadata_value__
#      define GET_GRIB_INDEX get_grib_index__
#      define GET_GRIB_INDEX_GUESS get_grib_index_guess__
#      define GET_GRIB_INDEX_VALIDTIME_GUESS get_grib_index_validtime_guess__
#      define GET_GRIB_INDICES get_grib_indices__
#      define ALLOC_INDEX_FILE alloc_index_file__
#      define FREE_INDEX_FILE free_index_file__
#      define GET_NUM_TIMES get_num_times__
#      define GET_LEVEL1 get_level1__
#      define GET_LEVEL2 get_level2__
#      define GET_TIME get_time__
#      define GET_GRID_INFO_SIZE get_grid_info_size__
#      define LOAD_GRID_INFO load_grid_info__
#      define FREE_GRID_INFO free_grid_info__
#      define PRINT_GRID_INFO print_grid_info__
#      define READ_GRIB read_grib__
#      define WRITE_GRIB write_grib__
#      define GET_GRIB_SEARCH_SIZE get_grib_search_size__
#      define LOAD_GRIB_SEARCH load_grib_search__
#      define FREE_GRIB_SEARCH free_grib_search__
#      define GET_SIZEOF_GRID get_sizeof_grid__
#      define GET_GRIB_CENTER get_grib_center__
#      define GET_GRIB_SUBCENTER get_grib_subcenter__
#      define GET_GRIB_TBLVERSION get_grib_tblversion__
#      define GET_GRIB_PROCID get_grib_procid__
#   else
#      define GET_FILEINDEX_SIZE get_fileindex_size_
#      define INDEX_FILE index_file_
#      define GET_METADATA_VALUE get_metadata_value_
#      define GET_GRIB_INDEX get_grib_index_
#      define GET_GRIB_INDEX_GUESS get_grib_index_guess_
#      define GET_GRIB_INDEX_VALIDTIME get_grib_index_validtime_
#      define GET_GRIB_INDEX_VALIDTIME_GUESS get_grib_index_validtime_guess_
#      define GET_GRIB_INDICES get_grib_indices_
#      define ALLOC_INDEX_FILE alloc_index_file_
#      define FREE_INDEX_FILE free_index_file_
#      define GET_NUM_TIMES get_num_times_
#      define GET_LEVEL1 get_level1_
#      define GET_LEVEL2 get_level2_
#      define GET_TIME get_time_
#      define GET_GRID_INFO_SIZE get_grid_info_size_
#      define LOAD_GRID_INFO load_grid_info_
#      define FREE_GRID_INFO free_grid_info_
#      define PRINT_GRID_INFO print_grid_info_
#      define READ_GRIB read_grib_
#      define WRITE_GRIB write_grib_
#      define GET_GRIB_SEARCH_SIZE get_grib_search_size_
#      define LOAD_GRIB_SEARCH load_grib_search_
#      define FREE_GRIB_SEARCH free_grib_search_
#      define GET_SIZEOF_GRID get_sizeof_grid_
#      define GET_GRIB_CENTER get_grib_center_
#      define GET_GRIB_SUBCENTER get_grib_subcenter_
#      define GET_GRIB_TBLVERSION get_grib_tblversion_
#      define GET_GRIB_PROCID get_grib_procid_
#   endif
# endif
#endif

typedef struct {
  char VarName[100];
  char DateStr[100];
  char Element[100];
  char Value[1000];
} MetaData_Elements;

typedef struct {
  int num_elements;
  MetaData_Elements *elements;
} MetaData;

typedef struct {
  char valid_time[30];
} Times_Elements;

typedef struct {
  int num_elements;
  Times_Elements *elements;
} Times;

typedef struct {
  MetaData *metadata;
  GribInfo *gribinfo;
  Times *times;
} FileIndex;

typedef struct {
  char varname[200];
  char initdate[20];
  int leveltype;
  int level1;
  int level2;
  float fcst_time;
  int accum_period;
  int grid_id;
  int projection;
  int xpoints;
  int ypoints;
  float center_lat;
  float center_lon;
  float Di;
  float Dj;
  float central_lon;
  int proj_center_flag;
  float latin1;
  float latin2;
  Grib1_Tables *grib_tables;
} Grid_Info;

int GET_FILEINDEX_SIZE(int *size);

int ALLOC_INDEX_FILE(FileIndex *fileindex);
void FREE_INDEX_FILE(FileIndex *fileindex);

int INDEX_FILE(int *fid, FileIndex *fileindex);

int GET_METADATA_VALUE(FileIndex *fileindex, char Element[], char DateStr[], 
		       char VarName[], char Value[], int *stat, int strlen1, 
		       int strlen2, int strlen3, int strlen4, int strlen5);

int GET_GRIB_INDEX(FileIndex *fileindex,
		   int *center, int *subcenter, int *parmtbl,
		   int *parmid, char DateStrIn[], 
		   int *leveltype, int *level1, int *level2, int *fcsttime1,
		   int *fcsttime2, int *index, int strlen1, int strlen2);

int GET_GRIB_INDEX_GUESS(FileIndex *fileindex, int *center, int *subcenter, 
			 int *parmtbl, int *parmid, char DateStrIn[], 
			 int *leveltype, int *level1, int *level2, 
			 int *fcsttime1,int *fcsttime2, int *guessidx, 
			 int *index, int strlen1, int strlen2);

int GET_GRIB_INDICES(FileIndex *fileindex, int *center, int *subcenter, 
		     int *parmtbl,int *parmid, char DateStrIn[], 
		     int *leveltype, int *level1, int *level2, int *fcsttime1,
		     int *fcsttime2, int *indices, int *num_indices, 
		     int strlen1, int strlen2);

int GET_NUM_TIMES(FileIndex *fileindex, int *numtimes);

int GET_TIME(FileIndex *fileindex, int *idx, char time[]);

int GET_GRID_INFO_SIZE(int *size);

int LOAD_GRID_INFO(char *varname, char *initdate, int *leveltype, 
		   int *level1, int *level2, float *fcst_time, 
		   int *accum_period, int *grid_id, int *projection, 
		   int *xpoints, int *ypoints, float *center_lat, 
		   float *center_lon, float *Di, float *Dj,float *central_lon,
		   int *proj_center_flag, float *latin1, 
		   float *latin2, Grib1_Tables *grib_tables,
		   Grid_Info *grid_info, int strlen1, int strlen2);

int PRINT_GRID_INFO(Grid_Info *grid_info);

int WRITE_GRIB(Grid_Info *grid_info, int *filefd, float *data);

int READ_GRIB(FileIndex *fileindex, int *fid, int *index, float *data);

int GET_SIZEOF_GRID(FileIndex *fileindex, int *index, int *numcols, 
		    int *numrows);

int GET_LEVEL1(FileIndex *fileindex, int *idx, int *level1);

int GET_LEVEL2(FileIndex *fileindex, int *idx, int *level2);

int GET_GRIB_INDEX_VALIDTIME(FileIndex *fileindex, 
			     int *center, int *subcenter, int *parmtbl,
			     int *parmid, 
			     char DateStrIn[], int *leveltype, int *level1, 
			     int *level2, int *index, int strlen1, 
			     int strlen2);

int GET_GRIB_INDEX_VALIDTIME_GUESS(FileIndex *fileindex, int *center, 
				   int *subcenter, int *parmtbl, int *parmid, 
				   char DateStrIn[], int *leveltype, 
				   int *level1, int *level2, int *guessidx, 
				   int *index, int strlen1, int strlen2);

int GET_GRIB_CENTER(FileIndex *fileindex, int *parmid, int *center);

int GET_GRIB_SUBCENTER(FileIndex *fileindex, int *parmid, int *subcenter);

int GET_GRIB_TBLVERSION(FileIndex *fileindex, int *parmid, int *parmtbl);

int GET_GRIB_PROCID(FileIndex *fileindex, int *parmid, int *proc_id);

int GET_REGION_CENTER(char *MemoryOrderIn, int *projection, 
		      float *domain_center_lat, 
		      float *domain_center_lon, int *full_xsize, 
		      int *full_ysize, float *dx, float *dy, 
		      float *proj_central_lon, 
		      int *proj_center_flag, float *truelat1, 
		      float *truelat2, int *region_xsize, int *region_ysize, 
		      float *region_center_lat, float *region_center_lon, 
		      int strlen1);
