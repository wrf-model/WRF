/*
****************************************************************************
*
*  Routines for indexing, reading and writing grib files.  Routines
*    are designed to be called by Fortran.
*
*  All routines return 0 for success, 1 for failure, unless otherwise noted. 
*
*  Todd Hutchinson
*  WSI
*  05/17/2005
*
****************************************************************************
*/

#include "grib1_routines.h"
#include "gridnav.h"
#include "wrf_projection.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>

char *trim (char *str);
int index_metadata(GribInfo *gribinfo, MetaData *metadata, int fid);
int index_times(GribInfo *gribinfo, Times *times);
int find_time(Times *times, char valid_time[15]);
int get_gridnav_projection(int wrf_projection);
int get_byte(int input_int, int bytenum);

/* 
 * Allocate space for the fileindex structure
 */

int ALLOC_INDEX_FILE(FileIndex *fileindex)
{
  int status = 0;
  
  fileindex->gribinfo = (GribInfo *)malloc(sizeof(GribInfo));
  if (fileindex->gribinfo == NULL) {
    fprintf(stderr,"Allocating fileindex->gribinfo failed.\n");
    status = 1;
    return status;
  }
  
  fileindex->metadata = (MetaData *)malloc(sizeof(MetaData));
  if (fileindex->metadata == NULL) {
    fprintf(stderr,"Allocating fileindex->metadata failed.\n");
    status = 1;
    return status;
  }
  fileindex->metadata->elements = NULL;
  
  fileindex->times = (Times *)malloc(sizeof(Times));
  if (fileindex->times == NULL) {
    fprintf(stderr,"Allocating fileindex->times failed.\n");
    status = 1;
    return status;
  }
  fileindex->times->elements = NULL;
  
  return status;
}


void FREE_INDEX_FILE(FileIndex *fileindex)
{
  int status = 0;
  
  rg_free_gribinfo_elements(fileindex->gribinfo);
  free(fileindex->gribinfo);

  free(fileindex->metadata->elements);
  free(fileindex->metadata);

  free(fileindex->times->elements);
  free(fileindex->times);

}


int INDEX_FILE(int *fid, FileIndex *fileindex)
{

  int status;
  /* Index the grib records */

  status = rg_setup_gribinfo_i(fileindex->gribinfo,*fid,1);
  if (status < 0) {
    fprintf(stderr,"Error setting up gribinfo structure.\n");
    return 1;
  }

  /* Index the metadata section */

  status = index_metadata(fileindex->gribinfo, fileindex->metadata, *fid);
  if (status != 0) {
    fprintf(stderr,"Error setting up metadata structure.\n");
    return 1;
  }

  /* Setup a list of times based on times in grib records */

  status = index_times(fileindex->gribinfo, fileindex->times);
  if (status != 0) {
    fprintf(stderr,"Error indexing times in grib file.\n");
    return 1;
  }
  
  return 0;
}


int GET_FILEINDEX_SIZE(int *size)
{
  *size = sizeof(FileIndex);
  return *size;
}


int GET_NUM_TIMES(FileIndex *fileindex, int *numtimes)
{
  *numtimes = (fileindex->times)->num_elements;
  return *numtimes;
}


int GET_TIME(FileIndex *fileindex, int *idx, char time[])
{
  int num_times;
  int year, month, day, minute, hour, second;
  char time2[100];

  num_times = GET_NUM_TIMES(fileindex,&num_times);
  if (*idx > num_times) 
    {
      fprintf(stderr,"Tried to get time %d, but only %d times exist\n",
	      *idx, num_times);
      return 1;
    }

  strcpy(time,fileindex->times->elements[*idx-1].valid_time);
  
  /* Reformat time to meet WRF time format */

  sscanf(time, "%4d%2d%2d%2d%2d%2d", 
	 &year, &month, &day, &hour, &minute, &second);
  sprintf(time2, "%04d-%02d-%02d_%02d:%02d:%02d", 
	  year, month, day, hour, minute, second);
  strncpy(time,time2,19);

  return 0;
}


int GET_LEVEL1(FileIndex *fileindex, int *idx, int *level1)
{

  *level1 = (fileindex->gribinfo)->elements[*idx].usHeight1;

  return *level1;

}


int GET_LEVEL2(FileIndex *fileindex, int *idx, int *level2)
{

  *level2 = (fileindex->gribinfo)->elements[*idx].usHeight2;

  return *level2;

}


int index_metadata(GribInfo *gribinfo, MetaData *metadata, int fid)
{
  int status=0;
  int end;
  char string[11];
  int found_metadata=0;
  int idx;
  int pos;
  int fileend;
  int seekpos;
  int bytesread;
  char line[1000];
  char element[100],datestr[100],varname[100];
  char value[1000];
  int incomment;
  int charidx;
  int elemidx=0;
  FILE *stream;


  /* Associate a FILE *stream with the file id */
  stream = fdopen(fid,"r");
  if (stream == NULL) 
    {
      perror("Error associating stream with file descriptor");
      status = -1;
      return status;
    }
  
  /* 
   * First, set the position to end of grib data (the 
   *   metadata section comes after the grib data).
   */
  idx = rg_num_elements(gribinfo) - 1;
  end = rg_get_end(gribinfo,idx);
  pos = end + 1;
  fileend = lseek(fid,0,SEEK_END);

  /*
   * Now, start searching for metadata 
   */
  while (pos < (fileend - 10))
    {
      seekpos = pos;
      pos = lseek(fid,seekpos,SEEK_SET);
      if (pos != seekpos) 
	{
	  fprintf(stderr,"Error seeking %d bytes in file\n",end);
	  perror("");
	  return 1;
	}

      bytesread = read(fid,string,10);
      if (bytesread != 10) 
	{
	  fprintf(stderr,"Invalid read, pos: %d :\n",pos);
	  perror("");
	  pos += 1;
	  continue;
	}

      if (strncmp(string,"<METADATA>",10) == 0) 
	{
	  /* We found it, so break out ! */
	  found_metadata = 1;
	  break;
	}
      pos += 1;
    }


  /* Now, read metadata, line by line */
  incomment = 0;
  while(fgets(line,1000,stream) != NULL)
    {
      trim(line);

      /* Set comment flag, if we found a comment */
      if (strncmp(line,"<!--",4) == 0)
	{
	  incomment = 1;
	  strcpy(line,line+4);
	}
      
      /* Search for end of comment */
      if (incomment)
	{
	  charidx = 0;
	  while (charidx < strlen(line)) 
	    {
	      
	      if (strncmp(line+charidx,"-->",3) == 0)
		{
		  strcpy(line,line+charidx+3);
		  incomment = 0;
		  break;
		}
	      else
		{
		  charidx++;
		}
	    }
	}

      if (incomment) continue;
  

      /* Check for end of metadata */
      if (strncmp(line,"</METADATA>",11) == 0) 
	{
	  /* We found end of data, so, break out */
	  break;
	}

      /* Skip blank lines */
      if (strlen(line) == 0) continue;


      /* Parse line */
      trim(line);
      strcpy(element,"none");
      strcpy(datestr,"none");
      strcpy(varname,"none");
      strcpy(value,"none");
      if (sscanf(line,"%[^;=];%[^;=];%[^;=]=%[^\n]",varname,datestr,
		 element,value) == 4)
	{
	}
      else if (sscanf(line,"%[^;=];%[^;=]=%[^\n]",datestr,element,value) == 3)
	{
	  strcpy(varname,"none");
	}
      else if (sscanf(line,"%[^;=]=%[^\n]",element,value) == 2)
	{
	  strcpy(varname,"none");
	  strcpy(datestr,"none");
	}
      else 
	{
	  strcpy(varname,"none");
	  strcpy(datestr,"none");
	  strcpy(element,"none");
	  strcpy(value,"none");
	  fprintf(stderr,"Invalid line in metadata: \n%s",line);
	}

      trim(varname);
      trim(datestr);
      trim(element);
      trim(value);

      metadata->elements = 
	(MetaData_Elements *)realloc( metadata->elements, 
				     (elemidx+1)*sizeof(MetaData_Elements) );
      strcpy(metadata->elements[elemidx].VarName,varname);
      strcpy(metadata->elements[elemidx].DateStr,datestr);
      strcpy(metadata->elements[elemidx].Element,element);
      strcpy(metadata->elements[elemidx].Value,value);

      elemidx++;
	
    }

  metadata->num_elements = elemidx;
  
  return 0;
}




int index_times(GribInfo *gribinfo, Times *times)
{
  int idx;
  int status;
  int numtimes=0;
  int date;
  char valid_time[15];
  char tmp[15];
  int swapped;

  times->num_elements = 0;

  /* Loop through elements, and build list of times */

  for (idx=0; idx < gribinfo->num_elements; idx++) 
    {
      /* Calculate valid time */
      status = rg_get_valid_time(gribinfo,idx,valid_time);
      if (status != 0) 
	{
	  fprintf(stderr,"Could not retrieve valid time for index: %d\n",idx);
	  continue;
	}

      /* 
       * Check if this time is already contained in times
       *  If not, allocate space for it, and add it to list 
       */
      if (find_time(times,valid_time) < 0) 
	{
	  times->num_elements++;
	  times->elements = 
	    (Times_Elements *)
	    realloc(times->elements,times->num_elements*sizeof(Times_Elements));
	  if (times->elements == NULL) 
	    {
	      fprintf(stderr,"Allocating times->elements failed.\n");
	      status = 1;
	      return status;
	    }
	  strcpy(times->elements[times->num_elements - 1].valid_time,valid_time);
	}
    }

  /* Sort times */
  swapped = 1;
  while (swapped)
    {
      swapped=0;
      for (idx=1; idx < times->num_elements; idx++) 
	{
	  if (strcmp(times->elements[idx-1].valid_time,
		     times->elements[idx].valid_time) > 0)
	    {
	      strcpy(tmp,times->elements[idx-1].valid_time);
	      strcpy(times->elements[idx-1].valid_time,
		     times->elements[idx].valid_time);
	      strcpy(times->elements[idx].valid_time, tmp);
	      swapped = 1;
	    }
	}
    }

  return 0;
}



int find_time(Times *times, char valid_time[15])
{
  int idx;
  int found_elem = -1;

  for (idx = 0; idx < times->num_elements; idx++) 
    {
      if (strcmp(times->elements[idx].valid_time,valid_time) == 0) 
	{
	  found_elem = idx;
	  break;
	}
    }

  return found_elem;

}


int GET_METADATA_VALUE(FileIndex *fileindex, char ElementIn[], 
		       char DateStrIn[], char VarNameIn[], char Value[], 
		       int *stat, int strlen1, int strlen2, int strlen3, 
		       int strlen4, int strlen5)
{
  int elemidx;
  int elemnum;
  char VarName[200];
  char DateStr[200];
  char Element[200];
  int Value_Len;

  *stat = 0;

  strncpy(Element,ElementIn,strlen2);
  Element[strlen2] = '\0';
  strncpy(DateStr,DateStrIn,strlen3);
  DateStr[strlen3] = '\0';
  strncpy(VarName,VarNameIn,strlen4);
  VarName[strlen4] = '\0';
  Value_Len = strlen5;

  elemnum = -1;
  for (elemidx = 0; elemidx < fileindex->metadata->num_elements; elemidx++)
    {
      if (strcmp(Element,fileindex->metadata->elements[elemidx].Element) == 0)
	{
	  if (strcmp(DateStr,fileindex->metadata->elements[elemidx].DateStr) 
	      == 0)
	    {
	      if (strcmp(VarName,
			 fileindex->metadata->elements[elemidx].VarName) == 0)
		{
		  elemnum = elemidx;
		  break;
		}
	    }
	}
    }

  if (elemnum != -1)
    {
      strncpy(Value, fileindex->metadata->elements[elemnum].Value, Value_Len);
    } 
  else
    {
      strncpy(Value, "none", Value_Len);
      *stat = 1;
    }

  /* 
   * Pad end of string with one space.  This allows Fortran internal
   *   read function to work properly.
   */
  if (strlen(Value) < Value_Len) 
    {
      strcpy(Value + strlen(Value), " ");
    }

  return elemidx;
}


int GET_GRIB_INDEX(FileIndex *fileindex, int *center, int *subcenter, 
		   int *parmtbl, int *parmid, char DateStrIn[], 
		   int *leveltype, int *level1, int *level2, int *fcsttime1,
		   int *fcsttime2, int *index, int strlen1, int strlen2)
{
  char DateStr[1000];
  FindGrib findgrib;

  strncpy(DateStr,DateStrIn,strlen2);
  DateStr[strlen2] = '\0';
  grib_time_format(DateStr,DateStr);

  rg_init_findgrib(&findgrib);

  strncpy(findgrib.initdate,DateStrIn,strlen2);
  findgrib.initdate[strlen2] = '\0';
  findgrib.parmid          = *parmid;
  findgrib.leveltype       = *leveltype;
  findgrib.level1          = *level1;
  findgrib.level2          = *level2;
  findgrib.fcsttime1       = *fcsttime1;
  findgrib.fcsttime2       = *fcsttime2;
  findgrib.center_id       = *center;
  findgrib.subcenter_id    = *subcenter;
  findgrib.parmtbl_version = *parmtbl;
  
  *index = rg_get_index(fileindex->gribinfo, &findgrib);

  return *index;

}


int GET_GRIB_INDEX_GUESS(FileIndex *fileindex, int *center, int *subcenter, 
			 int *parmtbl, int *parmid, char DateStrIn[], 
			 int *leveltype, int *level1, int *level2, 
			 int *fcsttime1,int *fcsttime2, int *guessidx, 
			 int *index, int strlen1, int strlen2)
{
  char DateStr[1000];
  FindGrib findgrib;

  strncpy(DateStr,DateStrIn,strlen2);
  DateStr[strlen2] = '\0';
  grib_time_format(DateStr,DateStr);

  rg_init_findgrib(&findgrib);

  strncpy(findgrib.initdate,DateStrIn,strlen2);
  findgrib.initdate[strlen2] = '\0';
  findgrib.parmid          = *parmid;
  findgrib.leveltype       = *leveltype;
  findgrib.level1          = *level1;
  findgrib.level2          = *level2;
  findgrib.fcsttime1       = *fcsttime1;
  findgrib.fcsttime2       = *fcsttime2;
  findgrib.center_id       = *center;
  findgrib.subcenter_id    = *subcenter;
  findgrib.parmtbl_version = *parmtbl;
  
  *index = rg_get_index_guess(fileindex->gribinfo, &findgrib, *guessidx);

  return *index;

}


int GET_GRIB_CENTER(FileIndex *fileindex, int *parmid, int *center)
{

  *center = rg_get_center_id(fileindex->gribinfo,*parmid);

  return *center;

}


int GET_GRIB_SUBCENTER(FileIndex *fileindex, int *parmid, int *subcenter)
{

  *subcenter = rg_get_subcenter_id(fileindex->gribinfo,*parmid);

  return *subcenter;

}


int GET_GRIB_TBLVERSION(FileIndex *fileindex, int *parmid, int *parmtbl)
{

  *parmtbl = rg_get_parmtbl(fileindex->gribinfo,*parmid);

  return *parmtbl;

}


int GET_GRIB_PROCID(FileIndex *fileindex, int *parmid, int *proc_id)
{

  *proc_id = rg_get_proc_id(fileindex->gribinfo,*parmid);

  return *proc_id;

}


int GET_GRIB_INDEX_VALIDTIME(FileIndex *fileindex, int *center, 
		   int *subcenter, int *parmtbl, int *parmid, 
		   char DateStrIn[], int *leveltype, int *level1, int *level2,
		   int *index, int strlen1, int strlen2)
{
  char DateStr[1000];
  FindGrib findgrib;

  strncpy(DateStr,DateStrIn,strlen2);
  DateStr[strlen2] = '\0';
  grib_time_format(DateStr,DateStr);

  rg_init_findgrib(&findgrib);

  strncpy(findgrib.validdate,DateStr,strlen2);
  findgrib.initdate[strlen2] = '\0';
  findgrib.parmid          = *parmid;
  findgrib.leveltype       = *leveltype;
  findgrib.level1          = *level1;
  findgrib.level2          = *level2;
  findgrib.center_id       = *center;
  findgrib.subcenter_id    = *subcenter;
  findgrib.parmtbl_version = *parmtbl;
  
  *index = rg_get_index(fileindex->gribinfo, &findgrib);

  return *index;
}


int GET_GRIB_INDEX_VALIDTIME_GUESS(FileIndex *fileindex, int *center, 
				   int *subcenter, int *parmtbl, int *parmid, 
				   char DateStrIn[], int *leveltype, 
				   int *level1, int *level2, int *guessidx, 
				   int *index, int strlen1, int strlen2)
{
  char DateStr[1000];
  FindGrib findgrib;

  strncpy(DateStr,DateStrIn,strlen2);
  DateStr[strlen2] = '\0';
  grib_time_format(DateStr,DateStr);

  rg_init_findgrib(&findgrib);

  strncpy(findgrib.validdate,DateStr,strlen2);
  findgrib.initdate[strlen2] = '\0';
  findgrib.parmid          = *parmid;
  findgrib.leveltype       = *leveltype;
  findgrib.level1          = *level1;
  findgrib.level2          = *level2;
  findgrib.center_id       = *center;
  findgrib.subcenter_id    = *subcenter;
  findgrib.parmtbl_version = *parmtbl;
  
  *index = rg_get_index_guess(fileindex->gribinfo, &findgrib, *guessidx);

  return *index;
}


int GET_GRIB_INDICES(FileIndex *fileindex, int *center, int *subcenter, 
		     int *parmtbl,int *parmid, char DateStrIn[], 
		     int *leveltype, int *level1, int *level2, int *fcsttime1,
		     int *fcsttime2, int *indices, int *num_indices, 
		     int strlen1, int strlen2)
{
  char DateStr[1000];
  int status;
  FindGrib findgrib;

  strncpy(DateStr,DateStrIn,strlen2);
  DateStr[strlen2] = '\0';
  grib_time_format(DateStr,DateStr);

  rg_init_findgrib(&findgrib);

  strncpy(findgrib.initdate,DateStrIn,strlen2);
  findgrib.initdate[strlen2] = '\0';
  trim(findgrib.initdate);
  findgrib.parmid          = *parmid;
  findgrib.leveltype       = *leveltype;
  findgrib.level1          = *level1;
  findgrib.level2          = *level2;
  findgrib.fcsttime1       = *fcsttime1;
  findgrib.fcsttime2       = *fcsttime2;
  findgrib.center_id       = *center;
  findgrib.subcenter_id    = *subcenter;
  findgrib.parmtbl_version = *parmtbl;

  *num_indices = rg_get_indices(fileindex->gribinfo, &findgrib, indices);

  return (*num_indices);

}


int GET_GRID_INFO_SIZE(int *size)
{

  *size = sizeof(Grid_Info);

  return *size;

}


int LOAD_GRID_INFO(char *varnameIn, char *initdateIn, int *leveltype, 
		   int *level1, int *level2, float *fcst_time, 
		   int *accum_period, int *grid_id, int *projection, 
		   int *xpoints, int *ypoints, float *center_lat, 
		   float *center_lon, float *Di, float *Dj,float *central_lon,
		   int *proj_center_flag, float *latin1, 
		   float *latin2, Grib1_Tables *grib_tables,
		   Grid_Info *grid_info, int strlen1, int strlen2)
{

  char varname[1000], initdate[1000];

  strncpy(varname,varnameIn,strlen1);
  varname[strlen1] = '\0';
  strncpy(initdate,initdateIn,strlen2);
  initdate[strlen2] = '\0';

  strcpy(grid_info->varname, varname);
  strcpy(grid_info->initdate, initdate);
  grid_info->leveltype        = *leveltype;
  grid_info->level1           = *level1 ;
  grid_info->level2           = *level2 ;
  grid_info->fcst_time        = *fcst_time ;
  grid_info->accum_period     = *accum_period ;
  grid_info->grid_id          = *grid_id ;
  grid_info->projection       = *projection ;
  grid_info->xpoints          = *xpoints ;
  grid_info->ypoints          = *ypoints ;
  grid_info->center_lat       = *center_lat ;
  grid_info->center_lon       = *center_lon;
  grid_info->Di               = *Di ;
  grid_info->Dj               = *Dj ;
  grid_info->central_lon      = *central_lon ;
  grid_info->proj_center_flag = *proj_center_flag ;
  grid_info->latin1           = *latin1 ;
  grid_info->latin2           = *latin2 ;
  grid_info->grib_tables      = copy_grib_tables(grib_tables);

  return 0;

}

int PRINT_GRID_INFO(Grid_Info *grid_info)
{

  fprintf(stdout,"varname        =%s\n",grid_info->varname);
  fprintf(stdout,"initdate       =%s\n",grid_info->initdate);
  fprintf(stdout,"leveltype      =%d\n",grid_info->leveltype);
  fprintf(stdout,"level1         =%d\n",grid_info->level1);
  fprintf(stdout,"level2         =%d\n",grid_info->level2);
  fprintf(stdout,"fcst_time      =%f\n",grid_info->fcst_time);
  fprintf(stdout,"accum_period   =%d\n",grid_info->accum_period);
  fprintf(stdout,"grid_id        =%d\n",grid_info->grid_id);
  fprintf(stdout,"projection     =%d\n",grid_info->projection);
  fprintf(stdout,"xpoints        =%d\n",grid_info->xpoints);
  fprintf(stdout,"ypoints        =%d\n",grid_info->ypoints);
  fprintf(stdout,"center_lat     =%f\n",grid_info->center_lat);
  fprintf(stdout,"center_lon     =%f\n",grid_info->center_lon);
  fprintf(stdout,"Di             =%f\n",grid_info->Di);
  fprintf(stdout,"Dj             =%f\n",grid_info->Dj);
  fprintf(stdout,"central_lon    =%f\n",grid_info->central_lon);
  fprintf(stdout,"proj_center_flag =%d\n",grid_info->proj_center_flag);
  fprintf(stdout,"latin1         =%f\n",grid_info->latin1);
  fprintf(stdout,"latin2         =%f\n",grid_info->latin2);

  return 0;

}


int GET_SIZEOF_GRID(FileIndex *fileindex, int *index, int *numcols, 
		    int *numrows)
{

  *numcols = rg_get_numcols(fileindex->gribinfo,*index);

  *numrows = rg_get_numrows(fileindex->gribinfo,*index);

  return (*numcols)*(*numrows);

}


void FREE_GRID_INFO(Grid_Info *grid_info)
{
  FREE_GRIBMAP(grid_info->grib_tables);
}


int READ_GRIB(FileIndex *fileindex, int *fid, int *index, float *data)
{
  int status;

  status = rg_get_data_1d(fileindex->gribinfo,*index,data);

  return status;
}

#define LINESIZE 300

#define SECS_IN_SEC 1
#define SECS_IN_MIN 60
#define MINS_IN_HOUR 60
#define MINS_IN_5MINS 5
#define HOURS_IN_DAY 24

#define MAX_FCST 65535
#define MAX_FCST_SECS MAX_FCST*SECS_IN_SEC
#define MAX_FCST_MINS MAX_FCST*SECS_IN_MIN
#define MAX_FCST_5MINS MAX_FCST*MINS_IN_5MINS*SECS_IN_MIN
#define MAX_FCST_HOURS MAX_FCST*MINS_IN_HOUR*SECS_IN_MIN
#define MAX_FCST_DAYS MAX_FCST*HOURS_IN_DAY*MINS_IN_HOUR*SECS_IN_MIN

#define MAX1B_FCST 256
#define MAX1B_FCST_SECS MAX1B_FCST*SECS_IN_SEC
#define MAX1B_FCST_MINS MAX1B_FCST*SECS_IN_MIN
#define MAX1B_FCST_5MINS MAX1B_FCST*MINS_IN_5MINS*SECS_IN_MIN
#define MAX1B_FCST_HOURS MAX1B_FCST*MINS_IN_HOUR*SECS_IN_MIN
#define MAX1B_FCST_DAYS MAX1B_FCST*HOURS_IN_DAY*MINS_IN_HOUR*SECS_IN_MIN

typedef struct {
  int time_range;
  int fcst_unit;
  int P1;
  int P2;

  int time_range_ext;
  int fcst_unit_ext_1;
  int fcst_unit_ext_2;
  int P1_ext;
  int P2_ext;
} FcstTimeStruct;

int get_fcst_time(int accum_period, int fcst_secs, FcstTimeStruct *fcst_time);

/****************************************************************************
 *
 * This function takes in metadata in the grid_info structure, output data in 
 *   the *data array, and calls routines to write the metadata and data 
 *   in grib version 1 format the open file descriptor filefd.
 *
 ****************************************************************************/

int WRITE_GRIB(Grid_Info *grid_info, int *filefd, float *data)
{

  GRIB_HDR *gh=NULL;
  DATA_INPUT data_input;
  GEOM_IN geom_in;
  USER_INPUT user_input;
  int grid_projection;
  int status;
  float x_center, y_center;
  GridNav gridnav;
  float first_lat, first_lon, last_lat, last_lon;
  int year, month, day, hour, minute;
  float second;
  char varname2[1000];
  int table_index;
  int fcst_unit;
  int time_range;
  int P1, P2;
  int fcst_unit_ext_1, fcst_unit_ext_2;
  int P1_ext, P2_ext;
  int time_range_ext;
  char errmsg[1000];
  int center, subcenter, parmtbl;
  int tablenum;
  float dx, dy;
  FcstTimeStruct fcst_time;

  strcpy(varname2,grid_info->varname);
  trim(varname2);

  sscanf(grid_info->initdate,"%d-%d-%d_%d:%d:%f",
	 &year,&month,&day,&hour,&minute,&second);

  /* Get coords of center of grid */
  x_center = (grid_info->xpoints + 1)/2.;
  y_center = (grid_info->ypoints + 1)/2.;

  grid_projection = get_gridnav_projection(grid_info->projection);

 /* Convert grid spacing to degrees for LATLON projection */
  
  if ( (grid_info->projection == WRF_LATLON) || 
       (grid_info->projection == WRF_CASSINI) )
  {
      dx = grid_info->Di * KM_TO_DEGREES;
      dy = grid_info->Dj * KM_TO_DEGREES;
  }
  else
  {
      dx = grid_info->Di;
      dy = grid_info->Dj;
  }

 /* Initialize grid structure */

  status = GRID_init(grid_info->center_lat, grid_info->central_lon, 
		     grid_projection,
		     grid_info->latin1, grid_info->latin2, 
		     grid_info->xpoints, grid_info->ypoints,
		     dx, dy,
		     grid_info->center_lat, grid_info->center_lon, 
		     x_center, y_center,
		     &gridnav);
  if (!status)
    {
      fprintf(stderr,"write_grib: error from GRID_init\n");
    }

  /* get lat/lon of lower left corner */
  status = GRID_to_latlon(&gridnav, 1, 1, &first_lat, &first_lon);
  if (!status)
    {
      fprintf(stderr,
	      "write_grib: error from GRID_to_latlon for first lat/lon\n");
    }

  /* get lat/lon of upper right corner */
  status = GRID_to_latlon(&gridnav, grid_info->xpoints, grid_info->ypoints, 
			  &last_lat, &last_lon);
  if (!status)
    {
      fprintf(stderr,
	      "write_grib: error from GRID_to_latlon for last lat/lon\n");
    }

  /* Read the grib parameter table */
  status = GET_GRIB_PARAM(grid_info->grib_tables, varname2, &center,
			   &subcenter, &parmtbl, &tablenum, &table_index,
			  1,strlen(varname2));
  if (table_index < 0)
    {
      fprintf(stderr,\
              "Skipping %s, Could not find parameter for %s in gribmap.txt\n",\
              varname2,varname2);
      return 1;
    }

  /* 
   * We skip any parameters that are listed in parameter 255 in gribmap.txt.
   * Parameter 255 is used to indicate that a WRF parameter should not be 
   * output.  It is useful for parameters that are requested to be output in
   * the WRF Registry, but are already implicitly output in grib.
   */

  if (table_index == 255)
    {
      return 0;
    }

  /* 
   * Setup the geom_in structure for the grib library.  Here, we set
   *  the generic parms.  Below, we set the projection specific parms
   */
  geom_in.nx = grid_info->xpoints;
  geom_in.ny = grid_info->ypoints;
  geom_in.first_lat = first_lat;
  geom_in.first_lon = first_lon;
  geom_in.last_lat = last_lat;
  geom_in.last_lon = last_lon;
  geom_in.scan = 64;

  switch (grid_info->projection) 
    {
    case WRF_LATLON:
    case WRF_CASSINI:
      strcpy(geom_in.prjn_name,"spherical");
      geom_in.parm_1 = dy;
      geom_in.parm_2 = dx;
      geom_in.parm_3 = -1;
      geom_in.usRes_flag = 0;  /* 
				* Set to 0 here, MEL grib library will reset
				*  to 128 to indicate that direction 
				*  increments are given.
				*/
      break;
    case WRF_MERCATOR:
      strcpy(geom_in.prjn_name,"mercator");
      geom_in.parm_1 = grid_info->latin1;
      geom_in.parm_2 = dx;
      geom_in.parm_3 = dy;
      geom_in.usRes_flag = 136;
      break;
    case WRF_LAMBERT:
      strcpy(geom_in.prjn_name,"lambert");
      geom_in.usRes_flag = 0;   /* Set to 0 here, MEL grib library will reset 
				 * to 128.
				 */
      geom_in.parm_3 = grid_info->central_lon;
      geom_in.x_int_dis = dx;
      geom_in.y_int_dis = dy;
      geom_in.parm_1 = grid_info->latin1;
      geom_in.parm_2 = grid_info->latin2;
      break;
    case WRF_POLAR_STEREO:
      strcpy(geom_in.prjn_name,"polar_stereo");
      geom_in.usRes_flag = 0;   /* Set to 0 here, MEL grib library will reset 
				 * to 128.
				 */

      geom_in.parm_3 = -1;
      geom_in.x_int_dis = dx*(1.+sin(60. * PI/180.))
	/ (1.+sin(abs(grid_info->latin1) * PI/180.));
      geom_in.y_int_dis = dy*(1.+sin(60. * PI/180.))
	/ (1.+sin(abs(grid_info->latin1) * PI/180.));
      geom_in.parm_1 = -1;
      geom_in.parm_2 = grid_info->central_lon;
      break;
    default:
      fprintf(stderr,"Error, invalid projection: %d\n",grid_info->projection);
      return 1;
    }

  /* 
   * Setup the data_input structure.
   */
  data_input.nDec_sc_fctr = 
    grid_info->grib_tables->grib_table_info[tablenum].dec_sc_factor[table_index];
  data_input.usProc_id = 220;
  data_input.usGrid_id = grid_info->grid_id;
  data_input.usParm_id = 
    grid_info->grib_tables->grib_table_info[tablenum].parm_id[table_index];
  data_input.usParm_sub_id = 
    grid_info->grib_tables->grib_table_info[tablenum].subcenter;
  data_input.usLevel_id = grid_info->leveltype;

  if (grid_info->leveltype == 112) {
    data_input.nLvl_1 = grid_info->level2;
    data_input.nLvl_2 = grid_info->level1;
  } else {
    data_input.nLvl_1 = grid_info->level1;
    data_input.nLvl_2 = grid_info->level2;
  }

  data_input.nYear = year;
  data_input.nMonth = month;
  data_input.nDay = day;
  data_input.nHour = hour;
  data_input.nMinute = minute;
  data_input.nSecond = second;

  status = get_fcst_time(grid_info->accum_period, grid_info->fcst_time, 
			 &fcst_time);

  data_input.usFcst_id = fcst_time.fcst_unit;
  data_input.usFcst_per1 = fcst_time.P1;
  data_input.usFcst_per2 = fcst_time.P2;
  data_input.usTime_range_id = fcst_time.time_range;
  data_input.usTime_range_avg = 0;
  data_input.usTime_range_mis = 0;
  /* 
   * This is for WSI's extended PDS section 
   */
  data_input.PDS_41 = fcst_time.fcst_unit_ext_1;
  data_input.PDS_42 = fcst_time.P1_ext;
  data_input.PDS_46 = fcst_time.fcst_unit_ext_2;
  data_input.PDS_47 = fcst_time.P2_ext;
  data_input.PDS_51 = fcst_time.time_range_ext;
  data_input.PDS_52 = 0;


  data_input.nDec_sc_fctr = 
    grid_info->grib_tables->grib_table_info[tablenum].dec_sc_factor[table_index];
  user_input.usCenter_id = 
    grid_info->grib_tables->grib_table_info[tablenum].center;
  user_input.usParm_tbl = 
    grid_info->grib_tables->grib_table_info[tablenum].parmtbl;
  user_input.chCase_id='0';
  user_input.usSub_tbl = 0;
  user_input.usCenter_sub = 
    grid_info->grib_tables->grib_table_info[tablenum].subcenter;
  user_input.usTrack_num = 0;
  user_input.usGds_bms_id = 128;
  user_input.usBDS_flag = 0;
  user_input.usBit_pack_num = 0;

  status = init_gribhdr(&gh,errmsg);
  if (status != 0) {
    fprintf (stderr,"write_grib: Error writing %s: \n\t%s\n",varname2,errmsg);
    return 1;
  }

  status = grib_enc(data_input,user_input,geom_in,data,gh,errmsg);
  if (status != 0) {
    fprintf (stderr,"write_grib: Error writing %s: \n\t%s\n",varname2,errmsg);
    fprintf (stderr,"\tCheck precision for %s in gribmap.txt.\n",
	     varname2);
    return 1;
  }

  status = gribhdr2filed(gh,*filefd,errmsg);
  if (status != 0) {
    fprintf (stderr,"write_grib: Error writing %s: \n\t%s\n",varname2,errmsg);
    return 1;
  }

  free_gribhdr(&gh);

  return 0;
}

/***************************************************************************
 * Function to set up a structure containing forecast time parameters
 *  This encodes the standard grib forecast time parameters as well
 *  as WSI's extended forecast time parameters.
 ***************************************************************************/

int get_fcst_time(int accum_period, int fcst_secs, FcstTimeStruct *ft)
{
  /* 
   * Added ability to output a "5-minute" forecast time unit for the
   *   sake of WxProducer.  This allows WxPro to ingest data beyond
   *   18 hours, and accumulation data beyond 255 units.
   */

  /*
   * Initialize.
   */
  ft->time_range = 0;
  ft->fcst_unit = 0;
  ft->P1 = 0;
  ft->P2 = 0;
  ft->fcst_unit_ext_1 = 0;
  ft->fcst_unit_ext_2 = 0;
  ft->P1_ext = 0;
  ft->P2_ext = 0;
  ft->time_range_ext = 0;

  if (accum_period == 0) 
    {
      if (fcst_secs < MAX_FCST_SECS)
	{
	  ft->time_range = 10;
	  ft->fcst_unit = 254;
	  ft->P1 = get_byte(fcst_secs,2);
	  ft->P2 = get_byte(fcst_secs,1);
	}
      else if (((fcst_secs % SECS_IN_MIN) == 0) &&
	       (fcst_secs < MAX_FCST_MINS))
	{
	  ft->time_range = 10;
	  ft->fcst_unit = 0;
	  ft->P1 = get_byte(fcst_secs/SECS_IN_MIN,2);
	  ft->P2 = get_byte(fcst_secs/SECS_IN_MIN,1);
	}
      else if (((fcst_secs % SECS_IN_MIN*MINS_IN_HOUR) == 0) && 
	       (fcst_secs < MAX_FCST_HOURS))
	{
	  ft->time_range = 10;
	  ft->fcst_unit = 1;
	  ft->P1 = get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR),2);
	  ft->P2 = get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR),1);      
	}
      /* 
       * MAX_FCST_DAYS is causing an integer overflow, so, we'll just skip 
       *   the check here.  It's very unlikely that someone would exceed this
       *   anyway (5.6 million days!)
       */
      /*
      else if (((fcst_secs % SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY) == 0) &&
	       (fcst_secs < MAX_FCST_DAYS))
      */
      else if (((fcst_secs % SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY) == 0))
	{
	  ft->time_range = 10;
	  ft->fcst_unit = 2;
	  ft->P1 = 
	    get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY),2);
	  ft->P2 = 
	    get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR*HOURS_IN_DAY),1);
	}
      else if (((fcst_secs % SECS_IN_MIN*MINS_IN_5MINS) == 0)
	       && (fcst_secs < MAX_FCST_5MINS))
	{
	  ft->time_range = 10;
	  ft->fcst_unit = 50;
	  ft->P1 = get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS),2);
	  ft->P2 = get_byte(fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS),1);
	}
      else 
	{
	  ft->time_range = 255;
	  ft->fcst_unit = 0;
	  ft->P1 = 0;
	  ft->P2 = 0;
	  
	  ft->fcst_unit_ext_1 = 254;
	  ft->fcst_unit_ext_2 = 254;
	  ft->P1_ext = fcst_secs;
	  ft->P2_ext = 0;
	  ft->time_range_ext = 0;
	}
    }
  else  /* Accumulation period is not 0 */
    {
      if ((fcst_secs < MAX1B_FCST_HOURS) &&
	  (fcst_secs%(SECS_IN_MIN*MINS_IN_HOUR) == 0) && 
	  (accum_period%(SECS_IN_MIN*MINS_IN_HOUR) == 0))
	{
	  ft->time_range = 4;
	  ft->fcst_unit = 1;
	  ft->P1 = (fcst_secs-accum_period)/(SECS_IN_MIN*MINS_IN_HOUR);
	  ft->P2 = fcst_secs/(SECS_IN_MIN*MINS_IN_HOUR);
	}
      else if ((fcst_secs < MAX1B_FCST_MINS) &&
	       ((fcst_secs-accum_period)%SECS_IN_MIN == 0) && 
	       (fcst_secs%SECS_IN_MIN == 0))
	{
	  ft->time_range = 4;
	  ft->fcst_unit = 0;
	  ft->P1 = (fcst_secs-accum_period)/SECS_IN_MIN;
	  ft->P2 = fcst_secs/SECS_IN_MIN;
	}
      else if (fcst_secs < MAX1B_FCST_SECS)
	{
	  ft->time_range = 4;
	  ft->fcst_unit = 254;
	  ft->P1 = fcst_secs-accum_period;
	  ft->P2 = fcst_secs;
	}
      else if ((fcst_secs < MAX1B_FCST_5MINS) && 
	       (fcst_secs%(SECS_IN_MIN*MINS_IN_5MINS) == 0) &&
	       (accum_period%(SECS_IN_MIN*MINS_IN_5MINS) == 0)) 
	{
	  ft->time_range = 4;
	  ft->fcst_unit = 50;
	  ft->P1 = (fcst_secs-accum_period)/(SECS_IN_MIN*MINS_IN_5MINS);
	  ft->P2 = fcst_secs/(SECS_IN_MIN*MINS_IN_5MINS);
	}
      else
	{
	  ft->time_range = 255;
	  ft->fcst_unit = 0;
	  ft->P1 = 0;
	  ft->P2 = 0;
	  
	  ft->fcst_unit_ext_1 = 254;
	  ft->fcst_unit_ext_2 = 254;
	  ft->P1_ext = fcst_secs - accum_period;
	  ft->P2_ext = accum_period;
	  ft->time_range_ext = 203;    /* Duration */
	}
    }
  return 0;
}


/******************************************************************************
 * returns a byt from an input integer
 *****************************************************************************/

int get_byte(int input_int, int bytenum)
{
  int out;
  out = ((input_int >> (bytenum-1)*8) & ~(~0 <<8));
  return out;
}

/*************************************************************************
 * Converts from WRF time format to time format required by grib routines
 *************************************************************************/
int grib_time_format(char *DateStr, char *DateStrIn)
{
  int year,month,day,hour,minute,second;

  trim(DateStrIn);
  if (DateStrIn[0] == '*') {
    strcpy(DateStr,"*");
  }
  else
    {
      sscanf(DateStrIn,"%04d-%02d-%02d_%02d:%02d:%02d",
	     &year,&month,&day,&hour,&minute,&second);
      sprintf(DateStr,"%04d%02d%02d%02d%02d%02d",
	      year,month,day,hour,minute,second);
    }

  return 0;
}
