#include <stdio.h>
#include "gribmap.h"

/******************************************************************************
 *
 * The functions in this file are used for opening/reading, and searching for
 *   information in a grib table.
 *
 ******************************************************************************/

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define READ_GRIBMAP read_gribmap
#      define GET_GRIB_PARAM get_grib_param
#      define GET_GRIB_STRUCT_SIZE get_grib_struct_size
# else
#   ifdef F2CSTYLE
#      define READ_GRIBMAP  read_gribmap__
#      define GET_GRIB_PARAM get_grib_param__
#      define GET_GRIB_STRUCT_SIZE get_grib_struct_size__
#   else
#      define READ_GRIBMAP  read_gribmap_
#      define GET_GRIB_PARAM get_grib_param_
#      define GET_GRIB_STRUCT_SIZE get_grib_struct_size_
#   endif
# endif
#endif

int findchar(char *line, char thechar);

#ifdef TEST
int main()
{
  Grib_Table_Info grib_table_info;
  char filename[100];
  int index;
  int ret;
  
  strcpy(filename,"gribmap.txt");
  read_gribmap_(filename,&grib_table_info,&ret);

  get_grib_param_(&grib_table_info, "TSK", &index);
  fprintf(stderr,"got index: %d\n",index);
}
#endif

/******************************************************************************
 *
 * read_gribmap - reads a gribmap file and puts the information into the 
 *                 grib_table_info structure.
 *
 ******************************************************************************/

int READ_GRIBMAP(char *filename, Grib_Table_Info *grib_table_info, int *ret)
{

  FILE *mapptr;
  char line[500];
  int dummy;
  int linenum;
  int nxtidx, elemidx, charidx;
  char elems[6][100];

  /* Open parameter table file */
  mapptr = fopen(filename, "r");
  if (mapptr == NULL)
    {
      fprintf(stderr,"Could not open %s\n",filename);
      *ret=-1;
      return -1;
    }

  /* Skip over comments at begining of gribmap file */
  while (fgets(line,500,mapptr))
    {
      if (line[0] != '#') break;
    }
  sscanf(line,"%d:%d:%d:%d",&dummy,&(grib_table_info->center),
	 &(grib_table_info->subcenter),&(grib_table_info->parmtbl));

  /* 
   * Read each line of parameter table, and store information in the
   *   structure.
   */
  linenum = 0;
  while (fgets(line,500,mapptr) != NULL) 
    {
      /* Split up the elements that are seperated by : */
      nxtidx = 0;
      elemidx = 0;
      while ((charidx = findchar(line + nxtidx,':')) >= 0)
	{
	  strncpy(elems[elemidx],line + nxtidx,charidx);
	  elems[elemidx][charidx] = '\0';
	  elemidx++;
	  nxtidx += (charidx + 1);
	}

      /* 
       * Check to see if this line specifies the next grib table.  If so,
       *   break out
       */
      if (strcmp(elems[0],"-1") == 0) {
	break;
      }

      /* Grab the last field */
      strcpy(elems[elemidx],line + nxtidx);

      /* Split up comma-seperated field of wrf varnames */
      nxtidx = 0;
      elemidx = 0;
      while ((charidx = findchar(elems[3]+nxtidx,',')) >= 0) 
	{
	  strncpy(grib_table_info->wrf_param[linenum][elemidx],
		  elems[3]+nxtidx,charidx);
	  grib_table_info->wrf_param[linenum][elemidx][charidx] = '\0';
	  elemidx++;
	  nxtidx += (charidx + 1);
	}
      /* Grab the last field */
      if (strlen(elems[3] + nxtidx) <= 0) 
	{
	  grib_table_info->wrf_param[linenum][elemidx][0] = '\0';;
	  grib_table_info->num_wrf_params[linenum] = 0;
	}
      else
	{
	  strcpy(grib_table_info->wrf_param[linenum][elemidx],elems[3] + nxtidx);
	  grib_table_info->num_wrf_params[linenum] = elemidx + 1;
	}
  
      grib_table_info->gribidx[linenum]=atoi(elems[0]);
      grib_table_info->dec_sc_factor[linenum]=atoi(elems[4]);

      linenum++;
    }

  grib_table_info->num_entries = linenum-1;
  *ret=0;
  return 0;
}

/******************************************************************************
 *
 * get_grib_param - searches through a grib_table_info structure and returns
 *                   the index for the input "varname".
 *
 ******************************************************************************/

int GET_GRIB_PARAM(Grib_Table_Info *grib_table_info, char *varname, int *index)
{
  int idx;
  int prm_idx;
  
  *index = -1;
  for (idx = 0; idx < grib_table_info->num_entries; idx++) 
    {
      for (prm_idx = 0; prm_idx < grib_table_info->num_wrf_params[idx]; 
	   prm_idx++) 
	{
	  if (strcmp(varname,grib_table_info->wrf_param[idx][prm_idx]) == 0) 
	    {
	      *index = idx;
	    }
	}
    }
  return *index;
}

/******************************************************************************
 *
 * get_grib_struct_size_ - returns the size (in bytes) of a grib_table_info
 *                         structure.
 *
 ******************************************************************************/

int GET_GRIB_STRUCT_SIZE(int *size)
{
  *size = sizeof(Grib_Table_Info);
  return *size;
}

/******************************************************************************
 *
 * Return the character index of the first instance of "thechar" in a string.
 *
 ******************************************************************************/

int findchar(char *line, char thechar)
{
  int returnidx, charnum;

  returnidx = -1;
  for (charnum = 0; charnum < strlen(line); charnum++) 
    {
      if (line[charnum] == thechar)
	{
	  returnidx = charnum;
	  break;
	}
    }
  return returnidx;
}
