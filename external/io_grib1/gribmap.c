#include <stdio.h>
#include <stdlib.h>
#include "gribmap.h"

/******************************************************************************
 *
 * The functions in this file are used for opening/reading, and searching for
 *   information in a grib table.
 *
 * All functions return 0 for success and 1 for failure, unless otherwise 
 *   noted.
 *****************************************************************************/

int findchar(char *line, char thechar);

#ifdef TEST
int main()
{
  Grib1_Tables grib_tables;
  char filename[300];
  int parm_id;
  int ret;
  int tablenum;
  int center,subcenter,parmtbl;
  
  strcpy(filename,"gribmap.txt");
  LOAD_GRIB1_TABLES(filename, &grib_tables, &ret);

  GET_GRIB_PARAM (&grib_tables, "TSK", &center,&subcenter,&parmtbl,
		  &tablenum, &parm_id);
  fprintf(stderr,"got parm_id: %d center: %d subcenter: %d parmtbl: %d\n",
	  parm_id,center,subcenter,parmtbl);

}
#endif

/******************************************************************************
 *
 * read_gribmap - reads a gribmap file and puts the information into the 
 *                 grib_table_info structure.
 *
 ******************************************************************************/

int READ_GRIBMAP (char *filename, Grib1_Tables *grib_tables, int *ret)
{

  FILE *mapptr;
  char line[MAX_LINE_CHARS];
  int dummy;
  int parmidx;
  int nxtidx, elemidx, charidx;
  char elems[6][MAX_LINE_CHARS];
  int tablenum;

  /* Open parameter table file */
  mapptr = fopen(filename, "r");
  if (mapptr == NULL)
    {
      fprintf(stderr,"Could not open %s\n",filename);
      *ret=1;
      return 1;
    }

  /* Skip over comments at begining of gribmap file */
  while (fgets(line,500,mapptr))
    {
      if (line[0] != '#') break;
    }

  tablenum = 0;
  grib_tables->num_tables = 1;
  grib_tables->grib_table_info = 
    (Grib1_Table_Info *)calloc(1,sizeof(Grib1_Table_Info));

  if (grib_tables->grib_table_info == NULL)
    {
      fprintf(stderr,"Could not allocate space for grib_table_info\n");
      *ret = 1;
      return 1;
    }
  grib_tables->grib_table_info[tablenum].num_entries = 0;

  sscanf(line,"%d:%d:%d:%d",&dummy,
	 &(grib_tables->grib_table_info[tablenum].center),
	 &(grib_tables->grib_table_info[tablenum].subcenter),
	 &(grib_tables->grib_table_info[tablenum].parmtbl));

  /* 
   * Read each line of parameter table, and store information in the
   *   structure.
   */
  while (fgets(line,MAX_LINE_CHARS,mapptr) != NULL) 
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

      parmidx = atoi(elems[0]);

      /* 
       * Check to see if this line specifies the next grib table.  If so,
       *   break out
       */
      if (parmidx == -1) {
	grib_tables->num_tables++;
	tablenum++;
	grib_tables->grib_table_info = 
	  (Grib1_Table_Info *)
	  realloc(grib_tables->grib_table_info,
		  grib_tables->num_tables*sizeof(Grib1_Table_Info));
	
	if (grib_tables->grib_table_info == NULL)
	  {
	    fprintf(stderr,
		    "Could not re-allocate space for grib_table_info\n");
	    *ret = 1;
	    return 1;
	  }
	grib_tables->grib_table_info[tablenum].num_entries = 0;
	sscanf(line,"%d:%d:%d:%d",&dummy,
	       &(grib_tables->grib_table_info[tablenum].center),
	       &(grib_tables->grib_table_info[tablenum].subcenter),
	       &(grib_tables->grib_table_info[tablenum].parmtbl));
	continue;
      }

      /* Assure that we have not gone beyond 256 entries! */
      if (grib_tables->grib_table_info[tablenum].num_entries >= 256) 
	{
	  fprintf(stderr,
"Error: Invalid number of lines in table %d in, \n skipping line: %s \n",
		  tablenum,line);
	  break;
	}

      /* Grab the last field */
      strcpy(elems[elemidx],line + nxtidx);

      /* Split up comma-seperated field of wrf varnames */
      nxtidx = 0;
      elemidx = 0;

      /* Allocate number of elements in wrf_param */
      grib_tables->grib_table_info[tablenum].wrf_param[parmidx] = 
	(char **)malloc(1*sizeof(char *));
      if (grib_tables->grib_table_info[tablenum].wrf_param[parmidx] == NULL)
	{
	  fprintf(stderr, "Error allocating space for wrf_param[%d], exiting\n",
		  parmidx);
	  *ret = 1;
	  return 1;
	}

      while ((charidx = findchar(elems[3]+nxtidx,',')) >= 0) 
	{

	  /* Allocate number of elements in wrf_param */
	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx] = 
	    (char **)
	    realloc(grib_tables->grib_table_info[tablenum].wrf_param[parmidx],
			     (elemidx+2)*sizeof(char *));
	  if (grib_tables->grib_table_info[tablenum].wrf_param[parmidx] 
	      == NULL)
	    {
	      perror("");
	      fprintf(stderr, 
		      "Error allocating space for wrf_param[%d], exiting\n", 
		      parmidx);
	      *ret = 1;
	      return 1;
	    }

	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx] = 
	    (char *)malloc((charidx+2)*sizeof(char));
	  if (grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx]
	      == NULL)
	    {
	      perror("");
	      fprintf(stderr, 
		      "Error allocating space for wrf_param[%d][%d], exiting\n",
		      parmidx,elemidx);
	      *ret = 1;
	      return 1;
	    }
	  
	  strncpy(grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx],
		  elems[3]+nxtidx,charidx);
	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx][charidx] = '\0';
	  elemidx++;
	  nxtidx += (charidx + 1);
	}

      /* Grab the last field */
      if (strlen(elems[3] + nxtidx) <= 0) 
	{
	  /* Case for no specified WRF fields */
	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx] =
	    (char *)malloc(1*sizeof(char));
	  if (grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx]
	      == NULL)
	    {
	      perror("");
	      fprintf(stderr, 
		      "Error allocating space for wrf_param[%d][%d], exiting\n",
		      parmidx,elemidx);
	      *ret = 1;
	      return 1;
	    }
	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx][0] 
	    = '\0';
	  grib_tables->grib_table_info[tablenum].num_wrf_params[parmidx] = 0;
	}
      else
	{
	  /* Allocate space for last element */
	  grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx] =
	    (char *)malloc((strlen(elems[3] + nxtidx)+1)*sizeof(char));
	  if (grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx]
	      == NULL)
	    {
	      perror("");
	      fprintf(stderr, 
		      "Error allocating space for wrf_param[%d][%d], exiting\n",
		      parmidx,elemidx);
	      *ret = 1;
	      return 1;
	    }
	  
	  strcpy(grib_tables->grib_table_info[tablenum].wrf_param[parmidx][elemidx],
		 elems[3] + nxtidx);
	  grib_tables->grib_table_info[tablenum].num_wrf_params[parmidx] = 
	    elemidx + 1;
	}
  
      grib_tables->grib_table_info[tablenum].parm_id[parmidx]=atoi(elems[0]);
      grib_tables->grib_table_info[tablenum].dec_sc_factor[parmidx]=atoi(elems[4]);

      grib_tables->grib_table_info[tablenum].num_entries++;
    }

  *ret=0;
  return 0;
}

/******************************************************************************
 *
 * get_grib_param - searches through a grib_table_info structure and returns
 *                   the index for the input "varname".
 *
 * returns index number, or, -1 for failure.
 *****************************************************************************/

int GET_GRIB_PARAM (Grib1_Tables *grib_tables, char *varname, int *center, 
		    int *subcenter, int *parmtbl, int *tablenum, int *index,
		    int strlen1, int strlen2)
{
  int idx;
  int prm_idx;
  int tableidx;
  char varnametmp[200];
  
  *index = -1;

  strncpy(varnametmp,varname,strlen2);
  varnametmp[strlen2] = '\0';
  trim(varnametmp);
  for (tableidx = 0; tableidx < grib_tables->num_tables ;tableidx++)
    {


      for (idx = 0; 
	   idx < grib_tables->grib_table_info[tableidx].num_entries;
	   idx++) 
	{
	  for (prm_idx = 0; 
	       prm_idx < 
		 grib_tables->grib_table_info[tableidx].num_wrf_params[idx]; 
	       prm_idx++) 
	    {
	      if (strcmp(varnametmp,
			 grib_tables->grib_table_info[tableidx].wrf_param[idx][prm_idx]) 
		  == 0) 
		{
		  *center = 
		    grib_tables->grib_table_info[tableidx].center;
		  *subcenter = grib_tables->grib_table_info[tableidx].subcenter;
		  *parmtbl = grib_tables->grib_table_info[tableidx].parmtbl;
		  *tablenum = tableidx;
		  *index = idx;
		  break;
		}
	    }
	}
      
    }
  return *index;
}

/******************************************************************************
 *
 * free_gribmap_ - returns the size (in bytes) of a grib_table_info
 *                         structure.
 *
 *****************************************************************************/

int FREE_GRIBMAP(Grib1_Tables *grib_tables)
{
  int idx, idx2;
  int tablenum;
  
  for (tablenum = 0; tablenum < grib_tables->num_tables; tablenum++)
    {
      for (idx = 0; idx < grib_tables->grib_table_info[tablenum].num_entries; 
	   idx++)
	{
	  for (idx2 = 0; 
	       idx2 < grib_tables->grib_table_info[tablenum].num_wrf_params[idx]; 
	       idx2++)
	    {
	      free(grib_tables->grib_table_info[tablenum].wrf_param[idx][idx2]);
	    }
	  if (grib_tables->grib_table_info[tablenum].num_wrf_params[idx] > 0)
	    {
	      free(grib_tables->grib_table_info[tablenum].wrf_param[idx]);
	    }
	}
    }
  free(grib_tables->grib_table_info);
  return 0;
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

/******************************************************************************
 *
 * get_grib1_table_info_size - returns the size (in bytes) of a grib_table_info
 *                         structure.
 *
 *****************************************************************************/

int GET_GRIB1_TABLE_INFO_SIZE (int *size)
{
  *size = sizeof(Grib1_Table_Info);
  return *size;
}

/******************************************************************************
 *
 * get_grib1_tables_size - returns the size (in bytes) of a grib_tables
 *                         structure.
 *
 *****************************************************************************/

int GET_GRIB1_TABLES_SIZE (int *size)
{
  *size = sizeof(Grib1_Tables);
  return *size;
}

/******************************************************************************
 *
 * load_grib1_table_info - reads a gribmap file and puts the information into 
 *                         the grib_table_info structure.
 *
 ******************************************************************************/

int LOAD_GRIB1_TABLES (char filename[], 
		       Grib1_Tables *grib_tables, int *ret, int strlen1)
{

  char tmpfilename[300];
  strncpy(tmpfilename,filename,strlen1);
  tmpfilename[strlen1] = '\0';

  READ_GRIBMAP(tmpfilename, grib_tables, ret);

  return *ret;
}

/******************************************************************************
 *
 * get_grid_info_size_ - returns the size (in bytes) of a grib_tables
 *                         structure.
 *
 *****************************************************************************/

int GET_GRID_INFO_SIZE (int *size)
{
  *size = sizeof(Grib1_Tables);
  return *size;
}


/******************************************************************************
 *
 * copy_grib_tables - allocates and fills a grib_tables structure
 *
 *****************************************************************************/

Grib1_Tables *copy_grib_tables(Grib1_Tables *grib_tables)
{
  int tblidx,prmidx,elmidx;
  int strsiz;

  Grib1_Tables *tmp;

  tmp = (Grib1_Tables *)malloc(sizeof(Grib1_Tables));

  memcpy(tmp,grib_tables,sizeof(Grib1_Tables));

  /* Now do the grib_table_info elements within grib_tables */

  tmp->grib_table_info = 
    (Grib1_Table_Info *)
    malloc(grib_tables->num_tables*sizeof(Grib1_Table_Info));
  if (tmp->grib_table_info == NULL) 
    {
      fprintf(stderr,
	      "copy_grib_tables: Could not allocate space for grib_table_info.  num_tables: %d\n",
	      grib_tables->num_tables);
      exit(1);
    }

  memcpy(tmp->grib_table_info,
	 grib_tables->grib_table_info,
	 grib_tables->num_tables*sizeof(Grib1_Table_Info));


  for (tblidx = 0; tblidx < grib_tables->num_tables; tblidx++) 
    {

      for (prmidx = 0; prmidx < MAX_PARAMS; prmidx++)
	{
	  if (grib_tables->grib_table_info[tblidx].num_wrf_params[prmidx] <= 0)
	    {
	      continue;
	    }

	  tmp->grib_table_info[tblidx].wrf_param[prmidx] = (char **)
	    malloc(grib_tables->grib_table_info[tblidx].num_wrf_params[prmidx] 
		   * sizeof(char *));

	  memcpy(tmp->grib_table_info[tblidx].wrf_param[prmidx],
		 grib_tables->grib_table_info[tblidx].wrf_param[prmidx],
		 grib_tables->grib_table_info[tblidx].num_wrf_params[prmidx]
		 * sizeof(char *));

	  for (elmidx = 0; 
	       elmidx < grib_tables->grib_table_info[tblidx].num_wrf_params[prmidx]; 
	       elmidx++) 
	    {
	      
	      strsiz = 
		strlen(grib_tables->grib_table_info[tblidx].wrf_param[prmidx][elmidx]) + 1;
	      tmp->grib_table_info[tblidx].wrf_param[prmidx][elmidx] = 
		(char *)
		malloc(strsiz * sizeof(char));

	      memcpy(tmp->grib_table_info[tblidx].wrf_param[prmidx][elmidx],
		     grib_tables->grib_table_info[tblidx].wrf_param[prmidx][elmidx],
		     strsiz * sizeof(char));

	    }

	}

    }

  return tmp;

}

