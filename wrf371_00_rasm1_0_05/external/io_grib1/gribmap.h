#define MAX_LINE_CHARS 2500
#define MAX_PARAMS 256

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define LOAD_GRIB1_TABLES load_grib1_tables
#      define GET_GRIB_PARAM get_grib_param
#      define FREE_GRIBMAP  free_gribmap
#      define GET_GRIB1_TABLE_INFO_SIZE get_grib1_table_info_size
#      define GET_GRIB1_TABLES_SIZE get_grib1_tables_size
#      define READ_GRIBMAP read_gribmap
# else
#   ifdef F2CSTYLE
#      define LOAD_GRIB1_TABLES load_grib1_tables__
#      define GET_GRIB_PARAM get_grib_param__
#      define FREE_GRIBMAP  free_gribmap__
#      define GET_GRIB1_TABLES_SIZE get_grib1_tables_size__
#      define READ_GRIBMAP read_gribmap__
#   else
#      define LOAD_GRIB1_TABLES load_grib1_tables_
#      define GET_GRIB_PARAM get_grib_param_
#      define FREE_GRIBMAP  free_gribmap_
#      define GET_GRIB1_TABLES_SIZE get_grib1_tables_size_
#      define READ_GRIBMAP read_gribmap_
#   endif
# endif
#endif


typedef struct {
  int center;
  int subcenter;
  int parmtbl;
  int parm_id[MAX_PARAMS];
  int dec_sc_factor[MAX_PARAMS];
  char **wrf_param[MAX_PARAMS];
  int num_wrf_params[MAX_PARAMS];
  int num_entries;
} Grib1_Table_Info;

typedef struct {
  int num_tables;
  Grib1_Table_Info *grib_table_info;
} Grib1_Tables;


int GET_GRIB_PARAM (Grib1_Tables *grib_tables, char *varname, int *center, 
		    int *subcenter, int *parmtbl, int *tablenum, int *index,
		    int strlen1, int strlen2);

int GET_GRIB1_TABLES_SIZE (int *size);

int LOAD_GRIB1_TABLES (char filename[], 
			   Grib1_Tables *grib_tables, int *ret, int strlen1);

Grib1_Tables *copy_grib_tables(Grib1_Tables *);
