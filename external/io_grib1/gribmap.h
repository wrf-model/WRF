typedef struct {
  int center;
  int subcenter;
  int parmtbl;
  int gribidx[256];
  int dec_sc_factor[256];
  char wrf_param[256][10][10];
  int num_wrf_params[256];
  int num_entries;
} Grib_Table_Info;

int read_gribmap_(char *filename, Grib_Table_Info *grib_table_info,int *ret);

int get_grib_param_(Grib_Table_Info *grib_table_info, char *varname, int *index);

int get_grib_struct_size_(int *size);
