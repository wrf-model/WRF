#define STRINGSIZE 160

/* The value used to fill missing data points */
#define FILL_VALUE -9999999.

typedef struct {
  int usGrid_id;
  int usParm_id;
  int usLevel_id;
  int usHeight1;
  int usHeight2;
  int offset;
  int end;
  char filename[200];
  FILE *fp;
  char initdate[15];
  char valid_time[15];
  int date;
  int century;
  int fcsttime1;
  int fcsttime2;
  int center_id;
  int parmtbl;
  int proc_id;
  int subcenter_id;
  PDS_INPUT *pds;
  grid_desc_sec *gds;
  BMS_INPUT *bms;
  BDS_HEAD_INPUT *bds_head;  
} Elements;

typedef struct {
  int num_elements;
  Elements *elements;
} GribInfo;

typedef struct {
  char initdate[100];
  char validdate[100];
  int parmid;
  int leveltype;
  int level1;
  int level2;
  int fcsttime1;
  int fcsttime2;
  int center_id;
  int subcenter_id;
  int parmtbl_version;
} FindGrib;

typedef struct {
  int center;
  int subcenter;
  int table;
  int parmid;
  char name[STRINGSIZE];
  char comment[STRINGSIZE];
} GribTableEntry;

typedef struct {
  int num_entries;
  GribTableEntry *parms;
} GribParameters;

typedef struct {
  float column;
  float row;
  float value;
  char id[20];
} PointData;

typedef struct {
  char prjnmm[24], stordsc[24];
  int colcnt, rowcnt;
  float origlat, origlon, origx, origy;
  float xintdis, yintdis, parm1, parm2, parm3;
} GRIB_PROJECTION_INFO_DEF;

/* Public function definitions */

int rg_setup_gribmap(GribParameters *gribmap, char file[]);
int rg_gribmap_parameter(GribParameters *gribmap, char name[], int table,
			 GribTableEntry *gribmap_parms);
void rg_free_gribmap_elements(GribParameters *gribmap);

int rg_setup_gribinfo(GribInfo *gribinfo, char files[][STRINGSIZE],
		      int use_fcst);
int rg_setup_gribinfo_f(GribInfo *gribinfo, FILE *fp, int use_fcst);
int rg_setup_gribinfo_i(GribInfo *gribinfo, int fid, int use_fcst);

int rg_get_index(GribInfo *gribinfo, FindGrib *find_grib);

int rg_get_index_guess(GribInfo *gribinfo, FindGrib *findgrib, int guess_index);

int rg_get_indices(GribInfo *gribinfo, FindGrib *find_grib, int *indices);

int rg_init_findgrib(FindGrib *findgrib);

int rg_get_grib_header(GribInfo *gribinfo, int index, PDS_INPUT *pds, 
		    grid_desc_sec *gds,BMS_INPUT *bms);

int rg_num_elements(GribInfo *gribinfo);

void rg_free_gribinfo_elements(GribInfo *gribinfo);

int rg_get_numrows(GribInfo *gribinfo,int index);

int rg_get_numcols(GribInfo *gribinfo,int index);

int rg_get_level1(GribInfo *gribinfo, int index);

int rg_get_level2(GribInfo *gribinfo, int index);

int rg_get_center_id(GribInfo *gribinfo, int index);

int rg_get_subcenter_id(GribInfo *gribinfo, int index);

int rg_get_proc_id(GribInfo *gribinfo, int index);

int rg_get_tblversion(GribInfo *gribinfo, int index);

float rg_get_point(GribInfo *gribinfo, int index, float column, float row);

int rg_get_points(GribInfo *gribinfo, int index, PointData *pointdata, 
		    int numpoints);

int rg_get_offset(GribInfo *gribinfo, int index);

int rg_get_end(GribInfo *gribinfo, int index);

int rg_get_valid_time(GribInfo *gribinfo, int index, char valid_time[]);

int rg_get_data(GribInfo *gribinfo, int index, float **data);

int rg_get_data_1d(GribInfo *gribinfo, int index, float *data);

int rg_write_grib(PDS_INPUT *pds, grid_desc_sec *gds, char filename[],
		  float **data);
int rg_fwrite_grib(PDS_INPUT *pds, grid_desc_sec *gds, float **data, 
		   FILE *fid);



/* The following are public functions also.  However, the interface to many
 *   of them needs to be reworked.  For example, it is not possible to specify
 *   a secondary height or forecast time with many of them.
 * Note: As of 11/11/04, the functions listed above have already been reworked.
 */

int rg_get_pressure_levels(GribInfo *gribinfo, int dates[], int centuries[], 
			int parm_id[], int finallevels[],int min_pres,
			int numparms);
int rg_get_msl_indices(GribInfo *gribinfo, char dates[][STRINGSIZE],
		    int centuries[], int usParm_id[],int usLevel_id[],
		    int usHeight1[],int infactor[],int numparms,
		    int grib_index[],int outfactor[]);
int rg_get_grib(GribInfo *gribinfo, int index,int scale,
	     float **grib_out,int *vect_comp_flag, 
	     GRIB_PROJECTION_INFO_DEF *Proj, BDS_HEAD_INPUT *bds_head);
int rg_get_dates(GribInfo *gribinfo,int usParm_id[],int usLevel_id[],
	      int usHeight1[],int numparms,int dates[],int century[],
	      int indices[]);
int rg_get_index_near_date(GribInfo *gribinfo,char targetdate[STRINGSIZE],
			int century,int hours_before,int hours_after,
			int usParm_id[],int usLevel_id[],int usHeight1[],
			int numparms);
int rg_get_date(GribInfo *gribinfo,int index);
int rg_get_century(GribInfo *gribinfo,int index);
int rg_get_forecast_time(GribInfo *gribinfo,int index);
