#include "gribw_macros.h"

#define UNDEFINED	9.999e20
#define UNDEFINED_LOW	9.9989e20
#define UNDEFINED_HIGH	9.9991e20
#define UNDEF_ERR	0.00001

#define UNDEFINED_VAL(x) ((x) >= UNDEFINED_LOW && (x) <= UNDEFINED_HIGH)

unsigned char *rd_GDS(unsigned char *pds, char *filename, int grid_type);

unsigned int size_24section(unsigned char *section);
int wrt_24section(unsigned char *section, char *filename);
int append_24section(unsigned char *section, int filedes);
unsigned char *rd_24section(char *filename);
unsigned char *cpGRIBsec(unsigned char *section);
void wrt_grib_msg(int fildes, unsigned char *pds, unsigned char *gds, unsigned 
     char *bms, unsigned char *bds);

void set_ParameterTable(unsigned char *pds, int table);
int get_ParameterTable(unsigned char *pds);

void set_Center(unsigned char *pds, int center);
int get_Center(unsigned char *pds);

void set_ProcessID(unsigned char *pds, int id);
int get_ProcessID(unsigned char *pds);

void set_Parameter(unsigned char *pds, int parameter);
int get_Parameter(unsigned char *pds);

void set_SubCenter(unsigned char *pds, int subcenter);
int get_SubCenter(unsigned char *pds);


void set_DecScale(unsigned char *pds, int dec_scale);
int get_DecScale(unsigned char *pds);
void set_PDSGridType(unsigned char *pds, int n);
int get_PDSGridType(unsigned char *pds);
void set_HasGDS(unsigned char *pds);
void clr_HasGDS(unsigned char *pds);
int get_HasGDS(unsigned char *pds);
void set_HasBMS(unsigned char *pds);
void clr_HasBMS(unsigned char *pds);
int get_HasBMS(unsigned char *pds);

void set_NextYear(unsigned char *pds);
void set_NextMonth(unsigned char *pds);
void set_NextDay(unsigned char *pds);

void set_BDSMaxBits(int n);
void set_BDSMinBits(int n);
int get_BDSMaxBits();
int get_BDSMinBits();


void list2bitstream(int *list, unsigned char *bitstream, int ndata, int nbits);
void flist2bitstream(float *list, unsigned char *bitstream, int ndata, int nbits);

void set_NMCparm(unsigned char *pds, char *name);
char *get_NMCparm(unsigned char *pds);
void set_NMCnparm(unsigned char *pds, int parm);
int get_NMCnparm(unsigned char *pds);

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer, unsigned int buf_len);

int read_grib(FILE *file, long pos, long len_grib, unsigned char *buffer);

double int_power(double x, int y);
int flt2ibm(float x, unsigned char *ibm);
double ibm2flt(unsigned char *ibm);

/* gio.c */
float *rd_f77data(int *n, FILE *in);
void rd_n_f77data(float *a, int n, FILE *in);
void wrt_n_f77data(float *a, int n, FILE *out);

/* f77_io.c */
int f77skip(FILE *stream);
void *f77read(FILE *stream, int *nbytes);

unsigned char *mk_void_BMS(unsigned char *pds);
unsigned char *mk_BMS(unsigned char *pds, float *bindata, int *n, float undef_low, float undef_hi);
unsigned char *mk_BDS(unsigned char *pds, float *bindata, int n);


int get_nxny(unsigned char *pds, unsigned char *gds, unsigned char *bms, unsigned char *bds);

int rd_grib_msg(FILE *input, long int *pos, unsigned char **pds,
    unsigned char **gds, unsigned char **bms, unsigned char **bds);

float *get_unpk_bds(unsigned char *pds, unsigned char *gds, unsigned char *bms,
        unsigned char *bds);
int unpk_bds(float *array, unsigned char *pds, unsigned char *gds, 
        unsigned char *bms, unsigned char *bds, int array_size);

int *get_bitmap(unsigned char *pds, unsigned char *gds, unsigned char *bms, 
      unsigned char *bds);
void unpk_bms(int *bitmap, unsigned char *bms, int nxny);

unsigned char *gds_grid2();
unsigned char *NCEP_GDS(unsigned char *pds, int grid_type);

/* private */
void set_int3(unsigned char *string, int n);
void set_int2(unsigned char *string, int n);
void set_PDSlevel(unsigned char *pds, int type, int value);

void set_TimeRange(unsigned char *pds, int time_range, int p1, int p2, 
int units, int nave, int nmissing);
void get_TimeRange(unsigned char *pds, int *time_range, int *p1, int *p2, 
int *units, int *nave, int *nmissing);

/* read the wgrib inventory file for pds/gds */
unsigned char *inv_pds(const char *inventory);
unsigned char *inv_gds(const char *inventory);

void wrt_grib_rec(unsigned char *pds, unsigned char *gds, float *data,
            int ndata,  int filedes);

#define _LEN24(pds)	((int) ((pds[0]<<16)+(pds[1]<<8)+pds[2]))
#define __LEN24(pds)	((pds) == NULL ? 0 : (int) ((pds[0]<<16)+(pds[1]<<8)+pds[2]))

#ifndef _GRIBW_TIME_
#include "gribw_time.h"
#endif

