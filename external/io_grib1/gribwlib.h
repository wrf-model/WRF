
#define UNDEFINED	9.999e20
#define UNDEFINED_LOW	9.9989e20
#define UNDEFINED_HIGH	9.9991e20

#define UNDEFINED_VAL(x) ((x) >= UNDEFINED_LOW && (x) <= UNDEFINED_HIGH)

unsigned char *rd_GDS(unsigned char *pds, char *filename, int grid_type);

unsigned char *cpGRIBsec(unsigned char *section);

void wrt_grib_msg(int fildes, unsigned char *pds, unsigned char *gds, unsigned 
     char *bms, unsigned char *bds);

int rd_grib_msg(FILE *input, long int *pos, unsigned char **pds,
    unsigned char **gds, unsigned char **bms, unsigned char **bds);

unsigned char *NCEP_GDS(unsigned char *pds, int grid_type);

void wrt_grib_rec(unsigned char *pds, unsigned char *gds, float *data,
            int ndata,  int fildes);

int rd_grib_rec(FILE *input, long int *pos, unsigned char **pds,
    unsigned char **gds, float **data, int *ndata);

int get_nxny(unsigned char *pds, unsigned char *gds, unsigned char *bms,
  unsigned char *bds);

float *get_unpk_bds(unsigned char *pds, unsigned char *gds, unsigned char *bms,
        unsigned char *bds);

void BDS_unpack(float *flt, unsigned char *bits, unsigned char *bitmap,
        int n_bits, int n, double ref, double scale);
  
int unpk_bds(float *array, unsigned char *pds, unsigned char *gds, 
  unsigned char *bms, unsigned char *bds, int array_size);

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib,
        unsigned char *buffer, unsigned int buf_len);

int read_grib(FILE *file, long pos, long len_grib, unsigned char *buffer);

long int scan3(FILE *input, int date, char *variable, char *level);




#ifndef _PDSTOOL_
#include "pdstool.h"
#endif
#ifndef _GDSTOOL_
#include "gdstool.h"
#endif
#ifndef _GRIBW_TIME_
#include "gribw_time.h"
#endif
