
unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer, unsigned int buf_len);

int read_grib(FILE *file, long pos, long len_grib, unsigned char *buffer);

double ibm2flt(unsigned char *ibm);
 
void BDS_unpack(float *flt, unsigned char *bits, unsigned char *bitmap,
        int n_bits, int n, double ref, double scale);
 
double int_power(double x, int y);

int flt2ieee(float x, unsigned char *ieee);

int wrtieee(float *array, int n, int header, FILE *output);

void levels(int, int);
 
void PDStimes(int time_range, int p1, int p2, int time_unit);

int missing_points(unsigned char *bitmap, int n);

void EC_ext(unsigned char *pds, char *prefix, char *suffix);

