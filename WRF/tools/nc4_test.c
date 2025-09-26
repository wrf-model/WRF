#include <stdio.h>
#include "netcdf.h"

#define ERR_RET do { \
fflush(stdout); /* Make sure our stdout is synced with stderr. */ \
fprintf(stderr, "Sorry! Unexpected result, %s, line: %d\n", \
        __FILE__, __LINE__);                                \
return -1;                                                   \
} while (0)

#define NDIMS 3   
#define FILE_NAME "nc4_test.nc"
#define X_LEN 120
#define Y_LEN 64
#define Z_LEN 128

int main(void)
{
   float data[X_LEN * Y_LEN * Z_LEN];
   int i, ncmode, ncid, dimids[NDIMS], var;
   size_t start[NDIMS] = {0, 0, 0};
   size_t count[NDIMS] = {X_LEN, Y_LEN, Z_LEN};
   ptrdiff_t stride[NDIMS] = {1, 1, 1};

  /* Initialize data. */
   for (i = 0; i < (X_LEN * Y_LEN * Z_LEN); i++)
      data[i] = i;

   printf("*** Testing netcdf-4 writes with compressed data...\n");

   ncmode = NC_CLOBBER|NC_NETCDF4;

   if (nc_create(FILE_NAME, NC_NETCDF4, &ncid)) ERR_RET;
   if (nc_def_dim(ncid, "time", X_LEN, &dimids[0])) ERR_RET;
   if (nc_def_dim(ncid, "lat",  Y_LEN, &dimids[1])) ERR_RET;
   if (nc_def_dim(ncid, "lon",  Z_LEN, &dimids[2])) ERR_RET;
   if (nc_def_var(ncid, "test", NC_FLOAT, NDIMS, dimids, &var)) ERR_RET;
   if (nc_def_var_deflate(ncid, var, 1, 1, 2)) ERR_RET;
   if (nc_enddef(ncid)) ERR_RET;
   if (nc_put_vars_float(ncid, var, start, count, stride, data)) ERR_RET;
   if (nc_close(ncid)) ERR_RET;

   printf("*** Tests successful!\n");

   return 0;

}
