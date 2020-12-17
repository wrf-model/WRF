#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>

/* 
To compile (after setting NETCDF environment variable):
cc -o update_msf update_msf.c -I${NETCDF}/include -L${NETCDF}/lib -lnetcdf

To run:
First, make a backup copy of the NetCDF file, since this program will modify the original.
Then, simply run "update_msf <your netcdf file>".
*/

int main(int argc, char ** argv) { 

   int i, j; 
   int ip;
   int istatus, ncid; 
   int varid, msft_id, msfu_id, msfv_id, msftx_id, msfty_id, msfux_id, msfuy_id, msfvx_id, msfvy_id, msfvy_inv_id;
   int we_id, we_stag_id, sn_id, sn_stag_id; 
   size_t we_len, we_stag_len, sn_len, sn_stag_len;
   int dimids[3];
   float * msft, * msfu, * msfv;
   char varname[1024];

   if (argc != 2) {
      fprintf(stderr,"\nUsage: update_msf <file>\n\n");    
      return 1;
   }

   /* Try to open the file */
   istatus = nc_open(argv[1], NC_WRITE, &ncid);
   if (istatus == NC_NOERR) {

      /* Get IDs of dimensions */
      istatus = nc_inq_dimid(ncid, "west_east", &we_id);
      istatus |= nc_inq_dimid(ncid, "south_north", &sn_id);
      istatus |= nc_inq_dimid(ncid, "west_east_stag", &we_stag_id);
      istatus |= nc_inq_dimid(ncid, "south_north_stag", &sn_stag_id);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not get dimensions for map scale factor fields\n");
         return 1;
      }

      /* Get lengths of the we and sn dimensions */
      istatus = nc_inq_dimlen(ncid, we_id, &we_len);
      istatus |= nc_inq_dimlen(ncid, sn_id, &sn_len);
      istatus |= nc_inq_dimlen(ncid, we_stag_id, &we_stag_len);
      istatus |= nc_inq_dimlen(ncid, sn_stag_id, &sn_stag_len);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not get length of dimensions for map scale factor fields\n");
         return 1;
      }

      msft = (float *)malloc(sizeof(float)*we_len*sn_len);
      msfu = (float *)malloc(sizeof(float)*we_stag_len*sn_len);
      msfv = (float *)malloc(sizeof(float)*we_len*sn_stag_len);

      /* Read in MSFT */
      sprintf(varname,"MAPFAC_M");
      istatus = nc_inq_varid (ncid, varname, &varid);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not find variable %s in %s!\n",varname,argv[1]);
         return 1;
      }
      istatus = nc_get_var_float(ncid, varid, msft);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not read MSFT for %s in %s!\n",varname,argv[1]);
         free(msft);
         free(msfu);
         free(msfv);
         return 1;
      }

      /* Read in MSFU */
      sprintf(varname,"MAPFAC_U");
      istatus = nc_inq_varid (ncid, varname, &varid);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not find variable %s in %s!\n",varname,argv[1]);
         return 1;
      }
      istatus = nc_get_var_float(ncid, varid, msfu);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not read MSFT for %s in %s!\n",varname,argv[1]);
         free(msft);
         free(msfu);
         free(msfv);
         return 1;
      }

      /* Read in MSFV */
      sprintf(varname,"MAPFAC_V");
      istatus = nc_inq_varid (ncid, varname, &varid);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not find variable %s in %s!\n",varname,argv[1]);
         return 1;
      }
      istatus = nc_get_var_float(ncid, varid, msfv);
      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not read MSFT for %s in %s!\n",varname,argv[1]);
         free(msft);
         free(msfu);
         free(msfv);
         return 1;
      }

      /* Write new fields back out to file */
      istatus = nc_redef(ncid);

      dimids[0] = NC_UNLIMITED;

      dimids[1] = sn_id;
      dimids[2] = we_id;
      istatus = nc_def_var(ncid, "MAPFAC_MX", NC_FLOAT, 3, dimids, &msftx_id);
      istatus = nc_def_var(ncid, "MAPFAC_MY", NC_FLOAT, 3, dimids, &msfty_id);

      dimids[1] = sn_id;
      dimids[2] = we_stag_id;
      istatus = nc_def_var(ncid, "MAPFAC_UX", NC_FLOAT, 3, dimids, &msfux_id);
      istatus = nc_def_var(ncid, "MAPFAC_UY", NC_FLOAT, 3, dimids, &msfuy_id);

      dimids[1] = sn_stag_id;
      dimids[2] = we_id;
      istatus = nc_def_var(ncid, "MAPFAC_VX", NC_FLOAT, 3, dimids, &msfvx_id);
      istatus = nc_def_var(ncid, "MAPFAC_VY", NC_FLOAT, 3, dimids, &msfvy_id);
      istatus = nc_def_var(ncid, "MF_VX_INV", NC_FLOAT, 3, dimids, &msfvy_inv_id);

      ip = 104;
      istatus = nc_put_att_int(ncid, msftx_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfty_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfux_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfuy_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfvx_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfvy_id, "FieldType", NC_INT, 1, &ip);
      istatus = nc_put_att_int(ncid, msfvy_inv_id, "FieldType", NC_INT, 1, &ip);

      istatus = nc_put_att_text(ncid, msftx_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfty_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfux_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfuy_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfvx_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfvy_id, "MemoryOrder", 3, "XY ");
      istatus = nc_put_att_text(ncid, msfvy_inv_id, "MemoryOrder", 3, "XY ");

      istatus = nc_put_att_text(ncid, msftx_id, "description", 29, "Map scale factor on mass grid");
      istatus = nc_put_att_text(ncid, msfty_id, "description", 29, "Map scale factor on mass grid");
      istatus = nc_put_att_text(ncid, msfux_id, "description", 26, "Map scale factor on u-grid");
      istatus = nc_put_att_text(ncid, msfuy_id, "description", 26, "Map scale factor on u-grid");
      istatus = nc_put_att_text(ncid, msfvx_id, "description", 26, "Map scale factor on v-grid");
      istatus = nc_put_att_text(ncid, msfvy_id, "description", 26, "Map scale factor on v-grid");
      istatus = nc_put_att_text(ncid, msfvy_inv_id, "description", 26, "Map scale factor on v-grid");

      istatus = nc_put_att_text(ncid, msftx_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfty_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfux_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfuy_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfvx_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfvy_id, "units", 0, "");
      istatus = nc_put_att_text(ncid, msfvy_inv_id, "units", 0, "");

      istatus = nc_put_att_text(ncid, msftx_id, "stagger", 0, "");
      istatus = nc_put_att_text(ncid, msfty_id, "stagger", 0, "");
      istatus = nc_put_att_text(ncid, msfux_id, "stagger", 1, "X");
      istatus = nc_put_att_text(ncid, msfuy_id, "stagger", 1, "X");
      istatus = nc_put_att_text(ncid, msfvx_id, "stagger", 1, "Y");
      istatus = nc_put_att_text(ncid, msfvy_id, "stagger", 1, "Y");
      istatus = nc_put_att_text(ncid, msfvy_inv_id, "stagger", 1, "Y");

      istatus = nc_put_att_text(ncid, msftx_id, "coordianates", 10, "XLONG XLAT");
      istatus = nc_put_att_text(ncid, msfty_id, "coordianates", 10, "XLONG XLAT");
      istatus = nc_put_att_text(ncid, msfux_id, "coordianates", 14, "XLONG_U XLAT_U");
      istatus = nc_put_att_text(ncid, msfuy_id, "coordianates", 14, "XLONG_U XLAT_U");
      istatus = nc_put_att_text(ncid, msfvx_id, "coordianates", 14, "XLONG_V XLAT_V");
      istatus = nc_put_att_text(ncid, msfvy_id, "coordianates", 14, "XLONG_V XLAT_V");
      istatus = nc_put_att_text(ncid, msfvy_inv_id, "coordianates", 14, "XLONG_V XLAT_V");

      istatus = nc_enddef(ncid);
      
      istatus = nc_put_var_float(ncid, msftx_id, msft);
      istatus |= nc_put_var_float(ncid, msfty_id, msft);

      istatus |= nc_put_var_float(ncid, msfux_id, msfu);
      istatus |= nc_put_var_float(ncid, msfuy_id, msfu);

      istatus |= nc_put_var_float(ncid, msfvx_id, msfv);
      istatus |= nc_put_var_float(ncid, msfvy_id, msfv);

      for(j=0; j<sn_stag_len; j++) {
         for(i=0; i<we_len; i++) {
            if (msfv[i+we_len*j] != 0.)
               msfv[i+we_len*j] = 1.0/msfv[i+we_len*j];
            else
               msfv[i+we_len*j] = 0.0;
         }
      }
      istatus |= nc_put_var_float(ncid, msfvy_inv_id, msfv);

      if (istatus != NC_NOERR) {
         fprintf(stderr,"Error: Could not write array to %s!\n",argv[1]);
         free(msft);
         free(msfu);
         free(msfv);
         return 1;
      }

      /* Close file */
      free(msft);
      free(msfu);
      free(msfv);
      istatus = nc_close(ncid);
   }
   else {
      fprintf(stderr,"Error: Could not open %s as a NetCDF file!\n",argv[1]);
      return 1;
   }

   return 0;
}
