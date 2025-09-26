/*
 *****************************************************************************
 * rg_write_grib - function which encapsulates parts of the MEL grib library
 *              writing routines.  
 *
 * Todd Hutchinson
 * TASC
 * tahutchinson@tasc.com
 * (781)942-2000 x3108
 * 7/1/99
 *
 * Interface:
 *   Input:
 *     pds -      a pointer to a structure which stores information about the 
 *                grib product definition section.
 *     gds -      a pointer to a structure which stores information about the 
 *                grib grid definition section.
 *     filename - This is the output grib file which will be created if 
 *                it does not already exist.  If this file already exists,
 *                grib records will be appended to it.
 *     data -     a 2-d array holding the values at grid points for the grid
 *                that is to be output.
 *   Return:
 *     1 for success
 *    -1 for failure
 *
 *   Caveats:   This function only supports a "latlon" grid
 *              currently, this is equivalent to a cylindrical equidistant
 *              projection.
 *
 ***************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include "gribfuncs.h"

int rg_write_grib(PDS_INPUT *pds, grid_desc_sec *gds, char filename[],
	       float **data)
{
  char tmpfile[240];
  char tmpstring[240];
  FILE *fid;
  int status;

  sprintf(tmpfile,"/tmp/tmpgribfile_%d",getpid());
  fid = fopen(tmpfile,"wb");
  if (fid == NULL) {
    fprintf(stderr,"rg_write_grib: Could not open %s\n",tmpfile);
    return -1;
  }

  status = rg_fwrite_grib(pds,gds,data,fid);
  if (status != 1)
    {
      fprintf(stderr,"rg_write_grib: rg_fwrite_grib failed\n");
      return -1;
    }
  
  /* append tmpfile to filename */
  sprintf(tmpstring,"cat %s >> %s",tmpfile,filename);
  system(tmpstring);
  unlink(tmpfile);
 
  close(fid);

  return(1);
}

int rg_fwrite_grib(PDS_INPUT *pds, grid_desc_sec *gds, float **data, FILE *fid)
{

  GRIB_HDR *gh=NULL;
  DATA_INPUT data_input;
  GEOM_IN geom_in;
  USER_INPUT user_input;
  char errmsg[240];
  float *data_one_d;
  int status;
  int i,j;

  if ((gds->head.usData_type == LATLON_PRJ) || 
      (gds->head.usData_type == GAUSS_PRJ)) {
    if (gds->head.usData_type == GAUSS_PRJ) {
      strcpy(geom_in.prjn_name,"gaussian");
    } else {
      strcpy(geom_in.prjn_name,"spherical");
    }
    geom_in.nx = gds->llg.usNi;
    geom_in.ny = gds->llg.usNj;
    geom_in.parm_1 = (gds->llg.iDj)/1000.;
    geom_in.parm_2 = (gds->llg.iDi)/1000.;
    geom_in.first_lat = gds->llg.lLat1/1000.;
    geom_in.first_lon = gds->llg.lLon1/1000.;
    geom_in.last_lat = gds->llg.lLat2/1000.;
    geom_in.last_lon = gds->llg.lLon2/1000.;
    geom_in.scan = gds->llg.usScan_mode;
    geom_in.usRes_flag = gds->llg.usRes_flag;
  } else if (gds->head.usData_type == LAMB_PRJ) {
    strcpy(geom_in.prjn_name,"lambert");
    geom_in.nx = gds->lam.iNx;
    geom_in.ny = gds->lam.iNy;
    geom_in.x_int_dis = gds->lam.ulDx/1000.;
    geom_in.y_int_dis = gds->lam.ulDy/1000.;
    geom_in.parm_1 = (gds->lam.lLat_cut2)/1000.;
    geom_in.parm_2 = (gds->lam.lLat_cut1)/1000.;
    geom_in.parm_3 = (gds->lam.lLon_orient)/1000.;
    geom_in.first_lat = gds->lam.lLat1/1000.;
    geom_in.first_lon = gds->lam.lLon1/1000.;
    geom_in.scan = gds->lam.usScan_mode;
    geom_in.usRes_flag = gds->lam.usRes_flag;    
  } else {
    fprintf(stderr,"rg_fwrite_grib: invalid input projection %d\n",
	    gds->head.usData_type);
    return -1;
  }

  data_input.usProc_id = pds->usProc_id;
  data_input.usGrid_id = pds->usGrid_id;
  data_input.usParm_id = pds->usParm_id;
  data_input.usParm_sub_id = pds->usCenter_sub;
  data_input.usLevel_id = pds->usLevel_id;
  data_input.nLvl_1 = pds->usHeight1;
  data_input.nLvl_2 = pds->usHeight2;
  data_input.nYear = pds->usYear + (pds->usCentury-1)*100;
  data_input.nMonth = pds->usMonth;
  data_input.nDay = pds->usDay;
  data_input.nHour = pds->usHour;
  data_input.nMinute = pds->usMinute;
  data_input.nSecond = 0;
  data_input.usFcst_id = pds->usFcst_unit_id;
  data_input.usFcst_per1 = pds->usP1;
  data_input.usFcst_per2 = pds->usP2;
  data_input.usTime_range_id = pds->usTime_range;
  data_input.usTime_range_avg = pds->usTime_range_avg;
  data_input.usTime_range_mis = pds->usTime_range_mis;
  /* We add an extra digit here, because the grib library seems to cut off
   *   one more than I would prefer.
   */
  data_input.nDec_sc_fctr = pds->sDec_sc_fctr+1;

  user_input.chCase_id='0';
  user_input.usParm_tbl=pds->usParm_tbl;
  user_input.usSub_tbl=pds->usSub_tbl;
  /*
  user_input.usCenter_id=190;
  */
  user_input.usCenter_id = pds->usCenter_id;
  user_input.usCenter_sub=pds->usCenter_sub;
  user_input.usTrack_num=0;
  user_input.usGds_bms_id = 128;
  user_input.usBDS_flag=0;
  user_input.usBit_pack_num=0;

 
  data_one_d = (float *)calloc(geom_in.nx*geom_in.ny,sizeof(float));
  if (data_one_d == NULL) {
    fprintf(stderr,"rg_fwrite_grib: could not allocate space for data_one_d\n");
    return -1;
  }

  for (i=0; i<geom_in.nx; i++) {
    for (j=0; j<geom_in.ny; j++) {
      data_one_d[i+j*geom_in.nx] = data[j][i];
    }
  }

  status = init_gribhdr(&gh,errmsg);
  if (status != 0) {
    fprintf (stderr,"rg_fwrite_grib: %s",errmsg);
    return -1;
  }

  status = grib_enc(data_input,user_input,geom_in,data_one_d,gh,errmsg);
  if (status != 0) {
    fprintf (stderr,"rg_fwrite_grib: %s",errmsg);
    return -1;
  }

  status = gribhdr2file(gh,fid,errmsg);
  if (status != 0) {
    fprintf (stderr,"rg_fwrite_grib: %s",errmsg);
    return -1;
  }

  free_gribhdr(&gh);

  return 1;
  
}
