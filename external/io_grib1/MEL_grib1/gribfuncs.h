#include <stdio.h>
#include "grib.h"	/* all gribs structs */
#include "input.h"	/* GEOM+IN, DATA+INPUT, USER_INPUT */

/* check Compiler,  prototypes needed for ANSI-C */

/*#if defined( __cplusplus ) || defined( __STDC__ ) || defined( __GNUC__ )*/
/* Changed by Todd Hutchinson to force the need for PROTOTYPE */
#if 1

#define PROTOTYPE_NEEDED 1

int Expand_gribhdr (GRIB_HDR *, long, char *);
int prt_badmsg (GRIB_HDR  *gh, char *errmsg);
int make_grib_log (char *,char *,unsigned long,long,PDS_INPUT,
		grid_desc_sec,BDS_HEAD_INPUT,BMS_INPUT,float *,char *);
int FTP_getfile (char *,char *,char *);
int apply_bitmap (BMS_INPUT *,float **,float,BDS_HEAD_INPUT *,char *);
int create_inpLambert (GEOM_IN,void **,char *);
int create_inpLatlon (GEOM_IN,void **,char *);
void create_inpPDS (DATA_INPUT,USER_INPUT,PDS_INPUT *);
int create_inpPolar  (GEOM_IN,void **,char *);
void display_gribhdr (GRIB_HDR *hdr);
void free_gribhdr (GRIB_HDR **);
void gbyte (char *,unsigned long *,unsigned long *,unsigned long);
void gbyte_quiet (char *,unsigned long *,unsigned long *,unsigned long);
int grib_dec (char *,PDS_INPUT *,grid_desc_sec *,BDS_HEAD_INPUT *,
		BMS_INPUT *,float **,char *);
int grib_enc (DATA_INPUT,USER_INPUT,GEOM_IN,float *,GRIB_HDR *,char *);
float grib_ibm_local(unsigned long ibm_float);
int grib_seek (char *,long *,int,GRIB_HDR *,char *);
int gribgetbds (char *,short,BMS_INPUT *,grid_desc_sec *,float **,
		BDS_HEAD_INPUT *, char *);
int gribgetbms (char *,BMS_INPUT *,int,unsigned long,char *);
int gribgetgds (char *,grid_desc_sec *,char *);
int gribgetpds (char *,PDS_INPUT *,char *);
int gribhdr2file (GRIB_HDR *,FILE *,char *);
int gribputbds (USER_INPUT,long,short,float *,BDS_HEAD_INPUT *,
		GRIB_HDR **,char *);
int gribputgds (GEOM_IN,GDS_HEAD_INPUT *,void **,GRIB_HDR **,char *);
int gribputpds(DATA_INPUT,USER_INPUT,PDS_INPUT *,GRIB_HDR**,char *);
void hdr_print (char *,unsigned char *,int );
void init_dec_struct (PDS_INPUT *,grid_desc_sec *,BMS_INPUT *,BDS_HEAD_INPUT *);
void init_enc_struct (DATA_INPUT *,GEOM_IN *,USER_INPUT *);
int init_gribhdr (GRIB_HDR **,char *);
void init_struct (void *, int);
int inp2Grib_Lambert (void **,LAMBERT *,long *,char *);
int inp2grib_Latlon (void **,LATLON *,long *,char *);
int inp2grib_PDS (PDS_INPUT *,PDS_GRIB **,char *);
int inp2grib_PolarSt (void **,POLAR  *,long *,char *);
int ld_dec_lookup (char *,char *);
int ld_enc_config (char *,USER_INPUT *,char *);
int ld_enc_ffinfo (char *, DATA_INPUT *,char *);
int ld_enc_geomfile (char *,GEOM_IN *,char *);
int ld_enc_ieeeff (char *,float *,int,char *);
int ld_enc_lookup (char *,char *);
int ld_grib_origctrs (char *,char *,char *);
unsigned long grib_local_ibm (double local_float);
void make_default_grbfn (DATA_INPUT,USER_INPUT,char *);
int map_lvl (char *,DATA_INPUT *,float *,float *,char *);
int map_parm (char *,DATA_INPUT *,float *,float *,char *);
int pack_spatial (long *,unsigned short *,float *,float *,
		unsigned long **,short,long *,char *);
void prt_inp_struct (PDS_INPUT *,grid_desc_sec *, BMS_INPUT *,
		struct BDS_HEAD_INPUT *,float **);
void upd_child_errmsg (char *,char *);
void w3ft33_(float *ain,float **out, int *nsflag);
#else
#define PROTOTYPE_NEEDED 0

int Expand_gribhdr ();
int prt_badmsg ();
int  make_grib_log ();
int FTP_getfile  ();
int apply_bitmap ();
int create_inpLambert ();
int create_inpLatlon  ();
void create_inpPDS ();
int create_inpPolar   ();
void display_gribhdr ();
void free_gribhdr ();
void gbyte ();
void gbyte_quiet ();
int grib_dec ();
int grib_enc ();
float grib_ibm_local();
int grib_seek ();
int gribgetbds ();
int gribgetbms ();
int gribgetgds ();
int gribgetpds ();
int gribhdr2file ();
int gribputbds ( );
int gribputgds ();
int gribputpds();
void hdr_print ();
void init_dec_struct ();
void init_enc_struct ();
int init_gribhdr ();
void init_struct ();
int inp2Grib_Lambert ();
int inp2grib_Latlon  ();
int inp2grib_PDS ();
int inp2grib_PolarSt ();
int ld_dec_lookup ();
int ld_enc_config ();
int ld_enc_ffinfo ();
int ld_enc_geomfile ();
int ld_enc_ieeeff ();
int ld_enc_lookup ();
int ld_grib_origctrs ();
unsigned long grib_local_ibm ();
void make_default_grbfn ();
int map_lvl ();
int map_parm ();
int pack_spatial ();
void prt_inp_struct ();
void upd_child_errmsg ();
#endif
