typedef struct DATA_INPUT {
   unsigned short usProc_id;       /* Generating Process ID Number (Table A) */
   unsigned short usGrid_id;       /* Grid Identification (Table B)          */
   unsigned short usParm_id;       /* GRIB parameter id */
   unsigned short usParm_sub_id;   /* GRIB parameter sub-id */
   unsigned short usLevel_id;      /* GRIB level id */
   int            nLvl_1;          /* 1st level value - scaled to an integer*/
   int            nLvl_2;          /* 2nd level value - scaled to an integer*/
   int            nYear;           /* year of data e.g. 1993 */
   int            nMonth;          /* month of year e.g. 8 */
   int            nDay;            /* day of month e.g. 31 */
   int            nHour;           /* hour of day e.g. 0 */
   int            nMinute;         /* minute of hour e.g. 0 */
   int            nSecond;         /* second of minute e.g. 0 */
   unsigned short usFcst_id;       /*  Forecast time unit id - Table 4 */
   unsigned short usFcst_per1;     /* forecast time 1 (tau) e.g. 0. */
   unsigned short usFcst_per2;     /* forecast time 2 (tau) e.g. 0. */
   unsigned short usTime_range_id; /* Time range indicator - Table 5 */
   unsigned short usTime_range_avg;/* Number in average */
   unsigned short usTime_range_mis;/* Number missing from average */
   int            nDec_sc_fctr;    /* Decimal scale factor */
   /* WSI Extended PDS fields */
   unsigned short PDS_41;          /* Forecast time 1 unit id - Table 4 */
   int            PDS_42;          /* forecast time 1 (up to 4 bytes) */
   unsigned short PDS_46;          /* Forecast time 2 unit id - Table 4 */
   int            PDS_47;          /* forecast time 2 */
   unsigned short PDS_51;          /* Time range indicator - Table 5 */
  unsigned short  PDS_52;          /* Top of atmosphere--used with sigma coord*/
} DATA_INPUT;

typedef struct GEOM_IN {        /* info from tables grid_reg_geom/as_reg_im */
    char   prjn_name[21];       /* projection name */
    char   stor_dsc[21];        /* (+x in +y)/(+x in -y)/(-y in +x)/etc */
    long   nx;                  /* count of columns */
    long   ny;                  /* count of rows */
    double lat;                 /* lat of origin in degrees */
    double lon;                 /* lon of origin in degrees */
    double orig_ix;             /* column # for origin, left column is 1 */
    double orig_iy;             /* row # for origin; top row is 1 */
    double x_int_dis;           /* distance interval between columns in km */
    double y_int_dis;           /* distance interval between rows in km */
    double parm_1;              /* geom parm 1, depends on projection
				 * Spherical:	j Direction Increment (Latitude)
				 * Lambert:*/	
    double parm_2;              /* geom parm 2, depends on projection */
    double parm_3;              /* geom parm 3, depends on projection */
/* Do NOT MODIFY parameters before this point */
/* Additional Parameters Required by GRIB */
    double first_lat;		/* latitude of grid point (1,1) */
    double first_lon;		/* longitude of grid point (1,1) */
    double last_lat;		/* latitude of grid point (nx,ny) */
    double last_lon;		/* longitude of grid point (nx,ny) */
    unsigned short scan;	/* Scan mode value from Table 8 */
    unsigned short usRes_flag;  /* Resolution and Component Flags (Table 7)    */
} GEOM_IN;

typedef struct USER_INPUT {         /* user's input from input.dat            */
/* PDS Section */
    unsigned char  chCase_id;   /* User defined case ID (1 digit alphanumeric)*/
    unsigned short usParm_tbl;  /* GRIB Table Version Number             */
    unsigned short usSub_tbl;   /* Local Table Version Number            */
    unsigned short usCenter_id; /* ID of Originating Center (Table 0)     */
    unsigned short usGds_bms_id;/* GDS and BMS Flag (Table 1)             */
    unsigned short usCenter_sub;/* Sub-Table Entry for originating Ctr (Tbl 0)*/
    unsigned short usTrack_num; /* Tracking ID for data set               */
    unsigned short usBDS_flag;  /* Binary Data Section Flag (Table 11)    */
    unsigned short usBit_pack_num; /* Number of bits into which data is packed*/
} USER_INPUT;
