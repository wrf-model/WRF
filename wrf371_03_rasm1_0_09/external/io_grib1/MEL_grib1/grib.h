/*** FILE:  grib.h ***/
/* Revisions:
10/16/97/atn:  +usData_type to mercator, space view structs 
11/04/97/atn:  reduced DEF+MSG_LEN  from 250k;
02/18/98/atn:  + Projection codes; -ENC_DELIMITOR; -struct space_view;
04/22/98/atn:  + extension flag;
*/

#define EXTENSION_FLAG 99	/* Implies extensions if equals PDS Oct41 */
#define DEF_MSG_LEN    50000		/* size of GRIB HDR 's entire_msg */
#define MAX_PROJ_SIZE  46               /* MaxSize of GDS minus 6 bytes*/
					/* Currently set to LATLON; */
/*#define MAX_INP_PROJ_SIZE 72*/	/* Size of Max Input GDS block, */
					/* currently set to GDS_LATLON_INPUT */
/* Todd Hutchinson 8/11/05 */
/*
 * The above definition for MAX_INP_PROJ_SIZE only works with machines that
 *    have 4 byte long ints.  Here, we will quadruple this to handle up to 
 *    16 byte long ints.  This was an issue on the IBM in 64 bit mode.
 */
#define MAX_INP_PROJ_SIZE 288

/* WMO projection codes 
	***  MUST keep parallel to 'prjn_name' array       *** 
	***  IF altered, must also update the 'PRJ_COUNT'  *** 
*/
#define LATLON_PRJ		0
#define MERC_PRJ		1
#define LAMB_PRJ		3
#define GAUSS_PRJ 	 	4
#define POLAR_PRJ		5
#define ALBERS_PRJ		8
#define ROT_LATLON_PRJ		10
#define OBLIQ_LAMB_PRJ		13
#define ROT_GAUSS_PRJ	 	14
#define STR_LATLON_PRJ		20
#define STR_GAUSS_PRJ   	24
#define STR_ROT_LATLON_PRJ	30
#define STR_ROT_GAUSS_PRJ 	34

#define PRJ_COUNT		35	/* num of elements in prjn_name[] */

/* WMO projection names (use WMO codes above to index) */
static char* 	prjn_name[] = {
	"(0) Latitude/Longitude Grid",
	"(1) Mercator Grid",
	"(2) Unsupported Grid",
	"(3) Lambert Grid",
	"(4) Gaussian Grid",
	"(5) Polar Grid",
	"(6) Unsupported Grid",
	"(7) Unsupported Grid",
	"(8) Albers equal-area Grid",
	"(9) Unsupported Grid",
	"(10) Rotated Latitude/Longitude Grid",
	"(11) Unsupported Grid",
	"(12) Unsupported Grid",
	"(13) Oblique Lambert Grid",
	"(14) Rotated Gaussian Grid",
	"(15) Unsupported Grid",
	"(16) Unsupported Grid",
	"(17) Unsupported Grid",
	"(18) Unsupported Grid",
	"(19) Unsupported Grid",
	"(20) Stretched Latlon Grid",
	"(21) Unsupported Grid",
	"(22) Unsupported Grid",
	"(23) Unsupported Grid",
	"(24) Stretched Gaussian Grid",
	"(25) Unsupported Grid",
	"(26) Unsupported Grid",
	"(27) Unsupported Grid",
	"(28) Unsupported Grid",
	"(29) Unsupported Grid",
	"(30) Stretched Rotated Latlon Grid",
	"(31) Unsupported Grid",
	"(32) Unsupported Grid",
	"(33) Unsupported Grid",
	"(34) Stretched Rotated Gaussian Grid"
	};
	
/*.................................................................*/
typedef struct GRIB_HDR {	/* holds one Grib Msg & its info */
   char shuffled;                       /* set if sections are out of order */
   long	msg_length;			/* length in bytes of entire msg */
   long ids_len;			/* length in bytes of Ident Sect  */
   long pds_len; 			/* length in bytes of Prod Defn Sect */
   long gds_len; 			/* length in bytes of Grid Defn Sect */
   long bms_len; 			/* length in bytes of Bitmap Sect */
   long bds_len; 			/* length in bytes of Bin Data Sect */
   long eds_len; 			/* length in bytes of Ending Sect */
   long abs_size;			/* num bytes malloced to entire_msg*/
   unsigned char *entire_msg;		/* arr holding entire Grib msg */
   unsigned char *ids_ptr;		/* pts to 'GRIB' w/in entire msg */
   unsigned char *pds_ptr;		/* pts to PDS w/in entire msg */
   unsigned char *gds_ptr;		/* pts to GDS w/in entire msg */
   unsigned char *bms_ptr;		/* pts to BMS w/in entire msg */
   unsigned char *bds_ptr;		/* pts to BDS w/in entire msg */
   unsigned char *eds_ptr;		/* pts to '7777' w/in entire msg */
}  GRIB_HDR;

typedef struct PDS_INPUT{             /*  User input structure - PDS */
   unsigned short   uslength;            /* PDS Length - depends on extensions */
   unsigned short   usEd_num;            /* GRIB Edition number - #1 (IndS)  */
   unsigned short   usParm_tbl;          /* Parameter table number (1)    */
   unsigned short   usCenter_id;         /* Id of originating center (Table 0)*/
   unsigned short   usProc_id;           /* Generating process Id number (Table A) */
   unsigned short   usGrid_id;           /* Grid Identification (Table B)  */
   unsigned short   usGds_bms_id;        /* GDS and BMS flag (Table 1)     */
   unsigned short   usParm_id;           /* Parameter and unit id (Table 2) */
   unsigned short   usLevel_id;          /* Type of level or layer id (Table 3/3a) */
   unsigned short   usLevel_octets;      /* number of octets used in Table 3 (0, 1, 2 values) */
   unsigned short   usHeight1;           /* Height1, pressure1,etc of level (Table 3)*/
   unsigned short   usHeight2;           /* Height2, pressure2,etc of level (Table 3)*/
   unsigned short   usYear;              /* Year of century -Initial or ref. */
   unsigned short   usMonth;             /* Month of year   -time of forecast */
   unsigned short   usDay;               /* Day of month                  */
   unsigned short   usHour;              /* Hour of day                   */
   unsigned short   usMinute;            /* Minute of hour                */
   unsigned short   usFcst_unit_id;      /* Forecast time unit (Table 4)  */
   unsigned long    usP1;                /* Period of time (Number of time units)  */
   unsigned long    usP2;                /* Time interval between forecasts  */
   unsigned short   usTime_range;        /* Time range indicator (Table 5)   */
   unsigned short   usTime_range_avg;    /* Number included in average if flag set */
   unsigned short   usTime_range_mis;    /* Number missing from average      */
   unsigned short   usCentury;           /* Centry of Initial time (19)      */
   unsigned short   usCenter_sub;        /* Oct 26: Sub Center id          */
   short            sDec_sc_fctr;        /* Decimal scale factor             */
   unsigned short   ausZero[12];         /* Reserved                         */
   unsigned short   usExt_flag;	 	 /* Oct 41: Grib extensions usage flag*/
   unsigned short   usSecond;		 /* Second of Minute	             */
   unsigned short   usTrack_num;	 /* Tracking ID for data set	     */
   unsigned short   usParm_sub;		 /* Sub-Table Entry for parameter and unit (Table 2) */
   unsigned short   usSub_tbl;           /* Sub-Table version number */
   /* WSI Extended PDS fields */
   unsigned short PDS_41;          /* Forecast time 1 unit id - Table 4 */
   long           PDS_42;          /* forecast time 1 (up to 4 bytes) */
   unsigned short PDS_46;          /* Forecast time 2 unit id - Table 4 */
   long           PDS_47;          /* forecast time 2 */
   unsigned short PDS_51;          /* Time range indicator - Table 5 */
  unsigned short PDS_52;           /* Top of atmosphere--used with sigma coord*/
}PDS_INPUT;

typedef struct GDS_LAM_INPUT {     /* Input: Lambert Conformal Grid */
   unsigned short   usData_type;     /* Data representation type ( Table 6)    */
   int		    iNx;                 /* Nx - # of points along x-axis   */
   int		    iNy;                 /* Ny - # of points along y-axis   */
   long             lLat1;               /* Latitude of first grid point    */
   long             lLon1;               /* Longitude of first grid point   */
   unsigned short   usRes_flag;          /* Resolution and component flag (Table 7)*/
   long             lLon_orient;         /* Orientaion of grid - longitude  */
   unsigned long    ulDx;                /* X-direction grid length         */
   unsigned long    ulDy;                /* Y-direction grid length         */
   unsigned short   usProj_flag;         /* Projection center flag          */
   unsigned short   usScan_mode;         /* Scan mode                       */
   long             lLat_cut1;           /* First latitude which secant cone cuts  */
   long             lLat_cut2;           /* Second latitude from pole       */
   long             lLat_southpole;      /* Latitude of southern pole (millidegree)*/
   long             lLon_southpole;      /* Longitude of southern pole      */
   int		    usZero;              /* Reserved (set to 0)             */
}GDS_LAM_INPUT;

typedef struct GDS_LATLON_INPUT{   /* Input: Latitude/Longitude Grid */
   unsigned short   usData_type;     /* Data representation type ( Table 6)    */
   int		    usNi;                /* Number of points along a parallel */
   int		    usNj;                /* Number of points along a meridian */
   long             lLat1;               /* Latitude of first grid point      */
   long             lLon1;               /* Longitude of first grid point     */
   unsigned short   usRes_flag;          /* Resolution and component flag (Table 7)*/
   long             lLat2;               /* Latitude of last grid point       */
   long             lLon2;               /* Longitude of last grid point      */
   int              iDi;                 /* I-direction increment             */
   int              iDj;                 /* J-direction increment             */
   unsigned short   usScan_mode;         /* Scanning mode (Table 8)           */
   long		    usZero;              /* Reserved (set to 0)               */
   long             lLat_southpole;      /* Latitude of southern pole (millidegree)*/
   long             lLon_southpole;      /* Longitude of southern pole        */
   long             lRotate;             /* Angle of rotation                 */
   long             lPole_lat;           /* Latitude of pole of stretching (millidegree) */
   long             lPole_lon;           /* Longitude of pole of stretching   */
   long             lStretch;            /* Stretching factor                 */
}GDS_LATLON_INPUT;

typedef struct GDS_PS_INPUT {     /* Input: Polar Stereographic Grid */
   unsigned short   usData_type;     /* Data representation type ( Table 6)    */
   unsigned short   usNx;                /* Nx - # of points along x-axis */
   unsigned short   usNy;                /* Ny - # of points along y-axis */
   long             lLat1;               /* Latitude of first grid point */
   long             lLon1;               /* Longitude of first grid point */
   unsigned short   usRes_flag;          /* Resolution and component flag (Table 7) */
   long             lLon_orient;         /* Orientaion of grid - longitude */
   unsigned long    ulDx;                /* X-direction grid length */
   unsigned long    ulDy;                /* Y-direction grid length */
   unsigned short   usProj_flag;         /* Projection center flag */
   unsigned short   usScan_mode;         /* Scan mode */
   unsigned short   usZero;              /* Reserved (set to 0) */
} GDS_PS_INPUT;

typedef struct mercator  /*  mercator grids */
   {
   unsigned short   usData_type;     /* Data representation type ( Table 6)    */
   int cols;               /* Ni - Number of points along a latitude circle */
   int rows;               /* Nj - Number of points along a longitude meridian */
   long first_lat;         /* La1 - Latitude of first grid point */
   long first_lon;         /* Lo1 - Longitude of first grid point */
   unsigned short   usRes_flag;  /* Resolution and component flag (Table 7)*/
   long La2;               /* latitude of last grid point, or # point / row */
   long Lo2;               /* longitude of last grid point, or # point / column */
   long latin;             /* Latin - the latitude at which the mercator 
                                      projection intersects the earth */
   unsigned short   usZero1;   /* Reserved (set to 0)                    */
   unsigned short   usScan_mode; /* Scanning mode (Table 8)                */
   float lon_inc;       /* Di - the longitudinal direction increment 
                           (west to east) */
   float lat_inc;       /* Dj - the latitudinal direction increment 
                           (south to north) */
   long             usZero;              /* Reserved (set to 0)                    */
 }mercator;

typedef struct BDS_HEAD_INPUT {          /* BDS Header Input                */
   unsigned long    length;              /* BDS Length */
   unsigned short   usBDS_flag;          /* BDS flag (Table 11)                    */
   int              Bin_sc_fctr;         /* Binary scale factor                    */
   float            fReference;          /* Reference value (minimum value)        */
   unsigned short   usBit_pack_num;      /* Number of bits into which data is packed*/
   unsigned long    ulGrid_size;         /* Number of grid points                  */
   float            fPack_null;          /* Pack_null value for packing data       */
}BDS_HEAD_INPUT;

typedef struct GDS_HEAD_INPUT {        /* internal GDS Header Input */
   unsigned short   usNum_v;             /* Number of vertical cords   */
   unsigned short   usPl_Pv;             /* PV or PL location  */
   unsigned short   usData_type;         /* Data representation type (Table 6)     */
   unsigned short   uslength;            /* GDS Length - depends on projection */
   int              *thin;               /* array to hold sizes of thinned rows */
}GDS_HEAD_INPUT;

typedef struct IDS_GRIB {              /* IDS -Indicator Section 0               */
   unsigned char   szId[4];              /* "GRIB" Identifier                      */
   unsigned char   achTtl_length[3];     /* Total length of GRIB msg               */
   unsigned char   chEd_num;             /* GRIB Edition number  - #1              */
} IDS_GRIB;

typedef struct PDS_GRIB {              /* PDS -Product Definition Section 1       */
   unsigned char    achPDS_length[3];    /* Section length (in octets)             */
   unsigned char    chParm_tbl;          /* Parameter table number (1)             */
   unsigned char    chCenter_id;         /* Id of originating center (Table 0)     */
   unsigned char    chProc_id;           /* Generating process Id number (Table A) */
   unsigned char    chGrid_id;           /* Grid Identification (Table B)          */
   unsigned char    chGds_bms_id;        /* GDS and BMS flag (Table 1)             */
   unsigned char    chParm_id;           /* Parameter and unit id (Table 2)        */
   unsigned char    chLevel_id;          /* Type of level or layer id (Table 3/3a) */
   unsigned char    achHeight[2];        /* Height, pressure,etc of level (Table 3)*/
   unsigned char    chYear;              /* Year of century     -Initial or ref.   */
   unsigned char    chMonth;             /* Month of year       -time of forecast  */
   unsigned char    chDay;               /* Day of month                           */
   unsigned char    chHour;              /* Hour of day                            */
   unsigned char    chMinute;            /* Minute of hour                         */
   unsigned char    chFcst_unit_id;      /* Forecast time unit (Table 4)           */
   unsigned char    chP1;                /* Period of time (Number of time units)  */
   unsigned char    chP2;                /* Time interval between forecasts        */
   unsigned char    chTime_range;        /* Time range indicator (Table 5)         */
   unsigned char    achTime_range_avg[2]; /* Number included in average if flag set */
   unsigned char    chTime_range_mis;    /* Number missing from average            */
   unsigned char    chCentury;           /* Centry of Initial time (19)            */
   unsigned char    chCenter_sub;        /* Oct-26: Sub Center Id  */
   unsigned char    achDec_sc_fctr[2];   /* Decimal scale factor                   */
   unsigned char    achZero[12];         /* Reserved                               */

   /* WSI Extended PDS fields */
   unsigned char PDS_41;                 /* Forecast time 1 unit id - Table 4 */
   unsigned char PDS_42[4];              /* forecast time 1 (up to 4 bytes) */
   unsigned char PDS_46;                 /* Forecast time 2 unit id - Table 4 */
   unsigned char PDS_47[4];              /* forecast time 2 */
   unsigned char PDS_51;                 /* Time range indicator - Table 5 */
   unsigned char PDS_52[2];           /* Top of atmosphere--used with sigma coord*/
  
  /* 
   * The following was removed by Todd Hutchinson, WSI, 4/11/2002 
   *   The extended pds section is now replaced with the values above
   */
  /* unsigned char    chExt_flag;          Oct-41: Grib extensions usage flag*/
  /* unsigned char    chSecond;            Second of Minute                       */
  /* unsigned char    chTrack_num[2];      Tracking ID for data set               */
  /* unsigned char    chParm_sub;          Sub-Table Entry for parameter and unit (Table 2) */
  /* unsigned char    chSub_tbl;           Sub-Table Version number */
} PDS_GRIB;

typedef struct GDS_HEAD {              /* GDS header                              */
   unsigned char    achGDS_length[3];    /* Section length (in octets)             */
   unsigned char    chNV;                /* # of vertical coord. parameters (not used)*/
   unsigned char    chPV;                /* Location of vert. coord., 255 if none  */ 
   unsigned char    chData_type;         /* Data representation type (Table 6)     */
} GDS_HEAD;

typedef struct LAMBERT {               /* Lambert Conformal Grid                  */
   unsigned char    achNx[2];                /* Nx - # of points along x-axis          */
   unsigned char    achNy[2];                /* Ny - # of points along y-axis          */
   unsigned char    achLat1[3];          /* Latitude of first grid point           */
   unsigned char    achLon1[3];          /* Longitude of first grid point          */
   unsigned char    chRes_flag;          /* Resolution and component flag (Table 7)*/
   unsigned char    achLon_orient[3];    /* Orientaion of grid - longitude         */
   unsigned char    achDx[3];            /* X-direction grid length                */
   unsigned char    achDy[3];            /* Y-direction grid length                */
   unsigned char    chProj_flag;         /* Projection center flag                 */
   unsigned char    chScan_mode;         /* Scan mode                              */
   unsigned char    achLat_cut1[3];      /* First latitude which secant cone cuts  */
   unsigned char    achLat_cut2[3];      /* Second latitude from pole              */
   unsigned char    achLat_southpole[3]; /* Latitude of southern pole (millidegree)*/
   unsigned char    achLon_southpole[3]; /* Longitude of southern pole             */
   unsigned char    achZero[2];              /* Reserved (set to 0)                    */
} LAMBERT;

typedef struct POLAR {                   /* Polar Stereographic Grid */
   unsigned char    achNx[2];            /* Nx - # of points along x-axis */
   unsigned char    achNy[2];            /* Ny - # of points along y-ayis */
   unsigned char    achLat1[3];          /* Latitude of first grid point */
   unsigned char    achLon1[3];          /* Longitude of first grid point */
   unsigned char    chRes_flag;          /* Resolution and component flag (Table 7) */
   unsigned char    achLon_orient[3];    /* Orientaion of grid - longitude */
   unsigned char    achDx[3];            /* X-direction grid length */
   unsigned char    achDy[3];            /* Y-direction grid length */
   unsigned char    chProj_flag;         /* Projection center flag */
   unsigned char    chScan_mode;         /* Scan mode */
   unsigned char    achZero[4];          /* Reserved (set to 0) */
} POLAR;

typedef struct MERCATOR {                /* Mercator Grid */
  unsigned char     achNi[2];            /* Ni - Number of points along latitude circle */
  unsigned char     achNj[2];            /* Nj - Number of points along longtitude meridian */
  unsigned char     achLat1[3];          /* Latitude of first grid point */
  unsigned char     achLon1[3];          /* Longtitude of first grid point */
  unsigned char     chRes_flag;          /* Resolution and component flag (Table 7) */
  unsigned char     achLat2[3];          /* latitude of last grid point */
  unsigned char     achLon2[3];          /* longitude of last grid point */
  unsigned char     achLatin[3];         /* latitude(s) at which the Mercator projection
					    cylinder intersects the earth */
  unsigned char     achZero1;            /* Reserved (set to 0) */
  unsigned char     chScan_mode;         /* Scan mode */
  unsigned char     achDi[3];            /* longitudinal direction increment (meters) */
  unsigned char     achDj[3];            /* latitudinal direction increment (meters) */
  unsigned char     achZero2[8];         /* Reserved (set to 0) */
} MERCATOR;

typedef struct LATLON {                  /* Input: Latitude/Longitude Grid          */
   unsigned char    achNi[2];            /* Number of points along a parallel      */
   unsigned char    achNj[2] ;           /* Number of points along a meridian      */
   unsigned char    achLat1[3];          /* Latitude of first grid point           */
   unsigned char    achLon1[3];          /* Longitude of first grid point          */
   unsigned char    chRes_flag;          /* Resolution and component flag (Table 7)*/
   unsigned char    achLat2[3];          /* Latitude of last grid point            */
   unsigned char    achLon2[3];          /* Longitude of last grid point           */
   unsigned char    achDi[2];             /* I-direction increment                  */
   unsigned char    achDj[2];             /* J-direction increment                  */
   unsigned char    chScan_mode;         /* Scanning mode (Table 8)                */
   unsigned char    achZero[4];              /* Reserved (set to 0)                    */
   unsigned char    achLat_southpole[3]; /* Latitude of southern pole (millidegree)*/
   unsigned char    achLon_southpole[3]; /* Longitude of southern pole             */
   unsigned char    achRotate[4];        /* Angle of rotation                      */
   unsigned char    achPole_lat[3];      /* Latitude of pole of stretching (millidegree) */
   unsigned char    achPole_lon[3];      /* Longitude of pole of stretching        */
   unsigned char    achStretch[4];       /* Stretching factor                      */
} LATLON;

typedef struct BDS_HEAD {                /* Binary Data Section 4                     */
   unsigned char    achBDS_length[3];    /* Section length                         */
   unsigned char    chBDS_flag;          /* Flag (Table 11)                        */
   unsigned char    achBin_sc_fctr[2];   /* Binary Scale Factor                    */
   unsigned char    achReference[4];     /* Reference value (minimum value)IBM format*/
   unsigned char    chBit_pack_num;      /* Number of bits into which data is packed*/
} BDS_HEAD;

typedef struct EDS_GRIB {                /* End Section 5                           */
   unsigned char    szEDS_id[4];         /* "7777" Ascii characters                */
} EDS_GRIB;

typedef struct grid_desc_sec     /* Grid Description Section */
{
   struct GDS_HEAD_INPUT   head;     /* GDS Header section - common to all */
   struct GDS_LATLON_INPUT llg;      /* Latitude/Longitude or Gaussian grids */
   struct GDS_LAM_INPUT    lam;      /* lambert conformal grids */
   struct GDS_PS_INPUT     pol;      /* polar stereographic grids */
   struct mercator         merc;         /* mercator grids */
}grid_desc_sec;

typedef struct BMS_GRIB 		/* Bit Map Section 3 			*/
{
  unsigned char	 achBMS_length[3];	/* Section length			*/
  unsigned char	 chUnused_bits;		/* #unused bits in bitmap stream	*/
  unsigned char  achBMS_id[2];		/* 0 or a predefined bitmap id		*/
} BMS_GRIB;

typedef struct BMS_INPUT	/* User Input structure - BMS */
{
  unsigned long  uslength;	/* section length */
  unsigned short usUnused_bits;	/* number of Unused bits */
  unsigned short usBMS_id;	/* 0 or a predefined id  */
  unsigned long  ulbits_set;	/* num of datapts present */
  char		*bit_map;	/* pts to beg. of BM bstream */
} BMS_INPUT;

