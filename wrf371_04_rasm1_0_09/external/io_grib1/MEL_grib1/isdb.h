#ifndef ISDB_INCLUDE
#define ISDB_INCLUDE

#ifdef _NET_NEONS
#include <nn_client.h>
#endif /* _NET_NEONS */

/* max value from rand; HW/OS dependent */
#define MAX_RAND 2147483647.  	/* SUN */		
/* #define MAX_RAND 32767.	*/		/* HP, Solaris */

/* Vers 4.2.1 was installed 10/01/97 on Kelvin */
#define NEONS_VRSN_MAJOR  4	/* major release version of NEONS software */
#define NEONS_VRSN_MINOR  2	/* minor release version of NEONS software */
#define NEONS_VRSN_MINOR2 1	/* (sub) minor release version of software */
				/* (for bug fixes) 			   */

#define BYTE_BIT_CNT  8		/* count of bits per byte */
#define WORD_BIT_CNT  sizeof(long)*BYTE_BIT_CNT /* count of bits per word */
#define WORD_BYTE_CNT 4		/* count of bytes per word */
 
#define	OPN_RD    1		/* open database for read only */
#define	OPN_WR_RD 2		/* open database for write+read */

#define MAX_FILE_CNT  15	/* maximum count of open files */

#define ISDB_MODE   0666 	/* mode for image files placed into db */
#define LOCK_MODE   0200 	/* mode for locking files while writing */
#define INGEST_OWN "dba"   	/* owner of image files before loaded in db */ 
#define INGEST_MODE 0644 	/* mode for ingest files before loaded in db */ 


#define CLNDR_HOUR   0		/* calendar time, units = hours */
#define CLIMO_DAY    1		/* climatology time, units = day in year */
#define CLIMO_WEEK   2		/* climatology time, units = week in year */
#define CLIMO_MONTH  3		/* climatology time, units = month in year */
#define CLIMO_SEASON 4		/* climatology time, units = season in year */

typedef struct {		/* date structure */
   int year;			/* year number since 0 BC */
   int month;			/* month number in year */
   int day;			/* day number in month */
   int type;			/* time coordinate type, default time type */
				/* is calendar time 			   */
} DATE;

typedef struct {		/* info from table as_band */
    long   chan_num;		/* channel number within sensor */
    char   band_name[31];	/* name of band */
    long   bit_cnt;		/* count of bits in pixel */
    float  scl_fctr;		/* scaling factor */
    float  reference;		/* reference value */
    char   unit_name[31];	/* name of physical units */
} AS_BAND;

typedef struct {		/* info from tables grid_reg_geom/as_reg_im */
    char   prjn_name[21];	/* projection name */
    char   stor_dsc[21];	/* (+x in +y)/(+x in -y)/(-y in +x)/etc */ 
    long   nx;			/* count of columns */
    long   ny;			/* count of rows */
    double lat;			/* lat of origin in degrees */
    double lon;			/* lon of origin in degrees */
#ifdef OLD_REG_GEOM
    long orig_ix;		/* column # for origin, left column is 1 */
    long orig_iy;		/* row # for origin; top row is 1 */
    float x_int_dis;		/* distance interval between columns in km */
    float y_int_dis;		/* distance interval between rows in km */
    float parm_1;		/* geom parm 1, depends on projection */
    float parm_2;		/* geom parm 2, depends on projection */
    float parm_3;		/* geom parm 3, depends on projection */
#else
    double orig_ix;		/* column # for origin, left column is 1 */
    double orig_iy;		/* row # for origin; top row is 1 */
    double x_int_dis;		/* distance interval between columns in km */
    double y_int_dis;		/* distance interval between rows in km */
    double parm_1;		/* geom parm 1, depends on projection */
    double parm_2;		/* geom parm 2, depends on projection */
    double parm_3;		/* geom parm 3, depends on projection */
#endif /* OLD_REG_GEOM */
} REG_GEOM;

typedef struct {		/* info from table as_sat_im */
    long   bgn_lin_num;		/* beginning line number in orbit or pass */ 
    long   bgn_smp_num;		/* beginning sample number in scan line */
    long   lin_int;		/* lin interval relative to sensor scan mode */
    long   smp_int;		/* smp interval relative to sensor scan mode */
    float  roll_ang;		/* satellite roll angle in degrees */
    float  pch_ang;		/* satellite pitch angle in degrees */
    float  yaw_ang;		/* satellite yaw angle in degrees */
} SAT_GEOM;

typedef struct {		/* info from table grid_spct_geom */
    char   stor_dsc[21];	/* (+x in +y)/(+x in -y)/(-y in +x)/etc */ 
    char   trnc_type[21];	/* spectral truncation type (triangular/etc) */ 
    long   coef_cnt;		/* count of complex coefficients used */ 
    long   max_lat_wav_num;	/* max latitudinal wavenumber (M in GRIB) */
    long   max_lon_wav_num_1;	/* max longitudinal wavenumber 1 (J in GRIB) */
    long   max_lon_wav_num_2;	/* max longitudinal wavenumber 2 (K in GRIB) */
} SPCT_GEOM;

typedef struct {		/* info from table sat_oe */
    char   seq_name[21];	/* orb-elem sequence name */
    DATE   date;		/* date for orbital elements */
    double hour;		/* hour of day for orbital elements */
    double parm_1;		/* orb-elem parm 1 value */
    double parm_2;		/* orb-elem parm 2 value */
    double parm_3;		/* orb-elem parm 3 value */
    double parm_4;		/* orb-elem parm 4 value */
    double parm_5;		/* orb-elem parm 5 value */
    double parm_6;		/* orb-elem parm 6 value */
    double parm_7;		/* orb-elem parm 7 value */
} ORB_ELEM;

typedef struct {		/* bit-map structure */
   short lin_cnt;		/* count of lines in bitmap */
   short smp_cnt;		/* count of samples per line in bitmap */
   short pad_bit_cnt;		/* count of bits for padding lines in bitmap */
   short ofst_byte_cnt;		/* byte offset into bitmap array where actual
				   bitmap data begins */
   unsigned char *bmap;		/* unsigned char array containing bitmap */
} BITMAP;

#endif  /* ISDB_INCLUDE */
