#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "pds4.h"
#include "gds.h"
#include "bms.h"
#include "bds.h"
#include "cnames.h"
#include "grib.h"

#define VERSION "v1.8.0.9i  (12-03-04) Wesley Ebisuzaki\n\t\tDWD-tables 2,201-203 (8-19-2003) Helmut P. Frank\n\t\tspectral: Luis Kornblueh (MPI)"

#define CHECK_GRIB

/*
 * wgrib.c extract/inventory grib records
 *
 *                              Wesley Ebisuzaki
 *
 * 11/94 - v1.0
 * 11/94 - v1.1: arbitary size grids, -i option
 * 11/94 - v1.2: bug fixes, ieee option, more info
 * 1/95  - v1.2.4: fix headers for SUN acc
 * 2/95  - v1.2.5: add num_ave in -s listing
 * 2/95  - v1.2.6: change %d to %ld
 * 2/95  - v1.2.7: more output, added some polar stereographic support
 * 2/95  - v1.2.8: max min format changed %f to %g, tidying up more info
 * 3/95  - v1.3.0: fix bug with bitmap, allow numbers > UNDEFINED
 * 3/95  - v1.3.1: print number of missing points (verbose)
 * 3/95  - v1.3.2: -append option added
 * 4/95  - v1.3.2a,b: more output, polar stereo support (-V option)
 * 4/95  - v1.3.3: added ECMWF parameter table (prelim)
 * 6/95  - v1.3.4: nxny from BDS rather than gds?
 * 9/95  - v1.3.4d: speedup in grib write
 * 11/95 - v1.3.4f: new ECMWF parameter table (from Mike Fiorino), EC logic
 * 2/96  - v1.3.4g-h: prelim fix for GDS-less grib files
 * 2/96  - v1.3.4i: faster missing(), -V: "pos n" -> "n" (field 2)
 * 3/96  - v1.4: fix return code (!inventory), and short records near EOF
 * 6/96  - v1.4.1a: faster grib->binary decode, updated ncep parameter table, mod. in clim. desc
 * 7/96  - v1.5.0: parameter-table aware, -v option changed, added "comments"
 *                 increased NTRY to 100 in seek_grib
 * 11/96 - v1.5.0b: added ECMWF parameter table 128
 * 1/97 - v1.5.0b2: if nxny != nx*ny { nx = nxny; ny = 1 }
 * 3/97 - v1.5.0b5: added: -PDS -GDS, Lambert Conformal
 * 3/97 - v1.5.0b6: added: -verf
 * 4/97 - v1.5.0b7: added -PDS10, -GDS10 and enhanced -PDS -GDS
 * 4/97 - v1.5.0b8: "bitmap missing x" -> "bitmap: x undef"
 * 5/97 - v1.5.0b9: thinned grids meta data
 * 5/97 - v1.5.0b10: changed 0hr fcst to anal for TR=10 and P1=P2=0
 * 5/97 - v1.5.0b10: added -H option
 * 6/97 - v1.5.0b12: thinned lat-long grids -V option
 * 6/97 - v1.5.0b13: -4yr
 * 6/97 - v1.5.0b14: fix century mark Y=100 not 0
 * 7/97 - v1.5.0b15: add ncep opn grib table
 * 12/97 - v1.6.1.a: made ncep_opn the default table
 * 12/97 - v1.6.1.b: changed 03TOT to O3TOT in operational ncep table
 * 1/98  - v1.6.2: added Arakawa E grid meta-data
 * 1/98  - v1.6.2.1: added some mode data, Scan -> scan
 * 4/98  - v1.6.2.4: reanalysis id code: subcenter==0 && process==180
 * 5/98  - v1.6.2.5: fix -H code to write all of GDS
 * 7/98  - v1.7: fix decoding bug for bitmap and no. bits > 24 (theoretical bug)
 * 7/98  - v1.7.0.b1: add km to Mercator meta-data
 * 5/99  - v1.7.2: bug with thinned grids & bitmaps (nxny != nx*ny)
 * 5/99  - v1.7.3: updated NCEP opn grib table
 * 8/99  - v1.7.3.1: updated level information
 * 9/00  - v1.7.3.4a: check for missing grib file
 * 2/01  - v1.7.3.5: handle data with precision greater than 31 bits
 * 8/01  - vDWD   : added DWD GRIB tables 201, 202, 203, Helmut P. Frank
 * 9/01  - vDWD   : added output "Triangular grid", Helmut P. Frank
 * 9/01  - v1.7.4: merged Hemut P. Frank's changes to current wgrib source code
 * 3/02  - vMPIfM: added support for spectral data type
 * 4/02  - v1.8:   merge vMPIfM changes, some fixes/generalizations
 * 10/02  - v1.8.0.1: added cptec table 254
 * 10/02  - v1.8.0.2: no test of grib test if no gds, level 117 redone
 * 10/02  - v1.8.0.3: update ncep_opn grib and levels
 * 11/02  - v1.8.0.3a: updated ncep_opn and ncep table 129
 * 9/03 - v1.8.0.4: update dwd tables (Helmut P. Frank), -dwdgrib option
 * 9/03 - v1.8.0.5: fix scan mode and change format
 * 10/03 - v1.8.0.7: Changes from Norwegian Met. Inst (ec tab #131, ex_ext)
 * 10/03 - v1.8.0.8: added -ncep_ens option
 *
 */

/*
 * MSEEK = I/O buffer size for seek_grib
 */

#define MSEEK 1024
#define BUFF_ALLOC0	40000


#ifndef min
#define min(a,b)  ((a) < (b) ? (a) : (b))
#define max(a,b)  ((a) < (b) ? (b) : (a))
#endif

#ifndef DEF_T62_NCEP_TABLE
#define DEF_T62_NCEP_TABLE	rean
#endif
enum Def_NCEP_Table def_ncep_table = DEF_T62_NCEP_TABLE;
int minute = 0;
int ncep_ens = 0;

int main(int argc, char **argv) {

    unsigned char *buffer;
    float *array;
    double temp, rmin, rmax;
    int i, nx, ny, file_arg;
    long int len_grib, pos = 0, nxny, buffer_size, n_dump, count = 1;
    unsigned char *msg, *pds, *gds, *bms, *bds, *pointer;
    FILE *input, *dump_file = NULL;
    char line[200];
    enum {BINARY, TEXT, IEEE, GRIB, NONE} output_type = NONE;
    enum {DUMP_ALL, DUMP_RECORD, DUMP_POSITION, DUMP_LIST, INVENTORY} 
	mode = INVENTORY;
    enum {none, dwd, simple} header = simple;

    long int dump = -1;
    int verbose = 0, append = 0, v_time = 0, year_4 = 0, output_PDS_GDS = 0;
    int print_GDS = 0, print_GDS10 = 0, print_PDS = 0, print_PDS10 = 0;
    char *dump_file_name = "dump", open_parm[3];
    int return_code = 0;

    if (argc == 1) {
	fprintf(stderr, "\nPortable Grib decoder for %s etc.\n",
	    (def_ncep_table == opn_nowarn || def_ncep_table == opn) ?
	    "NCEP Operations" : "NCEP/NCAR Reanalysis");
	fprintf(stderr, "   it slices, dices    %s\n", VERSION);
	fprintf(stderr, "   usage: %s [grib file] [options]\n\n", argv[0]);

	fprintf(stderr, "Inventory/diagnostic-output selections\n");
	fprintf(stderr, "   -s/-v                   short/verbose inventory\n");
	fprintf(stderr, "   -V                      diagnostic output (not inventory)\n");
	fprintf(stderr, "   (none)                  regular inventory\n");

	fprintf(stderr, " Options\n");
	fprintf(stderr, "   -PDS/-PDS10             print PDS in hex/decimal\n");
	fprintf(stderr, "   -GDS/-GDS10             print GDS in hex/decimal\n");
	fprintf(stderr, "   -verf                   print forecast verification time\n");
	fprintf(stderr, "   -ncep_opn/-ncep_rean    default T62 NCEP grib table\n");
	fprintf(stderr, "   -4yr                    print year using 4 digits\n");
	fprintf(stderr, "   -min                    print minutes\n");
	fprintf(stderr, "   -ncep_ens               ensemble info encoded in ncep format\n");

	fprintf(stderr, "Decoding GRIB selection\n");
	fprintf(stderr, "   -d [record number|all]  decode record number\n");
	fprintf(stderr, "   -p [byte position]      decode record at byte position\n");
	fprintf(stderr, "   -i                      decode controlled by stdin (inventory list)\n");
	fprintf(stderr, "   (none)                  no decoding\n");

	fprintf(stderr, " Options\n");
	fprintf(stderr, "   -text/-ieee/-grib/-bin  convert to text/ieee/grib/bin (default)\n");
	fprintf(stderr, "   -nh/-h                  output will have no headers/headers (default)\n");
	fprintf(stderr, "   -dwdgrib                output dwd headers, grib (do not append)\n");
	fprintf(stderr, "   -H                      output will include PDS and GDS (-bin/-ieee only)\n");
	fprintf(stderr, "   -append                 append to output file\n");
	fprintf(stderr, "   -o [file]               output file name, 'dump' is default\n");
	exit(8);
    }
    file_arg = 0;
    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i],"-PDS") == 0) {
	    print_PDS = 1;
	    continue;
	}
	if (strcmp(argv[i],"-PDS10") == 0) {
	    print_PDS10 = 1;
	    continue;
	}
	if (strcmp(argv[i],"-GDS") == 0) {
	    print_GDS = 1;
	    continue;
	}
	if (strcmp(argv[i],"-GDS10") == 0) {
	    print_GDS10 = 1;
	    continue;
	}
	if (strcmp(argv[i],"-v") == 0) {
	    verbose = 1;
	    continue;
	}
	if (strcmp(argv[i],"-V") == 0) {
	    verbose = 2;
	    continue;
	}
	if (strcmp(argv[i],"-s") == 0) {
	    verbose = -1;
	    continue;
	}
	if (strcmp(argv[i],"-text") == 0) {
	    output_type = TEXT;
	    continue;
	}
	if (strcmp(argv[i],"-bin") == 0) {
	    output_type = BINARY;
	    continue;
	}
	if (strcmp(argv[i],"-ieee") == 0) {
	    output_type = IEEE;
	    continue;
	}
	if (strcmp(argv[i],"-grib") == 0) {
	    output_type = GRIB;
	    continue;
	}
	if (strcmp(argv[i],"-nh") == 0) {
	    header = none;
	    continue;
	}
	if (strcmp(argv[i],"-h") == 0) {
	    header = simple;
	    continue;
	}
	if (strcmp(argv[i],"-dwdgrib") == 0) {
	    header = dwd;
	    output_type = GRIB;
	    continue;
	}
	if (strcmp(argv[i],"-append") == 0) {
	    append = 1;
	    continue;
	}
	if (strcmp(argv[i],"-verf") == 0) {
	    v_time = 1;
	    continue;
        }
	if (strcmp(argv[i],"-d") == 0) {
	    if (strcmp(argv[i+1],"all") == 0) {
	        mode = DUMP_ALL;
	    }
	    else {
	        dump = atol(argv[i+1]);
	        mode = DUMP_RECORD;
	    }
	    i++;
	    if (output_type == NONE) output_type = BINARY;
	    continue;
	}
	if (strcmp(argv[i],"-p") == 0) {
	    pos = atol(argv[i+1]);
	    i++;
	    dump = 1;
	    if (output_type == NONE) output_type = BINARY;
	    mode = DUMP_POSITION;
	    continue;
	}
	if (strcmp(argv[i],"-i") == 0) {
	    if (output_type == NONE) output_type = BINARY;
	    mode = DUMP_LIST;
	    continue;
	}
	if (strcmp(argv[i],"-H") == 0) {
	    output_PDS_GDS = 1;
	    continue;
	}
	if (strcmp(argv[i],"-NH") == 0) {
	    output_PDS_GDS = 0;
	    continue;
	}
	if (strcmp(argv[i],"-4yr") == 0) {
	    year_4 = 1;
	    continue;
	}
	if (strcmp(argv[i],"-ncep_opn") == 0) {
	    def_ncep_table = opn_nowarn;
	    continue;
	}
	if (strcmp(argv[i],"-ncep_rean") == 0) {
	    def_ncep_table = rean_nowarn;
	    continue;
	}
	if (strcmp(argv[i],"-o") == 0) {
	    dump_file_name = argv[i+1];
	    i++;
	    continue;
	}
	if (strcmp(argv[i],"--v") == 0) {
	    printf("wgrib: %s\n", VERSION);
	    exit(0);
	}
	if (strcmp(argv[i],"-min") == 0) {
	    minute = 1;
	    continue;
	}
	if (strcmp(argv[i],"-ncep_ens") == 0) {
	    ncep_ens = 1;
	    continue;
	}
	if (file_arg == 0) {
	    file_arg = i;
	}
	else {
	    fprintf(stderr,"argument: %s ????\n", argv[i]);
	}
    }
    if (file_arg == 0) {
	fprintf(stderr,"no GRIB file to process\n");
	exit(8);
    }
    if ((input = fopen(argv[file_arg],"rb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[file_arg]);
        exit(7);
    }

    if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
	fprintf(stderr,"not enough memory\n");
    }
    buffer_size = BUFF_ALLOC0;

    /* open output file */
    if (mode != INVENTORY) {
	open_parm[0] = append ? 'a' : 'w'; open_parm[1] = 'b'; open_parm[2] = '\0';
	if (output_type == TEXT) open_parm[1] = '\0';

	if ((dump_file = fopen(dump_file_name,open_parm)) == NULL) {
	    fprintf(stderr,"could not open dump file\n");
	    exit(8);
        }
	if (header == dwd && output_type == GRIB) wrtieee_header(0, dump_file);
    }

    /* skip dump - 1 records */
    for (i = 1; i < dump; i++) {
	msg = seek_grib(input, &pos, &len_grib, buffer, MSEEK);
	if (msg == NULL) {
	    fprintf(stderr, "ran out of data or bad file\n");
	    exit(8);
	}
	pos += len_grib;
    }
    if (dump > 0) count += dump - 1;
    n_dump = 0;

    for (;;) {
	if (n_dump == 1 && (mode == DUMP_RECORD || mode == DUMP_POSITION)) break;
	if (mode == DUMP_LIST) {
	    if (fgets(line,sizeof(line), stdin) == NULL) break;
            line[sizeof(line) - 1] = 0;
            if (sscanf(line,"%ld:%ld:", &count, &pos) != 2) {
		fprintf(stderr,"bad input from stdin\n");
                fprintf(stderr,"   %s\n", line);
	        exit(8);
	    }
	}

	msg = seek_grib(input, &pos, &len_grib, buffer, MSEEK);
	if (msg == NULL) {
	    if (mode == INVENTORY || mode == DUMP_ALL) break;
	    fprintf(stderr,"missing GRIB record(s)\n");
	    exit(8);
	}

        /* read all whole grib record */
        if (len_grib + msg - buffer > buffer_size) {
            buffer_size = len_grib + msg - buffer + 1000;
            buffer = (unsigned char *) realloc((void *) buffer, buffer_size);
            if (buffer == NULL) {
                fprintf(stderr,"ran out of memory\n");
                exit(8);
            }
        }
        read_grib(input, pos, len_grib, buffer);

	/* parse grib message */

	msg = buffer;
        pds = (msg + 8);
        pointer = pds + PDS_LEN(pds);
#ifdef DEBUG
	printf("LEN_GRIB= 0x%x\n", len_grib);
	printf("PDS_LEN= 0x%x: at 0x%x\n", PDS_LEN(pds),pds-msg);
#endif
        if (PDS_HAS_GDS(pds)) {
            gds = pointer;
            pointer += GDS_LEN(gds);
#ifdef DEBUG
	    printf("GDS_LEN= 0x%x: at 0x%x\n", GDS_LEN(gds), gds-msg);
#endif
        }
        else {
            gds = NULL;
        }

        if (PDS_HAS_BMS(pds)) {
            bms = pointer;
            pointer += BMS_LEN(bms);
#ifdef DEBUG
	    printf("BMS_LEN= 0x%x: at 0x%x\n", BMS_LEN(bms),bms-msg);
#endif
        }
        else {
            bms = NULL;
        }

        bds = pointer;
        pointer += BDS_LEN(bds);
#ifdef DEBUG
	printf("BDS_LEN= 0x%x: at 0x%x\n", BDS_LEN(bds),bds-msg);
#endif

#ifdef DEBUG
	printf("END_LEN= 0x%x: at 0x%x\n", 4,pointer-msg);
	if (pointer-msg+4 != len_grib) {
	    fprintf(stderr,"Len of grib message is inconsistent.\n");
	}
#endif

        /* end section - "7777" in ascii */
        if (pointer[0] != 0x37 || pointer[1] != 0x37 ||
            pointer[2] != 0x37 || pointer[3] != 0x37) {
            fprintf(stderr,"\n\n    missing end section\n");
            fprintf(stderr, "%2x %2x %2x %2x\n", pointer[0], pointer[1], 
		pointer[2], pointer[3]);
#ifdef DEBUG
	    printf("ignoring missing end section\n");
#else
	    exit(8);
#endif
        }

	/* figure out size of array */
	if (gds != NULL) {
	    GDS_grid(gds, bds, &nx, &ny, &nxny);
	}
	else if (bms != NULL) {
	    nxny = nx = BMS_nxny(bms);
	    ny = 1;
	}
	else {
	    if (BDS_NumBits(bds) == 0) {
                nxny = nx = 1;
                fprintf(stderr,"Missing GDS, constant record .. cannot "
                    "determine number of data points\n");
	    }
	    else {
	        nxny = nx = BDS_NValues(bds);
	    }
	    ny = 1;
	}

#ifdef CHECK_GRIB
	if (gds && ! GDS_Harmonic(gds)) {
	/* this grib check only works for simple packing */
	/* turn off if harmonic */
	    if (BDS_NumBits(bds) != 0) {
	        i = BDS_NValues(bds);
	        if (bms != NULL) {
	            i += missing_points(BMS_bitmap(bms),nxny);
	        }
	        if (i != nxny) {
	            fprintf(stderr,"grib header at record %ld: two values of nxny %ld %d\n",
			count,nxny,i);
		    fprintf(stderr,"   LEN %d DataStart %d UnusedBits %d #Bits %d nxny %ld\n",
			BDS_LEN(bds), BDS_DataStart(bds),BDS_UnusedBits(bds),
			BDS_NumBits(bds), nxny);
		    return_code = 15;
		    nxny = nx = i;
		    ny = 1;
	        }
	    }
 
        }
#endif
 
        if (verbose <= 0) {
	    printf("%ld:%ld:d=", count, pos);
	    PDS_date(pds,year_4,v_time);
	    printf(":%s:", k5toa(pds));

            if (verbose == 0) printf("kpds5=%d:kpds6=%d:kpds7=%d:TR=%d:P1=%d:P2=%d:TimeU=%d:",
	        PDS_PARAM(pds),PDS_KPDS6(pds),PDS_KPDS7(pds),
	        PDS_TimeRange(pds),PDS_P1(pds),PDS_P2(pds),
                PDS_ForecastTimeUnit(pds));
	    levels(PDS_KPDS6(pds), PDS_KPDS7(pds),PDS_Center(pds)); printf(":");
	    PDStimes(PDS_TimeRange(pds),PDS_P1(pds),PDS_P2(pds),
                PDS_ForecastTimeUnit(pds));
	    if (PDS_Center(pds) == ECMWF) EC_ext(pds,"",":");
	    ensemble(pds, verbose);
	    printf("NAve=%d",PDS_NumAve(pds));
	    if (print_PDS || print_PDS10) print_pds(pds, print_PDS, print_PDS10, verbose);
	    if (gds && (print_GDS || print_GDS10)) print_gds(gds, print_GDS, print_GDS10, verbose);
            printf("\n");
       }
       else if (verbose == 1) {
	    printf("%ld:%ld:D=", count, pos);
            PDS_date(pds, 1, v_time);
	    printf(":%s:", k5toa(pds));
	    levels(PDS_KPDS6(pds), PDS_KPDS7(pds), PDS_Center(pds)); printf(":");
            printf("kpds=%d,%d,%d:",
	        PDS_PARAM(pds),PDS_KPDS6(pds),PDS_KPDS7(pds));
	    PDStimes(PDS_TimeRange(pds),PDS_P1(pds),PDS_P2(pds),
                PDS_ForecastTimeUnit(pds));
	    if (PDS_Center(pds) == ECMWF) EC_ext(pds,"",":");
	    ensemble(pds, verbose);
	    GDS_winds(gds, verbose);
            printf("\"%s", k5_comments(pds));
	    if (print_PDS || print_PDS10) print_pds(pds, print_PDS, print_PDS10, verbose);
	    if (gds && (print_GDS || print_GDS10)) print_gds(gds, print_GDS, print_GDS10, verbose);
            printf("\n");
	}
        else if (verbose == 2) {
	    printf("rec %ld:%ld:date ", count, pos);
	    PDS_date(pds, 1, v_time);
	    printf(" %s kpds5=%d kpds6=%d kpds7=%d levels=(%d,%d) grid=%d ", 
	        k5toa(pds), PDS_PARAM(pds), PDS_KPDS6(pds), PDS_KPDS7(pds), 
                PDS_LEVEL1(pds), PDS_LEVEL2(pds), PDS_Grid(pds));
	        levels(PDS_KPDS6(pds),PDS_KPDS7(pds),PDS_Center(pds));

	    printf(" ");
	    if (PDS_Center(pds) == ECMWF) EC_ext(pds,""," ");
	    ensemble(pds, verbose);
	    PDStimes(PDS_TimeRange(pds),PDS_P1(pds),PDS_P2(pds),
                 PDS_ForecastTimeUnit(pds));
	    if (bms != NULL) 
		printf(" bitmap: %d undef", missing_points(BMS_bitmap(bms),nxny));
            printf("\n  %s=%s\n", k5toa(pds), k5_comments(pds));
	
            printf("  timerange %d P1 %d P2 %d TimeU %d  nx %d ny %d GDS grid %d "
		"num_in_ave %d missing %d\n", 
	        PDS_TimeRange(pds),PDS_P1(pds),PDS_P2(pds), 
                PDS_ForecastTimeUnit(pds), nx, ny, 
                gds == NULL ? -1 : GDS_DataType(gds), 
                PDS_NumAve(pds), PDS_NumMissing(pds));

	    printf("  center %d subcenter %d process %d Table %d", 
		PDS_Center(pds),PDS_Subcenter(pds),PDS_Model(pds),
                PDS_Vsn(pds));
	    GDS_winds(gds, verbose);
	    printf("\n");

	    if (gds && GDS_LatLon(gds) && nx != -1) 
		printf("  latlon: lat  %f to %f by %f  nxny %ld\n"
                       "          long %f to %f by %f, (%d x %d) scan %d "
                       "mode %d bdsgrid %d\n",
		  0.001*GDS_LatLon_La1(gds), 0.001*GDS_LatLon_La2(gds),
		  0.001*GDS_LatLon_dy(gds), nxny, 0.001*GDS_LatLon_Lo1(gds),
		  0.001*GDS_LatLon_Lo2(gds), 0.001*GDS_LatLon_dx(gds),
	    	  nx, ny, GDS_LatLon_scan(gds), GDS_LatLon_mode(gds),
		  BDS_Grid(bds));
	    else if (gds && GDS_LatLon(gds) && nx == -1) {
		printf("  thinned latlon: lat  %f to %f by %f  nxny %ld\n"
                       "          long %f to %f, %ld grid pts   (%d x %d) scan %d"
			" mode %d bdsgrid %d\n",
		  0.001*GDS_LatLon_La1(gds), 0.001*GDS_LatLon_La2(gds),
		  0.001*GDS_LatLon_dy(gds), nxny, 0.001*GDS_LatLon_Lo1(gds),
		  0.001*GDS_LatLon_Lo2(gds),
	    	  nxny, nx, ny, GDS_LatLon_scan(gds), GDS_LatLon_mode(gds),
		  BDS_Grid(bds));
		  GDS_prt_thin_lon(gds);
	    }
	    else if (gds && GDS_Gaussian(gds) && nx != -1)
		printf("  gaussian: lat  %f to %f\n"
                       "            long %f to %f by %f, (%d x %d) scan %d"
			" mode %d bdsgrid %d\n",
		  0.001*GDS_LatLon_La1(gds), 0.001*GDS_LatLon_La2(gds),
		  0.001*GDS_LatLon_Lo1(gds), 0.001*GDS_LatLon_Lo2(gds), 
		  0.001*GDS_LatLon_dx(gds),
	    	  nx, ny, GDS_LatLon_scan(gds), GDS_LatLon_mode(gds),
		  BDS_Grid(bds));
	    else if (gds && GDS_Gaussian(gds) && nx == -1) {
		printf("  thinned gaussian: lat  %f to %f\n"
                       "     lon %f   %ld grid pts   (%d x %d) scan %d"
			" mode %d bdsgrid %d  nlat:\n",
		  0.001*GDS_LatLon_La1(gds), 0.001*GDS_LatLon_La2(gds),
		  0.001*GDS_LatLon_Lo1(gds),
	    	  nxny, nx, ny, GDS_LatLon_scan(gds), GDS_LatLon_mode(gds),
		  BDS_Grid(bds));
		  GDS_prt_thin_lon(gds);
	    }
	    else if (gds && GDS_Polar(gds))
		printf("  polar stereo: Lat1 %f Long1 %f Orient %f\n"
			"     %s pole (%d x %d) Dx %d Dy %d scan %d mode %d\n",
		    0.001*GDS_Polar_La1(gds),0.001*GDS_Polar_Lo1(gds),
		    0.001*GDS_Polar_Lov(gds),
		    GDS_Polar_pole(gds) == 0 ? "north" : "south", nx,ny,
		    GDS_Polar_Dx(gds),GDS_Polar_Dy(gds),
		    GDS_Polar_scan(gds), GDS_Polar_mode(gds));
	    else if (gds && GDS_Lambert(gds))
		printf("  Lambert Conf: Lat1 %f Lon1 %f Lov %f\n"
                       "      Latin1 %f Latin2 %f LatSP %f LonSP %f\n"
                       "      %s (%d x %d) Dx %f Dy %f scan %d mode %d\n",
                     0.001*GDS_Lambert_La1(gds),0.001*GDS_Lambert_Lo1(gds),
                     0.001*GDS_Lambert_Lov(gds),
                     0.001*GDS_Lambert_Latin1(gds), 0.001*GDS_Lambert_Latin2(gds),
                     0.001*GDS_Lambert_LatSP(gds), 0.001*GDS_Lambert_LonSP(gds),
                      GDS_Lambert_NP(gds) ? "North Pole": "South Pole",
                     GDS_Lambert_nx(gds), GDS_Lambert_ny(gds),
                     0.001*GDS_Lambert_dx(gds), 0.001*GDS_Lambert_dy(gds),
                     GDS_Lambert_scan(gds), GDS_Lambert_mode(gds));
	    else if (gds && GDS_Mercator(gds))
		printf("  Mercator: lat  %f to %f by %f km  nxny %ld\n"
                       "          long %f to %f by %f km, (%d x %d) scan %d"
			" mode %d Latin %f bdsgrid %d\n",
		  0.001*GDS_Merc_La1(gds), 0.001*GDS_Merc_La2(gds),
		  0.001*GDS_Merc_dy(gds), nxny, 0.001*GDS_Merc_Lo1(gds),
		  0.001*GDS_Merc_Lo2(gds), 0.001*GDS_Merc_dx(gds),
	    	  nx, ny, GDS_Merc_scan(gds), GDS_Merc_mode(gds), 
		  0.001*GDS_Merc_Latin(gds), BDS_Grid(bds));
	    else if (gds && GDS_ssEgrid(gds))
		printf("  Semi-staggered Arakawa E-Grid: lat0 %f lon0 %f nxny %d\n"
                       "    dLat %f dLon %f (%d x %d) scan %d mode %d\n",
		  0.001*GDS_ssEgrid_La1(gds), 0.001*GDS_ssEgrid_Lo1(gds), 
                  GDS_ssEgrid_n(gds)*GDS_ssEgrid_n_dum(gds), 
                  0.001*GDS_ssEgrid_dj(gds), 0.001*GDS_ssEgrid_di(gds), 
                  GDS_ssEgrid_Lo2(gds), GDS_ssEgrid_La2(gds),
                  GDS_ssEgrid_scan(gds), GDS_ssEgrid_mode(gds));
            else if (gds && GDS_ss2dEgrid(gds))
                printf("  Semi-staggered Arakawa E-Grid (2D): lat0 %f lon0 %f nxny %d\n"
                       "    dLat %f dLon %f (tlm0d %f tph0d %f) scan %d mode %d\n",
                   0.001*GDS_ss2dEgrid_La1(gds), 0.001*GDS_ss2dEgrid_Lo1(gds),
                   GDS_ss2dEgrid_nx(gds)*GDS_ss2dEgrid_ny(gds),
                   0.001*GDS_ss2dEgrid_dj(gds), 0.001*GDS_ss2dEgrid_di(gds),
                   0.001*GDS_ss2dEgrid_Lo2(gds), 0.001*GDS_ss2dEgrid_La2(gds),
                   GDS_ss2dEgrid_scan(gds), GDS_ss2dEgrid_mode(gds));
	    else if (gds && GDS_fEgrid(gds)) 
		printf("  filled Arakawa E-Grid: lat0 %f lon0 %f nxny %d\n"
                       "    dLat %f dLon %f (%d x %d) scan %d mode %d\n",
		  0.001*GDS_fEgrid_La1(gds), 0.001*GDS_fEgrid_Lo1(gds), 
                  GDS_fEgrid_n(gds)*GDS_fEgrid_n_dum(gds), 
                  0.001*GDS_fEgrid_dj(gds), 0.001*GDS_fEgrid_di(gds), 
                  GDS_fEgrid_Lo2(gds), GDS_fEgrid_La2(gds),
                  GDS_fEgrid_scan(gds), GDS_fEgrid_mode(gds));
	    else if (gds && GDS_RotLL(gds))
		printf("  rotated LatLon grid  lat %f to %f  lon %f to %f\n"
		       "    nxny %ld  (%d x %d)  dx %d dy %d  scan %d  mode %d\n"
		       "    transform: south pole lat %f lon %f  rot angle %f\n", 
		   0.001*GDS_RotLL_La1(gds), 0.001*GDS_RotLL_La2(gds), 
		   0.001*GDS_RotLL_Lo1(gds), 0.001*GDS_RotLL_Lo2(gds),
		   nxny, GDS_RotLL_nx(gds), GDS_RotLL_ny(gds),
		   GDS_RotLL_dx(gds), GDS_RotLL_dy(gds),
		   GDS_RotLL_scan(gds), GDS_RotLL_mode(gds),
		   0.001*GDS_RotLL_LaSP(gds), 0.001*GDS_RotLL_LoSP(gds),
		   GDS_RotLL_RotAng(gds) );
	    else if (gds && GDS_Gnomonic(gds))
		printf("  Gnomonic grid\n");
	    else if (gds && GDS_Harmonic(gds))
		printf("  Harmonic (spectral):  pentagonal spectral truncation: nj %d nk %d nm %d\n",
		       GDS_Harmonic_nj(gds), GDS_Harmonic_nk(gds),
		       GDS_Harmonic_nm(gds));
		if (gds && GDS_Harmonic_type(gds) == 1)
		  printf("  Associated Legendre polynomials\n");
            else if (gds && GDS_Triangular(gds))
                printf("  Triangular grid:  nd %d ni %d (= 2^%d x 3^%d)\n",
		    GDS_Triangular_nd(gds), GDS_Triangular_ni(gds), 
                    GDS_Triangular_ni2(gds), GDS_Triangular_ni3(gds) );
	    if (print_PDS || print_PDS10) 
                print_pds(pds, print_PDS, print_PDS10, verbose);
	    if (gds && (print_GDS || print_GDS10)) 
                 print_gds(gds, print_GDS, print_GDS10, verbose);
	}

	if (mode != INVENTORY && output_type == GRIB) {
	    if (header == dwd) wrtieee_header((int) len_grib, dump_file);
	    fwrite((void *) msg, sizeof(char), len_grib, dump_file);
	    if (header == dwd) wrtieee_header((int) len_grib, dump_file);
	    n_dump++;
	}

	if ((mode != INVENTORY && output_type != GRIB) || verbose > 1) {
	    /* decode numeric data */
 
            if ((array = (float *) malloc(sizeof(float) * nxny)) == NULL) {
                fprintf(stderr,"memory problems\n");
                exit(8);
            }

	    temp = int_power(10.0, - PDS_DecimalScale(pds));

 	    BDS_unpack(array, bds, BMS_bitmap(bms), BDS_NumBits(bds), nxny,
			   temp*BDS_RefValue(bds),temp*int_power(2.0, BDS_BinScale(bds)));

	    if (verbose > 1) {
		rmin = FLT_MAX;
		rmax = -FLT_MAX;
	        for (i = 0; i < nxny; i++) {
		    if (fabs(array[i]-UNDEFINED) > 0.0001*UNDEFINED) {
	                rmin = min(rmin,array[i]);
	                rmax = max(rmax,array[i]);
		    }
	        }
	        printf("  min/max data %g %g  num bits %d "
			" BDS_Ref %g  DecScale %d BinScale %d\n", 
		    rmin, rmax, BDS_NumBits(bds), BDS_RefValue(bds),
		    PDS_DecimalScale(pds), BDS_BinScale(bds));
	    }

	    if (mode != INVENTORY && output_type != GRIB) {
		/* dump code */
		if (output_PDS_GDS == 1) {
		    /* insert code here */
	            if (output_type == BINARY || output_type == IEEE) {
			/* write PDS */
			i = PDS_LEN(pds) + 4;
	                if (header == simple && output_type == BINARY) 
				fwrite((void *) &i, sizeof(int), 1, dump_file);
	                if (header == simple && output_type == IEEE) wrtieee_header(i, dump_file);
	                fwrite((void *) "PDS ", 1, 4, dump_file);
	                fwrite((void *) pds, 1, i - 4, dump_file);
	                if (header == simple && output_type == BINARY) 
				fwrite((void *) &i, sizeof(int), 1, dump_file);
	                if (header == simple && output_type == IEEE) wrtieee_header(i, dump_file);

			/* write GDS */
			i = (gds) ?  GDS_LEN(gds) + 4 : 4;
	                if (header == simple && output_type == BINARY) 
				fwrite((void *) &i, sizeof(int), 1, dump_file);
	                if (header == simple && output_type == IEEE) wrtieee_header(i, dump_file);
	                fwrite((void *) "GDS ", 1, 4, dump_file);
	                if (gds) fwrite((void *) gds, 1, i - 4, dump_file);
	                if (header == simple && output_type == BINARY) 
				fwrite((void *) &i, sizeof(int), 1, dump_file);
	                if (header == simple && output_type == IEEE) wrtieee_header(i, dump_file);
		    }
		} 

	        if (output_type == BINARY) {
	            i = nxny * sizeof(float);
	            if (header == simple) fwrite((void *) &i, sizeof(int), 1, dump_file);
	            fwrite((void *) array, sizeof(float), nxny, dump_file);
	            if (header == simple) fwrite((void *) &i, sizeof(int), 1, dump_file);
	        }
		else if (output_type == IEEE) {
		    wrtieee(array, nxny, header, dump_file);
		}
	        else if (output_type == TEXT) {
	            /* number of points in grid */
	            if (header == simple) {
		        if (nx <= 0 || ny <= 0 || nxny != nx*ny) {
                            fprintf(dump_file, "%ld %d\n", nxny, 1);
			}
			else {
			    fprintf(dump_file, "%d %d\n", nx, ny);
			}
		    }
	            for (i = 0; i < nxny; i++) {
		        fprintf(dump_file,"%g\n", array[i]);
		    }
	        }
	        n_dump++;
	    }
	    free(array);
	    if (verbose > 0) printf("\n");
	}
	    
        pos += len_grib;
        count++;
    }

    if (mode != INVENTORY) {
	if (header == dwd && output_type == GRIB) wrtieee_header(0, dump_file);
	if (ferror(dump_file)) {
		fprintf(stderr,"error writing %s\n",dump_file_name);
		exit(8);
	}
    }
    fclose(input);
    return (return_code);
}

void print_pds(unsigned char *pds, int print_PDS, int print_PDS10, int verbose) {
    int i, j;

    j = PDS_LEN(pds);
    if (verbose < 2) {
        if (print_PDS && verbose < 2) {
            printf(":PDS=");
            for (i = 0; i < j; i++) {
                printf("%2.2x", (int) pds[i]);
            }
        }
        if (print_PDS10 && verbose < 2) {
            printf(":PDS10=");
            for (i = 0; i < j; i++) {
                printf(" %d", (int) pds[i]);
            }
        }
    }
    else {
        if (print_PDS) {
            printf("  PDS(1..%d)=",j);
            for (i = 0; i < j; i++) {
                if (i % 20 == 0) printf("\n    %4d:",i+1);
                printf(" %3.2x", (int) pds[i]);
            }
            printf("\n");
        }
        if (print_PDS10) {
            printf("  PDS10(1..%d)=",j);
            for (i = 0; i < j; i++) {
                if (i % 20 == 0) printf("\n    %4d:",i+1);
                printf(" %3d", (int) pds[i]);
            }
            printf("\n");
        }
    }
}

void print_gds(unsigned char *gds, int print_GDS, int print_GDS10, int verbose) {
    int i, j;

    j = GDS_LEN(gds);
    if (verbose < 2) {
        if (print_GDS && verbose < 2) {
            printf(":GDS=");
            for (i = 0; i < j; i++) {
                printf("%2.2x", (int) gds[i]);
            }
        }
        if (print_GDS10 && verbose < 2) {
            printf(":GDS10=");
            for (i = 0; i < j; i++) {
                printf(" %d", (int) gds[i]);
            }
        }
    }
    else {
        if (print_GDS) {
            printf("  GDS(1..%d)=",j);
            for (i = 0; i < j; i++) {
                if (i % 20 == 0) printf("\n    %4d:",i+1);
                printf(" %3.2x", (int) gds[i]);
            }
            printf("\n");
        }
        if (print_GDS10) {
            printf("  GDS10(1..%d)=",j);
            for (i = 0; i < j; i++) {
                if (i % 20 == 0) printf("\n    %4d:",i+1);
                printf(" %3d", (int) gds[i]);
            }
            printf("\n");
        }
    }
}
