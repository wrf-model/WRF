/*
  Program Name:
  Author(s)/Contact(s):
  Abstract:
  History Log:
 
  Usage:
  Parameters: <Specify typical arguments passed>
  Input Files:
        <list file names and briefly describe the data they include>
  Output Files:
        <list file names and briefly describe the information they include>
 
  Condition codes:
        <list exit condition or error codes returned >
        If appropriate, descriptive troubleshooting instructions or
        likely causes for failures could be mentioned here with the
        appropriate error code
 
  User controllable options: <if applicable>
*/


/*                                                          */
/* Extracts specified GRIB records from a GRIB file.        */
/*                                                          */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

#define MIN(A,B) ((A)<(B) ? A : B)

#define MAXGRIB 2000000

void process_grib1_record(int fw, int lengrib);
void process_grib2_record(int fw, int lengrib, unsigned char discipline);
void check_and_write(int fw, int lengrib);

unsigned char ii[MAXGRIB];
unsigned char *sec1_ptr, *sec2_ptr, *sec3_ptr, *sec4_ptr, *sec5_ptr, *sec6_ptr, *sec7_ptr;
int fd;  /* Integer referring to input file. */
unsigned int sec0_len, sec1_len, sec2_len, sec3_len, sec4_len, sec5_len, sec6_len, sec7_len;
unsigned char itri;
unsigned int iparm;
double level1_xval, level2_xval;
int output_count, record_count;

int codeval[100], levtyp[100];
double levval1[100], levval2[100];
int secval[100], bytnum[100], bytend[100], bytval[100];

int ccount, lcount, scount;
char verbose, quiet, force;

unsigned char imonth, iday, ihour, iminute;
unsigned int iyear;
unsigned char edition;
unsigned char level1_type;
unsigned char level2_type;
  

int main(int argc, char *argv[])
{ /* main */
  int c;
  unsigned char hhh[8];
  unsigned char hh[4];
  char optstring[12] = "vqfg:c:s:l:h";
  int lengrib;
  int expected_edition;
  unsigned char discipline;

  char *input_file;  /* Input file name. */
  char *output_file; /* Output file name. */

  int fw;  /* Integer referring to output file. */
  off_t offset;

  char *usage = "usage:\n  %s [-help | -h] [-v] [-q] [-f]\n"\
    "    [-c <codeval> | -c <discipline>,<category>,<parameter>]\n"\
    "    [-l <levtyp>,<levval11>,<levval2>]\n"\
    "    [-s <secnum>,<bytnum>[:<bytend>],<bytval>] <infile> <outfile>\n\n";

  lcount = 0;
  ccount = 0;
  scount = 0;

  output_count = 0;
  record_count = 0;
  verbose = 0;
  quiet = 0;
  force = 0;

  while ( (c=getopt(argc, argv, optstring)) != -1 ) {
    switch (c){
    case 'v':
      verbose = 1;
      break;
    case 'q':
      quiet = 1;
      break;
    case 'f':
      force = 1;
      break;
    case 'c':
      {
	int i1, i2, i3;
	int nread;
	i1 = -9999;
	i2 = -9999;
	i3 = -9999; 

	nread = sscanf(optarg, "%d,%d,%d", &i1, &i2, &i3);

	switch (nread) {
	default:
	  fprintf(stderr, usage,argv[0]); 
	  exit (1);
	case 1:
	  expected_edition = 1;
	  codeval[ccount] = i1;
	  break;
	case 3:
	  expected_edition = 2;
	  codeval[ccount] = i1*1000000+i2*1000+i3;
	  break;
	}
	  
	ccount++;
	break;
      }
    case 'l':
      {
	int i1;
	float f2, f3;
	int nread;

 	i1 = -9999;
	f2 = -9999;
	f3 = -9999;
	// printf("optarg = '%s'\n", optarg);
	nread = sscanf(optarg, "%d,%f,%f", &i1, &f2, &f3);
	// printf("nread = %i\n", nread);
	// printf("i1, f2, f3 = %i %f %f\n", i1, f2, f3);
	levtyp[lcount] = i1;
	levval1[lcount] = f2;
	levval2[lcount] = f3;
      
	// printf("-l %i,%f,%f\n", levtyp[lcount], levval1[lcount], levval2[lcount]);
	lcount++;
	break;
      }

    case 's':
      {
	int i1, i2, i3, i4;
	i1 = -9999;
	i2 = -9999;
	i3 = -9999; 
	i4 = -9999; 

/* 	printf("optarg = %s\n", optarg); */
	
	if (strchr(optarg,':')) {
	  sscanf(optarg, "%d,%d:%d,%d", &i1, &i2, &i3, &i4);
 	  // printf("i1 i2 i3 i4 = %i %i %i %i\n", i1, i2, i3, i4);
	  secval[scount] = i1;
	  bytnum[scount] = i2;
	  bytend[scount] = i3;
	  bytval[scount] = i4;
	  if (bytval[scount] < 0) {
	    double pfac;
	    int imv;
	    pfac = ((bytend[scount]-bytnum[scount]+1)*8-1);
	    imv = pow(2.,pfac);
	    bytval[scount] = imv - bytval[scount];
	  }
	  if (bytend[scount] < bytnum[scount]) {
	    fprintf(stderr, "\n    In -s option bytend must be greater than bytnum\n");
	    fprintf(stderr, "    -s secval,bytnum[:bytend],bytval\n");
	    fprintf(stderr, "\n***** Error exit *****\n\n");
	    exit(1);
	  }
	}
	else {
	  sscanf(optarg, "%d,%d,%d,%d", &i1, &i2, &i3);
	  secval[scount] = i1;
	  bytnum[scount] = i2;
	  bytval[scount] = i3;
	  bytend[scount] = bytnum[scount];
	}
      
	// printf("-s %i,%i,%i,%i\n", secval[scount], bytnum[scount], bytval[scount], bytend[scount]);
	scount++;
	break;
      }
    case 'h':
      {
	fprintf(stderr, "\n");
	fprintf(stderr, "**************************************************************************\n\n");
	fprintf(stderr, "%s\n           -- extracts specified fields from a GRIB (Edition 1 or Edition 2) file.\n", argv[0]);
	fprintf(stderr, "\n");
	fprintf(stderr, usage,argv[0]); 
	fprintf(stderr, "where:\n      <infile>  -- the GRIB-formatted file to read.\n");
	fprintf(stderr, "      <outfile> -- the GRIB-formatted file to create.\n\n");
	fprintf(stderr, "options:\n  -help  : print this help message and exit.\n\n");
	fprintf(stderr, "  -v  : more printout (verbose).\n\n");
	fprintf(stderr, "  -q  : less printout (quiet).\n\n");
	fprintf(stderr, "  -f  : overwrite an existing output file without asking (force).\n\n");
	fprintf(stderr, "  -c  : extract fields as specified by a comma-separated list of integers,\n");
	fprintf(stderr, "        where:\n");
	fprintf(stderr, "           <codeval> is the GRIB Editon 1 parameter code.\n");
	fprintf(stderr, "        or:\n");
	fprintf(stderr, "           <discipline> is the GRIB Edition 2 product discipline.\n");
	fprintf(stderr, "           <category> is the GRIB Edition 2 parameter category.\n");
	fprintf(stderr, "           <parameter> is the GRIB Edition 2 parameter number.\n");
	fprintf(stderr, "  -l  : extract levels as specified by a comma-separated list of integers,\n");
	fprintf(stderr, "        where:\n");
	fprintf(stderr, "           <levtyp> is the GRIB level-type code.\n");
	fprintf(stderr, "           <levval1> is the level value.\n");
	fprintf(stderr, "           <levval2> is a second level value (if level is defined by two values).\n\n");
	fprintf(stderr, "                    **** NOTE **** GRIB Edition 1 defines pressure levels in mb.\n");
	fprintf(stderr, "                                   GRIB Edition 2 defines pressure levels in Pa.\n");
	fprintf(stderr, "  -s  : extract only fields that match section number, byte number, and byte value.\n");
	fprintf(stderr, "        Multiple -s flags may be specified.  A range of byte numbers may be specified.\n");
	fprintf(stderr, "        where:\n");
	fprintf(stderr, "           <secnum> is the GRIB section number to look in.\n");
	fprintf(stderr, "           <bytnum> is the byte number in that GRIB section (or the beginning\n");
	fprintf(stderr, "                    of a range of bytes) to look at.\n");
	fprintf(stderr, "           <bytend> is the end byte number of a range of bytes to look at.\n");
	fprintf(stderr, "           <bytval> is the value to match for the specified byte or byte range.\n");
	fprintf(stderr, "\n\nFor example:  \n\n");
	fprintf(stderr, "      '%s -c 0,2,3'\n            will extract all V fields from a GRIB Edition2 file.\n\n", argv[0]);
	fprintf(stderr, "      '%s -c 11 -c 33'\n            will extract all T and U fields from a GRIB Edition 1 file.\n\n",
		argv[0]);
	fprintf(stderr, "      '%s -c 11 -l 100'\n            will extract all P-level T fields from a GRIB Edition 1 file.\n\n",
		argv[0]);
	fprintf(stderr, "      '%s -l 100,500'\n            will extract all 500 mb fields from a GRIB Edition 1 file,\n",
		argv[0]);
	fprintf(stderr, "                 and all 500 Pa (5 mb) fields from a GRIB Edition 2 file.\n\n");
	fprintf(stderr, "**************************************************************************\n");
	exit (-1); 
      }

    default :
      fprintf(stderr, usage, argv[0]);
      exit (-1);
    }
  }

  if (ccount == 0) {
    codeval[ccount] = -9999;
/*     levtyp[ccount] = -9999; */
/*     levval1[ccount] = -9999; */
/*     levval2[ccount] = -9999; */
  }

  if (scount == 0) {
    secval[scount] = -9999;
    bytnum[scount] = -9999;
    bytend[scount] = -9999;
    bytval[scount] = -9999;
  }

  /* Check that the input file is provided as a command-line argument*/
  input_file = argv[optind];
  if (input_file == NULL){
    fprintf(stderr, "\nInput file <infile> is a required argument.\n");
    fprintf(stderr, usage, argv[0]);
    exit (9);
  }

  /* Check that the output file is provided as a command-line argument*/
  output_file = argv[optind+1];
  if (output_file == NULL) {
    fprintf(stderr, "\nOutput file <outfile> is a required argument.\n");
    fprintf(stderr, usage, argv[0]);
    exit (9);
  }

  /*   Open the input file for reading   */
  if ( ! ( quiet ) ) {
    printf("\nInput file = %s\n", input_file);
  }
  fd = open(argv[optind], O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "\n ***** Problem opening input file.  abort *****\n");
    fprintf(stderr, "       File name: '%s'\n\n", argv[optind]);
    exit(-4);
  }

  if ( ! force ) {

    /*   Check whether the output file already exists.       */
    /*   If so, query the user before overwriting that file. */

    struct stat fbuf;

    if (stat(output_file, &fbuf) == 0) {
      char ans;
      fprintf(stderr, "\aOutput file '%s' already exists.  Overwrite? (n) > ", output_file);
      ans = getchar();
      fprintf (stderr, "\n");
      if ((ans != 'y') && (ans != 'Y')) {
	fprintf(stderr, " ***** OK.  Not overwriting file '%s'.  Exit. *****\n\n", output_file);
	exit (1);
      }
    }
  }

  /* Open the output file for writing.*/
  if ( ! ( quiet ) ) {
    printf("Output file = %s\n", output_file);
  }
  fw = creat(output_file, 0644);

  while (1) {
    hh[0] = 0;
    /* Scan forward for a string "GRIB" */
    while ( strncmp(hh, "GRIB", 4) != 0 ) {
      if (read(fd, hh, 4) != 4) goto shutdown; 
      offset = -(off_t)3;
      lseek(fd, offset, SEEK_CUR);
    }
    lseek(fd, 3, SEEK_CUR);

    /*   We've found the beginning of a GRIB record, now check    */
    /*   the GRIB edition number.  GRIB Editions 1 and 2 are      */
    /*   acceptable.                                              */
    
    read(fd, hh, 4);
    edition = hh[3];

    switch (edition) {
    case 1:
      sec0_len = 8;
      break;
    case 2:
      sec0_len = 16;
      discipline = hh[2];
      break;
    default:
      fprintf(stderr,"Edition number unsupported:  %i\n", edition);
      exit(1);
    }
    
    sec2_len = 0;
    sec3_len = 0;
    sec4_len = 0;
    sec5_len = 0;
    sec6_len = 0;
    sec7_len = 0;

    /*   We've found the beginning of a GRIB record, and we know  */
    /*   The GRIB edition number.  Now find the size of the whole */                     
    /*   GRIB record.                                             */

    switch (edition) {
    case 1:
      lengrib = hh[0]*65536+hh[1]*256+hh[2];
      break;
    case 2:
      read(fd, hhh, 8);
      lengrib = ((((((hhh[0]*256+hhh[1])*256+hhh[2])*256+hhh[3])*256+hhh[4])*256+hhh[5])*256+hhh[6])*256+hhh[7];
      break;
    default:
      fprintf(stderr,"This should not happen (1)");
      exit(1);
    }

    /* rewind back to the beginning of the GRIB record */
    offset = -(off_t)(sec0_len);
    lseek(fd, offset, SEEK_CUR);

    if (lengrib > MAXGRIB) {
      fprintf(stderr, "\n   ***** Increase MAXGRIB > %i\n", lengrib);
      fprintf(stderr, "   ***** MAXGRIB is currently set to %i\n\n", MAXGRIB);
      exit (-7);
    }

    record_count++;

    /* Read the whole GRIB record. */

    read(fd, ii, lengrib);

    /* Process */

    switch (edition) {

    default:

      fprintf(stderr,"This should not happen (2)");
      exit(1);

    case 1:
      /* if (expected_edition==1) { */
	process_grib1_record(fw, lengrib);
	/* } */
      break;

    case 2:
      /* if (expected_edition == 2) { */
	process_grib2_record(fw, lengrib, discipline);
	/* } */
      break;
    }
  }

 shutdown:
  close(fw);
  if ( ! ( quiet ) ) {
    printf("\noutput_count = %i\n", output_count);
  }

  return(0);
  
}

void process_grib1_record(int fw, int lengrib) {
  unsigned char icentury;
  unsigned char gds_or_bms;
  sec1_ptr = &(ii[8]);
  sec1_len = (sec1_ptr[0]*256+sec1_ptr[1])*256+sec1_ptr[2];
  // printf("sec1_start = %i;   sec1_len = %i\n", sec1_ptr-ii, sec1_len);

  gds_or_bms = sec1_ptr[7];

  switch(gds_or_bms){
  default:
    fprintf(stderr, "Problem (1) with optional sections\n");
    exit (1);
  case 0:
  case 64:
    sec2_ptr = NULL;
    sec2_len = 0;
    break;
  case 128:
  case 192:
    sec2_ptr = &(ii[8+sec1_len]);
    sec2_len = (sec2_ptr[0]*256+sec2_ptr[1])*256+sec2_ptr[2];
    break;
  }
  // if (sec2_ptr) printf("sec2_start = %i;   sec2_len = %i\n", sec2_ptr-ii, sec2_len);

  switch(gds_or_bms){
  default:
    fprintf(stderr, "Problem (2) with optional sections\n");
    exit (1);
  case 0:
  case 128:
    sec3_ptr = NULL;
    sec3_len = 0;
    break;
  case 64:
  case 192:
    sec3_ptr = &(ii[8+sec1_len+sec2_len]);
    sec3_len = (sec3_ptr[0]*256+sec3_ptr[1])*256+sec3_ptr[2];
    break;
  }
  // if (sec3_ptr) printf("sec3_start = %i;   sec3_len = %i\n", sec3_ptr-ii, sec3_len);

  sec4_ptr = &(ii[8+sec1_len+sec2_len+sec3_len]);
  sec4_len = (sec4_ptr[0]*256+sec4_ptr[1])*256+sec4_ptr[2];

  // printf("sec4_start = %i;   sec4_len = %i\n", sec4_ptr-ii, sec4_len);

  /* Extract the century, year, month, day, hour, and minute */
  /* of reference time                                       */
  icentury = ii[32];
  iyear = ii[20] + (icentury-MIN(ii[20],1))*100;
  imonth = ii[21];
  iday = ii[22];
  ihour = ii[23];
  iminute = ii[24];

  /* Extract the parameter number */

  iparm = ii[16]; 

  /* Extract the level type */

  level1_type = ii[17];

  /* Time Range Indicator */
  itri = ii[28];

  /* Extract the level values */

  level1_xval = ii[18];
  level2_xval = ii[19];

  /* for certain types of levels, the two bytes represent a single value */
  if ( (level1_type == 100) || (level1_type == 103) || (level1_type == 105) ||
       (level1_type == 107) || (level1_type == 109) || (level1_type == 111) ||
       (level1_type == 113) || (level1_type == 115) || (level1_type == 117) || 
       (level1_type == 118) || (level1_type == 119) || (level1_type == 125) ||
       (level1_type == 160) || (level1_type == 200) || (level1_type == 201)) {

    level1_xval = level1_xval*256+level2_xval;
    level2_xval = -9999;
	
  }

  check_and_write(fw, lengrib);

}

void process_grib2_record (int fw, int lengrib, unsigned char discipline) {
  unsigned int secsize;
  unsigned int level1_factor;
  unsigned int level2_factor;
  unsigned int scaled_level1_value;
  unsigned int scaled_level2_value;
  unsigned char *ipt;
  unsigned char parameter_category;
  unsigned char parameter_number;

  /* Scan forward to find the beginning of various GRIB sections */
  ipt = ii;
  ipt += sec0_len;

  while (ipt - ii < lengrib) {
    if ((ipt[0] == '7') && (ipt[1] == '7') && (ipt[2] == '7') && (ipt[3] == '7')) {
      // printf("Hit end of record.\n");
      return;
    }
    switch (ipt[4]) { /* ipt[4] encodes the section identifier (1-7) */
    default:
      fprintf(stderr,"This should not happen (3)");
      exit(1);
    case 1:
      sec1_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec1_len = ((ii[sec0_len+0]*256+ii[sec0_len+1])*256+ii[sec0_len+2])*256+ii[sec0_len+3];
      /* Extract the century, year, month, day, hour, and minute */
      /* of reference time                                       */
      iyear   = ii[sec0_len+12]*256+ii[sec0_len+13];
      imonth  = ii[sec0_len+14];
      iday    = ii[sec0_len+15];
      ihour   = ii[sec0_len+16];
      iminute = ii[sec0_len+17];
      break;
    case 2:
      sec2_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec2_len = secsize;
      break;
    case 3:
      sec3_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec3_len = secsize;
      break;
    case 4:
      sec4_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec4_len = secsize;
      parameter_category = ipt[9];
      parameter_number   = ipt[10];
      iparm = discipline*1000000 + parameter_category*1000 + parameter_number;
      // fprintf(stderr, "parameter_category, parameter_number = %3i %3i %10i\n", parameter_category, parameter_number, iparm);
      level1_type = ipt[22];
      level1_factor = ipt[23];
      scaled_level1_value = ((ipt[24]*256+ipt[25])*256+ipt[26])*256+ipt[27];
      level2_type = ipt[28];
      level2_factor = ipt[29];
      scaled_level2_value = ((ipt[30]*256+ipt[31])*256+ipt[32])*256+ipt[33];
      level1_xval = scaled_level1_value / pow(10., level1_factor);
      level2_xval = scaled_level2_value / pow(10., level2_factor);
      break;
    case 5:
      sec5_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec5_len = secsize;
      break;
    case 6:
      sec6_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec6_len = secsize;
      break;
    case 7:
      sec7_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec7_len = secsize;
      check_and_write(fw, lengrib);
      break;
    }
    ipt += secsize;
  }
}

void check_and_write(int fw, int lengrib) {
  int output_length;
  int lskip;
  int k;
  unsigned char *ipt;
  int m, foundval;

  /* If the parameter is one we want, write it out. */
  if (ccount>0) {
    lskip = 1;
    for (k=0; k<ccount; k++) {
      if ((iparm == codeval[k]) || (codeval[k] == -9999)) {
	lskip = 0;
      }
    }
    if (lskip) {
      if ( ! quiet ) printf("Skipping (A) %i %7.7i\n", record_count, iparm);
      return;
    }
  }

  /* If the level is one we want, write it out. */
  if (lcount>0) {
    lskip = 1;
    for (k=0; k<lcount; k++) {

      if ( ( ( level1_type == levtyp[k]             ) || (levtyp[k]  < -9998)) &&
	   ( ( fabsf(level1_xval - levval1[k])<1.E-6) || (levval1[k] < -9998)) &&
	   ( ( fabsf(level2_xval - levval2[k])<1.E-6) || (levval2[k] < -9998))) {
	// printf("Match for %7.7i %i %f %f\n", iparm, level1_type, level1_xval, level2_xval);
	lskip = 0;
      }
    }
    if (lskip) {
      if ( ! quiet ) printf("Skipping (B) %i %7.7i\n", record_count, iparm);
      return;
    }
  }
  

  /* Check specified bytes */
  if (scount>0) {/* section specifications */
    lskip = 1;
    for (k=0; k<scount; k++) {
      switch (secval[k]) {
      case 0:
	ipt = ii;
	break;
      case 1:
	ipt = sec1_ptr;
	break;
      case 2:
	ipt = sec2_ptr;
	break;
      case 3:
	ipt = sec3_ptr;
	break;
      case 4:
	ipt = sec4_ptr;
	break;
      case 5:
	ipt = sec5_ptr;
	break;
      case 6:
	ipt = sec6_ptr;
	break;
      case 7:
	ipt = sec7_ptr;
	break;
      default:
	fprintf(stderr, "Problem in specific byte:  Section %i\n", secval[k]);
	exit (1);
      }

      /* Handle the range of bytes */
      foundval = 0;
      for (m = bytnum[k]; m<=bytend[k]; m++) {
	// printf("Byte number %i:  %i:  ", &(ipt[m-1]) - ii, ipt[m-1]);
	foundval = (foundval*256) + ipt[m-1];
	// printf("Foundval = %i\n", foundval);
      }
      if ( ! quiet ) {
	// printf("Section %i starts at %i\n", secval[k], ipt-ii);
	printf("test value: %i   value found: %i\n", bytval[k], foundval);
      }
      // printf("foundval, bytval[k] = %i %i\n", foundval, bytval[k]);
      if (foundval == bytval[k]) {
	lskip = 0;
      }
    }
    if (lskip) {
      if ( ! quiet ) printf("Skipping (C) %i %7.7i\n", record_count, iparm);
      return;
    }
  }

  if ( ! lskip ) {
    unsigned char sec0[16];
    int n;

    output_count++;
    switch(edition){
    case 1:
      write(fw, ii, lengrib);
      break;
    case 2:

      output_length = 16 + sec1_len + sec2_len + sec3_len + sec4_len + sec5_len + sec6_len + sec7_len + 4;
      memcpy(sec0, ii, 16);
      // Byte-swapped record length.
      sec0[15] = output_length & 0xFF;
      sec0[14] = (output_length>>8) & 0xFF;
      sec0[13] = (output_length>>16) & 0xFF;
      sec0[12] = (output_length>>24) & 0xFF;

      write(fw, sec0, 16);
      write(fw, sec1_ptr, sec1_len);
      if (sec2_len) write(fw, sec2_ptr, sec2_len);
      write(fw, sec3_ptr, sec3_len);
      write(fw, sec4_ptr, sec4_len);
      write(fw, sec5_ptr, sec5_len);
      write(fw, sec6_ptr, sec6_len);
      write(fw, sec7_ptr, sec7_len);
      write(fw, "7777", 4);
      break;
    }
    if (verbose) {
      if (level2_xval != -9999) {
	printf("%5i %7.7i %4i %f %f  %4i-%2.2i-%2.2i_%2.2i:%2.2i\n", 
	       record_count, iparm, level1_type, level1_xval, level2_xval,
	       iyear, imonth, iday, ihour, iminute);
      }
      else {
	printf("%4i %3i %4i %f  **   %4i-%2.2i-%2.2i_%2.2i:%2.2i\n", 
	       record_count, iparm, level1_type, level1_xval,
	       iyear, imonth, iday, ihour, iminute);
      }
    }
  }
  else {
    if ( ! quiet ) printf("Skipping (D) %i %7.7i\n", record_count, iparm);
  }

}
