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

void process_grib1_record();
void process_grib2_record(int lengrib);
void check_and_write();

unsigned char ii[MAXGRIB];
unsigned char *sec0_ptr, *sec1_ptr, *sec2_ptr, *sec3_ptr, *sec4_ptr, *sec5_ptr, *sec6_ptr, *sec7_ptr, *sec8_ptr;
int fd;  /* Integer referring to input file. */
unsigned int sec0_len, sec1_len, sec2_len, sec3_len, sec4_len, sec5_len, sec6_len, sec7_len, sec8_len;

int secval, bytnum, bytend;

char verbose, quiet, decimal;

unsigned char edition;
  

int main(int argc, char *argv[])
{ /* main */
  int c;
  unsigned char hhh[8];
  unsigned char hh[4];
  char optstring[12] = "vqds:h";
  int lengrib;

  char *input_file;  /* Input file name. */
  char *output_file; /* Output file name. */

  char *usage = "usage:\n  %s [-help] [-v] [-q]\n"\
    "    [-s secnum,bytnum[:bytend],bytval] <infile>\n\n";

  verbose = 0;
  quiet = 1;
  decimal = 0;

  while ( (c=getopt(argc, argv, optstring)) != -1 ) {
    switch (c){
    case 'v':
      verbose = 1;
      quiet = 0;
      break;
    case 'q':
      quiet = 1;
      verbose = 0;
      break;
    case 'd':
      decimal = 1;
      break;
    case 's':
      {
	int i1, i2, i3;
	i1 = -9999;
	i2 = -9999;
	i3 = -9999; 

/* 	printf("optarg = %s\n", optarg); */
	
	if (strchr(optarg,':')) {
	  sscanf(optarg, "%d,%d:%d", &i1, &i2, &i3);
 	  // printf("i1 i2 i3= %i %i %i\n", i1, i2, i3);
	  secval = i1;
	  bytnum = i2;
	  bytend = i3;
	  if (bytend < bytnum) {
	    fprintf(stderr, "\n    In -s option bytend must be greater than bytnum\n");
	    fprintf(stderr, "    -s secval,bytnum[:bytend],bytval\n");
	    fprintf(stderr, "\n***** Error exit *****\n\n");
	    exit(1);
	  }
	}
	else {
	  sscanf(optarg, "%d,%d,%d", &i1, &i2);
	  secval = i1;
	  bytnum = i2;
	  bytend = bytnum;
	}
      
	if (bytnum < 1) {
	  printf("Byte number must be greater than zero.\n");
	  exit (1);
	}

	// printf("-s %i,%i,%i\n", secval, bytnum, bytend);
	break;
      }
    case 'h':
      {
	fprintf(stderr, "\n");
	fprintf(stderr, "**************************************************************************\n\n");
	fprintf(stderr, "%s -- extracts specified bytes from a GRIB-formatted file.\n", argv[0]);
	fprintf(stderr, "\n");
	fprintf(stderr, usage,argv[0]); 
	fprintf(stderr, "where:\n      infile  -- the GRIB-formatted file to read.\n");
	fprintf(stderr, "      outfile -- the GRIB-formatted file to create.\n\n");
	fprintf(stderr, "options:\n  -help  : print this help message and exit.\n\n");
	fprintf(stderr, "  -v  : more printout (verbose).\n\n");
	fprintf(stderr, "  -q  : less printout (quiet).\n\n");
	fprintf(stderr, "  -s  : extract byte at section number, byte number.\n");
	fprintf(stderr, "        A range of byte numbers may be specified.\n");
	fprintf(stderr, "**************************************************************************\n");
	exit (-1); 
      }

    default :
      fprintf(stderr, usage, argv[0]);
      exit (-1);
    }
  }

  /* Check that the input file is provided as a command-line argument*/
  input_file = argv[optind];
  if (input_file == NULL){
    fprintf(stderr, "\nInput file <infile> is a required argument.\n");
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

  while (1) {
    hh[0] = 0;
    /* Scan forward for a string "GRIB" */
    while ( strncmp(hh, "GRIB", 4) != 0 ) {
      if (read(fd, hh, 4) != 4) goto shutdown; 
      lseek(fd, -3, SEEK_CUR);
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
      break;
    default:
      fprintf(stderr,"Edition number unsupported:  %i\n", edition);
      exit(1);
    }
    
    sec1_len = 0;
    sec2_len = 0;
    sec3_len = 0;
    sec4_len = 0;
    sec5_len = 0;
    sec6_len = 0;
    sec7_len = 0;
    sec8_len = 0;

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
    }

    /* rewind back to the beginning of the GRIB record */
    lseek(fd, -sec0_len, SEEK_CUR);

    if (lengrib > MAXGRIB) {
      fprintf(stderr, "\n   ***** Increase MAXGRIB > %i\n", lengrib);
      fprintf(stderr, "   ***** MAXGRIB is currently set to %i\n\n", MAXGRIB);
      exit (-7);
    }

    /* Read the whole GRIB record. */

    read(fd, ii, lengrib);

    /* Process */

    switch (edition) {

    case 1:
      process_grib1_record();
      break;

    case 2:
      process_grib2_record(lengrib);
      break;
    }
  }

 shutdown:

  return(0);
  
}

void process_grib1_record() {

  sec0_ptr = ii;
  sec0_len = 8;

  sec1_ptr = &(ii[sec0_len]);
  sec1_len = (sec1_ptr[0]*256+sec1_ptr[1])*256+sec1_ptr[2];

  switch(sec1_ptr[7]){
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
    sec2_ptr = &(ii[sec0_len+sec1_len]);
    sec2_len = (sec2_ptr[0]*256+sec2_ptr[1])*256+sec2_ptr[2];
    break;
  }
  // if (sec2_ptr) printf("sec2_start = %i;   sec2_len = %i\n", sec2_ptr-ii, sec2_len);

  switch(sec1_ptr[7]){
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
    sec3_ptr = &(ii[sec0_len+sec1_len+sec2_len]);
    sec3_len = (sec3_ptr[0]*256+sec3_ptr[1])*256+sec3_ptr[2];
    break;
  }
  // if (sec3_ptr) printf("sec3_start = %i;   sec3_len = %i\n", sec3_ptr-ii, sec3_len);

  sec4_ptr = &(ii[sec0_len+sec1_len+sec2_len+sec3_len]);
  sec4_len = (sec4_ptr[0]*256+sec4_ptr[1])*256+sec4_ptr[2];

  sec5_ptr = &(ii[sec0_len+sec1_len+sec2_len+sec3_len+sec4_len]);
  sec5_len = 4;

  // printf("sec4_start = %i;   sec4_len = %i\n", sec4_ptr-ii, sec4_len);

  check_and_write();

}

void process_grib2_record (int lengrib) {
  unsigned int secsize;
  unsigned char *ipt;

  /* Scan forward to find the beginning of various GRIB sections */
  ipt = ii;
  ipt += sec0_len;
  sec0_ptr = ii;

  while (ipt - ii < lengrib) {
    if ((ipt[0] == '7') && (ipt[1] == '7') && (ipt[2] == '7') && (ipt[3] == '7')) {
      sec8_ptr = ipt;
      sec8_len = 4;
      if (secval==8) check_and_write();
      return;
    }
    switch (ipt[4]) { /* ipt[4] encodes the section identifier (1-7) */
    default:
      fprintf(stderr,"This should not happen (3)");
      exit(1);
    case 1:
      sec1_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec1_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 2:
      sec2_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec2_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 3:
      sec3_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec3_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 4:
      sec4_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec4_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 5:
      sec5_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec5_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 6:
      sec6_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec6_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    case 7:
      sec7_ptr = &(ii[ipt-ii]);
      secsize = ((ipt[0]*256+ipt[1])*256+ipt[2])*256+ipt[3];
      sec7_len = secsize;
      if (secval==ipt[4]) check_and_write();
      break;
    }
    ipt += secsize;
  }
}

void check_and_write() {
  unsigned char *ipt;
  int m, foundval;

  /* Extract specified bytes */
  switch (secval) {
  case 0:
    ipt = ii;
    if (bytend > sec0_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 1:
    ipt = sec1_ptr;
    if (bytend > sec1_len) {
      fprintf(stderr, "Byte request (%i) beyond section length (%i)\n", bytend, sec1_len);
      exit (1);
    }
    break;
  case 2:
    ipt = sec2_ptr;
    if (bytend > sec2_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 3:
    ipt = sec3_ptr;
    if (bytend > sec3_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 4:
    ipt = sec4_ptr;
    if (bytend > sec4_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 5:
    ipt = sec5_ptr;
    if (bytend > sec5_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 6:
    ipt = sec6_ptr;
    if (bytend > sec6_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 7:
    ipt = sec7_ptr;
    if (bytend > sec7_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  case 8:
    ipt = sec8_ptr;
    if (bytend > sec8_len) {
      fprintf(stderr, "Byte request beyond section length\n");
      exit (1);
    }
    break;
  default:
    fprintf(stderr, "Problem in specific byte:  Section %i\n", secval);
    exit (1);
  }

  /* Extract bytes */
  if ( ! quiet ) {
    printf("Section %i starts at %i\n", secval, ipt-ii+1);

    foundval = 0;
    for (m = bytnum; m<=bytend; m++) {
      fprintf(stderr, "Byte number %i:  %i:  ", &(ipt[m]) - ii, ipt[m-1]);
    }
  }


  if ( !decimal ) {
    for (m = bytnum; m<=bytend; m++) {
      printf("%i ", ipt[m-1]);
    }
    printf("\n");
  }
  else {
    foundval = 0;
    for (m = bytnum; m<=bytend; m++) {
      foundval = (foundval*256) + ipt[m-1];
    }
    printf ("%i\n", foundval);
  }

}
