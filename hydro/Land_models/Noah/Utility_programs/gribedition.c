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

  

int main(int argc, char *argv[])
{ /* main */
  int fd;            /* Integer referring to input file. */
  char *input_file;  /* Input file name. */
  unsigned char edition;
  unsigned char hh[4];

  /* Check that the input file is provided as a command-line argument*/
  input_file = argv[1];
  if (input_file == NULL){
    fprintf(stderr, "\nInput file <infile> is a required argument.\n");
    printf("%i\n",-1);
    exit (1);
  }

  /*   Open the input file for reading   */
  fd = open(input_file, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "\n ***** Problem opening input file.  Error exit. *****\n");
    fprintf(stderr, "       File name: '%s'\n\n", input_file);
    printf("-2\n");
    exit (2);
  }

  /* Scan forward for a string "GRIB" */
  while ( strncmp(hh, "GRIB", 4) != 0 ) {
    if (read(fd, hh, 4) != 4) goto shutdown; 
    lseek(fd, -3, SEEK_CUR);
  }
  lseek(fd, 3, SEEK_CUR);

  /*   We've found the beginning of a GRIB record, now read four    */
  /*   more bytes and check the GRIB edition number.  GRIB Editions */
  /*   1 and 2 are acceptable.                                      */
    
  if (read(fd, hh, 4) != 4) goto shutdown;
  edition = hh[3];

  switch (edition) {
  case 1:
  case 2:
    printf("%i\n",edition);
    close(fd);
    exit (0);
  default:
    fprintf(stderr,"\n ***** Edition number unsupported:  %i\n", edition);
    printf("%i\n",-3);
    close(fd);
    exit(1);
  }
    

 shutdown:
  fprintf(stderr, "\n ***** Problem reading input file.  Error exit. *****\n");
  fprintf(stderr, "       File name: '%s'\n\n", input_file);
  close(fd);
  printf("%i\n",-4);
  exit (3);
  
}

