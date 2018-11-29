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
#include <string.h>
#ifdef _BZIP_YES
#include <bzlib.h> 
#endif

#if defined (CRAY)

#define c_gribopen C_GRIBOPEN
#define c_close C_CLOSE
#define c_writeopen C_WRITEOPEN
#define io_fread IO_FREAD
#define io_fwrite IO_FWRITE
#define io_fseek IO_FSEEK
#define io_ftell IO_FTELL

#elif defined (__sgi) || defined (__sun) || defined (__alpha) || defined (__linux)

#define c_gribopen c_gribopen_
#define c_close c_close_
#define c_writeopen c_writeopen_
#define io_fread io_fread_
#define io_fwrite io_fwrite_
#define io_fseek io_fseek_
#define io_ftell io_ftell_

#elif defined (IBM) || defined (HP)

#define c_gribopen c_gribopen
#define c_close c_close
#define c_writeopen c_writeopen
#define io_fread io_fread
#define io_fwrite io_fwrite
#define io_fseek io_fseek
#define io_ftell io_ftell

#endif


struct GribFileInfo {
  FILE *fd;
  int compression;
#ifdef _BZIP_YES
  BZFILE* b;
#endif

};

void c_gribopen(char *filename, struct GribFileInfo **GFPTR, int *ierr) {
/* 
   Purpose:
   
      Open a file for reading.

   Input:

      filename:  The (null-terminated) name of the file to open.

   Output:
 
      GFPTR:  A pointer to a structure containing various file metadata,
              including the all important FILE pointer.

      ierr:   An error status flag.  ierr == 0 -- File opened successfully.
                                     ierr == 1 -- Unsuccessful attempt to open the file
                                                  (reason for failure undetermined).

   Side effects:

      The file is opened, and the file stream pointer is positioned at
      the beginning of the file.  A determination is made (based on
      file suffix) on whether the file is bzip2-compressed or not.  If
      the file is bzip2-compressed, the file is opened for BZIP2
      sequential reading.

*/


  struct GribFileInfo *lg;
  
  /* Allocate space for my new GribFileInfo structure */
  lg = (struct GribFileInfo*) calloc(1,sizeof(struct GribFileInfo));

  /* Open the file, setting the FILE pointer in my new GribFileInfo structure */
  lg->fd = fopen(filename, "r");
  if (! lg->fd) {
    *ierr = 1;
  }
  else {
    *ierr = 0;
  }

#ifdef _BZIP_YES
  /* If the file ends in ".bz2", assume it is a bzip2-compressed file, and */
  /* initialize things for reading a bzip2-compressed file.                */
  
  if (strstr(filename,".bz2\0")) {
    int bzerror;
    int verbosity = 1;
    int small = 0;
    /*  printf("BZIP2 compressed file.\n"); */
    lg->b = BZ2_bzReadOpen( &bzerror, lg->fd, verbosity, small, NULL, 0 );
    if ( bzerror != BZ_OK ) {
      BZ2_bzReadClose ( &bzerror, lg->b );
      printf ("Problem BZopen.\n");
      exit (1);
    }
    lg->compression = 2;
  }
  else {
    lg->compression = 0;
  }
#else
  lg->compression = 0;
#endif

  /*  printf("c_gribopen:  lg->fd = %lu\n", lg->fd); */
  *GFPTR = lg;
}

void c_writeopen(char *filename, struct GribFileInfo **GFPTR) {
  struct GribFileInfo *lg;
  lg = (struct GribFileInfo*) calloc(1,sizeof(struct GribFileInfo));
  lg->fd = fopen(filename, "w");
}

void c_close(struct GribFileInfo **GFPTR) {
  int bzerror;
  int ierr;
  switch ((*GFPTR)->compression) {
  default:
    printf("Unrecognized compression:  %i\n",(*GFPTR)->compression);
    exit (1);
#ifdef _BZIP_YES
  case 2:
    BZ2_bzReadClose ( &bzerror, (*GFPTR)->b );
    if ( bzerror != BZ_OK ) {
      printf ("Problem with call to BZ2_bz_ReadClose.\n");
      exit (1);
    }
    fclose((*GFPTR)->fd);
    free((*GFPTR));
    break;
#endif
  case 0:
    ierr = fclose((*GFPTR)->fd);
    if (ierr) {
      printf("close error:  %i\n", ierr);
      exit (1);
    }
    free((*GFPTR));
    break;
  }
}

void io_fread(struct GribFileInfo **GFPTR, char *buf, int *nread, int *iread, int *ierr) {
  int read_return;
  int bzerror;
  /*  printf("io_fread:  GFPTR->fd = %lu\n", (*GFPTR)->fd); */
  
  switch ((*GFPTR)->compression) {
  default:
    printf("Unrecognized compression:  %i\n",(*GFPTR)->compression);
    exit (1);
#ifdef _BZIP_YES
  case 2:
    read_return = BZ2_bzRead ( &bzerror, (*GFPTR)->b, buf, *nread );
    break;
#endif
  case 0:
    read_return = fread(buf, 1, *nread, (*GFPTR)->fd);
    break;
  }
  if (read_return != *nread) {
    *iread = read_return;
    *ierr = 1;
  }
  else {
    *ierr = 0;
    *iread = read_return;
  }
  return;
}

void io_fwrite(struct GribFileInfo **GFPTR, char *buf, int *nwrite, int *ierr) {

  int write_return;
  write_return = fwrite(buf, 1, *nwrite, (*GFPTR)->fd);
  if (write_return != *nwrite) {
    *ierr = 1;
  }
  else {
    *ierr = 0;
  }
}

void io_fseek(struct GribFileInfo **GFPTR, int *nbytes, int *mode) {
  int iseek;
  printf("No fseek!\n");
  exit (1);
  switch (*mode) {
  case 0:
    iseek = fseek((*GFPTR)->fd, *nbytes, SEEK_CUR);
    break;
  case 1:
    iseek = fseek((*GFPTR)->fd, *nbytes, SEEK_SET);
    break;
  case 2:
    iseek = fseek((*GFPTR)->fd, *nbytes, SEEK_END);
    break;
  default:
    printf("io_fseek_:  Unrecognized mode:  %i\n", *mode);
    exit (1);
  }

  if (iseek != 0) {
    printf("io_fseek_:  Seek problem.\n");
    exit (1);
  }

}

void io_ftell(struct GribFileInfo **GFPTR, long *itell) {
  *itell = ftell((*GFPTR)->fd);
  if (itell < 0) {
    printf("io_ftell_:  Problem from ftell.\n");
    exit (1);
  }
}
