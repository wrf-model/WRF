/*  FILE: cio.c  */
/*  C functions to write bytes to UNIX files - called from FORTRAN */
/*  c_open
    bn_read
    bnwrit
    c_close */
/*  bsrfil */
/*  870417  */

#if defined(CRAY)

#define c_open C_OPEN
#define c_close C_CLOSE
#define bn_read BN_READ
#define bn_seek BN_SEEK

#endif

/* length of the char string from the fortran file is 132, plus one for null terminator */
#define FORT_FILE_LEN 133

#ifdef _UNDERSCORE

#define c_open c_open_
#define c_close c_close_
#define bn_read bn_read_
#define bn_seek bn_seek_

#endif

#ifdef _DOUBLEUNDERSCORE

#define c_open c_open__
#define c_close c_close__
#define bn_read bn_read__
#define bn_seek bn_seek__

#endif

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#ifndef _WIN32
# include <unistd.h>
# include <sys/ioctl.h>
# include <sys/uio.h>
#endif

/* ****************************************************************** */

c_open(unit, nunit, name, mode, err, oflag)
 /*
  * unit  = Fortran unit number 
  * nunit = UNIX file descriptor associated with 'unit'
  * name  = UNIX file name 
  * mode  = 0 : write only - file will be created if it doesn't exist, 
                           - otherwise will be rewritten
          = 1 : read only
          = 2 : read/write
  * err   = 0 : no error opening file.
         != 0 : Error opening file
  * oflag =  0 : no notification if file opened OK (errors are printed)
          =  1 : file name and unit number printed (and errors)
          = -1 : no print at all (not even errors)
  */
    int             *unit;
    int             *nunit;
    int             *mode;
    int             *err;
    int             *oflag;
    char            name[FORT_FILE_LEN];
{
    int             fd, i;
    char            fname[FORT_FILE_LEN];
    extern int      errno;	/* I/O error return */

    if (*oflag >= 1)
	printf("Copen: File = %s\nFortran Unit = %d\n", name, *unit);

    /* strip trailing blanks and add null character to name */
    for (i = 0; name[i] != ' ' && name[i] != '\0' && i < FORT_FILE_LEN; ++i)
	fname[i] = name[i];
    fname[i] = '\0';

/* if (*mode == 0)    WRITE ONLY
   printf ("UNIX File descriptor: %d\n", fd = open (fname, O_WRONLY));
     printf ("UNIX File descriptor: %d\n", fd = creat (fname, 0777));
   else if (*mode == 1)   READ ONLY
     printf ("UNIX File descriptor: %d\n", fd = open (fname, O_RDONLY));
   else   READ/WRITE
     printf ("UNIX File descriptor: %d\n", fd = open (fname, O_RDWR));*/

    if (*mode == 0)		/* WRITE ONLY */
	fd = creat(fname, 0777);
    else if (*mode == 1)	/* READ ONLY */
	fd = open(fname, O_RDONLY);
    else			/* READ/WRITE */
	fd = open(fname, O_RDWR);
    if (*oflag >= 1)
	printf("UNIX File descriptor: %d\n\n", fd);

    *err = 0;
    if (fd == -1) {		/* error opening file */
      if (*oflag >= 0){
	printf("Error opening %s  Error status: %d\n", fname, errno);
	perror("c_open.c");
      };
      *err = errno;
    };

    *nunit = fd;
    return (0);
}

/* ****************************************************************** */
bn_seek(fd, bread, mode, iprint)

/*  Move the read/write file pointer
       fd     : Unix file descriptor.
       bread  : Number of bytes to move the pointer.
       mode   : How to move the pointer:
               = 0 : move the pointer ahead BREAD bytes.
               < 0 : move the pointer to location BREAD.
               > 0 : move the pointer to the end + BREAD bytes. (?)
       iprint : Flag to turn on (iprint = 1)  or off (iprint = 0) print.

   Location 0 [bn_seek(fd,0,-1,0)] puts us just before the first byte, 
   so the next bn_read will get byte 1.
*/

    int            *fd, *bread, *mode, *iprint;

{
    off_t           i, offset;
    int             how_to_space;

    if (*mode == 0)
	how_to_space = SEEK_CUR;
    else if (*mode < 0)
	how_to_space = SEEK_SET;
    else
	how_to_space = SEEK_END;

    offset = *bread;
    i = lseek(*fd, offset, how_to_space);
    if (*iprint != 0) 
       printf(" lseek return=%d, *mode=%d\n", i, *mode);

    return(0);
}

/* ****************************************************************** */

bn_read(fd, buf, nbuf, bread, ios, idiag)
 /*
  * fd = UNIX file descriptor number (NOT a Fortran unit) 
  * buf = area into which to read 
  * nbuf = number of bytes to read from fd 
  * bread = number actually read 
  * ios = error number returned to Fortran: 
          1 = End of File
          2 = Error in reading
  * idiag : if non-zero, error and EOF messages will be printed
  */

    int            *fd, *nbuf, buf[], *bread, *ios, *idiag;
{
    int             bytesread;

/*  printf ("BNREAD Fd = %d Nbuf = %d\n", *fd, *nbuf);  */
    bytesread = read(*fd, buf, *nbuf);
/*  printf ("Bytes %d   stat %d\n", bytesread, errno);  */

    if (bytesread == -1) {	/* error reading file */
	if (*idiag != 0)
	    printf("Error reading C unit %d\n", *fd);
	perror("bn_read.c");
	*ios = 2;
	/*  *ios = errno; */
    } else if (bytesread == 0) {/* end-of-file on input */
	if (*idiag != 0)
	    printf("End of file on C unit %d\n", *fd);
            *ios = 1; 
	/*  *ios = errno; */
    } else {			/* read OK */

	/*
	 * printf ("BNREAD - bytes read = %d   Buf = %d %d %d\n", bytesread,
	 * buf[0], buf[1], buf[2]);
	 */ 
	*ios = 0;
    };

    *bread = bytesread;
    return(0);
}

/* ****************************************************************** */

bnwrit_(fd, buf, nbuf, bwritten, err, idiag)
    int            *fd, *nbuf, buf[], *bwritten, *err, *idiag;

 /*
  * fd = UNIX file descriptor number (NOT a Fortran unit) buf = area from
  * which to write nbuf = number of bytes to write to fd bwritten = number
  * actually written err = UNIX error number returned to FORTRAN idiag : if
  * non-zero, error and EOF messages will be printed
  */

{
    int             byteswritten;

    /*
     * printf ("BNWRIT Fd = %d Nbuf = %d   Buf = %d %d %d\n", fd, *nbuf,
     * buf[0], buf[1], buf[2]);
     */
    byteswritten = write(*fd, buf, *nbuf);
    /* printf ("Bytes %d   stat %d\n", byteswritten, errno);  */

    *err = 0;
    if (byteswritten == -1) {	/* error writing file */
	if (*idiag != 0)
	    printf("Error writing C unit %d\n", *fd);
	perror("bnwrit.c");
	*err = errno;
    };

    *bwritten = byteswritten;
    return(0);
}

/* ****************************************************************** */

c_close(nunit, iprint, err)
/*
Close a C (UNIX?) file descriptor:
  nunit  : (INPUT)  : The C (UNIX?) file descriptor to close.
  iprint : (INPUT)  : Print flag ( iprint == 0 : no print on successful close)
                                 ( iprint != 0 : Some printout)
  err    : (OUTPUT) : Error flag ( err = 0 : Successful close)
                                 ( err = 1 : Error on close)
     */
    int            *nunit, *iprint, *err;
{
    extern int      errno;	/* I/O error return */
    int             istat;

    if ( *iprint != 0 )
      printf("\n *** CCLOSE : Closing file descriptor: NUNIT = %d \n", 
	     *nunit);

    istat = close(*nunit);
    if (istat == 0) {
      if ( *iprint != 0 )
	printf(" *** CCLOSE successful: File descriptor: NUNIT = %d \n", 
	       *nunit);
    }
    else
      printf("CCLOSE error: %d : File descriptor NUNIT = %d \n", 
	     istat, *nunit);

    *err = istat;
    return(0);
}
