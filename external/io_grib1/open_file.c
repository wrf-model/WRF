#include <stdio.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define OPEN_FILE open_file
#      define CLOSE_FILE close_file
#      define FLUSH_FILE flush_file
# else
#   ifdef F2CSTYLE
#      define OPEN_FILE open_file__
#      define CLOSE_FILE close_file__
#      define FLUSH_FILE flush_file__
#   else
#      define OPEN_FILE open_file_
#      define CLOSE_FILE close_file_
#      define FLUSH_FILE flush_file_
#   endif
# endif
#endif

/* 
 * Fortran-callable function to open/close files
 */
int open_file_(char *filename, int *outfd, int *ierr, int strlen) 
{
  char filename2[1000];
  strncpy(filename2,filename,strlen);
  filename2[strlen]='\0';
  *outfd = creat(filename2,0644);
  if (*outfd == -1) 
    {
      fprintf(stderr,"setting ierr to -1, filename: %s\n",filename);
      perror("");
      *ierr = -1;
      return -1;
    }
  else
    {
      *ierr = 0;
      return 0;
    }
}

int close_file_(int *fd)
{
  close(*fd);
  return 0;
}

int flush_file_(int *fd)
{
  fsync(*fd);
  return 0;
}

