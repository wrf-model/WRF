#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _UNDERSCORE
#define cio_set_log_filename cio_set_log_filename_
#define cio_prints cio_prints_
#define cio_printf cio_printf_
#define cio_printi cio_printi_
#endif
#ifdef _DOUBLEUNDERSCORE
#define cio_set_log_filename cio_set_log_filename__
#define cio_prints cio_prints__
#define cio_printf cio_printf__
#define cio_printi cio_printi__
#endif

char * logfilename = 0;
FILE * cio_out     = 0;

void cio_set_log_filename(char * s, int * n)
{
   /* Allow changes to the log filename so long as
    *   we haven't actually opened the file (and written
    *   to it).
    */
   if (!cio_out && logfilename)
   {
      free(logfilename);
      logfilename = 0;
   }

   if (!logfilename)
   {
      logfilename = (char *)malloc(*n+1);  
      strncpy(logfilename, s, *n);
      logfilename[*n] = '\0';
   }
}

void cio_printf(int * fd, float * f)
{
   if (!logfilename) return;

   if (*fd != 0) 
   {
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%f", *f);
      fflush(cio_out);
   }
   else
   {
      fprintf(stdout, "%f", *f);
      fflush(stdout);
   }
}

void cio_printi(int * fd, int * i)
{
   if (!logfilename) return;

   if (*fd != 0) 
   {
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%i", *i);
      fflush(cio_out);
   }
   else
   {
      fprintf(stdout, "%i", *i);
      fflush(stdout);
   }
}

void cio_prints(int * fd, char * s, int * n)
{
   if (!logfilename) return;

   if (*fd != 0) 
   {
      s[*n] = '\0';
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%s", s);
      fflush(cio_out);
   }
   else
   {
      s[*n] = '\0';
      fprintf(stdout, "%s", s);
      fflush(stdout);
   }
}
