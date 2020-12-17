/******************************************************************************

  KPP - The Kinetic PreProcessor
        Builds simulation code for chemical kinetic systems

  Copyright (C) 1995-1996 Valeriu Damian and Adrian Sandu
  Copyright (C) 1997-2005 Adrian Sandu

  KPP is free software; you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation (http://www.gnu.org/copyleft/gpl.html); either version 2 of the
  License, or (at your option) any later version.

  KPP is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, consult http://www.gnu.org/copyleft/gpl.html or
  write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307,  USA.

  Adrian Sandu
  Computer Science Department
  Virginia Polytechnic Institute and State University
  Blacksburg, VA 24060
  E-mail: sandu@cs.vt.edu

******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#if 0
#include <malloc.h>
#endif
#include <unistd.h>
#include <string.h>
#include "gdata.h"
#include "scan.h"

#define MAX_BUFFER 200

void ScanError( char *fmt, ... )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Error :%s:%d: %s\n", crt_filename, crt_line_no, buf );
  nError++;
}

void ParserError( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Error :%s:%d: %s\n", crtFile, crtLine, buf );
  nError++;
}

void ScanWarning( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Warning :%s:%d: %s\n", crt_filename, crt_line_no, buf );
  nWarning++;
}

void ParserWarning( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Warning :%s:%d: %s\n", crtFile, crtLine, buf );
  nWarning++;
}

void Error( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Error : %s\n", buf );
  nError++;
}

void Warning( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "Warning : %s\n", buf );
  nWarning++;
}

void Message( char *fmt, ...  )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "    Message :%s:%d: %s\n", crt_filename, crt_line_no, buf );
}

void FatalError( int status, char *fmt, ... )
{
Va_list args;
char buf[ MAX_BUFFER ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  fprintf( stdout, "\nFatal error : %s\nProgram aborted\n", buf );
  exit(status);
}

char * FileName( char *fname, char *env, char *dir, char *ext )
{
static char pathname[MAX_PATH];
char *path;
char *crtpath;
char *p;
FILE *fp;
static char name[MAX_PATH];
int noext;

  strcpy(name, fname);
  p = name + strlen(name);
  noext = 1;
  while( p > name ) {  
    if( *p == '.') {
      noext = 0; 
      break;
    }
    if( *p == '/' ) break;
    p--;
  } 

  if( noext ) strcat(name, ext);

  fp = fopen(name,"r");
  if( fp ) {
    fclose(fp);
    return name;
  }  
   
  path = getenv(env); 
  if( path ) {
    crtpath = path;
    p = pathname;
    while( 1 ) {
      if( isspace(*crtpath) ) {
        crtpath++;
        continue;
      }
      if((*crtpath == ':')||(*crtpath==0)) {
        *p = 0;
        sprintf(pathname,"%s/%s",pathname,name);
        fp = fopen(pathname,"r");
        if( fp ) {
          fclose(fp);
          return pathname;
        }  
        if (*crtpath==0) break;
        crtpath++;
        p = pathname;
        continue;
      }
      *p++ = *crtpath++;
    }
  }
  
  sprintf(pathname, "%s/%s/%s", Home, dir, name);

  fp = fopen(pathname,"r");
  if( fp ) {
    fclose(fp);
    return pathname;
  }  
    
  return name;
}
